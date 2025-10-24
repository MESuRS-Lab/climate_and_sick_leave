library(lubridate)
library(dplyr)
library(readxl)
library(here)
library(ggplot2)
library(zoo)

# Vérifier:
# 4) est ce qu'on garde que le 1er arrêt?
# pour l'instant oui, sinon pb pour les contrôles
# vérifier si ça n'écarte pas des gens fragiles

# Load data
# Ce dataset contient les données d'exposition pour chaque jour
# Eg si on veut que un arrêt jour j ait les données jour j, ou j-2, ou j-7... Il faut changer
variable_data <- read_xlsx(here("data", "df_combined.xlsx"))
# lag = nombre de jours qu'on recule/qu'on ne prend pas en compte
# durée = nombre de jours sur lesquels on fait la moyenne glissante
# exemple: si on veut faire la moyenne sur j-4, j-3, j-2, alors:
# lag = 2 (on ignore j et j-1, 2 jours) et durée = 3 (on fait la moyenne sur 3 jours)
lag_test=2
duree_test=3
function_test = mean
# on veut faire une moyenne glissante des données d'exposition en fonction du lag et de la durée de la fenêtre
variable_data_m <- variable_data %>%
  mutate(date_debut_arret = as_date(dt_debut_arret)) %>%
  group_by(departement) %>%
  mutate(TM_mean_roll = rollapply(
    lag(TM_mean, n=lag_test),
    width = duree_test,
    FUN = function_test,
    align = "right",
    fill = NA)) %>%
  mutate(pm25_roll = rollapply(
    lag(pm25, n=lag_test),
    width = duree_test,
    FUN = function_test,
    align = "right",
    fill = NA)) %>%
  mutate(NO2_roll = rollapply(
    lag(NO2, n=lag_test),
    width = duree_test,
    FUN = function_test,
    align = "right",
    fill = NA)) %>%
  mutate(O3_roll = rollapply(
    lag(O3, n=lag_test),
    width = duree_test,
    FUN = function_test,
    align = "right",
    fill = NA)) %>%
  ungroup


case_data23 <- read.csv(here("data", "extract_23_cor.csv"))
case_data24 <- read.csv(here("data", "extract_24_cor.csv"))
case_data = bind_rows(case_data23, case_data24)
case_data_m <- case_data %>%
  mutate(date_naissance = ifelse(date_naissance == "invalid_date", "1899-01-01", date_naissance)) %>%
  mutate(date_debut_arret = as_date(dt_debut_arret),
         date_fin_arret_maladie = as_date(date_fin_arret_maladie),
         date_naissance = as_date(date_naissance)) %>%
  mutate(annee = year(date_debut_arret)) %>%
  rename(departement = code_postale_etab) %>%
  # on efface les départements manquants ou Outre-Mer
  filter(departement != "", departement != "nu", departement != "97", departement != "98") %>%
  # on peut effacer les individus avec sexe manquant ou date de naissance invalide
  # filter(sexe != "", date_naissance != as_date("1899-01-01")) %>%
  mutate(status = 1) %>%
  mutate(wkday = wday(date_debut_arret, label = T)) %>%
  # on ne garde que la première instance de arrêt par individu
  group_by(pseudo_id_indiv, annee) %>%
  slice_min(order_by = date_debut_arret, n = 1) %>%
  ungroup()

# Create the case crossover dataset
case_crossover_data <- case_data_m %>%
  left_join(variable_data_m, by = c("date_debut_arret", "departement")) %>%
  select(status, pseudo_id_indiv, departement, date_debut_arret, day_debut_arret,
         date_fin_arret_maladie, sexe, date_naissance, csp_group_cor,
         secteur_etab, effectif_entrep, nature_contrat, Temps_travail, TM_mean,
         pm25, NO2, O3)

# Create control dates for each case
control_dates <- case_crossover_data %>%
  rowwise() %>%
  do({
    data.frame(
      date_debut_arret = c(.data$date_debut_arret - 7, .data$date_debut_arret - 14),
      pseudo_id_indiv = .data$pseudo_id_indiv,
      departement = .data$departement
    )
  }) %>%
  ungroup()

# Join control dates with variable data to get control variables
control_data <- control_dates %>%
  left_join(variable_data_m, by = c("date_debut_arret", "departement")) %>%
  mutate(status = 0) %>%
  mutate(annee = year(date_debut_arret)) %>%
  select(status, pseudo_id_indiv, annee, departement, date_debut_arret, TM_mean, pm25, NO2, O3, day_debut_arret) %>%
  left_join(case_data_m %>% select(pseudo_id_indiv, annee, sexe, date_naissance, csp_group_cor,
                                   secteur_etab, effectif_entrep, nature_contrat, Temps_travail, date_fin_arret_maladie),
            by = c("pseudo_id_indiv", "annee")) %>%
  select(colnames(case_crossover_data))

# Combine case and control data
# on supprime les cas qui n'ont pas 2 contrôles
# ATTENTION le 3 doit changer ici si jamais on a + de contrôles par cas
final_data <- bind_rows(case_crossover_data, control_data) %>%
  filter(!is.na(TM_mean) & !is.na(pm25) & !is.na(NO2) & !is.na(O3))

good_ids = final_data %>%
  count(pseudo_id_indiv) %>%
  filter(n %in% c(3,6)) %>%
  select(pseudo_id_indiv) %>% pull

final_data <- final_data %>%
  filter(pseudo_id_indiv %in% good_ids)

# Optionally, you can save the final dataset
# write.csv(final_data, here("data", "final_case_crossover_data.csv"), row.names = FALSE)

model = glm(data = final_data, formula = status ~ TM_mean+pm25+NO2+O3, family = binomial(link = "logit"))
summary(model)

