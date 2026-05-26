library(lubridate)
library(dplyr)
library(readxl)
library(here)
library(ggplot2)
library(zoo)
library(tidyr)

## STEP 1 : Load data #####

# Case data (ne change jamais)
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

case_data_m %>%
  count(date_debut_arret) %>%
  mutate(wkday = wday(date_debut_arret, label = T)) %>%
  ggplot(aes(x = date_debut_arret, y = n, colour = wkday)) +
  geom_point() +
  labs(y = "Nombre d'arrêts par jour", x = "Date de début d'arrêt")

# Ce dataset contient les données d'exposition pour chaque jour
# Eg si on veut que un arrêt jour j ait les données jour j, ou j-2, ou j-7... Il faut changer
variable_data <- read_xlsx(here("data", "df_combined.xlsx"))
resp_data <- read.csv(here("data", "ira-departement.csv"))

#reformat the date from the format yearweek to a more useful format
resp_data = resp_data %>%
  filter(Classe.d.âge == "15-64 ans") %>%
  filter(grepl("2023|2024", X1er.jour.de.la.semaine)) %>%
  select(c(1,3,6))
colnames(resp_data) = c("date_debut_arret", "departement", "incidence_IRA")

#expand the dataset to include all combinations of date_debut_arret and departement, filling missing values with NA
resp_data = resp_data %>%
  complete(date_debut_arret, departement) %>%
  mutate(incidence_IRA = ifelse(is.na(incidence_IRA), 0, incidence_IRA),
         date_debut_arret = as_date(date_debut_arret))

resp_data_corse = resp_data %>%
  filter(departement %in% c("2A", "2B")) %>%
  group_by(date_debut_arret) %>%
  summarise(incidence_IRA = mean(incidence_IRA)) %>%
  ungroup %>%
  mutate(departement = "20")

resp_data = resp_data %>%
  filter(!departement %in% c("2A", "2B")) %>%
  bind_rows(resp_data_corse) %>%
  arrange(date_debut_arret, departement) %>%
  mutate(departement = as.character(as.numeric(departement)))

#copy the dataset such that all days are included, not just the weeks
resp_data = rbind(resp_data,
                  resp_data %>% mutate(date_debut_arret = date_debut_arret + 1),
                  resp_data %>% mutate(date_debut_arret = date_debut_arret + 2),
                  resp_data %>% mutate(date_debut_arret = date_debut_arret + 3),
                  resp_data %>% mutate(date_debut_arret = date_debut_arret + 4),
                  resp_data %>% mutate(date_debut_arret = date_debut_arret + 5),
                  resp_data %>% mutate(date_debut_arret = date_debut_arret + 6))

ggplot(resp_data, aes(x = date_debut_arret, y = incidence_IRA, colour = departement)) +
  geom_line() +
  labs(y = "Incidence d'IRA par jour", x = "Date de début d'arrêt")

variable_data %>%
  group_by(date_debut_arret) %>%
  mutate(pm25=mean(pm25, na.rm=T)) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = as_date(date_debut_arret), y = pm25))

variable_data = left_join(variable_data, resp_data, by = c("date_debut_arret", "departement"))


reformat_variable_data = function(variable_data, lag_test, duree_test, function_test){
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
  
  return(variable_data_m)
}



## STEP 2 : Create control dates for each case #####
create_controls = function(case_data_m, control_per_case, onlybefore=T){
  
  if(onlybefore){
    if(control_per_case==2){
      
      control_dates <- case_data_m %>%
        rowwise() %>%
        do({
          data.frame(
            date_debut_arret = c(.data$date_debut_arret - 7, .data$date_debut_arret - 14),
            pseudo_id_indiv = .data$pseudo_id_indiv,
            departement = .data$departement
          )
        }) %>%
        ungroup()
      
      return(control_dates)
      
    } else if(control_per_case==1){
      
      control_dates <- case_data_m %>%
        rowwise() %>%
        do({
          data.frame(
            date_debut_arret = c(.data$date_debut_arret - 7),
            pseudo_id_indiv = .data$pseudo_id_indiv,
            departement = .data$departement
          )
        }) %>%
        ungroup()
      
      return(control_dates)
      
    } else {
      stop("control_per_case must be 1 or 2")
    } 
  } else {
    if(control_per_case==2){
      
      control_dates <- case_data_m %>%
        rowwise() %>%
        do({
          data.frame(
            date_debut_arret = c(.data$date_debut_arret - 7, .data$date_debut_arret + 7),
            pseudo_id_indiv = .data$pseudo_id_indiv,
            departement = .data$departement
          )
        }) %>%
        ungroup()
      
      return(control_dates)
      
    } else if(control_per_case==1){
      
      control_dates <- case_data_m %>%
        rowwise() %>%
        do({
          data.frame(
            date_debut_arret = c(.data$date_debut_arret - 7),
            pseudo_id_indiv = .data$pseudo_id_indiv,
            departement = .data$departement
          )
        }) %>%
        ungroup()
      
      return(control_dates)
      
    } else {
      stop("control_per_case must be 1 or 2")
    }
    
  }
  
}

## STEP 3 : Create case-crossover dataset #####
# This needs to be re-run if you change the lag/duration of exposure variables
create_case_crossover_data = function(case_data_m, variable_data_m, control_dates, control_per_case){
  
  case_crossover_data <- case_data_m %>%
    left_join(variable_data_m, by = c("date_debut_arret", "departement")) %>%
    select(status, pseudo_id_indiv, departement, date_debut_arret, day_debut_arret,
           date_fin_arret_maladie, sexe, date_naissance, csp_group_cor,
           secteur_etab, effectif_entrep, nature_contrat, Temps_travail, TM_mean_roll,
           pm25_roll, NO2_roll, O3_roll, incidence_IRA)
  
  # Join control dates with variable data to get control variables
  control_data <- control_dates %>%
    left_join(variable_data_m, by = c("date_debut_arret", "departement")) %>%
    mutate(status = 0) %>%
    mutate(annee = year(date_debut_arret)) %>%
    select(status, pseudo_id_indiv, annee, departement, date_debut_arret, TM_mean_roll, pm25_roll, NO2_roll, O3_roll, incidence_IRA, day_debut_arret) %>%
    left_join(case_data_m %>% select(pseudo_id_indiv, annee, sexe, date_naissance, csp_group_cor,
                                     secteur_etab, effectif_entrep, nature_contrat, Temps_travail, date_fin_arret_maladie),
              by = c("pseudo_id_indiv", "annee")) %>%
    select(colnames(case_crossover_data))
  
  
  # Combine case and control data
  # on ne garde que les cas qui ont le bon nombre
  final_data <- bind_rows(case_crossover_data, control_data) %>%
    filter(!is.na(TM_mean_roll) & !is.na(pm25_roll) & !is.na(NO2_roll) & !is.na(O3_roll))
  
  if(control_per_case == 1){
    good_ids = final_data %>%
      count(pseudo_id_indiv) %>%
      # filter(n %in% c(3,6)) %>%
      filter(n %in% c(2,4)) %>%
      select(pseudo_id_indiv) %>% pull
  } else if(control_per_case == 2){
    good_ids = final_data %>%
      count(pseudo_id_indiv) %>%
      filter(n %in% c(3,6)) %>%
      select(pseudo_id_indiv) %>% pull
  } else {
    stop("control_per_case muste be 1 or 2")
  }
  
  final_data <- final_data %>%
    filter(pseudo_id_indiv %in% good_ids)
  
  return(final_data)
  
}

## STEP 4 : Run the regression #####

run_regression = function(final_data, scenario, multivar = F){
  
  if(multivar){
    
    res = data.frame(scenario = scenario,
                     TM_or = NA,
                     TM_p = NA,
                     pm25_or = NA,
                     pm25_p = NA,
                     NO2_or = NA,
                     NO2_p = NA,
                     O3_or = NA,
                     O3_p = NA,
                     IRA_or = NA,
                     IRA_p = NA,
                     AIC = NA)
    
    model = glm(data = final_data, formula = status ~ TM_mean_roll+pm25_roll+NO2_roll+O3_roll+incidence_IRA, family = binomial(link = "logit"))
    res$TM_or = exp(coef(model))["TM_mean_roll"]
    res$TM_p = summary(model)$coefficients["TM_mean_roll", 4]
    res$pm25_or = exp(coef(model)*10)["pm25_roll"]
    res$pm25_p = summary(model)$coefficients["pm25_roll", 4]
    res$NO2_or = exp(coef(model)*10)["NO2_roll"]
    res$NO2_p = summary(model)$coefficients["NO2_roll", 4]
    res$O3_or = exp(coef(model)*10)["O3_roll"]
    res$O3_p = summary(model)$coefficients["O3_roll", 4]
    res$IRA_or = exp(coef(model))["incidence_IRA"]
    res$IRA_p = summary(model)$coefficients["incidence_IRA", 4]
    res$AIC = model$aic
    
  } else {
    
    res = data.frame(scenario = scenario,
                     pm25_or = NA,
                     pm25_p = NA,
                     pm25_AIC = NA,
                     NO2_or = NA,
                     NO2_p = NA,
                     NO2_AIC = NA,
                     O3_or = NA,
                     O3_p = NA,
                     O3_AIC = NA)
    
    model_pm25 = glm(data = final_data, formula = status ~ TM_mean_roll+pm25_roll+incidence_IRA, family = binomial(link = "logit"))
    res$pm25_or = exp(coef(model_pm25)*10)["pm25_roll"]
    res$pm25_p = summary(model_pm25)$coefficients["pm25_roll", 4]
    res$pm25_AIC = model_pm25$aic
    
    model_NO2 = glm(data = final_data, formula = status ~ TM_mean_roll+NO2_roll+incidence_IRA, family = binomial(link = "logit"))
    res$NO2_or = exp(coef(model_NO2)*10)["NO2_roll"]
    res$NO2_p = summary(model_NO2)$coefficients["NO2_roll", 4]
    res$NO2_AIC = model_NO2$aic
    
    model_O3 = glm(data = final_data, formula = status ~ TM_mean_roll+O3_roll+incidence_IRA, family = binomial(link = "logit"))
    res$O3_or = exp(coef(model_O3)*10)["O3_roll"]
    res$O3_p = summary(model_O3)$coefficients["O3_roll", 4]
    res$O3_AIC = model_O3$aic
    # model = glm(data = final_data, formula = status ~ TM_mean_roll+pm25_roll+NO2_roll+O3_roll, family = binomial(link = "logit"))
    
  }
  
  return(res)
  
}

res_univar = data.frame()
res_multivar = data.frame()

# KRIIT ####
scenario = "kriit"
lag_test=2
duree_test=3
function_test = mean
control_per_case = 2

variable_data_m = reformat_variable_data(variable_data, lag_test, duree_test, function_test)

ggplot(variable_data_m) +
  geom_point(aes(incidence_IRA, incidence_am)) +
  theme_bw()

ggplot(variable_data_m %>% filter(!is.na(TM_mean_roll))) +
  geom_point(aes(TM_mean_roll, incidence_IRA)) +
  theme_bw()


case_data_m = case_data_m %>%
  filter(duree_am < 14) %>%
  filter(date_debut_arret %in% unique(variable_data_m$date_debut_arret)) %>%
  filter(wkday %in% c("mer\\.","mar\\.","ven\\.","jeu\\."))

control_dates = create_controls(case_data_m, control_per_case, onlybefore = F)
final_data = create_case_crossover_data(case_data_m, variable_data_m, control_dates, control_per_case)

res_univar = rbind(res_univar, run_regression(final_data, scenario))
res_multivar = rbind(res_multivar, run_regression(final_data, paste0(scenario, "_multivar"), multivar = T))

# BASELINE ####
scenario = "baseline"
lag_test=2
duree_test=3
function_test = mean
control_per_case = 2

variable_data_m = reformat_variable_data(variable_data, lag_test, duree_test, function_test)

control_dates = create_controls(case_data_m, control_per_case)
final_data = create_case_crossover_data(case_data_m, variable_data_m, control_dates, control_per_case)

res_univar = rbind(res_univar, run_regression(final_data, scenario))
res_multivar = rbind(res_multivar, run_regression(final_data, paste0(scenario, "_multivar"), multivar = T))

# ANALYSIS 2 ####
scenario = "analysis 2"
lag_test=4
duree_test=3
function_test = mean
control_per_case = 2

variable_data_m = reformat_variable_data(variable_data, lag_test, duree_test, function_test)
# control_dates = create_controls(case_data_m, control_per_case)
final_data = create_case_crossover_data(case_data_m, variable_data_m, control_dates, control_per_case)

res_univar = rbind(res_univar, run_regression(final_data, scenario))
res_multivar = rbind(res_multivar, run_regression(final_data, paste0(scenario, "_multivar"), multivar = T))

# ANALYSIS 3 ####
scenario = "analysis 3"
lag_test=4
duree_test=5
function_test = mean
control_per_case = 2

variable_data_m = reformat_variable_data(variable_data, lag_test, duree_test, function_test)
# control_dates = create_controls(case_data_m, control_per_case)
final_data = create_case_crossover_data(case_data_m, variable_data_m, control_dates, control_per_case)

res_univar = rbind(res_univar, run_regression(final_data, scenario))
res_multivar = rbind(res_multivar, run_regression(final_data, paste0(scenario, "_multivar"), multivar = T))

# ANALYSIS 4 ####
scenario = "analysis 4"
lag_test=2
duree_test=3
function_test = max
control_per_case = 2

variable_data_m = reformat_variable_data(variable_data, lag_test, duree_test, function_test)
# control_dates = create_controls(case_data_m, control_per_case)
final_data = create_case_crossover_data(case_data_m, variable_data_m, control_dates, control_per_case)

res_univar = rbind(res_univar, run_regression(final_data, scenario))
res_multivar = rbind(res_multivar, run_regression(final_data, paste0(scenario, "_multivar"), multivar = T))

# ANALYSIS 1 ####
scenario = "analysis 1"
lag_test=2
duree_test=3
function_test = mean
control_per_case = 1

variable_data_m = reformat_variable_data(variable_data, lag_test, duree_test, function_test)
control_dates = create_controls(case_data_m, control_per_case)
final_data = create_case_crossover_data(case_data_m, variable_data_m, control_dates, control_per_case)

res_univar = rbind(res_univar, run_regression(final_data, scenario))
res_multivar = rbind(res_multivar, run_regression(final_data, paste0(scenario, "_multivar"), multivar = T))


# Cleanup results table

res_univar_clean = res_univar %>%
  mutate(pm25_or = round(pm25_or, 3),
         pm25_p = round(pm25_p, 4),
         NO2_or = round(NO2_or, 3),
         NO2_p = round(NO2_p, 4),
         O3_or = round(O3_or, 3),
         O3_p = round(O3_p, 4))

res_multivar_clean = res_multivar %>%
  mutate(pm25_or = round(pm25_or, 3),
         pm25_p = round(pm25_p, 4),
         NO2_or = round(NO2_or, 3),
         NO2_p = round(NO2_p, 4),
         O3_or = round(O3_or, 3),
         O3_p = round(O3_p, 4))
