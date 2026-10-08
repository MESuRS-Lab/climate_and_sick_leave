library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(here)

# Charge dataset
data_2023 <- read_csv(here("data","extract_23_cor.csv"))
data_2024 <- read_csv(here("data","extract_24_cor.csv"))

#### Obtain graph of number of first sick leaves by month (all durations) ####

# Filter first sick leave per individual
first_arret_2023 <- data_2023 %>%
  mutate(dt_debut_arret = as_date(dt_debut_arret)) %>%
  filter(!is.na(dt_debut_arret), year(dt_debut_arret) == 2023) %>%
  group_by(pseudo_id_indiv) %>%
  slice_min(order_by = dt_debut_arret, n = 1, with_ties = FALSE) %>%
  ungroup()

first_arret_2024 <- data_2024 %>%
  mutate(dt_debut_arret = as_date(dt_debut_arret)) %>%
  filter(!is.na(dt_debut_arret), year(dt_debut_arret) == 2024) %>%
  group_by(pseudo_id_indiv) %>%
  slice_min(order_by = dt_debut_arret, n = 1, with_ties = FALSE) %>%
  ungroup()

# Agregate by month
tempo_mois_2023 <- first_arret_2023 %>%
  mutate(mois = floor_date(dt_debut_arret, "month")) %>%
  count(mois, name = "n_indiv_premier_arret") %>%
  mutate(mois_nom = month(mois, label = TRUE,locale = "en_US")) %>%
  mutate(year = 2023)

tempo_mois_2024 <- first_arret_2024 %>%
  mutate(mois = floor_date(dt_debut_arret, "month")) %>%
  count(mois, name = "n_indiv_premier_arret")%>%
  mutate(mois_nom = month(mois, label = TRUE,  locale = "en_US")) %>%
  mutate(year = 2024)

tempo_mois <- bind_rows(tempo_mois_2023, tempo_mois_2024)

# Graph 
ggplot(tempo_mois, aes(x = mois_nom, y = n_indiv_premier_arret, color = factor(year), group = factor(year))) +
  geom_point(size = 3) +
  geom_line(linewidth = 1)+
  scale_color_manual(values = c("#F3B5A2","#96534b")) +
  labs(x = "Month", y = "Number of individuals with first sick leave", color = "Year") +
  theme_minimal()+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)
  )+
  ylim(0,215000)

# Save
ggsave(here("figures", "first_sick_leave_by_month_all_durations.jpg"), width = 7000, height = 3500, dpi = 600, units = "px")

#### Obtain graph of number of first sick leaves by month (under 15 days only) ####

# Filter first sick leave per individual
first_arret_2023 <- data_2023 %>%
  filter(duree_am <=15) %>%
  mutate(dt_debut_arret = as_date(dt_debut_arret)) %>%
  filter(!is.na(dt_debut_arret), year(dt_debut_arret) == 2023) %>%
  group_by(pseudo_id_indiv) %>%
  slice_min(order_by = dt_debut_arret, n = 1, with_ties = FALSE) %>%
  ungroup()

first_arret_2024 <- data_2024 %>%
  filter(duree_am <=15) %>%
  mutate(dt_debut_arret = as_date(dt_debut_arret)) %>%
  filter(!is.na(dt_debut_arret), year(dt_debut_arret) == 2024) %>%
  group_by(pseudo_id_indiv) %>%
  slice_min(order_by = dt_debut_arret, n = 1, with_ties = FALSE) %>%
  ungroup()

# Agregate by month
tempo_mois_2023 <- first_arret_2023 %>%
  mutate(mois = floor_date(dt_debut_arret, "month")) %>%
  count(mois, name = "n_indiv_premier_arret") %>%
  mutate(mois_nom = month(mois, label = TRUE,locale = "en_US")) %>%
  mutate(year = 2023)

tempo_mois_2024 <- first_arret_2024 %>%
  mutate(mois = floor_date(dt_debut_arret, "month")) %>%
  count(mois, name = "n_indiv_premier_arret")%>%
  mutate(mois_nom = month(mois, label = TRUE,  locale = "en_US")) %>%
  mutate(year = 2024)

tempo_mois <- bind_rows(tempo_mois_2023, tempo_mois_2024)

# Graph 
ggplot(tempo_mois, aes(x = mois_nom, y = n_indiv_premier_arret, color = factor(year), group = factor(year))) +
  geom_point(size = 3) +
  geom_line(linewidth = 1)+
  scale_color_manual(values = c("#F3B5A2","#96534b")) +
  labs(x = "Month", y = "Number of individuals with first sick leave", color = "Year") +
  theme_minimal()+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)
  )+
  ylim(0,210000)

# Save
ggsave(here("figures", "first_sick_leave_by_month_under_15_days.jpg"), width = 7000, height = 3500, dpi = 600, units = "px")


#### Obtain graph of number of all sick leaves by month (all durations) ####

tempo_mois_2023 <- data_2023 %>%
  #filter(duree_am <=15) %>%
  mutate(mois = floor_date(dt_debut_arret, "month")) %>%
  count(mois, name = "n_indiv_premier_arret") %>%
  mutate(mois_nom = month(mois, label = TRUE,locale = "en_US")) %>%
  mutate(year = 2023)

tempo_mois_2024 <- data_2024 %>%
  #filter(duree_am <=15) %>%
  mutate(mois = floor_date(dt_debut_arret, "month")) %>%
  count(mois, name = "n_indiv_premier_arret")%>%
  mutate(mois_nom = month(mois, label = TRUE,  locale = "en_US")) %>%
  mutate(year = 2024)%>%
  slice_head(n=12)

tempo_mois <- bind_rows(tempo_mois_2023, tempo_mois_2024)

ggplot(tempo_mois, aes(x = mois_nom, y = n_indiv_premier_arret, color = factor(year), group = factor(year))) +
  geom_point(size = 3) +
  geom_line(linewidth = 1)+
  scale_color_manual(values = c("#F3B5A2","#96534b")) +
  labs(x = "Month", y = "Number of individuals with sick leave", color = "Year") +
  theme_minimal()+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)
  )+
  ylim(0,230000)

ggsave(here("figures", "all_sick_leaves_by_month_all_durations.jpg"), width = 7000, height = 3500, dpi = 600, units = "px")


#### Obtain graph of number of all sick leaves by month (under 15 days) ####

tempo_mois_2023 <- data_2023 %>%
  filter(duree_am <=15) %>%
  mutate(mois = floor_date(dt_debut_arret, "month")) %>%
  count(mois, name = "n_indiv_premier_arret") %>%
  mutate(mois_nom = month(mois, label = TRUE,locale = "en_US")) %>%
  mutate(year = 2023)

tempo_mois_2024 <- data_2024 %>%
  filter(duree_am <=15) %>%
  mutate(mois = floor_date(dt_debut_arret, "month")) %>%
  count(mois, name = "n_indiv_premier_arret")%>%
  mutate(mois_nom = month(mois, label = TRUE,  locale = "en_US")) %>%
  mutate(year = 2024)%>%
  slice_head(n=12)

tempo_mois <- bind_rows(tempo_mois_2023, tempo_mois_2024)

ggplot(tempo_mois, aes(x = mois_nom, y = n_indiv_premier_arret, color = factor(year), group = factor(year))) +
  geom_point(size = 3) +
  geom_line(linewidth = 1)+
  scale_color_manual(values = c("#F3B5A2","#96534b")) +
  labs(x = "Month", y = "Number of individuals with sick leave", color = "Year") +
  theme_minimal()+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)
  )+
  ylim(0,210000)

ggsave(here("figures", "all_sick_leaves_by_month_under_15_days.jpg"), width = 7000, height = 3500, dpi = 600, units = "px")

#### Obtain graph of number of all sick leaves by week (all durations) ####

# Agregate by week
tempo_sem_2023 <- data_2023 %>%
  mutate(semaine = as.Date(floor_date(dt_debut_arret, "week", week_start = 1))) %>%
  count(semaine, name = "n_indiv_arret") %>%
  mutate(year = 2023)%>%
  mutate(semaine_nbr = row_number())

tempo_sem_2024 <- data_2024 %>%
  mutate(semaine = as.Date(floor_date(dt_debut_arret, "week", week_start = 1))) %>%
  count(semaine, name = "n_indiv_arret") %>%
  mutate(year = 2024)%>%
  mutate(semaine_nbr = row_number())

tempo_sem <- bind_rows(tempo_sem_2023, tempo_sem_2024)

ggplot(tempo_sem, aes(x = semaine, y = n_indiv_arret, color = factor(year), group = factor(year))) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#F3B5A2","#96534b")) +
  
  # Axe x avec dates
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week") +
  
  labs(x = "Week start date", y = "Number of individuals with sick leave", color = "Year") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 14)
  ) +
  scale_x_date(
    name = "Week start date",
    date_labels = "%Y-%m-%d",
    date_breaks = "1 week",
    minor_breaks = NULL,
    expand = c(0.01, 0.01)  # <-- supprime les marges
  ) +
  # Facette par année pour avoir deux axes x indépendants
  facet_wrap( ~ year, scales = "free_x", ncol = 1)

ggsave(here("figures", "all_sick_leaves_by_week_all_durations.jpg"), width = 7000, height = 3500, dpi = 500, units = "px")


#### Obtain graph of number of all sick leaves by week (all durations) ####

# Agregate by week
tempo_sem_2023 <- data_2023 %>%
  filter(duree_am <=15) %>%
  mutate(semaine = as.Date(floor_date(dt_debut_arret, "week", week_start = 1))) %>%
  count(semaine, name = "n_indiv_arret") %>%
  mutate(year = 2023)%>%
  mutate(semaine_nbr = row_number())

tempo_sem_2024 <- data_2024 %>%
  filter(duree_am <=15) %>%
  mutate(semaine = as.Date(floor_date(dt_debut_arret, "week", week_start = 1))) %>%
  count(semaine, name = "n_indiv_arret") %>%
  mutate(year = 2024)%>%
  mutate(semaine_nbr = row_number())

tempo_sem <- bind_rows(tempo_sem_2023, tempo_sem_2024)

ggplot(tempo_sem, aes(x = semaine, y = n_indiv_arret, color = factor(year), group = factor(year))) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#F3B5A2","#96534b")) +
  
  # Axe x avec dates
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week") +
  
  labs(x = "Week start date", y = "Number of individuals with sick leave", color = "Year") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 14)
  ) +
  scale_x_date(
    name = "Week start date",
    date_labels = "%Y-%m-%d",
    date_breaks = "1 week",
    minor_breaks = NULL,
    expand = c(0.01, 0.01)  # <-- supprime les marges
  ) +
  # Facette par année pour avoir deux axes x indépendants
  facet_wrap( ~ year, scales = "free_x", ncol = 1)

ggsave(here("figures", "all_sick_leaves_by_week_under_15_days.jpg"), width = 7000, height = 3500, dpi = 500, units = "px")
