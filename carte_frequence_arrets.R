# --- Packages (installer au besoin) ---
# install.packages(c("sf","dplyr","ggplot2","classInt","readr","readxl","stringr","cowplot","scales"))

library(sf)
library(dplyr)
library(ggplot2)
library(classInt)
library(readr)
library(readxl)
library(stringr)
library(cowplot)
library(scales)
library(here)

# Choose the year and run the complete script
year = 2023 # 2024 # 

# --- 0) Charger les données ---
if(year == 2023){
  data <- read_csv(here("data","extract_23_cor.csv"))
}else{
  data <- read_csv(here("data","extract_24_cor.csv"))
  
}
effectif_par_dpt <- read_csv(here("data","nb_indiv_dep_annee_cor.csv"))

# Dénominateur 2024 (moyenne des effectifs par code dept)
effectif_par_dpt_2024 <- effectif_par_dpt %>%
  filter(substr(mois_declare, 1, 4) == paste0(year)) %>%
  group_by(code_postale_etab) %>%
  summarise(moyenne_effectif = mean(nombre_individus, na.rm = TRUE), .groups = "drop")

# --- 1) Charger les départements (GeoJSON) ---
url_geo <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson"
fr_dept <- st_read(url_geo, quiet = TRUE)

# --- 2) Restreindre à la France métropolitaine (Corse incluse) ---
fr_dept_metro <- fr_dept %>%
  mutate(code_chr = as.character(code)) %>%
  filter(nchar(code_chr) == 2)  # enlève les DOM (codes à 3 chiffres)

# --- 3) Clé commune ; fusionner 2A/2B en "20" (comme dans vos données) ---
fr_dept_metro <- fr_dept_metro %>%
  mutate(dept_key = if_else(code_chr %in% c("2A","2B"), "20", code_chr))

# --- 4) Agrégations côté données ---

# a) Numérateur: nb d'individus distincts avec ≥ 1 arrêt par département
data_agg <- data %>%
  filter(duree_am<15)%>%
  filter(!is.na(code_postale_etab)) %>%
  mutate(
    dept_key = if_else(code_postale_etab == 20, "20",
                       str_pad(as.character(code_postale_etab), width = 2, pad = "0"))
  ) %>%
  filter(dept_key %in% unique(fr_dept_metro$dept_key)) %>%
  group_by(dept_key) %>%
  summarise(n_indiv_avec_arret = n_distinct(pseudo_id_indiv), .groups = "drop")

# b) Dénominateur: effectif total par département
effectif_agg <- effectif_par_dpt_2024 %>%
  mutate(
    dept_key = if_else(code_postale_etab == 20, "20",
                       str_pad(as.character(code_postale_etab), width = 2, pad = "0"))
  ) %>%
  filter(dept_key %in% unique(fr_dept_metro$dept_key)) %>%
  group_by(dept_key) %>%
  summarise(moyenne_effectif = sum(moyenne_effectif, na.rm = TRUE), .groups = "drop")
# Si 'moyenne_effectif' est déjà au niveau département (non à sommer), remplacez la ligne précédente par :
# summarise(moyenne_effectif = dplyr::first(moyenne_effectif), .groups = "drop")

# --- 5) Calcul fréquence (%) + étiquettes arrondies à l'unité ---
freq_dpt <- fr_dept_metro %>%
  st_drop_geometry() %>%
  select(code = code_chr, dept_key, nom) %>%
  left_join(data_agg, by = "dept_key") %>%
  left_join(effectif_agg, by = "dept_key") %>%
  mutate(
    n_indiv_avec_arret = coalesce(n_indiv_avec_arret, 0),
    freq_pct = if_else(is.na(moyenne_effectif) | moyenne_effectif == 0,
                       NA_real_,
                       100 * n_indiv_avec_arret / moyenne_effectif),
    label_str = if_else(is.na(freq_pct), "", paste0(scales::number(freq_pct, accuracy = 1), "%"))
  )

# --- 6) Joindre aux géométries + points d’étiquette ---
map_df <- fr_dept_metro %>%
  left_join(freq_dpt %>% select(dept_key, freq_pct, label_str), by = "dept_key")

#shared_rng    <- c(20, 45)
#shared_breaks <- seq(20, 45, by = 5)

shared_rng    <- c(15, 40)

shared_breaks <- seq(15, 40, by = 5)


# Point interne pour placer chaque label
label_pts <- st_point_on_surface(map_df) %>%
  mutate(label_str = if_else(is.na(freq_pct), "", label_str))

# --- 7) Définir Île-de-France & préparer données encart ---
idf_codes <- c("75","77","78","91","92","93","94","95")
map_idf   <- map_df %>% filter(dept_key %in% idf_codes)
label_idf <- label_pts %>% filter(dept_key %in% idf_codes)

# Labels hors Île-de-France pour la carte principale
label_main <- label_pts %>% filter(!dept_key %in% idf_codes)

# Boîte en pointillés autour de l'IDF (sur la carte principale)
idf_bbox   <- st_as_sfc(st_bbox(st_union(map_idf)))
bbox_layer <- geom_sf(data = idf_bbox, fill = NA, color = "black",
                      linewidth = 0.6, linetype = "dashed")

# Palette cohérente entre carte principale et encart
rng <- range(map_df$freq_pct, na.rm = TRUE)


# --- 8) Carte principale (sans degrés ni quadrillage, sans labels IDF) ---
p_main <- ggplot() +
  geom_sf(data = map_df, aes(fill = freq_pct), color = "grey60", linewidth = 0.15) +
  bbox_layer +
  scale_fill_gradient(
    name   = "% individuals with ≥1 sick leave",
    low    = "#FFE5E5",
    high   = "#B30000",
    na.value = "white",
    limits = shared_rng,                    # ⟵ bornes identiques
    breaks = shared_breaks,
    labels = scales::label_number(accuracy = 1, suffix = "%"),
    oob    = scales::squish                 # ⟵ valeurs <20 ou >45 « écrasées » aux bornes
  ) +
  geom_sf_text(data = label_main, aes(label = label_str), size = 2.8, color = "black",fontface = "bold") +
  coord_sf(datum = NA) +  # enlève graticules et axes lat/long
  labs(
    #title = "Fréquence d'arrêt maladie par département (France métropolitaine)",
    #subtitle = "Part d'individus ayant au moins eu un arrêt en 2024"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  )

scale_fill_gradient(
  low = "#FCDDD3", high = "#BA2310", na.value = "white",
  limits = shared_rng, breaks = shared_breaks,
  oob = scales::squish, guide = "none"
)

# --- 9) Encart IDF (labels plus grands, pas de légende ni graticules) ---
p_idf <- ggplot() +
  geom_sf(data = map_idf, aes(fill = freq_pct), color = "grey60", linewidth = 0.2) +
  scale_fill_gradient(
    low = "#FCDDD3", high = "#BA2310", na.value = "white",
    limits = shared_rng, breaks = shared_breaks,
    oob = scales::squish, guide = "none"
  ) +
  geom_sf_text(data = label_idf, aes(label = label_str), size = 3.8, color = "black",fontface = "bold") +
  coord_sf(
    xlim = st_bbox(map_idf)[c("xmin","xmax")],
    ylim = st_bbox(map_idf)[c("ymin","ymax")],
    expand = FALSE,
    datum = NA
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = "grey40", linewidth = 0.4)
  )

# --- 10) Assembler (placer l’encart en bas à droite) ---
final_plot <- ggdraw(p_main) +
  draw_plot(p_idf, x = 0.62, y = 0.65, width = 0.35, height = 0.35)

print(final_plot)

# --- 11) Export (optionnel) ---
ggsave(here("figures", paste0("sick_leave_frequency_per_departement_under_15_days_",year,".png")), final_plot, width = 5000, height = 3500, units = "px",dpi = 500)
