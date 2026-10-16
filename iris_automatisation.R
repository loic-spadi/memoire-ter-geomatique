# ===== Installation des packages et activation ================================

# install.packages("readr")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("tidyr")
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# ===== Ouverture des bases de données =========================================
#Avec l'appui de l'intelligence artificielle générative sur la validation
#du code et rectifications

data_diplome = read_delim("base-ic-diplomes-formation-2022.CSV", delim = ";",
                          locale = locale(decimal_mark = "."),
                          col_types = cols(
                            IRIS = col_character(),
                            COM = col_character(),
                            TYP_IRIS = col_character(),
                            LAB_IRIS = col_character(),
                            .default = col_double()
                          ))

data_rev = read_delim("BASE_TD_FILO_IRIS_2021_DISP.csv", delim = ";",
                      locale = locale(decimal_mark = ","),
                      col_types = cols(
                        IRIS = col_character(),
                        .default = col_double()
                      ))

data_com = read_delim("table-appartenance-geo-communes-2024.csv", delim = ",",
                      col_types = cols(
                        .default = col_character()
                      ))

data_typo = read_delim("AAV2020_au_01-01-2026.csv", delim = ",",
                      col_types = cols(
                        .default = col_character()
                      ))

# ===== Listing des métropoles et encapsulage du code dans une fonction ========

epci_metropoles = c(
  "Aix-Marseille"          = "200054807",
  "Bordeaux"               = "243300316",
  "Brest"                  = "242900314",
  "Clermont-Ferrand"       = "246300701",
  "Dijon"                  = "242100410",
  "Grenoble"               = "200040715",
  "Lille"                  = "200093201",
  "Lyon"                   = "200046977",
  "Metz"                   = "200039865",
  "Montpellier"            = "243400017",
  "Nancy"                  = "245400676",
  "Nantes"                 = "244400404",
  "Nice"                   = "200030195",
  "Orléans"                = "244500468",
  "Paris"                  = "200054781",
  "Rennes"                 = "243500139",
  "Rouen"                  = "200023414",
  "Saint-Etienne"          = "244200770",
  "Strasbourg"             = "246700488",
  "Toulon"                 = "248300543",
  "Toulouse"               = "243100518",
  "Tours"                  = "243700754"
)

analyse_metropole = function(nom_ville) {
  
  code_epci <- epci_metropoles[nom_ville]

# ===== Filtrage sur la métropole (diplômes) ===================================
  
  com_metro = data_com %>%
    filter(EPCI == code_epci) %>%
    pull(CODGEO)
  
  data_diplome = data_diplome %>%
    mutate(
      COM_commune = case_when(
        substr(COM, 1, 3) == "751"  ~ "75056",
        substr(COM, 1, 4) == "6938" ~ "69123",
        substr(COM, 1, 3) == "132"  ~ "13055",
        TRUE ~ COM
      )
    )
  
  diplomes_metro = data_diplome %>%
    filter(COM_commune %in% com_metro) %>%
    mutate(across(starts_with("P22_"), ~ round(.x, 0)))
  
  # les données ont été arrondies pour avoir un nombre entier d'individus, étant
  # donné les caractéristiques de l'INSEE qui vise à faire des estimations ^^
  
  # Avec l'appui de l'intelligence artificielle générative pour constuire cette
  # partie du code ^^
  
# ===== Application d'une typologie de la base AAV =============================
  
  diplomes_metro = diplomes_metro %>%
    left_join(
      data_typo %>% select(CODGEO, CATEAAV2020),
      by = c("COM_commune" = "CODGEO")
    ) %>%
    mutate(
      typologie = case_when(
        CATEAAV2020 == "11" ~ "Commune-centre",
        CATEAAV2020 == "12" ~ "Banlieue",
        CATEAAV2020 == "20" ~ "Périurbain",
        TRUE                ~ NA_character_
      )
    ) %>%
    filter(!is.na(typologie))
  
# ===== application d'une pondération par âge de scolarité convenue ============
  
  diplomes_metro = diplomes_metro %>%
    mutate(
      scolarite_pondi = (
        P22_NSCOL15P_DIPLMIN * 5  +
          P22_NSCOL15P_BEPC    * 9  +
          P22_NSCOL15P_CAPBEP  * 11 +
          P22_NSCOL15P_BAC     * 12 +
          P22_NSCOL15P_SUP2    * 14 +
          P22_NSCOL15P_SUP34   * 15 +
          P22_NSCOL15P_SUP5    * 17
      ),
      total_pop_scolarise = round(P22_NSCOL15P, 0),
      duree_moy_scolarise = round(scolarite_pondi / total_pop_scolarise, 2)
    )
  
# ===== filtrage par métropole (revenus) =======================================
  
  revenus_metro = data_rev %>%
    mutate(
      COM_rev = substr(IRIS, 1, 5),
      COM_commune = case_when(
        substr(COM_rev, 1, 3) == "751"  ~ "75056",
        substr(COM_rev, 1, 4) == "6938" ~ "69123",
        substr(COM_rev, 1, 3) == "132"  ~ "13055",
        TRUE ~ COM_rev
      )
    ) %>%
    filter(COM_commune %in% com_metro) %>%
    select(IRIS, revenu_median = DISP_MED21)
  
# ===== Création d'un tableau récap. des données par IRIS (rev+dipl) ===========
  
  data_iris = diplomes_metro %>%
    select(IRIS, COM_commune, duree_moy_scolarise, typologie) %>%
    left_join(revenus_metro, by = "IRIS") %>%
    arrange(IRIS)

# =====  Préparation de standardisation des données ============================
  
  data_iris = data_iris %>%
    mutate(
      cume_age = cume_dist(duree_moy_scolarise),
      cume_revenu = cume_dist(revenu_median)
    )
  
  brute = ggplot(data_iris, aes(x = revenu_median, y = duree_moy_scolarise)) +
    geom_point(color = "steelblue", alpha = 0.7) +
    labs(title = paste0("Données brutes -", nom_ville),
         x = "Revenu médian (€)",
         y = "Durée moyenne de scolarité")+
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5))
  
# ===== Distribution cumulative de revenus =====================================
  # Avec l'appui de l'intelligence artificielle générative sur la création de la
  # courbe de distribution cumulative remise à l'échelle avec ajout d'un
  # second axe
  
  max_barre = max(table(cut(data_iris$revenu_median,
                             breaks = 10))) / nrow(data_iris) * 100
  
  distri_cumul = ggplot(data_iris, aes(x = revenu_median)) +
    geom_histogram(aes(y = after_stat(count / sum(count) * 100)),
                   bins  = 10,
                   fill  = "steelblue",
                   color = "white",
                   na.rm = TRUE) +
    stat_ecdf(aes(y = after_stat(y) * max_barre),
              color     = "red",
              linewidth = 1,
              na.rm     = TRUE) +
    scale_y_continuous(
      name     = "Effectif (%)",
      sec.axis = sec_axis(~ . / max_barre * 100,
                          name = "Cumul (%)")
    )+
    labs(title = paste0("Distribution cumulative des revenus médians par IRIS - ",
                        nom_ville),
         x     = "Revenu médian (€)",
         y     = "Effectif (%)") +
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5))
  
  norma = ggplot(data_iris, aes(x = cume_revenu, y = cume_age)) +
    geom_point(color = "steelblue", alpha = 0.7) +
    labs(title = paste0("Données normalisées avant rotation - ", nom_ville),
         x     = "Revenu normalisé",
         y     = "Scolarité normalisée") +
    xlim(0, 1) + ylim(0, 1) +
    stat_smooth(method = "lm", alpha = 0.1, color = "black")+
    theme(aspect.ratio=1)+
    coord_equal()+
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5))
  
# ===== Application d'une rotation géométrique =================================
  
  angle = pi / 4
  data_iris = data_iris %>%
    mutate(
      x_rot = cume_age * cos(angle) - cume_revenu * sin(angle),
      y_rot = cume_age * sin(angle) + cume_revenu * cos(angle)
    )
  
  data_iris = data_iris %>%
    mutate(
      x_final = cume_dist(x_rot),
      y_final = cume_dist(y_rot),
    )
  
# ===== calculs d'indicateurs par typologie communale ==========================
  # Avec l'appui de l'IA pour la vérification des indicateurs
  
  indicateurs_typo = data_iris %>%
    group_by(typologie) %>%
    summarise(
      metropole    = nom_ville,
      nb_iris      = n(),
      bary_x       = mean(x_final, na.rm = TRUE),
      bary_y       = mean(y_final, na.rm = TRUE),
      var_x        = var(x_final, na.rm = TRUE),
      var_y        = var(y_final, na.rm = TRUE),
      cov_xy       = cov(x_final, y_final, use = "complete.obs"),
      lambda1      = ((var_x + var_y) / 2) +
                     sqrt(((var_x - var_y) / 2)^2 + cov_xy^2),
      lambda2      = ((var_x + var_y) / 2) -
                     sqrt(((var_x - var_y) / 2)^2 + cov_xy^2),
      surface      = pi * sqrt(lambda1 * qchisq(0.95, 2)) *
                     sqrt(lambda2 * qchisq(0.95, 2)),
      excentricite = sqrt(1 - (lambda2 / lambda1))
    ) %>%
    ungroup()
  
  barycentres_typo = indicateurs_typo %>%
    select(typologie, bary_x, bary_y)
  
  # avec l'appui de l'IA pour la variable "ligne_metropole"
  
  ligne_metropole = indicateurs_typo %>%
    select(typologie, surface) %>%
    pivot_wider(names_from = typologie, values_from = surface) %>%
    mutate(metropole = nom_ville) %>%
    relocate(metropole)
  
  # boucle de sécurité "for" établi par l'intelligence artificielle
  
  for (col in c("Commune-centre", "Banlieue", "Périurbain")) {
    if (!col %in% names(ligne_metropole)) {
      ligne_metropole[[col]] = NA_real_
    }
  }

# ===== Calcul de la moyenne des indicateurs de surface ========================
  
  ligne_metropole = ligne_metropole %>%
    mutate(
      EC_moyen = rowMeans(across(c("Commune-centre", "Banlieue", "Périurbain")),
                          na.rm = TRUE)
    )%>%
    rename(
      Centre = `Commune-centre`,
      Banlieue = Banlieue,
      Periurbain = Périurbain
    )
  
  capita = ggplot(data_iris, aes(x = x_final, y = y_final, color = typologie)) +
    geom_point(alpha = 0.7) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey") +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey") +
    annotate("text", x = 0.5, y = 1, label = "Volume conséquent",
             color = "grey30", size = 3, fontface = "italic") +
    annotate("text", x = 0.5, y = 0, label = "Volume faible",
             color = "grey30", size = 3, fontface = "italic") +
    annotate("text", x = 0.9, y = 0.5, label = "Capital économique +",
             color = "grey30", size = 3, fontface = "italic") +
    annotate("text", x = 0.1, y = 0.5, label = "Capital culturel +",
             color = "grey30", size = 3, fontface = "italic") +
    scale_color_brewer(palette = "Set1") +
    coord_fixed(ratio = 1) +
    labs(title = paste0("Distances sociales après rotation - ", nom_ville),
         x     = "Structure du capital",
         y     = "Volume de capital",
         color = "Typologie urbaine") +
    theme_minimal() +
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  
# ===== graphique final - structure du capital et ellipses par typologie =======
  
  ellipse = ggplot(data_iris, aes(x = x_final, y = y_final, color = typologie)) +
    geom_point(alpha = 0.7) +
    stat_ellipse(level = 0.95) +
    geom_point(data = barycentres_typo,
               aes(x = bary_x, y = bary_y, color = typologie),
               shape = 3, size = 5)+
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey") +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey") +
    annotate("text", x = 0.5, y = 1, label = "Volume conséquent",
             color = "grey30", size = 3, fontface = "italic") +
    annotate("text", x = 0.5, y = 0, label = "Volume faible",
             color = "grey30", size = 3, fontface = "italic") +
    annotate("text", x = 0.9, y = 0.5, label = "Capital économique +",
             color = "grey30", size = 3, fontface = "italic") +
    annotate("text", x = 0.1, y = 0.5, label = "Capital culturel +",
             color = "grey30", size = 3, fontface = "italic") +
    scale_color_brewer(palette = "Set1") +
    coord_fixed(ratio = 1) +
    labs(title = paste0("Ellipses de dispersion par typologie - ", nom_ville),
         x     = "Structure du capital",
         y     = "Volume de capital",
         color = "Typologie urbaine") +
    theme_minimal() +
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  
# ===== creation d'un dossier par ville et intégration des plots et fichier ====
# des indicateurs par typologie
  
  dir.create(paste0("outputs/", nom_ville), recursive = TRUE,
             showWarnings = FALSE)
  write.csv(indicateurs_typo,
            file = paste0("outputs/", nom_ville, "/indicateurs par typologie.csv"),
            row.names = FALSE)
  
  ggsave(paste0("outputs/", nom_ville, "/donnees_brutes.png"),
         plot = brute,
         width  = 10,
         height = 6,
         dpi    = 300)
  
  ggsave(paste0("outputs/", nom_ville, "/distribution_cumulative.png"),
         plot = distri_cumul,
         width  = 10,
         height = 6,
         dpi    = 300)
  
  ggsave(paste0("outputs/", nom_ville, "/donnees_normalises.png"),
         plot = norma,
         width  = 8,
         height = 8,
         dpi    = 300)
  
  ggsave(paste0("outputs/", nom_ville, "/structure_capital.png"),
         plot = capita,
         width  = 8,
         height = 8,
         dpi    = 300)
  
  ggsave(paste0("outputs/", nom_ville, "/ellipses_par_typologie.png"),
         plot = ellipse,
         width  = 8,
         height = 8,
         dpi    = 300)
  
  return(list(data = data_iris, ecart_type = ligne_metropole))
}

liste_metropoles = names(epci_metropoles)
data_results = list()
tableau_sd = data.frame()

# ===== Etablissement d'une boucle pour compter toutes les métropoles ==========

for(ville in liste_metropoles) {
  cat("\n --- Analyse pour", ville, "---\n")
  resultat = analyse_metropole(ville)
  data_results[[ville]] = resultat$data
  tableau_sd = bind_rows(tableau_sd, resultat$ecart_type)
}

# ===== Appeler le résultat des indicateurs typo sur une ligne par métropole ===

moyenne = tableau_sd %>%
  summarise(
      metropole = "Moyenne EC",
      EC_Centre = mean(EC_Centre, na.rm = TRUE),
      EC_Banlieue = mean(EC_Banlieue, na.rm = TRUE),
      EC_Periurbain = mean(EC_Periurbain, na.rm = TRUE),
      EC_moyen = mean(EC_moyen, na.rm = TRUE)
  )

final_tab = bind_rows(tableau_sd, moyenne)

write_csv(final_tab, "outputs/tableau_ecart_type_metropoles.csv")
print(final_tab)