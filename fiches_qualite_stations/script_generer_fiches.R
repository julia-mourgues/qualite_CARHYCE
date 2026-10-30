#' Ce script permet de générer les fiches de mise en qualité pour chaque station.
#' Il faut penser à modifier les objets contenant la région, la structure et 
#' les auteurs pour adapter la mise en forme des fiches.
#' Il faut aussi penser à modifier les chemins d'accès des fichiers

library(tidyverse)

# définir les objets
region_etude <- "Normandie"
structure_etude <- "DR Normandie - Service Régional Connaissance"
auteurs_etude <- "MOURGUES Julia, RICHARD Benoit"

# lecture des stations

stations_etude<-readRDS(here::here("Data/clean_data", 
          "codes_stations_suivies.rds"))

# lecture des données
base::load(here::here("Data/clean_data", "CARHYCE_data.rda"))


#Création des dossiers pour stocker les fichiers créés
base::dir.create(
  "R:/Public/04_SRC/CARHYCE/CDD/Mise_en_qualite/Fiches_stations/Fiches_stations_2026",
  recursive = TRUE,
  showWarnings = FALSE
)

base::dir.create(
  "fiches_qualite_stations/figures", 
  showWarnings = FALSE) #permet de stocker les fichiers temporaires


#-------------------------------------------------------------------------------
# Format html
#-------------------------------------------------------------------------------

for (i in base::seq_along(stations_etude)) {
  
  id <- stations_etude[i]
  
  lib<- Table_stations_etude %>% 
    filter(Code_station_SANDRE== id) %>% 
    select(SME_LB_STATION_MESURE_EAUX_SURFACE)%>% 
    distinct()
  
  dept <- Table_stations_etude%>% 
    filter(Code_station_SANDRE== id) %>% 
    select(SME_CD_DEPARTEMENT) %>% 
    distinct()
  
  dossier_dept <- file.path(
    "R:/Public/04_SRC/CARHYCE/CDD/Mise_en_qualite/Fiches_stations/Fiches_stations_2026",
    dept
  )
  
  #ranger les fiches station par département
  dir.create(
    dossier_dept,
    recursive = TRUE,
    showWarnings = FALSE
  )

  base::message(
    "Génération de la fiche ",
    i, "/",
    length(stations_etude),
    " - Station ",
    id,
    " ",
    lib)

  #boucle pour ne pas arrêter de générer l'ensemble des fiches si une erreur 
  #se produit pour une fiche
  
  base::tryCatch(
    
    {
      rmarkdown::render(
        here::here(
          "fiches_qualite_stations",
          "Template_Fiches_qualite.Rmd"
        ),
        output_format = "html_document",
        output_dir =
          dossier_dept,
        output_file =
          base::paste0("Station-", id,"-",lib, ".html"),
        params = list(
          code_station = id,
          region= region_etude,
          structure= structure_etude,
          auteurs= auteurs_etude
        ),
        clean = TRUE,
        quiet = TRUE
      )
      
      base::message("Fiche créée : Fiche-station-", id, ".html")
      
    },
    
    error = function(e) {
      
      base::message(
        "ERREUR pour la station ",
        id,
        " : ",
        e$message
      )
      
    }
    
  )
}


# Supprimer les éventuels fichiers temporaires restants
base::unlink("fiches_qualite_stations/figures", recursive = TRUE)
base::unlink("fiches_qualite_stations/rosm.cache", recursive = TRUE)
base::unlink("fiches_qualite_stations/*.log")
base::unlink("fiches_qualite_stations/*.aux")
base::unlink("fiches_qualite_stations/*.tex")
base::unlink("fiches_qualite_stations/*.out")

