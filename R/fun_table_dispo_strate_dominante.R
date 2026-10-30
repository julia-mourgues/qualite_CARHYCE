dispo_SDO <- function(data,
                      id_station){
  
  table_dispo_SDO <- data %>%
    dplyr::filter(Code_station_SANDRE == id_station) %>% 
    dplyr::select(
      OPE_ID,
      OPE_DATE_REALISATION,
      TRA_NUM,
      BER_RIVE,
      BER_PRES_ABSENCE_STRATE_ARBOREE,
      BER_PRES_ABSENCE_STRATE_ARBUSTIVE,
      BER_PRES_ABSENCE_STRATE_HERBACEE,
      SDO_LIBELLE
    ) %>%
    dplyr::distinct() %>% 
    #création des variables contenant la mise en forme
    dplyr::mutate(
      OPE_DATE_REALISATION = base::as.Date(
        OPE_DATE_REALISATION, 
        format = "%d/%m/%Y"),
      TRA_NUM= base::as.numeric(TRA_NUM),
      OPE_ID= base::as.numeric(OPE_ID),
      SDO_LIBELLE = if_else(
        is.na(SDO_LIBELLE) | SDO_LIBELLE=="Non Renseigné" | SDO_LIBELLE=="Pas d'info" ,
        "❌",
        as.character(SDO_LIBELLE)
      ),
      BER_PRES_ABSENCE_STRATE_ARBOREE = case_when(
        BER_PRES_ABSENCE_STRATE_ARBOREE=="true" ~"Arborée présente",
        BER_PRES_ABSENCE_STRATE_ARBOREE=="false" ~"Absence",
        TRUE~BER_PRES_ABSENCE_STRATE_ARBOREE),
      BER_PRES_ABSENCE_STRATE_ARBUSTIVE = case_when(
        BER_PRES_ABSENCE_STRATE_ARBUSTIVE=="true" ~"Arbustive présente",
        BER_PRES_ABSENCE_STRATE_ARBUSTIVE=="false" ~"Absence",
        TRUE~BER_PRES_ABSENCE_STRATE_ARBUSTIVE),
      BER_PRES_ABSENCE_STRATE_HERBACEE = case_when(
        BER_PRES_ABSENCE_STRATE_HERBACEE=="true" ~"Herbacée présente",
        BER_PRES_ABSENCE_STRATE_HERBACEE=="false" ~"Absence",
        TRUE~BER_PRES_ABSENCE_STRATE_HERBACEE)
      ) %>%
    
    #ranger par ordre croissant de date et d'opération
    dplyr::arrange(OPE_DATE_REALISATION,OPE_ID, TRA_NUM) %>%
    mutate(
      groupe = paste(
        format(OPE_DATE_REALISATION, "%d/%m/%Y"),
        "- Opération ",
        OPE_ID
      )
    ) %>% 
    dplyr::select(-OPE_DATE_REALISATION, -OPE_ID) %>% 
    
    #Mise en forme avec gt()
    gt::gt(
      groupname_col = "groupe"
    ) %>% 
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_row_groups()
    ) %>% 
    gt::tab_options(
      table.font.size = 12,            # taille de police
      table.border.top.color = "black",
      table.border.bottom.color = "black",
      table.border.top.width = px(2),
      table.border.bottom.width = px(2)
    ) %>% 
    gt::cols_label(
      BER_RIVE = "Rive",
      BER_PRES_ABSENCE_STRATE_ARBOREE = "Strate arborée",
      BER_PRES_ABSENCE_STRATE_ARBUSTIVE = "Strate arbustive",
      BER_PRES_ABSENCE_STRATE_HERBACEE = "Strate herbacée",
      SDO_LIBELLE= "Strate Dominante"
    ) %>%
    gt::tab_style(
      style = cell_text(
        color = "#44a611",
        weight = "bold"
      ),
      locations = cells_body(
        columns = BER_PRES_ABSENCE_STRATE_ARBOREE,
        rows = BER_PRES_ABSENCE_STRATE_ARBOREE == "Arborée présente"
      )
    ) %>% 
    gt::tab_style(
      style = cell_text(
        color = "#44a611",
        weight = "bold"
      ),
      locations = cells_body(
        columns = BER_PRES_ABSENCE_STRATE_ARBUSTIVE,
        rows = BER_PRES_ABSENCE_STRATE_ARBUSTIVE == "Arbustive présente"
      )
    ) %>% 
    gt::tab_style(
      style = cell_text(
        color = "#44a611",
        weight = "bold"
      ),
      locations = cells_body(
        columns = BER_PRES_ABSENCE_STRATE_HERBACEE,
        rows = BER_PRES_ABSENCE_STRATE_HERBACEE == "Herbacée présente"
      )
    ) %>% 
    gt::cols_align(align = "center") %>%
    gt::tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    gt::tab_header(
      title = "Présence des différents types de strate et strate dominante par transect"
    ) %>%
    gt::tab_source_note(
      source_note = md("❌ **Donnée indisponible**")
    ) %>%
    gt::tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_title(groups = "title")
    )
  
  #retourner le tableau mis en forme
  return(table_dispo_SDO)
}