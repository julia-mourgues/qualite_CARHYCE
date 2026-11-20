#' Créer le graphique permettant de visualiser le nombre de transect par opération
graph_n_transect<- function(data){
  
  data_filtre <- data %>% 
    dplyr::select(
       Code_station_SANDRE, 
       code_pt_prelevement,
       OPE_ID,
       OPE_ID_NUM,
       OPE_DATE_REALISATION,
       OPE_VERSION_PROTOCOLE_LIBELLE,
       TRA_ID,
       TRA_NUM )%>% 
    dplyr::distinct() %>% 
    dplyr::group_by(
       Code_station_SANDRE, 
       code_pt_prelevement,
       OPE_ID,
       OPE_ID_NUM,
       OPE_DATE_REALISATION,
       OPE_VERSION_PROTOCOLE_LIBELLE) %>% 
    dplyr::summarise(
      n_tra= dplyr::n_distinct(TRA_ID),
      .groups = "drop") %>% 
    dplyr::arrange(OPE_DATE_REALISATION) %>% 
    dplyr::mutate(
      OPE_DATE_REALISATION = base::factor(
        OPE_DATE_REALISATION,
        levels = base::unique(OPE_DATE_REALISATION)),
      code_pt_prelevement = base::factor(
        code_pt_prelevement,
        levels = base::unique(code_pt_prelevement)),
      lib_n_tra = case_when(
        n_tra <13 ~ "Nombre de transects insuffisant",
        n_tra <15 ~"Nombre de transects accepté mais évaluation déradée",
        TRUE ~"Nombre de transects OK"
      )
    )
  
  #objet contenant le nombr de points de prélèvement
  n_PP <- dplyr::n_distinct(data_filtre$code_pt_prelevement)
  max_y <- base::max(data_filtre$n_tra, na.rm = TRUE) +2
  
  #---Graphique
  p <- ggplot2::ggplot(data_filtre ) +
    
    #points interactifs correspondants aux nombres de transects
    ggiraph::geom_col_interactive(
      ggplot2::aes(
        x = OPE_DATE_REALISATION,
        y = n_tra,
        fill = lib_n_tra ,
        tooltip = base::paste0(
          "ope_id: ",
          OPE_ID,
          "<br>",
          "code_ppt: ",
          code_pt_prelevement,
          "<br>",
          "ope_date: ",
          OPE_DATE_REALISATION,
          "<br>",
          "nombre de transects: ",
          n_tra,
          "<br>",
          "protocole: ",
          OPE_VERSION_PROTOCOLE_LIBELLE,
          "<br>")),
      color="black",
      width =0.4
    )+
    ggplot2::scale_fill_manual(
      values = c(
        "Nombre de transects OK" = "steelblue",
        "Nombre de transects accepté mais évaluation déradée" = "orange",
        "Nombre de transects insuffisant" = "red"))+
    
    #mise en forme du graphique
    ggplot2::scale_y_continuous(limits = c(0, max_y)) +
    ggplot2::theme_bw() +
    ggplot2::labs(
         title = "Comparaison du nombre de transects entre opérations ", 
         x="Date opération",
         y = "Nombre de transects",
         fill="Légende")+
    ggplot2::theme(
      axis.title = ggplot2::element_text(size=8, face="bold"),
      axis.text= ggplot2::element_text(size=7),
      plot.title = ggplot2::element_text(size = 9, face = "bold"),
      legend.title = ggplot2::element_text(size = 8, face = "bold"),
      strip.text = ggplot2::element_text(size = 7, face = "bold"),
      legend.text = ggplot2::element_text(size=6.5)
    )
  
  
  #mise en forme conditionnelle s'il manque des valeurs de n_tra
  if (base::any(base::is.na(data_filtre$n_tra))){
    
    #identifier l'opération pour laquelle il manque la donnée
    ope_na <- data_filtre %>%
      dplyr::filter(is.na(n_tra)) %>%
      dplyr::mutate(
        pos = base::match(OPE_ID, 
                          base::levels(
                            base::factor(data_filtre$OPE_ID))),
        xmin = pos - 0.2,
        xmax = pos + 0.2
      )%>%
      dplyr::distinct(OPE_ID, code_pt_prelevement, OPE_DATE_REALISATION,xmin, xmax)
    
    #afficher un message pour prévenir et donner l'identifiant de l'opération
    base::cat(
      base::paste0(
        "<div class='alerte-rouge'>",
        "⚠ Valeur Nombre de transects manquante pour l'opération : ",
        base::paste(ope_na$OPE_ID, collapse = ", "),
        "</div>"
      )
    )
    
    p<- p+
      #ajouter un rectangle hachuré  
      ggpattern::geom_rect_pattern(
        data = ope_na,
        ggplot2::aes(
          xmin = xmin,
          xmax = xmax,
          ymin = -Inf,
          ymax = Inf,
          pattern = "Donnée manquante"
        ),
        inherit.aes = FALSE,
        pattern_fill = "lightcoral",
        pattern_colour = "lightcoral",
        pattern_alpha = 0.8,
        pattern_angle = 45,
        pattern_density = 0.4,
        fill = "white"
      )+
      ggpattern::scale_pattern_manual(
        values=c("Donnée manquante" = "stripe") # hachures
      )+
      #ajout d'un rectangle interactif par dessus
      ggiraph::geom_rect_interactive(
        data = ope_na,
        ggplot2::aes(
          xmin = xmin,
          xmax = xmax,
          ymin = -Inf,
          ymax = Inf,
          tooltip = paste0(
            "ope_id: ",
            OPE_ID,
            "<br>",
            "code_ppt: ",
            code_pt_prelevement,
            "<br>",
            "ope_date: ",
            OPE_DATE_REALISATION,
            "<br>")
        ),
        fill = NA,
        colour = NA
      )+
      ggplot2::labs(pattern = "")
  }
  
  # si il y a plusieurs points de prélèvement, 
  # afficher le graphique avec des facettes par point de prélèvement
  if (n_PP > 1) {
    p <- p +
      ggplot2::facet_wrap(
        ~code_pt_prelevement,
        ncol = 1
      )
  }
  
  ggiraph::girafe(ggobj = p,
                  height_svg = 3.5)
}