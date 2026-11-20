#' Créer le graphique permettant de visualiser le nombre de points par transect 
#' et par opération

graph_n_points_tra<- function(data){
  
  data_filtre <- data %>% 
    dplyr::select(
       Code_station_SANDRE, 
       code_pt_prelevement,
       OPE_ID,
       OPE_ID_NUM,
       OPE_DATE_REALISATION,
       OPE_VERSION_PROTOCOLE_LIBELLE,
       label,
       OPE_DIP,
       TRA_ID,
       TRA_NUM,
       TRA_LPB,
       POI_ID)%>% 
    dplyr::distinct() %>% 
    dplyr::group_by(
       Code_station_SANDRE, 
       code_pt_prelevement,
       OPE_ID,
       OPE_ID_NUM,
       OPE_DATE_REALISATION,
       OPE_VERSION_PROTOCOLE_LIBELLE,
       label,
       OPE_DIP,
       TRA_ID,
       TRA_NUM,
       TRA_LPB) %>% 
    dplyr::summarise(
      n_points= dplyr::n_distinct(POI_ID),
      .groups = "drop") %>% 
    mutate(n_theorique= round(TRA_LPB/OPE_DIP ),
           n_min= round(n_theorique*0.8),
           n_max= round(n_theorique*1.2),
           TRA_NUM= base::as.numeric(TRA_NUM)) %>% 
    dplyr::arrange(OPE_DATE_REALISATION, TRA_NUM) %>% 
    dplyr::mutate(
      label = base::factor(
        label,
        levels = base::unique(label)))
  
  
  max_y <- base::max(data_filtre$n_points, na.rm = TRUE) +2
  
  #---Graphique
  p <- ggplot2::ggplot(data_filtre ) +
    
    #points interactifs correspondants aux nombres de transects
    ggiraph::geom_point_interactive(
      ggplot2::aes(
        x = TRA_NUM,
        y = n_points,
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
          "numéro de transect: ",
          TRA_NUM,
          "<br>",
          "nombre de points: ",
          n_points,
          "<br>",
          "nombre théorique: ",
          n_theorique,
          "<br>",
          "protocole: ",
          OPE_VERSION_PROTOCOLE_LIBELLE,
          "<br>")),
      shape=21,
      size =2.5,
      fill="steelblue"
    )+
    ggiraph::geom_point_interactive(
      ggplot2::aes(x=TRA_NUM,
           y=n_max,
           color=n_max>=n_points,
          tooltip = base::paste0(
            "numéro de transect: ",
            TRA_NUM,
            "<br>",
            "nombre max de points attendu: ",
            n_max,
            "<br>")),
           shape=6,
           size=1)+
    ggiraph::geom_point_interactive(
       ggplot2::aes(x=TRA_NUM,
           y=n_min,
           color=n_min<=n_points,
           tooltip = base::paste0(
             "numéro de transect: ",
             TRA_NUM,
             "<br>",
             "nombre min de points attendu: ",
             n_min,
             "<br>")),
       shape=2,
       size=1)+
    ggplot2::scale_color_manual(
      values = c(
        "TRUE" = "darkgreen",
        "FALSE" = "red"),
      guide = "none")+
    ggplot2::facet_wrap(~label, 
                        ncol=2)+
    
    #mise en forme du graphique
    ggplot2::scale_y_continuous(limits = c(0, max_y)) +
    ggplot2::scale_x_continuous(
      breaks = seq(
        from = 1,
        to = base::max(data_filtre$TRA_NUM, na.rm = TRUE),
        by = 2))+
    ggplot2::theme_bw() +
    ggplot2::labs(
         title = "Comparaison du nombre de points par transect entre opérations ", 
         x="Numéro de transect",
         y = "Nombre de points")+
    ggplot2::theme(
      axis.title = ggplot2::element_text(size=8, face="bold"),
      axis.text= ggplot2::element_text(size=7),
      plot.title = ggplot2::element_text(size = 9, face = "bold"),
      legend.title = ggplot2::element_text(size = 8, face = "bold"),
      strip.text = ggplot2::element_text(size = 7, face = "bold"),
      legend.text = ggplot2::element_text(size=6.5)
    )


  ggiraph::girafe(ggobj = p,
                  height_svg = 5)
}