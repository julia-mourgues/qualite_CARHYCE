#'Créer un graphique permettant de visualiser les données de colmatage par opération.
#'La présence de NA est matérialisée par une zone hachurée. 
#'La présence de valeur = -999 est matérialisée par un triangle rouge.
#'
graph_colmatage_ope <- function(data) {
  
  data <- data %>%
    dplyr::arrange(OPE_DATE_REALISATION) %>% 
    dplyr::mutate(
      label_ope = base::factor(
        label_ope,
        levels = base::unique(label_ope)))
  
  #--- Graphique
  p <- ggplot(data) +
    
    #Points interactifs correspondant aux valeurs de DIP
    ggiraph::geom_point_interactive(
      ggplot2::aes(
        x = MCO_POSITION_LATERALE,
        y = MCO_MESURE,
        tooltip = base::paste0(
          "ope_date: ",
          OPE_DATE_REALISATION,
          "<br>",
          "radier: ",
          MCO_RADIER,
          "<br>",
          "position_laterale: ",
          MCO_POSITION_LATERALE,
          "<br>",
          "valeur_colmatage: ",
          MCO_MESURE,
          "<br>",
          "protocole: ",
          OPE_VERSION_PROTOCOLE_LIBELLE,
          "<br>"),
        fill = MCO_MESURE == -999,
        shape = MCO_MESURE == -999,),
      size = 3
    )+
    ggplot2::scale_fill_manual(
      values = c(
        "TRUE" = "red",
        "FALSE" = "steelblue"),
      guide = "none")+
    ggplot2::scale_shape_manual(
      values = c(
        "TRUE" = 23,
        "FALSE" = 21),
      guide = "none"
    )+
    #utilisation de facet_grid pour avoir une facette par radier et par opération
    ggplot2::facet_grid(label_ope~MCO_RADIER)+
    
    #Mise en forme du graphique
    ggplot2::theme_bw() +
    ggplot2::labs(
        title = "Mesures de colmatage par position latérale, radier et par opération ", 
         x="Position latérale",
         y = "Valeur colmatage (cm)")+
    ggplot2::theme(axis.title = ggplot2::element_text(size=10, face="bold"),
          axis.text= ggplot2::element_text(size=9),
          plot.title = ggplot2::element_text(size = 12, face = "bold"),
          legend.title = ggplot2::element_text(size = 10, face = "bold"),
          strip.text = ggplot2::element_text(size = 8, face = "bold"),
          legend.text = ggplot2::element_text(size=8),
          legend.position = "right"
    )
  

  
  #mise en forme conditionnelle s'il manque des valeurs de colmatage
  if (base::any(base::is.na(data$MCO_MESURE))){
    
    #identifier l'opération pour laquelle il manque la donnée
    mesure_na <- data %>%
      dplyr::filter(is.na(MCO_MESURE)) %>%
      dplyr::mutate(
        pos = base::match(MCO_POSITION_LATERALE, 
                          base::levels(
                            base::factor(data$MCO_POSITION_LATERALE))),
        xmin = pos - 0.2,
        xmax = pos + 0.2
      )%>%
      dplyr::distinct(OPE_ID, 
                      code_pt_prelevement, 
                      OPE_DATE_REALISATION, 
                      label_ope, 
                      MCO_RADIER, 
                      MCO_POSITION_LATERALE,
                      xmin, 
                      xmax)
    
    #si absence de valeur = -999, alors on fixe les limites de l'axe x avec les 
    #min et max de nos valeurs de colmatage
    if(!base::any(data$MCO_MESURE ==-999, na.rm=TRUE)){
      max_y <- base::max(data$MCO_MESURE, na.rm = TRUE) +5
      min_y <- base::min(data$MCO_MESURE, na.rm = TRUE) -5

      p<-p+
        ggplot2:: scale_y_continuous(limits = c(min_y, max_y))
    }

    #si présence de valeur = -999, alors on modifie les limites de l'axe des y
    if(base::any(data$MCO_MESURE ==-999, na.rm=TRUE)){

      max_y <- base::max(data$MCO_MESURE, na.rm = TRUE) +200

      p<-p+
        ggplot2::scale_y_continuous(limits = c(-1100, max_y))
    }
    
    p<- p+
      #ajouter un rectangle hachuré  
      ggpattern::geom_rect_pattern(
        data = mesure_na,
        ggplot2::aes(
          xmin = xmin,
          xmax = xmax,
          ymin = -Inf,
          ymax = Inf,
          pattern = "NA"
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
        values=c("NA" = "stripe")
      )+
      #ajout d'un rectangle interactif par dessus
      ggiraph::geom_rect_interactive(
        data = mesure_na,
        ggplot2::aes(
          xmin = xmin,
          xmax = xmax,
          ymin = -Inf,
          ymax = Inf,
          tooltip = base::paste0(
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
        colour = NA)+
      ggplot2::labs(pattern = "")
  }
  
  
  ggiraph::girafe(ggobj = p)
}
