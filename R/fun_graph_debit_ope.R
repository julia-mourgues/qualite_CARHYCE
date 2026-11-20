#'Créer un graphique permettant de visualiser le débit du cours d'eau par opération
#'
graph_debit_ope<- function(data) {
  
  #objet contenant le nombre de points de prélèvement
  n_PP <- dplyr::n_distinct(data$code_pt_prelevement)
  
  #objet contenant la hauteur de l'axe y
  max_y <- base::max(data$OPE_DEBIT_MESURE, na.rm = TRUE)+2
  
  data <- data %>% 
    arrange(OPE_DATE_REALISATION) %>% 
    mutate(OPE_DATE_REALISATION= factor(OPE_DATE_REALISATION),
           levels = base::unique(OPE_DATE_REALISATION))
  
  #---Graphique
  p <- ggplot2::ggplot(data ) +
    
    #points interactifs correspondants au débit de la station
    ggiraph::geom_point_interactive(
      ggplot2::aes(
        x = OPE_DATE_REALISATION,
        y = OPE_DEBIT_MESURE,
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
          "valeur_debit: ",
          OPE_DEBIT_MESURE,
          "<br>",
          "protocole: ",
          OPE_VERSION_PROTOCOLE_LIBELLE,
          "<br>")),
      size = 6,
      shape = 21,
      fill="steelblue"
    )+
    
    
    #mise en forme du graphique
    ggplot2::scale_y_continuous(limits = c(0, max_y)) +
    ggplot2::theme_bw() +
    ggplot2::labs(
       title = "Comparaison des débits mesurés entre opérations ", 
       x="Date opération",
       y = "Débit (m3/s)")+
    ggplot2::theme(axis.title = ggplot2::element_text(size=16, face="bold"),
          axis.text= ggplot2::element_text(size=14),
          plot.title = ggplot2::element_text(size = 16, face = "bold"),
          legend.title = ggplot2::element_text(size = 15, face = "bold"),
          strip.text = ggplot2::element_text(size = 14, face = "bold"),
          legend.text = ggplot2::element_text(size=14),
          legend.position = "bottom"
    )
  
  
  # #mise en forme conditionnelle s'il manque des valeurs de débit
  if (base::any(base::is.na(data$OPE_DEBIT_MESURE))){
    
    #identifier l'opération pour laquelle il manque la donnée
    ope_na <- data %>%
      dplyr::filter(base::is.na(OPE_DEBIT_MESURE)) %>%
      dplyr::mutate(
        pos = base::match(OPE_ID, 
                          base::levels(
                            base::factor(data$OPE_ID))),
        xmin = pos - 0.2,
        xmax = pos + 0.2
      )%>%
      dplyr::distinct(OPE_ID, code_pt_prelevement, OPE_DATE_REALISATION,xmin, xmax)
    
    #afficher un message pour prévenir et donner l'identifiant de l'opération
    base::cat(
      base::paste0(
        "<div class='alerte-rouge'>",
        "⚠ Valeur débit manquante pour l'opération : ",
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
                  height_svg = 6)
}
