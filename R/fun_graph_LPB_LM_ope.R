#Création de la fonction permettant de visualiser les largeurs plein bord et 
#mouillée pour chaque opération, en distingant les points de prélèvement

graph_LPB_LM_ope <- function (data){
  
  #objet contenant le nombre de points de prélèvement 
  n_PP <- dplyr::n_distinct(data$code_pt_prelevement)
  
  #définir la hauteur de l'axe y
  max_y <- base::max(data$OPE_LPBEV, data$OPE_LMEV, na.rm = TRUE) +03
  
  #---Vérification des dates et valeurs manquantes
  
  #identifier les opérations qui n'ont pas de valeur de largeur PB
  ope_sans_LPBEV <- data %>%
    dplyr::filter(base::is.na(OPE_LPBEV)) %>%
    dplyr::distinct(OPE_ID)
  
  #si présence d'opération sans largeur PB
  if (nrow(ope_sans_LPBEV) > 0) {
    base::cat(
      base::paste0(
        "<div class='alerte-rouge'>",
        "⚠ Valeur Largeur Plein Bord manquante pour l'opération : ",
        base::paste(ope_sans_LPBEV$OPE_ID, collapse = ", "),
        "</div>"
      )
    )
  }
  
  #identifier les opérations qui n'ont pas de valeur de largeur mouillée
  ope_sans_LMEV <- data %>%
    dplyr::filter(base::is.na(OPE_LMEV)) %>%
    dplyr::distinct(OPE_ID)
  
  #si présence d'opération sans largeur mouillée
  if (nrow(ope_sans_LMEV) > 0) {
    base::cat(
      base::paste0(
        "<div class='alerte-rouge'>",
        "⚠ Valeur Largeur mouillée manquante pour l'opération : ",
        base::paste(ope_sans_LMEV$OPE_ID, collapse = ", "),
        "</div>"
      )
    )
  }
  
  data <- data %>% 
    arrange(OPE_DATE_REALISATION) %>% 
    mutate(
      OPE_ID = factor(
        OPE_ID,
        levels = unique(OPE_ID)
      )
    )
  
  #---Graphique
  p <- ggplot2::ggplot(data) +
    
    #points interactifs correspondants aux largeurs plein bord
    ggiraph::geom_point_interactive(
      ggplot2::aes(
        x = OPE_ID,
        y = OPE_LPBEV,
        fill = "Largeur Plein Bord",
        tooltip = base::paste0(
          "ope_id: ", OPE_ID,
          "<br>code_ppt: ", code_pt_prelevement,
          "<br>ope_date: ", OPE_DATE_REALISATION,
          "<br>valeur_lpb: ", OPE_LPBEV,
          "<br>protocole: ", OPE_VERSION_PROTOCOLE_LIBELLE
        )
      ),
      size = 4,
      shape = 22
    ) +
    # Points interactifs correspondants aux largeurs mouillées
    ggiraph::geom_point_interactive(
      ggplot2::aes(
        x = OPE_ID,
        y = OPE_LMEV,
        fill = "Largeur Mouillée",
        tooltip = base::paste0(
          "ope_id: ", OPE_ID,
          "<br>code_ppt: ", code_pt_prelevement,
          "<br>ope_date: ", OPE_DATE_REALISATION,
          "<br>valeur_lm: ", OPE_LMEV,
          "<br>protocole: ", OPE_VERSION_PROTOCOLE_LIBELLE
        )
      ),
      size = 4,
      shape = 21
    ) +
    
    #Définir les couleurs associées aux deux types de largeurs
    ggplot2::scale_fill_manual(
      values = c(
        "Largeur Plein Bord" = "#5773CCFF",
        "Largeur Mouillée" = "#FFB900FF"
      )
    ) +
    
    #mise en forme du graphique
    ggplot2::scale_y_continuous(limits = c(0, max_y)) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = "Comparaison des largeurs plein bord et largeurs mouillées par opération",
      x = "Numéro d'opération",
      y = "Largeur (m)",
      fill = "Type de largeur"
    ) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 8, face = "bold"),
      plot.title = ggplot2::element_text(size = 10, face = "bold"),
      legend.title = ggplot2::element_text(size = 8, face = "bold"),
      axis.text= ggplot2::element_text(size=7),
      strip.text = ggplot2::element_text(size = 7, face = "bold"),
      legend.text = ggplot2::element_text(size=6.5)
    )
  
  #Détecter les cas où largeur PB < Largeur mouillée et les faire ressortir graphiquement
  ope_incoherents <- data %>%
    dplyr::filter(
      !is.na(OPE_LPBEV),
      !is.na(OPE_LMEV),
      OPE_LPBEV < OPE_LMEV) %>%
    dplyr::mutate(
      xmin = OPE_ID - 1,
      xmax = OPE_ID + 1,
      ymin = OPE_LPBEV - 2,
      ymax= OPE_LMEV + 3)
  
  #si il y a des largeurs plein bord < largeur mouillée, on encadre les points 
  #concernés en rouge
  if (nrow(ope_incoherents) > 0) {
    
    p <- p +
      ggplot2::geom_rect(
        data = ope_incoherents,
        ggplot2::aes(
          xmin = xmin,
          xmax = xmax,
          ymin = ymin,
          ymax = ymax
        ),
        inherit.aes = FALSE,
        fill = NA,
        alpha = 0.1,
        colour = "red",
        linewidth = 0.5
      )
  }
  
  # si il y a plusieurs points de prélèvement, 
  # afficher le graphique avec des facettes par point de prélèvement
  if (n_PP > 1) {
    p <- p +
      ggplot2::facet_wrap(~code_pt_prelevement, 
                          ncol = 1)
  }
  
  ggiraph::girafe(ggobj = p,
                  height_svg = 3.5)
}
