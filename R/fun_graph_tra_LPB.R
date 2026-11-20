#' Créer le graphique présentant le largeurs plein bord et mouillées de chaque transect
#'
graph_LPB_LM_tra <- function(data){
  
  data_filtre<-data %>% 
    dplyr::select(
       -POI_ID,
       -POI_NUM,
       -POI_PROFONDEUR) %>% 
    dplyr:: distinct() %>% 
    dplyr::mutate(
      TRA_NUM= base::as.numeric(TRA_NUM)) %>% 
    dplyr::arrange(OPE_DATE_REALISATION, TRA_NUM) %>% 
    dplyr::mutate(
      label = base::factor(
        label,
        levels = base::unique(label)))
  
  #définir la hauteur de l'axe y
  min_y <- base::min(data_filtre$TRA_LPB, 
                     data_filtre$TRA_L_MOUILLEE, 
                     na.rm = TRUE) - 3
  max_y <- base::max(data_filtre$TRA_LPB, 
                     data_filtre$TRA_L_MOUILLEE, 
                     na.rm = TRUE) + 2
  
  #---Vérification des dates et valeurs manquantes
  
  #identifier les opérations qui n'ont pas de valeur de largeur PB
  tra_sans_LPB<- data_filtre %>%
    dplyr::filter(base::is.na(TRA_LPB)) %>%
    dplyr::distinct(OPE_ID,TRA_NUM)
  
  #si présence d'opération sans largeur PB
  if (nrow(tra_sans_LPB) > 0) {
    cat(
      paste0(
        "<div class='alerte-rouge'>",
        "⚠ Valeur Largeur Plein Bord manquante pour : ",
        paste("Opération ",
              tra_sans_LPB$OPE_ID, 
              " - Transect ", 
              tra_sans_LPB$TRA_NUM, 
              collapse = ", "),
        "</div>"
      )
    )
  }
  
  #identifier les opérations qui n'ont pas de valeur de largeur mouillée
  tra_sans_LM <- data_filtre %>%
    dplyr::filter(base::is.na(TRA_L_MOUILLEE)) %>%
    dplyr::distinct(OPE_ID,TRA_NUM)
  
  #si présence d'opération sans largeur mouillée
  if (nrow(tra_sans_LM) > 0) {
    cat(
      paste0(
        "<div class='alerte-rouge'>",
        "⚠ Valeur Largeur mouillée manquante pour : ",
        paste("Opération ",tra_sans_LM$OPE_ID, " - Transect ", tra_sans_LM$TRA_NUM, collapse = ", "),
        "</div>"
      )
    )
  }
  
  #---Graphique
  p <- ggplot2::ggplot(data_filtre) +
    
    #points interactifs correspondants aux largeurs plein bord
    ggiraph::geom_point_interactive(
      aes(
        x = TRA_NUM,
        y = TRA_LPB,
        fill = "Largeur Plein Bord",
        tooltip = paste0(
          "ope_id: ", OPE_ID,
          "<br>code_ppt: ",code_pt_prelevement,
          "<br>ope_date: ", OPE_DATE_REALISATION,
          "<br>valeur_lpb: ", TRA_LPB
        )
      ),
      size = 4,
      shape = 22
    ) +
    # Points interactifs correspondants aux largeurs mouillées
    ggiraph::geom_point_interactive(
      aes(
        x = TRA_NUM,
        y = TRA_L_MOUILLEE,
        fill = "Largeur Mouillée",
        tooltip = paste0(
          "ope_id: ", OPE_ID,
          "<br>code_ppt: ",code_pt_prelevement,
          "<br>ope_date: ", OPE_DATE_REALISATION,
          "<br>valeur_lm: ", TRA_L_MOUILLEE
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
    ggplot2::facet_wrap(~label, 
                        ncol=2)+
    
    #mise en forme du graphique
    scale_x_continuous(
      breaks = seq(
        from = 1,
        to = max(data_filtre$TRA_NUM, na.rm = TRUE),
        by = 2
      )
    )+
    ggplot2::scale_y_continuous(limits = c(min_y, max_y)) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = "Comparaison des largeurs plein bord et largeurs mouillées par 
      transect et <br> par opération",
      x = "Numéro de transect",
      y = "Largeur (m)",
      fill = "Type de largeur"
    ) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 11, face = "bold"),
      plot.title = ggtext::element_markdown(size = 12, face = "bold"),
      legend.title = ggplot2::element_text(size = 11, face = "bold"),
      legend.position = "bottom",
      legend.title.position = "top",
      strip.text = ggplot2::element_text(size = 8, face = "bold"),
    )
  
  #Détecter les cas où largeur PB < Largeur mouillée et les faire ressortir graphiquement
  tra_incoherents <- data_filtre %>%
    dplyr::filter(
      !is.na(TRA_LPB),
      !is.na(TRA_L_MOUILLEE),
      TRA_LPB < TRA_L_MOUILLEE) %>%
    dplyr::mutate(
      xmin = TRA_NUM - 0.5,
      xmax = TRA_NUM + 0.5,
      ymin = TRA_LPB - 2,
      ymax= TRA_L_MOUILLEE + 3)
  
  #si il y a des largeurs plein bord < largeur mouillée, on encadre les points 
  #concernés en rouge
  if (nrow(tra_incoherents) > 0) {
    
    p <- p +
      ggplot2::geom_rect(
        data = tra_incoherents,
        aes(
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
  #afficher le graphique interactif
  ggiraph::girafe(ggobj = p,
                  height_svg = 5.5)
}