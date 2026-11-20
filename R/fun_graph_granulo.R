#'Créer un graphique permettant de visualiser la répartition des valeurs de 
#'granulométrie par opération

graph_granulo <- function(data){
  
  data <- data %>%
    arrange(OPE_DATE_REALISATION) %>% 
    mutate(
      label_ope = factor(
        label_ope,
        levels = unique(label_ope)
      )
    )
  p <- ggplot2::ggplot(
    dplyr::filter(data, MGR_MESURES > -1)) +
    ggiraph::geom_histogram_interactive(
      ggplot2::aes(
        x = MGR_MESURES, 
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
          "valeur_granulo: ",
          MGR_MESURES,
          "<br>",
          "protocole: ",
          OPE_VERSION_PROTOCOLE_LIBELLE,
          "<br>")),
      fill = "steelblue",
      colour = "black"
    )+
    ggplot2::facet_wrap(~label_ope, 
               ncol=2,
               scales = "free",
               drop = FALSE)+
    ggplot2::theme_bw()+
    ggplot2::labs(
       title="Distribution des mesures de granulométrie (Wolman) par opération",
       x= "Granulométrie",
       y= "Nombre de valeurs")+
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 10, face = "bold"),
      plot.title = ggplot2::element_text(size = 12, face = "bold"),
      legend.title = ggplot2::element_text(size = 10, face = "bold"),
      axis.text= ggplot2::element_text(size=9),
      strip.text = ggplot2::element_text(size = 9, face = "bold"),
      legend.text = ggplot2::element_text(size=6.5))
  
  if(base::any(data$MGR_MESURES == -999, na.rm=TRUE)|
     base::any(base::is.na(data$MGR_MESURES))) {
    
    annots <- data %>%
      dplyr::group_by( label_ope) %>%
      dplyr::summarise(
        n_valides = sum(MGR_MESURES > -1, na.rm = TRUE),
        n_999 = base::sum(MGR_MESURES == -999, na.rm = TRUE),
        n_NA = base::sum(is.na(MGR_MESURES)),
        .groups = "drop"
      ) %>%
      dplyr::filter(n_valides > 0,,
                    n_999 > 0 | n_NA > 0) %>%
      dplyr::mutate(
        x = Inf,
        y = Inf,
        label = base::paste0(
          base::ifelse(n_999 > 0,
                 base::paste0("⚠ ", n_999, " valeur(s) = -999"),
                 ""),
          base::ifelse(n_999 > 0 & n_NA > 0,
                 "\n",
                 ""),
          base::ifelse(n_NA > 0,
                 base::paste0("⚠ ", n_NA, " valeur(s) NA"),
                 ""))
      )
    
    
    p <- p +
      ggplot2::geom_label(
        data = annots,
        ggplot2::aes(
          x = x,
          y = y,
          label = label
        ),
        fill = "#FDECEC",
        colour = "red",
        inherit.aes = FALSE,
        hjust = 1.1,
        vjust = 1.1,
        size=3
      )
  }
  
  ope_sans_granulo <- data %>%
    dplyr::group_by(label_ope) %>%
    dplyr::summarise(
      toutes_NA = all(is.na(MGR_MESURES)),
      .groups = "drop"
    ) %>%
    dplyr::filter(toutes_NA)
  
  if (nrow(ope_sans_granulo) > 0) {
    
    base::cat(
      base::paste0(
        "<div class='alerte-rouge'>",
        "⚠ Aucune valeur de granulométrie pour : ",
        base::paste(ope_sans_granulo$label_ope, collapse = ", "),
        "</div>"
      )
    )
  }
  
  ggiraph::girafe(ggobj=p,
                  height_svg = 5)
}