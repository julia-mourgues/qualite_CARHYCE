#'Créer un graphique permettant de visualiser la profondeur des points 
#'de chaque transect et par opération.
#'Si la profondeur est = 0 alors le point apparaît en rouge car cela traduit 
#'une erreur de protocole.

graph_prof_point<- function(data){
  
  data_filtre <- data %>% 
    dplyr::mutate(
      TRA_NUM= base::as.numeric(TRA_NUM),
      TRA_HPB_cm = (TRA_HPB) *100,
      TRA_HPB_cm_inv = (-TRA_HPB_cm),
      TRA_HPB_cm_inv = ifelse(TRA_HPB_cm_inv == -99900, NA, TRA_HPB_cm_inv)) %>% 
    dplyr::arrange(OPE_ID_NUM, TRA_NUM) %>% 
    dplyr::mutate(
      label = base::factor(
        label,
        levels = base::unique(label)),
      #création d'une légende pour identifier si la profondeur du point 1 est égale à -HPB
      legende= case_when(
      POI_NUM=="1"& POI_PROFONDEUR == 0 ~ "Profondeur 0",
      POI_NUM=="1"& POI_PROFONDEUR ==TRA_HPB_cm_inv ~"Profondeur = -HPB",
      TRUE~ "OK"))
  
  #--- Graphique
  p <- ggplot2::ggplot(data_filtre) +
    
    #ligne correspondant à la hauteur plein bord du transect
    ggiraph::geom_line_interactive(
      ggplot2::aes(x=POI_NUM, 
                  y=TRA_HPB_cm_inv,
                  tooltip = paste0(
                    "ope_id: ",
                    OPE_ID,
                    "<br>",
                    "code_ppt: ",
                    code_pt_prelevement,
                    "<br>",
                    "HPB_transect: ",
                    TRA_HPB_cm,
                    " cm <br>")),
              color="darkgreen",
              linewidth=1.2)+
    
    #ligne en pointillets pour relier les points
    ggplot2::geom_line( ggplot2::aes(
      x = POI_NUM,
      y = 0),
      color="blue",
      linetype="longdash")+
    
    ggplot2::geom_line( ggplot2::aes(
      x = POI_NUM,
      y = POI_PROFONDEUR),
      linetype="dashed")+
    
    #Points interactifs correspondant aux valeurs de DIP
    ggiraph::geom_point_interactive(
      ggplot2::aes(
        x = POI_NUM,
        y = POI_PROFONDEUR,
        fill = legende,
        shape=legende,
        tooltip = paste0(
          "ope_id: ",
          OPE_ID,
          "<br>",
          "code_ppt: ",
          code_pt_prelevement,
          "<br>",
          "ope_date: ",
          OPE_DATE_REALISATION,
          "<br>",
          "num_transect: ",
          TRA_NUM,
          "<br>",
          "num_point: ",
          POI_NUM,
          "<br>",
          "profondeur: ",
          POI_PROFONDEUR,
          " cm <br>",
          "Substrat: ",
          SUB_DEFINITION,
          "<br>",
          "protocole: ",
          OPE_VERSION_PROTOCOLE_LIBELLE,
          "<br>")
      ),
      size = 2.5
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        "Profondeur 0" = 21,
        "Profondeur = -HPB"=24,
        "OK" = 21
      )
    )+
    ggplot2::scale_fill_manual(
      values = c(
        "Profondeur 0" = "red",
        "Profondeur = -HPB"="orange",
        "OK" = "steelblue"
      )
    )+
    ggplot2::scale_x_continuous(
      breaks = seq(
        from = 1,
        to = max(data_filtre$POI_NUM, na.rm = TRUE),
        by = 2
      )
    )+
    ggplot2::scale_y_reverse(
      expand = expansion(add = 30))+

    ggplot2::facet_grid(TRA_NUM~label, scales="free")+

    
    #Mise en forme du graphique
    ggplot2::theme_bw() +


    ggplot2::labs(
      title = "Profondeur des points de chaque transect, par opération", 
      x="Numéro de point",
      y = "Profondeur du point (cm)",
      fill= "Légende",
      shape="Légende")+
    ggplot2::theme(
      axis.title = ggplot2::element_text(size=11, face="bold"),
      plot.title = ggplot2::element_text(size = 12, face = "bold"),
      legend.title = ggplot2::element_text(size=11, face="bold"),
      strip.text = ggplot2::element_text(size = 8, face = "bold"),
      legend.position = "bottom")

  ggiraph::girafe(ggobj = p,
                  height_svg = 15)
}