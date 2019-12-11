#' gofer diagram
#' @rdname gofer
#' @name gofer
#' @param data # include here descriptions of each argument we include in the function
#' @param data_age # dataframe in long format with four columns. 
#' Column 1 (Study) should contain a character vector of study indentification 
#' in the form of 'First author, year'. Eg Conway,2019. Second column (age_type)
#' is a grouping variable with either 'mean' or 'median. Third column 
#' (age_measure) is a grouping variable with either 'average', 'lower' or 'upper'.
#' Fourth column (age) should be a numeric value for age.
#' @param range_title measurement type used for range title
#' @param range_unit unit used for range
#' @param range_results_subtitle measurement type used for results
#' @param colour_study_title fill colour of the study title
#' @param colour_RoB_title fill colour of the risk of bias title
#' @param colour_participants_title fill colour of the participants title
#' @param colour_mean_age colour for mean age
#' @param shape_mean_age middle data point shape for mean age
#' @param colour_median_age colour for median age
#' @param shape_median_age middle data point shape for median age
#' @param colour_age_background background colour for age
#' @param colour_age_gridlines gridline colour for age
#' @param colour_measurements_title fill colour of the measurements title
#' @param colour_sample_size colour of the sample size
#' @param colour_range colour for range
#' @param colour_range_background background colour for range
#' @param colour_range_gridlines gridline colour for range
#' @param colour_measurements colour of the paired measurements
#' @param colour_results_title fill colour of the results title
#' @param colour_results colour of the results
#' @param colour_results_background background colour for results
#' @param colour_results_gridlines gridline colour for results
#' @param age_legend_text_direction either vertical or horizontal
#' @param age_legend_text_size size of text for age legend
#' @param age_legend_position position of legend within plot
#' @param grade_rating GRADE rating for meta-analysis. Either High, Moderate
#' Low, Very Low
#' @param measurements_logscale TRUE or FALSE to use log scale transformation for measurements component
#' 
#' @export
#' 
gofer <- function(data, 
                  data_age,
                  ma_effect, 
                  ma_lower, 
                  ma_upper, 
                  dodge_width = 0.7,
                  range_title = "TEMPERATURE", 
                  range_unit = "°C",
                  range_results_subtitle = "temperature (°C)",
                  colour_study_title = "#0080ff", 
                  colour_RoB_title = "#52ae32",
                  colour_participants_title = "#ee7219",
                  colour_mean_age = "coral", 
                  shape_mean_age = "circle",
                  colour_median_age = "orange", 
                  shape_median_age = "diamond",
                  colour_age_background = "white",
                  colour_age_gridlines = "#FFE5CC",
                  colour_measurements_title = "#930093",
                  colour_sample_size = "#930093", 
                  colour_range = "#990099",
                  colour_range_background = "white",
                  colour_range_gridlines = "#FFCCFF",
                  colour_measurements = "#930093",
                  colour_results_title = "#002a60", 
                  colour_results = "#002a60",
                  colour_results_background = "#BFD5E3",
                  colour_results_gridlines = "white",
                  age_legend_text_direction = "horizontal",
                  age_legend_text_size = 8,
                  age_legend_position = c(0.5, 0.02),
                  measurements_logscale =FALSE,
                  grade_rating
){
  
  study <- {{ data }} %>% 
    dplyr::distinct(Study,.keep_all = TRUE) %>% 
    tidyr::separate(Study, c("Study", "Year"), sep = ", ")  %>% 
    dplyr::mutate(Study = as.factor(Study)) %>% 
    dplyr::mutate(Year = as.numeric(Year)) %>%
    dplyr::arrange(-Year) %>% 
    ggplot2::ggplot()+
    ggfittext::geom_fit_text(ggplot2::aes(x=stats::reorder(Study, Year), y=0, label=Study), 
                             place = "left", reflow=TRUE, 
                             fontface="plain",
                             position = ggplot2::position_dodge(width = dodge_width), show.legend=FALSE)+
    ggplot2::theme_void()+
    ggplot2::coord_flip()
  
  
  results <- {{ data }} %>% 
    tidyr::separate(Study, c("Study", "Year"), sep = ", ")  %>% 
    dplyr::mutate(Study = as.factor(Study)) %>% 
    dplyr::mutate(Year = as.numeric(Year)) %>%
    dplyr::arrange(-Year) %>% 
    ggplot2::ggplot()+
    ggplot2::geom_point(ggplot2::aes(x=stats::reorder(Study, Year), y=bias, alpha=group, size=n), colour = colour_results, position = ggplot2::position_dodge(width = dodge_width), show.legend=FALSE)+
    ggplot2::geom_linerange(ggplot2::aes(x=stats::reorder(stats::reorder(Study, Year), Year), ymin=lower, ymax=upper, alpha=group), colour = colour_results,size=1, position = ggplot2::position_dodge(width = dodge_width), show.legend=FALSE)+
    ggplot2::geom_hline(yintercept = ma_effect, linetype = 2, col = "#002a60") +
    ggplot2::scale_alpha_discrete(range = c(rep(1,4)))+
    ggplot2::coord_flip()+ 
    ggplot2::theme_void()+
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = colour_results_background, colour = "white",
                                               size = 2, linetype = "solid"),
      panel.grid.major = ggplot2::element_line(size = 0.5, linetype = 'solid',
                                               colour = colour_results_gridlines), 
      panel.grid.minor = ggplot2::element_blank()
    )+
    ggplot2::scale_y_continuous(expand=c(0.1,0.1), limits=c(pmin(min({{ data }}$lower), ma_lower), pmax(max({{ data }}$upper), ma_upper)))+
    ggplot2::scale_x_discrete(breaks = NULL) #removes horizontal grid lines

  
  participants <- {{ data }} %>% 
    tidyr::separate(Study, c("Study", "Year"), sep = ", ")  %>% 
    dplyr::mutate(Study = as.factor(Study)) %>% 
    dplyr::mutate(Year = as.numeric(Year)) %>%
    dplyr::arrange(-Year) %>% 
    ggplot2::ggplot(ggplot2::aes(x=stats::reorder(Study, Year), y=n))+
    ggplot2::geom_linerange(ggplot2::aes(x=stats::reorder(Study, Year), ymin=0, ymax=n, alpha=group), 
                            colour = colour_sample_size, 
                            size=0.5, position = ggplot2::position_dodge(width = dodge_width), show.legend=FALSE)+
    ggplot2::geom_label(ggplot2::aes(x=stats::reorder(Study, Year), y=n, label = n, alpha=group), 
                        colour=colour_sample_size,
                        label.r = grid::unit(0.4, "lines"),
                        label.padding = grid::unit(0.2, "lines"),
                        size=3,
                        position = ggplot2::position_dodge(width = dodge_width), show.legend=FALSE)+
    ggplot2::scale_alpha_discrete(range = c(rep(1,4)))+
    ggplot2::theme_void()+
    ggplot2::coord_flip()+
    ggplot2::theme_void()+
    ggplot2::geom_text(ggplot2::aes( x=stats::reorder(Study, Year), y=-5, alpha=group), 
                       label =  "\uf0c0", 
                       family =  "FontAwesome", 
                       size =  3.5,
                       vjust="center", 
                       colour = colour_sample_size,
                       position = ggplot2::position_dodge(width = dodge_width), show.legend=FALSE)+
    ggplot2::scale_y_continuous(expand = ggplot2::expand_scale(mult = c(0.1, .1)))
  
  
  
  if (measurements_logscale == TRUE){
    measurements <-  {{ data }} %>% 
      tidyr::separate(Study, c("Study", "Year"), sep = ", ")  %>% 
      dplyr::mutate(id = dplyr::row_number()) %>% 
      dplyr::mutate(Study = as.factor(Study)) %>% 
      dplyr::mutate(Year = as.numeric(Year)) %>%
      dplyr::arrange(-Year) %>% 
      ggplot2::ggplot(ggplot2::aes(x=stats::reorder(Study, Year), y=n))+
      ggplot2::geom_linerange(ggplot2::aes(x=stats::reorder(Study, Year), ymin=0, ymax=N, alpha=group), 
                              colour = colour_measurements, 
                              size=0.5, position = ggplot2::position_dodge(width = dodge_width), show.legend=FALSE)+
      ggplot2::geom_label(ggplot2::aes(x=stats::reorder(Study, Year), y=N, label = N, alpha=group), 
                          colour=colour_measurements,
                          label.r = grid::unit(0.4, "lines"),
                          label.padding = grid::unit(0.2, "lines"),
                          size=3,
                          position = ggplot2::position_dodge(width = dodge_width), show.legend=FALSE)+
      ggplot2::scale_alpha_discrete(range = c(rep(1,4)))+
      ggplot2::coord_flip()+
      ggplot2::theme_void()+
      ggplot2::scale_y_continuous(trans="log", 
                                  expand = ggplot2::expand_scale(mult = c(0.1, .1)))
  } if (measurements_logscale == FALSE) {
    measurements <- {{ data }} %>% 
      tidyr::separate(Study, c("Study", "Year"), sep = ", ")  %>% 
      dplyr::mutate(id = dplyr::row_number()) %>% 
      dplyr::mutate(Study = as.factor(Study)) %>% 
      dplyr::mutate(Year = as.numeric(Year)) %>%
      dplyr::arrange(-Year) %>% 
      ggplot2::ggplot(ggplot2::aes(x=stats::reorder(Study, Year), y=n))+
      ggplot2::geom_linerange(ggplot2::aes(x=stats::reorder(Study, Year), ymin=0, ymax=N, alpha=group), 
                              colour = colour_measurements, 
                              size=0.5, position = ggplot2::position_dodge(width = dodge_width), show.legend=FALSE)+
      ggplot2::geom_label(ggplot2::aes(x=stats::reorder(Study, Year), y=N, label = N, alpha=group), 
                          colour=colour_measurements,
                          label.r = grid::unit(0.4, "lines"),
                          label.padding = grid::unit(0.2, "lines"),
                          size=3,
                          position = ggplot2::position_dodge(width = dodge_width), show.legend=FALSE)+
      ggplot2::scale_alpha_discrete(range = c(rep(1,4)))+
      ggplot2::coord_flip()+
      ggplot2::theme_void()+
      ggplot2::scale_y_continuous( expand = ggplot2::expand_scale(mult = c(0.1, .1)))
  }
  
  
  comparison <- {{ data }} %>% 
    tidyr::separate(Study, c("Study", "Year"), sep = ", ")  %>% 
    dplyr::mutate(comparison = dplyr::recode(comparison, Eso = "Esophageal")) %>%
    dplyr::mutate(comparison = dplyr::recode(comparison, PA = "Pulmonary artery")) %>% 
    dplyr::mutate(comparison = dplyr::recode(comparison, Ax = "Axillary")) %>% 
    dplyr::mutate(Study = as.factor(Study)) %>% 
    dplyr::mutate(Year = as.numeric(Year)) %>%
    dplyr::arrange(-Year) %>%
    ggplot2::ggplot()+
    ggfittext::geom_fit_text(ggplot2::aes(x=stats::reorder(Study, Year), y=0, label = comparison, alpha=group), place = "right", reflow=TRUE, 
                             position = ggplot2::position_dodge(width = dodge_width), show.legend=FALSE)+
    ggplot2::scale_alpha_discrete(range = c(rep(1,4)))+
    ggplot2::theme_void()+
    ggplot2::coord_flip()
  
  patients <- {{ data }} %>% 
    tidyr::separate(Study, c("Study", "Year"), sep = ", ")  %>% 
    dplyr::mutate(Study = as.factor(Study)) %>% 
    dplyr::mutate(Year = as.numeric(Year)) %>%
    dplyr::distinct(Study,.keep_all = TRUE) %>%
    dplyr::arrange(-Year) %>%
    ggplot2::ggplot(ggplot2::aes(x=stats::reorder(Study, Year), y=0,label=patients))+
    ggfittext::geom_fit_text(ggplot2::aes(x=stats::reorder(Study, Year), y=0), place = "center", reflow=TRUE
    )+
    ggplot2::theme_void()+
    ggplot2::coord_flip()
  
  
  RoB_icon <- {{ data }} %>% 
    tidyr::separate(Study, c("Study", "Year"), sep = ", ")  %>% 
    dplyr::mutate(Study = as.factor(Study)) %>% 
    dplyr::mutate(Year = as.numeric(Year)) %>%
    dplyr::distinct(Study,.keep_all = TRUE) %>% 
    tidyr::pivot_longer(cols = dplyr::starts_with("RoB"), names_to = "RoB_domain", values_to = "RoB_classification") %>% 
    dplyr::select(Study, Year, RoB_domain, RoB_classification) %>% 
    dplyr::mutate(RoB_domain = forcats::fct_recode(RoB_domain, "Zero heat flux" = "RoB_spoton")) %>% 
    dplyr::mutate(RoB_domain = forcats::fct_recode(RoB_domain, "Comparator" = "RoB_comparator")) %>% 
    dplyr::mutate(RoB_domain = forcats::fct_recode(RoB_domain, "Participant flow" = "RoB_flow")) %>% 
    dplyr::mutate(RoB_domain = forcats::fct_recode(RoB_domain, "Participant selection" = "RoB_selection")) %>% 
    dplyr::mutate(RoB_classification = factor(RoB_classification, 
                                              levels = c("high","low","unclear")
    )) %>% 
    dplyr::arrange(-Year) %>%
    ggplot2::ggplot(ggplot2::aes(x=stats::reorder(Study, Year), y=0, fill=RoB_classification, label= RoB_domain, alpha=RoB_domain))+
    ggplot2::geom_label(colour = "white", position = ggplot2::position_dodge(width = dodge_width), 
                        show.legend=FALSE, size=3, label.padding=grid::unit(0.1, "lines"), )+
    ggplot2::theme_void()+
    ggplot2::coord_flip()+
    ggplot2::scale_fill_manual(values=c("high" = "#DC143C","low"= "#32CD32","unclear"= "#ffa500"))+
    ggplot2::scale_alpha_discrete(range = c(rep(1,4)))
  
  
  ma <-  ggplot2::ggplot() +
    ggplot2::geom_polygon(ggplot2::aes(x = c(ma_upper, ma_effect, ma_lower, ma_effect), 
                                       y = c(ma_effect, 1, ma_effect, -1)),
                          colour = "#002a60",
                          fill=NA,
                          size=2)+
    ggplot2::geom_segment(ggplot2::aes(x=ma_effect,xend=ma_effect, y=-1, yend=1),
                          colour = "#002a60",
                          fill=NA,
                          size=2)+
    ggplot2::theme_void()+
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "#BFD5E3", colour = "white",
                                               size = 2, linetype = "solid"),
      panel.grid.major = ggplot2::element_line(size = 0.5, linetype = 'solid',
                                               colour = "white"), 
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x=ggplot2::element_text(size = 8)
    )+
    ggplot2::scale_x_continuous(expand=c(0.1,0.1), limits=c(pmin(min({{ data }}$lower), ma_lower), pmax(max({{ data }}$upper), ma_upper)))+
    ggplot2::scale_y_discrete(breaks = NULL)  #removes horizontal grid lines
  
  ma_grob <- ggplot2::ggplotGrob(ma)
  ma_axis <- ma_grob$grobs[[which(ma_grob$layout$name == "axis-b")]]$children$axis[2] 
  
  ma_grob$grobs[[which(ma_grob$layout$name == "axis-b")]] <- ggplot2::zeroGrob()
  ma_grob$heights[ma_grob$layout$t[which(ma_grob$layout$name == "axis-b")]] <- grid::unit(0, "cm")
  
  
  # Study characteristics data
  # Age  
  age <- {{ data_age }} %>%
    tidyr::separate(Study, c("Study", "Year"), sep = ", ")  %>% 
    dplyr::mutate(Study = as.factor(Study)) %>% 
    dplyr::mutate(Year = as.numeric(Year)) %>%
    tidyr::pivot_wider(names_from = age_measure, values_from = age) %>% 
    dplyr::arrange(-Year) %>%
    ggplot2::ggplot(ggplot2::aes(x = stats::reorder(Study, Year),  
                                 col = age_type)) +
    ggplot2::geom_pointrange(ggplot2::aes(
      y=average, 
      ymin=lower,
      ymax=upper
    )
    )+
    ggplot2::theme_void() +
    ggplot2::coord_flip() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = colour_age_background, colour = "white",
                                               size = 2, linetype = "solid"),
      panel.grid.major = ggplot2::element_line(size = 0.5, linetype = 'solid',
                                               colour = colour_age_gridlines), 
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x=ggplot2::element_text(size=8),
      legend.title = ggplot2::element_blank(),
      legend.direction = age_legend_text_direction,
      legend.position = age_legend_position,
      legend.text = ggplot2::element_text(size=age_legend_text_size)
    )+
    ggplot2::scale_x_discrete(breaks = NULL) +
    ggplot2::scale_colour_manual(values=c("mean_sd" = "#999999", "median_iqr" = "#ffa500" ),
                                 labels=c("mean_sd" ="Mean (SD)","median_iqr" = "Median (IQR)"), na.translate=FALSE)
  
  
  age_grob <- ggplot2::ggplotGrob(age)
  age_axis <- age_grob$grobs[[which(age_grob$layout$name == "axis-b")]]$children$axis[2] 
  
  age_grob$grobs[[which(age_grob$layout$name == "axis-b")]] <- ggplot2::zeroGrob()
  age_grob$heights[age_grob$layout$t[which(age_grob$layout$name == "axis-b")]] <- grid::unit(0, "cm")
  
  
  
  sex <- {{ data }} %>%
    tidyr::separate(Study, c("Study", "Year"), sep = ", ")  %>% 
    dplyr::mutate(Study = as.factor(Study)) %>% 
    dplyr::mutate(Year = as.numeric(Year)) %>%
    dplyr::distinct(Study,.keep_all = TRUE) %>% 
    tidyr::pivot_longer(cols = c("female", "male"), names_to = "sex", values_to = "values") %>% 
    dplyr::arrange(-Year) %>%
    ggplot2::ggplot(ggplot2::aes(x=stats::reorder(Study, Year), y=values, fill = sex)) + 
    ggplot2::geom_col(position = "fill", show.legend = FALSE,
                      width = 0.3) +
    ggplot2::scale_fill_manual(values = c("female" = "#ff8ea2", "male" = "#8edaff"))+
    ggplot2::coord_flip() +
    ggplot2::theme_void()
  
  flags <- {{ data }} %>%
    tidyr::separate(Study, c("Study", "Year"), sep = ", ")  %>% 
    dplyr::mutate(Study = as.factor(Study)) %>% 
    dplyr::mutate(Year = as.numeric(Year)) %>%
    dplyr::distinct(Study,.keep_all = TRUE) %>% 
    dplyr::arrange(-Year) %>%
    ggplot2::ggplot(ggplot2::aes(x=stats::reorder(Study, Year), y=code)) +
    ggimage::geom_flag(y = 0.5, ggplot2::aes(image = code), size = 0.2) +
    ggplot2::coord_flip() +
    ggplot2::theme_void()
  
  
  temp <-  {{ data }} %>%
    tidyr::separate(Study, c("Study", "Year"), sep = ", ")  %>% 
    dplyr::mutate(Study = as.factor(Study)) %>% 
    dplyr::mutate(Year = as.numeric(Year)) %>%
    dplyr::distinct(Study,.keep_all = TRUE) %>% 
    dplyr::arrange(-Year) %>%
    ggplot2::ggplot(ggplot2::aes(x=stats::reorder(Study, Year), ymin = lower_temp, ymax = upper_temp)) +
    ggplot2::geom_errorbar(size = 1, color = colour_range, width = 0.5) +
    
    ggplot2::coord_flip() +
    
    ggplot2::theme_void()+
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = colour_range_background, colour = "white",
                                               size = 2, linetype = "solid"),
      panel.grid.major = ggplot2::element_line(size = 0.5, linetype = 'solid',
                                               colour = colour_range_gridlines), 
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x=ggplot2::element_text(size=8)
    )+
    
    ggplot2::scale_x_discrete(breaks = NULL)  
  
  temp_grob <- ggplot2::ggplotGrob(temp)
  temp_axis <- temp_grob$grobs[[which(temp_grob$layout$name == "axis-b")]]$children$axis[2] 
  
  temp_grob$grobs[[which(temp_grob$layout$name == "axis-b")]] <- ggplot2::zeroGrob()
  temp_grob$heights[temp_grob$layout$t[which(temp_grob$layout$name == "axis-b")]] <- grid::unit(0, "cm")
  
  
  # using gtable seems to be the best option - see here for documentation: https://gtable.r-lib.org/index.html
  RoB_img <- png::readPNG(RCurl::getURLContent("https://raw.githubusercontent.com/awconway/gofer/master/inst/RoB-wide.png"))
  sex_img <- png::readPNG(RCurl::getURLContent("https://raw.githubusercontent.com/awconway/gofer/master/inst/sex.png"))
  
  gt_grid <- gtable::gtable(widths = grid::unit(c(0.05, #1 left space
                                                  0.5,#2 First author
                                                  1,#3 Year
                                                  0.5, #4 Country
                                                  1.2, #5 population
                                                  0.3, #6 space around RoB
                                                  0.8,#7 Rob_text
                                                  0.2,#8 Rob_icon
                                                  0.3, #9 Space around RoB
                                                  1, #10 Age,
                                                  1, #11 Sex
                                                  1.3,#12 sample size
                                                  1.3, #13 comparison
                                                  1,#14 range
                                                  2,#15 Paired measurements
                                                  3,#16 Results
                                                  0.1, #17 right space for meta-analysis
                                                  0.05 #18 Right space
  ), 'null'), 
  heights = grid::unit(c(0.1, #1 top outer space
                         0.5, #2 section headers
                         0.5, #3 Column headers
                         0.15, #4 Axis text
                         rep(1,length(unique({{ data }}$Study))),#5-18 study rows
                         0.5, #19 meta-analysis and other axis text row
                         0.2, #20 meta-analysis axis text row
                         0.1 #21 bottom outer space
  ), 'null'))
  
  # section head
  section_t <- 2
  
  # column head
  column_head_t <- 3
  
  # axis text
  
  axis_t <- 4
  
  ## study section
  study_section_l <- 2
  
  study_section_r <- 5
  
  ## rob section
  
  rob_section_l <- 6
  
  rob_section_r <- 9
  
  ## participants section
  
  participants_section_l <- 10
  
  participants_section_r <- 11
  
  ## temp section
  
  temp_section_l <- 12
  
  temp_section_r <- 15
  
  ## results section
  
  results_section_l <- 17
  
  results_section_r <- 17
  
  column_t <- 5
  
  column_b <- 4+ length(unique({{ data }}$Study))
  
  
  # gtable_show_layout(gt_grid) # use this to see layout
  gt <- gt_grid  %>%
    
    # Study design section header
    
    gtable::gtable_add_grob(ggplot2::ggplotGrob(ggplot2::ggplot(data=NULL,
                                                                ggplot2::aes(x=0,y=0, 
                                                                             label = "\uf05a STUDY"))+
                                                  ggfittext::geom_fit_text(family='fontawesome',
                                                                           colour = "white",
                                                                           fontface="bold",
                                                                           place = "center",
                                                                           grow = FALSE)+
                                                  ggplot2::theme_void()+
                                                  ggplot2::theme(plot.background = ggplot2::element_rect(fill = colour_study_title))
    ), t=section_t,l=study_section_l, r=study_section_r) %>% 
    # First author column
    gtable::gtable_add_grob(ggplot2::ggplotGrob(study), t=column_t,l=3, b=column_b) %>% 
    
    # Country column
  
  gtable::gtable_add_grob(ggplot2::ggplotGrob(flags), t=column_t,l=4, b=column_b) %>%
    
    # Population column
  
  gtable::gtable_add_grob(ggplot2::ggplotGrob(patients), t=column_t, l=5, b=column_b) %>% 
    
    # RoB_text column
  
  gtable::gtable_add_grob(ggplot2::ggplotGrob(RoB_icon), t=column_t, l=7, r=8, b=column_b) %>%
    gtable::gtable_add_grob(grid::rasterGrob(RoB_img), t=column_head_t, b=axis_t, l=rob_section_l, r=rob_section_r) %>% 
    gtable::gtable_add_grob(ggplot2::ggplotGrob(ggplot2::ggplot(data=NULL,
                                                                ggplot2::aes(x=0,y=0, 
                                                                             label = "\uf046 RISK OF BIAS"))+
                                                  ggfittext::geom_fit_text(
                                                    family="fontawesome",
                                                    colour = "white",
                                                    fontface="bold",
                                                    place = "center",
                                                    grow = FALSE)+
                                                  ggplot2::theme_void()+
                                                  ggplot2::theme(plot.background = ggplot2::element_rect(fill = colour_RoB_title))
    ), t=section_t,l=rob_section_l, r=rob_section_r) %>% 
    
    # Age column
    
    gtable::gtable_add_grob(age_grob, t=column_t,l=10, b=column_b) %>%
    gtable::gtable_add_grob(ggplot2::ggplotGrob(ggplot2::ggplot(data=NULL,
                                                                ggplot2::aes(x=0,y=0, 
                                                                             label = "Age"))+
                                                  ggfittext::geom_fit_text(
                                                    fontface="bold",
                                                    place = "center")+
                                                  ggplot2::theme_void()+
                                                  ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white"))
    ), t=column_head_t,l=10) %>% 
    gtable::gtable_add_grob(age_axis, t=axis_t,l=10) %>% # takes axis text from ma and adds to bottom
    
    
    
    # Sex column
    
    gtable::gtable_add_grob(ggplot2::ggplotGrob(sex), t=column_t,l=11, b=column_b) %>%
    
    gtable::gtable_add_grob(grid::rasterGrob(sex_img), t=column_head_t, b=axis_t, l=11) %>% 
    
    
    # Participants section header 
    gtable::gtable_add_grob(ggplot2::ggplotGrob(ggplot2::ggplot(data=NULL,
                                                                ggplot2::aes(x=0,y=0, 
                                                                             label = "\uf007 PARTICIPANTS"))+
                                                  ggfittext::geom_fit_text(
                                                    family="fontawesome",
                                                    colour = "white",
                                                    fontface="bold",
                                                    place = "center",
                                                    grow = FALSE)+
                                                  ggplot2::theme_void()+
                                                  ggplot2::theme(plot.background = ggplot2::element_rect(fill = colour_participants_title))
    ), t=section_t,l=participants_section_l, r=participants_section_r) %>% 
    
    # Temperature section header
    
    gtable::gtable_add_grob(ggplot2::ggplotGrob(ggplot2::ggplot(data=NULL,
                                                                ggplot2::aes(x=0,y=0, 
                                                                             label = glue::glue("\uf06d {range_title} MEASUREMENTS")))+
                                                  ggfittext::geom_fit_text(colour = "white",
                                                                           family = "fontawesome",
                                                                           fontface="bold",
                                                                           place = "center",
                                                                           grow = FALSE)+
                                                  ggplot2::theme_void()+
                                                  ggplot2::theme(plot.background = ggplot2::element_rect(fill = colour_measurements_title))
    ), t=section_t,l=temp_section_l, r=temp_section_r) %>% 
    
    # Comparison column 
    
    gtable::gtable_add_grob(ggplot2::ggplotGrob(comparison), t=column_t, l=12, b=column_b) %>% 
    gtable::gtable_add_grob(ggplot2::ggplotGrob(ggplot2::ggplot(data=NULL,
                                                                ggplot2::aes(x=0,y=0, 
                                                                             label = "Comparison"))+
                                                  ggfittext::geom_fit_text(
                                                    fontface="bold",
                                                    place = "center",
                                                    reflow = TRUE)+
                                                  ggplot2::theme_void()+
                                                  ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white"))
    ), t=column_head_t,l=12) %>% 
    
    # temperature range column
    
    gtable::gtable_add_grob(temp_grob, t=column_t,l=14, b=column_b) %>%
    gtable::gtable_add_grob(ggplot2::ggplotGrob(ggplot2::ggplot(data=NULL,
                                                                ggplot2::aes(x=0,y=0, 
                                                                             label = glue::glue("Range ({range_unit})")))+
                                                  ggfittext::geom_fit_text(
                                                    fontface="bold",
                                                    place = "center")+
                                                  ggplot2::theme_void()+
                                                  ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white"))
    ), t=column_head_t,l=14) %>% 
    
    gtable::gtable_add_grob(temp_axis, t=axis_t,l=14) %>% # takes axis text from ma and adds to bottom
    
    
    # Sample size 
    
    gtable::gtable_add_grob(ggplot2::ggplotGrob(participants), t=column_t,l=13, b=column_b) %>%
    gtable::gtable_add_grob(ggplot2::ggplotGrob(ggplot2::ggplot(data=NULL,
                                                                ggplot2::aes(x=0,y=0, 
                                                                             label = "Sample size"))+
                                                  ggfittext::geom_fit_text(
                                                    fontface="bold",
                                                    place = "center")+
                                                  ggplot2::theme_void()+
                                                  ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white"))
    ), t=column_head_t,l=13) %>% 
    
    # paired measurements
    
    gtable::gtable_add_grob(ggplot2::ggplotGrob(measurements), t=column_t, l=15, b=column_b) %>% 
    gtable::gtable_add_grob(ggplot2::ggplotGrob(ggplot2::ggplot(data=NULL,
                                                                ggplot2::aes(x=0,y=0, 
                                                                             label = "Paired measurements"))+
                                                  ggfittext::geom_fit_text(
                                                    fontface="bold",
                                                    place = "left",
                                                    reflow = TRUE)+
                                                  ggplot2::theme_void()+
                                                  ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white"))
    ), t=column_head_t,l=15) %>% 
    
    # Results section header
    
    gtable::gtable_add_grob(ggplot2::ggplotGrob(ggplot2::ggplot(data=NULL,
                                                                ggplot2::aes(x=0,y=0, 
                                                                             label = "\uf1fe RESULTS"))+
                                                  ggfittext::geom_fit_text(
                                                    family='fontawesome',
                                                    colour = "white",
                                                    fontface="bold",
                                                    place = "center",
                                                    grow = FALSE)+
                                                  ggplot2::theme_void()+
                                                  ggplot2::theme(plot.background = ggplot2::element_rect(fill = colour_results_title))
    ), t=section_t,l=16) %>%
    
    # Results column
    
    gtable::gtable_add_grob(ggplot2::ggplotGrob(results), t=column_t,l=16, b=column_b) %>% 
    
    gtable::gtable_add_grob(ggplot2::ggplotGrob(ggplot2::ggplot(data=NULL,
                                                                ggplot2::aes(x=0,y=0, 
                                                                             label = glue::glue("Difference in {range_results_subtitle} between comparator and ZHF")))+
                                                  ggfittext::geom_fit_text(
                                                    fontface="italic",
                                                    place = "center",
                                                    reflow = TRUE)+
                                                  ggplot2::theme_void()+
                                                  ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white"))
    ), t=column_head_t,b=axis_t, l=16) %>% 
    
    # Meta-analysis column
    
    gtable::gtable_add_grob(ma_grob, t=4+ length(unique({{ data }}$Study))+1,l=16) %>% 
    gtable::gtable_add_grob(ma_axis, t=4+ length(unique({{ data }}$Study))+2,l=16) %>% # takes axis text from ma and adds to bottom
    
    
    # Meta-analysis grob to left of results
    
    gtable::gtable_add_grob(ggplot2::ggplotGrob(ggplot2::ggplot(data=NULL,
                                                                ggplot2::aes(x=0,y=0, 
                                                                             label = "Mean bias (95% limits of agreement)"))+
                                                  # geom_tile(fill = "#002a60")+
                                                  ggfittext::geom_fit_text( colour = "white",
                                                                            fontface="bold",
                                                                            place = "center",
                                                                            reflow=TRUE)+
                                                  ggplot2::theme_void()+
                                                  ggplot2::theme(
                                                    panel.background = ggplot2::element_rect(fill = "#002a60", colour = "white",
                                                                                             size = 2, linetype = "solid"),
                                                    panel.grid.major = ggplot2::element_blank(), 
                                                    panel.grid.minor = ggplot2::element_blank()
                                                  )+
                                                  ggplot2::scale_y_continuous(expand=c(0.1,0.1))
                                                #   theme(plot.background = element_rect(fill = "#002a60"))
    ), t=4+ length(unique({{ data }}$Study))+1, l=14, r=15) %>% 
    

  # Total number of studies
  
  gtable::gtable_add_grob(ggplot2::ggplotGrob(ggplot2::ggplot(data=NULL,
                                                              ggplot2::aes(x=0,y=0, 
                                                                           label = "Studies"))+
                                                ggfittext::geom_fit_text(
                                                  colour = "white",
                                                  fontface="bold",
                                                  place = "center",
                                                  grow = FALSE)+
                                                ggplot2::theme_void()+
                                                ggplot2::theme(plot.background = ggplot2::element_rect(fill = colour_results_title))
  ), t=4+ length(unique({{ data }}$Study))+1, l=3, r=3) %>% 
    
    gtable::gtable_add_grob(grid::textGrob(length(unique({{ data }}$Study)), gp=grid::gpar(col=colour_results_title, fontface="bold")),
                            t=4+ length(unique({{ data }}$Study))+1,l=4,r=4)  %>% 
    
    # Total number of participants
    
    gtable::gtable_add_grob(ggplot2::ggplotGrob(ggplot2::ggplot(data=NULL,
                                                                ggplot2::aes(x=0,y=0, 
                                                                             label = "Participants"))+
                                                  ggfittext::geom_fit_text(
                                                    colour = "white",
                                                    fontface="bold",
                                                    place = "center",
                                                    grow = FALSE)+
                                                  ggplot2::theme_void()+
                                                  ggplot2::theme(plot.background = ggplot2::element_rect(fill = colour_results_title))
    ), t=4+ length(unique({{ data }}$Study))+1, l=5, r=5) %>% 
    
    gtable::gtable_add_grob(grid::textGrob(sum({{ data }}$n, na.rm=TRUE), gp=grid::gpar(col=colour_results_title, fontface="bold")),
                            t=4+ length(unique({{ data }}$Study))+1,l=6,r=7)  %>% 
    
    
    # Total number of measurements
    
    gtable::gtable_add_grob(ggplot2::ggplotGrob(ggplot2::ggplot(data=NULL,
                                                                ggplot2::aes(x=0,y=0, 
                                                                             label = "Measurements"))+
                                                  ggfittext::geom_fit_text(
                                                    colour = "white",
                                                    fontface="bold",
                                                    place = "center",
                                                    grow = FALSE)+
                                                  ggplot2::theme_void()+
                                                  ggplot2::theme(plot.background = ggplot2::element_rect(fill = colour_results_title))
    ), t=4+ length(unique({{ data }}$Study))+1, l=8, r=10) %>% 
    
    gtable::gtable_add_grob(grid::textGrob(sum({{ data }}$N, na.rm=TRUE), gp=grid::gpar(col=colour_results_title, fontface="bold")),
                            t=4+ length(unique({{ data }}$Study))+1,l=11,r=11)  %>% 
    
    
    # GRADE rating
    
    gtable::gtable_add_grob(ggplot2::ggplotGrob(ggplot2::ggplot(data=NULL,
                                                                ggplot2::aes(x=0,y=0, 
                                                                             label = "GRADE rating"))+
                                                  ggfittext::geom_fit_text(
                                                    colour = "white",
                                                    fontface="bold",
                                                    place = "center",
                                                    grow = FALSE)+
                                                  ggplot2::theme_void()+
                                                  ggplot2::theme(plot.background = ggplot2::element_rect(fill = colour_results_title))
    ), t=4+ length(unique({{ data }}$Study))+1, l=12, r=12) %>% 
    
    gtable::gtable_add_grob(grid::textGrob(grade_rating, gp=grid::gpar(col=colour_results_title, fontface="bold")),
                            t=4+ length(unique({{ data }}$Study))+1,l=13,r=13)  %>% 
    
    gtable::gtable_add_grob(grid::roundrectGrob(gp=grid::gpar(fill="transparent", col=colour_results_title)),
                                     t=4+ length(unique({{ data }}$Study))+1,l=2,r=17)  %>% 
    
    # gray line across the top
    gtable::gtable_add_grob(grid::segmentsGrob( # line across the bottom
      x0 = grid::unit(0,"npc"),
      y0 = grid::unit(0,"npc"),
      x1 = grid::unit(1,"npc"),
      y1 = grid::unit(0,"npc"),
      gp = grid::gpar(col="gray")),
                            t=4, l=2,r=17) 
  


  # function to add lines under each study
  addlinegrobs<-function(gt){
    for (i in 1:length(unique({{ data }}$Study))+4){
      gt<-gt %>% gtable::gtable_add_grob(grid::segmentsGrob( # line across the bottom
        x0 = grid::unit(0,"npc"),
        y0 = grid::unit(0,"npc"),
        x1 = grid::unit(1,"npc"),
        y1 = grid::unit(0,"npc"),
        gp = grid::gpar(col="gray")),
                                         t=i,l=3,r=17)
    }
    return(gt)
  }

  gt <- addlinegrobs(gt)
  
  
  # function to add years
  addyeargrob <- function(gt){
    
    chick=data.frame(x=c(factor("factor")), 
                     y=c(seq(1,100, by=1)))
    
    df <- data %>% 
      tidyr::separate(Study, c("Study", "Year"), sep = ", ")  %>% 
      dplyr::mutate(Study = as.factor(Study)) %>% 
      dplyr::mutate(Year = as.numeric(Year)) %>%
      dplyr::distinct(Study,.keep_all = TRUE)%>% 
      dplyr::arrange(-Year)
      
    years <- unique(df$Year)
    for (i in 1:length(unique(df$Year))){
      gt <- gt %>%  gtable::gtable_add_grob(ggplot2::ggplotGrob(ggplot2::ggplot(data = dplyr::count(chick, x), ggplot2::aes(x=x, y=n)
                                                                                             
                                                                                             )+
                                                                  ggchicklet::geom_chicklet(radius = grid::unit(3, 'mm'), 
                                                                                            fill = colour_study_title) +
                                                                  
                                                                  ggplot2::geom_text(ggplot2::aes(x=x, y=n/2)
                                                                                     ,label = years[i],
                                                                                           colour = "white",
                                                                                           fontface="bold"
                                                                                           )+
                                                                  ggplot2::theme_void()
                                            ),
                                                           t=head(which(df$Year==years[i]),1)+4, 
                                                           b=tail(which(df$Year==years[i]),1)+4, 
                                                           l=2,r=2) 

    }
    return(gt)
  }
  
  gt <- addyeargrob(gt)   
  

   
   
}
