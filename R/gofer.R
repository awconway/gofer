#' gofer diagram
#' @rdname gofer
#' @name gofer
#' @param data # include here descriptions of each argument we include in the function
#' 
#' @export
#' 
gofer <- function(data, ma_effect, ma_lower, ma_upper, dodge_width = 0.7){

year <- {{ data }} %>%
  dplyr::distinct(Study,.keep_all = TRUE) %>% 
  tidyr::separate(Study, c("Study", "Year"), sep = ", ")  %>% 
  dplyr::mutate(Study = as.factor(Study)) %>% 
  dplyr::mutate(Year = as.numeric(Year)) %>%
  ggplot2::ggplot(ggplot2::aes(x= stats::reorder(Study, Year), y=0, label=Year))+
  ggplot2::geom_label(ggplot2::aes( fill=Year), colour = "white", fontface = "bold", show.legend = FALSE)+
  ggplot2::theme_void()+
  ggplot2::coord_flip()

study <- {{ data }} %>% 
  dplyr::distinct(Study,.keep_all = TRUE) %>% 
  tidyr::separate(Study, c("Study", "Year"), sep = ", ")  %>% 
  dplyr::mutate(Study = as.factor(Study)) %>% 
  dplyr::mutate(Year = as.numeric(Year)) %>%
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
  ggplot2::ggplot()+
  ggplot2::geom_point(ggplot2::aes(x=stats::reorder(Study, Year), y=bias, alpha=group, size=n), colour = "#002a60", position = ggplot2::position_dodge(width = dodge_width), show.legend=FALSE)+
  ggplot2::geom_linerange(ggplot2::aes(x=stats::reorder(stats::reorder(Study, Year), Year), ymin=lower, ymax=upper, alpha=group), colour = "#002a60",size=1, position = ggplot2::position_dodge(width = dodge_width), show.legend=FALSE)+
  ggplot2::geom_hline(yintercept = ma_effect, linetype = 2, col = "#002a60") +
  ggplot2::scale_alpha_discrete(range = c(rep(1,4)))+
  ggplot2::coord_flip()+ 
  ggplot2::theme_void()+
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "#BFD5E3", colour = "white",
                                    size = 2, linetype = "solid"),
    panel.grid.major = ggplot2::element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = ggplot2::element_blank()
  )+
  ggplot2::scale_y_continuous(expand=c(0.1,0.1), limits=c(pmin(min({{ data }}$lower), ma_lower), pmax(max({{data}}$upper), ma_upper)))+
  ggplot2::scale_x_discrete(breaks = NULL) #removes horizontal grid lines
# ggrepel::geom_label_repel(
#   force=23,
#   direction="y",
#   hjust   = 0,
#   segment.color = "grey50",
#   size=2)

participants <- {{ data }} %>% 
  tidyr::separate(Study, c("Study", "Year"), sep = ", ")  %>% 
  dplyr::mutate(Study = as.factor(Study)) %>% 
  dplyr::mutate(Year = as.numeric(Year)) %>%
  ggplot2::ggplot(ggplot2::aes(x=stats::reorder(Study, Year), y=n))+
  ggplot2::geom_linerange(ggplot2::aes(x=stats::reorder(Study, Year), ymin=0, ymax=n, alpha=group), 
                 colour = "#930093", 
                 size=0.5, position = ggplot2::position_dodge(width = dodge_width), show.legend=FALSE)+
  ggplot2::geom_label(ggplot2::aes(x=stats::reorder(Study, Year), y=n, label = n, alpha=group), 
             colour="#930093",
             label.r = grid::unit(0.4, "lines"),
             label.padding = grid::unit(0.2, "lines"),
             size=3,
             position = ggplot2::position_dodge(width = dodge_width), show.legend=FALSE)+
  ggplot2::scale_alpha_discrete(range = c(rep(1,4)))+
  ggplot2::theme_void()+
  ggplot2::coord_flip()+
  ggplot2::theme_void()+
  # theme(
  #   panel.background = element_rect(fill = "white", colour = "white",
  #                                   linetype = "solid"),
  #   panel.grid.major = element_line(size = 0.1, linetype = 'solid',
  #                                   colour = "#ffceff")
  # )+
  # scale_x_discrete(breaks = NULL)+ #removes horizontal grid lines
  ggplot2::geom_text(ggplot2::aes( x=stats::reorder(Study, Year), y=-5, alpha=group), 
            label =  "\uf0c0", 
            family =  "FontAwesome", 
            size =  3.5,
            vjust="center", 
            colour = "#930093",
            position = ggplot2::position_dodge(width = dodge_width), show.legend=FALSE)+
  ggplot2::scale_y_continuous(expand = ggplot2::expand_scale(mult = c(0.1, .1)))



measurements <- {{ data }} %>% 
  tidyr::separate(Study, c("Study", "Year"), sep = ", ")  %>% 
  dplyr::mutate(id = dplyr::row_number()) %>% 
  dplyr::mutate(Study = as.factor(Study)) %>% 
  dplyr::mutate(Year = as.numeric(Year)) %>%
  ggplot2::ggplot(ggplot2::aes(x=stats::reorder(Study, Year), y=n))+
  ggplot2::geom_linerange(ggplot2::aes(x=stats::reorder(Study, Year), ymin=0, ymax=N, alpha=group), 
                 colour = "#930093", 
                 size=0.5, position = ggplot2::position_dodge(width = dodge_width), show.legend=FALSE)+
  ggplot2::geom_label(ggplot2::aes(x=stats::reorder(Study, Year), y=N, label = N, alpha=group), 
             colour="#930093",
             label.r = grid::unit(0.4, "lines"),
             label.padding = grid::unit(0.2, "lines"),
             size=3,
             position = ggplot2::position_dodge(width = dodge_width), show.legend=FALSE)+
  ggplot2::scale_alpha_discrete(range = c(rep(1,4)))+
  ggplot2::coord_flip()+
  ggplot2::theme_void()+
  # theme(
  #   panel.background = element_rect(fill = "white", colour = "white",
  #                                   linetype = "solid"),
  #   panel.grid.major = element_line(size = 0.1, linetype = 'solid',
  #                                   colour = "#ffceff"),
  #   # axis.line.y = element_line(size = 0.5, linetype = 'solid',colour = "#930093") # vertical line at zero
  # )+
  # scale_x_discrete(breaks = NULL)+ #removes horizontal grid lines
  ggplot2::scale_y_continuous(trans='log', expand = ggplot2::expand_scale(mult = c(0.1, .1)))


comparison <- {{ data }} %>% 
  tidyr::separate(Study, c("Study", "Year"), sep = ", ")  %>% 
  dplyr::mutate(comparison = dplyr::recode(comparison, Eso = "Esophageal")) %>%
  dplyr::mutate(comparison = dplyr::recode(comparison, PA = "Pulmonary artery")) %>% 
  dplyr::mutate(comparison = dplyr::recode(comparison, Ax = "Axillary")) %>% 
  dplyr::mutate(Study = as.factor(Study)) %>% 
  dplyr::mutate(Year = as.numeric(Year)) %>%
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
  ggplot2::ggplot(ggplot2::aes(x=stats::reorder(Study, Year), y=0,label=patients))+
  ggfittext::geom_fit_text(ggplot2::aes(x=stats::reorder(Study, Year), y=0), place = "center", reflow=TRUE
                           )+
  ggplot2::theme_void()+
  ggplot2::coord_flip()

# 
# comments <-  {{ data }} %>% 
#   separate(Study, c("Study", "Year"), sep = ", ")  %>% 
#   mutate(Study = as.factor(Study)) %>% 
#   mutate(Year = as.numeric(Year)) %>%
#   naniar::replace_with_na(replace = list(comments = "NA")) %>%
#   ggplot()+
#   geom_fit_text(aes(x=reorder(Study, Year), y=0, label = comments, alpha=group), place = "left", reflow=TRUE, 
#                 position = position_dodge(width = dodge_width), show.legend=FALSE)+
#   scale_alpha_discrete(range = c(rep(1,4)))+
#   theme_void()+
#   coord_flip()


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
                                            levels = c("high","low","unclear"),
                                            labels = c( "\uf00d","\uf00c", "\uf128")
  )) %>% 
  ggplot2::ggplot(ggplot2::aes(x=stats::reorder(Study, Year), y=0, colour=RoB_classification, label= RoB_domain, alpha=RoB_domain))+
  # ggplot2::geom_point(
  #                     size=3,position = ggplot2::position_dodge(width = dodge_width),
  #                     show.legend=FALSE)+
  # ggplot2::geom_text(aes(label=RoB_classification),
  #                    family =  "FontAwesome",
  #                    size =  1.5,
  #                    position = ggplot2::position_dodge(width = dodge_width),
  #                    show.legend=FALSE)+
  geom_label(position = ggplot2::position_dodge(width = dodge_width), show.legend=FALSE, size=3, label.padding=unit(0.1, "lines"))+
  # ggfittext::geom_fit_text(place = "left", reflow=FALSE, grow = FALSE,
  #                          position = ggplot2::position_dodge(width = dodge_width), show.legend=FALSE)+
  ggplot2::theme_void()+
  ggplot2::coord_flip()+
  ggplot2::scale_color_manual(values=c("red", "green", "orange"))+
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
  ggplot2::scale_y_continuous(expand=c(0,0), limits=c(-1,1))+
  ggplot2::scale_x_continuous(expand=c(0.1,0.1), limits=c(pmin(min({{ data }}$lower), ma_lower), pmax(max({{data}}$upper), ma_upper)))+
  ggplot2::scale_y_discrete(breaks = NULL)  #removes horizontal grid lines

ma_grob <- ggplot2::ggplotGrob(ma)
ma_axis <- ma_grob$grobs[[which(ma_grob$layout$name == "axis-b")]]$children$axis[2] 

ma_grob$grobs[[which(ma_grob$layout$name == "axis-b")]] <- ggplot2::zeroGrob()
ma_grob$heights[ma_grob$layout$t[which(ma_grob$layout$name == "axis-b")]] <- grid::unit(0, "cm")


# Study characteristics data
# Age  
age <- {{ data }} %>%
  tidyr::separate(Study, c("Study", "Year"), sep = ", ")  %>% 
  dplyr::mutate(Study = as.factor(Study)) %>% 
  dplyr::mutate(Year = as.numeric(Year)) %>%
  dplyr::distinct(Study,.keep_all = TRUE) %>% 
  ggplot2::ggplot(ggplot2::aes(x=stats::reorder(Study, Year), y=0)) +
  ggplot2::geom_point(ggplot2::aes(x = stats::reorder(Study, Year), y = mean_age), size = 2, col = "orange red") +
  ggplot2::geom_linerange(ggplot2::aes(x = stats::reorder(Study, Year), ymin = lower_mean, ymax = upper_mean), size = 1, col = "orange red") +
  ggplot2::geom_point(ggplot2::aes(x = stats::reorder(Study, Year), y = median_age), size = 3, col = "orange", shape = "diamond") +
  ggplot2::geom_errorbar(ggplot2::aes(x = stats::reorder(Study, Year), ymin = lower_IQR, ymax = upper_IQR), size = 1, col = "orange", width = 0.5) +
  
  ggplot2::theme_void() +
  
  ggplot2::coord_flip() +
  
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "white", colour = "white",
                                    size = 2, linetype = "solid"),
    panel.grid.major = ggplot2::element_line(size = 0.5, linetype = 'solid',
                                    colour = "#FFE5CC"), 
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.x=ggplot2::element_text(size=8)
  )+
  
  ggplot2::scale_x_discrete(breaks = NULL)  

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
  ggplot2::ggplot(ggplot2::aes(x=stats::reorder(Study, Year), y=values, fill = sex)) + 
  ggplot2::geom_col(position = "fill", show.legend = FALSE,
           width = 0.3) +
  ggplot2::scale_fill_manual(values = c("female" = "#ff8ea2", "male" = "#8edaff"))+
  
  # geom_text(label = "\uf183", family = "FontAwesome", x = 13.25, y = -0.25, size = 7, show.legend = FALSE) +
  # geom_text(label = "\uf182", family = "FontAwesome", x = 13.25, y = 1.25, fill = "salmon", color = "salmon") +
  # 
  ggplot2::coord_flip() +
  
  # expand_limits(y = -0.35) +
  # expand_limits(y = 1.35) +
  
  ggplot2::theme_void()

flags <- {{ data }} %>%
  tidyr::separate(Study, c("Study", "Year"), sep = ", ")  %>% 
  dplyr::mutate(Study = as.factor(Study)) %>% 
  dplyr::mutate(Year = as.numeric(Year)) %>%
  dplyr::distinct(Study,.keep_all = TRUE) %>% 
  ggplot2::ggplot(ggplot2::aes(x=stats::reorder(Study, Year), y=Country)) +
  ggimage::geom_flag(y = 0.5, ggplot2::aes(image = code), size = 0.2) +
  
  ggplot2::coord_flip() +
  
  ggplot2::theme_void()


temp <-  {{ data }} %>%
  tidyr::separate(Study, c("Study", "Year"), sep = ", ")  %>% 
  dplyr::mutate(Study = as.factor(Study)) %>% 
  dplyr::mutate(Yeademr = as.numeric(Year)) %>%
  dplyr::distinct(Study,.keep_all = TRUE) %>% 
  ggplot2::ggplot(ggplot2::aes(x=stats::reorder(Study, Year), ymin = lower_temp, ymax = upper_temp)) +
  ggplot2::geom_errorbar(size = 1, color = "#990099", width = 0.5) +
  
  ggplot2::coord_flip() +
  
  ggplot2::theme_void()+
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "white", colour = "white",
                                    size = 2, linetype = "solid"),
    panel.grid.major = ggplot2::element_line(size = 0.5, linetype = 'solid',
                                    colour = "#FFCCFF"), 
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.x=ggplot2::element_text(size=8)
  )+
  
  ggplot2::scale_x_discrete(breaks = NULL)  

temp_grob <- ggplot2::ggplotGrob(temp)
temp_axis <- temp_grob$grobs[[which(temp_grob$layout$name == "axis-b")]]$children$axis[2] 

temp_grob$grobs[[which(temp_grob$layout$name == "axis-b")]] <- ggplot2::zeroGrob()
temp_grob$heights[temp_grob$layout$t[which(temp_grob$layout$name == "axis-b")]] <- grid::unit(0, "cm")


# using gtable seems to be the best option - see here for documentation: https://gtable.r-lib.org/index.html
RoB_img <- png::readPNG(RCurl::getURLContent("https://raw.githubusercontent.com/awconway/zhf-review/master/gofer/RoB_wide.png"))
sex_img <- png::readPNG(RCurl::getURLContent("https://raw.githubusercontent.com/awconway/zhf-review/master/gofer/sex.png"))

gt_grid <- gtable::gtable(widths = grid::unit(c(0.05, #1 left space
                                  1,#2 First author
                                  0.5,#3 Year
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
                                  1.8,#15 Paired measurements
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
                               ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#0080ff"))
  ), t=section_t,l=study_section_l, r=study_section_r) %>% 
  # First author column
  gtable::gtable_add_grob(ggplot2::ggplotGrob(study), t=column_t,l=2, b=column_b) %>% 
  # gtable_add_grob(ggplotGrob(ggplot(data=NULL,
  #                                   aes(x=0,y=0, 
  #                                       label = "First author"))+
  #                              geom_fit_text(fontface="bold",
  #                                            place = "center")+
  #                              theme_void()+
  #                              theme(plot.background = element_rect(fill = "white"))
  # ), t=3,l=2) %>% 
  # Year column
  gtable::gtable_add_grob(ggplot2::ggplotGrob(year), t=column_t, l=3, b=column_b) %>% 
  # gtable_add_grob(ggplotGrob(ggplot(data=NULL,
  #                                   aes(x=0,y=0, 
  #                                       label = "Year"))+
  #                              geom_fit_text(
  #                                fontface="bold",
  #                                place = "center")+
  #                              theme_void()+
  #                              theme(plot.background = element_rect(fill = "white"))
  # ), t=3,l=3) %>% 
  
  # Country column

gtable::gtable_add_grob(ggplot2::ggplotGrob(flags), t=column_t,l=4, b=column_b) %>%
  # gtable_add_grob(ggplotGrob(ggplot(data=NULL,
  #                                   aes(x=0,y=0, 
  #                                       label = "Country"))+
  #                              geom_fit_text(
  #                                fontface="bold",
  #                                place = "center")+
  #                              theme_void()+
  #                              theme(plot.background = element_rect(fill = "white"))
  # ), t=3,l=4) %>% 
  
  # Population column

gtable::gtable_add_grob(ggplot2::ggplotGrob(patients), t=column_t, l=5, b=column_b) %>% 
  # gtable_add_grob(ggplotGrob(ggplot(data=NULL,
  #                                   aes(x=0,y=0, 
  #                                       label = "Population"))+
  #                              geom_fit_text(
  #                                fontface="bold",
  #                                place = "center")+
  #                              theme_void()+
  #                              theme(plot.background = element_rect(fill = "white"))
  # ), t=3,l=5) %>% 
  
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
                               ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#52ae32"))
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
  # gtable_add_grob(ggplotGrob(ggplot(data=NULL,
  #                                   aes(x=0,y=0, 
  #                                       label = "Sex"))+
  #                              geom_fit_text(
  #                                fontface="bold",
  #                                place = "center")+
  #                              theme_void()+
  #                              theme(plot.background = element_rect(fill = "white"))
  # ), t=3,l=11) %>% 
  
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
                               ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#ee7219"))
  ), t=section_t,l=participants_section_l, r=participants_section_r) %>% 
  
  # Temperature section header
  
  gtable::gtable_add_grob(ggplot2::ggplotGrob(ggplot2::ggplot(data=NULL,
                                    ggplot2::aes(x=0,y=0, 
                                        label = "\uf06d TEMPERATURE MEASUREMENTS"))+
                               ggfittext::geom_fit_text(colour = "white",
                                             family = "fontawesome",
                                             fontface="bold",
                                             place = "center",
                                             grow = FALSE)+
                               ggplot2::theme_void()+
                               ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#930093"))
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
                                        label = "Range (°C)"))+
                               ggfittext::geom_fit_text(
                                 fontface="bold",
                                 place = "center")+
                               ggplot2::theme_void()+
                               ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white"))
  ), t=column_head_t,l=14) %>% 
  
  gtable::gtable_add_grob(temp_axis, t=axis_t,l=14) %>% # takes axis text from ma and adds to bottom
  
  
  # Comments column
  
  # gtable_add_grob(ggplotGrob(comments), t=column_t, l=14, b=column_b) %>% 
  
  
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
                               ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#002a60"))
  ), t=section_t,l=16) %>%
  
  # Results column
  
  gtable::gtable_add_grob(ggplot2::ggplotGrob(results), t=column_t,l=16, b=column_b) %>% 
  
  gtable::gtable_add_grob(ggplot2::ggplotGrob(ggplot2::ggplot(data=NULL,
                                    ggplot2::aes(x=0,y=0, 
                                        label = "Difference in temperature (°C) between comparator and ZHF"))+
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
                                        label = "Pooled mean bias with population limits of agreement"))+
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
  ), t=4+ length(unique({{ data }}$Study))+1, l=13, r=15) %>% 
  
  # Dark blue on right of meta-analysis result
  
  gtable::gtable_add_grob(ggplot2::ggplotGrob(ggplot2::ggplot(data=NULL,
                                    ggplot2::aes(x=0,y=0))+
                               ggplot2::theme_void()+
                               ggplot2::theme(
                                 panel.background = ggplot2::element_rect(fill = "#002a60", colour = "white",
                                                                 size = 2, linetype = "solid"),
                                 panel.grid.major = ggplot2::element_blank(), 
                                 panel.grid.minor = ggplot2::element_blank()
                               )+
                               ggplot2::scale_y_continuous(expand=c(0.1,0.1))
                             #   theme(plot.background = element_rect(fill = "#002a60"))
  ), t=4+ length(unique({{ data }}$Study))+1, l=17)


# function to add rectangles around each study
addrectgrobs<-function(gt){
  for (i in 1:length(unique({{ data }}$Study))+4){ 
    gt<-gt %>% gtable::gtable_add_grob(grid::roundrectGrob(gp=grid::gpar(fill="transparent", col="gray")),
                               t=i,l=2,r=17) 
  }
  return(gt)
}

gt <- addrectgrobs(gt)  

grid::grid.newpage() # use newpage and grid.draw to plot the gtable

grid::grid.draw(gt)

  
}