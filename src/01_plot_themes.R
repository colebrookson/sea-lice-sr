########## 
##########
# All themes for the different plots made in R
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-02-02
##########
##########

# set theme for small map
theme_small_map <- function() {
  color_background = "white"
  color_axis_text = "black"
  color_axis_title = "black"
  color_title = "black"
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill = color_background,
                                            color = color_background)) +
    theme(plot.background = element_rect(fill = color_background,
                                            color = color_background)) +
    theme(panel.border = element_rect(colour = "black", size = 1.3)) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.ticks = element_blank()) +
    theme(plot.title = element_text(color = color_title,
                                    size = 15, vjust = 1.25)) +
    theme(axis.text.x = element_text(size = 12, color = color_axis_text,
                                        angle = 90)) +
    theme(axis.text.y = element_text(size = 12, color = color_axis_text)) +
    theme(axis.title.x = element_text(size = 18, color = color_axis_title,
                                        vjust = 0)) +
    theme(axis.title.y = element_text(size = 18, color = color_axis_title,
                                        vjust = 1.25)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.ticks = element_line(colour = "black")) +
    theme(axis.line.x = element_line(color = "black", size = 0.15),
          axis.line.y = element_line(color = "black", size = 0.15),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14))
}