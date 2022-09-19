########## 
##########
# All functions for joining and comparing the predictions together
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-08-24
##########
##########

#############################
# plot_predictions() function 
#############################
join_prediction_df = function(pred1_in, pred1_yr, pred2, pred3, pred4) {
  
  #' Take in all four data frames, and join them together so we can compare the 
  #' different assumptions in the estimates
  
  all_pred_df = rbind(
    pred1_in,
    pred1_yr,
    pred2,
    pred3,
    pred4
  )
  
  return(all_pred_df)
}

#############################
# shift_legend() function 
#############################
shift_legend = function(p){
  
  #' This function is not originally mine!!!! This function was found on 
  #' stack overflow, and all credit for this should go to Z. Lin - 
  #' profile here: https://stackoverflow.com/users/8449629/z-lin
  #' 
  #' Function moves the legend on a facet_wrapped plot so that not as much
  #' dead space is wasted in the plot 
  
  # check if p is a valid object
  if(!"gtable" %in% class(p)){
    if("ggplot" %in% class(p)){
      gp = ggplotGrob(p) # convert to grob
    } else {
      message(paste0("This is neither a ggplot object nor a grob generated ",
      "from ggplotGrob. Returning original plot."))
      return(p)
    }
  } else {
    gp = p
  }
  
  # check for unfilled facet panels
  facet.panels = grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels = sapply(facet.panels, 
                               function(i) "zeroGrob" %in% 
                                 class(gp[["grobs"]][[i]]))
  empty.facet.panels = facet.panels[empty.facet.panels]
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. 
            Returning original plot.")
    return(p)
  }
  
  # establish extent of unfilled facet panels (including any axis cells btwn)
  empty.facet.panels = gp[["layout"]][empty.facet.panels, ]
  empty.facet.panels = list(min(empty.facet.panels[["t"]]), 
                             min(empty.facet.panels[["l"]]),
                             max(empty.facet.panels[["b"]]), 
                             max(empty.facet.panels[["r"]]))
  names(empty.facet.panels) = c("t", "l", "b", "r")
  
  # extract legend & copy over to location of unfilled facet panels
  guide.grob = which(gp[["layout"]][["name"]] == "guide-box")
  if(length(guide.grob) == 0){
    message("There is no legend present. Returning original plot.")
    return(p)
  }
  gp =gtable::gtable_add_grob(x = gp,
                        grobs = gp[["grobs"]][[guide.grob]],
                        t = empty.facet.panels[["t"]],
                        l = empty.facet.panels[["l"]],
                        b = empty.facet.panels[["b"]],
                        r = empty.facet.panels[["r"]],
                        name = "new-guide-box")
  
  # squash the original guide box's row / column (whichever applicable)
  # & empty its cell
  guide.grob = gp[["layout"]][guide.grob, ]
  if(guide.grob[["l"]] == guide.grob[["r"]]){
    gp = cowplot::gtable_squash_cols(gp, cols = guide.grob[["l"]])
  }
  if(guide.grob[["t"]] == guide.grob[["b"]]){
    gp = cowplot::gtable_squash_rows(gp, rows = guide.grob[["t"]])
  }
  gp = cowplot::gtable_remove_grobs(gp, "guide-box")
  
  return(gp)
}

#############################
# plot_predictions() function 
#############################
plot_predictions = function(all_pred_df, path) {
  
  #' Plot the different estimates beside each other for each year for 
  #' comparison
  
  # check the grouping values are factors
  all_pred_df = all_pred_df %>% 
    dplyr::mutate(
      scenario = as.factor(scenario),
      year = as.factor(as.character(year))
      )
  
  # make the plot
  test = ggplot(data = all_pred_df) + 
    geom_errorbar(aes(x = year, ymin = lower, ymax = upper),
                  colour = "black", width = 0) +
    geom_point(aes(x = year, y = fit, fill = scenario), 
               colour = "black", shape = 21, size = 3) +
    facet_wrap(~scenario) +
    scale_fill_manual("Scenario",
                      values = PNWColors::pnw_palette("Bay", 5, 
                                                      type = "discrete"),
                      labels = c(
                        "Scenario 1 - Individual",
                        "Scenario 1 - Year",
                        "Scenario 2", 
                        "Scenario 3",
                        "Scenario 4"
                      )) +
    ggthemes::theme_base() +
    theme(
      axis.text.x = element_text(angle = 90),
      strip.text.x = element_blank()
    ) +
    labs(x = "Year", y = "Maximum Likelihood Estimate & 95% CI's")
  
  # make the new drawn object
  png(filename = paste0(
    path, "all-scenario-yearly-lice-per-fish-estimates.png"),
    units = "px", 
    width = 800,
    height = 450
    )
  
  grid::grid.draw(shift_legend(test))
  
  dev.off()

}

#############################
# write_bound_file() function 
#############################
write_bound_file = function(df, file_path) {
  
  #' Take the bound df and write it out so a temp file exists
  
  readr::write_csv(
    df, file_path
  )
}

#############################
# execute_predictions_plot() function 
#############################
execute_predictions_plot = function(pred1_in, pred1_yr, pred2, 
                                    pred3, pred4, file_path, fig_path) {
  
  #' use helper functions to plot the different scenario results and also 
  #' return the object created of the bound together scenarios to be used 
  #' later in the regression against farm fish
  
  # join the various df's together
  all_pred_df = join_prediction_df(pred1_in, pred1_yr, pred2, pred3, pred4)
  
  # write out the file 
  write_bound_file(all_pred_df, file_path)
  
  # plot the predictions (with legend shift helper)
  plot_predictions(all_pred_df, fig_path)
  
  return(all_pred_df)
  
}
