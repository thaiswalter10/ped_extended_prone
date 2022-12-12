# Take one variable as input (eg: pao2, fio2 ...)
# Rearrange datafram in 5 colomns:
#"gupi", "pp_session_number", "name_var", "time_point" and "value"
# the output data frame contains only the variable which was given as input. 
make_single_var_df <- function(variable, dat, list_time_points){
  list_var <- map(list_time_points, ~str_c(variable, all_of(.x), sep  = "_")) %>% unlist
  list_df <- map(list_var, ~select(dat, all_of(.x), gupi, pp_session_number))
  i = 1
  for (df in list_df){
    list_df[[i]] = df %>% 
      mutate(time_point = str_sub(list_var[i], -6, -1),
             name_var := (str_split(list_var[i], "_"))[[1]][1]) %>%
      rename(value = list_var[i])
    i = i+1
  }
  reduce(list_df, add_row)
}

# For a given variable list, 
# apply make_single_var_df to each variable, then add up all lines. 
# The output is a single dataframe. 
create_df_to_plot_vent_var <- function(df = dv, funct = make_single_var_df, label = x_label){
  list_var_to_plot <- c("fio2", "pltp", "pf", "drvp", "cpl")
  level_key <- label %>% pull(x_label) %>% setNames(label$given_time_point)
  df_to_plot_vent_var <- map_dfr(list_var_to_plot, funct, df, label$given_time_point) %>% 
    as_tibble %>%
    mutate(time_point = recode(time_point, !!!level_key) %>% as_factor)
}

plot_vent_var <- function(variable, df = df_to_plot_vent_var){
  ggplot(df %>% filter(name_var == variable), aes(x= time_point, y=value)) +
    geom_boxplot(outlier.shape = NA, fill="grey", alpha=0.3) +
    stat_summary(fun = mean, geom="line", aes(group=1))  + 
    stat_summary(fun=mean, geom="point", shape = 2) +
    theme(panel.background = element_rect(fill = "transparent"),
          axis.line = element_line(size = 0.2, linetype = "solid"),
          axis.text.x = element_text(size=12, angle=0)) +
    scale_x_discrete(name = element_blank())
}

plot_pf <- function(df = df_to_plot_vent_var){
  pf_plot <- ggdraw(plot_vent_var("pf", df) + 
                      geom_signif(comparisons = list(c("Baseline", "Supine")),
                                  annotations="**",
                                  y_position = 400,
                                  map_signif_level=TRUE
                      ) +
                      geom_signif(comparisons = list(c("Prone, H+16", "End of Prone")),
                                  annotations="*",
                                  y_position = 360,
                                  map_signif_level=TRUE
                      ) + 
                      scale_y_continuous(limits = c(0, 430), 
                                         name = "Pao2 / Fio2 ratio", 
                                         breaks = seq(from=0, to=420, by=50)
                      )
  ) 
}

plot_cpl <- function(df= df_to_plot_vent_var){
  cpl_plot <- ggdraw(plot_vent_var(variable = "cpl", df) + 
                       scale_y_continuous(limits = c(0,75), name = "Compliance (mL/cm H20)") +
                       geom_signif(comparisons = list(c("Baseline", "Supine")),
                                   annotations="**",
                                   y_position = 60,
                                   map_signif_level=TRUE
                       )
  )   
}

plot_drvp <- function(df = df_to_plot_vent_var){
  drvp_plot <- ggdraw(plot_vent_var("drvp", df) + 
                        scale_y_continuous(name = "Driving Pressure (cm H20)") +
                        geom_signif(comparisons = list(c("Baseline", "Supine")),
                                    annotations="**",
                                    y_position = 27,
                                    map_signif_level=TRUE
                        )
  )
}

plot_3_var <- function(df= df_to_plot_vent_var){
  cpl_plot <- ggdraw(plot_vent_var(variable = "cpl", df) + 
                       scale_y_continuous(limits = c(0,75), name = "Compliance (mL/cm H20)") +
                       geom_signif(comparisons = list(c("Baseline", "Supine")),
                                   annotations="**",
                                   y_position = 60,
                                   map_signif_level=TRUE
                       )
                     ) 
  pf_plot <- ggdraw(plot_vent_var("pf", df) + 
                    geom_signif(comparisons = list(c("Baseline", "Supine")),
                                annotations="**",
                                y_position = 400,
                                map_signif_level=TRUE
                                ) +
                    geom_signif(comparisons = list(c("Prone, H+16", "End of Prone")),
                                annotations="*",
                                y_position = 360,
                                map_signif_level=TRUE
                                ) + 
                    scale_y_continuous(limits = c(0, 430), 
                                       name = "Pao2 / Fio2 ratio", 
                                       breaks = seq(from=0, to=420, by=50)
                                       )
              ) 
  drvp_plot <- ggdraw(plot_vent_var("drvp", df) + 
                        scale_y_continuous(name = "Driving Pressure (cm H20)") +
                        geom_signif(comparisons = list(c("Baseline", "Supine")),
                                    annotations="**",
                                    y_position = 27,
                                    map_signif_level=TRUE
                        )
                      )
  plot_grid(cpl_plot, pf_plot, drvp_plot, labels = c('a', 'b', 'c'), ncol=1)
}
