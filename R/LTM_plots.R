
## TODO: Add vertical break lines for other methods besides the long-term median

# 
# ltm_plot_spidf_ts <- function(spidf_obj, tree_id = "",
#                               df_breaks = NULL) {
#   stopifnot(inherits(spidf_obj, "spidf"))
# 
#   # Define labels for known time-series columns
#   series_cols <- c(
#     "spi"            = ifelse(is_regularized(spidf_obj), "Daily interpolated", "Original SPI"),
#     "spi_mov_wind"   = "Moving-wind. upp. quantile",
#     "spi_smooth"     = "Whittaker-smooth.",
#     "spi_mov_smooth" = "Whittaker-smooth. mov. wind."
#   )
# 
#   # Identify existing series columns
#   selected_cols <- intersect(names(series_cols), names(spidf_obj))
# 
#   # Reshape to long format
#   df_long <- spidf_obj %>%
#     dplyr::select(ti, dplyr::all_of(selected_cols)) %>%
#     tidyr::pivot_longer(
#       cols = dplyr::all_of(selected_cols),
#       names_to = "type",
#       values_to = "value",
#       values_drop_na = TRUE
#     ) %>%
#     dplyr::mutate(type = dplyr::recode(type, !!!series_cols))
# 
#   # Optional breaks data
#   df_plot_breaks <- NULL
#   if (!is.null(df_breaks)) {
#     df_plot_breaks <- df_breaks %>%
#       dplyr::filter(has_breaks & has_valid_breaks_lt_med) %>%
#       dplyr::mutate(break_date = as.Date(break_date))
#   }
# 
#   # Okabe-Ito Hex Codes
#   c("#E69F00", "#56B4E9",
#     "#009E73", "#F5C710",
#     "#0072B2","#D55E00",
#     "#CC79A7")
#   
#   # Color palettes
#   color_map_ts <- c(
#     "Original SPI"                  = "#CFCDCD",
#     "Daily interpolated"            = "#ABABAB",
#     "Moving-wind. upp. quantile"    = "#000000",
#     "Whittaker-smooth."             = "#D55E00",
#     "Whittaker-smooth. mov. wind."  = "#0072B2"
#   )
# 
#   color_map_algs <- c(
#     "cpm"     = "#CC79A7",
#     "ed"      = "#F0E442",
#     "bfast01" = "#999999",
#     "mcp"     = "#000000",
#     "stc"     = "#882255",
#     "wbs"     = "#117733"
#   )
# 
#   line_types <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
# 
#   # Filter colors
#   color_map_ts   <- color_map_ts[names(color_map_ts) %in% unique(df_long$type)]
#   color_map_algs <- color_map_algs[names(color_map_algs) %in% unique(df_plot_breaks$algorithm)]
#   line_types <- line_types[1:length(unique(df_plot_breaks$algorithm))]
# 
#   # Plot
#   p <- ggplot(df_long, aes(x = ti, y = value)) +
#     geom_line(aes(color = type), linewidth = 0.1, alpha = 0.6) +
#     geom_point(aes(color = type), size = 0.1, alpha = 0.6)
# 
#   if (!is.null(df_plot_breaks)) {
#     p <- p +
#       geom_vline(
#         data = df_plot_breaks,
#         aes(xintercept = break_date, linetype = algorithm, color = algorithm),
#         linewidth = 1,
#         alpha = 0.6,
#         show.legend = TRUE
#       )
#   }
# 
#   p <- p +
#     scale_color_manual(
#       name = "Time series",
#       values = c(color_map_ts, color_map_algs),
#       breaks = c(names(color_map_ts), names(color_map_algs)),
#       # values = c(color_map_ts),
#       #breaks = c(names(color_map_ts))
#     ) +
#     scale_linetype_manual(
#       name = "Break detection algorithm",
#       #values = rep("dotted", length(color_map_algs)),
#       values = line_types,
#       breaks = names(color_map_algs)
#     ) +
#     labs(
#       title = paste("Time Series of", get_spi(spidf_obj)),
#       subtitle = paste0(
#         "Location: [", round(get_longitude(spidf_obj), 5), ", ",
#         round(get_latitude(spidf_obj), 5), "] | ",
#         "Period: ", format(get_range_start(spidf_obj)), " to ",
#         format(get_range_end(spidf_obj)), " | ",
#         "Proc. Level: ", get_proc_level(spidf_obj),
#         ifelse(tree_id != "" && tree_id != "---", paste0(" | Tree ID: ", tree_id), "")
#       ),
#       x = "Date",
#       y = get_spi(spidf_obj)
#     ) +
#     theme_minimal(base_size = 15) +
#     theme(
#       legend.position = "bottom",
#       legend.box = "vertical",
#       legend.spacing.y = unit(0.2, 'cm'),
#       legend.title = element_text(face = "bold")
#     ) +
#     guides(
#       color = guide_legend(order = 1),
#       linetype = guide_legend(order = 2)
#     )
# 
#   return(p)
# }
# 

ltm_plot_spidf_ts <- function(spidf_obj, tree_id = "",
                              df_breaks = NULL,
                              only_valid_breaks = TRUE,
                              valid_breaks_mode ="any",
                              validators_sel=NULL) {
  
  stopifnot(inherits(spidf_obj, "spidf"))
  stopifnot(is.logical(only_valid_breaks), length(only_valid_breaks) == 1L, !is.na(only_valid_breaks))
  
  
  
  valid_breaks_mode <- match.arg(valid_breaks_mode, choices = c("any", "all"))
  
  validator_map <- c(
    lt_med   = "has_valid_breaks_lt_med",
    st_med   = "has_valid_breaks_st_med",
    st_trend = "has_valid_breaks_st_trend"
  )
  
  if (!is.null(validators_sel)) {
    validators_sel <- unique(validators_sel)
    
    invalid_validators <- setdiff(validators_sel, names(validator_map))
    if (length(invalid_validators) > 0L) {
      stop(
        "Invalid value(s) in `validators_sel`: ",
        paste(invalid_validators, collapse = ", "),
        ". Allowed values are: ",
        paste(names(validator_map), collapse = ", ")
      )
    }
  }
  
  validator_cols <- if (is.null(validators_sel)) {
    unname(validator_map)
  } else {
    unname(validator_map[validators_sel])
  }
  
  
  # Define labels for known time-series columns
  series_cols <- c(
    "spi"            = ifelse(is_regularized(spidf_obj), "Daily interpolated", "Original SPI"),
    "spi_mov_wind"   = "Moving-wind. upp. quantile",
    "spi_smooth"     = "Whittaker-smooth.",
    "spi_mov_smooth" = "Whittaker-smooth. mov. wind."
  )
  
  # Identify existing series columns
  selected_cols <- intersect(names(series_cols), names(spidf_obj))
  
  if (length(selected_cols) == 0L) {
    stop("No known time-series columns were found in `spidf_obj`.")
  }
  
  # Reshape to long format
  df_long <- spidf_obj %>%
    dplyr::select(ti, dplyr::all_of(selected_cols)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(selected_cols),
      names_to = "type",
      values_to = "value",
      values_drop_na = TRUE
    ) %>%
    dplyr::mutate(type = dplyr::recode(type, !!!series_cols))
  
  # Optional breaks data
  df_plot_breaks <- NULL
  
  if (!is.null(df_breaks)) {
    
    validator_cols_present <- intersect(validator_cols, names(df_breaks))
    
    df_plot_breaks <- df_breaks %>%
      dplyr::filter(has_breaks)
    
    if (only_valid_breaks) {
      if (length(validator_cols_present) == 0L) {
        warning(
          "`only_valid_breaks = TRUE`, but no validator columns were found in `df_breaks`. ",
          "Plotting all detected breaks (`has_breaks = TRUE`)."
        )
      } else if(valid_breaks_mode == "any"){
        df_plot_breaks <- df_plot_breaks %>%
          dplyr::filter(
            dplyr::if_any(
              dplyr::all_of(validator_cols_present),
              ~ dplyr::coalesce(.x, FALSE)
            )
          )
      }
      else if(valid_breaks_mode == "all"){
        df_plot_breaks <- df_plot_breaks %>%
          dplyr::filter(
            dplyr::if_all(
              dplyr::all_of(validator_cols_present),
              ~ dplyr::coalesce(.x, FALSE)
            )
          )
        
      }else{
        print("Invalid option in valid_breaks_mode")
      }
    }
    
    df_plot_breaks <- df_plot_breaks %>%
      dplyr::mutate(break_date = as.Date(break_date))
  }
  
  # Okabe-Ito Hex Codes
  c("#E69F00", "#56B4E9",
    "#009E73", "#F5C710",
    "#0072B2", "#D55E00",
    "#CC79A7")
  
  # Color palettes
  color_map_ts <- c(
    "Original SPI"                  = "#CFCDCD",
    "Daily interpolated"            = "#ABABAB",
    "Moving-wind. upp. quantile"    = "#000000",
    "Whittaker-smooth."             = "#D55E00",
    "Whittaker-smooth. mov. wind."  = "#0072B2"
  )
  
  color_map_algs <- c(
    "cpm"     = "#CC79A7",
    "ed"      = "#F0E442",
    "bfast01" = "#999999",
    "mcp"     = "#000000",
    "stc"     = "#882255",
    "wbs"     = "#117733"
  )
  
  base_line_types <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
  
  # Filter colors
  color_map_ts <- color_map_ts[names(color_map_ts) %in% unique(df_long$type)]
  
  algs_present <- character(0)
  if (!is.null(df_plot_breaks) && nrow(df_plot_breaks) > 0L) {
    algs_present <- unique(df_plot_breaks$algorithm)
  }
  
  color_map_algs <- color_map_algs[names(color_map_algs) %in% algs_present]
  line_types <- stats::setNames(
    base_line_types[seq_along(color_map_algs)],
    names(color_map_algs)
  )
  
  # Plot
  p <- ggplot(df_long, aes(x = ti, y = value)) +
    geom_line(aes(color = type), linewidth = 0.1, alpha = 0.6) +
    geom_point(aes(color = type), size = 0.1, alpha = 0.6)
  
  if (!is.null(df_plot_breaks) && nrow(df_plot_breaks) > 0L) {
    p <- p +
      geom_vline(
        data = df_plot_breaks,
        aes(xintercept = break_date, linetype = algorithm, color = algorithm),
        linewidth = 1,
        alpha = 0.6,
        show.legend = TRUE
      )
  }
  
  p <- p +
    scale_color_manual(
      name = "Time series",
      values = c(color_map_ts, color_map_algs),
      breaks = c(names(color_map_ts), names(color_map_algs))
    ) +
    labs(
      title = paste("Time Series of", get_spi(spidf_obj)),
      subtitle = paste0(
        "Location: [", round(get_longitude(spidf_obj), 5), ", ",
        round(get_latitude(spidf_obj), 5), "] | ",
        "Period: ", format(get_range_start(spidf_obj)), " to ",
        format(get_range_end(spidf_obj)), " | ",
        "Proc. Level: ", get_proc_level(spidf_obj),
        ifelse(tree_id != "" && tree_id != "---", paste0(" | Tree ID: ", tree_id), "")
      ),
      x = "Date",
      y = get_spi(spidf_obj)
    ) +
    theme_minimal(base_size = 15) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.spacing.y = grid::unit(0.2, "cm"),
      legend.title = element_text(face = "bold")
    )
  
  if (length(line_types) > 0L) {
    p <- p +
      scale_linetype_manual(
        name = "Break detection algorithm",
        values = line_types,
        breaks = names(color_map_algs)
      ) +
      guides(
        color = guide_legend(order = 1),
        linetype = guide_legend(order = 2)
      )
  } else {
    p <- p +
      guides(
        color = guide_legend(order = 1)
      )
  }
  
  return(p)
}



plot_valid_breaks <- function(df_breaks) {
  required_cols <- c("algorithm", "break_date", "break_magn", "has_valid_breaks_lt_med")
  stopifnot(all(required_cols %in% colnames(df_breaks)))
  
  df_valid <- df_breaks %>%
    dplyr::filter(has_valid_breaks_lt_med == TRUE, !is.na(break_date), !is.na(break_magn))
  
  # ---- Plot 1: Frequency of break dates ----
  freq_data <- df_valid %>%
    dplyr::group_by(break_date) %>%
    dplyr::summarise(freq = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(freq))
  
  p1 <- ggplot(freq_data, aes(x = reorder(as.character(break_date), freq), y = freq)) +
    geom_col(fill = "#69b3a2") +
    coord_flip() +
    scale_y_continuous(
      #breaks = scales::breaks_pretty(),
      breaks = seq(0, max(freq_data$freq, na.rm = TRUE), by = 1),
      labels = scales::label_number(accuracy = 1)
    ) +
    labs(
      x = "Break date",
      y = "Frequency",
      title = "Frequency of long-term median valid break dates"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.y = element_text(size = 10)
    )
  
  # ---- Plot 2: Violin + boxplot of break magnitudes by algorithm ----
  p2 <- ggplot(df_valid, aes(x = algorithm, y = break_magn)) +
    geom_violin(trim = FALSE, fill = "gray80", color = "black", alpha = 0.4) +
    geom_boxplot(width = 0.3, outlier.shape = NA, alpha = 0.3) +
    labs(
      x = "Algorithm",
      y = "Break magnitude",
      title = "Distribution of long-term median valid break magnitudes"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
  
  patchwork::wrap_plots(p1, p2, ncol = 2)
}
