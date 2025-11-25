library(tidyverse)
library(jsonlite)
library(shiny)


folder <- "/Users/merve/Library/CloudStorage/OneDrive-YaleUniversity/Projects/Studies/featureDepTempAttnt/experiment/1-pilot/1-1pair/data/filtered"
files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)

df <- purrr::map_df(files, ~ read.csv(.x, stringsAsFactors = FALSE, check.names = FALSE)) %>%
  mutate(trial = row_number())

extract_hue <- function(hsl_string) {
  as.numeric(str_extract(hsl_string, "(?<=hsl\\().+?(?=,)"))
}
parse_json <- function(x) {
  if (is.na(x) || x == "") {
    return(NULL)
  }
  tryCatch(fromJSON(x), error = function(e) NULL)
}

parsed <- df %>%
  rowwise() %>%
  mutate(
    sizeEvents = list({
      raw <- parse_json(sizeChangeLog)
      if (is.null(raw)) {
        return(tibble())
      }
      tibble(size_time = raw$timeMs, size_width = map_dbl(raw$rectSize, 1), size_height = map_dbl(raw$rectSize, 2))
    }),
    colorEvents = list({
      raw <- parse_json(colorChangeLog)
      if (is.null(raw)) {
        return(tibble())
      }
      tibble(color_time = raw$timeMs, hue = map_dbl(raw$colorHsl, extract_hue))
    }),
    keyEvents = list({
      raw <- parse_json(keypressLog)
      if (is.null(raw)) {
        return(tibble())
      }
      tibble(key_time = raw$key_time_ms %||% raw$time_ms)
    })
  ) %>%
  ungroup()

size_timing_df <- parsed %>%
  select(trial, attendedFeature, sizeTempo, colorTempo, sizeEvents) %>%
  unnest(sizeEvents)
color_timing_df <- parsed %>%
  select(trial, attendedFeature, sizeTempo, colorTempo, colorEvents) %>%
  unnest(colorEvents)
key_timing_df <- parsed %>%
  select(trial, attendedFeature, sizeTempo, colorTempo, keyEvents) %>%
  unnest(keyEvents)



size_big_timing <- size_timing_df %>%
  group_by(trial) %>%
  mutate(
    size_val = pmax(size_width, size_height),
    big_val = max(unique(size_val)),
    is_big = size_val == big_val
  ) %>%
  filter(is_big) %>%
  select(trial, size_time)

color_big_timing <- color_timing_df %>%
  group_by(trial) %>%
  mutate(
    big_hue = max(unique(hue)),
    is_big = hue == big_hue
  ) %>%
  filter(is_big) %>%
  select(trial, color_time)

pick_key <- function(keys, t) {
  win <- keys %>% filter(abs(key_time - t) <= 500)
  if (nrow(win) == 0) {
    return(NA_real_)
  }
  win %>%
    arrange(key_time) %>%
    slice(1) %>%
    pull(key_time)
}

rel_size <- filter(key_timing_df, attendedFeature == "size") %>%
  group_by(trial, sizeTempo, colorTempo) %>%
  group_modify(~ {
    keys <- .x
    s <- size_big_timing %>% filter(trial == .y$trial)
    if (nrow(s) == 0) {
      return(tibble())
    }
    tibble(
      size_change = s$size_time,
      key_selected = map_dbl(s$size_time, ~ pick_key(keys, .x)),
      rel_time = key_selected - size_change
    ) %>% filter(!is.na(rel_time))
  }) %>%
  ungroup()

rel_color <- filter(key_timing_df, attendedFeature == "color") %>%
  group_by(trial, sizeTempo, colorTempo) %>%
  group_modify(~ {
    keys <- .x
    c <- color_big_timing %>% filter(trial == .y$trial)
    if (nrow(c) == 0) {
      return(tibble())
    }
    tibble(
      color_change = c$color_time,
      key_selected = map_dbl(c$color_time, ~ pick_key(keys, .x)),
      rel_time = key_selected - color_change
    ) %>% filter(!is.na(rel_time))
  }) %>%
  ungroup()

rel_size <- rel_size %>% mutate(tempo_pair = paste0(sizeTempo, "-", colorTempo))
rel_color <- rel_color %>% mutate(tempo_pair = paste0(sizeTempo, "-", colorTempo))

# Set explicit order for tempo pairs
pairs4 <- c("350-400", "400-350", "450-500", "500-450")
rel_size$tempo_pair <- factor(rel_size$tempo_pair, levels = pairs4)
rel_color$tempo_pair <- factor(rel_color$tempo_pair, levels = pairs4)








size_color_pairs <- size_big_timing %>%
  group_by(trial) %>%
  group_modify(~ {
    s <- .x$size_time
    c <- color_big_timing %>%
      filter(trial == .y$trial) %>%
      pull(color_time)
    if (length(c) == 0) {
      return(tibble())
    }
    tibble(
      size_time = s,
      color_time = map_dbl(s, ~ c[which.min(abs(c - .x))]),
      size_color_delta = size_time - map_dbl(s, ~ c[which.min(abs(c - .x))])
    )
  }) %>%
  ungroup()




rel_key_size_vs_sc <- filter(key_timing_df, attendedFeature == "size") %>%
  group_by(trial, sizeTempo, colorTempo) %>%
  group_modify(~ {
    keys <- .x
    sc <- size_color_pairs %>% filter(trial == .y$trial)
    if (nrow(sc) == 0) {
      return(tibble())
    }
    tibble(
      size_color_delta = sc$size_color_delta,
      size_time = sc$size_time,
      key_selected = map_dbl(sc$size_time, ~ pick_key(keys, .x)),
      key_minus_size = map_dbl(sc$size_time, ~ {
        k <- pick_key(keys, .x)
        if (is.na(k)) {
          return(NA_real_)
        }
        k - .x
      })
    )
  }) %>%
  ungroup() %>%
  mutate(
    tempo_pair = paste0(sizeTempo, "-", colorTempo),
    tempo_pair = factor(tempo_pair, levels = pairs4)
  )

rel_key_color_vs_sc <- filter(key_timing_df, attendedFeature == "color") %>%
  group_by(trial, sizeTempo, colorTempo) %>%
  group_modify(~ {
    keys <- .x
    sc <- size_color_pairs %>% filter(trial == .y$trial)
    if (nrow(sc) == 0) {
      return(tibble())
    }
    tibble(
      size_color_delta = sc$size_color_delta,
      color_time = sc$color_time,
      key_selected = map_dbl(sc$color_time, ~ pick_key(keys, .x)),
      key_minus_color = map_dbl(sc$color_time, ~ {
        k <- pick_key(keys, .x)
        if (is.na(k)) {
          return(NA_real_)
        }
        k - .x
      })
    )
  }) %>%
  ungroup() %>%
  mutate(
    tempo_pair = paste0(sizeTempo, "-", colorTempo),
    tempo_pair = factor(tempo_pair, levels = pairs4)
  )

aligned_data <- rbind(rel_key_color_vs_sc,rel_key_color_vs_sc)

plot_df <- bind_rows(size_big_timing, color_big_timing, key_timing_df) %>%
  mutate(tempo_pair = factor(tempo_pair, levels = pairs4))

# -------------------------
# COMBINED SHINY APP WITH TABS
# -------------------------
ui <- fluidPage(
  titlePanel("Analysis Dashboard"),
  tabsetPanel(
    # Tab 1: Relative Onsets Distribution
    tabPanel(
      "Relative Onsets Distribution",
      sidebarLayout(
        sidebarPanel(
          sliderInput("binw", "Bin Width (ms):", min = 5, max = 200, value = 50, step = 5),
          sliderInput("range", "Target-Distractor Onset Difference Range (+-):", min = 50, max = 500, value = 200, step = 10)
        ),
        mainPanel(
          plotOutput("dyn_plot", height = "700px")
        )
      )
    ),
    # Tab 2: Raw Keypress Timeline
    tabPanel(
      "Raw Keypress Timeline",
      sidebarLayout(
        sidebarPanel(
          sliderInput("trial_sel", "Trial:",
            min = min(df$trial), max = max(df$trial), value = 1, step = 1
          ),
          sliderInput("time_window", "Time Window (ms):",
            min = 10, max = max(plot_df$time, na.rm = TRUE),
            value = max(plot_df$time, na.rm = TRUE), step = 10
          )
        ),
        mainPanel(
          plotOutput("p_raw", height = "700px")
        )
      )
    ),
    # Tab 3: Coefficient of Variation
    tabPanel(
      "Coefficient of Variation",
      plotOutput("cv_plot", height = "700px")
    )
  )
)

server <- function(input, output, session) {
  # Tab 1: Relative Onsets Distribution
  output$dyn_plot <- renderPlot({
    rng <- input$range
    bw <- input$binw

    df_zoom <- aligned_data %>%
      filter(attended_delta >= -rng, attended_delta <= rng) %>%
      mutate(
        bin = cut(attended_delta,
          breaks = seq(-rng, rng, by = bw),
          include.lowest = TRUE, right = FALSE
        )
      ) %>%
      group_by(attend_panel, tempo_pair, bin) %>%
      summarise(mean_rel = mean(attended_rel, na.rm = TRUE), .groups = "drop")

    df_zoom$attend_panel <- factor(df_zoom$attend_panel,
      levels = c("Size-Attended", "Color-Attended")
    )

    ggplot(df_zoom, aes(x = bin, y = mean_rel, color = attend_panel, group = 1)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      facet_grid(attend_panel ~ tempo_pair) +
      scale_color_manual(values = c(
        "Size-Attended" = "steelblue",
        "Color-Attended" = "darkred"
      )) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none"
      ) +
      labs(
        x = "Attended <U+2013> Unattended (dynamic bins)",
        y = "Mean Relative Keypress Time"
      )
  })

  # Tab 2: Raw Keypress Timeline
  output$p_raw <- renderPlot({
    d <- plot_df %>%
      filter(
        trial == input$trial_sel,
        time <= input$time_window
      ) %>%
      mutate(
        event_type = case_when(
          col == "orange" ~ "Size changes",
          col == "purple" ~ "Color changes",
          col == "black" ~ "Keypresses",
          TRUE ~ col
        )
      )

    ggplot(d) +
      geom_segment(aes(x = time, xend = time, y = y0, yend = y1, color = event_type), linewidth = 1.2) +
      scale_color_manual(
        values = c(
          "Size changes" = "orange",
          "Color changes" = "purple",
          "Keypresses" = "black"
        ),
        name = "Event Type"
      ) +
      facet_grid(attendedFeature ~ tempo_pair, scales = "free_x") +
      coord_cartesian(ylim = c(-0.25, 0.30)) +
      theme_bw() +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 10),
        legend.position = "right",
        plot.margin = margin(5, 5, 5, 5)
      ) +
      labs(x = "Time (ms)")
  })

  # Tab 3: Coefficient of Variation
  output$cv_plot <- renderPlot({
    ggplot(cv_df, aes(x = attendedFeature, y = CV, fill = bar_color)) +
      geom_col(width = 0.6) +
      scale_fill_identity() +
      facet_grid(. ~ tempo_pair) +
      theme_bw() +
      labs(x = NULL, y = "Coefficient of Variation (IKI)")
  })
}

shinyApp(ui, server)
