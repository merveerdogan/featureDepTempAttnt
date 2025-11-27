library(tidyverse)
library(jsonlite)
library(shiny)
library(ggplot2)
library(dplyr)



folder <- "/Users/merve/Library/CloudStorage/OneDrive-YaleUniversity/Projects/Studies/featureDepTempAttnt/experiment/1-pilot/1-1pair/data/filtered"
files  <- list.files(folder, pattern="\\.csv$", full.names=TRUE)

df <- purrr::map_df(files, ~ read.csv(.x, stringsAsFactors=FALSE, check.names=FALSE)) %>%
  mutate(trial = row_number())

# --------------------------------------------------
# HELPERS
# --------------------------------------------------

parse_json <- function(x) {
  if (is.na(x) || x == "") return(NULL)
  tryCatch(fromJSON(x), error=function(e) NULL)
}

extract_hue <- function(hsl_string) {
  as.numeric(str_extract(hsl_string, "(?<=hsl\\().+?(?=,)"))
}

keypress_angle <- function(k, events) {
  events <- sort(unique(events))
  
  if (length(events) < 2)
    return(rep(NA_real_, length(k)))
  
  # find interval each key belongs to
  idx <- findInterval(k, events, left.open = TRUE)
  
  # keys before first event or after last event → NA
  idx[idx == 0 | idx >= length(events)] <- NA
  
  angle <- rep(NA_real_, length(k))
  
  valid <- !is.na(idx)
  
  e1 <- events[idx[valid]]
  e2 <- events[idx[valid] + 1]
  
  angle[valid] <- 360 * (k[valid] - e1) / (e2 - e1)
  angle
}




# --------------------------------------------------
# PARSE LOGS
# --------------------------------------------------

parsed <- df %>%
  rowwise() %>%
  mutate(
    sizeEvents = list({
      raw <- parse_json(sizeChangeLog)
      if (is.null(raw)) return(NULL)
      tibble(
        size_time  = raw$timeMs,
        size_width = map_dbl(raw$rectSize, 1),
        size_height = map_dbl(raw$rectSize, 2)
      )
    }),
    
    colorEvents = list({
      raw <- parse_json(colorChangeLog)
      if (is.null(raw)) return(NULL)
      tibble(
        color_time = raw$timeMs,
        hue        = map_dbl(raw$colorHsl, extract_hue)
      )
    }),
    
    keyEvents = list({
      raw <- parse_json(keypressLog)
      if (is.null(raw)) return(NULL)
      tibble(
        key_time = if (!is.null(raw$key_time_ms)) raw$key_time_ms else raw$time_ms
      )
    })
  ) %>%
  ungroup()

# --------------------------------------------------
# UNNEST EACH EVENT TYPE CORRECTLY
# --------------------------------------------------

size_timing_df <- parsed %>%
  select(trial, attendedFeature, sizeTempo, colorTempo, sizeEvents) %>%
  unnest(sizeEvents)

color_timing_df <- parsed %>%
  select(trial, attendedFeature, sizeTempo, colorTempo, colorEvents) %>%
  unnest(colorEvents)

key_timing_df <- parsed %>%
  select(trial, attendedFeature, sizeTempo, colorTempo, keyEvents) %>%
  unnest(keyEvents)

# --------------------------------------------------
# BIG SIZE EVENTS
# --------------------------------------------------
size_big <- size_timing_df %>%
  group_by(trial) %>%
  filter(pmax(size_width, size_height) == max(pmax(size_width, size_height))) %>%
  select(trial, size_time) %>%
  left_join(df %>% select(trial, attendedFeature, sizeTempo, colorTempo), by="trial") %>%
  arrange(trial, size_time) %>%
  group_by(trial, attendedFeature, sizeTempo, colorTempo) %>%
  summarise(
    big_times  = list(sort(size_time)),
    key_times  = list(key_timing_df$key_time[key_timing_df$trial == unique(trial)]),
    size_key_angles = list(
      round(
        keypress_angle(
          key_timing_df$key_time[key_timing_df$trial == unique(trial)],
          sort(size_time)
        ),
        2
      )
    ),
    .groups = "drop"
  )



# --------------------------------------------------
# BIG COLOR EVENTS
# --------------------------------------------------

color_big <- color_timing_df %>%
  group_by(trial) %>%
  filter(hue == max(hue)) %>%
  select(trial, color_time) %>%
  left_join(df %>% select(trial, attendedFeature, sizeTempo, colorTempo), by="trial") %>%
  arrange(trial, color_time) %>%
  group_by(trial, attendedFeature, sizeTempo, colorTempo) %>%
  summarise(
    big_times  = list(sort(color_time)),
    key_times  = list(key_timing_df$key_time[key_timing_df$trial == unique(trial)]),
    color_key_angles = list(
      round(
        keypress_angle(
          key_timing_df$key_time[key_timing_df$trial == unique(trial)],
          sort(color_time)
        ),
        2
      )
    ),
    .groups = "drop"
  )

circular_all_data <- size_big %>%
  left_join(
    color_big %>% 
      select(trial, color_times = big_times, color_key_angles = color_key_angles),
    by = "trial"
  ) %>% 
  mutate(
    tempo_pair = ifelse(
      attendedFeature == "color",
      paste0(colorTempo, "-", sizeTempo),
      paste0(sizeTempo, "-", colorTempo)
    )
  )

conditions <- circular_all_data %>%
  group_by(attendedFeature, tempo_pair) %>%
  summarise(total_trials = n_distinct(trial), .groups="drop")


circular_long <- circular_all_data %>%
  select(trial, attendedFeature, tempo_pair, size_key_angles, color_key_angles) %>%
  mutate(
    size_key_angles  = map(size_key_angles, ~ .x),
    color_key_angles = map(color_key_angles, ~ .x)
  ) %>%
  pivot_longer(
    cols = c(size_key_angles, color_key_angles),
    names_to = "angle_type",
    values_to = "angles"
  ) %>%
  unnest(angles)



# --------------------------------------------------
# PLOT (works instantly)
# --------------------------------------------------
ggplot(circular_long, aes(x = angles, fill = angle_type)) +
  geom_histogram(binwidth = 10, alpha = 0.7, position = "identity") +
  
  # --- 0° at top, clockwise ---
  coord_polar(start = -pi/2, direction = 1) +
  
  scale_fill_manual(values = c(
    "size_key_angles"  = "steelblue",
    "color_key_angles" = "firebrick"
  )) +
  
  # --- set ANGLE labels (0°, 90°, 180°, 270°) ---
  scale_x_continuous(
    breaks = c(0, 90, 180, 270),
    labels = c("0°", "90°", "180°", "270°"),
    limits = c(0, 360)
  ) +
  
  # --- keep radial grid + numbers ---
  theme_minimal() +
  
  facet_grid(attendedFeature ~ tempo_pair) +
  
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1, "lines"),
    
    axis.title = element_blank(),   # no axis titles
    axis.text.y = element_text(size = 8),  # keep radial labels
    axis.text.x = element_text(size = 10), # keep angle labels
    axis.ticks = element_line()
  )


### INTERACTIVE PLOTS ###
ui <- fluidPage(
  tabsetPanel(
    tabPanel("All trials",
             plotOutput("avg_plot")
    ),
    tabPanel("Trial-byTrial",
             sliderInput("trial_idx","Trial index", min=1,  max = unique(conditions$total_trials), value = 1,step = 1),
             plotOutput("trial_plot")
    )
  )
)
server <- function(input, output, session){
  
  output$avg_plot <- renderPlot({
    ggplot(circular_long, aes(x = angles, fill = angle_type)) +
      geom_histogram(binwidth = 10, alpha = 0.7, position="identity") +
      coord_polar(start=-pi/2, direction=1) +
      scale_fill_manual(values=c("size_key_angles"="steelblue","color_key_angles"="firebrick")) +
      scale_x_continuous(breaks=c(0,90,180,270),
                         labels=c("0°","90°","180°","270°"),
                         limits=c(0,360)) +
      theme_minimal() +
      facet_grid(attendedFeature ~ tempo_pair)
  })
  
  output$trial_plot <- renderPlot({
    dat <- dat <- circular_long %>%
      group_by(attendedFeature, tempo_pair) %>%
      summarise(
        trials_sorted = sort(unique(trial)),
        .groups = "drop"
      ) %>%
      group_by(attendedFeature, tempo_pair) %>%
      mutate(cond_trial_index = row_number()) %>%
      filter(cond_trial_index == input$trial_idx) %>%
      inner_join(
        circular_long,
        by = c(
          "attendedFeature",
          "tempo_pair",
          "trials_sorted" = "trial"
        )
      )
    
    ggplot(dat, aes(x = angles, fill = angle_type)) +
      geom_histogram(binwidth = 10, alpha = 0.7, position="identity") +
      coord_polar(start=-pi/2, direction=1) +
      scale_fill_manual(values=c("size_key_angles"="steelblue","color_key_angles"="firebrick")) +
      scale_x_continuous(breaks=c(0,90,180,270),
                         labels=c("0°","90°","180°","270°"),
                         limits=c(0,360)) +
      theme_minimal() +
      facet_grid(attendedFeature ~ tempo_pair)
  })
}

shinyApp(ui, server)

