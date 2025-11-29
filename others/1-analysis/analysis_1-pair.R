library(tidyverse)
library(jsonlite)
library(shiny)
library(ggplot2)
library(dplyr)


folder <- "/Users/merve/Library/CloudStorage/OneDrive-YaleUniversity/Projects/Studies/featureDepTempAttnt/experiment/1-pilot/1-1pair/dataLocal/filtered"
files  <- list.files(folder, pattern="\\.csv$", full.names=TRUE)

df <- purrr::map_df(files, ~ read.csv(.x, stringsAsFactors = FALSE, check.names = FALSE))

df <- df %>%
  filter(trial_category == "test") %>%
  group_by(participantID) %>%
  mutate(trial = row_number()) %>%
  ungroup()

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
      if (is.null(raw)) NULL else
        tibble(
          size_time  = raw$timeMs,
          size_width = map_dbl(raw$rectSize, 1),
          size_height= map_dbl(raw$rectSize, 2)
        )
    }),
    colorEvents = list({
      raw <- parse_json(colorChangeLog)
      if (is.null(raw)) NULL else
        tibble(
          color_time = raw$timeMs,
          hue        = map_dbl(raw$colorHsl, extract_hue)
        )
    }),
    keyEvents = list({
      raw <- parse_json(keypressLog)
      if (is.null(raw)) NULL else
        tibble(
          key_time = if (!is.null(raw$key_time_ms)) raw$key_time_ms else raw$time_ms
        )
    })
  ) %>%
  ungroup()%>%
  mutate(participantID = as.character(as.integer(factor(participantID))))





# --------------------------------------------------
# UNNEST EACH EVENT TYPE CORRECTLY
# --------------------------------------------------

size_timing_df <- parsed %>%
  select(participantID, trial, attendedFeature, sizeTempo, colorTempo, sizeEvents) %>%
  unnest(sizeEvents)

color_timing_df <- parsed %>%
  select(participantID,trial, attendedFeature, sizeTempo, colorTempo, colorEvents) %>%
  unnest(colorEvents)

key_timing_df <- parsed %>%
  select(participantID, trial, attendedFeature, sizeTempo, colorTempo, keyEvents) %>%
  unnest(keyEvents)

# --------------------------------------------------
# BIG SIZE EVENTS
# --------------------------------------------------
size_big <- size_timing_df %>%
  group_by(participantID, trial) %>%
  filter(pmax(size_width, size_height) ==
           max(pmax(size_width, size_height))) %>%
  ungroup() %>%
  arrange(participantID, trial, size_time) %>%
  mutate(
    tempo_pair = ifelse(
      attendedFeature == "color",
      paste0(colorTempo, "-", sizeTempo),
      paste0(sizeTempo, "-", colorTempo)
    ))  %>%
  group_by(participantID, trial, attendedFeature, tempo_pair) %>%
  summarise(
    big_times = list(sort(size_time)),
    key_times = list(
      key_timing_df$key_time[
        key_timing_df$participantID == participantID &
          key_timing_df$trial == trial
      ]
    ),
    size_key_angles = list(
      round(
        keypress_angle(
          key_timing_df$key_time[
            key_timing_df$participantID == participantID &
              key_timing_df$trial == trial
          ],
          sort(size_time)
        ), 2
      )
    ),
    .groups = "drop"
  )





# --------------------------------------------------
# BIG COLOR EVENTS
# --------------------------------------------------
color_big <- color_timing_df %>%
  group_by(participantID, trial) %>%
  filter(hue == max(hue)) %>%
  ungroup() %>%
  arrange(participantID, trial, color_time) %>%
  mutate(
    tempo_pair = ifelse(
      attendedFeature == "color",
      paste0(colorTempo, "-", sizeTempo),
      paste0(sizeTempo, "-", colorTempo)
    ))  %>%
  group_by(participantID, trial, attendedFeature, tempo_pair) %>%
  summarise(
    color_times  = list(sort(color_time)),
    key_times  = list(
      key_timing_df$key_time[
        key_timing_df$participantID == participantID &
          key_timing_df$trial == trial
      ]
    ),
    color_key_angles = list(
      round(
        keypress_angle(
          key_timing_df$key_time[
            key_timing_df$participantID == participantID &
              key_timing_df$trial == trial
          ],
          sort(color_time)
        ), 2
      )
    ),
    .groups = "drop"
  )



circular_all_data <- size_big %>%
  left_join(
    color_big %>%
      select(participantID, trial, 
             color_times,
             color_key_angles = color_key_angles),
    by = c("participantID","trial")        # <-- FIX: join on BOTH keys
  )


conditions <- circular_all_data %>%
  group_by(participantID, attendedFeature, tempo_pair) %>%
  summarise(total_trials = n_distinct(trial), .groups="drop")


circular_long <- circular_all_data %>%
  select(participantID, trial, attendedFeature, tempo_pair,
         size_key_angles, color_key_angles, key_times) %>%
  mutate(
    size_key_angles  = map(size_key_angles, ~ tibble(angles = .x)),
    color_key_angles = map(color_key_angles, ~ tibble(angles = .x)),
    key_times        = map(key_times, ~ tibble(key_time = .x))
  ) %>%
  pivot_longer(
    cols = c(size_key_angles, color_key_angles),
    names_to = "angle_type",
    values_to = "angle_df"
  ) %>%
  mutate(
    combined = map2(angle_df, key_times, ~ bind_cols(.x, .y))
  ) %>%
  select(-angle_df, -key_times) %>%
  unnest(combined)


  



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
    tabPanel("Raw Distribution",
             plotOutput("raw_dist")
    ),
    tabPanel("All trials",
             plotOutput("avg_plot")
    ),
    tabPanel("Trial-byTrial",
             sliderInput("trial_idx","Trial index", min=1,  max = unique(conditions$total_trials), value = 1,step = 1),
             plotOutput("trial_plot")
    ),
    tabPanel("Time Window Within Trial",
             sliderInput("angle_range",
                         "Keypress Event Range",
                         min = 1,
                         max = 100,
                         value = c(1,5),
                         step = 1),
             plotOutput("angle_plot")
    ),
    tabPanel("Time Window Within Trial - v2",
             fluidRow(
               column(
                 width = 6,
                 div(style="padding-right:20px;",
                     sliderInput("win_size",
                                 "Window size (keypress events):",
                                 min = 1, max = 50,
                                 value = 20, step = 1)
                 )
               ),
               column(
                 width = 6,
                 div(style="padding-left:20px;",
                     sliderInput("win_start",
                                 "Window start index:",
                                 min = 1, max = 80,
                                 value = 1, step = 1)
                 )
               )
             ),
             plotOutput("moving_window_plot")
    )
  )
)

server <- function(input, output, session){
  output$raw_dist <- renderPlot({
    
    N=length(unique(dat$participantID))
    ggplot(circular_long, aes(x = key_time, fill = attendedFeature)) +
      geom_dotplot() +
      scale_fill_manual(values = c("color"="firebrick","size"="steelblue")) +
      theme_minimal() +
      facet_grid(
        attendedFeature ~ tempo_pair,
        labeller = labeller(
          tempo_pair = function(x) {
            paste0("target ", sub("-.*","",x),
                   "  -  distractor ", sub(".*-","",x),"")
          }
        )
      ) +
      theme(strip.text = element_text(size=12))+
      ggtitle(paste0("N=", N)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold",size = 18))
    })
  
  
  output$avg_plot <- renderPlot({
   
    ggplot(circular_long, aes(x = angles, fill = angle_type)) +
      geom_histogram(binwidth = 10, alpha = 0.7, position="identity") +
      coord_polar(start=-pi/2, direction=1) +
      scale_fill_manual(
        name = "Key time as a reference to",
        values = c("size_key_angles" = "steelblue",
                   "color_key_angles" = "firebrick"),
        labels = c("size_key_angles" = "Size",
                   "color_key_angles" = "Color")
      ) +
      scale_x_continuous(
        breaks = c(0,90,180,270),
        labels = c("0°","90°","180°","270°"),
        limits = c(0,360)
      ) +
      theme_minimal() +
      facet_grid(
        attendedFeature ~ tempo_pair,
        labeller = labeller(
          tempo_pair = function(x) {
            paste0("target ", sub("-.*","",x),
                   "  -  distractor ", sub(".*-","",x))
          }
        )
      ) +
      theme(
        strip.text = element_text(size = 12),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
      ) +
      ggtitle(paste0("N=", N)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18))
  })
  
  
  output$trial_plot <- renderPlot({
    dat <- circular_long %>%
      arrange(participantID) %>%
      mutate(participant_num = as.integer(factor(participantID))) %>%
      group_by(participant_num, attendedFeature, tempo_pair) %>%
      summarise(
        trials_sorted = sort(unique(trial)),
        .groups = "drop"
      ) %>%
      group_by(participant_num, attendedFeature, tempo_pair) %>%
      mutate(cond_trial_index = row_number()) %>%
      filter(cond_trial_index == input$trial_idx) %>%
      inner_join(
        circular_long %>%
          mutate(participant_num = as.integer(factor(participantID))),
        by = c(
          "participant_num",
          "attendedFeature",
          "tempo_pair",
          "trials_sorted" = "trial"
        )
      )
    
    ggplot(circular_long, aes(x = angles, fill = angle_type)) +
      geom_histogram(binwidth = 10, alpha = 0.7, position = "identity") +
      coord_polar(start = -pi/2, direction = 1) +
      scale_fill_manual(
        name = "Key time as a reference to",
        values = c("size_key_angles" = "steelblue",
                   "color_key_angles" = "firebrick"),
        labels = c("size_key_angles" = "Size",
                   "color_key_angles" = "Color")
      ) +
      scale_x_continuous(
        breaks = c(0,90,180,270),
        labels = c("0°","90°","180°","270°"),
        limits = c(0,360)
      ) +
      theme_minimal() +
      facet_grid(
        attendedFeature ~ tempo_pair,
        labeller = labeller(
          tempo_pair = function(x) {
            paste0("target ", sub("-.*","",x),
                   "  -  distractor ", sub(".*-","",x))
          }
        )
      ) +
      theme(
        strip.text = element_text(size = 12),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
      ) +
      ggtitle(paste0("N=", N)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18))
    
  })
  
  
  ### TAB 3 ###
  output$angle_plot <- renderPlot({
    dat <- circular_long %>%
      group_by(participantID,trial, attendedFeature, tempo_pair) %>%
      arrange(angles) %>%
      mutate(angle_index = row_number()) %>%
      filter(angle_index >= input$angle_range[1],
             angle_index <= input$angle_range[2])
    
    ggplot(dat, aes(x = angles, fill = angle_type)) +
      geom_histogram(binwidth = 10, alpha = 0.7, position="identity") +
      coord_polar(start = -pi/2, direction = 1) +
      scale_fill_manual(
        name = "Key time as a reference to",
        values = c("size_key_angles" = "steelblue",
                   "color_key_angles" = "firebrick"),
        labels = c("size_key_angles" = "Size",
                   "color_key_angles" = "Color")
      ) +
      scale_x_continuous(
        breaks = c(0,90,180,270),
        labels = c("0°","90°","180°","270°"),
        limits = c(0,360)
      ) +
      theme_minimal() +
      facet_grid(
        attendedFeature ~ tempo_pair,
        labeller = labeller(
          tempo_pair = function(x) {
            paste0("target ", sub("-.*","",x),
                   "  -  distractor ", sub(".*-","",x))
          }
        )
      ) +
      theme(
        strip.text = element_text(size = 12),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
      ) +
      ggtitle(paste0("N=", N)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18))
  })
  
  
  ###TAB 4 ###
  output$moving_window_plot <- renderPlot({
    
    win_start <- input$win_start
    win_end   <- input$win_start + input$win_size - 1
    
    dat <- circular_long %>%
      group_by(participantID, trial, attendedFeature, tempo_pair) %>%
      arrange(key_time) %>%                          # order by time
      mutate(key_index = row_number()) %>%          # index keypress events
      filter(key_index >= win_start,
             key_index <= win_end)
    
    ggplot(dat, aes(x = angles, fill = angle_type)) +
      geom_histogram(binwidth = 10, alpha = 0.7, position="identity") +
      coord_polar(start = -pi/2, direction = 1) +
      scale_fill_manual(
        name = "Key time as a reference to",
        values = c("size_key_angles" = "steelblue",
                   "color_key_angles" = "firebrick"),
        labels = c("size_key_angles" = "Size",
                   "color_key_angles" = "Color")
      ) +
      scale_x_continuous(
        breaks = c(0,90,180,270),
        labels = c("0°","90°","180°","270°"),
        limits = c(0,360)
      ) +
      theme_minimal() +
      facet_grid(
        attendedFeature ~ tempo_pair,
        labeller = labeller(
          tempo_pair = function(x) {
            paste0("target ", sub("-.*","",x),
                   "  -  distractor ", sub(".*-","",x))
          }
        )
      ) +
      theme(
        strip.text = element_text(size = 12),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
      ) +
      ggtitle(paste0("N=", N)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18))
  })
}

shinyApp(ui, server)

