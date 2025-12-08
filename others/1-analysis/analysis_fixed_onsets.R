library(tidyverse)
library(jsonlite)
library(shiny)
library(ggplot2)
library(dplyr)
library(circular)
library(cowplot)


folder <- "/Users/merve/Library/CloudStorage/OneDrive-YaleUniversity/Projects/Studies/featureDepTempAttnt/experiment/1-pilot/1-1pair/dataLocal/p1.3"
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
# UNNEST EACH EVENT TYPE 
# --------------------------------------------------

size_timing_df <- parsed %>%
  select(participantID, trial, attendedFeature, startTimeDiff, sizeEvents) %>%
  unnest(sizeEvents)

color_timing_df <- parsed %>%
  select(participantID,trial, attendedFeature, startTimeDiff, colorEvents) %>%
  unnest(colorEvents)

key_timing_df <- parsed %>%
  select(participantID, trial, attendedFeature, startTimeDiff, keyEvents) %>%
  unnest(keyEvents)

# --------------------------------------------------
# BIG SIZE EVENTS
# --------------------------------------------------
size_big <- size_timing_df %>%
  group_by(participantID, trial) %>%
  filter(pmax(size_width, size_height) ==
           max(pmax(size_width, size_height))) %>%
  ungroup() %>%
  arrange(participantID, trial, size_time)   %>%
  group_by(participantID, trial, attendedFeature, startTimeDiff) %>%
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
  group_by(participantID, trial, attendedFeature, startTimeDiff) %>%
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
  group_by(participantID, attendedFeature, startTimeDiff) %>%
  summarise(total_trials = n_distinct(trial), .groups="drop")


circular_long <- circular_all_data %>%
  select(participantID, trial, attendedFeature, startTimeDiff,
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

circular_long2 <- circular_long %>%
  mutate(att_status = case_when(
    attendedFeature == "color" & angle_type == "color_key_angles" ~ "attended",
    attendedFeature == "color" & angle_type == "size_key_angles" ~ "distractor",
    attendedFeature == "size"  & angle_type == "size_key_angles"  ~ "attended",
    attendedFeature == "size"  & angle_type == "color_key_angles" ~ "distractor",
    TRUE ~ NA
  ))




key_count_summary <- circular_all_data %>%
  group_by(participantID, trial, attendedFeature, startTimeDiff) %>%
  summarise(
    key_count = lengths(key_times),
    size_count = lengths(big_times),
    color_count = lengths(color_times),
    .groups = "drop"
  )




# --------------------------------------------------
# PLOTTING
# --------------------------------------------------

circularBinWidth <- 30

fixed_max_count <- circular_long %>%
  group_by(participantID, attendedFeature, startTimeDiff) %>%
  reframe({
    p <- ggplot(cur_data(), aes(x = angles)) +
      geom_histogram(binwidth = circularBinWidth, boundary = 0, closed = "left")
    tibble(m = max(ggplot_build(p)$data[[1]]$count))
  }) %>%
  pull(m) %>%
  max()


filter_same_tempo <- function(df) {
  df %>%
    mutate(
      t1 = sub("-.*", "", startTimeDiff),
      t2 = sub(".*-", "", startTimeDiff),
      same_tempo = (t1 == t2),
      target_type = ifelse(attendedFeature == "color",
                           "color_key_angles",
                           "size_key_angles")
    ) %>%
    filter(!same_tempo | angle_type == target_type)
}





# --------------------------------------------------
# UI - FOR TABS
# --------------------------------------------------
ui <- fluidPage(
  tabsetPanel(
    # -----------------
    # TAB 1
    # -----------------
    tabPanel("Distribution - All",
             checkboxInput("show_kde_avg", "Summary Lines", value = FALSE),
             plotOutput("avg_plot"),
             plotOutput("avg_summary"),
    ),
    # -----------------
    # TAB 2
    # -----------------
    tabPanel("Distribution - Trial-by-Trial",
             sliderInput("trial_idx","Trial index",
                         min=1,
                         max=unique(conditions$total_trials),
                         value=1, step=1),
             plotOutput("trial_plot"),
             plotOutput("trial_summary")),
    # -----------------
    # TAB 3
    # -----------------
    tabPanel("Distribution - Within Trial",
             sliderInput("angle_range","Keypress Event Range",
                         min=1, max=100,
                         value=c(1,5), step=1),
             plotOutput("event_plot"),
             plotOutput("event_summary")),

  # -----------------
  # TAB 4 - Phase vs Onset Difference
  # -----------------
  tabPanel("Phase Average",
        plotOutput("phase_onset_plot", height = "650px")
    ),
  # -----------------
  # TAB 6 - Phase vs Onset Difference
  # -----------------
  tabPanel(
    "Participant Phase Average",
    sidebarLayout(
      sidebarPanel(
        h4("Key Event Selection"),
        sliderInput("key_event_idx",
                    "Select Key Event Number:",
                    min = 1, max = 100,
                    value = 1, step = 1),
      ),
      
      mainPanel(
        plotOutput("participant_phase_plot", height = "800px")
      )
    )
  )
  )
)



server <- function(input, output, session) {
  # --------------------------------------------------
  # TAB 1 - All Trials
  # --------------------------------------------------
  compute_circ_density_avg <- function(angle_vec, bw = 20) {
    angle_vec <- angle_vec[!is.na(angle_vec)]
    if (length(angle_vec) == 0) return(NULL)
    
    ang_c <- circular(angle_vec, units = "degrees", template = "none", modulo = "2pi")
    
    d <- density.circular(
      ang_c,
      bw     = bw,
      kernel = "vonmises",
      from   = 0,
      to     = 360,
      n      = 360
    )
    
    tibble(angle = d$x, density = d$y)
  }
  
  output$avg_plot <- renderPlot({
    dat <- circular_long
    # ---------- KDE (grouped by attendedFeature × startTimeDiff × angle_type) ----------
    if (isTRUE(input$show_kde_avg)) {
      kde_df <- dat %>%
        group_by(attendedFeature, startTimeDiff, angle_type) %>%
        reframe(
          compute_circ_density_avg(angles)
        ) %>%
        ungroup()
    }
    
    # ---------- Base histogram ----------
    p <- ggplot(dat, aes(x = angles, fill = angle_type)) +
      geom_histogram(
        aes(y = after_stat(density)),
        binwidth = circularBinWidth,
        boundary = 0,
        closed   = "left",
        alpha    = 0.6,
        position = "identity",
        color    = "white"
      ) +
      labs(fill = "Reference Feature") +
      scale_fill_manual(
        values = c(
          "color_key_angles" = "#FF7F7F",
          "size_key_angles"  = "#66D1C8"
        ),
        labels = c(
          "color_key_angles" = "Color",
          "size_key_angles"  = "Size"
        )
      ) +
      coord_polar(start = -pi/2, direction = 1) +
      scale_x_continuous(
        breaks = c(0, 90, 180, 270),
        limits = c(0, 360),
        labels = c("0°", "90°", "180°", "270°")
      ) +
      facet_grid(attendedFeature ~ startTimeDiff) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey80"),
        strip.text       = element_text(size = 12, face = "bold"),
        panel.spacing    = unit(5, "lines"),
        axis.text.x      = element_text(margin = margin(t = 6)),
        axis.text.y  = element_blank(),   # removes numbers
        axis.ticks.y = element_blank()    # removes tick marks
      )
    
    # ---------- Add KDE lines if toggled ----------
    if (isTRUE(input$show_kde_avg)) {
      p <- p +
        geom_line(
          data = kde_df,
          aes(x = angle,
              y = density,
              color = angle_type,
              group = angle_type),
          linewidth = 1.1,
          alpha = 0.9
        ) +
        scale_color_manual(
          values = c(
            "color_key_angles" = "#FF4C4C",
            "size_key_angles"  = "#00B0A4"
          ),
          labels = c(
            "color_key_angles" = "Color KDE",
            "size_key_angles"  = "Size KDE"
          )
        ) +
        labs(color = "KDE")
    }
    
    p
  })
  
  
  # --------------------------------------------------
  # TAB 2 - Trial-by-Trial
  # --------------------------------------------------
  output$trial_plot <- renderPlot({
    dat0 <- circular_long %>%
      mutate(pn = as.integer(factor(participantID))) %>%
      group_by(pn, attendedFeature, startTimeDiff) %>%
      summarise(trials_sorted = sort(unique(trial)), .groups="drop") %>%
      group_by(pn, attendedFeature, startTimeDiff) %>%
      mutate(idx=row_number()) %>%
      filter(idx == input$trial_idx) %>%
      inner_join(
        circular_long %>% mutate(pn = as.integer(factor(participantID))),
        by = c("pn","attendedFeature","startTimeDiff","trials_sorted"="trial")
      )
    
    dat <- dat0
    
    ggplot(dat, aes(x = angles, fill = angle_type)) +
      geom_histogram(
        binwidth = circularBinWidth,
        boundary=0,
        closed   = "left",
        alpha    = 0.6,
        position = "identity",
        color    = "white",
      ) +
      labs(fill = "Reference Feature") +
      scale_fill_manual(
        values = c(
          "color_key_angles" = "#FF7F7F",
          "size_key_angles"  = "#66D1C8"
        ),
        labels = c(
          "color_key_angles" = "Color",
          "size_key_angles"  = "Size"
        )
      ) +
      coord_polar(start = -pi/2, direction = -1) +
      scale_x_continuous(
        breaks = c(0, 90, 180, 270),
        limits = c(0, 360),
        labels = c("0°", "90°", "180°", "270°"))+
      facet_grid(attendedFeature ~ startTimeDiff) +
      facet_grid(attendedFeature ~ startTimeDiff) +
      theme_minimal(base_size = 14) +
      theme(
        axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey80"),
        strip.text       = element_text(size = 12, face = "bold"),
        panel.spacing    = unit(5, "lines"),
        axis.text.x      = element_text(margin = margin(t = 6)),
      )
  })
  
  
  # --------------------------------------------------
  # TAB 3 - Event Window
  # --------------------------------------------------
  output$event_plot <- renderPlot({
    dat0 <- circular_long %>%
      group_by(participantID, trial, attendedFeature, startTimeDiff, angle_type) %>%
      arrange(key_time) %>%
      mutate(k=row_number()) %>%
      filter(k >= input$angle_range[1],
             k <= input$angle_range[2])
    
    dat <- dat0
    
    ggplot(dat, aes(x = angles, fill = angle_type)) +
      geom_histogram(
        binwidth = circularBinWidth,
        boundary=0,
        closed   = "left",
        alpha    = 0.6,
        position = "identity",
        color    = "white",
      ) +
      scale_fill_manual(values=c(
        "color_key_angles"="#FF7F7F",
        "size_key_angles"="#66D1C8"
      )) +
      coord_polar(start=-pi/2, direction=1) +
      scale_x_continuous(limits=c(0,360),
                         breaks=c(0,90,180,270),
                         labels=c("0°","90°","180°","270°")) +
      facet_grid(attendedFeature ~ startTimeDiff) +
      theme_minimal(base_size = 14) +
      theme(
        axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey80"),
        strip.text       = element_text(size = 12, face = "bold"),
        panel.spacing    = unit(5, "lines"),
        axis.text.x      = element_text(margin = margin(t = 6)),
      )
  })
  
  
  # --------------------------------------------------
  # TAB 4 - Circular Phase Average Only
  # --------------------------------------------------

  # Circular summary per onset difference
  df_phase_summary <- 
  circular_long2 %>%
  filter(!is.na(angles), att_status == "attended") %>%
  group_by(attendedFeature, startTimeDiff) %>%
  summarise(
    mean_phase = mean.circular(
      circular(angles, units = "degrees", modulo = "2pi")
    ) %>% as.numeric(),

    sd_phase = sd.circular(
      circular(angles, units = "degrees", modulo = "2pi")
    ) %>% as.numeric(),

    .groups = "drop"
  )


  
  output$phase_onset_plot <- renderPlot({
    
    ggplot(df_phase_summary, aes(x = startTimeDiff, y = mean_phase, color = attendedFeature)) +
      
      # --- THEMING ---
      theme_minimal(base_size = 15) +
      theme(
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      
      scale_y_continuous(
        limits = c(0, 360),
        breaks = c(0, 90, 180, 270),
        labels = c("0°","90°","180°","270°")
      ) +
      scale_x_continuous(
        breaks = sort(unique(df$startTimeDiff)),
        labels = sort(unique(df$startTimeDiff))
      )+
      scale_color_manual(
        values = c(
          "color" = "#FF7F7F",
          "size"  = "#66D1C8"
        ),
        labels = c(
          "color" = "Color Attended",
          "size"  = "Size Attended"
        )
      ) +
  
      labs(
        x = "Target–Distractor Onset Difference (ms)",
        y = "Circular Phase (°)",
        title = "Circular Mean Phase",
        subtitle = "Averaged across all participants and trials",
        color = "Attended Feature"
      ) +
        list(
          geom_line(linewidth = 1.5),
          geom_point(size = 4)
        )
  })
  

  
  # --------------------------------------------------
  # TAB 5 - Participant Summary
  # --------------------------------------------------
  
  df_phase_individual <- reactive({
    idx <- input$key_event_idx
    
    filter(circular_long2, att_status== 'attended') %>%
      group_by(participantID, trial) %>%
      arrange(key_time) %>%
      mutate(event_idx = row_number()) %>%
      filter(event_idx == idx) %>%     # <-- choose Nth keypress
      ungroup() %>%
      mutate(phase_deg = angles %% 360) %>%    # angles = phase
      select(participantID, attendedFeature, startTimeDiff, phase_deg)
  })
  
  df_phase_individual_summary <- reactive({
    df_phase_individual() %>%
      group_by(participantID, attendedFeature, startTimeDiff) %>%
      summarise(
        mean_phase = mean.circular(
          circular(phase_deg, units="degrees", modulo="2pi")
        ) %>% as.numeric(),
        
        sd_phase = sd.circular(
          circular(phase_deg, units="degrees", modulo="2pi")
        ) %>% as.numeric(),
        
        .groups = "drop"
      )
  })
  
  
  output$participant_phase_plot <- renderPlot({
    
    df <- df_phase_individual_summary()
    
    ggplot(df, aes(x = startTimeDiff,
                   y = mean_phase,
                   color = attendedFeature)) +
        list(
          geom_line(linewidth = 1.2),
          geom_point(size = 3))+
      facet_wrap(~ participantID, ncol = 5) +
      scale_color_manual(
        values = c(
          "color" = "#FF7F7F",
          "size"  = "#66D1C8"
        ),
        labels = c(
          "color" = "Color Attended",
          "size"  = "Size Attended"
        )
      ) +
      scale_y_continuous(
        limits = c(0, 360),
        breaks = c(0, 90, 180, 270),
        labels = c("0°","90°","180°","270°")
      ) +
      scale_x_continuous(
        breaks = sort(unique(df$startTimeDiff)),
        labels = sort(unique(df$startTimeDiff))
      )+
      labs(
        x = "Onset Difference (ms)",
        y = "Circular Phase (°)",
        title = paste0("Participant Phase (Key Event ", input$key_event_idx, ")"),
        color = "Feature"
      ) +
      
      theme_minimal(base_size = 14) +
      theme(
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text   = element_text(size = 12, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        panel.spacing = unit(1.5, "lines")
      )
  })
  
  
}



shinyApp(ui, server)


