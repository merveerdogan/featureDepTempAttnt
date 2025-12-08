library(tidyverse)
library(jsonlite)
library(shiny)
library(ggplot2)
library(dplyr)
library(circular)
library(cowplot)


folder <- "/Users/merve/Library/CloudStorage/OneDrive-YaleUniversity/Projects/Studies/featureDepTempAttnt/experiment/1-pilot/1-1pair/dataLocal/p1.2"
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


key_count_summary <- circular_all_data %>%
  group_by(participantID, trial, attendedFeature, tempo_pair) %>%
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
  group_by(participantID, attendedFeature, tempo_pair) %>%
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
      t1 = sub("-.*", "", tempo_pair),
      t2 = sub(".*-", "", tempo_pair),
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
    tabPanel("All Trials",
             checkboxInput("show_kde_avg", "Summary Lines", value = FALSE),
             plotOutput("avg_plot"),
             plotOutput("avg_summary"),
    ),
    # -----------------
    # TAB 2
    # -----------------
    tabPanel("Trial-by-Trial",
             sliderInput("trial_idx","Trial index",
                         min=1,
                         max=unique(conditions$total_trials),
                         value=1, step=1),
             plotOutput("trial_plot"),
             plotOutput("trial_summary")),
    # -----------------
    # TAB 3
    # -----------------
    tabPanel("Time Window Within Trial",
             sliderInput("angle_range","Keypress Event Range",
                         min=1, max=100,
                         value=c(1,5), step=1),
             plotOutput("event_plot"),
             plotOutput("event_summary")),
    # -----------------
    # TAB 4
    # -----------------
    tabPanel(
      "Participant Summary",
      sidebarLayout(
      sidebarPanel(
      h4("Select Participants"),
      
      # Select/Unselect all buttons
      fluidRow(
        column(6, actionButton("select_all", "Select All")),
        column(6, actionButton("unselect_all", "Unselect All"))
      ),
      br(),
      checkboxGroupInput(
        inputId = "selected_participants",
        label = "Participants:",
        choices = NULL,
        selected = NULL
      ),
      checkboxInput("show_hist", "Show raw histogram", value = TRUE),
      hr(),
      h4("Bandwidth (KDE smoothing)"),
      sliderInput(
        inputId = "bw",
        label = "Bandwidth:",
        min = 5, max = 50, value = 20, step = 1
      )
    ),
    mainPanel(
      plotOutput("circ_kde_plot", height = "600px")
    )
    )),
    # -----------------
    # TAB 5 - Participants Angle Means Summary
    # -----------------
    tabPanel(
      "Angle Means Summary",
      sidebarLayout(
        sidebarPanel(
          h4("Toggle Streams"),
          
          checkboxInput("show_size_att",  "Size – Attended",  value = TRUE),
          checkboxInput("show_size_dist", "Size – Distractor", value = TRUE),
          checkboxInput("show_color_att", "Color – Attended",  value = TRUE),
          checkboxInput("show_color_dist","Color – Distractor",value = TRUE),
        ),
        mainPanel(
          plotOutput("angle_mean_plot", height = "650px")
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
    
    dat <- filter_same_tempo(circular_long)
    
    # ---------- KDE (grouped by attendedFeature × tempo_pair × angle_type) ----------
    if (isTRUE(input$show_kde_avg)) {
      
      kde_df <- dat %>%
        group_by(attendedFeature, tempo_pair, angle_type) %>%
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
      facet_grid(attendedFeature ~ tempo_pair) +
      theme_minimal(base_size = 14) +
      theme(
        axis.ticks.y     = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey80"),
        strip.text       = element_text(size = 12, face = "bold"),
        panel.spacing    = unit(5, "lines"),
        axis.text.x      = element_text(margin = margin(t = 6))
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
      group_by(pn, attendedFeature, tempo_pair) %>%
      summarise(trials_sorted = sort(unique(trial)), .groups="drop") %>%
      group_by(pn, attendedFeature, tempo_pair) %>%
      mutate(idx=row_number()) %>%
      filter(idx == input$trial_idx) %>%
      inner_join(
        circular_long %>% mutate(pn = as.integer(factor(participantID))),
        by = c("pn","attendedFeature","tempo_pair","trials_sorted"="trial")
      )
    
    dat <- filter_same_tempo(dat0)
    
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
      facet_grid(attendedFeature ~ tempo_pair) +
      facet_grid(attendedFeature ~ tempo_pair) +
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
      group_by(participantID, trial, attendedFeature, tempo_pair, angle_type) %>%
      arrange(key_time) %>%
      mutate(k=row_number()) %>%
      filter(k >= input$angle_range[1],
             k <= input$angle_range[2])
    
    dat <- filter_same_tempo(dat0)
    
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
      facet_grid(attendedFeature ~ tempo_pair) +
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
  # TAB 4 - PARTICIPANT SUMMARY
  # --------------------------------------------------
  circular_long2 <- circular_long %>%
    mutate(att_status = case_when(
      attendedFeature == "color" & angle_type == "color_key_angles" ~ "attended",
      attendedFeature == "color" & angle_type == "size_key_angles" ~ "distractor",
      attendedFeature == "size"  & angle_type == "size_key_angles"  ~ "attended",
      attendedFeature == "size"  & angle_type == "color_key_angles" ~ "distractor",
      TRUE ~ NA
    ))
  
  # --- update participant selector ---
  observe({
    updateCheckboxGroupInput(
      session, "selected_participants",
      choices = sort(unique(circular_long2$participantID)),
      selected = unique(circular_long2$participantID)
    )
  })
  
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(
      session,
      "selected_participants",
      selected = circular_long2$participantID
    )
  })
  
  observeEvent(input$unselect_all, {
    updateCheckboxGroupInput(
      session,
      "selected_participants",
      selected = character(0)
    )
  })
  
  # KDE function (safe)
  compute_circ_density <- function(angle_vec, bw, participantID, attendedFeature, tempo_pair, att_status) {
    
    angle_vec <- angle_vec[!is.na(angle_vec)]
    
    if (length(angle_vec) == 0) {
      return(tibble(angle = numeric(0), density = numeric(0)))
    }
    
    ang_c <- circular(angle_vec, units = "degrees", template = "none", modulo = "2pi")
    
    d <- density.circular(
      ang_c,
      bw = bw,
      kernel = "vonmises",
      from = 0, to = 360,
      n = 360
    )
    
    tibble(
      participantID = participantID,
      attendedFeature = attendedFeature,
      tempo_pair = tempo_pair,
      att_status = att_status,
      angle = d$x,
      density = d$y
    )
  }
  
  raw_df <- reactive({
    req(input$selected_participants)
    circular_long2 %>%
      filter(participantID %in% input$selected_participants)
  })
  
  # --- reactive KDE summary (recomputed when participants OR bw change) ---
  summary_df <- reactive({
    req(input$selected_participants, input$bw)
    circular_long2 %>%
      filter(participantID %in% input$selected_participants) %>%
      group_by(participantID, attendedFeature, tempo_pair, att_status) %>%
      reframe(
        compute_circ_density(
          angle_vec = angles,
          bw = input$bw,
          participantID = first(participantID),
          attendedFeature = first(attendedFeature),
          tempo_pair = first(tempo_pair),
          att_status = first(att_status)
        )
      ) %>%
      ungroup()
  })
  
  # ---- plot ----
  output$circ_kde_plot <- renderPlot({
    
    df_kde  <- summary_df()     # smoothed KDE
    df_hist <- raw_df()         # raw angles for histograms
    validate(need(nrow(df_kde) > 0, "No data for selected participants."))
    
    p <- ggplot() +
      # --- RAW HISTOGRAM (toggle on/off) ------------------------
    { if (isTRUE(input$show_hist))
      geom_histogram(
        data = df_hist,
        aes(
          x = angles,
          fill = angle_type,
          group = interaction(participantID, att_status)
        ),
        binwidth = circularBinWidth,
        boundary = 0,
        closed   = "left",
        alpha    = 0.35,
        color    = "white",
        position = "identity"
      )
    } +
      # histogram colors same as your avg_plot
      labs(fill = "Reference Feature") +
      scale_fill_manual(
        values = c(
          "color_key_angles" = "#FF7F7F",
          "size_key_angles"  = "#66D1C8"
        ),
        labels = c(
          "color_key_angles" = "Color",
          "size_key_angles"  = "Size"
        )) +
      # --- KDE LINES -------------------------------------------
    geom_line(
      data = df_kde,
      aes(
        x = angle,
        y = density,
        color   = factor(participantID),
        linetype = att_status,
        group = interaction(participantID, att_status)
      ),
      linewidth = 1
    ) +
      
      scale_linetype_manual(values = c(attended = "solid", distractor = "dotted")) +
      # --- FACETS & POLAR ORIENTATION --------------------------
    facet_grid(attendedFeature ~ tempo_pair) +
      coord_polar(start = -pi/2, direction = 1) +
      scale_x_continuous(
        breaks = c(0, 90, 180, 270),
        limits = c(0, 360),
        labels = c("0°", "90°", "180°", "270°")) +
      
      # --- THEMING ---------------------------------------------
    theme_minimal(base_size = 14) +
      theme(
        axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        panel.spacing = unit(5, "lines")
      ) +
      
      labs(
        title = "Circular Distribution per Participant",
        subtitle = paste("Bandwidth =", input$bw),
        color = "Participant",
        fill  = "Raw Distribution",
        linetype = "Stream"
      )
    
    p
  })
  
  
  # --------------------------------------------------
  # TAB 5 - ANGLE MEANS SUMMARY (BAR GRAPH)
  # --------------------------------------------------
  
  # First compute stream category similar to att_status in Tab 4
  angle_means_df <- reactive({
    
    circular_long %>%
      mutate(
        stream = case_when(
          attendedFeature == "size"  & angle_type == "size_key_angles"  ~ "size_att",
          attendedFeature == "size"  & angle_type == "color_key_angles" ~ "size_dist",
          attendedFeature == "color" & angle_type == "color_key_angles" ~ "color_att",
          attendedFeature == "color" & angle_type == "size_key_angles"  ~ "color_dist",
          TRUE ~ NA
        )
      ) %>%
      group_by(participantID, tempo_pair, stream) %>%
      summarise(mean_angle = mean(angles, na.rm=TRUE), .groups="drop")
  })
  
  
  output$angle_mean_plot <- renderPlot({
    
    df <- angle_means_df()
    
    # Apply toggles to filter streams
    keep_streams <- c(
      if (input$show_size_att)  "size_att",
      if (input$show_size_dist) "size_dist",
      if (input$show_color_att) "color_att",
      if (input$show_color_dist)"color_dist"
    )
    
    df <- df %>% 
      filter(stream %in% keep_streams)
    
    validate(need(nrow(df) > 0, "No streams selected."))
    
    # readable labels for legend
    df$stream_label <- recode(df$stream,
                              "size_att" = "Size – Attended",
                              "size_dist" = "Size – Distractor",
                              "color_att" = "Color – Attended",
                              "color_dist" = "Color – Distractor"
    )
    
    ggplot(df, aes(x = tempo_pair, y = mean_angle, fill = stream_label)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      facet_wrap(~ participantID, ncol = 5) +
      theme_minimal(base_size = 14) +
      theme(
        panel.border = element_rect(color="black", fill=NA, linewidth = 0.8),
        strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))
      ) +
      labs(
        title = "Average Response Angle per Tempo Pair",
        x = "Tempo Pair",
        y = "Mean Angle (°)",
        fill = "Stream Type"
      )
  })
  
  
}



shinyApp(ui, server)


