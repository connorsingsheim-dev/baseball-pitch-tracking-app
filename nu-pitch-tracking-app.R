library(shiny)
library(dplyr)
library(tibble)

# Sample roster 

PITCHERS <- c(
  "Pitcher 1",
  "Pitcher 2",
  "Pitcher 3",
  "Pitcher 4",
  "Pitcher 5"
)


# USER INTERFACE

ui <- fluidPage(
  
  tags$div(
    style = "
      width:100%;
      display:flex;
      justify-content:center;
      border-bottom:2px solid #4E2A84;
      padding:12px 0;
      margin-bottom:15px;
    ",
    
    tags$div(
      style = "
        width:100%;
        max-width:1200px;
        display:flex;
        align-items:center;
        justify-content:center;
        gap:14px;
      ",
      
      tags$div(
        tags$div(
          "Compete Stats",
          style = "font-size:26px; font-weight:700; text-align:center;"
        ),
        tags$div(
          "Northwestern Baseball",
          style = "
      font-size:14px;
      opacity:0.75;
      text-align:center;
    "
        )
      )
    )
  ),
  
  
  sidebarLayout(
    
    sidebarPanel(
      
      textInput(
        "week",
        "Week label",
        value = "Week 1"
      ),
      
      textInput(
        "outing_id",
        "Outing/Game ID",
        value = "Scrim1"
      ),
      
      dateInput(
        "date",
        "Date",
        value = Sys.Date()
      ),
      
      selectInput(
        "nu_side",
        "Northwestern is:",
        choices = c("Home", "Away"),
        selected = "Home"
      ),
      
      numericInput(
        "home_runs",
        "Home Runs",
        value = 0,
        min = 0
      ),
      
      numericInput(
        "away_runs",
        "Away Runs",
        value = 0,
        min = 0
      ),
      
      selectInput(
        "pitcher",
        "Pitcher",
        choices = PITCHERS,
        selected = PITCHERS[1]
      ),
      
      numericInput(
        "inning",
        "Inning",
        value = 1,
        min = 1,
        max = 20,
        step = 1
      ),
      
      selectInput(
        "half",
        "Half-inning",
        choices = c("Top", "Bottom"),
        selected = "Top"
      ),
      
      hr(),
      
      h4("Pitch buttons"),
      
      actionButton(
        "btn_ball",
        "Ball"
      ),
      
      actionButton(
        "btn_cs",
        "Called Strike"
      ),
      
      actionButton(
        "btn_sw",
        "Swinging Strike"
      ),
      
      actionButton(
        "btn_foul",
        "Foul"
      ),
      
      actionButton(
        "btn_inplay_out",
        "In-play Out"
      ),
      
      actionButton(
        "btn_inplay_hit",
        "In-play Hit"
      ),
      
      actionButton(
        "btn_hbp",
        "HBP"
      ),
      
      br(),
      br(),
      
      h4("Undo"),
      
      actionButton(
        "undo_pitch",
        "Undo last pitch"
      ),
      
      actionButton(
        "undo_pa",
        "Undo last PA / out"
      ),
      
      br(),
      br(),
      
      h4("Plate appearance outcome"),
      
      selectInput(
        "pa_outcome",
        "Outcome of PA",
        choices = c(
          "K",
          "BB",
          "HBP",
          "1B",
          "2B",
          "3B",
          "HR",
          "Out",
          "ROE"
        ),
        selected = "Out"
      ),
      
      numericInput(
        "runs_scored",
        "Runs scored on this PA (batting team)",
        value = 0,
        min = 0
      ),
      
      numericInput(
        "er_scored",
        "Earned runs on this PA (pitcher on mound)",
        value = 0,
        min = 0
      ),
      
      numericInput(
        "outs_this_pa",
        "Outs on this PA",
        value = 0,
        min = 0,
        max = 3
      ),
      
      actionButton(
        "end_pa",
        "End PA (save batter)"
      ),
      
      hr(),
      
      h4("Inning control"),
      
      actionButton(
        "next_inning",
        "Next Inning (Top)"
      ),
      
      hr(),
      
      downloadButton(
        "download_pitches",
        "Download pitch-level CSV"
      )
    ),
    
    
    mainPanel(
      
      h3("Current state"),
      verbatimTextOutput("state_text"),
      
      h3("Current at-bat: pitch log"),
      tableOutput("pitch_summary"),
      
      h3("This half-inning: plate appearances"),
      tableOutput("pa_summary")
    )
  )
)



# SERVER

server <- function(input, output, session) {
  
  
  # Reactive game state
  
  rv <- reactiveValues(
    
    pitches = tibble(
      week = character(),
      outing_id = character(),
      date = as.Date(character()),
      pitcher = character(),
      inning = integer(),
      half = character(),
      batter_no = integer(),
      pitch_no_pa = integer(),
      pitch_result = character(),
      balls_before = integer(),
      strikes_before = integer()
    ),
    
    pa = tibble(
      week = character(),
      outing_id = character(),
      date = as.Date(character()),
      pitcher = character(),
      inning = integer(),
      half = character(),
      batter_no = integer(),
      pa_outcome = character(),
      runs_scored = integer(),
      er_scored = integer(),
      outs_this_pa = integer()
    ),
    
    current_batter = 1,
    balls = 0,
    strikes = 0,
    outs = 0
  )
  
  
  # Add pitch
  
  add_pitch <- function(result_label) {
    
    df <- rv$pitches
    
    new_row <- tibble(
      week = input$week,
      outing_id = input$outing_id,
      date = as.Date(input$date),
      pitcher = input$pitcher,
      inning = input$inning,
      half = input$half,
      batter_no = rv$current_batter,
      
      pitch_no_pa =
        sum(
          df$batter_no == rv$current_batter &
            df$inning == input$inning &
            df$half == input$half
        ) + 1,
      
      pitch_result = result_label,
      balls_before = rv$balls,
      strikes_before = rv$strikes
    )
    
    rv$pitches <- bind_rows(
      df,
      new_row
    )
    
    
    # Update count
    
    if (result_label == "Ball") {
      
      rv$balls <- rv$balls + 1
      
    } else if (
      result_label %in%
      c(
        "Called Strike",
        "Swinging Strike"
      )
    ) {
      
      rv$strikes <- rv$strikes + 1
      
    } else if (
      result_label == "Foul" &&
      rv$strikes < 2
    ) {
      
      rv$strikes <- rv$strikes + 1
    }
  }
  
  
  # Pitch buttons
  
  observeEvent(
    input$btn_ball,
    {
      add_pitch("Ball")
    }
  )
  
  observeEvent(
    input$btn_cs,
    {
      add_pitch("Called Strike")
    }
  )
  
  observeEvent(
    input$btn_sw,
    {
      add_pitch("Swinging Strike")
    }
  )
  
  observeEvent(
    input$btn_foul,
    {
      add_pitch("Foul")
    }
  )
  
  observeEvent(
    input$btn_inplay_out,
    {
      add_pitch("In-play Out")
    }
  )
  
  observeEvent(
    input$btn_inplay_hit,
    {
      add_pitch("In-play Hit")
    }
  )
  
  observeEvent(
    input$btn_hbp,
    {
      add_pitch("HBP")
    }
  )
  
  
  # Undo last pitch
  
  observeEvent(
    input$undo_pitch,
    {
      
      df <- rv$pitches
      
      if (nrow(df) == 0) {
        return(NULL)
      }
      
      last_row <- df[nrow(df), ]
      
      if (
        last_row$batter_no == rv$current_batter &&
        last_row$inning == input$inning &&
        last_row$half == input$half &&
        last_row$pitcher == input$pitcher
      ) {
        
        rv$balls <- last_row$balls_before
        rv$strikes <- last_row$strikes_before
      }
      
      rv$pitches <- df[-nrow(df), ]
    }
  )
  
  
  # End plate appearance
  
  observeEvent(
    input$end_pa,
    {
      
      rv$pa <- bind_rows(
        rv$pa,
        
        tibble(
          week = input$week,
          outing_id = input$outing_id,
          date = as.Date(input$date),
          pitcher = input$pitcher,
          inning = input$inning,
          half = input$half,
          batter_no = rv$current_batter,
          pa_outcome = input$pa_outcome,
          runs_scored = input$runs_scored,
          er_scored = input$er_scored,
          outs_this_pa = input$outs_this_pa
        )
      )
      
      
      # Update score
      
      if (input$runs_scored > 0) {
        
        if (input$half == "Top") {
          
          updateNumericInput(
            session,
            "away_runs",
            value =
              input$away_runs +
              input$runs_scored
          )
          
        } else {
          
          updateNumericInput(
            session,
            "home_runs",
            value =
              input$home_runs +
              input$runs_scored
          )
        }
      }
      
      
      # Update outs
      
      rv$outs <-
        rv$outs +
        input$outs_this_pa
      
      
      # Automatically advance half-inning at three outs
      
      if (rv$outs >= 3) {
        
        rv$outs <- 0
        
        if (input$half == "Top") {
          
          updateSelectInput(
            session,
            "half",
            selected = "Bottom"
          )
          
        } else {
          
          updateSelectInput(
            session,
            "half",
            selected = "Top"
          )
          
          updateNumericInput(
            session,
            "inning",
            value = input$inning + 1
          )
        }
      }
      
      
      # Reset count and advance batter
      
      rv$balls <- 0
      rv$strikes <- 0
      
      rv$current_batter <-
        rv$current_batter + 1
      
      
      # Reset PA inputs
      
      updateNumericInput(
        session,
        "outs_this_pa",
        value = 0
      )
      
      updateNumericInput(
        session,
        "runs_scored",
        value = 0
      )
      
      updateNumericInput(
        session,
        "er_scored",
        value = 0
      )
    }
  )
  
  
  # Undo last plate appearance
  
  observeEvent(
    input$undo_pa,
    {
      
      pa_df <- rv$pa
      
      if (nrow(pa_df) == 0) {
        return(NULL)
      }
      
      last_pa <-
        pa_df[nrow(pa_df), ]
      
      pa_df <-
        pa_df[-nrow(pa_df), ]
      
      rv$pa <- pa_df
      
      
      # Reverse runs from last PA
      
      if (last_pa$runs_scored > 0) {
        
        if (last_pa$half == "Top") {
          
          new_away <- max(
            0,
            input$away_runs -
              last_pa$runs_scored
          )
          
          updateNumericInput(
            session,
            "away_runs",
            value = new_away
          )
          
        } else {
          
          new_home <- max(
            0,
            input$home_runs -
              last_pa$runs_scored
          )
          
          updateNumericInput(
            session,
            "home_runs",
            value = new_home
          )
        }
      }
      
      
      # Recompute current batter
      
      rv$current_batter <-
        ifelse(
          nrow(pa_df) == 0,
          1,
          max(pa_df$batter_no) + 1
        )
      
      
      # Recompute game state
      
      if (nrow(pa_df) == 0) {
        
        rv$outs <- 0
        
      } else {
        
        last_pa2 <-
          pa_df[nrow(pa_df), ]
        
        outs_in_half <-
          pa_df |>
          filter(
            inning == last_pa2$inning,
            half == last_pa2$half
          ) |>
          summarise(
            outs =
              sum(outs_this_pa)
          ) |>
          pull(outs)
        
        rv$outs <-
          outs_in_half %% 3
        
        updateNumericInput(
          session,
          "inning",
          value = last_pa2$inning
        )
        
        updateSelectInput(
          session,
          "half",
          selected = last_pa2$half
        )
      }
      
      
      # Reset count
      
      rv$balls <- 0
      rv$strikes <- 0
    }
  )
  
  
  # Manual inning control
  
  observeEvent(
    input$next_inning,
    {
      
      updateNumericInput(
        session,
        "inning",
        value = input$inning + 1
      )
      
      updateSelectInput(
        session,
        "half",
        selected = "Top"
      )
      
      rv$outs <- 0
      rv$balls <- 0
      rv$strikes <- 0
    }
  )
  
  
  # Current game state
  
  output$state_text <- renderText({
    
    score_text <- paste0(
      "Score (Away-Home): ",
      input$away_runs,
      "-",
      input$home_runs
    )
    
    nu_label <- ifelse(
      input$nu_side == "Home",
      "(NU = Home)",
      "(NU = Away)"
    )
    
    paste0(
      "Pitcher: ",
      input$pitcher,
      "\n",
      
      "Week: ",
      input$week,
      " | Outing: ",
      input$outing_id,
      " | Date: ",
      as.character(input$date),
      "\n",
      
      "Inning: ",
      input$inning,
      " ",
      input$half,
      " | Outs: ",
      rv$outs,
      "\n",
      
      "Batter #: ",
      rv$current_batter,
      "\n",
      
      "Count: ",
      rv$balls,
      "-",
      rv$strikes,
      " (B-S)\n",
      
      score_text,
      " ",
      nu_label
    )
  })
  
  
  # Current at-bat pitch log
  
  output$pitch_summary <- renderTable({
    
    df <- rv$pitches
    
    if (nrow(df) == 0) {
      return(NULL)
    }
    
    df |>
      filter(
        pitcher == input$pitcher,
        inning == input$inning,
        half == input$half,
        batter_no == rv$current_batter
      ) |>
      arrange(
        pitch_no_pa
      ) |>
      transmute(
        `Pitch #` = pitch_no_pa,
        Result = pitch_result,
        `S before` = strikes_before,
        `B before` = balls_before
      )
  })
  
  
  # Completed plate appearances in current half-inning
  
  output$pa_summary <- renderTable({
    
    df_pa <- rv$pa
    
    if (nrow(df_pa) == 0) {
      return(NULL)
    }
    
    df_pa <-
      df_pa |>
      filter(
        pitcher == input$pitcher,
        inning == input$inning,
        half == input$half
      )
    
    if (nrow(df_pa) == 0) {
      return(NULL)
    }
    
    
    pitch_counts <-
      rv$pitches |>
      filter(
        pitcher == input$pitcher,
        inning == input$inning,
        half == input$half
      ) |>
      group_by(
        batter_no
      ) |>
      summarise(
        Pitches = n(),
        .groups = "drop"
      )
    
    
    df_pa |>
      left_join(
        pitch_counts,
        by = "batter_no"
      ) |>
      arrange(
        batter_no
      ) |>
      transmute(
        `Batter #` = batter_no,
        Pitches = Pitches,
        Outcome = pa_outcome,
        Runs = runs_scored,
        ER = er_scored,
        `Outs on PA` = outs_this_pa
      )
  })
  
  
  # Export pitch and plate appearance data
  
  output$download_pitches <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "pitch_data_",
        input$outing_id,
        ".csv"
      )
    },
    
    content = function(file) {
      
      export_data <-
        rv$pitches |>
        left_join(
          rv$pa,
          by = c(
            "week",
            "outing_id",
            "date",
            "pitcher",
            "inning",
            "half",
            "batter_no"
          )
        )
      
      write.csv(
        export_data,
        file,
        row.names = FALSE
      )
    }
  )
}


shinyApp(
  ui = ui,
  server = server
)