#
# FILE: server.R
#
# 4/29/2018 tso   New file
#                 Server to show the UI pane on the left and the graph/table on the right
#
# Shiny template https://deanattali.com/blog/building-shiny-apps-tutorial/
# server.R
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
# shinyapps
library(rhandsontable)

server <- function(input, output, session) {

  source("get_path.R")
  source("create_subset_df.R")

  # https://stackoverflow.com/questions/31415301/shiny-responds-to-enter
  # https://stackoverflow.com/questions/24973549/r-shiny-key-input-binding 
  # Check for keyboard "ENTER" before plotting!
  #
  sym <- "CTAS"
  gURL <- ""
  values <- list(A = c('1', '30', '60', '90', 'ALL'))
  slider_values <- c('1', '30', '60', '90', 'ALL')
  
  curr.val <- "" # Corresponds to the current displayed input$myinput
  curr.go  <- 0  # Corresponds to the last known GO value (integer)

  lastEvent <- reactive({
    # Is reactive to the following events
    # input$key
    input$lastkeypresscode
    # input$symbol
    input$GO
    input$key
    input$symbol
    
    input_key <- input$GO
    # input_symbol <- input$symbol
    # input_key <- input$key
    
    # Decide which action should be taken
    if (input_key > curr.go) {
      # The user pushed the GO actionButton, so take action
      if (input$key != "")
        action <- 1
      else
        action <- 2
      curr.go <- input_key
    } else if (input$lastkeypresscode == 13) {
      # The user pressed the Enter key, so take action
      # action <- 1
      if (input$key != "")
        action <- 1
      else
        action <- 2
    } else {
      # The user did anything else, so do nothing
      action <- 0
    }
    
    return(action)
  })
  
  output$summary <- renderPrint({
    #print(input$target)
    # print(values[[input$selection]][input$target + 1])
    print(slider_values[input$target + 1])
  })
  
  output$selectUI <- renderUI({

    # To find out 30, 60, 90,... from the slider widget
    sel_values <- paste(paste0('"', slider_values, '"'), collapse = ',')
    # DEBUG
    # print(sel_values)
    # output$debug <- renderPrint({
    #   print(sel_values)
    # })
    list(
      (HTML(
        sprintf('
                <script type="text/javascript">
                $(document).ready(function() {
                var vals = [%s];
                $(\'#target\').data(\'ionRangeSlider\').update(
                {values:vals,
                min: 0,
                max: %s,
                from:%s})
                })
                </script>
                ', sel_values,
                length(slider_values) - 1,
                length(slider_values) - 1)))
    )
  })
    
  # DEBUG
    output$results = renderPrint({
    if (lastEvent() == 1) {
      curr.val <<- isolate(input$key)
    }
    else if (lastEvent() == 2) {
      curr.val <<- isolate(input$symbol)
    }
      # curr.val <<- isolate(input$mRange)
      # DEBUG
    curr.val <<- isolate(input$target)
    curr.val
  })
  
  output$coolplot <- renderPlot({
    
    if (lastEvent() == 1 ||
        lastEvent() == 2) {
      validate(
        need(input$key != "", label = "SITE SIGNATURE")
      )
      key <- input$key
      validate(
        need(input$symbol != "", "Enter one stock symbol of interest")
      )
      sym <- input$symbol
      
      # validate(
      #   # need(input$sizeType != "", "Enter one stock symbol of interest")
      # )
      size <- tolower(input$sizeType);
      
      # force to be compact if the slider is less than 3 months
      # for now!!
      curr.val <<- isolate(input$target)
      print(curr.val)
      m <- as.numeric(curr.val)
      print(m)
      if (m < 4) {
        print("Force to retrieve COMPACT")
        size <- "compact"
      }
      else {
        print("Compute all data to be retrieved")
        print(size)
      }
      #
      
      url <- get_path(key, sym, outputsize = size)
      gURL <- url
      # url <- get_path("D1F6903U02X4VC1B", sym)
      # url <- get_path("D1F6903U02X4VC1B", sym, outputsize = "full")
      
      # Read data from the source
      # df <- read.csv("./daily_adjusted_AAPL.csv", header = TRUE)
      df <- read.csv(url, header = TRUE)
      # nrow(df)
      
      # Make sure date is in correct format
      df$timestamp <- as.Date(df$timestamp)
      # str(df)
      
      # Remove the last 2 columns we don't care about - dividend_amount, split_coefficient
      (df <- df[,c(1:7)])
      
      # df <- df %>%
      #   mutate(Range_SD_range = Range - sd_range)
      
      # Compute the Range
      df <- df %>%
        mutate(Range = high - low)
      # Compute the SD
      (sd_range <- sd(df$Range))
      # Compute the Range - SD_range
      df <- df %>%
        mutate(Range_SD_range = Range - sd_range)
      #
      # # Requirement# 0
      # # df
      # Bonus: Compute lm and plot it
      
      # print(isolate(input$target))
      curr.val <<- isolate(input$target)
      print(curr.val)
      m <- as.numeric(curr.val)
      print(m)
      if (m < 4) {
        print("m<4")
        df_s <- create_subset_df(df, m)
        # df_s <- create_subset_df(df, 2)
      }
      else {
        print("else")
        df_s <- df
      }
      # df_s <- create_subset_df(df, 3)
      head(df_s)
      
      lm <- lm (df_s$adjusted_close ~ df_s$timestamp)
      summary(lm)
      s <- input$symbol
      
      # TBD: what is difference between ds() and ds
      ggplot(data = df_s,
             mapping = aes(x = timestamp, y = `adjusted_close`)) +
        geom_line(size = 1, color = "indianred4") +
        # geom_point() +
        ggtitle(s) +
        labs(x = "Time", y = "Adj_Close") +
        theme(
          axis.title.x = element_text(
            color = "firebrick3",
            size = 14,
            face = "bold"
          ),
          axis.title.y = element_text(
            color = "firebrick3",
            size = 14,
            face = "bold"
          )
        ) +
        geom_smooth(method = 'lm')
      
        # http://stla.github.io/stlapblog/posts/shiny_editTable.html 
        # REVISIT:
        # 4/29/2018 - Comment out to show the graph instead.
        #             - Need to find out how to show both graph and the table
        # output$hot <- renderRHandsontable({
        #   rhandsontable(df_s, rowHeaders = NULL, stretchH = "all")
        # })
    }
  })
}
  