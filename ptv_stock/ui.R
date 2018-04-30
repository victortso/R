#
# FILE: ui.R
#
# 4/29/2018 tso   New file
#                 UI element for the shiny app
#
# Shiny template https://deanattali.com/blog/building-shiny-apps-tutorial/
#
# https://stackoverflow.com/questions/31415301/shiny-responds-to-enter
# To respond to key input only when enter is issued.

js <- '
  $(document).on("keydown", function(e) {
      Shiny.onInputChange("lastkeypresscode", e.keyCode);
  });
'

values <- list(A = c('1', '30', '60', '90', 'ALL'))

ui <- fluidPage(
  titlePanel("PTV Stock Maniac!"),
  sidebarLayout(
    sidebarPanel("One Stock Symbol at a time",
                 br(), br(),
                 tags$script(js),
                 textInput(inputId = "key", label = "SITE SIGNATURE"),
                 textInput(inputId = "symbol", label = "SYMBOL", "AAPL"),
                 tags$script('
                              $(document).on("keydown", function (e) {
                              Shiny.onInputChange("lastkeypresscode", e.keyCode);
                              });
                              '),
                 actionButton("GO", "Submit"),
                 br(), br(),
                 radioButtons(inputId = "sizeType", "RETRIEVE RAW DATA FORM:",
                              choices = c("Compact", "Full"),
                              selected = "Compact"),
                 # selectInput('selection', 'selection', c('A',  'B'), 'A'),
                 uiOutput('selectUI'),
                 sliderInput(inputId = "target", label = "COMPUTE TIME PERIOD IN DAYS:",
                             min = 0, max = length(values$A) - 1,
                             step = 1,
                             value = length(values$A) - 1),
                 # sliderInput(inputId = "mRange", "Month", min = 0, max = 120, step = 30,
                 #           value = 30, post = "days"),
                 verbatimTextOutput('summary'),
                 verbatimTextOutput("results")
    ),
    # mainPanel("the results will go here",
    mainPanel(plotOutput("coolplot"),
              br(), br(),
              h2(textOutput("key")),
              br(), br(),
              h2(textOutput("symbol"))
              ,
              rHandsontableOutput("hot")
    )
  )
)


# print(ui)
# This shows ui is just all HTML

