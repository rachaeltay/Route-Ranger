library(shiny)

stops <- c('PGP', 'Kent Ridge MRT', 'LT29', 'UHall', 'OPP UHC', 'YIH', 'Central Library', 'LT13', 'AS7', 'COM2', 'BIZ2', 'OPP House 12', 'House 7')
timeIntervals <- c("Monthly", "Weekly", "Daily", "Hourly")

ui <- fluidPage(
  titlePanel(div(h1("The Route Ranger"), style="color:red")),
  sidebarLayout(
    sidebarPanel(selectInput("busStop", "Select Bus Stop", sort(stops), selected="COM2"),
                 selectInput("timeFrame", "Select Time Frame", timeIntervals, selected="Hourly"),
                 conditionalPanel("input.timeFrame == 'Hourly'", dateInput("startDate1", "Choose a Day")),
                 conditionalPanel("input.timeFrame != 'Hourly'", dateInput("startDate2", "Starting Date"), dateInput("endDate", "Ending Date")),
                 actionButton("genResult" , "Show Stop Usage!")
                 ),
    mainPanel(div(tags$label("Number of people boarding"), textOutput("boarding"),
              tags$label("Number of people aighting"), textOutput("alighting"), style="color:teal"), br(),
              div(plotOutput("plot"), 
                  # test outputs
                  textOutput("testText"), dataTableOutput("testTable"))
              )
              )
  )