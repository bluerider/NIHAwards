dashboardPage(
  skin = "purple",
  dashboardHeader(title= "Data Incubator Project"),
  dashboardSidebar(
    fluidRow(
      column(12, sliderInput(inputId='year_choice', 'Year:',
                             min = 2007, max = 2017,
                             value = 2017)))),
  dashboardBody(
    plotOutput("map_plot", width= "100%", height="800px")
  )
)