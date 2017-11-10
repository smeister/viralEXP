library(shiny)

ui <- navbarPage(title="Virus Infectivity Calculation",
  tabPanel("getMPN",
    #
    fixedRow(
      "Example:"
    ),
    fixedRow(
      #
      tags$img(src='IMG for App.png', align = "center", height = "300px",width = "900px")
    ),
    fixedRow(style = "border-radius: 10px; border-width: 2px; border-style: solid;border-color: #000000;height:300px;width:900px",
      #
      "hehe"
    ),
    fixedRow(
      #
      # Insert here the result graph
      "huehue"
    )
  ),
  tabPanel("getK",
    #
    "hihi"
  )
)


server <- function(input, output) {
  # Create table with 3 columns (x,n,v)

  # Table getMPN output

  # PlotMPN output
}



# Run the application
shinyApp(ui = ui, server = server)

