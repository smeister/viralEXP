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
      hotable("import1")
      #hotable("import2")
      #hotable("import3")
      #column(hotable("import1"))
      #column(hotable("import2"), width = 3),
      #column(hotable("import3"), width = 3),
      #column(hotable("import4"), width = 3),
      #column(hotable("import5"), width = 3),
      #column(hotable("import6"), width = 3)
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

?data.frame

server <- function(input, output) {
  # Create table with 3 columns (x,n,v)
  output$import1 <- renderHotable({
    df<-data.frame(v1=rep(0,8), x1=rep(0,8),n1=rep(0,8))
  }, readOnly = FALSE)
  output$import2 <- renderHotable({
    df<-data.frame(v1=rep(0,8), x1=rep(0,8),n1=rep(0,8))
  }, readOnly = FALSE)
  output$import3 <- renderHotable({
    df<-data.frame(v1=rep(0,8), x1=rep(0,8),n1=rep(0,8))
  }, readOnly = FALSE)
  output$import4 <- renderHotable({
    df<-data.frame(v1=rep(0,8), x1=rep(0,8),n1=rep(0,8))
  }, readOnly = FALSE)
  output$import5 <- renderHotable({
    df<-data.frame(v1=rep(0,8), x1=rep(0,8),n1=rep(0,8))
  }, readOnly = FALSE)
  output$import6 <- renderHotable({
    df<-data.frame(v1=rep(0,8), x1=rep(0,8),n1=rep(0,8))
  }, readOnly = FALSE)

  # Table getMPN output
  #lapply(DF[is.num], round, 8)
  # PlotMPN output
}



# Run the application
shinyApp(ui = ui, server = server)

