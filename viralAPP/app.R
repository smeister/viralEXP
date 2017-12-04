library(shiny)
library(rhandsontable)
library(viralEXP)

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
    br(),
    fixedRow(style = "border-radius: 10px; border-width: 2px; border-style: solid;border-color: #000000;height:320px;width:900px",
      # Table 1
      column(width=2,
        fixedRow(
          column(width=12,strong("Sample 1"),align="center"),
          column(width=3,div(style="width: 40px;text-align:center;font-size: 12px",textInput(inputId = "dil.nb1",label = "Dil.", value = 8))),
          column(width=4,div(style="width: 56px;text-align:center;font-size: 12px",textInput(inputId = "vol.nb1",label = "Vol.", value = 0.01))),
          column(width=3,div(style="width: 35px;text-align:center;font-size: 12px",textInput(inputId = "rep.nb1",label = "Rep.", value = 5))),
          column(width=12,rHandsontableOutput("import1"))
        )

      ),
      # Table 2
      column(width=2,
             fixedRow(
               column(width=12,strong("Sample 2"),align="center"),
               column(width=3,div(style="width: 40px;text-align:center;font-size: 12px",textInput(inputId = "dil.nb2",label = "Dil.", value = 8))),
               column(width=4,div(style="width: 56px;text-align:center;font-size: 12px",textInput(inputId = "vol.nb2",label = "Vol.", value = 0.01))),
               column(width=3,div(style="width: 35px;text-align:center;font-size: 12px",textInput(inputId = "rep.nb2",label = "Rep.", value = 5))),
               column(width=12,rHandsontableOutput("import2"))
             )

      ),
      # Table 3
      column(width=2,
             fixedRow(
               column(width=12,strong("Sample 3"),align="center"),
               column(width=3,div(style="width: 40px;text-align:center;font-size: 12px",textInput(inputId = "dil.nb3",label = "Dil.", value = 8))),
               column(width=4,div(style="width: 56px;text-align:center;font-size: 12px",textInput(inputId = "vol.nb3",label = "Vol.", value = 0.01))),
               column(width=3,div(style="width: 35px;text-align:center;font-size: 12px",textInput(inputId = "rep.nb3",label = "Rep.", value = 5))),
               column(width=12,rHandsontableOutput("import3"))
             )

      ),
      # Table 4
      column(width=2,
             fixedRow(
               column(width=12,strong("Sample 4"),align="center"),
               column(width=3,div(style="width: 40px;text-align:center;font-size: 12px",textInput(inputId = "dil.nb4",label = "Dil.", value = 8))),
               column(width=4,div(style="width: 56px;text-align:center;font-size: 12px",textInput(inputId = "vol.nb4",label = "Vol.", value = 0.01))),
               column(width=3,div(style="width: 35px;text-align:center;font-size: 12px",textInput(inputId = "rep.nb4",label = "Rep.", value = 5))),
               column(width=12,rHandsontableOutput("import4"))
             )

      ),
      # Table 5
      column(width=2,
             fixedRow(
               column(width=12,strong("Sample 5"),align="center"),
               column(width=3,div(style="width: 40px;text-align:center;font-size: 12px",textInput(inputId = "dil.nb5",label = "Dil.", value = 8))),
               column(width=4,div(style="width: 56px;text-align:center;font-size: 12px",textInput(inputId = "vol.nb5",label = "Vol.", value = 0.01))),
               column(width=3,div(style="width: 35px;text-align:center;font-size: 12px",textInput(inputId = "rep.nb5",label = "Rep.", value = 5))),
               column(width=12,rHandsontableOutput("import5"))
             )

      ),
      # Table 6
      column(width=2,
             fixedRow(
               column(width=12,strong("Sample 6"),align="center"),
               column(width=3,div(style="width: 40px;text-align:center;font-size: 12px",textInput(inputId = "dil.nb6",label = "Dil.", value = 8))),
               column(width=4,div(style="width: 56px;text-align:center;font-size: 12px",textInput(inputId = "vol.nb6",label = "Vol.", value = 0.01))),
               column(width=3,div(style="width: 35px;text-align:center;font-size: 12px",textInput(inputId = "rep.nb6",label = "Rep.", value = 5))),
               column(width=12,rHandsontableOutput("import6"))
             )

      )
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
  # Table 1
  output$import1 <- renderRHandsontable({
    dil.nb<-as.numeric(input$dil.nb1)
    vol.nb<-as.numeric(input$vol.nb1)
    rep.nb<-as.numeric(input$rep.nb1)
    df1<-data.frame(v1=format(repDIL(vol.nb,dil.nb,10), scientific = 1),
                    x1=as.integer(rep(0,dil.nb)),
                    n1=as.integer(rep(rep.nb,dil.nb)))
    rhandsontable(df1, rowHeaders = NULL) %>%
      hot_col("v1", readOnly = TRUE) %>%
      hot_col("n1", readOnly = TRUE, halign="htCenter") %>%
      hot_col("x1", halign="htCenter") %>%
      hot_cols(colWidths =c(63,28,28))
  })
  # Table 1
  output$import2 <- renderRHandsontable({
    dil.nb<-as.numeric(input$dil.nb2)
    vol.nb<-as.numeric(input$vol.nb2)
    rep.nb<-as.numeric(input$rep.nb2)
    df1<-data.frame(v2=format(repDIL(vol.nb,dil.nb,10), scientific = 1),
                    x2=as.integer(rep(0,dil.nb)),
                    n2=as.integer(rep(rep.nb,dil.nb)))
    rhandsontable(df1, rowHeaders = NULL) %>%
      hot_col("v2", readOnly = TRUE) %>%
      hot_col("n2", readOnly = TRUE, halign="htCenter") %>%
      hot_col("x2", halign="htCenter") %>%
      hot_cols(colWidths =c(63,28,28))
  })
  # Table 3
  output$import3 <- renderRHandsontable({
    dil.nb<-as.numeric(input$dil.nb3)
    vol.nb<-as.numeric(input$vol.nb3)
    rep.nb<-as.numeric(input$rep.nb3)
    df1<-data.frame(v3=format(repDIL(vol.nb,dil.nb,10), scientific = 1),
                    x3=as.integer(rep(0,dil.nb)),
                    n3=as.integer(rep(rep.nb,dil.nb)))
    rhandsontable(df1, rowHeaders = NULL) %>%
      hot_col("v3", readOnly = TRUE) %>%
      hot_col("n3", readOnly = TRUE, halign="htCenter") %>%
      hot_col("x3", halign="htCenter") %>%
      hot_cols(colWidths =c(63,28,28))
  })
  # Table 4
  output$import4 <- renderRHandsontable({
    dil.nb<-as.numeric(input$dil.nb4)
    vol.nb<-as.numeric(input$vol.nb4)
    rep.nb<-as.numeric(input$rep.nb4)
    df1<-data.frame(v4=format(repDIL(vol.nb,dil.nb,10), scientific = 1),
                    x4=as.integer(rep(0,dil.nb)),
                    n4=as.integer(rep(rep.nb,dil.nb)))
    rhandsontable(df1, rowHeaders = NULL) %>%
      hot_col("v4", readOnly = TRUE) %>%
      hot_col("n4", readOnly = TRUE, halign="htCenter") %>%
      hot_col("x4", halign="htCenter") %>%
      hot_cols(colWidths =c(63,28,28))
  })
  # Table 5
  output$import5 <- renderRHandsontable({
    dil.nb<-as.numeric(input$dil.nb5)
    vol.nb<-as.numeric(input$vol.nb5)
    rep.nb<-as.numeric(input$rep.nb5)
    df1<-data.frame(v5=format(repDIL(vol.nb,dil.nb,10), scientific = 1),
                    x5=as.integer(rep(0,dil.nb)),
                    n5=as.integer(rep(rep.nb,dil.nb)))
    rhandsontable(df1, rowHeaders = NULL) %>%
      hot_col("v5", readOnly = TRUE) %>%
      hot_col("n5", readOnly = TRUE, halign="htCenter") %>%
      hot_col("x5", halign="htCenter") %>%
      hot_cols(colWidths =c(63,28,28))
  })
  # Table 6
  output$import6 <- renderRHandsontable({
    dil.nb<-as.numeric(input$dil.nb6)
    vol.nb<-as.numeric(input$vol.nb6)
    rep.nb<-as.numeric(input$rep.nb6)
    df1<-data.frame(v6=format(repDIL(vol.nb,dil.nb,10), scientific = 1),
                    x6=as.integer(rep(0,dil.nb)),
                    n6=as.integer(rep(rep.nb,dil.nb)))
    rhandsontable(df1, rowHeaders = NULL) %>%
      hot_col("v6", readOnly = TRUE) %>%
      hot_col("n6", readOnly = TRUE, halign="htCenter") %>%
      hot_col("x6", halign="htCenter") %>%
      hot_cols(colWidths =c(63,28,28))
  })

}


# Run the application
shinyApp(ui = ui, server = server)

