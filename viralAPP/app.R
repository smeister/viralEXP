library(shiny)
library(shinysky)
library(rhandsontable)
library(viralEXP)

ui <- navbarPage(title="Virus Infectivity Calculation",
  tabPanel("getMPN",
    #
    fixedRow(
      column(width=12,
        #
        "getMPN Example:"
      )
    ),
    br(),
    fixedRow(
      #
      column(width=12,
        tags$img(src='IMG for App.png', align = "center", height = "310px",width = "930px")
      )
    ),
    br(),
    column(width=12,
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
           )
    ),
    br(),
    column(width=12,
      fixedRow(
        column(width = 12,
               actionButton(inputId = "clickMPN", label = "Calculate MPN"),
               downloadButton("saveMPN","Save")),
        column(width=5,tableOutput(outputId = "table")),
        column(width=4,plotOutput(outputId = "graph", height = "250px"))
      )
    )
  ),
  tabPanel("getK",
    #
    fixedRow(
      column(width=12,
             #
             "getK Example:"
      )
    ),
    br(),
    fixedRow(
      #
      column(width=12,
             tags$img(src='IMG for App.png', align = "center", height = "310px",width = "930px")
      )
    ),
    br(),
    column(width=12,
      #
      fixedRow(style = "border-radius: 10px; border-width: 2px; border-style: solid;border-color: #000000;height:320px;width:900px",
        "insert three tables"
      )
    ),
    br(),
    fixedRow(
      column(width = 12,
             actionButton(inputId = "clickK", label = "Calculate K"),
             downloadButton("saveDataK","Save")),
      column(width=6,tableOutput(outputId = "tableK")),
      column(width=6,plotOutput(outputId = "graphK", height = "250px"))
    )
  )
)





server <- function(input, output) {
  ########################################################################
  ########################### MPN input tables ###########################
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
  ########################### MPN input tables ###########################
  ########################################################################

  ########################################################################
  ########################### MPN calculation ############################
  observeEvent(input$clickMPN, {
    # Table 1
    df1 <- hot_to_r(input$import1)
    df1$x1<-as.numeric(as.character(df1$x1))
    df1$n1<-as.numeric(as.character(df1$n1))
    df1$v1<-as.numeric(as.character(df1$v1))
    df1<-getMPN(x=df1$x1, n=df1$n1, v=df1$v1)
    # Table 1
    df2 <- hot_to_r(input$import2)
    df2$x2<-as.numeric(as.character(df2$x2))
    df2$n2<-as.numeric(as.character(df2$n2))
    df2$v2<-as.numeric(as.character(df2$v2))
    df2<-getMPN(x=df2$x2, n=df2$n2, v=df2$v2)
    # Table 3
    df3 <- hot_to_r(input$import3)
    df3$x3<-as.numeric(as.character(df3$x3))
    df3$n3<-as.numeric(as.character(df3$n3))
    df3$v3<-as.numeric(as.character(df3$v3))
    df3<-getMPN(x=df3$x3, n=df3$n3, v=df3$v3)
    # Table 4
    df4 <- hot_to_r(input$import4)
    df4$x4<-as.numeric(as.character(df4$x4))
    df4$n4<-as.numeric(as.character(df4$n4))
    df4$v4<-as.numeric(as.character(df4$v4))
    df4<-getMPN(x=df4$x4, n=df4$n4, v=df4$v4)
    # Table 5
    df5 <- hot_to_r(input$import5)
    df5$x5<-as.numeric(as.character(df5$x5))
    df5$n5<-as.numeric(as.character(df5$n5))
    df5$v5<-as.numeric(as.character(df5$v5))
    df5<-getMPN(x=df5$x5, n=df5$n5, v=df5$v5)
    # Table 6
    df6 <- hot_to_r(input$import6)
    df6$x6<-as.numeric(as.character(df6$x6))
    df6$n6<-as.numeric(as.character(df6$n6))
    df6$v6<-as.numeric(as.character(df6$v6))
    df6<-getMPN(x=df6$x6, n=df6$n6, v=df6$v6)

    # Final calculation
    output$graph<-renderPlot({
      plot(df1,df2,df3,df4,df5,df6)
    })
    saveData<-reactive({
      plot(df1,df2,df3,df4,df5,df6, plot=FALSE)
    })
    output$table <- renderTable({
      saveData()
    })
    # Save Data
    thedata <- reactive(output$table)
    output$saveMPN <- downloadHandler(
      filename = function () {
        paste("data ",Sys.Date(), ".csv", sep="")
      },
      content = function(file){
        write.table(saveData(), file, row.names=FALSE, sep=",")
      }
    )
  })

  ########################### MPN calculation ############################
  ########################################################################

}

# Run the application
shinyApp(ui = ui, server = server)



