library(shiny)
library(shinysky)
library(rhandsontable)
library(viralEXP)

ui <- navbarPage(title="Virus Infectivity Calculation",
  tabPanel("getMPN",
    fixedRow(
      column(width=12,
        #
        "getMPN Example:"
      )
    ),
    fixedRow(
      #
      column(width=12,
        tags$img(src='IMG for App.png', align = "center", height = "300px",width = "900px")
      )
    ),
    br(),
    column(width=12,
           fixedRow(style = "border-radius: 10px; border-width: 2px; border-style: solid;border-color: #000000;width:900px",
                    # Table 1
                    column(width=2,style="padding:8px;",
                           fixedRow(
                             column(width=12,strong("Sample 1"),align="center")
                           ),
                           fixedRow(
                             column(width=12,
                                    column(width=4,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "dil.nb1",label = "Dil.", value = 8)),
                                    column(width=5,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "vol.nb1",label = "Vol.", value = 0.01)),
                                    column(width=3,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "rep.nb1",label = "Rep.", value = 5))
                             )
                           ),
                           fixedRow(
                             column(width=12,offset=0,rHandsontableOutput("import1"))
                           )

                    ),
                    # Table 2
                    column(width=2,style="padding:8px;",
                           fixedRow(
                             column(width=12,strong("Sample 2"),align="center")
                           ),
                           fixedRow(
                             column(width=12,
                                    column(width=4,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "dil.nb2",label = "Dil.", value = 8)),
                                    column(width=5,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "vol.nb2",label = "Vol.", value = 0.01)),
                                    column(width=3,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "rep.nb2",label = "Rep.", value = 5))
                             )
                           ),
                           fixedRow(
                             column(width=12,offset=0,rHandsontableOutput("import2"))
                           )

                    ),
                    # Table 3
                    column(width=2,style="padding:8px;",
                           fixedRow(
                             column(width=12,strong("Sample 3"),align="center")
                           ),
                           fixedRow(
                             column(width=12,
                                    column(width=4,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "dil.nb3",label = "Dil.", value = 8)),
                                    column(width=5,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "vol.nb3",label = "Vol.", value = 0.01)),
                                    column(width=3,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "rep.nb3",label = "Rep.", value = 5))
                             )
                           ),
                           fixedRow(
                             column(width=12,offset=0,rHandsontableOutput("import3"))
                           )

                    ),
                    # Table 4
                    column(width=2,style="padding:8px;",
                           fixedRow(
                             column(width=12,strong("Sample 4"),align="center")
                           ),
                           fixedRow(
                             column(width=12,
                                    column(width=4,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "dil.nb4",label = "Dil.", value = 8)),
                                    column(width=5,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "vol.nb4",label = "Vol.", value = 0.01)),
                                    column(width=3,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "rep.nb4",label = "Rep.", value = 5))
                             )
                           ),
                           fixedRow(
                             column(width=12,offset=0,rHandsontableOutput("import4"))
                           )

                    ),
                    # Table 5
                    column(width=2,style="padding:8px;",
                           fixedRow(
                             column(width=12,strong("Sample 5"),align="center")
                           ),
                           fixedRow(
                             column(width=12,
                                    column(width=4,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "dil.nb5",label = "Dil.", value = 8)),
                                    column(width=5,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "vol.nb5",label = "Vol.", value = 0.01)),
                                    column(width=3,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "rep.nb5",label = "Rep.", value = 5))
                             )
                           ),
                           fixedRow(
                             column(width=12,offset=0,rHandsontableOutput("import5"))
                           )

                    ),
                    # Table 6
                    column(width=2,style="padding:8px;",
                           fixedRow(
                             column(width=12,strong("Sample 6"),align="center")
                           ),
                           fixedRow(
                             column(width=12,
                                    column(width=4,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "dil.nb6",label = "Dil.", value = 8)),
                                    column(width=5,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "vol.nb6",label = "Vol.", value = 0.01)),
                                    column(width=3,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "rep.nb6",label = "Rep.", value = 5))
                             )
                           ),
                           fixedRow(
                             column(width=12,offset=0,rHandsontableOutput("import6"))
                           )

                    )
           )
    ),
    fixedRow( # MPN results (table and graph)
      column(width = 12,
             br(),
             column(width=2, actionButton(inputId = "clickMPN", label = "Calculate MPN")),
             column(width=1, downloadButton("saveMPN","Save"))
      )
    ),
    fixedRow(style="height:320px;width:950px",
      column(width=12,
             br(),
             column(width=6,tableOutput(outputId = "table")),
             column(width=6,plotOutput(outputId = "graph", height = "300px"))

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
             tags$img(src='IMG for App.png', align = "center", height = "300px",width = "900px")
      )
    ),
    br(),
    column(width=12,
      #
      fixedRow(style = "border-radius: 10px; border-width: 2px; border-style: solid;border-color: #000000;width:900px",
               # table 1
               column(width=4,style="padding:8px;",
                      fixedRow(
                        column(width=12,strong("Biological replicate 1"),align="center")
                      ),
                      fixedRow(
                        column(width=12,
                               column(width=3,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "kdil.nb1",label = "Dil.", value=8)),
                               column(width=6,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "kvol.nb1",label = "Vol.", value = 0.01)),
                               column(width=3,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "krep.nb1",label = "Rep.", value = 5))
                        )
                      ),
                      fixedRow(
                        column(width=12,offset=0,rHandsontableOutput("getKDataA"))
                      ),
                      fixedRow(
                        column(width=12,offset=0,rHandsontableOutput("getKTimeA"))
                      )

               ),
               # Table 2
               column(width=4,style="padding:8px;",
                      fixedRow(
                        column(width=12,strong("Biological replicate 2"),align="center")
                      ),
                      fixedRow(
                        column(width=12,
                               column(width=3,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "kdil.nb2",label = "Dil.", value=8)),
                               column(width=6,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "kvol.nb2",label = "Vol.", value = 0.01)),
                               column(width=3,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "krep.nb2",label = "Rep.", value = 5))
                        )
                      ),
                      fixedRow(
                        column(width=12,offset=0,rHandsontableOutput("getKDataB"))
                      ),
                      fixedRow(
                        column(width=12,offset=0,rHandsontableOutput("getKTimeB"))
                      )

               ),
               # Table 3
               column(width=4,style="padding:8px;",
                      fixedRow(
                        column(width=12,strong("Biological replicate 3"),align="center")
                      ),
                      fixedRow(
                        column(width=12,
                               column(width=3,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "kdil.nb3",label = "Dil.", value=8)),
                               column(width=6,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "kvol.nb3",label = "Vol.", value = 0.01)),
                               column(width=3,offset=0,style="padding:0px;text-align:center;font-size: 12px",textInput(inputId = "krep.nb3",label = "Rep.", value = 5))
                        )
                      ),
                      fixedRow(
                        column(width=12,offset=0,rHandsontableOutput("getKDataC"))
                      ),
                      fixedRow(
                        column(width=12,offset=0,rHandsontableOutput("getKTimeC"))
                      )

               )


      ),
      fixedRow( # MPN results (table and graph)
        column(width = 12,
               br(),
               column(width=2, actionButton(inputId = "clickK", label = "Calculate K")),
               column(width=1, downloadButton("saveDataK","Save"))
        )
      ),
      fixedRow(
        column(width=12,
               br(),
               column(width=6,tableOutput(outputId = "tableK")),
               column(width=6,plotOutput(outputId = "graphK", height = "300px"))
        )
      )
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
                    n1=as.integer(rep(rep.nb,dil.nb)),
                    x1=as.integer(rep(NA,dil.nb))
                    )
    rhandsontable(df1, rowHeaders = NULL, overflow = 'visible') %>%
      hot_col("v1", readOnly = TRUE) %>%
      hot_col("n1", readOnly = TRUE) %>%
      hot_cols(colWidths =c(63,35,35),halign="htCenter")
  })
  # Table 2
  output$import2 <- renderRHandsontable({
    dil.nb<-as.numeric(input$dil.nb2)
    vol.nb<-as.numeric(input$vol.nb2)
    rep.nb<-as.numeric(input$rep.nb2)
    df1<-data.frame(v2=format(repDIL(vol.nb,dil.nb,10), scientific = 1),
                    n2=as.integer(rep(rep.nb,dil.nb)),
                    x2=as.integer(rep(NA,dil.nb))
                    )
    rhandsontable(df1, rowHeaders = NULL, overflow = 'visible') %>%
      hot_col("v2", readOnly = TRUE) %>%
      hot_col("n2", readOnly = TRUE) %>%
      hot_cols(colWidths =c(63,35,35), halign="htCenter")
  })
  # Table 3
  output$import3 <- renderRHandsontable({
    dil.nb<-as.numeric(input$dil.nb3)
    vol.nb<-as.numeric(input$vol.nb3)
    rep.nb<-as.numeric(input$rep.nb3)
    df1<-data.frame(v3=format(repDIL(vol.nb,dil.nb,10), scientific = 1),
                    n3=as.integer(rep(rep.nb,dil.nb)),
                    x3=as.integer(rep(NA,dil.nb))
                    )
    rhandsontable(df1, rowHeaders = NULL, overflow = 'visible') %>%
      hot_col("v3", readOnly = TRUE) %>%
      hot_col("n3", readOnly = TRUE) %>%
      hot_cols(colWidths =c(63,35,35), halign="htCenter")
  })
  # Table 4
  output$import4 <- renderRHandsontable({
    dil.nb<-as.numeric(input$dil.nb4)
    vol.nb<-as.numeric(input$vol.nb4)
    rep.nb<-as.numeric(input$rep.nb4)
    df1<-data.frame(v4=format(repDIL(vol.nb,dil.nb,10), scientific = 1),
                    n4=as.integer(rep(rep.nb,dil.nb)),
                    x4=as.integer(rep(NA,dil.nb))
                    )
    rhandsontable(df1, rowHeaders = NULL, overflow = 'visible') %>%
      hot_col("v4", readOnly = TRUE) %>%
      hot_col("n4", readOnly = TRUE) %>%
      hot_cols(colWidths =c(63,35,35), halign="htCenter")
  })
  # Table 5
  output$import5 <- renderRHandsontable({
    dil.nb<-as.numeric(input$dil.nb5)
    vol.nb<-as.numeric(input$vol.nb5)
    rep.nb<-as.numeric(input$rep.nb5)
    df1<-data.frame(v5=format(repDIL(vol.nb,dil.nb,10), scientific = 1),
                    n5=as.integer(rep(rep.nb,dil.nb)),
                    x5=as.integer(rep(NA,dil.nb))
                    )
    rhandsontable(df1, rowHeaders = NULL, overflow = 'visible') %>%
      hot_col("v5", readOnly = TRUE) %>%
      hot_col("n5", readOnly = TRUE) %>%
      hot_cols(colWidths =c(63,35,35), halign="htCenter")
  })
  # Table 6
  output$import6 <- renderRHandsontable({
    dil.nb<-as.numeric(input$dil.nb6)
    vol.nb<-as.numeric(input$vol.nb6)
    rep.nb<-as.numeric(input$rep.nb6)
    df1<-data.frame(v6=format(repDIL(vol.nb,dil.nb,10), scientific = 1),
                    n6=as.integer(rep(rep.nb,dil.nb)),
                    x6=as.integer(rep(NA,dil.nb))
                    )
    rhandsontable(df1, rowHeaders = NULL, overflow = 'visible') %>%
      hot_col("v6", readOnly = TRUE) %>%
      hot_col("n6", readOnly = TRUE) %>%
      hot_cols(colWidths =c(63,35,35), halign="htCenter")
  })
  ########################### MPN input tables ###########################
  ########################################################################

  ########################################################################
  ############################ K input tables ############################
  # Table K1
  output$getKDataA <- renderRHandsontable({
    dil.nb<-as.numeric(input$kdil.nb1)
    vol.nb<-as.numeric(input$kvol.nb1)
    rep.nb<-as.numeric(input$krep.nb1)
    df<-data.frame(v=format(repDIL(vol.nb,dil.nb,10), scientific = 1),
                    n=as.integer(rep(rep.nb,dil.nb)))

    df1<-data.frame(df, matrix(NA, ncol = 6, nrow = dil.nb))
    df1<-cbind(df1[1:2], lapply(df1[3:length(df1)], as.integer))
    rhandsontable(df1, rowHeaders = NULL, overflow = 'visible') %>%
      hot_col("v", readOnly = TRUE) %>%
      hot_col("n", readOnly = TRUE) %>%
      hot_cols(colWidths =c(65,rep(31,7)),halign="htCenter")
  })
  # Time/Dose table K1
  output$getKTimeA <- renderRHandsontable({
    df<-data.frame(Time.Dose="NA")

    df1<-data.frame(df, matrix(NA, ncol = 6, nrow = 1))
    df1<-cbind(df1[1], lapply(df1[2:length(df1)], as.integer))
    rhandsontable(df1, rowHeaders = NULL, overflow = 'visible') %>%
      hot_col("Time.Dose", readOnly = TRUE) %>%
      hot_cols(colWidths =c(96,rep(31,6)),halign="htCenter")
  })
  # Table K2
  output$getKDataB <- renderRHandsontable({
    dil.nb<-as.numeric(input$kdil.nb2)
    vol.nb<-as.numeric(input$kvol.nb2)
    rep.nb<-as.numeric(input$krep.nb2)
    df<-data.frame(v=format(repDIL(vol.nb,dil.nb,10), scientific = 1),
                   n=as.integer(rep(rep.nb,dil.nb)))

    df1<-data.frame(df, matrix(NA, ncol = 6, nrow = dil.nb))
    df1<-cbind(df1[1:2], lapply(df1[3:length(df1)], as.integer))
    rhandsontable(df1, rowHeaders = NULL, overflow = 'visible') %>%
      hot_col("v", readOnly = TRUE) %>%
      hot_col("n", readOnly = TRUE) %>%
      hot_cols(colWidths =c(65,rep(31,7)),halign="htCenter")
  })
  # Time/Dose table K2
  output$getKTimeB <- renderRHandsontable({
    df<-data.frame(Time.Dose="NA")

    df1<-data.frame(df, matrix(NA, ncol = 6, nrow = 1))
    df1<-cbind(df1[1], lapply(df1[2:length(df1)], as.integer))
    rhandsontable(df1, rowHeaders = NULL, overflow = 'visible') %>%
      hot_col("Time.Dose", readOnly = TRUE) %>%
      hot_cols(colWidths =c(96,rep(31,6)),halign="htCenter")
  })
  # Table K3
  output$getKDataC <- renderRHandsontable({
    dil.nb<-as.numeric(input$kdil.nb3)
    vol.nb<-as.numeric(input$kvol.nb3)
    rep.nb<-as.numeric(input$krep.nb3)
    df<-data.frame(v=format(repDIL(vol.nb,dil.nb,10), scientific = 1),
                   n=as.integer(rep(rep.nb,dil.nb)))

    df1<-data.frame(df, matrix(NA, ncol = 6, nrow = dil.nb))
    df1<-cbind(df1[1:2], lapply(df1[3:length(df1)], as.integer))
    rhandsontable(df1, rowHeaders = NULL, overflow = 'visible') %>%
      hot_col("v", readOnly = TRUE) %>%
      hot_col("n", readOnly = TRUE) %>%
      hot_cols(colWidths =c(65,rep(31,7)),halign="htCenter")
  })
  # Time/Dose table K3
  output$getKTimeC <- renderRHandsontable({
    df<-data.frame(Time.Dose="NA")

    df1<-data.frame(df, matrix(NA, ncol = 6, nrow = 1))
    df1<-cbind(df1[1], lapply(df1[2:length(df1)], as.integer))
    rhandsontable(df1, rowHeaders = NULL, overflow = 'visible') %>%
      hot_col("Time.Dose", readOnly = TRUE) %>%
      hot_cols(colWidths =c(96,rep(31,6)),halign="htCenter")
  })
  ############################ K input tables ############################
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
    # Table 2
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

  ########################################################################
  ############################ K calculation #############################
  observeEvent(input$clickK, {
    # Biological replicate 1
    dfA <- hot_to_r(input$getKDataA)
    timeA<-hot_to_r(input$getKTimeA)
    if (any(is.na(dfA)) | any(is.na(timeA))) {
      repA<-NULL
    } else {
      dfA1<-getMPN(x=as.numeric(as.character(dfA$X1)), n=as.numeric(as.character(dfA$n)), v=as.numeric(as.character(dfA$v)))
      dfA2<-getMPN(x=as.numeric(as.character(dfA$X2)), n=as.numeric(as.character(dfA$n)), v=as.numeric(as.character(dfA$v)))
      dfA3<-getMPN(x=as.numeric(as.character(dfA$X3)), n=as.numeric(as.character(dfA$n)), v=as.numeric(as.character(dfA$v)))
      dfA4<-getMPN(x=as.numeric(as.character(dfA$X4)), n=as.numeric(as.character(dfA$n)), v=as.numeric(as.character(dfA$v)))
      dfA5<-getMPN(x=as.numeric(as.character(dfA$X5)), n=as.numeric(as.character(dfA$n)), v=as.numeric(as.character(dfA$v)))
      dfA6<-getMPN(x=as.numeric(as.character(dfA$X6)), n=as.numeric(as.character(dfA$n)), v=as.numeric(as.character(dfA$v)))
      repA<-getK(dfA1,dfA2,dfA3,dfA4,dfA5,dfA6,timeVECT=as.numeric(as.vector(timeA[1,2:length(timeA)])))
    }
    # Biological replicate 2
    dfB <- hot_to_r(input$getKDataB)
    timeB<-hot_to_r(input$getKTimeB)
    if (any(is.na(dfB)) | any(is.na(timeB))) {
      repB<-NULL
    } else {
      dfB1<-getMPN(x=as.numeric(as.character(dfB$X1)), n=as.numeric(as.character(dfB$n)), v=as.numeric(as.character(dfB$v)))
      dfB2<-getMPN(x=as.numeric(as.character(dfB$X2)), n=as.numeric(as.character(dfB$n)), v=as.numeric(as.character(dfB$v)))
      dfB3<-getMPN(x=as.numeric(as.character(dfB$X3)), n=as.numeric(as.character(dfB$n)), v=as.numeric(as.character(dfB$v)))
      dfB4<-getMPN(x=as.numeric(as.character(dfB$X4)), n=as.numeric(as.character(dfB$n)), v=as.numeric(as.character(dfB$v)))
      dfB5<-getMPN(x=as.numeric(as.character(dfB$X5)), n=as.numeric(as.character(dfB$n)), v=as.numeric(as.character(dfB$v)))
      dfB6<-getMPN(x=as.numeric(as.character(dfB$X6)), n=as.numeric(as.character(dfB$n)), v=as.numeric(as.character(dfB$v)))
      repB<-getK(dfB1,dfB2,dfB3,dfB4,dfB5,dfB6,timeVECT=as.numeric(as.vector(timeB[1,2:length(timeB)])))
    }
    # Biological replicate 3
    dfC <- hot_to_r(input$getKDataC)
    timeC<-hot_to_r(input$getKTimeC)
    if (any(is.na(dfC)) | any(is.na(timeC))) {
      repC<-NULL
    } else {
      dfC1<-getMPN(x=as.numeric(as.character(dfC$X1)), n=as.numeric(as.character(dfC$n)), v=as.numeric(as.character(dfC$v)))
      dfC2<-getMPN(x=as.numeric(as.character(dfC$X2)), n=as.numeric(as.character(dfC$n)), v=as.numeric(as.character(dfC$v)))
      dfC3<-getMPN(x=as.numeric(as.character(dfC$X3)), n=as.numeric(as.character(dfC$n)), v=as.numeric(as.character(dfC$v)))
      dfC4<-getMPN(x=as.numeric(as.character(dfC$X4)), n=as.numeric(as.character(dfC$n)), v=as.numeric(as.character(dfC$v)))
      dfC5<-getMPN(x=as.numeric(as.character(dfC$X5)), n=as.numeric(as.character(dfC$n)), v=as.numeric(as.character(dfC$v)))
      dfC6<-getMPN(x=as.numeric(as.character(dfC$X6)), n=as.numeric(as.character(dfC$n)), v=as.numeric(as.character(dfC$v)))
      repC<-getK(dfC1,dfC2,dfC3,dfC4,dfC5,dfC6,timeVECT=as.numeric(as.vector(timeC[1,2:length(timeC)])))
    }
    ### getK outputs ###
    # output$graphK<-renderPlot({
    #   plot(repA)
    # })
    # saveDataK<-reactive({
    #   print(repA)
    # })
    # output$tableK <- renderTable({
    #   saveDataK()
    # })

    if (is.null(c(repA,repB,repC))) {
      validate(
        need(input$data != "", "Please select a data set")
      )
    } else if (!is.null(repA) & is.null(c(repB,repC))) {
      # only repA
      output$graphK<-renderPlot({
        plot(repA)
      })
      saveDataK<-reactive({
        print(repA)
      })
      output$tableK <- renderTable({
        saveDataK()
      })
    } else if (!is.null(c(repA,repB)) & is.null(repC)) {
      # repA and repB
      results<-getK(repA,repB)
      output$graphK<-renderPlot({
        plot(results)
      })
      saveDataK<-reactive({
        print(results)
      })
      output$tableK <- renderTable({
        saveDataK()
      })
    } else if (!is.null(c(repA,repB,repC))) {
      # repA, repB and repC
      results<-getK(repA,repB,repC)
      output$graphK<-renderPlot({
        plot(results)
      })
      saveDataK<-reactive({
        print(results)
      })
      output$tableK <- renderTable({
        saveDataK()
      })
    }
    ####################
  })
  ############################ K calculation #############################
  ########################################################################

}

# Run the application
shinyApp(ui = ui, server = server)



