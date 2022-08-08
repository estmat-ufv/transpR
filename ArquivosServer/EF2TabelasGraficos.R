###############
##&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@
#output$Box2 <- renderInfoBox({
#
#    infoBox(
#      HTML('<div style = "text-align: justify;"><p>(UFG-GO) O gráfico a seguir apresenta os dez países com a maior taxa de mortalidade decorrente
#do uso de drogas. Na tabela encontra-se o número estimado de mortes causadas por uso de drogas por
#continente. Sabendo que a população da Islândia é de 320137 habitantes, determine o percentual aproximado de
#mortes desse país em relação ao número de mortes estimadas para o continente europeu.
#\n
#a) texto aqui\n
#b) texto aqui
#
#</p></div>'),
#      color = "purple",fill = TRUE
#    )
#})
##&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@

###########################################
#############Tabelas - Graficos de Colunas#
###########################################

output$table3 <- DT::renderDataTable({

  data <- df2 %>%
    filter(Ano==input$vetornome3) %>%
    group_by(UF) %>%
    summarise(Nota=round(mean(NT), digits = 2),
              .groups = 'drop')

  DT::datatable(data,
                class = 'cell-border stripe',
                extensions = 'Buttons', options = list(
                  dom = 'Bfrtip',
                  buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
                ))
})
######################################
#############Graficos de Colunas######
#############Estados (Notas)######
######################################



output$plot2 <- renderPlotly({

  if(input$vetornome4 == "LP - 5° Ano"){

    df1 <- df3 %>%
      filter(Ano==input$vetornome3) %>%
      group_by(UF) %>%
      filter(MEDIA_5_LP!=0) %>%
      summarise(Nota=round(mean(MEDIA_5_LP), digits = 2),
                .groups = 'drop')
    } else {

    if(input$vetornome4 == "MT - 5° Ano"){

      df1 <- df3 %>%
        filter(Ano==input$vetornome3) %>%
        group_by(UF) %>%
        filter(MEDIA_5_MT!=0) %>%
        summarise(Nota=round(mean(MEDIA_5_MT), digits = 2),
                  .groups = 'drop')

      } else {

      if(input$vetornome4 == "LP - 9° Ano"){

   df1 <- df3 %>%
          filter(Ano==input$vetornome3) %>%
          group_by(UF) %>%
     filter(MEDIA_9_LP!=0) %>%
          summarise(Nota=round(mean(MEDIA_9_LP), digits = 2),
                    .groups = 'drop')
   } else {
     df1 <- df3 %>%
       filter(Ano==input$vetornome3) %>%
       group_by(UF) %>%
       filter(MEDIA_9_MT!=0) %>%
       summarise(Nota=round(mean(MEDIA_9_MT), digits = 2),
                 .groups = 'drop')
     }}}

  # df1 <- reactive({
  #   switch(input$vetornome4,
  #          "Nota em Ciências da Natureza"==NOTACN,
  #          "Nota em Ciências Humanas"==NOTACH,
  #          "Nota em Linguagens"==NOTALC,
  #          "Nota em Matemática"==NOTAMT,
  #          "Nota em Redação"==RED,
  #          "Nota Média"==NT
  #   )
  # })



  plot2 <- ggplot(df1,
                  aes(x=reorder(factor(UF), Nota),
                      y = Nota,
                      fill = UF,
                      text = paste('<br>UF:', UF,
                                   '<br>Nota:', Nota))) +
    geom_bar(stat='identity',
    position = 'dodge',
    show.legend = FALSE
  ) +
    geom_text(
      aes(label = Nota,
          y = Nota+0.1*min(Nota)),
      hjust = -0.5, size = 4,
      position = position_dodge(width = 1),
      inherit.aes = TRUE
    ) +
    coord_flip() +
    xlab("Unidade Federativa") +
    ylab("Nota Média") +
    theme_light() +
    ggtitle(paste0(input$vetornome4," no SAEB do ano de ", input$vetornome3))

  plotly::ggplotly(plot2, tooltip = "text") %>%
    plotly::layout(showlegend = FALSE, uniformtext=list(minsize=8, mode='hide')) #%>%
    #plotly::style(textposition = 'center')

})


######################################
#############Graficos de Colunas######
#############Estados (Percentuais)######
######################################

###############
##&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@
output$BoxP2 <- renderInfoBox({

  teste <- df3 %>%
    filter(Ano==input$vetorPnome3 &
             MEDIA_5_LP!=0&
             MEDIA_5_MT!=0&
             MEDIA_9_LP!=0&
             MEDIA_9_MT!=0) %>%
    group_by(UF) %>% summarise(MEDIA_5_LP=round(mean(MEDIA_5_LP),digits = 2),
                               MEDIA_5_MT=round(mean(MEDIA_5_MT),digits = 2),
                               MEDIA_9_LP=round(mean(MEDIA_9_LP),digits = 2),
                               MEDIA_9_MT=round(mean(MEDIA_9_MT),digits = 2), .groups = 'drop')

  Max5LP <-  max(teste$MEDIA_5_LP)
  UF5LP <- teste[teste$MEDIA_5_LP==max(teste$MEDIA_5_LP),1]
  Max5MT <-  max(teste$MEDIA_5_MT)
  UF5MT <- teste[teste$MEDIA_5_MT==max(teste$MEDIA_5_MT),1]
  Max9LP <-  max(teste$MEDIA_9_LP)
  UF9LP <- teste[teste$MEDIA_9_LP==max(teste$MEDIA_9_LP),1]
  Max9MT <-  max(teste$MEDIA_9_MT)
  UF9MT <- teste[teste$MEDIA_9_MT==max(teste$MEDIA_9_MT),1]

  if(input$vetorPnome4 == "LP - 5° Ano"){

    infoBox(
      paste0("A maior nota em ",
             input$vetorPnome4,
             " foi ",
             Max5LP,
             " obtida por ",
             UF5LP,
             ". Consideramos a mesma como 100% e as demais foram calculadas proporcionalmente."),
      color = "purple",fill = TRUE
    )
  } else {

    if(input$vetorPnome4 == "MT - 5° Ano"){

      infoBox(
        paste0("A maior nota em ",
               input$vetorPnome4,
               " foi ",
               Max5MT,
               " obtida por ",
               UF5MT,
               ". Consideramos a mesma como 100% e as demais foram calculadas proporcionalmente."),
        color = "purple",fill = TRUE
      )

    } else {

      if(input$vetorPnome4 == "LP - 9° Ano"){

        infoBox(
          paste0("A maior nota em ",
                 input$vetorPnome4,
                 " foi ",
                 Max9LP,
                 " obtida por ",
                 UF9LP,
                 ". Consideramos a mesma como 100% e as demais foram calculadas proporcionalmente."),
          color = "purple",fill = TRUE
        )

      } else {
        infoBox(
          paste0("A maior nota em ",
                 input$vetorPnome4,
                 " foi ",
                 Max9MT,
                 " obtida por ",
                 UF9MT,
                 ". Consideramos a mesma como 100% e as demais foram calculadas proporcionalmente."),
          color = "purple",fill = TRUE
        )

      }}}


})


output$plotP2 <- renderPlotly({

  teste <- df3 %>%
    filter(Ano==input$vetorPnome3 &
             MEDIA_5_LP!=0&
             MEDIA_5_MT!=0&
             MEDIA_9_LP!=0&
             MEDIA_9_MT!=0) %>%
    group_by(UF) %>% summarise(MEDIA_5_LP=round(mean(MEDIA_5_LP),digits = 2),
                               MEDIA_5_MT=round(mean(MEDIA_5_MT),digits = 2),
                               MEDIA_9_LP=round(mean(MEDIA_9_LP),digits = 2),
                               MEDIA_9_MT=round(mean(MEDIA_9_MT),digits = 2), .groups = 'drop')

  # Max5LP <-  max(teste$MEDIA_5_LP)
  # UF5LP <- teste[teste$MEDIA_5_LP==max(teste$MEDIA_5_LP),1]
  # Max5MT <-  max(teste$MEDIA_5_MT)
  # UF5MT <- teste[teste$MEDIA_5_MT==max(teste$MEDIA_5_MT),1]
  # Max9LP <-  max(teste$MEDIA_9_LP)
  # UF9LP <- teste[teste$MEDIA_9_LP==max(teste$MEDIA_9_LP),1]
  # Max9MT <-  max(teste$MEDIA_9_MT)
  # UF9MT <- teste[teste$MEDIA_9_MT==max(teste$MEDIA_9_MT),1]


  teste <- teste %>%
    mutate(MEDIA_5_LP=round((MEDIA_5_LP/max(MEDIA_5_LP))*100,digits = 2),
           MEDIA_5_MT=round((MEDIA_5_MT/max(MEDIA_5_MT))*100,digits = 2),
           MEDIA_9_LP=round((MEDIA_9_LP/max(MEDIA_9_LP))*100,digits = 2),
           MEDIA_9_MT=round((MEDIA_9_MT/max(MEDIA_9_MT))*100,digits = 2))

#
#   names(teste) <- c("UF",
#                     "LP - 5° Ano",
#                     "MT - 5° Ano",
#                     "LP - 9° Ano",
#                     "MT - 9° Ano")

  if(input$vetorPnome4 == "LP - 5° Ano"){

    df1 <- teste %>%
      select(UF, MEDIA_5_LP)
  } else {

    if(input$vetorPnome4 == "MT - 5° Ano"){

      df1 <- teste %>%
        select(UF, MEDIA_5_MT)

    } else {

      if(input$vetorPnome4 == "LP - 9° Ano"){

        df1 <- teste %>%
          select(UF, MEDIA_9_LP)

      } else {
        df1 <- teste %>%
          select(UF, MEDIA_9_MT)

      }}}

  names(df1) <- c("UF", "Nota")
  plot2 <- ggplot(df1, aes(x=reorder(factor(UF), Nota),
                           y = Nota,
                           fill = UF,
                           text = paste('<br>UF:', UF,
                                                   '<br>Nota:', Nota))) +
    geom_bar(stat='identity',
             position = 'dodge',
             show.legend = FALSE
    ) +
    geom_text(
      aes(label = paste0(Nota, "%"),
          y = Nota+0.1*min(Nota)),
      hjust = -0.5, size = 4,
      position = position_dodge(width = 1),
      inherit.aes = TRUE
    ) +
    coord_flip() +
    xlab("Unidade Federativa") +
    ylab("Nota Média") +
    theme_light() +
    ggtitle(paste0("Nota Percentual de ",input$vetorPnome4," do SAEB do Ano de ", input$vetorPnome3, " Comparada ao Estado com Maior Nota"))

  plotly::ggplotly(plot2, tooltip = "text") %>%
    plotly::layout(showlegend = FALSE, uniformtext=list(minsize=8, mode='hide')) #%>%
    #plotly::style(textposition = 'center')

})

######################################
#############Graficos de Colunas######
#############Municipios######
######################################



table1EF2 <- reactive({
  table1EF2 <- df3 %>% filter(Ano==input$EF2Mun1)
})

observeEvent(input$EF2Mun1, {
  updateSelectizeInput(session,'EF2Mun2',
                       choices=sort(unique(table1EF2()$UF), decreasing = FALSE))
})

table2EF2 <- reactive({
  table2EF2 <- table1EF2() %>% filter(UF==input$EF2Mun2)
})

observeEvent(input$EF2Mun2, {
  updateSelectInput(session,'EF2Mun3',
                    choices=sort(unique(table2EF2()$MUNICIPIO), decreasing = FALSE))
})

table4EF2 <-  eventReactive(req(input$EF2Mun3), ignoreNULL=FALSE, {

  table3EF2 <- table2EF2() %>%
    filter(MUNICIPIO == input$EF2Mun3) %>%
    as.data.frame() %>%
    select(-Ano, -UF)
  names(table3EF2) <- c("Municipio",
                  "LP - 5° Ano",
                  "MT - 5° Ano",
                  "LP - 9° Ano",
                  "MT - 9° Ano")

  table4EF2 <- table3EF2 %>%
    gather(key = "Provas", value = "Notas", -Municipio) %>%
    filter(Notas!=0)
})

output$plotEF2MUN <- renderPlotly({


  plot2 <- ggplot(table4EF2(),
                  aes(x=reorder(factor(Provas), Notas),
                           y = Notas,
                           fill = factor(Provas),
                      text = paste('<br>Provas:', Provas,
                                   '<br>Nota:', Notas))) +
    geom_bar(stat='identity',
             position = 'dodge',
             show.legend = FALSE
    ) +
    geom_text(
      aes(label = Notas,
          y = Notas+0.1*min(Notas)),
      hjust = -0.5, size = 4,
      position = position_dodge(width = 1),
      inherit.aes = TRUE
    ) +
    coord_flip() +
    xlab("Provas") +
    ylab("Notas") +
    theme_light() +
    ggtitle(paste0("Nota da cidade de ", input$EF2Mun3," no Saeb do ano de ", input$EF2Mun1))

  plotly::ggplotly(plot2, tooltip = "text") %>%
    plotly::layout(showlegend = FALSE, uniformtext=list(minsize=8, mode='hide')) #%>%
    #plotly::style(textposition = 'center')

})

#####################################
#########Tabelas de  Dupla Entrada###
#####################################

output$tableDupla2 <- DT::renderDataTable({

  tb1 <- df3 %>%
    filter(Ano==input$vetorDuplaAno) %>%
    group_by(UF) %>%
    summarise(MEDIA_5_LP=round(mean(MEDIA_5_LP), digits = 2),
              MEDIA_5_MT=round(mean(MEDIA_5_MT), digits = 2),
              MEDIA_9_LP=round(mean(MEDIA_9_LP), digits = 2),
              MEDIA_9_MT=round(mean(MEDIA_9_MT), digits = 2))

  # tb2 <- df3 %>%
  #   filter(Ano==input$vetorDuplaAno) %>%
  #   group_by(UF) %>%
  #   filter(MEDIA_5_MT!= 0) %>%
  #   summarise(Nota5MT=round(mean(MEDIA_5_MT), digits = 2),
  #             .groups = 'drop')
  #
  # tb3 <- df3 %>%
  #   filter(Ano==input$vetorDuplaAno) %>%
  #   group_by(UF) %>%
  #   filter(MEDIA_9_LP!= 0) %>%
  #   summarise(Nota9LP=round(mean(MEDIA_9_LP), digits = 2),
  #             .groups = 'drop')
  #
  # tb4 <- df3 %>%
  #   filter(Ano==input$vetorDuplaAno) %>%
  #   group_by(UF) %>%
  #   filter(MEDIA_9_MT!= 0) %>%
  #   summarise(Nota9MT=round(mean(MEDIA_9_MT), digits = 2),
  #             .groups = 'drop')
  #
  # if(input$vetorDupla2 == 1){
  #   data <- tb1
  # } else {
  #   if(input$vetorDupla2 == 2){
  #   data <- tb2
  #   } else {
  #     if(input$vetorDupla2 == 3){
  #   data <- tb3
  #     } else {
  #   data <- tb1
  #     }}}


  DT::datatable(tb1,
                class = 'cell-border stripe',
                extensions = 'Buttons', options = list(
                  dom = 'Bfrtip',
                  buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
                ))
})


#####################################
############Ramos e Folhas###########
#####################################

output$plotTree <- renderPrint({
  tb <- df3 %>% filter(UF==input$vetorTree &
                       Ano==input$vetorTreeAno)

  if(input$vetorTreeProva==1) {
    print(stem(tb$MEDIA_5_LP))
  } else {
    if(input$vetorTreeProva==2) {
      print(stem(tb$MEDIA_5_MT))
    } else {
      if(input$vetorTreeProva==3) {
        print(stem(tb$MEDIA_9_LP))
      } else {
        print(stem(tb$MEDIA_9_MT))
      }
    }
  }




})

#####################################
############Gráficos de Pizza########
#####################################

EF1 <- reactive({
  EF1 <- df3 %>% filter(Ano==input$vetorPie1)
})

observeEvent(input$vetorPie1, {
  updateSelectizeInput(session,'vetorPie2',
                    choices=sort(unique(EF1()$UF), decreasing = FALSE))
})

EF2 <- reactive({
  EF2 <- EF1() %>% filter(UF==input$vetorPie2)
})

observeEvent(input$vetorPie2, {
  updateSelectInput(session,'vetorPie3',
                    choices=sort(unique(EF2()$MUNICIPIO), decreasing = FALSE))
})

EF4 <-  eventReactive(input$vetorPie3, ignoreNULL=FALSE, {

  EF3 <- EF2() %>%
    filter(MUNICIPIO == input$vetorPie3) %>%
    as.data.frame() %>% select(c(1,3:6))
  names(EF3) <- c("Municipio",
                    "LP - 5° Ano",
                    "MT - 5° Ano",
                    "LP - 9° Ano",
                    "MT - 9° Ano")
  EF4 <- EF3 %>%
    gather(key = "Provas", value = "Notas", -Municipio)

})


output$plotPie <- renderPlotly({
  EF5 <- EF4() %>%
    arrange(desc(Notas)) %>%
    mutate(prop = Notas / sum(tb4()$Notas) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )
  EF5 <- EF5[EF5$Notas>0,]
  # ggplot(EF5, aes(x="",
  #                 y=prop,
  #                 fill=Provas)) +
  # geom_bar(stat="identity",
  #          width=1,
  #          color="white") +
  # coord_polar("y",
  #             start=0) +
  # theme_void() +
  # theme(legend.position="none") +
  # geom_text(aes(y = ypos,
  #               label = paste0(Provas, "\n ", Notas)),
  #           color = "black", nudge_x = 0.2, size=6) #+
  # #scale_fill_brewer(palette="Set1")

  plot_ly(EF5,
          values=~Notas,
          marker = list(line = list(color = '#FFFFFF',
                                    width = 1)),
          type="pie",
          textposition = "outside",
          textinfo = 'text',
          hoverinfo = 'text',
          source = "subset",
          text=~paste0("<br>",Provas,
                       "<br>",Notas),
          insidetextfont = list(color = '#FFFFFF'),
          textfont = list(color = "black", size = 20)) %>%
    layout(showlegend = FALSE,separators = ',.') %>%
    config(displayModeBar = F)

})


# output$tableDupla5 <- DT::renderDataTable({
#   tb5 <- tb4() %>%
#     arrange(desc(Notas)) %>%
#     mutate(prop = Notas / sum(tb4()$Notas) *100) %>%
#     mutate(ypos = cumsum(prop)- 0.5*prop )
#
#   DT::datatable(tb5,
#                 class = 'cell-border stripe',
#                 extensions = 'Buttons', options = list(
#                   dom = 'Bfrtip',
#                   buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
#                 ))
# })
