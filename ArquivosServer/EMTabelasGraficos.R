###############
##&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@
#output$Box3 <- renderInfoBox({
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
#
#
#})
##&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@


######################################
#############Graficos de Colunas######
######################################

EM1 <- reactive({
  EM1 <- df1 %>% filter(Ano==input$vetorEM1)
})

observeEvent(input$vetorEM1, {
  updateSelectizeInput(session,'vetorEM2',
                       choices=sort(unique(EM1()$UF), decreasing = FALSE))
})

EM2 <- reactive({
  EM2 <- EM1() %>% filter(UF==input$vetorEM2)
})

observeEvent(input$vetorEM2, {
  updateSelectizeInput(session,'vetorEM3',
                       choices=sort(unique(EM2()$MUNICIPIO), decreasing = FALSE))
})

EM3 <-  eventReactive(input$vetorEM3, ignoreNULL=FALSE, {

  EM3 <- EM2() %>%
    filter(MUNICIPIO == input$vetorEM3) %>%
    group_by(LINGUAGEM_CIDADA) %>%
    summarise(VALOR = sum(VALOR))
})

output$plot1EM3 <- renderPlotly({

  plot3 <- ggplot(EM3(),
         aes(x=factor(LINGUAGEM_CIDADA),
             y = VALOR,
             fill = LINGUAGEM_CIDADA,
             text = paste("Programa:",LINGUAGEM_CIDADA,
                          "<br>",
                          "Valor:",paste0("R$",formatC(VALOR, digits = 2, big.mark=',', format = 'f')),
                          "<br>",
                          "Município:", input$vetorEM3))) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label=VALOR),
              position = position_dodge(width = 0.9),
              vjust = -0.5) +
    xlab("LINGUAGEM CIDADA") +
    ylab("Valor Recebido") +
    theme_light() +
    ggtitle(paste0("Valor Total Recebido = R$ ", sum(EM3()$VALOR)))


  plotly::ggplotly(plot3, tooltip = "text") %>%
    plotly::layout(showlegend = FALSE) #%>%
    #plotly::style(textposition = "top")

})

###########################################
#############Tabelas - Graficos de Colunas#
###########################################

output$tableEM4 <- DT::renderDataTable({



  DT::datatable(EM3(),
                class = 'cell-border stripe',
                extensions = 'Buttons', options = list(
                  dom = 'Bfrtip',
                  buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
                ))
})

#####################################
#########Tabelas de  Dupla Entrada###
#####################################

DP1 <- reactive({
  DP1 <- df2 %>% filter(Ano==input$vetorEM4)
})

observeEvent(input$vetorEM4, {
  updateSelectizeInput(session,'vetorEM5',
                       choices=sort(unique(DP1()$UF), decreasing = FALSE))
})

DP2 <-  reactive({
  DP2 <- DP1() %>%
    filter(UF == input$vetorEM5) %>%
    select(-c(UF, Ano))
})

output$tableDuplaEM <- DT::renderDataTable({

  DT::datatable(DP2(),
                class = 'cell-border stripe',
                extensions = 'Buttons', options = list(
                  dom = 'Bfrtip',
                  buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
                ))
})



#####################################
############Ramos e Folhas###########
#####################################

output$plotEMTree <- renderPrint({

  tb <- df2 %>% filter(UF==input$EMTreeUF &
                         Ano==input$EMTreeAno)

  if(input$EMTreeProva==1) {
    print(stem(tb$NOTA_CN))
  } else {
    if(input$EMTreeProva==2) {
      print(stem(tb$NOTA_CH))
    } else {
      if(input$EMTreeProva==3) {
        print(stem(tb$NOTA_LC))
      } else {
        if(input$EMTreeProva==4) {
          print(stem(tb$NOTA_MT))
        } else {
          if(input$EMTreeProva==5) {
            print(stem(tb$RED))
          } else {
            print(stem(tb$NT))
          }
        }
      }
    }
  }

})

#####################################
############Gráficos de Pizza########
#####################################

tb1 <- reactive({
  tb1 <- df2 %>% filter(Ano==input$EMPie1)
})

observeEvent(input$EMPie1, {
  updateSelectizeInput(session,'EMPie2',
                       choices=sort(unique(tb1()$UF), decreasing = FALSE))
})

tb2 <- reactive({
  tb2 <- tb1() %>% filter(UF==input$EMPie2)
})

observeEvent(input$EMPie2, {
  updateSelectInput(session,'EMPie3',
                    choices=sort(unique(tb2()$MUNICIPIO), decreasing = FALSE))
})

tb4 <-  eventReactive(input$EMPie3, ignoreNULL=FALSE, {

  tb3 <- tb2() %>%
    filter(MUNICIPIO == input$EMPie3) %>%
    as.data.frame() %>% select(c(1,4:8))

  names(tb3) <- c("Municipio",
                    "Ciências da Natureza",
                    "Ciências Humanas",
                    "Linguagens",
                    "Matemática",
                    "Redação")

  tb4 <- tb3 %>%
    gather(key = "Provas", value = "Notas", -Municipio)

})

# tb3 <- reactive({
#   tb3 <- tb2() %>%
#     filter(MUNICIPIO == input$vetorPie3) %>%
#     as.data.frame() %>% select(c(1,4:8))
# })

# tb4 <- reactive({
#   tb4 <- tb3() %>%
#     gather(key = "Provas", value = "Notas", -MUNICIPIO)
# })

# tb5 <- reactive({
#   tb5 <- tb4() %>%
#     arrange(desc(Notas)) %>%
#     mutate(prop = Notas / sum(tb4()$Notas) *100) %>%
#     mutate(ypos = cumsum(prop)- 0.5*prop )
# })


output$EMplotPie <- renderPlotly({
  tb5 <- tb4() %>%
    arrange(desc(Notas)) %>%
    mutate(prop = Notas / sum(tb4()$Notas) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )
  tb5 <- tb5[tb5$Notas>0,]
  # ggplot(tb5, aes(x="",
  #                 y=prop,
  #                 fill=Provas)) +
  #   geom_bar(stat="identity",
  #            width=1,
  #            color="white") +
  #   coord_polar("y",
  #               start=0) +
  #   theme_void() +
  #   theme(legend.position="none") +
  #   geom_text(aes(y = ypos,
  #                 label = paste0(Provas, "\n ", Notas)),
  #             color = "black", nudge_x = 0.2, size=6) #+
  #   #scale_fill_brewer(palette="Set1")

  plot_ly(tb5,
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
          textfont = list(color = "black", size = 15)) %>%
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

#####################################
############Histogramas##############
#####################################

output$plotHist <- renderPlot({

})

#####################################
############Box-Plot#################
#####################################

output$EMplotBox <- renderPlotly({


  teste <- df2 %>%
    filter(Ano==input$EMBox1 & UF == input$EMBox2) %>%
    select(-c(MUNICIPIO, UF, ID, Cand, Ano))
  names(teste) <- c("Ciências da Natureza",
                  "Ciências Humanas",
                  "Linguagens",
                  "Matemática",
                  "Redação",
                    "Média Geral")
  teste2 <- teste %>%
    gather(key = "Provas", value = "Notas")

  bx <- teste2 %>% ggplot(aes(Provas, Notas, fill = Provas)) +
    geom_boxplot(show.legend = FALSE) +
    ggtitle("Boxplot")

  plotly::ggplotly(bx) %>%
    plotly::layout(showlegend = FALSE)

})
