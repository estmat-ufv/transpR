###############
##&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@
output$Box1 <- renderInfoBox({

#  infoBox(
#    HTML('<div style = "text-align: justify;"><p>(UFG-GO) O gráfico a seguir apresenta os dez países com a maior taxa de mortalidade decorrente
#do uso de drogas. Na tabela encontra-se o número estimado de mortes causadas por uso de drogas por
#continente. Sabendo que a população da Islândia é de 320137 habitantes, determine o percentual aproximado de
#mortes desse país em relação ao número de mortes estimadas para o continente europeu.
#\n
#a) texto aqui\n
#b) texto aqui
#
#</p></div>'),
#    color = "purple",fill = TRUE
#  )

})
##&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@

# output$BoxEFMun1 <- renderInfoBox({
#
#   infoBox(
#     HTML('<div style = "text-align: justify;"><p>(UFG-GO) O gráfico a seguir apresenta os dez países com a maior taxa de mortalidade decorrente
# do uso de drogas. Na tabela encontra-se o número estimado de mortes causadas por uso de drogas por
# continente. Sabendo que a população da Islândia é de 320137 habitantes, determine o percentual aproximado de
# mortes desse país em relação ao número de mortes estimadas para o continente europeu.
# \n
# a) texto aqui\n
# b) texto aqui
#
# </p></div>'),
#     color = "purple",fill = TRUE
#   )
#
# })

##&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@

# output$BoxEFMun2 <- renderInfoBox({
#
#   infoBox(
#     HTML('<div style = "text-align: justify;"><p>(UFG-GO) O gráfico a seguir apresenta os dez países com a maior taxa de mortalidade decorrente
# do uso de drogas. Na tabela encontra-se o número estimado de mortes causadas por uso de drogas por
# continente. Sabendo que a população da Islândia é de 320137 habitantes, determine o percentual aproximado de
# mortes desse país em relação ao número de mortes estimadas para o continente europeu.
# \n
# a) texto aqui\n
# b) texto aqui
#
# </p></div>'),
#     color = "purple",fill = TRUE
#   )
#
# })
###########################################
#############Tabelas - Graficos de Colunas#
###########################################

output$table1 <- DT::renderDataTable({

  data <- df1 %>%
    filter(Ano==input$vetornome1) %>%
    group_by(UF) %>%
    summarise(Valor=round((sum(VALOR)), digits = 5),
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
######################################


output$plot1 <- renderPlotly({

teste <- df1 %>%
  filter(Ano==input$vetornome1) %>%
  group_by(UF) %>%
  summarise(Valor=round((sum(VALOR)/1000000000), digits = 5),
              .groups = 'drop')
plot1 <- ggplot(teste,
       aes(reorder(factor(UF), desc(Valor)),
           y = Valor,
           fill = UF,
           text = paste("UF",UF,
                        "<br>",
                        "Valor",paste0("R$",formatC(1000000000*Valor, digits = 2, big.mark=',', format = 'f'))))) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=Valor),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3) +
  xlab("Unidade Federativa") +
  ylab("Valor Recebido") +
  theme_light() +
  ggtitle(paste0("Valores Recebidos do Governo Federal no ano de ", input$vetornome1, " em bilhões de reais"))

plotly::ggplotly(plot1, tooltip = "text") %>%
    plotly::layout(showlegend = FALSE) #%>%
    #plotly::style(textposition = "top")

})

######################################
#############Graficos de Colunas######
######################################

MunEF1 <- reactive({
  MunEF1 <- df1 %>% filter(UF==input$EFMun1)
})

observeEvent(input$EFMun1, {
  updateSelectizeInput(session,'EFMun2',
                       choices=sort(unique(MunEF1()$MUNICIPIO), decreasing = FALSE))
})

MunEF2 <-  eventReactive(req(input$EFMun2), ignoreNULL=FALSE, {

  MunEF2 <- MunEF1() %>%
    filter(MUNICIPIO==input$EFMun2) %>%
    group_by(MUNICIPIO, Ano) %>%
    summarise(Valor=round((sum(VALOR)), digits = 5),
              .groups = 'drop') %>%
    as.data.frame()
})

output$plotEFMun1 <- renderPlotly({

  minimo <- (min(unique(MunEF2()$Ano)))
  maximo <- (max(unique(MunEF2()$Ano)))

  plot1 <- ggplot(MunEF2(),
                  aes(factor(Ano),
                      Valor,
                      fill = factor(Ano),
                      text = paste("Ano:",Ano,
                                   "<br>",
                                   "Valor:",paste0("R$",formatC(Valor, digits = 2, big.mark=',', format = 'f')),
                                   "<br>",
                                   "Município:", input$EFMun2))) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label=sprintf("R$ %.2f", Valor)),
              position = position_dodge(width = 0.9),
              vjust = -0.5) +
    xlab("Ano") +
    ylab("Valor Recebido") +
    theme_light() +
    ggtitle(paste0("Valores Recebidos de ",minimo," a ", maximo, " por ",unique(MunEF2()$MUNICIPIO)))

  plotly::ggplotly(plot1, tooltip = "text") %>%
    plotly::layout(showlegend = FALSE) #%>%
    #plotly::style(textposition = "top")

})

######################################
#############Graficos de Colunas######
######################################

MunEF3 <- reactive({
  MunEF3 <- df1 %>% filter(Ano==input$EFMun3)
})

observeEvent(input$EFMun3, {
  updateSelectizeInput(session,'EFMun4',
                       choices=sort(unique(MunEF3()$UF), decreasing = FALSE))
})

MunEF4 <- reactive({
  MunEF4 <- MunEF3() %>% filter(UF==input$EFMun4)
})

observeEvent(input$EFMun4, {
  updateSelectizeInput(session,'EFMun5',
                       choices=sort(unique(MunEF4()$MUNICIPIO), decreasing = FALSE))
})

MunEF5 <-  eventReactive(req(input$EFMun5), ignoreNULL=FALSE, {

  MunEF5 <- MunEF4() %>%
    filter(MUNICIPIO==input$EFMun5) %>%
    group_by(MUNICIPIO, LINGUAGEM_CIDADA) %>%
    summarise(Valor=round((sum(VALOR)), digits = 5),
              .groups = 'drop') %>%
    as.data.frame()
})

output$plotEFMun2 <- renderPlotly({

  #minimo <- (min(unique(MunEF2()$Ano)))
  #maximo <- (max(unique(MunEF2()$Ano)))

  plot1 <- ggplot(MunEF5(),
                  aes(x=factor(LINGUAGEM_CIDADA),
                      y = Valor,
                      fill = factor(LINGUAGEM_CIDADA),
                      text = paste("Programa:",LINGUAGEM_CIDADA,
                                   "<br>",
                                   "Valor:",paste0("R$",formatC(Valor, digits = 2, big.mark=',', format = 'f')),
                                   "<br>",
                                   "Município:", input$EFMun5))) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label=sprintf("R$ %.2f", Valor)),
              position = position_dodge(width = 0.9),
              vjust = -0.5) +
    xlab("LINGUAGEM_CIDADA") +
    ylab("Valor Recebido") +
    theme_light() +
    ggtitle(paste0("Valores Recebidos pelo Município de ", unique(MunEF5()$MUNICIPIO), " no ano de ", input$EFMun3))

  plotly::ggplotly(plot1, tooltip = "text") %>%
    plotly::layout(showlegend = FALSE) #%>%
    #plotly::style(textposition = "top")

})

#####################################
#########Tabelas de  Dupla Entrada###
##############Estados#############
#####################################

output$EstadosDupla <- DT::renderDataTable({
  teste <- df1 %>%
    filter(Ano==input$vetorEstadosDupla1) %>%
    ungroup() %>%
    select(UF, LINGUAGEM_CIDADA, VALOR) %>%
    group_by(UF, LINGUAGEM_CIDADA) %>%
    summarise(VALOR=sum(VALOR), .groups = 'drop')

  teste <- teste %>%
    pivot_wider(names_from = LINGUAGEM_CIDADA,
                values_from = VALOR,
                values_fill = 0) %>%
    mutate(Total=rowSums(.[-1]))
  teste$UF <- as.character(teste$UF)
  teste <- teste %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(where(is.character), ~"Total")))


  DT::datatable(teste,
                class = 'cell-border stripe',
                extensions = 'Buttons', options = list(
                  dom = 'Bfrtip',
                  buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
                ))
})
#####################################
#########Tabelas de  Dupla Entrada###
##############Municipios#############
#####################################

table1 <- reactive({
  table1 <- df1 %>% filter(UF==input$vetorDupla1)
})

observeEvent(req(input$vetorDupla1), {
  updateSelectizeInput(session,'vetorDupla2',
                       choices=sort(unique(table1()$MUNICIPIO), decreasing = FALSE))
})

table2 <- eventReactive(req(input$vetorDupla2), ignoreNULL=FALSE, {

   table2 <- table1() %>%
     filter(MUNICIPIO==input$vetorDupla2) %>%
     ungroup() %>%
     select(Ano, LINGUAGEM_CIDADA, VALOR) %>%
     group_by(Ano, LINGUAGEM_CIDADA) %>%
     summarise(VALOR=sum(VALOR), .groups = 'drop')

   table2 <- table2 %>%
     pivot_wider(names_from = LINGUAGEM_CIDADA,
                 values_from = VALOR,
                 values_fill = 0) %>%
     mutate(Total=rowSums(.[-1]))
   table2$Ano <- as.character(table2$Ano)
   table2 <- table2 %>%
     bind_rows(summarise(.,
                         across(where(is.numeric), sum),
                         across(where(is.character), ~"Total")))
 })

output$tableDupla <- DT::renderDataTable({

# tb1 <- df3 %>%
#   filter(UF == input$vetorDupla2 & Ano == input$vetorDupla1) %>%
#   select(-c(UF, Ano))
# names(tb1) <- c("Município",
#                 "LP - 5° Ano",
#                 "MT - 5° Ano",
#                 "LP - 9° Ano",
#                 "MT - 9° Ano")

DT::datatable(table2(),
              class = 'cell-border stripe',
              extensions = 'Buttons', options = list(
              dom = 'Bfrtip',
              buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
              ))
})

