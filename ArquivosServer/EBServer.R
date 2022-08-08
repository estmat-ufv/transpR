##################################
#############Grafico de Colunas Empilhadas
#################################

BEB1 <- reactive({
  BEB1 <- df1 %>% filter(Ano==input$EB1)
})

observeEvent(input$EB1, {
  updateSelectizeInput(session,'EB2',
                       choices=sort(unique(BEB1()$UF), decreasing = FALSE))
})

BEB2 <- reactive({
  BEB2 <- BEB1() %>% filter(UF==input$EB2)
})

observeEvent(input$EB2, {
  updateSelectInput(session,'EB3',
                    choices=sort(unique(BEB2()$MUNICIPIO), decreasing = FALSE))
})

BEB3 <-  eventReactive(input$EB3, ignoreNULL=FALSE, {

  BEB3 <- BEB2() %>%
    filter(MUNICIPIO == input$EB3) %>%
    ungroup() %>%
    select(c(MUNICIPIO, LINGUAGEM_CIDADA, VALOR)) %>%
    group_by(MUNICIPIO, LINGUAGEM_CIDADA) %>%
    summarise(VALOR=sum(VALOR),
              .groups = 'drop')
})

output$plotEB1 <- renderPlotly({

  BEB3() %>%
    gather(variable, value, LINGUAGEM_CIDADA) %>%
    plotly::plot_ly(x = ~variable,
            y = ~VALOR,
            type = 'bar',
            name = ~value,
            hovertemplate = "Valor Recebido: R$ %{y}",
            text = ~VALOR,
            textfont = list(size = 15),
            textposition = "outside") %>%
    plotly::layout(yaxis = list(title = 'Valor Recebido'),
           xaxis = list(title = 'Variável'),
           barmode = 'stack')


})
##################################
#############Graficos
#################################

# output$table8 <- DT::renderDataTable({
#
#   DT::datatable(BEB1(),
#                 class = 'cell-border stripe',
#                 extensions = 'Buttons', options = list(
#                   dom = 'Bfrtip',
#                   buttons = c('pageLength','copy', 'csv', 'excel', 'pdf', 'print')
#                 ))
# })
#
#
#
#
