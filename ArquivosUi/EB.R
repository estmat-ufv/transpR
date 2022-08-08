tabsetPanel(
  tabPanel("Explicações",

  ),#tabPanel1
  tabPanel("Gráficos de Colunas ou de Barras",
           tabsetPanel(
             tabPanel("Barras Empilhadas",


           fluidRow(column(3,sidebarPanel(width = 12,
                           selectInput("EB1",
                                       strong("Escolha uma opção:"),
                                       choices=sort(unique(df1$Ano), decreasing = FALSE),
                                       selected = "2020"),
                           selectizeInput("EB2",
                                       strong("Escolha uma opção:"),
                                       choices=NULL),
                           selectInput("EB3",
                                       strong("Escolha uma opção:"),
                                       choices=NULL))),
                    column(9, mainPanel(br(),#
                                        fluidRow(column(12,plotlyOutput("plotEB1", height = 600))))
                           ))
           ))),
  ##
  tabPanel("Gráficos de Pizza",
           fluidRow(column(3,
                           selectInput("vetornome8",
                                       strong("Escolha uma opção:"),
                                       choices=c("ADILSONVOL2CAP6EXE1",
                                                 "ADILSONVOL2CAP6EXE2",
                                                 "ADILSONVOL2CAP20EXEMP1"),
                                       selected = "ADILSONVOL2CAP6EXE1")),
                    column(9,
                           DT::dataTableOutput("table9"))))


)# barra de navegacao interna
