tabsetPanel(
  tabPanel("Explicações",
           column(width = 10,
                  align="center",
             tags$iframe(#width="90vh",
                         #height="90vh",
                         src="EF1.gif",
                         frameborder="0",
                         #allow="accelerometer;
                         #autoplay;
                         #encrypted-media;
                         #gyroscope;
                         #picture-in-picture",
                         style='width:100vw;height:100vh;',
                         allowfullscreen=NA,
                         type = "video/mp4",
                         autoplay=TRUE,
                         muted=TRUE,
                         playsinline=TRUE,
                         loop=TRUE,
                         controls=TRUE))
           #)
           #tags$video(id="video2", type = "video/mp4",src = "EF1.mp4", controls = "controls")
  ),#tabPanel1
  tabPanel("Gráficos de Colunas ou Barras",
           tabsetPanel(
             tabPanel("Total - Estados",
           column(3, sidebarPanel(width = 12,
                  selectInput("vetornome1",
                              strong("Escolha o Ano:"),
                              choices=unique(df1$Ano),
                              selected = max(df1$Ano)),
                  HTML('<div style = "text-align: justify;"><p>
                                                   Um gráfico de colunas é criado exibindo um campo
                                                   de string no eixo x e um campo de contagem, campo
                                                   numérico ou taxa/proporção no eixo y. O
                                                   comprimento de cada coluna representa o valor
                                                   de cada categoria. é um gráfico com barras retangulares
                                                   e comprimento proporcional aos valores que ele
                                                   apresenta. As barras podem ser desenhadas na
                                                   vertical ou na horizontal. Um eixo do gráfico mostra especificamente
                                                   o que está sendo comparado enquanto o outro eixo representa
                                                   valores discretos. Alguns gráficos de barra apresentam barras
                                                   agrupadas em grupos (gráficos de barras agrupadas) e outros
                                                   mostram as barras divididas em sub-partes para mostrar efeito
                                                   acumulativo (gráficos de barras empilhadas)</p></div>'),
                  tags$ul(
                    tags$b("Fonte:"), tags$a(href = "https://pt.wikipedia.org/wiki/Gr%C3%A1fico_de_barras#:~:text=Em%20uma%20coluna%20de%20um,de%20barra%20agrupados%20e%20empilhados.", "Wikipédia, a enciclopédia livre.")
                  ),
                  br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                  br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br())#sidebarPanel
           ),#Column
           column(9, mainPanel(br(),#
                               fluidRow(#column(6,DT::dataTableOutput("table1", width = "400px")),
                                 column(12,plotlyOutput("plot1", height = 500,width = "1000px"))),
                               br(),
                               #fluidRow(column(12,infoBoxOutput("Box1"), tags$style("#Box1 {width:1000px;}"))),#fluidRow,
                               br(),
                               width = 9)#mainPanel
                  )#Column
           ),
             tabPanel("Total - Municípios",
                      column(3, sidebarPanel(width = 12,
                                             selectInput("EFMun1",
                                                         strong("Escolha o Estado:"),
                                                         choices=sort(unique(df1$UF), decreasing = FALSE),
                                                         selected = "MG"),
                                             selectizeInput("EFMun2",
                                                         strong("Escolha o Município:"),
                                                         choices=NULL),
                                             HTML('<div style = "text-align: justify;"><p>
                                                   Um gráfico de colunas é criado exibindo um campo
                                                   de string no eixo x e um campo de contagem, campo
                                                   numérico ou taxa/proporção no eixo y. O
                                                   comprimento de cada coluna representa o valor
                                                   de cada categoria. é um gráfico com barras retangulares
                                                   e comprimento proporcional aos valores que ele
                                                   apresenta. As barras podem ser desenhadas na
                                                   vertical ou na horizontal. Um eixo do gráfico mostra especificamente
                                                   o que está sendo comparado enquanto o outro eixo representa
                                                   valores discretos. Alguns gráficos de barra apresentam barras
                                                   agrupadas em grupos (gráficos de barras agrupadas) e outros
                                                   mostram as barras divididas em sub-partes para mostrar efeito
                                                   acumulativo (gráficos de barras empilhadas)</p></div>'),
                                             tags$ul(
                                               tags$b("Fonte:"), tags$a(href = "https://pt.wikipedia.org/wiki/Gr%C3%A1fico_de_barras#:~:text=Em%20uma%20coluna%20de%20um,de%20barra%20agrupados%20e%20empilhados.", "Wikipédia, a enciclopédia livre.")
                                             ),
                                             br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                             br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br())#sidebarPanel
                      ),#Column
                      column(9, mainPanel(br(),#
                                          fluidRow(column(12,infoBoxOutput("BoxEFMun1"), tags$style("#BoxEFMun1 {width:1000px;}"))),#fluidRow
                                          br(),
                                          fluidRow(#column(6,DT::dataTableOutput("table1", width = "400px")),
                                            column(12,plotlyOutput("plotEFMun1", height = 600,width = "1100px"))),
                                          br(),
                                          width = 9)#mainPanel
                      )#Column
           ),
           tabPanel("Fonte do Recurso por Município",
                    column(3, sidebarPanel(width = 12,
                                           selectInput("EFMun3",
                                                       strong("Escolha o Estado:"),
                                                       choices=sort(unique(df1$Ano), decreasing = FALSE),
                                                       selected = max(unique(df1$Ano))),
                                           selectInput("EFMun4",
                                                       strong("Escolha o Estado:"),
                                                       choices=NULL),
                                           selectizeInput("EFMun5",
                                                          strong("Escolha o Município:"),
                                                          choices=NULL),
                                           HTML('<div style = "text-align: justify;"><p>
                                                   Um gráfico de colunas é criado exibindo um campo
                                                   de string no eixo x e um campo de contagem, campo
                                                   numérico ou taxa/proporção no eixo y. O
                                                   comprimento de cada coluna representa o valor
                                                   de cada categoria. é um gráfico com barras retangulares
                                                   e comprimento proporcional aos valores que ele
                                                   apresenta. As barras podem ser desenhadas na
                                                   vertical ou na horizontal. Um eixo do gráfico mostra especificamente
                                                   o que está sendo comparado enquanto o outro eixo representa
                                                   valores discretos. Alguns gráficos de barra apresentam barras
                                                   agrupadas em grupos (gráficos de barras agrupadas) e outros
                                                   mostram as barras divididas em sub-partes para mostrar efeito
                                                   acumulativo (gráficos de barras empilhadas)</p></div>'),
                                           tags$ul(
                                             tags$b("Fonte:"), tags$a(href = "https://pt.wikipedia.org/wiki/Gr%C3%A1fico_de_barras#:~:text=Em%20uma%20coluna%20de%20um,de%20barra%20agrupados%20e%20empilhados.", "Wikipédia, a enciclopédia livre.")
                                           ),
                                           br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                           br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br())#sidebarPanel
                    ),#Column
                    column(9, mainPanel(br(),#
                                        fluidRow(column(12,infoBoxOutput("BoxEFMun2"), tags$style("#BoxEFMun2 {width:1000px;}"))),#fluidRow
                                        br(),
                                        fluidRow(#column(6,DT::dataTableOutput("table1", width = "400px")),
                                          column(12,plotlyOutput("plotEFMun2", height = 600,width = "1100px"))),
                                        br(),
                                        width = 9)#mainPanel
                    )#Column
           )
           )),#tabPanel2
  tabPanel("Gráfico de Linhas",
           column(3, sidebarPanel(width = 12,
                                           selectInput("Linha1",
                                                       strong("Escolha o Estado:"),
                                                       choices=sort(unique(df1$UF), decreasing = FALSE),
                                                       selected = "MG"),
                                           selectizeInput("Linha2",
                                                          strong("Escolha o Município:"),
                                                          choices=NULL),
                                           HTML('<div style = "text-align: justify;"><p>
                                                   Um gráfico de colunas é criado exibindo um campo
                                                   de string no eixo x e um campo de contagem, campo
                                                   numérico ou taxa/proporção no eixo y. O
                                                   comprimento de cada coluna representa o valor
                                                   de cada categoria. é um gráfico com barras retangulares
                                                   e comprimento proporcional aos valores que ele
                                                   apresenta. As barras podem ser desenhadas na
                                                   vertical ou na horizontal. Um eixo do gráfico mostra especificamente
                                                   o que está sendo comparado enquanto o outro eixo representa
                                                   valores discretos. Alguns gráficos de barra apresentam barras
                                                   agrupadas em grupos (gráficos de barras agrupadas) e outros
                                                   mostram as barras divididas em sub-partes para mostrar efeito
                                                   acumulativo (gráficos de barras empilhadas)</p></div>'),
                                           tags$ul(
                                             tags$b("Fonte:"), tags$a(href = "https://pt.wikipedia.org/wiki/Gr%C3%A1fico_de_barras#:~:text=Em%20uma%20coluna%20de%20um,de%20barra%20agrupados%20e%20empilhados.", "Wikipédia, a enciclopédia livre.")
                                           ),
                                           br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                           br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br())#sidebarPanel
                    ),#Column
                    column(9, mainPanel(br(),#
                                        fluidRow(column(12,infoBoxOutput("BoxLinha1"), tags$style("#BoxLinha1 {width:1000px;}"))),#fluidRow
                                        br(),
                                        fluidRow(#column(6,DT::dataTableOutput("table1", width = "400px")),
                                          column(12,plotlyOutput("plotLinha1", height = 600,width = "1100px"))),
                                        br(),
                                        width = 9)#mainPanel
                    )#Column
  ),#tabPanel3
  tabPanel("Tabelas de Dupla Entrada",
           tabsetPanel(
             tabPanel("Estados",
                      column(3, sidebarPanel(width = 12,
                                             selectInput("vetorEstadosDupla1",
                                                         strong("Escolha uma opção:"),
                                                         choices=unique(df1$Ano),
                                                         selected = max(unique(df1$Ano))),
                                             HTML('<div style = "text-align: justify;"><p>
                             As  tabelas  de  dupla  entrada  ou  tabelas
                             de  contingência são formadas  pelo  cruzamento
                             de  duas variáveis categóricas, portanto, é uma
                             matriz de tamanho “lxc” formada por “l” linhas
                             contendo as categorias de uma variável na linha
                             e “c” colunas contendo as categorias de outra
                             variável na coluna"</p></div>'),
                                             br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                             br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br())#sidebarPanel
                      ),#Column
                      mainPanel(br(),
                                fluidRow(column(12,DT::dataTableOutput("EstadosDupla"))),#fluidRow
                      )#mainPanel
                      ),
               tabPanel("Municípios",
           column(3, sidebarPanel(width = 12,
                                  selectInput("vetorDupla1",
                                              strong("Escolha uma opção:"),
                                              choices=unique(df3$UF),
                                              selected = "MG"),
                                  selectizeInput("vetorDupla2",
                                              strong("Escolha uma opção:"),
                                              choices=NULL),
                                  HTML('<div style = "text-align: justify;"><p>
                             As  tabelas  de  dupla  entrada  ou  tabelas
                             de  contingência são formadas  pelo  cruzamento
                             de  duas variáveis categóricas, portanto, é uma
                             matriz de tamanho “lxc” formada por “l” linhas
                             contendo as categorias de uma variável na linha
                             e “c” colunas contendo as categorias de outra
                             variável na coluna"</p></div>'),
                                  br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                  br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br())#sidebarPanel
           ),#Column
           mainPanel(br(),
                     fluidRow(column(12,DT::dataTableOutput("tableDupla"))),#fluidRow
                               )#mainPanel
  )))

)# tabsetPanel
