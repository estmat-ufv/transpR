tabsetPanel(
  tabPanel("Explicações",
           column(width = 12,
                  align="center",
                  tags$iframe(#width="90vh",
                    #height="90vh",
                    src="EF2.jpeg",
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
             tabPanel("Estados (Notas)",
           column(3, sidebarPanel(width = 12,
                                  selectInput("vetornome3",
                                              strong("Escolha uma opção:"),
                                              choices=unique(df3$Ano),
                                              selected = max(df3$Ano)),
                                  selectInput("vetornome4",
                                              strong("Escolha uma opção:"),
                                              choices=c("LP - 5° Ano",
                                                        "MT - 5° Ano",
                                                        "LP - 9° Ano",
                                                        "MT - 9° Ano"),
                                              selected = "LP - 5° Ano"),
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
                               fluidRow(column(12,infoBoxOutput("Box2"), tags$style("#Box2 {width:1000px;}"))),#fluidRow
                               br(),
                               fluidRow(column(12,plotlyOutput("plot2", height = 900, width = "1000px"))),
                               br(),
                               # fluidRow(HTML('<div style = "text-align: justify;"><p>
                               #                     Um gráfico de colunas é criado exibindo um campo
                               #                     de string no eixo x e um campo de contagem, campo
                               #                     numérico ou taxa/proporção no eixo y. O
                               #                     comprimento de cada coluna representa o valor
                               #                     de cada categoria. é um gráfico com barras retangulares
                               #                     e comprimento proporcional aos valores que ele
                               #                     apresenta. As barras podem ser desenhadas na
                               #                     vertical ou na horizontal. Um eixo do gráfico mostra especificamente
                               #                     o que está sendo comparado enquanto o outro eixo representa
                               #                     valores discretos. Alguns gráficos de barra apresentam barras
                               #                     agrupadas em grupos (gráficos de barras agrupadas) e outros
                               #                     mostram as barras divididas em sub-partes para mostrar efeito
                               #                     acumulativo (gráficos de barras empilhadas)</p></div>'),
                               #                 tags$ul(
                               #                   tags$b("Fonte:"), tags$a(href = "https://pt.wikipedia.org/wiki/Gr%C3%A1fico_de_barras#:~:text=Em%20uma%20coluna%20de%20um,de%20barra%20agrupados%20e%20empilhados.", "Wikipédia, a enciclopédia livre.")
                               #                 )),
                               width = 9)#mainPanel
           )#Column
  ),
  tabPanel("Estados (Percentuais)",
           column(3, sidebarPanel(width = 12,
                                  selectInput("vetorPnome3",
                                              strong("Escolha uma opção:"),
                                              choices=unique(df3$Ano),
                                              selected = max(df3$Ano)),
                                  selectInput("vetorPnome4",
                                              strong("Escolha uma opção:"),
                                              choices=c("LP - 5° Ano",
                                                        "MT - 5° Ano",
                                                        "LP - 9° Ano",
                                                        "MT - 9° Ano"),
                                              selected = "LP - 5° Ano"),
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
                               fluidRow(column(12,infoBoxOutput("BoxP2"), tags$style("#BoxP2 {width:1000px;}"))),#fluidRow
                               br(),
                               fluidRow(column(12,plotlyOutput("plotP2", height = 900, width = "1000px"))),
                               br(),
                               # fluidRow(HTML('<div style = "text-align: justify;"><p>
                               #                     Um gráfico de colunas é criado exibindo um campo
                               #                     de string no eixo x e um campo de contagem, campo
                               #                     numérico ou taxa/proporção no eixo y. O
                               #                     comprimento de cada coluna representa o valor
                               #                     de cada categoria. é um gráfico com barras retangulares
                               #                     e comprimento proporcional aos valores que ele
                               #                     apresenta. As barras podem ser desenhadas na
                               #                     vertical ou na horizontal. Um eixo do gráfico mostra especificamente
                               #                     o que está sendo comparado enquanto o outro eixo representa
                               #                     valores discretos. Alguns gráficos de barra apresentam barras
                               #                     agrupadas em grupos (gráficos de barras agrupadas) e outros
                               #                     mostram as barras divididas em sub-partes para mostrar efeito
                               #                     acumulativo (gráficos de barras empilhadas)</p></div>'),
                               #                 tags$ul(
                               #                   tags$b("Fonte:"), tags$a(href = "https://pt.wikipedia.org/wiki/Gr%C3%A1fico_de_barras#:~:text=Em%20uma%20coluna%20de%20um,de%20barra%20agrupados%20e%20empilhados.", "Wikipédia, a enciclopédia livre.")
                               #                 )),
                               width = 9)#mainPanel
           )#Column
  ),
  tabPanel("Municípios (Notas)",
           column(3, sidebarPanel(width = 12,
                                  selectInput("EF2Mun1",
                                              strong("Escolha uma opção:"),
                                              choices=unique(df3$Ano),
                                              selected = max(df3$Ano)),
                                  selectInput("EF2Mun2",
                                              strong("Escolha uma opção:"),
                                              choices=NULL),
                                  selectInput("EF2Mun3",
                                              strong("Escolha uma opção:"),
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
                               fluidRow(column(12,infoBoxOutput("BoxEF2MUN"), tags$style("#BoxEF2MUN {width:1000px;}"))),#fluidRow
                               br(),
                               fluidRow(column(12,plotlyOutput("plotEF2MUN", height = 600, width = "1000px"))),
                               br(),
                               # fluidRow(HTML('<div style = "text-align: justify;"><p>
                               #                     Um gráfico de colunas é criado exibindo um campo
                               #                     de string no eixo x e um campo de contagem, campo
                               #                     numérico ou taxa/proporção no eixo y. O
                               #                     comprimento de cada coluna representa o valor
                               #                     de cada categoria. é um gráfico com barras retangulares
                               #                     e comprimento proporcional aos valores que ele
                               #                     apresenta. As barras podem ser desenhadas na
                               #                     vertical ou na horizontal. Um eixo do gráfico mostra especificamente
                               #                     o que está sendo comparado enquanto o outro eixo representa
                               #                     valores discretos. Alguns gráficos de barra apresentam barras
                               #                     agrupadas em grupos (gráficos de barras agrupadas) e outros
                               #                     mostram as barras divididas em sub-partes para mostrar efeito
                               #                     acumulativo (gráficos de barras empilhadas)</p></div>'),
                               #                 tags$ul(
                               #                   tags$b("Fonte:"), tags$a(href = "https://pt.wikipedia.org/wiki/Gr%C3%A1fico_de_barras#:~:text=Em%20uma%20coluna%20de%20um,de%20barra%20agrupados%20e%20empilhados.", "Wikipédia, a enciclopédia livre.")
                               #                 )),
                               width = 9)#mainPanel
           )#Column
              )

              )),#tabPanel1
  ##
  tabPanel("Tabelas de Dupla Entrada",
           column(3, sidebarPanel(width = 12,
                                  selectInput("vetorDuplaAno",
                                              strong("Escolha uma opção:"),
                                              choices=unique(df3$Ano),
                                              selected = max(df3$Ano)),
                                              # choices=c("Nota 5° Ano - Lingua Portuguesa"=1,
                                              #           "Nota 5° Ano - Matemática"=2,
                                              #           "Nota 9° Ano - Lingua Portuguesa"=3,
                                              #           "Nota 9° Ano - Matemática"=4),
                                              #selected = 1),
                                  HTML('<div style = "text-align: justify;"><p>
                                                   As  tabelas  de  dupla  entrada  ou  tabelas
                             de  contingência são formadas  pelo  cruzamento
                             de  duas variáveis categóricas, portanto, é uma
                             matriz de tamanho “lxc” formada por “l” linhas
                             contendo as categorias de uma variável na linha
                             e “c” colunas contendo as categorias de outra
                             variável na coluna"</p></div>'),
                                  # tags$ul(
                                  #   tags$b("Fonte:"), tags$a(href = "https://pt.wikipedia.org/wiki/Gr%C3%A1fico_de_barras#:~:text=Em%20uma%20coluna%20de%20um,de%20barra%20agrupados%20e%20empilhados.", "Wikipédia, a enciclopédia livre.")
                                  # ),
                                  br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                  br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br())#sidebarPanel
           ),#Column
           mainPanel(br(),
                     fluidRow(column(12,DT::dataTableOutput("tableDupla2"))),#fluidRow
           )#mainPanel
  ),#tabPanel2,

  tabPanel("Diagrama de Ramos e Folhas",
           fluidRow(column(3,
                           selectInput("vetorTreeAno",
                                       strong("Escolha uma opção:"),
                                       choices=unique(df3$Ano),
                                       selected = max(df3$Ano)),
                           selectInput("vetorTree",
                                       strong("Escolha uma opção:"),
                                       choices=unique(df3$UF),
                                       selected = "MG"),
                           selectInput("vetorTreeProva",
                                       strong("Escolha uma opção:"),
                                       choices=c("Nota 5° Ano - Lingua Portuguesa"=1,
                                                 "Nota 5° Ano - Matemática"=2,
                                                 "Nota 9° Ano - Lingua Portuguesa"=3,
                                                 "Nota 9° Ano - Matemática"=4),
                                       selected = 1)),
                    column(9,
                           verbatimTextOutput("plotTree")))),
  tabPanel("Gráficos de Pizza",
           fluidRow(column(3,
                           selectInput("vetorPie1",
                                       strong("Escolha uma opção:"),
                                       choices=unique(df3$Ano),
                                       selected = max(df3$Ano)),
                           selectizeInput("vetorPie2",
                                       strong("Escolha uma opção:"),
                                       choices=NULL),
                           selectInput("vetorPie3",
                                       strong("Escolha uma opção:"),
                                       choices = NULL)),
                    column(9,
                           plotlyOutput("plotPie"))))
                           #DT::dataTableOutput("tableDupla5"))))

)# barra de navegacao interna


#Grafico de barras ou colunas
#Criar uma tabela de dupla entrada
#Graficos com variaveis categoricas (discretas e continuas)
#Criar um grafico pictoricos
