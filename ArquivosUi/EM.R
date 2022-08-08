tabsetPanel(
  tabPanel("Explicações",
           column(width = 12,
                  align="center",
                  tags$iframe(#width="90vh",
                    #height="90vh",
                    src="EM.jpeg",
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
           column(3, sidebarPanel(width = 12,
                                  selectInput("vetorEM1",
                                              strong("Escolha uma opção:"),
                                              choices=sort(unique(df1$Ano), decreasing = FALSE),
                                              selected = max(df1$Ano)),
                                  selectizeInput("vetorEM2",
                                              strong("Escolha uma opção:"),
                                              choices=NULL),
                                  selectizeInput("vetorEM3",
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
                               fluidRow(column(12,infoBoxOutput("Box3"), tags$style("#Box3 {width:1000px;}"))),#fluidRow
                               br(),
                               fluidRow(#column(6,DT::dataTableOutput("tableEM4", width = "400px")),
                                        column(12,plotlyOutput("plot1EM3", height = 500,width = "900px"))),
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
  ),#tabPanel1
  ##
  tabPanel("Tabelas de Dupla Entrada",
           column(3, sidebarPanel(width = 12,
                                  selectInput("vetorEM4",
                                              strong("Escolha uma opção:"),
                                              choices=unique(df2$Ano),
                                              selected = max(df2$Ano)),
                                  selectizeInput("vetorEM5",
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
                                  # tags$ul(
                                  #   tags$b("Fonte:"), tags$a(href = "https://pt.wikipedia.org/wiki/Gr%C3%A1fico_de_barras#:~:text=Em%20uma%20coluna%20de%20um,de%20barra%20agrupados%20e%20empilhados.", "Wikipédia, a enciclopédia livre.")
                                  # ),
                                  br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                  br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br())#sidebarPanel
           ),#Column
           mainPanel(br(),
                     fluidRow(column(12,DT::dataTableOutput("tableDuplaEM"))),#fluidRow
           )#mainPanel
  ),#tabPanel2

  # tabPanel("Gráficos Pictóricos",
  #          fluidRow(column(3,
  #                          selectInput("vetorPicEM",
  #                                      strong("Escolha uma opção:"),
  #                                      choices=NULL,
  #                                      selected = NULL)),
  #                   column(9,
  #                          #echarts4rOutput("plotPicEM"),
  #                          br(),
  #                          hr(),
  #                          p("Um gráfico de imagem, ou pictograma, é um gráfico usado para exibir informações que usam imagens ou símbolos para representar dados."),
  #                          tags$ul(
  #                            tags$li(tags$b("Simpsons:"), tags$a(href = "https://rstudio-pubs-static.s3.amazonaws.com/571217_ae5270f8aaa541ac9a69b0f337613277.html", "Pictogramas e infografías con R")),
  #                            tags$li(tags$b("Lápis:"), tags$a(href = "https://www.listendata.com/2019/06/create-infographics-with-r.html", "Create Infographics With R")),
  #                            tags$li(tags$b("Foguetes:"), tags$a(href = "https://rstudio-pubs-static.s3.amazonaws.com/571217_ae5270f8aaa541ac9a69b0f337613277.html", "Pictogramas e infografías con R"))
  #                          )))),#tabPanel3,

  tabPanel("Diagrama de Ramos e Folhas",
           fluidRow(column(3,
                           selectInput("EMTreeAno",
                                       strong("Escolha uma opção:"),
                                       choices=unique(df3$Ano),
                                       selected = max(df3$Ano)),
                           selectInput("EMTreeUF",
                                       strong("Escolha uma opção:"),
                                       choices=unique(df3$UF),
                                       selected = "MG"),
                           selectInput("EMTreeProva",
                                       strong("Escolha uma opção:"),
                                       choices=c("Nota em Ciências da Natureza"=1,
                                                 "Nota em Ciências Humanas"=2,
                                                 "Nota em Linguagens"=3,
                                                 "Nota em Matemática"=4,
                                                 "Nota em Redação"=5,
                                                 "Nota Média"=6),
                                       selected = 1)),
                    column(9,
                           verbatimTextOutput("plotEMTree")))),
  tabPanel("Gráficos de Pizza",
           fluidRow(column(3,
                           selectInput("EMPie1",
                                       strong("Escolha uma opção:"),
                                       choices=unique(df2$Ano),
                                       selected = max(df2$Ano)),
                           selectizeInput("EMPie2",
                                          strong("Escolha uma opção:"),
                                          choices=NULL),
                           selectInput("EMPie3",
                                       strong("Escolha uma opção:"),
                                       choices = NULL)),
                    column(9,
                           plotlyOutput("EMplotPie")))),
  tabPanel("Boxplot",
           fluidRow(column(3,
                           selectInput("EMBox1",
                                       strong("Escolha uma opção:"),
                                       choices=unique(df2$Ano),
                                       selected = max(df2$Ano)),
                           selectInput("EMBox2",
                                       strong("Escolha uma opção:"),
                                       choices=unique(df2$UF),
                                       selected = "MG")),
                    column(9,
                           plotlyOutput("EMplotBox"))))

)# barra de navegacao interna


#Grafico de barras ou colunas
#Criar uma tabela de dupla entrada
#Graficos com variaveis categoricas (discretas e continuas)
#Criar um grafico pictoricos
