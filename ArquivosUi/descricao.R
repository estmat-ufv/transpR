tabsetPanel(
           tabPanel("Sobre o Dashboard",

                    fluidRow(
                      column(12,
                             h1("Sobre o Dashboard"),
                             sidebarPanel(
                               p(style="text-align: justify; font-size:20px", "Este dashboard é resultado do trabalho de
                                        dissertação de mestrado do estudante do Curso de Mestrado Profissional
                                        em Rede Nacional Adelson Viégas Ferreira, orientado pelo professor
                                        Fernando de Souza Bastos, ambos da Universidade Federal de Viçosa -
                                        campus UFV - Florestal."),
                               p(style="text-align: justify;","O código utilizado para construir
                                          a aplicação está disponível em um repositório aberto no GitHub
                                          (https://github.com/fsbmat-ufv/stats4ebII)."),
                               br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                             ),
                             column(width = 5,
                                    align="center",
                                    tags$iframe(#width="90vh",
                                      #height="90vh",
                                      src="Capa3.png",
                                      frameborder="0",
                                      #allow="accelerometer;
                                      #autoplay;
                                      #encrypted-media;
                                      #gyroscope;
                                      #picture-in-picture",
                                      style='width:66vw;height:100vh;',
                                      allowfullscreen=NA,
                                      type = "video/mp4",
                                      autoplay=TRUE,
                                      muted=TRUE,
                                      playsinline=TRUE,
                                      loop=TRUE,
                                      controls=TRUE))
                             #h4(style="text-align: justify;","  Um dos objetivos da Estatística
                             #é apresentar dados de forma simples e objetiva, ou seja, de forma
                             #significativa. Freqüentemente, os conjuntos de dados envolvem uma
                             #quantidade enorme de valores. São muitos para imprimir em um texto
                             #escrito. É aí que os gráficos podem ser necessários, permitindo que os
                             #estatísticos forneçam uma interpretação visual de histórias numéricas
                             #complexas.
#
                             #Bons gráficos transmitem informações de forma rápida e fácil para o
                             #usuário. Os gráficos destacam as características salientes dos dados.
                             #Eles podem mostrar relacionamentos que não são óbvios ao estudar uma
                             #lista de números. Eles também podem fornecer uma maneira conveniente
                             #de comparar diferentes conjuntos de dados.
#
                             #Diferentes situações exigem diferentes tipos de gráficos e é útil ter
                             #um bom conhecimento dos tipos disponíveis. O tipo de dado geralmente
                             #determina qual gráfico é apropriado para uso. Dados qualitativos, dados
                             #quantitativos e dados emparelhados, cada um usa diferentes tipos de
                             #gráficos."),
                      )))
           )# barra de navegacao interna

