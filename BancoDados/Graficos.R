rm(list=ls())#limpar memoria
cat("\014")#limpar console
library(data.table)
library(tidyverse)
library(plotly)
setwd("~/Projetos/3Projetos_de_Pesquisa/2020/ProfMat/Adelson/ProjetoDissertacao/shiny/BancoDados")
options("scipen"=100)
df1 <- readRDS("Transferencias.Rds")
df2 <- readRDS("Enem.Rds")
df3 <- readRDS("saeb.Rds")
df1 %>% ggplot() +
  geom_line(aes(factor(Ano), VALOR), stat="identity")


#Recurso por Estado

teste <- df1 %>%
  filter(Ano==2019) %>%
  group_by(UF) %>%
  summarise(Valor=round((sum(VALOR))/1000000000, digits = 5),
            .groups = 'drop')
ggplot(teste,
       aes(x=factor(UF),
           y = Valor,
           fill = UF)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=Valor),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("Unidade Federativa") +
  ylab("Valor Recebido") +
  theme_light() +
  ggtitle(paste0("Valores Recebidos do Governo Federal \n no ano de 2019"),
          subtitle = paste0("Valores Recebidos em Bilhões de Reais (x", 10^9,")")) #+
  # geom_label(
  #   label="Valores em Bilhões de Reais",
  #   x="RN",
  #   y=4.5,
  #   label.padding = unit(0.55, "lines"), # Rectangle size around label
  #   label.size = 0.35,
  #   color = "black",
  #   fill="#69b3a2"
  # )

## Boxplot

teste <- df1 %>% filter(VALOR<=1000000 & Ano == 2019)

teste %>% ggplot(aes(UF, VALOR, fill = UF)) +
  geom_boxplot(show.legend = FALSE)

## Boxplot

teste <- df2 %>% filter(Ano == 2019)

teste %>% ggplot(aes(UF, NOTA_CN, fill = UF)) +
  geom_boxplot(show.legend = FALSE)

###Treemap

Recursos <-df1 %>%
  filter(UF == "RJ" & Ano == 2019) %>%
  group_by(MUNICIPIO) %>%
  summarise(Valor = sum(VALOR))

Enem <-df2 %>%
  filter(UF == "RJ" & Ano == 2019) %>%
  group_by(MUNICIPIO) %>%
  summarise(Nota = mean(NT))

df <- inner_join(Recursos, Enem, by = "MUNICIPIO")

df$escala <- scale(df$Nota) #necessário para criar valores negativos para deixar as disparidades mais evidentes
library("treemap")
x <- treemap(df, index = "MUNICIPIO", vSize = "Valor", vColor = "escala",
             type = "value", palette = "-RdGy", lowerbound.cex.labels = 0.3,
             title  =  "Treemap de Recursos Recebidos e Notas do ENEM",
             overlap.labels=0.2)


#Nota Media ENEM por Estado

teste <- df2 %>%
  filter(Ano==2019) %>%
  group_by(UF) %>%
  summarise(Nota=round(mean(NT), digits = 2),
            .groups = 'drop')
ggplot(teste,
       aes(x=factor(UF),
           y = Nota,
           fill = UF)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=Nota),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("Unidade Federativa") +
  ylab("Nota Média") +
  theme_light() +
  ggtitle(paste0("Nota Média do ENEM no ano de 2019"),
          subtitle = "Soma de Todas as Notas dividido por 5")

#### Buble Chart

Pop <- fread("PopEstadoMUN.csv", dec = ".")
Pop$POPULACAO <- as.numeric(gsub(".", "", Pop$POPULACAO, fixed = TRUE))
Pop$MUNICIPIO <- str_trim(Pop$MUNICIPIO)
Pop$MUNICIPIO <- str_to_upper(Pop$MUNICIPIO)
Pop$MUNICIPIO <- abjutils::rm_accent(Pop$MUNICIPIO)
#saveRDS(Pop, "PopEstadoMUN.Rds")


Recursos <-df1 %>%
  filter(UF == "MG" & Ano == 2019) %>%
  group_by(MUNICIPIO) %>%
  summarise(Valor = round(sum(VALOR), digits = 2))

Enem <-df2 %>%
  filter(UF == "MG" & Ano == 2019) %>%
  group_by(MUNICIPIO) %>%
  summarise(Nota = mean(NT))
Pop2 <- Pop %>%
  filter(UF == "MG")
df <- inner_join(Recursos, Enem, by = "MUNICIPIO")
df <- inner_join(df, Pop2, by = "MUNICIPIO")

df$PerCapita <- round(df$Valor/df$POPULACAO, digits = 2)

df <- df %>% filter(MUNICIPIO!= "RIO DE JANEIRO")
radius <- sqrt(df$POPULACAO/pi)
symbols(df$Valor,
        df$Nota,
        circles=radius,
        inches=0.35,
        fg="white",
        bg="red",
        xlab="Valor Recebido",
        ylab="Nota") +
  text(df$Valor, df$Nota, df$MUNICIPIO, cex=0.5)

colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')

# Using Plotly
#library(plotly)
plotly::plot_ly(
  df, x = ~`Nota`, y = ~`PerCapita`,
  color = ~`MUNICIPIO`, type = "scatter",
  mode="markers", size=~`radius`,
  marker = list(symbol = 'circle', sizemode = 'diameter',
                line = list(width = 2, color = '#FFFFFF'),
                opacity=0.4),
  text = ~paste('Município:', `MUNICIPIO`,
                '<br>UF:', `UF`,
                '<br>Nota:', round(`Nota`,2),
                '<br>Valor Total Recebido:', round(`Valor`,2),
                '<br>Population:',POPULACAO,
                '<br>Valor Per-Capita:',PerCapita)) %>%
  layout(showlegend = FALSE)

#Nota_CN ENEM por Estado

teste <- df2 %>%
  filter(Ano==2019) %>%
  group_by(UF) %>%
  summarise(Nota=round(mean(NOTA_CN), digits = 2),
            .groups = 'drop')
ggplot(teste,
       aes(x=factor(UF),
           y = Nota,
           fill = UF)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=Nota),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("Unidade Federativa") +
  ylab("Nota Média") +
  theme_light() +
  ggtitle(paste0("Nota Média de Ciências da Natureza no ano de 2019"))

#Candidatos ENEM por Estado

teste <- df2 %>%
  filter(Ano==2019) %>%
  group_by(UF) %>%
  summarise(Cand=round(sum(Cand), digits = 0),
            .groups = 'drop')
ggplot(teste,
       aes(x=factor(UF),
           y = Cand,
           fill = UF)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=Cand),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("Unidade Federativa") +
  ylab("Número de Candidatos") +
  theme_light() +
  ggtitle(paste0("Número de Candidatos por Estado no ano de 2019"))

#Media de Idade por Estado

teste <- df2 %>%
  filter(Ano==2019) %>%
  group_by(UF) %>%
  summarise(Idade=round(mean(ID), digits = 2),
            .groups = 'drop')
ggplot(teste,
       aes(x=factor(UF),
           y = Idade,
           fill = UF)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=Idade),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("Unidade Federativa") +
  ylab("Idade Média dos Candidatos") +
  theme_light() +
  ggtitle(paste0("Idade Média dos Candidatos por Estado no ano de 2019"))

#################################
# UF <- fread("UF.csv")
# names(UF) <- c("UF", "Sigla")
# UF$UF <- str_trim(UF$UF)
# UF$UF <- str_to_upper(UF$UF)
# UF$UF <- abjutils::rm_accent(UF$UF)
# df3$UF <- str_trim(df3$UF)
# df3$UF <- str_to_upper(df3$UF)
# df3$UF <- abjutils::rm_accent(df3$UF)
# df3 <- inner_join(df3, UF, by = "UF")
# df3 <- df3 %>% select(1,8, 3:7)
# names(df3) <- c("MUNICIPIO",
#                 "UF",
#                 "MEDIA_5_LP",
#                 "MEDIA_5_MT",
#                 "MEDIA_9_LP",
#                 "MEDIA_9_MT",
#                 "ANO")
# saveRDS(df3, "saeb.Rds")


#Nota Media LP 5 SAEB por Estado

str(df3$MEDIA_5_LP)

teste <- df3 %>%
  filter(Ano==2019) %>%
  group_by(UF) %>%
  summarise(Nota=round(mean(MEDIA_5_LP), digits = 2),
            .groups = 'drop')
ggplot(teste,
       aes(x=factor(UF),
           y = Nota,
           fill = UF)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=Nota),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("Unidade Federativa") +
  ylab("Nota Média") +
  theme_light() +
  ggtitle(paste0("Nota Média de Lingua Portuguesa do Quinto Ano \n do Saeb no ano de 2019"))


#Nota Media LP 9 SAEB por Estado (Cidades com Nota diferente de zero)

teste <- df3 %>%
  filter(Ano==2019) %>%
  group_by(UF) %>%
  filter(MEDIA_9_LP!= 0) %>%
  summarise(Nota=round(mean(MEDIA_9_LP), digits = 2),
            .groups = 'drop')
ggplot(teste,
       aes(x=factor(UF),
           y = Nota,
           fill = UF)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=Nota),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("Unidade Federativa") +
  ylab("Nota Média") +
  theme_light() +
  ggtitle(paste0("Nota Média de Lingua Portuguesa do Nono Ano \n do Saeb no ano de 2019"))


# Num. de Cidades que nao responderam LP 9 SAEB por Estado (Cidades com Nota igual a zero)

teste <- df3 %>%
  filter(Ano==2017) %>%
  group_by(UF) %>%
  filter(MEDIA_9_LP== 0) %>%
  summarise(Count=n(),
            .groups = 'drop')
ggplot(teste,
       aes(x=factor(UF),
           y = Count,
           fill = UF)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=Count),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("Unidade Federativa") +
  ylab("Quantidade") +
  theme_light() +
  ggtitle(paste0("Número de Cidades que Não Tiveram Avaliação de Lingua Portuguesa no ano de 2019"))

#########################################################

#Valores Recebidos por Municipio

teste <- df1 %>%
  filter(MUNICIPIO == "LAJINHA" & Ano == 2019)

ggplot(teste,
       aes(x=factor(LINGUAGEM_CIDADA),
           y = VALOR,
           fill = LINGUAGEM_CIDADA)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=VALOR),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("LINGUAGEM CIDADA") +
  ylab("Valor Recebido") +
  theme_light() +
  ggtitle(paste0("Valor Total Recebido = R$ ", sum(teste$VALOR)))

#Nota do Enem por Municipio

teste <- df2 %>%
  filter(MUNICIPIO == "LAJINHA" & Ano == 2019)

teste <- reshape2::melt(teste[c(1,4:8)])

ggplot(teste,
       aes(x=factor(variable),
           y = value,
           fill = variable)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=value),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("Provas do ENEM") +
  ylab("Nota") +
  theme_light() +
  ggtitle(paste0("A Nota Média no ENEM foi de ", mean(teste$value)))

#Nota do Enem por Municipio

teste <- df2 %>%
  filter(Ano == 2019)

teste <- teste %>%
  filter(UF == "MG")

teste <- teste %>%
  filter(MUNICIPIO == "LAJINHA") %>%
  as.data.frame() %>% select(c(1,4:8))

names(teste) <- c("Municipio",
                "Ciências da Natureza",
                "Ciências Humanas",
                "Linguagens",
                "Matemática",
                "Redação")

#teste <- reshape2::melt(teste[c(1,4:8)])
#teste <- teste[c(1,4:8)]
teste <- teste %>%
  gather(key = "Provas", value = "Notas", -Municipio)

# Compute the position of labels
teste <- teste %>%
  arrange(desc(Notas)) %>%
  mutate(prop = Notas / sum(teste$Notas) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# Basic piechart
ggplot(teste, aes(x="", y=prop, fill=Provas)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position="none") +
  geom_text(aes(y = ypos,
                label = paste0(Provas, "\n value: ", Notas)), nudge_x = 0.2, color = "black", size=6) +
  scale_fill_brewer(palette="Set1")

plot_ly(teste,
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

#plotly::ggplotly(pie)

#Nota do Saeb por Municipio

teste <- df3 %>%
  filter(MUNICIPIO == "LAJINHA" & Ano == 2019) %>%
  as.data.frame()

teste <- reshape2::melt(teste[c(1,3:6)])

ggplot(teste,
       aes(x=factor(variable),
           y = value,
           fill = variable)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=value),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("Provas do ENEM") +
  ylab("Nota") +
  theme_light() +
  ggtitle(paste0("A Nota Média no ENEM foi de ", mean(teste$value)))

###

# Create test data.

teste <- df3 %>%
  filter(MUNICIPIO == "LAJINHA" & Ano == 2019) %>%
  as.data.frame()

teste <- reshape2::melt(teste[c(1,3:4)])

# Compute percentages
teste$fraction = teste$value / sum(teste$value)

# Compute the cumulative percentages (top of each rectangle)
teste$ymax = cumsum(teste$fraction)

# Compute the bottom of each rectangle
teste$ymin = c(0, head(teste$ymax, n=-1))

# Compute label position
teste$labelPosition <- (teste$ymax + teste$ymin) / 2

# Compute a good label
teste$label <- paste0(teste$variable, "\n value: ", teste$value)

# Make the plot
ggplot(teste, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=variable)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")


#Nota do Saeb por Municipio

teste <- df3 %>%
  filter(Ano == 2017)

teste <- teste %>%
  filter(UF == "AC")

teste <- teste %>%
  filter(MUNICIPIO == "ASSIS BRASIL") %>%
  as.data.frame() %>% select(c(1,3:6))

names(teste) <- c("Municipio",
                "LP - 5° Ano",
                "MT - 5° Ano",
                "LP - 9° Ano",
                "MT - 9° Ano")

#teste <- reshape2::melt(teste[c(1,4:8)])
#teste <- teste[c(1,4:8)]
teste <- teste %>%
  gather(key = "Provas", value = "Notas", -Municipio)

teste <- teste[teste$Notas>0,]


# Compute the position of labels
teste <- teste %>%
  arrange(desc(Notas)) %>%
  mutate(end_angle = 2*pi*cumsum(Notas)/sum(teste$Notas),      # ending angle for each pie slice
         start_angle = lag(end_angle, default = 0),   # starting angle for each pie slice
         mid_angle = 0.5*(start_angle + end_angle))   %>% # middle of each pie slice, for the text label
  mutate(prop = Notas / sum(teste$Notas) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop) %>%
  mutate(hjust = ifelse(mid_angle>pi, 1, 0),
         vjust = ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1))

rpie = 1 # pie radius
rlabel = 0.6 * rpie # radius of the labels; a number slightly larger than 0.5 seems to work better,
# but 0.5 would place it exactly in the middle as the question asks for.

library(ggforce)
# Basic piechart
rlabel = 1.05 * rpie # now we place labels outside of the pies

ggplot(teste) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = rpie,
                   start = start_angle, end = end_angle, fill = Provas)) +
  geom_text(aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle), label = paste0(Provas, "\n value: ", Notas),
                hjust = hjust, vjust = vjust)) +
  coord_fixed() +
  scale_x_continuous(limits = c(-1.5, 1.4), name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1, 1), name = "", breaks = NULL, labels = NULL)




###########

teste <- df2 %>%
  filter(Ano==2019 & UF == "MG") %>%
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
  geom_boxplot(show.legend = FALSE)

plotly::ggplotly(bx)
##################################

teste <- df1 %>% filter(Ano==2019 &
                          MUNICIPIO=="ABAIARA") %>%
  ungroup() %>%
  select(c(MUNICIPIO, LINGUAGEM_CIDADA, VALOR)) %>%
  group_by(MUNICIPIO, LINGUAGEM_CIDADA) %>%
  summarise(VALOR=sum(VALOR),
            .groups = 'drop')

teste %>%
  gather(variable, value, LINGUAGEM_CIDADA) %>%
  plot_ly(x = ~variable,
          y = ~VALOR,
          type = 'bar',
          name = ~value,
          hovertemplate = "Valor Recebido: R$ %{y}",
          text = ~VALOR,
          textfont = list(size = 15),
          textposition = "outside") %>%
  layout(yaxis = list(title = 'Valor Recebido'),
         xaxis = list(title = 'Variável'),
         barmode = 'stack')

teste %>%
  plot_ly() %>%
  add_bars(x = ~LINGUAGEM_CIDADA,
           y = ~VALOR,
           type = 'bar',
           name = ~LINGUAGEM_CIDADA,
           hovertemplate = "Valor Recebido: %{y}"
  ) %>%
  ## Not working
  #    add_text(x = ~Animals, y = ~value,
  #             text = "gotcha", textposition = "top center") %>%
  layout(yaxis = list(title = 'Valor Recebido'), barmode = 'stack')

df <- read.table(text=
"Rank F1     F2     F3
1    500    250    50
2    400    100    30
3    300    155    100
4    200    90     10", header=TRUE)

df %>%
  gather(variable, value, F1:F3) %>%
  ggplot(aes(x = Rank, y = value, fill = variable)) +
  geom_bar(stat = "identity")

teste %>%
  gather(variable, value, LINGUAGEM_CIDADA) %>%
  ggplot(aes(x = variable, y = VALOR, fill = value)) +
  geom_bar(stat = "identity")


teste %>%
  gather(variable, value, LINGUAGEM_CIDADA) %>%
  plot_ly(x = ~variable,
          y = ~VALOR,
          type = 'bar',
          name = ~value,
          hovertemplate = "Valor Recebido: R$ %{y}",
          text = ~VALOR,
          textfont = list(size = 15),
          textposition = "outside") %>%
           layout(yaxis = list(title = 'Valor Recebido'),
                  xaxis = list(title = 'Variável'),
                  barmode = 'stack') #%>%
  # add_annotations(
  #   x = ~variable,
  #   y = ~VALOR,
  #   text = ~value,
  #   showarrow = F,
  #   textfont = list(size = 10)
  # )
###################################

City<-c("X","Y","Z","X","Z","X","Y")
House_Unit_Id<-c("H1","H2","H3","H4","H5","H6","H7")
Adult<-c(50,100,60,40,50,80,60)
Child<-c(40,0,40,20,50,20,30)
Baby<-c(10,0,0,40,0,0,10)
data<-data.frame(City,House_Unit_Id,Adult,Child,Baby)

library(plyr)
# Changing the data frame before plotting ... there is propably an easier way to do this!
newdata <- ldply(3:5,function(n){tempdata <- data[,c(1,n)]
colnames(tempdata)[2] <- "Number"
tempdata$type <- colnames(data[n])
return(tempdata)})
newdata <- ddply(newdata,.(City,type),summarize,Number=sum(Number))
# Total for each city
datatotal <- ddply(newdata,~City,summarize,n=sum(Number))
# Merge the data frames together
newdata <- merge(newdata,datatotal)
# Calc the percentages
newdata$perc <- newdata$Number/newdata$n

plot_ly(newdata,x = ~City, y = ~perc*100, type = 'bar',color = ~type,text=~Number,hoverinfo = 'text') %>%
  layout(yaxis = list(title = 'Percentage (%)'),barmode = "stack")

teste <- df1 %>% filter(UF=="MG")
teste <- teste %>%
  filter(MUNICIPIO=="LAJINHA") %>%
  group_by(MUNICIPIO, Ano) %>%
  summarise(Valor=round((sum(VALOR)), digits = 5),
            .groups = 'drop')

minimo <- min(unique(teste$Ano))
maximo <- max(unique(teste$Ano))

plot1 <- ggplot(teste,
                aes(x=factor(Ano),
                    y = Valor,
                    fill = factor(Ano))) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=paste0("R$ ",Valor)),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("Ano") +
  ylab("Valor Recebido") +
  theme_light() +
  ggtitle(paste0("Valores Recebidos de ",minimo," a ", maximo, " por ",unique(teste$MUNICIPIO)))

plotly::ggplotly(plot1) %>%
  plotly::layout(showlegend = FALSE) %>%
  plotly::style(textposition = "top")

teste <- df1 %>%
  filter(Ano=="2019") %>%
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

teste <- df3 %>%
  filter(Ano==2019 &
         MEDIA_5_LP!=0&
         MEDIA_5_MT!=0&
         MEDIA_9_LP!=0&
         MEDIA_9_MT!=0) %>%
  group_by(UF) %>% summarise(MEDIA_5_LP=round(mean(MEDIA_5_LP),digits = 2),
                             MEDIA_5_MT=round(mean(MEDIA_5_MT),digits = 2),
                             MEDIA_9_LP=round(mean(MEDIA_9_LP),digits = 2),
                             MEDIA_9_MT=round(mean(MEDIA_9_MT),digits = 2),.groups = 'drop')

Max5LP <-  max(teste$MEDIA_5_LP)
UF5LP <- teste[teste$MEDIA_5_LP==max(teste$MEDIA_5_LP),1]
Max5MT <-  max(teste$MEDIA_5_MT)
UF5MT <- teste[teste$MEDIA_5_MT==max(teste$MEDIA_5_MT),1]
Max9LP <-  max(teste$MEDIA_9_LP)
UF9LP <- teste[teste$MEDIA_9_LP==max(teste$MEDIA_9_LP),1]
Max9MT <-  max(teste$MEDIA_9_MT)
UF9MT <- teste[teste$MEDIA_9_MT==max(teste$MEDIA_9_MT),1]



teste <- teste %>%
  mutate(MEDIA_5_LP=round((MEDIA_5_LP/max(MEDIA_5_LP))*100,digits = 2),
         MEDIA_5_MT=round((MEDIA_5_MT/max(MEDIA_5_MT))*100,digits = 2),
         MEDIA_9_LP=round((MEDIA_9_LP/max(MEDIA_9_LP))*100,digits = 2),
         MEDIA_9_MT=round((MEDIA_9_MT/max(MEDIA_9_MT))*100,digits = 2))


names(teste) <- c("UF",
                "LP - 5° Ano",
                "MT - 5° Ano",
                "LP - 9° Ano",
                "MT - 9° Ano")

teste %>%
  gather(key = "Provas", value = "Notas", -Municipio) %>%
  filter(Notas!=0)

teste <- df2 %>% group_by(Ano) %>%
  summarise(NOTA_CN=mean(NOTA_CN),
            NOTA_CH=mean(NOTA_CH),
            NOTA_LC=mean(NOTA_LC),
            NOTA_MT=mean(NOTA_MT),
            RED    =mean(RED    ))


teste <- df1 %>%
  filter(UF=="MG" & MUNICIPIO=="LAJINHA") %>%
  group_by(MUNICIPIO, Ano) %>%
  summarise(Valor=round((sum(VALOR)), digits = 5),
            .groups = 'drop') %>%
  as.data.frame()

minimo <- (min(unique(teste$Ano)))
maximo <- (max(unique(teste$Ano)))

plot1 <- ggplot(teste,
                aes(x=Ano,
                    y = Valor,
                    fill = factor(Ano),
                    group = 1)) +
  geom_line(show.legend = FALSE)+
  geom_point(size = 4,
             shape = 21,
             show.legend = FALSE) + # Also use a point with a color fill +
  geom_text(aes(label=sprintf("R$ %.2f", Valor)),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  xlab("Ano") +
  ylab("Valor Recebido") +
  theme_light() +
  ggtitle(paste0("Valores Recebidos de ",minimo," a ", maximo, " por ",unique(teste$MUNICIPIO)))

teste <- df1 %>%
  filter(UF=="MG",
         MUNICIPIO=="FLORESTAL",
         LINGUAGEM_CIDADA=="FUNDEB")

teste %>%
  ggplot(aes(Ano, VALOR))+
  geom_line(aes(y=VALOR), color = "blue", size = 1.5)+
  discrete_scale(c(2018, 2019, 2020))+
  geom_point(color = "black", size = 2.5)+
  theme_economist()
