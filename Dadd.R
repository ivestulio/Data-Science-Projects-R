#################################################################################
#           ANÁLISE DE DADOS DE COVID DA CIDADE DO ESTADO DE SÃO PAULO          #
#################################################################################

#PACOTES:
if(!require(dplyr)) install.packages("dplyr") # Manipulação de dados
library(dplyr)


  # IMPORTANDO DADOS 

  # Path:
setwd("~/Desktop/PROGRAMAÇAO/Linguagem R para analise de dados")

  # data
covid_sp <- read.csv("dados_covid_sp.csv", sep =";", encoding = "UTF-8")
View(covid_sp)
head(covid_sp)

#----------------------------TRATAMENTO DE DADOS-------------------------------#



  # Renomeando Colunas: 

covid_sp <- rename(covid_sp, municipio = nome_munic)
covid_sp <- rename(covid_sp, data = datahora, rotulo_mapa = map_leg, codigo_mapa = map_leg_s)

  # Excluindo colunas 

covid_sp$cod_ra <- NULL
covid_sp <- select(covid_sp, -c(21))
covid_sp <- subset(covid_sp, select = -c(codigo_ibge, cod_drs))
covid_sp <- select(covid_sp, -c(14,15) )
covid_sp <-select(covid_sp, -c(17:19))

  # Excluindo dados de linhas :

covid_sp <-covid_sp %>% filter(municipio != "Ignorado")


  
  # Valores Missing:
    # NA = Valores Ausentes 
    # NAN = Valor indefinido 

sapply(covid_sp, function(x) sum(is.na(x)))
sapply(covid_sp, function(x) sum(is.nan(x)))

  # Tipos de dados:
str(covid_sp)
glimpse(covid_sp)

covid_sp$data <- as.Date(covid_sp$data, format = '%Y-%m-%d')
covid_sp$casos_pc <- as.numeric(gsub(",", ".",covid_sp$casos_pc))
covid_sp$casos_mm7d <- as.numeric(gsub(",", ".",covid_sp$casos_mm7d))
covid_sp$obitos_pc <- as.numeric(gsub(",", ".",covid_sp$obitos_pc))
covid_sp$obitos_mm7d <- as.numeric(gsub(",", ".",covid_sp$obitos_mm7d))
covid_sp$letalidade <- as.numeric(gsub(",", ".",covid_sp$letalidade))



  # Coluna com porcentagem de idosos na cidade:

covid_sp["idoso(%)"] <- 100*covid_sp$pop_60/covid_sp$pop

  # Exportar o Arquivo Tratado:

write.table(covid_sp, file="covidSP_tratado.csv", sep ="," )


  # Selecionar alguns dados:

covid_campinas <- covid_sp %>% filter(covid_sp$municipio == "Campinas")
View(covid_campinas)
covid_campinas['area'] <- covid_campinas$area/100
covid_guaruhos <- covid_sp %>% filter(covid_sp$municipio=="Guarulhos")

summary(covid_campinas)


plot(covid_campinas$data, covid_campinas$casos_mm7d, title= ("Média Móvel"), col="red")

plot(covid_campinas$data, covid_campinas$obitos_mm7d, title=("Média móvel"), col="purple")

hist(covid_campinas$obitos_novos, col="blue")

boxplot(covid_sp$casos_novos)

# ------------------ANÁLISE EXPLORATÓRIA-------------------------------------# 

if(!require(rstatix)) install.packages("rstatix") # Pacote de Estatística 
library(rstatix)

  # Correlação Linear:
library(corrplot)
plot(covid_campinas$casos, covid_campinas$obitos)

if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
if(!require(ggpubr)) install.packages("ggpubr")
library(ggpubr)


ggplot(data=covid_campinas, mapping = aes(x=casos, y=obitos))+
  geom_point()+
  geom_smooth(method="lm", col="red")
  # stat_regline_equation(aes(label=paste(..eq.label..,..adj.rr.label..,
  #                                       sep+"*plain(\" ,   \")~~")),label.x=15000, label.y=1800)
+theme_gray()

matriz_corr <- cor(covid_campinas[5:13], method ="spearman")

corrplot(matriz_corr, method = "color",
         type = "full", order ="original",addCoef.col = "black")


  # Gráficos Lineares por Cidades:

covid_cidades <- covid_sp %>% filter(municipio %in% c("Campinas", "Guarulhos","Sorocaba"))




ggplot(covid_cidades, aes(x=casos, y=obitos_pc, color=municipio))+
  geom_line()+
  labs(title="Evolução dos obitos",
       x="Casos",
       y="Obitos",
       color="Meses") +theme_classic()









install.packages("ggplot2")
library(ggplot2)
.rs.restartR()
ggplot(covid_sp)
