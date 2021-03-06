#========================================#
# VIOLENCIA E OPINIAO PUBLICA            #    
#========================================#
# Recife - Pernambuco - Brasil           #
# Janeiro 2018                           #    
#----------------------------------------#
# Claudio A. Monteiro                    #
# Gabriella Fidelis                      #
# Leticia Machado                        #
#----------------------------------------#
# Any question contact the developers    #
# #UseFreeSoftware                       #
#----------------------------------------#

# instalar pacotes necessarios
# install.packages(c("readl", "stringr", "dplyr", "stargazer", "MASS","ordinal","erer", "ggplot2", ""))

# definir diretorio de trabalho
setwd("~/Consulting/Gabi")

# carregar pacote
library(readxl); library(stringr); library(dplyr); library(stargazer); library(MASS);
library(ordinal); library(erer); library(ggplot2); library(rcompanion)

# ler bancos de dados
LapopBrazil_2006 <- read_excel("Dados/LapopBrazil_2006.xlsx")
LapopBrazil_2008 <- read_excel("Dados/LapopBrazil_2008.xlsx")
LapopBrazil_2010 <- read_excel("Dados/LapopBrazil_2010.xlsx")
LapopBrazil_2012 <- read_excel("Dados/LapopBrazil_2012.xlsx")
LapopBrazil_2014 <- read_excel("Dados/LapopBrazil_2014.xlsx")
LapopBrazil_2017 <- read_excel("Dados/LapopBrazil_2017.xlsx")

# selecionar variaveis de interesse na base #

# Renomear colunas
colnames(LapopBrazil_2006) <- c("Urbaniza��o",  "G�nero" ,  "saliencia_problema", 
                                "Avalia��o_GovFederal", "Voto", "Idade",         
                                "Escolaridade", "Renda_Individual", "Renda_Familiar",  "Ra�a", "sofreu_violencia")

colnames(LapopBrazil_2008)<-  c("Urbaniza��o",  "G�nero" ,  "saliencia_problema", 
                                "Avalia��o_GovFederal", "Voto", "Escolaridade",         
                                "Idade", "Renda_Familiar",  "Ra�a", "sofreu_violencia")

colnames(LapopBrazil_2010) <-  c("Urbaniza��o",  "G�nero" ,  "saliencia_problema", 
                                 "sofreu_violencia","Avalia��o_GovFederal", "Voto", "Escolaridade",         
                                 "Idade", "Renda_Familiar",  "Ra�a")

colnames(LapopBrazil_2012) <- c("Urbaniza��o",  "G�nero" ,  "saliencia_problema", 
                                "sofreu_violencia","Avalia��o_GovFederal", "Voto", "Escolaridade",         
                                "Renda_Familiar",  "Ra�a", "Idade")

colnames(LapopBrazil_2014) <- c("Urbaniza��o",  "G�nero" , "Idade", "saliencia_problema", 
                                "sofreu_violencia","Avalia��o_GovFederal", "Voto", "Escolaridade",         
                                "Renda_Familiar",  "Ra�a")

colnames(LapopBrazil_2017) <- c("Urbaniza��o" , "Idade","G�nero", "sofreu_violencia" ,"Avalia��o_GovFederal",
                                "Voto","Escolaridade","Ra�a","Renda_Familiar", "saliencia_problema")


# criar coluna de ano
LapopBrazil_2006$Ano <- 2006
LapopBrazil_2008$Ano <- 2008
LapopBrazil_2010$Ano <- 2010
LapopBrazil_2012$Ano <- 2012
LapopBrazil_2014$Ano <- 2014
LapopBrazil_2017$Ano <- 2017

# Criar variavel de voto
LapopBrazil_2006 <- mutate(LapopBrazil_2006, Voto_Mandat�rio = ifelse(Voto == "1.0" , 1, 0))
LapopBrazil_2008 <- mutate(LapopBrazil_2008, Voto_Mandat�rio = ifelse(Voto == 1501 , 1, 0))
LapopBrazil_2010 <- mutate(LapopBrazil_2010, Voto_Mandat�rio = ifelse(Voto == 1501 , 1, 0))
LapopBrazil_2012 <- mutate(LapopBrazil_2012, Voto_Mandat�rio = ifelse(Voto == 1501 , 1, 0))
LapopBrazil_2014 <- mutate(LapopBrazil_2014, Voto_Mandat�rio = ifelse(Voto == 1501 , 1, 0))
LapopBrazil_2017 <- mutate(LapopBrazil_2017, Voto_Mandat�rio = ifelse(Voto == 1501 , 1, 0))

# Recodificar idade
LapopBrazil_2008$Idade <- str_replace(LapopBrazil_2008$Idade, "0", "NA")
LapopBrazil_2010$Idade <- str_replace(LapopBrazil_2010$Idade, "988", "NA")

LapopBrazil_2012$Idade <- str_replace(LapopBrazil_2012$Idade, "999999", "NA")
LapopBrazil_2012$Idade <- as.numeric(LapopBrazil_2012$Idade)
LapopBrazil_2012$Idade <- -(LapopBrazil_2012$Idade - 2012)

#--------------------#
# Recodificar Raca

#2006
LapopBrazil_2006$Ra�a <- str_replace(LapopBrazil_2006$Ra�a, "1", "4")
LapopBrazil_2006$Ra�a <- str_replace(LapopBrazil_2006$Ra�a, "3", "1")
LapopBrazil_2006$Ra�a <- str_replace(LapopBrazil_2006$Ra�a, "4", "5")
LapopBrazil_2006$Ra�a <- str_replace(LapopBrazil_2006$Ra�a, "5", "3")
LapopBrazil_2006$Ra�a <- str_replace(LapopBrazil_2006$Ra�a, "7", "6")
LapopBrazil_2006$Ra�a <-factor(LapopBrazil_2006$Ra�a, levels = c("1","2", "3", "4", "5", "6"), 
                               labels = c("Branco", "Pardo", "Indio", "Preto" ,"Amarelo", "Outra"))

LapopBrazil_2006$Ra�a <- str_replace(LapopBrazil_2006$Ra�a, "88", "NA") 
LapopBrazil_2006$Ra�a <- str_replace(LapopBrazil_2006$Ra�a, "99", "NA")

#2008
LapopBrazil_2008$Ra�a <- str_replace(LapopBrazil_2008$Ra�a, "7", "6")
LapopBrazil_2008$Ra�a <-factor(LapopBrazil_2008$Ra�a, levels = c("1","2","3","4","5","6"), 
                               labels = c("Branco", "Pardo", "Indio", "Preto" ,"Amarelo", "Outra"))

LapopBrazil_2006$Ra�a <- str_replace(LapopBrazil_2006$Ra�a, "8", "NA") 

#2010
LapopBrazil_2010$Ra�a <- str_replace(LapopBrazil_2010$Ra�a, "7", "6")
LapopBrazil_2010$Ra�a <-factor(LapopBrazil_2010$Ra�a, levels = c("1","2","3","4","5","6"), 
                               labels = c("Branco", "Pardo", "Indio", "Preto" ,"Amarelo", "Outra"))

LapopBrazil_2010$Ra�a <- str_replace(LapopBrazil_2010$Ra�a, "88", "NA") 
LapopBrazil_2010$Ra�a <- str_replace(LapopBrazil_2010$Ra�a, "98", "NA")

#2012
LapopBrazil_2012$Ra�a <- str_replace(LapopBrazil_2012$Ra�a, "5", "2")
LapopBrazil_2012$Ra�a <- str_replace(LapopBrazil_2012$Ra�a, "6", "5")
LapopBrazil_2012$Ra�a <- str_replace(LapopBrazil_2012$Ra�a, "7", "6")
LapopBrazil_2012$Ra�a <-factor(LapopBrazil_2012$Ra�a, levels = c("1","2","3","4","5","6"), 
                               labels = c("Branco", "Pardo", "Indio", "Preto" ,"Amarelo", "Outra"))

LapopBrazil_2012$Ra�a <- str_replace(LapopBrazil_2012$Ra�a, "88", "NA") 
LapopBrazil_2012$Ra�a <- str_replace(LapopBrazil_2012$Ra�a, "98", "NA")


#2014
LapopBrazil_2014$Ra�a <- str_replace(LapopBrazil_2014$Ra�a, "5", "2")
LapopBrazil_2014$Ra�a <- str_replace(LapopBrazil_2014$Ra�a, "6", "5")
LapopBrazil_2014$Ra�a <- str_replace(LapopBrazil_2014$Ra�a, "7", "6")
LapopBrazil_2014$Ra�a <-factor(LapopBrazil_2014$Ra�a, levels = c("1","2","3","4","5","6"), 
                               labels = c("Branco", "Pardo", "Indio", "Preto" ,"Amarelo", "Outra"))

LapopBrazil_2012$Ra�a <- str_replace(LapopBrazil_2012$Ra�a, "88", "NA") 
LapopBrazil_2012$Ra�a <- str_replace(LapopBrazil_2012$Ra�a, "98", "NA")


#2016/17
LapopBrazil_2017$Ra�a <- str_replace(LapopBrazil_2017$Ra�a, "5", "2")
LapopBrazil_2017$Ra�a <- str_replace(LapopBrazil_2017$Ra�a, "1506", "5")
LapopBrazil_2017$Ra�a <- str_replace(LapopBrazil_2017$Ra�a, "7", "6")
LapopBrazil_2017$Ra�a <-factor(LapopBrazil_2017$Ra�a, levels = c("1","2","3","4","5","6"), 
                               labels = c("Branco", "Pardo", "Indio", "Preto" ,"Amarelo", "Outra"))

LapopBrazil_2017$Ra�a <- str_replace(LapopBrazil_2017$Ra�a, "888888", "NA") 
LapopBrazil_2017$Ra�a <- str_replace(LapopBrazil_2017$Ra�a, "988888", "NA")


# Remover variavel de banco 1
LapopBrazil_2006 <- LapopBrazil_2006[,-8]

# mergir bancos
data_modelo1 <- rbind(LapopBrazil_2006,LapopBrazil_2008, LapopBrazil_2010,
                      LapopBrazil_2012, LapopBrazil_2014, LapopBrazil_2017)

data_modelo2 <- LapopBrazil_2006

#### Fun��o de Manipul��o do Banco ####


funcao.mani <- function(data){
  
  # Voto no Mandatario
  data$Voto_Mandat�rio <- factor(data$Voto_Mandat�rio, levels = c("0", "1"), labels = c("N�o_Votou", "Votou"))

  # Recodificar Saliencia da Violencia
  data <- mutate(data, Sali�ncia_Viol�ncia = ifelse(saliencia_problema == 57 , 1, 0)) 
  data$Sali�ncia_Viol�ncia <- factor(data$Sali�ncia_Viol�ncia, levels = c("0", "1"), labels = c("Outros", "Viol�ncia"))
  
  # Recodificar Vitima da Violencia
  data <- mutate(data, V�tima_Viol�ncia = ifelse(sofreu_violencia == 1 , 1, 0)) 
  data$V�tima_Viol�ncia <- factor(data$V�tima_Viol�ncia, levels = c("0", "1"), labels = c("N�o_V�tima", "V�tima"))
  
  # Recodificar Avaliacao do Gov. Fed.
  data$Avalia��o_GovFederal <- str_replace(data$Avalia��o_GovFederal, "8", "") # Recodificar NAs 
  data$Avalia��o_GovFederal <- str_replace(data$Avalia��o_GovFederal, "9", "")
  
  data <- mutate(data, Avalia��o_GovernoFederal = "")
  data$Avalia��o_GovernoFederal[data$Avalia��o_GovFederal == 1] <- 5
  data$Avalia��o_GovernoFederal[data$Avalia��o_GovFederal == 2] <- 4
  data$Avalia��o_GovernoFederal[data$Avalia��o_GovFederal == 3] <- 3
  data$Avalia��o_GovernoFederal[data$Avalia��o_GovFederal == 4] <- 2
  data$Avalia��o_GovernoFederal[data$Avalia��o_GovFederal == 5] <- 1
  data$Avalia��o_GovernoFederal <- factor(data$Avalia��o_GovernoFederal, 
                                          levels=c("1", "2", "3", "4", "5"), 
                                          labels = c("P�ssimo", "Ruim","Regular", "Bom", "Muito Bom"),
                                          ordered = TRUE)
  
  data$Avalia��o_GovFederal <- factor(data$Avalia��o_GovFederal, 
                                          levels=c("1", "2", "3", "4", "5"), 
                                          labels = c("Muito Bom", "Bom","Regular", "Ruim", "P�ssimo"),
                                          ordered = TRUE)
  
  # Recodificar Urbanizazao
  data <- mutate(data, Urbaniza��o = ifelse(Urbaniza��o == 1, 1, 0))
  data$Urbaniza��o <- factor(data$Urbaniza��o, levels = c("0", "1"), labels = c("Rural", "Urbano"))

  # Recodificar Genero
  data$G�nero <- str_replace(data$G�nero, "2", "0")
  data$G�nero <- factor(data$G�nero, levels = c("0", "1"), labels = c("Mulher","Homem"))

  # Recodificar Renda 
  data$Renda_Familiar <- str_replace(data$Renda_Familiar, "88", "NA") 
  data$Renda_Familiar <- str_replace(data$Renda_Familiar, "98", "NA") 
  data$Renda_Familiar <- as.numeric(data$Renda_Familiar)

# Recodificar Escolaridade
data$Escolaridade <- str_replace(data$Escolaridade, "88", "NA") 
data$Escolaridade <- str_replace(data$Escolaridade, "98", "NA") 
data$Escolaridade <- str_replace(data$Escolaridade, "888888", "NA") 
data$Escolaridade <- str_replace(data$Escolaridade, "988888", "NA") 
data$Escolaridade <- as.numeric(data$Escolaridade)

# as numeric idade
data$Idade <- as.numeric(data$Idade)

return(data)
}


dados_modelo1 <- funcao.mani(data_modelo1)
dados_modelo2 <- funcao.mani(data_modelo2)

dados_modelo2 <- dados_modelo2[complete.cases(dados_modelo2),]
dados_modelo1 <- dados_modelo1[complete.cases(dados_modelo1),]

#================================#
# MODELOS DE REGRESSAO LOGISTICA #
#================================#

#---- odds ratio plot 1 ----#
plot_odds<-function(x, title = NULL){
  tmp<-data.frame(cbind(exp(coef(x)), exp(confint(x))))
  odds<-tmp[-1,]
  names(odds)<-c('OR', 'lower', 'upper')
  odds$vars<-row.names(odds)
  ticks<-c(1)
  
  ggplot(odds, aes(y= OR, x = reorder(vars, OR))) +
    geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
    scale_y_log10(breaks=ticks, labels = ticks) +
    geom_hline(yintercept = 1, linetype=2) +
    coord_flip() +
    labs(title = title, x = 'Variables', y = 'OR') +
    theme_bw()
}

#---------------------------------#
# MODELO 1 - Reg. Log. Ordinal
#---------------------------------#

dados_modelo1.5000 <- sample_n(dados_modelo1, 5000)
dados_modelo1.1000 <- sample_n(dados_modelo1, 1000)

# executar modelo
modelo1 <- clm(Avalia��o_GovernoFederal ~ 
                 V�tima_Viol�ncia +
                 Sali�ncia_Viol�ncia +
                 G�nero +
                 Idade +
                 Escolaridade +
                 Renda_Familiar +
                 Ra�a+
                 Urbaniza��o, data = dados_modelo1.5000)


# visualizar resultados do modelo
summary(modelo1)
stargazer(modelo1,  type = "text", title = "Resultados Modelo 1", style = "ajps", apply.coef = exp,  p.auto=FALSE)

# Estatisticas de ajuste 
nagelkerke(fit = modelo1)

#---------------------------------#
# MODELO 2 - Reg. Log. Binomial
#---------------------------------#
dados_modelo2 <- mutate(dados_modelo2, Ra�aIndio = ifelse(Ra�a == "Indio", 1, 0))
dados_modelo2 <- mutate(dados_modelo2, Ra�aBranco = ifelse(Ra�a == "Branco", 1, 0))
dados_modelo2 <- mutate(dados_modelo2, Ra�aPardo = ifelse(Ra�a == "Pardo", 1, 0))


dados_modelo2.500 <- sample_n(dados_modelo2, 500)

# executar modelo
modelo2 <- glm( Voto_Mandat�rio ~  
                  V�tima_Viol�ncia +
                  Sali�ncia_Viol�ncia +
                  G�nero +
                  Idade +
                  Escolaridade +
                  Ra�aBranco+
                  Ra�aPardo+
                  Urbaniza��o, 
                  data = dados_modelo2.500, family = binomial(link = "logit"))

# visualizar resultados
summary(modelo2)
stargazer(modelo2,  type = "text", title = "Resultados Modelo 2", style = "ajps",  apply.coef = exp,  p.auto=FALSE)

# Estatisticas de ajuste 
nagelkerke(fit = modelo2)

#---- plot models ----#
cp1 <- plot_odds(modelo2, "")
cp1

#=====================================
# DESCRITIVAS renda idade escolaridade

summary(dados_modelo2$Urbaniza��o)
summary(dados_modelo2$G�nero)
summary(dados_modelo2$Idade)
summary(dados_modelo2$Escolaridade)
summary(dados_modelo2$Renda_Familiar)
summary(as.factor(dados_modelo2$Ra�a))
summary(as.factor(dados_modelo2$Voto_Mandat�rio))
summary(as.factor(dados_modelo2$Sali�ncia_Viol�ncia))
summary(as.factor(dados_modelo2$V�tima_Viol�ncia))

summary(dados_modelo1$Urbaniza��o)
summary(dados_modelo1$G�nero)
summary(dados_modelo1$Avalia��o_GovFederal)
summary(dados_modelo1$Idade)
summary(dados_modelo1$Escolaridade)
summary(dados_modelo1$Renda_Familiar)
summary(as.factor(dados_modelo1$Ra�a))
summary(as.factor(dados_modelo1$Sali�ncia_Viol�ncia))
summary(as.factor(dados_modelo1$V�tima_Viol�ncia))

#==================================
# Ranking Tema

# contar respotas de tema
table_tema <- data.frame(table(dados_modelo1$saliencia_problema))

# criar variavel de prop.
table_tema$tema_prop <- table_tema$Freq / (sum(table_tema$Freq) - 56)
table_tema$tema_prop <- round(table_tema$tema_prop, 3)*100

# ordenar banco do tema.prop
table_tema <- table_tema[order(table_tema$tema_prop),]

# selecionar 15 temas mais salientes
table_tema <- table_tema[36:45,]


# nomear 
table_tema$nomes <- c("Desigualdade", "Delinqu�ncia, crime, viol�ncia", "Educa��o, falta de, m� qualidade",
                      "Outro", "Economia, problemas com, crise de", "Seguran�a (falta de)", "Corrup��o", 
                      "Desemprego/falta de emprego","Viol�ncia" ,"Sa�de, falta de servi�o")

# ordernar por nomes
table_tema$nomes <- factor(table_tema$nomes, levels = table_tema$nomes[order(table_tema$Freq)])


# plotagem
ggplot(table_tema, aes(x = table_tema$nomes, y = table_tema$tema_prop))+
  geom_bar(stat = "identity", fill = "#30011e")+
  geom_label(aes(x = table_tema$nomes, y = table_tema$tema_prop, label = table_tema$tema_prop), vjust=0)+
  labs(x = "", y = "Porcentagem do Total de Respostas V�lidas") +
  coord_flip()

#salvar
ggsave("tema_saliencia.png", width = 8, height = 4, units = "in")




















