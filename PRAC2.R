


if (!require('ggplot2'))install.packages('ggplot2'); library('ggplot2')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('plotly')) install.packages('plotly'); library('plotly')
if (!require('plyr')) install.packages('plyr'); library('plyr')
if (!require('corrplot')) install.packages('corrplot'); library('corrplot')
if (!require('Matrix')) install.packages('Matrix'); library('Matrix')
if(!require('reshape2')) install.packages('reshape2');library(reshape2)

df_heart <- read.csv("heart.csv", encoding = 'UTF-8')
str(df_heart)
head(df_heart)
summary(df_heart)

heart_disease <- sum(df_heart$HeartDisease==1)/nrow(df_heart)
percentatge_disease <- heart_disease*100
percentatge_disease

normal_heart <- sum(df_heart$HeartDisease==0)/nrow(df_heart)
percentatge_normal <- normal_heart*100
percentatge_normal

fbs <- sum(df_heart$FastingBS==1)/nrow(df_heart)
fbs_percentatge <- fbs*100
fbs_percentatge

no_fbs <- sum(df_heart$FastingBS==0)/nrow(df_heart)
no_fbs_percentatge <- no_fbs*100
no_fbs_percentatge

hd <- melt(df_heart[,-c(12)])
ggplot(hd, aes(x=value))+
  facet_wrap(~variable,scales= "free_x")+
  geom_histogram()

plot_data <- function(data_in){
  # Creem una llista de gràfics 
  plots <- list()
  
  # Obtenim un vector amb els noms de les variables
  names <- colnames(data_in[1:12])
  
  # Recorrem el vector de les variables i guardem la representació 
  # en la llista de gràfics 
  for (i in 1:length(names)){
    
    fig <- plot_ly(x = data_in[,i],
                   type = "histogram",
                   histnorm = "count",
                   name = names[i])
    
    plots[[i]] <- fig
  }
  
  # Representem tots els gràfics obtinguts en 4 files
  representacio <- subplot(plots, nrows = 4)
  representacio
}

plot_data(df_heart)

ggplot(hd, aes(x=value))+
  facet_wrap(~variable,scales= "free_x")+
  geom_boxplot()

hd2 <- melt(df_clean[,-c(12)])

ggplot(hd2, aes(x=value))+
  facet_wrap(~variable,scales= "free_x")+
  geom_boxplot()


df_clean <- df_heart

colSums(is.na(df_clean))
colSums(df_clean=="")
sum(duplicated(df_clean))

det_outliers <- function(variable, name){
  
  # Creem el gràfic
  figura <- plot(type = 'box')
  
  # Representem les variables
  figura <- figura %>% add_boxplot(y = variable,
                             jitter = 0.2, 
                             pointpos = -1.5, 
                             boxpoints = 'all',
                             marker = list(color = 'rgb(23,32,42)'),
                             line = list(color = 'rgb(23,32,42)'),
                             name = name)
  
  figura <- figura %>% layout(title = paste("Detecció d'outliers en la variable:", name))
  
  # Obtenim els possibles outliers:
  outliers <- boxplot.stats(variable)$out
  
  return(list(outliers=outliers, figura=figura))
}

deteccio = det_outliers(df_clean$Age,"Age")
deteccio$fig

det = det_outliers(df_clean$RestingBP, "RestingBP")
det$fig

det$outliers

df_clean <- df_clean[-which(df_clean$RestingBP %in% 0),]
det = det_outliers(df_clean$RestingBP, "RestingBP")
det$fig

det = det_outliers(df_clean$Cholesterol, "Cholesterol")
# Representem les dades amb un gràfic BoxPlot
det$fig

det$outliers

df_clean$Cholesterol[df_clean$Cholesterol==0] <- NA
# Substituim els NA per la mitjana de colesterol.
df_clean$Cholesterol[is.na(df_clean$Cholesterol)] <- mean(df_clean$Cholesterol,na.rm=T)

det = det_outliers(df_clean$Cholesterol, "Cholesterol")
# Representem les dades amb un gràfic BoxPlot
det$fig

det = det_outliers(df_clean$MaxHR,"MaxHR")
# Representem les dades amb un gràfic BoxPlot
det$fig

det = det_outliers(df_clean$Oldpeak,"Oldpeak")
# Representem les dades amb un gràfic BoxPlot
det$fig
det$outliers

#########################################
str(df_clean)
summary(df_clean)

plot_data(df_clean)

################################################
df_disc <- df_clean
df_disc["Age"] <- cut(df_disc$Age, breaks = c(-Inf, 45,65, +Inf),
                      labels = c("Adult", "Maduresa", "Tercera edat"))
summary(df_disc$Age)


df_disc["RestingBP"] <- cut(df_disc$RestingBP, breaks = c(-Inf, 120, 130, +Inf),
                            labels = c("Normal", "Alta", "Molt alta"))

summary(df_disc$RestingBP)

df_disc["Cholesterol"] <- cut(df_disc$Cholesterol,breaks = c(-Inf, 170, 200, +Inf),
                              labels = c("Normal", "Alt", "Molt alt"))
summary(df_disc$Cholesterol)

df_disc["FastingBS"] <- cut(df_disc$FastingBS, breaks = c(-Inf, 0.9, +Inf),
                            labels = c("Normal", "Alt"))
summary(df_disc$FastingBS)

df_disc["MaxHR"] <- cut(df_disc$MaxHR, breaks = c(-Inf, 100, 170, +Inf),
                        labels = c("Normal", "Alta", "Molt alta"))
summary(df_disc$MaxHR)

df_disc["Oldpeak"] <- cut(df_disc$Oldpeak, breaks = c(-Inf, 1.5, 2.5, +Inf),
                          labels = c("Normal", "Alt", "Molt alt"))
summary(df_disc$Oldpeak)


#############################################################
df_corr <- df_clean

df_corr$Sex[df_corr$Sex=='M']<-0
df_corr$Sex[df_corr$Sex=='F']<-1
df_corr$Sex <- as.numeric(as.character(df_corr$Sex))

df_corr$RestingECG[df_corr$RestingECG=='Normal']<-0
df_corr$RestingECG[df_corr$RestingECG=='ST']<-1
df_corr$RestingECG[df_corr$RestingECG=='LVH']<-2
df_corr$RestingECG <- as.numeric(as.character(df_corr$RestingECG))

df_corr$ExerciseAngina[df_corr$ExerciseAngina=='Y']<-1
df_corr$ExerciseAngina[df_corr$ExerciseAngina=='N']<-0
df_corr$ExerciseAngina <- as.numeric(as.character(df_corr$ExerciseAngina))

df_corr$ST_Slope[df_corr$ST_Slope=='Down']<-0
df_corr$ST_Slope[df_corr$ST_Slope=='Flat']<-1
df_corr$ST_Slope[df_corr$ST_Slope=='Up']<-2
df_corr$ST_Slope <- as.numeric(as.character(df_corr$ST_Slope))

df_corr$ChestPainType[df_corr$ChestPainType=='ASY']<-0
df_corr$ChestPainType[df_corr$ChestPainType=='ATA']<-1
df_corr$ChestPainType[df_corr$ChestPainType=='NAP']<-2
df_corr$ChestPainType[df_corr$ChestPainType=='TA']<-3
df_corr$ChestPainType <- as.numeric(as.character(df_corr$ChestPainType))

correlacio <- round(cor(df_corr),1)

corrplot(cor(df_corr[1:12]), type="lower", method = "number")



analisis_outliers <- function(variable, name){
  
  # Creem el gràfic
  fig <- plot_ly(type = 'box')
  
  # Representem la variable
  fig <- fig %>% add_boxplot(y = variable,
                             jitter = 0.3, 
                             pointpos = -1.8, 
                             boxpoints = 'all',
                             marker = list(color = 'rgb(7,40,89)'),
                             line = list(color = 'rgb(7,40,89)'),
                             name = name)
  
  fig <- fig %>% layout(title = paste("Anàlisi d'Outliers de la variable", name))
  
  # Obtenim els possibles outliers:
  outliers <- boxplot.stats(variable)$out
  
  return(list(outliers=outliers, fig=fig))
}
analisis = analisis_outliers(df_clean$Age,"Age")
analisis$fig


##########################################################


analisis_expl <- function(varToCompare){
  # Analitzem la freqüència d'aparició de la variable objectiu (HeartDisease) 
  # i la variable a comparar (VarToCompare). 
  frec <-count(df_disc, c(varToCompare, "HeartDisease"))
  
  # Obtenim els valors que per la varToCompare són 0
  NotHasHeartDisease <-subset(frec, HeartDisease==0)
  
  # Obtenim els valors de la variable a comparar
  variable <- NotHasHeartDisease[NotHasHeartDisease$varToCompare,]
  variable <- NotHasHeartDisease[,1]
  
  # Ens quedem només amb les variables:
  NotHasHeartDisease <- NotHasHeartDisease[,3]
  
  # Obtenim els valors que per la varToCompare són 1
  HasHeartDisease <- subset(frec, HeartDisease==1)
  HasHeartDisease <- HasHeartDisease[,3]
  
  # Creem el Dataframe amb l'estructura de les dades
  data <- data.frame(variable, NotHasHeartDisease, HasHeartDisease)
  
  # Representem les dades en un gràfic de barres
  fig <- plot_ly(data, x = variable, y=NotHasHeartDisease, type = 'bar', name = 'NotHasHeartDisease')
  fig <- fig %>% add_trace(y = HasHeartDisease, name = 'HasHeartDisease')
  fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
  fig
}

analisis_expl("Age")
analisis_expl("Sex")
analisis_expl("ChestPainType")
analisis_expl("FastingBS")
analisis_expl("RestingECG")
analisis_expl("ExerciseAngina")
analisis_expl("ST_Slope")

################################################

ggplot(data=df_corr, aes(x=Age, fill=as.factor(HeartDisease)))+geom_bar(position = "fill")+
  ggtitle("Relació entre les variables Age i HeartDisease")+
  xlab("Grup d'edats")+
  ylab("Proporció de persones amb HeartDisease segons Age")


analisis_expl <- function(var_comp){
  
  frec <-count(df_clean, c(var_comp, "HeartDisease"))
  
  NoHD <-subset(frec, HeartDisease==0)
  
  variable <- NoHD[NoHD$var_comp,]
  variable <- NoHD[,1]
  
  NoHD <- NoHD[,3]
  
  YesHD <- subset(frec, HeartDisease==1)
  YesHD <- YesHD[,3]
  
  data <- data.frame(variable, NoHD, YesHD)
  
  fig <- plot_ly(data, x = variable, y=NoHD, type = 'bar', name = 'NoHeartDisease')
  fig <- fig %>% add_trace(y = YesHD, name = 'HeartDisease')
  fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
  fig
}


