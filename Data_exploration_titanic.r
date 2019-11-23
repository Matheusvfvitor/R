setwd("C:/Users/mathe/OneDrive/Documentos/R/Jack_Project")

library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(wordcloud)
library(RColorBrewer)

data_train <- read.csv("train.csv")

head(data_train)
str(data_train)
summary(data_train)

data_train  %>% filter(Embarked == "")

data_train[data_train$PassengerId == 62 ,12] = "S"
data_train[data_train$PassengerId == 830 ,12] = "S"

summary(data_train$Embarked)

data_train <- data_train %>% separate(Name, c("name", "surname") ,sep = "\\.")  %>% separate(name, c("name", "Title"), sep = "\\,")

data_train <- data_train[,c(1,2,3,5,6,4,7,8,9,10,11,12,13,14)]

data_train

chart = ggplot(data_train, aes(Pclass, fill = factor(Title)))+
geom_bar()+
theme_classic()

chart

data_train  %>% group_by(Title)  %>% summarise(Total = n(), MedIdad = mean(Age, na.rm = TRUE), FirstClass = sum(Pclass==1),SecondClass = sum(Pclass ==2), ThirdClass = sum(Pclass==3))

data_train  %>% group_by(Title) %>%  summarise(Max = max(Age, na.rm = TRUE), Min = min(Age, na.rm = TRUE))

data_train = data_train  %>% mutate(FxEtaria = case_when(Age<=10~"Criança", Age>10 & Age<=18 ~"Adolescente", Age>18 & Age<=45 ~"Adulto", Age>45~"Idoso"))

data_train  %>% group_by(Title, FxEtaria, Pclass) %>%  summarise(Med = mean(Age), Tot = n(),Max = max(Age, na.rm = TRUE), Min = min(Age, na.rm = TRUE), sd = sd(Age, na.rm = TRUE))

data_train  %>% group_by(Title) %>% summarise(sd= sd(Age, na.rm = TRUE), Med = mean(Age, na.rm = TRUE))

chart = ggplot(data_train[order(data_train$Title),], aes(Fare, Age, col = factor(Title)))+
geom_point(alpha=0.5)+
theme_classic()

chart

chart = ggplot(data_train, aes(Title,Age, fill=Title))+
geom_boxplot()+
theme_classic()+
guides(fill = FALSE)

chart

hist = ggplot(data_train,aes(Age))+
geom_histogram(fill="#69b3a2")+
theme_classic()

hist

hist = ggplot(data_train,aes(Age, fill = factor(Survived)))+
geom_histogram()+
theme_classic()+
facet_wrap(.~Pclass)

hist

data_train[is.na(data_train$Age),]  %>% group_by(Title)  %>% summarise(Total = n(), FirstClass = sum(Pclass==1),SecondClass = sum(Pclass ==2), ThirdClass = sum(Pclass==3))

data_train[is.na(data_train$Age) & data_train$Title == " Miss" & (data_train$Pclass == 1 | data_train$Pclass == 2) ,]

data_train[is.na(data_train$Age) & data_train$Title == " Miss" & data_train$Pclass == 3 & data_train$Parch == 0 & data_train$SibSp == 0,8] = 25 

data_train[is.na(data_train$Age) & data_train$Title == " Miss" & (data_train$Pclass == 3) ,]

data_train[is.na(data_train$Age) & data_train$Title == " Miss" & (data_train$SibSp >1) ,8] = round(rnorm(1,4.613636),1)

data_train[is.na(data_train$Age) & data_train$Title == " Miss" & (data_train$Pclass == 3) ,]

data_train[is.na(data_train$Age) & data_train$Title == " Miss" & (data_train$Parch == 0),8] =  33
Agora vamos verificar quem são essas pessoas 😉 .. #MomentoFofoca

<iframe src="https://giphy.com/embed/3ohjUP1gQOdakW6r6M" width="480" height="202" frameBorder="0" class="giphy-embed" allowFullScreen></iframe><p><a href="https://giphy.com/gifs/luansantana-santana-luan-3ohjUP1gQOdakW6r6M">via GIPHY</a></p>
data_train[is.na(data_train$Age) & data_train$Title == " Miss" & (data_train$Pclass == 3) ,]

data_train[data_train$name == "Peter",]
Como foi possível encontrar os dados de Anna no google, vamos alterar a sua idade.
data_train[is.na(data_train$Age) & data_train$name == "Peter",8] = 2

data_train[data_train$name == "Bourke",]

data_train[is.na(data_train$Age) & data_train$name == "Bourke",8] = 41

data_train[data_train$name == "Johnston",]

data_train[is.na(data_train$Age) & data_train$name == "Johnston",8] = 7

data_train[is.na(data_train$Age),]  %>% group_by(Title)  %>% summarise(Total = n(), FirstClass = sum(Pclass==1),SecondClass = sum(Pclass ==2), ThirdClass = sum(Pclass==3))

data_train[is.na(data_train$Age) & data_train$Title == " Dr",8] <- 33
data_train[is.na(data_train$Age) & data_train$Title == " Master",8] <- 5

data_train[is.na(data_train$Age),]  %>% group_by(Title)  %>% summarise(Total = n(), FirstClass = sum(Pclass==1),SecondClass = sum(Pclass ==2), ThirdClass = sum(Pclass==3))

data_train[is.na(data_train$Age) & data_train$Title == " Mr" & data_train$Pclass == 1,8] <- round(rnorm(1,35.669725,11.627757),0)
data_train[is.na(data_train$Age) & data_train$Title == " Mr" & data_train$Pclass == 2,8] <- round(rnorm(1,30,11.627757),0)
data_train[is.na(data_train$Age) & data_train$Title == " Mr" & data_train$Pclass == 3,8] <- round(rnorm(1,30,11.627757),0)
data_train[is.na(data_train$Age) & data_train$Title == " Mrs" & data_train$Pclass == 1,8] <- round(rnorm(1,35.669725,11.627757),0)
data_train[is.na(data_train$Age) & data_train$Title == " Mrs" & data_train$Pclass == 2,8] <- round(rnorm(1,35.669725,11.627757),0)
data_train[is.na(data_train$Age) & data_train$Title == " Mrs" & data_train$Pclass == 3,8] <- round(rnorm(1,35.669725,11.627757),0)

data_train  %>% group_by(Title) %>% summary(sd= sd(Age, na.rm = TRUE), Med = mean(Age, na.rm = TRUE))

data_train  %>% group_by(Title) %>% summarise(sd= sd(Age, na.rm = TRUE), Med = mean(Age, na.rm = TRUE))

data_train[is.na(data_train$Age),]
Agora chegou a hora de fazermos a análise dos dados de sobrevivência !!!
sobreviventes <- data_train  %>% group_by(Survived)  %>% summarise(Total = n())

sobreviventes
ggp = plot_ly(sobreviventes, labels = ~Survived, values = ~Total, type = "pie")

ggp

mychart <- ggplot(data_train, aes(Pclass, fill = factor(Survived)))+
theme_classic()+
geom_bar()

mychart
Nome das famílias com maior número de sobreviventes.
wordcloud(data_train$name, min.freq = 2, max.words = 80, color = brewer.pal(7, "Paired"))

mychart <- ggplot(data_train, aes(x = Pclass, fill = factor(Survived)))+
geom_bar()+
theme_classic()+
facet_wrap(.~Sex)

ggp <- ggplotly(mychart)
htmlwidgets::saveWidget(as_widget(ggp), "index.html")

ggp

mychart <- ggplot(data_train, aes(x = Age, y=Fare, col= factor(Sex)))+
geom_point(alpha = 0.5)+
theme_classic()+
facet_wrap(.~Survived)

ggp <- ggplotly(mychart)
htmlwidgets::saveWidget(as_widget(ggp), "index.html")

ggp

data = data_train  %>% group_by(Embarked)  %>% summarise(Total = n())
data
ggp = plot_ly(data, labels=~ Embarked, values= ~Total, type = "pie")
htmlwidgets::saveWidget(as_widget(ggp), "index.html")
ggp


mychart <- ggplot(data_train, aes(Embarked, fill= factor(Embarked)))+
geom_bar(position = "dodge")+
theme_classic()

ggp <- ggplotly(mychart)
htmlwidgets::saveWidget(as_widget(ggp), "index.html")

ggp

mychart <- ggplot(data_train, aes(Survived, fill= factor(Embarked)))+
geom_bar()+
theme_classic()

ggp <- ggplotly(mychart)
htmlwidgets::saveWidget(as_widget(ggp), "index.html")

ggp

mychart <- ggplot(data_train, aes(x = Pclass, fill= factor(Embarked)))+
geom_bar(position = "dodge")+
theme_classic()+
facet_wrap(.~Survived)

ggp <- ggplotly(mychart)
htmlwidgets::saveWidget(as_widget(ggp), "index.html")

ggp

write.csv(data_train,file="data_train_final.csv")


