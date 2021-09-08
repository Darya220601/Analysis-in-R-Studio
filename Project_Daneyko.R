#Задание 1
library(cluster)
#считываю данные по клиентам
setwd("D:/")
#удаляю первый столбец с ID человека, т.к c точки зрения предсказанияn он не несёт полезной информации
clients <-read.csv("credits.csv")

#статистический анализ данных для всей выборки
summary(clients)

#Коэффийиенты корреляции между попарно взятыми признаками
cor(clients)


#Разбиваем на кластеры

c1 = kmeans(clients, 2)
rownames(clients)[which(c1$cluster==1)]
rownames(clients)[which(c1$cluster==2)]
c1$size # 15 25

c2 = kmeans(clients, 3)
rownames(clients)[which(c2$cluster==1)]
rownames(clients)[which(c2$cluster==2)]
rownames(clients)[which(c2$cluster==3)]
c2$size #14 15 11

plot(agnes(clients))


#Задание 2
install.packages("DAAG") 
library(DAAG) 

install.packages("tree") 
library(tree) 

install.packages("maptree") 
library(maptree) 
setwd("D:/")
clients2 <-read.csv("кредиты.csv")
clients2$Personal.Loan = as.factor(ifelse(clients2$Personal.Loan == 1, "YES", "NO"))
str(clients2)
summary(clients2)
summary(subset(clients2, Personal.Loan =="YES"))
summary(subset(clients2, Personal.Loan =="NO"))


set.seed(1)
train.index <- sample(c(1:dim(clients2)[1]), dim(clients2)[1]*0.3)
train.df <- clients2[train.index, ]
valid.df <- clients2[-train.index, ]

tr <- tree(Personal.Loan ~., train.df)
plot(tr, type = "uniform")
text(tr)
draw.tree(tr)

tr1 <- snip.tree(tr, nodes=2)
plot(tr1, type = "uniform")
text(tr1)
draw.tree(tr1)

predict(tr1, valid.df, type="class") 





