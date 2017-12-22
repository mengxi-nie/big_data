install.packages("readr")
install.packages("psych")
install.packages('NbClust')
install.packages("caret")
install.packages('corrplot')
install.packages("fpc", dependencies=T)
install.packages('MASS')
install.packages("e1071")
install.packages("party")
install.packages('car')
install.packages("class")
install.packages("descr")
install.packages("ggplot2")
install.packages("cluster")

library(cluster)
library(party)
library(e1071)
library(MASS)
library(fpc)
library(NbClust)
library(readr)
library(psych)
library(caret)
library(corrplot)
library(car)
library(class)
library(descr)

setwd('C:/Users/evash/Documents/GWU/17-Fall-GWU/Big Data/project2/')
rm(list=ls())
mr<- read.csv('./data/agaricus-lepiota.data')
View(mr)
summary(mr)
describe(mr)
str(mr)
mr.df<-as.data.frame(mr)
mr.df
names(mr)<-c("class","cshape","csurface","ccolor","bruises","odor","gattach","gspace","gsize","gcolor","sshape","sroot","ssabove","ssbelow","scabove","scbelow","vtype","vcolor","rnumber","rtype","spcolor","popnum","habitat")
summary(mr)
mr


#Draw the attribution
#corrplot(cor(data), type = "lower", method = "number")

pairs(class~sshape+sroot,data = mr)
plot(class~ sshape+sroot,data = mr)



#Choose the proper attribution
mrf<-as.data.frame(mr,stringAsFactors=FALSE)
mrf[,c(1,23)]<-sapply(mrf[,c(1,23)],as.character)
mrf$class[mrf$class=='e']<-0
mrf$class[mrf$class=='p']<-1
mrf

mrf[,c(2,23)]<-sapply(mrf[,c(2,23)],as.character)
mrf$cshape[mrf$cshape=='b']<-1
mrf$cshape[mrf$cshape=='c']<-3
mrf$cshape[mrf$cshape=='x']<-5
mrf$cshape[mrf$cshape=='f']<-7
mrf$cshape[mrf$cshape=='k']<-9
mrf$cshape[mrf$cshape=='s']<-11

mrf[,c(3,23)]<-sapply(mrf[,c(3,23)],as.character)

mrf$csurface[mrf$csurface=='f']<-1
mrf$csurface[mrf$csurface=='g']<-3
mrf$csurface[mrf$csurface=='y']<-5
mrf$csurface[mrf$csurface=='s']<-7


mrf[,c(4,23)]<-sapply(mrf[,c(4,23)],as.character)
mrf$ccolor[mrf$ccolor=='c']<-1
mrf$ccolor[mrf$ccolor=='b']<-3
mrf$ccolor[mrf$ccolor=='g']<-5
mrf$ccolor[mrf$ccolor=='n']<-5
mrf$ccolor[mrf$ccolor=='r']<-7
mrf$ccolor[mrf$ccolor=='p']<-9
mrf$ccolor[mrf$ccolor=='u']<-11
mrf$ccolor[mrf$ccolor=='e']<-13
mrf$ccolor[mrf$ccolor=='w']<-15
mrf$ccolor[mrf$ccolor=='y']<-17

mrf[,c(5,23)]<-sapply(mrf[,c(5,23)],as.character)
mrf$bruises[mrf$bruises=='t']<-1
mrf$bruises[mrf$bruises=='f']<-3
#mrf

mrf[,c(6,23)]<-sapply(mrf[,c(6,23)],as.character)
mrf$odor[mrf$odor=='a']<-1
mrf$odor[mrf$odor=='l']<-3
mrf$odor[mrf$odor=='p']<-5
mrf$odor[mrf$odor=='n']<-7
mrf$odor[mrf$odor=='c']<-9
mrf$odor[mrf$odor=='y']<-11
mrf$odor[mrf$odor=='f']<-13
mrf$odor[mrf$odor=='m']<-15
mrf$odor[mrf$odor=='s']<-17

mrf[,c(7,23)]<-sapply(mrf[,c(7,23)],as.character)
mrf$gattach[mrf$gattach=='a']<-1
mrf$gattach[mrf$gattach=='d']<-3
mrf$gattach[mrf$gattach=='f']<-5
mrf$gattach[mrf$gattach=='n']<-7

mrf[,c(8,23)]<-sapply(mrf[,c(8,23)],as.character)
mrf$gspace[mrf$gspace=='c']<-1
mrf$gspace[mrf$gspace=='w']<-3
mrf$gspace[mrf$gspace=='d']<-5

mrf[,c(9,23)]<-sapply(mrf[,c(9,23)],as.character)
mrf$gsize[mrf$gsize=='b']<-1
mrf$gsize[mrf$gsize=='n']<-3

mrf[,c(10,23)]<-sapply(mrf[,c(10,23)],as.character)
mrf$gcolor[mrf$gcolor=='k']<-1
mrf$gcolor[mrf$gcolor=='n']<-3
mrf$gcolor[mrf$gcolor=='b']<-5
mrf$gcolor[mrf$gcolor=='h']<-7
mrf$gcolor[mrf$gcolor=='g']<-9
mrf$gcolor[mrf$gcolor=='r']<-11
mrf$gcolor[mrf$gcolor=='o']<-13
mrf$gcolor[mrf$gcolor=='p']<-15
mrf$gcolor[mrf$gcolor=='u']<-17
mrf$gcolor[mrf$gcolor=='e']<-19
mrf$gcolor[mrf$gcolor=='w']<-21
mrf$gcolor[mrf$gcolor=='y']<-23

mrf[,c(11,23)]<-sapply(mrf[,c(11,23)],as.character)
mrf$sshape[mrf$sshape=='e']<-1
mrf$sshape[mrf$sshape=='t']<-3

mrf[,c(12,23)]<-sapply(mrf[,c(12,23)],as.character)
mrf$sroot[mrf$sroot=='b']<-1
mrf$sroot[mrf$sroot=='c']<-3
mrf$sroot[mrf$sroot=='u']<-5
mrf$sroot[mrf$sroot=='e']<-7
mrf$sroot[mrf$sroot=='z']<-9
mrf$sroot[mrf$sroot=='r']<-11
mrf$sroot[mrf$sroot=='?']<-13
mrf
mrf[,c(13,23)]<-sapply(mrf[,c(13,23)],as.character)
mrf$ssabove[mrf$ssabove=='f']<-1
mrf$ssabove[mrf$ssabove=='y']<-3
mrf$ssabove[mrf$ssabove=='k']<-5
mrf$ssabove[mrf$ssabove=='s']<-7

mrf[,c(14,23)]<-sapply(mrf[,c(14,23)],as.character)
mrf$ssbelow[mrf$ssbelow=='f']<-1
mrf$ssbelow[mrf$ssbelow=='y']<-3
mrf$ssbelow[mrf$ssbelow=='k']<-5
mrf$ssbelow[mrf$ssbelow=='s']<-7

mrf[,c(15,23)]<-sapply(mrf[,c(15,23)],as.character)
mrf$scabove[mrf$scabove=='n']<-1
mrf$scabove[mrf$scabove=='b']<-3
mrf$scabove[mrf$scabove=='c']<-5
mrf$scabove[mrf$scabove=='g']<-7
mrf$scabove[mrf$scabove=='o']<-9
mrf$scabove[mrf$scabove=='p']<-11
mrf$scabove[mrf$scabove=='e']<-13
mrf$scabove[mrf$scabove=='w']<-15
mrf$scabove[mrf$scabove=='y']<-17

mrf[,c(16,23)]<-sapply(mrf[,c(16,23)],as.character)
mrf$scbelow[mrf$scbelow=='n']<-1
mrf$scbelow[mrf$scbelow=='b']<-3
mrf$scbelow[mrf$scbelow=='c']<-5
mrf$scbelow[mrf$scbelow=='g']<-7
mrf$scbelow[mrf$scbelow=='o']<-9
mrf$scbelow[mrf$scbelow=='p']<-11
mrf$scbelow[mrf$scbelow=='e']<-13
mrf$scbelow[mrf$scbelow=='w']<-15
mrf$scbelow[mrf$scbelow=='y']<-17

mrf[,c(17,23)]<-sapply(mrf[,c(17,23)],as.character)
mrf$vtype[mrf$vtype=='p']<-1
mrf$vtype[mrf$vtype=='u']<-3

mrf[,c(18,23)]<-sapply(mrf[,c(18,23)],as.character)
mrf$vcolor[mrf$vcolor=='n']<-1
mrf$vcolor[mrf$vcolor=='o']<-3
mrf$vcolor[mrf$vcolor=='w']<-5
mrf$vcolor[mrf$vcolor=='y']<-7

mrf[,c(19,23)]<-sapply(mrf[,c(19,23)],as.character)
mrf$rnumber[mrf$rnumber=='n']<-1
mrf$rnumber[mrf$rnumber=='o']<-3
mrf$rnumber[mrf$rnumber=='t']<-5

mrf[,c(20,23)]<-sapply(mrf[,c(20,23)],as.character)
mrf$rtype[mrf$rtype=='c']<-1
mrf$rtype[mrf$rtype=='e']<-3
mrf$rtype[mrf$rtype=='f']<-5
mrf$rtype[mrf$rtype=='l']<-7
mrf$rtype[mrf$rtype=='n']<-9
mrf$rtype[mrf$rtype=='p']<-11
mrf$rtype[mrf$rtype=='s']<-13
mrf$rtype[mrf$rtype=='z']<-15


mrf[,c(21,23)]<-sapply(mrf[,c(21,23)],as.character)
mrf$spcolor[mrf$spcolor=='k']<-1
mrf$spcolor[mrf$spcolor=='n']<-3
mrf$spcolor[mrf$spcolor=='b']<-5
mrf$spcolor[mrf$spcolor=='h']<-7
mrf$spcolor[mrf$spcolor=='r']<-9
mrf$spcolor[mrf$spcolor=='o']<-11
mrf$spcolor[mrf$spcolor=='u']<-13
mrf$spcolor[mrf$spcolor=='w']<-15
mrf$spcolor[mrf$spcolor=='y']<-17


mrf[,c(22,23)]<-sapply(mrf[,c(22,23)],as.character)
mrf$popnum[mrf$popnum=='a']<-1
mrf$popnum[mrf$popnum=='c']<-3
mrf$popnum[mrf$popnum=='n']<-5
mrf$popnum[mrf$popnum=='s']<-7
mrf$popnum[mrf$popnum=='v']<-9
mrf$popnum[mrf$popnum=='y']<-11

mrf[,23]<-sapply(mrf[,23],as.character)
mrf$habitat[mrf$habitat=='g']<-1
mrf$habitat[mrf$habitat=='l']<-3
mrf$habitat[mrf$habitat=='m']<-5
mrf$habitat[mrf$habitat=='p']<-7
mrf$habitat[mrf$habitat=='u']<-9
mrf$habitat[mrf$habitat=='w']<-11
mrf$habitat[mrf$habitat=='d']<-13

mrf

#Preparing the training data
options(digits = 4)
set.seed(4)
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10)
write.csv(mrf,"./data/processedData.csv",row.names = FALSE)
processeddata = read.csv('./data/processedData.csv')
#scale(processeddata)
summary(processeddata)

normalize<-function(x){
  return((x-min(x)) /(max(x)-min(x)))
}

data<-as.data.frame(lapply(processeddata[1:23],normalize))
summary(data)
data[is.na(data)]<-0
na.omit(data)
write.csv(data,"./data/normalizedData.csv",row.names = FALSE)

View(data)
mrf=data

mrpca_sc <-scree(data[,-17],factors = TRUE)
mrpca_sc

mrpca16<-principal(data[,-17], nfactors=16, rotate="none")
mrpca16
head(mrpca16$scores)
round(unclass(mrpca16$weights),2)
plot(mrpca16)
biplot(mrpca16,scale=0)

mrpca8<-principal(data[,-17], nfactors=8, rotate="varimax")
mrpca8
head(mrpca8$scores)
round(unclass(mrpca8$weights),2)
plot(mrpca8)
biplot(mrpca8,scale=0)

mrpca2<-principal(data[,-17], nfactors=2, rotate="none")
mrpca2
#head(mrpca2$scores)
round(unclass(mrpca2$weights),2)
#plot(mrpca2)
biplot(mrpca2,scale=0)

prin_comp<-prcomp(data[,-17],scale=T)
names(prin_comp)
dim(prin_comp$x)
summary(prin_comp)
biplot(prin_comp,scale=0)
std_dev <- prin_comp$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:22]
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
biplot(prin_comp,scale=0)


head(data)
mrpca<-princomp(data[,-17],cor=TRUE)
summary(mrpca)
loadings(mrpca)
head(mrpca$scores)
stats.arrests <- data.frame(mrpca$scores[,-c(12:22)])
head(stats.arrests)
plot(stats.arrests)

stats1.arrests <- data.frame(mrpca$scores[,c(1,6)])
head(stats1.arrests)
plot(stats1.arrests)


head(data)
mrf_new = data
head(mrf_new)
mrf_new[4] <-NULL
mrf_new[4] <-NULL
mrf_new[5] <-NULL
mrf_new[8] <-NULL
mrf_new[8] <-NULL
mrf_new[10] <-NULL
mrf_new[10] <-NULL
mrf_new[10] <-NULL
mrf_new[10] <-NULL
mrf_new[10] <-NULL
mrf_new[10] <-NULL
mrf_new[10] <-NULL
mrf_new[10] <-NULL
head(mrf_new)

data.pam3<-pam(mrf, k = 3, metric = "euclidean", stand = FALSE)
data.pam3

data.pam5<-pam(mrf, k = 5, metric = "euclidean", stand = FALSE)
data.pam5

data.pam7<-pam(mrf, k = 7, metric = "euclidean", stand = FALSE)
data.pam7

data_new.pam3<-pam(mrf_new, k = 3, metric = "euclidean", stand = FALSE)
data_new.pam3

data_new.pam5<-pam(mrf_new, k = 5, metric = "euclidean", stand = FALSE)
data_new.pam5

data_new.pam7<-pam(mrf_new, k = 7, metric = "euclidean", stand = FALSE)
data_new.pam7

train70.df<-sample(nrow(mrf),0.7*nrow(mrf))
mrtrain70.df<-mrf[train70.df,]
mrtest70.df<-mrf[-train70.df,]
train70labels<-mrtrain70.df[,1]
table(mrtrain70.df$class)
table(mrtest70.df$class)
library(class)
knn70.3 <- knn(train=mrtrain70.df, test=mrtest70.df, cl=train70labels, 3)
knn70.3
test70labels<-mrtest70.df[,1]
ct70_1<-CrossTable(test70labels, knn70.3)
ct70_1
knn70.5 <- knn(train=mrtrain70.df, test=mrtest70.df, cl=train70labels, 5)
knn70.5
ct70_2<-CrossTable(test70labels, knn70.5)
ct70_2
knn70.7 <- knn(train=mrtrain70.df, test=mrtest70.df, cl=train70labels, 7)
knn70.7
ct70_3<-CrossTable(test70labels, knn70.7)
ct70_3

train70_new.df<-sample(nrow(mrf_new),0.7*nrow(mrf_new))
mrtrain70_new.df<-mrf[train70_new.df,]
mrtest70_new.df<-mrf[-train70_new.df,]
train70_newlabels<-mrtrain70_new.df[,1]
table(mrtrain70_new.df$class)
table(mrtest70_new.df$class)
library(class)
knn70.3_new <- knn(train=mrtrain70_new.df, test=mrtest70_new.df, cl=train70_newlabels, 3)
knn70.3_new
test70_newlabels<-mrtest70_new.df[,1]
ct70_1_new<-CrossTable(test70_newlabels, knn70.3_new)
ct70_1_new
knn70.5_new <- knn(train=mrtrain70_new.df, test=mrtest70_new.df, cl=train70_newlabels, 5)
knn70.5_new
ct70_2_new<-CrossTable(test70_newlabels, knn70.5_new)
ct70_2_new
knn70.7_new <- knn(train=mrtrain70_new.df, test=mrtest70_new.df, cl=train70_newlabels, 7)
knn70.7_new
ct70_3_new<-CrossTable(test70_newlabels, knn70.7_new)
ct70_3_new

train60.df<-sample(nrow(mrf),0.6*nrow(mrf))
mrtrain60.df<-mrf[train60.df,]
mrtest60.df<-mrf[-train60.df,]
train60labels<-mrtrain60.df[,1]
knn60.3 <- knn(train=mrtrain60.df, test=mrtest60.df, cl=train60labels, 3)
#knn60.3
test60labels<-mrtest60.df[,1]
ct60_1<-CrossTable(test60labels, knn60.3)
ct60_1
knn60.5 <- knn(train=mrtrain60.df, test=mrtest60.df, cl=train60labels, 5)
knn60.5
ct60_2<-CrossTable(test60labels, knn60.5)
ct60_2
knn60.7 <- knn(train=mrtrain60.df, test=mrtest60.df, cl=train60labels, 7)
knn60.7
ct60_3<-CrossTable(test60labels, knn60.7)
ct60_3
#table(mrtrain60.df$class)
#table(mrtest60.df$class)

train60_new.df<-sample(nrow(mrf_new),0.6*nrow(mrf_new))
mrtrain60_new.df<-mrf[train60_new.df,]
mrtest60_new.df<-mrf[-train60_new.df,]
train60_newlabels<-mrtrain60_new.df[,1]
table(mrtrain60_new.df$class)
table(mrtest60_new.df$class)
library(class)
knn60.3_new <- knn(train=mrtrain60_new.df, test=mrtest60_new.df, cl=train60_newlabels, 3)
knn60.3_new
test60_newlabels<-mrtest60_new.df[,1]
ct60_1_new<-CrossTable(test60_newlabels, knn60.3_new)
ct60_1_new
knn60.5_new <- knn(train=mrtrain60_new.df, test=mrtest60_new.df, cl=train60_newlabels, 5)
knn60.5_new
ct60_2_new<-CrossTable(test60_newlabels, knn60.5_new)
ct60_2_new
knn60.7_new <- knn(train=mrtrain60_new.df, test=mrtest60_new.df, cl=train60_newlabels, 7)
knn60.7_new
ct60_3_new<-CrossTable(test60_newlabels, knn60.7_new)
ct60_3_new


train50.df<-sample(nrow(mrf),0.5*nrow(mrf))
mrtrain50.df<-mrf[train50.df,]
mrtest50.df<-mrf[-train50.df,]
train50labels<-mrtrain50.df[,1]
knn50.3 <- knn(train=mrtrain50.df, test=mrtest50.df, cl=train50labels, 3)
#knn50.3
test50labels<-mrtest50.df[,1]
ct50_1<-CrossTable(test50labels, knn50.3)
ct50_1
knn50.5 <- knn(train=mrtrain50.df, test=mrtest50.df, cl=train50labels, 5)
knn50.5
ct50_2<-CrossTable(test50labels, knn50.5)
ct50_2
knn50.7 <- knn(train=mrtrain50.df, test=mrtest50.df, cl=train50labels, 7)
knn50.7
ct50_3<-CrossTable(test50labels, knn50.7)
ct50_3
#table(mrtrain50.df$class)
#table(mrtest50.df$class)

train50_new.df<-sample(nrow(mrf_new),0.5*nrow(mrf_new))
mrtrain50_new.df<-mrf[train50_new.df,]
mrtest50_new.df<-mrf[-train50_new.df,]
train50_newlabels<-mrtrain50_new.df[,1]
table(mrtrain50_new.df$class)
table(mrtest50_new.df$class)
library(class)
knn50.3_new <- knn(train=mrtrain50_new.df, test=mrtest50_new.df, cl=train50_newlabels, 3)
knn50.3_new
test50_newlabels<-mrtest50_new.df[,1]
ct50_1_new<-CrossTable(test50_newlabels, knn50.3_new)
ct50_1_new
knn50.5_new <- knn(train=mrtrain50_new.df, test=mrtest50_new.df, cl=train50_newlabels, 5)
knn50.5_new
ct50_2_new<-CrossTable(test50_newlabels, knn50.5_new)
ct50_2_new
knn50.7_new <- knn(train=mrtrain50_new.df, test=mrtest50_new.df, cl=train50_newlabels, 7)
knn50.7_new
ct50_3_new<-CrossTable(test50_newlabels, knn50.7_new)
ct50_3_new

train70 = na.omit(mrtrain70.df)
train70





library(ggplot2)
set.seed(20)
mr.kc3<-kmeans(data,3)
mr.kc3
mr.kc3_new<-kmeans(mrf_new,3)
mr.kc3_new
#mr.kc3$center
#mr.kc3$cluster <- as.factor(mr.kc3$cluster)
#ggplot(data, aes(class, cshape, color = data$cluster)) + geom_point()


mr.kc5<-kmeans(data,5)
mr.kc5
mr.kc5$center
mr.kc5_new<-kmeans(mrf_new,3)
mr.kc5_new

mr.kc7<-kmeans(data,7)
mr.kc7
mr.kc7$center
mr.kc7_new<-kmeans(mrf_new,3)
mr.kc7_new


#table(mrtrain70.df$class)
#table(mrtest70.df$class)



#View(mrtrain60.df)
#summary(mrtrain60.df)
#View(mrtest60.df)

myFormula_70 <- mrtrain70.df$class ~ mrtrain70.df$sshape+mrtrain70.df$sroot
mr_70_ctree <- ctree(myFormula_70,data=mrtrain70.df)
mrtrain70.df$class
table(predict(mr_70_ctree), mrtrain70.df$class)
plot(mr_70_ctree,type="simple")
testPred <- predict(mr_70_ctree, newdata = mrtest70.df)
View(testPred)
length(testPred)
length(mrtest70.df$class)
length(mrtrain70.df$class)
table(testPred)

sms_60_classifier <- naiveBayes(mrtrain60.df,mrtrain60.df[,1])
sms_60_predictions<-predict(sms_60_classifier,mrtest60.df)
myFormula_60 <- mrtrain60.df$class ~ mrtrain60.df$sshape+mrtrain60.df$sroot
mr_60_ctree <- ctree(myFormula_60,data=mrtrain60.df)
table(predict(mr_60_ctree), mrtrain60.df$class)
plot(mr_60_ctree,type="simple")
predict(mr_60_ctree)
testPred <- predict(mr_60_ctree, newdata = mrtest60.df)
table(testPred)

sms_50_classifier <- naiveBayes(mrtrain50.df,mrtrain50.df[,1])
sms_50_predictions<-predict(sms_50_classifier,mrtest50.df)
myFormula_50 <- mrtrain50.df$class ~ mrtrain50.df$sshape+mrtrain50.df$sroot
mr_50_ctree <- ctree(myFormula_50,data=mrtrain50.df)
table(predict(mr_50_ctree), mrtrain50.df$class)
plot(mr_50_ctree,type="simple")
predict(mr_50_ctree)
testPred <- predict(mr_50_ctree, newdata = mrtest50.df)
table(testPred)


mrf_lm <- lm(data$class ~ data$cshape+data$csurface+data$ccolor+data$bruises+data$odor+
               data$gattach+data$gspace+data$gsize+data$gcolor+data$sshape+data$sroot+
               data$ssabove+data$ssbelow+data$scabove+data$scbelow+data$vcolor+
               data$rnumber+data$rtype+data$spcolor+data$popnum+data$habitat, data = mrf)
mrf_lm
confint(mrf_lm)
#plot(mrf_lm)
#vif(mrCollinear)

train70_lm <- lm( class ~ cshape + csurface + ccolor + bruises + odor+
                    gattach + gspace + gsize + gcolor + sshape + sroot + 
                    ssabove + ssbelow + scabove + scbelow + vcolor+
                    rnumber + rtype + spcolor + popnum + habitat, data = mrtrain70.df)
train70_lm
plot(train70_lm)
test70_lm_Pre <- predict(train70_lm, data = mrtest70.df)
test70_lm_Pre
plot(test70_lm_Pre)

train70_new_lm <- lm( class ~ cshape + csurface + odor+
                      gspace + gsize + gcolor + 
                      ssabove + ssbelow + habitat, data = mrtrain70_new.df)
train70_new_lm
plot(train70_new_lm)
test70_new_lm_Pre <- predict(train70_new_lm, data = mrtest70_new.df)
test70_new_lm_Pre
plot(test70_new_lm_Pre)

train60_lm <- lm( class ~ cshape + csurface + ccolor + bruises + odor+
                    gattach + gspace + gsize + gcolor + sshape + sroot + 
                    ssabove + ssbelow + scabove + scbelow + vcolor+
                    rnumber + rtype + spcolor + popnum + habitat, data = mrtrain60.df)
train60_lm
plot(train60_lm)
test60_lm_Pre <- predict(train60_lm, data = mrtest60.df)
test60_lm_Pre
plot(test60_lm_Pre)

train60_new_lm <- lm( class ~ cshape + csurface + odor+
                        gspace + gsize + gcolor + 
                        ssabove + ssbelow + habitat, data = mrtrain60_new.df)
train60_new_lm
plot(train60_new_lm)
test60_new_lm_Pre <- predict(train60_new_lm, data = mrtest60_new.df)
test60_new_lm_Pre
plot(test60_new_lm_Pre)

train50_lm <- lm(class ~ cshape + csurface + ccolor + bruises + odor+
                   gattach + gspace + gsize + gcolor + sshape + sroot + 
                   ssabove + ssbelow + scabove + scbelow + vcolor+
                   rnumber + rtype + spcolor + popnum + habitat, data = mrtrain50.df)
train50_lm
test50_lm_Pre <- predict(train50_lm, data = mrtest50.df)
test50_lm_Pre
plot(test50_lm_Pre)

train50_new_lm <- lm( class ~ cshape + csurface + odor+
                        gspace + gsize + gcolor + 
                        ssabove + ssbelow + habitat, data = mrtrain50_new.df)
train50_new_lm
plot(train50_new_lm)
test50_new_lm_Pre <- predict(train50_new_lm, data = mrtest50_new.df)
test50_new_lm_Pre
plot(test50_new_lm_Pre)

mrf_glm <- glm(data$class ~ data$cshape+data$csurface+data$ccolor+data$bruises+data$odor+
                 data$gattach+data$gspace+data$gsize+data$gcolor+data$sshape+data$sroot+
                 data$ssabove+data$ssbelow+data$scabove+data$scbelow+data$vcolor+
                 data$rnumber+data$rtype+data$spcolor+data$popnum+data$habitat, family=gaussian, data = mrf)
mrf_glm

train70_glm <- glm(class ~ cshape + csurface + ccolor + bruises + odor+
                     gattach + gspace + gsize + gcolor + sshape + sroot + 
                     ssabove + ssbelow + scabove + scbelow + vcolor+
                     rnumber + rtype + spcolor + popnum + habitat, data = mrtrain70.df)
train70_glm
plot(train70_glm)
test70_glm_Pre <- predict(train70_glm, data = mrtest70.df)
test70_glm_Pre
plot(test70_glm_Pre)

train70_new_glm <- glm( class ~ cshape + csurface + odor+
                        gspace + gsize + gcolor + 
                        ssabove + ssbelow + habitat, data = mrtrain70_new.df)
train70_new_glm
plot(train70_new_glm)
test70_new_glm_Pre <- predict(train70_new_glm, data = mrtest70_new.df)
test70_new_glm_Pre
plot(test70_new_glm_Pre)

train60_glm <- glm(class ~ cshape + csurface + ccolor + bruises + odor+
                     gattach + gspace + gsize + gcolor + sshape + sroot + 
                     ssabove + ssbelow + scabove + scbelow + vcolor+
                     rnumber + rtype + spcolor + popnum + habitat, data = mrtrain60.df)
train60_glm
test60_glm_Pre <- predict(train60_glm, data = mrtest60.df)
test60_glm_Pre
plot(test60_glm_Pre)

train60_new_glm <- glm( class ~ cshape + csurface + odor+
                        gspace + gsize + gcolor + 
                        ssabove + ssbelow + habitat, data = mrtrain60_new.df)
train60_new_glm
plot(train60_new_glm)
test60_new_glm_Pre <- predict(train60_new_glm, data = mrtest60_new.df)
test60_new_glm_Pre
plot(test60_new_glm_Pre)

train50_glm <- glm(class ~ cshape + csurface + ccolor + bruises + odor+
                     gattach + gspace + gsize + gcolor + sshape + sroot + 
                     ssabove + ssbelow + scabove + scbelow + vcolor+
                     rnumber + rtype + spcolor + popnum + habitat, data = mrtrain50.df)
train50_glm
plot(train50_glm)
test50_glm_Pre <- predict(train50_glm, data = mrtest50.df)
test50_glm_Pre
plot(test50_glm_Pre)

train50_new_glm <- glm( class ~ cshape + csurface + odor+
                        gspace + gsize + gcolor + 
                        ssabove + ssbelow + habitat, data = mrtrain50_new.df)
train50_new_glm
plot(train50_new_glm)
test50_new_glm_Pre <- predict(train50_new_glm, data = mrtest50_new.df)
test50_new_glm_Pre
plot(test50_new_glm_Pre)
#points(mrtest50.df$class, test50_glm_Pre,col = "blue", pch =4)


data(mrf)
attach(mrf)
mrfsvm <- svm(class~ccolor+cshape, data = mrf,kernel="linear")
mrfsvm
mrfsvm_new <- svm(class~cshape + odor + csurface + gspace + ssabove, data = mrf_new,kernel="linear")
mrfsvm_new




















