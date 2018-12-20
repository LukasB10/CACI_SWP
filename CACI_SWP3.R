#SWP 3 - Describing Data, Segmentation & Clustering

seg.raw <- read.csv("indivData.csv")
str(seg.raw)
seg.raw

summary(seg.raw)
seg.df <- seg.raw [ , -7]
summary(seg.df)

seg.summ <- function (data , groups) 
{aggregate (data , list(groups), function (x) mean(as.numeric (x)))}

# recode factor variables to binary data
seg.summ ( seg.df , seg.raw$Segment )
seg.df$ownhome.num<- as.numeric(seg.df$ownHome)
aggregate(seg.df,by=list(seg.raw$Segment),mean,na.rm=TRUE)

seg.df.num<-seg.df
seg.df.num$gender <- ifelse(seg.df$gender=="Male",0,1)
seg.df.num$ownHome <- ifelse(seg.df$ownHome =="ownNo",0 ,1)
seg.df.num$subscribe <- ifelse(seg.df$subscribe =="subNo" ,0 ,1)
summary(seg.df)
summary(seg.df.num)

head(seg.df)
tmp<-dist(seg.df[,-3])
as.matrix(tmp)[1:5,1:5]

tmp<-scale(seg.df.num)
str(tmp)
summary(tmp)

# compute distances
str(seg.df.num)
seg.df.scale<-apply(seg.df.num,2,scale)
t(seg.df.scale)%*%seg.df.scale/299 # check with compution of correlation 
summary(seg.df.scale)
summary(seg.df.num)
str(seg.df.scale)
seg.df.scale<-as.matrix(seg.df.scale)
seg.df.scale.dist <- dist(seg.df.scale) # compute distance on standardized data
as.matrix(seg.df.scale.dist)[1:5,1:5]


?daisy
summary(seg.df)
tmp <- daisy(seg.df)
as.matrix(tmp)[1:5,1:5]

library(cluster)
str(seg.df.num)
seg.dist <- daisy(seg.df.num,stand=TRUE) # check different ways to compute dissimilarities
str(seg.dist)
as.matrix(seg.dist)[1:5,1:5]

seg.dist <- daisy(seg.df) # check different ways to compute dissimilarities
str(seg.dist)
as.matrix(seg.dist)[1:5,1:5]

seg.dist <- dist(seg.df.num) # check different ways to compute dissimilarities
str(seg.dist)
as.matrix(seg.dist)[1:5,1:5]


# hierachical clustering with distances from daisy
?daisy
seg.hc <- hclust (seg.dist , method ="single")
?hclust
str(seg.hc)
plot(seg.hc)
seg.hc
summary(seg.hc)
plot(cut(as.dendrogram(seg.hc), h =0.5)$lower[[1]])
plot(rev(seg.hc$height^2))
plot(rev(seg.hc$height^2)[1:50], type="b")
seg.df[c(101 , 107) , ]

seg.hc.segment <- cutree(seg.hc, k=4)
str(seg.hc.segment)
table(seg.hc.segment)
seg.df$single4<-seg.hc.segment
str(seg.df)


seg.summ ( seg.df , seg.df$single4 )
summary(seg.df)

# hierachical clustering with distances from standardized data and Euclidean

seg.hc <- hclust (seg.df.scale.dist , method ="single")
plot(seg.hc)
plot(cut(as.dendrogram(seg.hc), h =0.5)$lower[[1]])
plot(rev(seg.hc$height^2))
plot(rev(seg.hc$height^2)[1:50], type="b")
seg.df[c(101 , 107) , ]

seg.hc.segment <- cutree(seg.hc, k=4)
str(seg.hc.segment)
seg.df.scale.dist$single4<-seg.hc.segment
str(seg.df)



# interpret cluster solution
cor(cophenetic(seg.hc), seg.dist )
plot(seg.hc)
rect.hclust(seg.hc, k=9, border ="red")
seg.hc.segment <- cutree(seg.hc, k=4)
table(seg.hc.segment )

str(seg.hc.segment)
seg.summ(seg.df,seg.hc.segment )

plot(jitter(as.numeric(seg.df$gender)) ~
       jitter(as.numeric(seg.df$ subscribe)),
     col= seg.hc.segment,yaxt="n", xaxt ="n", ylab ="", xlab="")
axis(1, at=c(1,2),labels=c(" Subscribe : No","Subscribe:Yes"))
axis(2, at=c(1,2),labels=levels(seg.df$gender))





# k-Means
set.seed (1) 
seg.k <- kmeans(seg.df.num , centers =10)
str(seg.k)
seg.k
seg.summ (seg.df , seg.k$cluster )

seg.k$betweenss
seg.k$totss
seg.k$betweenss/seg.k$totss
set.seed (987) 
seg.k <- kmeans(seg.df.num , centers =10)
seg.k$betweenss

t<-kmeans(tnw[,2:10],centers=4)
t

ss.all<-data.frame(i=1:10000,fit=0)
head(ss.all)
for(i in 1:10000){
  set.seed(i+100)
  tmp <- kmeans(seg.df.num , centers =4)
  ss.all[ss.all$i==i,]$fit<-tmp$betweenss
}
str(ss.all)
ss.all[which.max(ss.all$fit), ]
ss.all[1:10,]
seg.k <- kmeans(seg.df.num , centers =4)
seg.k
seg.summ (seg.df , seg.k$cluster )
set.seed (102) 
seg.k <- kmeans(seg.df.num , centers =4)
seg.summ (seg.df , seg.k$cluster )
boxplot(seg.df.num$income~seg.k$cluster ,
        xlab ="Income", ylab ="Segment",
        horizontal = TRUE )
library ( cluster )
clusplot(seg.df , seg.k$cluster , color =TRUE , shade =TRUE ,
         labels =4, lines =0, main ="K- means cluster plot ")



# Cluster Persons from bluetooth speaker questionaire

bluetooth<-read.csv("indivData.csv")
head(bluetooth)
str(bluetooth)
summary(bluetooth)
bluetooth.dist<-dist(apply(bluetooth[,13:26],2,scale)) # one set of variables selected
str(bluetooth.dist)
as.matrix(bluetooth.dist)[1:5,1:5]
library(MASS)
fit <- isoMDS(bluetooth.dist, k = 2)
fit
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", main = "Metric MDS", 
     pch = 19, ylim = c(-5.5, 5.5), xlim = c(-5.5, 5.5))
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", main = "Metric MDS", 
     pch = 19, )
text(x, y, labels = bluetooth$id, cex = 1, pos = 4)
abline(h = 0, v = 0, col = "grey")

bluetoothclust <- hclust(bluetooth.dist, method ="single")
plot(bluetoothclust)
bluetoothclust <- hclust(bluetooth.dist, method ="ward.D2")
plot(bluetoothclust)

summary(bluetooth)
bluetoothclust.segment <- cutree(bluetoothclust, k=4)
str(bluetoothclust.segment)


table(bluetoothclust.segment)

seg.summ(bluetooth,bluetoothclust.segment)


plot(rev(seg.hc$height^2))
plot(rev(seg.hc$height^2)[1:50], type="b")


set.seed (1) 
seg.k <- kmeans(bluetooth.dist , centers =4)
seg.summ (bluetooth,seg.k$cluster )


#test

#Lukas