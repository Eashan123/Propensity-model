
library("AzureML")
ws <- workspace()
dat <- download.datasets(ws, "BikeBuyerWithLocation")

head(dat)

summary(dat)

datt <- dat

str(datt)

table(is.na(datt))

install.packages("caret",repos = "/tmp/RtmpmvMU3c/downloaded_packages")

install.packages("caret")

install.packages("mlbench")

install.packages("FSelector") #for chi-squared test

library(caret)
library(mlbench)

#Filter based Feature selection .Chi-squared test.
#https://www.quora.com/How-is-chi-test-used-for-feature-selection-in-machine-learning    


#Man this code is horror code . If you run it it will kill your kernel . Dont run it .
    library(FSelector)#For method
    
     
     
    #Calculate the chi square statistics 
    weights<- chi.squared(BikeBuyer~., datt)
     
     
    # Print the results 
    print(weights)
     
     
    # Select top five variables
    subset<- cutoff.k(weights, 10)
     
     
    # Print the final formula that can be used in classification
    f<- as.simple.formula(subset, "BikeBuyer")
    print(f)

dim(dat)

dim(datt)

summary(datt)

head(datt,n=20)

# types
sapply(datt, class)

datt <- datt[,-1]

#standard deviation
sapply(datt[,1:17],sd)

#selecting only the requierd columns
datt2 <- subset(datt, select = -c(Latitude,Longitude,Marital.Status,Gender) )
str(datt2)

datt <- datt2

head(datt)

head(datt,n=20)

#data type

str(datt)

install.packages("outliers")
library(outliers)

outlier_tf = outlier(datt$Yearly.Income,logical=TRUE)

sum(outlier_tf)

#we dont require marital status.
datt[, 'Marital.Status'] <- as.factor(datt[, 'Marital.Status'])
summary(datt$Marital.Status)

#we converted it to numeric for k-means , if required I may convert it to numeric ahead depending on what algorithm i am dealing with
datt$Marital.Status <- as.numeric(datt$Marital.Status)
str(datt)

str(datt)
head(datt,n=30)

#we dont require gender.
#Dont make it orderd and check the graph
datt[, 'Gender'] <- factor(datt[, 'Gender'],ordered = TRUE,levels = c("Female","Male"))
str(datt)

datt[, 'Gender'] <- as.factor(datt[, 'Gender'])
str(datt)

datt$Gender <- as.numeric(datt$Gender)
str(datt)

datt$Yearly.Income <- as.numeric(datt$Yearly.Income)
str(datt)

datt[, 'Education'] <- as.factor(datt[, 'Education'])
str(datt)

# Just for yur understanding of how factors work .
 datt[, 'Education'] <- factor(datt[, 'Education'],levels = c("Partial High School","High School","Partial College","Bachelors","Graduate Degree"))
str(datt)

x2 = as.numeric(x1)
str(x2)

levels(datt$Education)

datt$Education <- as.numeric(datt$Education)
str(datt)
summary(datt$Education)

datt[, 'Occupation'] <- factor(datt[, 'Occupation'],levels = c("Clerical","Manual","Skilled Manual","Professional","Management"))
summary(datt$Occupation)

#if required we may convert it to numeric later
datt$Occupation <- as.numeric(datt$Occupation)
str(datt)


datt[, 'Home.Owner'] <- factor(datt[, 'Home.Owner'])
summary(datt$Home.Owner)

#Integer 0/1 value
xx <- as.integer(datt$Home.Owner == "Yes")
str(xx)
datt$Home.Owner <- xx
str(datt)

#we conver it into 1 and 0 that is integer , not to numeric.
#if required we may convert it to numeric later
datt$Home.Owner <- as.numeric(datt$Home.Owner)
str(datt)

xxx <- factor(datt[, 'Commute.Distance'],levels = c("0-1 Miles","1-2 Miles","2-5 Miles","5-10 Miles","10+ Miles"))
str(xxx)
datt$Commute.Distance <- as.numeric(xxx)
str(datt)

datt[, 'Commute.Distance'] <- factor(datt[, 'Commute.Distance'])
summary(datt$Commute.Distance)

#if required we may convert it to numeric later
datt$Commute.Distance <- as.numeric(datt$Commute.Distance)
str(datt)

datt[, 'Region'] <- factor(datt[, 'Region'],levels = c("North America","Europe","Pacific"))
summary(datt$Region)

#if required we may convert it to numeric later
datt$Region <- as.numeric(datt$Region)
str(datt)


datt$Age <- as.numeric(datt$Age)
str(datt)

#Integer 0/1 value
bb <- as.integer(datt$BikeBuyer == "Yes")
str(bb)
datt$BikeBuyer <- bb
str(datt)

#There is a huge class imbalance . 90%no and 10%yes.
datt[, 'BikeBuyer'] <- factor(datt[, 'BikeBuyer'])
summary(datt$BikeBuyer)

#if required we may convert it to numeric later
datt$BikeBuyer <- as.numeric(datt$BikeBuyer)
str(datt)

datt[, 'City'] <- factor(datt[, 'City'])
summary(datt$City)

levels(datt$City)

summary(datt$City)

k <- as.numeric(datt$City)
str(k)
summary(k)
datt$City <- k
str(datt)

datt[, 'Zip.Code'] <- factor(datt[, 'Zip.Code'])
summary(datt$Zip.Code)

levels(datt$Zip.Code)

z <- as.numeric(datt$Zip.Code)
str(z)
summary(z)
datt$Zip.Code <- z
str(datt)

datt[, 'Country'] <- factor(datt[, 'Country'])
levels(datt$Country)
str(datt$Country)
summary(datt$Country)

c <- as.numeric(datt$Country)
str(c)
summary(c)
datt$Country <- c
str(datt)

datt$Children <- as.numeric(datt$Children)
str(datt)

datt$Cars <- as.numeric(datt$Cars)

str(datt)

head(datt)

str(datt)

summary(datt$Marital.Status)

summary(datt$Gender)

summary(datt$Yearly.Income)

summary(datt$Children)
head(datt,n=200)

summary(datt$Education) #only education is missing values and we really cannot afford to losse this data

summary(datt$Occupation)

summary(datt$Home.Owner)

summary(datt$Cars)

summary(datt$Commute.Distance)

summary(datt$Region)

summary(datt$Age)

summary(datt$BikeBuyer)

#to count no of missing values.
sum(is.na(datt$Education))

sum(is.na(datt))
dim(datt)

datt <- na.omit(datt)
dim(datt)

#I have replaced all missing numeric data to its median.But if you see above we'll notice that we are not having any numerical value which is missing .This function is not needed at all.
impute <- function(data, type) {
  for (i in which(sapply(data, is.numeric))) {
    data[is.na(data[, i]), i] <- type(data[, i],  na.rm = TRUE)
  }
  return(data)}
newdata <- impute(datt,mean)

#we must work with new data
dim(newdata)

str(newdata)
sum(is.na(newdata))
dim(newdata)

datt <- newdata
sum(is.na(datt))
dim(datt)

dim(datt)

#we must work with newdata
duplicated(newdata)

#showing duplicated rows 
duplicated(datt)

newdata[duplicated(newdata), ]

datt[duplicated(datt), ]

newdatt <- newdata[!duplicated(newdata), ]

newdatt <- datt[!duplicated(datt), ]

dim(newdatt)

# class distribution
cbind(freq=table(newdatt$BikeBuyer), percentage=prop.table(table(newdatt$BikeBuyer))*100)

head(newdatt,n=20)

str(newdatt)
sum(is.na(newdatt))

library(ggplot2)

plot(newdatt$Yearly.Income)

ggplot(newdatt,aes(Marital.Status)) + geom_bar()
#First we would see univariate distribution to get a glimpse into the dimension of our data

#Doesnt work this way
plot(newdatt$Mrital.Status)

ggplot(newdatt,aes(Gender)) + geom_bar()

ggplot(newdatt,aes(Yearly.Income)) + geom_bar()

ggplot(newdatt,aes(Yearly.Income)) + geom_scatterplot()

ggplot(newdatt,aes(Yearly.Income)) + geom_histogram()

#just because we want to plot somethng on x-axis, so factor(0) is a null value basically
ggplot(newdatt,aes(x=factor(0),y=Yearly.Income)) + geom_boxplot()

ggplot(newdatt,aes(Education))+geom_bar()

ggplot(newdatt,aes(Occupation))+geom_bar()

ggplot(newdatt,aes(Home.Owner))+geom_bar()

ggplot(newdatt,aes(Cars))+geom_bar()
#What if I make Cars as a factor would it change my model working

ggplot(newdatt,aes(Commute.Distance))+geom_bar()

ggplot(newdatt,aes(Region))+geom_bar()

ggplot(newdatt,aes(Age))+geom_bar()
#This is because its a continuous variable

ggplot(newdatt,aes(Age))+geom_histogram()

ggplot(newdatt,aes(BikeBuyer))+geom_bar()
#This shows severe class imbalance.This case is similar to card fraudulent case.Also try it with character metadata

#wrong way to plot categorical variables.2x
options(repr.plot.width=5,repr.plot.height=5)
ggplot(newdatt,aes(Gender,Marital.Status)) + geom_point() + xlab("Bike Buyer") + ylab("Marital Status") + ggtitle("Relation between Marital Status and Propensity to Buy")
#It seems scatter plot works well when data is continuous and not when the variables are factors like ours.We dont understand anything with this sort of relationship

plot(newdatt$Marital.Status,newdatt$BikeBuyer)
# so here we see that the probability of people who are married is more likely to buy bikes.

# This wont be plotted as we have converted categorical variables in numerical
ggplot(newdatt) + geom_bar(aes(BikeBuyer,fill = Marital.Status))#Stacked bar-chart default
ggplot(newdatt) + geom_bar(aes(BikeBuyer,fill = Marital.Status),position = "dodge") #side-by-side bar chart
ggplot(newdatt) + geom_bar(aes(BikeBuyer,fill = Marital.Status),position = "fill") #Filled bar chart

# other way to plot 2x categorical variables
spineplot(newdatt$Marital.Status,newdatt$BikeBuyer)

spineplot(newdatt$Gender,newdatt$BikeBuyer)
#male is very slight more

ggplot(newdatt) + geom_bar(aes(BikeBuyer,fill = Gender))#Stacked bar-chart default
ggplot(newdatt) + geom_bar(aes(BikeBuyer,fill = Gender),position = "dodge") #side-by-side bar chart
ggplot(newdatt) + geom_bar(aes(BikeBuyer,fill = Gender),position = "fill") #Filled bar chart

spineplot(newdatt$Yearly.Income,newdatt$BikeBuyer)

plot(newdatt$Yearly.Income,newdatt$BikeBuyer)
#doesnt plot between numerical and categorical feature

ggplot(newdatt, aes(x=BikeBuyer, y=Yearly.Income)) + stat_summary(fun.y="mean", geom="bar")
#stat_summary helps in plotting a categorical variable against a numerical variable.But I am not able to understand what is this conveying. I am just not getting any idea.

ggplot(newdatt, aes(x=BikeBuyer, y=Children)) + stat_summary(fun.y="mean", geom="bar")
#This graph is conveying something . Try understand it.

spineplot(newdatt$Education,newdatt$BikeBuyer)

ggplot(newdatt) + geom_bar(aes(BikeBuyer,fill = Education))#Stacked bar-chart default
ggplot(newdatt) + geom_bar(aes(BikeBuyer,fill = Education),position = "dodge") #side-by-side bar chart
ggplot(newdatt) + geom_bar(aes(BikeBuyer,fill = Education),position = "fill") #Filled bar chart

spineplot(newdatt$Occupation,newdatt$BikeBuyer)

ggplot(newdatt) + geom_bar(aes(BikeBuyer,fill = Occupation))#Stacked bar-chart default
ggplot(newdatt) + geom_bar(aes(BikeBuyer,fill = Occupation),position = "dodge") #side-by-side bar chart
ggplot(newdatt) + geom_bar(aes(BikeBuyer,fill = Occupation),position = "fill") #Filled bar chart

spineplot(newdatt$Home.Owner,newdatt$BikeBuyer)

ggplot(newdatt, aes(x=BikeBuyer, y=Cars)) + stat_summary(fun.y="mean", geom="bar")

spineplot(newdatt$Commute.Distance,newdatt$BikeBuyer)

spineplot(newdatt$Region,newdatt$BikeBuyer)

ggplot(newdatt) + geom_bar(aes(BikeBuyer,fill = Region))#Stacked bar-chart default
ggplot(newdatt) + geom_bar(aes(BikeBuyer,fill = Region),position = "dodge") #side-by-side bar chart
ggplot(newdatt) + geom_bar(aes(BikeBuyer,fill = Region),position = "fill") #Filled bar chart

ggplot(newdatt, aes(x=Age, y=BikeBuyer)) + stat_summary(fun.y="mean", geom="bar")

ggplot(newdatt,aes(Yearly.Income,as.numeric(BikeBuyer))) + geom_point(position = position_jitter(w=0.05,h=0.05)) + geom_smooth()
#refer nina zumel book .here this graph makes no sense.

#doesnt work
plot(newdatt$Marital.Status,newdatt$BikeBuyer)

str(newdatt)

newdatts <- newdatt$Yearly.Income
qnt <- quantile(newdatts, probs=c(.25, .71), na.rm = T)
caps <- quantile(newdatts, probs=c(.05, .90), na.rm = T)
H <- 1.5 * IQR(newdatts, na.rm = T)
newdatts[newdatts < (qnt[1] - H)] <- caps[1]
newdatts[newdatts > (qnt[2] + H)] <- caps[2]
k <- newdatts
newdatt$Yearly.Income <- k
str(k)
summary(k)
summary(newdatt$Yearly.Income)

str(newdatt)

install.packages("outliers")
library(outliers)

str(newdatts)

outlier_tf = outlier(newdatt$Yearly.Income,logical=TRUE)
sum(outlier_tf)

#What were the outliers
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
#Removing the outliers
newdatto = newdatt[-find_outlier,]
nrow(newdatto)

head(newdatt,3)

a <- scale(newdatt$Yearly.Income, center = TRUE, scale = TRUE)
str(a)
newdatt$Yearly.Income <- a
str(newdatt)

b <- scale(newdatt$Age, center = TRUE, scale = TRUE)
str(b)
newdatt$Age <- b
str(newdatt)

d <- scale(newdatt$Longitude, center = TRUE, scale = TRUE)
str(b)
newdatt$Longitude <- d
str(newdatt)

#we are normalizing the data [0-1]
install.packages("reshape")

library("reshape")

newdatt_ss<-rescaler(newdatt[],"range")

dim(newdatto)
str(newdatto)

#for unsupervised learning ""Practical Guide To Cluster Analysis in R.
#Disadvantage is that this works only when all the data is numeric.Does not work selectively .
head(newdatt,n=7)
newdatts <- scale(newdatt['Yearly.Income','Age','Cars'])
head(newdatts, n = 7)

performScaling <- TRUE  # Turn it on/off for experimentation.

if (performScaling) {

    # Loop over each column.
    for (colName in names(newdatt)) {

        # Check if the column contains numeric data.
        if(class(newdatt[,colName]) == 'integer' | class(newdatt[,colName]) == 'numeric') {

            # Scale this column (scale() function applies z-scaling).
            newdatt[,colName] <- scale(newdatt[,colName])
        }
    }
}

#reshape package [0-1] scaling
performScaling <- TRUE  # Turn it on/off for experimentation.

if (performScaling) {

    # Loop over each column.
    for (colName in names(newdatt)) {

        # Check if the column contains numeric data.
        if(class(newdatt[,colName]) == 'integer' | class(newdatt[,colName]) == 'numeric') {

            # Scale this column (scale() function applies z-scaling).
            newdatt[,colName] <- rescaler(newdatt[,colName],"range")
        }
    }
}

head(newdatt,7) #rescaler

head(newdatt,7) #scale

install.packages("devtools")

library(devtools)

devtools::install_github("kassambara/factoextra")

install.packages("NbClust")

install.packages("factoextra", repos='http://cran.us.r-project.org')

#Its not needed now , not very important , now.
#Computing euclidean distance pg:30 . I have not understood how it works 
#for unsupervised learning "Practical Guide To Cluster Analysis in R"
dist.eucl <- dist(newdatt_s, method = "euclidean")
round(as.matrix(dist.eucl)[1:12, 1:12], 1)

#we	will	use	the	correlation	coefficient	as	a	unit	of	valuation.This has been taken from Unsupervised learning in R pdf.Page:218
install.packages("FSelector")

library(FSelector)

#with my new 13 features.Seems region is weakest.
Scores <- linear.correlation(BikeBuyer~.,newdatt)
Scores

Scores <- linear.correlation(BikeBuyer~.,newdatt)
Scores

Subset <- cutoff.k(Scores,10)
as.data.frame(Subset)

#2nd method for feature selection embedded method . I am not able to understnd how it works .Leave it dont waste your time with it.
install.packages("wskm")
library("wskm")
set.seed(100)

model <- ewkm(newdatt,3,lambda=2,maxiter=1000)

install.packages("cluster")
library("cluster")


clusplot(newdatt,model$cluster,color=TRUE,shade=TRUE,labels=5,lines=2,main='Cluster	Analysis BikeBuyer')

round(model$weights*100,20)

?clusplot()

#wrapper method for feature selection :
install.packages("clustvarsel")
library(clustvarsel)

install.packages("mclust")
library(mclust)

#Run this when you have some other work to do . This takes hell of the time.
out = clustvarsel(newdatt,G = 1:12)
out

#Filter method for feature selection
install.packages("corrplot")
library(corrplot)

correlationMatrix <- cor(newdatt_s)

#So we are not having any attributes in our dataset that are highly correlated.
highlyCorrelated <- findCorrelation(correlationMatrix,cutoff=0.60)
highlyCorrelated
names(newdatt_s[,highlyCorrelated])


#searching for missing values.
NewData<-newdatt_s
	head(NewData[!complete.cases(NewData),],10)

#METHODS FOR ASSESSING CLUSTERING TENDENCY
install.packages("clustertend")
library(clustertend)
# Compute Hopkins statistic for iris dataset
set.seed(123)
hopkins(newdatt, n = nrow(newdatt)-1)

#For finding ideal no of clusters ."Nina Zumel Book" pg:192.
install.packages("fpc")

library(fpc)

str(newdatt)

clustering.ch <- kmeansruns(newdatt, krange=1:17, criterion="ch")
clustering.ch$bestk

clustering.ch <- kmeansruns(newdatt$Yearly.Income, krange=1:17, criterion="ch")
clustering.ch$bestk

clustering.asw <- kmeansruns(newdatt, krange=1:17, criterion="asw")
clustering.asw$bestk

#k-means "Unsupervised learning in R" page 118
set.seed(42)
KM <- kmeans(newdatt,11,iter.max=1000,algorithm = c("Forgy"))

clusteringI.asw <- kmeansruns(newdatt$Yearly.Income, krange=1:17, criterion="asw")
clusteringI.asw$bestk

print(KM)

kdatt <- cbind(newdatt, cluster = KM$cluster)
head(kdatt)

install.packages("cluster")
library(cluster)

str(newdatt)

#Overall cluster
#refered from https://stackoverflow.com/questions/33999224/silhouette-plot-in-r
dis = dist(newdatt)^2
sil = silhouette (KM$cluster, dis)
plot(sil)

#Marital.Status This for newdatt . All 17 features inclusive but i havent scaled the data.Considering 11 clusters.
set.seed(42)
a <- subset(newdatt, select = -c(Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_M <- kmeans(a,11,iter.max=1000,algorithm = c("Forgy"))
dis_M = dist(newdatt)^2
#dis_M = dist(b)^2
sil_M = silhouette (KM_M$cluster, dis_M)
plot(sil_M)

set.seed(42)
aa <- subset(newdatt, select = -c(Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_M <- kmeans(aa,11,iter.max=1000,algorithm = c("Forgy"))
#dis_G = dist(newdatt_s)^2
dis_M = dist(aa)^2
sil_M = silhouette (KM_M$cluster, dis_M)
plot(sil_M)

#look at this error . It gives you a hint in understanding the no of clusters

#Marital.Status This for newdatt . All 17 features inclusive but i havent scaled the data.Considering 2 clusters.
set.seed(42)
a <- subset(newdatt, select = -c(Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_M <- kmeans(a,2,iter.max=1000,algorithm = c("Forgy"))
dis_M = dist(newdatt)^2
#dis_M = dist(b)^2
sil_M = silhouette (KM_M$cluster, dis_M)
plot(sil_M)

set.seed(42)
aa <- subset(newdatt, select = -c(Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_M <- kmeans(aa,2,iter.max=1000,algorithm = c("Forgy"))
#dis_G = dist(newdatt_s)^2
dis_M = dist(aa)^2
sil_M = silhouette (KM_M$cluster, dis_M)
plot(sil_M)

#Marital.Status This for newdatt . All 17 features inclusive.Data is scaled.
set.seed(42)
a <- subset(newdatt, select = -c(Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_M <- kmeans(b,2,iter.max=1000,algorithm = c("Forgy"))
dis_M = dist(newdatt)^2
#dis_M = dist(b)^2
sil_M = silhouette (KM_M$cluster, dis_M)
plot(sil_M)

set.seed(42)
aa <- subset(newdatt, select = -c(Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_M <- kmeans(aa,2,iter.max=1000,algorithm = c("Forgy"))
#dis_G = dist(newdatt_s)^2
dis_M = dist(aa)^2
sil_M = silhouette (KM_M$cluster, dis_M)
plot(sil_M)

#Gender This for newdatt . All 17 features inclusive.But I havent scaled the data.
set.seed(42)
b <- subset(newdatt, select = -c(Marital.Status,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_G <- kmeans(b,2,iter.max=1000,algorithm = c("Forgy"))
dis_G = dist(newdatt)^2
#dis_M = dist(b)^2
sil_G = silhouette (KM_G$cluster, dis_G)
plot(sil_G)

set.seed(42)
bb <- subset(newdatt, select = -c(Marital.Status,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_G <- kmeans(bb,2,iter.max=1000,algorithm = c("Forgy"))
#dis_G = dist(newdatt_s)^2
dis_G = dist(bb)^2
sil_G = silhouette (KM_G$cluster, dis_G)
plot(sil_G)

#Gender This for newdatt . All 17 features inclusive.I have scaled the data.
set.seed(42)
b <- subset(newdatt, select = -c(Marital.Status,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_G <- kmeans(b,2,iter.max=1000,algorithm = c("Forgy"))
dis_G = dist(newdatt)^2
#dis_M = dist(b)^2
sil_G = silhouette (KM_G$cluster, dis_G)
plot(sil_G)

set.seed(42)
bb <- subset(newdatt, select = -c(Marital.Status,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_G <- kmeans(bb,2,iter.max=1000,algorithm = c("Forgy"))
#dis_G = dist(newdatt_s)^2
dis_G = dist(bb)^2
sil_G = silhouette (KM_G$cluster, dis_G)
plot(sil_G)

#Yearly Income This for newdatt . All 17 features inclusive.I havent scaled the data and i take 2
set.seed(42)
c <- subset(newdatt, select = -c(Marital.Status,Gender,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_I <- kmeans(c,2,iter.max=1000,algorithm = c("Forgy"))
dis_I = dist(newdatt)^2
#dis_M = dist(a)^2
sil_I = silhouette (KM_I$cluster, dis_I)
plot(sil_I)

set.seed(42)
c <- subset(newdatt, select = -c(Marital.Status,Gender,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_I <- kmeans(c,2,iter.max=1000,algorithm = c("Forgy"))
#dis_I = dist(newdatt_s)^2
dis_I = dist(c)^2
sil_I = silhouette (KM_I$cluster, dis_I)
plot(sil_I)

#Yearly Income This for newdatt . All 17 features inclusive.I havent scaled the data and i have taken 11 clusters.
set.seed(42)
c <- subset(newdatt, select = -c(Marital.Status,Gender,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_I <- kmeans(c,11,iter.max=1000,algorithm = c("Forgy"))
dis_I = dist(newdatt)^2
#dis_M = dist(a)^2
sil_I = silhouette (KM_I$cluster, dis_I)
plot(sil_I)

set.seed(42)
c <- subset(newdatt, select = -c(Marital.Status,Gender,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_I <- kmeans(c,11,iter.max=1000,algorithm = c("Forgy"))
#dis_I = dist(newdatt_s)^2
dis_I = dist(c)^2
sil_I = silhouette (KM_I$cluster, dis_I)
plot(sil_I)

#Yearly Income This for newdatt . All 17 features inclusive
set.seed(42)
c <- subset(newdatt, select = -c(Marital.Status,Gender,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_I <- kmeans(c,2,iter.max=1000,algorithm = c("Forgy"))
dis_I = dist(newdatt)^2
#dis_M = dist(a)^2
sil_I = silhouette (KM_I$cluster, dis_I)
plot(sil_I)

set.seed(42)
c <- subset(newdatt, select = -c(Marital.Status,Gender,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_I <- kmeans(c,2,iter.max=1000,algorithm = c("Forgy"))
#dis_I = dist(newdatt_s)^2
dis_I = dist(c)^2
sil_I = silhouette (KM_I$cluster, dis_I)
plot(sil_I)

#Yearly Income
set.seed(42)
c <- subset(newdatt_s, select = -c(Marital.Status,Gender,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer) )
KM_I <- kmeans(c,2,iter.max=1000,algorithm = c("Forgy"))
dis_I = dist(newdatt_s)^2
#dis_M = dist(a)^2
sil_I = silhouette (KM_I$cluster, dis_I)
plot(sil_I)

set.seed(42)
c <- subset(newdatt_s, select = -c(Marital.Status,Gender,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer) )
KM_I <- kmeans(c,2,iter.max=1000,algorithm = c("Forgy"))
#dis_I = dist(newdatt_s)^2
dis_I = dist(c)^2
sil_I = silhouette (KM_I$cluster, dis_I)
plot(sil_I)

#children.I havent scaled data and taken 2 clusters.I have included all 17 features.
set.seed(42)
d <- subset(newdatt, select = -c(Gender,Yearly.Income,Marital.Status,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_C <- kmeans(d,2,iter.max=1000,algorithm = c("Forgy"))
dis_C = dist(newdatt)^2
#dis_M = dist(d)^2
sil_C = silhouette (KM_M$cluster, dis_C)
plot(sil_C)

set.seed(42)
d <- subset(newdatt, select = -c(Gender,Yearly.Income,Marital.Status,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_C <- kmeans(d,2,iter.max=1000,algorithm = c("Forgy"))
#dis_C = dist(newdatt_s)^2
dis_C = dist(d)^2
sil_C = silhouette (KM_C$cluster, dis_C)
plot(sil_C)


#children
set.seed(42)
d <- subset(newdatt, select = -c(Gender,Yearly.Income,Marital.Status,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_C <- kmeans(d,2,iter.max=1000,algorithm = c("Forgy"))
dis_C = dist(newdatt)^2
#dis_M = dist(d)^2
sil_C = silhouette (KM_M$cluster, dis_C)
plot(sil_C)

set.seed(42)
d <- subset(newdatt, select = -c(Gender,Yearly.Income,Marital.Status,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_C <- kmeans(d,2,iter.max=1000,algorithm = c("Forgy"))
#dis_C = dist(newdatt_s)^2
dis_C = dist(d)^2
sil_C = silhouette (KM_C$cluster, dis_C)
plot(sil_C)


#children
set.seed(42)
d <- subset(newdatt_s, select = -c(Gender,Yearly.Income,Marital.Status,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer) )
KM_C <- kmeans(d,2,iter.max=1000,algorithm = c("Forgy"))
dis_C = dist(newdatt_s)^2
dis_M = dist(a)^2
sil_C = silhouette (KM_M$cluster, dis_C)
plot(sil_C)

set.seed(42)
d <- subset(newdatt_s, select = -c(Gender,Yearly.Income,Marital.Status,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer) )
KM_C <- kmeans(d,2,iter.max=1000,algorithm = c("Forgy"))
#dis_C = dist(newdatt_s)^2
dis_C = dist(d)^2
sil_C = silhouette (KM_C$cluster, dis_C)
plot(sil_C)


#education. I havent scaled the data and taken 5 clusters.I have included all 17 features.Since it has 5 unique values.
set.seed(42)
e <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_E <- kmeans(e,5,iter.max=1000,algorithm = c("Forgy"))
dis_E = dist(newdatt)^2
#dis_M = dist(a)^2
sil_E = silhouette (KM_E$cluster, dis_E)
plot(sil_E)

set.seed(42)
e <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_E <- kmeans(e,5,iter.max=1000,algorithm = c("Forgy"))
#dis_E = dist(newdatt_s)^2
dis_E = dist(e)^2
sil_E = silhouette (KM_E$cluster, dis_E)
plot(sil_E)


#education. I havent scaled the data and taken 2 clusters.I have included all 17 features
set.seed(42)
e <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_E <- kmeans(e,2,iter.max=1000,algorithm = c("Forgy"))
dis_E = dist(newdatt)^2
#dis_M = dist(a)^2
sil_E = silhouette (KM_E$cluster, dis_E)
plot(sil_E)

set.seed(42)
e <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_E <- kmeans(e,2,iter.max=1000,algorithm = c("Forgy"))
#dis_E = dist(newdatt_s)^2
dis_E = dist(e)^2
sil_E = silhouette (KM_E$cluster, dis_E)
plot(sil_E)


#education
set.seed(42)
e <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_E <- kmeans(e,5,iter.max=1000,algorithm = c("Forgy"))
dis_E = dist(newdatt)^2
#dis_M = dist(a)^2
sil_E = silhouette (KM_E$cluster, dis_E)
plot(sil_E)

set.seed(42)
e <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_E <- kmeans(e,5,iter.max=1000,algorithm = c("Forgy"))
#dis_E = dist(newdatt_s)^2
dis_E = dist(e)^2
sil_E = silhouette (KM_E$cluster, dis_E)
plot(sil_E)


#occupation.I havent scaled the data .Included all 17 features.I have taken 2.
set.seed(42)
f <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_O <- kmeans(f,2,iter.max=1000,algorithm = c("Forgy"))
dis_O = dist(newdatt)^2
#dis_M = dist(a)^2
sil_O = silhouette (KM_O$cluster, dis_O)
plot(sil_O)

set.seed(42)
f <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_O <- kmeans(f,2,iter.max=1000,algorithm = c("Forgy"))
#dis_O = dist(newdatt_s)^2
dis_O = dist(f)^2
sil_O = silhouette (KM_O$cluster, dis_O)
plot(sil_O)



#occupation.I havent scaled the data .Included all 17 features.I have taken 5 clusters as it has 5 unique values.
set.seed(42)
f <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_O <- kmeans(f,5,iter.max=1000,algorithm = c("Forgy"))
dis_O = dist(newdatt)^2
#dis_M = dist(a)^2
sil_O = silhouette (KM_O$cluster, dis_O)
plot(sil_O)

set.seed(42)
f <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_O <- kmeans(f,5,iter.max=1000,algorithm = c("Forgy"))
#dis_O = dist(newdatt_s)^2
dis_O = dist(f)^2
sil_O = silhouette (KM_O$cluster, dis_O)
plot(sil_O)



#occupation
set.seed(42)
f <- subset(newdatt_s, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer) )
KM_O <- kmeans(f,2,iter.max=1000,algorithm = c("Forgy"))
dis_O = dist(newdatt_s)^2
#dis_M = dist(a)^2
sil_O = silhouette (KM_O$cluster, dis_O)
plot(sil_O)

set.seed(42)
f <- subset(newdatt_s, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer) )
KM_O <- kmeans(f,2,iter.max=1000,algorithm = c("Forgy"))
#dis_O = dist(newdatt_s)^2
dis_O = dist(f)^2
sil_O = silhouette (KM_O$cluster, dis_O)
plot(sil_O)



#Home.Owner.I havent scaled the data . Included all 17 features and taken 2 clusters.
set.seed(42)
g <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_H <- kmeans(g,2,iter.max=1000,algorithm = c("Forgy"))
dis_H = dist(newdatt)^2
#dis_M = dist(a)^2
sil_H = silhouette (KM_H$cluster, dis_H)
plot(sil_H)

set.seed(42)
g <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_H <- kmeans(g,2,iter.max=1000,algorithm = c("Forgy"))
#dis_H = dist(newdatt_s)^2
dis_H = dist(g)^2
sil_H = silhouette (KM_H$cluster, dis_H)
plot(sil_H)


#Home.Owner
set.seed(42)
g <- subset(newdatt_s, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Cars,Commute.Distance,Region,Age,BikeBuyer) )
KM_H <- kmeans(g,2,iter.max=1000,algorithm = c("Forgy"))
dis_H = dist(newdatt_s)^2
#dis_M = dist(a)^2
sil_H = silhouette (KM_H$cluster, dis_H)
plot(sil_H)

set.seed(42)
g <- subset(newdatt_s, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Cars,Commute.Distance,Region,Age,BikeBuyer) )
KM_H <- kmeans(g,2,iter.max=1000,algorithm = c("Forgy"))
#dis_H = dist(newdatt_s)^2
dis_H = dist(g)^2
sil_H = silhouette (KM_H$cluster, dis_H)
plot(sil_H)


#CARS.I havent scaled the data and included all 17 features.Tken 5 as cluster since 5 unique values.
set.seed(42)
h <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_C <- kmeans(h,5,iter.max=1000,algorithm = c("Forgy"))
dis_C = dist(newdatt)^2
#dis_M = dist(a)^2
sil_C = silhouette (KM_C$cluster, dis_C)
plot(sil_C)

set.seed(42)
h <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_C <- kmeans(h,5,iter.max=1000,algorithm = c("Forgy"))
#dis_C = dist(newdatt_s)^2
dis_C = dist(h)^2
sil_C = silhouette (KM_C$cluster, dis_C)
plot(sil_C)

#CARS.I havent scaled the data and included all 17 features.Tken 2 as cluster since 5 unique values.
set.seed(42)
h <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_C <- kmeans(h,2,iter.max=1000,algorithm = c("Forgy"))
dis_C = dist(newdatt)^2
#dis_M = dist(a)^2
sil_C = silhouette (KM_C$cluster, dis_C)
plot(sil_C)

set.seed(42)
h <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_C <- kmeans(h,2,iter.max=1000,algorithm = c("Forgy"))
#dis_C = dist(newdatt_s)^2
dis_C = dist(h)^2
sil_C = silhouette (KM_C$cluster, dis_C)
plot(sil_C)

#CARS
set.seed(42)
h <- subset(newdatt_s, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Commute.Distance,Region,Age,BikeBuyer) )
KM_C <- kmeans(h,2,iter.max=1000,algorithm = c("Forgy"))
dis_C = dist(newdatt_s)^2
#dis_M = dist(a)^2
sil_C = silhouette (KM_C$cluster, dis_C)
plot(sil_C)

set.seed(42)
h <- subset(newdatt_s, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Commute.Distance,Region,Age,BikeBuyer) )
KM_C <- kmeans(h,2,iter.max=1000,algorithm = c("Forgy"))
#dis_C = dist(newdatt_s)^2
dis_C = dist(h)^2
sil_C = silhouette (KM_C$cluster, dis_C)
plot(sil_C)

#commute.distance.I have included all 17 features .Taken 5 clusters as 5 unique values.
set.seed(42)
i <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_CD <- kmeans(i,5,iter.max=1000,algorithm = c("Forgy"))
dis_CD = dist(newdatt)^2
#dis_M = dist(a)^2
sil_CD = silhouette (KM_CD$cluster, dis_CD)
plot(sil_CD)

set.seed(42)
i <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_CD <- kmeans(i,5,iter.max=1000,algorithm = c("Forgy"))
#dis_CD = dist(newdatt_s)^2
dis_CD = dist(i)^2
sil_CD = silhouette (KM_CD$cluster, dis_CD)
plot(sil_CD)

#commute.distance.I have included all 17 features .Taken 2 clusters 
set.seed(42)
i <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_CD <- kmeans(i,2,iter.max=1000,algorithm = c("Forgy"))
dis_CD = dist(newdatt)^2
#dis_M = dist(a)^2
sil_CD = silhouette (KM_CD$cluster, dis_CD)
plot(sil_CD)

set.seed(42)
i <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_CD <- kmeans(i,2,iter.max=1000,algorithm = c("Forgy"))
#dis_CD = dist(newdatt_s)^2
dis_CD = dist(i)^2
sil_CD = silhouette (KM_CD$cluster, dis_CD)
plot(sil_CD)

#commute.distance
set.seed(42)
i <- subset(newdatt_s, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Region,Age,BikeBuyer) )
KM_CD <- kmeans(i,2,iter.max=1000,algorithm = c("Forgy"))
dis_CD = dist(newdatt_s)^2
#dis_M = dist(a)^2
sil_CD = silhouette (KM_CD$cluster, dis_CD)
plot(sil_CD)

set.seed(42)
i <- subset(newdatt_s, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Region,Age,BikeBuyer) )
KM_CD <- kmeans(i,2,iter.max=1000,algorithm = c("Forgy"))
#dis_CD = dist(newdatt_s)^2
dis_CD = dist(i)^2
sil_CD = silhouette (KM_CD$cluster, dis_CD)
plot(sil_CD)

#region.I have included all 17 feaures and i havent scaled the daata.Included 3 clusters.
set.seed(42)
j <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_R <- kmeans(j,3,iter.max=1000,algorithm = c("Forgy"))
dis_R = dist(newdatt)^2
#dis_M = dist(a)^2
sil_R = silhouette (KM_R$cluster, dis_R)
plot(sil_R)

set.seed(42)
j <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_R <- kmeans(j,3,iter.max=1000,algorithm = c("Forgy"))
#dis_R = dist(newdatt_s)^2
dis_R = dist(j)^2
sil_R = silhouette (KM_R$cluster, dis_R)
plot(sil_R)

#region.I have included all 17 feaures and i havent scaled the daata.Included 2 clusters.
set.seed(42)
j <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_R <- kmeans(j,2,iter.max=1000,algorithm = c("Forgy"))
dis_R = dist(newdatt)^2
#dis_M = dist(a)^2
sil_R = silhouette (KM_R$cluster, dis_R)
plot(sil_R)

set.seed(42)
j <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_R <- kmeans(j,2,iter.max=1000,algorithm = c("Forgy"))
#dis_R = dist(newdatt_s)^2
dis_R = dist(j)^2
sil_R = silhouette (KM_R$cluster, dis_R)
plot(sil_R)

#region
set.seed(42)
j <- subset(newdatt_s, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Age,BikeBuyer) )
KM_R <- kmeans(j,2,iter.max=1000,algorithm = c("Forgy"))
dis_R = dist(newdatt_s)^2
#dis_M = dist(a)^2
sil_R = silhouette (KM_R$cluster, dis_R)
plot(sil_R)

set.seed(42)
j <- subset(newdatt_s, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Age,BikeBuyer) )
KM_R <- kmeans(j,2,iter.max=1000,algorithm = c("Forgy"))
#dis_R = dist(newdatt_s)^2
dis_R = dist(j)^2
sil_R = silhouette (KM_R$cluster, dis_R)
plot(sil_R)

#age.I have included all 17 features, havent scaled the data .Has 44 unique values lets take 11 ideal clusters
set.seed(42)
k <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_A <- kmeans(k,11,iter.max=1000,algorithm = c("Forgy"))
dis_A = dist(newdatt)^2
#dis_M = dist(a)^2
sil_A = silhouette (KM_A$cluster, dis_A)
plot(sil_A)

set.seed(42)
k <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_A <- kmeans(k,11,iter.max=1000,algorithm = c("Forgy"))
#dis_A = dist(newdatt_s)^2
dis_A = dist(k)^2
sil_A = silhouette (KM_A$cluster, dis_A)
plot(sil_A)


#age.I have included all 17 features, havent scaled the data .Has 44 unique values lets take 2 ideal clusters
set.seed(42)
k <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_A <- kmeans(k,2,iter.max=1000,algorithm = c("Forgy"))
dis_A = dist(newdatt)^2
#dis_M = dist(a)^2
sil_A = silhouette (KM_A$cluster, dis_A)
plot(sil_A)

set.seed(42)
k <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,BikeBuyer,Latitude,Longitude,City,Zip.Code,Country) )
KM_A <- kmeans(k,2,iter.max=1000,algorithm = c("Forgy"))
#dis_A = dist(newdatt_s)^2
dis_A = dist(k)^2
sil_A = silhouette (KM_A$cluster, dis_A)
plot(sil_A)


#age
set.seed(42)
k <- subset(newdatt_s, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,BikeBuyer) )
KM_A <- kmeans(k,2,iter.max=1000,algorithm = c("Forgy"))
dis_A = dist(newdatt_s)^2
#dis_M = dist(a)^2
sil_A = silhouette (KM_A$cluster, dis_A)
plot(sil_A)

set.seed(42)
k <- subset(newdatt_s, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,BikeBuyer) )
KM_A <- kmeans(k,2,iter.max=1000,algorithm = c("Forgy"))
#dis_A = dist(newdatt_s)^2
dis_A = dist(k)^2
sil_A = silhouette (KM_A$cluster, dis_A)
plot(sil_A)


#BikeBuyer.I have included all 17 features , havent scaled the data.2 clusters
set.seed(42)
l <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,Latitude,Longitude,City,Zip.Code,Country) )
KM_BB <- kmeans(l,2,iter.max=1000,algorithm = c("Forgy"))
dis_BB = dist(newdatt)^2
#dis_M = dist(a)^2
sil_BB = silhouette (KM_BB$cluster, dis_BB)
plot(sil_BB)

set.seed(42)
l <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,Latitude,Longitude,City,Zip.Code,Country) )
KM_BB <- kmeans(l,2,iter.max=1000,algorithm = c("Forgy"))
#dis_BB = dist(newdatt_s)^2
dis_BB = dist(l)^2
sil_BB = silhouette (KM_BB$cluster, dis_BB)
plot(sil_BB)

#BikeBuyer
set.seed(42)
l <- subset(newdatt_s, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age) )
KM_BB <- kmeans(l,2,iter.max=1000,algorithm = c("Forgy"))
dis_BB = dist(newdatt_s)^2
#dis_M = dist(a)^2
sil_BB = silhouette (KM_BB$cluster, dis_BB)
plot(sil_BB)

set.seed(42)
l <- subset(newdatt_s, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age) )
KM_BB <- kmeans(l,2,iter.max=1000,algorithm = c("Forgy"))
#dis_BB = dist(newdatt_s)^2
dis_BB = dist(l)^2
sil_BB = silhouette (KM_BB$cluster, dis_BB)
plot(sil_BB)

#Latitude.I have included all 17 features , havent scaled the data.11 clusters
set.seed(42)
m <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Longitude,City,Zip.Code,Country) )
KM_L <- kmeans(m,11,iter.max=1000,algorithm = c("Forgy"))
dis_L = dist(newdatt)^2
#dis_M = dist(a)^2
sil_L = silhouette (KM_L$cluster, dis_L)
plot(sil_L)

set.seed(42)
m <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Longitude,City,Zip.Code,Country) )
KM_L <- kmeans(m,11,iter.max=1000,algorithm = c("Forgy"))
#dis_BB = dist(newdatt_s)^2
dis_L = dist(m)^2
sil_L = silhouette (KM_L$cluster, dis_L)
plot(sil_L)

#Latitude.I have included all 17 features , havent scaled the data.11 clusters
set.seed(42)
m <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Longitude,City,Zip.Code,Country) )
KM_L <- kmeans(m,2,iter.max=1000,algorithm = c("Forgy"))
dis_L = dist(newdatt)^2
#dis_M = dist(a)^2
sil_L = silhouette (KM_L$cluster, dis_L)
plot(sil_L)

set.seed(42)
m <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Longitude,City,Zip.Code,Country) )
KM_L <- kmeans(m,2,iter.max=1000,algorithm = c("Forgy"))
#dis_BB = dist(newdatt_s)^2
dis_L = dist(m)^2
sil_L = silhouette (KM_L$cluster, dis_L)
plot(sil_L)

#Longitude.I have included all 17 features , havent scaled the data.11 clusters
set.seed(42)
n <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,City,Zip.Code,Country) )
KM_Lo <- kmeans(n,11,iter.max=1000,algorithm = c("Forgy"))
dis_Lo = dist(newdatt)^2
#dis_M = dist(a)^2
sil_Lo = silhouette (KM_Lo$cluster, dis_Lo)
plot(sil_Lo)

set.seed(42)
n <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,City,Zip.Code,Country) )
KM_Lo <- kmeans(n,11,iter.max=1000,algorithm = c("Forgy"))
#dis_BB = dist(newdatt_s)^2
dis_Lo = dist(n)^2
sil_Lo = silhouette (KM_Lo$cluster, dis_Lo)
plot(sil_Lo)

#Longitude.I have included all 17 features , havent scaled the data.2 clusters
set.seed(42)
n <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,City,Zip.Code,Country) )
KM_Lo <- kmeans(n,2,iter.max=1000,algorithm = c("Forgy"))
dis_Lo = dist(newdatt)^2
#dis_M = dist(a)^2
sil_Lo = silhouette (KM_Lo$cluster, dis_Lo)
plot(sil_Lo)

set.seed(42)
n <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,City,Zip.Code,Country) )
KM_Lo <- kmeans(n,2,iter.max=1000,algorithm = c("Forgy"))
#dis_BB = dist(newdatt_s)^2
dis_Lo = dist(n)^2
sil_Lo = silhouette (KM_Lo$cluster, dis_Lo)
plot(sil_Lo)

#City.I have included all 17 features , havent scaled the data.11 clusters
set.seed(42)
o <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,Zip.Code,Country) )
KM_CY <- kmeans(o,11,iter.max=1000,algorithm = c("Forgy"))
dis_CY = dist(newdatt)^2
#dis_M = dist(a)^2
sil_CY = silhouette (KM_CY$cluster, dis_CY)
plot(sil_CY)

set.seed(42)
o <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,Zip.Code,Country) )
KM_CY <- kmeans(o,11,iter.max=1000,algorithm = c("Forgy"))
#dis_BB = dist(newdatt_s)^2
dis_CY = dist(o)^2
sil_CY = silhouette (KM_CY$cluster, dis_CY)
plot(sil_CY)

#City.I have included all 17 features , havent scaled the data.11 clusters
set.seed(42)
o <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,Zip.Code,Country) )
KM_CY <- kmeans(o,2,iter.max=1000,algorithm = c("Forgy"))
dis_CY = dist(newdatt)^2
#dis_M = dist(a)^2
sil_CY = silhouette (KM_CY$cluster, dis_CY)
plot(sil_CY)

set.seed(42)
o <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,Zip.Code,Country) )
KM_CY <- kmeans(o,2,iter.max=1000,algorithm = c("Forgy"))
#dis_BB = dist(newdatt_s)^2
dis_CY = dist(o)^2
sil_CY = silhouette (KM_CY$cluster, dis_CY)
plot(sil_CY)

#Zip.Code.I have included all 17 features , havent scaled the data.2 clusters
set.seed(42)
p <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Country) )
KM_Z <- kmeans(p,2,iter.max=1000,algorithm = c("Forgy"))
dis_Z = dist(newdatt)^2
#dis_M = dist(a)^2
sil_Z = silhouette (KM_Z$cluster, dis_Z)
plot(sil_Z)

set.seed(42)
p <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Country) )
KM_Z <- kmeans(p,2,iter.max=1000,algorithm = c("Forgy"))
#dis_BB = dist(newdatt_s)^2
dis_Z = dist(p)^2
sil_Z = silhouette (KM_Z$cluster, dis_Z)
plot(sil_Z)

#Zip.Code.I have included all 17 features , havent scaled the data.11 clusters
set.seed(42)
p <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Country) )
KM_Z <- kmeans(p,11,iter.max=1000,algorithm = c("Forgy"))
dis_Z = dist(newdatt)^2
#dis_M = dist(a)^2
sil_Z = silhouette (KM_Z$cluster, dis_Z)
plot(sil_Z)

set.seed(42)
p <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Country) )
KM_Z <- kmeans(p,11,iter.max=1000,algorithm = c("Forgy"))
#dis_BB = dist(newdatt_s)^2
dis_Z = dist(p)^2
sil_Z = silhouette (KM_Z$cluster, dis_Z)
plot(sil_Z)

#Country.I have included all 17 features , havent scaled the data.11 clusters
set.seed(42)
q <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code) )
KM_CU <- kmeans(q,2,iter.max=1000,algorithm = c("Forgy"))
dis_CU = dist(newdatt)^2
#dis_M = dist(a)^2
sil_CU = silhouette (KM_CU$cluster, dis_CU)
plot(sil_CU)

set.seed(42)
q <- subset(newdatt, select = -c(Marital.Status,Gender,Yearly.Income,Children,Education,Occupation,Home.Owner,Cars,Commute.Distance,Region,Age,BikeBuyer,Latitude,Longitude,City,Zip.Code) )
KM_CU <- kmeans(q,2,iter.max=1000,algorithm = c("Forgy"))
#dis_BB = dist(newdatt_s)^2
dis_CU = dist(q)^2
sil_CU = silhouette (KM_CU$cluster, dis_CU)
plot(sil_CU)

# Getting info abut cluster size
KM$size

#	Centers	of	clusters	three	clusters	by	variable
KM$centers

install.packages("ROSE")
library(ROSE)

install.packages('rpart')
library(rpart)

?accuracy.meas

?ovun.sample

str(newdatt)

install.packages("DMwR")

library(DMwR)

str(newdatt)

# class distribution
cbind(freq=table(newdatt$BikeBuyer), percentage=prop.table(table(newdatt$BikeBuyer))*100)

newData <- SMOTE(BikeBuyer ~ ., newdatt, perc.over = 435,perc.under=100)
#table(newData$BikeBuyer)
str(newData)

# class distribution
cbind(freq=table(newData$BikeBuyer), percentage=prop.table(table(newData$BikeBuyer))*100)

newdatt <- newData
str(newdatt)
cbind(freq=table(newdatt$BikeBuyer), percentage=prop.table(table(newdatt$BikeBuyer))*100)

#over sampling
data_balanced_over <- ovun.sample(BikeBuyer ~ ., data = newdatt, method = "over",N = 6505)$data
table(data_balanced_over$BikeBuyer)
cbind(freq=table(data_balanced_over$BikeBuyer), percentage=prop.table(table(data_balanced_over$BikeBuyer))*100)

str(data_balanced_over)
newdatt_over <- data_balanced_over
str(newdatt_over)
head(newdatt_over)

data_balanced_under <- ovun.sample(BikeBuyer ~ ., data = newdatt, method = "under", N = 2492, seed = 1)$data
table(data_balanced_under$BikeBuyer)

newdatt_under <- data_balanced_under
str(newdatt_under)

data_balanced_both <- ovun.sample(BikeBuyer ~ ., data = newdatt, method = "both", p=0.4,N=4597, seed = 1)$data
table(data_balanced_both$BikeBuyer)
cbind(freq=table(data_balanced_both$BikeBuyer), percentage=prop.table(table(data_balanced_both$BikeBuyer))*100)

newdatt_both <- data_balanced_both
str(newdatt_both)

#doesnt work if you scale your data.
data.rose <- ROSE(BikeBuyer ~ ., data = newdatt, seed = 1)$data
table(data.rose$BikeBuyer)

newdatt_rose <- data.rose
str(newdatt_rose)

# Split out validation dataset
# create a list of 80% of the rows in the original dataset we can use for trainingset.seed(123)
validationIndex <- createDataPartition(newdatt$BikeBuyer, p=0.75, list=FALSE)
# select 20% of the data for validation
validation <- newdatt[-validationIndex,]
# use the remaining 80% of data to training and testing the models
newdat <- newdatt[validationIndex,]
str(newdat)
str(newdatt)

# 10-fold cross validation with 4 repeats
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"

install.packages('e1071', dependencies=TRUE)

install.packages('klaR', dependencies=TRUE)

install.packages('kernlab')

str(newdatt_rose)

str(newdatt)

str(newdatt_over)

str(newdat)

#newl
#This one is with totally new features , expecting to get better results
#in this one I have done splitting after data preprocessing and my data is "newdat"
#categorical only newdatt with scaling
#LG
set.seed(35)
fit.glm <- train(BikeBuyer~., data=newdat, method="glm", metric=metric, trControl=trainControl)
# GLMNET
set.seed(35)
fit.glmnet <- train(BikeBuyer~., data=newdat, method="glmnet", metric=metric,
trControl=trainControl)
# KNN
set.seed(35)
fit.knn <- train(BikeBuyer~., data=newdat, method="knn", metric=metric, trControl=trainControl)
# CART
set.seed(35)
fit.cart <- train(BikeBuyer~., data=newdat, method="rpart", metric=metric,
trControl=trainControl)
# SVM
set.seed(35)
fit.svm <- train(BikeBuyer~., data=newdat, method="svmRadial", metric=metric,
trControl=trainControl)
# Compare algorithms
results <- resamples(list(LG=fit.glm,GLMNET=fit.glmnet, KNN=fit.knn,CART=fit.cart, SVM=fit.svm))
summary(results)

a <- scale(newdat$Yearly.Income, center = TRUE, scale = TRUE)
str(a)
newdat$Yearly.Income <- a
str(newdat)

b <- scale(newdat$Age, center = TRUE, scale = TRUE)
str(b)
newdat$Age <- b
str(newdat)

install.packages("reshape")
library(reshape)
#reshape package [0-1] scaling
performScaling <- TRUE  # Turn it on/off for experimentation.

if (performScaling) {

    # Loop over each column.
    for (colName in names(newdat)) {

        # Check if the column contains numeric data.
        if(class(newdat[,colName]) == 'integer' | class(newdatt[,colName]) == 'numeric') {

            # Scale this column (scale() function applies z-scaling).
            newdat[,colName] <- rescaler(newdat[,colName],"range")
        }
    }
}


str(newdat)

#newl after data preprocessing.
#This one is with totally new features , expecting to get better results
#in this one I have done splitting after data preprocessing and my data is "newdat"

#LG
set.seed(35)
fit.glm <- train(BikeBuyer~., data=newdat, method="glm", metric=metric, trControl=trainControl)
# GLMNET
set.seed(35)
fit.glmnet <- train(BikeBuyer~., data=newdat, method="glmnet", metric=metric,trControl=trainControl)
# KNN
set.seed(35)
fit.knn <- train(BikeBuyer~., data=newdat, method="knn", metric=metric, trControl=trainControl)
# CART
set.seed(35)
fit.cart <- train(BikeBuyer~., data=newdat, method="rpart", metric=metric,trControl=trainControl)
# SVM
set.seed(35)
fit.svm <- train(BikeBuyer~., data=newdat, method="svmRadial", metric=metric,trControl=trainControl)
# Compare algorithms
results <- resamples(list(LG=fit.glm,GLMNET=fit.glmnet, KNN=fit.knn,CART=fit.cart, SVM=fit.svm))
summary(results)

#To tune SVM we are taking BikeBuyer at end.
t <- newdat$BikeBuyer
BikeBuyerN <- t
str(t)
summary(t)
newdattb <- subset(newdat, select = -c(BikeBuyer) )
str(newdattb)
newdattb <- cbind(newdattb,BikeBuyerN)
str(newdattb)

#Tuning SVM
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
set.seed(121)
grid <- expand.grid(.sigma=c(0.025, 0.05, 0.1, 0.15), .C=seq(1, 12, by=1))
fit.svm <- train(BikeBuyerN~., data=newdattb, method="svmRadial", metric=metric, tuneGrid=grid, trControl=trainControl)
print(fit.svm)
plot(fit.svm)

#This one is with totally new features , expecting to get better results
#in this one I have done splitting after data preprocessing and my data is "newdat"
#categorical only newdatt with scaling
#LG
set.seed(35)
fit.glm <- train(BikeBuyer~., data=newdatt_over, method="glm", metric=metric, trControl=trainControl)
# GLMNET
set.seed(35)
fit.glmnet <- train(BikeBuyer~., data=newdatt_over, method="glmnet", metric=metric,
trControl=trainControl)
# KNN
set.seed(35)
fit.knn <- train(BikeBuyer~., data=newdatt_over, method="knn", metric=metric, trControl=trainControl)
# CART
set.seed(35)
fit.cart <- train(BikeBuyer~., data=newdatt_over, method="rpart", metric=metric,
trControl=trainControl)
# SVM
set.seed(35)
fit.svm <- train(BikeBuyer~., data=newdatt_over, method="svmRadial", metric=metric,
trControl=trainControl)
# Compare algorithms
results <- resamples(list(LG=fit.glm,GLMNET=fit.glmnet, KNN=fit.knn,CART=fit.cart, SVM=fit.svm))
summary(results)

#in this one I have done splitting after data preprocessing and my data is "newdat"
#categorical only newdatt with scaling
# LG
set.seed(35)
fit.glm <- train(BikeBuyer~., data=newdat, method="glm", metric=metric, trControl=trainControl)
# GLMNET
set.seed(35)
fit.glmnet <- train(BikeBuyer~., data=newdat, method="glmnet", metric=metric,
trControl=trainControl)
# KNN
set.seed(35)
fit.knn <- train(BikeBuyer~., data=newdat, method="knn", metric=metric, trControl=trainControl)
# CART
set.seed(35)
fit.cart <- train(BikeBuyer~., data=newdat, method="rpart", metric=metric,
trControl=trainControl)
# SVM
set.seed(35)
fit.svm <- train(BikeBuyer~., data=newdat, method="svmRadial", metric=metric,
trControl=trainControl)
# Compare algorithms
results <- resamples(list(LG=fit.glm,GLMNET=fit.glmnet, KNN=fit.knn,
CART=fit.cart, SVM=fit.svm))
summary(results)

This Thing has performed well.This implies I have handled y data well.

#categorical only newdatt with scaling
# LG
set.seed(7)
fit.glm <- train(BikeBuyer~., data=newdatt, method="glm", metric=metric, trControl=trainControl)
# GLMNET
set.seed(7)
fit.glmnet <- train(BikeBuyer~., data=newdatt, method="glmnet", metric=metric,
trControl=trainControl)
# KNN
set.seed(7)
fit.knn <- train(BikeBuyer~., data=newdatt, method="knn", metric=metric, trControl=trainControl)
# CART
set.seed(7)
fit.cart <- train(BikeBuyer~., data=newdatt, method="rpart", metric=metric,
trControl=trainControl)
# SVM
set.seed(7)
fit.svm <- train(BikeBuyer~., data=newdatt, method="svmRadial", metric=metric,
trControl=trainControl)
# Compare algorithms
results <- resamples(list(LG=fit.glm,GLMNET=fit.glmnet, KNN=fit.knn,
CART=fit.cart, SVM=fit.svm))
summary(results)

#categorical oversampling with scaling
# LG
set.seed(7)
fit.glm <- train(BikeBuyer~., data=newdatt_over, method="glm", metric=metric, trControl=trainControl)
# GLMNET
set.seed(7)
fit.glmnet <- train(BikeBuyer~., data=newdatt_over, method="glmnet", metric=metric,
trControl=trainControl)
# KNN
set.seed(7)
fit.knn <- train(BikeBuyer~., data=newdatt_over, method="knn", metric=metric, trControl=trainControl)
# CART
set.seed(7)
fit.cart <- train(BikeBuyer~., data=newdatt_over, method="rpart", metric=metric,
trControl=trainControl)
# SVM
set.seed(7)
fit.svm <- train(BikeBuyer~., data=newdatt_over, method="svmRadial", metric=metric,
trControl=trainControl)
# Compare algorithms
results <- resamples(list(LG=fit.glm,GLMNET=fit.glmnet, KNN=fit.knn,
CART=fit.cart, SVM=fit.svm))
summary(results)

#categorical undersampling with scaling
# LG
set.seed(7)
fit.glm <- train(BikeBuyer~., data=newdatt_under, method="glm", metric=metric, trControl=trainControl)
# GLMNET
set.seed(7)
fit.glmnet <- train(BikeBuyer~., data=newdatt_under, method="glmnet", metric=metric,
trControl=trainControl)
# KNN
set.seed(7)
fit.knn <- train(BikeBuyer~., data=newdatt_under, method="knn", metric=metric, trControl=trainControl)
# CART
set.seed(7)
fit.cart <- train(BikeBuyer~., data=newdatt_under, method="rpart", metric=metric,
trControl=trainControl)
# SVM
set.seed(7)
fit.svm <- train(BikeBuyer~., data=newdatt_under, method="svmRadial", metric=metric,
trControl=trainControl)
# Compare algorithms
results <- resamples(list(LG=fit.glm,GLMNET=fit.glmnet, KNN=fit.knn,
CART=fit.cart, SVM=fit.svm))
summary(results)

#categorical bothsampling with scaling
# LG
set.seed(7)
fit.glm <- train(BikeBuyer~., data=newdatt_both, method="glm", metric=metric, trControl=trainControl)
# GLMNET
set.seed(7)
fit.glmnet <- train(BikeBuyer~., data=newdatt_both, method="glmnet", metric=metric,
trControl=trainControl)
# KNN
set.seed(7)
fit.knn <- train(BikeBuyer~., data=newdatt_both, method="knn", metric=metric, trControl=trainControl)
# CART
set.seed(7)
fit.cart <- train(BikeBuyer~., data=newdatt_both, method="rpart", metric=metric,
trControl=trainControl)
# SVM
set.seed(7)
fit.svm <- train(BikeBuyer~., data=newdatt_both, method="svmRadial", metric=metric,
trControl=trainControl)
# Compare algorithms
results <- resamples(list(LG=fit.glm,GLMNET=fit.glmnet, KNN=fit.knn,
CART=fit.cart, SVM=fit.svm))
summary(results)

#categorical but no sampling and no  scaling
# LG
set.seed(7)
fit.glm <- train(BikeBuyer~., data=newdatt, method="glm", metric=metric, trControl=trainControl)
# GLMNET
set.seed(7)
fit.glmnet <- train(BikeBuyer~., data=newdatt, method="glmnet", metric=metric,
trControl=trainControl)
# KNN
set.seed(7)
fit.knn <- train(BikeBuyer~., data=newdatt, method="knn", metric=metric, trControl=trainControl)
# CART
set.seed(7)
fit.cart <- train(BikeBuyer~., data=newdatt, method="rpart", metric=metric,
trControl=trainControl)
# SVM
set.seed(7)
fit.svm <- train(BikeBuyer~., data=newdatt, method="svmRadial", metric=metric,
trControl=trainControl)
# Compare algorithms
results <- resamples(list(LG=fit.glm,GLMNET=fit.glmnet, KNN=fit.knn,
CART=fit.cart, SVM=fit.svm))
summary(results)

#categorical but no sampling and no  scaling and outliers removed
# LG
set.seed(7)
fit.glm <- train(BikeBuyer~., data=newdatto, method="glm", metric=metric, trControl=trainControl)
# GLMNET
set.seed(7)
fit.glmnet <- train(BikeBuyer~., data=newdatto, method="glmnet", metric=metric,
trControl=trainControl)
# KNN
set.seed(7)
fit.knn <- train(BikeBuyer~., data=newdatto, method="knn", metric=metric, trControl=trainControl)
# CART
set.seed(7)
fit.cart <- train(BikeBuyer~., data=newdatto, method="rpart", metric=metric,
trControl=trainControl)
# SVM
set.seed(7)
fit.svm <- train(BikeBuyer~., data=newdatto, method="svmRadial", metric=metric,
trControl=trainControl)
# Compare algorithms
results <- resamples(list(LG=fit.glm,GLMNET=fit.glmnet, KNN=fit.knn,
CART=fit.cart, SVM=fit.svm))
summary(results)

#categorical oversampling without scaling
# LG
set.seed(7)
fit.glm <- train(BikeBuyer~., data=newdatt_over, method="glm", metric=metric, trControl=trainControl)
# GLMNET
set.seed(7)
fit.glmnet <- train(BikeBuyer~., data=newdatt_over, method="glmnet", metric=metric,
trControl=trainControl)
# KNN
set.seed(7)
fit.knn <- train(BikeBuyer~., data=newdatt_over, method="knn", metric=metric, trControl=trainControl)
# CART
set.seed(7)
fit.cart <- train(BikeBuyer~., data=newdatt_over, method="rpart", metric=metric,
trControl=trainControl)
# SVM
set.seed(7)
fit.svm <- train(BikeBuyer~., data=newdatt_over, method="svmRadial", metric=metric,
trControl=trainControl)
# Compare algorithms
results <- resamples(list(LG=fit.glm,GLMNET=fit.glmnet, KNN=fit.knn,
CART=fit.cart, SVM=fit.svm))
summary(results)
dotplot(results)

#categorical undersampling without scaling
# LG
set.seed(7)
fit.glm <- train(BikeBuyer~., data=newdatt_under, method="glm", metric=metric, trControl=trainControl)
# GLMNET
set.seed(7)
fit.glmnet <- train(BikeBuyer~., data=newdatt_under, method="glmnet", metric=metric,
trControl=trainControl)
# KNN
set.seed(7)
fit.knn <- train(BikeBuyer~., data=newdatt_under, method="knn", metric=metric, trControl=trainControl)
# CART
set.seed(7)
fit.cart <- train(BikeBuyer~., data=newdatt_under, method="rpart", metric=metric,
trControl=trainControl)
# SVM
set.seed(7)
fit.svm <- train(BikeBuyer~., data=newdatt_under, method="svmRadial", metric=metric,
trControl=trainControl)
# Compare algorithms
results <- resamples(list(LG=fit.glm,GLMNET=fit.glmnet, KNN=fit.knn,
CART=fit.cart, SVM=fit.svm))
summary(results)

#categorical bothsampling without scaling
# LG
set.seed(7)
fit.glm <- train(BikeBuyer~., data=newdatt_both, method="glm", metric=metric, trControl=trainControl)
# GLMNET
set.seed(7)
fit.glmnet <- train(BikeBuyer~., data=newdatt_both, method="glmnet", metric=metric,
trControl=trainControl)
# KNN
set.seed(7)
fit.knn <- train(BikeBuyer~., data=newdatt_both, method="knn", metric=metric, trControl=trainControl)
# CART
set.seed(7)
fit.cart <- train(BikeBuyer~., data=newdatt_both, method="rpart", metric=metric,
trControl=trainControl)
# SVM
set.seed(7)
fit.svm <- train(BikeBuyer~., data=newdatt_both, method="svmRadial", metric=metric,
trControl=trainControl)
# Compare algorithms
results <- resamples(list(LG=fit.glm,GLMNET=fit.glmnet, KNN=fit.knn,
CART=fit.cart, SVM=fit.svm))
summary(results)

#categorical rose sampling without scaling
# LG
set.seed(7)
fit.glm <- train(BikeBuyer~., data=newdatt_rose, method="glm", metric=metric, trControl=trainControl)
# GLMNET
set.seed(7)
fit.glmnet <- train(BikeBuyer~., data=newdatt_rose, method="glmnet", metric=metric,
trControl=trainControl)
# KNN
set.seed(7)
fit.knn <- train(BikeBuyer~., data=newdatt_rose, method="knn", metric=metric, trControl=trainControl)
# CART
set.seed(7)
fit.cart <- train(BikeBuyer~., data=newdatt_rose, method="rpart", metric=metric,
trControl=trainControl)
# SVM
set.seed(7)
fit.svm <- train(BikeBuyer~., data=newdatt_rose, method="svmRadial", metric=metric,
trControl=trainControl)
# Compare algorithms
results <- resamples(list(LG=fit.glm,GLMNET=fit.glmnet, KNN=fit.knn,
CART=fit.cart, SVM=fit.svm))
summary(results)

# 10-fold cross validation with 3 repeats
#Stop working with total numeric data . You have failed again and againg . No more wasting of time .
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "RMSE"
# LG
set.seed(35)
fit.glm <- train(BikeBuyer~., data=newdatt, method="glm", metric=metric, preProc=c("BoxCox"),
trControl=trainControl)
# LDA
set.seed(35)
fit.lda <- train(BikeBuyer~., data=newdatt, method="lda", metric=metric, preProc=c("BoxCox"),
trControl=trainControl)
# GLMNET
set.seed(35)
fit.glmnet <- train(BikeBuyer~., data=newdatt, method="glmnet", metric=metric,
preProc=c("BoxCox"), trControl=trainControl)
# KNN
set.seed(35)
fit.knn <- train(BikeBuyer~., data=newdatt, method="knn", metric=metric, preProc=c("BoxCox"),
trControl=trainControl)
# CART
set.seed(35)
fit.cart <- train(BikeBuyer~., data=newdatt, method="rpart", metric=metric,
preProc=c("BoxCox"), trControl=trainControl)
# SVM
set.seed(35)
fit.svm <- train(BikeBuyer~., data=newdatt, method="svmRadial", metric=metric,
preProc=c("BoxCox"), trControl=trainControl)
# Compare algorithms
transformResults <- resamples(list(LG=fit.glm, LDA=fit.lda, GLMNET=fit.glmnet, KNN=fit.knn,
CART=fit.cart, SVM=fit.svm))
summary(transformResults)

# 10-fold cross validation with 3 repeats
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
set.seed(7)
grid <- expand.grid(.sigma=c(0.025, 0.05, 0.1, 0.15), .C=seq(1, 11, by=1))
fit.svm <- train(BikeBuyer~., data=newdat, method="svmRadial", metric=metric, tuneGrid=grid,trControl=trainControl)
print(fit.svm)

# 10-fold cross validation with 3 repeats
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
set.seed(7)
grid <- expand.grid(.sigma=c(0.025, 0.05, 0.1, 0.15), .C=seq(1, 11, by=1))
fit.svm <- train(BikeBuyer~., data=newdatt, method="svmRadial", metric=metric, tuneGrid=grid,trControl=trainControl)
print(fit.svm)

plot(fit.svm)

# 10-fold cross validation with 3 repeats
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
set.seed(7)
grid <- expand.grid(.sigma=c(0.025, 0.05, 0.1, 0.15), .C=seq(1, 11, by=1))
fit.svm <- train(BikeBuyer~., data=newdatt_over, method="svmRadial", metric=metric, tuneGrid=grid,trControl=trainControl)
print(fit.svm)
