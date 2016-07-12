require(xlsx)
require(ROCR)
require(DAAG)
res <- read.xlsx("/home/aditya/Downloads/data.xlsx", 1)
names(res)
  
summary(res)

#removing the unnecessary columns 
cols.dont.want <- c("NA", "NA..1","NA.") # if you want to remove multiple columns
res <- res[, ! names(res) %in% cols.dont.want, drop = F]


#changing the na values in the new customer field to 2
res$new_customer[which(is.na(res$new_customer))] <- 2  

#changing the null values in platform field to Unknown 
res$platform[which(res$platform == "")] <- "Unknown"  
res$platform <- factor(res$platform)

#assuming that a person needs to login before buying anything,we will replace na values in
#gross sales with 0 where new_customer is 2
res$gross_sales[which(res$new_customer==2 & is.na(res$gross_sales))]<-0

#replacing the rest of the gross sales by the average of the gross sales
res$gross_sales[is.na(res$gross_sales)] <-mean(res$gross_sales[!is.na(res$gross_sales)])

#creating a date column
res$date <- res$day

#inserting the days of the week and the month in the data
res$day <- weekdays(as.Date(res$date, "%y-%m-%d"))
res$day <- as.factor(res$day)
res$day <- factor(res$day)

res$month <- months(as.Date(res$date, "%y-%m-%d"))
res$month <- as.factor(res$month)
res$month <- factor(res$month)

#adding an attribute that determines if a sale was made or not
res$sales=0
res$sales[res$gross_sales>0]=1
res$sales=as.factor(res$sales)


# 75% of the data is training data
set.seed(2)
smp_size <- floor(0.75 * nrow(res))
train_ind=sample(seq_len(nrow(res)), size = smp_size)
train <- res[train_ind, ]
test <- res[-train_ind, ]

library(ggplot2)
library(reshape2)
library(gridExtra)



# Aggregaring data based on average visits, bounces, addtocart and orders group by user
getAggregate <- function(funcName){
  activity <- aggregate(list(res$visits, res$bounces, res$add_to_cart, res$orders), list(res$new_customer), FUN=funcName)
  names(activity) <- c("Customers","Visits","Bounces","Add to Cart", "Order")
  activity <- activity[,-1]
}

activity <- getAggregate("mean")
Customers=c("New_User","Returning User","Neither")     # create list of names
data=data.frame(cbind(activity),Customers)   # combine them into a data frame
data.m <- melt(data, id.vars='Customers')

# Plotting Code
plot1 <- ggplot(data.m, aes(Customers, value)) + ggtitle("Customer Visit Pattern[On Average]") +  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +ylab("Average Count") 

activity <- getAggregate("sum")
data=data.frame(cbind(activity),Customers)   # combine them into a data frame
data.m <- melt(data, id.vars='Customers')

plot2 <- ggplot(data.m, aes(Customers, value)) + ggtitle("Customer Visit Pattern [Total]") +geom_bar(aes(fill = variable), position = "dodge", stat="identity")  + ggtitle("Customer Visit Pattern[Log Scale]") + scale_y_log10("Total Count")

grid.arrange(plot1, plot2, nrow = 2)


#analyzing data based on days and months

activity <- aggregate(list(res$visits, res$bounces, res$add_to_cart, res$orders), list(res$month), FUN="mean")
names(activity) <- c("Month","Visits","Bounces","Add to Cart", "Order")
activity <- activity[,-1]

Months=unique(res$month)     # create list of names
data=data.frame(cbind(activity),Months)   # combine them into a data frame
data.m <- melt(data, id.vars='Months')
data.m$Months <- factor(data.m$Months, levels=c("January", "February", "June","July","August","September","October","November","December"))

plot3 <- ggplot(data.m, aes(x = Months, y = value)) + geom_line(size=1, aes(group=variable,color=factor(variable)))+geom_point(color="blue") +  ggtitle("Monthly Activity Trend") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylab("Average Count")

activity <- aggregate(list(res$visits, res$bounces, res$add_to_cart, res$orders), list(res$day), FUN="mean")
names(activity) <- c("Days","Visits","Bounces","Add to Cart", "Order")
activity <- activity[,-1]

Days=unique(res$day)     # create list of names
data=data.frame(cbind(activity),Days)   # combine them into a data frame
data.m <- melt(data, id.vars='Days')
data.m$Days <- factor(data.m$Days, levels=c("Monday", "Tuesday","Wednesday" ,"Thursday","Friday","Saturday","Sunday"))

plot4 <- ggplot(data.m, aes(x = Days, y = value)) + geom_line(size=1, aes(group=variable,color=factor(variable)))+geom_point(color="blue") +  ggtitle("Day-Wise Activity Trend") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylab("Average Count")

grid.arrange(plot3, plot4, nrow = 2)


#checking if there's a trend based on the platform and the  particular site visited
qplot(factor(platform), data=res, geom="bar", fill=factor(site))  + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +xlab("Platform") +ylab("Visitor Count") + ggtitle("Platform Vs Site Share")


clustData <- subset(res, select=c("new_customer","orders","bounces","add_to_cart","product_page_views"))

plot(clustData[,2:ncol(clustData)], pch=20, col=clustData$new_customer+1, main="Scatter Matrix on User Activities")


#A very important aspect of this data set is to check if any sale was made or not,on any
#website,applying logistic regression on the data to check the result

train=na.omit(train)
sale_model=glm(formula=sales~.,
               data=train,family = binomial())

sale_model=step(sale_model,direction = 'both')

test=na.omit(test)
p <-predict(sale_model,newdata=test[,-15],type="response")
p<-ifelse(p>0.5,1,0)
pr <- prediction(p,test$sales)
prf <- performance(pr, measure = "auc")
plot(prf)
summary(sale_model)

#using the stepwise forward and backward selection the attributes that will included in the
#model will be product_page_view,search_page_views,bounces,add_to_cart and platform

sale_model_new=glm(formula=gross_sales~
                 day+new_customer+add_to_cart+platform+visits+
                 distinct_sessions+month+search_page_views+product_page_views+bounces+sales,
               data=train)

sale_model_new=step(sale_model_new,direction = 'both')
