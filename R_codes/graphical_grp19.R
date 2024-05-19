# Set the working directory to where your CSV file is located
setwd("D:/Campus/Level 3/Sem 1/IS 3001/Grp.Proj/Project")
# Read the CSV file
mydata <- read.csv("supermarket_sales.csv")
data1=data.frame(mydata)
#View(Data)
t1=table(mydata$Product.line)
prod=names(t1)
prod
table(mydata$City)

#barchart
barplot(table(mydata$Branch), names.arg=c("A","B","C"), col="blue",main="Number of branches ",ylab="count")

barplot(table(mydata$Payment),main="Payment method use",
        names.arg =c("Ewallet","Cash","Credit card"),
        ylab = "Proportion" )

#boxplot
pop_des<-svydes()
boxplot(Rating~Product.line,data1,all.outliers = TRUE)

#scatterplot
plot(mydata$Total,mydata$rating,
     main="Scatterplot of ratings and total bill amount",
     xlab="total amount",ylab="ratings")
plot(mydata$Total,mydata$Tax.5.,
     main="Scatterplot of tax and total bill amount",
     xlab="total amount",ylab="tax")

plot(mydata$Quantity,mydata$Total,
     main="Scatterplot of tax and total bill amount",
     xlab="Quantity",ylab="Total")

#histogram
hist(x = mydata$Total,prob=F,main="Histogram of total ",xlab="
total")