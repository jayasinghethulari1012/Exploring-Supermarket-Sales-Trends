#install.packages("survey")
#install.packages("sampler")
#install.packages("sampling")

#import libraries
library("survey")
library("sampler")
library("sampling")

#set working directory
setwd("E:/R stat")
mydata<-read.csv("supermarket_sales.csv")

#calculate the necessary sample size
n=rsampcalc(nrow(mydata),e=3,ci=95) 
n
#Calculate the size of each stratum
sam_sz=ssampcalc(df=mydata,n=n,strata = Product.line)
sam_sz
#drawing sample 1 -------------------------------------------------------
set.seed(15548)
str_sam1=ssamp(df=mydata,n=n,strata = Product.line)
data1=data.frame(str_sam1)
data3=data.frame(sam_sz[,1],sam_sz[,3])

#include the weights
strat_sample1=merge(data3,data1,by="Product.line")
#View(strat_sample)

#estimating in stratified sample 1 ........................
attach(strat_sample1)
strat1_des=svydesign(id=~1,strata=~Product.line,weights = ~wt,data=strat_sample1)

#sample totals
#total
svytotal(~Total,strat1_des,deff=TRUE)
#total gross income
svytotal(~gross_income,strat1_des,deff=TRUE)
#total cost
svytotal(~cogs,strat1_des,deff=TRUE)
#total tax
svytotal(~Tax.5.,strat1_des,deff=TRUE)

#sample means
#total
svymean(~Total,strat1_des)
#rating
svymean(~Rating,strat1_des)
#gross income
svymean(~gross_income,strat1_des)

#sample Proportion
#customer type
svymean(~C_type,strat1_des)
#pay method
svymean(~Payment,strat1_des)
#Gender
svymean(~Gender,strat1_des)
#branch
svymean(~Branch,strat1_des)

#>>>>>>>>>>>sub groups
#mean rating by strata
svyby(~Rating,by=~Product.line,strat1_des,svymean)
#mean total by strata
svyby(~Total,by=~Product.line,strat1_des,svymean)

#total tax by strat
svyby(~Tax.5.,by=~Product.line,strat1_des,svytotal)
#---------------------------
#ratio estimation
svyratio(~Total,~Tax.5.,design =strat1_des)

detach(strat_sample1)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#drawing sample 2 in stratified sampling
set.seed(15540)
str_sam2=ssamp(df=mydata,n=n,strata = Product.line)
data1=data.frame(str_sam2)
data3=data.frame(sam_sz[,1],sam_sz[,3])

#include the weights
strat_sample2=merge(data3,data1,by="Product.line")
#View(strat_sample)

#estimating in stratified sample 1 ........................
attach(strat_sample2)
strat2_des=svydesign(id=~1,strata=~Product.line,weights = ~wt,data=strat_sample2)

#sample totals
#total
svytotal(~Total,strat2_des,deff=TRUE)
#total gross income
svytotal(~gross_income,strat2_des,deff=TRUE)
#total cost
svytotal(~cogs,strat2_des,deff=TRUE)
#total tax
svytotal(~Tax.5.,strat2_des,deff=TRUE)

#sample means
#total
svymean(~Total,strat2_des)
#rating
svymean(~Rating,strat2_des)
#gross income
svymean(~gross_income,strat2_des)

#sample Proportion
#customer type
svymean(~C_type,strat2_des)
#pay method
svymean(~Payment,strat2_des)
#Gender
svymean(~Gender,strat2_des)
#branch
svymean(~Branch,strat_des)

#>>>>>>>>>>>sub groups
#mean rating by strata
svyby(~Rating,by=~Product.line,strat2_des,svymean)
#mean total by strata
svyby(~Total,by=~Product.line,strat2_des,svymean)

#total tax by strat
svyby(~Tax.5.,by=~Product.line,strat2_des,svytotal)

#--------------------------------------------------
#ration estimation in stratified sampling

svyratio(~Total,~Tax.5.,design =strat2_des)


detach(strat_sample2)




