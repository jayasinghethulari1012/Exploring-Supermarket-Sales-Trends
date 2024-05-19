#install.packages("survey")
#install.packages("sampler")

#import libraries
library(survey)
library(sampler)

# Set the working directory to where your CSV file is located
setwd("E:/R stat")

# Read the CSV file
Data <- read.csv("supermarket_sales.csv")
#View(Data)

# Calculating pop values
attach(Data)

#Mean------------------------------
#rating
rating_mean=mean(Rating)
rating_mean
#total
avg_total=mean(Total)
avg_total
#income
avg_income=mean(gross_income)
avg_income


#Total------------------
#gross income
tot_income=sum(gross_income)
tot_income
#tax
tot_tax=sum(Tax.5.)
tot_tax
#total cost
tot_cost=sum(cogs)
tot_cost


#Proportion----------
#Customer type
customer_typ=table(C_type)/length(C_type)
customer_typ
#payment
pay_method=table(Payment)/length(Payment)
pay_method
#Gender
gender=table(Gender)/length(Gender)
gender
#product line
prod_line=table(Product.line)/length(Product.line)
prod_line

detach(Data)
#********************************************************************

#simple random sample 

#sample size for SRS for margin of error 3
srs_size=rsampcalc(nrow(Data),e=3,ci=95) 
srs_size

set.seed(15549)
#drawing sample 1 in SRS ++++++++++++++++++++++++++++++++++++++++++++++++
srs1=rsamp(Data,n=srs_size,rep =FALSE)


#Estimations SRS 1

attach(srs1)
srs1_des=svydesign(id=~1,strata=NULL,data =srs1)
summary(srs1_des)
#sample mean 
#mean total
svymean(~Total,srs1_des)
#mean rating
svymean(~Rating,srs1_des)
#mean gross income
svymean(~gross_income,srs1_des)

#sample totals
#Total Tax
svytotal(~Tax.5.,srs1_des)
#total cost
svytotal(~cogs,srs1_des)
#total gross income
svytotal(~gross_income,srs1_des)

#sample Proportion
#customer type
svymean(~C_type,srs1_des)
#pay method
svymean(~Payment,srs1_des)
#Gender
svymean(~Gender,srs1_des)
#branch
svymean(~Branch,srs1_des)
#--------------------------------
#ratio estimators
#total bill estimation bill with tax pay
svyratio(srs1$Total,srs1$Tax.5.,design = srs1_des)

 

detach(srs1)
#---------------------------------------------------------------------

#SRS_2

set.seed(15551)
#drawing a SRS 2
srs2=rsamp(Data,n=srs_size,rep =FALSE)

#Estimations SRS 2
attach(srs2)
srs2_des=svydesign(id=~1,strata=NULL,data =srs2)

#sample mean 
#mean total
svymean(~Total,srs2_des)
#mean rating
svymean(~Rating,srs2_des)
#mean income
svymean(~gross_income,srs2_des)

#sample totals
#Total Tax
svytotal(~Tax.5.,srs2_des)
#total cost
svytotal(~cogs,srs2_des)
#gross income
svytotal(~gross_income,srs2_des)

#sample Proportion
#customer type
svymean(~C_type,srs2_des)
#pay method
svymean(~Payment,srs2_des)
#Gender
svymean(~Gender,srs2_des)
#branch
svymean(~Branch,srs2_des)
#---------------------------------------------------------
#ratio estimates
#total bill estimation bill with tax pay
svyratio(srs2$Total,srs2$Tax.5.,design = srs2_des)
detach(srs2)