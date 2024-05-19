
#install.packages("survey")
#install.packages("sampler")
#install.packages("sampling")

#importing the libraries
library("survey")
library("sampler")
library("sampling")

#set working directry
setwd("E:/R stat")
mydata<-read.csv("supermarket_sales.csv")
#------------------------------------------------------------------
#drawing sample 1 in cluster 
#first select few clusters randomly
set.seed(15550)
cl = cluster(mydata,clustername = "City",size = 2,method = "srswor",description = T)
clus_st1 = getdata(mydata,cl)
t1 = table(clus_st1$City)
t1
#View(clus_st2)


#getting srs from selected clusters
cities = names(t1)
clus_st2 = data.frame()

for (i in cities) {
  srs_size=rsampcalc(nrow(clus_st1[clus_st1$City==i,]),e=3,ci=95) #sample size for SRS
  stg2 = clus_st1[clus_st1$City==i,][sample(1:t1[i],srs_size,replace=FALSE),]
  
  clus_st2 = rbind(clus_st2,stg2)
}
#head(clus_st2)
#View(clus_st2) # = 2-stage cluster sample
t2=table(clus_st2$City)
t2


##weights calculating
N=3
n=2
weighted_val = c()
for (i in 1:sum(t2)) {
  city = clus_st2[i,"City"]
  weighted_val[i] = (N*t1[city])/(n*t2[city])
}
clus_st2 = cbind(clus_st2,weighted_val)
#head(clus_st2)
#tail(stage_2_2nd)
#View(stage_2_2nd)

#esitmating in two stage cluster sample 1-----------------------------------
attach(clus_st2)
clus_des = svydesign(id=~City, weights = ~weighted_val, data = clus_st2)
clus_des

#sample_means
#mean total
svymean(~Total,clus_des,deff=TRUE)
#mean rating
svymean(~Rating,clus_des,deff=TRUE)
#gross income
svymean(~gross_income,clus_des,deff=TRUE)

#sample_totals
#total tax
svytotal(~Tax.5.,clus_des)
#total cost
svytotal(~cogs,clus_des)
#total gross income
svytotal(~gross_income,clus_des)

#sample_proportions
#gender
svymean(~Gender,clus_des)

#sample Proportion
#customer type
table(C_type)/length(C_type)
#pay method
table(Payment)/length(Payment)
#Gender
table(Gender)/length(Gender)
#branch
table(Branch)/length(Branch)

#ratio estimation..........................................................

m_i = 300
t_i_hat_2 = c()
for (i in 1:600) {
  sec = stage_2_2nd[i,"Sector"]
  t_i_hat_2[i] = (t1[sec]*stage_2_2nd$Income.in.thousand[i])/(m_i)
}
stage_2_2nd = cbind(stage_2_2nd,t_i_hat_2)
head(stage_2_2nd)


t1 = table(stage_1_2nd$Sector)
#Ratio estimation for income_sample 1
yr_bar_hat = sum(stage_2_2nd$t_i_hat_2)/sum(t1)
yr_bar_hat

detach(clus_st2)


#++++++++++++++++++,,,,,,,,,,,,,,,,,,,,,,,,,,+++++++++++++++++++++++++++++++++++++++

#drawing sample 2 in  cluster 
#first select few clusters randomly
set.seed(15561)
cl2 = cluster(mydata,clustername = "City",size = 2,method = "srswor",description = T)
clus2_st1 = getdata(mydata,cl2)
t2 = table(clus2_st1$City)
t2
#View(clus_st2)


#getting srs from selected clusters
cities = names(t2)
clus2_st2 = data.frame()

for (i in cities) {
  srs_size2=rsampcalc(nrow(clus2_st1[clus2_st1$City==i,]),e=3,ci=95) #sample size for SRS
  stg2 = clus2_st1[clus2_st1$City==i,][sample(1:t2[i],srs_size2,replace=FALSE),]
  
  clus2_st2 = rbind(clus2_st2,stg2)
}
#head(clus2_st2)
#View(clus2_st2) # = 2-stage cluster sample
tb=table(clus2_st2$City)
tb


##weights calculating
N=3
n=2
weighted_val2 = c()
for (i in 1:sum(tb)) {
  city = clus2_st2[i,"City"]
  weighted_val2[i] = (N*t2[city])/(n*tb[city])
}
clus2_st2 = cbind(clus2_st2,weighted_val2)
#head(clus2_st2)
#tail(clus2_st2)
#View(clus2_st2)

#esitmating in two stage cluster sample 1-----------------------------------
attach(clus2_st2)
clus2_des = svydesign(id=~City, weights = ~weighted_val2, data = clus2_st2)
clus2_des

#sample_means
#mean total
svymean(~Total,clus2_des,deff=TRUE)
#mean rating
svymean(~Rating,clus2_des,deff=TRUE)
#gross income
svymean(~gross_income,clus2_des,deff=TRUE)

#sample_totals
#total tax
svytotal(~Tax.5.,clus2_des)
#total cost
svytotal(~cogs,clus2_des)
#total gross income
svytotal(~gross_income,clus2_des)

#sample_proportions
#gender
svymean(~Gender,clus2_des)
#sample Proportion
#customer type
table(C_type)/length(C_type)
#pay method
table(Payment)/length(Payment)
#Gender
table(Gender)/length(Gender)
#branch
table(Branch)/length(Branch)

detach(clus2_st2)





