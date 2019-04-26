rm(list=ls())

######Part 1: Create the dataset for the analysis######
#There are 35 csv files.Read all of them. Reference:https://stackoverflow.com/questions/26401985/read-multiple-files-into-r-no-column-names  
library(plyr)
file<- ldply(list.files(pattern="*.csv"), 
               function(file){ 
                 data <- read.csv(file, header=T,skip=1) 
                 })
View(file)
#Select all data from 2005. For most dataset, data from this year is available.
#This project focuses mainly for significance of coefficicient, not the predictive results.
file_2005<-subset(file,Year==2005)
str(file_2005)
#After select all 2005 data, just drop this column. 
file_2005$Year<-NULL
View(file_2005)
#For some reason, the row number is not good.Reset it.
row.names(file_2005) <- NULL
View(file_2005)
str(file_2005)
#For new dataset creation, get all distinct combination of column values and only leave first 4 columns
file_2005_temp<-unique(file_2005[,c('Region.Country.Area','X','Series','Value')])
str(file_2005_temp)
View(file_2005_temp)
#The following codes will transform file_2005_temp into a list of dataframes.Reference:https://stackoverflow.com/questions/5319839/read-multiple-csv-files-into-separate-data-frames 
series_list<-unique(file_2005_temp$Series)
series_list
file_modeling<-lapply(series_list, function(x) file_2005_temp[file_2005_temp$Series==x,])
View(file_modeling)
file_modeling
#Join all dataframes in the list file_modeling. Reference:https://daranzolin.github.io/2016-12-10-join-list-dataframes/
library(purrr)
library(tidyverse)
file_1<-file_modeling %>% reduce(left_join, by = "Region.Country.Area")
View(file_1)
#Use this sequence to drop duplicate columns
drop_seq<-list(seq(5,length(file_1),3))
drop_seq
for (i in drop_seq){
  file_1[,i]<-NULL
}
View(file_1)
#Extract region.country.area names and codes
code_and_name<-file_1[,1:2]
colnames(code_and_name)<-c("Region.Country.Area_Code","Region.Country.Area_Name")
View(code_and_name)
#Select columns containing series names
series_names_seq<-seq(3,length(file_1),1)
series_names_seq
series_names<-file_1[,series_names_seq]
View(series_names)
colSums(is.na(series_names)) 
#In series_names, if # of NA values is greater than 100, then the column will be dropped. 
#Reference: https://stackoverflow.com/questions/34902809/remove-columns-of-dataframe-based-on-conditions-in-r
cond<-colSums(is.na(series_names)) <100
series_names_mod<-series_names[, cond, drop = FALSE]
View(series_names_mod)
rowSums(is.na(series_names_mod)) #Okay.No NA in the 81st row.
#The 81st row will be used to create the header for the new dataset. 
header_new<-series_names_mod[81,]
row.names(header_new)<-NULL
View(header_new)
header_new_seq<-seq(1,length(header_new),2)
header_list<-header_new[,header_new_seq]
View(header_list)
header_1<-unique(header_list[1,])
names(header_list)<-NULL
header_1<-unlist(c(header_list))
class(header_1)
#Remove all "Series" from series_names_mod
series_names_mod[,seq(1,length(series_names_mod),2)]<-NULL
View(series_names_mod)
colnames(series_names_mod)<-NULL
#Now, change all column names in series_names_mod with names in header_list
colnames(series_names_mod)<-header_1
View(series_names_mod)
#Finally the new dataset is created
data<-cbind(code_and_name,series_names_mod)
View(data)
#The target will be GDP Growth.
#And other columns containing GDP data will be dropped as they will not be used in modeling.
data$`GDP in current prices (millions of US dollars)`<-NULL
data$`GDP per capita (US dollars)`<-NULL
data$`GDP in constant 2010 prices (millions of US dollars)`<-NULL
#Write the data.
write.csv(data,file="data.csv",row.names = FALSE)


######Part 2: Explore/Clean the data######
#Well now it's the time to read the data.csv
dta<-read.csv("data.csv")
View(dta)
#Check # of NA in each column now.
NA_num<-colSums(is.na(dta))
NA_num
#Max number
max(NA_num) #"Consumer.price.index..General" column
#Remove rows with NA in "Consumer.price.index..General" column
dta2<-dta[!is.na(dta$Consumer.price.index..General),]
View(dta2)
#Check # of NA in each column again.
colSums(is.na(dta2))
#Now the numbers of NA values in each column are quite small.
#Now fill all NA values using the mean of each column
sapply(dta2,class) #Noticed that many columns are factors. For unknown reason, the loop to change type does not work. So type these codes instead.
dta2$Tourist.visitor.arrivals..thousands.<-as.numeric(gsub(",","",dta2$Tourist.visitor.arrivals..thousands.))
dta2$Tourism.expenditure..millions.of.US.dollars.<-as.numeric(gsub(',','',dta2$Tourism.expenditure..millions.of.US.dollars.))
dta2$Forest.cover..thousand.hectares.<-as.numeric(gsub(',','',dta2$Forest.cover..thousand.hectares.))
dta2$Permanent.crops..thousand.hectares.<-as.numeric(gsub(',','',dta2$Permanent.crops..thousand.hectares.))
dta2$Arable.land..thousand.hectares.<-as.numeric(gsub(',','',dta2$Arable.land..thousand.hectares.))
dta2$Land.area..thousand.hectares.<-as.numeric(gsub(',','',dta2$Land.area..thousand.hectares.))
dta2$Total.supply..petajoules.<-as.numeric(gsub(',','',dta2$Total.supply..petajoules.))
dta2$Net.imports..Imports...Exports...Bunkers...petajoules.<-as.numeric(gsub(',','',dta2$Net.imports..Imports...Exports...Bunkers...petajoules.))
dta2$Primary.energy.production..petajoules.<-as.numeric(gsub(',','',dta2$Primary.energy.production..petajoules.))
dta2$Balance.imports.exports..millions.of.US.dollars.<-as.numeric(gsub(',','',dta2$Balance.imports.exports..millions.of.US.dollars.))
dta2$Exports.FOB..millions.of.US.dollars.<-as.numeric(gsub(',','',dta2$Exports.FOB..millions.of.US.dollars.))
dta2$Imports.CIF..millions.of.US.dollars.<-as.numeric(gsub(',','',dta2$Imports.CIF..millions.of.US.dollars.))
dta2$Exchange.rates..period.average..national.currency.per.US.dollar.<-as.numeric(gsub(',','',dta2$Exchange.rates..period.average..national.currency.per.US.dollar.))
dta2$Exchange.rates..end.of.period..national.currency.per.US.dollar.<-as.numeric(gsub(',','',dta2$Exchange.rates..end.of.period..national.currency.per.US.dollar.))
dta2$Balance.of.Payments..Financial.account..millions.of.US.dollars.<-as.numeric(gsub(',','',dta2$Balance.of.Payments..Financial.account..millions.of.US.dollars.))
dta2$Balance.of.Payments..Current.account..millions.of.US.dollars.<-as.numeric(gsub(',','',dta2$Balance.of.Payments..Current.account..millions.of.US.dollars.))
dta2$Students.enrolled.in.primary.education..thousands.<-as.numeric(gsub(',','',dta2$Students.enrolled.in.primary.education..thousands.))
dta2$Students.enrolled.in.secondary.education..thousands.<-as.numeric(gsub(',','',dta2$Students.enrolled.in.secondary.education..thousands.))
dta2$International.migrant.stock..Both.sexes..number.<-as.numeric(gsub(',','',dta2$International.migrant.stock..Both.sexes..number.))
dta2$Population.density<-as.numeric(gsub(',','',dta2$Population.density))
dta2$Population.mid.year.estimates.for.females..millions.<-as.numeric(gsub(',','',dta2$Population.mid.year.estimates.for.females..millions.))
dta2$Population.mid.year.estimates.for.males..millions.<-as.numeric(gsub(',','',dta2$Population.mid.year.estimates.for.males..millions.))
dta2$Population.mid.year.estimates..millions.<-as.numeric(gsub(',','',dta2$Population.mid.year.estimates..millions.))
dta2$Emissions..thousand.metric.tons.of.carbon.dioxide.<-as.numeric(gsub(',','',dta2$Emissions..thousand.metric.tons.of.carbon.dioxide.))
dta2$Maternal.mortality.ratio..deaths.per.100.000.population.<-as.numeric(gsub(',','',dta2$Maternal.mortality.ratio..deaths.per.100.000.population.))
dta2$International.migrant.stock..Both.sexes..number.<-as.numeric(gsub(',','',dta2$International.migrant.stock..Both.sexes..number.))
View(dta2)
sapply(dta2,class)
#Fill NA with mean of the column
#Reference: https://stackoverflow.com/questions/25835643/replace-missing-values-with-column-mean
for(i in 3:ncol(dta2)){
  dta2[is.na(dta2[,i]), i] <- mean(dta2[,i], na.rm = TRUE)
}

View(dta2)
#Split first 2 columns (code and name)
code<-dta2$Region.Country.Area_Code
dta2$Region.Country.Area_Code<-NULL
names<-dta2$Region.Country.Area_Name
dta2$Region.Country.Area_Name<-NULL
View(dta2)
####Other: Visualization (TBD) (May be incorporated in the report)
#To make sure that the inference works well the distribution of the target variable should be checked.
v1<-ggplot(data=dta2)+geom_histogram(mapping=aes(x=dta2$GDP.real.rates.of.growth..percent.),color="black",fill="white")
v1 #No need for transformation

######Part 3: Modeling######
###Linear Regression
library(olsrr)
lm<-lm(GDP.real.rates.of.growth..percent.~.,data=dta2)
ols_step_both_p(lm)
#Based on selected features run the linear regression model:
lm_new<-lm(GDP.real.rates.of.growth..percent.~ Services....of.gross.value.added.+
             Agricultural.production..Index.Base..2004.2006...100.+
             International.migrant.stock..Both.sexes....total.population.+
             Sex.ratio..males.per.100.females.+Industry....of.gross.value.added.+Employment.by.industry..Agriculture.....Female
           +Life.expectancy.at.birth.for.females..years.+Total.fertility.rate..children.per.women.
           +Food.production..Index.Base..2004.2006...100.,data=dta2)
summary(lm_new)
#Based on the summary the following coefficients are significant at 5% significance level:
#Agricultural.production..Index.Base..2004.2006...100.          
#International.migrant.stock..Both.sexes....total.population.  
#Sex.ratio..males.per.100.females.                            
#Industry....of.gross.value.added.                             
#Employment.by.industry..Agriculture.....Female              
#Life.expectancy.at.birth.for.females..years.                  
#Total.fertility.rate..children.per.women.                      
#Food.production..Index.Base..2004.2006...100.     

###Gradient Boosting
library(caret)
GBO <- train(GDP.real.rates.of.growth..percent.
   ~., data = dta2, method = "xgbTree",
  trControl = trainControl("cv", number = 5)
)
#Importance
varImp(GBO)
#Create a visualization
x<-c('Industry....of.gross.value.added','Services....of.gross.value.added','Percentage.of.individuals.using.the.internet',
     'Gross.enrollment.ratio...Primary..female','Population.aged.0.to.14.years.old..percentage',
     'Tourism.expenditure..millions.of.US.dollars','Major.trading.partner.3....of.imports',
     'Major.trading.partner.2....of.exports','Agricultural.production..Index.Base..2004.2006...100',
     'Important.sites.for.terrestrial.biodiversity.protected....of.total.sites.protected')
y<-c(100.000,36.859,22.938,19.124,17.189,16.811,15.577,14.183,14.105,14.042)
xy<-data.frame(x,y)
#Reference:https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2
v2<-ggplot(data=xy,aes(x=x,y=y))+geom_col(fill="white",color="black")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
v2
#Top 10 most important:
#Industry....of.gross.value.added. 
#Services....of.gross.value.added. 
#Percentage.of.individuals.using.the.internet
#Gross.enrollment.ratio...Primary..female.  
#Population.aged.0.to.14.years.old..percentage. 
#Tourism.expenditure..millions.of.US.dollars.
#Major.trading.partner.3....of.imports.
#Major.trading.partner.2....of.exports.
#Agricultural.production..Index.Base..2004.2006...100.
#Important.sites.for.terrestrial.biodiversity.protected....of.total.sites.protected.  





