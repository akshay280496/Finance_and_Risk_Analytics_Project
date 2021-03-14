# setting up working directory #

setwd("D:\\BABI\\BABI-18th Residency\\Assignment\\Assignment_Work")
getwd()

# loading the library files #

library(readxl)
library(writexl)
library(dplyr)
library(readr)
library(kableExtra)
library(ggplot2)
library(naniar)
library(visdat)
library(corrplot)
library(StatMeasures)
library(crayon)
library(gridExtra)
library(DataExplorer)
library(lattice)
library(mlr)
library(explore)
library(lmtest)
library(DMwR)
library(HH)
library(pROC)
library(e1071)
library(caret)

# reading the raw data #

company <- read_excel("raw-data.xlsx", sheet = "raw data")
View(company)
summary(company)
dim(company)

# reading the validation data #

validation <- read_excel("validation_data.xlsx", sheet = "valdata")
View(validation)
summary(validation)
dim(validation)

#creation of report

create_report(company)

# creation of a new variable "Default" #

company$Default <- ifelse(company$`Networth Next Year`>0,0,1)
str(company$Default)
company$Default <- as.factor(company$Default)

# renaming the "Default-1" column in validation dataset and then converting the datatype of the same #

validation = validation%>% rename(Default = "Default - 1")
str(validation$Default)
validation$Default <- as.factor(validation$Default)

# Removing the "Deposits column" in company and validation dataset #

company <- company[,-22]
validation <- validation[,-22]

######### Datatype conversion ##########

# company dataset #

company$`PE on BSE` <- as.numeric(company$`PE on BSE`)
company$`Creditors turnover` <- as.numeric(company$`Creditors turnover`)
company$`Debtors turnover` <- as.numeric(company$`Debtors turnover`)
company$`Finished goods turnover` <- as.numeric(company$`Finished goods turnover`)
company$`WIP turnover` <- as.numeric(company$`WIP turnover`)
company$`Raw material turnover` <- as.numeric(company$`Raw material turnover`)
company$`Shares outstanding` <- as.numeric(company$`Shares outstanding`)
company$`Equity face value` <- as.numeric(company$`Equity face value`)

# validation dataset #

validation$`Creditors turnover` <- as.numeric(validation$`Creditors turnover`)
validation$`Debtors turnover` <- as.numeric(validation$`Debtors turnover`)
validation$`Finished goods turnover` <- as.numeric(validation$`Finished goods turnover`)
validation$`WIP turnover` <- as.numeric(validation$`WIP turnover`)
validation$`Raw material turnover` <- as.numeric(validation$`Raw material turnover`)
validation$`Shares outstanding` <- as.numeric(validation$`Shares outstanding`)
validation$`Equity face value` <- as.numeric(validation$`Equity face value`)
validation$`PE on BSE` <- as.numeric(validation$`PE on BSE`)

str(validation)
str(company)

#################### missing value treatment #########################
 
# percentage of missing values in each column #

contains_any_na <- sapply(company, function(x) {
  count<-as.integer(sum(is.na(x)),length=0)
  percentage<-round(count/NROW(x),4)*100
  datatype<-class(x)
  frame<-c(datatype,count,percentage)
}
)

missing_frame <- t(contains_any_na)
dim(missing_frame)
colnames(missing_frame) <- c("datatype","missing records count", "percentage")
print(missing_frame)
write.csv(missing_frame,"missing_cols.csv")

# since "Networth Next Year" has been encoded as "Default", we can remove that column #

company1 <- subset(company, select = -c(1,2,19,24,31,33,51))
dim(company1)
names(company1)
summary(company1)

validation1 <- subset(validation, select = -c(1,19,24,31,33,51))
dim(validation1)
names(validation1)
summary(validation1)

# outlier function #

outlierpercentage <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  mean1 <- mean(var_name, na.rm = T)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  na2 <- sum(is.na(var_name))
  cat(paste("Outliers identified: ",(na2 - na1), " from ", tot, " observations \n"))
  cat(paste("Proportion (%) of outliers: ", round((na2 - na1) / tot*100,3),"\n"))
  cat(paste("Number of NA's: ",na1,"\n"))
  cat(paste("NA percentage is:", round(na1/(tot+na1)*100,3),"\n"))
  cat(paste("Mean of the outliers: ", mo,"\n"))
  mean2 <- mean(var_name, na.rm = T)
  cat(paste("Mean without removing outliers: ", round(mean1,3),"\n"))
  cat(paste("Mean if we remove outliers: ", round(mean2,3),"\n"))
  
}

# replace median function #

replace_median<-function(df,colname){
  colname <- eval(substitute(colname),eval(df))
  colname <- ifelse(is.na(colname),median(colname, na.rm = TRUE),colname)
  return (colname)
}

# treat outliers #

treat_outliers<-function(df,colname){
  colname <- eval(substitute(colname),eval(df))
  colname<-ifelse(colname>=quantile(colname,0.99),quantile(colname,0.99),colname)
  colname<-ifelse(colname<=quantile(colname,0.01),quantile(colname,0.01),colname)
  return (colname)
}

# graphs #

graphs<-function(df,colname,dependent,xlabel){ 
  colname <- eval(substitute(colname),eval(df))
  
     if(dependent==TRUE){
    ggplot(company1,aes(x=Default ,y=(colname))) +
      geom_boxplot(fill="orange",outlier.colour="black", outlier.shape=16,
                   outlier.size=2, notch=FALSE)+
      labs(title = paste("Credit risk and ", xlabel),
           x="Default", y= xlabel)+
      theme(panel.background = element_rect(fill = 'light yellow'),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5))
  }
}

#histogram (uni-variate) graphs

histogram <- function(df, colname, his, xlabel){
  colname <- eval(substitute(colname), eval(df))
  
  if(his==T){
    hist((colname), xlab = xlabel, col = "green", 
         main = paste("Histogram for ",xlabel),breaks = 10)
  }
}

######### Treatment of each and every columns ###########

# company dataset #

# Total income #
par(bg="light blue")
cat(green$bold("The column is \"total income\""))
outlierpercentage(company1, `Total income`)
cat("summary before treatment")
summary(company1$`Total income`)
company1$`Total income`<-replace_median(company1,`Total income`)
company1$`Total income`<-treat_outliers(company1,`Total income`)
cat("summary after treatment")
summary(company1$`Total income`)
graphs(company1,`Total income`,TRUE,"Total income")
histogram(company1,`Total income`,TRUE,"Total income")

# Change in stock #
par(bg="light blue")
cat(green$bold("The column is \"change in stock\""))
outlierpercentage(company1, `Change in stock`)
cat("summary before treatment")
summary(company1$`Change in stock`)
company1$`Change in stock`<-replace_median(company1,`Change in stock`)
company1$`Change in stock`<-treat_outliers(company1,`Change in stock`)
cat("summary after treatment")
summary(company1$`Change in stock`)
histogram(company1,`Change in stock`,TRUE,"Change in Stock")
graphs(company1,`Change in stock`,TRUE,"Change in Stock")

# Income from financial services #
par(bg="light blue")
cat(green$bold("The column is \"income from financial services\""))
outlierpercentage(company1, `Income from financial services`)
cat("summary before treatment")
summary(company1$`Income from financial services`)
company1$`Income from financial services`<-replace_median(company1,`Income from financial services`)
company1$`Income from financial services`<-treat_outliers(company1,`Income from financial services`)
cat("summary after treatment")
summary(company1$`Income from financial services`)
histogram(company1,`Income from financial services`,TRUE,"Income from financial services")
graphs(company1,`Income from financial services`,TRUE,"Income from financial services")

# Finished goods turnover #
par(bg="light blue")
cat(green$bold("The column is \"Finished goods turnover\""))
outlierpercentage(company1, `Finished goods turnover`)
cat("summary before treatment")
summary(company1$`Finished goods turnover`)
company1$`Finished goods turnover`<-replace_median(company1,`Finished goods turnover`)
company1$`Finished goods turnover`<-treat_outliers(company1,`Finished goods turnover`)
cat("summary after treatment")
summary(company1$`Finished goods turnover`)
histogram(company1,`Finished goods turnover`,TRUE,"Finished goods turnover")
graphs(company1,`Finished goods turnover`,TRUE,"Finished goods turnover")

# Shares outstanding #
par(bg="light blue")
cat(green$bold("The column is \"Shares outstanding\""))
outlierpercentage(company1, `Shares outstanding`)
cat("summary before treatment")
summary(company1$`Shares outstanding`)
company1$`Shares outstanding`<-replace_median(company1,`Shares outstanding`)
company1$`Shares outstanding`<-treat_outliers(company1,`Shares outstanding`)
cat("summary after treatment")
summary(company1$`Shares outstanding`)
histogram(company1,`Shares outstanding`,TRUE,"Shares outstanding")
graphs(company1,`Shares outstanding`,TRUE,"Shares outstanding")

# Total expenses #
par(bg="light blue")
cat(green$bold("The column is \"Total expenses\""))
outlierpercentage(company1, `Total expenses`)
cat("summary before treatment")
summary(company1$`Total expenses`)
company1$`Total expenses`<-replace_median(company1,`Total expenses`)
company1$`Total expenses`<-treat_outliers(company1,`Total expenses`)
cat("summary after treatment")
summary(company1$`Total expenses`)
histogram(company1,`Shares outstanding`,TRUE,"Shares outstanding")
graphs(company1,`Shares outstanding`,TRUE,"Shares outstanding")

# Profit after tax #
par(bg="light blue")
cat(green$bold("The column is \"Profit after Tax\""))
outlierpercentage(company1, `Profit after tax`)
cat("summary before treatment")
summary(company1$`Profit after tax`)
company1$`Profit after tax`<-replace_median(company1,`Profit after tax`)
company1$Profit.after.tax<-treat_outliers(company1,`Profit after tax`)
cat("summary after treatment")
summary(company1$`Profit after tax`)
histogram(company1,`Profit after tax`,TRUE,"Profit after tax")
graphs(company1,`Profit after tax`,TRUE,"Profit after tax")

# PBDITA #
par(bg="light blue")
cat(green$bold("The column is \"PBDITA\""))
outlierpercentage(company1, PBDITA)
cat("summary before treatment")
summary(company1$PBDITA)
company1$PBDITA<-replace_median(company1,PBDITA)
company1$PBDITA<-treat_outliers(company1,PBDITA)
cat("summary after treatment")
summary(company1$PBDITA)
histogram(company1,PBDITA,TRUE,"PBDITA")
graphs(company1,PBDITA,TRUE,"PBDITA")


# PBT #
par(bg="light blue")
cat(green$bold("The column is \"PBT\""))
outlierpercentage(company1, PBT)
cat("summary before treatment")
summary(company1$PBT)
company1$PBT<-replace_median(company1,PBT)
company1$PBT<-treat_outliers(company1,PBT)
cat("summary after treatment")
summary(company1$PBT)
histogram(company1,PBT,TRUE,"PBT")
graphs(company1,PBT,TRUE,"PBT")


# Cash profit #
par(bg="light blue")
cat(green$bold("The column is \"Cash profit\""))
outlierpercentage(company1, `Cash profit`)
cat("summary before treatment")
summary(company1$`Cash profit`)
company1$`Cash profit`<-replace_median(company1,`Cash profit`)
company1$Cash.profit<-treat_outliers(company1,`Cash profit`)
cat("summary after treatment")
summary(company1$`Cash profit`)
histogram(company1,`Cash profit`,TRUE,"Cash profit")
graphs(company1,`Cash profit`,TRUE,"Cash profit")


# PBDITA as % of total income #
par(bg="light blue")
cat(green$bold("The column is \"PBDITA as % of total income\""))
outlierpercentage(company1, `PBDITA as % of total income`)
cat("summary before treatment")
summary(company1$`PBDITA as % of total income`)
company1$`PBDITA as % of total income`<-replace_median(company1,`PBDITA as % of total income`)
company1$`PBDITA as % of total income`<-treat_outliers(company1,`PBDITA as % of total income`)
cat("summary after treatment")
summary(company1$`PBDITA as % of total income`)
histogram(company1,`PBDITA as % of total income`,TRUE,"PBDITA as % of total income")
graphs(company1,`PBDITA as % of total income`,TRUE,"PBDITA as % of total income")

# Equity face value #
par(bg="light blue")
cat(green$bold("The column is \"Equity face value\""))
outlierpercentage(company1, `Equity face value`)
cat("summary before treatment")
summary(company1$`Equity face value`)
company1$`Equity face value`<-replace_median(company1,`Equity face value`)
company1$`Equity face value`<-treat_outliers(company1,`Equity face value`)
cat("summary after treatment")
summary(company1$`Equity face value`)
histogram(company1,`Equity face value`,TRUE,"Equity face value")
graphs(company1,`Equity face value`,TRUE,"Equity face value")

# WIP turnover #
par(bg="light blue")
cat(green$bold("The column is \"WIP turnover\""))
outlierpercentage(company1, `WIP turnover`)
cat("summary before treatment")
summary(company1$`WIP turnover`)
company1$`WIP turnover`<-replace_median(company1,`WIP turnover`)
company1$`WIP turnover`<-treat_outliers(company1,`WIP turnover`)
cat("summary after treatment")
summary(company1$`WIP turnover`)
histogram(company1,`WIP turnover`,TRUE,"WIP turnover")
graphs(company1,`WIP turnover`,TRUE,"WIP turnover")

# Borrowings #
par(bg="light blue")
cat(green$bold("The column is \"Borrowings\""))
outlierpercentage(company1, Borrowings)
cat("summary before treatment")
summary(company1$Borrowings)
company1$Borrowings<-replace_median(company1,Borrowings)
company1$Borrowings<-treat_outliers(company1,Borrowings)
cat("summary after treatment")
summary(company1$Borrowings)
histogram(company1,Borrowings,TRUE,"Borrowings")
graphs(company1,Borrowings,TRUE,"Borrowings")

# Raw material turnover #
par(bg="light blue")
cat(green$bold("The column is \"Raw material turnover\""))
outlierpercentage(company1, `Raw material turnover`)
cat("summary before treatment")
summary(company1$`Raw material turnover`)
company1$`Raw material turnover`<-replace_median(company1,`Raw material turnover`)
company1$`Raw material turnover`<-treat_outliers(company1,`Raw material turnover`)
cat("summary after treatment")
summary(company1$`Raw material turnover`)
histogram(company1,`Raw material turnover`,TRUE,"Raw material turnover")
graphs(company1,`Raw material turnover`,TRUE,"Raw material turnover")

# Creditors turnover #
par(bg="light blue")
cat(green$bold("The column is \"Creditors turnover\""))
outlierpercentage(company1, `Creditors turnover`)
cat("summary before treatment")
summary(company1$`Creditors turnover`)
company1$`Creditors turnover`<-replace_median(company1,`Creditors turnover`)
company1$`Creditors turnover`<-treat_outliers(company1,`Creditors turnover`)
cat("summary after treatment")
summary(company1$`Creditors turnover`)
histogram(company1,`Creditors turnover`,TRUE,"Creditors turnover")
graphs(company1,`Creditors turnover`,TRUE,"Creditors turnover")

#Debtors turnover #
par(bg="light blue")
cat(green$bold("The column is \"Debtors turnover\""))
outlierpercentage(company1, `Debtors turnover`)
cat("summary before treatment")
summary(company1$`Debtors turnover`)
company1$`Debtors turnover`<-replace_median(company1,`Debtors turnover`)
company1$`Debtors turnover`<-treat_outliers(company1,`Debtors turnover`)
cat("summary after treatment")
summary(company1$`Debtors turnover`)
histogram(company1,`Debtors turnover`,TRUE,"Debtors turnover")
graphs(company1,`Debtors turnover`,TRUE,"Debtors turnover")

# Sales #
par(bg="light blue")
cat(green$bold("The column is \"Sales\""))
outlierpercentage(company1, Sales)
cat("summary before treatment")
summary(company1$Sales)
company1$Sales<-replace_median(company1,Sales)
company1$Sales<-treat_outliers(company1,Sales)
cat("summary after treatment")
summary(company1$Sales)
histogram(company1,Sales,TRUE,"Sales")
graphs(company1,Sales,TRUE,"Sales")

# Net fixed assets #
par(bg="light blue")
cat(green$bold("The column is \"Net fixed assets\""))
outlierpercentage(company1, `Net fixed assets`)
cat("summary before treatment")
summary(company1$`Net fixed assets`)
company1$`Net fixed assets`<-replace_median(company1,`Net fixed assets`)
company1$`Net fixed assets`<-treat_outliers(company1,`Net fixed assets`)
cat("summary after treatment")
summary(company1$`Net fixed assets`)
histogram(company1,`Net fixed assets`,TRUE,"Net fixed assets")
graphs(company1,`Net fixed assets`,TRUE,"Net fixed assets")

# Current liabilities & provisions #
par(bg="light blue")
cat(green$bold("The column is \"Current liabilities & provisions\""))
outlierpercentage(company1, `Current liabilities & provisions`)
cat("summary before treatment")
summary(company1$`Current liabilities & provisions`)
company1$`Current liabilities & provisions`<-replace_median(company1,`Current liabilities & provisions`)
company1$`Current liabilities & provisions`<-treat_outliers(company1,`Current liabilities & provisions`)
cat("summary after treatment")
summary(company1$`Current liabilities & provisions`)
histogram(company1,`Current liabilities & provisions`,TRUE,"Current liabilities & provisions")
graphs(company1,`Current liabilities & provisions`,TRUE,"Current liabilities & provisions")

# Quick ratio (times) #
par(bg="light blue")
cat(green$bold("The column is \"Quick ratio (times)\""))
outlierpercentage(company1, `Quick ratio (times)`)
cat("summary before treatment")
summary(company1$`Quick ratio (times)`)
company1$`Quick ratio (times)`<-replace_median(company1,`Quick ratio (times)`)
company1$`Quick ratio (times)`<-treat_outliers(company1,`Quick ratio (times)`)
cat("summary after treatment")
summary(company1$`Quick ratio (times)`)
histogram(company1,`Quick ratio (times)`,TRUE,"Quick ratio (times)")
graphs(company1,`Quick ratio (times)`,TRUE,"Quick ratio (times)")

# Current ratio (times) #
par(bg="light blue")
cat(green$bold("The column is \"Current ratio (times)\""))
outlierpercentage(company1, `Current ratio (times)`)
cat("summary before treatment")
summary(company1$`Current ratio (times)`)
company1$`Current ratio (times)`<-replace_median(company1,`Current ratio (times)`)
company1$`Current ratio (times)`<-treat_outliers(company1,`Current ratio (times)`)
cat("summary after treatment")
summary(company1$`Current ratio (times)`)
histogram(company1,`Current ratio (times)`,TRUE,"Current ratio (times)")
graphs(company1,`Current ratio (times)`,TRUE,"Current ratio (times)")

# Cash to current liabilities (times) #
par(bg="light blue")
cat(green$bold("The column is \"Cash to current liabilities (times)\""))
outlierpercentage(company1, `Cash to current liabilities (times)`)
cat("summary before treatment")
summary(company1$`Cash to current liabilities (times)`)
company1$`Cash to current liabilities (times)`<-replace_median(company1,`Cash to current liabilities (times)`)
company1$`Cash to current liabilities (times)`<-treat_outliers(company1,`Cash to current liabilities (times)`)
cat("summary after treatment")
summary(company1$`Cash to current liabilities (times)`)
histogram(company1,`Cash to current liabilities (times)`,TRUE,"Cash to current liabilities (times)")
graphs(company1,`Cash to current liabilities (times)`,TRUE,"Cash to current liabilities (times)")

# Reserves and funds #
par(bg="light blue")
cat(green$bold("The column is \"Reserves and funds\""))
outlierpercentage(company1, `Reserves and funds`)
cat("summary before treatment")
summary(company1$`Reserves and funds`)
company1$`Reserves and funds`<-replace_median(company1,`Reserves and funds`)
company1$`Reserves and funds`<-treat_outliers(company1,`Reserves and funds`)
cat("summary after treatment")
summary(company1$`Reserves and funds`)
histogram(company1,`Reserves and funds`,TRUE,"Reserves and funds")
graphs(company1,`Reserves and funds`,TRUE,"Reserves and funds")

# Cash to average cost of sales per day #
par(bg="light blue")
cat(green$bold("The column is \"Cash to average cost of sales per day\""))
outlierpercentage(company1, `Cash to average cost of sales per day`)
cat("summary before treatment")
summary(company1$`Cash to average cost of sales per day`)
company1$`Cash to average cost of sales per day`<-replace_median(company1,`Cash to average cost of sales per day`)
company1$`Cash to average cost of sales per day`<-treat_outliers(company1,`Cash to average cost of sales per day`)
cat("summary after treatment")
summary(company1$`Cash to average cost of sales per day`)
histogram(company1,`Cash to average cost of sales per day`,TRUE,"Cash to average cost of sales per day")
graphs(company1,`Cash to average cost of sales per day`,TRUE,"Cash to average cost of sales per day")

# PBT as % of total income #
par(bg="light blue")
cat(green$bold("The column is \"PBT as % of total income\""))
outlierpercentage(company1, `PBT as % of total income`)
cat("summary before treatment")
summary(company1$`PBT as % of total income`)
company1$`PBT as % of total income`<-replace_median(company1,`PBT as % of total income`)
company1$`PBT as % of total income`<-treat_outliers(company1,`PBT as % of total income`)
cat("summary after treatment")
summary(company1$`PBT as % of total income`)
histogram(company1,`PBT as % of total income`,TRUE,"PBT as % of total income")
graphs(company1,`PBT as % of total income`,TRUE,"PBT as % of total income")

# PAT as % of total income #
par(bg="light blue")
cat(green$bold("The column is \"PAT as % of total income\""))
outlierpercentage(company1, `PAT as % of total income`)
cat("summary before treatment")
summary(company1$`PAT as % of total income`)
company1$`PAT as % of total income`<-replace_median(company1,`PAT as % of total income`)
company1$`PAT as % of total income`<-treat_outliers(company1,`PAT as % of total income`)
cat("summary after treatment")
summary(company1$`PAT as % of total income`)
histogram(company1,`PAT as % of total income`,TRUE,"PAT as % of total income")
graphs(company1,`PAT as % of total income`,TRUE,"PAT as % of total income")

# Cash profit as % of total income #
par(bg="light blue")
cat(green$bold("The column is \"Cash profit as % of total income\""))
outlierpercentage(company1, `Cash profit as % of total income`)
cat("summary before treatment")
summary(company1$`Cash profit as % of total income`)
company1$`Cash profit as % of total income`<-replace_median(company1,`Cash profit as % of total income`)
company1$`Cash profit as % of total income`<-treat_outliers(company1,`Cash profit as % of total income`)
cat("summary after treatment")
summary(company1$`Cash profit as % of total income`)
histogram(company1,`Cash profit as % of total income`,TRUE,"Cash profit as % of total income")
graphs(company1,`Cash profit as % of total income`,TRUE,"Cash profit as % of total income")

# Current assets #
par(bg="light blue")
cat(green$bold("The column is \"Current assets\""))
outlierpercentage(company1, `Current assets`)
cat("summary before treatment")
summary(company1$`Current assets`)
company1$`Current assets`<-replace_median(company1,`Current assets`)
company1$`Current assets`<-treat_outliers(company1,`Current assets`)
cat("summary after treatment")
summary(company1$`Current assets`)
histogram(company1,`Current assets`,TRUE,"Current assets")
graphs(company1,`Current assets`,TRUE,"Current assets")

# Cumulative retained profits #
par(bg="light blue")
cat(green$bold("The column is \"Cumulative retained profits\""))
outlierpercentage(company1, `Cumulative retained profits`)
cat("summary before treatment")
summary(company1$`Cumulative retained profits`)
company1$`Cumulative retained profits`<-replace_median(company1,`Cumulative retained profits`)
company1$`Cumulative retained profits`<-treat_outliers(company1,`Cumulative retained profits`)
cat("summary after treatment")
summary(company1$`Cumulative retained profits`)
histogram(company1,`Cumulative retained profits`,TRUE,"Cumulative retained profits")
graphs(company1,`Cumulative retained profits`,TRUE,"Cumulative retained profits")

# Net working capital #
par(bg="light blue")
cat(green$bold("The column is \"Net working capital\""))
outlierpercentage(company1, `Net working capital`)
cat("summary before treatment")
summary(company1$`Net working capital`)
company1$`Net working capital`<-replace_median(company1,`Net working capital`)
company1$`Net working capital`<-treat_outliers(company1,`Net working capital`)
cat("summary after treatment")
summary(company1$`Net working capital`)
histogram(company1,`Net working capital`,TRUE,"Net working capital")
graphs(company1,`Net working capital`,TRUE,"Net working capital")

# Total capital #
par(bg="light blue")
cat(green$bold("The column is \"Total capital\""))
outlierpercentage(company1, `Total capital`)
cat("summary before treatment")
summary(company1$`Total capital`)
company1$`Total capital`<-replace_median(company1,`Total capital`)
company1$`Total capital`<-treat_outliers(company1,`Total capital`)
cat("summary after treatment")
summary(company1$`Total capital`)
histogram(company1,`Total capital`,TRUE,"Total capital")
graphs(company1,`Total capital`,TRUE,"Total capital")

# Total assets #
par(bg="light blue")
cat(green$bold("The column is \"Total assets\""))
outlierpercentage(company1, `Total assets`)
cat("summary before treatment")
summary(company1$`Total assets`)
company1$`Total capital`<-treat_outliers(company1,`Total assets`)
cat("summary after treatment")
summary(company1$`Total assets`)
histogram(company1,`Total assets`,TRUE,"Total assets")
graphs(company1,`Total assets`,TRUE,"Total assets")

# Net worth #
par(bg="light blue")
cat(green$bold("The column is \"Net worth\""))
outlierpercentage(company1, `Net worth`)
cat("summary before treatment")
summary(company1$`Net worth`)
company1$`Net worth`<-treat_outliers(company1,`Net worth`)
cat("summary after treatment")
summary(company1$`Net worth`)
histogram(company1,`Net worth`,TRUE,"Net worth")
graphs(company1,`Net worth`,TRUE,"Net worth")

# PAT as % of net worth #
par(bg="light blue")
cat(green$bold("The column is \"PAT as % of net worth\""))
outlierpercentage(company1, `PAT as % of net worth`)
cat("summary before treatment")
summary(company1$`PAT as % of net worth`)
company1$`PAT as % of net worth`<-treat_outliers(company1,`PAT as % of net worth`)
cat("summary after treatment")
summary(company1$`PAT as % of net worth`)
histogram(company1,`PAT as % of net worth`,TRUE,"PAT as % of net worth")
graphs(company1,`PAT as % of net worth`,TRUE,"PAT as % of net worth")

# Shareholders funds #
par(bg="light blue")
cat(green$bold("The column is \"Shareholders funds\""))
outlierpercentage(company1, `Shareholders funds`)
cat("summary before treatment")
summary(company1$`Shareholders funds`)
company1$`Shareholders funds`<-treat_outliers(company1,`Shareholders funds`)
cat("summary after treatment")
summary(company1$`Shareholders funds`)
histogram(company1,`Shareholders funds`,TRUE,"Shareholders funds")
graphs(company1,`Shareholders funds`,TRUE,"Shareholders funds")

# Capital employed #
par(bg="light blue")
cat(green$bold("The column is \"Capital employed\""))
outlierpercentage(company1, `Capital employed`)
cat("summary before treatment")
summary(company1$`Capital employed`)
company1$`Capital employed`<-treat_outliers(company1,`Capital employed`)
cat("summary after treatment")
summary(company1$`Capital employed`)
histogram(company1,`Capital employed`,TRUE,"Capital employed")
graphs(company1,`Capital employed`,TRUE,"Capital employed")

# TOL/TNW #
par(bg="light blue")
cat(green$bold("The column is \"TOL/TNW\""))
outlierpercentage(company1, `TOL/TNW`)
cat("summary before treatment")
summary(company1$`TOL/TNW`)
company1$`TOL/TNW`<-treat_outliers(company1,`TOL/TNW`)
cat("summary after treatment")
summary(company1$`TOL/TNW`)
histogram(company1,`TOL/TNW`,TRUE,"TOL/TNW")
graphs(company1,`TOL/TNW`,TRUE,"TOL/TNW")

# Total term liabilities / tangible net worth #
par(bg="light blue")
cat(green$bold("The column is \"Total term liabilities / tangible net worth\""))
outlierpercentage(company1, `Total term liabilities / tangible net worth`)
cat("summary before treatment")
summary(company1$`Total term liabilities / tangible net worth`)
company1$`Total term liabilities / tangible net worth`<-treat_outliers(company1,`Total term liabilities / tangible net worth`)
cat("summary after treatment")
summary(company1$`Total term liabilities / tangible net worth`)
histogram(company1,`Total term liabilities / tangible net worth`,TRUE,"Total term liabilities / tangible net worth")
graphs(company1,`Total term liabilities / tangible net worth`,TRUE,"Total term liabilities / tangible net worth")

# Contingent liabilities / Net worth (%) #
par(bg="light blue")
cat(green$bold("The column is \"Contingent liabilities / Net worth (%)\""))
outlierpercentage(company1, `Contingent liabilities / Net worth (%)`)
cat("summary before treatment")
summary(company1$`Contingent liabilities / Net worth (%)`)
company1$`Contingent liabilities / Net worth (%)`<-treat_outliers(company1,`Contingent liabilities / Net worth (%)`)
cat("summary after treatment")
summary(company1$`Contingent liabilities / Net worth (%)`)
histogram(company1,`Contingent liabilities / Net worth (%)`,TRUE,"Contingent liabilities / Net worth (%)")
graphs(company1,`Contingent liabilities / Net worth (%)`,TRUE,"Contingent liabilities / Net worth (%)")

# Debt to equity ratio (times) #
par(bg="light blue")
cat(green$bold("The column is \"Debt to equity ratio (times)\""))
outlierpercentage(company1, `Debt to equity ratio (times)`)
cat("summary before treatment")
summary(company1$`Debt to equity ratio (times)`)
company1$`Debt to equity ratio (times)`<-treat_outliers(company1,`Debt to equity ratio (times)`)
cat("summary after treatment")
summary(company1$`Debt to equity ratio (times)`)
histogram(company1,`Debt to equity ratio (times)`,TRUE,"Debt to equity ratio (times)")
graphs(company1,`Debt to equity ratio (times)`,TRUE,"Debt to equity ratio (times)")

# EPS #
par(bg="light blue")
cat(green$bold("The column is \"EPS\""))
outlierpercentage(company1, EPS)
cat("summary before treatment")
summary(company1$EPS)
company1$EPS<-treat_outliers(company1,EPS)
cat("summary after treatment")
summary(company1$EPS)
histogram(company1,EPS,TRUE,"EPS")
graphs(company1,EPS,TRUE,"EPS")

# Adjusted EPS #
par(bg="light blue")
cat(green$bold("The column is \"Adjusted EPS\""))
outlierpercentage(company1, `Adjusted EPS`)
cat("summary before treatment")
summary(company1$`Adjusted EPS`)
company1$`Adjusted EPS`<-treat_outliers(company1,`Adjusted EPS`)
cat("summary after treatment")
summary(company1$`Adjusted EPS`)
histogram(company1,`Adjusted EPS`,TRUE,"Adjusted EPS")
graphs(company1,`Adjusted EPS`,TRUE,"Adjusted EPS")

# Total liabilities #
par(bg="light blue")
cat(green$bold("The column is \"Total liabilities\""))
outlierpercentage(company1, `Total liabilities`)
cat("summary before treatment")
summary(company1$`Total liabilities`)
company1$`Total liabilities`<-treat_outliers(company1,`Total liabilities`)
cat("summary after treatment")
summary(company1$`Total liabilities`)
histogram(company1,`Total liabilities`,TRUE,"Total liabilities")
graphs(company1,`Total liabilities`,TRUE,"Total liabilities")

#removing the last two columns which are duplicate columns

names(company1)
company1 <- company1[,-c(46,47)]

#Correlation plot

cordata<- subset(company1, select = -c(Default))
vals=cor(cordata)
pdf("correlation_matrix.pdf",width = 10, height = 10)
corrplot(vals, method="circle")
dev.off()

#bi-variate analysis report

company1 %>% explore(target = Default)

#replacing the zeroes in the blank values of validation dataset

sapply(validation1, function(x){sum(is.na(x))})
validation1[is.na(validation1)]=0  #replace the zeroes in validation dataset
any(is.na(validation1)) #re-checking for null values

############LOGISTIC REGRESSION modelling################

#Model with all the columns

lg_model1 <- glm(Default~., data = company1, family = binomial())
summary(lg_model1)
vif(lg_model1)

#Removing the "Total liabilities" column

lg_model2 <- glm(Default~. -`Total liabilities`, data = company1, family = binomial())
summary(lg_model2)
vif(lg_model2)

#Removing the Total income column

lg_model3 <- glm(Default~. -`Total liabilities` -`Total income`, data = company1, family = binomial())
summary(lg_model3)
vif(lg_model3)

#Removing the "Sales" column

lg_model4 <- glm(Default~. -`Total liabilities` -`Total income` -Sales, data = company1, family = binomial())
summary(lg_model4)
vif(lg_model4)

#Removing the "Net worth" column

lg_model5 <- glm(Default~. -`Total liabilities` -`Total income` -Sales -`Net worth`,
                 data = company1, family = binomial())
summary(lg_model5)
vif(lg_model5)

#Removing the "Capital Employed" column

lg_model6 <- glm(Default~. -`Total liabilities` -`Total income` -Sales -`Net worth` -`Capital employed`,
                 data = company1, family = binomial())
summary(lg_model6)
vif(lg_model6)

#Removing "Profit after tax" column

lg_model7 <- glm(Default~. -`Total liabilities` -`Total income` -Sales -`Net worth` -`Capital employed`
                 -`Profit after tax`, data = company1, family = binomial())
summary(lg_model7)
vif(lg_model7)

#Removing "EPS" column

lg_model8 <- glm(Default~. -`Total liabilities` -`Total income` -Sales -`Net worth` -`Capital employed`
                 -`Profit after tax` -EPS, data = company1, family = binomial())
summary(lg_model8)
vif(lg_model8)

#Removing "PBT as % of total income" column

lg_model9 <- glm(Default~. -`Total liabilities` -`Total income` -Sales -`Net worth` -`Capital employed`
                 -`Profit after tax` -EPS -`PBT as % of total income`, data = company1, family = binomial())
summary(lg_model9)
vif(lg_model9)

#Removing "Total capital" column

lg_model10 <- glm(Default~. -`Total liabilities` -`Total income` -Sales -`Net worth` -`Capital employed`
                  -`Profit after tax` -EPS -`PBT as % of total income` -`Total capital`,
                  data = company1, family = binomial())
summary(lg_model10)
vif(lg_model10)

#Removing "PBDITA" column

lg_model11 <- glm(Default~. -`Total liabilities` -`Total income` -Sales -`Net worth` -`Capital employed`
                  -`Profit after tax` -EPS -`PBT as % of total income` -`Total capital` -PBDITA,
                  data = company1, family = binomial())
summary(lg_model11)
vif(lg_model11)

#Removing "Reserves and funds" column

lg_model12 <- glm(Default~. -`Total liabilities` -`Total income` -Sales -`Net worth` 
                  -`Capital employed` -`Profit after tax` 
                  -EPS -`PBT as % of total income` -`Total capital` -PBDITA
                  -`Reserves and funds`,data = company1, family = binomial())
summary(lg_model12)
vif(lg_model12)

#Taking only the significant columns from lg_model12

lg_model13 <- glm(Default~`Cash profit as % of total income`+`Cumulative retained profits`+
                    `TOL/TNW`+`Debt to equity ratio (times)`+`Cash to average cost of sales per day`+
                    `Total term liabilities / tangible net worth`+`Total assets`+
                    `Net fixed assets`,data = company1,
                  family = binomial())
summary(lg_model13)
vif(lg_model13)

#Prediction and Confusion matrix of validation data

tdata <- predict(lg_model13, validation1, type = "response")
t_confmat = table(predicted = ifelse(tdata>0.07,1,0), actual=validation1$Default)
confusionMatrix(t_confmat,positive = "1", mode = "everything")

tdata2 <- predict(lg_model12, validation1, type = "response")
t_confmat2 = table(predicted = ifelse(tdata2>0.07,1,0), actual=validation1$Default)
confusionMatrix(t_confmat2,positive = "1", mode = "everything")

