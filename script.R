rm(list = ls())

library(ggplot2)
library(ggvis)
library(dplyr)
library(plotly)
library(readr)
library(arules)
library(rJava)
library(xlsx)
py <- plotly()

#--------------------------------------------------------------------------------------
#--------------------------------- Customer Lifetime Value-----------------------------

#Function to prepare data
prepareData <- function(data,start,end, id_colname="CustomerID", 
                        date_colname="Date", sales_colname = "Total Sales") {
  
  data <- data[order(data[ , date_colname], decreasing = TRUE), ]
  data <- data[data[ , date_colname] >= start, ]
  data <- data[data[ , date_colname] <= end, ]
  ndata <- data[!duplicated(data[ , id_colname]), ]
  
  Recency <- as.numeric(difftime(end, ndata[ , date_colname], units = "days"))
  ndata <- cbind(ndata, Recency)
  ndata <- ndata[order(ndata[ , date_colname]), ]
  
  tmp <- as.data.frame(table(data[ , id_colname]))
  Frequency <- tmp[ , 2]
  ndata <- cbind(ndata, Frequency)
  
  tmp <- as.data.frame(tapply(data[ , sales_colname], data[ , id_colname], sum))
  Monetary <- tmp[ , 1] / Frequency
  ndata <- cbind(ndata, Monetary)
  
  return(ndata)
}	

#--------------------------------------------------------------------
#function to build the CLV Model

calcCLV <- function(rec, freq, rev, cost, customers, periods, disc, pModel) {
  df <- data.frame(period = c(0), rec = c(rec), freq = c(freq), customers = c(customers), value = c(0))
  
  for (i in 1:periods) {
    backstep <- df[df$period == i-1, ]
    nrow <- nrow(backstep)
    
    for (j in 1:nrow) {
      rec <- backstep[j, ]$rec
      freq <- backstep[j, ]$freq
      customers <- backstep[j, ]$customers
      p <- predict(pModel, data.frame(Recency = rec, Frequency = freq), type = "response")[1]
      buyers <- customers * p
      
      # Predict odds of a "Buy" for this period
      df <- rbind( df, c(i, 0, freq+1, buyers, buyers*(rev-cost) / (1+disc)^i ))
      
      # Predict odds of a "No-Buy" for this period
      df <- rbind( df, c(i, rec+1, freq, customers-buyers, (customers-buyers)*(0-cost) / (1+disc)^i ))
    }
  }
  
  return(sum(df$value))
}

#--------------------------------------------------------------------
#function to get data and calculate parameters

customerLV <- function(filename) {
  
  setwd("/Users/soumy/Desktop/CLV")
  clv_data <-read_csv(filename)
  clv_data  <- as.data.frame(cbind(clv_data [,1], clv_data [,2], clv_data [,4]))
  names(clv_data) <- c("CustomerID", "Date", "Total Sales")
  
  #Set date format, remove sales less than or equal to zero
  clv_data [,2] <- as.Date( as.character(clv_data [,2]), "%Y%m%d")
  clv_data <- clv_data[apply(clv_data[c(3)],1,function(z) !any(z<=0)),] 
  
  #Setting timeline for training data (5 months) 
  start_train <- as.Date("2017-06-01")
  end_train <- as.Date("2017-10-31")
  
  #Setting timeline for testing data (1 month)
  start_test <- as.Date("2017-11-01")
  end_test <- as.Date("2017-11-30")

  #Get training and testing data
  training_data <- prepareData(clv_data, start_train, end_train)					
  test_data <- prepareData(clv_data, start_test, end_test)		
  
  # Set purchasing cycle to 1 month
  test_period <- as.numeric(difftime(end_test, start_test))
  training_data$Recency <- training_data$Recency
  
  # Set Monetary bins size with $10
  breaks <- seq(0, round(max(training_data$Monetary) + 9), by = 10)						
  training_data $Monetary <- as.numeric( cut(training_data$Monetary, breaks, labels = FALSE) ) 
  
  # Add "Buy" / "No Buy" column to data
  Buy <- rep(0, nrow(training_data))
  training_data <- cbind(training_data, Buy)
  
  # Identify customers who purchased during the forecast period
  training_data[training_data$CustomerID %in% test_data$CustomerID, ]$Buy <- 1
  
  # Create CLV Training data
  clv_traindata <- training_data
  
  # Run Logistic Regression model based on  Recency, Frequency, and Monetary trends.
  regression_model_RFM <- glm(Buy ~ Recency + Frequency, data = clv_traindata, family = binomial(link = "logit"))
  

  rec = 0 			# recency state  0
  freq = 1 			# frequency state 1
  Rev = 100 		# expected revenue from customer.
  Cost = 0 		# Associated cost for each potential customer (Buy or No-Buy) per period.
  n = 1 			# customers with same Recency and Frequency
  periods = 3 	# churn period for customers.
  dr = 0.02 		# discount rate
  model = regression_model_RFM 
  
  
  CLV_in_dollars<- calcCLV(rec, freq, Rev, Cost, n, periods, dr, model)
  return(CLV_in_dollars)
}

sprintf("Customer Lifetime Value for Testro-X is %f USD.", customerLV("testrox_clv.csv"))
sprintf("Customer Lifetime Value for Redwood is %f USD.", customerLV("redwood_clv.csv"))
sprintf("Customer Lifetime Value for Floracil50 is %f USD.", customerLV("floracil50_clv.csv"))
sprintf("Customer Lifetime Value for Cortigon is %f USD.", customerLV("cortigon_clv.csv"))
sprintf("Customer Lifetime Value for Sensolin is %f USD.", customerLV("sensolin_clv.csv"))


#--------------------------------------------------------------------------------------
#--------------------------------- Repurchase Rate ------------------------------------

#Function to calculate repurchase rate

repurchaserate  <- function(filename) {
  
  setwd("/Users/soumy/Desktop/repurchase rate")
  sales_data <-read_csv(filename)
  
  #removing nulls and zeros
  sales_data <- sales_data[apply(sales_data[c(3)],1,function(z) !any(z==0)),] 
  
  #Total number of unique purchases
  total_cust <- dim(sales_data)[1]
  
  #Customers where orders > 1, reorders
  rep_data <- sales_data[apply(sales_data[c(3)],1,function(z) !any(z>1)),] 
  
  #Number of customers who reordered
  repeat_cust <- dim(rep_data)[1]
  
  #Repurchase rate
  repeat_rate <- (repeat_cust/total_cust)*100
  repeat_rate <- round(repeat_rate,digits=2)
  return(repeat_rate)
}

sprintf("Repurchase rate for Testro-X is %f %%", repurchaserate("testrox_repo.csv"))
sprintf("Repurchase rate for Redwood is %f %%", repurchaserate("redwood_repo.csv"))
sprintf("Repurchase rate for Floracil50 is %f %%", repurchaserate("floracil50_repo.csv"))
sprintf("Repurchase rate for Cortigon is %f %%", repurchaserate("cortigon_repo.csv"))
sprintf("Repurchase rate for Sensolin is %f %%", repurchaserate("sensolin_repo.csv"))


#--------------------------------------------------------------------------------------
#--------------------------------- Frequently Bought Together -------------------------

#Read data
setwd("/Users/soumy/Desktop/most frequently bought together")
raw_data <-read_csv("mfbt_data.csv")

#Clean data and delete rows with no product ID, delete duplicates
without_na <- raw_data[complete.cases(raw_data[ ,4]),]
without_na <- without_na[,2:4]
without_na  <- without_na[!without_na$`Product ID`==0, ] 
without_na<- unique(without_na)

#prepare data into transactions, accepted format for arules package
transac <- as(split(without_na$`Product title`, without_na$`Order ID`), "transactions")
inspect(transac)


#build association rules with support = 0.005, confidence = 0.05, order by lift
rules <- apriori (transac, parameter = list(supp = 0.005, conf = 0.05, target = "rules")) 
inspect(head(rules, by = "lift"))

#remove redundant rules
subsetRules <- which(colSums(is.subset(rules, rules)) <= 1) 
length(subsetRules)  
rules <- rules[-subsetRules]
tables <- as(rules, "data.frame")

#rules, ordered by confidence
rules_conf <- sort (rules, by="confidence", decreasing=TRUE)
inspect(rules_conf)

#convert to data frame
mrules <- as(rules_conf, "data.frame")

#-----------------------------------------------------------------------------------------

