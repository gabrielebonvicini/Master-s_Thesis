##1 Call the useful library
library(lubridate)
library(readxl)
library(purrr)
library(quantmod)
library(shiny) 
library(kableExtra) #for tables
library(knitr)      #for tables 
library(moments)
library(docstring)

##2 Files import 
##The files were already saved in .xlsx format with all the data ready to be run. This process
##saved time and RAM memory to makes the process faster. 
sd_ENV_app <- read_xlsx("sd_ENV.xlsx")
sd_ESG_app <- read_xlsx("sd_ESG.xlsx")
sd_SOCIAL_app <- read_xlsx("sd_SOCIAL.xlsx")
sd_GOV_app <- read_xlsx("sd_GOV.xlsx")

prices_open_month_app <- read_xlsx("prices_open_month_app.xlsx")
prices_close_month_app <- read_xlsx("prices_close_month_app.xlsx")

bid_month_app <- read_xlsx("bid_month_app.xlsx")
ask_month_app <- read_xlsx("ask_month_app.xlsx")

##3 Set the colnames()
sub <- substr( colnames(bid_month_app), start = 1, stop =7 )
colnames(bid_month_app) <- sub
colnames(ask_month_app) <- sub
colnames(prices_open_month_app) <- sub
colnames(prices_close_month_app) <- sub

##4 Quartile functions 
stocks_upper_quartiles <- function(data, n = 6) {
  
  df <- as.data.frame( matrix(nrow = nrow(data), ncol = ncol(data)))
  colnames(df) <- colnames(data)
  for(r in 1:nrow(data)){
    for(c in 7:ncol(data)){
      if(!is.na(data[r,c]) && data[r,c] > quantile(data[,c], probs = 0.75, type  = 1, na.rm = T)[[1]]) {
        df[r,c] <- data[r,1]
      }
    }
  }
  df <- df[ ,-c(1:n)]          
  return(df)
  #end of function
}


upper_quart_selector <- function(df){
  ## Upper quartile selection #take just the name of the stocks
  try <- stocks_upper_quartiles(df)
  #and create a dataframe with in every column the name of the stocks in the best quartile 
  df_upper_perce <- as.data.frame(matrix(nrow = nrow(try),ncol = ncol(try)))
  colnames(df_upper_perce) <- colnames(try)
  for(i in 1:length(try)){ 
    if( length(try[!is.na(try[,i]),i]) == 0 ){ df_upper_perce[1:length(try[!is.na(try[,i]),i]),i] = NA }
    else { 
      df_upper_perce[1:length(try[!is.na(try[,i]),i]),i] <- cbind(
        try[!is.na(try[,i]),i]
      )
    } #this is the dataframe with the first quartile in every column
    #end of ifelse
  } 
  #end of loop
  return(df_upper_perce)
}


stocks_lower_quartiles <- function(data, n = 6) {
  
  df <- as.data.frame( matrix(nrow = nrow(data), ncol = ncol(data)))
  colnames(df) <- colnames(data)
  for(r in 1:nrow(data)){
    for(c in 7:ncol(data)){
      if(!is.na(data[r,c]) && data[r,c] < quantile(data[,c], probs = 0.25, type  = 1, na.rm = T)[[1]]) {
        df[r,c] <- data[r,1]
      }
    }
  }
  df <- df[ ,-c(1:n)]
  return(df)
}


lower_quart_selector <- function(df){ 
  try <- stocks_lower_quartiles(df)
  #and create a dataframe with in every column the name of the stocks in the best quartile 
  df_lower_perce <- as.data.frame(matrix(nrow = nrow(try),ncol = ncol(try)))
  colnames(df_lower_perce) <- colnames(try)
  for(i in 1:length(try)){ 
    if( length(try[!is.na(try[,i]),i]) == 0 ){ df_lower_perce[1:length(try[!is.na(try[,i]),i]),i] = NA }
    else { 
      df_lower_perce[1:length(try[!is.na(try[,i]),i]),i] <- cbind(
        try[!is.na(try[,i]),i]
      )
    } #this is the dataframe with the first quartile in every column
    #end of ifelse
  } 
  #end of loop
  return(df_lower_perce)
  #end of function
}

stocks_middle_quartiles <- function(data, n = 6) {
  
  df <- as.data.frame( matrix(nrow = nrow(data), ncol = ncol(data)))
  colnames(df) <- colnames(data)
  for(r in 1:nrow(data)){
    for(c in 7:ncol(data)){
      if(!is.na(data[r,c]) && data[r,c] <= quantile(data[,c], probs = 0.75, type  = 1, na.rm = T)[[1]] &
         !is.na(data[r,c]) && data[r,c] >= quantile(data[,c], probs = 0.25, type  = 1, na.rm = T)[[1]]){
        df[r,c] <- data[r,1]
      }
    }
  }
  df <- df[ ,-c(1:n)]
  return(df)
  #end of function
}


middle_quart_selector <- function(df){ 
  try <- stocks_middle_quartiles(df)
  #and create a dataframe with in every column the name of the stocks in the best quartile 
  df_middle_perce <- as.data.frame(matrix(nrow = nrow(try),ncol = ncol(try)))
  colnames(df_middle_perce) <- colnames(try)
  for(i in 1:length(try)){ 
    if( length(try[!is.na(try[,i]),i]) == 0 ){ df_middle_perce[1:length(try[!is.na(try[,i]),i]),i] = NA }
    else { 
      df_middle_perce[1:length(try[!is.na(try[,i]),i]),i] <- cbind(
        try[!is.na(try[,i]),i]
      )
    } #this is the dataframe with the first quartile in every column
    #end of ifelse
  } 
  #end of loop
  return(df_middle_perce)
  #end of function 
}

##5 trade analysis function 
trade_analysis <- function(
    quartile = c("upper", "lower", "middle"), pillar = c("ESG", "ENV" , "SOCIAL", "GOV")
){ 
 
  ##1 If-else to decide the dataframe to use
  ##Choose the pillar
  if(pillar == "ESG"){ df <- sd_ESG_app } 
  else if (pillar == "SOCIAL") { df <- sd_SOCIAL_app}
  else if (pillar == "ENV"){ df <- sd_ENV_app} 
  else if (pillar == "GOV") { df <- sd_GOV_app}
  
  ##Choose the quartile
  if(quartile == "upper") { df <- upper_quart_selector(df)} 
  else if (quartile == "lower"){ df <- lower_quart_selector(df)}
  else if (quartile == "middle"){ df <- middle_quart_selector(df)}
  
  ##2 Dataframe construction 
  quartile_port <- data.frame( #list of stocks to invest in
    matrix(nrow = nrow(df), ncol = ncol(prices_open_month))
  )
  colnames(quartile_port) <- colnames(prices_open_month)
  
  ##Create the data.frame with the columns  
  for(c in 4:ncol(quartile_port)){
    quartile_port[ ,c] <- df[ ,colnames(df)==colnames(quartile_port[c])]
  }
  quartile_port <- quartile_port[ , -c(1:3)] #DON'T run again 
  
  ##3 Buy and Sell variables definition 
  ##Empty elements
  buy <- list()
  sell <- list()
  vec_of_stocks <- list()
  
  ##For loop to assign the stocks 
  for(i in 1:length(quartile_port)){
    vec_of_stocks[[i]] <- quartile_port[ ,i] 
    buy[[i]] <- prices_open_month[which(prices_open_month$Stock %in% vec_of_stocks[[i]]),i+3]
    sell[[i]] <- prices_close_month[which(prices_close_month$Stock %in% vec_of_stocks[[i]]),i+3]
  }
  
  ##Transform them into a list object 
  for(i in 1:length(buy)){
    buy[i] <- as.list(buy[[i]][1])
    sell[i] <- as.list(sell[[i]][1])
  }
  
  ##4 Returns of the strategy 
  ##Empty varaibles 
  percentage <- buy
  len <- numeric(length(buy))
  total_percentage <- numeric(length = length(buy))
  total_ret_money <- numeric(length(buy))
  w_single <- numeric (length = length(buy))
  
  ## Compute the returns  
  for(i in 1:length(buy)){
    if (length(buy[[i]]) > 0){ #condition to skip NULL and NA elements in buy
      
      ##4.1 collect in one vector the lenght of the number of stocks in the quartile in each period
      len[i] <- length(buy[[i]])
      
      ##4.2 You have the total wealth taht you will equally split among stocks 
      w <- 100000
      
      ##4.3 Compute the returns(percentages) with a loop
      for(l in 1:len[i]) {  #with 'l' changing every day 'i' and ranging for every stocks. 
        #This is to account for the number of stocks changing every month
        w_single[i] <- w / length(buy[[i]][[l]]) #total wealth / number of stocks in every period
        percentage[[i]][l] <-  ( as.numeric(buy[[i]][[l]]) -  #opening price of stock 'l' in date 'i' 
                                   as.numeric(sell[[i]][[l]]) ) / #closing price of stock 'l' in date 'i'
          ( as.numeric(buy[[i]][[l]]) )  #obtain the percentage 
        ##End of second loop
      }
      
      ##4.4  Find the cumulated percentage for the day 'i' and multiply it for w_single to find the $ amount
      total_percentage[i] <- sum( as.numeric( #cumulated percentage in day 'i'
        percentage[[i]][[l]] )   #percentage in day 'i' of stock 'l' + perce day 'i' stock 'l+1' ....
        , na.rm = T)
      total_ret_money[i] <- total_percentage[i] * w_single[i]
      ##4.5 compute the transaction costs (in another loop otherwise is too slow)
      
      ##End of if
    } 
    
    ##Else if buy[[i]] has no observation -- > substitute with 0 and non NA (to not create problems)
    else { 
      percentage[[i]] <- 0
      ##End of else
    }
    ##End of first loop
  }
  
  ##5 Transaction costs of the strategy implementation
  ##Empty elements
  bid <- list()
  ask <- list()
  vec_of_stocks <- list()
  
  ##Assign the name of the stocks to bid and ask  
  for(i in 1:length(quartile_port)){
    vec_of_stocks[[i]] <- quartile_port[ ,i]
    bid[[i]] <- bid_month[which(bid_month$Stock %in% vec_of_stocks[[i]]),i+3]
    ask[[i]] <- ask_month[which(ask_month$Stock %in% vec_of_stocks[[i]]),i+3]
  }
  
  ##Transform them into a list object 
  for(i in 1:length(bid)){
    bid[i] <- as.list(bid[[i]][1])
    ask[i] <- as.list(ask[[i]][1])
  }
  
  ##Empty elements 
  tcost_percentage <- numeric(length(bid))
  tcost_money <- numeric(length(bid))
  percentage_cost <- bid  
  len <- numeric( (length(bid)) )
  w_single <- numeric(length = length(bid))
  
  ##Transaction costs of the portfolio 
  for(i in 1:length(bid)){
    if(length(bid[[i]])>0){ 
      
      ##5.1 collect in one vector the lenght of the number of stocks in the quartile in each period
      len[i] <- length(bid[[i]])
      
      ##5.2 You have the total wealth taht you will equally split among stocks 
      w <- 100000
      
      ##5.3 compute the transaction costs (percentage)
      for(l in 1:len[i]) { 
        w_single[i] <- w / length(bid[[i]]) #number of stocks in every period
        percentage_cost[[i]][l] <-  ( as.numeric(ask[[i]][l]) -  #ask of stock 'l' in date 'i'  
                                        as.numeric(bid[[i]][l]) ) / #bid of stock 'l' in date 'i'
          ( ( as.numeric(bid[[i]][l]) + 
                as.numeric(ask[[i]][l]) ) /2 ) #mid quote to obtain the %bid-ask
        ##End of second loop
      }
      
      ##5.4 Multiply the wealth invested in every stock times the %Bid-Ask spread
      tcost_percentage[i] <- sum ( as.numeric(
        percentage_cost[[i]] ) , na.rm = T
      ) 
      tcost_money[i] <- tcost_percentage[i] * w_single[i]
      ##End of first loop 
    }
    
    ##Else if buy[[i]] has no observation -- > substitute with 0 and non NA (to not create problems)
    else {
      percentage_cost[[i]] <- 0
      ##End of else
    }
    ##End of if 
  } 
  
  ##6 Final dataset creation 
  ##Transaction costs in Percentage
  tcost_percentage <- as.data.frame( tcost_percentage ) #in data.frame format
  rownames(tcost_percentage) <- colnames( prices_day )[4:ncol(prices_day)] #rownames 
  colnames(tcost_percentage) <- "Cost paid per day (percentage)" #column name
  
  ##Transaction costs in Dollar 
  tcost_money <- as.data.frame( tcost_money )
  rownames(tcost_money) <- colnames( prices_day )[4:ncol(prices_day)]
  colnames(tcost_money) <- "Cost paid per day (dollar)"
  
  ##Returns in percentage
  total_percentage <- as.data.frame( total_percentage )
  rownames(total_percentage) <- colnames( prices_day )[4:ncol(prices_day)]
  colnames(total_percentage) <- "Return per day (percentage)"
  
  ##Returns in dollar
  total_ret_money <- as.data.frame( total_ret_money )
  rownames(total_ret_money) <- colnames( prices_day )[4:ncol(prices_day)]
  colnames(total_ret_money) <- "Cost paid per day (dollar)"
  
  ##7 Return a list of dataframes 
  return( list (tcost_percentage = tcost_percentage, tcost_dollar = tcost_money, 
                returns_perce = total_percentage, returns_dollar = total_ret_money) )
  ##End of function 
}

