# Thesis code 

# Working Directory ----------------------------------------------------------

getwd()

# Library --------------------------------------------------------------------


library(lubridate)
library(readxl)
library(purrr)
library(quantmod)
library(shiny) 
library(kableExtra) #for tables
library(knitr)      #for tables 
library(moments)
library(docstring)   #to write a documentation to the built-in functions

# Section 1: Download and clean the data --------------------------------------- 

## 1.1:  SP500_RET returns as a control variable ------------------------------

SP500_RET = getSymbols("^GSPC", 
                       src = "yahoo",
                       from = "2014-12-01",
                       to = "2023-05-01",
                       auto.assign = T) #%>% 
#map(~monthlyReturn( Ad(get(.)))) %>% reduce(merge)
SP500_RET = GSPC
rm(GSPC)
SP500_RET <- monthlyReturn(Ad(get("SP500_RET")))
SP500_RET <- as.data.frame(SP500_RET)
write.csv(SP500_RET,  "C:\\Users\\gabri\\...\\SP500_RET.csv")

# In excel merge the two dataset into final_dataset 

## 1.2: Import the dataset (downloaded from BBG) ------------------------------

data_full <- read_xlsx("C:\\Users\\gabri\\...\\Final_dataset.xlsx",
                       sheet = "Final")

### 1.2.1: Data cleaning -----------------------------------------

#### 1.2.1.1: colnames ---------------------------------------------

colnames(data_full) #Name of the columns are wrong
## Store colnames into a temp file 
temp <- colnames( data_full[7:length(data_full)] ) 
temp <- substring(c(temp), first = 1, last = 7)  #To remove the ...7 at the end of the columns
## Assign the new names to the data_full columns
colnames(data_full)[7:length(data_full)] <- temp #Assign the new names 
colnames(data_full) #Assess the result
remove(temp) #Remove the temp file 

#### 1.2.1.2: Substitute #NA#NA of Excel with NAs ------------------------------- 

data_full[data_full=="#N/A N/A"] <- NA 

#### 1.2.1.3: Take only the percentile rank --------------------------------------- 

## Take the p_rank dataset
p_rank <- cbind(data_full[ , 1:6], data_full[ , c( data_full[1,] ) == "Percentile rank"] )

## If you need the score dataset... 
#score <- cbind(data_full[ , 1:6], data_full[ , c( data_full[1,] ) == "Score"] )

## Remove the first row (DON'T RUN TWICE)
p_rank <- p_rank[ -c(1), ] 
#score <- score[-c(1),]


# Section 2: Summary Statistics --------------------------------------------

## 2.1: Bloomberg ESG --------------------------------- 

## Take the BBG ESG ratings from the full dataset
BBG <- as.data.frame( p_rank[p_rank$Provider == "BBG" & 
                               p_rank$Field =="ESG"  ,] )   
## Remove the empty lines (DON'T RUN TWICE)
BBG <- BBG[ -c(1:3),] 

num_col <- function(x,n){ 
  #' Column into numeric
  #' 
  #' This function allows to transform a column of a dataset into a numeric format, omitting
  #' NAs values 
  #' 
  #' @param x the dataset in which to perform the transformation 
  #' 
  #' @param n the column in which to perform the transormation
  #' 
  as.numeric( na.omit( x[,n]) )  
} 

### 2.1.1: Average ------------------------------

## Create a vector with "i" elements. Every element "i" is the average of the scores in 
## column "i" (indicating one month)
avg_vec <- c()
for( i in 1 : length(BBG)-6) { #The first 6 columns are not dates 
  avg_vec[i] <- mean(num_col(BBG,i+6))  
}

## Average all the values to obtain the total average 
BBG_ESG_avg <- mean(avg_vec) 
rm(avg_vec ,i) #remove unseseful elements 

### 2.1.2: Standard deviation ---------------------------

## Create a vector with "i" elements. Every element "i" is the standard deviation of the scores in 
## column "i" (indicating one month) 
sd_vec <- c()
for(i in 1: length(BBG)-6){ 
  sd_vec[i] <- sd(num_col(BBG, i+6))
}

## Average all the values to obtain the total standard deviation
BBG_ESG_sd <- mean(sd_vec)
rm(sd_vec, i) # remove unseful varaibles 

### 2.1.3: Number of observations -----------------------------------------------

## Create a vector with "i" elements. Every element "i" is the number of observations of the scores in 
## column "i" (indicating one month) 
n_vec <- c()
for(i in 1: length(BBG)-6){ 
  n_vec[i] <- sum ( length(num_col(BBG,i+6)) )
}

## Sum all the values to obtain the total number of observations
BBG_ESG_obs <- sum(n_vec)
rm(n_vec,i)

## 2.2: Bloomberg Environment ----------------------------------------


## From here oneard, the above steps presented in Section 2.1 are reiterated for every pillar 
## and for every provider
BBG <- as.data.frame( p_rank[p_rank$Provider == "BBG" & 
                               p_rank$Field =="ENV"  ,] ) 

avg_vec <- c()
for( i in 1 : length(BBG)-6) {
  avg_vec[i] <- mean(num_col(BBG,i+6))  
}
BBG_ENV_avg <- mean(avg_vec) 
rm(avg_vec ,i) 

sd_vec <- c()
for(i in 1: length(BBG)-6){ 
  sd_vec[i] <- sd(num_col(BBG, i+6))
}
BBG_ENV_sd <- mean(sd_vec)
rm(sd_vec, i)

n_vec <- c()
for(i in 1: length(BBG)-6){ 
  n_vec[i] <- sum ( length(num_col(BBG,i+6)) )
}
BBG_ENV_obs <- sum(n_vec)
rm(n_vec,i)

##2.3: Bloomberg Social --------------------------------------

BBG <- as.data.frame( p_rank[p_rank$Provider == "BBG" & 
                               p_rank$Field =="SOCIAL"  ,] ) 

avg_vec <- c()
for( i in 1 : length(BBG)-6) {
  avg_vec[i] <- mean(num_col(BBG,i+6))  
}
BBG_SOCIAL_avg <- mean(avg_vec) 
rm(avg_vec ,i)

sd_vec <- c()
for(i in 1: length(BBG)-6){ 
  sd_vec[i] <- sd(num_col(BBG, i+6))
}
BBG_SOCIAL_sd <- mean(sd_vec)
rm(sd_vec, i)

n_vec <- c()
for(i in 1: length(BBG)-6){ 
  n_vec[i] <- sum ( length(num_col(BBG,i+6)) )
}
BBG_SOCIAL_obs <- sum(n_vec)
rm(n_vec,i)

##2.4: BLoomberg Governance -------------------------------------------------------

BBG <- as.data.frame( p_rank[p_rank$Provider == "BBG" & 
                               p_rank$Field =="GOV"  ,] ) 

avg_vec <- c()
for( i in 1 : length(BBG)-6) {
  avg_vec[i] <- mean(num_col(BBG,i+6))  
}
BBG_GOV_avg <- mean(avg_vec)
rm(avg_vec ,i)

sd_vec <- c()
for(i in 1: length(BBG)-6){ 
  sd_vec[i] <- sd(num_col(BBG, i+6))
}
BBG_GOV_sd <- mean(sd_vec)
rm(sd_vec, i)

n_vec <- c()
for(i in 1: length(BBG)-6){ 
  n_vec[i] <- sum ( length(num_col(BBG,i+6)) )
}
BBG_GOV_obs <- sum(n_vec)
rm(n_vec,i)

##2.5: S&P ESG ---------------------------------------

## Missing data

##2.6: S&P Environmental -------------------------------------

SP <- as.data.frame( p_rank[p_rank$Provider == "S&P" & 
                              p_rank$Field =="ENV"  ,] )

avg_vec <- c()
for( i in 1 : length(SP)-6) {
  avg_vec[i] <- mean(num_col(SP,i+6))  
}
SP_ENV_avg <- mean(na.omit( avg_vec) )
rm(avg_vec ,i) 

sd_vec <- c()
for(i in 1: length(SP)-6){ 
  sd_vec[i] <- sd(num_col(SP, i+6))
}
SP_ENV_sd <- mean(na.omit (sd_vec) ) 
rm(sd_vec, i)

n_vec <- c()
for(i in 1: length(SP)-6){ 
  n_vec[i] <- sum ( length(num_col(SP,i+6)) )
}
SP_ENV_obs <- sum( n_vec )
rm(n_vec,i)

##2.7: S&P Governance -------------------------------------------

SP <- as.data.frame( p_rank[p_rank$Provider == "S&P" & 
                              p_rank$Field =="GOV"  ,] )
avg_vec <- c()
for( i in 1 : length(SP)-6) {
  avg_vec[i] <- mean(num_col(SP,i+6))  
}
SP_GOV_avg <- mean(na.omit( avg_vec) )
rm(avg_vec ,i) 

sd_vec <- c()
for(i in 1: length(SP)-6){ 
  sd_vec[i] <- sd(num_col(SP, i+6))
}
SP_GOV_sd <- mean(na.omit (sd_vec) ) 
rm(sd_vec, i)

n_vec <- c()
for(i in 1: length(SP)-6){ 
  n_vec[i] <- sum ( length(num_col(SP,i+6)) )
}
SP_GOV_obs <- sum( n_vec )
rm(n_vec,i) 

##2.8: S&P Social
SP <- as.data.frame( p_rank[p_rank$Provider == "S&P" & 
                              p_rank$Field =="SOCIAL"  ,] )

avg_vec <- c()
for( i in 1 : length(SP)-6) {
  avg_vec[i] <- mean(num_col(SP,i+6))  
}
SP_SOCIAL_avg <- mean(na.omit( avg_vec) )  
rm(avg_vec ,i)

sd_vec <- c()
for(i in 1: length(SP)-6){ 
  sd_vec[i] <- sd(num_col(SP, i+6))
}
SP_SOCIAL_sd <- mean(na.omit (sd_vec) ) 
rm(sd_vec, i)

n_vec <- c()
for(i in 1: length(SP)-6){ 
  n_vec[i] <- sum ( length(num_col(SP,i+6)) )
}
SP_SOCIAL_obs <- sum( n_vec )
rm(n_vec,i)

##2.8: IdealRatings ESG ----------------------------------

IDEAL <- as.data.frame( p_rank[p_rank$Provider == "IDEAL RATING" & 
                                 p_rank$Field =="ESG"  ,] )

avg_vec <- c()
for( i in 1 : length(IDEAL)-6) {
  avg_vec[i] <- mean(num_col(IDEAL,i+6))  
}
IDEAL_ESG_avg <- mean(na.omit( avg_vec) ) 
rm(avg_vec ,i) 

sd_vec <- c()
for(i in 1: length(IDEAL)-6){ 
  sd_vec[i] <- sd(num_col(IDEAL, i+6))
}
IDEAL_ESG_sd <- mean(na.omit (sd_vec) ) 
rm(sd_vec, i)

n_vec <- c()
for(i in 1: length(IDEAL)-6){ 
  n_vec[i] <- sum ( length(num_col(IDEAL,i+6)) )
}
IDEAL_ESG_obs <- sum( n_vec )
rm(n_vec,i)


##2.9: Arabesque ESG ------------------------------------

ARABESQUE <- as.data.frame( p_rank[p_rank$Provider == "ARABESQUE" & 
                                     p_rank$Field =="ESG"  ,] )

avg_vec <- c()
for( i in 1 : length(ARABESQUE)-6) {
  avg_vec[i] <- mean(num_col(ARABESQUE,i+6))  
}
ARABESQUE_ESG_avg <- mean(na.omit( avg_vec) )
rm(avg_vec ,i) 

sd_vec <- c()
for(i in 1: length(ARABESQUE)-6){ 
  sd_vec[i] <- sd(num_col(ARABESQUE, i+6))
}
ARABESQUE_ESG_sd <- mean(na.omit (sd_vec) ) 
rm(sd_vec, i)

n_vec <- c()
for(i in 1: length(ARABESQUE)-6){ 
  n_vec[i] <- sum ( length(num_col(ARABESQUE,i+6)) )
}
ARABESQUE_ESG_obs <- sum( n_vec )
rm(n_vec,i)

##2.10 Arabesque Social -------------------------------------------------

ARABESQUE <- as.data.frame( p_rank[p_rank$Provider == "ARABESQUE" & 
                                     p_rank$Field =="SOCIAL"  ,] )

avg_vec <- c()
for( i in 1 : length(ARABESQUE)-6) {
  avg_vec[i] <- mean(num_col(ARABESQUE,i+6))  
}
ARABESQUE_SOCIAL_avg <- mean(na.omit( avg_vec) )
rm(avg_vec ,i) 

sd_vec <- c()
for(i in 1: length(ARABESQUE)-6){ 
  sd_vec[i] <- sd(num_col(ARABESQUE, i+6))
}
ARABESQUE_SOCIAL_sd <- mean(na.omit (sd_vec) ) 
rm(sd_vec, i)

n_vec <- c()
for(i in 1: length(ARABESQUE)-6){ 
  n_vec[i] <- sum ( length(num_col(ARABESQUE,i+6)) )
}
ARABESQUE_SOCIAL_obs <- sum( n_vec )
rm(n_vec,i)

##2.11: Arabesque Governance ---------------------------------------------------

ARABESQUE <- as.data.frame( p_rank[p_rank$Provider == "ARABESQUE" & 
                                     p_rank$Field =="GOV"  ,] )

avg_vec <- c()
for( i in 1 : length(ARABESQUE)-6) {
  avg_vec[i] <- mean(num_col(ARABESQUE,i+6))  
}
ARABESQUE_GOV_avg <- mean(na.omit( avg_vec) )
rm(avg_vec ,i) 

sd_vec <- c()
for(i in 1: length(ARABESQUE)-6){ 
  sd_vec[i] <- sd(num_col(ARABESQUE, i+6))
}
ARABESQUE_GOV_sd <- mean(na.omit (sd_vec) ) 
rm(sd_vec, i)

n_vec <- c()
for(i in 1: length(ARABESQUE)-6){ 
  n_vec[i] <- sum ( length(num_col(ARABESQUE,i+6)) )
}
ARABESQUE_GOV_obs <- sum( n_vec )
rm(n_vec,i)

##2.12 Arabesque Environmental ------------------------------------------------

ARABESQUE <- as.data.frame( p_rank[p_rank$Provider == "ARABESQUE" & 
                                     p_rank$Field =="ENV"  ,] )

avg_vec <- c()
for( i in 1 : length(ARABESQUE)-6) {
  avg_vec[i] <- mean(num_col(ARABESQUE,i+6))  
}
ARABESQUE_ENV_avg <- mean(na.omit( avg_vec) )
rm(avg_vec ,i) 

sd_vec <- c()
for(i in 1: length(ARABESQUE)-6){ 
  sd_vec[i] <- sd(num_col(ARABESQUE, i+6))
}
ARABESQUE_ENV_sd <- mean(na.omit (sd_vec) ) 
rm(sd_vec, i)

n_vec <- c()
for(i in 1: length(ARABESQUE)-6){ 
  n_vec[i] <- sum ( length(num_col(ARABESQUE,i+6)) )
}
ARABESQUE_ENV_obs <- sum( n_vec )
rm(n_vec,i)

##2.13: Refinitiv ESG ------------------------------------------------

REFINITIV <- as.data.frame( p_rank[p_rank$Provider == "REFINITIV" & 
                                     p_rank$Field =="ESG"  ,] )

avg_vec <- c()
for( i in 1 : length(REFINITIV)-6) {
  avg_vec[i] <- mean(num_col(REFINITIV,i+6))  
}
REFINITIV_ESG_avg <- mean(na.omit( avg_vec) )  #check why it is not 0.5 in the excel
rm(avg_vec ,i)

sd_vec <- c()
for(i in 1: length(REFINITIV)-6){ 
  sd_vec[i] <- sd(num_col(REFINITIV, i+6))
}
REFINITIV_ESG_sd <- mean(na.omit (sd_vec) ) 
rm(sd_vec, i)

n_vec <- c()
for(i in 1: length(REFINITIV)-6){ 
  n_vec[i] <- sum ( length(num_col(REFINITIV,i+6)) )
}
REFINITIV_ESG_obs <- sum( n_vec )
rm(n_vec,i)

##2.14: Refinitiv Social ----------------------------------------

REFINITIV <- as.data.frame( p_rank[p_rank$Provider == "REFINITIV" & 
                                     p_rank$Field =="SOCIAL"  ,] )

avg_vec <- c()
for( i in 1 : length(REFINITIV)-6) {
  avg_vec[i] <- mean(num_col(REFINITIV,i+6))  
}
REFINITIV_SOCIAL_avg <- mean(na.omit( avg_vec) )
rm(avg_vec ,i) 

sd_vec <- c()
for(i in 1: length(REFINITIV)-6){ 
  sd_vec[i] <- sd(num_col(REFINITIV, i+6))
}
REFINITIV_SOCIAL_sd <- mean(na.omit (sd_vec) ) 
rm(sd_vec, i)

n_vec <- c()
for(i in 1: length(REFINITIV)-6){ 
  n_vec[i] <- sum ( length(num_col(REFINITIV,i+6)) )
}
REFINITIV_SOCIAL_obs <- sum( n_vec )
rm(n_vec,i)

##2.15: Refinitiv Governance ------------------------------------------

REFINITIV <- as.data.frame( p_rank[p_rank$Provider == "REFINITIV" & 
                                     p_rank$Field =="GOV"  ,] )

avg_vec <- c()
for( i in 1 : length(REFINITIV)-6) {
  avg_vec[i] <- mean(num_col(REFINITIV,i+6))  
}
REFINITIV_GOV_avg <- mean(na.omit( avg_vec) )
rm(avg_vec ,i) 

sd_vec <- c()
for(i in 1: length(REFINITIV)-6){ 
  sd_vec[i] <- sd(num_col(REFINITIV, i+6))
}
REFINITIV_GOV_sd <- mean(na.omit (sd_vec) ) 
rm(sd_vec, i)

n_vec <- c()
for(i in 1: length(REFINITIV)-6){ 
  n_vec[i] <- sum ( length(num_col(REFINITIV,i+6)) )
}
REFINITIV_GOV_obs <- sum( n_vec )
rm(n_vec,i)

##2.16: Refinitiv Environmental  ------------------------------------------------

REFINITIV <- as.data.frame( p_rank[p_rank$Provider == "REFINITIV" & 
                                     p_rank$Field =="ENV"  ,] )

avg_vec <- c()
for( i in 1 : length(REFINITIV)-6) {
  avg_vec[i] <- mean(num_col(REFINITIV,i+6))  
}
REFINITIV_ENV_avg <- mean(na.omit( avg_vec) )
rm(avg_vec ,i) 

sd_vec <- c()
for(i in 1: length(REFINITIV)-6){ 
  sd_vec[i] <- sd(num_col(REFINITIV, i+6))
}
REFINITIV_ENV_sd <- mean(na.omit (sd_vec) ) 
rm(sd_vec, i)

n_vec <- c()
for(i in 1: length(REFINITIV)-6){ 
  n_vec[i] <- sum ( length(num_col(REFINITIV,i+6)) )
}
REFINITIV_ENV_obs <- sum( n_vec )
rm(n_vec,i)

# Section 3: Correlation analysis --------------------------------------------


##3.1: Method 1 -----------------------------------------------------------

#cor_data <- as.data.frame(c())
#cor_data[1,"Stock"] <- p_rank[1,"Stock"]
#cor_data[1,"Date"] <- colnames(p_rank[7])
#cor_data[1,"Field"] <- p_rank[1,"Field"]
#cor_data[1,"BBG"] <- p_rank[p_rank$Provider == "BBG",colnames(p_rank[7])][1]
#cor_data[1,"IDEAL"] <- p_rank[p_rank$Provider == "IDEAL",colnames(p_rank[7])][1]

#cor_data[2,"Stock"] <- p_rank[1,"Stock"]
#cor_data[2,"Date"] <- colnames(p_rank[8])
#cor_data[2,"Field"] <- p_rank[1,"Field"]
#cor_data[2,"BBG"] <- p_rank[p_rank$Provider == "BBG",colnames(p_rank[8])][2]
#cor_data[2,"IDEAL"] <- p_rank[p_rank$Provider == "IDEAL",colnames(p_rank[8])][2]

#...
#cor_data[3,"Stock"] <- p_rank[2,"Stock"]
#cor_data[3,"Date"] <- colnames(p_rank[7])
#cor_data[3,"Field"] <- p_rank[2,"Field"]
#cor_data[3,"BBG"] <- p_rank[p_rank$Provider == "BBG",colnames(p_rank[7])][3]
#cor_data[3,"IDEAL"] <- p_rank[p_rank$Provider == "IDEAL",colnames(p_rank[7])][3]

# translated in loop
#cor_data <- as.data.frame(c())
#for(i in 1: nrow(p_rank)){
#  for( j in 7 : length(p_rank)){
#    for(n in 1:nrow(p_rank)){
#  cor_data[i,"Stock"] <- p_rank[n,"Stock"]
#  cor_data[i,"Date"] <- colnames(p_rank[j])
#  cor_data[i,"Field"] <- p_rank[n,"Field"]
#  cor_data[i,"BBG"] <- p_rank[p_rank$Provider == "BBG",colnames(p_rank[j])][i]
#  cor_data[i,"IDEAL"] <- p_rank[p_rank$Provider == "IDEAL",colnames(p_rank[j])][i]
#  }
#}
#}

# Or ... 

##3.2: Method 2 --------------------------------------------------------------

###3.2.1: cor between BBG and IDEAl (ESG) -------------------------------------

## The for loop aims to select the data of the two providers and merge them (by ISIN) into a 
## new matrix.

try <- c()
cor_vec <- c()
for(i in 7 : length(p_rank)){ 
  try <- na.omit( merge ( #Create a merged data frame without NAs
    cbind( ISIN = p_rank[p_rank$Provider == "BBG" & p_rank$Field == "ESG" , "ISIN" ], #add A column with ISIN 
           p_rank[p_rank$Provider =="BBG" & p_rank$Field == "ESG" , i ]), #select the first provider ESG ratings
    cbind( ISIN = p_rank[p_rank$Provider =="IDEAL RATING" & p_rank$Field == "ESG" , "ISIN"], #add a column with ISIN
           p_rank[p_rank$Provider =="IDEAL RATING" & p_rank$Field == "ESG" , i] ), #select the second provider ESG ratings
    by = "ISIN") ) 
  ## Compute the correlation at each point in time (every month)
  cor_vec[i] <- cor( as.matrix( cbind( as.numeric(try[ ,2]) , as.numeric(try[ ,3]) ) ) )[2,1]
## End of loop
  }

avg_cor_BBG_IDEAL_ESG <- mean( na.omit( cor_vec) )
rm(try, i , cor_vec)

###3.2.2: cor between BBG and Arabesque (ESG) ----------------------------------- 

## The process is the same abovementioned, therefore the code is not commented. 
## In section 3.3 a faster method is presented

try <- c()
cor_vec <- c()
for(i in 7 : length(p_rank)){ 
  try <- na.omit( merge ( 
    cbind( ISIN = p_rank[p_rank$Provider == "BBG" & p_rank$Field == "ESG" , "ISIN" ], 
           p_rank[p_rank$Provider =="BBG" & p_rank$Field == "ESG" , i ]),
    cbind( ISIN = p_rank[p_rank$Provider =="ARABESQUE" & p_rank$Field == "ESG" , "ISIN"], 
           p_rank[p_rank$Provider =="ARABESQUE" & p_rank$Field == "ESG" , i] ), 
    by = "ISIN") ) 
  cor_vec[i] <- cor( as.matrix( cbind( as.numeric(try[ ,2]) , as.numeric(try[ ,3]) ) ) )[2,1]
}

avg_cor_BBG_ARABESQUE_ESG <- mean( na.omit( cor_vec) )
rm(try, i , cor_vec)


### 3.2.3: cor between BBG and S&P (ESG) ----------------------------------------
try <- c()
cor_vec <- c()
for(i in 7 : length(p_rank)){ 
  try <- na.omit( merge ( 
    cbind( ISIN = p_rank[p_rank$Provider == "BBG" & p_rank$Field == "ESG" , "ISIN" ], 
           p_rank[p_rank$Provider =="BBG" & p_rank$Field == "ESG" , i ]),
    cbind( ISIN = p_rank[p_rank$Provider =="S&P" & p_rank$Field == "ESG" , "ISIN"], 
           p_rank[p_rank$Provider =="S&P" & p_rank$Field == "ESG" , i] ), 
    by = "ISIN") ) 
  cor_vec[i] <- cor( as.matrix( cbind( as.numeric(try[ ,2]) , as.numeric(try[ ,3]) ) ) )[2,1]
}

avg_cor_BBG_SP_ESG <- mean( na.omit(as.numeric( cor_vec) ) )
rm(try, i , cor_vec)

###3.2.4: cor between ARABESQUE and IDEAL (ESG) --------------------------------

try <- c()
cor_vec <- c()
for(i in 7 : length(p_rank)){ 
  try <- na.omit( merge ( 
    cbind( ISIN = p_rank[p_rank$Provider == "ARABESQUE" & p_rank$Field == "ESG" , "ISIN" ], 
           p_rank[p_rank$Provider =="ARABESQUE" & p_rank$Field == "ESG" , i ]),
    cbind( ISIN = p_rank[p_rank$Provider =="IDEAL RATING" & p_rank$Field == "ESG" , "ISIN"], 
           p_rank[p_rank$Provider =="IDEAL RATING" & p_rank$Field == "ESG" , i] ), 
    by = "ISIN") ) 
  cor_vec[i] <- cor( as.matrix( cbind( as.numeric(try[ ,2]) , as.numeric(try[ ,3]) ) ) )[2,1]
}

avg_cor_ARABESQUE_IDEAL_ESG <- mean( na.omit( cor_vec) )
rm(try, i , cor_vec)

###3.2.5: cor between ARABESQUE and S&P (ESG) ---------------------------------------------------

try <- c()
cor_vec <- c()
for(i in 7 : length(p_rank)){ 
  try <- na.omit( merge ( 
    cbind( ISIN = p_rank[p_rank$Provider == "ARABESQUE" & p_rank$Field == "ESG" , "ISIN" ], 
           p_rank[p_rank$Provider =="ARABESQUE" & p_rank$Field == "ESG" , i ]),
    cbind( ISIN = p_rank[p_rank$Provider =="S&P" & p_rank$Field == "ESG" , "ISIN"], 
           p_rank[p_rank$Provider =="S&P" & p_rank$Field == "ESG" , i] ), 
    by = "ISIN") ) 
  cor_vec[i] <- cor( as.matrix( cbind( as.numeric(try[ ,2]) , as.numeric(try[ ,3]) ) ) )[2,1]
}

avg_cor_ARABESQUE_SP_ESG <- mean( na.omit( cor_vec) )
rm(try, i , cor_vec)

###3.2.6: cor between S&P and Ideal rating (ESG) -------------------------------------------

try <- c()
cor_vec <- c()
for(i in 7 : length(p_rank)){ 
  try <- na.omit( merge ( 
    cbind( ISIN = p_rank[p_rank$Provider == "IDEAL RATING" & p_rank$Field == "ESG" , "ISIN" ], 
           p_rank[p_rank$Provider =="IDEAL RATING" & p_rank$Field == "ESG" , i ]),
    cbind( ISIN = p_rank[p_rank$Provider =="S&P" & p_rank$Field == "ESG" , "ISIN"], 
           p_rank[p_rank$Provider =="S&P" & p_rank$Field == "ESG" , i] ), 
    by = "ISIN") ) 
  cor_vec[i] <- cor( as.matrix( cbind( as.numeric(try[ ,2]) , as.numeric(try[ ,3]) ) ) )[2,1]
}

avg_cor_IDEAL_SP_ESG <- mean( na.omit( cor_vec) )
rm(try, i , cor_vec)

###3.2.7: cor between BBG and ARABESQUE (ENV) ------------------------------------------------

try <- c()
cor_vec <- c()
for(i in 7 : length(p_rank)){ 
  try <- na.omit( merge ( 
    cbind( ISIN = p_rank[p_rank$Provider == "BBG" & p_rank$Field == "ENV" , "ISIN" ], 
           p_rank[p_rank$Provider =="BBG" & p_rank$Field == "ENV" , i ]),
    cbind( ISIN = p_rank[p_rank$Provider =="ARABESQUE" & p_rank$Field == "ENV" , "ISIN"], 
           p_rank[p_rank$Provider =="ARABESQUE" & p_rank$Field == "ENV" , i] ), 
    by = "ISIN") ) 
  cor_vec[i] <- cor( as.matrix( cbind( as.numeric(try[ ,2]) , as.numeric(try[ ,3]) ) ) )[2,1]
}

avg_cor_BBG_ARABESQUE_ENV <- mean( na.omit( cor_vec) )
rm(try, i , cor_vec)

###3.2.8: cor between BBG and IDEAL (ENV) ----------------------------------------

try <- c()
cor_vec <- c()
for(i in 7 : length(p_rank)){ 
  try <- na.omit( merge ( 
    cbind( ISIN = p_rank[p_rank$Provider == "BBG" & p_rank$Field == "ENV" , "ISIN" ], 
           p_rank[p_rank$Provider =="BBG" & p_rank$Field == "ENV" , i ]),
    cbind( ISIN = p_rank[p_rank$Provider =="IDEAL RATING" & p_rank$Field == "ENV" , "ISIN"], 
           p_rank[p_rank$Provider =="IDEAL RATING" & p_rank$Field == "ENV" , i] ), 
    by = "ISIN") ) 
  cor_vec[i] <- cor( as.matrix( cbind( as.numeric(try[ ,2]) , as.numeric(try[ ,3]) ) ) )[2,1]
}

avg_cor_BBG_IDEAL_ENV <- mean( na.omit( cor_vec) )
rm(try, i , cor_vec)

###3.2.9: cor between BBG and S&P (ENV) ------------------------------------------
try <- c()
cor_vec <- c()
for(i in 7 : length(p_rank)){ 
  try <- na.omit( merge ( 
    cbind( ISIN = p_rank[p_rank$Provider == "BBG" & p_rank$Field == "ENV" , "ISIN" ], 
           p_rank[p_rank$Provider =="BBG" & p_rank$Field == "ENV" , i ]),
    cbind( ISIN = p_rank[p_rank$Provider =="S&P" & p_rank$Field == "ENV" , "ISIN"], 
           p_rank[p_rank$Provider =="S&P" & p_rank$Field == "ENV" , i] ), 
    by = "ISIN") ) 
  cor_vec[i] <- cor( as.matrix( cbind( as.numeric(try[ ,2]) , as.numeric(try[ ,3]) ) ) )[2,1]
}

avg_cor_BBG_SP_ENV <- mean( na.omit( cor_vec) )
rm(try, i , cor_vec)

###3.2.10: cor between SP and ARABESQUE (ENV) ------------------------------------------------ 
try <- c()
cor_vec <- c()
for(i in 7 : length(p_rank)){ 
  try <- na.omit( merge ( 
    cbind( ISIN = p_rank[p_rank$Provider == "S&P" & p_rank$Field == "ENV" , "ISIN" ], 
           p_rank[p_rank$Provider =="S&P" & p_rank$Field == "ENV" , i ]),
    cbind( ISIN = p_rank[p_rank$Provider =="ARABESQUE" & p_rank$Field == "ENV" , "ISIN"], 
           p_rank[p_rank$Provider =="ARABESQUE" & p_rank$Field == "ENV" , i] ), 
    by = "ISIN") ) 
  cor_vec[i] <- cor( as.matrix( cbind( as.numeric(try[ ,2]) , as.numeric(try[ ,3]) ) ) )[2,1]
}

avg_cor_SP_ARABESQUE_ENV <- mean( na.omit( cor_vec) )
rm(try, i , cor_vec)

###3.2.11: cor between S&P and IDEAL (ENV) -------------------------------------------
try <- c()
cor_vec <- c()
for(i in 7 : length(p_rank)){ 
  try <- na.omit( merge ( 
    cbind( ISIN = p_rank[p_rank$Provider == "S&P" & p_rank$Field == "ENV" , "ISIN" ], 
           p_rank[p_rank$Provider =="S&P" & p_rank$Field == "ENV" , i ]),
    cbind( ISIN = p_rank[p_rank$Provider =="IDEAL RATING" & p_rank$Field == "ENV" , "ISIN"], 
           p_rank[p_rank$Provider =="IDEAL RATING" & p_rank$Field == "ENV" , i] ), 
    by = "ISIN") ) 
  cor_vec[i] <- cor( as.matrix( cbind( as.numeric(try[ ,2]) , as.numeric(try[ ,3]) ) ) )[2,1]
}

avg_cor_SP_IDEAL_ENV <- mean( na.omit( cor_vec) )
rm(try, i , cor_vec)

##3.2.12: cor between ARABESQUE and IDEAL (ENV) -------------------------------------------

try <- c()
cor_vec <- c()
for(i in 7 : length(p_rank)){ 
  try <- na.omit( merge ( 
    cbind( ISIN = p_rank[p_rank$Provider == "ARABESQUE" & p_rank$Field == "ENV" , "ISIN" ], 
           p_rank[p_rank$Provider =="ARABESQUE" & p_rank$Field == "ENV" , i ]),
    cbind( ISIN = p_rank[p_rank$Provider =="IDEAL RATING" & p_rank$Field == "ENV" , "ISIN"], 
           p_rank[p_rank$Provider =="IDEAL RATING" & p_rank$Field == "ENV" , i] ), 
    by = "ISIN") ) 
  cor_vec[i] <- cor( as.matrix( cbind( as.numeric(try[ ,2]) , as.numeric(try[ ,3]) ) ) )[2,1]
}

avg_cor_ARABESQUE_IDEAL_ENV <- mean( na.omit( cor_vec) )
rm(try, i , cor_vec)

###3.2.13: cor between BBG and ARABESQUE (SOCIAL) ------------------------------------------

try <- c()
cor_vec <- c()
for(i in 7 : length(p_rank)){ 
  try <- na.omit( merge ( 
    cbind( ISIN = p_rank[p_rank$Provider == "BBG" & p_rank$Field == "SOCIAL" , "ISIN" ], 
           p_rank[p_rank$Provider =="BBG" & p_rank$Field == "SOCIAL" , i ]),
    cbind( ISIN = p_rank[p_rank$Provider =="ARABESQUE" & p_rank$Field == "SOCIAL" , "ISIN"], 
           p_rank[p_rank$Provider =="ARABESQUE" & p_rank$Field == "SOCIAL" , i] ), 
    by = "ISIN") ) 
  cor_vec[i] <- cor( as.matrix( cbind( as.numeric(try[ ,2]) , as.numeric(try[ ,3]) ) ) )[2,1]
}

avg_cor_BBG_ARABESQUE_SOCIAL <- mean( na.omit( cor_vec) )
rm(try, i , cor_vec)

###3.2.14: cor between BBG and IDEAL (SOCIAL) --------------------------------------------

try <- c()
cor_vec <- c()
for(i in 7 : length(p_rank)){ 
  try <- na.omit( merge ( 
    cbind( ISIN = p_rank[p_rank$Provider == "BBG" & p_rank$Field == "SOCIAL" , "ISIN" ], 
           p_rank[p_rank$Provider =="BBG" & p_rank$Field == "SOCIAL" , i ]),
    cbind( ISIN = p_rank[p_rank$Provider =="IDEAL RATING" & p_rank$Field == "SOCIAL" , "ISIN"], 
           p_rank[p_rank$Provider =="IDEAL RATING" & p_rank$Field == "SOCIAL" , i] ), 
    by = "ISIN") ) 
  cor_vec[i] <- cor( as.matrix( cbind( as.numeric(try[ ,2]) , as.numeric(try[ ,3]) ) ) )[2,1]
}

avg_cor_BBG_IDEAL_SOCIAL <- mean( na.omit( cor_vec) )
rm(try, i , cor_vec)

###3.2.15: cor between BBG and S&P (SOCIAL) -------------------------------------------------

try <- c()
cor_vec <- c()
for(i in 7 : length(p_rank)){ 
  try <- na.omit( merge ( 
    cbind( ISIN = p_rank[p_rank$Provider == "BBG" & p_rank$Field == "SOCIAL" , "ISIN" ], 
           p_rank[p_rank$Provider =="BBG" & p_rank$Field == "SOCIAL" , i ]),
    cbind( ISIN = p_rank[p_rank$Provider =="S&P" & p_rank$Field == "SOCIAL" , "ISIN"], 
           p_rank[p_rank$Provider =="S&P" & p_rank$Field == "SOCIAL" , i] ), 
    by = "ISIN") ) 
  cor_vec[i] <- cor( as.matrix( cbind( as.numeric(try[ ,2]) , as.numeric(try[ ,3]) ) ) )[2,1]
}

avg_cor_BBG_SP_SOCIAL <- mean( na.omit( cor_vec) )
rm(try, i , cor_vec)

###3.2.16: cor between S&P and ARABESQUE (SOCIAL) -----------------------------------------

try <- c()
cor_vec <- c()
for(i in 7 : length(p_rank)){ 
  try <- na.omit( merge ( 
    cbind( ISIN = p_rank[p_rank$Provider == "S&P" & p_rank$Field == "SOCIAL" , "ISIN" ], 
           p_rank[p_rank$Provider =="S&P" & p_rank$Field == "SOCIAL" , i ]),
    cbind( ISIN = p_rank[p_rank$Provider =="ARABESQUE" & p_rank$Field == "SOCIAL" , "ISIN"], 
           p_rank[p_rank$Provider =="ARABESQUE" & p_rank$Field == "SOCIAL" , i] ), 
    by = "ISIN") ) 
  cor_vec[i] <- cor( as.matrix( cbind( as.numeric(try[ ,2]) , as.numeric(try[ ,3]) ) ) )[2,1]
}

avg_cor_SP_ARABESQUE_SOCIAL <- mean( na.omit( cor_vec) )
rm(try, i , cor_vec)

###3.2.17: cor between S&P and IDEAL (SOCIAL) -------------------------------------------

try <- c()
cor_vec <- c()
for(i in 7 : length(p_rank)){ 
  try <- na.omit( merge ( 
    cbind( ISIN = p_rank[p_rank$Provider == "S&P" & p_rank$Field == "SOCIAL" , "ISIN" ], 
           p_rank[p_rank$Provider =="S&P" & p_rank$Field == "SOCIAL" , i ]),
    cbind( ISIN = p_rank[p_rank$Provider =="IDEAL RATING" & p_rank$Field == "SOCIAL" , "ISIN"], 
           p_rank[p_rank$Provider =="IDEAL RATING" & p_rank$Field == "SOCIAL" , i] ), 
    by = "ISIN") ) 
  cor_vec[i] <- cor( as.matrix( cbind( as.numeric(try[ ,2]) , as.numeric(try[ ,3]) ) ) )[2,1]
}

avg_cor_SP_IDEAL_SOCIAL <- mean( na.omit( cor_vec) )
rm(try, i , cor_vec)

###3.2.18: cor between ARABESQUE AND IDEAL (SOCIAL) ------------------------------------------

try <- c()
cor_vec <- c()
for(i in 7 : length(p_rank)){ 
  try <- na.omit( merge ( 
    cbind( ISIN = p_rank[p_rank$Provider == "IDEAL RATING" & p_rank$Field == "SOCIAL" , "ISIN" ], 
           p_rank[p_rank$Provider =="IDEAL RATING" & p_rank$Field == "SOCIAL" , i ]),
    cbind( ISIN = p_rank[p_rank$Provider =="ARABESQUE" & p_rank$Field == "SOCIAL" , "ISIN"], 
           p_rank[p_rank$Provider =="ARABESQUE" & p_rank$Field == "SOCIAL" , i] ), 
    by = "ISIN") ) 
  cor_vec[i] <- cor( as.matrix( cbind( as.numeric(try[ ,2]) , as.numeric(try[ ,3]) ) ) )[2,1]
}

avg_cor_IDEAL_ARABESQUE_SOCIAL <- mean( na.omit( cor_vec) )
rm(try, i , cor_vec)

###3.2.19: cor between BBG and ARABESQUE (GOV) -----------------------------------------------

try <- c()
cor_vec <- c()
for(i in 7 : length(p_rank)){ 
  try <- na.omit( merge ( 
    cbind( ISIN = p_rank[p_rank$Provider == "BBG" & p_rank$Field == "GOV" , "ISIN" ], 
           p_rank[p_rank$Provider =="BBG" & p_rank$Field == "GOV" , i ]),
    cbind( ISIN = p_rank[p_rank$Provider =="ARABESQUE" & p_rank$Field == "GOV" , "ISIN"], 
           p_rank[p_rank$Provider =="ARABESQUE" & p_rank$Field == "GOV" , i] ), 
    by = "ISIN") ) 
  cor_vec[i] <- cor( as.matrix( cbind( as.numeric(try[ ,2]) , as.numeric(try[ ,3]) ) ) )[2,1]
}

avg_cor_BBG_ARABESQUE_GOV <- mean( na.omit( cor_vec) )
rm(try, i , cor_vec)

###3.2.20: cor between BBG and S&P (GOV) ------------------------------------------------

try <- c()
cor_vec <- c()
for(i in 7 : length(p_rank)){ 
  try <- na.omit( merge ( 
    cbind( ISIN = p_rank[p_rank$Provider == "BBG" & p_rank$Field == "GOV" , "ISIN" ], 
           p_rank[p_rank$Provider =="BBG" & p_rank$Field == "GOV" , i ]),
    cbind( ISIN = p_rank[p_rank$Provider =="S&P" & p_rank$Field == "GOV" , "ISIN"], 
           p_rank[p_rank$Provider =="S&P" & p_rank$Field == "GOV" , i] ), 
    by = "ISIN") ) 
  cor_vec[i] <- cor( as.matrix( cbind( as.numeric(try[ ,2]) , as.numeric(try[ ,3]) ) ) )[2,1]
}

avg_cor_BBG_SP_GOV <- mean( na.omit( cor_vec) )
rm(try, i , cor_vec)

###3.2.21: cor between BBG and IDEAL (GOV) --------------------------------------------

try <- c()
cor_vec <- c()
for(i in 7 : length(p_rank)){ 
  try <- na.omit( merge ( 
    cbind( ISIN = p_rank[p_rank$Provider == "BBG" & p_rank$Field == "GOV" , "ISIN" ], 
           p_rank[p_rank$Provider =="BBG" & p_rank$Field == "GOV" , i ]),
    cbind( ISIN = p_rank[p_rank$Provider =="IDEAL" & p_rank$Field == "GOV" , "ISIN"], 
           p_rank[p_rank$Provider =="IDEAL" & p_rank$Field == "GOV" , i] ), 
    by = "ISIN") ) 
  cor_vec[i] <- cor( as.matrix( cbind( as.numeric(try[ ,2]) , as.numeric(try[ ,3]) ) ) )[2,1]
}

avg_cor_BBG_IDEAL_GOV <- mean( na.omit( cor_vec) )
rm(try, i , cor_vec)

###3.2.22: cor between ARABESQUE and SP (GOV) ------------------------------------------------

try <- c()
cor_vec <- c()
for(i in 7 : length(p_rank)){ 
  try <- na.omit( merge ( 
    cbind( ISIN = p_rank[p_rank$Provider == "ARABESQUE" & p_rank$Field == "GOV" , "ISIN" ], 
           p_rank[p_rank$Provider =="ARABESQUE" & p_rank$Field == "GOV" , i ]),
    cbind( ISIN = p_rank[p_rank$Provider =="S&P" & p_rank$Field == "GOV" , "ISIN"], 
           p_rank[p_rank$Provider =="S&P" & p_rank$Field == "GOV" , i] ), 
    by = "ISIN") ) 
  cor_vec[i] <- cor( as.matrix( cbind( as.numeric(try[ ,2]) , as.numeric(try[ ,3]) ) ) )[2,1]
}

avg_cor_ARABESQUE_SP_GOV <- mean( na.omit( cor_vec) )
rm(try, i , cor_vec)

###3.2.23: cor between Arabesque and IDeal (GOV) --------------------------------------------------

try <- c()
cor_vec <- c()
for(i in 7 : length(p_rank)){ 
  try <- na.omit( merge ( 
    cbind( ISIN = p_rank[p_rank$Provider == "ARABESQUE" & p_rank$Field == "GOV" , "ISIN" ], 
           p_rank[p_rank$Provider =="ARABESQUE" & p_rank$Field == "GOV" , i ]),
    cbind( ISIN = p_rank[p_rank$Provider =="IDEAL" & p_rank$Field == "GOV" , "ISIN"], 
           p_rank[p_rank$Provider =="IDEAL" & p_rank$Field == "GOV" , i] ), 
    by = "ISIN") ) 
  cor_vec[i] <- cor( as.matrix( cbind( as.numeric(try[ ,2]) , as.numeric(try[ ,3]) ) ) )[2,1]
}

avg_cor_ARABESQUE_IDEAL_GOV <- mean( na.omit( cor_vec) )
rm(try, i , cor_vec)

###3.2.24: cor between SP and IDEAL (GOV) -----------------------------------------------

try <- c()
cor_vec <- c()
for(i in 7 : length(p_rank)){ 
  try <- na.omit( merge ( 
    cbind( ISIN = p_rank[p_rank$Provider == "IDEAL" & p_rank$Field == "GOV" , "ISIN" ], 
           p_rank[p_rank$Provider =="IDEAL" & p_rank$Field == "GOV" , i ]),
    cbind( ISIN = p_rank[p_rank$Provider =="S&P" & p_rank$Field == "GOV" , "ISIN"], 
           p_rank[p_rank$Provider =="S&P" & p_rank$Field == "GOV" , i] ), 
    by = "ISIN") ) 
  cor_vec[i] <- cor( as.matrix( cbind( as.numeric(try[ ,2]) , as.numeric(try[ ,3]) ) ) )[2,1]
}

avg_cor_IDEAL_SP_GOV <- mean( na.omit( cor_vec) )
rm(try, i , cor_vec)

##3.3: Method 3 (faster) ---------------------------------------------------

cor_field <- function(provider1 , provider2, field){ 
  #' Function to compute the correlation of a given field for two providers. 
  #' 
  #' @param provider1 the first provider. Should be one of the following characters: "BBG" for Bloomberg
  #' ; "IDEAL" for Ideal Ratings ; "REFINITIV" for Refinitiv ; "ARABESQUE" for Arabesque ; 
  #' "S&P" for S&P Global
  #' 
  #' @param provider2 the second provider. It shouldn't be the same as the first 
  #' 
  #' @param field the pillar to choose. Should be one of the following: "ESG" ; "ENV" ; "SOCIAL" "; 
  #' "GOV".
  #' 
  try <- c()
  cor_vec <- c()
  for(i in 7 : length(p_rank)){ 
    try <- na.omit( merge ( 
      cbind( ISIN = p_rank[p_rank$Provider == provider1 & p_rank$Field == field , "ISIN" ], 
             p_rank[p_rank$Provider ==provider1 & p_rank$Field == field , i ]),
      cbind( ISIN = p_rank[p_rank$Provider ==provider2 & p_rank$Field == field , "ISIN"], 
             p_rank[p_rank$Provider == provider2 & p_rank$Field == field , i] ), 
      by = "ISIN") ) 
    cor_vec[i] <- cor( as.matrix( cbind( as.numeric(try[ ,2]) , as.numeric(try[ ,3]) ) ) )[2,1]
  }
  return( mean (na.omit (cor_vec)))
} 

## Use the function to compute the correlations between providers

###3.3.1: ESG ------------------------------------------------

avg_cor_BBG_REFINITIV_ESG <- cor_field(provider1 = "BBG", provider2 = "REFINITIV", field = "ESG")
avg_cor_SP_REFINITIV_ESG <- cor_field(provider1 = "S&P", provider2 = "REFINITIV", field = "ESG")
avg_cor_IDEAL_REFINITIV_ESG <- cor_field(provider1 = "IDEAL", provider2 = "REFINITIV", field = "ESG")
avg_cor_ARABESQUE_REFINITIV_ESG <- cor_field(provider1 = "ARABESQUE", provider2 = "REFINITIV", field = "ESG")

###3.3.2: ENV -----------------------------------------------------------

avg_cor_BBG_REFINITIV_ENV <- cor_field(provider1 = "BBG", provider2 = "REFINITIV", field = "ENV")
avg_cor_SP_REFINITIV_ENV <- cor_field(provider1 = "S&P", provider2 = "REFINITIV", field = "ENV")
avg_cor_IDEAL_REFINITIV_ENV <- cor_field(provider1 = "IDEAL", provider2 = "REFINITIV", field = "ENV")
avg_cor_ARABESQUE_REFINITIV_ENV <- cor_field(provider1 = "ARABESQUE", provider2 = "REFINITIV", field = "ENV")

###3.3.3: GOV --------------------------------------------------------------

avg_cor_BBG_REFINITIV_GOV <- cor_field(provider1 = "BBG", provider2 = "REFINITIV", field = "GOV")
avg_cor_SP_REFINITIV_GOV <- cor_field(provider1 = "S&P", provider2 = "REFINITIV", field = "GOV")
avg_cor_IDEAL_REFINITIV_GOV <- cor_field(provider1 = "IDEAL", provider2 = "REFINITIV", field = "GOV")
avg_cor_ARABESQUE_REFINITIV_GOV <- cor_field(provider1 = "ARABESQUE", provider2 = "REFINITIV", field = "GOV")

###3.3.4: SOCIAL ----------------------------------------------------------------

avg_cor_BBG_REFINITIV_SOCIAL <- cor_field(provider1 = "BBG", provider2 = "REFINITIV", field = "SOCIAL")
avg_cor_SP_REFINITIV_SOCIAL <- cor_field(provider1 = "S&P", provider2 = "REFINITIV", field = "SOCIAL")
avg_cor_IDEAL_REFINITIV_SOCIAL <- cor_field(provider1 = "IDEAL", provider2 = "REFINITIV", field = "SOCIAL")
avg_cor_ARABESQUE_REFINITIV_SOCIAL <- cor_field(provider1 = "ARABESQUE", provider2 = "REFINITIV", field = "SOCIAL")

##3.3.5: Average correlation for every pillar -----------------------------------------------------

avg_cor_ESG <- mean( c( na.omit( avg_cor_ARABESQUE_SP_ESG ),na.omit( avg_cor_BBG_ARABESQUE_ESG ) ,
                        na.omit( avg_cor_BBG_IDEAL_ESG ) , na.omit( avg_cor_BBG_SP_ESG ),
                        na.omit(avg_cor_IDEAL_SP_ESG), na.omit(avg_cor_ARABESQUE_IDEAL_ESG),
                        na.omit(avg_cor_ARABESQUE_REFINITIV_ESG),
                        na.omit(avg_cor_BBG_REFINITIV_ESG), na.omit(avg_cor_SP_REFINITIV_ESG), 
                        na.omit(avg_cor_IDEAL_REFINITIV_ESG) ) )

avg_cor_ENV <- mean( c( na.omit( avg_cor_SP_ARABESQUE_ENV ),na.omit( avg_cor_BBG_ARABESQUE_ENV ) ,
                        na.omit( avg_cor_BBG_IDEAL_ENV ) , na.omit( avg_cor_BBG_SP_ENV ) ,
                        na.omit(avg_cor_SP_IDEAL_ENV), na.omit(avg_cor_ARABESQUE_IDEAL_ENV),
                        na.omit(avg_cor_ARABESQUE_REFINITIV_ENV),
                        na.omit(avg_cor_BBG_REFINITIV_ENV), na.omit(avg_cor_SP_REFINITIV_ENV), 
                        na.omit(avg_cor_IDEAL_REFINITIV_ENV) ))

avg_cor_SOCIAL <- mean( c( na.omit( avg_cor_SP_ARABESQUE_SOCIAL ),na.omit( avg_cor_BBG_ARABESQUE_SOCIAL ) ,
                           na.omit( avg_cor_BBG_IDEAL_SOCIAL ) , na.omit( avg_cor_BBG_SP_SOCIAL ) ,
                           na.omit(avg_cor_SP_IDEAL_SOCIAL), na.omit(avg_cor_IDEAL_ARABESQUE_SOCIAL),
                           na.omit(avg_cor_ARABESQUE_REFINITIV_SOCIAL),
                           na.omit(avg_cor_BBG_REFINITIV_SOCIAL), na.omit(avg_cor_SP_REFINITIV_SOCIAL), 
                           na.omit(avg_cor_IDEAL_REFINITIV_SOCIAL) ))

avg_cor_GOV <- mean( c( na.omit( avg_cor_ARABESQUE_SP_GOV ),na.omit( avg_cor_BBG_ARABESQUE_GOV ) ,
                        na.omit( avg_cor_BBG_IDEAL_GOV ) , na.omit( avg_cor_BBG_SP_GOV ) ,
                        na.omit(avg_cor_IDEAL_SP_GOV), na.omit(avg_cor_ARABESQUE_IDEAL_GOV),
                        na.omit(avg_cor_ARABESQUE_REFINITIV_GOV),
                        na.omit(avg_cor_BBG_REFINITIV_GOV), na.omit(avg_cor_SP_REFINITIV_GOV), 
                        na.omit(avg_cor_IDEAL_REFINITIV_GOV) ))

###3.3.6: Total observations for every pillar ----------------------------------------------

total_obs_ENV <- ARABESQUE_ENV_obs + BBG_ENV_obs + SP_ENV_obs + REFINITIV_ENV_obs
total_obs_ESG <- ARABESQUE_ESG_obs + BBG_ESG_obs + REFINITIV_ESG_obs + IDEAL_ESG_obs
total_obs_SOCIAL <- ARABESQUE_SOCIAL_obs + BBG_SOCIAL_obs + REFINITIV_SOCIAL_obs + SP_SOCIAL_obs
total_obs_GOV <- ARABESQUE_GOV_obs + BBG_GOV_obs + REFINITIV_GOV_obs + SP_GOV_obs

total_avg_ESG <- mean( ARABESQUE_ESG_avg, BBG_ESG_avg, REFINITIV_ESG_avg, IDEAL_ESG_avg ) 
total_avg_ENV <- mean( ARABESQUE_ENV_avg , BBG_ENV_avg , REFINITIV_ENV_avg , SP_ENV_avg )
total_avg_SOCIAL <- mean( ARABESQUE_SOCIAL_avg,  BBG_SOCIAL_avg , REFINITIV_SOCIAL_avg , SP_SOCIAL_avg )
total_avg_GOV <-  mean( ARABESQUE_GOV_avg , BBG_GOV_avg , REFINITIV_GOV_avg , SP_GOV_avg )

total_avg_ESG <- mean(ARABESQUE_ESG_sd, BBG_ESG_sd, REFINITIV_ESG_sd, IDEAL_ESG_sd)
total_avg_ENV <- mean(ARABESQUE_ENV_sd, BBG_ENV_sd, REFINITIV_ENV_sd, SP_ENV_sd)
total_avg_SOCIAL <-mean(ARABESQUE_SOCIAL_sd, BBG_SOCIAL_sd, REFINITIV_SOCIAL_sd, SP_SOCIAL_sd) 
total_avg_GOV <- mean(ARABESQUE_GOV_sd, BBG_GOV_sd, REFINITIV_GOV_sd, SP_GOV_sd)

# Section 4: Create a table with summary statistics ---------------------------------------------

##4.1: For Rating disagreement ---------------------------------------------------

###4.1.2: Data.frame definition -----------------------------------------------

sum_stat <- data.frame(
  ## create a dataframe with the following data: 
  ##1 Missing values
  rbind(length(na.omit(x_ESG)), length(na.omit(x_ENV)), length(na.omit(x_SOCIAL)), length(na.omit(x_GOV))),
  ##2 Averages 
  rbind(total_avg_ESG, total_avg_ENV, total_avg_SOCIAL, total_avg_GOV),
  ##3 Standard deviations
  rbind( sd(x_ESG,na.rm = T), sd(x_ENV,na.rm = T), sd(x_SOCIAL,na.rm = T), sd(x_GOV,na.rm = T)),
  ##4 Max value
  rbind( max(x_ESG, na.rm = T),max(x_ENV, na.rm = T), max(x_SOCIAL, na.rm = T), max(x_GOV, na.rm = T) ),
  ##5 Min value
  rbind( min(x_ESG,na.rm = T), min(x_ENV,na.rm = T), min(x_SOCIAL,na.rm = T), min(x_GOV,na.rm = T) ),
  ##6 Interquartile range
  rbind( IQR(x_ESG,na.rm = T), IQR(x_ENV,na.rm = T),IQR(x_SOCIAL,na.rm = T),IQR(x_GOV,na.rm = T) ), 
  ##7 Skewness
  rbind( skewness(x_ESG,na.rm = T),skewness(x_ENV,na.rm = T),skewness(x_SOCIAL,na.rm = T),skewness(x_GOV,na.rm = T) ),
  ##8 Kurtosis
  rbind( kurtosis(x_ESG, na.rm = T),kurtosis(x_ENV, na.rm = T),kurtosis(x_SOCIAL, na.rm = T),kurtosis(x_GOV, na.rm = T))
## End of data.frame()
  ) 

## Define the column and row names
colnames(sum_stat) <- c("N", "Mean", "SD", "Max", "Min", "IQR", "Skewness", "Kurtosis")
rownames(sum_stat) <- c("ESG", "ENV", "SOCIAL", "GOV")
sum_stat <- round(sum_stat, digits = 2) #round the digits

###4.1.3: Create the table -------------------------------------------

sum_stat %>% 
  kbl() %>%
  kable_material(full_width = F, html_font = "Cambria") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T) %>%  #striped table
  add_header_above(c("Summary Statistics for rating disagreement" = 9))  #add the title 


##4.2: For ratings (not disagreement) --------------------------------------------------

###4.2.1: Data.frame definition ---------------------------------------

sum_stat1 <- data.frame(
  ## Create a dataframe with the following variables 
  ##1 Number of obs 
  rbind(total_obs_ESG, total_obs_ENV, total_obs_SOCIAL, total_obs_GOV),
  ##2 Average of the ratings (quartile rankings) 
  rbind(total_avg_ESG, total_avg_ENV, total_avg_SOCIAL, total_avg_GOV),
  ##3 Average of the standard deviation 
  rbind(total_avg_ESG, total_avg_ENV, total_avg_SOCIAL, total_avg_GOV)
) 

## Define the columns and rows names
colnames( sum_stat1 ) <- c("N", "Mean", "SD")
rownames( sum_stat1) <- c("ESG", "ENV", "SOCIAL", "GOV")

###4.2.2: Create the table ------------------------------------------------

sum_stat1 %>% 
  kbl() %>%
  kable_material(full_width = F, html_font = "Cambria") %>%
  kable_styling(bootstrap_options = "striped", full_width = T) %>%
  add_header_above(c("Summary Statistics for rating providers" = 4))

##4.3: Correlation between rating providers ----------------------------------------

###4.3.1: ESG ---------------------------------------------------------

#### 4.3.1.1: Define the data.frame 
cor_data_esg <- data.frame( matrix(nrow = 5, ncol = 5))

## Create a dataframe that has value 1 in the diagonal, correlation in the lower part, 
## and blank values in the upper part 
cor_data_esg <- data.frame(
  ##1 1 in element [1,1]
  rbind(1,avg_cor_BBG_REFINITIV_ESG, avg_cor_BBG_IDEAL_ESG,
        avg_cor_BBG_SP_ESG, avg_cor_BBG_ARABESQUE_ESG),
  ##2 ... 
  rbind("",1, avg_cor_IDEAL_REFINITIV_ESG,
        avg_cor_SP_REFINITIV_ESG, avg_cor_ARABESQUE_REFINITIV_ESG),
  rbind("", "",1,
        avg_cor_IDEAL_SP_ESG, avg_cor_ARABESQUE_IDEAL_ESG), 
  rbind("", "", "",
        1, avg_cor_ARABESQUE_SP_ESG),
  rbind("", "", 
        "", "", 1)
## End of data.frame()
  )

## Define the columns and rows names 
colnames(cor_data_esg) <- c("BBG", "REFINITIV", "IDEAL", "SP", "ARABESQUE" ) 
rownames(cor_data_esg) <- c("BBG", "REFINITIV", "IDEAL", "SP", "ARABESQUE" )

## Round the non-NAs and non-blanks value of the data.frame 
digits <- 2  #define the number of digits (for round())
x <- cor_data_esg[,2]
rm(x)
rounded_df <- apply(cor_data_esg, 2, function(x) { #apply the function to the columns of cor_data_esg
  ## If is NA or a blank, the value remain unchanged, otherwise is rounded 
  x <- ifelse(is.na(x) | !nzchar(x), x, round(as.numeric(x), digits))
## End of apply
  })

## Transform it into a data.frame()
rounded_df <- as.data.frame(rounded_df)
cor_data_esg <- rounded_df

## Create the table
cor_data_esg %>% 
  kbl() %>%
  kable_material(full_width = T, html_font = "Cambria") %>%
  kable_styling(bootstrap_options = "striped", full_width = T) %>%
  add_header_above(c("ESG total pillar" = 6)) 

###4.3.2:  ENV --------------------------------------------

## Perform the same step as above 
cor_data_env <- data.frame( matrix(nrow = 5, ncol = 5))

cor_data_env <- data.frame(
  rbind(1,avg_cor_BBG_REFINITIV_ENV, avg_cor_BBG_IDEAL_ENV,
        avg_cor_BBG_SP_ENV, avg_cor_BBG_ARABESQUE_ENV),
  rbind("",1, avg_cor_IDEAL_REFINITIV_ENV,
        avg_cor_SP_REFINITIV_ENV, avg_cor_ARABESQUE_REFINITIV_ENV),
  rbind("", "",1,
        avg_cor_SP_IDEAL_ENV, avg_cor_ARABESQUE_IDEAL_ENV), 
  rbind("", "", "",
        1, avg_cor_SP_ARABESQUE_ENV),
  rbind("", "", 
        "", "", 1)
)

digits <- 2 
rounded_df <- apply(cor_data_env, 2, function(x) {
  x <- ifelse(is.na(x) | !nzchar(x), x, round(as.numeric(x), digits))
})

rounded_df <- as.data.frame(rounded_df)
cor_data_env <- rounded_df
colnames(cor_data_env) <- c("BBG", "REFINITIV", "IDEAL", "SP", "ARABESQUE" ) 
rownames(cor_data_env) <- c("BBG", "REFINITIV", "IDEAL", "SP", "ARABESQUE" )

cor_data_env %>% 
  kbl() %>%
  kable_material(full_width = T, html_font = "Cambria") %>%
  kable_styling(bootstrap_options = "striped", full_width = T) %>%
  add_header_above(c("ENV pillar" = 6))

###4.3.3.: SOCIAL ------------------------------------------------

cor_data_social <- data.frame( matrix(nrow = 5, ncol = 5))

cor_data_social <- data.frame(
  rbind(1,avg_cor_BBG_REFINITIV_SOCIAL, avg_cor_BBG_IDEAL_SOCIAL,
        avg_cor_BBG_SP_SOCIAL, avg_cor_BBG_ARABESQUE_SOCIAL),
  rbind("",1, avg_cor_IDEAL_REFINITIV_SOCIAL,
        avg_cor_SP_REFINITIV_SOCIAL, avg_cor_ARABESQUE_REFINITIV_SOCIAL),
  rbind("", "",1,
        avg_cor_SP_IDEAL_SOCIAL, avg_cor_IDEAL_ARABESQUE_SOCIAL), 
  rbind("", "", "",
        1, avg_cor_SP_ARABESQUE_SOCIAL),
  rbind("", "", 
        "", "", 1)
)

colnames(cor_data_social) <- c("BBG", "REFINITIV", "IDEAL", "SP", "ARABESQUE" ) 
rownames(cor_data_social) <- c("BBG", "REFINITIV", "IDEAL", "SP", "ARABESQUE" )

rounded_df <- apply(cor_data_social, 2, function(x) {
  x <- ifelse(is.na(x) | !nzchar(x), x, round(as.numeric(x), digits))
})

rounded_df <- as.data.frame(rounded_df)
cor_data_social <- rounded_df

cor_data_social %>% 
  kbl() %>%
  kable_material(full_width = T, html_font = "Cambria") %>%
  kable_styling(bootstrap_options = "striped", full_width = T) %>%
  add_header_above(c("SOCIAL pillar" = 6)) 

###4.3.4: GOV -----------------------------------------------

cor_data_gov <- data.frame( matrix(nrow = 5, ncol = 5))

cor_data_gov <- data.frame(
  rbind(1,avg_cor_BBG_REFINITIV_GOV, avg_cor_BBG_IDEAL_GOV,
        avg_cor_BBG_SP_GOV, avg_cor_BBG_ARABESQUE_GOV),
  rbind("",1, avg_cor_IDEAL_REFINITIV_GOV,
        avg_cor_SP_REFINITIV_GOV, avg_cor_ARABESQUE_REFINITIV_GOV),
  rbind("", "",1,
        avg_cor_IDEAL_SP_GOV, avg_cor_ARABESQUE_IDEAL_GOV), 
  rbind("", "", "",
        1, avg_cor_ARABESQUE_SP_GOV),
  rbind("", "", 
        "", "", 1)
)

colnames(cor_data_gov) <- c("BBG", "REFINITIV", "IDEAL", "SP", "ARABESQUE" ) 
rownames(cor_data_gov) <- c("BBG", "REFINITIV", "IDEAL", "SP", "ARABESQUE" )

rounded_df <- apply(cor_data_gov, 2, function(x) {
  x <- ifelse(is.na(x) | !nzchar(x), x, round(as.numeric(x), digits))
  })

rounded_df <- as.data.frame(rounded_df)
cor_data_gov <- rounded_df

cor_data_gov %>% 
  kbl() %>%
  kable_material(full_width = T, html_font = "Cambria") %>%
  kable_styling(bootstrap_options = "striped", full_width = T) %>%
  add_header_above(c("GOV pillar" = 6)) 

###4.3.5: Total average correlation for every pillar ------------------------------

avgcore <- data.frame(
  cbind(avg_cor_ESG, avg_cor_ENV, avg_cor_SOCIAL, avg_cor_GOV)  
)
colnames(avgcore) <- c("ESG", "ENV", "SOCIAL", "GOV")
avgcore <- round(avgcore, digits = 2)

avgcore %>% 
  kbl() %>%
  kable_material(full_width = T, html_font = "Cambria") %>%
  kable_styling(bootstrap_options = "striped", full_width = T) %>%
  add_header_above(c("Average Correlation" = 4)) 



# Section 5: OLS Regressions --------------------------------------

## Define the columns 
## Column 7 is the starting point of the dataset
## Column 103 is the end
col_start = 7 #02/2015
col_end = 103 #02/2023

##5.1: Prepare the data and useful functions  ------------------------------------

###5.1.1: Vector with ISIN codes, to match variables afterwards ---------------------------

ISIN_vec <- c()
for(i in 1:503){ 
  ## Take the ISIN codes and Assign them to a vector 
  ISIN_vec[i] <- p_rank[ , "ISIN"][i] 
}

###5.1.2: Create a list() of stocks -------------------------------------------------

#assign the ISIN to every element in the list 
stocks_list_score <- list() #empty list
length(stocks_list_score) = length(ISIN_vec) #length of the list == length of list of ISIN codes
for(i in 1 : length(ISIN_vec)){
  ## Every element of the list is associated to an ISIN code
  names(stocks_list_score)[i] <- ISIN_vec[i] 
## End of for 
  }

###5.1.3: Assign elements to the list -------------------------

prov_field_longdata <- function(provider, field){  
#' Assign to every ISIN code in the list, in one column the date, and in another column
#' the score about the pillar ù
#' 
#' @param provider One rating provider. Should be one of the following: "BBG" for Bloomberg; 
#' "REFINITIV" for Refinitiv; "IDEAL" for IdealRatings ; "ARABESQUE" for Arabesque 
#' 
#' @param field To choose one pillar. Could be "ESG"; "ENV"; "SOCIAL"; "GOV"
#' 
  ## Empty lists
  df <- list()
  df1 <- list()
  
  for(i in 1: length(stocks_list_score)){ 
    ## Data frame 1
    df[[i]] <-   p_rank[p_rank$Provider == provider & p_rank$Field == field & 
                          p_rank$ISIN == names(stocks_list_score)[i], #select the provider and ISIN  
                        1:length(p_rank)] 
    ## Data frame 2 -- > select just the complete cases (Without NA)
    df1[[i]] <- rbind( df[[i]][ complete.cases(df[[i]][ ,1]), ] ) 
  }
  
  ## Transform it into a data.frame()
  df <- as.data.frame( matrix(nrow = length(df1), ncol = length(df1[[1]]) ))
  colnames(df) <- colnames(p_rank) #change colnames() 
  for(i in 1:length(df1)){ 
   ## Substitute with NAs if there aren't observations 
     if( nrow( df1[[i]] ) == 0 ){ 
      df[i, ] <- c(  rep(NA, length.out = 103) )
    } else { 
      df[i, ] <- df1[[i]]
    }
  }
  ## Return the complete cases of df
  return( df[complete.cases(df[ ,1]), ] )
## End of function 
  } 

###5.1.4: Retrive the dataset you  need ---------------------------------------------

BBG_ESG <- prov_field_longdata(provider = "BBG", field = "ESG")
SP_ESG <- prov_field_longdata(provider = "S&P", field = "ESG")
ARABESQUE_ESG <- prov_field_longdata(provider = "ARABESQUE", field = "ESG")
IDEAL_ESG <- prov_field_longdata(provider = "IDEAL RATING", field = "ESG")
REFINITIV_ESG <- prov_field_longdata(provider = "REFINITIV", field = "ESG")

BBG_ENV <- prov_field_longdata(provider = "BBG", field = "ENV")
SP_ENV <- prov_field_longdata(provider = "S&P", field = "ENV")
ARABESQUE_ENV <- prov_field_longdata(provider = "ARABESQUE", field = "ENV")
IDEAL_ENV <- prov_field_longdata(provider = "IDEAL RATING", field = "ENV")
REFINITIV_ENV <- prov_field_longdata(provider = "REFINITIV", field = "ENV")

BBG_SOCIAL <- prov_field_longdata(provider = "BBG", field = "SOCIAL")
SP_SOCIAL <- prov_field_longdata(provider = "S&P", field = "SOCIAL")
ARABESQUE_SOCIAL <- prov_field_longdata(provider = "ARABESQUE", field = "SOCIAL")
IDEAL_SOCIAL <- prov_field_longdata(provider = "IDEAL RATING", field = "SOCIAL")
REFINITIV_SOCIAL <- prov_field_longdata(provider = "REFINITIV", field = "SOCIAL")

BBG_GOV <- prov_field_longdata(provider = "BBG", field = "GOV")
SP_GOV <- prov_field_longdata(provider = "S&P", field = "GOV")
ARABESQUE_GOV <- prov_field_longdata(provider = "ARABESQUE", field = "GOV")
IDEAL_GOV <- prov_field_longdata(provider = "IDEAL RATING", field = "GOV")
REFINITIV_GOV <- prov_field_longdata(provider = "REFINITIV", field = "GOV")

##5.2: Create a dataset with Standard Deviations -------------------------------------

###5.2.3: Function to retrive the SDs --------------------------------------------------

sd_dataframe <- function( field, col_start = 7, col_end = 103 ){   #col is the column from which you want to take dates
  #' This function create a dataset with the standard deviation of the pillars (they should be 
  #' already written as quartile rankings), for the selected columns. Every column will correspond to 
  #' a given month 
  #' 
  #' @param field A character indicating the pillar that the user want to select. Can be 
  #' "ESG" ; "ENV" ; "SOCIAL" ; "GOV"
  #' 
  #' @param col_start a number indicating the initial column of the dataset (starting date). The minimum v
  #' value for this parameter is 7, corresponding to 02/2015, and the maximum is 103, corresponding 
  #' to 02/2023 
  #' 
  #' @param col_end a number variable indicating the ending column of the dataset (final date). 
  #' It can assume values between 7 and 103
  #' 
  if(field == "ESG") { #Create the dataset with ESG data between providers if the 
                       #parameter filed is ESG 
    df <- rbind(BBG_ESG , IDEAL_ESG , ARABESQUE_ESG , SP_ESG , REFINITIV_ESG )
  } else if(field == "ENV"){                                      
    df <- rbind(BBG_ENV , IDEAL_ENV , ARABESQUE_ENV , SP_ENV , REFINITIV_ENV)
  } else if(field == "GOV"){ 
    df <- rbind(BBG_GOV , IDEAL_GOV , ARABESQUE_GOV , SP_GOV , REFINITIV_GOV)
  } else if (field == "SOCIAL"){
    df <- rbind(BBG_SOCIAL , IDEAL_SOCIAL , ARABESQUE_SOCIAL , SP_SOCIAL , REFINITIV_SOCIAL )
  }
  ##Prepare the data data.frame() to store the sd 
  sd_data <- as.data.frame(matrix( nrow = nrow(df), ncol = ( ncol(df)-col_start+7 - (-col_end+103) ) ) ) 
  
  ##Assign the names and set the data.frame
  colnames(sd_data)[7:ncol(sd_data)] <- colnames(df)[col_start:(ncol(df) - (-col_end+103) )]
  colnames(sd_data)[1:6] <- colnames(df)[1:6]
  sd_data[ ,1:6] <- df[  , colnames(df)[1:6]]
  
  ##Compute the standard deviation 
  for(i in 1:length(ISIN_vec)){ #loop for every stock
    for(j in 7:(length(df)-col_start+7 - (-col_end + 103) ) ) { #loop for every month
      ##Compute the standard deviation 
      sd_data[i,j] <- sd( df[ df$ISIN == ISIN_vec[i] , colnames(df)[j+col_start-7]] , na.rm = T ) 
    ##End of second loop
    } 
    ##End of first loop
    }
  sd_data <- sd_data[1:503,]
  return(sd_data)
  ##End of function
}

###5.2.4: Retrive the data with the function -----------------------------------------

sd_ESG <- sd_dataframe(field = "ESG",col_start = col_start, col_end = col_end)
sd_ENV <- sd_dataframe(field = "ENV",col_start = col_start, col_end = col_end)
sd_GOV <- sd_dataframe(field = "GOV",col_start = col_start, col_end = col_end)
sd_SOCIAL <- sd_dataframe(field = "SOCIAL",col_start = col_start, col_end = col_end)


##5.3: function to convert dataset into a vector ------------------------------------

vectorize <- function(data) { 
  #' Function to transfrom a dataset into a vector. The function starts with the values in the first 
  #' column and put them into a vector format, then it starts with the second column
  #' Consider that the function is thought to start from column 7 of the dataset. THerefore the data
  #' should start from 7 
  #' 
  #' @param data the dataset to transform a datset into a vector  
  y <- c() #empty vectors 
  y2 <- c()
  for(i in 7:ncol(data)){ 
    y2 <- data[ ,i] #takes data into column i... 
    y <- c( y , y2 ) #bind them to previous data
  ##End of loop
    }
  y <- as.numeric(y) 
  return(y)
  ##End of function
}

###5.4: Function to winsorize   ---------------------------------------------------

##source: https://www.r-bloggers.com/2011/06/winsorization/ 
winsorize2 <- function (x, multiple=3){
  #' Function to winsorize a given vector. 
  #' 
  #' @param x vector to be winsorized
  #' 
  #' @param multiple number value that determines the extent of the winsorization. If multiple 
  #' is set to 3, the function will replace numbers that are 3 times the Median Absolute Deviation
  #'  
##If multiple is comprised between 0 and 1, retrive an error message
  if(length(multiple) != 1 || multiple <= 0) {
    stop("'multiple' should not be comprised between 0 and 1")
  ##End of if
    }
  med <- median(x,na.rm = T) #Compute the median, omit NAs
  y <- x - med  #Subtract the median to every element 
  sc <- mad(y, center=0, na.rm = T) * multiple #Compute the median absolute deviation and multiply by multiple
  y[ y > sc ] <- sc #if y>sc -- > assign the value of sc 
  y[ y < -sc ] <- -sc #if y<sc -- > assign the value of -sc 
  y + med #add back the median to the winosirze data
##End of function 
  }



##5.5: Create the control variables ---------------------------------------------

###5.5.1: Function to select the control variables -----------------------------

C_var_select <- function(col_start = 7 , col_end = 103, 
                         variable ){ #variable that you want to select 
  #' This function allows the user to select the control variables. The total dataset has a lot of 
  #' different variables (from Bloomberg terminal) as Beta, Leverage, Current ratio.... 
  #' This function allows to automate the selection of variables 
  #' 
  #'  
  #' @param col_start a number indicating the initial column of the dataset (starting date). The minimum v
  #' value for this parameter is 7, corresponding to 02/2015, and the maximum is 103, corresponding 
  #' to 02/2023 
  #' 
  #' @param col_end a number variable indicating the ending column of the dataset (final date). 
  #' It can assume values between 7 and 103
  #' 
  #' @param variable A character variable indicating the control variable that the user want to select. A
  #' non exaustive list : "VOLATILITY_30D"; "CUR_MKT_CAP" ; "BETA_ADJ_OVERRIDABLE"; 
  #' "VIX"; "PX_VOLUME"; "INFLATION" ; "PX_LAST" ... 
##Select the dataset with Score values (the dataset has for every month a column representing 
##quartile ranks and a column with Absolute scores) 
   data <- cbind( 
    data_full[,1:6], data_full[ , c( data_full[1,] ) == "Score"] 
  )
##Take the data selected with parameter "variable"
  data <- data[data$Tag == variable ,]
  data <- data[complete.cases(data[,1]),]
  data2 <- cbind(data[ , 1:6] , data[ , col_start:col_end])
  
  return(data2)
  ##End of function
}



###5.5.2: Create the control variables ------------------------------

vol_30d_stock <- vectorize(
  C_var_select(col_start =  col_start, 
               col_end = col_end, 
               variable = "VOLATILITY_30D") )/100  #because it is in percentage

vol_30d_stock_lag1 <- vectorize(
  C_var_select(col_start =  col_start -1, 
               col_end = col_end-1, 
               variable = "VOLATILITY_30D") )/100

Beta <- vectorize(
  C_var_select(col_start =  col_start, 
               col_end = col_end, 
               variable = "BETA_ADJ_OVERRIDABLE") )

Mkt_cap <- vectorize(
  C_var_select(col_start =  col_start, 
               col_end = col_end, 
               variable = "CUR_MKT_CAP") )*1000000  #because it is expressed in M USD

VIX <- vectorize(
  C_var_select(col_start =  col_start, 
               col_end = col_end, 
               variable = "VIX") )  

Volume <- vectorize(
  C_var_select(col_start =  col_start, 
               col_end = col_end, 
               variable = "PX_VOLUME") )#number of shares trades in the month 

default_prob_5y <- vectorize(
  C_var_select(col_start =  col_start, 
               col_end = col_end, 
               variable = "BB_5Y_DEFAULT_PROB") )

default_prob_1y <- vectorize(
  C_var_select(col_start =  col_start, 
               col_end = col_end, 
               variable = "BB_1YR_DEFAULT_PROB") )

default_prob_3m <- vectorize(
  C_var_select(col_start =  col_start, 
               col_end = col_end, 
               variable = "BB_3M_DEFAULT_PROB") )

int_rate <- vectorize(
  C_var_select(col_start =  col_start, 
               col_end = col_end, 
               variable = "INTEREST") )

inflation <- vectorize(
  C_var_select(col_start =  col_start, 
               col_end = col_end, 
               variable = "INFLATION") )/100

price_last <- vectorize(
  C_var_select(col_start =  col_start, 
               col_end = col_end, 
               variable = "PX_LAST") )/100

cash <- vectorize(
  C_var_select(col_start =  col_start, 
               col_end = col_end, 
               variable = "CF_CASH_&_CASH_EQUIV_BEG_BAL") )

wacc <- vectorize(
  C_var_select(col_start =  col_start, 
               col_end = col_end, 
               variable = "WACC") )

SP500_RET <- vectorize(
  C_var_select(col_start =  col_start, 
               col_end = col_end, 
               variable = "SP500_RET") )

BOOK_TM <- vectorize(
  C_var_select(col_start =  col_start, 
               col_end = col_end, 
               variable = "BOOK_VAL_PER_SH") )

CUR_RATIO <- vectorize(
  C_var_select(col_start =  col_start, 
               col_end = col_end, 
               variable = "CUR_RATIO") )

LEV <- vectorize(
  C_var_select(col_start =  col_start, 
               col_end = col_end, 
               variable = "FNCL_LVRG") )

ROA <- vectorize(
  C_var_select(col_start =  col_start, 
               col_end = col_end, 
               variable = "RETURN_ON_ASSET") )

ROE <- vectorize(
  C_var_select(col_start =  col_start, 
               col_end = col_end, 
               variable = "RETURN_COM_EQY") )

BETA <- vectorize(
  C_var_select(col_start =  col_start, 
               col_end = col_end, 
               variable = "BETA_ADJ_OVERRIDABLE") )


AMI_ILL <- vectorize(
  C_var_select(col_start =  col_start, 
               col_end = col_end, 
               variable = "AMIHUD_ILL") )

##Control variable for momentum should be runned after the creation of the 
##returns vector -- > see section 5.7.1.3

##5.6: Create the independent variables ------------------------------------------------

ENV <- vectorize( #vectorize() transform the dataset into a vector 
  sd_dataframe(  #sd_dataframe() create a dataframe with standard deviations between providers
    field = "ENV",col_start = col_start, col_end = col_end
    ##End of sd_dataframe()
    ) 
  ##End of vectorize
  )

ESG <- vectorize( sd_dataframe(field = "ESG",col_start = col_start, col_end = col_end) )
GOV <- vectorize( sd_dataframe(field = "GOV",col_start = col_start, col_end = col_end) )
SOCIAL <- vectorize( sd_dataframe(field = "SOCIAL",col_start = col_start, col_end = col_end) )

##5.7: Create the dependent variables ---------------------------------

###5.7.1: Returns --------------------------------------------------------------

####5.7.1.1: Function to select returns -----------------------------------------

returns_select <- function(col_start = 7, col_end = 103){
  #' This function allows the user to select the period in which to compute the returns. 
  #' Then the function compute the returns as (P1-P0)/P0
  #' 
  #'  @param col_start a number indicating the initial column of the dataset (starting date). The minimum v
  #' value for this parameter is 7, corresponding to 02/2015, and the maximum is 103, corresponding 
  #' to 02/2023 
  #' 
  #' @param col_end a number variable indicating the ending column of the dataset (final date). 
  #' It can assume values between 7 and 103
  #'
##Create data frame with the data
  prices <- cbind(data_full[ , 1:6], data_full[ , c( data_full[1,] ) == "Score"] ) #remove quartile ranks
  prices <- prices[ prices$Field == "PX_LAST", ]           #Take only last prices 
  prices <- prices[ complete.cases(prices[,1]), ]          #take only the complete cases
##Prepare the dataset for returns  
  returns <- as.data.frame(matrix(nrow = nrow(prices), ncol = (ncol(prices)-col_start+7 - (-col_end+103) ) ))   
  returns[ , 1:6] <- prices[ , colnames(prices)[1:6]] #the first six columns are the same (with stocks, ISIN...)
  colnames(returns)[1:6] <- colnames(prices)[ 1:6]    #Also the names 
  #names from 7 oneard, select the columns selected by the function
  colnames(returns)[7:ncol(returns)] <- colnames(prices)[(7+(col_start-7)): ( ncol(prices) + col_end-103 ) ] 
  ##For loop to create the returns from the prices
  for(i in 1:nrow(prices)){ #for the stocks
    for(j in 7:ncol(returns) -1 ){ #for the months
      returns[i,j+1] <- ( as.numeric(prices[i,j+1 + col_start - 7 ])  - 
                            as.numeric(prices[i,j + col_start -7 ]) ) / 
        as.numeric(prices[i,j + col_start -7  ]) 
    ##End of second loop
      }
  ##End of first loop
    }
  return(returns)
  ##End of function
}

####5.7.1.2: Select the returns ------------------------------------------------

##From 02/2015 to 02/2023
rets <- returns_select(col_start = col_start, col_end = col_end)
y = vectorize(rets) #vectorize the returns dataset 
rm(rets)
RETS_LAG <-  c(rep(NA, 1), y[1:(length(y) - 1)]) #lagged vector (1 period lag)
RETS_LAG2 <- c(rep(NA, 2), y[1:(length(y) - 2)]) #lagged vector (2 periods lag)

##Tests
#length(y) == length(RETS_LAG2)
#tail(y)
#tail(RETS_LAG2)

####5.7.1.3: Control variable for Momentum (MOM) ----------------------------------
MOM <- ifelse(y > 0 & RETS_LAG > 0, 1,
              ifelse(y < 0 & RETS_LAG < 0, -1, 0))


###5.7.2: CDS spread ----------------------------------------------------------

####5.7.2.1: Function to select the CDS spread --------------------------------

CDS_select <- function(col_start = 7, col_end = 103){
  #' This function allows the user to select the period in which to consider the CDS spread. 
  #' 
  #'  @param col_start a number indicating the initial column of the dataset (starting date). The minimum v
  #' value for this parameter is 7, corresponding to 02/2015, and the maximum is 103, corresponding 
  #' to 02/2023 
  #' 
  #' @param col_end a number variable indicating the ending column of the dataset (final date). 
  #' It can assume values between 7 and 103
  #'
##Take the CDS spread data (absolute values and not quartile rank, named "Score")
  CDS <- cbind( 
    data_full[,1:6], data_full[ , c( data_full[1,] ) == "Score"] 
  )
  CDS <- CDS[CDS$Tag == "RSK_BB_IMPLIED_CDS_SPREAD",]
  CDS <- CDS[complete.cases(CDS[,1]),]    #complete cases
  CDS_change <- cbind(CDS[ ,1:6], CDS[ , col_start:col_end]) #select the columns of interest 
  CDS1 <- cbind(CDS[ ,1:6], CDS[ , col_start:col_end])     #do it for a second dataset
##For loop to compute the percentage changes (if needed)
  for(i in 1:nrow(CDS_change)){    #'i' changing for stocks
    for(j in 7:ncol(CDS_change)-1){    #'i' changing for months 
      CDS_change[i,j+1] <- ( as.numeric(CDS[i,j+1 + col_start -7])  - 
                               as.numeric(CDS[i,j + col_start-7]) )        
    ##End of second loop
      }
  ##End of first loop
    }
  return( list (percentage_change = CDS_change, absolute = CDS1) ) #Return two datasets
  ##End of function 
}

####5.7.2.2: Select the CDS spread ------------------------------------------------

CDS <- CDS_select(col_start = col_start, col_end = col_end)

##Take the vector form
y_cds <- vectorize(CDS$absolute)
##Assess if winsorization is needed
plot(y_cds, cex = 0.5)
##Winsorize
y_cds <- winsorize2(y_cds, multiple = 10)

###5.7.3: Bid-Ask spread ------------------------------------------------------

####5.7.3.1: Function to select the Bid-Ask spread ----------------------------

BAspread_select <- function(col_start = 7 , col_end = 103){ 
  #' This function allows the user to select the period in which to consider the Bid-Ask spread. 
  #' 
  #'  @param col_start a number indicating the initial column of the dataset (starting date). The minimum v
  #' value for this parameter is 7, corresponding to 02/2015, and the maximum is 103, corresponding 
  #' to 02/2023 
  #' 
  #' @param col_end a number variable indicating the ending column of the dataset (final date). 
  #' It can assume values between 7 and 103
  #'
  BAspread <- cbind( 
    data_full[,1:6], data_full[ , c( data_full[1,] ) == "Score"] 
  )
  
  BAspread <- BAspread[BAspread$Tag == "AVERAGE_BID_ASK_SPREAD_%",]
  BAspread <- BAspread[complete.cases(BAspread[,1]),]   #complete cases
  BAspread_change <- cbind(BAspread[ , 1:6] , BAspread[ , col_start:col_end]) #Select the columns (months)
##For loop to compute the absolute changes 
  for(i in 1:nrow(BAspread_change)){   #'i' changing for stocks
    for(j in 7:ncol(BAspread_change)-1){   #'i' changing for months
      BAspread_change[i,j+1] <- ( as.numeric(BAspread[i,j+1 + col_start-7])  - 
                                    as.numeric(BAspread[i,j + col_start-7]) ) 
    ##End of second loop
      }
  ##End of first loop
    }
##Create a dataset with monthly values (not monthly changes)
  BAspread <- cbind(BAspread[ , 1:6] , BAspread[ , col_start:col_end])
##Return both the monthly changes and the monthly values   
  return( list (percentage_change = BAspread_change, absolute = BAspread) ) 
  ##End of function
}

####5.7.3.2: Select the Bid-Ask spread --------------------------------------------

BAspread <- BAspread_select(col_start = col_start, col_end = col_end)
##Transform the dataset into a vector 
y_BAspread <- BAspread$absolute  #select the values 
y_BAspread <- vectorize(y_BAspread)  #vectorize
##Assess if winsorization is needed
#plot( y_BAspread, ylim = c(0,5) ) 
##The plot analysis highlight there are few outliers 
y_BAspread[y_BAspread > 2 ] <- NA #remove outliers

##5.8: Regressions ------------------------------------------------------------------

###5.8.1: Returns ----------------------------------------------------------------------

summary( lm(y ~ ESG + VIX + MOM + Mkt_cap + CUR_RATIO + LEV + BOOK_TM + RETS_LAG + RETS_LAG2) ) #ESG
summary( lm(y ~ ENV + VIX + MOM + Mkt_cap + CUR_RATIO + LEV + BOOK_TM + RETS_LAG + RETS_LAG2) ) #ENV
summary( lm(y ~ SOCIAL + VIX + MOM + Mkt_cap + CUR_RATIO + LEV + BOOK_TM + RETS_LAG + RETS_LAG2) ) #SOCIAL
summary( lm(y ~ GOV + VIX + MOM + Mkt_cap + CUR_RATIO + LEV + BOOK_TM + RETS_LAG + RETS_LAG2) ) #GOV
##With ESG = disagreement about ESG ; ENV = Disagreement about ENV ; SOCIAL = ...

####5.8.1.1: Table with results ---------------------------------------------------

###5.8.2: CDS spread --------------------------------------------------------

summary( lm( y_cds ~ ESG + inflation + int_rate + Mkt_cap + default_prob_1y + 
              default_prob_5y + ROA + ROE + LEV + CUR_RATIO + BETA) ) #ESG 
summary( lm( y_cds ~ ENV + inflation + int_rate + Mkt_cap + default_prob_1y + 
               default_prob_5y + ROA + ROE + LEV + CUR_RATIO + BETA) ) #ENV
summary( lm( y_cds ~ SOCIAL + inflation + int_rate + Mkt_cap + default_prob_1y + 
               default_prob_5y + ROA + ROE + LEV + CUR_RATIO + BETA) ) #SOCIAL
summary( lm( y_cds ~ GOV + inflation + int_rate + Mkt_cap + default_prob_1y + 
               default_prob_5y + ROA + ROE + LEV + CUR_RATIO + BETA) ) #GOV
##With ESG = disagreement about ESG ; ENV = Disagreement about ENV ; SOCIAL = ...

####5.8.2.1: Table with results -----------------------------------------------

###5.8.3: Bid-Ask spread --------------------------------------------------------

summary( lm(y_BAspread ~ ESG + Volume + VIX 
            + vol_30d_stock + Mkt_cap +AMI_ILL) ) #ESG
summary( lm(y_BAspread ~ ENV + Volume + VIX 
            + vol_30d_stock + Mkt_cap +AMI_ILL) ) #ENV
summary( lm(y_BAspread ~ SOCIAL + Volume + VIX 
            + vol_30d_stock + Mkt_cap +AMI_ILL) ) #SOCIAL
summary( lm(y_BAspread ~ GOV + Volume + VIX 
            + vol_30d_stock + Mkt_cap +AMI_ILL) ) #GOV

####5.8.3.1: Table with results -------------------------------------------------

#Section 6: Portfolio creation  ----------------------------------------------

##In this section a simple strategy will be implemented (buy every day at opening price
##and sell at closing price) to assess the impact in transaction costs. Especially, it will 
##be shown the difference in investing in stocks in the upper quartile (HIGHER DISAGREEMENT)
##and in the lower quartile (LOWER DISAGREEMENT).

##6.1: Divide the stocks in quartiles (better; lower; middle performer) ------------

###6.1.1: Upper quartile selection ------------------------------------------------

##Function 1 
stocks_upper_quart <- function(data, n = 6) {
  #' Function to create a dataframe storing the stocks in the upper quartile
  #' 
  #' When I have to select stocks in the upper quartile (upper 0.25) I use this function
  #' 
  #' This function will be an input for the following function 
  #' 
  #' @param data Is the dataframe in which you want to perform the selection. This is a dataframe in which 
  #' you have the list of the stocks as first column and then other column representing different
  #' dates. These data must be data regarding the disagreement (standard deviation of the ratings 
  #' between the different providers and built with the function sd_dataframe() )
  #' 
  #' @param  n number of column to eliminate in the original dataframe
  #' 
##Create the data.frame() 
  df <- as.data.frame( matrix(nrow = nrow(data), ncol = ncol(data))) #empty data.frame
  colnames(df) <- colnames(data) #set the column names equal to the selected dataframe
##For loop to select the stocks in the upper quartile
  for(r in 1:nrow(data)){ #'i' ranging from 1 to number of stocks
    for(c in 7:ncol(data)){ #'c' ranging from 1 to number of months (columns)
      ##If one sd is higher than the upper quartile -- > select the name of that stock (stored in column 1)
      if(!is.na(data[r,c]) && data[r,c] > quantile(data[,c], probs = 0.75, type  = 1, na.rm = T)[[1]]) {
        df[r,c] <- data[r,1]
      ##End of if
        }
    ##End of second loop
      }
  ##End of first loop
  }
  ##To incorporate possible columns that you may want to eliminate
  df <- df[ ,-c(1:n)]          
  return(df)
  ##End of function
}

##Function 2 
upper_quart_selector <- function(df){
  #' Function to create the dataframe with the upper dataframe. The function takes as an input the 
  #' dataset with the names of the stocks in the upper quartile. For every column, the function takes 
  #' just the non NAs values and aggregate them 
  #' 
  #' @param df the dataframe to be chosen should be one of the sd_ 
##Use the previous function  
  try <- stocks_upper_quart(df)
##Create a dataframe with in every column the name of the stocks in the best quartile 
  df_upper_quart <- as.data.frame(matrix(nrow = nrow(try),ncol = ncol(try))) #empty data.frame
  colnames(df_upper_quart) <- colnames(try) #set the colnames
##For loop  
  for(i in 1:length(try)){
    ##If try has no value in one column -- > return NAs
    if( length(try[!is.na(try[,i]),i]) == 0 ){ df_upper_quart[1:length(try[!is.na(try[,i]),i]),i] = NA }
    ##Else -- > take the non NAs values and bind them
    else { 
      df_upper_quart[1:length(try[!is.na(try[,i]),i]),i] <- cbind(
        try[!is.na(try[,i]),i]
      )
    } 
    ##End of if-else
  } 
  ##End of loop
  return(df_upper_quart)
##End of function 
  }

##Select the upper datasets

ENV_upper_quart <- upper_quart_selector(sd_ENV)
ESG_upper_quart <- upper_quart_selector(sd_ESG)
GOV_upper_quart <- upper_quart_selector(sd_GOV)
SOCIAL_upper_quart <- upper_quart_selector(sd_SOCIAL)

###6.1.2: Lower quartile selection -------------------------------------------------

##Function 1 
##The function is the same as above (just the sign in quartile() change),
##for details see stocks_upper_quart()
stocks_lower_quart <- function(data, n = 6) {
  #' Function to create a dataframe storing the stocks in the lower quartile
  #' 
  #' When I have to select stocks in the lower quartile (lower 0.25) I use this function
  #' 
  #' This function will be an input for the following function 
  #' 
  #' @param data Is the dataframe in which you want to perform the selection. This is a dataframe in which 
  #' you have the list of the stocks as first column and then other column representing different
  #' dates. These data must be data regarding the disagreement (standard deviation of the ratings 
  #' between the different providers and built with the function sd_dataframe() )
  #' 
  #' @param  n number of column to eliminate in the original dataframe
  #' 
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

##Function 2 
##The function is the same as above 
lower_quart_selector <- function(df){ 
  #' Function to create the dataframe with the lower dataframe. The function takes as an input the 
  #' dataset with the names of the stocks in the lower quartile. For every column, the function takes 
  #' just the non NAs values and aggregate them 
  #' 
  #' @param df the dataframe to be chosen should be one of the sd_
  try <- stocks_lower_quart(df)
  #and create a dataframe with in every column the name of the stocks in the best quartile 
  df_lower_quart <- as.data.frame(matrix(nrow = nrow(try),ncol = ncol(try)))
  colnames(df_lower_quart) <- colnames(try)
  for(i in 1:length(try)){ 
    if( length(try[!is.na(try[,i]),i]) == 0 ){ df_lower_quart[1:length(try[!is.na(try[,i]),i]),i] = NA }
    else { 
      df_lower_quart[1:length(try[!is.na(try[,i]),i]),i] <- cbind(
        try[!is.na(try[,i]),i]
      )
    } 
##End of if-else
  } 
  ##End of loop
  return(df_lower_quart)
  ##End of function
}

##Select the lower datasets
ENV_lower_quart <- lower_quart_selector(sd_ENV)
ESG_lower_quart <- lower_quart_selector(sd_ESG)
GOV_lower_quart <- lower_quart_selector(sd_GOV)
SOCIAL_lower_quart <- lower_quart_selector(sd_SOCIAL)

###6.1.3: Middle quartile selection ---------------------------------------------------

##Function 1 
##The function is the same as above (just the sign in quartile() change),
##for details see stocks_upper_quart()
stocks_middle_quart <- function(data, n = 6) {
  #' Function to create a dataframe storing the stocks in the middle quartile
  #' 
  #' When I have to select stocks in the middle quartile (between 0.25 and 0.75) I use this function
  #' 
  #' This function will be an input for the following function 
  #' 
  #' @param data Is the dataframe in which you want to perform the selection. This is a dataframe in which 
  #' you have the list of the stocks as first column and then other column representing different
  #' dates. These data must be data regarding the disagreement (standard deviation of the ratings 
  #' between the different providers and built with the function sd_dataframe() )
  #' 
  #' @param  n number of column to eliminate in the original dataframe
  
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
  ##End of function
}

##Function 2 
##The function is the same as above 
middle_quart_selector <- function(df){ 
  #' Function to create the dataframe with the middle dataframe. The function takes as an input the 
  #' dataset with the names of the stocks in the middle quartile. For every column, the function takes 
  #' just the non NAs values and aggregate them 
  #' 
  #' @param df the dataframe to be chosen should be one of the sd_dataframe
  try <- stocks_middle_quart(df)
  df_middle_quart <- as.data.frame(matrix(nrow = nrow(try),ncol = ncol(try)))
  colnames(df_middle_quart) <- colnames(try)
  for(i in 1:length(try)){ 
    if( length(try[!is.na(try[,i]),i]) == 0 ){ df_middle_quart[1:length(try[!is.na(try[,i]),i]),i] = NA }
    else { 
      df_middle_quart[1:length(try[!is.na(try[,i]),i]),i] <- cbind(
        try[!is.na(try[,i]),i]
      )
    } 
  } 
  return(df_middle_quart)
  ##End of function 
} 

##Select the datasets
ENV_middle_quart <- middle_quart_selector(sd_ENV)
ESG_middle_quart <- middle_quart_selector(sd_ESG)
GOV_middle_quart <- middle_quart_selector(sd_GOV)
SOCIAL_middle_quart <- middle_quart_selector(sd_SOCIAL)

##6.2: Check if quartile datasets are correct ------------------------------------------------

##Sum the non NAs by column in ENV  (total dataset with 503 stocks in rows)
non_na_counts <- lapply(sd_ENV, function(col) sum(!is.na(col)) )
##Convert the result to a data.frame()
non_na_counts_df <- data.frame(Date = names(non_na_counts), #column named date 
                               Count = unlist(non_na_counts)) #column named Count store the concatened values
##Vector with the count of the non NAs 
non_na_counts_sd = c(non_na_counts_df$Count[7:nrow(non_na_counts_df)])

##Do the same for the dataset with the quartiles 
middle <- lapply(ENV_middle_quart, function(col) sum(!is.na(col)))
lower <- lapply(ENV_lower_quart, function(col) sum(!is.na(col)))
upper <- lapply(ENV_upper_quart, function(col) sum(!is.na(col)))

middle <- data.frame(Date = names(middle), Count = unlist(middle))
lower  <- data.frame(Date = names(lower), Count = unlist(lower))
upper <- data.frame(Date = names(upper), Count = unlist(upper))

##Create a vector with the sum of the different quartiles
##Logic: the sum of the sotcks in the three quartiles' dataset should be equal to the total
sum  <- numeric(nrow(upper))
for(i in 1:nrow(upper) ){
  sum[i] <- as.numeric(upper$Count[i]) + as.numeric( middle$Count[i]) + as.numeric(lower$Count[i])
}

##Verify with a for loop
for(i in 1:length(sum)){
  ##If sum[i] is diverse from the NON-NAs in the total dataset -- > paste an error and stop the loop
  if( sum[i] != non_na_counts_sd[i] ){ 
    p <- paste0("element ",i, " differ")
    print(p)
    break
    ##End of if  
  } 
  ##Else -- > print ok message
  else {
    p <- paste0("OK")
    print(p)
    ##End of else
  }
  ##End of loop
}

##Remove everything 
rm(sum, upper, lower, middle, non_na_counts_df, non_na_counts, non_na_counts_sd)

##6.3: Prices' data (for rets) ---------------------------------------------------------

###6.3.1: Data selection and cleaning ------------------------------------------

##Dataset with the daily prices 
prices_day <- read_xlsx("C:\\Users\\gabri\\...\\Prezzi.xlsx", 
                        sheet = "Sheet2")
##Open prices
prices_open <- prices_day[prices_day$Tag == "PX_OPEN",] #select 
prices_open <- prices_open[ -c(1:7),]     #remove first seven columns #DON'T RUN TWICE
prices_open <- prices_open[-c(nrow(prices_open)),]  #remove the last row  #DON'T RUN TWICE

##Dataset with daily prices written to match the monthly obs of sd (disagreement) 
##(ex. 01/04/2019 -- > 04/2019)
prices_month <- read_xlsx("C:\\Users\\gabri\\...\\Prezzi.xlsx", 
                          sheet = "ToMonthly")

##Open prices (with columns in the format MM/YYYY)
prices_open_month <- prices_month[prices_month$Tag == "PX_OPEN",] #select 
prices_open_month <- prices_open_month[-c(1:7),] #remove first seven columns #DON'T RUN TWICE
prices_open_month <- prices_open_month[-c(nrow(prices_open_month)),] #remove the last row  #DON'T RUN TWICE

##Close prices 
prices_close_month <- prices_month[prices_month$Tag == "PX_LAST",] #select 
prices_close_month <- prices_close_month[-c(1:8),] #DON'T RUN TWICE
#prices_close_month <- prices_close_month[-c(nrow(prices_close_month)),] #DON'T RUN TWICE 

##remove the '...3' in the dates 
colnames(prices_open_month) <- prices_month[1,] #DON'T run again 
colnames(prices_close_month) <- prices_month[1,] #DON'T run again

### 6.3.2: Select the stocks in a given quartile ----------------------------------

##Here an upper quartile is selected just for example. Later on a function will automate the 
##selection

##Empty dataset 
upper_port <- data.frame( #list of stocks in the upper quartile
  matrix(nrow = nrow(ENV_upper_quart), ncol = ncol(prices_open_month))
)
colnames(upper_port) <- colnames(prices_open_month) #colnames 

##Fill the empty dataset
##This create a dataset with in every column a monthly representation of a day
##(ex. 02/2015 ; 02/2015 ; 02/2015 .... 03/2015 ....) and in the rows the stocks 
##that in that month were in the upper quartile. This is made to match the dates of the 
##daily prices with the monthly disagreement. 
##Logic: every month a new rating is issued, and the quartile change. Until that moment, the quartile is the same 
##every day in the month 
for(c in 4:ncol(upper_port)){
  upper_port[ ,c] <- ENV_upper_quart[ ,colnames(ENV_upper_quart)==colnames(upper_port[c])]
}
upper_port <- upper_port[ , -c(1:3)] #DON'T RUN TWICE

###6.3.3: Define a list with buy and sell prices ----------------------------------

##This part is to define prices at which to buy (open prices) and prices at which to sell 
##(closing prices). The prices are stored in a list, with in every element a date, inside the elements
##a list of stocks and corresponding prices. 

##Empty elements
buy <- list()
sell <- list()
vec_of_stocks <- list()
##Assign elements to the lists
for(i in 1:length(upper_port)){
  vec_of_stocks[[i]] <- upper_port[ ,i] #list with vector of stocks in the upper quartile in every element 
  ##Element 'i' of buy/sell is a dataset with : in rows -- > open/close price of the stocks in the 
  ##selected percentile ; in columns -- > corresponding dates
  buy[[i]] <- prices_open_month[which(prices_open_month$Stock %in% vec_of_stocks[[i]]),i+3]
  sell[[i]] <- prices_close_month[which(prices_close_month$Stock %in% vec_of_stocks[[i]]),i+3]
}

##Transform them into a list 
for(i in 1:length(buy)){
  buy[i] <- as.list(buy[[i]][1])
  sell[i] <- as.list(sell[[i]][1])
}

##6.4: Price's data (for transaction costs) ----------------------------------------------

###6.4.1: Data selection and cleaning ----------------------------------------------

##Bid 
##The steps are similar to the ones performed in 6.3.1
bid_month <- prices_month[prices_month$Tag == "PX_BID",]
bid_month <- bid_month[-c(1:3),] #DON'T RUN TWICE
bid_month <- bid_month[-c( (nrow(bid_month)-4):nrow(bid_month) ), ] #DON'T RUN TWICE

##Ask 
ask_month <- prices_month[prices_month$Tag == "PX_ASK",]
ask_month <- ask_month[-c(1:4),] #DON'T RUN TWICE
ask_month <- ask_month[-c( (nrow(ask_month)-3):nrow(ask_month) ), ] #DON'T RUN TWICE

colnames(bid_month) <- prices_month[1,] #DON'T RUN TWICE
colnames(ask_month) <- prices_month[1,] #DON'T RUN TWICE
 

##6.5: Perform the strategy (RETURNS computation) -------------------------------------------------

##Empty elements
percentage <- buy    #will store the percentage returns for every stocks in every day 
len <- numeric(length(buy))  #to compute the wealth invested in each stock day by day 
total_percentage <- numeric(length = length(buy))  #will store the sum of the percentage returns for every day
total_ret_money <- numeric(length(buy))   #will store the daily return in $
w_single <- numeric (length = length(buy))  #the amoun invested in every stock (change every month due to 
                                            #different number of stocks in the quartile when month change)

##Implementation

for(i in 1:length(buy)){   #with 'i' ranging for every day
  
  ##If in one period there are observations -- > compute the returns and transaction costs arising 
  if (length(buy[[i]]) > 0){ #condition to skip NULL elements in buy without breaking the loop
    
    ##1 collect in one vector the lenght of the number of stocks in the quartile in each period 
    len[i] <- length(buy[[i]])
    ##2 You have the total wealth taht you will equally split among stocks 
    w <- 100000
    ##3 Compute the returns(percentages) with a loop
    for(l in 1:len[i]) {  #with 'l' changing every day 'i' and ranging for every stocks. 
                          #This is to account for the number of stocks changing every month
      w_single[i] <- w / length(buy[[i]][[l]])    #total wealth / number of stocks in every period
      percentage[[i]][l] <-  ( as.numeric(buy[[i]][[l]]) -   #opening price of stock 'l' in date 'i' 
                                 as.numeric(sell[[i]][[l]]) ) /  #closing price of stock 'l' in date 'i'
        ( as.numeric(buy[[i]][[l]]) )  #divide to obtain the percentage 
      ##End of second loop
    }
    ##4 Find the cumulated percentage for the day 'i' and multiply it for w_single to find the $ amount
    total_percentage[i] <- sum( as.numeric(   #cumulated percentage in day 'i'
      percentage[[i]][[l]] )    #percentage in day 'i' of stock 'l' + perce day 'i' stock 'l+1' ....
      , na.rm = T)
    total_ret_money[i] <- total_percentage[i] * w_single[i]  #$ amount 
    ##5 compute the transaction costs (in another loop otherwise too slow)
    
    ##End of if
  } 
  ##Else if buy[[i]] has no observation -- > substitute with 0 and non NA (to not create problems)
  else { 
    percentage[[i]] <- 0
    ##End of else
  }
  ##End of first loop
}

###6.5.1: Assess if results are correct -----------------------------------------------

warnings()  #ok 
##The following two elements should give the same result
sum( total_percentage * w_single ) 
sum( total_percentage  ) 
sum( total_ret_money/w_single ,na.rm = T) #correct 


##6.6: Perform the strategy (TRANSACTION COSTS computation) -------------------------------

##Empty elements  
##See section 6.5 for detailed explaination 
bid <- list()
ask <- list()
vec_of_stocks <- list()
 
for(i in 1:length(upper_port)){
  vec_of_stocks[[i]] <- upper_port[ ,i]
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

##Transaction costs computation 
for(i in 1:length(bid)){ #with 'i' ranging for every day
  
  ##If in one period there are observations -- > compute the returns and transaction costs arising 
  if(length(bid[[i]])>0){ #condition to skip NULL elements in buy without breaking the loop
    ##1 collect in one vector the lenght of the number of stocks in the quartile in each period
    len[i] <- length(bid[[i]])
    ##2 You have the total wealth taht you will equally split among stocks  
    w <- 100000
    ##3 Compute the transaction costs (in percentages) with a loop
  
      for(l in 1:len[i]) { #with 'l' changing every day 'i' and ranging for every stocks. 
        #This is to account for the number of stocks changing every month
      w_single[i] <- w / length(bid[[i]]) #total wealth / number of stocks in every period
      percentage_cost[[i]][l] <-  ( as.numeric(ask[[i]][l]) -  #ask of stock 'l' in date 'i'  
                                      as.numeric(bid[[i]][l]) ) / #bid of stock 'l' in date 'i'
        ( ( as.numeric(bid[[i]][l]) + 
              as.numeric(ask[[i]][l]) ) /2 ) #divide by the mid quote to obtain the %bid-ask
      ##End of second loop
    }
    ##4 Multiply the wealth invested in every stock times the %Bid-Ask spread
    tcost_percentage[i] <- sum ( as.numeric(
      percentage_cost[[i]] ) , na.rm = T
    ) 
    tcost_money[i] <- tcost_percentage[i] * w_single[i]
    ##End of first loop 
  }
  ##5 Else if buy[[i]] has no observation -- > substitute with 0 and non NA (to not create problems)
  else {
    percentage_cost[[i]] <- 0
    ##End of else
  }
  ##End of if 
} 

###6.6.1: Assess if results are correct --------------------------------------------

##Sum of the transaction costs
##The two elements below should be equal 
sum(tcost_money)
sum(tcost_percentage * w_single)

#Section 7: Portfolio creation with function (faster) -----------------------------

##7.1: Create the function ---------------------------------------------------------

trade_analysis <- function(
    quartile = c("upper", "lower", "middle"), pillar = c("ESG", "ENV" , "SOCIAL", "GOV")
){ 
  #' Portfolio construction 
  #' 
  #' This function automatize the process of investing in the uppper or lower quartile stocks and computing 
  #' the relatives transaction costs. With this function you can select the quartile in which you want to 
  #' invest (upper, lower, middle) and also the pillar (ENV,ESG. ... ) on which to base the analysis 
  #' 
  #' @param quartile is the quartile (upper lower or middle), from which to select the stocks
  #' 
  #' @param pillar is the pillar you want to analyse. could be "ESG", "ENV", "SOCIAL", "GOV"
  #' 
  ##1 If-else to decide the dataframe to use
  ##Choose the pillar
  if(pillar == "ESG"){ df <- sd_ESG } 
  else if (pillar == "SOCIAL") { df <- sd_SOCIAL}
  else if (pillar == "ENV"){ df <- sd_ENV} 
  else if (pillar == "GOV") { df <- sd_GOV}
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
                returns_quart = total_percentage, returns_dollar = total_ret_money) )
  ##End of function 
}

###7.1.1: Retrive the data with the function ---------------------------------------

lower <- trade_analysis(quartile = "lower", pillar = "ENV")
sum( lower$tcost_dollar )  #upper means more disagreement 
upper <- trade_analysis(quartile = "upper", pillar = "ENV")

##7.2: Create a table (Comparison upper-lower portfolio) ----------------------------------

###7.2.1: The data.frame() ---------------------------------------------------------------

##Define the data.frame() to create the table 
table_upper <- data.frame( #Define the first column
  c(   #"Name of the row"   =         Value to display 
    "Returns of the period" = paste0( "$ ", sum(upper$returns_dollar,na.rm = T) %>% round(digits = 2) ) ,
    "Transaction costs (roundtrip costs)" = paste0( "$ ", sum(upper$tcost_dollar,na.rm = T) %>% round(digits = 2) ),
    "Higher daily Return" = paste0( max(upper$returns_quart[,1]*100)%>% round(digits = 2), " %" ), 
    "Lower daily Return" = paste0( min(upper$returns_quart[,1]*100) %>% round(digits = 2), " %")
  )
  ##End of data.frame()
)

table_upper <- cbind(table_upper, c( #bind the first column with another column ....
  "", "", rownames( upper$returns_quart)[ which.max(upper$returns_quart[,1])],  #display max(ret) and min(ret)
  rownames( upper$returns_quart)[ which.min(upper$returns_quart[,1])]
) )
colnames(table_upper) <- c( "Value" , "Date occurred" ) #name 

##Same process as above for the lower quartile 
table_lower <- data.frame(
  c(
    "Returns of the period" = paste0( "$ ", sum(lower$returns_dollar, na.rm = T) %>% round(digits = 2) ) ,
    "Transaction costs (roundtrip costs)" = paste0( "$ ", sum(lower$tcost_dollar, na.rm = T) %>% round(digits = 2) ),
    "Higher daily Return" = paste0( max(lower$returns_quart[,1]*100)%>% round(digits = 2), " %" ), 
    "Lower daily Return" = paste0( min(lower$returns_quart[,1]*100) %>% round(digits = 2), " %")
  )
)

table_lower <- cbind(table_lower, c(
  "", "", rownames( lower$returns_quart)[ which.max(lower$returns_quart[,1])], 
  rownames( lower$returns_quart)[ which.min(lower$returns_quart[,1])]
) )
colnames(table_lower) <- c( "Value" , "Date occurred" )

##Total table 
table <- cbind(table_upper, table_lower)

###7.2.2: Visualize the results -----------------------------------------------

##Create the table (visualization into the viewer window)
table %>%
  kbl( 
    caption = paste0("Period from ", rownames(upper$tcost_percentage)[1], " to ", 
                        rownames(upper$tcost_percentage)[nrow(upper$tcost_percentage)]," ", 
                        " Pillar:", "   ESG") 
       ##End of kbl()
       ) %>%
  kable_styling(full_width = T, html_font = "Cambria", 
                "striped")  %>%
  add_header_above( c("." = 1, "Upper quartile" = 2, "Lower quartile" = 2 ) ) %>%
  add_footnote(label = "The upper quartile is the one with the stocks with more disagreement")


#Section 8: Shiny application -----------------------------------------------------

##In this section a Shiny application to automatize the results will be created

##8.1: UI ---------------------------------------------------------

ui <- fluidPage(
  ##1 Main and subtitle 
  h2("Analyse transaction costs and returns"), #title
  p("You can select the field and quartile of which you want to assess the transaction costs
    and returns of a simple strategy- The strategy buy stocks at opening price
    and sell them at closing price, for every day in the period from 2018 to end of 2021"), #subtitle
  
  ##2 Inputs definition
  ##2.1 Input 1 -- > Quartile 
  sidebarLayout(
    selectInput(inputId = "quartile",
                label = "Select the Quartile",
                choices = c("lower", "middle", "upper", "comparison lower/upper")
    ),
    ##2.2 Input 2 -- > Pillar 
    selectInput(
      inputId = "pillar",
      label = "Select the pillar",
      choices = c("ENV", "GOV", "SOCIAL", "ESG")
    )
    ##End of sidebarLayout()
  ),
  
  ##3 Outputs
  mainPanel(
    tableOutput(outputId = "Output_tab"), #table output
    
    plotOutput(outputId = "Output_plot") #plot output
    ##End of mainPanel()
  )
  ##End of fluid page
)

##8.2: Server ----------------------------------------------------------------

server <- function(input, output){ 
  
  ##1 Table Output (dataset definition)
  output$Output_tab <- renderUI({
    ##1.1 Create a different table if comaprison is chosen (comparison needs two tables)
    if(input$quartile == "comparison lower/upper"){
      ##1.2 Data.frames  
      df1 <- trade_analysis(quartile = "lower", pillar = input$pillar)
      df2 <- trade_analysis(quartile = "upper", pillar = input$pillar)
      
      ##1.3 Data.frame for the table (Upper quartile) 
      table_upper <- data.frame(
        c(
          "Returns of the period" = paste0( "$ ", sum(df2$returns_dollar,na.rm = T) %>% round(digits = 2) ) ,
          "Transaction costs (roundtrip costs)" = paste0( "$ ", sum(df2$tcost_dollar, na.rm = T) %>% round(digits = 2) ),
          "Higher daily Return" = paste0( max(df2$returns_quart[,1]*100)%>% round(digits = 2), " %" ), 
          "Lower daily Return" = paste0( min(df2$returns_quart[,1]*100) %>% round(digits = 2), " %")
        )
      )
      
      table_upper <- cbind(table_upper, c(
        "", "", rownames( df2$returns_quart)[ which.max(df2$returns_quart[,1])], 
        rownames( df2$returns_quart)[ which.min(df2$returns_quart[,1])]
      ) )
      colnames(table_upper) <- c( "Value" , "Date occurred" ) 
      
      ##1.4 Data.frame for the table (lower quartile)
      table_lower <- data.frame(
        c(
          "Returns of the period" = paste0( "$ ", sum(df1$returns_dollar,na.rm = T) %>% round(digits = 2) ) ,
          "Transaction costs (roundtrip costs)" = paste0( "$ ", sum(df1$tcost_dollar,na.rm = T) %>% round(digits = 2) ),
          "Higher daily Return" = paste0( max(df1$returns_quart[,1]*100)%>% round(digits = 2), " %" ), 
          "Lower daily Return" = paste0( min(df1$returns_quart[,1]*100) %>% round(digits = 2), " %")
        )
      )
      
      table_lower <- cbind(table_lower, c(
        "", "", rownames( df1$returns_quart)[ which.max(df1$returns_quart[,1])], 
        rownames( df1$returns_quart)[ which.min(df1$returns_quart[,1])]
      ) )
      colnames(table_lower) <- c( "Value" , "Date occcurred" )
      
      ##1.5 Upper + Lower 
      table <- cbind(table_upper, table_lower)
    } 
    ##1.5 Else if not comparison ... one single dataframe should be created
    else {
      df <- trade_analysis(quartile = input$quartile, pillar = input$pillar)
      table <- data.frame(
        c(
          "Returns of the period" = paste0( "$ ", sum(df$returns_dollar,na.rm = T) %>% round(digits = 2) ) ,
          "Transaction costs (roundtrip costs)" = paste0( "$ ", sum(df$tcost_dollar,na.rm = T) %>% round(digits = 2) ),
          "Higher daily Return" = paste0( max(df$returns_quart[,1]*100)%>% round(digits = 2), " %" ), 
          "Lower daily Return" = paste0( min(df$returns_quart[,1]*100) %>% round(digits = 2), " %")
        )
      )
      
      table <- cbind(table, c(
        "", "", rownames( df$returns_quart)[ which.max(df$returns_quart[,1])], 
        rownames( df$returns_quart)[ which.min(df$returns_quart[,1])]
      ) )
      colnames(table) <- c( "Value" , "Date occcurred" )
      ##End of else
    }
    
    ##2 Table Output (table creation)

    ##2.1 If comaprison upper lower ... 
    if(input$quartile == "comparison lower/upper"){
      t <- table %>% 
        kbl( caption = paste0("Period from ", rownames(upper$tcost_percentage)[1], " to ", 
                              rownames(upper$tcost_percentage)[nrow(upper$tcost_percentage)]) ) %>%
        kable_styling(full_width = T, html_font = "Cambria", "striped")  %>%
        add_header_above( c("." = 1, "Upper quartile" = 2, "Lower quartile" = 2 ) ) %>%
        add_footnote(label = "The Upper quartile is the one with the stocks with more disagreement" )
      ##End of if
    } 
    ##Else if not comparison upper/lower ...
    else {
      t <- table %>%
        kbl( caption = paste0("Period from ", rownames(upper$tcost_percentage)[1], " to ", 
                              rownames(upper$tcost_percentage)[nrow(upper$tcost_percentage)]) ) %>%
        kable_styling(full_width = T, html_font = "Cambria", "striped") 
      ##End of else
    }
    
    return( HTML(as.character(t)) ) #transform the HTML into a table 
    ##End of renderUI() (table output)
  })
  
  ##3 Plot Output 
  output$Output_plot <- renderPlot({
    ##3.1 Again if comaprison upper/lower is selected, more plots are needed 
    if(input$quartile == "comparison lower/upper"){
      par(mfrow = c(2,2)) #four charts
      
      ##3.2 Image of the cumulated transaction costs and returns
      df1 <- trade_analysis("lower" , pillar = input$pillar)
      df2 <- trade_analysis("upper", pillar = input$pillar)
      
      ##3.3 Build the cumulative sum of returns
      cumu_sum_ret <- numeric(length(df1$returns_quart[,1])) #empty element
      for(i in 1:length(df1$returns_quart[,1])){   
        if(i == 1){ #the first element
          cumu_sum_ret[i] <- df1$returns_quart[,1][i]*100 
          ##End of if 
        } else { #The following elements  
          cumu_sum_ret[i] <- cumu_sum_ret[i-1] + df1$returns_quart[,1][i]*100
          ##End of else
        }
        ##End of loop
      }
      
      ##3.4 The same for the upper quartile 
      
      ## Buidl the cumulative sum of returns
      cumu_sum_ret2 <- numeric(length(df2$returns_quart[,1])) #now for returns
      for(i in 1:length(df2$returns_quart[,1])){   
        if(i == 1){ 
          cumu_sum_ret2[i] <- df2$returns_quart[,1][i]*100 
          ##End of if 
        } else { 
          cumu_sum_ret2[i] <- cumu_sum_ret2[i-1] + df2$returns_quart[,1][i]*100
          ##End of else
        }
        ##End of loop
      }
      
      ##3.5 Set the x-axis
      ##Date axis 
      date_axes <- as.Date( colnames(prices_day)[4:length(prices_day)], 
                            tryFormats = "%d/%m/%Y" ) #the x_axes need to be of a date format
      
      ##3.6 Plot the lower quartile 
      ##Scatter plot of transaction costs 
      plot(df1$tcost_percentage[,1]*100, x = date_axes, xaxt = "n" , yaxt = "n" , #xaxt = "n" -- > do 
                                                                              #not show x-axis
           ylim = c(-10,10), cex = 0.3, pch = 20, #pch = 20 -- > fulld dots 
           xlab =  "Dates", ylab = "percentage" ) 
      ##Scatter plot of returns
      points(y = df1$returns_quart[,1]*100, x = date_axes, col = "red", cex = 0.3, pch = 20)  
      ##Legend
      legend("topright", legend = c("Costs", "Returns"), col = c("black", "red"),pch = 20)
      
      ##3.7 Fix the axes 
      ##The x-axix (1)
      axis.Date(1, at = seq(date_axes[1], date_axes[length(date_axes)] , "6 months") ,  
                labels = T, format = "%m/%Y", tcl = -0.5) #add an axes with dates 
      ##The y-xis (2) 
      axis(2, at = seq(-10,10,by = 1), labels = F )  
      axis(2, at = seq(-10,10,by = 2), labels = paste0(seq(-10,10,by = 2) , "%"), las = 1 ) 
                                                                        #las = 1 -- > show horizontally                           
      
      ##3.8 Plot line of cumulated rets   
      plot(x = date_axes, y = cumu_sum_ret, type = "l", xaxt = "n", yaxt = "n" ,  
           ylim = c(-50,50), ylab = "percentage", xlab = "Dates") 
      ##x-axis (1)
      axis.Date(1, at = seq(date_axes[1], date_axes[length(date_axes)] , "6 months") , 
                labels = T, format = "%m/%Y", tcl = -0.5) #add an axes with dates 
      ##Legend
      legend("topright", legend = c( "Cumulative returns"), 
             col = c("black"), lwd = 2)
      
      ##y-axis (2)
      axis(2, at =  seq(-50,50, by = 10), labels = paste0( seq(-50,50, by = 10) , "%") , 
           las = 1) 
      
      ##Add a main 
      title( paste0(input$pillar, " ", " lower quartile portfolio" ), 
             outer = T, line = -1) #line = -1 -- > above the first two charts
      
      ##3.9 Do the same for the upper quartile
      
      ##Scatter plot of returns and transaction costs
      ##Transaction costs
      plot(df2$tcost_percentage[,1]*100, x = date_axes, xaxt = "n" , yaxt = "n" ,  
           ylim = c(-10,10), cex = 0.3, pch = 20,
           xlab =  "Dates", ylab = "percentage" ) 
      #Returns 
      points(y = df2$returns_quart[,1]*100, x = date_axes, col = "red", cex = 0.3, pch = 20)
      ##Legend
      legend("topright", legend = c("Costs", "Returns"), col = c("black", "red"), pch = 20)
      
      #x-axis (1)
      axis.Date(1, at = seq(date_axes[1], date_axes[length(date_axes)] , "6 months") , 
                labels = T, format = "%m/%Y", tcl = -0.5) #add an axes with dates 
      ##y-axis (2)
      axis(2, at = seq(-10,10,by = 1), labels = F )  
      axis(2, at = seq(-10,10,by = 2), labels = paste0(seq(-10,10,by = 2) , "%"), las = 1 )
      
      ##Line showing cumulated returns 
      ##Line chart
      plot(x = date_axes, y = cumu_sum_ret2, type = "l", xaxt = "n", yaxt = "n" ,  
           ylim = c(-50,50), ylab = "percentage", xlab = "Dates") #costs
      ##x-axis 
      axis.Date(1, at = seq(date_axes[1], date_axes[length(date_axes)] , "6 months") , 
                labels = T, format = "%m/%Y", tcl = -0.5) #add an axes with dates 
      ##Legend
      legend("topright", legend = c("Cumulative returns"), 
             col = c("black"), lwd = 2)
      
      ##y-axis (2) 
      axis(2, at =  seq(-50,50, by = 10), labels = paste0( seq(-50,50, by = 10) , "%") , 
           las = 1) #to write horizontally  
      
      ##Add a title  
      title( paste0(input$pillar, " ", " upper quartile portfolio" ), 
             outer = T, line = -18) #line = -18 -- > In the middle of the four charts
      ##End of if (comparison == T)
    } 
    ##3.10 Else (if input$quartile != comparison -- > two plots are needed instead of four)
    else  { 
      par(mfrow = c(1,2)) #two charts 
      df <- trade_analysis(quartile = input$quartile, pillar = input$pillar)
      
      ##Cumulated returns
      cumu_sum_ret <- numeric(length(df$returns_quart[,1])) 
      for(i in 1:length(df$returns_quart[,1])){   
        if(i == 1){ 
          cumu_sum_ret[i] <- df$returns_quart[,1][i]*100 
          ##End of if 
        } else { 
          cumu_sum_ret[i] <- cumu_sum_ret[i-1] + df$returns_quart[,1][i]*100
          ##End of else
        }
        ##End of loop
      }
      
      ##Date axis  
      date_axes <- as.Date( colnames(prices_day)[4:length(prices_day)], 
                            tryFormats = "%d/%m/%Y" ) #the x_axes need to be of a date format
      
      ##Scatter plot of rets and transaction costs (percentage)
      plot(df$tcost_percentage[,1]*100, x = date_axes, xaxt = "n" , yaxt = "n" ,  
           ylim = c(-10,10), cex = 0.3, pch = 20, #costs scatter
           xlab =  "Dates", ylab = "percentage" ) 
      points(y = df$returns_quart[,1]*100, x = date_axes, col = "red", cex = 0.3, pch = 20) #returns 
      legend("topright", legend = c("Costs", "Returns"), col = c("black", "red"),pch = 20)
      ##Fix the axes
      axis.Date(1, at = seq(date_axes[1], date_axes[length(date_axes)] , "6 months") , 
                labels = T, format = "%m/%Y", tcl = -0.5) #add an axes with dates 
      axis(2, at = seq(-10,10,by = 1), labels = F )  
      axis(2, at = seq(-10,10,by = 2), labels = paste0(seq(-10,10,by = 2) , "%"), las = 1 )
      
      ##Line cahrt with cumulated elements 
      plot(x = date_axes, y = cumu_sum_ret, type = "l", xaxt = "n", yaxt = "n" ,  
           ylim = c(-50,50), ylab = "percentage", xlab = "Dates") #rets
      axis.Date(1, at = seq(date_axes[1], date_axes[length(date_axes)] , "6 months") , 
                labels = T, format = "%m/%Y", tcl = -0.5) #add an axes with dates 
      legend("topright", legend = c("Cumulative returns"), 
             col = c("black"), lwd = 2)
      axis(2, at = seq(-50,50, by = 10), labels = paste0( seq(-50,50, by = 10) , "%"), 
           las = 1 ) #to write horizaontally 
      
      ##Main
      title( paste0(input$pillar, input$quartile , " " ,"quartile portfolio") ,
             outer = T,line = -1)
      
      ##End of else
    }
    ##End of renderPlot()
  })
  ##End of server()  
}

##8.3: Run the application --------------------------------------------------------

shinyApp(ui = ui , server = server)
##Publish the application 
#the application is published through another R file  

#Section 9: Plots -------------------------------------------------------

##9.1 Regression Plots (Bid-ask spread) --------------------------------------

##Just the statistically significant (ESG and ENV)
par(mfrow = c(1,2))

###9.1.1: ESG ----------------------------------------------

summary( lm(y_BAspread ~ ESG +  Volume + VIX 
            + vol_30d_stock + Mkt_cap +AMI_ILL) ) #ESG

##Create the scatter plot 
plot(x = ESG + Volume + VIX + vol_30d_stock + Mkt_cap +AMI_ILL, 
     y = y_BAspread, cex = 0.2, pch = 20, 
     ylim = c(0,1),  #Note that visualizing between 0 and 1 you lose some observations
     ylab = "Bid-Ask spread", xlab = "ESG + fixed effects", main = "ESG") 

##Set the variable to create the line of fit (x + control variables)
tot <- ( ESG +  Volume + VIX + vol_30d_stock + Mkt_cap +AMI_ILL)

##Add the line of fit 
abline(lm(y_BAspread ~ tot ) , col = "red"  , lwd = 2)  

##Remove x + control variables
rm(tot)

###9.1.2: Environmental ----------------------------------------------

summary( lm(y_BAspread ~ ENV + Volume + VIX 
            + vol_30d_stock + Mkt_cap +AMI_ILL) ) #ENV

##Create the scatter plot 
plot(x = ENV + Volume + VIX + vol_30d_stock + Mkt_cap +AMI_ILL, 
     y = y_BAspread, cex = 0.2, pch = 20, 
     ylim = c(0,1), #Note that visualizing between 0 and 1 you lose some observations
     ylab = "Bid-Ask spread", xlab = "ENV + fixed effects" , main= "ENV")

##Set the variable to create the line of fit (x + control variables)
tot <- ( ENV +  Volume + VIX + vol_30d_stock + Mkt_cap +AMI_ILL)

##Add the line of fit 
abline(lm(y_BAspread ~ tot ) , col = "blue"  , lwd = 2)  

##Remove x + control variables
rm(tot)

##Add a common main 
title("Disagreement and Bid-Ask spread", outer = T, line = -1 )

##9.2:  Density of the Standard deviation of the various pillar -------------------------

##Four plots
par(mfrow = (c(2,2)))

##Create histograms and density lines 
hist(x = ESG, freq = F, main = "ESG", ylim = c(0,4))
lines(density(ESG, na.rm = T), col = "red") #ESG
hist(x = ENV, freq = F, main = "ENV", ylim = c(0,4))
lines(density(ENV, na.rm = T), col = "red") #ENV
hist(x = SOCIAL, freq = F, main = "SOCIAL", ylim = c(0,4))
lines(density(SOCIAL, na.rm = T), col = "red") #SOCIAL
hist(x = GOV, freq = F, main = "GOV", ylim = c(0,4))
lines(density(GOV, na.rm = T), col = "red") #GOV

##Add a common title
title(main = "Distribution of the Standard deviation among ratings",
      outer = T, line = -1)

##9.3: Plot tcosts and returns -----------------------------

par(mfrow = c(1,2)) #two charts 
par(mar = c(5,4,4,5) + 0.5) #higher margins for the double y-axis

##Create the vector with the cumulated returns
cumu_sum_ret <- numeric(length(total_percentage)) #now for returns
for(i in 1:length(total_percentage)){   
  if(i == 1){ 
    cumu_sum_ret[i] <- total_percentage[i]*100 
    #end of if 
  } else { 
    cumu_sum_ret[i] <- cumu_sum_ret[i-1] + total_percentage[i]*100
    #end of else
  }
  #end of loop
}

##Create a date axis 
date_axes <- as.Date( colnames(prices_day)[4:length(prices_day)], 
                      tryFormats = "%d/%m/%Y" ) #the x_axes need to be of a date format 

##Assess the correct length of the y-axis for rets
min(total_percentage)
max(total_percentage)

##Scatter plot of returns and transaction costs 
##Returns
plot(total_percentage*100, x = date_axes, xaxt = "n" , yaxt = "n" ,  
     ylim = c(-10,10), cex = 0.3, pch = 20, 
     xlab =  "Dates", ylab = "Returns (%)",
     col = "#288ba8") #rets

##x-axis (date)
axis.Date(1, at = seq(date_axes[1], date_axes[length(date_axes)] , "6 months") , 
          labels = T, format = "%m/%Y", tcl = -0.5) #add an x-axis with dates


##y-axis (returns)
axis(2, at = seq(-10,10,by = 1), labels = F )  #y-axis
axis(2, at = seq(-10,10,by = 2), labels = paste0(seq(-10,10,by = 2) , "%"), las = 1 ) 

##allow a new plot
par(new = T)

##New scatter (costs)
plot( x = date_axes , y = tcost_percentage, 
      xlab = "" , ylab = "" , ylim = c(-0.2,0.2),
      xaxt = "n", yaxt = "n", 
      cex = 0.3 , pch = 20, col = "red" )

##y.axis (costs)
axis(4, at = seq(-0.2 , 0.2 , 0.05) , labels = paste0( seq(-0.2 , 0.2 , 0.05) , "%"), 
     las = 1 , col= "red"  , col.axis = "red" )
##New ylabel (costs)
mtext("Costs (%)", side =  4, 
      line = 3.8, col = "red")  #source: https://stackoverflow.com/questions/6142944/how-can-i-plot-with-2-different-y-axes

##add a legend
legend("topright", legend = c("Returns", "Costs"), col = c("#288ba8", "red"),
       pch = 20,) #pch = 20 -- > full dots  


##Assess the correct length for the other chart cumulated rets
c( min(cumu_sum_ret), max(cumu_sum_ret) )

##Line of cumulated returns
plot(x = date_axes, y = cumu_sum_ret, type = "l", xaxt = "n", yaxt = "n" ,  
     ylim = c(-50,50), ylab = "Returns (%)", xlab = "Dates",
     col = "#288ba8") #rets
axis.Date(1, at = seq(date_axes[1], date_axes[length(date_axes)] , "6 months") , 
          labels = T, format = "%m/%Y", tcl = -0.5) #add an axes with dates 
legend("topright", legend = c( "Cumulative returns"), 
       lwd = 2, col = "#288ba8", cex = 0.5)

##Add the y axes 
axis(2, at = seq(-50,50, by = 5), labels = paste0( seq(-50,50, by = 5) , "%"),
      las = 1 )  

##Add a common main 
title("ESG upper quartile portfolio", outer = T,line = -1)


