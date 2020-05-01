##################################################################
#                                                                #
#Let's forecast sales by accounts                                #
#                                                                #  
#                                                                #   
##################################################################

#############################################
#             Install Packages              # 
#############################################


list.of.packages <- c('tidyverse','lubridate','stringr','forecast')

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) {  
  install.packages(new.packages)
}

library('tidyverse')
library('lubridate')
library('stringr')
library('forecast')
#############################################
#             Reading Data                  # 
#############################################

sales <- read.csv2(file ="C:\\Users\\domingo.alvarez\\Downloads\\Roche\\Sales_Expense_InputFile.csv",
                   sep="," , dec = ".")

account <- read.csv2(file ="C:\\Users\\domingo.alvarez\\Downloads\\Roche\\Account_Potential_Data.csv",
                     sep="," )

########################################################
#     Adding account potential to sales                #
########################################################

sales <- left_join(sales, account, by.x=sales$ACCOUNT_ID, by.y=account$ACCOUNT_ID)


########################################################
#     MONTH should be a date                           #
########################################################

sales$MONTH<- as.Date(sales$MONTH, "%m/%d/%Y")

########################################################
#     Renaming variables                               #
########################################################

sales<- sales %>% rename(DATE=MONTH, 
                         UNIT_SALES=UNIT_SALES..in.Million...,
                         TOTAL_EXPENSE=TOTAL_EXPENSE..in.Million...,
                         SALESFORCE_EXPENSE=SALESFORCE_EXPENSE..in.Million...,
                         DIGITAL_EXPENSE=DIGITAL_EXPENSE..in.Million...)


########################################################
#  Empty dataset to output                             #
########################################################


salida <- data.frame(ACCOUNT_ID = character(),
                     COUNTRY = character(), 
                     PRODUCT = character(),
                     DATE = as.Date(character()),
                     UNIT_SALES = double(),
                     UNIT_SALES_LOW =double(),
                     UNIT_SALES_HIGH =double())

########################################################
#  Vector for countries, accounts and drugs            #
########################################################


countries <- as.vector(unique(sales$COUNTRY))
accounts <- as.vector(unique(sales$ACCOUNT_ID))
drugs <- as.vector(unique(sales$PRODUCT))



########################################################
#  Running over every account                          #
########################################################


for(h in 1:length(drugs)){
  
  for (j in 1:length(countries)) {
    
    for (i in 1:length(accounts)) {
      
      datos <- filter(sales,PRODUCT==drugs[h],
                      COUNTRY==countries[j],
                      ACCOUNT_ID==accounts[i])
      if (dim(datos)[1]==0 || datos$PRODUCT=="Drug_3"){ 
        next
      }
      else{
        
#dataset for the rest of expenses data that accounts has
        
        rest_datos_ac <- filter(sales,PRODUCT!=drugs[h],
                                COUNTRY==countries[j],
                                ACCOUNT_ID==accounts[i])
        
        rest_datos_ac <- aggregate(x= rest_datos_ac[c( 
          "TOTAL_EXPENSE", 
          "SALESFORCE_EXPENSE", 
          "DIGITAL_EXPENSE")],
          FUN = sum,
          by = list(Group.date = rest_datos_ac$DATE))
        
        rest_datos_ac <- rename(rest_datos_ac, DATE=Group.date,
                                TOTAL_EXPENSE_RESTAC=TOTAL_EXPENSE,
                                SALESFORCE_EXPENSE_RESTAC=SALESFORCE_EXPENSE,
                                DIGITAL_EXPENSE_RESTAC=DIGITAL_EXPENSE)
        
#dataset for the rest of expenses data in account's country   
        
        rest_datos_country <- filter(sales, 
                                     ACCOUNT_ID!=accounts[i],
                                     COUNTRY==countries[j])
        
        rest_datos_country <- aggregate(x= rest_datos_country[c( 
          "TOTAL_EXPENSE", 
          "SALESFORCE_EXPENSE", 
          "DIGITAL_EXPENSE")],
          FUN = sum,
          by = list(Group.date = rest_datos_country$DATE))
        
        rest_datos_country <- rename(rest_datos_country, DATE=Group.date,
                                     TOTAL_EXPENSE_CNTRY=TOTAL_EXPENSE,
                                     SALESFORCE_EXPENSE_CNTRY=SALESFORCE_EXPENSE,
                                     DIGITAL_EXPENSE_CNTRY=DIGITAL_EXPENSE)

#dataset for the rest of expenses data        
        rest_datos <- filter(sales, 
                             ACCOUNT_ID!=accounts[i],
        )
        
        rest_datos <- aggregate(x= rest_datos[c( 
          "TOTAL_EXPENSE", 
          "SALESFORCE_EXPENSE", 
          "DIGITAL_EXPENSE")],
          FUN = sum,
          by = list(Group.date = rest_datos$DATE))
        
        rest_datos <- rename(rest_datos, DATE=Group.date,
                             TOTAL_EXPENSE_REST=TOTAL_EXPENSE,
                             SALESFORCE_EXPENSE_REST=SALESFORCE_EXPENSE,
                             DIGITAL_EXPENSE_REST=DIGITAL_EXPENSE)

#some accounts do not have data to 2018-10-01 
#we might consider the did not sell anything that month
        if(max(datos$DATE)<"2018-10-01"){
          p <- as.integer(round((as.Date("2018-10-01")-max(datos$DATE))/30))
          l <- max(datos$DATE)
          w <- nrow(datos)
          for (k in 1:p) {
            datos <-rbind(datos, datos[w+k-1,])
            month(l)<- month(l)+1
            datos$DATE[w+k]<- l
            datos$UNIT_SALES[w+k] <- 0
            datos$TOTAL_EXPENSE[w+k] <- 0
            datos$SALESFORCE_EXPENSE[w+k] <- 0
            datos$DIGITAL_EXPENSE[w+k] <- 0
          }
        }
        
#lets put all that data together       
        
        
        datos <- left_join(datos, rest_datos_ac ) 
        datos <- left_join(datos, rest_datos ) 
        datos <- left_join(datos, rest_datos_country ) 
        
        f <- nrow(datos)
        d<- datos$DATE[nrow(datos)]
        
#creating rows for next 9 months        
        
        for (m in 1:9) {
          
          month(d)<- month(d)+1
          
          datos <-rbind(datos, datos[f+m-1,])
          datos$DATE[f+m]<- d
        }
        
        datos[is.na(datos)]<-0
        
        train_index <- f
        n_total <- nrow(datos)
        
        train <- datos[1:(train_index),]
        

#since I don't have any data for future expenses I'll have to forecast them
        
        predicted <- numeric(n_total-train_index)
        predicted_low <- numeric(n_total-train_index)
        predicted_high <- numeric(n_total-train_index)
        
        predicted_total <- numeric(n_total-train_index)
        predicted_salesforce <- numeric(n_total-train_index)
        predicted_digital <- numeric(n_total-train_index)
        
        predicted_total_restac <- numeric(n_total-train_index)
        predicted_salesforce_restac <- numeric(n_total-train_index)
        predicted_digital_restac <- numeric(n_total-train_index)
        
        predicted_total_rest <- numeric(n_total-train_index)
        predicted_salesforce_rest <- numeric(n_total-train_index)
        predicted_digital_rest <- numeric(n_total-train_index)
        
        predicted_total_cntry <- numeric(n_total-train_index)
        predicted_salesforce_cntry <- numeric(n_total-train_index)
        predicted_digital_cntry <- numeric(n_total-train_index)
        
        
        for (q in 1:(n_total-train_index)) {

#training data for expenses estimations
          train <- datos[1:(train_index-1+q),]

#arima models for expenses estimations
          
          arima_model_total <- auto.arima(as.ts(train$TOTAL_EXPENSE),stepwise = F)
          arima_model_salesforce <- auto.arima(as.ts(train$SALESFORCE_EXPENSE),stepwise =F)
          arima_model_digital <- auto.arima(as.ts(train$DIGITAL_EXPENSE),stepwise = F)
          
          arima_model_total_restac <- auto.arima(as.ts(train$TOTAL_EXPENSE_RESTAC),stepwise = F)
          arima_model_salesforce_restac <- auto.arima(as.ts(train$SALESFORCE_EXPENSE_RESTAC),stepwise = F)
          arima_model_digital_restac <- auto.arima(as.ts(train$DIGITAL_EXPENSE_RESTAC),stepwise = F)
          
          arima_model_total_rest <- auto.arima(as.ts(train$TOTAL_EXPENSE_REST),stepwise = F)
          arima_model_salesforce_rest <- auto.arima(as.ts(train$SALESFORCE_EXPENSE_REST),stepwise = F)
          arima_model_digital_rest <- auto.arima(as.ts(train$DIGITAL_EXPENSE_REST),stepwise = F)
          
          arima_model_total_cntry <- auto.arima(as.ts(train$TOTAL_EXPENSE_CNTRY),stepwise = F)
          arima_model_salesforce_cntry <- auto.arima(as.ts(train$SALESFORCE_EXPENSE_CNTRY),stepwise = F)
          arima_model_digital_cntry <- auto.arima(as.ts(train$DIGITAL_EXPENSE_CNTRY),stepwise = F)
 
#arima models for expenses estimations                   
          pred_total <- forecast(arima_model_total,1)
          pred_salesforce <- forecast(arima_model_salesforce,1)
          pred_digital <- forecast(arima_model_digital,1)
          
          pred_total_restac <- forecast(arima_model_total_restac,1)
          pred_salesforce_restac <- forecast(arima_model_salesforce_restac,1)
          pred_digital_restac <- forecast(arima_model_digital_restac,1)
          
          pred_total_rest <- forecast(arima_model_total_rest,1)
          pred_salesforce_rest <- forecast(arima_model_salesforce_rest,1)
          pred_digital_rest <- forecast(arima_model_digital_rest,1)
          
          pred_total_cntry <- forecast(arima_model_total_cntry,1)
          pred_salesforce_cntry <- forecast(arima_model_salesforce_cntry,1)
          pred_digital_cntry <- forecast(arima_model_digital_cntry,1)
          
          if(pred_total$mean<0){
            predicted_total[q] <-0 
            datos$TOTAL_EXPENSE[(train_index+q)]<-0
          }else {predicted_total[q] <- pred_total$mean
          datos$TOTAL_EXPENSE[(train_index+q)]<- pred_total$mean}
          
          if(pred_salesforce$mean<0){
            predicted_salesforce[q] <-0 
            datos$SALESFORCE_EXPENSE[(train_index+q)] <-0
          }else {predicted_salesforce[q] <- pred_salesforce$mean
          datos$SALESFORCE_EXPENSE[(train_index+q)] <- pred_salesforce$mean}
          
          if(pred_digital$mean<0){
            predicted_digital[q] <-0 
            datos$DIGITAL_EXPENSE[(train_index+q)] <-0
          }else {predicted_digital[q] <- pred_digital$mean
          datos$DIGITAL_EXPENSE[(train_index+q)] <- pred_digital$mean}
          
          if(pred_total_restac$mean<0){
            predicted_total_restac[q] <-0 
            datos$TOTAL_EXPENSE_RESTAC[(train_index+q)] <- 0
          }else {predicted_total_restac[q] <- pred_total_restac$mean
          datos$TOTAL_EXPENSE_RESTAC[(train_index+q)] <- pred_total_restac$mean}
          
          if(pred_salesforce_restac$mean<0){
            predicted_salesforce_restac[q] <-0 
            datos$SALESFORCE_EXPENSE_RESTAC[(train_index+q)] <-0
          }else {predicted_salesforce_restac[q] <- pred_salesforce_restac$mean
          datos$SALESFORCE_EXPENSE_RESTAC[(train_index+q)] <- pred_salesforce_restac$mean}
          
          if(pred_digital_restac$mean<0){
            predicted_digital_restac[q] <-0 
            datos$DIGITAL_EXPENSE_RESTAC[(train_index+q)] <-0
          }else {predicted_digital_restac[q] <- pred_digital_restac$mean
          datos$DIGITAL_EXPENSE_RESTAC[(train_index+q)] <- pred_digital_restac$mean}
          
          if(pred_total_rest$mean<0){
            predicted_total_rest[q] <-0 
            datos$TOTAL_EXPENSE_REST[(train_index+q)] <- 0
          }else {predicted_total_rest[q] <- pred_total_rest$mean
          datos$TOTAL_EXPENSE_REST[(train_index+q)] <- pred_total_rest$mean}
          
          if(pred_salesforce_rest$mean<0){
            predicted_salesforce_rest[q] <-0 
            datos$SALESFORCE_EXPENSE_REST[(train_index+q)] <- 0
          }else{predicted_salesforce_rest[q] <- pred_salesforce_rest$mean
          datos$SALESFORCE_EXPENSE_REST[(train_index+q)] <- pred_salesforce_rest$mean}
          
          if(pred_digital_rest$mean<0){
            predicted_digital_rest[q] <-0 
            datos$DIGITAL_EXPENSE_REST[(train_index+q)] <- 0
          }else {predicted_digital_rest[q] <- pred_digital_rest$mean
          datos$DIGITAL_EXPENSE_REST[(train_index+q)] <- pred_digital_rest$mean}
          
          if(pred_total_cntry$mean<0){
            predicted_total_cntry[q] <-0
            datos$TOTAL_EXPENSE_CNTRY[(train_index+q)] <- 0
          }else {predicted_total_cntry[q] <- pred_total_cntry$mean
          datos$TOTAL_EXPENSE_CNTRY[(train_index+q)] <- pred_total_cntry$mean}
          
          if(pred_salesforce_cntry$mean<0){
            predicted_salesforce_cntry[q] <-0 
            datos$SALESFORCE_EXPENSE_CNTRY[(train_index+q)] <- 0
          }else {predicted_salesforce_cntry[q] <- pred_salesforce_cntry$mean
          datos$SALESFORCE_EXPENSE_CNTRY[(train_index+q)] <- pred_salesforce_cntry$mean}
          
          if(pred_digital_cntry$mean<0){
            predicted_digital_cntry[q] <-0 
            datos$DIGITAL_EXPENSE_CNTRY[(train_index+q)] <- 0
          }else {predicted_digital_cntry[q] <- pred_digital_cntry$mean
          datos$DIGITAL_EXPENSE_CNTRY[(train_index+q)] <- pred_digital_cntry$mean}
          
        }
        
        test <- datos[(train_index+1):n_total,]
        
#arima model and predictions for each accounts
        
        for (r in 1:(n_total-train_index)) {
          train <- datos[1:(train_index-1+r),]
          arima_model <- auto.arima(as.ts(train$UNIT_SALES),stepwise = F, 
                                    xreg = cbind(train$TOTAL_EXPENSE, 
                                                 train$SALESFORCE_EXPENSE,
                                                 train$DIGITAL_EXPENSE,
                                                 train$TOTAL_EXPENSE_RESTAC, 
                                                 train$SALESFORCE_EXPENSE_RESTAC,
                                                 train$DIGITAL_EXPENSE_RESTAC,
                                                 train$TOTAL_EXPENSE_REST, 
                                                 train$SALESFORCE_EXPENSE_REST,
                                                 train$DIGITAL_EXPENSE_REST,
                                                 train$TOTAL_EXPENSE_CNTRY, 
                                                 train$SALESFORCE_EXPENSE_CNTRY,
                                                 train$DIGITAL_EXPENSE_CNTRY
                                    ))
          
          
          pred <- forecast(arima_model, 1, xreg = cbind(test$TOTAL_EXPENSE[r], 
                                                        test$SALESFORCE_EXPENSE[r],
                                                        test$DIGITAL_EXPENSE[r],
                                                        test$TOTAL_EXPENSE_RESTAC[r], 
                                                        test$SALESFORCE_EXPENSE_RESTAC[r],
                                                        test$DIGITAL_EXPENSE_RESTAC[r],
                                                        test$TOTAL_EXPENSE_REST[r], 
                                                        test$SALESFORCE_EXPENSE_REST[r],
                                                        test$DIGITAL_EXPENSE_REST[r],
                                                        test$TOTAL_EXPENSE_CNTRY[r], 
                                                        test$SALESFORCE_EXPENSE_CNTRY[r],
                                                        test$DIGITAL_EXPENSE_CNTRY[r]
          ))
          if(pred$mean<0){
            predicted[r] <-0
          }else {predicted[r] <- pred$mean}
          
          if(pred$lower[2]<0){
            predicted_low[r]<-0
          }else {predicted_low[r]<- pred$lower[2]}
          
          if(pred$upper[2]<0){
            predicted_high[r]<-0
          }else {predicted_high[r]<- pred$upper[2]}
          
          
          datos$UNIT_SALES[train_index+r] <- predicted[r] 
        }
        
        
        output <- datos[-(1:train_index),] %>%
          select(ACCOUNT_ID,COUNTRY, PRODUCT, DATE, UNIT_SALES)
        
        output$YEAR <- as.vector(year(output$DATE))
        output$MONTH <- as.vector(month(output$DATE))
        output <- output[,-4]
        output$UNIT_SALES_LOW <- as.vector(predicted_low)
        output$UNIT_SALES_HIGH <- as.vector(predicted_high)
        
        salida <- rbind(salida, output)
        salida <- salida[,c(1,2,3,5,6,4,7,8)]
        
      }
    }
  }
}

write.csv2(salida, file = "C:\\Users\\domingo.alvarez\\Downloads\\Roche\\output.csv",
           row.names = F)

