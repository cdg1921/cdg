# import data
data_stock = read.table("D:/data.tsv",header = TRUE, sep = "\t")  #Suppost the data file located in 'D:/'

#Transfer the Timestamp to valid format,for example, from  2016\\5\\9 13:38 to 2016-05-09 13:38:00
data_stock$Timestamp <- gsub("[\\]","-",data_stock$Timestamp) 
data_stock$Timestamp <- as.POSIXlt(data_stock$Timestamp)

#outcome = buy + fee; income = sell - fee; profit_or_loss = income - outcome.
for (i in 1:nrow(data_stock)) { # i = 1
    if(data_stock[i,c('TradeType')] == 'Buy'){
      data_stock[i,c('money')] <- data_stock[i,c('Quantity')] * data_stock[i,c('Price')] + data_stock[i,c('Fee')]
    }
    else{
      data_stock[i,c('money')] <- data_stock[i,c('Quantity')] * data_stock[i,c('Price')] - data_stock[i,c('Fee')]
    }
}

data_stock <- data.frame(data_stock)
data_stock <- data_stock[order(data_stock[,2],data_stock[,8]),]

#Sell detail
data_sell <- data_stock[which(data_stock$TradeType %in% c('Sell')),]

#Buy detail(the stock has been selled)
data_buy <- data_stock[which((data_stock$TradeType %in% c('Buy'))& (data_stock$StkCode %in% c(data_sell$StkCode))),]

#Buy detail(the stock has never been selled)
data_buy_and_not_sell <- subset(data_stock[which((data_stock$TradeType %in% c('Buy'))& !(data_stock$StkCode %in% c(data_sell$StkCode))),], select = c(Trader,Fee))


data_buy_and_sell <- aggregate(x=data_sell[c('Quantity','money')], by = list(data_sell$Trader, data_sell$StkCode), FUN=sum)

#Initial the varibles to 0
data_buy_and_sell$Buy_Quantity_sum <- 0
data_buy_and_sell$Buy_money_sum <- 0   
data_buy_and_sell$profit_or_loss <- 0     #profit/loss

#Rename col name
colnames(data_buy_and_sell)[1:2]<-c("Trader","StkCode")

#buy and sell,profit/loss  include the trade profit/loss and fee
if(nrow(data_buy)==0){
  print('NO BUY TRADE!!')
} else if(nrow(data_buy_and_sell)==0){
  print('NO SELL TRADE!!')
}else{
  for(i in 1:nrow(data_buy)){# i = 1
    for (j in 1:nrow(data_buy_and_sell)) {# j = 1
      if((data_buy[i,c('Trader')] == data_buy_and_sell[j,c('Trader')])&(data_buy[i,c('StkCode')] == data_buy_and_sell[j,c('StkCode')])){ #同一用户买同一只股票
        #money_last <- data_buy_and_sell[j,c('Buy_money_sum')]       #截止至上一次的交易额
        Quantity_last <- data_buy_and_sell[j,c('Buy_Quantity_sum')] #截止至上一次的交易量
        
        data_buy_and_sell[j,c('Buy_Quantity_sum')] = data_buy_and_sell[j,c('Buy_Quantity_sum')] + data_buy[i,c('Quantity')]
        data_buy_and_sell[j,c('Buy_money_sum')] = data_buy_and_sell[j,c('Buy_money_sum')] + data_buy[i,c('money')]
        
        if(data_buy_and_sell[j,c('Buy_Quantity_sum')] < data_buy_and_sell[j,c('Quantity')]){
          next()
        }
        else if(data_buy_and_sell[j,c('Buy_Quantity_sum')] == data_buy_and_sell[j,c('Quantity')]){
          data_buy_and_sell[j,c('profit_or_loss')]= data_buy_and_sell[j,c('money')] - data_buy_and_sell[j,c('Buy_money_sum')]
        }
        else if(data_buy_and_sell[j,c('Buy_Quantity_sum')] > data_buy_and_sell[j,c('Quantity')]& Quantity_last < data_buy_and_sell[j,c('Quantity')]){
          over_money <- (data_buy_and_sell[j,c('Buy_Quantity_sum')] - data_buy_and_sell[j,c('Quantity')])*data_buy[i,c('Price')]
          data_buy_and_sell[j,c('profit_or_loss')]= data_buy_and_sell[j,c('money')] - data_buy_and_sell[j,c('Buy_money_sum')] + over_money
        }
        else{
          data_buy_and_sell[j,c('profit_or_loss')]= data_buy_and_sell[j,c('profit_or_loss')] - data_buy[i,c('Fee')]  #后面买入，还没有卖出的股票
        }
        
      }
    }
  }
  #Just keep Trader and profit_or_loss
  data_buy_and_sell <- subset(data_buy_and_sell, select=c(Trader,profit_or_loss))
}


#If buy and never sell the stock, the profit_or_loss is just the fee
data_profit <- merge(data_buy_and_sell,data_buy_and_not_sell,by='Trader',all.x = TRUE)
data_profit[is.na(data_profit)] <- 0
data_profit$profit_or_loss <- data_profit$profit_or_loss - data_profit$Fee
data_profit <- subset(data_profit, select=c(Trader,profit_or_loss))


#Remove the header
dimnames(data_profit) <- list(1:nrow(data_profit), 1:ncol(data_profit))  

#Save the result
write.table(data_profit,file = "D:/data_result.tsv", col.name = FALSE, quote = FALSE, sep = "\t",row.names = FALSE)

#clear invalid datas and varibles
#rm(data_buy_and_not_sell,data_buy_and_sell,data_stock,data_profit)
rm(list = ls())   #clear the environment

#Reload and print the result
data_stock1 = read.table("D:/data_result.tsv",header = TRUE, sep = "\t")
print(data_stock1)
#Results will be shown as follow:
#   V1      V2
#1 AAA   96.48
#2 BBB -252.96
#Note:in R,if a table doesn't include headers， V1,V2,……will be default

#Finally,We can put the script file in 'D:/', and then can run the script through the following script:
#system.time(source("D:/Stock_profit_or_loss_analyse.r"))
#add analyse
