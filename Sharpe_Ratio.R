library(tidyverse)
library(tidyquant)

#read data
contrafund=read.csv("M:/OMSA/MGT6203/contrafund.csv",header=TRUE)
tail(contrafund)

#select timeframe for rate
contrafund2=contrafund[445:456,]



#get stock prices
stocks=tq_get(c("META","AMZN","MSFT","GOOGL"),get="stock.prices",from="2017-01-01",to="2017-12-31") 
stocks

#retunrs
returns=
  stocks %>%
  group_by(symbol)%>%
  tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "monthly")
tail(returns)

#add weights
weights_df <- tibble(symbol = c("META", "AMZN", "MSFT","GOOGL"),
                     weights = c(0.30, 0.20, 0.10,0.40))
portfolio_returns=tq_portfolio(data=returns,assets_col=symbol, returns_col=monthly.returns,weights=weights_df,col_rename=NULL,wealth.index=FALSE)


#addmarket rate
portfolio_returns$rate=contrafund2$Risk.Free
head(portfolio_returns)

#convert date into row
portfolio_returns$date=as.Date(portfolio_returns$date,format="%y/%m/%d")
df_portfolio_returns=as.data.frame(portfolio_returns)
pr2=df_portfolio_returns[,-1]
rownames(pr2)=df_portfolio_returns[,1]
head(pr2)

#Sharpe ratio
sr=SharpeRatio(pr2[,1,drop=FALSE],pr2[,2,drop=FALSE],FUN="StdDev")
sr

#cumilative retuns portfolio
pcr=Return.cumulative(pr2$portfolio.returns,geometric = TRUE)
pcr

#cumilative return market
mcr=Return.cumulative(contrafund2$Market.Return)
mcr