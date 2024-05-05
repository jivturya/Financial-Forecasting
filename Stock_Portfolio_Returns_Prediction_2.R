library(tidyverse)
library(tidyquant)

#get stock prices
stocks=tq_get(c("AMZN","VMW","MSFT","AMD"),get="stock.prices",from="2010-01-01",to="2021-01-01") 
tail(stocks)

#ticker and weight vector
tickers = c("AMZN","VMW","MSFT","AMD")
wts = c(0.3,0.1,0.4,0.2)

#returns
ret_data= stocks %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "ret")
head(ret_data)

#portfolio returns
port_ret = ret_data %>%
  tq_portfolio(assets_col = symbol,
               returns_col = ret,
               weights = wts,
               col_rename = 'port_ret',
               geometric = FALSE)
head(port_ret)

#sp500 benchmark +returns
sp500=tq_get("^GSPC",get="stock.prices",from="2010-01-01",to="2021-01-01") 
tail(sp500)

bench_ret = sp500 %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "bench_ret")


#conbined table
comb_ret = left_join(port_ret,bench_ret, by = 'date')
head(comb_ret)

#beta calculation
model = lm(comb_ret$port_ret ~ comb_ret$bench_ret)
model_beta = model$coefficients[2]
model_beta
