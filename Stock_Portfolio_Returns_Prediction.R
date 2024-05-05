library(tidyverse)
library(tidyquant)

#get stock prices
stocks=tq_get(c("AMZN","VMW","MSFT","AMD"),get="stock.prices",from="2010-01-01",to="2021-01-01") 
tail(stocks)

#ticker and weight vector
wts1 = c(0.25,0.25,0.25,0.25)
wts2 = c(0.4,0.2,0.3,0.1)
wts3 = c(0.1,0.5,0.2,0.2)
wts4 = c(0.2,0.1,0.4,0.3)
wts5 = c(0.5,0.1,0.3,0.1)

#returns
ret_data= stocks %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "ret")
head(ret_data)

#portfolio returns (multiple weights)
port_ret = ret_data %>%
  tq_portfolio(assets_col = symbol,
               returns_col = ret,
               #weights = wts1,
               #weights=wts2,
               #weights=wts3,
               weights=wts4,
               #weights=wts5,
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
