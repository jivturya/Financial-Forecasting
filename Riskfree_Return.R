library(lubridate)
#Read data
ups_df=read.csv("M:/OMSA/MGT6203/UPS_KO.csv",header=TRUE)
tail(ups_df)

df2=ups_df[10:53,]
head(df2)
tail(df2)

#convert date into row
df2$Date=ym(df2$Date)
colnames(df2)[1] <- "date"
head(df2)
df3=df2[,-1]
rownames(df3)=df2[,1]
head(df3)

#riskfree return UPS
ups_rfr=Return.excess(df3[,5,drop=FALSE],df3[,4,drop=FALSE])
ups_rfr_df=data.frame(date=index(ups_rfr), coredata(ups_rfr))
head(ups_rfr_df)

#riskfree return KO
ko_rfr=Return.excess(df3[,6,drop=FALSE],df3[,4,drop=FALSE])
ko_rfr_df=data.frame(date=index(ko_rfr), coredata(ko_rfr))
head(ko_rfr_df)

#combined table
comb_rf1 = left_join(df2,ups_rfr_df, by = 'date')
head(comb_rf1)

#last combined table
comb_rf2 = left_join(comb_rf1,ko_rfr_df, by = 'date')
head(comb_rf2)


#UPS model1
ups_model=lm(UPS...RF~Mkt_RF+SMB+HML,data=comb_rf2)
summary(ups_model)

#UPS model1
ko_model=lm(KO...RF~Mkt_RF+SMB+HML,data=comb_rf2)
summary(ko_model)
