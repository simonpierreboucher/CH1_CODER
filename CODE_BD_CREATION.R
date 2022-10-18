

## Intalled r packages 
install.packages("tidyverse")
install.packages("stargazer")
install.packages("medRV")
install.packages("xts")
install.packages("tseries")
install.packages("lubridate")
install.packages("nlme")
install.packages("dyn")

## Request r package already installed 
library(xts)
library(tidyverse)
library(stargazer)
library(xts)
library(lubridate)
library("highfrequency")
library(tseries)
library(texreg)
library("nlme")

# GET INTRADAY PRICES DATA 


setwd("~/Desktop/codeR_CH1")

X<-read.csv("CL_5m.csv",header = FALSE,sep = ";")
names(X)[1]<-"Date"
names(X)[2]<-"Time"
names(X)[3]<-"Open"
names(X)[4]<-"High"
names(X)[5]<-"Low"
names(X)[6]<-"Close"
names(X)[7]<-"Volume"
X$DAY<-as.numeric(substr(X$Date,1,2))
X$MONTH<-as.numeric(substr(X$Date,4,5))
X$YEAR<-as.numeric(substr(X$Date,7,10))
X$HOUR<-as.numeric(substr(X$Time,1,2))
X$MINUTE<-as.numeric(substr(X$Time,4,5))
X$r_t1<-(X$Close-X$Open)/X$Open
X$t<-X$HOUR*60+X$MINUTE
X2<-X
X2$t<-X2$t+5
X2<-data.frame(X2$YEAR,X2$MONTH,X2$DAY,X2$t,X2$r_t1)
names(X2)[1] <- 'YEAR'
names(X2)[2] <- 'MONTH'
names(X2)[3] <- 'DAY'
names(X2)[4] <- 't'
names(X2)[5] <-'r_m1'
X <- merge(X,X2,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/yhp46rkwnp4owd1/Initial_jobless_claims.csv?dl=1"
A1<-read.csv(url(URL))
A1$SURPRISE <- A1$Actual - A1$Forecast
sigma1 <- sd(A1$SURPRISE)
A1$SURPRISE_S1 <- A1$SURPRISE/sigma1
A1$t<-A1$HOUR*60+A1$MINUTE
A1<-data.frame(A1$YEAR,A1$MONTH,A1$DAY,A1$t,A1$SURPRISE_S1)
names(A1)[1] <- 'YEAR'
names(A1)[2] <- 'MONTH'
names(A1)[3] <- 'DAY'
names(A1)[4] <- 't'
names(A1)[5] <-'SURPRISE_S1'

X <- merge(X,A1,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

URL="https://www.dropbox.com/s/5l4qfy3w3vvzmgn/ADP_employment.csv?dl=1"
A2<-read.csv(url(URL))
A2$SURPRISE <- A2$Actual - A2$Forecast
sigma2 <- sd(A2$SURPRISE)
A2$SURPRISE_S2 <- A2$SURPRISE/sigma2
A2$t<-A2$HOUR*60+A2$MINUTE
A2<-data.frame(A2$YEAR,A2$MONTH,A2$DAY,A2$t,A2$SURPRISE_S2)
names(A2)[1] <- 'YEAR'
names(A2)[2] <- 'MONTH'
names(A2)[3] <- 'DAY'
names(A2)[4] <- 't'
names(A2)[5] <-'SURPRISE_S2'
X <- merge(X,A2,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/p8gkqlmk3tprx44/CB_Consumer.csv?dl=1"
A3<-read.csv(url(URL))
A3$SURPRISE <- A3$Actual - A3$Forecast
sigma3 <- sd(A3$SURPRISE)
A3$SURPRISE_S3 <- A3$SURPRISE/sigma3
A3$t<-A3$HOUR*60+A3$MINUTE
A3<-data.frame(A3$YEAR,A3$MONTH,A3$DAY,A3$t,A3$SURPRISE_S3)
names(A3)[1] <- 'YEAR'
names(A3)[2] <- 'MONTH'
names(A3)[3] <- 'DAY'
names(A3)[4] <- 't'
names(A3)[5] <-'SURPRISE_S3'
X <- merge(X,A3,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

URL="https://www.dropbox.com/s/bakb4abqodl0n10/Advance_retail_sales.csv?dl=1"
A4<-read.csv(url(URL))
A4$SURPRISE <- A4$Actual - A4$Forecast
sigma4 <- sd(A4$SURPRISE)
A4$SURPRISE_S4 <- A4$SURPRISE/sigma4
A4$t<-A4$HOUR*60+A4$MINUTE
A4<-data.frame(A4$YEAR,A4$MONTH,A4$DAY,A4$t,A4$SURPRISE_S4)
names(A4)[1] <- 'YEAR'
names(A4)[2] <- 'MONTH'
names(A4)[3] <- 'DAY'
names(A4)[4] <- 't'
names(A4)[5] <-'SURPRISE_S4'

X <- merge(X,A4,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/rnueh4nojue20y8/Building_permit%20.csv?dl=1"
A5<-read.csv(url(URL))
A5$SURPRISE <- A5$Actual - A5$Forecast
sigma5 <- sd(A5$SURPRISE)
A5$SURPRISE_S5 <- A5$SURPRISE/sigma5
A5$t<-A5$HOUR*60+A5$MINUTE
A5<-data.frame(A5$YEAR,A5$MONTH,A5$DAY,A5$t,A5$SURPRISE_S5)
names(A5)[1] <- 'YEAR'
names(A5)[2] <- 'MONTH'
names(A5)[3] <- 'DAY'
names(A5)[4] <- 't'
names(A5)[5] <-'SURPRISE_S5'

X <- merge(X,A5,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/e82g1sbs92brioi/Construction_spending.csv?dl=1"
A6<-read.csv(url(URL))
A6$SURPRISE <- A6$Actual - A6$Forecast
sigma6 <- sd(A6$SURPRISE)
A6$SURPRISE_S6 <- A6$SURPRISE/sigma6
A6$t<-A6$HOUR*60+A6$MINUTE
A6<-data.frame(A6$YEAR,A6$MONTH,A6$DAY,A6$t,A6$SURPRISE_S6)
names(A6)[1] <- 'YEAR'
names(A6)[2] <- 'MONTH'
names(A6)[3] <- 'DAY'
names(A6)[4] <- 't'
names(A6)[5] <-'SURPRISE_S6'

X <- merge(X,A6,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/h84c44xhk4mc40e/Consumer_credit.csv?dl=1"
A7<-read.csv(url(URL))
A7$SURPRISE <- A7$Actual - A7$Forecast
sigma7 <- sd(A7$SURPRISE)
A7$SURPRISE_S7 <- A7$SURPRISE/sigma7
A7$t<-A7$HOUR*60+A7$MINUTE
A7<-data.frame(A7$YEAR,A7$MONTH,A7$DAY,A7$t,A7$SURPRISE_S7)
names(A7)[1] <- 'YEAR'
names(A7)[2] <- 'MONTH'
names(A7)[3] <- 'DAY'
names(A7)[4] <- 't'
names(A7)[5] <-'SURPRISE_S7'

X <- merge(X,A7,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

URL="https://www.dropbox.com/s/l2m4j8x2p6i642o/Consumer_price_index.csv?dl=1"
A8<-read.csv(url(URL))
A8$SURPRISE <- A8$Actual - A8$Forecast
sigma8 <- sd(A8$SURPRISE)
A8$SURPRISE_S8 <- A8$SURPRISE/sigma8
A8$t<-A8$HOUR*60+A8$MINUTE
A8<-data.frame(A8$YEAR,A8$MONTH,A8$DAY,A8$t,A8$SURPRISE_S8)
names(A8)[1] <- 'YEAR'
names(A8)[2] <- 'MONTH'
names(A8)[3] <- 'DAY'
names(A8)[4] <- 't'
names(A8)[5] <-'SURPRISE_S8'

X <- merge(X,A8,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/j7uyjs118ehnejy/Durable_goods_orders.csv?dl=1"
A9<-read.csv(url(URL))
A9$SURPRISE <- A9$Actual - A9$Forecast
sigma9 <- sd(A9$SURPRISE)
A9$SURPRISE_S9 <- A9$SURPRISE/sigma9
A9$t<-A9$HOUR*60+A9$MINUTE
A9<-data.frame(A9$YEAR,A9$MONTH,A9$DAY,A9$t,A9$SURPRISE_S9)
names(A9)[1] <- 'YEAR'
names(A9)[2] <- 'MONTH'
names(A9)[3] <- 'DAY'
names(A9)[4] <- 't'
names(A9)[5] <-'SURPRISE_S9'

X <- merge(X,A9,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/zzknlv7pu0ie88b/Existing_home_sales.csv?dl=1"
A10<-read.csv(url(URL))
A10$SURPRISE <- A10$Actual - A10$Forecast
sigma10 <- sd(A10$SURPRISE)
A10$SURPRISE_S10 <- A10$SURPRISE/sigma10
A10$t<-A10$HOUR*60+A10$MINUTE
A10<-data.frame(A10$YEAR,A10$MONTH,A10$DAY,A10$t,A10$SURPRISE_S10)
names(A10)[1] <- 'YEAR'
names(A10)[2] <- 'MONTH'
names(A10)[3] <- 'DAY'
names(A10)[4] <- 't'
names(A10)[5] <-'SURPRISE_S10'

X <- merge(X,A10,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)



URL="https://www.dropbox.com/s/nozkudnltw6r0p4/Factory_orders.csv?dl=1"
A11<-read.csv(url(URL))
A11$SURPRISE <- A11$Actual - A11$Forecast
sigma11 <- sd(A11$SURPRISE)
A11$SURPRISE_S11 <- A11$SURPRISE/sigma11
A11$t<-A11$HOUR*60+A11$MINUTE
A11<-data.frame(A11$YEAR,A11$MONTH,A11$DAY,A11$t,A11$SURPRISE_S11)
names(A11)[1] <- 'YEAR'
names(A11)[2] <- 'MONTH'
names(A11)[3] <- 'DAY'
names(A11)[4] <- 't'
names(A11)[5] <-'SURPRISE_S11'

X <- merge(X,A11,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)



URL="https://www.dropbox.com/s/y35gccj7y9tvzub/GDP.csv?dl=1"

A12<-read.csv(url(URL))
A12$SURPRISE <- A12$Actual - A12$Forecast
sigma12 <- sd(A12$SURPRISE)
A12$SURPRISE_S12 <- A12$SURPRISE/sigma12
A12$t<-A12$HOUR*60+A12$MINUTE
A12<-data.frame(A12$YEAR,A12$MONTH,A12$DAY,A12$t,A12$SURPRISE_S12)
names(A12)[1] <- 'YEAR'
names(A12)[2] <- 'MONTH'
names(A12)[3] <- 'DAY'
names(A12)[4] <- 't'
names(A12)[5] <-'SURPRISE_S12'

X <- merge(X,A12,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)




URL="https://www.dropbox.com/s/l2jxw6lm13iedhc/Housing_start.csv?dl=1"
A13<-read.csv(url(URL))
A13$SURPRISE <- A13$Actual - A13$Forecast
sigma13 <- sd(A13$SURPRISE)
A13$SURPRISE_S13 <- A13$SURPRISE/sigma13
A13$t<-A13$HOUR*60+A13$MINUTE
A13<-data.frame(A13$YEAR,A13$MONTH,A13$DAY,A13$t,A13$SURPRISE_S13)
names(A13)[1] <- 'YEAR'
names(A13)[2] <- 'MONTH'
names(A13)[3] <- 'DAY'
names(A13)[4] <- 't'
names(A13)[5] <-'SURPRISE_S13'

X <- merge(X,A13,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/fzry11e8swj3rv2/Industrial_production.csv?dl=1"
A14<-read.csv(url(URL))
A14$SURPRISE <- A14$Actual - A14$Forecast
sigma14 <- sd(A14$SURPRISE)
A14$SURPRISE_S14 <- A14$SURPRISE/sigma14
A14$t<-A14$HOUR*60+A14$MINUTE
A14<-data.frame(A14$YEAR,A14$MONTH,A14$DAY,A14$t,A14$SURPRISE_S14)
names(A14)[1] <- 'YEAR'
names(A14)[2] <- 'MONTH'
names(A14)[3] <- 'DAY'
names(A14)[4] <- 't'
names(A14)[5] <-'SURPRISE_S14'

X <- merge(X,A14,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)




URL="https://www.dropbox.com/s/o5gwq9vo0fiihqx/Michigan.csv?dl=1"
A15<-read.csv(url(URL))
A15$SURPRISE <- A15$Actual - A15$Forecast
sigma15 <- sd(A15$SURPRISE)
A15$SURPRISE_S15 <- A15$SURPRISE/sigma15
A15$t<-A15$HOUR*60+A15$MINUTE
A15<-data.frame(A15$YEAR,A15$MONTH,A15$DAY,A15$t,A15$SURPRISE_S15)
names(A15)[1] <- 'YEAR'
names(A15)[2] <- 'MONTH'
names(A15)[3] <- 'DAY'
names(A15)[4] <- 't'
names(A15)[5] <-'SURPRISE_S15'

X <- merge(X,A15,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/y8pjv26j5pk0gpm/New_home_sales.csv?dl=1"
A16<-read.csv(url(URL))
A16$SURPRISE <- A16$Actual - A16$Forecast
sigma16 <- sd(A16$SURPRISE)
A16$SURPRISE_S16 <- A16$SURPRISE/sigma16
A16$t<-A16$HOUR*60+A16$MINUTE
A16<-data.frame(A16$YEAR,A16$MONTH,A16$DAY,A16$t,A16$SURPRISE_S16)
names(A16)[1] <- 'YEAR'
names(A16)[2] <- 'MONTH'
names(A16)[3] <- 'DAY'
names(A16)[4] <- 't'
names(A16)[5] <-'SURPRISE_S16'

X <- merge(X,A16,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/nykinyx0v651ddf/Nonfarm_employment.csv?dl=1"
A17<-read.csv(url(URL))
A17$SURPRISE <- A17$Actual - A17$Forecast
sigma17 <- sd(A17$SURPRISE)
A17$SURPRISE_S17 <- A17$SURPRISE/sigma17
A17$t<-A17$HOUR*60+A17$MINUTE
A17<-data.frame(A17$YEAR,A17$MONTH,A17$DAY,A17$t,A17$SURPRISE_S17)
names(A17)[1] <- 'YEAR'
names(A17)[2] <- 'MONTH'
names(A17)[3] <- 'DAY'
names(A17)[4] <- 't'
names(A17)[5] <-'SURPRISE_S17'

X <- merge(X,A17,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/1oab2f2r96ijj1x/Pending_home_sales.csv?dl=1"
A18<-read.csv(url(URL))
A18$SURPRISE <- A18$Actual - A18$Forecast
sigma18 <- sd(A18$SURPRISE)
A18$SURPRISE_S18 <- A18$SURPRISE/sigma18
A18$t<-A18$HOUR*60+A18$MINUTE
A18<-data.frame(A18$YEAR,A18$MONTH,A18$DAY,A18$t,A18$SURPRISE_S18)
names(A18)[1] <- 'YEAR'
names(A18)[2] <- 'MONTH'
names(A18)[3] <- 'DAY'
names(A18)[4] <- 't'
names(A18)[5] <-'SURPRISE_S18'

X <- merge(X,A18,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/av884s7bfv6bwlk/Personal_consumption.csv?dl=1"
A19<-read.csv(url(URL))
A19$SURPRISE <- A19$Actual - A19$Forecast
sigma19 <- sd(A19$SURPRISE)
A19$SURPRISE_S19 <- A19$SURPRISE/sigma19
A19$t<-A19$HOUR*60+A19$MINUTE
A19<-data.frame(A19$YEAR,A19$MONTH,A19$DAY,A19$t,A19$SURPRISE_S19)
names(A19)[1] <- 'YEAR'
names(A19)[2] <- 'MONTH'
names(A19)[3] <- 'DAY'
names(A19)[4] <- 't'
names(A19)[5] <-'SURPRISE_S19'

X <- merge(X,A19,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/r3kricv3436rv6n/Personal_income.csv?dl=1"
A20<-read.csv(url(URL))
A20$SURPRISE <- A20$Actual - A20$Forecast
sigma20 <- sd(A20$SURPRISE)
A20$SURPRISE_S20 <- A20$SURPRISE/sigma20
A20$t<-A20$HOUR*60+A20$MINUTE
A20<-data.frame(A20$YEAR,A20$MONTH,A20$DAY,A20$t,A20$SURPRISE_S20)
names(A20)[1] <- 'YEAR'
names(A20)[2] <- 'MONTH'
names(A20)[3] <- 'DAY'
names(A20)[4] <- 't'
names(A20)[5] <-'SURPRISE_S20'

X <- merge(X,A20,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/cg3dponhzojde77/Producer_price_index.csv?dl=1"
A21<-read.csv(url(URL))
A21$SURPRISE <- A21$Actual - A21$Forecast
sigma21 <- sd(A21$SURPRISE)
A21$SURPRISE_S21 <- A21$SURPRISE/sigma21
A21$t<-A21$HOUR*60+A21$MINUTE
A21<-data.frame(A21$YEAR,A21$MONTH,A21$DAY,A21$t,A21$SURPRISE_S21)
names(A21)[1] <- 'YEAR'
names(A21)[2] <- 'MONTH'
names(A21)[3] <- 'DAY'
names(A21)[4] <- 't'
names(A21)[5] <-'SURPRISE_S21'

X <- merge(X,A21,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/8dgs5qtotikikzl/Trade_balance.csv?dl=1"
A22<-read.csv(url(URL))
A22$SURPRISE <- A22$Actual - A22$Forecast
sigma22 <- sd(A22$SURPRISE)
A22$SURPRISE_S22 <- A22$SURPRISE/sigma22
A22$t<-A22$HOUR*60+A22$MINUTE
A22<-data.frame(A22$YEAR,A22$MONTH,A22$DAY,A22$t,A22$SURPRISE_S22)
names(A22)[1] <- 'YEAR'
names(A22)[2] <- 'MONTH'
names(A22)[3] <- 'DAY'
names(A22)[4] <- 't'
names(A22)[5] <-'SURPRISE_S22'

X <- merge(X,A22,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

load("~/Desktop/codeR_CH1/CL_COT.Rda")
load("~/Desktop/codeR_CH1/CL_PC.Rda")


X_COT <- merge(X,CL_COT,by=c("DAY","MONTH","YEAR"),all.x = FALSE)
X_PC <- merge(X,CL_PC,by=c("DAY","MONTH","YEAR"),all.x = FALSE)
save(X_COT,file = "BD_CL_COT.Rda")
save(X_PC,file = "BD_CL_PC.Rda")


X<-read.csv("GC_5m.csv",header = FALSE,sep = ";")
names(X)[1]<-"Date"
names(X)[2]<-"Time"
names(X)[3]<-"Open"
names(X)[4]<-"High"
names(X)[5]<-"Low"
names(X)[6]<-"Close"
names(X)[7]<-"Volume"
X$DAY<-as.numeric(substr(X$Date,1,2))
X$MONTH<-as.numeric(substr(X$Date,4,5))
X$YEAR<-as.numeric(substr(X$Date,7,10))
X$HOUR<-as.numeric(substr(X$Time,1,2))
X$MINUTE<-as.numeric(substr(X$Time,4,5))
X$r_t1<-(X$Close-X$Open)/X$Open
X$t<-X$HOUR*60+X$MINUTE
X2<-X
X2$t<-X2$t+5
X2<-data.frame(X2$YEAR,X2$MONTH,X2$DAY,X2$t,X2$r_t1)
names(X2)[1] <- 'YEAR'
names(X2)[2] <- 'MONTH'
names(X2)[3] <- 'DAY'
names(X2)[4] <- 't'
names(X2)[5] <-'r_m1'
X <- merge(X,X2,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/yhp46rkwnp4owd1/Initial_jobless_claims.csv?dl=1"
A1<-read.csv(url(URL))
A1$SURPRISE <- A1$Actual - A1$Forecast
sigma1 <- sd(A1$SURPRISE)
A1$SURPRISE_S1 <- A1$SURPRISE/sigma1
A1$t<-A1$HOUR*60+A1$MINUTE
A1<-data.frame(A1$YEAR,A1$MONTH,A1$DAY,A1$t,A1$SURPRISE_S1)
names(A1)[1] <- 'YEAR'
names(A1)[2] <- 'MONTH'
names(A1)[3] <- 'DAY'
names(A1)[4] <- 't'
names(A1)[5] <-'SURPRISE_S1'

X <- merge(X,A1,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

URL="https://www.dropbox.com/s/5l4qfy3w3vvzmgn/ADP_employment.csv?dl=1"
A2<-read.csv(url(URL))
A2$SURPRISE <- A2$Actual - A2$Forecast
sigma2 <- sd(A2$SURPRISE)
A2$SURPRISE_S2 <- A2$SURPRISE/sigma2
A2$t<-A2$HOUR*60+A2$MINUTE
A2<-data.frame(A2$YEAR,A2$MONTH,A2$DAY,A2$t,A2$SURPRISE_S2)
names(A2)[1] <- 'YEAR'
names(A2)[2] <- 'MONTH'
names(A2)[3] <- 'DAY'
names(A2)[4] <- 't'
names(A2)[5] <-'SURPRISE_S2'
X <- merge(X,A2,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/p8gkqlmk3tprx44/CB_Consumer.csv?dl=1"
A3<-read.csv(url(URL))
A3$SURPRISE <- A3$Actual - A3$Forecast
sigma3 <- sd(A3$SURPRISE)
A3$SURPRISE_S3 <- A3$SURPRISE/sigma3
A3$t<-A3$HOUR*60+A3$MINUTE
A3<-data.frame(A3$YEAR,A3$MONTH,A3$DAY,A3$t,A3$SURPRISE_S3)
names(A3)[1] <- 'YEAR'
names(A3)[2] <- 'MONTH'
names(A3)[3] <- 'DAY'
names(A3)[4] <- 't'
names(A3)[5] <-'SURPRISE_S3'
X <- merge(X,A3,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

URL="https://www.dropbox.com/s/bakb4abqodl0n10/Advance_retail_sales.csv?dl=1"
A4<-read.csv(url(URL))
A4$SURPRISE <- A4$Actual - A4$Forecast
sigma4 <- sd(A4$SURPRISE)
A4$SURPRISE_S4 <- A4$SURPRISE/sigma4
A4$t<-A4$HOUR*60+A4$MINUTE
A4<-data.frame(A4$YEAR,A4$MONTH,A4$DAY,A4$t,A4$SURPRISE_S4)
names(A4)[1] <- 'YEAR'
names(A4)[2] <- 'MONTH'
names(A4)[3] <- 'DAY'
names(A4)[4] <- 't'
names(A4)[5] <-'SURPRISE_S4'

X <- merge(X,A4,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/rnueh4nojue20y8/Building_permit%20.csv?dl=1"
A5<-read.csv(url(URL))
A5$SURPRISE <- A5$Actual - A5$Forecast
sigma5 <- sd(A5$SURPRISE)
A5$SURPRISE_S5 <- A5$SURPRISE/sigma5
A5$t<-A5$HOUR*60+A5$MINUTE
A5<-data.frame(A5$YEAR,A5$MONTH,A5$DAY,A5$t,A5$SURPRISE_S5)
names(A5)[1] <- 'YEAR'
names(A5)[2] <- 'MONTH'
names(A5)[3] <- 'DAY'
names(A5)[4] <- 't'
names(A5)[5] <-'SURPRISE_S5'

X <- merge(X,A5,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/e82g1sbs92brioi/Construction_spending.csv?dl=1"
A6<-read.csv(url(URL))
A6$SURPRISE <- A6$Actual - A6$Forecast
sigma6 <- sd(A6$SURPRISE)
A6$SURPRISE_S6 <- A6$SURPRISE/sigma6
A6$t<-A6$HOUR*60+A6$MINUTE
A6<-data.frame(A6$YEAR,A6$MONTH,A6$DAY,A6$t,A6$SURPRISE_S6)
names(A6)[1] <- 'YEAR'
names(A6)[2] <- 'MONTH'
names(A6)[3] <- 'DAY'
names(A6)[4] <- 't'
names(A6)[5] <-'SURPRISE_S6'

X <- merge(X,A6,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/h84c44xhk4mc40e/Consumer_credit.csv?dl=1"
A7<-read.csv(url(URL))
A7$SURPRISE <- A7$Actual - A7$Forecast
sigma7 <- sd(A7$SURPRISE)
A7$SURPRISE_S7 <- A7$SURPRISE/sigma7
A7$t<-A7$HOUR*60+A7$MINUTE
A7<-data.frame(A7$YEAR,A7$MONTH,A7$DAY,A7$t,A7$SURPRISE_S7)
names(A7)[1] <- 'YEAR'
names(A7)[2] <- 'MONTH'
names(A7)[3] <- 'DAY'
names(A7)[4] <- 't'
names(A7)[5] <-'SURPRISE_S7'

X <- merge(X,A7,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

URL="https://www.dropbox.com/s/l2m4j8x2p6i642o/Consumer_price_index.csv?dl=1"
A8<-read.csv(url(URL))
A8$SURPRISE <- A8$Actual - A8$Forecast
sigma8 <- sd(A8$SURPRISE)
A8$SURPRISE_S8 <- A8$SURPRISE/sigma8
A8$t<-A8$HOUR*60+A8$MINUTE
A8<-data.frame(A8$YEAR,A8$MONTH,A8$DAY,A8$t,A8$SURPRISE_S8)
names(A8)[1] <- 'YEAR'
names(A8)[2] <- 'MONTH'
names(A8)[3] <- 'DAY'
names(A8)[4] <- 't'
names(A8)[5] <-'SURPRISE_S8'

X <- merge(X,A8,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/j7uyjs118ehnejy/Durable_goods_orders.csv?dl=1"
A9<-read.csv(url(URL))
A9$SURPRISE <- A9$Actual - A9$Forecast
sigma9 <- sd(A9$SURPRISE)
A9$SURPRISE_S9 <- A9$SURPRISE/sigma9
A9$t<-A9$HOUR*60+A9$MINUTE
A9<-data.frame(A9$YEAR,A9$MONTH,A9$DAY,A9$t,A9$SURPRISE_S9)
names(A9)[1] <- 'YEAR'
names(A9)[2] <- 'MONTH'
names(A9)[3] <- 'DAY'
names(A9)[4] <- 't'
names(A9)[5] <-'SURPRISE_S9'

X <- merge(X,A9,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/zzknlv7pu0ie88b/Existing_home_sales.csv?dl=1"
A10<-read.csv(url(URL))
A10$SURPRISE <- A10$Actual - A10$Forecast
sigma10 <- sd(A10$SURPRISE)
A10$SURPRISE_S10 <- A10$SURPRISE/sigma10
A10$t<-A10$HOUR*60+A10$MINUTE
A10<-data.frame(A10$YEAR,A10$MONTH,A10$DAY,A10$t,A10$SURPRISE_S10)
names(A10)[1] <- 'YEAR'
names(A10)[2] <- 'MONTH'
names(A10)[3] <- 'DAY'
names(A10)[4] <- 't'
names(A10)[5] <-'SURPRISE_S10'

X <- merge(X,A10,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)



URL="https://www.dropbox.com/s/nozkudnltw6r0p4/Factory_orders.csv?dl=1"
A11<-read.csv(url(URL))
A11$SURPRISE <- A11$Actual - A11$Forecast
sigma11 <- sd(A11$SURPRISE)
A11$SURPRISE_S11 <- A11$SURPRISE/sigma11
A11$t<-A11$HOUR*60+A11$MINUTE
A11<-data.frame(A11$YEAR,A11$MONTH,A11$DAY,A11$t,A11$SURPRISE_S11)
names(A11)[1] <- 'YEAR'
names(A11)[2] <- 'MONTH'
names(A11)[3] <- 'DAY'
names(A11)[4] <- 't'
names(A11)[5] <-'SURPRISE_S11'

X <- merge(X,A11,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)



URL="https://www.dropbox.com/s/y35gccj7y9tvzub/GDP.csv?dl=1"

A12<-read.csv(url(URL))
A12$SURPRISE <- A12$Actual - A12$Forecast
sigma12 <- sd(A12$SURPRISE)
A12$SURPRISE_S12 <- A12$SURPRISE/sigma12
A12$t<-A12$HOUR*60+A12$MINUTE
A12<-data.frame(A12$YEAR,A12$MONTH,A12$DAY,A12$t,A12$SURPRISE_S12)
names(A12)[1] <- 'YEAR'
names(A12)[2] <- 'MONTH'
names(A12)[3] <- 'DAY'
names(A12)[4] <- 't'
names(A12)[5] <-'SURPRISE_S12'

X <- merge(X,A12,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)




URL="https://www.dropbox.com/s/l2jxw6lm13iedhc/Housing_start.csv?dl=1"
A13<-read.csv(url(URL))
A13$SURPRISE <- A13$Actual - A13$Forecast
sigma13 <- sd(A13$SURPRISE)
A13$SURPRISE_S13 <- A13$SURPRISE/sigma13
A13$t<-A13$HOUR*60+A13$MINUTE
A13<-data.frame(A13$YEAR,A13$MONTH,A13$DAY,A13$t,A13$SURPRISE_S13)
names(A13)[1] <- 'YEAR'
names(A13)[2] <- 'MONTH'
names(A13)[3] <- 'DAY'
names(A13)[4] <- 't'
names(A13)[5] <-'SURPRISE_S13'

X <- merge(X,A13,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/fzry11e8swj3rv2/Industrial_production.csv?dl=1"
A14<-read.csv(url(URL))
A14$SURPRISE <- A14$Actual - A14$Forecast
sigma14 <- sd(A14$SURPRISE)
A14$SURPRISE_S14 <- A14$SURPRISE/sigma14
A14$t<-A14$HOUR*60+A14$MINUTE
A14<-data.frame(A14$YEAR,A14$MONTH,A14$DAY,A14$t,A14$SURPRISE_S14)
names(A14)[1] <- 'YEAR'
names(A14)[2] <- 'MONTH'
names(A14)[3] <- 'DAY'
names(A14)[4] <- 't'
names(A14)[5] <-'SURPRISE_S14'

X <- merge(X,A14,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)




URL="https://www.dropbox.com/s/o5gwq9vo0fiihqx/Michigan.csv?dl=1"
A15<-read.csv(url(URL))
A15$SURPRISE <- A15$Actual - A15$Forecast
sigma15 <- sd(A15$SURPRISE)
A15$SURPRISE_S15 <- A15$SURPRISE/sigma15
A15$t<-A15$HOUR*60+A15$MINUTE
A15<-data.frame(A15$YEAR,A15$MONTH,A15$DAY,A15$t,A15$SURPRISE_S15)
names(A15)[1] <- 'YEAR'
names(A15)[2] <- 'MONTH'
names(A15)[3] <- 'DAY'
names(A15)[4] <- 't'
names(A15)[5] <-'SURPRISE_S15'

X <- merge(X,A15,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/y8pjv26j5pk0gpm/New_home_sales.csv?dl=1"
A16<-read.csv(url(URL))
A16$SURPRISE <- A16$Actual - A16$Forecast
sigma16 <- sd(A16$SURPRISE)
A16$SURPRISE_S16 <- A16$SURPRISE/sigma16
A16$t<-A16$HOUR*60+A16$MINUTE
A16<-data.frame(A16$YEAR,A16$MONTH,A16$DAY,A16$t,A16$SURPRISE_S16)
names(A16)[1] <- 'YEAR'
names(A16)[2] <- 'MONTH'
names(A16)[3] <- 'DAY'
names(A16)[4] <- 't'
names(A16)[5] <-'SURPRISE_S16'

X <- merge(X,A16,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/nykinyx0v651ddf/Nonfarm_employment.csv?dl=1"
A17<-read.csv(url(URL))
A17$SURPRISE <- A17$Actual - A17$Forecast
sigma17 <- sd(A17$SURPRISE)
A17$SURPRISE_S17 <- A17$SURPRISE/sigma17
A17$t<-A17$HOUR*60+A17$MINUTE
A17<-data.frame(A17$YEAR,A17$MONTH,A17$DAY,A17$t,A17$SURPRISE_S17)
names(A17)[1] <- 'YEAR'
names(A17)[2] <- 'MONTH'
names(A17)[3] <- 'DAY'
names(A17)[4] <- 't'
names(A17)[5] <-'SURPRISE_S17'

X <- merge(X,A17,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/1oab2f2r96ijj1x/Pending_home_sales.csv?dl=1"
A18<-read.csv(url(URL))
A18$SURPRISE <- A18$Actual - A18$Forecast
sigma18 <- sd(A18$SURPRISE)
A18$SURPRISE_S18 <- A18$SURPRISE/sigma18
A18$t<-A18$HOUR*60+A18$MINUTE
A18<-data.frame(A18$YEAR,A18$MONTH,A18$DAY,A18$t,A18$SURPRISE_S18)
names(A18)[1] <- 'YEAR'
names(A18)[2] <- 'MONTH'
names(A18)[3] <- 'DAY'
names(A18)[4] <- 't'
names(A18)[5] <-'SURPRISE_S18'

X <- merge(X,A18,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/av884s7bfv6bwlk/Personal_consumption.csv?dl=1"
A19<-read.csv(url(URL))
A19$SURPRISE <- A19$Actual - A19$Forecast
sigma19 <- sd(A19$SURPRISE)
A19$SURPRISE_S19 <- A19$SURPRISE/sigma19
A19$t<-A19$HOUR*60+A19$MINUTE
A19<-data.frame(A19$YEAR,A19$MONTH,A19$DAY,A19$t,A19$SURPRISE_S19)
names(A19)[1] <- 'YEAR'
names(A19)[2] <- 'MONTH'
names(A19)[3] <- 'DAY'
names(A19)[4] <- 't'
names(A19)[5] <-'SURPRISE_S19'

X <- merge(X,A19,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/r3kricv3436rv6n/Personal_income.csv?dl=1"
A20<-read.csv(url(URL))
A20$SURPRISE <- A20$Actual - A20$Forecast
sigma20 <- sd(A20$SURPRISE)
A20$SURPRISE_S20 <- A20$SURPRISE/sigma20
A20$t<-A20$HOUR*60+A20$MINUTE
A20<-data.frame(A20$YEAR,A20$MONTH,A20$DAY,A20$t,A20$SURPRISE_S20)
names(A20)[1] <- 'YEAR'
names(A20)[2] <- 'MONTH'
names(A20)[3] <- 'DAY'
names(A20)[4] <- 't'
names(A20)[5] <-'SURPRISE_S20'

X <- merge(X,A20,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/cg3dponhzojde77/Producer_price_index.csv?dl=1"
A21<-read.csv(url(URL))
A21$SURPRISE <- A21$Actual - A21$Forecast
sigma21 <- sd(A21$SURPRISE)
A21$SURPRISE_S21 <- A21$SURPRISE/sigma21
A21$t<-A21$HOUR*60+A21$MINUTE
A21<-data.frame(A21$YEAR,A21$MONTH,A21$DAY,A21$t,A21$SURPRISE_S21)
names(A21)[1] <- 'YEAR'
names(A21)[2] <- 'MONTH'
names(A21)[3] <- 'DAY'
names(A21)[4] <- 't'
names(A21)[5] <-'SURPRISE_S21'

X <- merge(X,A21,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/8dgs5qtotikikzl/Trade_balance.csv?dl=1"
A22<-read.csv(url(URL))
A22$SURPRISE <- A22$Actual - A22$Forecast
sigma22 <- sd(A22$SURPRISE)
A22$SURPRISE_S22 <- A22$SURPRISE/sigma22
A22$t<-A22$HOUR*60+A22$MINUTE
A22<-data.frame(A22$YEAR,A22$MONTH,A22$DAY,A22$t,A22$SURPRISE_S22)
names(A22)[1] <- 'YEAR'
names(A22)[2] <- 'MONTH'
names(A22)[3] <- 'DAY'
names(A22)[4] <- 't'
names(A22)[5] <-'SURPRISE_S22'

X <- merge(X,A22,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

load("~/Desktop/codeR_CH1/GC_COT.Rda")
load("~/Desktop/codeR_CH1/GC_PC.Rda")


X_COT <- merge(X,GC_COT,by=c("DAY","MONTH","YEAR"),all.x = FALSE)
X_PC <- merge(X,GC_PC,by=c("DAY","MONTH","YEAR"),all.x = FALSE)
save(X_COT,file = "BD_GC_COT.Rda")
save(X_PC,file = "BD_GC_PC.Rda")


X<-read.csv("GC_5m.csv",header = FALSE,sep = ";")
names(X)[1]<-"Date"
names(X)[2]<-"Time"
names(X)[3]<-"Open"
names(X)[4]<-"High"
names(X)[5]<-"Low"
names(X)[6]<-"Close"
names(X)[7]<-"Volume"
X$DAY<-as.numeric(substr(X$Date,1,2))
X$MONTH<-as.numeric(substr(X$Date,4,5))
X$YEAR<-as.numeric(substr(X$Date,7,10))
X$HOUR<-as.numeric(substr(X$Time,1,2))
X$MINUTE<-as.numeric(substr(X$Time,4,5))
X$r_t1<-(X$Close-X$Open)/X$Open
X$t<-X$HOUR*60+X$MINUTE
X2<-X
X2$t<-X2$t+5
X2<-data.frame(X2$YEAR,X2$MONTH,X2$DAY,X2$t,X2$r_t1)
names(X2)[1] <- 'YEAR'
names(X2)[2] <- 'MONTH'
names(X2)[3] <- 'DAY'
names(X2)[4] <- 't'
names(X2)[5] <-'r_m1'
X <- merge(X,X2,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/yhp46rkwnp4owd1/Initial_jobless_claims.csv?dl=1"
A1<-read.csv(url(URL))
A1$SURPRISE <- A1$Actual - A1$Forecast
sigma1 <- sd(A1$SURPRISE)
A1$SURPRISE_S1 <- A1$SURPRISE/sigma1
A1$t<-A1$HOUR*60+A1$MINUTE
A1<-data.frame(A1$YEAR,A1$MONTH,A1$DAY,A1$t,A1$SURPRISE_S1)
names(A1)[1] <- 'YEAR'
names(A1)[2] <- 'MONTH'
names(A1)[3] <- 'DAY'
names(A1)[4] <- 't'
names(A1)[5] <-'SURPRISE_S1'

X <- merge(X,A1,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

URL="https://www.dropbox.com/s/5l4qfy3w3vvzmgn/ADP_employment.csv?dl=1"
A2<-read.csv(url(URL))
A2$SURPRISE <- A2$Actual - A2$Forecast
sigma2 <- sd(A2$SURPRISE)
A2$SURPRISE_S2 <- A2$SURPRISE/sigma2
A2$t<-A2$HOUR*60+A2$MINUTE
A2<-data.frame(A2$YEAR,A2$MONTH,A2$DAY,A2$t,A2$SURPRISE_S2)
names(A2)[1] <- 'YEAR'
names(A2)[2] <- 'MONTH'
names(A2)[3] <- 'DAY'
names(A2)[4] <- 't'
names(A2)[5] <-'SURPRISE_S2'
X <- merge(X,A2,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/p8gkqlmk3tprx44/CB_Consumer.csv?dl=1"
A3<-read.csv(url(URL))
A3$SURPRISE <- A3$Actual - A3$Forecast
sigma3 <- sd(A3$SURPRISE)
A3$SURPRISE_S3 <- A3$SURPRISE/sigma3
A3$t<-A3$HOUR*60+A3$MINUTE
A3<-data.frame(A3$YEAR,A3$MONTH,A3$DAY,A3$t,A3$SURPRISE_S3)
names(A3)[1] <- 'YEAR'
names(A3)[2] <- 'MONTH'
names(A3)[3] <- 'DAY'
names(A3)[4] <- 't'
names(A3)[5] <-'SURPRISE_S3'
X <- merge(X,A3,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

URL="https://www.dropbox.com/s/bakb4abqodl0n10/Advance_retail_sales.csv?dl=1"
A4<-read.csv(url(URL))
A4$SURPRISE <- A4$Actual - A4$Forecast
sigma4 <- sd(A4$SURPRISE)
A4$SURPRISE_S4 <- A4$SURPRISE/sigma4
A4$t<-A4$HOUR*60+A4$MINUTE
A4<-data.frame(A4$YEAR,A4$MONTH,A4$DAY,A4$t,A4$SURPRISE_S4)
names(A4)[1] <- 'YEAR'
names(A4)[2] <- 'MONTH'
names(A4)[3] <- 'DAY'
names(A4)[4] <- 't'
names(A4)[5] <-'SURPRISE_S4'

X <- merge(X,A4,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/rnueh4nojue20y8/Building_permit%20.csv?dl=1"
A5<-read.csv(url(URL))
A5$SURPRISE <- A5$Actual - A5$Forecast
sigma5 <- sd(A5$SURPRISE)
A5$SURPRISE_S5 <- A5$SURPRISE/sigma5
A5$t<-A5$HOUR*60+A5$MINUTE
A5<-data.frame(A5$YEAR,A5$MONTH,A5$DAY,A5$t,A5$SURPRISE_S5)
names(A5)[1] <- 'YEAR'
names(A5)[2] <- 'MONTH'
names(A5)[3] <- 'DAY'
names(A5)[4] <- 't'
names(A5)[5] <-'SURPRISE_S5'

X <- merge(X,A5,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/e82g1sbs92brioi/Construction_spending.csv?dl=1"
A6<-read.csv(url(URL))
A6$SURPRISE <- A6$Actual - A6$Forecast
sigma6 <- sd(A6$SURPRISE)
A6$SURPRISE_S6 <- A6$SURPRISE/sigma6
A6$t<-A6$HOUR*60+A6$MINUTE
A6<-data.frame(A6$YEAR,A6$MONTH,A6$DAY,A6$t,A6$SURPRISE_S6)
names(A6)[1] <- 'YEAR'
names(A6)[2] <- 'MONTH'
names(A6)[3] <- 'DAY'
names(A6)[4] <- 't'
names(A6)[5] <-'SURPRISE_S6'

X <- merge(X,A6,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/h84c44xhk4mc40e/Consumer_credit.csv?dl=1"
A7<-read.csv(url(URL))
A7$SURPRISE <- A7$Actual - A7$Forecast
sigma7 <- sd(A7$SURPRISE)
A7$SURPRISE_S7 <- A7$SURPRISE/sigma7
A7$t<-A7$HOUR*60+A7$MINUTE
A7<-data.frame(A7$YEAR,A7$MONTH,A7$DAY,A7$t,A7$SURPRISE_S7)
names(A7)[1] <- 'YEAR'
names(A7)[2] <- 'MONTH'
names(A7)[3] <- 'DAY'
names(A7)[4] <- 't'
names(A7)[5] <-'SURPRISE_S7'

X <- merge(X,A7,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

URL="https://www.dropbox.com/s/l2m4j8x2p6i642o/Consumer_price_index.csv?dl=1"
A8<-read.csv(url(URL))
A8$SURPRISE <- A8$Actual - A8$Forecast
sigma8 <- sd(A8$SURPRISE)
A8$SURPRISE_S8 <- A8$SURPRISE/sigma8
A8$t<-A8$HOUR*60+A8$MINUTE
A8<-data.frame(A8$YEAR,A8$MONTH,A8$DAY,A8$t,A8$SURPRISE_S8)
names(A8)[1] <- 'YEAR'
names(A8)[2] <- 'MONTH'
names(A8)[3] <- 'DAY'
names(A8)[4] <- 't'
names(A8)[5] <-'SURPRISE_S8'

X <- merge(X,A8,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/j7uyjs118ehnejy/Durable_goods_orders.csv?dl=1"
A9<-read.csv(url(URL))
A9$SURPRISE <- A9$Actual - A9$Forecast
sigma9 <- sd(A9$SURPRISE)
A9$SURPRISE_S9 <- A9$SURPRISE/sigma9
A9$t<-A9$HOUR*60+A9$MINUTE
A9<-data.frame(A9$YEAR,A9$MONTH,A9$DAY,A9$t,A9$SURPRISE_S9)
names(A9)[1] <- 'YEAR'
names(A9)[2] <- 'MONTH'
names(A9)[3] <- 'DAY'
names(A9)[4] <- 't'
names(A9)[5] <-'SURPRISE_S9'

X <- merge(X,A9,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/zzknlv7pu0ie88b/Existing_home_sales.csv?dl=1"
A10<-read.csv(url(URL))
A10$SURPRISE <- A10$Actual - A10$Forecast
sigma10 <- sd(A10$SURPRISE)
A10$SURPRISE_S10 <- A10$SURPRISE/sigma10
A10$t<-A10$HOUR*60+A10$MINUTE
A10<-data.frame(A10$YEAR,A10$MONTH,A10$DAY,A10$t,A10$SURPRISE_S10)
names(A10)[1] <- 'YEAR'
names(A10)[2] <- 'MONTH'
names(A10)[3] <- 'DAY'
names(A10)[4] <- 't'
names(A10)[5] <-'SURPRISE_S10'

X <- merge(X,A10,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)



URL="https://www.dropbox.com/s/nozkudnltw6r0p4/Factory_orders.csv?dl=1"
A11<-read.csv(url(URL))
A11$SURPRISE <- A11$Actual - A11$Forecast
sigma11 <- sd(A11$SURPRISE)
A11$SURPRISE_S11 <- A11$SURPRISE/sigma11
A11$t<-A11$HOUR*60+A11$MINUTE
A11<-data.frame(A11$YEAR,A11$MONTH,A11$DAY,A11$t,A11$SURPRISE_S11)
names(A11)[1] <- 'YEAR'
names(A11)[2] <- 'MONTH'
names(A11)[3] <- 'DAY'
names(A11)[4] <- 't'
names(A11)[5] <-'SURPRISE_S11'

X <- merge(X,A11,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)



URL="https://www.dropbox.com/s/y35gccj7y9tvzub/GDP.csv?dl=1"

A12<-read.csv(url(URL))
A12$SURPRISE <- A12$Actual - A12$Forecast
sigma12 <- sd(A12$SURPRISE)
A12$SURPRISE_S12 <- A12$SURPRISE/sigma12
A12$t<-A12$HOUR*60+A12$MINUTE
A12<-data.frame(A12$YEAR,A12$MONTH,A12$DAY,A12$t,A12$SURPRISE_S12)
names(A12)[1] <- 'YEAR'
names(A12)[2] <- 'MONTH'
names(A12)[3] <- 'DAY'
names(A12)[4] <- 't'
names(A12)[5] <-'SURPRISE_S12'

X <- merge(X,A12,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)




URL="https://www.dropbox.com/s/l2jxw6lm13iedhc/Housing_start.csv?dl=1"
A13<-read.csv(url(URL))
A13$SURPRISE <- A13$Actual - A13$Forecast
sigma13 <- sd(A13$SURPRISE)
A13$SURPRISE_S13 <- A13$SURPRISE/sigma13
A13$t<-A13$HOUR*60+A13$MINUTE
A13<-data.frame(A13$YEAR,A13$MONTH,A13$DAY,A13$t,A13$SURPRISE_S13)
names(A13)[1] <- 'YEAR'
names(A13)[2] <- 'MONTH'
names(A13)[3] <- 'DAY'
names(A13)[4] <- 't'
names(A13)[5] <-'SURPRISE_S13'

X <- merge(X,A13,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/fzry11e8swj3rv2/Industrial_production.csv?dl=1"
A14<-read.csv(url(URL))
A14$SURPRISE <- A14$Actual - A14$Forecast
sigma14 <- sd(A14$SURPRISE)
A14$SURPRISE_S14 <- A14$SURPRISE/sigma14
A14$t<-A14$HOUR*60+A14$MINUTE
A14<-data.frame(A14$YEAR,A14$MONTH,A14$DAY,A14$t,A14$SURPRISE_S14)
names(A14)[1] <- 'YEAR'
names(A14)[2] <- 'MONTH'
names(A14)[3] <- 'DAY'
names(A14)[4] <- 't'
names(A14)[5] <-'SURPRISE_S14'

X <- merge(X,A14,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)




URL="https://www.dropbox.com/s/o5gwq9vo0fiihqx/Michigan.csv?dl=1"
A15<-read.csv(url(URL))
A15$SURPRISE <- A15$Actual - A15$Forecast
sigma15 <- sd(A15$SURPRISE)
A15$SURPRISE_S15 <- A15$SURPRISE/sigma15
A15$t<-A15$HOUR*60+A15$MINUTE
A15<-data.frame(A15$YEAR,A15$MONTH,A15$DAY,A15$t,A15$SURPRISE_S15)
names(A15)[1] <- 'YEAR'
names(A15)[2] <- 'MONTH'
names(A15)[3] <- 'DAY'
names(A15)[4] <- 't'
names(A15)[5] <-'SURPRISE_S15'

X <- merge(X,A15,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/y8pjv26j5pk0gpm/New_home_sales.csv?dl=1"
A16<-read.csv(url(URL))
A16$SURPRISE <- A16$Actual - A16$Forecast
sigma16 <- sd(A16$SURPRISE)
A16$SURPRISE_S16 <- A16$SURPRISE/sigma16
A16$t<-A16$HOUR*60+A16$MINUTE
A16<-data.frame(A16$YEAR,A16$MONTH,A16$DAY,A16$t,A16$SURPRISE_S16)
names(A16)[1] <- 'YEAR'
names(A16)[2] <- 'MONTH'
names(A16)[3] <- 'DAY'
names(A16)[4] <- 't'
names(A16)[5] <-'SURPRISE_S16'

X <- merge(X,A16,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/nykinyx0v651ddf/Nonfarm_employment.csv?dl=1"
A17<-read.csv(url(URL))
A17$SURPRISE <- A17$Actual - A17$Forecast
sigma17 <- sd(A17$SURPRISE)
A17$SURPRISE_S17 <- A17$SURPRISE/sigma17
A17$t<-A17$HOUR*60+A17$MINUTE
A17<-data.frame(A17$YEAR,A17$MONTH,A17$DAY,A17$t,A17$SURPRISE_S17)
names(A17)[1] <- 'YEAR'
names(A17)[2] <- 'MONTH'
names(A17)[3] <- 'DAY'
names(A17)[4] <- 't'
names(A17)[5] <-'SURPRISE_S17'

X <- merge(X,A17,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/1oab2f2r96ijj1x/Pending_home_sales.csv?dl=1"
A18<-read.csv(url(URL))
A18$SURPRISE <- A18$Actual - A18$Forecast
sigma18 <- sd(A18$SURPRISE)
A18$SURPRISE_S18 <- A18$SURPRISE/sigma18
A18$t<-A18$HOUR*60+A18$MINUTE
A18<-data.frame(A18$YEAR,A18$MONTH,A18$DAY,A18$t,A18$SURPRISE_S18)
names(A18)[1] <- 'YEAR'
names(A18)[2] <- 'MONTH'
names(A18)[3] <- 'DAY'
names(A18)[4] <- 't'
names(A18)[5] <-'SURPRISE_S18'

X <- merge(X,A18,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/av884s7bfv6bwlk/Personal_consumption.csv?dl=1"
A19<-read.csv(url(URL))
A19$SURPRISE <- A19$Actual - A19$Forecast
sigma19 <- sd(A19$SURPRISE)
A19$SURPRISE_S19 <- A19$SURPRISE/sigma19
A19$t<-A19$HOUR*60+A19$MINUTE
A19<-data.frame(A19$YEAR,A19$MONTH,A19$DAY,A19$t,A19$SURPRISE_S19)
names(A19)[1] <- 'YEAR'
names(A19)[2] <- 'MONTH'
names(A19)[3] <- 'DAY'
names(A19)[4] <- 't'
names(A19)[5] <-'SURPRISE_S19'

X <- merge(X,A19,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/r3kricv3436rv6n/Personal_income.csv?dl=1"
A20<-read.csv(url(URL))
A20$SURPRISE <- A20$Actual - A20$Forecast
sigma20 <- sd(A20$SURPRISE)
A20$SURPRISE_S20 <- A20$SURPRISE/sigma20
A20$t<-A20$HOUR*60+A20$MINUTE
A20<-data.frame(A20$YEAR,A20$MONTH,A20$DAY,A20$t,A20$SURPRISE_S20)
names(A20)[1] <- 'YEAR'
names(A20)[2] <- 'MONTH'
names(A20)[3] <- 'DAY'
names(A20)[4] <- 't'
names(A20)[5] <-'SURPRISE_S20'

X <- merge(X,A20,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/cg3dponhzojde77/Producer_price_index.csv?dl=1"
A21<-read.csv(url(URL))
A21$SURPRISE <- A21$Actual - A21$Forecast
sigma21 <- sd(A21$SURPRISE)
A21$SURPRISE_S21 <- A21$SURPRISE/sigma21
A21$t<-A21$HOUR*60+A21$MINUTE
A21<-data.frame(A21$YEAR,A21$MONTH,A21$DAY,A21$t,A21$SURPRISE_S21)
names(A21)[1] <- 'YEAR'
names(A21)[2] <- 'MONTH'
names(A21)[3] <- 'DAY'
names(A21)[4] <- 't'
names(A21)[5] <-'SURPRISE_S21'

X <- merge(X,A21,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/8dgs5qtotikikzl/Trade_balance.csv?dl=1"
A22<-read.csv(url(URL))
A22$SURPRISE <- A22$Actual - A22$Forecast
sigma22 <- sd(A22$SURPRISE)
A22$SURPRISE_S22 <- A22$SURPRISE/sigma22
A22$t<-A22$HOUR*60+A22$MINUTE
A22<-data.frame(A22$YEAR,A22$MONTH,A22$DAY,A22$t,A22$SURPRISE_S22)
names(A22)[1] <- 'YEAR'
names(A22)[2] <- 'MONTH'
names(A22)[3] <- 'DAY'
names(A22)[4] <- 't'
names(A22)[5] <-'SURPRISE_S22'

X <- merge(X,A22,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

load("~/Desktop/codeR_CH1/HG_COT.Rda")
load("~/Desktop/codeR_CH1/HG_PC.Rda")


X_COT <- merge(X,HG_COT,by=c("DAY","MONTH","YEAR"),all.x = FALSE)
X_PC <- merge(X,HG_PC,by=c("DAY","MONTH","YEAR"),all.x = FALSE)
save(X_COT,file = "BD_HG_COT.Rda")
save(X_PC,file = "BD_HG_PC.Rda")


X<-read.csv("NG_5m.csv",header = FALSE,sep = ";")
names(X)[1]<-"Date"
names(X)[2]<-"Time"
names(X)[3]<-"Open"
names(X)[4]<-"High"
names(X)[5]<-"Low"
names(X)[6]<-"Close"
names(X)[7]<-"Volume"
X$DAY<-as.numeric(substr(X$Date,1,2))
X$MONTH<-as.numeric(substr(X$Date,4,5))
X$YEAR<-as.numeric(substr(X$Date,7,10))
X$HOUR<-as.numeric(substr(X$Time,1,2))
X$MINUTE<-as.numeric(substr(X$Time,4,5))
X$r_t1<-(X$Close-X$Open)/X$Open
X$t<-X$HOUR*60+X$MINUTE
X2<-X
X2$t<-X2$t+5
X2<-data.frame(X2$YEAR,X2$MONTH,X2$DAY,X2$t,X2$r_t1)
names(X2)[1] <- 'YEAR'
names(X2)[2] <- 'MONTH'
names(X2)[3] <- 'DAY'
names(X2)[4] <- 't'
names(X2)[5] <-'r_m1'
X <- merge(X,X2,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/yhp46rkwnp4owd1/Initial_jobless_claims.csv?dl=1"
A1<-read.csv(url(URL))
A1$SURPRISE <- A1$Actual - A1$Forecast
sigma1 <- sd(A1$SURPRISE)
A1$SURPRISE_S1 <- A1$SURPRISE/sigma1
A1$t<-A1$HOUR*60+A1$MINUTE
A1<-data.frame(A1$YEAR,A1$MONTH,A1$DAY,A1$t,A1$SURPRISE_S1)
names(A1)[1] <- 'YEAR'
names(A1)[2] <- 'MONTH'
names(A1)[3] <- 'DAY'
names(A1)[4] <- 't'
names(A1)[5] <-'SURPRISE_S1'

X <- merge(X,A1,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

URL="https://www.dropbox.com/s/5l4qfy3w3vvzmgn/ADP_employment.csv?dl=1"
A2<-read.csv(url(URL))
A2$SURPRISE <- A2$Actual - A2$Forecast
sigma2 <- sd(A2$SURPRISE)
A2$SURPRISE_S2 <- A2$SURPRISE/sigma2
A2$t<-A2$HOUR*60+A2$MINUTE
A2<-data.frame(A2$YEAR,A2$MONTH,A2$DAY,A2$t,A2$SURPRISE_S2)
names(A2)[1] <- 'YEAR'
names(A2)[2] <- 'MONTH'
names(A2)[3] <- 'DAY'
names(A2)[4] <- 't'
names(A2)[5] <-'SURPRISE_S2'
X <- merge(X,A2,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/p8gkqlmk3tprx44/CB_Consumer.csv?dl=1"
A3<-read.csv(url(URL))
A3$SURPRISE <- A3$Actual - A3$Forecast
sigma3 <- sd(A3$SURPRISE)
A3$SURPRISE_S3 <- A3$SURPRISE/sigma3
A3$t<-A3$HOUR*60+A3$MINUTE
A3<-data.frame(A3$YEAR,A3$MONTH,A3$DAY,A3$t,A3$SURPRISE_S3)
names(A3)[1] <- 'YEAR'
names(A3)[2] <- 'MONTH'
names(A3)[3] <- 'DAY'
names(A3)[4] <- 't'
names(A3)[5] <-'SURPRISE_S3'
X <- merge(X,A3,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

URL="https://www.dropbox.com/s/bakb4abqodl0n10/Advance_retail_sales.csv?dl=1"
A4<-read.csv(url(URL))
A4$SURPRISE <- A4$Actual - A4$Forecast
sigma4 <- sd(A4$SURPRISE)
A4$SURPRISE_S4 <- A4$SURPRISE/sigma4
A4$t<-A4$HOUR*60+A4$MINUTE
A4<-data.frame(A4$YEAR,A4$MONTH,A4$DAY,A4$t,A4$SURPRISE_S4)
names(A4)[1] <- 'YEAR'
names(A4)[2] <- 'MONTH'
names(A4)[3] <- 'DAY'
names(A4)[4] <- 't'
names(A4)[5] <-'SURPRISE_S4'

X <- merge(X,A4,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/rnueh4nojue20y8/Building_permit%20.csv?dl=1"
A5<-read.csv(url(URL))
A5$SURPRISE <- A5$Actual - A5$Forecast
sigma5 <- sd(A5$SURPRISE)
A5$SURPRISE_S5 <- A5$SURPRISE/sigma5
A5$t<-A5$HOUR*60+A5$MINUTE
A5<-data.frame(A5$YEAR,A5$MONTH,A5$DAY,A5$t,A5$SURPRISE_S5)
names(A5)[1] <- 'YEAR'
names(A5)[2] <- 'MONTH'
names(A5)[3] <- 'DAY'
names(A5)[4] <- 't'
names(A5)[5] <-'SURPRISE_S5'

X <- merge(X,A5,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/e82g1sbs92brioi/Construction_spending.csv?dl=1"
A6<-read.csv(url(URL))
A6$SURPRISE <- A6$Actual - A6$Forecast
sigma6 <- sd(A6$SURPRISE)
A6$SURPRISE_S6 <- A6$SURPRISE/sigma6
A6$t<-A6$HOUR*60+A6$MINUTE
A6<-data.frame(A6$YEAR,A6$MONTH,A6$DAY,A6$t,A6$SURPRISE_S6)
names(A6)[1] <- 'YEAR'
names(A6)[2] <- 'MONTH'
names(A6)[3] <- 'DAY'
names(A6)[4] <- 't'
names(A6)[5] <-'SURPRISE_S6'

X <- merge(X,A6,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/h84c44xhk4mc40e/Consumer_credit.csv?dl=1"
A7<-read.csv(url(URL))
A7$SURPRISE <- A7$Actual - A7$Forecast
sigma7 <- sd(A7$SURPRISE)
A7$SURPRISE_S7 <- A7$SURPRISE/sigma7
A7$t<-A7$HOUR*60+A7$MINUTE
A7<-data.frame(A7$YEAR,A7$MONTH,A7$DAY,A7$t,A7$SURPRISE_S7)
names(A7)[1] <- 'YEAR'
names(A7)[2] <- 'MONTH'
names(A7)[3] <- 'DAY'
names(A7)[4] <- 't'
names(A7)[5] <-'SURPRISE_S7'

X <- merge(X,A7,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

URL="https://www.dropbox.com/s/l2m4j8x2p6i642o/Consumer_price_index.csv?dl=1"
A8<-read.csv(url(URL))
A8$SURPRISE <- A8$Actual - A8$Forecast
sigma8 <- sd(A8$SURPRISE)
A8$SURPRISE_S8 <- A8$SURPRISE/sigma8
A8$t<-A8$HOUR*60+A8$MINUTE
A8<-data.frame(A8$YEAR,A8$MONTH,A8$DAY,A8$t,A8$SURPRISE_S8)
names(A8)[1] <- 'YEAR'
names(A8)[2] <- 'MONTH'
names(A8)[3] <- 'DAY'
names(A8)[4] <- 't'
names(A8)[5] <-'SURPRISE_S8'

X <- merge(X,A8,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/j7uyjs118ehnejy/Durable_goods_orders.csv?dl=1"
A9<-read.csv(url(URL))
A9$SURPRISE <- A9$Actual - A9$Forecast
sigma9 <- sd(A9$SURPRISE)
A9$SURPRISE_S9 <- A9$SURPRISE/sigma9
A9$t<-A9$HOUR*60+A9$MINUTE
A9<-data.frame(A9$YEAR,A9$MONTH,A9$DAY,A9$t,A9$SURPRISE_S9)
names(A9)[1] <- 'YEAR'
names(A9)[2] <- 'MONTH'
names(A9)[3] <- 'DAY'
names(A9)[4] <- 't'
names(A9)[5] <-'SURPRISE_S9'

X <- merge(X,A9,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/zzknlv7pu0ie88b/Existing_home_sales.csv?dl=1"
A10<-read.csv(url(URL))
A10$SURPRISE <- A10$Actual - A10$Forecast
sigma10 <- sd(A10$SURPRISE)
A10$SURPRISE_S10 <- A10$SURPRISE/sigma10
A10$t<-A10$HOUR*60+A10$MINUTE
A10<-data.frame(A10$YEAR,A10$MONTH,A10$DAY,A10$t,A10$SURPRISE_S10)
names(A10)[1] <- 'YEAR'
names(A10)[2] <- 'MONTH'
names(A10)[3] <- 'DAY'
names(A10)[4] <- 't'
names(A10)[5] <-'SURPRISE_S10'

X <- merge(X,A10,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)



URL="https://www.dropbox.com/s/nozkudnltw6r0p4/Factory_orders.csv?dl=1"
A11<-read.csv(url(URL))
A11$SURPRISE <- A11$Actual - A11$Forecast
sigma11 <- sd(A11$SURPRISE)
A11$SURPRISE_S11 <- A11$SURPRISE/sigma11
A11$t<-A11$HOUR*60+A11$MINUTE
A11<-data.frame(A11$YEAR,A11$MONTH,A11$DAY,A11$t,A11$SURPRISE_S11)
names(A11)[1] <- 'YEAR'
names(A11)[2] <- 'MONTH'
names(A11)[3] <- 'DAY'
names(A11)[4] <- 't'
names(A11)[5] <-'SURPRISE_S11'

X <- merge(X,A11,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)



URL="https://www.dropbox.com/s/y35gccj7y9tvzub/GDP.csv?dl=1"

A12<-read.csv(url(URL))
A12$SURPRISE <- A12$Actual - A12$Forecast
sigma12 <- sd(A12$SURPRISE)
A12$SURPRISE_S12 <- A12$SURPRISE/sigma12
A12$t<-A12$HOUR*60+A12$MINUTE
A12<-data.frame(A12$YEAR,A12$MONTH,A12$DAY,A12$t,A12$SURPRISE_S12)
names(A12)[1] <- 'YEAR'
names(A12)[2] <- 'MONTH'
names(A12)[3] <- 'DAY'
names(A12)[4] <- 't'
names(A12)[5] <-'SURPRISE_S12'

X <- merge(X,A12,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)




URL="https://www.dropbox.com/s/l2jxw6lm13iedhc/Housing_start.csv?dl=1"
A13<-read.csv(url(URL))
A13$SURPRISE <- A13$Actual - A13$Forecast
sigma13 <- sd(A13$SURPRISE)
A13$SURPRISE_S13 <- A13$SURPRISE/sigma13
A13$t<-A13$HOUR*60+A13$MINUTE
A13<-data.frame(A13$YEAR,A13$MONTH,A13$DAY,A13$t,A13$SURPRISE_S13)
names(A13)[1] <- 'YEAR'
names(A13)[2] <- 'MONTH'
names(A13)[3] <- 'DAY'
names(A13)[4] <- 't'
names(A13)[5] <-'SURPRISE_S13'

X <- merge(X,A13,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/fzry11e8swj3rv2/Industrial_production.csv?dl=1"
A14<-read.csv(url(URL))
A14$SURPRISE <- A14$Actual - A14$Forecast
sigma14 <- sd(A14$SURPRISE)
A14$SURPRISE_S14 <- A14$SURPRISE/sigma14
A14$t<-A14$HOUR*60+A14$MINUTE
A14<-data.frame(A14$YEAR,A14$MONTH,A14$DAY,A14$t,A14$SURPRISE_S14)
names(A14)[1] <- 'YEAR'
names(A14)[2] <- 'MONTH'
names(A14)[3] <- 'DAY'
names(A14)[4] <- 't'
names(A14)[5] <-'SURPRISE_S14'

X <- merge(X,A14,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)




URL="https://www.dropbox.com/s/o5gwq9vo0fiihqx/Michigan.csv?dl=1"
A15<-read.csv(url(URL))
A15$SURPRISE <- A15$Actual - A15$Forecast
sigma15 <- sd(A15$SURPRISE)
A15$SURPRISE_S15 <- A15$SURPRISE/sigma15
A15$t<-A15$HOUR*60+A15$MINUTE
A15<-data.frame(A15$YEAR,A15$MONTH,A15$DAY,A15$t,A15$SURPRISE_S15)
names(A15)[1] <- 'YEAR'
names(A15)[2] <- 'MONTH'
names(A15)[3] <- 'DAY'
names(A15)[4] <- 't'
names(A15)[5] <-'SURPRISE_S15'

X <- merge(X,A15,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/y8pjv26j5pk0gpm/New_home_sales.csv?dl=1"
A16<-read.csv(url(URL))
A16$SURPRISE <- A16$Actual - A16$Forecast
sigma16 <- sd(A16$SURPRISE)
A16$SURPRISE_S16 <- A16$SURPRISE/sigma16
A16$t<-A16$HOUR*60+A16$MINUTE
A16<-data.frame(A16$YEAR,A16$MONTH,A16$DAY,A16$t,A16$SURPRISE_S16)
names(A16)[1] <- 'YEAR'
names(A16)[2] <- 'MONTH'
names(A16)[3] <- 'DAY'
names(A16)[4] <- 't'
names(A16)[5] <-'SURPRISE_S16'

X <- merge(X,A16,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/nykinyx0v651ddf/Nonfarm_employment.csv?dl=1"
A17<-read.csv(url(URL))
A17$SURPRISE <- A17$Actual - A17$Forecast
sigma17 <- sd(A17$SURPRISE)
A17$SURPRISE_S17 <- A17$SURPRISE/sigma17
A17$t<-A17$HOUR*60+A17$MINUTE
A17<-data.frame(A17$YEAR,A17$MONTH,A17$DAY,A17$t,A17$SURPRISE_S17)
names(A17)[1] <- 'YEAR'
names(A17)[2] <- 'MONTH'
names(A17)[3] <- 'DAY'
names(A17)[4] <- 't'
names(A17)[5] <-'SURPRISE_S17'

X <- merge(X,A17,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/1oab2f2r96ijj1x/Pending_home_sales.csv?dl=1"
A18<-read.csv(url(URL))
A18$SURPRISE <- A18$Actual - A18$Forecast
sigma18 <- sd(A18$SURPRISE)
A18$SURPRISE_S18 <- A18$SURPRISE/sigma18
A18$t<-A18$HOUR*60+A18$MINUTE
A18<-data.frame(A18$YEAR,A18$MONTH,A18$DAY,A18$t,A18$SURPRISE_S18)
names(A18)[1] <- 'YEAR'
names(A18)[2] <- 'MONTH'
names(A18)[3] <- 'DAY'
names(A18)[4] <- 't'
names(A18)[5] <-'SURPRISE_S18'

X <- merge(X,A18,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/av884s7bfv6bwlk/Personal_consumption.csv?dl=1"
A19<-read.csv(url(URL))
A19$SURPRISE <- A19$Actual - A19$Forecast
sigma19 <- sd(A19$SURPRISE)
A19$SURPRISE_S19 <- A19$SURPRISE/sigma19
A19$t<-A19$HOUR*60+A19$MINUTE
A19<-data.frame(A19$YEAR,A19$MONTH,A19$DAY,A19$t,A19$SURPRISE_S19)
names(A19)[1] <- 'YEAR'
names(A19)[2] <- 'MONTH'
names(A19)[3] <- 'DAY'
names(A19)[4] <- 't'
names(A19)[5] <-'SURPRISE_S19'

X <- merge(X,A19,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/r3kricv3436rv6n/Personal_income.csv?dl=1"
A20<-read.csv(url(URL))
A20$SURPRISE <- A20$Actual - A20$Forecast
sigma20 <- sd(A20$SURPRISE)
A20$SURPRISE_S20 <- A20$SURPRISE/sigma20
A20$t<-A20$HOUR*60+A20$MINUTE
A20<-data.frame(A20$YEAR,A20$MONTH,A20$DAY,A20$t,A20$SURPRISE_S20)
names(A20)[1] <- 'YEAR'
names(A20)[2] <- 'MONTH'
names(A20)[3] <- 'DAY'
names(A20)[4] <- 't'
names(A20)[5] <-'SURPRISE_S20'

X <- merge(X,A20,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/cg3dponhzojde77/Producer_price_index.csv?dl=1"
A21<-read.csv(url(URL))
A21$SURPRISE <- A21$Actual - A21$Forecast
sigma21 <- sd(A21$SURPRISE)
A21$SURPRISE_S21 <- A21$SURPRISE/sigma21
A21$t<-A21$HOUR*60+A21$MINUTE
A21<-data.frame(A21$YEAR,A21$MONTH,A21$DAY,A21$t,A21$SURPRISE_S21)
names(A21)[1] <- 'YEAR'
names(A21)[2] <- 'MONTH'
names(A21)[3] <- 'DAY'
names(A21)[4] <- 't'
names(A21)[5] <-'SURPRISE_S21'

X <- merge(X,A21,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/8dgs5qtotikikzl/Trade_balance.csv?dl=1"
A22<-read.csv(url(URL))
A22$SURPRISE <- A22$Actual - A22$Forecast
sigma22 <- sd(A22$SURPRISE)
A22$SURPRISE_S22 <- A22$SURPRISE/sigma22
A22$t<-A22$HOUR*60+A22$MINUTE
A22<-data.frame(A22$YEAR,A22$MONTH,A22$DAY,A22$t,A22$SURPRISE_S22)
names(A22)[1] <- 'YEAR'
names(A22)[2] <- 'MONTH'
names(A22)[3] <- 'DAY'
names(A22)[4] <- 't'
names(A22)[5] <-'SURPRISE_S22'

X <- merge(X,A22,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

load("~/Desktop/codeR_CH1/NG_COT.Rda")
load("~/Desktop/codeR_CH1/NG_PC.Rda")


X_COT <- merge(X,NG_COT,by=c("DAY","MONTH","YEAR"),all.x = FALSE)
X_PC <- merge(X,NG_PC,by=c("DAY","MONTH","YEAR"),all.x = FALSE)
save(X_COT,file = "BD_NG_COT.Rda")
save(X_PC,file = "BD_NG_PC.Rda")




X<-read.csv("SI_5m.csv",header = FALSE,sep = ";")
names(X)[1]<-"Date"
names(X)[2]<-"Time"
names(X)[3]<-"Open"
names(X)[4]<-"High"
names(X)[5]<-"Low"
names(X)[6]<-"Close"
names(X)[7]<-"Volume"
X$DAY<-as.numeric(substr(X$Date,1,2))
X$MONTH<-as.numeric(substr(X$Date,4,5))
X$YEAR<-as.numeric(substr(X$Date,7,10))
X$HOUR<-as.numeric(substr(X$Time,1,2))
X$MINUTE<-as.numeric(substr(X$Time,4,5))
X$r_t1<-(X$Close-X$Open)/X$Open
X$t<-X$HOUR*60+X$MINUTE
X2<-X
X2$t<-X2$t+5
X2<-data.frame(X2$YEAR,X2$MONTH,X2$DAY,X2$t,X2$r_t1)
names(X2)[1] <- 'YEAR'
names(X2)[2] <- 'MONTH'
names(X2)[3] <- 'DAY'
names(X2)[4] <- 't'
names(X2)[5] <-'r_m1'
X <- merge(X,X2,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/yhp46rkwnp4owd1/Initial_jobless_claims.csv?dl=1"
A1<-read.csv(url(URL))
A1$SURPRISE <- A1$Actual - A1$Forecast
sigma1 <- sd(A1$SURPRISE)
A1$SURPRISE_S1 <- A1$SURPRISE/sigma1
A1$t<-A1$HOUR*60+A1$MINUTE
A1<-data.frame(A1$YEAR,A1$MONTH,A1$DAY,A1$t,A1$SURPRISE_S1)
names(A1)[1] <- 'YEAR'
names(A1)[2] <- 'MONTH'
names(A1)[3] <- 'DAY'
names(A1)[4] <- 't'
names(A1)[5] <-'SURPRISE_S1'

X <- merge(X,A1,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

URL="https://www.dropbox.com/s/5l4qfy3w3vvzmgn/ADP_employment.csv?dl=1"
A2<-read.csv(url(URL))
A2$SURPRISE <- A2$Actual - A2$Forecast
sigma2 <- sd(A2$SURPRISE)
A2$SURPRISE_S2 <- A2$SURPRISE/sigma2
A2$t<-A2$HOUR*60+A2$MINUTE
A2<-data.frame(A2$YEAR,A2$MONTH,A2$DAY,A2$t,A2$SURPRISE_S2)
names(A2)[1] <- 'YEAR'
names(A2)[2] <- 'MONTH'
names(A2)[3] <- 'DAY'
names(A2)[4] <- 't'
names(A2)[5] <-'SURPRISE_S2'
X <- merge(X,A2,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/p8gkqlmk3tprx44/CB_Consumer.csv?dl=1"
A3<-read.csv(url(URL))
A3$SURPRISE <- A3$Actual - A3$Forecast
sigma3 <- sd(A3$SURPRISE)
A3$SURPRISE_S3 <- A3$SURPRISE/sigma3
A3$t<-A3$HOUR*60+A3$MINUTE
A3<-data.frame(A3$YEAR,A3$MONTH,A3$DAY,A3$t,A3$SURPRISE_S3)
names(A3)[1] <- 'YEAR'
names(A3)[2] <- 'MONTH'
names(A3)[3] <- 'DAY'
names(A3)[4] <- 't'
names(A3)[5] <-'SURPRISE_S3'
X <- merge(X,A3,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

URL="https://www.dropbox.com/s/bakb4abqodl0n10/Advance_retail_sales.csv?dl=1"
A4<-read.csv(url(URL))
A4$SURPRISE <- A4$Actual - A4$Forecast
sigma4 <- sd(A4$SURPRISE)
A4$SURPRISE_S4 <- A4$SURPRISE/sigma4
A4$t<-A4$HOUR*60+A4$MINUTE
A4<-data.frame(A4$YEAR,A4$MONTH,A4$DAY,A4$t,A4$SURPRISE_S4)
names(A4)[1] <- 'YEAR'
names(A4)[2] <- 'MONTH'
names(A4)[3] <- 'DAY'
names(A4)[4] <- 't'
names(A4)[5] <-'SURPRISE_S4'

X <- merge(X,A4,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/rnueh4nojue20y8/Building_permit%20.csv?dl=1"
A5<-read.csv(url(URL))
A5$SURPRISE <- A5$Actual - A5$Forecast
sigma5 <- sd(A5$SURPRISE)
A5$SURPRISE_S5 <- A5$SURPRISE/sigma5
A5$t<-A5$HOUR*60+A5$MINUTE
A5<-data.frame(A5$YEAR,A5$MONTH,A5$DAY,A5$t,A5$SURPRISE_S5)
names(A5)[1] <- 'YEAR'
names(A5)[2] <- 'MONTH'
names(A5)[3] <- 'DAY'
names(A5)[4] <- 't'
names(A5)[5] <-'SURPRISE_S5'

X <- merge(X,A5,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/e82g1sbs92brioi/Construction_spending.csv?dl=1"
A6<-read.csv(url(URL))
A6$SURPRISE <- A6$Actual - A6$Forecast
sigma6 <- sd(A6$SURPRISE)
A6$SURPRISE_S6 <- A6$SURPRISE/sigma6
A6$t<-A6$HOUR*60+A6$MINUTE
A6<-data.frame(A6$YEAR,A6$MONTH,A6$DAY,A6$t,A6$SURPRISE_S6)
names(A6)[1] <- 'YEAR'
names(A6)[2] <- 'MONTH'
names(A6)[3] <- 'DAY'
names(A6)[4] <- 't'
names(A6)[5] <-'SURPRISE_S6'

X <- merge(X,A6,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/h84c44xhk4mc40e/Consumer_credit.csv?dl=1"
A7<-read.csv(url(URL))
A7$SURPRISE <- A7$Actual - A7$Forecast
sigma7 <- sd(A7$SURPRISE)
A7$SURPRISE_S7 <- A7$SURPRISE/sigma7
A7$t<-A7$HOUR*60+A7$MINUTE
A7<-data.frame(A7$YEAR,A7$MONTH,A7$DAY,A7$t,A7$SURPRISE_S7)
names(A7)[1] <- 'YEAR'
names(A7)[2] <- 'MONTH'
names(A7)[3] <- 'DAY'
names(A7)[4] <- 't'
names(A7)[5] <-'SURPRISE_S7'

X <- merge(X,A7,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

URL="https://www.dropbox.com/s/l2m4j8x2p6i642o/Consumer_price_index.csv?dl=1"
A8<-read.csv(url(URL))
A8$SURPRISE <- A8$Actual - A8$Forecast
sigma8 <- sd(A8$SURPRISE)
A8$SURPRISE_S8 <- A8$SURPRISE/sigma8
A8$t<-A8$HOUR*60+A8$MINUTE
A8<-data.frame(A8$YEAR,A8$MONTH,A8$DAY,A8$t,A8$SURPRISE_S8)
names(A8)[1] <- 'YEAR'
names(A8)[2] <- 'MONTH'
names(A8)[3] <- 'DAY'
names(A8)[4] <- 't'
names(A8)[5] <-'SURPRISE_S8'

X <- merge(X,A8,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/j7uyjs118ehnejy/Durable_goods_orders.csv?dl=1"
A9<-read.csv(url(URL))
A9$SURPRISE <- A9$Actual - A9$Forecast
sigma9 <- sd(A9$SURPRISE)
A9$SURPRISE_S9 <- A9$SURPRISE/sigma9
A9$t<-A9$HOUR*60+A9$MINUTE
A9<-data.frame(A9$YEAR,A9$MONTH,A9$DAY,A9$t,A9$SURPRISE_S9)
names(A9)[1] <- 'YEAR'
names(A9)[2] <- 'MONTH'
names(A9)[3] <- 'DAY'
names(A9)[4] <- 't'
names(A9)[5] <-'SURPRISE_S9'

X <- merge(X,A9,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/zzknlv7pu0ie88b/Existing_home_sales.csv?dl=1"
A10<-read.csv(url(URL))
A10$SURPRISE <- A10$Actual - A10$Forecast
sigma10 <- sd(A10$SURPRISE)
A10$SURPRISE_S10 <- A10$SURPRISE/sigma10
A10$t<-A10$HOUR*60+A10$MINUTE
A10<-data.frame(A10$YEAR,A10$MONTH,A10$DAY,A10$t,A10$SURPRISE_S10)
names(A10)[1] <- 'YEAR'
names(A10)[2] <- 'MONTH'
names(A10)[3] <- 'DAY'
names(A10)[4] <- 't'
names(A10)[5] <-'SURPRISE_S10'

X <- merge(X,A10,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)



URL="https://www.dropbox.com/s/nozkudnltw6r0p4/Factory_orders.csv?dl=1"
A11<-read.csv(url(URL))
A11$SURPRISE <- A11$Actual - A11$Forecast
sigma11 <- sd(A11$SURPRISE)
A11$SURPRISE_S11 <- A11$SURPRISE/sigma11
A11$t<-A11$HOUR*60+A11$MINUTE
A11<-data.frame(A11$YEAR,A11$MONTH,A11$DAY,A11$t,A11$SURPRISE_S11)
names(A11)[1] <- 'YEAR'
names(A11)[2] <- 'MONTH'
names(A11)[3] <- 'DAY'
names(A11)[4] <- 't'
names(A11)[5] <-'SURPRISE_S11'

X <- merge(X,A11,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)



URL="https://www.dropbox.com/s/y35gccj7y9tvzub/GDP.csv?dl=1"

A12<-read.csv(url(URL))
A12$SURPRISE <- A12$Actual - A12$Forecast
sigma12 <- sd(A12$SURPRISE)
A12$SURPRISE_S12 <- A12$SURPRISE/sigma12
A12$t<-A12$HOUR*60+A12$MINUTE
A12<-data.frame(A12$YEAR,A12$MONTH,A12$DAY,A12$t,A12$SURPRISE_S12)
names(A12)[1] <- 'YEAR'
names(A12)[2] <- 'MONTH'
names(A12)[3] <- 'DAY'
names(A12)[4] <- 't'
names(A12)[5] <-'SURPRISE_S12'

X <- merge(X,A12,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)




URL="https://www.dropbox.com/s/l2jxw6lm13iedhc/Housing_start.csv?dl=1"
A13<-read.csv(url(URL))
A13$SURPRISE <- A13$Actual - A13$Forecast
sigma13 <- sd(A13$SURPRISE)
A13$SURPRISE_S13 <- A13$SURPRISE/sigma13
A13$t<-A13$HOUR*60+A13$MINUTE
A13<-data.frame(A13$YEAR,A13$MONTH,A13$DAY,A13$t,A13$SURPRISE_S13)
names(A13)[1] <- 'YEAR'
names(A13)[2] <- 'MONTH'
names(A13)[3] <- 'DAY'
names(A13)[4] <- 't'
names(A13)[5] <-'SURPRISE_S13'

X <- merge(X,A13,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/fzry11e8swj3rv2/Industrial_production.csv?dl=1"
A14<-read.csv(url(URL))
A14$SURPRISE <- A14$Actual - A14$Forecast
sigma14 <- sd(A14$SURPRISE)
A14$SURPRISE_S14 <- A14$SURPRISE/sigma14
A14$t<-A14$HOUR*60+A14$MINUTE
A14<-data.frame(A14$YEAR,A14$MONTH,A14$DAY,A14$t,A14$SURPRISE_S14)
names(A14)[1] <- 'YEAR'
names(A14)[2] <- 'MONTH'
names(A14)[3] <- 'DAY'
names(A14)[4] <- 't'
names(A14)[5] <-'SURPRISE_S14'

X <- merge(X,A14,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)




URL="https://www.dropbox.com/s/o5gwq9vo0fiihqx/Michigan.csv?dl=1"
A15<-read.csv(url(URL))
A15$SURPRISE <- A15$Actual - A15$Forecast
sigma15 <- sd(A15$SURPRISE)
A15$SURPRISE_S15 <- A15$SURPRISE/sigma15
A15$t<-A15$HOUR*60+A15$MINUTE
A15<-data.frame(A15$YEAR,A15$MONTH,A15$DAY,A15$t,A15$SURPRISE_S15)
names(A15)[1] <- 'YEAR'
names(A15)[2] <- 'MONTH'
names(A15)[3] <- 'DAY'
names(A15)[4] <- 't'
names(A15)[5] <-'SURPRISE_S15'

X <- merge(X,A15,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/y8pjv26j5pk0gpm/New_home_sales.csv?dl=1"
A16<-read.csv(url(URL))
A16$SURPRISE <- A16$Actual - A16$Forecast
sigma16 <- sd(A16$SURPRISE)
A16$SURPRISE_S16 <- A16$SURPRISE/sigma16
A16$t<-A16$HOUR*60+A16$MINUTE
A16<-data.frame(A16$YEAR,A16$MONTH,A16$DAY,A16$t,A16$SURPRISE_S16)
names(A16)[1] <- 'YEAR'
names(A16)[2] <- 'MONTH'
names(A16)[3] <- 'DAY'
names(A16)[4] <- 't'
names(A16)[5] <-'SURPRISE_S16'

X <- merge(X,A16,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/nykinyx0v651ddf/Nonfarm_employment.csv?dl=1"
A17<-read.csv(url(URL))
A17$SURPRISE <- A17$Actual - A17$Forecast
sigma17 <- sd(A17$SURPRISE)
A17$SURPRISE_S17 <- A17$SURPRISE/sigma17
A17$t<-A17$HOUR*60+A17$MINUTE
A17<-data.frame(A17$YEAR,A17$MONTH,A17$DAY,A17$t,A17$SURPRISE_S17)
names(A17)[1] <- 'YEAR'
names(A17)[2] <- 'MONTH'
names(A17)[3] <- 'DAY'
names(A17)[4] <- 't'
names(A17)[5] <-'SURPRISE_S17'

X <- merge(X,A17,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/1oab2f2r96ijj1x/Pending_home_sales.csv?dl=1"
A18<-read.csv(url(URL))
A18$SURPRISE <- A18$Actual - A18$Forecast
sigma18 <- sd(A18$SURPRISE)
A18$SURPRISE_S18 <- A18$SURPRISE/sigma18
A18$t<-A18$HOUR*60+A18$MINUTE
A18<-data.frame(A18$YEAR,A18$MONTH,A18$DAY,A18$t,A18$SURPRISE_S18)
names(A18)[1] <- 'YEAR'
names(A18)[2] <- 'MONTH'
names(A18)[3] <- 'DAY'
names(A18)[4] <- 't'
names(A18)[5] <-'SURPRISE_S18'

X <- merge(X,A18,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/av884s7bfv6bwlk/Personal_consumption.csv?dl=1"
A19<-read.csv(url(URL))
A19$SURPRISE <- A19$Actual - A19$Forecast
sigma19 <- sd(A19$SURPRISE)
A19$SURPRISE_S19 <- A19$SURPRISE/sigma19
A19$t<-A19$HOUR*60+A19$MINUTE
A19<-data.frame(A19$YEAR,A19$MONTH,A19$DAY,A19$t,A19$SURPRISE_S19)
names(A19)[1] <- 'YEAR'
names(A19)[2] <- 'MONTH'
names(A19)[3] <- 'DAY'
names(A19)[4] <- 't'
names(A19)[5] <-'SURPRISE_S19'

X <- merge(X,A19,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/r3kricv3436rv6n/Personal_income.csv?dl=1"
A20<-read.csv(url(URL))
A20$SURPRISE <- A20$Actual - A20$Forecast
sigma20 <- sd(A20$SURPRISE)
A20$SURPRISE_S20 <- A20$SURPRISE/sigma20
A20$t<-A20$HOUR*60+A20$MINUTE
A20<-data.frame(A20$YEAR,A20$MONTH,A20$DAY,A20$t,A20$SURPRISE_S20)
names(A20)[1] <- 'YEAR'
names(A20)[2] <- 'MONTH'
names(A20)[3] <- 'DAY'
names(A20)[4] <- 't'
names(A20)[5] <-'SURPRISE_S20'

X <- merge(X,A20,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/cg3dponhzojde77/Producer_price_index.csv?dl=1"
A21<-read.csv(url(URL))
A21$SURPRISE <- A21$Actual - A21$Forecast
sigma21 <- sd(A21$SURPRISE)
A21$SURPRISE_S21 <- A21$SURPRISE/sigma21
A21$t<-A21$HOUR*60+A21$MINUTE
A21<-data.frame(A21$YEAR,A21$MONTH,A21$DAY,A21$t,A21$SURPRISE_S21)
names(A21)[1] <- 'YEAR'
names(A21)[2] <- 'MONTH'
names(A21)[3] <- 'DAY'
names(A21)[4] <- 't'
names(A21)[5] <-'SURPRISE_S21'

X <- merge(X,A21,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/8dgs5qtotikikzl/Trade_balance.csv?dl=1"
A22<-read.csv(url(URL))
A22$SURPRISE <- A22$Actual - A22$Forecast
sigma22 <- sd(A22$SURPRISE)
A22$SURPRISE_S22 <- A22$SURPRISE/sigma22
A22$t<-A22$HOUR*60+A22$MINUTE
A22<-data.frame(A22$YEAR,A22$MONTH,A22$DAY,A22$t,A22$SURPRISE_S22)
names(A22)[1] <- 'YEAR'
names(A22)[2] <- 'MONTH'
names(A22)[3] <- 'DAY'
names(A22)[4] <- 't'
names(A22)[5] <-'SURPRISE_S22'

X <- merge(X,A22,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

load("~/Desktop/codeR_CH1/SI_COT.Rda")
load("~/Desktop/codeR_CH1/SI_PC.Rda")


X_COT <- merge(X,SI_COT,by=c("DAY","MONTH","YEAR"),all.x = FALSE)
X_PC <- merge(X,SI_PC,by=c("DAY","MONTH","YEAR"),all.x = FALSE)
save(X_COT,file = "BD_SI_COT.Rda")
save(X_PC,file = "BD_SI_PC.Rda")


X<-read.csv("PA_5m.csv",header = FALSE,sep = ";")
names(X)[1]<-"Date"
names(X)[2]<-"Time"
names(X)[3]<-"Open"
names(X)[4]<-"High"
names(X)[5]<-"Low"
names(X)[6]<-"Close"
names(X)[7]<-"Volume"
X$DAY<-as.numeric(substr(X$Date,1,2))
X$MONTH<-as.numeric(substr(X$Date,4,5))
X$YEAR<-as.numeric(substr(X$Date,7,10))
X$HOUR<-as.numeric(substr(X$Time,1,2))
X$MINUTE<-as.numeric(substr(X$Time,4,5))
X$r_t1<-(X$Close-X$Open)/X$Open
X$t<-X$HOUR*60+X$MINUTE
X2<-X
X2$t<-X2$t+5
X2<-data.frame(X2$YEAR,X2$MONTH,X2$DAY,X2$t,X2$r_t1)
names(X2)[1] <- 'YEAR'
names(X2)[2] <- 'MONTH'
names(X2)[3] <- 'DAY'
names(X2)[4] <- 't'
names(X2)[5] <-'r_m1'
X <- merge(X,X2,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/yhp46rkwnp4owd1/Initial_jobless_claims.csv?dl=1"
A1<-read.csv(url(URL))
A1$SURPRISE <- A1$Actual - A1$Forecast
sigma1 <- sd(A1$SURPRISE)
A1$SURPRISE_S1 <- A1$SURPRISE/sigma1
A1$t<-A1$HOUR*60+A1$MINUTE
A1<-data.frame(A1$YEAR,A1$MONTH,A1$DAY,A1$t,A1$SURPRISE_S1)
names(A1)[1] <- 'YEAR'
names(A1)[2] <- 'MONTH'
names(A1)[3] <- 'DAY'
names(A1)[4] <- 't'
names(A1)[5] <-'SURPRISE_S1'

X <- merge(X,A1,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

URL="https://www.dropbox.com/s/5l4qfy3w3vvzmgn/ADP_employment.csv?dl=1"
A2<-read.csv(url(URL))
A2$SURPRISE <- A2$Actual - A2$Forecast
sigma2 <- sd(A2$SURPRISE)
A2$SURPRISE_S2 <- A2$SURPRISE/sigma2
A2$t<-A2$HOUR*60+A2$MINUTE
A2<-data.frame(A2$YEAR,A2$MONTH,A2$DAY,A2$t,A2$SURPRISE_S2)
names(A2)[1] <- 'YEAR'
names(A2)[2] <- 'MONTH'
names(A2)[3] <- 'DAY'
names(A2)[4] <- 't'
names(A2)[5] <-'SURPRISE_S2'
X <- merge(X,A2,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/p8gkqlmk3tprx44/CB_Consumer.csv?dl=1"
A3<-read.csv(url(URL))
A3$SURPRISE <- A3$Actual - A3$Forecast
sigma3 <- sd(A3$SURPRISE)
A3$SURPRISE_S3 <- A3$SURPRISE/sigma3
A3$t<-A3$HOUR*60+A3$MINUTE
A3<-data.frame(A3$YEAR,A3$MONTH,A3$DAY,A3$t,A3$SURPRISE_S3)
names(A3)[1] <- 'YEAR'
names(A3)[2] <- 'MONTH'
names(A3)[3] <- 'DAY'
names(A3)[4] <- 't'
names(A3)[5] <-'SURPRISE_S3'
X <- merge(X,A3,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

URL="https://www.dropbox.com/s/bakb4abqodl0n10/Advance_retail_sales.csv?dl=1"
A4<-read.csv(url(URL))
A4$SURPRISE <- A4$Actual - A4$Forecast
sigma4 <- sd(A4$SURPRISE)
A4$SURPRISE_S4 <- A4$SURPRISE/sigma4
A4$t<-A4$HOUR*60+A4$MINUTE
A4<-data.frame(A4$YEAR,A4$MONTH,A4$DAY,A4$t,A4$SURPRISE_S4)
names(A4)[1] <- 'YEAR'
names(A4)[2] <- 'MONTH'
names(A4)[3] <- 'DAY'
names(A4)[4] <- 't'
names(A4)[5] <-'SURPRISE_S4'

X <- merge(X,A4,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/rnueh4nojue20y8/Building_permit%20.csv?dl=1"
A5<-read.csv(url(URL))
A5$SURPRISE <- A5$Actual - A5$Forecast
sigma5 <- sd(A5$SURPRISE)
A5$SURPRISE_S5 <- A5$SURPRISE/sigma5
A5$t<-A5$HOUR*60+A5$MINUTE
A5<-data.frame(A5$YEAR,A5$MONTH,A5$DAY,A5$t,A5$SURPRISE_S5)
names(A5)[1] <- 'YEAR'
names(A5)[2] <- 'MONTH'
names(A5)[3] <- 'DAY'
names(A5)[4] <- 't'
names(A5)[5] <-'SURPRISE_S5'

X <- merge(X,A5,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/e82g1sbs92brioi/Construction_spending.csv?dl=1"
A6<-read.csv(url(URL))
A6$SURPRISE <- A6$Actual - A6$Forecast
sigma6 <- sd(A6$SURPRISE)
A6$SURPRISE_S6 <- A6$SURPRISE/sigma6
A6$t<-A6$HOUR*60+A6$MINUTE
A6<-data.frame(A6$YEAR,A6$MONTH,A6$DAY,A6$t,A6$SURPRISE_S6)
names(A6)[1] <- 'YEAR'
names(A6)[2] <- 'MONTH'
names(A6)[3] <- 'DAY'
names(A6)[4] <- 't'
names(A6)[5] <-'SURPRISE_S6'

X <- merge(X,A6,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/h84c44xhk4mc40e/Consumer_credit.csv?dl=1"
A7<-read.csv(url(URL))
A7$SURPRISE <- A7$Actual - A7$Forecast
sigma7 <- sd(A7$SURPRISE)
A7$SURPRISE_S7 <- A7$SURPRISE/sigma7
A7$t<-A7$HOUR*60+A7$MINUTE
A7<-data.frame(A7$YEAR,A7$MONTH,A7$DAY,A7$t,A7$SURPRISE_S7)
names(A7)[1] <- 'YEAR'
names(A7)[2] <- 'MONTH'
names(A7)[3] <- 'DAY'
names(A7)[4] <- 't'
names(A7)[5] <-'SURPRISE_S7'

X <- merge(X,A7,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

URL="https://www.dropbox.com/s/l2m4j8x2p6i642o/Consumer_price_index.csv?dl=1"
A8<-read.csv(url(URL))
A8$SURPRISE <- A8$Actual - A8$Forecast
sigma8 <- sd(A8$SURPRISE)
A8$SURPRISE_S8 <- A8$SURPRISE/sigma8
A8$t<-A8$HOUR*60+A8$MINUTE
A8<-data.frame(A8$YEAR,A8$MONTH,A8$DAY,A8$t,A8$SURPRISE_S8)
names(A8)[1] <- 'YEAR'
names(A8)[2] <- 'MONTH'
names(A8)[3] <- 'DAY'
names(A8)[4] <- 't'
names(A8)[5] <-'SURPRISE_S8'

X <- merge(X,A8,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/j7uyjs118ehnejy/Durable_goods_orders.csv?dl=1"
A9<-read.csv(url(URL))
A9$SURPRISE <- A9$Actual - A9$Forecast
sigma9 <- sd(A9$SURPRISE)
A9$SURPRISE_S9 <- A9$SURPRISE/sigma9
A9$t<-A9$HOUR*60+A9$MINUTE
A9<-data.frame(A9$YEAR,A9$MONTH,A9$DAY,A9$t,A9$SURPRISE_S9)
names(A9)[1] <- 'YEAR'
names(A9)[2] <- 'MONTH'
names(A9)[3] <- 'DAY'
names(A9)[4] <- 't'
names(A9)[5] <-'SURPRISE_S9'

X <- merge(X,A9,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/zzknlv7pu0ie88b/Existing_home_sales.csv?dl=1"
A10<-read.csv(url(URL))
A10$SURPRISE <- A10$Actual - A10$Forecast
sigma10 <- sd(A10$SURPRISE)
A10$SURPRISE_S10 <- A10$SURPRISE/sigma10
A10$t<-A10$HOUR*60+A10$MINUTE
A10<-data.frame(A10$YEAR,A10$MONTH,A10$DAY,A10$t,A10$SURPRISE_S10)
names(A10)[1] <- 'YEAR'
names(A10)[2] <- 'MONTH'
names(A10)[3] <- 'DAY'
names(A10)[4] <- 't'
names(A10)[5] <-'SURPRISE_S10'

X <- merge(X,A10,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)



URL="https://www.dropbox.com/s/nozkudnltw6r0p4/Factory_orders.csv?dl=1"
A11<-read.csv(url(URL))
A11$SURPRISE <- A11$Actual - A11$Forecast
sigma11 <- sd(A11$SURPRISE)
A11$SURPRISE_S11 <- A11$SURPRISE/sigma11
A11$t<-A11$HOUR*60+A11$MINUTE
A11<-data.frame(A11$YEAR,A11$MONTH,A11$DAY,A11$t,A11$SURPRISE_S11)
names(A11)[1] <- 'YEAR'
names(A11)[2] <- 'MONTH'
names(A11)[3] <- 'DAY'
names(A11)[4] <- 't'
names(A11)[5] <-'SURPRISE_S11'

X <- merge(X,A11,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)



URL="https://www.dropbox.com/s/y35gccj7y9tvzub/GDP.csv?dl=1"

A12<-read.csv(url(URL))
A12$SURPRISE <- A12$Actual - A12$Forecast
sigma12 <- sd(A12$SURPRISE)
A12$SURPRISE_S12 <- A12$SURPRISE/sigma12
A12$t<-A12$HOUR*60+A12$MINUTE
A12<-data.frame(A12$YEAR,A12$MONTH,A12$DAY,A12$t,A12$SURPRISE_S12)
names(A12)[1] <- 'YEAR'
names(A12)[2] <- 'MONTH'
names(A12)[3] <- 'DAY'
names(A12)[4] <- 't'
names(A12)[5] <-'SURPRISE_S12'

X <- merge(X,A12,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)




URL="https://www.dropbox.com/s/l2jxw6lm13iedhc/Housing_start.csv?dl=1"
A13<-read.csv(url(URL))
A13$SURPRISE <- A13$Actual - A13$Forecast
sigma13 <- sd(A13$SURPRISE)
A13$SURPRISE_S13 <- A13$SURPRISE/sigma13
A13$t<-A13$HOUR*60+A13$MINUTE
A13<-data.frame(A13$YEAR,A13$MONTH,A13$DAY,A13$t,A13$SURPRISE_S13)
names(A13)[1] <- 'YEAR'
names(A13)[2] <- 'MONTH'
names(A13)[3] <- 'DAY'
names(A13)[4] <- 't'
names(A13)[5] <-'SURPRISE_S13'

X <- merge(X,A13,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/fzry11e8swj3rv2/Industrial_production.csv?dl=1"
A14<-read.csv(url(URL))
A14$SURPRISE <- A14$Actual - A14$Forecast
sigma14 <- sd(A14$SURPRISE)
A14$SURPRISE_S14 <- A14$SURPRISE/sigma14
A14$t<-A14$HOUR*60+A14$MINUTE
A14<-data.frame(A14$YEAR,A14$MONTH,A14$DAY,A14$t,A14$SURPRISE_S14)
names(A14)[1] <- 'YEAR'
names(A14)[2] <- 'MONTH'
names(A14)[3] <- 'DAY'
names(A14)[4] <- 't'
names(A14)[5] <-'SURPRISE_S14'

X <- merge(X,A14,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)




URL="https://www.dropbox.com/s/o5gwq9vo0fiihqx/Michigan.csv?dl=1"
A15<-read.csv(url(URL))
A15$SURPRISE <- A15$Actual - A15$Forecast
sigma15 <- sd(A15$SURPRISE)
A15$SURPRISE_S15 <- A15$SURPRISE/sigma15
A15$t<-A15$HOUR*60+A15$MINUTE
A15<-data.frame(A15$YEAR,A15$MONTH,A15$DAY,A15$t,A15$SURPRISE_S15)
names(A15)[1] <- 'YEAR'
names(A15)[2] <- 'MONTH'
names(A15)[3] <- 'DAY'
names(A15)[4] <- 't'
names(A15)[5] <-'SURPRISE_S15'

X <- merge(X,A15,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/y8pjv26j5pk0gpm/New_home_sales.csv?dl=1"
A16<-read.csv(url(URL))
A16$SURPRISE <- A16$Actual - A16$Forecast
sigma16 <- sd(A16$SURPRISE)
A16$SURPRISE_S16 <- A16$SURPRISE/sigma16
A16$t<-A16$HOUR*60+A16$MINUTE
A16<-data.frame(A16$YEAR,A16$MONTH,A16$DAY,A16$t,A16$SURPRISE_S16)
names(A16)[1] <- 'YEAR'
names(A16)[2] <- 'MONTH'
names(A16)[3] <- 'DAY'
names(A16)[4] <- 't'
names(A16)[5] <-'SURPRISE_S16'

X <- merge(X,A16,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/nykinyx0v651ddf/Nonfarm_employment.csv?dl=1"
A17<-read.csv(url(URL))
A17$SURPRISE <- A17$Actual - A17$Forecast
sigma17 <- sd(A17$SURPRISE)
A17$SURPRISE_S17 <- A17$SURPRISE/sigma17
A17$t<-A17$HOUR*60+A17$MINUTE
A17<-data.frame(A17$YEAR,A17$MONTH,A17$DAY,A17$t,A17$SURPRISE_S17)
names(A17)[1] <- 'YEAR'
names(A17)[2] <- 'MONTH'
names(A17)[3] <- 'DAY'
names(A17)[4] <- 't'
names(A17)[5] <-'SURPRISE_S17'

X <- merge(X,A17,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/1oab2f2r96ijj1x/Pending_home_sales.csv?dl=1"
A18<-read.csv(url(URL))
A18$SURPRISE <- A18$Actual - A18$Forecast
sigma18 <- sd(A18$SURPRISE)
A18$SURPRISE_S18 <- A18$SURPRISE/sigma18
A18$t<-A18$HOUR*60+A18$MINUTE
A18<-data.frame(A18$YEAR,A18$MONTH,A18$DAY,A18$t,A18$SURPRISE_S18)
names(A18)[1] <- 'YEAR'
names(A18)[2] <- 'MONTH'
names(A18)[3] <- 'DAY'
names(A18)[4] <- 't'
names(A18)[5] <-'SURPRISE_S18'

X <- merge(X,A18,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/av884s7bfv6bwlk/Personal_consumption.csv?dl=1"
A19<-read.csv(url(URL))
A19$SURPRISE <- A19$Actual - A19$Forecast
sigma19 <- sd(A19$SURPRISE)
A19$SURPRISE_S19 <- A19$SURPRISE/sigma19
A19$t<-A19$HOUR*60+A19$MINUTE
A19<-data.frame(A19$YEAR,A19$MONTH,A19$DAY,A19$t,A19$SURPRISE_S19)
names(A19)[1] <- 'YEAR'
names(A19)[2] <- 'MONTH'
names(A19)[3] <- 'DAY'
names(A19)[4] <- 't'
names(A19)[5] <-'SURPRISE_S19'

X <- merge(X,A19,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/r3kricv3436rv6n/Personal_income.csv?dl=1"
A20<-read.csv(url(URL))
A20$SURPRISE <- A20$Actual - A20$Forecast
sigma20 <- sd(A20$SURPRISE)
A20$SURPRISE_S20 <- A20$SURPRISE/sigma20
A20$t<-A20$HOUR*60+A20$MINUTE
A20<-data.frame(A20$YEAR,A20$MONTH,A20$DAY,A20$t,A20$SURPRISE_S20)
names(A20)[1] <- 'YEAR'
names(A20)[2] <- 'MONTH'
names(A20)[3] <- 'DAY'
names(A20)[4] <- 't'
names(A20)[5] <-'SURPRISE_S20'

X <- merge(X,A20,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/cg3dponhzojde77/Producer_price_index.csv?dl=1"
A21<-read.csv(url(URL))
A21$SURPRISE <- A21$Actual - A21$Forecast
sigma21 <- sd(A21$SURPRISE)
A21$SURPRISE_S21 <- A21$SURPRISE/sigma21
A21$t<-A21$HOUR*60+A21$MINUTE
A21<-data.frame(A21$YEAR,A21$MONTH,A21$DAY,A21$t,A21$SURPRISE_S21)
names(A21)[1] <- 'YEAR'
names(A21)[2] <- 'MONTH'
names(A21)[3] <- 'DAY'
names(A21)[4] <- 't'
names(A21)[5] <-'SURPRISE_S21'

X <- merge(X,A21,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)


URL="https://www.dropbox.com/s/8dgs5qtotikikzl/Trade_balance.csv?dl=1"
A22<-read.csv(url(URL))
A22$SURPRISE <- A22$Actual - A22$Forecast
sigma22 <- sd(A22$SURPRISE)
A22$SURPRISE_S22 <- A22$SURPRISE/sigma22
A22$t<-A22$HOUR*60+A22$MINUTE
A22<-data.frame(A22$YEAR,A22$MONTH,A22$DAY,A22$t,A22$SURPRISE_S22)
names(A22)[1] <- 'YEAR'
names(A22)[2] <- 'MONTH'
names(A22)[3] <- 'DAY'
names(A22)[4] <- 't'
names(A22)[5] <-'SURPRISE_S22'

X <- merge(X,A22,by=c("DAY","MONTH","YEAR","t"),all.x = TRUE)

load("~/Desktop/codeR_CH1/PA_COT.Rda")
load("~/Desktop/codeR_CH1/PA_PC.Rda")


X_COT <- merge(X,PA_COT,by=c("DAY","MONTH","YEAR"),all.x = FALSE)
X_PC <- merge(X,PA_PC,by=c("DAY","MONTH","YEAR"),all.x = FALSE)
save(X_COT,file = "BD_PA_COT.Rda")
save(X_PC,file = "BD_PA_PC.Rda")


