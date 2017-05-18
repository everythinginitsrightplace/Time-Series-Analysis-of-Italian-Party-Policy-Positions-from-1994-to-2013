# Let's start from the loading of necessary packages
library(plm)
library(panelAR)
library(dplyr)
library(stats)
library(lmtest)
library(sjPlot)
library(sjmisc)
library(devtools)
Italy<- read.table(file = "Italy_lead_elec_laakso.txt", header = TRUE)
attach(Italy)
# We add here lagged variables
Italy_bad <- mutate(Italy, lag_rile = dplyr::lag(rile), 
                    lag_median = dplyr::lag(median_voter_previous), 
                    lag_meyer = dplyr::lag(nicheness_meyer), 
                    lag_bischof = dplyr::lag(nicheness_bishof), 
                    lag_spec = dplyr::lag(specialization), 
                    lag_golosov_el = dplyr::lag(golosov_el), 
                    lag_laakso_el = dplyr::lag(laakso_el), 
                    lag_golosov_par = dplyr::lag(golosov_par), 
                    lag_laakso_par = dplyr::lag(laakso_par), 
                    lad_GDP= dplyr::lag(GDP), lad_Inflation= dplyr::lag(Inflation),
                    lad_previous_quantity_of_seats_in_parlament= dplyr::lag(previous_quantity_of_seats_in_parlament))
Italy_bad<- Italy %>% group_by(id) %>% mutate(lag.rile = lag(rile, 1))
attach(Italy_bad)

Y <- cbind(rile)
X <- cbind(median_voter_previous, lag.rile, nicheness_meyer, laakso_el_lead, Inflation, previous_quantity_of_seats_in_parlament, GDP)
rdata<- plm.data(Italy_bad, index=c("id","year"))


#Ordinary least squares
ols<-lm(Y ~ X, data=rdata)
summary(ols)
#Graph 
yhat <- ols$fitted
plot(rdata$meyermiller, rdata$good_or_bad_attitude, pch=19, xlab="нишевость", ylab="позиция партии")
abline(lm(rdata$meyermiller~rdata$good_or_bad_attitude),lwd=3, col="red")



#OLS with dummies
fixed.dum <-lm(Y ~ X + niche + country - 1, data=rdata)
summary(fixed.dum)
yhat <- fixed.dum$fitted
library(car)
scatterplot(yhat~rdata$importance_of_issue|rdata$country, boxplots=FALSE, xlab="важность миграционного измерения", ylab="yhat",smooth=FALSE)
abline(lm(rdata$importance_of_issue~rdata$strength_of_right_party),lwd=3, col="red")
scatterplot(Y~Data|country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=rdata)
coplot(Y ~ Data|country, type="l", data=rdata)
coplot(Y ~ Data|country, type="b", data=rdata)

library(gplots)
plotmeans(Y ~ country, main="Heterogeineity across countries", data=rdata)
plotmeans(Y ~ Data, main="Heterogeineity across years", data=rdata)



library(apsrtable)
apsrtable(ols,fixed.dum, model.names = c("OLS", "OLS_DUM")) # Displays a table in Latex 



fixed <- plm(Y ~ X, data=rdata, index=c("id", "year"), model="within")
summary(fixed)
fixed.time <- plm(Y ~ X  + as.factor(year), data=rdata, index=c("id", "year"))
summary(fixed.time)
plmtest(fixed, c("time"), type=("bp"))
#ДFixed or OLS?
#if p-value < 0.05 => Fixed
pFtest(fixed, ols) # Testing for fixed effects, null: OLS better than fixed


#Random effects
random <- plm(Y ~ X, data=rdata, index=c("id", "year"), model="random")
summary(random)

#Fixed or random? If p-value < 0.05 => Fixed
phtest(fixed, random)



pool <- plm(Y ~ X + officeexcl, data=rdata, index=c("id", "year"), model="pooling")
summary(pool)
pooltime <- plm(Y ~ X + as.factor(year), data=rdata, index=c("id", "year"), model="pooling")
summary(pooltime)
poolparty <- plm(Y ~ X + party_name - 1, data=rdata, index=c("id", "year"), model="pooling")
summary(poolparty)

#OLS or Random? If р-value < 0.05=> Random
plmtest(pool, type=c("bp"))
pbgtest(fixed)




#Breusch-Pagan test. If p-value < 0.05=> heteroskedasticity
library(lmtest)
bptest(Y ~ X, data = rdata, studentize=F)
pcdtest(fixed, test = c("cd"))


#If p-value < 0.05, correlation
pbgtest(pool)


#

coeftest(pool, vcov=vcovBK(pool, type="HC1"))
coeftest(pool, vcov = vcovBK(pool, cluster="time",type = "HC3"))
coeftest(pool, vcov = vcovBK(pool, cluster="group",type = "HC3"))


coeftest(poolparty, vcov=vcovBK(poolparty, type="HC1"))
coeftest(pooltime, vcov=vcovBK(pooltime, type="HC1"))
##
## The clustering defaults to "group" for consistency with pvcovHC;
## nevertheless the most likely usage is cluster="time" for robustness vs.
## cross-sectional dependence, as in the original Beck and Katz paper (where
## it is applied to "pooling" models).