oridat = read.csv("Listings2013.csv")
dat = subset(oridat, dti_wprosper_loan >= 0 & dti_wprosper_loan <= 1)


dat$income_range = as.factor(dat$income_range)

library(MASS)

#There are three steps that I selected the variables for the linear model.

str(dat)

#The first step: pick meaningful variables.
#I went through all the variables¡¯ description and picked those variable 
#which I think will relate to the borrow rate. 

#The second step: Stepwise
#I used the variables I picked in the first step to build the linear regression model, 
#and used backward and forward stepwise to check the AIC for all variables.

fit <- lm(borrower_rate ~ was_delinquent_derog + satisfactory_accounts + income_range + monthly_debt 
          + delinquencies_over30_days + total_inquiries + bankcard_utilization
          + scorex + listing_monthly_payment + real_estate_balance + current_delinquencies, data=dat)


emptymodel = lm(borrower_rate~1, data=dat)
step(object = fit, direction = "backward")
step(object = emptymodel, direction = "forward",scope = borrower_rate ~ principal_balance + income_range + monthly_debt 
     + delinquencies_last7_years + is_homeowner + bankcard_utilization + total_inquiries)


#The third step: Comparing two models by using ANOVA in order to decide 
#which variables will be in my linear model.
#I built another simpler linear model, that simpler model will contain one variable less than 
#the model I built in the step two. Then I used ANOVA function comparing these two models to see 
#whether the p-value is significant or not. If the p-value is small enough, 
#it indicates that the complex model is preferred. 
#I did this process for every variable of the linear model and 
#if a variable did not get a small enough p-value, I will delete from the linear model.




fit1 = lm(borrower_rate ~ satisfactory_accounts + income_range + monthly_debt 
          + delinquencies_over30_days + total_inquiries + bankcard_utilization
          + scorex + listing_monthly_payment + real_estate_balance + current_delinquencies, data=dat)


AIC(fit1,fit)
anova(fit1,fit)




#After the linear model finished, I used four plots to see the regression diagnostics. 




#linearity
par(mfrow=c(2,2))
hist(x = residuals(fit),main = "")
plot(x = fit, which = 2)
plot(x = fit, which = 1)
plot(x = fit, which = 4)

residualPlots(fit)

#homegeniety of variance
library(lmtest)
bptest(fit)
vif(fit)
plot(x = fit, which =3 )

#independent residuals
dwtest(fit)
#outliers
dfb = dfbetas(fit)
head(dfb)

dff = dffits(fit)
head(dff)