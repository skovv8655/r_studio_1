cost <- read.table('Cost_Of_Living_Index.txt', sep = '\t', header = TRUE)

#1 Produce a scatterplot between the Cost of Living Index and EACH of the other index variables. As a result, there should be 4 scatterplots in total. Examine the relationship shown in each scatterplot in terms of its form, strength and directiion 
plot(Cost.of.Living.Index ~ Rent.Index, data = cost) 
#moderate positive linear relationship
plot(Cost.of.Living.Index ~ Groceries.Index, data = cost)
#Strong positive linear relationship
plot(Cost.of.Living.Index ~ Restaurant.Price.Index, data = cost) 
#Strong Positive linear relationship
plot(Cost.of.Living.Index ~ Local.Purchasing.Power.Index, data = cost) 
#Weak linear relationship



#2 Compute the correlation coefficients for all the scatterplots obtained above
cor(cost$Cost.of.Living.Index,cost$Rent.Index) #correlation of the COL index and rent
cor(cost$Cost.of.Living.Index,cost$Groceries.Index) #correlation of the COL index and groceries
cor(cost$Cost.of.Living.Index,cost$Restaurant.Price.Index)  #correlation of the COL index and restaurants
cor(cost$Cost.of.Living.Index,cost$Local.Purchasing.Power.Index)  #correlation of the COL index and LPP 

#4 Fit a linear regression model between the Cost of Living Index and each of the other index variables. As a result, there should be 4 regression models in total. Interpret the resulting estimated slope in each model.

mod1 <- lm(Cost.of.Living.Index ~ Rent.Index, data = cost)
coef (mod1)

mod2 <- lm(Cost.of.Living.Index ~ Groceries.Index, data = cost)
coef (mod2)

mod3 <- lm(Cost.of.Living.Index ~ Restaurant.Price.Index, data = cost)
coef (mod3)

mod4 <- lm(Cost.of.Living.Index ~ Local.Purchasing.Power.Index, data = cost)
coef (mod4)

#5 Based on the correlation coefficients and the regression models obtained above, which item would be the best predictor of overall cost in these cities? Which would be the worst? Explain.

summary(mod1)$r.squared
summary(mod2)$r.squared
summary(mod3)$r.squared
summary(mod4)$r.squared

#The better predictor of overall cost in these cities is the Groceries Index because its model (Model 2 has a high r (which tells us the correlation) as well as a high R-squared (0.909852), where as the worse model to judge the overall cost in these cities is the Local Purchasing Power Index because of its low r and R-Squared Value (0.2765729)

#6 Find the cost of living as predicted by Groceries Index and its residual for Beijing, China. (Hint: Find row index of Beijing in the dataset, and then use that index to extract the corresponding fitted value and residual from the regression result.)

#corresponding fitted value

predval = which(cost$City == 'Beijing, China') 
mod2$fitted.values[predval]

#residual from the regression result
mod2$residuals[predval]

