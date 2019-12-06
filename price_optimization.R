
# Price optimization model

# Linear demande curve equation 

#****************************************************
#simulation of random fake dataset : demand function 
#***************************************************


demand = function(p, alpha = -40, beta = 500, sd = 10) {
  
  error = rnorm(length(p), sd = sd)
  q = p*alpha + beta + error
  
  return(q)
}


set.seed(100)

prices = seq(from = 5, to = 10, by = 0.1)
q = demand(prices)


data = data.frame('prices' = prices,'quantity' = q)
head(data)


test=lm(quantity~prices, data = data)
summary(test)

# Correlation test between the variables of the dataset
corrtest = cor(data)


install.packages("psych")
library(psych)
pairs.panels(data)

# check if residuals follow a normal distribution

hist(test$residuals, col = "lightblue")

#**********************************
#Plot the demand curve
#**********************************

ggplot(data, aes(prices, quantity)) +
  geom_point(shape=1) +
  geom_smooth(method='lm') +
  ggtitle('Demand Curve')

mod1 = lm(prices~quantity, data = data)
summary(mod1)
hist(mod1$residuals, col="blue")

####### insert comments about the validity of the model

#***********************************************************************
#setup time series for random data of prices, demand, revenue and profit
#***********************************************************************

set.seed(10)
hist.prices = rnorm(252, mean = 6, sd = .5) # random prices defined by the company
hist.demand = demand(hist.prices) # demand curve defined in the chunck above
hist.revenue = hist.prices*hist.demand # From the revenue equation
unity.cost = 4 # production cost per unity
hist.cost = unity.cost*hist.demand
hist.profit = (hist.prices - unity.cost)*hist.demand # From the price equation

data = data.frame('Period' = seq(1,252),'Daily.Prices' = hist.prices,
                  'Daily.Demand' = hist.demand, 'Daily.Revenue' = hist.revenue,
                  'Daily.Cost' = hist.cost, 'Daily.Profit' = hist.profit)

ggplot(data, aes(Period, Daily.Prices)) +
  geom_line(color = 4) +
  ggtitle('Historical Prices used for exploitation')


ggplot(data, aes(Period, Daily.Revenue, colour = 'Revenue')) +
  geom_line() +
  geom_line(aes(Period, Daily.Profit, colour = 'Profit')) +
  geom_line(aes(Period, Daily.Cost, colour = 'Cost')) +
  labs(title = 'Historical Performance', colour = '')


#Recover the demand curve using a linear model

model_fit=lm(hist.demand~hist.prices) #linear model for demand
summary(model_fit)
hist(model_fit$residuals, col = "green")
pairs.panels(data)

# estimated parameters

beta=model_fit$coefficients[1]
alpha=model_fit$coefficients[2]

p.revenue = -beta/(2*alpha) # estimated price for revenue
p.profit = (alpha*unity.cost - beta)/(2*alpha) # estimated price for profit


#The final plot with the estimated prices:


true.revenue = function(p) p*(-40*p + 500) # Revenue with true parameters (chunck demand)
true.profit = function(p) (p - unity.cost)*(-40*p + 500) # price with true parameters

# estimated curves
estimated.revenue = function(p) p*(model_fit$coefficients[2]*p + model_fit$coefficients[1])
estimated.profit = function(p) (p - unity.cost)*(model_fit$coefficients[2]*p + model_fit$coefficients[1])


opt.revenue = true.revenue(p.revenue) # Revenue with estimated optimum price
opt.profit = true.profit(p.profit) # Profit with estimated optimum price

# plot
df = data.frame(x1 = p.revenue, x2 = p.profit,
                y1 = opt.revenue, y2 = opt.profit, y3 = 0)

ggplot(data = data.frame(Price = 0)) +
  stat_function(fun = true.revenue, mapping = aes(x = Price, color = 'True Revenue')) +
  stat_function(fun = true.profit, mapping = aes(x = Price, color = 'True Profit')) +
  stat_function(fun = estimated.revenue, mapping = aes(x = Price, color = 'Estimated Revenue')) +
  stat_function(fun = estimated.profit, mapping = aes(x = Price, color = 'Estimated Profit')) +
  scale_x_continuous(limits = c(4, 11)) +
  labs(title = 'True curves without noise') +
  ylab('Results') +
  scale_color_manual(name = "", values = c("True Revenue" = 2, "True Profit" = 3, "Estimated Revenue" = 4, "Estimated Profit" = 6)) +
  geom_segment(aes(x = x1, y = y1, xend = x1, yend = y3), data = df) +
  geom_segment(aes(x = x2, y = y2, xend = x2, yend = y3), data = df)


#Final observations
#As you can see, the estimated Revenue and estimated Profit curves are quite similar to the true ones without noise and the expected revenue for our estimated optimal policies looks very promising. Although the linear and monopolist assumption looks quite restrictive, this might not be the case, check Besbes and Zeevi (2015) and Cooper et al (2015).

#If one expect a large variance for \alpha, it might be useful to simulate \alpha, \beta and then the optimal price using Jensen’s inequality.

#References

#Phillips, Robert Lewis. Pricing and revenue optimization. Stanford University Press, 2005.

#Besbes, Omar, and Assaf Zeevi. “On the (surprising) sufficiency of linear models for dynamic pricing with demand learning.” Management Science 61.4 (2015): 723-739.

#Cooper, William L., Tito Homem-de-Mello, and Anton J. Kleywegt. “Learning and pricing with models that do not explicitly incorporate competition.” Operations research 63.1 (2015): 86-103.

#Talluri, Kalyan T., and Garrett J. Van Ryzin. The theory and practice of revenue management. Vol. 68. Springer Science & Business Media, 2006.



