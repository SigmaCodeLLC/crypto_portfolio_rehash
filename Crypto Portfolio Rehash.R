library(tidyverse) #to download the data
library(tidyquant) #to download the data
library(plotly) #to create interactive charts
library(timetk) #to manipulate the data series
library(forcats) #to manipulate categorical variables

#Lets download the price data for Bitcoin, Monero, Decentraland
#CRITERIA:
## Lowest four inflation rates that are...
  ### Proof of work (preferred), or proof of stake
  ### No memes tokens, and have purposeful utility
  ### No stablecoins

tick <- c('ADA-USD', 'BTC-USD', 'XMR-USD', 'MANA-USD')

price_data <- tq_get(tick,
                     from = '2009-01-03',
                     to = '2022-12-28',
                     get = 'stock.prices')

View(price_data)

#Next we will calculate the daily returns for these stocks. We will use the logarithmic returns.
log_ret_tidy <- price_data %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'ret',
               type = 'log')

#look at first few rows
head(log_ret_tidy)


#As you can see that this data is in tidy format. 
#We will use the spread() function to convert it to a wide format. 
#And we will also convert it into a time series object using xts() function.
log_ret_xts <- log_ret_tidy %>%
  spread(symbol, value = ret) %>%
  tk_xts()

head(log_ret_xts)

#replace NAs with 0
log_ret_xts[is.na(log_ret_xts)] <- 0


View(log_ret_xts)

#Next lets calculate the mean daily returns for each asset.
mean_ret <- colMeans(log_ret_xts)
print(round(mean_ret, 5))

#Next we will calculate the covariance matrix for all these stocks. 
#We will annualize it by multiplying by 252.
cov_mat <- cov(log_ret_xts) * 252

print(round(cov_mat,4))

#Before we apply our methods to thousands of random portfolio, let us demonstrate the steps on a single portfolio.
#To calculate the portfolio returns and risk (standard deviation) we will us need:
#Mean assets returns
#Portfolio weights
#Covariance matrix of all assets
#Random weights

#Lets create random weights first.
wts <- runif(n = length(tick))
print(wts)
print(sum(wts))

#We created some random weights, but the problem is that their sum is more than 1. 
#We can fix this as shown below.
wts <- wts/sum(wts)
print(wts)

sum(wts)

#Next we will calculate the annualized portfolio returns.
port_returns <- (sum(wts * mean_ret) + 1)^252 - 1

#Next we will calculate the portfolio risk (Standard deviation). 
#This will be annualized Standard deviation for the portfolio. 
#We will use linear algebra to calculate our portfolio risk.
port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))
print(port_risk)

#Next we will assume 0% risk free rate to calculate the Sharpe Ratio.
# Since Risk free rate is 0% 
sharpe_ratio <- port_returns/port_risk
print(sharpe_ratio)

#---------------------------------#
#Lets put all these steps together.

# Calculate the random weights
wts <- runif(n = length(tick))
wts <- wts/sum(wts)

# Calculate the portfolio returns
port_returns <- (sum(wts * mean_ret) + 1)^252 - 1

# Calculate the portfolio risk
port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))

# Calculate the Sharpe Ratio
sharpe_ratio <- port_returns/port_risk

print(wts)

print(port_returns)

print(port_risk)

print(sharpe_ratio)

#---------------------------------#
#We have everything we need to perform our optimization. 
#All we need now is to run this code on 10000 random portfolios. 
#For that we will use a for loop.
#Before we do that, we need to create empty vectors and matrix for storing our values.

num_port <- 10000

# Creating a matrix to store the weights

all_wts <- matrix(nrow = num_port,
                  ncol = length(tick))

# Creating an empty vector to store
# Portfolio returns

port_returns <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Standard deviation

port_risk <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Sharpe Ratio

sharpe_ratio <- vector('numeric', length = num_port)

#Next lets run the for loop 10000 times and set seed for reproducibility..
set.seed(1)

for (i in seq_along(port_returns)) {
  
  wts <- runif(length(tick))
  wts <- wts/sum(wts)
  
  # Storing weight in the matrix
  all_wts[i,] <- wts
  
  # Portfolio returns
  
  port_ret <- sum(wts * mean_ret)
  port_ret <- ((port_ret + 1)^252) - 1
  
  # Storing Portfolio Returns values
  port_returns[i] <- port_ret
  
  
  # Creating and storing portfolio risk
  port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
  port_risk[i] <- port_sd
  
  # Creating and storing Portfolio Sharpe Ratios
  # Assuming 0% Risk free rate
  
  sr <- port_ret/port_sd
  sharpe_ratio[i] <- sr
  
}

#All the heavy lifting has been done 
#and now we can create a data table to store all the values together.

# Storing the values in the table
portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)


# Converting matrix to a tibble and changing column names
all_wts <- tk_tbl(all_wts)

colnames(all_wts) <- colnames(log_ret_xts)

# Combing all the values together
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))

#Lets look at the first few values.
head(portfolio_values)

#We have the weights in each asset with the risk and returns along with the Sharpe ratio of each portfolio.
#Next lets look at the portfolios that matter the most.
#1. The minimum variance portfolio
#2. The tangency portfolio (the portfolio with highest sharpe ratio)
min_var <- portfolio_values[which.min(portfolio_values$Risk),]
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]

#Lets plot the weights of each portfolio. 
#First with the minimum variance portfolio.

p <- min_var %>%
  gather(`ADA-USD`:`XMR-USD`, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)

#Next lets look at the tangency portfolio or the the portfolio with the highest sharpe ratio.
p <- max_sr %>%
  gather(`ADA-USD`:`XMR-USD`, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Tangency Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)

#Finally lets plot all the random portfolios and visualize the efficient frontier.
p <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Annualized Risk',
       y = 'Annualized Returns',
       title = "Portfolio Optimization & Efficient Frontier") +
  geom_point(aes(x = Risk,
                 y = Return), data = min_var, color = 'red') +
  geom_point(aes(x = Risk,
                 y = Return), data = max_sr, color = 'red') +
  annotate('text', x = 0.496, y = 0.342, label = "Tangency Portfolio") +
  annotate('text', x = 0.42, y = 0.234, label = "Minimum variance portfolio") +
  annotate(geom = 'segment', x = 0.00, xend = 0.00,  y = 0.00, 
           yend = 0.00, color = 'red', arrow = arrow(type = "open")) +
  annotate(geom = 'segment', x = 0.00, xend = 0.00,  y = 0.00, 
           yend = 0.00, color = 'red', arrow = arrow(type = "open"))


ggplotly(p)