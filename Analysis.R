setwd('C:/Users/Lee/iCloudDrive/Document/Boston University/CS544 Foundation with R/Final Project')

library(plotly)

###### read cleaned csv
df = read.csv('AfterClean.csv')
par(mfrow=c(1,1))

# Only use countries that have more than 20 companies
tmp <- as.data.frame(table(df$Country))
tmp <- tmp[tmp$Freq > 20,]
df <- df[df$Country %in% tmp$Var1,]
# Sorts by country, alphabetically
df[order(df$Country),]

############## Returns table of frequency of country occurrence
temp = sort(table(df$Country), decreasing = TRUE)
temp = data.frame(temp)
plot_ly(temp, labels = ~Var1, values = ~Freq, type = 'pie') %>%
  layout(title = 'US, China, Indea, and UK have 70% startups')
plot_ly(temp, x = ~Var1, y = ~Freq, type = 'bar') %>%
  layout(title = 'Companies of these six countries', yaxis = list(title = 'Count'), xaxis = list(title = 'Country'))


################ Look at the percentage of companies in each industry
temp = sort(table(df[,'Industry']), decreasing = TRUE)
temp = data.frame(temp)

plot_ly(temp, x = ~Var1, y = ~Freq, type = 'bar') %>%
  layout(title = 'Companies of these six countries', yaxis = list(title = 'Count'), xaxis = list(title = 'Industry'))



############ Boxplot of valuation. In theory useful, but the outliers make it almost useless
temp = df$Valuation...B.
plot_ly(y = temp, type = 'box', name = 'All six countries') %>% layout(title = 'Valuation of all Companis', yaxis = list(title = 'Billion'))

# Create individual dataframes per country
df.China = subset(df, Country == 'China')
df.France = subset(df, Country == 'France')
df.Germany = subset(df, Country == 'Germany')
df.India = subset(df, Country == 'India')
df.UnitedKingdom = subset(df, Country == 'United Kingdom')
df.UnitedStates = subset(df, Country == 'United States')


par(mfrow=c(6, 1))

boxplot(df.China$Valuation...B., 
        col = "red",
        xlab = "Valuation (Billions)",
        ylab = "China",
        ylim=c(0,10),
        horizontal = TRUE)

boxplot(df.France$Valuation...B., 
        col = "orange",
        xlab = "Valuation (Billions)",
        ylab = "France",
        ylim=c(0,10),
        horizontal = TRUE)

boxplot(df.Germany$Valuation...B., 
        col = "yellow",
        xlab = "Valuation (Billions)",
        ylab = "Germany",
        ylim=c(0,10),
        horizontal = TRUE)

boxplot(df.India$Valuation...B., 
        col = "green",
        xlab = "Valuation (Billions)",
        ylab = "India",
        ylim=c(0,10),
        horizontal = TRUE)

boxplot(df.UnitedKingdom$Valuation...B., 
        col = "cyan",
        xlab = "Valuation (Billions)",
        ylab = "United Kingdom",
        ylim=c(0,10),
        horizontal = TRUE)

boxplot(df.UnitedStates$Valuation...B., 
        col = "violet",
        xlab = "Valuation (Billions)",
        ylab = "United States",
        ylim=c(0,10),
        horizontal = TRUE)


par(mfrow=c(1, 1))



############# Valuation vs Money Raised

# Overall
fit = lm(Total.Raised~Valuation...B., data = df)

plot_ly(data = df, x = ~Valuation...B.) %>%
  add_markers(y = ~Total.Raised) %>%
  add_lines(x = ~Valuation...B., y = fitted(fit)) %>%
  layout(showlegend = F, title = 'Valuation versus Money Raised')


# By Country
df$lmPoint = lm(data = df, Total.Raised ~ Valuation...B. * Country) %>% fitted.values()

plot_ly(data = df, x = ~Valuation...B., y = ~Total.Raised, color = ~Country, type = 'scatter', mode = 'markers') %>%
  add_trace(x = ~Valuation...B., y = ~lmPoint, mode = 'lines')%>%
  layout(title = 'Valuation versus Money Taised by country')


############ Central Limit Theorem for Valuation
mu <- mean(df$Valuation...B.)
sigma <- sd(df$Valuation...B.)
samples <- 1000
par(mfrow = c(2,2))
xbar <- numeric(samples)
messageQ = c()
for (size in c(10, 20, 30, 40)) {
  for (i in 1:samples) {
    xbar[i] <- mean(rnorm(size, mean = mu, sd = sigma))
  }
  
  hist(xbar,
       breaks = 15, xlim=c(0, mu+sigma),
       main = paste("Sample Size =", size))
  
  tmp = paste("Sample Size = ", size, " Mean = ", mean(xbar),
      " SD = ", sd(xbar))
  messageQ = append(messageQ, tmp)
}

for(i in 1:4){
  print(messageQ[i])
}

cat(" Total Data Size = ", length(df$Valuation...B.), " Mean = ", mu, " SD = ", sigma)


########## Sampling
### original data
library(sampling)
sample.size <- 80
pop <- nrow(df)
# plot 
temp = table(df$Country)
temp = sort(temp, decreasing = T)
temp = data.frame(temp)
plot_ly(temp, x = ~Var1, y = ~Freq, type = 'bar') %>%
  layout(title = 'Total Data', yaxis = list(title = 'Count'), xaxis = list(title = 'Country'))
# percentage composition
temp = table(df$Country)
temp = sort(temp, decreasing = T)
temp = prop.table(temp) * 100
totalData = data.frame(temp)
colnames(totalData) = c('Country', 'Percentage')

### SRSWOR
set.seed(6396)
s <- srswor(sample.size, pop)
sample.random <- df[s != 0,]
# plot
freq <- table(df$Country[s != 0])
freq = sort(freq, decreasing = T)
temp = data.frame(freq)
plot_ly(temp, x = ~Var1, y = ~Freq, type = 'bar') %>%
  layout(title = 'SRSWOR with size = 80', yaxis = list(title = 'Count'), xaxis = list(title = 'Country'))
# percentage composition
temp = prop.table(freq) * 100
srsworData = data.frame(temp)
colnames(srsworData) = c('Country', 'Percentage')
srsworData = rbind(srsworData, data.frame('Country' = "France", 'Percentage' = 0))

### Systematic Sampling
k <- ceiling(pop / sample.size)
set.seed(6396)
r <- sample(k, 1)
s <- seq(r, by = k, length = sample.size)
sample.systematic <- df[s,]
# plot
freq <- table(sample.systematic$Country)
freq = sort(freq, decreasing = T)
temp = data.frame(freq)
plot_ly(temp, x = ~Var1, y = ~Freq, type = 'bar') %>%
  layout(title = 'Systematic Sampling with size = 80', yaxis = list(title = 'Count'), xaxis = list(title = 'Country'))
# percentage composition
temp = prop.table(freq) * 100
sysData = data.frame(temp)
colnames(sysData) = c('Country', 'Percentage')

### Stratified sampling by valuationinsert row in dataframe with R
df.sorted <- df[order(df$Country),]
freq <- table(df.sorted$Country)
strata.sizes <- 80 * freq / sum(freq)
set.seed(6396)
strata <- sampling::strata(df.sorted, stratanames = c("Country"),
                           size = strata.sizes, method = "srswor",
                           description = TRUE)
set.seed(6396)
sample.strata <- getdata(df.sorted, strata)
# plot
freq <- table(sample.strata$Country)
freq = sort(freq, decreasing = T)
temp = data.frame(freq)
plot_ly(temp, x = ~Var1, y = ~Freq, type = 'bar') %>%
  layout(title = 'Stratified  Sampling with size = 80', yaxis = list(title = 'Count'), xaxis = list(title = 'Country'))
# percentage combination
temp = prop.table(freq) * 100
strData = data.frame(temp)
colnames(strData) = c('Country', 'Percentage')

### Sampling conclusion
totalData = as.data.frame(t(as.matrix(totalData)))
colnames(totalData) = NULL
rownames(totalData) = NULL

srsworData = as.data.frame(t(as.matrix(srsworData)))
colnames(srsworData) = NULL
rownames(srsworData) = NULL

sysData = as.data.frame(t(as.matrix(sysData)))
colnames(sysData) = NULL
rownames(sysData) = NULL

strData = as.data.frame(t(as.matrix(strData)))
colnames(strData) = NULL
rownames(strData) = NULL


