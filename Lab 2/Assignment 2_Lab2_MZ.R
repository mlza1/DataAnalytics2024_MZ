#This is provided code for Lab 2, the Exercise 1 for Lab 2 is in this section 
library(ggplot2)
### read in data
epi.results <- read.csv("~/Desktop/EPI.new.csv")
View(epi.results)
epi.weights <- read.csv("~/Desktop/epi.weights.csv")
View(epi.weights)
#### Exploratory Analysis with new variables####
epi.results$TCG.new
epi.results[1,5]
attach(epi.results)

# I will be using TCG.new, TCG.old and FSH.new
TCG.new
TCG.new[1]
## NA values
na.indices <- is.na(TCG.new) 
## drop NAs
TCG.new.compl <- TCG.new[!na.indices]
## convert to data frame and add country
TCG.new.compl <- data.frame(Country = country[!na.indices], TCG = TCG.new[!na.indices])
## summary stats
summary(TCG.new)
fivenum(epi.results$TCG.new,na.rm=TRUE)
## histograms
hist(epi.results$TCG.new)
hist(epi.results$TCG.new, seq(20., 80., 1.0), prob=TRUE)
rug(epi.results$TCG.new)
lines(density(epi.results$TCG.new,na.rm=TRUE,bw=1))
lines(density(epi.results$TCG.new,na.rm=TRUE,bw="SJ"))
x <- seq(20., 80., 1.0)
qn<- dnorm(x,mean=42, sd=5,log=FALSE)
lines(x,0.4*qn)
qn<- dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,0.12*qn)
##################
### Comparing distributions of 3 variables
boxplot(TCG.old, epi.results$TCG.new, FSH.new, names=c("TCG.old","TCG.new", "FSH.new"))

### Quantile-quantile plots
qqnorm(epi.results$TCG.new)
qqline(epi.results$TCG.new)
x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)
qqplot(qnorm(ppoints(200)),epi.results$TCG.new)
qqline(epi.results$TCG.new)
qqplot(rnorm(1000),epi.results$TCG.old)
qqline(epi.results$TCG.old)
d1 <- rnorm(10000)
d2 <- rnorm(10000)
qqplot(d1,d1)
qqline(d1)

#This is the 3 category QQ-PLOT#
par(mfrow = c(1, 3))
qqnorm(TCG.new, main = "Q-Q Plot of TCG.new")
qqline(TCG.new)
qqnorm(TCG.old, main = "Q-Q Plot of TCG.old")
qqline(TCG.old)
qqnorm(FSH.new, main = "Q-Q Plot of FSH.new")
qqline(FSH.new)

### Empirical Cumulative Distribution Function
plot(ecdf(epi.results$TCG.new), do.points=FALSE) 
plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE, main="Norm Dist vs. TCG.new ECDF")
lines(ecdf(epi.results$TCG.new))
plot(ecdf(TCG.old), do.points=FALSE, main="TCG.old vs. TCG.new vs. FSH.new vs. BER.old ECDF")
lines(ecdf(epi.results$TCG.old))
lines(ecdf(epi.results$FSH.new))
lines(ecdf(epi.results$BER.old))





####This is EXERCISE 2 Populations Dataset with TCG old and new values ####
## read data
populations_2023 <- read.csv("~/Desktop/populations_2023.csv")
View(populations_2023)
## drop country populations that don't exist in epi results
populations <- populations_2023[-which(!populations_2023$Country %in% epi.results$country),]
## sort populations by country name
populations <- populations[order(populations$Country),]
## drop country results that don't exist in populations
epi.results.sub <- epi.results[-which(!epi.results$country %in% populations$Country),]
## sort results by country name
epi.results.sub <- epi.results.sub[order(epi.results.sub$country),]
## only keep relevant columns
epi.results.sub <- epi.results.sub[,c("country","TCG.old","TCG.new")]
## convert to numeric
epi.results.sub$population <- as.numeric(populations$Population)
## compute population log
epi.results.sub$population_log <- log10(epi.results.sub$population)
boxplot(epi.results.sub$population_log)
attach(epi.results.sub)
## created linear model of EPI.new = a(population_log) + b
lin.mod.epinew <- lm(TCG.new~population_log,epi.results.sub)
plot(TCG.new~population_log,epi.results.sub)
abline(lin.mod.epinew)
summary(lin.mod.epinew)
plot(lin.mod.epinew)
ggplot(epi.results.sub, aes(x = population_log, y = TCG.new)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(lin.mod.epinew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')
## another lm
lin.mod.pop <- lm(population_log~TCG.new,epi.results.sub)
plot(population_log~TCG.old)
abline(lin.mod.pop)
summary(lin.mod.pop)
plot(lin.mod.pop)
ggplot(epi.results.sub, aes(x = TCG.old, y = population_log)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(lin.mod.pop, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')
## another lm
lin.mod.pop <- lm(population_log~TCG.new,epi.results.sub)
plot(population_log~FSH.new)
abline(lin.mod.pop)
summary(lin.mod.pop)
plot(lin.mod.pop)
ggplot(epi.results.sub, aes(x = TCG.old, y = population_log)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(lin.mod.pop, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')



#this is the summary stat section#
summary(TCG.new)
summary (TCG.old)
summary(FSH.new)

