#----------------------------------------------------------------------------#
####################### Probability Modelling Of Data ########################
#----------------------------------------------------------------------------#
################ Assignment 1: Vignesh Muthumani (10385771) ##################
#----------------------------------------------------------------------------#

# Q1.(c) --------------------------------------------------------------------#

# Generating 100 samples for each agent using the proposed model in Q1(a)
s <- 100
lambda <- 10
p <- 0.8
n <- 10

# For agent 1
agent1 <- rpois(s,lambda)
agent1

# For agent 2
agent2 <- rexp(s,lambda)
agent2

# For agent 3
agent3 <- rbinom(s,n,p)
agent3

# Q1.(d) --------------------------------------------------------------------#

# Summarizing the datasets generated
summary(agent1)
summary(agent2)
summary(agent3)

# Visualizing the datasets generated
# For agent 1
barplot(agent1, main = 'Agent 1', ylab = '# of customers in an hour',
        col = 'gray', border = 'gray50')

# For agent 2
plot(agent2, main = 'Agent 2', xlab = '# of customers', ylab = 'frequency',
     type = 'l', col = 'blue')

# For agent 3
hist(agent3, main = 'Agent 3', ylab = 'customers who purchased',
     col = 'gray', border = 'gray50', ylim = c(0,30))

# Q2.(b) --------------------------------------------------------------------#

# Sensor-1
alpha=1
beta=2
sensor1 <- rbeta(200,alpha,beta)
mean(sensor1)
var(sensor1)

# Sensor-2
alpha=2
beta=3
sensor2 <- rbeta(200,alpha,beta)
mean(sensor2)
var(sensor2)

# Sensor-3
alpha=3
beta=4
sensor3 <- rbeta(200,alpha,beta)
mean(sensor3)
var(sensor3)

# Sensor-4
alpha=4
beta=5
sensor4 <- rbeta(200,alpha,beta)
mean(sensor4)
var(sensor4)

# Sensor-5
alpha=5
beta=6
sensor5 <- rbeta(200,alpha,beta)
mean(sensor5)
var(sensor5)

# Q2.(c) --------------------------------------------------------------------#

# From the results, we can see that the sensor 5 has got the least variance of
# all. So the spread measure is low. When the variance is less, the accuracy
# will be high. Hence, we select the sensor 5 as the preferred sensor.

# Q2.(d) --------------------------------------------------------------------#

alpha <- 2
beta <- 4
par(mfrow = c(1,2))

# Sketching PDF 
x1 <- seq(from=0, to=5, by=0.05)
y1 <-dbeta(x1, alpha, beta)
plot(x1, y1, type = 'l', lwd = 2, main = 'PDF curve', col = 'blue')

# Sketching CDF 
x2 <- seq(from=0, to=5, by=0.05)
y2 <- pbeta(x2, alpha, beta)
plot(x2, y2, type="l", lwd=2, main = 'CDF curve', col='red')

# Q3. -----------------------------------------------------------------------#

# verifying the result of the probability of an individual scoring above 510
mu <- 500
sigma <- 100
p510 <- pnorm(510,mu,sigma,lower.tail=FALSE)
p510

# The result is same as the one that was mathematically derived.

# Q3.(a) --------------------------------------------------------------------#

# Sketching 4 normal PDFs with different means and same variance in one plot

sd <- 3
x <- -10:11
y1 <- dnorm(x,mean=1,sd)
y2 <- dnorm(x,mean=2,sd)
y3 <- dnorm(x,mean=3,sd)
y4 <- dnorm(x,mean=4,sd)

par(mfrow = c(1,1))
plot(x, y1, type='l', lwd=2, col='red', ylim = c(0,0.15), ylab = 'y',
     main = '4 normal PDFs of different means with same variance')
lines(x, y2, type='l', lwd=2, col='green')
lines(x, y3, type='l', lwd=2, col='blue')
lines(x, y4, type='l', lwd=2, col='brown')
legend(-10,0.145, legend=c('Mean=1','Mean=2','Mean=3','Mean=4'),
       lty = c(1,1), lwd=2, col = c('red','green','blue','brown'))

# Q3.(b) --------------------------------------------------------------------#

mu <- 500
sigma <- 100
p510 <- pnorm(510,mu,sigma)
p410 <- pnorm(410,mu,sigma)

p_410_510 <- p510-p410
p_410_510

# Q4. -----------------------------------------------------------------------#

s = 100
n = 10
p1 = p2 = p3 = p4 = 0.25

data <- rmultinom(s, n, p = c(p1,p2,p3,p4))
data

x1 <- data[1,]
x2 <- data[2,]

correlation <- cor(x1,x2)
correlation

# Optionally,
# Correlation(x1,x2) can also be calculated by, "covariance(x1,x2)/sd(x1).sd(x2)"

covariance <- cov(x1,x2)
covariance

sd1 <- sd(x1)
sd1

sd2 <- sd(x2)
sd2

correlation <- covariance / (sd1 * sd2)
correlation

# [Optional method]:

s = 100
n = 10
p1 = p2 = p3 = p4 = 0.25

x1 <- rbinom(s,n,p1)
x2 <- rbinom(s,n,p2)

correlation <- cor(x1,x2)
correlation

##############################################################################

