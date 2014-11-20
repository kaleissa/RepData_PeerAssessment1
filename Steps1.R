##### Quantile-Quantile Plots of steps Data
##### Sttructure is By Eric Cai - The Chemical Statistician
# clear all variables
## rm(list = ls(all.names = TRUE))


steps = (ActivityData$steps)

# sample size of "steps"
length(steps)

# summary of "steps"
summary(steps)

# remove missing values from "steps"
steps = steps[!is.na(steps)]
steps.e=
# having removed missing values, find the number of non-missing values in "ozone"
n = length(steps)

# calculate mean, variance and standard deviation of "ozone"
mean.steps = mean(steps)
var.steps = var(steps)
sd.steps = sd(steps)

## Now, let’s set the n points in the interval (0,1) for the n equi-probable point-wise  probabilities,
## each of which is assigned to the correspondingly ranked quantile.  
## (The smallest probability is assigned to the smallest quantile, and the largest probability is 
## assigned to the largest quantile.)  These probabilities will be used to calculate the 
## quantiles for each hypothesized theoretical distribution.

# set n points in the interval (0,1)
# use the formula k/(n+1), for k = 1,..,n
# this is a vector of the n probabilities
probabilities = (1:n)/(n+1)

## Since “ozone” is a continuous variable, let’s try fitting it to the normal distribution – the most commonly used distribution for continuous variables.  Notice my use of the qnorm() function to calculate the quantiles from the normal distribution.  Specifically, I used the sample mean and the sample standard deviation of “ozone” to specify the parameters of the normal distribution.

# calculate normal quantiles using mean and standard deviation from "ozone"
normal.quantiles = qnorm(probabilities, mean(steps, na.rm = T), sd(steps, na.rm = T))

## Finally, let’s plot the theoretical quantiles on the horizontal axis and the sample 
## quantiles on the vertical axis.  Notice my use of the abline() function to add the 
## identify line.  The two parameters call for a line with an intercept of 0 and a slope of 1.

# normal quantile-quantile plot for "ozone"
png("plot1.png")
plot(sort(normal.quantiles), sort(steps) , xlab = 'Theoretical Quantiles from Normal Distribution', ylab = 'Sample Quqnatiles of steps', main = 'Normal Quantile-Quantile Plot of steps')
abline(0,1)
dev.off()

## Here is the resulting plot:
  
##The plotted points do not fall closely onto the identity line, so the data do not seem 
## to come from the normal distribution.  Is there another distribution that would work better?
##
## Recall from my earlier post on kernel density estimation that the “ozone” data are right-skewed.  A gamma distribution with a small shape parameter tends to be right-skewed, so let’s try this instead.  Notice my use of the sample mean and sample variance to estimate the shape and the scale parameters.

# calculate gamma quantiles using mean and standard deviation from "steps" to calculate shape and scale 
## parameters.
gamma.quantiles = qgamma(probabilities, shape = mean.steps^2/var.steps, scale = var.steps/mean.steps)


# gamma quantile-quantile plot for "steps"
png("Plot2.png")
plot(sort(gamma.quantiles), sort(steps), xlab = 'Theoretical Quantiles from Gamma Distribution', ylab = 'Sample Quantiles of steps', main = 'Gamma Quantile-Quantile Plot of steps')
abline(0,1)
dev.off()

## Here is the resulting plot.
## gamma qq-plot ozone

## Clearly, the data fit the gamma distribution better!
  
##  Of course, this is no guarantee that this particular gamma distribution is the best distribution for this data set.  It does not even guarantee that the gamma distribution is the best family of distributions for this data set.  Nonetheless, it is a useful tool to visualize the goodness-of-fit of a data set to a distribution.

## R has functions for quickly producing Q-Q plots; they are qqnorm(), qqline(), and qqplot().  These are good functions to use.  I built the above Q-Q plots using more rudimentary functions because

## I wanted to use R’s rudimentary functions to illustrate the 5 steps of creating a Q-Q plot
## My method ensures that the sample quantiles and the theoretical quantiles are on the same scale.

