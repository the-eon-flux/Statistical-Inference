library(swirl)
swirl()

# Statistical Inference > Variance

# Variance & Std dev characterize the spread around the mean.

'
Var(X)      = E( (X-mu)^2 ) 
            = E( (X-E(X))^2 ) 
            = E(X^2)-E(X)^2

'
# Practice computing the variance of a dice roll now.

'From the definition of expected values, this means we will take a weighted sum
  over all possible values of X^2. The weight is the probability of X occurring.
'

# X^2
dice_sqr <- c(1,4,9,16,25,36)

# PMF values
fair_dice <- c(rep(0.1666667,6))

low_dice <- c(0.28571429, 0.23809524, 0.19047619, 0.14285714, 0.09523810, 0.04761905)

high_dice <- c(0.04761905, 0.09523810, 0.14285714, 0.19047619, 0.23809524, 0.28571429)


ex2_fair <- sum(dice_sqr * dice_fair)

# We know E(x) for fair dice is 3.5

ex2_fair - (3.5^2)
# This is the variance

# Eg 2 ; dice_high

sum(dice_high*dice_sqr) - edh ^ 2


'
Difference between a population variance sigma^2 and a sample variance s^2. 

They are defined similarly but with a slight difference. 
    The sample variance is defined as the sum of n squared distances from the sample mean divided by (n-1), 
                        where n is the number of samples or observations. 
    We divide by n-1 because this is the number of degrees of freedom in the system. 

The first n-1 samples or observations are independent given the mean. 

The last one is not independent since it can be calculated from the sample mean used in the formula.

'

# Basically {sd of a dist / sqrt(n)}, where n = replicates while taking avg ; estimates the 
# standard error of these means pre-test/ a priori.



#  Chebyshev's inequality helps interpret variances. 

# The probability that a random variable X is at least k standard deviations from its mean is less 
# than 1/(k^2)

#  In other words, the probability that X is at least 2 standard deviations from the mean 
#     is less than 1/4, 3 standard deviations 1/9, 4 standard deviations 1/16, etc.

'
However this estimate is quite conservative for random variables that are normally
| distributed, that is, with bell-curve distributions. In these cases, the probability of
| being at least 2 standard deviations from the mean is about 5% (as compared to
| Chebyshev''s upper bound of 25%) and the probability of being at least 3 standard
| deviations from the mean is roughly .2%.

'
# Statistical Inference > CommonDistros 

# Bernoulli Dist : For expts. with  2 possible outcomes

# PMF of a Bernoulli distribution : p^x * (1-p)^(1-x)

# Binomail random variable represents number of successes, k, out of n independent Bernoulli trials.
# i.,e sum of iid n independent Bernoulli trials.

# Q. Suppose we were going to flip a biased coin 5 times. The probability of tossing a head
# is .8 and a tail .2. What is the probability that you'll toss at least 3 heads.

# More than 3 means sum(p(3), p(4), p(5))
sum( choose(n, c(3,4,5)) * (0.8^c(3,4,5)) * (0.2 ^ (5 - c(3,4,5))) )

# Directly
pbinom(2, 5, 0.8, lower.tail = FALSE)

# normal or Gaussian distribution : Normally distributed variable is denoted as 

 'X ~ N(mu, sigma^2)'

 #  mu = 0 and sigma = 1 the resulting distribution is called the standard normal dist.

 # qnorm gives statistic (t or z) for the percentile alias p value.
 # pnorm gives p value for the given statistic (t or z)

# Poisson distribution : one parameter, lambda
  'The mean and variance of the Poisson distribution are both lambda.'

' Usage : used to model rates such as the rate of hard drive failures.
            Notation : X~Poisson(lambda*t) 
  {lambda is the expected count per unit of time & t is total time }

            '
ppois(3,2.5*4)
  
pbinom(5, 1000, 0.01, lower.tail = TRUE)
ppois(5, 1000*0.01)



