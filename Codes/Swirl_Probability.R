library(swirl)
install_from_swirl("Statistical Inference")

swirl()

# 1. Statistical Inference > Introduction

'Statistical inference involves formulating conclusions using data AND quantifying the
 uncertainty associated with those conclusions. The uncertainty could arise from 
 incomplete  or bad data.
'

# 2. Statistical Inference > Probability1

# 3. Statistical Inference > Probability2

'
A probability model connects data to a population using assumptions.

A probability mass function (PMF) gives the probability that a discrete random variable is
  exactly equal to some value.
 
A probability density function is associated with a continuous random variable.
  The probability of the random variable falling within a particular range of values 
  is given by ... the area under the density function but above the horizontal
  axis and between the lowest and greatest values of the range."

The cumulative distribution function (CDF) of a random variable X, either discrete or
 continuous, is the function F(x) equal to the probability that X is less than or 
 equal to x.

The survivor function S(x) of a random variable X is defined as the function of x 
 equal to the probability that the random variable X is greater than the value x. 
 This is the complement of the CDF F(x).

The quantile v of a CDF is the point x_v at which the CDF has the value v. 
 More precisely, F(x_v)=v. A percentile is a quantile in which v is expressed as a percentage.

'
# 4. Statistical Inference > ConditionalProbability 

'
Summary : 
            - Two events, A and B, are independent if they have no effect on each 
  other. Formally, P(A&B) = P(A)*P(B). 
  
            - It is easy to see that if A and B are independent, then P(A|B)=P(A).
   
            - Random variables are said to be iid if they are independent and
  identically distributed. By independent we mean "statistically unrelated from one another".
  Identically distributed means that "all have been drawn from the same population
  distribution".

'

# 5. Statistical Inference > Expected Values (Another term for expected value is mean.)

# The expected value of a random variable X, E(X), 
#    is a measure of its central tendency.

# E(X) is defined as a sum, over all possible values x, of the quantity x*p(x).
  # where p(x) is PMF p(x)


# Function ; Return expected value
expect_dice <- function(pmf){ mu <- 0; for (i in 1:6) mu <- mu + i*pmf[i]; mu}
  # takes a PMF as an input. For dice it's 6-long array of fractions.

# PMF values
fair_dice <- c(rep(0.1666667,6))

low_dice <- c(0.28571429, 0.23809524, 0.19047619, 0.14285714, 0.09523810, 0.04761905)

high_dice <- c(0.04761905, 0.09523810, 0.14285714, 0.19047619, 0.23809524, 0.28571429)

edh <- expect_dice(high_dice)
  # 4.333333 ; Expected value is higher than that of the fair dice.

edl <- expect_dice(low_dice)
  # 2.666667 ; Expected value is lower than that of the fair dice.

#  One of the nice properties of the expected value operation is that it's linear. This means
# that, if c is a constant, then E(cX) = c*E(X). Also, if X and Y are two random variables then
# E(X+Y)=E(X)+E(Y). It follows that E(aX+bY)=aE(X)+bE(Y).

'Suppose you were rolling our two loaded dice, dice_high and dice_low. You can use this
| linearity property of expectation to compute the expected value of their average. Let X_hi
| and X_lo represent the respective outcomes of the dice roll. The expected value of the
| average is E((X_hi + X_lo)/2) or .5 *( E(X_hi)+E(X_lo) ). Compute this now. Remember we
| stored the expected values in edh and edl.

'

0.5 *(sum(edh + edl))

# For continuous random variables, E(X) is the area under the function t*f(t),
# where f(t) is the PDF (probability density function) of X.

myfunc <- function(x){x^2/2}
integrate(myfunc, lower = 0, upper= 2)

mean(spop)
allsam #( 2 sampled vals from spop for 10 iterations)

# mean for each row
apply(allsam, 1, mean)

# Difference seen in these vals with mean of spop right.  The expected value
# or mean of the sample mean is the population mean.


# if we take the expected value of these sample means we'll see something amazing. 
mean(apply(allsam, 1, mean))
  # The result is the same as the mean of the original population spop.
  # Expected value/mean of the sample mean is the population mean.

# Takeaway : the sample mean is an unbiased estimator of the population mean.

































