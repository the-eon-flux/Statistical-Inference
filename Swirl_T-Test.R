library(swirl)
swirl()

## Statistical Inference > T Confidence Intervals

# Student's or Gosset's t distribution and t confidence intervals.

# Generic Formula For Confidence Intervals

' Est +/- qnorm *std error(Est)'

# Z- statistic :  Z=(X'-mu)/(sigma/sqrt(n))

# T- statistic :  t=(X'-mu)/(s/sqrt(n))
            # ONly diff : std. deviation of population is replaced by sample's.

'So the distribution of the t statistic is independent of the population mean and variance. 
  Instead it depends on the sample size n.
  
  T distribution distribution has only one param > (the number of samples - 1 ) df

      CI > X'' +/- t_(n-1)*s/sqrt(n);  where t_(n-1) is the relevant quantile at (n-1) df

'

# Sleep Data : increase in sleep hours for 10 patients on two soporific drugs.

data(sleep)

range(g1)   # -1.6  3.7
range(g2)   # -0.1  5.5

difference <- g2 -g1
mean(difference)

s <- sd(difference)

# Getting CI of difference
mn + c(-1,1)*qt(0.975, 9) * s/ sqrt(10)

# Using t.test for the same
t.test(difference)$conf.int

# T.test fn call can be made in 4 ways

rbind(
            mn + c(-1, 1) * qt(.975, n-1) * s / sqrt(n),
            as.vector(t.test(difference)$conf.int),
            as.vector(t.test(g2, g1, paired = TRUE)$conf.int),
            as.vector(t.test(extra ~ I(relevel(group, 2)), paired = TRUE, data = sleep)$conf.int)
)

# T CI for independent groups : 

# compare the mean blood pressure between two groups in a randomized trial

"
| The first is a group of 8 oral contraceptive users and the second is a group of 21
| controls. The two means are X''_{oc}=132.86 and X'_{c}=127.44 and the two sample
| standard deviations are s_{oc}= 15.34 and s_{c}= 18.23. Lets first compute the
| numerator of the pooled sample variance by weighting the sum of the two by their
| respective sample sizes. Recall the formula (n_x-1)(S_x)^2+(n_y-1)(S_y)^2 and fill in
| the values to create a variable sp

"

# Sp
sp <- (8-1)*(15.34)^2+(21-1)*(18.23)^2

# Df ? (F ~ (n_x-1)+(n_y-1))
ns <- 7 + 20

# Std. Error
sp <- sqrt(sp/ns)

# find the 95% confidence interval

# Getting CI using formula
(132.86 - 127.44) + c(-1,1)*qt(0.975, ns) * sp * sqrt((1/8 + 1/21))
            # -9.521097 20.361097

# Since CI has 0 in it. It means we can't say their means are diff. i.e, No significant diff. seen


# Sleep data eg.2 ; Treating them as independent
sp <- sqrt(sum(var(g1)*9, var(g2)*9)/18)
md + c(-1,1) * qt(0.975, 18) * sp * sqrt((1/10 + 1/10))

# T.test

t.test(g2, g1, paired = FALSE, var.equal = TRUE)$conf
t.test(g2, g1, paired = TRUE)$conf

# Unpaired unequal variance

"
Let's plug in the numbers from the blood pressure study to see how this works. Recall
| we have two groups, the first with size 8 and X'_{oc}=132.86 and s_{oc}=15.34 and the
| second with size 21 and X'_{c}=127.44 and s_{c}=18.23.

"
# df

num <- ((15.34^2/8) + (18.23^2 / 21) )^2

den <- (((15.34^4/8^2)/7) + ((18.23^4 / 21^2)/20))

mydf <- num / den

# CI
132.86 - 127.44 + c(-1,1) * qt(0.975, mydf) * sqrt(15.34^2 / 8 + 18.23^2/21)

## Statistical Inference > Hypothesis testing

# Father.Son data from UsingR pckg

dim(fs)
            #1078    2

# t.test

t.test(fs$fheight, fs$sheight, paired = TRUE)

t.test(fs$fheight -fs$sheight, paired = TRUE)

# t stat = 11.7885

11.7885 * sd(fs$sheight-fs$fheight)/sqrt(1078)


"
 If you set alpha to some value (p cutoff = .05) and ran many tests checking alternative
  hypotheses against H_0, that mu=mu_0, the set of all possible values for which you fail
  to reject H_0 forms the (1-alpha)% (that is 95%) confidence interval for mu_0.
"

mybin

## P Values 

"
The p-value is the probability under the null hypothesis of obtaining evidence as or
more extreme than your test statistic (obtained from your observed data) in the
direction of the alternative hypothesis.
"

# Eg. get a t statistic of 2.5 with 15 df testing H_0, (that mu = mu_0)
#  versus an alternative H_a (that mu > mu_0). find the probability of getting a t statistic as 
#  large as 2.5

pt(2, 15, lower.tail = FALSE)

pnorm(2, lower.tail = FALSE)

pbinom(6, 8, 0.5,lower.tail = FALSE)

pbinom(7, 8, 0.5,lower.tail = TRUE)

ppois(9, 5, lower.tail = FALSE)




############################ Quiz 3 ##############################

# Q1
1100 + c(-1,1)*qt(0.975, 8) * 30/ sqrt(9)

# Q2
qt(0.95, 8)

mn = -2
n = 9

mn +qt(.975, n-1) / sqrt(n) *  2.601904

s = 2 / qt(.975, n-1) / sqrt(n)
s # Answer

# Q3 Paired

# Q4 
sp <- sqrt((9 * 0.6 + 9 * 0.68) / (20 - 2))

3 - 5 + c(-1,1) * qt(0.975, 18) * sp * sqrt(1 /10 + 1/10)
            #  -2.751649 -1.248351


# Q5 Narrow Interval

# Q6 

# CI
6 - 4 + c(-1,1) * qt(0.975, 198) * sqrt(0.5^2 / 100 + 2^2/100)

# Q7

md = -3-1
n2 = n1 = 9
s1 = 1.5
s2 = 1.8
sp <- sqrt( ((n1 - 1) * s1^2 + (n2-1) * s2^2) / (n1 + n2-2))
semd = sp * sqrt(1 / n1 + 1/n2)
   
# Ans
md + c(-1, 1) * qt(.95, n1 + n2 - 2) * semd
