# power
"
Type I error, rejecting the null hypothesis when it's true. We 
structure our hypothesis test so that the probability of this happening is small. 
The other kind of error we could make is to fail to reject when the alternative is true 
(Type II error) False negative
"

'Power = 1 - Type II error
the most frequent use of power is to help us design studies.
'


# Resampling : Ways to perform population based statistical inferences, 
#                       while living within our data. 

# Resampled Inference
 # Construct CI, performing inferences in difficult cases


power.t.test(sd = 0.04, delta = 0.01, power = 0.9, sig.level = 0.1, alternative = "one.sided")

# Q8
power.t.test(sd = 0.04, delta = 0.01, n = 100, alternative = "one.sided")


G1 <- c(140, 138, 150, 148, 135)
G2 <- c(132, 135, 151, 146, 130)
D <- G2- G1

t.test(G2, G1, paired = "TRUE")

t.test(G2, G1, paired = TRUE, alternative = "two.sided" )

pt(3, 3, lower.tail = FALSE)

t.test()


power.t.test(n = 100, delta = 1, alternative = "one.sided", sd = 0.4)


n <- (qnorm(.95) + qnorm(.8)) ^ 2 * .04 ^ 2 / .01^2
n <- (qnorm(.05) + qnorm(.2)) ^ 2 * .04 ^ 2 / .01^2

qnorm(.95) + (1 / 0.4)

