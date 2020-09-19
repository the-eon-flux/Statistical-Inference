#Week 4 quiz

# Q1. Give P-value for the associated two sided T test.
            
            bl <- c(140, 138, 150, 148, 135)
            fu <- c(132, 135, 151, 146, 130)
            t.test(fu, bl, alternative = "two.sided", paired = TRUE)

# Q2. A sample of 9 men yielded a sample average brain volume of 1,100cc 
# and a standard deviation of 30cc. What is the complete set of values of μ0 that a 
# test of H_0: μ=μ0 would fail to reject the null hypothesis in a two sided 5% Students t-test?

            mx <- 1100
            n <- 9
            sdx <- 30
            
            mx + c(-1,1) * qt(0.975, n-1) * sdx / sqrt(n)
            # 1076.94 1123.06

# Q3. Researchers conducted a blind taste test of Coke versus Pepsi. Each of four people 
#     was asked which of two blinded drinks given in random order that they preferred. 
#     The data was such that 3 of the 4 people chose Coke. Assuming that this sample 
#     is representative, report a P-value for a test of the hypothesis that Coke is 
#     preferred to Pepsi using a one sided exact test.

            pbinom(2,4, prob = 0.5, lower.tail = FALSE)
            # 0.3125


# Q4.) Infection rates at a hospital above 1 infection per 100 person days at risk are 
#      believed to be too high and are used as a benchmark. A hospital that had previously 
#      been above the benchmark recently had 10 infections over the last 1,787 person days 
#      at risk. About what is the one sided P-value for the relevant test of whether the 
#      hospital is *below* the standard?

            Cutoff <- 1/100
            X <- 10
            Lamda <- Cutoff * 1787
            ppois(10, lambda = Lamda)
            # 0.03237153

# Q5.)

            n1 <- n2 <- 9
            m1 <- -1
            m2 <- 1
            s1 <- 1.5
            s2 <- 1.8
            
            df = (n1+n2 - 2)
            
            Sp_sq <- ( ( (n1 - 1) * s1^2 ) + ( (n2 -1)* s2^2 ) ) / df
            
            t_stat <- (m2 -m1) / sqrt(Sp_sq*(1/n1 + 1/n2))
            pt(t_stat, df, lower.tail = FALSE)
            # 0.01047169 ; 0.05 > 0.0104 > 0.01 
            # Ans : < 0.01

# Q6.) Brain volumes for 9 men yielded a 90% confidence interval of 1,077 cc to 1,123 cc. 
#      Would you reject in a two sided 5% hypothesis test of H0:μ=1078 ?
            # No
            
# Q7.) Researchers would like to conduct a study of 100 healthy adults to detect a four year mean 
# brain volume loss of .01 mm^3 Assume that the standard deviation of four year 
# volume loss in this population is .04~mm^3.04 mm3. About what would be the power of the 
# study for a 5% one sided test versus a null hypothesis of no volume loss?

n = 100; muA <- 0.01; s = 0.04
Z <- qnorm(0.95, sd = s/sqrt(n))
T <- qt(0.95, n-1)

pnorm(muA + qnorm(0.95)*(s), sd = 0.04, lower.tail = FALSE)

pnorm(muA + (Z*(s/sqrt(n))/n-1), lower.tail = FALSE)
# 0.837
pt(muA + (T*(s/sqrt(n))/n-1), n-1, lower.tail = FALSE)
# 0.83769




# Q8. Sample size ?
            Zpow <- qnorm(0.1)
            Zalpha <- qnorm(0.05)
            s = 0.04
            
            n <- ((Zpow + Zalpha )^2 * s^2) / 0.01^2
            
            # Verify
            pnorm(0.01 + qnorm(0.95)*(s/sqrt(138)), sd = 0.0034, lower.tail = FALSE)
            
            # 137 ; ~140

# Q9. Larger Power

