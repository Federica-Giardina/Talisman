# Sample siz
#Symptoms
delta <- 0.68     # this is the estimated difference between 
#treatment and placebo after adjusting 
sd_effect <- 0.18*sqrt(20)*sqrt(2)  # estimated standard deviation 
#of the difference, calculated by the se * sqrt(n) *sqrt(2) because there were 2
# measurements each



sample_size_result <- power.t.test(delta = delta, sd = sd_effect,
                                   sig.level = 0.05, power = 0.8,
                                   type = "two.sample", alternative = "two.sided")
print(sample_size_result)

# n = 44.97801 in each group

#Medurge

delta <- 0.40    # this is the estimated difference between 
#treatment and placebo after adjusting 
sd_effect <- 0.20*sqrt(20)*sqrt(2)  # estimated standard deviation 
#of the difference, calculated by the se * sqrt(n) *sqrt(2) because there were 2
# measurements each



sample_size_result <- power.t.test(delta = delta, sd = sd_effect,
                                   sig.level = 0.05, power = 0.8,
                                   type = "two.sample", alternative = "two.sided")
print(sample_size_result)

#n = 157.9437 # in each group