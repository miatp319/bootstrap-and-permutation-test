
set.seed(1507)

data2011 <- read.csv("VetSuicides_2011.csv")
data2005 <- read.csv("VetSuicides_2005.csv")

# (a) [0.5 point] Formulate the null- and alternative hypothesis.
#Answer:
#Null hypothesis: The mean suicide rate in 2011 is less than or equal to that in 2005
# Alternative hypothesis: The mean suicide rate in 2011 is large than that in 2005.


#(c) [1 point] Make use of plots to check normality of the data. Do you think the assumption of
#normality, made by these tests, holds? Find and report the sample means. 
#Do you think the difference in mean suicide rates between 2005 and 2011 will be significant?
#Answer:
hist(data2005$vet_rate,50,main="Distribution of suicide rate in 2005")
hist(data2011$vet_rate,50,main="Distribution of suicide rate in 2011")

mean_rate2005 = mean(data2005$vet_rate)
mean_rate2011 = mean(data2011$vet_rate)

#Two samples are likely not normal distribution
difference = data2011$vet_rate - data2005$vet_rate

mean_difference = mean(difference) #5.423036


#  (d) [1 point] Test the hypothesis that the suicide rate in 2011 is larger than the suicide rate in
#2005. Report the results. 
#Answer: Reject null hypothesis
result <- t.test(data2011$vet_rate, data2005$vet_rate, paired = TRUE, alternative = "greater")
result

#(e) [4 points in total] Perform a non-parametric bootstrap procedure on the difference in suicide
#rates between 2011 and 2005 to create the bootstrap sampling distribution. 
#Answer:
n_2011 = length(data2011[,1])
n_2005 = length(data2005[,1])

#Create bootstrap samples
#It is notable that the data is paired, thus we need to resample within each row
#to create bootstrap samples

B <- 10000
out = vector(length = B)
for (i in 1:B)
{
  random_id=sample(50,replace = TRUE) #with replacement, one row can appear multiple times in boostrap samples
  bootsample2005 <- data2005$vet_rate[random_id]
  bootsample2011 <- data2011$vet_rate[random_id]
  boot_diff = bootsample2011 - bootsample2005
  boot_mean_diff = mean(boot_diff)
  out[i] = boot_mean_diff
}

#Calculate bootstrap distribution

#*[1 point] Make a plot of the bootstrap distribution.
#Answer:
hist(out,50,xlab=" ",main="Bootstrap distribution of mean difference")
abline(v=mean_difference,col=2, lwd=1)
abline(v=mean(out),col=3, lwd=1)

#*[1 point] Comment on the bias and shape of the bootstrap distribution.
#Answer:
qqnorm(out)
qqline(out,col=2)
#The bootstrap distribution is nearly normal in shape and unbiased

#*[1 point] Construct the 95% bootstrap t confidence interval and the 95% bootstrap percentile interval.
#Answer:
alpha <- 0.05
tcv <- qt(1-alpha,n_2011-1) #n_2011 is the number of pairs

#t confidence interval
#The t-confidence interval is (3.73,+inf)
lowerbound = mean_difference-(tcv*sd(out))
lowerbound

#Bootstrap percentile interval
#The bootstrap percentile interval is (3.8,+inf)

boot_lower_pi <- quantile(out,alpha)
boot_lower_pi

#*[1 point] Comment on the use of the bootstrap t confidence interval and the bootstrap percentile interval for these data.
#Answer:
#The bootstrap t confidence interval and the bootstrap percentile interval provide close results.
#This can be explained by the assumptions of each type of confidence intervals. The bootstrap t
#confidence interval requires the bootstrap distribution has small bias and is nearly normal,
#which are both satisfied in this situation.

#(f) [2 points in total] Now use a permutation test to test the assumption that -on average- the
#suicide rate in 2011 is larger than in 2005:
#  -[1 point] Implement the permutation test procedure and plot the resulting permutation distribution.
#Answer:
combined_data <- merge(data2005, data2011, by = "state")
paired_data <- combined_data[,c('vet_rate.x','vet_rate.y')]
colnames(paired_data)[colnames(paired_data) == c('vet_rate.x','vet_rate.y')] <- c('vet_rate2005','vet_rate2011')
paired_data

B <- 10000 # Nr of permutation samples
out <- matrix(nrow = B, ncol = 1)

for (i in 1:B) {
  #Create random subset before swapping the elements
  replace_inds <- sample(c(TRUE, FALSE), nrow(paired_data), replace = TRUE)
  #Swap the elements in 2 samples, the key is retained
  paired_data[replace_inds, ] <- paired_data[replace_inds, 2:1]
  #Calculate the mean different and output vector
  perm_diff = paired_data$vet_rate2011 - paired_data$vet_rate2005
  perm_mean_diff = mean(perm_diff)
  out[i] <- perm_mean_diff
}
out
hist(out,100, xlab = "", main = "Permutation distribution of the mean difference (vet_rate2011 - vet_rate2005)")

# -[1 point] Report the permutation test estimate of the P-value.
#Answer:
pval<-(1+sum(out>mean_difference))/(B+1)
pval
  
