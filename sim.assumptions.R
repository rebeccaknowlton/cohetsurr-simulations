####################################################
###############   Checking assumptions    ##########
####################################################

#C1 we can check just by looking at our data generation for Y: monotonicity of mu(s,w) in s

#C2: when s is independent of w, this is equivalent to checking that P(S > s) in the treated is higher than the control for all s.


####################################################
###############    Setting 1    ####################
####################################################

# set parameters for mean and sd of the surrogate distributions 
params = get.parameters(setting=1)

mean0 <- params$s.mean.control
sd0 <- params$s.sd.control
mean1 <- params$s.mean.treat
sd1 <-  params$s.sd.treat

### Check assumption (C2) ###

# values for s grid
set.seed(1)
s.vals = c(rnorm(1000,mean0,sd0), rnorm(1000,mean1,sd1))
grid.size <- 10000
s.grid <- seq(quantile(s.vals,0.05), quantile(s.vals,0.95), length.out = grid.size)

# calculate cdf and pdf for both treatment and control
cdf0 <- pnorm(s.grid, mean = mean0, sd = sd0)
pdf0 <- dnorm(s.grid, mean = mean0, sd = sd0)
cdf1 <- pnorm(s.grid, mean = mean1, sd = sd1)
pdf1 <- dnorm(s.grid, mean = mean1, sd = sd1)

# to examine visually, plot P(S > s | w) = 1 - CDF
# according to (C2), the treatment group should be >= control group ()
# i.e., red line should be above blue line
plot(s.grid, 1 - cdf0, type = "l", col = "blue", xlab = "s", ylab = "P(S > s)", main = "Checking Assumption (C2)")
lines(s.grid, 1 - cdf1, col = "red")
legend('topright', legend = c("Control", "Treatment"), col = c("blue", "red"), lty = 1)

# how often numerically is treatment >= control, as desired?
sum(1-cdf1 >= 1-cdf0) / grid.size
#1

### Check assumption (C3) ###

# evaluate this assumption on a grid
grid.size <- 100
w1.grid <- seq(params$mean.w1, params$sd.w1, length.out = grid.size)
w2.grid <- seq(params$mean.w2, params$sd.w2, length.out = grid.size)
s.grid <- seq(quantile(s.vals,0.05), quantile(s.vals,0.95), length.out = grid.size)

large.grid <- expand.grid(s.grid, w1.grid, w2.grid)
names(large.grid) <- c("s", "w1", "w2")

large.grid$y1 <- params$beta0 + params$beta1 + params$beta2 * large.grid$s + params$beta3 * large.grid$s + cbind(large.grid$w1,large.grid$w2)%*%params$beta4  +  cbind(large.grid$w1,large.grid$w2)%*%params$beta5
 large.grid$y0 <- params$beta0 + params$beta2 * large.grid$s +  + cbind(large.grid$w1,large.grid$w2)%*%params$beta4 

sum(large.grid$y1 >= large.grid$y0) / (grid.size ^ 3)
# 1, this holds


### Check assumption (C4) ###

set.seed(1)
grid.size <- 10000
s0 <- rnorm(grid.size / 2, mean = mean0, sd = sd0)
s1 <- rnorm(grid.size / 2, mean = mean1, sd = sd1)

# report ranges
range(s0)
range(s1)

# is range of s0 contained in s1?
(range(s0)[1] > range(s1)[1]) & (range(s0)[2] < range(s1)[2])

# visual check -- plot the densities of the surrogate distributions
# range of blue distribution should be contained within range of red distribution
#yes looks true
s.grid <- seq(-8, 15, length.out = grid.size)
plot(s.grid, pdf0, type = "l", col = "blue", xlab = "x", ylab = "pdf", main = "pdf")
lines(s.grid, pdf1, col = "red")
legend('topright', legend = c("Control", "Treatment"), col = c("blue", "red"), lty = 1)
#holds



####################################################
############    Setting 2     ######################
####################################################
params = get.parameters(setting=2)

# params
mean0 <- params$s.mean.control
sd0 <- params$s.sd.control
mean1 <- params$s.mean.treat
sd1 <- params$s.sd.treat

### Check assumption (C2) ###

# values for s grid
grid.size <- 10000
data.temp = gen.data(n=10000, setting =2)

s.grid <- seq(min(data.temp$S), max(data.temp$S), length.out = grid.size)

# calculate cdf and pdf for both treatment and control
cdf0 <- plnorm(s.grid, mean = mean0, sd = sd0)
pdf0 <- dlnorm(s.grid, mean = mean0, sd = sd0)
cdf1 <- plnorm(s.grid, mean = mean1, sd = sd1)
pdf1 <- dlnorm(s.grid, mean = mean1, sd = sd1)

# to examine visually, plot P(S > s | w) = 1 - CDF
# according to (C2), the treatment group should be >= control group ()
# i.e., red line should be above blue line
plot(s.grid, 1 - cdf0, type = "l", col = "blue", xlab = "s", ylab = "P(S > s)", main = "Checking Assumption (C2)")
lines(s.grid, 1 - cdf1, col = "red")
legend('topright', legend = c("Control", "Treatment"), col = c("blue", "red"), lty = 1)

# how often numerically is treatment >= control, as desired?
sum(1-cdf1 >= 1-cdf0) / grid.size
# 0.9158

# for values of s where control > treatment (don't want), how much higher?
idx <- which(1 - cdf1 < 1 - cdf0)
fivenum((1 - cdf0[idx]) / (1 - cdf1[idx]) - 1)
# from the fivenum summary, the values are just barely larger; assumption (C2) very lightly violated

### Check assumption (C3) ###

# evaluate this assumption on a grid
grid.size <- 100
s.grid <- seq(min(data.temp$S), max(data.temp$S), length.out = grid.size)
w1.grid <- seq(0, 6, length.out = grid.size)
w2.grid <- seq(0, 6, length.out = grid.size)

large.grid <- expand.grid(s.grid, w1.grid, w2.grid)
names(large.grid) <- c("s", "w1", "w2")


large.grid$y1 <- params$beta0 + params$beta1 + params$beta2 * large.grid$s^2 + params$beta3 * large.grid$s +
    	cbind(large.grid$w1,large.grid$w2)%*%params$beta4  +
    	cbind(large.grid$w1,large.grid$w2)%*%params$beta5 
large.grid$y0 <- params$beta0 + params$beta2 * large.grid$s^2 +
    	cbind(large.grid$w1,large.grid$w2)%*%params$beta4  

sum(large.grid$y1 >= large.grid$y0) / (grid.size ^ 3)
# met for all combinations on the grid


### Check assumption (C4) ###

set.seed(1)
grid.size <- 10000
s0 <- rlnorm(grid.size / 2, mean = mean0, sd = sd0)
s1 <- rlnorm(grid.size / 2, mean = mean1, sd = sd1)

# report ranges
range(s0)
range(s1)

# is range of s0 contained in s1?
(range(s0)[1] > range(s1)[1]) & (range(s0)[2] < range(s1)[2])

# visual check -- plot the densities of the surrogate distributions
# range of blue distribution should be contained within range of red distribution
s.grid <- seq(3, 12, length.out = grid.size)
plot(s.grid, pdf0, type = "l", col = "blue", xlab = "x", ylab = "pdf", main = "pdf")
lines(s.grid, pdf1, col = "red")
legend('topright', legend = c("Control", "Treatment"), col = c("blue", "red"), lty = 1)


####################################################
###############    Setting 3    ####################
####################################################

# set parameters for mean and sd of the surrogate distributions 
params = get.parameters(setting=3)

### Check assumption (C2) ###
#now S depends on W, so a little more complicated, needs to be conditional on W...

# evaluate this assumption on a grid
grid.size <- 100
w1.grid <- seq(params$mean.w1, params$sd.w1, length.out = grid.size)
w2.grid <- seq(params$mean.w2, params$sd.w2, length.out = grid.size)
large.grid <- expand.grid(w1.grid, w2.grid)

mean0 <- params$s.mean.control
sd0 <- params$s.sd.control
mean1 <- params$s.mean.treat
sd1 <-  params$s.sd.treat
beta5 <-  params$beta5


# values for s grid
set.seed(1)
data.temp = gen.data(n=10000, setting =3)
grid.size <- 1000
s.grid <- seq(quantile(data.temp$S,0.05), quantile(data.temp$S,0.95), length.out = grid.size)

check.eachw = vector(length = dim(large.grid)[1])
for(i in 1:dim(large.grid)[1]) {
	cdf0 <- pnorm(s.grid, mean = mean0, sd = sd0)
	cdf1 <- pnorm(s.grid, mean = as.matrix(large.grid[i,])%*%params$beta5  + mean1, sd = sd1)
	check.eachw[i] = sum(1-cdf1 >= 1-cdf0) / grid.size
}
summary(check.eachw)
#1 , seems to hold for all W

### Check assumption (C3) ###

# evaluate this assumption on a grid, set grid with data
set.seed(1)
data.temp = gen.data(n=10000, setting =3)

large.grid <- as.data.frame(cbind(data.temp$S, data.temp$W1, data.temp$W2))
names(large.grid) <- c("s", "w1", "w2")

large.grid$y1 <- params$beta0 + params$beta1 + params$beta2 * large.grid$s + params$beta3 * large.grid$s + cbind(large.grid$w1,large.grid$w2)%*%params$beta4  +  cbind(large.grid$w1,large.grid$w2)%*%params$beta5
 large.grid$y0 <- params$beta0 + params$beta2 * large.grid$s +  + cbind(large.grid$w1,large.grid$w2)%*%params$beta4 

mean(large.grid$y1 >= large.grid$y0) 
# 0.9863, mostly holds, some space where it does not


### Check assumption (C4) ###

set.seed(1)
data.temp = gen.data(n=10000, setting =3)
s0 <- data.temp$S[data.temp$A==0]
s1 <- data.temp$S[data.temp$A==1]

# report ranges
range(s0)
range(s1)

# is range of s0 contained in s1?
(range(s0)[1] > range(s1)[1]) & (range(s0)[2] < range(s1)[2])
#OK

