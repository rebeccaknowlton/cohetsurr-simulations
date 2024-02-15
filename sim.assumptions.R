####################################################
###############    Setting 1    ####################
####################################################


# set parameters for mean and sd of the surrogate distributions 
mean0 <- 2
sd0 <- 2
mean1 <- 4
sd1 <- 3

### Check assumption (C2) ###

# values for s grid
grid.size <- 10000
s.grid <- seq(-8, 15, length.out = grid.size)

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
# 0.74

# for values of s where control > treatment (don't want), how much higher?
idx <- which(1 - cdf1 < 1 - cdf0)
fivenum((1 - cdf0[idx]) / (1 - cdf1[idx]) - 1)
# from the fivenum summary, the values are just barely larger; assumption (C2) very lightly violated

### Check assumption (C3) ###

# evaluate this assumption on a grid
grid.size <- 100
s.grid <- seq(-8, 15, length.out = grid.size)
w1.grid <- seq(2, 6, length.out = grid.size)
w2.grid <- seq(3, 13, length.out = grid.size)

large.grid <- expand.grid(s.grid, w1.grid, w2.grid)
names(large.grid) <- c("s", "w1", "w2")

large.grid$y1 <- 2.1 + 10*large.grid$s + 0.69*large.grid$w1 + 0.535*large.grid$w2
large.grid$y0 <- 2 + 7*large.grid$s + 0.5*large.grid$w1 + 0.25*large.grid$w2

sum(large.grid$y1 >= large.grid$y0) / (grid.size ^ 3)
# only met for 0.70 combinations on the grid :(

# investigate where on the grid this is happening
fivenum(large.grid[which(large.grid$y1 < large.grid$y0),]$s) # only happening with negative s
fivenum(large.grid[which(large.grid$y1 < large.grid$y0),]$w1) # similar to w1 distribution
fivenum(large.grid[which(large.grid$y1 < large.grid$y0),]$w2) # similar to w2 distribution

# what if I generate s values based on the actual distributions?
set.seed(1)
s0 <- rnorm(grid.size / 2, mean = mean0, sd = sd0)
s1 <- rnorm(grid.size / 2, mean = mean1, sd = sd1)
s.all <- append(s0, s1)

large.grid <- expand.grid(s.all, w1.grid, w2.grid)
names(large.grid) <- c("s", "w1", "w2")

large.grid$y1 <- 2.1 + 10*large.grid$s + 0.69*large.grid$w1 + 0.535*large.grid$w2
large.grid$y0 <- 2 + 7*large.grid$s + 0.5*large.grid$w1 + 0.25*large.grid$w2

sum(large.grid$y1 >= large.grid$y0) / (grid.size ^ 3)
# this is better, met 0.96 of the time

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
s.grid <- seq(-8, 15, length.out = grid.size)
plot(s.grid, pdf0, type = "l", col = "blue", xlab = "x", ylab = "pdf", main = "pdf")
lines(s.grid, pdf1, col = "red")
legend('topright', legend = c("Control", "Treatment"), col = c("blue", "red"), lty = 1)

####################################################
###############    Setting 2    ####################
####################################################

# set parameters for mean and sd of the surrogate distributions (my params I updated to meet C2 better)
mean0 <- 2
sd0 <- 2
mean1 <- 4
sd1 <- 3

### Check assumption (C2) ###

# values for s grid
grid.size <- 10000
s.grid <- seq(-8, 15, length.out = grid.size)

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
# 0.74

# for values of s where control > treatment (don't want), how much higher?
idx <- which(1 - cdf1 < 1 - cdf0)
fivenum((1 - cdf0[idx]) / (1 - cdf1[idx]) - 1)
# from the fivenum summary, the values are just barely larger; assumption (C2) very lightly violated

### Check assumption (C3) ###

# evaluate this assumption on a grid
grid.size <- 100
s.grid <- seq(-8, 15, length.out = grid.size)
w1.grid <- seq(2, 6, length.out = grid.size)
w2.grid <- seq(3, 13, length.out = grid.size)

large.grid <- expand.grid(s.grid, w1.grid, w2.grid)
names(large.grid) <- c("s", "w1", "w2")

large.grid$y1 <- 2.1 + 7.12*large.grid$s + 0.5*large.grid$w1 + 0.25*large.grid$w2 
large.grid$y0 <- 2 + 4.12*large.grid$s + 0.5*large.grid$w1 + 0.25*large.grid$w2

sum(large.grid$y1 >= large.grid$y0) / (grid.size ^ 3)
# only met for 0.65 combinations on the grid

# investigate where on the grid this is happening
fivenum(large.grid[which(large.grid$y1 < large.grid$y0),]$s) # only happening with negative s
fivenum(large.grid[which(large.grid$y1 < large.grid$y0),]$w1) # similar to w1 distribution
fivenum(large.grid[which(large.grid$y1 < large.grid$y0),]$w2) # similar to w2 distribution

# generate s values based on the actual distributions?
set.seed(1)
s0 <- rnorm(grid.size / 2, mean = mean0, sd = sd0)
s1 <- rnorm(grid.size / 2, mean = mean1, sd = sd1)
s.all <- append(s0, s1)

large.grid <- expand.grid(s.all, w1.grid, w2.grid)
names(large.grid) <- c("s", "w1", "w2")

large.grid$y1 <- 2.1 + 7.12*large.grid$s + 0.5*large.grid$w1 + 0.5*large.grid$w2 
large.grid$y0 <- 2 + 4.12*large.grid$s + 0.5*large.grid$w1 + 0.25*large.grid$w2 

sum(large.grid$y1 >= large.grid$y0) / (grid.size ^ 3)
# this is better, met 0.953 of the time

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
s.grid <- seq(-8, 15, length.out = grid.size)
plot(s.grid, pdf0, type = "l", col = "blue", xlab = "x", ylab = "pdf", main = "pdf")
lines(s.grid, pdf1, col = "red")
legend('topright', legend = c("Control", "Treatment"), col = c("blue", "red"), lty = 1)


####################################################
############    Setting 3     ######################
####################################################

# params
mean0 <- 1.45
sd0 <- 0.1
mean1 <- 1.7
sd1 <- 0.2

### Check assumption (C2) ###

# values for s grid
grid.size <- 10000
s.grid <- seq(3, 12, length.out = grid.size)

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
# 0.96

# for values of s where control > treatment (don't want), how much higher?
idx <- which(1 - cdf1 < 1 - cdf0)
fivenum((1 - cdf0[idx]) / (1 - cdf1[idx]) - 1)
# from the fivenum summary, the values are just barely larger; assumption (C2) very lightly violated

### Check assumption (C3) ###

# evaluate this assumption on a grid
grid.size <- 100
s.grid <- seq(3, 12, length.out = grid.size)
w1.grid <- seq(2, 6, length.out = grid.size)
w2.grid <- seq(3, 13, length.out = grid.size)

large.grid <- expand.grid(s.grid, w1.grid, w2.grid)
names(large.grid) <- c("s", "w1", "w2")


large.grid$y1 <- 2.1 + 12 * large.grid$s^2 + 6 * large.grid$s + 5*large.grid$w1 + 6*large.grid$w2
large.grid$y0 <- 2 + 12 * large.grid$s^2 + 1*large.grid$w1 + 1*large.grid$w2

sum(large.grid$y1 >= large.grid$y0) / (grid.size ^ 3)
# met for all combinations on the grid


# generate s, w1, and w2 values based on the actual distributions
set.seed(1)
s0 <- rlnorm(grid.size / 2, mean = mean0, sd = sd0)
s1 <- rlnorm(grid.size / 2, mean = mean1, sd = sd1)
s.all <- append(s0, s1)
w1.grid <- runif(grid.size, 2, 6)
w2.grid <- runif(grid.size, 3, 13)

large.grid <- expand.grid(s.all, w1.grid, w2.grid)
names(large.grid) <- c("s", "w1", "w2")

large.grid$y1 <- 2.1 + 12 * large.grid$s^2 + 6 * large.grid$s + 5*large.grid$w1 + 6*large.grid$w2
large.grid$y0 <- 2 + 12 * large.grid$s^2 + 1*large.grid$w1 + 1*large.grid$w2

sum(large.grid$y1 >= large.grid$y0) / (grid.size ^ 3)
# met for whole grid

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
###############    Setting 4    ####################
####################################################

# set parameters for mean and sd of the surrogate distributions 
mean0 <- 1.45
sd0 <- 0.1
mean1 <- 1.7
sd1 <- 0.2

### Check assumption (C2) ###

# values for s grid
grid.size <- 10000
s.grid <- seq(3, 12, length.out = grid.size)

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
# 0.96

# for values of s where control > treatment (don't want), how much higher?
idx <- which(1 - cdf1 < 1 - cdf0)
fivenum((1 - cdf0[idx]) / (1 - cdf1[idx]) - 1)
# from the fivenum summary, the values are just barely larger; assumption (C2) very lightly violated

### Check assumption (C3) ###

# evaluate this assumption on a grid
grid.size <- 100
s.grid <- seq(3, 12, length.out = grid.size)
w1.grid <- seq(2, 6, length.out = grid.size)
w2.grid <- seq(3, 13, length.out = grid.size)

large.grid <- expand.grid(s.grid, w1.grid, w2.grid)
names(large.grid) <- c("s", "w1", "w2")

large.grid$y1 <- 5 + 4*large.grid$s^2 + 5*large.grid$s + 1*large.grid$w1 + 1*large.grid$w2
large.grid$y0 <- 2 + 4*large.grid$s^2 + 1*large.grid$w1 + 1*large.grid$w2

sum(large.grid$y1 >= large.grid$y0) / (grid.size ^ 3)
# met everywhere on the grid

# what if I generate s values based on the actual distributions?
set.seed(1)
s0 <- rlnorm(grid.size / 2, mean = mean0, sd = sd0)
s1 <- rlnorm(grid.size / 2, mean = mean1, sd = sd1)
s.all <- append(s0, s1)

large.grid <- expand.grid(s.all, w1.grid, w2.grid)
names(large.grid) <- c("s", "w1", "w2")

large.grid$y1 <- 5 + 4*large.grid$s^2 + 5*large.grid$s + 1*large.grid$w1 + 1*large.grid$w2
large.grid$y0 <- 2 + 4*large.grid$s^2 + 1*large.grid$w1 + 1*large.grid$w2

sum(large.grid$y1 >= large.grid$y0) / (grid.size ^ 3)
# met everywhere on grid


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

