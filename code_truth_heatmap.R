# Make single plot for setting 1

setting =1

params = get.parameters(setting=setting)

set.seed(1)
# create W grid
if(setting == 1 | setting == 2 | setting == 3| setting == 4){
	w1.large <- runif(10000, params$mean.w1, params$sd.w1)
	w2.large <- runif(10000, params$mean.w2, params$sd.w2)
}
w1.grd <- seq(min(w1.large), max(w1.large), length =10)
w2.grd <- seq(min(w2.large), max(w2.large), length =10)
w1w2.grid <- matrix(cbind(w1.grd, w2.grd),ncol=2)
colnames(w1w2.grid) <- c("W1", "w2")
W.grid.expand <- expand.grid(split(w1w2.grid, rep(1:ncol(w1w2.grid), each = nrow(w1w2.grid))))
	
# calculate truth on grid
truth = get.truth(setting =setting, grid = as.matrix(W.grid.expand))
	
	

#heat map, cycle through all settings and make a panel plot
library(ggplot2)
data = as.data.frame(cbind(W.grid.expand, truth$R))
names(data) = c("W1","W2","R")

range.truth <- function(data) {
  return(c(min(data$R),max(data$R)))
}

range.truth(data)

data.sub=data
# Heatmap 
min.z=range.truth(data)[1]
max.z = range.truth(data)[2]
R.s = ggplot(data.sub, aes(W1, W2, fill= R)) + 
  geom_tile() + 
  scale_fill_gradient(low="yellow", high="red", limits = c(min.z,max.z)) + 
  xlab("W1") + 
  ylab("W2") + 
  labs(fill = expression(paste(R[S], "(W)",sep="")), title = expression("Setting 1", paste(R[S], "(W)",sep="")))
R.s



# cycle through all settings and make a panel plot
# use same W grid from above
# use min.z and max.z from setting 3 since it has the widest range

setting =3
params = get.parameters(setting=setting)
truth = get.truth(setting =setting, grid = as.matrix(W.grid.expand))

data = as.data.frame(cbind(W.grid.expand, truth$R))
names(data) = c("W1","W2","R")
data.sub=data
range.truth(data)
min.z=range.truth(data)[1]
max.z = range.truth(data)[2]

# Heatmap 
p.3 = ggplot(data.sub, aes(W1, W2, fill= R)) + 
  geom_tile() + scale_fill_gradient(low="yellow", high="red", limits = c(min.z,max.z)) + xlab("W1") + ylab("W2") + labs(fill = expression(paste(R[S], "(W)",sep="")), title = expression("Setting 3", paste(R[S], "(W)",sep="")))


setting = 1
params = get.parameters(setting=setting)
truth = get.truth(setting =setting, grid = as.matrix(W.grid.expand))

data = as.data.frame(cbind(W.grid.expand, truth$R))
names(data) = c("W1","W2","R")
data.sub=data
range.truth(data)

# Heatmap 
p.1 = ggplot(data.sub, aes(W1, W2, fill= R)) + 
  geom_tile() + 
  scale_fill_gradient(low="yellow", high="red", limits = c(min.z,max.z)) + 
  xlab("W1") +
  ylab("W2") +
  labs(fill = expression(paste(R[S], "(W)",sep="")), title = expression("Setting 1", paste(R[S], "(W)",sep="")))

setting =2
params = get.parameters(setting=setting)
truth = get.truth(setting =setting, grid = as.matrix(W.grid.expand))

data = as.data.frame(cbind(W.grid.expand, truth$R))
names(data) = c("W1","W2","R")
data.sub=data
range.truth(data)

# Heatmap 
p.2 = ggplot(data.sub, aes(W1, W2, fill= R)) + 
  geom_tile() +
  scale_fill_gradient(low="yellow", high="red", limits = c(min.z,max.z)) +
  xlab("W1") + 
  ylab("W2") + 
  labs(fill = expression(paste(R[S], "(W)",sep="")), title = expression("Setting 2", paste(R[S], "(W)",sep="")))
	

setting =4
params = get.parameters(setting=setting)
truth = get.truth(setting =setting, grid = as.matrix(W.grid.expand))

data = as.data.frame(cbind(W.grid.expand, truth$R))
names(data) = c("W1","W2","R")
data.sub=data
range.truth(data)

# Heatmap 
p.4 = ggplot(data.sub, aes(W1, W2, fill= R)) + 
  geom_tile() + scale_fill_gradient(low="yellow", high="red", limits = c(min.z,max.z)) + xlab("W1") + ylab("W2") + labs(fill = expression(paste(R[S], "(W)",sep="")), title = expression("Setting 4", paste(R[S], "(W)",sep="")))
  
library(gridExtra)
grid.arrange(p.1,p.2,p.3,p.4, ncol = 2)
  
  
#plot for region idea
 	
library(ggplot2)
setting =1
params = get.parameters(setting=setting)
truth = get.truth(setting =setting, grid = as.matrix(W.grid.expand))
data = as.data.frame(cbind(W.grid.expand, truth$R))
min.z=range.truth(truth)[1]
max.z = range.truth(truth)[2]
names(data) = c("W1","W2","R")
data.sub=data

threshold = 0.70
data.sub$flag = as.factor(1*(data.sub$R>threshold))
# Heatmap of true R
p.1 = ggplot(data.sub, aes(W1, W2, fill= R)) + 
  geom_tile() +
  scale_fill_gradient(low="yellow", high="red", limits = c(min.z,max.z)) + 
  xlab("W1") +
  ylab("W2") + 
  labs(fill = expression(paste(R[S], "(W)",sep="")), title = expression("Setting 1, True R", paste(R[S], "(W)",sep="")))
# Heatmap where true R > threshold
p.2 = ggplot(data.sub, aes(W1, W2, fill = flag)) + 
  geom_tile()+ 
  scale_fill_manual(breaks = levels(data.sub$flag), values = c("light grey","red"), labels = c("Not in region", "In region")) +
  labs(fill = paste("R >", threshold,sep=""), title = paste("Setting 1, True Region R >", threshold,sep=""))

set.seed(1)
n = 1000
data.temp = gen.data(n=n, setting =setting)
# run one iteration of the simulations here --> set bandwidths in the master file!
func.output <- complex.heterogeneity(y = data.temp$Y,
                                     s = data.temp$S,
                                     a = data.temp$A,
                                     W.mat = matrix(cbind(data.temp$W1, data.temp$W2), ncol = 2),
                                     type = "both",
                                     variance = TRUE,
                                     test = TRUE,
                                     W.grid = w1w2.grid, threshold=threshold, h.0=h.0,h.1=h.1,h.3=h.3)


return.grid = func.output$return.grid

data = as.data.frame(cbind(W.grid.expand, return.grid$R.s))
names(data) = c("W1","W2","R")
data.sub=data
min.z <- min(data$R)
max.z <- max(data$R)

p.3 = ggplot(data.sub, aes(W1, W2, fill= R)) + 
  geom_tile() + scale_fill_gradient(low="yellow", high="red", limits = c(min.z,max.z)) + xlab("W1") + ylab("W2") + labs(fill = expression(paste(R[S], "(W)",sep="")), title = expression("Setting 1, Estimated R", paste(R[S], "(W)",sep="")))

# adjust p values
pval.adj <- rep(NA, 100)
ranks <- rank(return.grid$pval.threshold, ties.method = "last")
p.m.over.k <- return.grid$pval.threshold * length(return.grid$pval.threshold) / ranks
for (r in 1:length(ranks)) {
  tmp.rank <- ranks[r]
  pval.adj[r] <- min(1, min(p.m.over.k[ranks >= tmp.rank]))
}
return.grid$pval.adj = pval.adj
data.sub$flag = as.factor(1*(return.grid$pval.adj<0.05))


p.4 = ggplot(data.sub, aes(W1, W2, fill = flag)) + geom_tile()+ scale_fill_manual(breaks = levels(as.factor(data.sub$flag)), values = c("light grey","red"), labels = c("Not in region", "In region")) + labs(fill = paste("R > ",threshold,sep=""), title = paste("Setting 1, Confidence Region R > ", threshold,sep=""))

library(gridExtra)
# parametric
grid.arrange(p.1,p.2,p.3,p.4, ncol = 2)

# two step
data = as.data.frame(cbind(W.grid.expand, return.grid$R.s.two.step))
names(data) = c("W1","W2","R")
data.sub=data
min.z <- min(data$R)

max.z <- max(data$R)

p.5 = ggplot(data.sub, aes(W1, W2, fill= R)) + 
  geom_tile() + scale_fill_gradient(low="yellow", high="red", limits = c(min.z,max.z)) + xlab("W1") + ylab("W2") + labs(fill = expression(paste(R[S], "(W)",sep="")), title = expression("Setting 1, Estimated R", paste(R[S], "(W)",sep="")))

# adjust p values
pval.adj <- rep(NA, 100)
ranks <- rank(return.grid$pval.threshold.two.step, ties.method = "last")
p.m.over.k <- return.grid$pval.threshold.two.step * length(return.grid$pval.threshold.two.step) / ranks
for (r in 1:length(ranks)) {
  tmp.rank <- ranks[r]
  pval.adj[r] <- min(1, min(p.m.over.k[ranks >= tmp.rank]))
}
return.grid$pval.adj = pval.adj
data.sub$flag = as.factor(1*(return.grid$pval.adj<0.05))

p.6 = ggplot(data.sub, aes(W1, W2, fill = flag)) + geom_tile()+ scale_fill_manual(breaks = levels(as.factor(data.sub$flag)), values = c("light grey","red"), labels = c("Not in region", "In region")) + labs(fill = paste("R > ",threshold,sep=""), title = paste("Setting 1, Confidence Region R > ", threshold,sep=""))

# two step
grid.arrange(p.1,p.2,p.5,p.6, ncol = 2)