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

set.seed(6)
n = 1000
data.temp = gen.data(n=n, setting =setting)
# run one iteration of the simulations here --> set bandwidths in the master file!
func.output <- complex.heterogeneity(y = data.temp$Y,
                                     s = data.temp$S,
                                     a = data.temp$A,
                                     W.mat = matrix(cbind(data.temp$W1, data.temp$W2), ncol = 2),
                                     type = "model",
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

#THIS IS WRONG, IT SHOULD BE p<0.05, but none are significant right now, I don't know why  
data.sub$flag = as.factor(1*(return.grid$pval.threshold<0.05))

p.4 = ggplot(data.sub, aes(W1, W2, fill = flag)) + geom_tile()+ scale_fill_manual(breaks = levels(as.factor(data.sub$flag)), values = c("light grey","red"), labels = c("Not in region", "In region")) + labs(fill = paste("R > ",threshold,sep=""), title = paste("Setting 1, Confidence Region R > ", threshold,sep=""))

 library(gridExtra)
  grid.arrange(p.1,p.2,p.3,p.4, ncol = 2)


###tesing region with parametric appraoch first

setting =4
threshold=0.7
parallel.num=1
set.seed(parallel.num*100)

outputfile = c()
pfile = c()
for (i in 1:num.sim) {

data.temp = gen.data(n=n, setting =setting)
  func.output <- complex.heterogeneity(y = data.temp$Y,
                                       s = data.temp$S,
                                       a = data.temp$A,
                                       W.mat = matrix(cbind(data.temp$W1, data.temp$W2), ncol = 2),
                                       type = "model",
                                       variance = TRUE,
                                       test = TRUE,
                                       W.grid = w1w2.grid, h.0=1,h.1=1,h.3=1, threshold=threshold)
  outputfile <- rbind(outputfile,func.output$return.grid)
  pfile = rbind(pfile, func.output$pval)
}

setting.parametric <- empty.sim


#very important
each.rows = grid.size*grid.size
truth = get.truth(setting =setting, grid = cbind(outputfile[1:each.rows,1], outputfile[1:each.rows,2]))

for(i in 1:100) {
	sub.outputfile = outputfile[((i-1)*each.rows + 1):((i-1)*each.rows + each.rows),]
	results.temp = sub.outputfile

  results.temp$delta.true <- truth$delta
  results.temp$delta.s.true <- truth$delta.s
  results.temp$R.s.true <- truth$R

  setting.parametric$delta[i,] <- results.temp$delta
  setting.parametric$delta.s[i,] <- results.temp$delta.s
  setting.parametric$R.s[i,] <- results.temp$R.s
  setting.parametric$delta.bias[i,] <- results.temp$delta - results.temp$delta.true
  setting.parametric$delta.s.bias[i,] <- results.temp$delta.s - results.temp$delta.s.true
  setting.parametric$R.s.bias[i,] <- results.temp$R.s - results.temp$R.s.true
  setting.parametric$delta.var[i,] <- results.temp$delta.var
  setting.parametric$delta.s.var[i,] <- results.temp$delta.s.var
  setting.parametric$R.s.var[i,] <- results.temp$R.s.var
  setting.parametric$R.s.lower[i,] <- results.temp$R.s.lower
  setting.parametric$R.s.upper[i,] <- results.temp$R.s.upper
   setting.parametric$delta.lower[i,] <- results.temp$delta.lower
  setting.parametric$delta.upper[i,] <- results.temp$delta.upper
 setting.parametric$delta.s.lower[i,] <- results.temp$delta.s.lower
  setting.parametric$delta.s.upper[i,] <- results.temp$delta.s.upper
  setting.parametric$pval.threshold[i,] <- results.temp$pval.threshold


}

apply(setting.parametric$pval.threshold, 2, function(x) mean(x<0.05))
#inflated ok, that is what we expected
bh.p = c()
for(jj in 1:dim(setting.parametric$pval.threshold)[1]){
	data.sub = cbind(c(1:dim(setting.parametric$pval.threshold)[2]), t(setting.parametric$pval.threshold[jj,]))
	data.new = data.sub[order(data.sub[,2]),]
	new.reject = cbind(data.new,1*(data.new[,2] <= c(1:length(data.sub[,2]))/16*0.05))
	bh.p = rbind(bh.p, t(new.reject[order(new.reject[,1]),3]))
}
apply(bh.p, 2, mean)
#it works!!s
#so when it rejects, it is saying it IS in the region
#so when I look at setting 4 and threshold = 0.77, that is the type 1 error because it is on the boundary of the null. To look at power, let's move threshold to 0.7

#this is setting 4, threshold is 0.77
  X1   X2   X3   X4   X5   X6   X7   X8   X9  X10  X11  X12  X13  X14  X15  X16 
0.05 0.05 0.05 0.05 0.06 0.05 0.06 0.07 0.06 0.05 0.06 0.05 0.06 0.05 0.06 0.07