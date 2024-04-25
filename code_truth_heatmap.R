#################################################################
############ Heat map plots #######################################
#################################################################

#run master file functions, including setting threshold

library(ggplot2)

#heat map plots for Figure 1 
#cycle through all settings to get truths and estimates
#note doing setting 3 twice here because haven't run yet

setting1.hold = c()
setting2.hold = c()
setting3.hold = c()

for(jj in 1:3) {

setting = jj
params = get.parameters(setting=setting)

set.seed(1)
# create W grid
params = get.parameters(setting=setting)
grid.size=4
w1.large <- runif(10000, params$mean.w1, params$sd.w1)
w2.large <- runif(10000, params$mean.w2, params$sd.w2)
w1.grd <- seq(quantile(w1.large, 0.2), quantile(w1.large, 0.8), length = grid.size)
w2.grd <- seq(quantile(w2.large, 0.2), quantile(w2.large, 0.8), length = grid.size)
w1w2.grid <- matrix(cbind(w1.grd, w2.grd),ncol=2)
colnames(w1w2.grid) <- c("W1", "W2")

W.grid.expand <- expand.grid(split(w1w2.grid, rep(1:ncol(w1w2.grid), each = nrow(w1w2.grid))))
	
# calculate truth on grid
truth = get.truth(setting =setting, grid = as.matrix(W.grid.expand))
	
setting.parametric <- empty.sim
setting.two.step <- empty.sim

outputfile=c()
pfile=c()
for(u in 1:20) {
	outputfile = rbind(outputfile,read.table(paste("outputfile", setting, "_032924",u,".txt", sep=""), header = T))
 pfile = rbind(pfile,read.table(paste("pfile", setting, "_032924_",u,".txt", sep=""), header = F, skip=1))

}

for(i in 1:dim(pfile)[1]) {
	sub.outputfile = outputfile[((i-1)*each.rows + 1):((i-1)*each.rows + each.rows),]
	results.temp = sub.outputfile

   setting.parametric$R.s[i,] <- results.temp$R.s
   setting.two.step$R.s[i,] <- results.temp$R.s.two.step
}

if(jj == 1) {
	setting1.hold = cbind(W.grid.expand, truth$R,apply(setting.parametric$R.s, 2, mean), apply(setting.two.step$R.s, 2, mean))
}
if(jj == 2) {
	setting2.hold = cbind(W.grid.expand, truth$R,apply(setting.parametric$R.s, 2, mean), apply(setting.two.step$R.s, 2, mean))
}
if(jj == 3) {
	setting3.hold = cbind(W.grid.expand, truth$R,apply(setting.parametric$R.s, 2, mean), apply(setting.two.step$R.s, 2, mean))
}
}	

all.R = cbind(setting1.hold[,-c(1:2)], setting2.hold[,-c(1:2)], setting3.hold[,-c(1:2)])
min.z=min(all.R)
max.z = max(all.R)

setting1.hold = as.data.frame(setting1.hold)
names(setting1.hold) = c("W1","W2", "truth", "parametric","twostep")
setting2.hold = as.data.frame(setting2.hold)
names(setting2.hold) = c("W1","W2", "truth", "parametric","twostep")
setting3.hold = as.data.frame(setting3.hold)
names(setting3.hold) = c("W1","W2", "truth", "parametric","twostep")
 
# Heatmap 
p.1.truth = ggplot(setting1.hold, aes(W1, W2, fill= truth)) + 
  geom_tile() + 
  scale_fill_gradient(low="yellow", high="red", limits = c(min.z,max.z)) + 
  xlab("W1") +
  ylab("W2") +
  labs(fill = expression(paste(R[S], "(W)",sep="")), title = expression("Setting 1 Truth", paste(R[S], "(W)",sep="")))

p.1.p = ggplot(setting1.hold, aes(W1, W2, fill= parametric)) + 
  geom_tile() + 
  scale_fill_gradient(low="yellow", high="red", limits = c(min.z,max.z)) + 
  xlab("W1") +
  ylab("W2") +
  labs(fill = expression(paste(R[S], "(W)",sep="")), title = expression("Setting 1 Parametric", paste(R[S], "(W)",sep="")))

p.1.t = ggplot(setting1.hold, aes(W1, W2, fill= twostep)) + 
  geom_tile() + 
  scale_fill_gradient(low="yellow", high="red", limits = c(min.z,max.z)) + 
  xlab("W1") +
  ylab("W2") +
  labs(fill = expression(paste(R[S], "(W)",sep="")), title = expression("Setting 1 Two-stage", paste(R[S], "(W)",sep="")))

p.2.truth = ggplot(setting2.hold, aes(W1, W2, fill= truth)) + 
  geom_tile() + 
  scale_fill_gradient(low="yellow", high="red", limits = c(min.z,max.z)) + 
  xlab("W1") +
  ylab("W2") +
  labs(fill = expression(paste(R[S], "(W)",sep="")), title = expression("Setting 2 Truth", paste(R[S], "(W)",sep="")))

p.2.p = ggplot(setting2.hold, aes(W1, W2, fill= parametric)) + 
  geom_tile() + 
  scale_fill_gradient(low="yellow", high="red", limits = c(min.z,max.z)) + 
  xlab("W1") +
  ylab("W2") +
  labs(fill = expression(paste(R[S], "(W)",sep="")), title = expression("Setting 2 Parametric", paste(R[S], "(W)",sep="")))

p.2.t = ggplot(setting2.hold, aes(W1, W2, fill= twostep)) + 
  geom_tile() + 
  scale_fill_gradient(low="yellow", high="red", limits = c(min.z,max.z)) + 
  xlab("W1") +
  ylab("W2") +
  labs(fill = expression(paste(R[S], "(W)",sep="")), title = expression("Setting 2 Two-stage", paste(R[S], "(W)",sep="")))

p.3.truth = ggplot(setting3.hold, aes(W1, W2, fill= truth)) + 
  geom_tile() + 
  scale_fill_gradient(low="yellow", high="red", limits = c(min.z,max.z)) + 
  xlab("W1") +
  ylab("W2") +
  labs(fill = expression(paste(R[S], "(W)",sep="")), title = expression("Setting 3 Truth", paste(R[S], "(W)",sep="")))

p.3.p = ggplot(setting3.hold, aes(W1, W2, fill= parametric)) + 
  geom_tile() + 
  scale_fill_gradient(low="yellow", high="red", limits = c(min.z,max.z)) + 
  xlab("W1") +
  ylab("W2") +
  labs(fill = expression(paste(R[S], "(W)",sep="")), title = expression("Setting 3 Parametric", paste(R[S], "(W)",sep="")))

p.3.t = ggplot(setting3.hold, aes(W1, W2, fill= twostep)) + 
  geom_tile() + 
  scale_fill_gradient(low="yellow", high="red", limits = c(min.z,max.z)) + 
  xlab("W1") +
  ylab("W2") +
  labs(fill = expression(paste(R[S], "(W)",sep="")), title = expression("Setting 3 Two-stage", paste(R[S], "(W)",sep="")))


library(gridExtra)
grid.arrange(p.1.truth, p.1.p,p.1.t, p.2.truth, p.2.p,p.2.t, p.3.truth, p.3.p,p.3.t, ncol = 3)
  
  
#################################################################
############ Region plots #######################################
#################################################################

threshold.1 = 0.75
threshold.2=0.7
threshold.3 = 0.5

setting1.region = c()
setting2.region = c()
setting3.region = c()

for(kk in 1:3) {

setting = kk

params = get.parameters(setting=setting)

set.seed(1)
# create W grid
w1.large <- runif(10000, params$mean.w1, params$sd.w1)
w2.large <- runif(10000, params$mean.w2, params$sd.w2)
w1.grd <- seq(min(w1.large), max(w1.large), length =4)
w2.grd <- seq(min(w2.large), max(w2.large), length =4)
w1w2.grid <- matrix(cbind(w1.grd, w2.grd),ncol=2)
colnames(w1w2.grid) <- c("W1", "w2")
W.grid.expand <- expand.grid(split(w1w2.grid, rep(1:ncol(w1w2.grid), each = nrow(w1w2.grid))))
	
# calculate truth on grid
truth = get.truth(setting =setting, grid = as.matrix(W.grid.expand))
	
setting.parametric <- empty.sim
setting.two.step <- empty.sim

outputfile=c()
pfile=c()
for(u in 1:20) {
	outputfile = rbind(outputfile,read.table(paste("outputfile", setting, "_032924",u,".txt", sep=""), header = T))
 pfile = rbind(pfile,read.table(paste("pfile", setting, "_032924_",u,".txt", sep=""), header = F, skip=1))

}



for(i in 1:dim(pfile)[1]) {
	sub.outputfile = outputfile[((i-1)*each.rows + 1):((i-1)*each.rows + each.rows),]
	results.temp = sub.outputfile

    setting.parametric$threshold.flag[i,] <- results.temp$threshold.flag
	setting.two.step$threshold.flag[i,] <- results.temp$threshold.flag.two.step
 	}

prop.p = colMeans(setting.parametric$threshold.flag)
prop.t = colMeans(setting.two.step$threshold.flag)

if(kk == 1) {
	setting1.region = cbind(W.grid.expand, truth$R,prop.p, prop.t)
}
if(kk == 2) {
	setting2.region = cbind(W.grid.expand, truth$R,prop.p, prop.t)
}
if(kk == 3) {
	setting3.region = cbind(W.grid.expand, truth$R,prop.p, prop.t)
}
}	


min.z=0
max.z = 1

setting1.region = as.data.frame(setting1.region)
names(setting1.region) = c("W1","W2", "truth", "parametric","twostep")
setting2.region = as.data.frame(setting2.region)
names(setting2.region) = c("W1","W2", "truth", "parametric","twostep")
setting3.region = as.data.frame(setting3.region)
names(setting3.region) = c("W1","W2", "truth", "parametric","twostep")
 
# Heatmap
setting1.region$flag.true <- as.factor(1*(setting1.region$truth>threshold.1))
 
p.1.truth = ggplot(setting1.region, aes(W1, W2, fill= flag.true)) + 
  geom_tile() + 
  xlab("W1") +
  ylab("W2") +
 scale_fill_manual(breaks = c("0","1"), values = c("light grey","red"), labels = c("Not in region", "In region")) +
  labs(fill = expression(paste("R > ", kappa ,sep="")), title = expression(paste("Setting 1, True Region R > ", kappa ,sep="")))

p.1.p = ggplot(setting1.region, aes(W1, W2, fill= parametric)) + 
  geom_tile() + 
  scale_fill_gradient(low="grey", high="red", limits = c(min.z,max.z)) + 
  xlab("W1") +
  ylab("W2") +
  labs(fill = "Proportion flagged \nwithin region", title = "Setting 1 Parametric")

p.1.t = ggplot(setting1.region, aes(W1, W2, fill= twostep)) + 
  geom_tile() + 
  scale_fill_gradient(low="grey", high="red", limits = c(min.z,max.z)) + 
  xlab("W1") +
  ylab("W2") +
  labs(fill = "Proportion flagged \nwithin region", title = "Setting 1 Two-stage")

setting2.region$flag.true <- as.factor(1*(round(setting2.region$truth,3)>threshold.2))
 
p.2.truth = ggplot(setting2.region, aes(W1, W2, fill= flag.true)) + 
  geom_tile() + 
  xlab("W1") +
  ylab("W2") +
 scale_fill_manual(breaks = c("0","1"), values = c("light grey","red"), labels = c("Not in region", "In region")) +
  labs(fill = expression(paste("R > ", kappa ,sep="")), title = expression(paste("Setting 2, True Region R > ", kappa ,sep="")))

p.2.p = ggplot(setting2.region, aes(W1, W2, fill= parametric)) + 
  geom_tile() + 
  scale_fill_gradient(low="grey", high="red", limits = c(min.z,max.z)) + 
  xlab("W1") +
  ylab("W2") +
  labs(fill = "Proportion flagged \nwithin region", title = "Setting 2 Parametric")

p.2.t = ggplot(setting2.region, aes(W1, W2, fill= twostep)) + 
  geom_tile() + 
  scale_fill_gradient(low="grey", high="red", limits = c(min.z,max.z)) + 
  xlab("W1") +
  ylab("W2") +
  labs(fill = "Proportion flagged \nwithin region", title = "Setting 2 Two-stage")

setting3.region$flag.true <- as.factor(1*(round(setting3.region$truth,3)>threshold.3))
 
p.3.truth = ggplot(setting3.region, aes(W1, W2, fill= flag.true)) + 
  geom_tile() + 
  xlab("W1") +
  ylab("W2") +
 scale_fill_manual(breaks = c("0","1"), values = c("light grey","red"), labels = c("Not in region", "In region")) +
  labs(fill = expression(paste("R > ", kappa ,sep="")), title = expression(paste("Setting 3, True Region R > ", kappa ,sep="")))

p.3.p = ggplot(setting3.region, aes(W1, W2, fill= parametric)) + 
  geom_tile() + 
  scale_fill_gradient(low="grey", high="red", limits = c(min.z,max.z)) + 
  xlab("W1") +
  ylab("W2") +
  labs(fill = "Proportion flagged \nwithin region", title = "Setting 3 Parametric")

p.3.t = ggplot(setting3.region, aes(W1, W2, fill= twostep)) + 
  geom_tile() + 
  scale_fill_gradient(low="grey", high="red", limits = c(min.z,max.z)) + 
  xlab("W1") +
  ylab("W2") +
  labs(fill = "Proportion flagged \nwithin region", title = "Setting 3 Two-stage")
  
  
library(gridExtra)
grid.arrange(p.1.truth, p.1.p,p.1.t, p.2.truth, p.2.p,p.2.t, p.3.truth, p.3.p,p.3.t, ncol = 3)



