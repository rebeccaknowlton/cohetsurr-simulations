
#run all the functions in the master file
#set the working directory to where the txt files are

setwd("C:/Users/rkkno/Documents/University of Texas at Austin/Complex heterogeneity/output files/Setting 1")
setting <- 3
setting.parametric <- empty.sim
setting.two.step <- empty.sim

outputfile=c()
pfile=c()
for(u in 1:20) {
	 outputfile = rbind(outputfile,read.table(paste("outputfile", setting, "_011824",u,".txt", sep=""), header = T))
 pfile = rbind(pfile,read.table(paste("pfile", setting, "_011824_",u,".txt", sep=""), header = T))

}

#proportion rejected, parametric
apply(pfile, 2, function(x) mean(x < 0.05))

#parametric f-test
#parametric sup test
#two stage sup test

#very important
# set.seed(1) -- need this line if get.truth function uses random sampling 
each.rows = grid.size*grid.size
truth = get.truth(setting =setting, grid = cbind(outputfile[1:each.rows,1], outputfile[1:each.rows,2]))

for(i in 1:1000) {
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

  setting.two.step$delta[i,] <- results.temp$delta.two.step
  setting.two.step$delta.s[i,] <- results.temp$delta.s.two.step
  setting.two.step$R.s[i,] <- results.temp$R.s.two.step
  setting.two.step$delta.bias[i,] <- results.temp$delta.two.step - results.temp$delta.true
  setting.two.step$delta.s.bias[i,] <- results.temp$delta.s.two.step - results.temp$delta.s.true
  setting.two.step$R.s.bias[i,] <- results.temp$R.s.two.step - results.temp$R.s.true
  setting.two.step$delta.var[i,] <- results.temp$delta.var.two.step
  setting.two.step$delta.s.var[i,] <- results.temp$delta.s.var.two.step
  setting.two.step$R.s.var[i,] <- results.temp$R.s.var.two.step
  setting.two.step$R.s.lower[i,] <- results.temp$R.s.lower.two.step
  setting.two.step$R.s.upper[i,] <- results.temp$R.s.upper.two.step
    setting.two.step$delta.lower[i,] <- results.temp$delta.lower.two.step
  setting.two.step$delta.upper[i,] <- results.temp$delta.upper.two.step
 setting.two.step$delta.s.lower[i,] <- results.temp$delta.s.lower.two.step
  setting.two.step$delta.s.upper[i,] <- results.temp$delta.s.upper.two.step
  setting.two.step$pval.threshold[i,] <- results.temp$pval.threshold.two.step

}
get.latex.tables(setting.parametric, setting.two.step, setting)


### trying to look at identification of region of strong surrogacy 

# unadjusted parametric
apply(setting.parametric$pval.threshold, 2, function(x) mean(x<0.05))
#for how many of the 1000, is at least 1 w gridpoint flagged as in the region?
mean(apply(setting.parametric$pval.threshold, 1, function(x) 1*(sum(x<0.05) >=1)))

# unadjusted two step
apply(setting.two.step$pval.threshold, 2, function(x) mean(x<0.05))
mean(apply(setting.two.step$pval.threshold, 1, function(x) 1*(sum(x<0.05) >=1)))

# adjusted parametric
pval.adj.p <- matrix(nrow = dim(setting.parametric$pval.threshold)[1], ncol = grid.size * grid.size)
for (jj in 1:dim(setting.parametric$pval.threshold)[1]) {
   data.sub <- setting.parametric$pval.threshold[jj,]
   ranks <- rank(data.sub, ties.method = "last")
   p.m.over.k <- data.sub * length(data.sub) / ranks
   for (r in 1:length(ranks)) {
     tmp.rank <- ranks[r]
     pval.adj.p[jj,r] <- min(1, min(p.m.over.k[ranks >= tmp.rank]))
   }
}
colMeans(pval.adj.p < 0.05)

# two step
pval.adj.ts <- matrix(nrow = dim(setting.two.step$pval.threshold)[1], ncol = grid.size * grid.size)
for (jj in 1:dim(setting.two.step$pval.threshold)[1]) {
  data.sub <- setting.two.step$pval.threshold[jj,]
  ranks <- rank(data.sub, ties.method = "last")
  p.m.over.k <- data.sub * length(data.sub) / ranks
  for (r in 1:length(ranks)) {
    tmp.rank <- ranks[r]
    pval.adj.ts[jj,r] <- min(1, min(p.m.over.k[ranks >= tmp.rank]))
  }
}
colMeans(pval.adj.ts < 0.05)

### plot for ID'ing a region

library(ggplot2)

W.grid.expand = cbind(outputfile[1:each.rows,1], outputfile[1:each.rows,2])

plot.data = as.data.frame(cbind(W.grid.expand, truth$R))
names(plot.data) = c("W1","W2","R.true")
plot.data$flag.true <- as.factor(1*(plot.data$R.true>threshold))

min.z= 0.6
max.z = 0.75

# Heatmap of true R
p.1 = ggplot(plot.data, aes(W1, W2, fill= R.true)) + 
  geom_tile() +
  scale_fill_gradient(low="yellow", high="red", limits = c(min.z,max.z)) + 
  xlab("W1") +
  ylab("W2") + 
  labs(fill = expression(paste(R[S], "(W)",sep="")), title = expression("Setting 1, True R", paste(R[S], "(W)",sep="")))
# Heatmap where true R > threshold
p.2 = ggplot(plot.data, aes(W1, W2, fill = flag.true)) + 
  geom_tile()+ 
  scale_fill_manual(breaks = levels(plot.data$flag.true), values = c("light grey","red"), labels = c("Not in region", "In region")) +
  labs(fill = paste("R >", threshold,sep=""), title = paste("Setting 1, True Region R >", threshold,sep=""))

plot.data$R.est.p <- colMeans(setting.parametric$R.s)
plot.data$R.est.ts <- colMeans(setting.two.step$R.s)

p.3 = ggplot(plot.data, aes(W1, W2, fill= R.est.p)) + 
  geom_tile() + scale_fill_gradient(low="yellow", high="red", limits = c(min.z,max.z)) + xlab("W1") + ylab("W2") + labs(fill = expression(paste(R[S], "(W)",sep="")), title = expression("Setting 1, Estimated R (Parametric)", paste(R[S], "(W)",sep="")))

plot.data$flag.p <- colMeans(pval.adj.p < 0.05)
plot.data$flag.ts <- colMeans(pval.adj.ts < 0.05)

p.4 = ggplot(plot.data, aes(W1, W2, fill = flag.p)) +
  geom_tile()+ scale_fill_gradient(low="lightgray", high="red", limits = c(0,1)) +
  xlab("W1") + ylab("W2") + labs(fill = expression(paste(R[S], "(W)",sep="")), title = expression("Setting 1, Confidence Region R (Parametric) > ", threshold, sep=""))
    
library(gridExtra)
grid.arrange(p.1,p.2,p.3,p.4, ncol = 2)

p.5 = ggplot(plot.data, aes(W1, W2, fill= R.est.ts)) + 
  geom_tile() + scale_fill_gradient(low="yellow", high="red", limits = c(min.z,max.z)) + xlab("W1") + ylab("W2") + labs(fill = expression(paste(R[S], "(W)",sep="")), title = expression("Setting 1, Estimated R (Two Step)", paste(R[S], "(W)",sep="")))
p.6 = ggplot(plot.data, aes(W1, W2, fill = flag.ts)) +
  geom_tile()+ scale_fill_gradient(low="lightgray", high="red", limits = c(0,0.5)) +
  xlab("W1") + ylab("W2") + labs(fill = expression(paste(R[S], "(W)",sep="")), title = expression("Setting 1, Confidence Region R (Two Step) > ", threshold, sep=""))

grid.arrange(p.1,p.2,p.5,p.6, ncol = 2)

