
#run all the functions in the master file
#set the working directory to where the txt files are

setwd("C:/Users/rkkno/Documents/University of Texas at Austin/Complex heterogeneity/output files/Setting 3")
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

