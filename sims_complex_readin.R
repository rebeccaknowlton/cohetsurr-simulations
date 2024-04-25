
#run all the functions in the master file
#set the working directory to where the txt files are

setwd("C:/Users/rkkno/Documents/University of Texas at Austin/Complex heterogeneity/Simulation files 4-24-24/output files")
setting <- 3
setting.parametric <- empty.sim
setting.two.step <- empty.sim

outputfile=c()
pfile=c()
for(u in 1:20) {
	outputfile = rbind(outputfile,read.table(paste("outputfile", setting, "_032924",u,".txt", sep=""), header = T))
 pfile = rbind(pfile,read.table(paste("pfile", setting, "_032924_",u,".txt", sep=""), header = F, skip=1))
}

#proportion rejected, parametric
apply(pfile[,1:2], 2, function(x) mean(x < 0.05, na.rm=T))
dim(pfile)

#parametric f-test
#two stage sup test

each.rows = grid.size*grid.size
truth = get.truth(setting =setting, grid = cbind(outputfile[1:each.rows,1], outputfile[1:each.rows,2]))



for(i in 1:dim(pfile)[1]) {
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
  setting.parametric$threshold.flag[i,] <- results.temp$threshold.flag

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
  setting.two.step$threshold.flag[i,] <- results.temp$threshold.flag.two.step
 
 
}
get.latex.tables(setting.parametric, setting.two.step, setting)


