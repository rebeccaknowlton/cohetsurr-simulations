
#run all the functions in the master file
#set the working directory to where the txt files are


setting.parametric <- empty.sim

outputfile=c()
pfile=c()
for(u in 1:10) {
 outputfile = rbind(outputfile,read.table(paste("outputfile", setting, "_",u,".txt", sep=""), header = T))
 pfile = rbind(pfile,read.table(paste("pfile", setting, "_",u,".txt", sep=""), header = T))
}

#proportion rejected
mean(pfile < 0.05)

#very important
each.rows = grid.size*grid.size
truth = get.truth(setting =setting, grid = cbind(outputfile[1:each.rows,1], outputfile[1:each.rows,2]))

for(i in 1:1000) {
	sub.outputfile = outputfile[((i-1)*each.rows + 1):((i-1)*each.rows + 16),]
	results.temp = sub.outputfile

  results.temp$`delta true` <- truth$delta
  results.temp$`delta.s true` <- truth$delta.s
  results.temp$`R.s true` <- truth$R

  setting.parametric$delta[i,] <- results.temp$delta
  setting.parametric$delta.s[i,] <- results.temp$delta.s
  setting.parametric$R.s[i,] <- results.temp$R.s
  setting.parametric$`delta.bias`[i,] <- results.temp$delta - results.temp$`delta true`
  setting.parametric$`delta.s.bias`[i,] <- results.temp$delta.s - results.temp$`delta.s true`
  setting.parametric$`R.s.bias`[i,] <- results.temp$R.s - results.temp$`R.s true`
  setting.parametric$`delta.var`[i,] <- results.temp$`delta.var`
  setting.parametric$`delta.s.var`[i,] <- results.temp$`delta.s.var`
  setting.parametric$`R.s.var`[i,] <- results.temp$`R.s.var`
}
get.latex.tables(setting.parametric, setting)

