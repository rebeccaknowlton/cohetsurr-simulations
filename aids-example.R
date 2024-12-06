library(Rsurrogate)
library(ggplot2)
library(tidyverse)

tmpg <- function(xx){exp(xx)}; tmpginv <- function(xx){log(xx)}
tmpg <- function(xx){xx^2}; tmpdg <- function(xx){2*xx}; tmpginv <- function(xx){sqrt(xx)}
log.link <- list(tmpg, tmpg, tmpginv); rm(tmpg, tmpginv)
tmpdir <- "./"
aids.base <- read.table(paste(tmpdir, "actg320.base.dat", sep=""))
aids.rna <- read.table(paste(tmpdir, "actg320.rna", sep=""))
aids.cd4 <- read.table(paste(tmpdir, "actg320.cd4", sep=""))
aids.trt <- read.table(paste(tmpdir, "actg320.trt", sep=""))

n.total = dim(aids.base)[1]
aids.base$TREAT = vector(length = n.total)
aids.base$CD4BASE = vector(length = n.total)
aids.base$CD424 = vector(length = n.total)
aids.base$RNABASE = vector(length = n.total)
aids.base$RNA24 = vector(length = n.total)

for(i in 1:n.total) {
  aids.base$TREAT[i] = aids.trt[aids.trt$PATID == aids.base$PATID[i],2]
  if(dim(aids.cd4[aids.cd4$PATID == aids.base$PATID[i] & aids.cd4$WEEK == 0,])[1] > 0) {aids.base$CD4BASE[i] = aids.cd4[aids.cd4$PATID == aids.base$PATID[i] & aids.cd4$WEEK == 0,]$CD4} else {aids.base$CD4BASE[i] = NA}
  if(dim(aids.cd4[aids.cd4$PATID == aids.base$PATID[i] & aids.cd4$WEEK == 24,])[1] > 0) {aids.base$CD424[i] = aids.cd4[aids.cd4$PATID == aids.base$PATID[i] & aids.cd4$WEEK == 24,]$CD4} else {aids.base$CD424[i] = NA}
  if(dim(aids.rna[aids.rna$PATID == aids.base$PATID[i] & aids.rna$WEEK == 0,])[1] >0) {aids.base$RNABASE[i] = aids.rna[aids.rna$PATID == aids.base$PATID[i] & aids.rna$WEEK == 0,]$ULOGRNA} else {aids.base$RNABASE[i] = NA}
  if(dim(aids.rna[aids.rna$PATID == aids.base$PATID[i] & aids.rna$WEEK == 24,])[1] >0) {aids.base$RNA24[i] = aids.rna[aids.rna$PATID == aids.base$PATID[i] & aids.rna$WEEK == 24,]$ULOGRNA} else {aids.base$RNA24[i] = NA}
}

#remember for RNA, LOWER is Better; for CD4 HIGHER is better

aids.base$CD4CHANGE = aids.base$CD424 - aids.base$CD4BASE
aids.base$RNACHANGE = aids.base$RNA24 - aids.base$RNABASE

# A = TREAT - 1
# Y = RNACHANGE
# S = CD4CHANGE
# W1 = CD4BASE
# W2 = RACE or AGE or PRIOZDV

# NOTE: function won't run without pre-specified W grid because W.mat has NA values. Only run on data with values for all the selected variables.

aids.data <- aids.base[!is.na(aids.base$CD4BASE),]
aids.data <- aids.data[!is.na(aids.data$RNACHANGE),]
aids.data <- aids.data[!is.na(aids.data$CD4CHANGE),]
nrow(aids.data) / nrow(aids.base)
nrow(aids.data)
sum(aids.data$TREAT==2)
sum(aids.data$TREAT==1)

# overall PTE
set.seed(1)
rr = R.s.estimate(sone = aids.data$CD4CHANGE[aids.data$TREAT==2], szero = aids.data$CD4CHANGE[aids.data$TREAT == 1], yone = aids.data$RNACHANGE[aids.data$TREAT ==2], yzero = aids.data$RNACHANGE[aids.data$TREAT==1], var = TRUE, conf.int = TRUE)
print(rr$R.s)
print(rr$conf.int.normal.R.s)


# new methods
set.seed(1)
aids.output <- complex.heterogeneity(y = aids.data$RNACHANGE,
                      s = aids.data$CD4CHANGE,
                      a = aids.data$TREAT - 1,
                      W.mat = matrix(cbind(aids.data$CD4BASE, aids.data$AGE), ncol = 2),
                      type = "both",
                      variance = TRUE,
                      test = TRUE,
                      grid.size = 10, 
                      threshold = 0.5)

aids.pval <- aids.output$pval
aids.results <- aids.output$return.grid

plot.data <- data.frame("W1" = aids.results[,1],
                        "W2" = aids.results[,2],
                        "Model.est" = aids.results$R.s,
                        "Model.lower" = aids.results$R.s.lower,
                        "Model.upper" = aids.results$R.s.upper,
                        "Two.step.est" = aids.results$R.s.two.step,
                        "Two.step.lower" = aids.results$R.s.lower.two.step,
                        "Two.step.upper" = aids.results$R.s.upper.two.step)

plot.data.long = pivot_longer(plot.data, cols = contains("est"), values_to = "R.estimate",names_to = "method")
plot.data.long$R.lower = rep(NA, length = nrow(plot.data.long))
plot.data.long$R.upper = rep(NA, length = nrow(plot.data.long))
plot.data.long$R.lower[plot.data.long$method == "Model.est"] <- plot.data.long$Model.lower[plot.data.long$method == "Model.est"]
plot.data.long$R.upper[plot.data.long$method == "Model.est"] <- plot.data.long$Model.upper[plot.data.long$method == "Model.est"]
plot.data.long$R.lower[plot.data.long$method == "Two.step.est"] <- plot.data.long$Two.step.lower[plot.data.long$method == "Two.step.est"]
plot.data.long$R.upper[plot.data.long$method == "Two.step.est"] <- plot.data.long$Two.step.upper[plot.data.long$method == "Two.step.est"]

# drop unnecessary cols
plot.data.long <- plot.data.long %>% select(-Model.lower, -Model.upper, -Two.step.lower, -Two.step.upper)
# rename method column
plot.data.long$method <- factor(plot.data.long$method , levels=c("Model.est", "Two.step.est"))
levels(plot.data.long$method) <- c("Model", "Two Step")

colors <- c("#56B4E9","#CC79A7")

plot.data.long$W2 = round(plot.data.long$W2)

ggplot(plot.data.long, aes(x = W1, y = R.estimate, color=method, group = method)) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  geom_ribbon(show.legend = FALSE, aes(ymin = R.lower, ymax = R.upper), alpha = 0.3) +
  labs(y = "R estimate",x = "Baseline CD4") +
  facet_wrap(~W2, nrow=2) +
  # geom_hline(yintercept = 0.5)+
  theme_bw() +
  coord_cartesian(ylim = c(-0.1, 1.2))


# ID a region - UPDATE
mean(aids.results$threshold.flag)
mean(aids.results$threshold.flag.two.step)

# make tables
table.data.model <- data.frame("CD4.Base" = aids.results[,1],
                               "Age" = aids.results[,2],
                               "R.s.est" = aids.results$R.s,
                               "R.s.SE" = sqrt(aids.results$R.s.var),
                               "R.s.lower" = aids.results$R.s.lower,
                               "R.s.upper" = aids.results$R.s.upper)
table.data.two.step <- data.frame("CD4.Base" = aids.results[,1],
                                  "Age" = aids.results[,2],
                                  "R.s.est" = aids.results$R.s.two.step,
                                  "R.s.SE" = sqrt(aids.results$R.s.var.two.step),
                                  "R.s.lower" = aids.results$R.s.lower.two.step,
                                  "R.s.upper" = aids.results$R.s.upper.two.step)

get.CI <- function(l, u) {paste0("(",sprintf("%0.2f", l),", ",sprintf("%0.2f", u),")")}
table.data.model$R.CI <- get.CI(table.data.model$R.s.lower, table.data.model$R.s.upper)
table.data.two.step$R.CI <- get.CI(table.data.two.step$R.s.lower, table.data.two.step$R.s.upper)

#index for w, boundary, middle,middle, boundary, average
j=sqrt(nrow(aids.results))
index.diag = seq(from = 1, to = j*j, by = j+1)

table.print <-table.data.model[index.diag,c(1,2,3,4,7)]
table.print[,1:4] <- round(table.print[,1:4],2)
rownames(table.print) <- NULL
latex.table(as.matrix(table.print), "aids.example.model", caption = "", dcolumn = T, rownames = FALSE)

table.print <- table.data.two.step[index.diag,c(1,2,3,4,7)]
table.print[,1:4] <- round(table.print[,1:4],2)
rownames(table.print) <- NULL
latex.table(as.matrix(table.print), "aids.example.two.step", caption = "", dcolumn = T, rownames = FALSE)


###########################################################
############# Empirically examine assumptions #############
###########################################################

### Assumption (C1): E(y1 | s, w) is monotone increasing in s ###

treatment.data <- aids.data[aids.data$TREAT==2,]
control.data <- aids.data[aids.data$TREAT==1,]

# Y = RNACHANGE
# S = CD4CHANGE
# w1 = CD4BASE
# W2 = AGE

# overall plot
ggplot(treatment.data, aes(CD4CHANGE, RNACHANGE)) + geom_point() + geom_smooth()

w1.breaks <- quantile(treatment.data$CD4BASE, c(0.33,0.67))
w2.breaks <- quantile(treatment.data$AGE, c(0.33,0.67))

# want to save idx that define subgroups of patients who are between these thresholds
idx1 <- which((treatment.data$CD4BASE < w1.breaks[1]) & (treatment.data$AGE < w2.breaks[1]))
idx2 <- which((treatment.data$CD4BASE < w1.breaks[1]) & (treatment.data$AGE >= w2.breaks[1]) & (treatment.data$AGE < w2.breaks[2]))
idx3 <- which((treatment.data$CD4BASE < w1.breaks[1]) & (treatment.data$AGE >= w2.breaks[2]))
idx4 <- which((treatment.data$CD4BASE >= w1.breaks[1]) & (treatment.data$CD4BASE < w1.breaks[2]) & (treatment.data$AGE < w2.breaks[1]))
idx5 <- which((treatment.data$CD4BASE >= w1.breaks[1]) & (treatment.data$CD4BASE < w1.breaks[2]) & (treatment.data$AGE >= w2.breaks[1]) & (treatment.data$AGE < w2.breaks[2]))
idx6 <- which((treatment.data$CD4BASE >= w1.breaks[1]) & (treatment.data$CD4BASE < w1.breaks[2]) & (treatment.data$AGE >= w2.breaks[2]))
idx7 <- which((treatment.data$CD4BASE >= w1.breaks[2]) & (treatment.data$AGE < w2.breaks[1]))
idx8 <- which((treatment.data$CD4BASE >= w1.breaks[2]) & (treatment.data$AGE >= w2.breaks[1]) & (treatment.data$AGE < w2.breaks[2]))
idx9 <- which((treatment.data$CD4BASE >= w1.breaks[2])  & (treatment.data$AGE >= w2.breaks[2]))

length(idx1)
length(idx2)
length(idx3)
length(idx4)
length(idx5)
length(idx6)
length(idx7)
length(idx8)
length(idx9)

# subgroup plots
p1 = ggplot(treatment.data[idx1,], aes(CD4CHANGE, RNACHANGE)) + geom_point() + geom_smooth() + labs(title = "Region 1")
p2 = ggplot(treatment.data[idx2,], aes(CD4CHANGE, RNACHANGE)) + geom_point() + geom_smooth() + labs(title = "Region 2")
p3 = ggplot(treatment.data[idx3,], aes(CD4CHANGE, RNACHANGE)) + geom_point() + geom_smooth() + labs(title = "Region 3")
p4 = ggplot(treatment.data[idx4,], aes(CD4CHANGE, RNACHANGE)) + geom_point() + geom_smooth() + labs(title = "Region 4")
p5 = ggplot(treatment.data[idx5,], aes(CD4CHANGE, RNACHANGE)) + geom_point() + geom_smooth() + labs(title = "Region 5")
p6 = ggplot(treatment.data[idx6,], aes(CD4CHANGE, RNACHANGE)) + geom_point() + geom_smooth() + labs(title = "Region 6")
p7 = ggplot(treatment.data[idx7,], aes(CD4CHANGE, RNACHANGE)) + geom_point() + geom_smooth() + labs(title = "Region 7")
p8 = ggplot(treatment.data[idx8,], aes(CD4CHANGE, RNACHANGE)) + geom_point() + geom_smooth()+ labs(title = "Region 8")
p9 = ggplot(treatment.data[idx9,], aes(CD4CHANGE, RNACHANGE)) + geom_point() + geom_smooth() + labs(title = "Region 9")

library(gridExtra)
grid.arrange(p1, p2, p3,p4, p5, p6,p7, p8, p9,ncol = 3 )


### Assumption (C2): P(s1 > s | w) >= P(s0 > s | w) ###

p1 = ggplot(aids.data[idx1,], aes(x = CD4CHANGE, color = factor(TREAT))) + stat_ecdf(geom = "step", aes(y = 1 - ..y..), size = 1)  + theme(legend.position = "none") +labs(y = "1 - Empirical CDF", title = "Region 1" ) 
p2 = ggplot(aids.data[idx2,], aes(x = CD4CHANGE, color = factor(TREAT))) + stat_ecdf(geom = "step", aes(y = 1 - ..y..), size = 1)  + theme(legend.position = "none") +labs(y = "1 - Empirical CDF", title = "Region 2" ) 
p3 = ggplot(aids.data[idx3,], aes(x = CD4CHANGE, color = factor(TREAT))) + stat_ecdf(geom = "step", aes(y = 1 - ..y..), size = 1)  + theme(legend.position = "none") +labs(y = "1 - Empirical CDF", title = "Region 3" ) 
p4 = ggplot(aids.data[idx4,], aes(x = CD4CHANGE, color = factor(TREAT))) + stat_ecdf(geom = "step", aes(y = 1 - ..y..), size = 1)  + theme(legend.position = "none") +labs(y = "1 - Empirical CDF", title = "Region 4" ) 
p5 = ggplot(aids.data[idx5,], aes(x = CD4CHANGE, color = factor(TREAT))) + stat_ecdf(geom = "step", aes(y = 1 - ..y..), size = 1)  + theme(legend.position = "none") +labs(y = "1 - Empirical CDF", title = "Region 5" ) 
p6 = ggplot(aids.data[idx6,], aes(x = CD4CHANGE, color = factor(TREAT))) + stat_ecdf(geom = "step", aes(y = 1 - ..y..), size = 1)  + theme(legend.position = "none") +labs(y = "1 - Empirical CDF", title = "Region 6" ) 
p7 = ggplot(aids.data[idx7,], aes(x = CD4CHANGE, color = factor(TREAT))) + stat_ecdf(geom = "step", aes(y = 1 - ..y..), size = 1)  + theme(legend.position = "none") +labs(y = "1 - Empirical CDF", title = "Region 7" ) 
p8 = ggplot(aids.data[idx8,], aes(x = CD4CHANGE, color = factor(TREAT))) + stat_ecdf(geom = "step", aes(y = 1 - ..y..), size = 1)  + theme(legend.position = "none") +labs(y = "1 - Empirical CDF", title = "Region 8" ) 
p9 = ggplot(aids.data[idx9,], aes(x = CD4CHANGE, color = factor(TREAT))) + stat_ecdf(geom = "step", aes(y = 1 - ..y..), size = 1)  + theme(legend.position = "none") +labs(y = "1 - Empirical CDF", title = "Region 9" ) 
grid.arrange(p1, p2, p3,p4, p5, p6,p7, p8, p9,ncol = 3 )


### Assumption (C3): E(y1 | s, w) >= E(y0 | s, w) ###

# overall plot
ggplot(aids.data, aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point()

# plots for subgroups -- these all look reasonable
p1 = ggplot(aids.data[idx1,], aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point() + labs(title = "Region 1") + theme(legend.position = "none")
p2 =ggplot(aids.data[idx2,], aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point() + labs(title = "Region 2") + theme(legend.position = "none")
p3 =ggplot(aids.data[idx3,], aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point() + labs(title = "Region 3") + theme(legend.position = "none")
p4 =ggplot(aids.data[idx4,], aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point() + labs(title = "Region 4") + theme(legend.position = "none")
p5 =ggplot(aids.data[idx5,], aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point() + labs(title = "Region 5") + theme(legend.position = "none")
p6 =ggplot(aids.data[idx6,], aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point() + labs(title = "Region 6") + theme(legend.position = "none")
p7 =ggplot(aids.data[idx7,], aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point() + labs(title = "Region 7") + theme(legend.position = "none")
p8 =ggplot(aids.data[idx8,], aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point() + labs(title = "Region 8") + theme(legend.position = "none")
p9 =ggplot(aids.data[idx9,], aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point() + labs(title = "Region 9") + theme(legend.position = "none")

grid.arrange(p1, p2, p3,p4, p5, p6,p7, p8, p9,ncol = 3 )

### Assumption (C4): s0 and s1 have the same support over a finite range ###

range(treatment.data$CD4CHANGE)
range(control.data$CD4CHANGE) # the control data goes more negative than the treatment data does, but not too much

hist(treatment.data$CD4CHANGE)
hist(control.data$CD4CHANGE)
