
setwd("C:/Users/rkkno/Documents/University of Texas at Austin/Complex heterogeneity/Simulation files 4-24-24/320 data")
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
# Notes: C2 and C3 seem OK
# C1 is concerning in some spots... discuss more
# C4 not met, is this problematic?
# C5: not testable
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

w1.breaks <- quantile(treatment.data$CD4BASE, c(0.25,0.5,0.75))
w2.breaks <- quantile(treatment.data$AGE, c(0.25,0.5,0.75))

# want to save idx that define subgroups of patients who are between these thresholds
idx1 <- which((treatment.data$CD4BASE < w1.breaks[1]) & (treatment.data$AGE < w2.breaks[1]))
idx2 <- which((treatment.data$CD4BASE < w1.breaks[1]) & (treatment.data$AGE >= w2.breaks[1]) & (treatment.data$AGE < w2.breaks[2]))
idx3 <- which((treatment.data$CD4BASE < w1.breaks[1]) & (treatment.data$AGE >= w2.breaks[2]) & (treatment.data$AGE < w2.breaks[3]))
idx4 <- which((treatment.data$CD4BASE < w1.breaks[1]) & (treatment.data$AGE >= w2.breaks[3]))
idx5 <- which((treatment.data$CD4BASE >= w1.breaks[1]) & (treatment.data$CD4BASE < w1.breaks[2]) & (treatment.data$AGE < w2.breaks[1]))
idx6 <- which((treatment.data$CD4BASE >= w1.breaks[1]) & (treatment.data$CD4BASE < w1.breaks[2]) & (treatment.data$AGE >= w2.breaks[1]) & (treatment.data$AGE < w2.breaks[2]))
idx7 <- which((treatment.data$CD4BASE >= w1.breaks[1]) & (treatment.data$CD4BASE < w1.breaks[2]) & (treatment.data$AGE >= w2.breaks[2]) & (treatment.data$AGE < w2.breaks[3]))
idx8 <- which((treatment.data$CD4BASE >= w1.breaks[1]) & (treatment.data$CD4BASE < w1.breaks[2]) & (treatment.data$AGE >= w2.breaks[3]))
idx9 <- which((treatment.data$CD4BASE >= w1.breaks[2]) & (treatment.data$CD4BASE < w1.breaks[3]) & (treatment.data$AGE < w2.breaks[1]))
idx10 <- which((treatment.data$CD4BASE >= w1.breaks[2]) & (treatment.data$CD4BASE < w1.breaks[3]) & (treatment.data$AGE >= w2.breaks[1]) & (treatment.data$AGE < w2.breaks[2]))
idx11 <- which((treatment.data$CD4BASE >= w1.breaks[2]) & (treatment.data$CD4BASE < w1.breaks[3]) & (treatment.data$AGE >= w2.breaks[2]) & (treatment.data$AGE < w2.breaks[3]))
idx12 <- which((treatment.data$CD4BASE >= w1.breaks[2]) & (treatment.data$CD4BASE < w1.breaks[3]) & (treatment.data$AGE >= w2.breaks[3]))
idx13 <- which((treatment.data$CD4BASE >= w1.breaks[3]) & (treatment.data$AGE < w2.breaks[1]))
idx14 <- which((treatment.data$CD4BASE >= w1.breaks[3]) & (treatment.data$AGE >= w2.breaks[1]) & (treatment.data$AGE < w2.breaks[2]))
idx15 <- which((treatment.data$CD4BASE >= w1.breaks[3]) & (treatment.data$AGE >= w2.breaks[2]) & (treatment.data$AGE < w2.breaks[3]))
idx16 <- which((treatment.data$CD4BASE >= w1.breaks[3]) & (treatment.data$AGE >= w2.breaks[3]))

length(idx1)
length(idx2)
length(idx3)
length(idx4)
length(idx5)
length(idx6)
length(idx7)
length(idx8)
length(idx9)
length(idx10)
length(idx11)
length(idx12)
length(idx13)
length(idx14)
length(idx15)
length(idx16)

# subgroup plots
ggplot(treatment.data[idx1,], aes(CD4CHANGE, RNACHANGE)) + geom_point() + geom_smooth() # not ok
ggplot(treatment.data[idx2,], aes(CD4CHANGE, RNACHANGE)) + geom_point() + geom_smooth()
ggplot(treatment.data[idx3,], aes(CD4CHANGE, RNACHANGE)) + geom_point() + geom_smooth()
ggplot(treatment.data[idx4,], aes(CD4CHANGE, RNACHANGE)) + geom_point() + geom_smooth()
ggplot(treatment.data[idx5,], aes(CD4CHANGE, RNACHANGE)) + geom_point() + geom_smooth() # not ok
ggplot(treatment.data[idx6,], aes(CD4CHANGE, RNACHANGE)) + geom_point() + geom_smooth()
ggplot(treatment.data[idx7,], aes(CD4CHANGE, RNACHANGE)) + geom_point() + geom_smooth()
ggplot(treatment.data[idx8,], aes(CD4CHANGE, RNACHANGE)) + geom_point() + geom_smooth()
ggplot(treatment.data[idx9,], aes(CD4CHANGE, RNACHANGE)) + geom_point() + geom_smooth() # not ok
ggplot(treatment.data[idx10,], aes(CD4CHANGE, RNACHANGE)) + geom_point() + geom_smooth()
ggplot(treatment.data[idx11,], aes(CD4CHANGE, RNACHANGE)) + geom_point() + geom_smooth()
ggplot(treatment.data[idx12,], aes(CD4CHANGE, RNACHANGE)) + geom_point() + geom_smooth()
ggplot(treatment.data[idx13,], aes(CD4CHANGE, RNACHANGE)) + geom_point() + geom_smooth()
ggplot(treatment.data[idx14,], aes(CD4CHANGE, RNACHANGE)) + geom_point() + geom_smooth()
ggplot(treatment.data[idx15,], aes(CD4CHANGE, RNACHANGE)) + geom_point() + geom_smooth() # not ok
ggplot(treatment.data[idx16,], aes(CD4CHANGE, RNACHANGE)) + geom_point() + geom_smooth() # not ok

### Assumption (C2): P(s1 > s | w) >= P(s0 > s | w) ###

# overall plot (plot S against W1 = CD4BASE)
ggplot(aids.data, aes(CD4BASE, CD4CHANGE, color = factor(TREAT))) + geom_smooth() + geom_point(alpha= 0.3)
ggplot(aids.data, aes(CD4BASE, CD4CHANGE, color = factor(TREAT))) + geom_smooth()
# even looking overall, this seems pretty much met except the far right

#split into different w2 subgroups
w2.breaks <- quantile(aids.data$AGE, c(0.25,0.5,0.75))

idx1 <- which(aids.data$AGE < w2.breaks[1])
idx2 <- which((aids.data$AGE >= w2.breaks[1]) & (aids.data$AGE < w2.breaks[2]))
idx3 <- which((aids.data$AGE >= w2.breaks[2]) & (aids.data$AGE < w2.breaks[3]))
idx4 <- which(aids.data$AGE >= w2.breaks[3])

# plots for subgroups -- these are actually less clear, but generally condition still seems met
ggplot(aids.data[idx1,], aes(CD4BASE, CD4CHANGE, color = factor(TREAT))) + geom_smooth() + geom_point()
ggplot(aids.data[idx2,], aes(CD4BASE, CD4CHANGE, color = factor(TREAT))) + geom_smooth() + geom_point()
ggplot(aids.data[idx3,], aes(CD4BASE, CD4CHANGE, color = factor(TREAT))) + geom_smooth() + geom_point()
ggplot(aids.data[idx4,], aes(CD4BASE, CD4CHANGE, color = factor(TREAT))) + geom_smooth() + geom_point()


### Assumption (C3): E(y1 | s, w) >= E(y0 | s, w) ###

# overall plot
ggplot(aids.data, aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point()

# split it into different covariate subgroups
w1.breaks <- quantile(aids.data$CD4BASE, c(0.25,0.5,0.75))
w2.breaks <- quantile(aids.data$AGE, c(0.25,0.5,0.75))

# want to save idx that define subgroups of patients who are between these thresholds
idx1 <- which((aids.data$CD4BASE < w1.breaks[1]) & (aids.data$AGE < w2.breaks[1]))
idx2 <- which((aids.data$CD4BASE < w1.breaks[1]) & (aids.data$AGE >= w2.breaks[1]) & (aids.data$AGE < w2.breaks[2]))
idx3 <- which((aids.data$CD4BASE < w1.breaks[1]) & (aids.data$AGE >= w2.breaks[2]) & (aids.data$AGE < w2.breaks[3]))
idx4 <- which((aids.data$CD4BASE < w1.breaks[1]) & (aids.data$AGE >= w2.breaks[3]))
idx5 <- which((aids.data$CD4BASE >= w1.breaks[1]) & (aids.data$CD4BASE < w1.breaks[2]) & (aids.data$AGE < w2.breaks[1]))
idx6 <- which((aids.data$CD4BASE >= w1.breaks[1]) & (aids.data$CD4BASE < w1.breaks[2]) & (aids.data$AGE >= w2.breaks[1]) & (aids.data$AGE < w2.breaks[2]))
idx7 <- which((aids.data$CD4BASE >= w1.breaks[1]) & (aids.data$CD4BASE < w1.breaks[2]) & (aids.data$AGE >= w2.breaks[2]) & (aids.data$AGE < w2.breaks[3]))
idx8 <- which((aids.data$CD4BASE >= w1.breaks[1]) & (aids.data$CD4BASE < w1.breaks[2]) & (aids.data$AGE >= w2.breaks[3]))
idx9 <- which((aids.data$CD4BASE >= w1.breaks[2]) & (aids.data$CD4BASE < w1.breaks[3]) & (aids.data$AGE < w2.breaks[1]))
idx10 <- which((aids.data$CD4BASE >= w1.breaks[2]) & (aids.data$CD4BASE < w1.breaks[3]) & (aids.data$AGE >= w2.breaks[1]) & (aids.data$AGE < w2.breaks[2]))
idx11 <- which((aids.data$CD4BASE >= w1.breaks[2]) & (aids.data$CD4BASE < w1.breaks[3]) & (aids.data$AGE >= w2.breaks[2]) & (aids.data$AGE < w2.breaks[3]))
idx12 <- which((aids.data$CD4BASE >= w1.breaks[2]) & (aids.data$CD4BASE < w1.breaks[3]) & (aids.data$AGE >= w2.breaks[3]))
idx13 <- which((aids.data$CD4BASE >= w1.breaks[3]) & (aids.data$AGE < w2.breaks[1]))
idx14 <- which((aids.data$CD4BASE >= w1.breaks[3]) & (aids.data$AGE >= w2.breaks[1]) & (aids.data$AGE < w2.breaks[2]))
idx15 <- which((aids.data$CD4BASE >= w1.breaks[3]) & (aids.data$AGE >= w2.breaks[2]) & (aids.data$AGE < w2.breaks[3]))
idx16 <- which((aids.data$CD4BASE >= w1.breaks[3]) & (aids.data$AGE >= w2.breaks[3]))

# plots for subgroups -- these all look reasonable
ggplot(aids.data[idx1,], aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point()
ggplot(aids.data[idx2,], aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point()
ggplot(aids.data[idx3,], aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point()
ggplot(aids.data[idx4,], aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point()
ggplot(aids.data[idx5,], aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point()
ggplot(aids.data[idx6,], aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point()
ggplot(aids.data[idx7,], aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point()
ggplot(aids.data[idx8,], aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point()
ggplot(aids.data[idx9,], aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point()
ggplot(aids.data[idx10,], aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point()
ggplot(aids.data[idx11,], aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point()
ggplot(aids.data[idx12,], aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point()
ggplot(aids.data[idx13,], aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point()
ggplot(aids.data[idx14,], aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point()
ggplot(aids.data[idx15,], aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point()
ggplot(aids.data[idx16,], aes(CD4CHANGE, RNACHANGE, color = factor(TREAT))) + geom_smooth() + geom_point()

### Assumption (C4): s0 and s1 have the same support over a finite range ###

range(treatment.data$CD4CHANGE)
range(control.data$CD4CHANGE) # the control data goes more negative than the treatment data does

hist(treatment.data$CD4CHANGE)
hist(control.data$CD4CHANGE)
