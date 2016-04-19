###http://statistical-research.com/plotting-likert-scales/?utm_source=rss&utm_medium=rss&utm_campaign=plotting-likert-scales
set.seed(1234)
library(e1071)
probs <- cbind(c(.4,.2/3,.2/3,.2/3,.4),c(.1/4,.1/4,.9,.1/4,.1/4),c(.2,.2,.2,.2,.2))
my.n <- 100
my.len <- ncol(probs)*my.n
raw <- matrix(NA,nrow=my.len,ncol=2)
raw <- NULL
for(i in 1:ncol(probs)){
  raw <- rbind(raw, cbind(i,rdiscrete(my.n,probs=probs[,i],values=1:5)))
}

r <- data.frame( cbind(
  as.numeric( row.names( tapply(raw[,2], raw[,1], mean) ) ),
  tapply(raw[,2], raw[,1], mean),
  tapply(raw[,2], raw[,1], mean) + sqrt( tapply(raw[,2], raw[,1], var)/tapply(raw[,2], raw[,1], length) ) * qnorm(1-.05/2,0,1),
  tapply(raw[,2], raw[,1], mean) - sqrt( tapply(raw[,2], raw[,1], var)/tapply(raw[,2], raw[,1], length) ) * qnorm(1-.05/2,0,1)
))
names(r) <- c("group","mean","ll","ul")

gbar <- tapply(raw[,2], list(raw[,2], raw[,1]), length)

sgbar <- data.frame( cbind(c(1:max(unique(raw[,1]))),t(gbar)) )

sgbar.likert<- sgbar[,2:6]


require(grid)
require(lattice)
require(latticeExtra)
require(HH)
sgbar.likert<- sgbar[,2:6]
likert(sgbar.likert,
       main='Example Diverging Stacked Bar Chart for Likert Scale',
       sub="Likert Scale")

likert(sgbar.likert, horizontal=FALSE,
       aspect=1.5,
       main="Example Diverging Stacked Bar Chart for Likert Scale",
       auto.key=list(space="right", columns=1,
                     reverse=TRUE, padding.text=2),
       sub="Likert Scale")

likert(sgbar.likert,
       auto.key=list(between=1, between.columns=2),
       xlab="Percentage",
       main="Example Diverging Stacked Bar Chart for Likert Scale",
       BrewerPaletteName="Blues",
       sub="Likert Scale")