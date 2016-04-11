### 4/11 rweiss quick results
df = df[df$treatment != '',]
result = Rmisc::summarySE(data=df, measurevar='lastweek_speed_rating', groupvars=c('treatment'), na.rm=T)
ggplot(result, aes(x=treatment, y=lastweek_speed_rating)) + geom_bar(stat="identity") + geom_errorbar(aes(ymin=lastweek_speed_rating-ci, ymax=lastweek_speed_rating+ci)) 
fit = lm(data=df, lastweek_speed_rating ~ treatment + log(kbps))
summary(fit)

df$diff = df$speed_rating - df$lastweek_speed_rating
ddply(df, .(treatment), summarise, mean(diff, na.rm=T))