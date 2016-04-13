rm(list=ls()) #clear the variable workspace

library(readr)
library(plyr)
library(Rmisc)
library(ggplot2)
library(lubridate)
library(scales)

# Treatment levels
# Client-seconds of wait time for nglayout to render
# 50 (aggressive)
# 250 (medium)
# 500 (ut)
# 1000 (weak)

#url = 'URL FOR CSV EXPORT GOES HERE'
df = read_csv(url)

df = plyr::rename(df, c(
  "Date Submitted" = 'date',
  "Country" = 'country',
  "State/Region" = 'state',
  #"URL Variable: action",
  #"URL Variable: controller",
  #"URL Variable: id",
  #"URL Variable: link_id",
  #"URL Variable: module",
  #"URL Variable: reason",
  #"URL Variable: test",
  "URL Variable: variation" = 'treatment',
  #"URL Variable: who"
  #"URL Variable: xname"
  "I don't want to share my data:Please tell us why you chose to opt out of the study. Check all that apply." = 'optoutwhy_sharedata',
  "Firefox became slow:Please tell us why you chose to opt out of the study. Check all that apply." = 'optoutwhy_becameslow',
  "Firefox started to crash:Please tell us why you chose to opt out of the study. Check all that apply."= 'optoutwhy_crashed',
  "The study interrupted me too frequently:Please tell us why you chose to opt out of the study. Check all that apply." = 'optoutwhy_interrupt',
  "Websites I frequently visit didn't work as expected:Please tell us why you chose to opt out of the study. Check all that apply." = 'optoutwhy_badfaves',
  #"Other:Please tell us why you chose to opt out of the study. Check all that apply." = 'optoutwhy_other1',
  #"Other:Please tell us why you chose to opt out of the study. Check all that apply." = 'optoutwhy_other2',
  "Variation" = 'treatment2',
  "Reason Study Ended" = 'reason_study_ended',
  "Do you use any other browsers besides Firefox on a regular basis?" = 'otherbrowsers',
  "How fast would you rate your other browser on the scale below?" = 'speed_rating',
  "Thinking back over the last week of testing, how fast would you rate Firefox on the scale below?" = 'lastweek_speed_rating',
  "Startup Time:Thinking back over the last week of using Firefox, please tell us about your experience with:" = 'exp_startuptime',
  "Page Load Time:Thinking back over the last week of using Firefox, please tell us about your experience with:" = 'exp_pageload',
  "Scrolling:Thinking back over the last week of using Firefox, please tell us about your experience with:" = 'exp_scrolling',
  "Crashing/Hanging:Thinking back over the last week of using Firefox, please tell us about your experience with:" = 'exp_crashing',
  "Speed of opening a New Tab:Thinking back over the last week of using Firefox, please tell us about your experience with:" = 'exp_newtabspeed',
  "Separate NA" = 'exp_separate', # XXX Check on this, it might be wrong
  "How likely or unlikely are you to use Firefox again tomorrow?" = 'use_fx_tomorrow',
  "Firefox crashes a lot:Why are you unlikely to use Firefox in the next few days?" = 'nouse_crashes',
  "Firefox seems slow:Why are you unlikely to use Firefox in the next few days?" = 'nouse_slow',
  "Firefox doesn't work with my websites:Why are you unlikely to use Firefox in the next few days?" = 'nouse_faves',
  "I'm not a regular Firefox user:Why are you unlikely to use Firefox in the next few days?" = 'nouse_notreg',
  "Another browser seems better:Why are you unlikely to use Firefox in the next few days?" = 'nouse_otherbrowser',
  #"Other -:Why are you unlikely to use Firefox in the next few days?" = 'nouse_other1',
  #"Other -:Why are you unlikely to use Firefox in the next few days?" = 'nouse_other2',
  "Firefox helps me feel in control of my online experience."  = 'control_exp',
  "Firefox allows me to complete tasks on the Internet in a way that works as I expect." = 'complete_tasks',
  "Thinking about the last few times you used Firefox, did it get better, get worse, or stay about the same?" = 'overall_quality',
  #"kbps",
  "Speed test"  = 'speedtest',
  "How likely or unlikely would you be to participate in another test to help improve Firefox?" = 'future_participation',
  "Please tell us why you would no longer be interested in helping to test Firefox" = 'future_attrition'
))

# check types (paste to excel)
#write.table(lapply(df, class), 'clipboard', sep="\t")

# recode selected character variables to factor
for(i in c(21, 23, 26:30, 33:35, 38:49, 52:54, 56:58)) {
  df[,i] <- factor(df[,i])
}

df$treatment = relevel(df$treatment, ref='ut')
df$treatment = plyr::mapvalues(df$treatment, from = c('ut','aggressive','medium','weak'), to = c('Control (250)', 'Aggressive (50)', 'Medium (500)', 'Weak (1000)'))
df = subset(df, treatment != '')

# some descriptives

# response sums by day of week
df$date = lubridate::ymd_hms(df$date)
df$wday = lubridate::wday(df$date, label=TRUE, abbr=TRUE)
df$day = lubridate::floor_date(df$date, 'day')
qplot(data=df, x=as.Date(day), geom="density", facets=~treatment) + theme_bw() + scale_x_date(date_breaks='1 day', date_labels='%b %d') + coord_flip()

#filter out from invalid dates
df = df[df$day %within% lubridate::interval(lubridate::mdy('04042016'), lubridate::mdy('04112016')),]

# let's look at simple differences in responses by treatment

# preferences
# XXX todo

# get sd of proportions of responses to each
get_results = function(df, response) {
  data = ddply(df, .(treatment), function(x) {
    props = prop.table(table(x[,response]))
    n = length(x$treatment)
    se = apply(props, 1, function(p) {sqrt(p*(1-p)/n)} )
    CI_95 = qnorm(.975)*se
    names(se) = names(props)
    return(data.frame(props, n, se, CI_95))})
  data$variable = relevel(startuptimes$Var1, ref="I don't know/Unsure")
  return(data)
}

plot_results = function(results) {
  ggplot(results, aes(x=treatment, y=Freq)) +
    geom_bar(stat='identity') + 
    geom_errorbar(aes(ymax=Freq+CI_95, ymin=Freq-CI_95)) +
    facet_wrap(~variable, ncol=1) +
    coord_flip() +
    theme_bw()
}

#exp_* variables
result = get_results(df, 'exp_startuptime') #interesting
plot_results(result)
result = get_results(df, 'exp_scrolling')
plot_results(result)
result = get_results(df, 'exp_crashing')
plot_results(result)
result = get_results(df, 'exp_pageload') # close to interesting
plot_results(result)
result = get_results(df, 'exp_newtabspeed') # interesting
plot_results(result)

# 100pt scale checks
# XXX we probably need to categorize this variable and look at proportional responses

# Graph of group mean differences
result = Rmisc::summarySE(data=df, measurevar='lastweek_speed_rating', groupvars=c('treatment'), na.rm=T, conf.interval = 0.95)
ggplot(result, aes(x=treatment, y=lastweek_speed_rating)) + 
#  geom_bar(stat="identity") + 
  geom_pointrange(aes(ymin=lastweek_speed_rating-ci, ymax=lastweek_speed_rating+ci)) +
  theme_bw() +
  scale_y_continuous(limits = c(50, 80)) +
  xlab('Treatment conditions') +
  ylab('Reported "fastness" of browser on 100pt scale (higher is better)') +
  coord_flip()

# simple model fits

#fit = lm(data=df, lastweek_speed_rating ~ treatment + log(kbps))
fit = lm(data=df, lastweek_speed_rating ~ treatment)
summary(fit) # nothing is significant against control...might want to try against another level
# p values are not great, r^2 is hilariously small

# looking at average differences between treatments and 
# differences between one browser and firefox
df$diff = df$speed_rating - df$lastweek_speed_rating
ddply(df, .(treatment), summarise, 
      mean=mean(diff, na.rm=T), 
      sd=sd(diff, na.rm=T), 
      n=sum(!is.na(diff)))
# huge sd, really small n

