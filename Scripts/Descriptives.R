# 2nd iteration - drop the WEAK responders from first run
df <- df[df$treatment != 'Weak (1000)',]


# response sums by day of week
df$date = lubridate::ymd_hms(df$date)
df$wday = lubridate::wday(df$date, label=TRUE, abbr=TRUE)
df$day = lubridate::floor_date(df$date, 'day')
qplot(data=df, x=as.Date(day), geom="density", facets=~treatment) + theme_bw() + scale_x_date(date_breaks='1 day', date_labels='%b %d') + coord_flip()

# PARAMETERIZE DATE INPUTS HERE
#filter out from invalid dates
df = df[df$day %within% lubridate::interval(lubridate::mdy('04042016'), lubridate::mdy('05042016')),]

# how many users completed the study
N.total <- nrow(df); cat(N.total,"users completed the study")
cat(100*nrow(df[df$`URL Variable: reason`== 'end-of-study',])/N.total,"% of users ran the study to conclusion", sep="")
cat(100*nrow(df[df$`URL Variable: reason`== 'user-ended-study',])/N.total,"% of users ended the study themselves", sep="")

# of those who quit, why?
# generally we need to count from char type questions of this nature
sapply(df[df$`URL Variable: reason`=='user-ended-study',26:30], function(x) {sum(x!='')})

# geographic breakdown (country) (there is count of 1 under 'Europe'; I guess this is self-populated?)
cat("Respondents came from a total of",length(unique(df$country)),"countries. Following are tables of counts and proportions by country:")
sort(table(df$country), decreasing=T)
sort(table(df$country)/N.total, decreasing=T)
# did the write.table(x, 'clipboard') trick to get into excel and make pretty

# predominantly US, what's the breakdown by state? (one is blank? looks like WY, )
cat("US respondents came from a total of",length(unique(df[df$country=='United States',]$state)),"states. Following are tables of counts and proportions by US state:")
sort(table(df[df$country=='United States',]$state), decreasing=T)
sort(table(df[df$country=='United States',]$state)/nrow(df[df$country=='United States',]), decreasing=T)

# nearly 6% of the entire sample are Californians
cat("Cali respondents came from a total of",length(unique(df[df$state=='CA',]$City)),"cities. Following are tables of counts and proportions by US state:")
sort(table(df[df$state=='CA',]$City), decreasing=T)
sort(table(df[df$state=='CA',]$City)/nrow(df[df$state=='CA',]), decreasing=T)

# if we wanted to join these data back to other Telemetry, how many ppl could we do that for?
cat(nrow(df[df$`URL Variable: who`!='unknown',]),"out of",N.total,"users have clientId available.")

# when users ended study, what did they say about the experience?
df[df$`URL Variable: reason`=='user-ended-study',38:49]

# how about the other users, what did they say about the experience?
df[df$`URL Variable: reason`=='end-of-study',38:49]




# let's look at simple differences in responses by treatment

# preferences
# XXX todo

# get sd of proportions of responses to each
get_results = function(df, response) {
  data = ddply(df, .(treatment), function(x) {
    props = prop.table(table(x[,response]))
    n = length(x$treatment)
    se = apply(props, 1, function(p) {sqrt(p*(1-p)/n)} )
    CI = qnorm(.90)*se
    names(se) = names(props)
    return(data.frame(props, n, se, CI))})
#  data$variable = relevel(data$Var1, ref=NA)
  return(data)
}

# fixing facet labels (from http://stackoverflow.com/questions/3472980/ggplot-how-to-change-facet-labels)
response_labeller <- function(variable,value){
  return(list('-1'="Is Worse", '0'="Is Same", '1'="Is Better")[value])
}

plot_results = function(results, title) {
  ggplot(results, aes(x=treatment, y=Freq)) +
    geom_bar(stat='identity') + 
    geom_errorbar(aes(ymax=Freq+CI, ymin=Freq-CI)) +
    facet_wrap(~Var1, ncol=1, labeller=response_labeller) +
    coord_flip() +
    theme_bw() +
    ggtitle(title)
}

#exp_* variables
result = get_results(df, 'exp_startuptime') #interesting
plot_results(result, 'exp_startuptime')
result = get_results(df, 'exp_scrolling')
plot_results(result, 'exp_scrolling')
result = get_results(df, 'exp_crashing')
plot_results(result, 'exp_crashing')
result = get_results(df, 'exp_pageload') # close to interesting
plot_results(result, 'exp_pageload')
result = get_results(df, 'exp_newtabspeed') # interesting
plot_results(result, 'exp_newtabspeed')


# added these 4/26 following mgrimes question on perf survey 1
result = get_results(df, 'overall_quality')
plot_results(result, 'overall_quality')

response_labeller <- function(variable,value){
  return(list('-2'="Is Very Unlikely", '-1'="Is Unlikely", '0'="Is Same", '1'="Is Likely", '2'="Is Very Likely")[value])
}

result = get_results(df, 'future_participation')
plot_results(result, 'future_participation')


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



