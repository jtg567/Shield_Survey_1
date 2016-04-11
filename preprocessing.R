library(readr)
library(plyr)
library(Rmisc)
library(ggplot2)
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
#summary(df)
