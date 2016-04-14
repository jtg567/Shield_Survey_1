# clear the variable workspace
rm(list=ls())

# run preprocessing.R up to "descriptives"

# how many users completed the study
N.total <- nrow(df); cat(N.total,"users completed the study")
cat(100*nrow(df[df$`URL Variable: reason`== 'end-of-study',])/N.total,"% of users ran the study to conclusion", sep="")
cat(100*nrow(df[df$`URL Variable: reason`== 'user-ended-study',])/N.total,"% of users ended the study themselves", sep="")

# of those who quit, why?
df[df$`URL Variable: reason`=='user-ended-study',26:30]
df[df$`URL Variable: reason`=='user-ended-study',38:49]

# geographic breakdown (country) (there is count of 1 under 'Europe'; I guess this is self-populated?)
cat("Respondents came from a total of",length(unique(df$country)),"countries. Following are tables of counts and proportions by country:")
sort(table(df$country), decreasing=T)
sort(table(df$country)/N.total, decreasing=T)

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

