# manually load CSV exported from Spark
d.in <- read.csv("C:\\Users\\Josh Gaunt\\Documents\\Projects\\Shield\\heartbeat.csv")

# merge Telemetry data to SG data after running Preprocessing.R first
merged <- merge(df, d.in, by="clientId")

# how many in each channel?
table(merged$channel)