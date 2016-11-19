df$timestamp<-as.POSIXct(df$timestamp,format="%m/%d/%y %H:%M")

df1.zoo<-zoo(df[,-1],df[,1]) #set date to Index

df2 <- merge(df1.zoo,zoo(,seq(start(df1.zoo),end(df1.zoo),by="min")), all=TRUE)


# Read a comma-delimited file that has the following content
# time,observations
# 2011/11/01,12
# 2012/01/01,320
# 2011/12/01,100
# 2012/06/01,7
raw.data <- read.delim("timefill.csv", header=T, sep=",")

# Convert the time column to a date column.
# Accessing a column is done by using the '$' sign
# like so: raw.data$time.
raw.data$time <- as.Date(raw.data$time)

# sort the data by time. The [*,] selects all rows that
# match the specified condition - in this case an order function
# applied to the time column.
sorted.data <- raw.data[order(raw.data$time),]

# Find the length of the dataset
data.length <- length(sorted.data$time)

# Find min and max. Because the data is sorted, this will be
# the first and last element.
time.min <- sorted.data$time[1]
time.max <- sorted.data$time[data.length]

# generate a time sequence with 1 month intervals to fill in
# missing dates
all.dates <- seq(time.min, time.max, by="month")

# Convert all dates to a data frame. Note that we're putting
# the new dates into a column called "time" just like the
# original column. This will allow us to merge the data.
all.dates.frame <- data.frame(list(time=all.dates))

# Merge the two datasets: the full dates and original data
merged.data <- merge(all.dates.frame, sorted.data, all=T)

# The above merge set the new observations to NA.
# To replace those with a 0, we must first find all the rows
# and then assign 0 to them.
merged.data$observations[which(is.na(merged.data$observations))] <- 0