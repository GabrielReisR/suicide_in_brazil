# Time lapse function
data <- ts(data.frame(x1=c(1:10), x2=c(11:20), x3=c(21:30)), start = c(2010,3), frequency = 4)
data
data <- data/stats::lag(data,-1) - 1
data
