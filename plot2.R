# Plot global active power vs time given filename for data
# Sample usage: source('plot2.R'); plot2('household_power_consumption.txt');
plot2 <- function (filename) {
	col_classes <- sapply(read.table(filename, header=TRUE, sep=";", nrows=1), class);
	power_matrix <- read.table(filename, header=TRUE, sep=";", colClasses=col_classes, na.strings = "?");
	# get subset of rows from 1/2/2007 to 2/2/2007
	rows1 <- which(power_matrix[,1]=="1/2/2007");
	rows2 <- which(power_matrix[,1]=="2/2/2007");
	sub <- c(rows1, rows2);
	sub_matrix <- power_matrix[sub,];
	# get time axis
	time <- strptime(paste(sub_matrix$Date, sub_matrix$Time), "%d/%m/%Y %H:%M:%S");	
	# Plot power vs time
	png(file = "plot2.png", width=480, height=480);
	plot(time, sub_matrix$Global_active_power, type='l', main='', ylab='Global Active Power (kilowatts)', xlab='');
	dev.off();
}