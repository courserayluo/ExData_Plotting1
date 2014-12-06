# Combined plots of active power, voltage, sub-metering & reactive power
# Sample usage: source('plot4.R'); plot4('household_power_consumption.txt');
plot4 <- function (filename) {
	col_classes <- sapply(read.table(filename, header=TRUE, sep=";", nrows=1), class);
	power_matrix <- read.table(filename, header=TRUE, sep=";", colClasses=col_classes, na.strings = "?");
	# get subset of rows from 1/2/2007 to 2/2/2007
	rows1 <- which(power_matrix[,1]=="1/2/2007");
	rows2 <- which(power_matrix[,1]=="2/2/2007");
	sub <- c(rows1, rows2);
	sub_matrix <- power_matrix[sub,];
	# get time axis
	time <- strptime(paste(sub_matrix$Date, sub_matrix$Time), "%d/%m/%Y %H:%M:%S");	
	# plot 4 graphs
	png(file = "plot4.png", width=480, height=480);
	par(mfrow=c(2,2));
	# active power subplot
	plot(time, sub_matrix$Global_active_power, type='l', main='', ylab='Global Active Power (kilowatts)', xlab='');
	# voltage subplot
	plot(time, sub_matrix$Voltage, type='l', main='', ylab='Voltage', xlab='datetime');
	# sub-metering subplot
	all_meters <- c(sub_matrix$Sub_metering_1, sub_matrix$Sub_metering_2, sub_matrix$Sub_metering_3);
	yrange <- range(all_meters);
	plot(time, sub_matrix$Sub_metering_1, type='l', main='', ylab='Energy sub metering', xlab='', col="black", ylim=yrange);
	lines(time, sub_matrix$Sub_metering_2, col="red");
	lines(time, sub_matrix$Sub_metering_3, col="blue");
	legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col=c("black","red","blue"), lty=c(1,1,1));
	# reactive power subplot
	plot(time, sub_matrix$Global_reactive_power, type='l', main='', ylab='Global_reactive_power', xlab='datetime');
	dev.off();
}	