# Plot histogram of global active power given filename for data
# Sample usage: source('plot1.R'); plot1('household_power_consumption.txt');
plot1 <- function (filename) {
	col_classes <- sapply(read.table(filename, header=TRUE, sep=";", nrows=1), class);
	power_matrix <- read.table(filename, header=TRUE, sep=";", colClasses=col_classes, na.strings = "?");
	# get subset of rows from 1/2/2007 to 2/2/2007
	rows1 <- which(power_matrix[,1]=="1/2/2007");
	rows2 <- which(power_matrix[,1]=="2/2/2007");
	sub <- c(rows1, rows2);
	sub_matrix <- power_matrix[sub,];
	# plot histogram: Note specifications ask for 480 x 480 while example files are 504 x 504
	png(file = "plot1.png", width=480, height=480);
	hist(sub_matrix$Global_active_power, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", col="red");
	dev.off();
}