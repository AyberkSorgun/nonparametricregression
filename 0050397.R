#HW04
# read data into memory
 
data_set <- read.csv("~/Desktop/hw04/hw04_data_set.csv")
training_set <- data_set[1:100,]
test_set <- data_set[101:133,]

# get x and y values
x_train <- training_set$x
y_train <- training_set$y
x_test <- test_set$x
y_test <- test_set$y

# get number of classes and number of samples
K <- max(y_train)
N <- length(y_train)

point_colors <- c("red", "blue")
minimum_value <- 0
maximum_value <- 60
data_interval <- seq(from = minimum_value, to = maximum_value, by = 0.01)

bin_width <- 3
left_borders <- seq(from = minimum_value, to = maximum_value - bin_width, by = bin_width)
right_borders <- seq(from = minimum_value + bin_width, to = maximum_value, by = bin_width)
p_head <- sapply(1:length(left_borders), function(b) {sum(y_train[left_borders[b] < x_train & x_train <= right_borders[b]])/sum(left_borders[b] < x_train & x_train <= right_borders[b])})

plot(x_train, y_train, type = "p", pch = 20, 
     ylim = c(min(y_train), max(y_train)), xlim = c(minimum_value, maximum_value),
     ylab = "density", xlab = "x", las = 1, main = sprintf("h = %g", bin_width),col="blue")
for (b in 1:length(left_borders)) {
  lines(c(left_borders[b], right_borders[b]), c(p_head[b], p_head[b]), lwd = 2, col = "black")
  if (b < length(left_borders)) {
    lines(c(right_borders[b], right_borders[b]), c(p_head[b], p_head[b + 1]), lwd = 2, col = "black") 
  }
}

legend("topright", legend=c("Training", "Test"),
       col=c("blue", "red"), pch= 20, cex=0.7)

points(x_test, y_test,
       col = "red", pch = 20)

error <- 0
for (b in 1:length(left_borders)) {
  error <- error + sum((y_test[left_borders[b] < x_test & x_test <= right_borders[b]]-p_head[b])^2)
}
error <- sqrt(error/33)

sprintf("Regressogram =>RMSE is %s when h is %s", error, bin_width)

p_head <- sapply(1:length(data_interval), function(b) {sum(y_train[abs((x_train - data_interval[b])/bin_width) <= 1/2])/sum(abs((x_train - data_interval[b])/bin_width) <= 1/2)})
plot(x_train, y_train, type = "p", pch = 20, 
     ylim = c(min(y_train), max(y_train)), xlim = c(minimum_value, maximum_value),
     ylab = "density", xlab = "x", las = 1, main = sprintf("h = %g", bin_width),col="blue")

lines(data_interval, p_head, type = "l", lwd = 2, col = "black")
legend("topright", legend=c("Training", "Test"),
       col=c("blue", "red"), pch= 20, cex=0.7)
points(x_test, y_test,
       col = "red", pch = 20)

error <- 0
for (b in 1:33) {
  error <- error + sum((y_test[b] - p_head[x_test[b]/0.01])^2)
}
error <- sqrt(error/33)

sprintf("Running Mean Smoother => RMSE is %s when h is %s", error, bin_width)


bin_width <- 1
p_head <- sapply(data_interval, function(x) {sum(1 / sqrt(2 * pi) * exp(-0.5 * (x - x_train)^2 / bin_width^2) * y_train) /sum(1 / sqrt(2 * pi) * exp(-0.5 * (x - x_train)^2 / bin_width^2))})
plot(x_train, y_train, type = "p", pch = 20, 
     ylim = c(min(y_train), max(y_train)), xlim = c(minimum_value, maximum_value),
     ylab = "density", xlab = "x", las = 1, main = sprintf("h = %g", bin_width),col="blue")
lines(data_interval, p_head, type = "l", lwd = 2, col = "black")
legend("topright", legend=c("Training", "Test"),
       col=c("blue", "red"), pch= 20, cex=0.7)
points(x_test, y_test,
       col = "red", pch = 20)

error <- 0

for (b in 1:33) {
  error <- error + sum((y_test[b] - p_head[x_test[b]/0.01])^2)
}
error <- sqrt(error/33)

sprintf("Kernel Smoother => RMSE is %s when h is %s", error, bin_width)


