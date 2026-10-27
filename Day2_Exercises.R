
#DAY 2 EXERCISES 
# QUESTION 1: COVARIANCE AND CORRELATION

A <- c(18, 2, 20)
B <- c(22, 28, 10)
C <- c(20, 40, 40)

cat("\\nQUESTION 1: COVARIANCE AND CORRELATION\\n")
cat("Covariance between B and C:\\n")
print(cov(B, C))

preferences <- data.frame(A, B, C)
cat("Covariance matrix:\\n")
print(cov(preferences))

cat("Correlation between B and C:\\n")
print(cor(B, C))

cat("Correlation matrix:\\n")
print(cor(preferences))

cat("Note: These are aggregated counts by age group, not individual-level observations.\\n")

#OUTPUT : 
#A          B          C
#A  1.0000000 -0.8183918 -0.4096440
#B -0.8183918  1.0000000 -0.1889822
#C -0.4096440 -0.1889822  1.0000000

# QUESTION 2: EQUAL-FREQUENCY PARTITIONING AND DATA SMOOTHING

prices <- c(
  1, 1, 5, 5, 8, 10, 10, 10, 10, 12, 14, 14, 14, 15, 15,
  15, 15, 15, 18, 18, 18, 18, 18, 20, 20, 20, 20, 20, 20,
  20, 21, 21, 21, 25, 25, 25, 25, 25, 28, 28, 30
)

prices <- sort(prices)
bins <- split(prices, cut(seq_along(prices), breaks = 3, labels = FALSE))

cat("\\nQUESTION 2: EQUAL-FREQUENCY BINS\\n")
print(bins)

smooth_means <- lapply(bins, function(bin) rep(mean(bin), length(bin)))
cat("Smoothing by bin means:\\n")
print(smooth_means)

smooth_boundaries <- lapply(bins, function(bin) {
  low <- min(bin)
  high <- max(bin)
  sapply(bin, function(value) {
    if (abs(value - low) <= abs(value - high)) low else high
  })
})
cat("Smoothing by bin boundaries:\\n")
print(smooth_boundaries)

hist(prices,
     main = "Histogram of All Electronics Prices",
     xlab = "Price",
     ylab = "Frequency",
     col = "lightblue",
     border = "white")
#OUTPUT: $`1`
#[1]  1  1  1  1  1 15 15 15 15 15 15 15 15 15

#$`2`
#[1] 15 15 15 15 20 20 20 20 20 20 20 20 20

#$`3`
#[1] 20 20 20 20 20 20 20 20 20 20 20 30 30 30



# QUESTION 3: MEAN, MEDIAN, RANGE AND BOXPLOT


class_A <- c(76, 35, 47, 64, 95, 66, 89, 36, 84)
class_B <- c(51, 56, 84, 60, 59, 70, 63, 66, 50)

cat("\\nQUESTION 3: CLASS COMPARISON\\n")
cat("Class A mean:", mean(class_A), "\\n")
cat("Class B mean:", mean(class_B), "\\n")
cat("Class A median:", median(class_A), "\\n")
cat("Class B median:", median(class_B), "\\n")
cat("Class A range:", diff(range(class_A)), "\\n")
cat("Class B range:", diff(range(class_B)), "\\n")

boxplot(class_A, class_B,
        names = c("Class A", "Class B"),
        main = "Class A vs Class B Exam Results",
        xlab = "Class",
        ylab = "Marks",
        col = c("lightblue", "lightgreen"))



# QUESTION 4: DATA NORMALIZATION



x <- c(200, 300, 400, 600, 1000)

cat("\\nQUESTION 4: DATA NORMALIZATION\\n")
minmax <- (x - min(x)) / (max(x) - min(x))
cat("Min-max normalized values:\\n")
print(minmax)

zscore <- (x - mean(x)) / sd(x)
cat("Z-score normalized values:\\n")
print(zscore)



# QUESTION 5: HISTOGRAM OF AirPassengers


data("AirPassengers")
passengers <- as.numeric(AirPassengers)

cat("\\nQUESTION 5: AirPassengers HISTOGRAM\\n")
hist(passengers,
     breaks = seq(100, 700, by = 150),
     xlim = c(100, 700),
     main = "Histogram of AirPassengers",
     xlab = "Passengers",
     ylab = "Frequency",
     col = "lightblue",
     border = "white")



# QUESTION 6: MULTIPLE LINES USING ONE PLOT FUNCTION

data(mtcars)
line_data <- cbind(mtcars$mpg, mtcars$qsec)

cat("\\nQUESTION 6: MULTIPLE LINE CHART\\n")
matplot(line_data,
        type = "l",
        lty = 1,
        col = c("blue", "red"),
        main = "MPG and QSEC",
        xlab = "Car Index",
        ylab = "Value")

legend("topright",
       legend = c("mpg", "qsec"),
       col = c("blue", "red"),
       lty = 1)



# QUESTION 7: LINEAR REGRESSION USING THE water DATASET
#
# Download/use the water dataset. Find whether mortality and
# hardness have a linear relationship using a plot. Fit a
# linear regression model and predict mortality when hardness = 88.


cat("\\nQUESTION 7: WATER DATASET REGRESSION\\n")
if (!requireNamespace("MASS", quietly = TRUE)) {
  cat("Package MASS is not installed. Install it using install.packages('MASS').\\n")
} else {
  data("water", package = "MASS")
  water_data <- MASS::water

  plot(water_data$hardness, water_data$mortality,
       main = "Mortality vs Hardness",
       xlab = "Hardness",
       ylab = "Mortality",
       pch = 19,
       col = "blue")

  model <- lm(mortality ~ hardness, data = water_data)
  abline(model, col = "red", lwd = 2)

  print(summary(model))
  cat("Predicted mortality when hardness = 88:\\n")
  print(predict(model, newdata = data.frame(hardness = 88)))
}



# QUESTION 8: BOXPLOT OF MPG AGAINST CYLINDERS
#
# Create a boxplot for the relationship between mpg (miles
# per gallon) and cyl (number of cylinders) in mtcars.


data(mtcars)

cat("\\nQUESTION 8: MPG VS CYLINDERS BOXPLOT\\n")
boxplot(mpg ~ cyl,
        data = mtcars,
        main = "MPG vs Number of Cylinders",
        xlab = "Cylinders",
        ylab = "Miles per Gallon (mpg)",
        col = "lightblue")



# QUESTION 9: BOXPLOT TO IDENTIFY TENNIS PLAYER OUTLIERS
#
# A tennis coach wants to determine whether any players are
# scoring outliers. Explain how a boxplot can visualize the
# distribution of points scored. Give a suitable example.


scores <- c(10, 12, 15, 18, 20, 22, 25, 28, 30, 100)

cat("\\nQUESTION 9: TENNIS SCORES AND OUTLIERS\\n")
boxplot(scores,
        main = "Tennis Player Scores",
        ylab = "Points Scored",
        col = "lightgreen")

cat("Potential outliers detected by boxplot.stats():\\n")
print(boxplot.stats(scores)$out)



# QUESTION 10: BLOOD PRESSURE VS AGE USING diabetes.csv

cat("\\nQUESTION 10: BLOOD PRESSURE VS AGE\\n")
if (file.exists("diabetes.csv")) {
  diabetes <- read.csv("diabetes.csv")

  # Handle common variations in column names
  names(diabetes) <- trimws(names(diabetes))
  age_col <- which(tolower(names(diabetes)) == "age")[1]
  bp_col <- which(tolower(names(diabetes)) == "bloodpressure")[1]

  if (is.na(age_col) || is.na(bp_col)) {
    cat("Error: diabetes.csv must contain Age and BloodPressure columns.\\n")
  } else {
    age <- diabetes[[age_col]]
    bp <- diabetes[[bp_col]]

    plot(age, bp,
         main = "Blood Pressure vs Age",
         xlab = "Age",
         ylab = "Blood Pressure",
         pch = 19,
         col = "blue")

    age_group <- cut(age,
                     breaks = c(0, 20, 30, 40, 50, 60, Inf),
                     right = FALSE,
                     include.lowest = TRUE)

    avg_bp <- aggregate(bp,
                        by = list(AgeGroup = age_group),
                        FUN = function(z) mean(z, na.rm = TRUE))
    names(avg_bp)[2] <- "AverageBloodPressure"

    barplot(avg_bp$AverageBloodPressure,
            names.arg = avg_bp$AgeGroup,
            main = "Average Blood Pressure by Age Group",
            xlab = "Age Group",
            ylab = "Average Blood Pressure",
            col = "lightblue",
            las = 2)
  }
} else {
  cat("File diabetes.csv not found. Put diabetes.csv in the working directory to run Question 10.\\n")
}

