data(iris)

# scatter plot for each pair of species

features <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
pairs_list <- combn(features, 2, simplify = FALSE)

colors <- c("setosa" = "red", "versicolor" = "blue", "virginica" = "green3")
species_colors <- colors[as.character(iris$Species)]

par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
for (pair in pairs_list) {
  plot(iris[[pair[1]]], iris[[pair[2]]],
       col  = species_colors,
       pch  = 19,
       xlab = pair[1],
       ylab = pair[2],
       main = paste(pair[1], "vs", pair[2]))
  legend("topright",
         legend = names(colors),
         col    = colors,
         pch    = 19,
         cex    = 0.7)
}
par(mfrow = c(1, 1))

#Group box plots for each feature vs species

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
for (feat in features) {
  boxplot(iris[[feat]] ~ iris$Species,
          col    = c("red", "blue", "green3"),
          xlab   = "Species",
          ylab   = feat,
          main   = paste("Boxplot of", feat, "by Species"))
}
par(mfrow = c(1, 1))

# Histograms of petal length and petal width by species

# Separate by species
setosa     <- subset(iris, Species == "setosa")
versicolor <- subset(iris, Species == "versicolor")
virginica  <- subset(iris, Species == "virginica")

par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
# Petal length histograms
hist(setosa$Petal.Length,     col = "red",    main = "Setosa - Petal Length",
     xlab = "Petal length (cm)", xlim = c(0, 8))
hist(versicolor$Petal.Length, col = "blue",   main = "Versicolor - Petal Length",
     xlab = "Petal length (cm)", xlim = c(0, 8))
hist(virginica$Petal.Length,  col = "green3", main = "Virginica - Petal Length",
     xlab = "Petal length (cm)", xlim = c(0, 8))
# Petal Width histograms
hist(setosa$Petal.Width,      col = "red",    main = "Setosa - Petal Width",
     xlab = "Petal width (cm)", xlim = c(0, 3))
hist(versicolor$Petal.Width,  col = "blue",   main = "Versicolor - Petal Width",
     xlab = "Petal width (cm)", xlim = c(0, 3))
hist(virginica$Petal.Width,   col = "green3", main = "Virginica - Petal Width",
     xlab = "Petal width (cm)", xlim = c(0, 3))
par(mfrow = c(1, 1))

# CI
confidence_interval <- function(x, conf_level = 0.95) {
  n <- length(x)
  x_bar <- mean(x)
  s <- sd(x)
  alpha <- 1 - conf_level
  t_crit <- qt(1 - alpha / 2, df = n - 1)
  margin <- t_crit * (s / sqrt(n))
  lower <- x_bar - margin
  upper <- x_bar + margin
  return(invisible(list(mean = x_bar, lower = lower, upper = upper,
                        conf_level = conf_level)))
}


#CIs for petal length of all species

cat("Setosa:")
(ci_setosa<- confidence_interval(setosa$Petal.Length))

cat("Versicolor:")
(ci_versicolor<- confidence_interval(versicolor$Petal.Length))

cat("Virginica:")
(ci_virginica<- confidence_interval(virginica$Petal.Length))

#2 sample hypothesis test
two_sample_t_test <- function(x1, x2, alpha = 0.05, alternative = "two.sided") {
  n1 <- length(x1);  n2 <- length(x2)
  m1 <- mean(x1);    m2 <- mean(x2)
  s1 <- sd(x1);      s2 <- sd(x2)
  
  # Welchss standard error and degrees of freedom
  se  <- sqrt(s1^2 / n1 + s2^2 / n2)
  t_stat <- (m1 - m2) / se
  
  # Welch-Satterthwaite degrees of freedom
  df_num <- (s1^2 / n1 + s2^2 / n2)^2
  df_den <- (s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2 - 1)
  df <- df_num / df_den
  
  # p-value based on alternative
  if (alternative == "two.sided") {
    p_val <- 2 * pt(-abs(t_stat), df = df)
  } else if (alternative == "greater") {
    p_val <- pt(t_stat, df = df, lower.tail = FALSE)
  } else if (alternative == "less") {
    p_val <- pt(t_stat, df = df, lower.tail = TRUE)
  } else {
    stop("alternative must be 'two.sided', 'greater', or 'less'")
  }
    if (p_val < alpha) {
    cat("  Decision: REJECT H0\n\n")
  } else {
    cat("  Decision: FAIL TO REJECT H0\n\n")
  }
  return(invisible(list(t_stat = t_stat, df = df, p_value = p_val,
                        reject = p_val < alpha)))
}


# hypothesis test (0.05)
cat("length test 1: Virginica > Versicolor")
test1 <- two_sample_t_test(virginica$Petal.Length, versicolor$Petal.Length, alpha = 0.05, alternative = "greater")

cat("length test 2: Versicolor > Setosa")
test2 <- two_sample_t_test(versicolor$Petal.Length, setosa$Petal.Length, alpha = 0.05, alternative = "greater")

classify_iris <- function(pl, pw) {
  if (pl < 2.5) return("setosa")
  else if (pw < 1.8) return("versicolor")
  else return("virginica")
}

predicted <- mapply(classify_iris, iris$Petal.Length, iris$Petal.Width)
accuracy  <- mean(predicted == as.character(iris$Species))
cat(sprintf("Classification rule accuracy: %.1f%%\n", accuracy * 100))
table(Predicted = predicted, Actual = iris$Species)
