



#####10.22 classnote
#large σ in PDF mean more broad size
dnorm(x = 0)
pnorm(1)
qnorm(0.8413447)
rnorm(1)
samplen <- replicate(500, sample(1:999, 30))
sample_mean <- apply(samplen, 2, mean)
round(mean(sample_mean))
mean(sample_mean)
BSDA::zsum.test(mean.x = 109, sigma.x = 15, n.x = 9, mu = 100)
zsum.test(mean.x = 109, sigma.x = 15, n.x = 9, mu = 100)
x <- c(24.29, 21.47, 28.58, 22.41, 23.12, 
       23.91, 28.51, 16.25, 22.57, 23.17)
t.test(x)#95 percent confidence interval:20.91987 25.93613(样本总体mean有95%在这个区间)
x <- c(91, 100, 70, 87, 104, 92, 104, 88, 72, 119)
t.test(x, mu = 100, alternative = "less")#如果研究问题具有明确的方向性，则应选择单尾检验；
#如果研究问题不涉及特定的方向性，或者研究者对任何方向的差异都感兴趣，则应选择双尾检验。

#EXERCISE4
#1
tce <- read.table(file = "E:\\download\\tce.csv", header = TRUE,sep = ',')
library(ggplot2)
ggplot(tce, aes(x = factor(1), y = TCE.mg.per.L)) +  # 这里x设置为factor(1)是因为箱线图不需要x轴上的分类
  geom_boxplot(width = 0.5) +  # 设置箱线图的宽度
  facet_wrap(~ Period, scales = "free_x") +  # 根据time列的值创建不同的面板，并允许x轴自由缩放（尽管这里x轴是固定的）
  labs(x = "",  # 因为x轴是固定的，所以不需要标签
       y = "TCE Concentration") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # 移除x轴的文本标签
        axis.ticks.x = element_blank())  # 移除x轴的刻度

#2 Paired sample t-test

#3,4
wl <- data.frame(
  id = LETTERS[1:10],
  before = c(20.900, 9.170, 5.960, 41.500, 34.300, 19.700, 38.900, 8.180, 9.130, 28.500),
  after = c(0.917, 8.770, 4.370, 4.340, 10.700, 1.480, 0.272, 0.520, 3.060, 1.900))
t.test(wl$before, wl$after, alternative = "greater", paired = TRUE)

#5
sample_mean <- 0.23
sample_sd <- 0.03
sample_size <- 11
population_mean <- 0.16
t_statistic <- (sample_mean - population_mean) / (sample_sd / sqrt(sample_size))
df <- sample_size - 1
p_value <- 2 * pt(-abs(t_statistic), df)
cat("t-statistic:", t_statistic, "\np-value:", p_value)

library(Envstats)
install.packages("EnvStats")



# Assuming Total.P.df is your data frame and Total.P is the column of interest

# Calculate the mean
mean_value <- mean(Total.P.df$CB3.1, na.rm = TRUE) # Use na.rm = TRUE to ignore NA values
mean_rounded <- round(mean_value, 4) # Round to 4 decimal places, which is 0.0001 mg

# Calculate the median
median_value <- median(Total.P.df$CB3.1, na.rm = TRUE)
median_rounded <- round(median_value, 4)

# Calculate the standard deviation
sd_value <- sd(Total.P.df$CB3.1, na.rm = TRUE)
sd_rounded <- round(sd_value, 4)

# Print the results
print(paste("Mean (rounded to 0.0001 mg):", mean_rounded, "mg"))
print(paste("Median (rounded to 0.0001 mg):", median_rounded, "mg"))
print(paste("Standard Deviation (rounded to 0.0001 mg):", sd_rounded, "mg"))

install.packages("e1071")
library(e1071)
skewness_value <- skewness(Total.P.df$CB3.1, na.rm = TRUE)
print(skewness_value)
round(skewness_value, 2)

iqr_value <- IQR(Total.P.df$CB3.1, na.rm = TRUE)
print(iqr_value)
print(paste(iqr_value, "mg"))


##10.29  class
#note
#x是数据的每一个值，x上面的横线代表mean
#线性回归蓝线经过x和y的均值，所以他是最好的





#exercise5
#1
iris_lm <- lm(Sepal.Length~Sepal.Width,  data = iris[iris$Species == 'setosa', ])
summary(iris_lm)
plot(iris$Sepal.Length[iris$Species=='setosa'], iris$Sepal.Width[iris$Species=='setosa'], las=1, pch=16)
abline(lm(Sepal.Length~Sepal.Width,  data = iris[iris$Species == 'setosa', ]))

iris_setosa <- iris[iris$Species == 'setosa', ]
plot(iris_setosa$Sepal.Length, iris_setosa$Sepal.Width, las = 1, pch = 16)
iris_lm <- lm(Sepal.Width ~ Sepal.Length, data = iris_setosa)
abline(iris_lm)

# Load the iris dataset
data(iris)

# Function to create scatter plots with regression line for each species
plot_iris <- function(species, data) {
  # Subset the data for the specified species
  subset_data <- subset(data, Species == species)
  
  # Create a 2x2 plot layout
  par(mfrow = c(2, 1))
  
  # Plot sepal length against sepal width with regression line
  plot(subset_data$Sepal.Length, subset_data$Sepal.Width,
       species),
       xlab = "Sepal Length (cm)", ylab = "Sepal Width (cm)",
       pch = 19, col = "blue")
  abline(lm(Sepal.Width ~ Sepal.Length, data = subset_data), col = "red")
  
  # Plot petal length against petal width with regression line
  plot(subset_data$Petal.Length, subset_data$Petal.Width,
      species),
       xlab = "Petal Length (cm)", ylab = "Petal Width (cm)",
       pch = 19, col = "green")
  abline(lm(Petal.Width ~ Petal.Length, data = subset_data), col = "red")
  
  # Reset plot layout
  par(mfrow = c(1, 1))
}

# Apply the function for each species
plot_iris("setosa", iris)
plot_iris("versicolor", iris)
plot_iris("virginica", iris)
#2
# Load the iris dataset
data(iris)

# Function to calculate correlation coefficients for each species
correlation_iris <- function(species, data) {
  # Subset the data for the specified species
  subset_data <- subset(data, Species == species)
  
  # Calculate correlation coefficients
  sepal_cor <- cor(subset_data$Sepal.Length, subset_data$Sepal.Width)
  petal_cor <- cor(subset_data$Petal.Length, subset_data$Petal.Width)
  
  # Return the correlation coefficients
  list(
    species = species,
    sepal_length_vs_width = sepal_cor,
    petal_length_vs_width = petal_cor
  )
}

# Apply the function for each species and store the results
correlations <- do.call(rbind, lapply(unique(iris$Species), function(x) correlation_iris(x, iris)))

# Print the results
print(correlations)

species sepal_length_vs_width petal_length_vs_width
1                 setosa               0.12345               0.78901
2             versicolor               0.45678               0.65432
3              virginica               0.34567               0.89012

#3
file.choose()
plant_data <- read.table(file = 'E:/download/Plant_height.csv', header = TRUE, sep = ',')
library(ggplot2)
library(dplyr)
plant_data <- plant_data %>%mutate(loght = log(height))
ggplot(plant_data, aes(x = temp, y = loght)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") + # Add regression line
  geom_vline(xintercept = mean(plant_data$temp), color = "blue", linetype = "dashed") + # Vertical line at mean temp
  geom_hline(yintercept = mean(plant_data$loght), color = "blue", linetype = "dashed") + # Horizontal line at mean log(height)
  annotate("text", x = mean(plant_data$temp), y = max(plant_data$loght), label = paste("Mean Temp:", round(mean(plant_data$temp), 2)), color = "blue", hjust = -0.1) +
  annotate("text", x = max(plant_data$temp), y = mean(plant_data$loght), label = paste("Mean log(Height):", round(mean(plant_data$loght), 2)), color = "blue", vjust = 1.5) +
  xlab("Temperature") +
  ylab("Logarithm of Plant Height") +
  theme_minimal()
#4
model <- lm(log(height) ~ temp, data = plant_data)
summary(model)

#5
plant_data$log_height <- log(plant_data$height)
correlation_test <- cor.test(plant_data$temp, plant_data$log_height)
cat("Correlation Coefficient:", correlation_test$estimate, "\n")
cat("P-value:", correlation_test$p.value, "\n")

sample_mean <- 1.51

sample_sd <- 0.28

n <- 13

population_mean <- 1

simulated_data <- rep(sample_mean, n)

t_test_result <- t.test(simulated_data, mu = 1)

t_score <- t_test_result$statistic

rounded_t_score <- round(t_score, 3)

rounded_t_score

sample_mean <- 1.51
sample_sd <- 0.28
n <- 13
population_mean <- 1
t_score <- (sample_mean - population_mean) / (sample_sd / sqrt(n))
rounded_t_score <- round(t_score, 3)
rounded_t_score
# Sample mean
sample_mean <- 1.51

# Sample standard deviation
sample_sd <- 0.28

# Sample size
n <- 13

# Population mean (null hypothesis value)
population_mean <- 2

# Simulate the data (not ideal, but works for extracting t-statistic)
simulated_data <- rep(sample_mean, n)

# Perform the one-sample t-test
t_test_result <- t.test(simulated_data, mu = population_mean)

# Extract the t-statistic from the test result
t_score <- t_test_result$statistic

# Round to 0.001
rounded_t_score <- round(t_score, 3)

# Print the result
rounded_t_score



### 11.5 classnote

#exercise6
#1
jaw <- read.table(file.choose(), header = TRUE)
ggplot(jaw) + geom_point(aes(age, bone))
j1 <- nls(bone ~ a * age/ (b + age), data = jaw, start = list(a = 140, b = 10))
summary(j1)
xj <- seq(0, 55, .01)
yj <- predict(j1, list(age = xj))
jawpredicted <- data.frame(xj, yj)
ggplot(jaw) + geom_point(aes(age, bone)) + geom_line(aes(xj, yj), data = jawpredicted, color = 'blue')

#2
data("Puromycin")
ggplot(Puromycin) + geom_point(aes(conc, rate))
untreated <- subset(Puromycin, state == "untreated")
treated <- subset(Puromycin, state == "treated")
m1 <- nls(rate ~ a * conc / (b + conc), data = untreated, start = list(a = 160, b = 0.1))
summary(m1)
m2 <- nls(rate ~ a * conc / (b + conc), data = treated, start = list(a = 200, b = 0.1))
summary(m2)
xv <- seq(0, 1.1, .01)
y1 <- predict(m1, list(conc = xv))
y2 <- predict(m2, list(conc = xv))
dtf_predicted <- data.frame(xv, y1, y2)
ggplot(Puromycin) + 
  geom_point(aes(conc, rate)) +
  geom_line(aes(xv, y1), data = dtf_predicted, color = 'blue') + 
  geom_line(aes(xv, y2), data = dtf_predicted, color = 'red')

#quize
library(EnvStats)
data("Air.df")
str(Air.df)
plot(Air.df$temperature[!is.na(Air.df$ozone) & !is.na(Air.df$temperature)], Air.df$ozone[!is.na(Air.df$ozone) & !is.na(Air.df$temperature)], 
     xlab = "Temperature", 
     ylab = "Ozone Concentration", pch = 19)
abline(lm(Air.df$ozone ~ Air.df$temperature), col = "red")
cor_coefficient <- cor(Air.df$temperature, Air.df$ozone, use = "pairwise.complete.obs")
rounded_cor_coefficient <- round(cor_coefficient, 4)
rounded_cor_coefficient
mm <- lm(ozone ~ temperature, data = Air.df)
slope <- coef(mm)[2]
intercept <- coef(mm)[1]
se_slope <- summary(model)$coefficients[2, 2]
se_intercept <- summary(model)$coefficients[1, 2]
se_slope <- summary(mm)$coefficients[2, 2]
se_intercept <- summary(mm)$coefficients[1, 2]


cat("Regressed Slope: ", round(slope, 4), "\n")
cat("Regressed Intercept: ", round(intercept, 4), "\n")
cat("Standard Error of Slope: ", round(se_slope, 4), "\n")
cat("Standard Error of Intercept: ", round(se_intercept, 4), "\n")

summary(mm)
summary_model <- summary(mm)
r_squared <- summary_model$r.squared
cat("Coefficient of Determination (R-squared): ", round(r_squared, 3), "\n")
cat("Interpretation: ", round(r_squared * 100, 3), "% of the variation in ozone concentration is predictable from the variation in temperature.\n")


##11.12 classnote
#exercise8
#1
library(agricolae)
dtf <- data.frame(HTC = c(38, 40, 32, 36, 40, 40, 38, 40, 38, 40, 36, 40, 40, 35, 45, 56, 60, 50, 50, 50, 35, 40, 40, 55, 35, 40, 42, 38, 46, 36))
dtf$treatment <- LETTERS[1 : 30]
dtf$treatment[1 : 15] <- rep('control', 15)
dtf$treatment[16:25] <- rep('DUSHU', 10)
dtf$treatment[26:30] <- rep('JINJI', 5)
ggplot(dtf) + geom_boxplot(aes(HTC, treatment))
HTC_aov <- aov(HTC ~ treatment, data = dtf)
summary(HTC_aov)


#6
data("iris")
ggplot(iris) + geom_boxplot(aes(Sepal.Length, Species))
anova_result <- aov(Sepal.Length ~ Species, data = iris)
summary(anova_result)

lsd_test <- LSD.test(anova_result, "Species", alpha = 0.05)
print(lsd_test)
model <- lm(Sepal.Length ~ Species, data = iris)
summary(glht(model, linfct = mcp(Species = "Tukey")), test = adjusted("bonferroni"))


#quize 6
#1
quize <- data.frame(x = c(11, 17, 20, 25, 40, 55), y = c(96, 129, 135, 145, 168, 235))
quize_n <- nls(y ~ a * x ^ b, data = quize, start = list(a = 1, b = 1))
summary(quize_n)
xq <- seq(0, 55, .055)
yq <- predict(quize_n, list(x = xq))
quize6 <- data.frame(xq, yq)
ggplot(quize) + geom_point(aes(x, y)) + geom_line(aes(xq, yq), data = quize6, color = 'blue') 


##11.19
##classnote
#Wilcoxon Rank-Sum test
insect <- read.csv(file.choose())
insect <- insect[insect$spray %in% c("C", "D"), ]

boxplot(bugs ~ spray, data = insect, horizontal = TRUE)

shapiro.test(insect$bugs[insect$spray == "C"])
shapiro.test(insect$bugs[insect$spray == "D"])

tapply(insect$bugs, insect$spray, median)

wilcox.test(bugs ~ spray, data = insect, conf.int=TRUE)
##w is the score/the p-value are less then 0.05, so the two sample have different median
##the different between spray D and C across the population will be between 1 and 4 bugs
##It does not estimate the difference in medians but rather the median of the difference (between the two samples)

median(outer(insect$bugs[insect$spray == 'C'], insect$bugs[insect$spray == 'D'], "-"))

#Wilcoxon Signed-Rank test
wl <- data.frame(
  id = LETTERS[1:10],
  before = c(198, 201, 210, 185, 204, 156, 167, 197, 220, 186),
  after = c(194, 203, 200, 183, 200, 153, 166, 197, 215, 184))

wilcox.test(wl$before, wl$after, paired = TRUE, conf.int = TRUE, correct = FALSE)

##Kruskal-Wallis test(ANOVA)
boxplot(bugs ~ spray, data = insect, horizontal = TRUE, notch = TRUE)

kruskal.test(bugs ~ spray, data = insect)

##Friedman’s test


##exercise8
##1
kruskal.test(HTC ~ treatment, data = dtf)

##2
tce <- read.csv(file.choose())
tcebefore <- subset(tce, Period == 'Before')
tceafter <- subset(tce, Period == 'After')
wilcox.test(tcebefore$TCE.mg.per.L, tceafter$TCE.mg.per.L, paired = TRUE, conf.int = TRUE, correct = FALSE)

##quize
money <- data.frame(Money = c(1896, 2606, 1649,2436, 2811, 2384, 2840, 2445, 1712, 2096, 1923, 2281, 2703, 2092, 1499, 2146, 1689, 2256, 1834, 2365, 1958, 1947, 2433, 1578, 1455, 1164, 1851, 1776, 2030, 1640, 1678, 1547))
money$region <- LETTERS[1 : 32]
money$region[1 : 8] <- rep('Northeast', 8)
money$region[9 : 16] <- rep('Midwest', 8)
money$region[17 : 24] <- rep('South', 8)
money$region[25 : 32] <- rep('West', 8)
critical_F_value <- qf(1 - 0.1, 3, 28)

money_aov <- aov(Money ~ region, data = money)
summary(money_aov)
ggplot(money) + geom_boxplot(aes(Money, region)) + labs(x = "money($)")


##11.26classnote
xqc <- iris[1:7, 1:2]
xpc <- princomp(xqc, cor=TRUE, score=TRUE)
par(mfrow = c(1,2))
plot(xqc$Sepal.Width, xqc$Sepal.Length, pch = as.character(1:7))
biplot(xpc)

com1 <- prcomp(iris[,1:4], center = TRUE, scale. = TRUE) 
summary(com1)

##exercies 2
hist(Air.df$wd)
windRose(Air.df, ws = 'ws', wd = 'wd')
##quize
#2
city <- data.frame(cityA = c(13.31, 13.67, 12.805, 13.1, 13.39, 12.375, 11.58, 14.58, 12.029, 13.69),
                   cityB = c(14.14, 12.54, 15.63, 13.66, 15.51, 13.23, 13.25, 15.2, 14.18, 14.53))
# 加载数据
city_A <- c(13.31, 13.67, 12.805, 13.1, 13.39, 12.375, 11.58, 14.58, 12.029, 13.69)
city_B <- c(14.14, 12.54, 15.63, 13.66, 15.51, 13.23, 13.25, 15.2, 14.18, 14.53)

# 进行威尔科克森秩和检验
wilcox.test(city_A, city_B)
sa <- shapiro.test(city_A)

library(mgcv)
help("mgcv-package")

##2025.01.02
##管道的用法
round(mean(iris$Sepal.Length), 1)

iris$Sepal.Length |> mean() |> round(1)
##同样是管道
#%>%

##function using
myfun1 <- function(x){
  round(mean(x), 1)
}
myfun1(iris$Sepal.Length)

myfun1 <- function(x){
  mymean <- mean(x, na.rm = TRUE)
  round(mymean, 1)
}
myfun1(iris$Sepal.Length)
mutate()

##合并文件
library(readxl)
myfile <- 'E:\\download\\ftaws\\风速 数据列表.xlsx'
mydtf <- read_excel(myfile)

myfiles <- list.files('E:\\download\\ftaws', full.names = TRUE)

i <- myfiles[1]
basename(i)
dtfi <- read_excel(i)
gsub(' 数据列表.xlsx', '', basename(i))

names(dtfi)[2] <- gsub(' 数据列表.xlsx', '', basename(i))

for (i in myfiles) 
  {
  dtf_temp <- read_excel(i)
  names(dtf_temp)[2] <- gsub(' 数据列表.xlsx', ' ', basename(i))
  dtfi <- merge(dtfi, dtf_temp, by = '时间', all = TRUE)
}

##1/9
library(readxl)
weizhuce <- read_xlsx(file.choose())
weizhuce2018 <- read_xlsx(file.choose())
jiehe <- rbind(weizhuce, weizhuce2018)
mydata <- list.files(path = 'F:\\study\\WIT\\medicine', pattern = '\\.xlsx$', full.names = TRUE)
i <- NULL
x <- data.frame()
y <- NULL
for (i in mydata) {
  y <- read_xlsx(i)
  x <- rbind(x, y)
}

##1.16
demo(plotmath)

phi1
plot(cars, xlab = expression(over(x, y)))
install.packages("latex2exp")
library(latex2exp)
library(corrplot)
