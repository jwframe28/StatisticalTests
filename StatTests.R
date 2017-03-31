# All of this is using CH03PR18.txt






# Brausch-Pagan Test

toluca <- read.table("C:\\Users\\John\\Downloads\\CH03PR18.txt", sep = "", header = T, stringsAsFactor = T)
reg1 <- lm(toluca$Hours ~ toluca$Size)
summary(reg1)
anova(reg1)

# Gives square of residuals
toluca.res <- (reg1$residuals^2)

# Gets natural log of the squared residuals
ln.toluca.res <- log10(toluca.res)

# Regresses squared residuals on X values
bp.reg <- lm(ln.toluca.res ~ toluca$Size)

# We want the SSR from the regression of squared residuals on X
summary(bp.reg)
anova(bp.reg)

# Set SSR <- *VALUE FROM PREVIOUS REGRESSION*
# Set SSE <- *VALUE FROM FIRST REGRESSION*

SSR <- 1.564
SSE <- 488.84

# length(toluca$size) is just n, so you can put whichever you want, even length(ln.toluca.res) etc.
bp.stat <- (SSR/2)/((SSE/length(toluca$Size))^2)


# Gives us the Chi-Squared values with alpha =.05 and df = 111

qchisq(.95, df=111)


# If bp.stat < qchisq() then conclude H0-- Error Variance is constant











# Brown-Forsythe Test



# Before we split the data, we need the residuals from the regression

reg2 <- lm(toluca$Hours ~ toluca$Size)

# Store the fitted values and residuals

bf.fitted <- reg2$fitted.values

bf.res <- reg2$residuals

# Attach these two vectors to the original data set

toluca <- cbind(toluca, bf.fitted, bf.res)

# Give the two new columns names

names(toluca) [3:4] <- c("Fitted", "Residuals")

# Splits the residuals by their Size (x-value)

group1 <- toluca[which(toluca$Size < 15),"Residuals"]
group2 <- toluca[which(toluca$Size > 14),"Residuals"]

# Get the length for the BF calculations

n <- length(group1) + length(group2)

# Get the absolute deviations from the median

M1 <- abs(group1 - median(group1))
M2 <- abs(group2 <- median(group2))

# Get the sample mean from each group

Mean.M1 <- mean(M1)
Mean.M2 <- mean(M2)

# Obtain sample variance

s.squared <- sum((M1-Mean.M1)^2)+sum((M2-Mean.M2)^2)/(n-2)

# Sqrt of variance gives us the error

s <- sqrt(s.squared)

# Get the bf statistic, I'm paranoid about parenthesis

bf.stat <- (Mean.M1 - Mean.M2)/(s*sqrt(1/length(group1) + 1/length(group2)))

##group2$Residuals
##typeof(group1)

# Get critical values for the t-distribution and compare
## qt(1-alpha/4, n-2)

# If bf.stat < critical-t value accept H0, error of variance is constant


















# Working - Hotelling Procedure


# Perform regression and decide our alpha value, here let's say alpha = .05

alpha <- .05

reg3 <- lm(toluca$Hours ~ toluca$Size)

anova(reg3)


# Save the MSE, taken from anova(reg3), it's 4.48 here

MSE <- 4.48

# Get b0 and b1 to find Yh for each Xh provided

# Here Yh= 6.86349 + .53327Xh, change values in the function below as needed
## Say Xh = 10, 15, 25 for this problem, start with Xh = 10

xh <- 10

sample.yh <- function(x)  {
yh <- 6.86349 + .53327*xh
}


## wh is for Working-Hotelling, probably could name them something better, but w/e

yh.wh <- sample.yh(xh)
yh.wh


# Obtain W
## W^2 <- 2F(1-alpha,2,df2) is done using qf()

f.crit <- qf(.9875, 1, 109)

w.squared <- 2*f.crit

w <- sqrt(w.squared)

# Get the length of X or Y here to get n. either works. s.wh here is the complicated part of the sample variance formula

s.wh2 <- (1/length(toluca$Size)+(xh-mean(toluca$Size))^2)/(sum((toluca$Size - mean(toluca$Size))^2))

s.wh.square <- MSE*s.wh2

s.wh <- sqrt(s.wh.square)



# Now we can calculate our CI for yh


lower.bound.wh <- (yh - (w*s.wh))
upper.bound.wh <- (yh + (w*s.wh))






########## Starting using data from the practice midterm at this point ############






# Bonferroni Procedure



# Still use the original set of steps we used for Working-Hotelling procedure
## xh = 3, 4, 5     g = 3
### t(1-alpha/2g ;  n-2),  qt(.983,7)

t.crit <- qt(.983,7)


s.b2 <- (1/length(Prac$Exposure)+(xh-mean(Prac$Exposure))^2)/(sum((Prac$Exposure - mean(Prac$Exposure))^2))

s.b.square <- MSE*s.b

s.b <- sqrt(s.b.square)



lower.bound.b <- (yh - (b*s.b))
upper.bound.b <- (yh + (b*s.b))










# Bonferroni Procedure



# Still use the original set of steps we used for Working-Hotelling procedure
## xh = 3, 4, 5     g = 3
### t(1-alpha/2g ;  n-2),  qt(.983,7)
##SAME MSE/REGRESSIONS
MSE <- 196.3
xh <- 3

sample.b <- function(x)  {
yh <- 6.86349 + .53327*xh
}

sample.b(xh)


t.crit <- qt(.983,7)


s.b2 <- (1/length(Prac$Exposure)+(xh-mean(Prac$Exposure))^2)/(sum((Prac$Exposure - mean(Prac$Exposure))^2))

s.b.square <- MSE*s.b

s.b <- sqrt(s.b.square)



lower.bound.b <- (yh - (b*s.b))
upper.bound.b <- (yh + (b*s.b))















# Regression through origin
## regression <- lm(y ~ x - 1)







# Regression through origin


reg.o <- lm(Prac$Mortality ~ Prac$Exposure - 1)
summary(reg.o)
anova(reg.o)



## Regression through origin estimates of variance/confidence limits

## still, Yh = b1*x + e

### MSE <- value of MSE from regression through origin


#### t = t(1-alpha/2;n-1)  so the code is:   qt(1-alpha/2,n-1) where n is the length of your x or y vector

# b1 estimate

s.squared.b1 <- function(x) {
sb1 <- MSE/sum((Prac$Exposure)^2)
}



# E(Yh) estimate

s.squared.yh <- function(x) {
syh <- (xh^2)*MSE/sum((Prac$Exposure)^2)
}




# Yhnew estimate



s.squared.yhnew <- function(x)  {
syhnew <- MSE*((1+(xh^2))/(sum(Prac$Exposure^2)))
}