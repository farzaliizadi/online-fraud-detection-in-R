getwd()
setwd("C:/Users/Izadi/Documents")

#1. Reading Data
df = read.csv("dfCSV.csv")
df
is.data.frame(df)
dim(df)     # 628 12
str(df)
#2. Transfers.fraud_flag should be factor but it is integer
#3.Transfers.empty_communication_flag should be factor but it is integer

fix(df)     # Shows data in a clear table
names(df)
row.names(df)
head(df, n=5)
tail(df, n=5)
df$transfers.fraud_flag                 # To see fraud and non_fraud column
df$transfers.empty_communication_flag   # To see one more column separately 
#4. To see if there is a NA (missing values)
is.na(df[,1])
is.na(df$transfers.fraud_flag)
is.na(df)
# As separating TRUE from FALSE for each column or whole data.frame  is difficult,
#it is better to get sum.
sum(is.na(df))
#sum=0  shows that there is no NA values in whole data set.
#5. ploting amount, fraud and original balance with respect to time
plot(df$transfers.timestamp,df$transfers.amount)
plot(df$transfers.timestamp,df$transfers.fraud_flag)
plot(df$transfers.timestamp,df$transfers.orig_balance_before)
plot(df)
#6. Also to see how correlated the numeric variable.
cor(df$transfers.amount, df$transfers.fraud_flag)            # 0.5344391 positive correlation
cor(df$transfers.timestamp,df$transfers.amount)              # 0.06707493 positive but small
cor(df$transfers.timestamp,df$transfers.fraud_flag)          # 0.2045278 positive
cor(df$transfers.timestamp,df$transfers.orig_balance_before) #  0.01514717 positive but small
#7. Since there are 12 variavles and column names are longer, head and tail don't 
# show up  in one page I want to see every 3 columns consecutively.
#8.
head(df[,1])              # col-1 = transfers.fraud_flag   which is  (fraud, non-fraud)=(1, 0)  
head(df[,3])              # col-3 = transfers.timestamp   which is time and numeric.
head(df[, 10:12])         # col-10 = transfers.empty_communication_flag which is 
                          #(non-empty, empty)=(1, 0)
                          # col-11 = transfers.orig_balance_before
                          # col-12 = transfers.amount which is numeric.

#9. I wish to see the column names and 6 obsevations in a row format. 
head(df$transfers.fraud_flag)
head(df$transfers.timestamp)             
head(df$transfers.empty_communication_flag)
head(df$transfers.amount)
head(df$transfers.orig_balance_before)

#10. All other columns are characters.
head(df[,2]) 
head(df[,4:5])
head(df[,6:9])

#11. Let us get summary for only numeric columns and 
#variables of  col-3, col-11, and col-12.
summary(df[,1])
summary(df[,3])
summary(df[,10:12])

#12. Next we subsetting fraud from non-fraud cases and get the numbers and pecentages.
S1 <- subset(df, df$transfers.fraud_flag==1)
S1
names(S1)


#13. Determine the sum of legitimate and fraudulent cases.
s1<- sum(df$transfers.fraud_flag==1) 
s1                                         #  number of frauds = 14 
s0 <- sum(df$transfers.fraud_flag==0)  
s0                                         #  number of non-frauds =614
ps1 <- s1/(s1+s0)                      
ps1                                        # % of frauds = 0.02229299
ps0 <- s0/(s1+s0)                        
ps0                                        # % of non-frauds = 0.977707 

v1 <- c(1, 0)                              # vector of fraud and non-fraud
class_distribution <- c(ps1, ps0)
class_distribution 
names(class_distribution) <- v1
#14. Determine fraction of legitimate and fraudulent cases
class_distribution
#15. Here we get the 14 cases for stolen amount
S1$transfers.amount 

#16.Total amount stolen
Total_amount <- sum(S1$transfers.amount)
Total_amount
print(paste("The total stolen amount of money for the"
            ,s1,"fraudulent cases is equal to $",Total_amount))

#17 Hist Chart of frequency of NOn-FRAUD and FRAUD
hist(rep(0:1, c(614,14)), col=c("dodgerblue", "red") ,
     xlab= " NUMERS OF OBSEVATIONS", ylab ="FREQUENCY OF FRAUD NON-FRAUD",
     main = "Frequency  of fraudulent Cases")
legend(.7, 500, legend=c("1", "0"),
       col=c("red", "blue"), lty=1:1, cex=0.8)

#18. Pie chart  of fraudulent Cases 
slices <- c(ps1,ps0)
lbls   <- c("FRAUD", "NON-FRAUD")
pie(slices,labels =lbls, col=c("dodgerblue", "red") 
    ,main = "Pie Chatrt of fraudulent Cases")
pct = as.numeric(class_distribution)
df1 <- data.frame(levels = c("1", "0"), pct) 
df1
#19. Creting data.frame df2 having only binay column {1,0} and all other numeric columns
# by droping the character columns.
df2 <- df[-c(2,4,5,6,7,8,9)]
names(df2)
head(df2)
S1$transfers.amount
#20. Building model with Generalized logistic regression
set.seed(100)
logistic <- glm(transfers.fraud_flag ~ . , data=df2, family="binomial")
summary(logistic)

#21. We can see all these information in console: 
# In Coefficients: Estimate column gives the logistic function parameters.

#  b0=-1.036e+01  b1=1.189e-07   b2= 4.326e+00 b3=1.686e-05 b4= 5.209e-04

# *** means %100  statistically siginificant.
# **  means 99.9  statistically siginificant.
# *   means 99.5  statistically siginificant.
#     means not   statistically siginificant.

#22. Now x=b0 + b1*transfers.timestamp  + b1*transfers.empty_communication_flag
#     + b2*transfers.orig_balance_before +b3*transfers.amount 
e <- exp(1)
# y=logit(x)=1/(1+e^-x)
# From each row we pick up  transfers.amount and transfers.timestamp and plug in x
# Then get y, if y< 0.5 that row is non-fraud, if y >= 0.5 then that row is fraud.

b0 <- -1.036e+01
b1 <-1.189e-07   ;x1 <-  "transfers.timestamp" 
b2 <-  4.326e+00  ;x2 <- "transfers.empty_communication_flag"
b3 <- 1.686e-05  ;x3 <- "transfers.orig_balance_before"
b4 <- 5.209e-04  ; x4 <- "transfers.amount" 
# x <- b0+b1*x1+b2*x2+b3*x3+b4*x4

w0 <- c(1, 1, 1, 1, 1 ,1, 1, 1, 1, 1, 1 ,1, 1, 1) 
#23.  This column used for multply to intercpe in oder not 
# to change its value for each ovservation.
B <- c(b0,b1,b2,b3,b4)
#*********************************************************************************************
# 24. In this step, we pick up thoses observations having fraud cases 
#and try to see how many fraud cases the logistic function predicts correctly. 
# The following obsevations are all from the fraud rows.
transfers.fraud_flag= f1 <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1)

transfers.timestamp = u1 <- c(33341120, 49435837, 33022824, 38296341, 37775151, 46914060, 
        49436512, 49436080, 33340865, 37763682, 44165127,36880588, 49436390, 10704152)

transfers.empty_communication_flag = u2 <- c(0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

transfers.orig_balance_before = u3 <- c( 54653, 29407, 26839, 1987, 1186, 10233, 10044
                                   , 22424, 55986,  2510, 6537, 7152, 17662, 4290)

transfers.amount = u4 <- c(1514, 8537, 7616, 1531, 809, 1793, 9312, 5824, 1638, 1627
                           , 3522, 7596, 9312, 3779)

M  <- cbind(w0,u1,u2,u3,u4)
M
B <- c(-1.036e+01, 1.189e-07, 4.326e+00, 1.686e-05, 5.209e-04)
X <- M%*%B
x <- as.vector(X)   #I wish to see the values as row format not column format.
x
#25.  This step computes the logistic values for fraud cases.
u <- c()
for (x in X){
  u <- union(u, c((1/(1+exp(1)^(-x)))))
}
print(u)

#[1] 0.009142393 0.991728605 0.909873695 0.343087269 0.249528135 0.657142012 0.992339317 0.962878830
#[9] 0.432265722 0.342071764 0.761706322 0.918970522 0.993256360 0.061776461

#24.  As we see 8 out of 14 cases are >=0.5 that is the glm model predicts those correctly and 
#the remaings are misclassified by the model. Next step to see how it predicts non-fraud cases.

transfers.fraud_flag = f0 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

transfers.timestamp =  v1 <- c(21966103, 40885557, 19945191, 27404301, 6566236, 17576922, 29583007
                    , 14857126, 22221450, 38214048, 18146865, 9332683, 14701823, 11785034)

transfers.empty_communication_flag = v2 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) 

transfers.orig_balance_before = v3 <- c(5412, 7268, 1971, 10603, 6228, 4933
                  , 1779, 1866, 4582, 6218, 1728, 6707, 1801, 2393)     

transfers.amount = v4 <- c(33, 40, 227, 20, 5176, 54, 71, 27, 28, 59, 35, 82, 85, 16)

N  <- cbind(w0,v1,v2,v3,v4)
N
B <- c(-1.036e+01, 1.189e-07, 4.326e+00, 1.686e-05, 5.209e-04)
Z <-  N%*%B
z <- as.vector(Z)
z

v <- c()

for (z in Z){
  v <- union(v, c((1/(1+exp(1)^(-z)))))
}
print(v)
#25. As we see from the logistic function values, they are all < 0.5 which shows that the glm model
# predicts all the values correctly as non-fraud cases. 
#[1] 0.0004806962 0.0047006966 0.0003946796 0.0009943325 0.0011371503 0.0002861223 0.0011399815 0.0001939022
#[9] 0.0004873542 0.0033992985 0.0002872208 0.0001122634 0.0001959781 0.0001350029
#*****************************************************************************************************






