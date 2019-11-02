
Online_Retail <- read_excel("C:/Users/ludai/Desktop/Ludai_File/MSBA/BAN620Data Mining/Lab2/Online Retail.xlsx", 
                            col_types = c("text", "text", "text", 
                            "numeric", "date", "numeric", "numeric", 
                             "text"))
library(readxl)
Online_Retail <- read_excel("C:/Users/ludai/Desktop/Ludai_File/MSBA/BAN620Data Mining/Lab2/Online Retail.xlsx", 
                            col_types = c("text", "text", "text", 
                                          "numeric", "date", "numeric", "numeric", 
                                          "text"))

names(Online_Retail)
#Summary of dataset
summary(Online_Retail)
#Observe class type of Country
# Set it as factor
Online_Retail$Country <- as.factor(Online_Retail$Country)
#Check summary again
# How about invoice no?

testInvNo <- as.numeric(Online_Retail$InvoiceNo)
# Where are the NA's
which(is.na(testInvNo), arr.ind=TRUE)
# How many records
length(testInvNo)
# How many NA's
summary(testInvNo)

# convert to factor and then numeric
testInvNo <- as.factor(Online_Retail$InvoiceNo)
head(testInvNo, n=20)
testInvNo <- as.numeric(testInvNo)

# On dataset

Online_Retail$InvoiceNo <- as.factor(Online_Retail$InvoiceNo)
head(Online_Retail[,c(1,3,8)])
ORSample <- Online_Retail[1:500,c(1,3,8)]
ORSampleCT <- ORSample[,-2]
ORSampleCTUQ <- unique(ORSampleCT)
rm(ORSampleCT)
names(ORSampleCTUQ) <- c("InvoiceNo", "Description")
names(ORSampleCTUQ)
names(ORSample)
ORSample2 <- rbind(ORSample[,1:2], ORSampleCTUQ)
ORSample[,c(1,3)]
tail(ORSample2)
ORSample2[order(ORSample2$InvoiceNo),]
write.csv(ORSample2[order(ORSample2$InvoiceNo),], "online.csv")


library(arules)
ortr <- read.transactions(file="online.csv", format="single", sep=",", cols=c("InvoiceNo", "Description") )
# rm(ortr) remove objects
rules <- apriori(ortr, parameter = list(support = 0.01, confidence = 0.9, maxlen=3))
summary(rules)
inspect(head(sort(rules, by ="lift"),3))

