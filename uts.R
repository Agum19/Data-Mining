#SVM
#Library
library(tidyverse)
library(e1071)
library(caret)
library(readr)
library(dplyr)


#Reading Data
data <- read.delim("D:/Agum/KULIAH/SEMESTER 5/Data Mining/UTS/Ujian/echocardiogram.txt", 
                   header=F, sep=",", dec=".")
data
str(data)

#Preprocessing Data
#Mengubah menjadi Data Frame
data <- data.frame(data)
#Menampilkan Data
View(data)
#Kolom 10,11,12 tidak digunakan karena dapat di abaikan
data <- data[,-c(10,11,12)] 
#Mengganti Missing value
data <- na_if(data, "?")
data <- na_if(data, "")
#Data ke 50 dan 51 dihapus karena terlalu banyak missing value nya
data <- data[-c(50,51),] 


#Mengubah data menjadi numeric dan kategorik
data$V1 <- as.numeric(data$V1)
data$V3 <- as.numeric(data$V3)
data$V5 <- as.numeric(data$V5)
data$V6 <- as.numeric(data$V6)
data$V7 <- as.numeric(data$V7)
data$V8 <- as.numeric(data$V8)
data$V9 <- as.numeric(data$V9)
data$V2 <- as.factor(data$V2)
data$V4 <- as.factor(data$V4)
data$V13 <- as.factor(data$V13)


#Mengganti nilai pada missing value
data <- data %>%
  mutate(V1 = replace(V1,
                      is.na(V1),
                      mean(V1, na.rm = T)))

data <- data %>%
  mutate(V3 = replace(V3,
                      is.na(V3),
                      mean(V3, na.rm = T)))

data <- data %>%
  mutate(V5 = replace(V5,
                      is.na(V5),
                      mean(V5, na.rm = T)))

data <- data %>%
  mutate(V6 = replace(V6,
                      is.na(V6),
                      mean(V6, na.rm = T)))

data <- data %>%
  mutate(V7 = replace(V7,
                      is.na(V7),
                      mean(V7, na.rm = T)))

data <- data %>%
  mutate(V8 = replace(V8,
                      is.na(V8),
                      mean(V8, na.rm = T)))

data <- data %>%
  mutate(V9 = replace(V9,
                      is.na(V9),
                      mean(V9, na.rm = T)))
#mencari nilai modus dari data
table(data$V4)
table(data$V2)
table(data$V13)
#terlihat bahwa nilai 0 merupakan modus pada data.

#mengganti missing Value
data <- data %>%
  mutate(V2 = replace(V2,
                      is.na(V2),
                      0))

data <- data %>%
  mutate(V4 = replace(V4,
                      is.na(V4),
                      0))

data <- data %>%
  mutate(V13 = replace(V13,
                      is.na(V13),
                      0))



#Menentukan Data Test dan Data Training
set.seed(42) # angka random
sampling <- sample(1:nrow(data), 0.8*nrow(data))
training_set <- data[sampling,]
test_set <- data[-sampling,]


modelSVM <- svm(V13~., data=training_set)
summary(modelSVM)

pred <- predict(modelSVM, test_set)
confusionMatrix(table(Predicted = pred, Actual=test_set$V13))


pred <- predict(modelSVM, )


set.seed(123)

#membuat model
library(base)
modelterbaik <- tune(svm, V13~., data=training_set,
               ranges = list(epsilon = seq(0,1,0.1),
                             cost = 2^(2:9)))  
summary(modelterbaik)
modelterbaik

bestmodel <- modelterbaik$best.model
bestmodel

#Evaluasi
pred <- predict(bestmodel, test_set)
confusionMatrix(table(Predicted = pred, Actual=test_set$V13))


recall(pred, test_set$V13)
precision(pred, test_set$V13)
F_meas(pred, test_set$V13)

