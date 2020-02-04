library(dplyr)
library(waveslim)
setwd("C:\\projectdata") # 경로 지정

raw <- read.csv("three.csv", header=T) # csv 읽기
sum(is.na(raw)) # 결측치 확인해본 결과 결측치 없음
mean <- data.frame(raw$Max) # mean 불러오기
mean <- abs(mean) # 고장은 0보다 크거나 작아야하므로 절대값을 씌우

# 1m/s의 차이가 있을 경우 가정
# 두 풍속의 차이이므로 차이가 1m/s 미만일 경우 고장이 아니고 이상일 경우 고장이라고 판단
normal <- as.matrix(filter(mean, raw.Max<=1)) 
anormal <- as.matrix(filter(mean, raw.Max>1)) 

# train, test set으로 미리 나눔. test는 anormal의 갯수와 비슷하게 설정.
ran_index <- sample(x=c("train", "test"), size=nrow(normal), replace=TRUE, prob=c(0.95, 0.05)) 
normal_train <- normal[ran_index=="train"]
normal_test <- normal[ran_index=="test"]

# 특징추출하기위해 데이터 60개씩 구간으로 나타냄(10시간)
# 6개씩 나누고 나온 나머지들은 제거
num <- 64
normal_train <- array(normal_train, c(num,floor(length(normal_train)/num)))
normal_test <- array(normal_test, c(num,floor(length(normal_test)/num)))
anormal <- array(anormal, c(num,floor(length(anormal)/num)))
# Spectrum 방법을 이용한 특징 추출 함수 
Spectrum = function(x,level,func){
  
  value = matrix(0,0,nrow=ncol(x),ncol=2)
  for(i in 1:ncol(x)){
    temp = dwt(x[,i],func,level)
    x.axis = 1:level
    y.axis = c()
    for(j in 1:level){
      y.axis[j] = log2(mean(temp[[j]]^2))
    }
    reg = lm(y.axis ~ x.axis)
    value[i,] = coef(reg)
  }
  
  return(value)
}

# 각 구간에 대해 최대값과 평균값을 특징으로 추출하는 함수   
Feature_Max_Mean = function(x){
  value = matrix(0,0,nrow=ncol(x), ncol=2)
  for(i in 1:ncol(x)){
    r_max <- max(x[,i])
    r_mean <- mean(x[,i])
    value[i,] <- c(r_max, r_mean)
  }
  return(value)
}

# train 데이터와 test 데이터에 대해서 Spectrum 특징추출 이후 plotting
train = Spectrum(normal_train,3,"haar")
colnames(train) <- c("x1", "x2")
test_normal = Spectrum(normal_test,3,"haar")
test_anormal = Spectrum(anormal,3,"haar")

plot(train[,1], train[,2], col='5', xlim=c(-20,10), ylim=c(-3,3), xlab="x1", ylab="x2")
par(new=T)
plot(test_anormal[,1], test_anormal[,2], col='3', xlim=c(-20,10), ylim=c(-3,3), xlab="", ylab="")
par(new=T)
plot(test_normal[,1], test_normal[,2], col='4', xlim=c(-20,10), ylim=c(-3,3), xlab="", ylab="")
legend("topright", legend=c("train", "test_anormal", "test_normal"), pch=c(20, 43), col=c(5, 3, 4), bg="gray")

# train 데이터와 test 데이터에 대해서 max값과 mean값을 통해 특징추출 이후 plotting
train = Feature_Max_Mean(normal_train)
colnames(train) <- c("x1", "x2")
test_normal = Feature_Max_Mean(normal_test)
test_anormal = Feature_Max_Mean(anormal)

plot(train[,1], train[,2], col='5', xlim=c(0,10), ylim=c(0,2), xlab="x1", ylab="x2")
par(new=T)
plot(test_anormal[,1], test_anormal[,2], col='3', xlim=c(0,10), ylim=c(0,2), xlab="", ylab="")
par(new=T)
plot(test_normal[,1], test_normal[,2], col='4', xlim=c(0,10), ylim=c(0,2), xlab="", ylab="")
legend("topright", legend=c("train", "test_anormal", "test_normal"), pch=c(20, 43), col=c(5, 3, 4), bg="gray")




#######분석
library(e1071)

y <- rep(1,length(train[,1]))
data <- cbind(train,y)

gamma <- c(1/2,1/4,1/8,1/16,1/32)
nu <- c(1/2,1/4,1/8,1/16,1/32)
# 각 gamma와 nu값에 따른 테스트 결과 출력
for(i in 1:length(gamma)){
  for(j in 1:length(nu)){
    model <- svm(y~., data=data ,type='one-classification', gamma=gamma[i], nu=nu[j]) #ocsvm 
    pred <- predict(model, test_normal)
    pred2 <- predict(model, test_anormal)
    result <- c(sum(pred)/dim(test_normal)[[1]], (dim(test_anormal)[[1]]-sum(pred2))/dim(test_anormal)[[1]])
    tab <- rbind(c(gamma[i], nu[j]), result)
    rownames(tab) <- c("parameter", "accuracy")
    colnames(tab) <- c("gamma", "nu")
    print(tab)    
  }
}

model1 <- svm(y~., data=data ,type='one-classification', gamma=0.0625, nu=0.03125) # Mean_Spectrum
model2 <- svm(y~., data=data ,type='one-classification', gamma=0.25, nu=0.125) # Max_Spectrum
model3 <- svm(y~., data=data ,type='one-classification', gamma=0.125, nu=0.125) # Min_Spectrum

model4 <- svm(y~., data=data ,type='one-classification', gamma=0.25, nu=0.03125) # Mean_MaxMean
model5 <- svm(y~., data=data ,type='one-classification', gamma=0.0625, nu=0.0625) # Max_MaxMean
model6 <- svm(y~., data=data ,type='one-classification', gamma=0.03125, nu=0.03125) # Min_Spectrum

