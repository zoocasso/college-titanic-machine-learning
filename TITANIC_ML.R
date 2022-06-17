#install library
install.packages("stringr")
library(stringr)
library(tidyr)
library(dplyr)
library(caret)
library(ggplot2)

#데이터 정제
trainCSV=read.csv("/Users/zoocasso/Desktop/TITANIC_ML/csv/train.csv",header=TRUE,sep=',',as.is=TRUE)
testCSV=read.csv("/Users/zoocasso/Desktop/TITANIC_ML/csv/test.csv",header=TRUE,sep=',',as.is=TRUE)
sampleCSV=read.csv("/Users/zoocasso/Desktop/TITANIC_ML/csv/sample_submission.csv",header=TRUE,sep=',',as.is=TRUE)

trainCSV$Survived = factor(trainCSV$Survived)
trainCSV$Sex = factor(trainCSV$Sex)
trainCSV[is.na(trainCSV)] <- 0
testCSV[is.na(testCSV)] <- 0

# 데이터 가공 및 시각화
trainCSV %>% ggplot(aes(Age,Survived))+geom_jitter(alpha = 0.5)
trainCSV %>% ggplot(aes(Sex,Survived))+geom_jitter(alpha = 0.5)
trainCSV %>% ggplot(aes(Pclass,Survived))+geom_jitter(alpha = 0.5)
trainCSV %>% ggplot(aes(Parch,Survived))+geom_jitter(alpha = 0.5)
trainCSV %>% ggplot(aes(SibSp,Survived))+geom_jitter(alpha = 0.5)
trainCSV %>% ggplot(aes(Fare,Survived))+geom_jitter(alpha = 0.5)

trainCSV %>% ggplot(aes(Pclass,Survived,col=Sex))+geom_jitter(alpha = 0.5) #의미있는 결과예시

# 모델선택
control=trainControl(method='cv',number=10)
#METHOD='svmLinear'
#METHOD='svmLinearWeights'
#METHOD='svmRadial'
#METHOD='svmRadialWeights'
#METHOD='svmPoly'
#METHOD='rpart'
#METHOD='knn'
#METHOD='glm'

sR1=train(Survived~Pclass, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR2=train(Survived~Sex, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR3=train(Survived~Age, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR4=train(Survived~Parch, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR5=train(Survived~SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR6=train(Survived~Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)

sR7=train(Survived~Pclass+Sex, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR8=train(Survived~Pclass+Age, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR9=train(Survived~Pclass+Parch, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR10=train(Survived~Pclass+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR11=train(Survived~Pclass+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR12=train(Survived~Sex+Age, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR13=train(Survived~Sex+Parch, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR14=train(Survived~Sex+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR15=train(Survived~Sex+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR16=train(Survived~Age+Parch, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR17=train(Survived~Age+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR18=train(Survived~Age+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR19=train(Survived~Parch+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR20=train(Survived~Parch+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR21=train(Survived~SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)

sR22=train(Survived~Pclass+Sex+Age, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR23=train(Survived~Pclass+Sex+Parch, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR24=train(Survived~Pclass+Sex+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR25=train(Survived~Pclass+Sex+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR26=train(Survived~Pclass+Age+Parch, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR27=train(Survived~Pclass+Age+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR28=train(Survived~Pclass+Age+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR29=train(Survived~Pclass+Parch+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR30=train(Survived~Pclass+Parch+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR31=train(Survived~Pclass+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR32=train(Survived~Sex+Age+Parch, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR33=train(Survived~Sex+Age+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR34=train(Survived~Sex+Age+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR35=train(Survived~Sex+Parch+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR36=train(Survived~Sex+Parch+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR37=train(Survived~Sex+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR38=train(Survived~Age+Parch+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR39=train(Survived~Age+Parch+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR40=train(Survived~Age+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR41=train(Survived~Parch+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)

sR42=train(Survived~Pclass+Sex+Age+Parch, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR43=train(Survived~Pclass+Sex+Age+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR44=train(Survived~Pclass+Sex+Age+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR45=train(Survived~Pclass+Sex+Parch+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR46=train(Survived~Pclass+Sex+Parch+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR47=train(Survived~Pclass+Sex+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR48=train(Survived~Pclass+Age+Parch+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR49=train(Survived~Pclass+Age+Parch+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR50=train(Survived~Pclass+Age+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR51=train(Survived~Pclass+Parch+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR52=train(Survived~Sex+Age+Parch+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR53=train(Survived~Sex+Age+Parch+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR54=train(Survived~Sex+Age+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR55=train(Survived~Sex+Parch+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR56=train(Survived~Age+Parch+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)

sR57=train(Survived~Pclass+Sex+Age+Parch+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR58=train(Survived~Pclass+Sex+Age+Parch+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR59=train(Survived~Pclass+Sex+Age+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR60=train(Survived~Pclass+Sex+Parch+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR61=train(Survived~Pclass+Age+Parch+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)
sR62=train(Survived~Sex+Age+Parch+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)

sR63=train(Survived~Pclass+Sex+Age+Parch+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control)

# 랜덤포래스트 모델 선택
#METHOD='rf'
#NTREE=100
#NTREE=300
#NTREE=500

sR1=train(Survived~Pclass, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR2=train(Survived~Sex, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR3=train(Survived~Age, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR4=train(Survived~Parch, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR5=train(Survived~SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR6=train(Survived~Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)

sR7=train(Survived~Pclass+Sex, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR8=train(Survived~Pclass+Age, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR9=train(Survived~Pclass+Parch, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR10=train(Survived~Pclass+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR11=train(Survived~Pclass+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR12=train(Survived~Sex+Age, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR13=train(Survived~Sex+Parch, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR14=train(Survived~Sex+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR15=train(Survived~Sex+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR16=train(Survived~Age+Parch, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR17=train(Survived~Age+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR18=train(Survived~Age+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR19=train(Survived~Parch+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR20=train(Survived~Parch+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR21=train(Survived~SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)

sR22=train(Survived~Pclass+Sex+Age, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR23=train(Survived~Pclass+Sex+Parch, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR24=train(Survived~Pclass+Sex+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR25=train(Survived~Pclass+Sex+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR26=train(Survived~Pclass+Age+Parch, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR27=train(Survived~Pclass+Age+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR28=train(Survived~Pclass+Age+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR29=train(Survived~Pclass+Parch+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR30=train(Survived~Pclass+Parch+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR31=train(Survived~Pclass+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR32=train(Survived~Sex+Age+Parch, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR33=train(Survived~Sex+Age+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR34=train(Survived~Sex+Age+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR35=train(Survived~Sex+Parch+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR36=train(Survived~Sex+Parch+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR37=train(Survived~Sex+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR38=train(Survived~Age+Parch+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR39=train(Survived~Age+Parch+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR40=train(Survived~Age+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR41=train(Survived~Parch+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)

sR42=train(Survived~Pclass+Sex+Age+Parch, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR43=train(Survived~Pclass+Sex+Age+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR44=train(Survived~Pclass+Sex+Age+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR45=train(Survived~Pclass+Sex+Parch+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR46=train(Survived~Pclass+Sex+Parch+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR47=train(Survived~Pclass+Sex+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR48=train(Survived~Pclass+Age+Parch+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR49=train(Survived~Pclass+Age+Parch+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR50=train(Survived~Pclass+Age+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR51=train(Survived~Pclass+Parch+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR52=train(Survived~Sex+Age+Parch+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR53=train(Survived~Sex+Age+Parch+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR54=train(Survived~Sex+Age+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR55=train(Survived~Sex+Parch+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR56=train(Survived~Age+Parch+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)

sR57=train(Survived~Pclass+Sex+Age+Parch+SibSp, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR58=train(Survived~Pclass+Sex+Age+Parch+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR59=train(Survived~Pclass+Sex+Age+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR60=train(Survived~Pclass+Sex+Parch+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR61=train(Survived~Pclass+Age+Parch+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)
sR62=train(Survived~Sex+Age+Parch+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)

sR63=train(Survived~Pclass+Sex+Age+Parch+SibSp+Fare, data=trainCSV, method=METHOD,metric='Accuracy', trControl=control, ntree=NTREE)

resamp=resamples(list(r1=sR1, r2=sR2, r3=sR3, r4=sR4,r5=sR5,r6=sR6,r7=sR7,r8=sR8,r9=sR9,
                       r10=sR10, r11=sR11, r12=sR12, r13=sR13, r14=sR14, r15=sR15, r16=sR16, r17=sR17, r18=sR18, r19=sR19,
                       r20=sR20, r21=sR21, r22=sR22, r23=sR23, r24=sR24, r25=sR25, r26=sR26, r27=sR27, r28=sR28, r29=sR29,
                       r30=sR30, r31=sR31, r32=sR32, r33=sR33, r34=sR34, r35=sR35, r36=sR36, r37=sR37, r38=sR38, r39=sR39,
                       r40=sR40, r41=sR41, r42=sR42, r43=sR43, r44=sR44, r45=sR45, r46=sR46, r47=sR47, r48=sR48, r49=sR49,
                       r50=sR50, r51=sR51, r52=sR52, r53=sR53, r54=sR54, r55=sR55, r56=sR56, r57=sR57, r58=sR58, r59=sR59,
                       r60=sR60, r61=sR61, r62=sR62, r63=sR63))
sort(resamp, decreasing = TRUE)
dotplot(resamp)

#예측결과
predictTestCSV=predict(sR44, newdata=testCSV, na.action=na.pass)

PassengerId = testCSV$PassengerId
Survived = predictTestCSV
resultCSV = data.frame(PassengerId,Survived)
write.csv(resultCSV, file="/Users/zoocasso/Desktop/TITANIC_ML/resultCSV.csv",row.names=FALSE)
table(resultCSV$Survived)
