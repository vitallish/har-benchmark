rm(list=ls())

require(caret)
library(plyr);library(dplyr)
require(doSNOW)

load(file.path("data","explored.R"))

#Preprocessing----
train_classe<-train_sub$classe
train_sub<-train_sub[,-col_rm]
dummyVar<-dummyVars(classe~.,train_sub)
train_sub<-predict(dummyVar,train_sub)

nearZeroVar(train_sub)

pp_cent_scale<- preProcess(train_sub[,-c(1:6)],method=c("scale","center"))
train_sub[,-c(1:6)] <- predict(pp_cent_scale,train_sub[,-c(1:6)])

# Tune ----
set.seed(12345)
randSamp<-sample(1:nrow(train_sub),5000)
cl<-makeCluster(4,type="SOCK")
registerDoSNOW(cl)

set.seed(1234)
gbmMod5K<-train(x=train_sub[randSamp,],y=train_classe[randSamp],method="gbm",
                tuneLength=10,
                trControl= trainControl(method="cv",
                                        allowParallel = T, seeds = NULL))

stopCluster(cl)
invisible(gc())

# Evaluate ----
mrow<-which.max(gbmMod5K$results$Kappa)
gbmMod5K$results[gbmMod5K$results$Kappa > (gbmMod5K$results$Kappa - gbmMod5K$results$KappaSD)[mrow],] %>%
    ggplot(aes(x=interaction.depth, y = Kappa),data=.) + geom_line(aes(x=interaction.depth, y = Kappa,color=as.factor(n.trees)))

# judging by this and choosing the "simplest model" within 1 SD an interaction.depth of 5, and n.trees= 150
#retrain with these params

gbmModFinal<-train(x=train_sub[randSamp,],y=train_classe[randSamp],method="gbm",
                   tuneGrid=data.frame(shrinkage = 0.1,interaction.depth = 5, n.trees=150),
                   trControl= trainControl(method="none",
                                           allowParallel = F, seeds = NULL))

gbmPredict<-predict(gbmModFinal, train_sub[-randSamp,]) == train_classe[-randSamp]
mean(gbmPredict)
# this model scales really well with 96% accuracy on new samples.


#let's train on more of the training set and predict again. How do parameters hold up
set.seed(12345)
randSamp<-sample(1:nrow(train_sub),10000)
gbmModFinal<-train(x=train_sub[randSamp,],y=train_classe[randSamp],method="gbm",
                   tuneGrid=data.frame(shrinkage = 0.1,interaction.depth = 5, n.trees=150),
                   trControl= trainControl(method="none",
                                           allowParallel = T, seeds = NULL))

gbmPredict<-predict(gbmModFinal, train_sub[-randSamp,]) == train_classe[-randSamp]
mean(gbmPredict)

#excellent, accuracy actually just went up! Now we train on the full train_sub sample to make the final gbmModel----
gbmModFinal<-train(x=train_sub,y=train_classe,method="gbm",
                   tuneGrid=data.frame(shrinkage = 0.1,interaction.depth = 5, n.trees=150),
                   trControl= trainControl(method="none",
                                           allowParallel = T, seeds = NULL))


# Let's also look an SVM Radial model ----

set.seed(12345)
randSamp<-sample(1:nrow(train_sub),5000)
cl<-makeCluster(4,type="SOCK")
registerDoSNOW(cl)

set.seed(1234)
svmPredict5k<-train(x=train_sub[randSamp,],y=train_classe[randSamp],method="svmRadial",
                tuneLength=10,metric="Kappa",
                trControl= trainControl(method="cv",
                                        allowParallel = T, seeds = NULL))

stopCluster(cl)
invisible(gc())

plot(svmPredict5k)

mrow<-which.max(svmPredict5k$results$Kappa)
svmPredict5k$results[svmPredict5k$results$Kappa > (svmPredict5k$results$Kappa - svmPredict5k$results$KappaSD)[mrow],]

#choose C based on 32
svmPredictFinal<-train(x=train_sub[randSamp,],y=train_classe[randSamp],method="svmRadial",
                   tuneGrid=data.frame(C=32, sigma=0.01071563),
                   trControl= trainControl(method="none",
                                           allowParallel = T, seeds = NULL))
svmPredict<-predict(svmPredictFinal, train_sub[-randSamp,]) == train_classe[-randSamp]
mean(svmPredict)
#96.7%

#use full set
svmPredictFinal<-train(x=train_sub,y=train_classe,method="svmRadial",
                       tuneGrid=data.frame(C=32, sigma=0.01071563),
                       trControl= trainControl(method="none",
                                               allowParallel = T, seeds = NULL))


# Finally, let's predict on a simple tree to see how well we can do with an interpertable model.

cl<-makeCluster(4,type="SOCK")
registerDoSNOW(cl)

set.seed(1234)
cartPredict5k<-train(x=train_sub,y=train_classe,method="rpart",
                    tuneLength=10,metric="Kappa",
                    trControl= trainControl(method="cv",
                                            allowParallel = T, seeds = NULL))

stopCluster(cl)
invisible(gc())

plot(cartPredict5k)
require(partykit)
plot(as.party(cartPredict5k$finalModel))


