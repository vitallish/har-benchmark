rm(list=ls())

require(caret)
library(plyr);library(dplyr)
require(doSNOW)


load(file.path("data","explored.R"))

ppInstruct<-
    setRefClass("ppInstruct",
                fields = list(base = "data.frame",
                              func = "list",
                              param = "list",
                              sub = "list",
                              toChange = "data.frame",
                              changed = "logical"),
                methods = list(
                    addStep = function(inFunc,inParam,inSub){
                        func<<-c(func,list(inFunc))
                        param<<-c(param,list(inParam))
                        sub<<- c(sub,list(inSub))
                    },
                    eval = function(){
                        for (i in length(func)){
                            toChange[sub[[i]]] <<- do.call(func[[i]], param[[i]])
                        }
                        toChange
                    }

                )
    )


createPPList<-function(x){
    ppList<-NULL
    ppList$eval<-NA

    ppList$apply<-function(y){

        }

        ppList$sub$one<-
        ppList$fun$one<-predict
        ppList$param$one<-list(object =
                                   x[,ppList$sub$one] %>%
                                   preProcess(method=c("BoxCox","center","scale")),
                               newdata = y[,ppList$sub$one])

        ppList$eval()
        }

    ppList
}
niceParam<-list(object =
                    x[,ppList$sub$one] %>%
                    preProcess(method=c("BoxCox","center","scale")),
                newdata = y[,ppList$sub$one])

foo<-ppInstruct$new(base=train_sub)
foo$addStep(predict, ,inSub = -c(2,col_rm,160) )


nice<-foo$apply(train_sub)
nice2<-foo$eval()

test<-doppList(x=train_sub,ppList = ppList)



cl<-makeCluster(4,type="SOCK")
registerDoSNOW(cl)

set.seed(1234)
nnetMod<-train(classe~.,data=train_sub[1:100,-c(2,col_rm)],method="nnet",
                tuneGrid=expand.grid(size=c(5,10),decay=c(0.1,.3)),
                trControl= trainControl(method="repeatedcv",
                                        allowParallel = T, seeds = NULL),
               preProcess = c("BoxCox", "center","scale"),
               MaxNWts=Inf)

stopCluster(cl)
invisible(gc())

plot(rpartMod)

plot(as.party(rpartMod$finalModel))

train_res <- predict(nnetMod,train_sub[,-col_rm])

mean(train_res==train_sub$classe)
