# Download the Data ----
if(!file.exists(file.path("data","train.csv"))){
    download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
                  file.path("data","train.csv"))
    download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
                  file.path("data","test.csv"))
}
# Import the training Set ----
train_raw <- read.csv(file.path("data","train.csv"),na.strings = c("NA","#DIV/0!"))


#initial exploration----
require(dplyr)
dim(train_raw)

# there are 6 columns class="logical" which only contain NAs
log_sel <- (sapply(train_raw,class) == "logical") %>% which


# the test data will only contain live statistics, no summary stats from 'new_window'
# it is possible to ignore the following columns

summ_stat_reg<-"^(kurtosis|skewness|max|min|amplitude|var|avg|stddev)_"
summ_sel<-grep(summ_stat_reg,names(train_raw))

# other columns to remove include timepoints, 1st column and windows features:
misc_cols <- c(1,3:7)

# combine into a list of all columns to remove initially
col_rm <-c(log_sel,summ_sel,misc_cols) %>% unique





# 1 factor variable is the name, the other is the final classification

# Explore each variable----
train_exp<-train_raw %>%
    select(-col_rm)

# The dataset will have the following col_types:
train_exp %>%
    sapply(class) %>%
    table

# There is a small class imbalance where A exists about 2x as much as each of the
table(train_exp$classe)
table(train_exp$user_name) # failry even with user names



featurePlot(x = train_exp[,2:7], y=train_exp$classe,
            plot = "density",
            scales = list(x = list(relation="free"),
                          y = list(relation="free")),
            auto.key = list(columns = 5), alpha = 1/10)
# the density plots show that many of the variables appear to have multiple peaks.
# this becomes difficult to normalize, so it may be beneficial to focus on models which do
# not require normal variable distributions
require(corrplot)
nearZeroVar(train_exp, saveMetrics= TRUE)

trainCor<-cor(train_exp[-5373,2:53])
corCols <- findCorrelation(trainCor, cutoff = .95) +1

corrplot(trainCor[-corCols,-corCols],tl.cex=.3)

featurePlot(x = train_exp[,corCols], y=train_exp$classe,
            plot = "pairs",
            scales = list(x = list(relation="free"),
                          y = list(relation="free")),
            auto.key = list(columns = 5), alpha = 1/10)

# row 5373 from the training set contains an extreme outlier for gyros_dumbbell_x
# and it will be removed

onetimeout_row<-which.min(train_raw$gyros_dumbbell_x)
hist(train_raw$gyros_dumbbell_x)

hist(train_raw$gyros_dumbbell_x[-onetimeout_row])

train_raw<-train_raw[-onetimeout_row,]
train_exp<-train_exp[-onetimeout_row,]

# Any NA's?

any(is.na(train_exp)) #no

# Export explored data----
# because there is so much data in the training set, I will split it up 80/20 to create
# a separate cross validation set for model comparison
require(caret)
set.seed(12345)

inTrain<-createDataPartition(train_raw$classe, p=.8, list=F)

train_sub <- train_raw[inTrain,]
cv_sub <- train_raw[-inTrain,]

save(file = file.path("data","explored.R"), list=c("train_sub","cv_sub","col_rm"))

#other tips----
#calculate skewness e1071:skewness

pcaObject<-prcomp(train_exp[2:53], center=T, scale=T)
percVar<-pcaObject$sd^2/sum(pcaObject$sd)

pcaObject$x # the transformed objects

ggplot()+
    geom_point(aes(x=pcaObject$x[,1], y = pcaObject$x[,2],color=train_exp$classe),alpha=0.2)

    featurePlot(x = pcaObject$x[,1:2], y= train_exp$classe, plot="pairs",alpha=1/50,
                auto.key = list(columns = 5))

#it's interesting that the PC1 v 2 splits into 5 groups, but they are not relevent.
# knn may have issues if operating on the pca dataset
#what is spatial signs



