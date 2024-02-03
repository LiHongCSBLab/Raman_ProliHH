library(argparser)
library(doParallel)
library(tictoc)
library(caret)

p = arg_parser("Run training pipeline SVM OVO Within-donor task")
p = add_argument(p, "--outer_fold", default = 5L,
                 help = "Outer loop for evaluation")
p = add_argument(p, "--inner_fold", default = 4L,
                 help = "Inner loop for tuning")
p = add_argument(p, "--repeats", default = 5L,
                 help = "Repeat time for outer loop")
p = add_argument(p, "--train_data", default = './',
                 help = "Train data of the given donor")
p = add_argument(p, "--output_path", default = './',
                 help = "Train data of the given donor")

argv = parse_args(p)

outer_fold = as.integer( argv$outer_fold )
inner_fold = as.integer( argv$inner_fold )
repeats = as.integer( argv$repeats )
train_path = argv$train_data
output_path = argv$output_path

# print(output_path)

if (! dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
    print("Creat dir")
    print(output_path)
}

# Read in data
print('Train:')
print(train_path)

train_data = readRDS(train_path)

print("Dim of train data:")
print(dim(train_data))

train_test_SVM_OVO <- function(dataTrain, dataTest, k_fold){

print(table(dataTrain$Group))
#  P1  P4  P9 PHH 
# 492 485 493 413
print(table(dataTest$Group))
#  P1  P4  P9 PHH 
# 123 118 122 109
# dataTrain <- data[ trainIndex,]
# dataTest <- data[-trainIndex,]

fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = k_fold,
  ## repeated 5 times
  # repeats = 5,
  allowParallel = TRUE,
  verboseIter = TRUE
)
    
# Define ranges for the two parameters
C_range = c(0.1, 1, 2, 5, 10, 100)
# sigma_range = sapply(seq(-3,1,0.0125), function(x) 10^x)

# Create the grid of parameters
fitGrid <- expand.grid(C = C_range # ,
                       # sigma = sigma_range
                      )

fulldataTrain = dataTrain
fulldataTest = dataTest

# phhp1Train = fulldataTrain[which(fulldataTrain$Group %in% c("PHH","P1")),]
# phhp1Test =  fulldataTest[which(fulldataTest$Group %in% c("PHH","P1")),]

# p4p9Train = fulldataTrain[which(fulldataTrain$Group %in% c("P4","P9")),]
# p4p9Test = fulldataTest[which(fulldataTest$Group %in% c("P4","P9")),]

# phhp4Train = fulldataTrain[which(fulldataTrain$Group %in% c("PHH","P4")),]
# phhp4Test = fulldataTest[which(fulldataTest$Group %in% c("PHH","P4")),]

# phhp9Train = fulldataTrain[which(fulldataTrain$Group %in% c("PHH","P9")),]
# phhp9Test = fulldataTest[which(fulldataTest$Group %in% c("PHH","P9")),]

# p1p4Train = fulldataTrain[which(fulldataTrain$Group %in% c("P1","P4")),]
# p1p4Test = fulldataTest[which(fulldataTest$Group %in% c("P1","P4")),]

# p1p9Train = fulldataTrain[which(fulldataTrain$Group %in% c("P1","P9")),]
# p1p9Test = fulldataTest[which(fulldataTest$Group %in% c("P1","P9")),]

# # 减少因子的levels
# phhp1Train$Group = factor(phhp1Train$Group)
# phhp1Test$Group = factor(phhp1Test$Group)

# p4p9Train$Group = factor(p4p9Train$Group)
# p4p9Test$Group = factor(p4p9Test$Group)

# phhp4Train$Group = factor(phhp4Train$Group)
# phhp4Test$Group = factor(phhp4Test$Group)

# phhp9Train$Group = factor(phhp9Train$Group)
# phhp9Test$Group = factor(phhp9Test$Group)

# p1p4Train$Group = factor(p1p4Train$Group)
# p1p4Test$Group = factor(p1p4Test$Group)

# p1p9Train$Group = factor(p1p9Train$Group)
# p1p9Test$Group = factor(p1p9Test$Group)


# 构建模型
tic()
cl <- makeCluster(32)
registerDoParallel(cl)
print("Start training ...")

# svmPHHP1 = train(Group~., data=phhp1Train,method="svmLinear", trControl=fitControl,
#                  tuneGrid = fitGrid)
# svmPHHP4 = train(Group~., data=phhp4Train,method="svmLinear", trControl=fitControl,
#                  tuneGrid = fitGrid)
# svmPHHP9 = train(Group~., data=phhp9Train,method="svmLinear", trControl=fitControl,
#                  tuneGrid = fitGrid)
# svmP1P4 = train(Group~., data=p1p4Train,method="svmLinear", trControl=fitControl,
#                  tuneGrid = fitGrid)
# svmP1P9 = train(Group~., data=p1p9Train,method="svmLinear", trControl=fitControl,
#                  tuneGrid = fitGrid)
# svmP4P9 = train(Group~., data=p4p9Train,method="svmLinear", trControl=fitControl,
#                  tuneGrid = fitGrid)


svmOVR = train(Group~., data=fulldataTrain, method="svmLinear", trControl=fitControl,
                 tuneGrid = fitGrid)

stopCluster(cl)
toc()
print("Training finished.")

svmOVOpred = data.frame(fulldataTest[,2215])
# svmOVOpred$PHHP1 = predict(svmPHHP1, fulldataTest[,-2215], verbose = TRUE)
# svmOVOpred$PHHP4 = predict(svmPHHP4, fulldataTest[,-2215], verbose = TRUE)
# svmOVOpred$PHHP9 = predict(svmPHHP9, fulldataTest[,-2215], verbose = TRUE)
# svmOVOpred$P1P4 = predict(svmP1P4, fulldataTest[,-2215], verbose = TRUE)
# svmOVOpred$P1P9 = predict(svmP1P9, fulldataTest[,-2215], verbose = TRUE)
# svmOVOpred$P4P9 = predict(svmP4P9, fulldataTest[,-2215], verbose = TRUE)

svmOVOpred$predict = predict(svmOVR, fulldataTest[,-2215], verbose = TRUE)

# svmOVOpredTMP = apply(svmOVOpred,2,as.character)

# getmode <- function(v) {
#    uniqv <- unique(v)
#    unlist(uniqv[which.max(tabulate(match(v, uniqv)))])
# }
# svmOVOpred$final = apply(svmOVOpredTMP, 1, getmode)

return(svmOVOpred)
}


repeat_folds <- createMultiFolds(
    y = train_data$Group, 
    k = outer_fold, 
    times = repeats
)

i = 0

for (trainIndex in repeat_folds) {
    i = i + 1
    print("Fold:")
    print(i)
    dataTrain <- train_data[ trainIndex, -c(2215, 2217, 2218)]
    dataTest <- train_data[ -trainIndex, -c(2215, 2217, 2218)]
    OVO_SVM <- train_test_SVM_OVO(dataTrain, dataTest, inner_fold)
    OVO_SVM$Fold = i
    saveRDS(
        OVO_SVM, 
        paste0(output_path,'/','fold',i,'.rds')
    )
    write.csv(
        OVO_SVM, 
        paste0(output_path,'/','fold',i,'.csv')
    )  
}


