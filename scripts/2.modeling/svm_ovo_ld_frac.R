
library(argparser)
library(doParallel)
library(tictoc)
library(caret)

p = arg_parser("Run training pipeline SVM OVO Within-donor task")
p = add_argument(p, "--inner_fold", default = 4L,
                 help = "Inner loop for tuning")
p = add_argument(p, "--repeats", default = 4L,
                 help = "Repeat time for outer loop")
p = add_argument(p, "--frac", default = 0.9,
                 help = "The proportion of data for training")
p = add_argument(p, "--train_data",
                 help = "Train data of the donor pool", narg = Inf)
p = add_argument(p, "--test_data", 
                 help = "Test data of the given donor", narg = Inf)
p = add_argument(p, "--output_path", default = './',
                 help = "Train data of the given donor")


argv = parse_args(p)

# outer_fold = as.integer( argv$outer_fold )
inner_fold = as.integer( argv$inner_fold )
repeats = as.integer( argv$repeats )
frac = as.numeric( argv$frac )
train_paths = argv$train_data
test_paths = argv$test_data
output_path = argv$output_path

# print(output_path)

if (! dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
    print("Creat dir")
    print(output_path)
}

# Read in data
train_data = data.frame()
print('Train:')
for (path in train_paths) {
    print(path)
    data = readRDS(path)
    colnames(data)[1:2214] = as.character(c(1:2214))
    train_data = rbind(train_data, data)
}


test_data = data.frame()
print('Test:')
for (path in test_paths) {
    print(path)
    data = readRDS(path)
    colnames(data)[1:2214] = as.character(c(1:2214))
    test_data = rbind(test_data, data)
}

# train_data = readRDS(train_path)

print("Dim of train data:")
print(dim(train_data))

print("Dim of test data:")
print(dim(test_data))

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

phhp1Train = fulldataTrain[which(fulldataTrain$Group %in% c("PHH","P1")),]
phhp1Test =  fulldataTest[which(fulldataTest$Group %in% c("PHH","P1")),]

p4p9Train = fulldataTrain[which(fulldataTrain$Group %in% c("P4","P9")),]
p4p9Test = fulldataTest[which(fulldataTest$Group %in% c("P4","P9")),]

phhp4Train = fulldataTrain[which(fulldataTrain$Group %in% c("PHH","P4")),]
phhp4Test = fulldataTest[which(fulldataTest$Group %in% c("PHH","P4")),]

phhp9Train = fulldataTrain[which(fulldataTrain$Group %in% c("PHH","P9")),]
phhp9Test = fulldataTest[which(fulldataTest$Group %in% c("PHH","P9")),]

p1p4Train = fulldataTrain[which(fulldataTrain$Group %in% c("P1","P4")),]
p1p4Test = fulldataTest[which(fulldataTest$Group %in% c("P1","P4")),]

p1p9Train = fulldataTrain[which(fulldataTrain$Group %in% c("P1","P9")),]
p1p9Test = fulldataTest[which(fulldataTest$Group %in% c("P1","P9")),]

# 减少因子的levels
phhp1Train$Group = factor(phhp1Train$Group)
phhp1Test$Group = factor(phhp1Test$Group)

p4p9Train$Group = factor(p4p9Train$Group)
p4p9Test$Group = factor(p4p9Test$Group)

phhp4Train$Group = factor(phhp4Train$Group)
phhp4Test$Group = factor(phhp4Test$Group)

phhp9Train$Group = factor(phhp9Train$Group)
phhp9Test$Group = factor(phhp9Test$Group)

p1p4Train$Group = factor(p1p4Train$Group)
p1p4Test$Group = factor(p1p4Test$Group)

p1p9Train$Group = factor(p1p9Train$Group)
p1p9Test$Group = factor(p1p9Test$Group)


# 构建模型
tic()
cl <- makeCluster(detectCores())
registerDoParallel(cl)
print("Start training ...")

svmPHHP1 = train(Group~., 
                data=phhp1Train,
                method="svmLinear", 
                trControl=fitControl,
                 tuneGrid = fitGrid)
svmPHHP4 = train(Group~., 
                data=phhp4Train,
                method="svmLinear", 
                trControl=fitControl,
                 tuneGrid = fitGrid)
svmPHHP9 = train(Group~., 
                data=phhp9Train,
                method="svmLinear", 
                trControl=fitControl,
                 tuneGrid = fitGrid)
svmP1P4 = train(Group~., 
                data=p1p4Train,
                method="svmLinear", 
                trControl=fitControl,
                 tuneGrid = fitGrid)
svmP1P9 = train(Group~., 
                data=p1p9Train,
                method="svmLinear", 
                trControl=fitControl,
                 tuneGrid = fitGrid)
svmP4P9 = train(Group~., 
                data=p4p9Train,
                method="svmLinear", 
                trControl=fitControl,
                 tuneGrid = fitGrid)

stopCluster(cl)
toc()
print("Training finished.")

svmOVOpred = data.frame(fulldataTest[,2215])
svmOVOpred$PHHP1 = predict(svmPHHP1, fulldataTest[,-2215], verbose = TRUE)
svmOVOpred$PHHP4 = predict(svmPHHP4, fulldataTest[,-2215], verbose = TRUE)
svmOVOpred$PHHP9 = predict(svmPHHP9, fulldataTest[,-2215], verbose = TRUE)
svmOVOpred$P1P4 = predict(svmP1P4, fulldataTest[,-2215], verbose = TRUE)
svmOVOpred$P1P9 = predict(svmP1P9, fulldataTest[,-2215], verbose = TRUE)
svmOVOpred$P4P9 = predict(svmP4P9, fulldataTest[,-2215], verbose = TRUE)

svmOVOpredTMP = apply(svmOVOpred,2,as.character)

getmode <- function(v) {
   uniqv <- unique(v)
   unlist(uniqv[which.max(tabulate(match(v, uniqv)))])
}
svmOVOpred$final = apply(svmOVOpredTMP, 1, getmode)

return(svmOVOpred)
}

cat("Sampling: ", frac, '\n')

repeat_folds <- createDataPartition(
    train_data$Group, p = frac,
    list = TRUE, times = repeats
)

# print(dim(train_index))

# repeat_folds <- createMultiFolds(
#     y = train_data$Group, 
#     k = outer_fold, 
#     times = repeats
# )

i = 0

for (train_index in repeat_folds) {
    i = i + 1
    cat("Fold:", i, '\n')
    dataTrain <- train_data[ train_index, -c(2215, 2217, 2218)]
    dataTest <- test_data[ -train_index, -c(2215, 2217, 2218)]
    OVO_SVM <- train_test_SVM_OVO(dataTrain, dataTest, inner_fold)
    OVO_SVM$Repeat = i
    saveRDS(
        OVO_SVM, 
        paste0(output_path,'/','repeat',i,'.rds')
    )
    write.csv(
        OVO_SVM, 
        paste0(output_path,'/','repeat',i,'.csv')
    )
}