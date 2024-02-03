# Set working directory to the folder containing spectra in .txt
home <- "/picb/bigdata/project/shenbihan/2-Raman/raw_data/005"
setwd("/picb/bigdata/project/shenbihan/2-Raman/raw_data/005")
folders <- list.files()
# Create a dataframe contaning all spectra
combinedata <- read.table(
  paste(home, folders[1], "1_01.txt",sep="/"), 
  header = FALSE, sep = "\t")[,1]

combinedata$Filename <- "Filename"
combinedata$Folder  <- "Folder"

for (folder in folders) {
  files  <-  list.files(paste(home, folder, sep = "/"))
  for (filename in files)
    {
      data <- read.table(
        paste(home, folder, filename, sep = "/"), 
        header = FALSE, sep = "\t")[, -1]
      data$Filename <- filename
      data$Folder <- folder
      combinedata <- cbind(combinedata, data)
    }
}
combinedata <- as.data.frame(combinedata)
colnames(combinedata)[1] <- "Wavenumber"
combinedata <- t(combinedata)
# 总共2464个文件

# 去除了第一列和第二列
combinedata <- combinedata[, -c(1, 2)]
# Remove rows with 0s
combinedata[combinedata == 0] <- NA
# unlist
combinedata <- data.frame(matrix(unlist(combinedata),
                                 nrow = nrow(combinedata)),
                          stringsAsFactors = FALSE)
# 去除了4个样本，分别为
combinedata <- combinedata[complete.cases(combinedata), ]
grep("Filename", combinedata) # X2240 Filename columns name
grep("Folder", combinedata) # X2241 Folder columns name


# Count no of spectra in each group
tablea <- table(grep("P1", combinedata$X2241))
tableb <- table(grep("P4", combinedata$X2241))
tablec <- table(grep("P9", combinedata$X2241))
tabled <- table(grep("PHH", combinedata$X2241))

# Create a numeric matrix
## Numeric matrix for intensities
annot <- combinedata[-1,c(2240, 2241)]
spec <- as.matrix(combinedata[-1,-c(2240, 2241)])
## Numeric vector for wavenumber
wn <- as.numeric(combinedata[1,-c(2240, 2241)]) 
t.spec <- t(spec) # Transpose
str(t.spec)
str(wn)
# Plot spectra
matplot(wn, t.spec, type = "l") # See all
title(main = "raw Raman spectra - 005")
View(t.spec)
mode(spec) <- "numeric"
mode(t.spec) <- "numeric"

spec.max <- apply(spec, 1, max)
spec.t.max  <- apply(t.spec, 1, max)

# Remove the samples with maximun intensity > 1000
matplot(wn, t.spec[,which(spec.max < 1000)], type = "l") # See all
title(main = "raw Raman spectra - 005 - intensity < 1000")
# which(spec.max > 1000)

# Remove the samples with maximun intensity > 1100
matplot(wn, t.spec[,which(spec.max < 1100)], type = "l") # See all
title(main = "raw Raman spectra - 005 - intensity < 1100")
# which(spec.max > 1000)

# 下面选择 t.spec[,which(spec.max < 1100)] 作为后续分析的spec

# Select Fingerprint region : 320 - 1800cm-1
annot.rm <- annot[which(spec.max < 1100), ]
spec.rm <- spec[which(spec.max < 1100), c(26:2239)]
t.spec.rm <- t.spec[c(26:2239), which(spec.max < 1100)]
wn <- wn[c(26:2239)]
############ Baseline correction ---------------------------------------
library(baseline)

# mode(spec) <- "numeric"
# mode(t.spec) <- "numeric"

spec.bl <- baseline.rollingBall(spec.rm,wm=200L, ws=100L)
# Plot any one orginal vs corrected spectrum
# plot(spec.bl, specNo = 1)
# Output
corr <- spec.bl$corrected
t.corr <- t(corr)
# Plotmatplot(wn, t.spec, type="l") # orginal spectra
matplot(wn, t.corr, type="l", main="corrected Raman spectrum - 005") # baseline-corrected spectra

############# Vector normalisation ------------------------------------
# normalize a row by the sum of its entries
for (n in seq_len(nrow(corr))) {
  S <- sum(corr[n,]) # Add range here if specfic wn desired
  corr[n,] <- corr[n,]/S
}
t.corr <- t(corr)
# rownames(corr) <- wn
# colnames(t.corr) <- wn
# Plot
matplot(wn, t.corr, type="l", main = "Vector normalized Raman spectrum - 005")

# 去除尖刺的峰
corr.max <- apply(corr,1,max)
plot(density(corr.max))
length(corr.max[corr.max > 0.004]) #9
matplot(
  wn, t.corr[,which(corr.max < 0.004)], type="l", 
  main = "Vector normalized Raman spectrum - 005 - < 0.004"
  )

length(corr.max[corr.max > 0.004]) #9
matplot(
  wn, t.corr[,which(corr.max < 0.004)], type="l", 
  main = "Vector normalized Raman spectrum - 005 - < 0.004"
  )

# 人工选取区域去除尖刺
corr.max.sep <- apply(
  corr[,c(seq(1,950,1),seq(1200,1400,1),seq(1750,1900,1),
  seq(2100,2214,1))],
  1,
  max
  )



plot(density(corr.max.sep))
length(which(corr.max.sep > 0.002)) # 13
matplot(
  wn, t.corr[, which((corr.max < 0.004) & (corr.max.sep < 0.002))], type="l", 
  main = "Vector normalized Raman spectrum-005- <0.004 & 950<0.002"
  )

corr.rm <- corr[which((corr.max < 0.004) & (corr.max.sep < 0.002)), ] 
annot.rm <- annot[which((corr.max < 0.004) & (corr.max.sep < 0.002)), ] 

# As data frame and change condition name
data <- data.frame(corr.rm, row.names = NULL)
colnames(data) <- wn
# Add file name
library(stringr)
str(data)
data$File <- annot.rm$X2240
data$Group <- annot.rm$X2241
# word(data$File,1,sep = fixed("_"))
data$CellLine <- word(data$File,1,sep = fixed("_"))
# data$Replicate <- word(data$File,2,sep = fixed("_"))
tmp  <- word(data$File,2,sep = fixed("_"))
data$Replicate <- unlist(
  lapply(tmp,
         FUN = function(x){strsplit(x, "[.]")[[1]][1]})
  )

# data$Time <-word(data$File,4,sep = fixed("_"))
# data$Cell <-word(data$File,5,sep = fixed("_"))
# Find col index for class and variables
grep("File", colnames(data)) #2215
grep("Group", colnames(data)) #2216
grep("CellLine", colnames(data)) #2217
grep("Replicate", colnames(data)) #2218

