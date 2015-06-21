# Loading required libraries
library("readr")
library("plyr")
# Setting the working directory above the folder where data is saved

setwd("F:/GettingData/project/UCI HAR Dataset")

# Feature and Inertial data will be read and processed separately and modified
# Reading in training and test feature data and combining with subject and activity info

#################
# fixed width column is being read

train_dat <- read_fwf("./train/X_train.txt", col_positions = fwf_empty("./train/X_train.txt"))
train_labels<-read.csv("./train/y_train.txt", header = FALSE)
train_subject <-read.csv("./train/subject_train.txt", header = FALSE)
train_dl<- cbind(train_subject, train_labels, train_dat)
names(train_dl)[1:2] <- c("Subject","Label")

#################
test_dat <- read_fwf("./test/X_test.txt", col_positions = fwf_empty("./test/X_test.txt"))
test_labels<-read.csv("./test/y_test.txt", header = FALSE)
test_subject <-read.csv("./test/subject_test.txt", header = FALSE)
test_dl<- cbind(test_subject, test_labels, test_dat)
colnames(test_dl)[1:2] <- c("Subject","Label")
#################

# Combining train and test feature data

total_dat <-rbind(train_dl,test_dl)

total_dat_n = as.data.frame(matrix(nrow = 1, ncol = 4))
colnames(total_dat_n) <- c("Subject", "Label", "Mean_features", "Stdev_features")

# Calculating Mean and Stdev for features, using old school method after encountering some issues with advanced functions

for(i in 1:nrow(total_dat))
{
total_dat_n[i,1] = total_dat[i,1]
total_dat_n[i,2] = total_dat[i,2]
total_dat_n[i,3] =  mean(as.numeric(total_dat[i,3:563]))
total_dat_n[i,4] =  sd(as.numeric(total_dat[i,3:563]))
}


#################
# Reading in Inertial data
# Inertial signals - Test

body_acc_x_test <- read_fwf("./test/Inertial Signals/body_acc_x_test.txt", col_positions = fwf_empty("./test/Inertial Signals/body_acc_x_test.txt"))
body_acc_y_test <- read_fwf("./test/Inertial Signals/body_acc_y_test.txt", col_positions = fwf_empty("./test/Inertial Signals/body_acc_y_test.txt"))
body_acc_z_test <- read_fwf("./test/Inertial Signals/body_acc_z_test.txt", col_positions = fwf_empty("./test/Inertial Signals/body_acc_z_test.txt"))
body_gyro_x_test <- read_fwf("./test/Inertial Signals/body_gyro_x_test.txt", col_positions = fwf_empty("./test/Inertial Signals/body_gyro_x_test.txt"))
body_gyro_y_test <- read_fwf("./test/Inertial Signals/body_gyro_y_test.txt", col_positions = fwf_empty("./test/Inertial Signals/body_gyro_y_test.txt"))
body_gyro_z_test <- read_fwf("./test/Inertial Signals/body_gyro_z_test.txt", col_positions = fwf_empty("./test/Inertial Signals/body_gyro_z_test.txt"))
total_acc_x_test <- read_fwf("./test/Inertial Signals/total_acc_x_test.txt", col_positions = fwf_empty("./test/Inertial Signals/total_acc_x_test.txt"))
total_acc_y_test <- read_fwf("./test/Inertial Signals/total_acc_y_test.txt", col_positions = fwf_empty("./test/Inertial Signals/total_acc_y_test.txt"))
total_acc_z_test <- read_fwf("./test/Inertial Signals/total_acc_z_test.txt", col_positions = fwf_empty("./test/Inertial Signals/total_acc_z_test.txt"))

# Inertial signals - Train
body_acc_x_train <- read_fwf("./train/Inertial Signals/body_acc_x_train.txt", col_positions = fwf_empty("./train/Inertial Signals/body_acc_x_train.txt"))
body_acc_y_train <- read_fwf("./train/Inertial Signals/body_acc_y_train.txt", col_positions = fwf_empty("./train/Inertial Signals/body_acc_y_train.txt"))
body_acc_z_train <- read_fwf("./train/Inertial Signals/body_acc_z_train.txt", col_positions = fwf_empty("./train/Inertial Signals/body_acc_z_train.txt"))
body_gyro_x_train <- read_fwf("./train/Inertial Signals/body_gyro_x_train.txt", col_positions = fwf_empty("./train/Inertial Signals/body_gyro_x_train.txt"))
body_gyro_y_train <- read_fwf("./train/Inertial Signals/body_gyro_y_train.txt", col_positions = fwf_empty("./train/Inertial Signals/body_gyro_y_train.txt"))
body_gyro_z_train <- read_fwf("./train/Inertial Signals/body_gyro_z_train.txt", col_positions = fwf_empty("./train/Inertial Signals/body_gyro_z_train.txt"))
total_acc_x_train <- read_fwf("./train/Inertial Signals/total_acc_x_train.txt", col_positions = fwf_empty("./train/Inertial Signals/total_acc_x_train.txt"))
total_acc_y_train <- read_fwf("./train/Inertial Signals/total_acc_y_train.txt", col_positions = fwf_empty("./train/Inertial Signals/total_acc_y_train.txt"))
total_acc_z_train <- read_fwf("./train/Inertial Signals/total_acc_z_train.txt", col_positions = fwf_empty("./train/Inertial Signals/total_acc_z_train.txt"))

# Combining test and train inertial data for each acceleration type

bacx <-rbind(body_acc_x_test, body_acc_x_train)
bacy <-rbind(body_acc_y_test, body_acc_y_train)
bacz <-rbind(body_acc_z_test, body_acc_z_train)

bgx <-rbind(body_gyro_x_test, body_gyro_x_train)
bgy <-rbind(body_gyro_y_test, body_gyro_y_train)
bgz <-rbind(body_gyro_z_test, body_gyro_z_train)

tax <-rbind(total_acc_x_test, total_acc_x_train)
tay <-rbind(total_acc_y_test, total_acc_y_train)
taz <-rbind(total_acc_z_test, total_acc_z_train)

# Calculating mean , stdev, with descriptive column names adding to main dataframe created earlier

for(i in 1:nrow(total_dat_n))
{
  total_dat_n[i,"body_acc_x_train_mean"] = mean(as.numeric(bacx[i,1:128]))
  total_dat_n[i,"body_acc_y_train_mean"] = mean(as.numeric(bacy[i,1:128]))
  total_dat_n[i,"body_acc_z_train_mean"] = mean(as.numeric(bacz[i,1:128]))
  total_dat_n[i,"body_gyro_x_train_mean"] = mean(as.numeric(bgx[i,1:128]))
  total_dat_n[i,"body_gyro_y_train_mean"] = mean(as.numeric(bgy[i,1:128]))
  total_dat_n[i,"body_gyro_z_train_mean"] = mean(as.numeric(bgz[i,1:128]))
  total_dat_n[i,"total_acc_x_train_mean"] = mean(as.numeric(tax[i,1:128]))
  total_dat_n[i,"total_acc_y_train_mean"] = mean(as.numeric(tay[i,1:128]))
  total_dat_n[i,"total_acc_z_train_mean"] = mean(as.numeric(taz[i,1:128]))

  total_dat_n[i,"body_acc_x_train_stdev"] = sd(as.numeric(bacx[i,1:128]))
  total_dat_n[i,"body_acc_y_train_stdev"] = sd(as.numeric(bacy[i,1:128]))
  total_dat_n[i,"body_acc_z_train_stdev"] = sd(as.numeric(bacz[i,1:128]))
  total_dat_n[i,"body_gyro_x_train_stdev"] = sd(as.numeric(bgx[i,1:128]))
  total_dat_n[i,"body_gyro_y_train_stdev"] = sd(as.numeric(bgy[i,1:128]))
  total_dat_n[i,"body_gyro_z_train_stdev"] = sd(as.numeric(bgz[i,1:128]))
  total_dat_n[i,"total_acc_x_train_stdev"] = sd(as.numeric(tax[i,1:128]))
  total_dat_n[i,"total_acc_y_train_stdev"] = sd(as.numeric(tay[i,1:128]))
  total_dat_n[i,"total_acc_z_train_stdev"] = sd(as.numeric(taz[i,1:128]))
  
  print(i)
}

# Replacing activity numbers with Descriptive activity names
total_dat_n$Label <- as.factor(total_dat_n$Label)
levels(total_dat_n$Label)<- c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")

# Aggregation over all observations by activity and subject

total_dat_n_final <- aggregate(. ~ Subject+Label,data = total_dat_n,  FUN = "mean")

write.table(file = "UCI_HAR_aggregated_data.txt", sep =",", row.names = FALSE, total_dat_n_final)

