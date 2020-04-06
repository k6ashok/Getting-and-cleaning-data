run_analysis <- function(){
library(dplyr)
															#All the project files are downloaded to the working directory before we run this script

															# The following code reads the train and test files

tac_xtest <-  read.delim("./test/Inertial Signals/total_acc_x_test.txt",header = FALSE)	#tac = total_acc
tac_ytest <-  read.delim("./test/Inertial Signals/total_acc_y_test.txt",header = FALSE)
tac_ztest <-  read.delim("./test/Inertial Signals/total_acc_z_test.txt",header = FALSE)
bac_xtest <-  read.delim("./test/Inertial Signals/body_acc_x_test.txt",header = FALSE)	#bac = body_acc
bac_ytest <-  read.delim("./test/Inertial Signals/body_acc_y_test.txt",header = FALSE)
bac_ztest <-  read.delim("./test/Inertial Signals/body_acc_z_test.txt",header = FALSE)
bgy_xtest <-  read.delim("./test/Inertial Signals/body_gyro_x_test.txt",header = FALSE)	#bgy = body_gyro
bgy_ytest <-  read.delim("./test/Inertial Signals/body_gyro_y_test.txt",header = FALSE)
bgy_ztest <-  read.delim("./test/Inertial Signals/body_gyro_z_test.txt",header = FALSE)

tac_xtrain <-  read.delim("./train/Inertial Signals/total_acc_x_train.txt",header = FALSE)
tac_ytrain <-  read.delim("./train/Inertial Signals/total_acc_y_train.txt",header = FALSE)
tac_ztrain <-  read.delim("./train/Inertial Signals/total_acc_z_train.txt",header = FALSE)
bac_xtrain <-  read.delim("./train/Inertial Signals/body_acc_x_train.txt",header = FALSE)
bac_ytrain <-  read.delim("./train/Inertial Signals/body_acc_y_train.txt",header = FALSE)
bac_ztrain <-  read.delim("./train/Inertial Signals/body_acc_z_train.txt",header = FALSE)
bgy_xtrain <-  read.delim("./train/Inertial Signals/body_gyro_x_train.txt",header = FALSE)
bgy_ytrain <-  read.delim("./train/Inertial Signals/body_gyro_y_train.txt",header = FALSE)
bgy_ztrain <-  read.delim("./train/Inertial Signals/body_gyro_z_train.txt",header = FALSE)


ytest <- read.delim("./test/y_test.txt",header = FALSE)
ytrain <- read.delim("./train/y_train.txt",header = FALSE)

subtest <-  read.delim("./test/subject_test.txt",header = FALSE)
subtrain <-  read.delim("./train/subject_train.txt",header = FALSE)

															# The variable names are changed to make it easy reference
names(tac_xtest) <- "tac_x"
names(tac_ytest) <- "tac_y"
names(tac_ztest) <- "tac_z"

names(tac_xtrain) <- "tac_x"
names(tac_ytrain) <- "tac_y"
names(tac_ztrain) <- "tac_z"

names(bac_xtest) <- "bac_x"
names(bac_ytest) <- "bac_y"
names(bac_ztest) <- "bac_z"

names(bac_xtrain) <- "bac_x"
names(bac_ytrain) <- "bac_y"
names(bac_ztrain) <- "bac_z"

names(bgy_xtest) <- "bgy_x"
names(bgy_ytest) <- "bgy_y"
names(bgy_ztest) <- "bgy_z"

names(bgy_xtrain) <- "bgy_x"
names(bgy_ytrain) <- "bgy_y"
names(bgy_ztrain) <- "bgy_z"

names(ytest) <- "activity"
names(ytrain) <- "activity"

names(subtest) <- "subject_id"
names(subtrain) <- "subject_id"


															# The test and train files are merged for each measurement
bacx <- bind_rows(bac_xtest,bac_xtrain)
bacy <- bind_rows(bac_ytest,bac_ytrain)
bacz <- bind_rows(bac_ztest,bac_ztrain)

tacx <- bind_rows(tac_xtest,tac_xtrain)
tacy <- bind_rows(tac_ytest,tac_ytrain)
tacz <- bind_rows(tac_ztest,tac_ztrain)

bgyx <- bind_rows(bgy_xtest,bgy_xtrain)
bgyy <- bind_rows(bgy_ytest,bgy_ytrain)
bgyz <- bind_rows(bgy_ztest,bgy_ztrain)

															# The activity and subject ID, test and train files are merged
activity_labels <- bind_rows(ytest,ytrain)
subject_id <- bind_rows(subtest,subtrain)


															#All measurement files are merged
all_xyz <- bind_cols(bacx,bacy,bacz,tacx,tacy,tacz,bgyx,bgyy,bgyz)



															#complete file
full_file <- bind_cols(subject_id,activity_labels,all_xyz)



															#The measurement variables are converted 
															# to numeric in order to perform 
															#mathematical operations such as mean and std deviation. 

bacx_num <- sapply(1:nrow(bacx),function(x){as.numeric(strsplit(bacx$bac_x,' ')[[x]])})	#splits character and converts to numeric
unlist_bacx_num <- unlist(bacx_num)										# unlists in order to remove NA's coerced due to additional spaces
unlist_bacx_num <- unlist_bacx_num[!is.na(unlist_bacx_num)]						# remove NAs
len <- length(unlist_bacx_num)/nrow(bacx)
list_bacx_num <- split(unlist_bacx_num,ceiling(seq_along (unlist_bacx_num)/len))		#recreates list without NAs

bacy_num <- sapply(1:nrow(bacy),function(x){as.numeric(strsplit(bacy$bac_y,' ')[[x]])})    #process repeated for all measurements
unlist_bacy_num <- unlist(bacy_num)
unlist_bacy_num <- unlist_bacy_num[!is.na(unlist_bacy_num)]
len <- length(unlist_bacy_num)/nrow(bacy)
list_bacy_num <- split(unlist_bacy_num,ceiling(seq_along (unlist_bacy_num)/len))

bacz_num <- sapply(1:nrow(bacz),function(x){as.numeric(strsplit(bacz$bac_z,' ')[[x]])})
unlist_bacz_num <- unlist(bacz_num)
unlist_bacz_num <- unlist_bacz_num[!is.na(unlist_bacz_num)]
len <- length(unlist_bacz_num)/nrow(bacz)
list_bacz_num <- split(unlist_bacz_num,ceiling(seq_along (unlist_bacz_num)/len))

tacx_num <- sapply(1:nrow(tacx),function(x){as.numeric(strsplit(tacx$tac_x,' ')[[x]])})
unlist_tacx_num <- unlist(tacx_num)
unlist_tacx_num <- unlist_tacx_num[!is.na(unlist_tacx_num)]
len <- length(unlist_tacx_num)/nrow(tacx)
list_tacx_num <- split(unlist_tacx_num,ceiling(seq_along (unlist_tacx_num)/len))

tacy_num <- sapply(1:nrow(tacy),function(x){as.numeric(strsplit(tacy$tac_y,' ')[[x]])})
unlist_tacy_num <- unlist(tacy_num)
unlist_tacy_num <- unlist_tacy_num[!is.na(unlist_tacy_num)]
len <- length(unlist_tacy_num)/nrow(tacy)
list_tacy_num <- split(unlist_tacy_num,ceiling(seq_along (unlist_tacy_num)/len))

tacz_num <- sapply(1:nrow(tacz),function(x){as.numeric(strsplit(tacz$tac_z,' ')[[x]])})
unlist_tacz_num <- unlist(tacz_num)
unlist_tacz_num <- unlist_tacz_num[!is.na(unlist_tacz_num)]
len <- length(unlist_tacz_num)/nrow(tacz)
list_tacz_num <- split(unlist_tacz_num,ceiling(seq_along (unlist_tacz_num)/len))

bgyx_num <- sapply(1:nrow(bgyx),function(x){as.numeric(strsplit(bgyx$bgy_x,' ')[[x]])})
unlist_bgyx_num <- unlist(bgyx_num)
unlist_bgyx_num <- unlist_bgyx_num[!is.na(unlist_bgyx_num)]
len <- length(unlist_bgyx_num)/nrow(bgyx)
list_bgyx_num <- split(unlist_bgyx_num,ceiling(seq_along (unlist_bgyx_num)/len))

bgyy_num <- sapply(1:nrow(bgyy),function(x){as.numeric(strsplit(bgyy$bgy_y,' ')[[x]])})
unlist_bgyy_num <- unlist(bgyy_num)
unlist_bgyy_num <- unlist_bgyy_num[!is.na(unlist_bgyy_num)]
len <- length(unlist_bgyy_num)/nrow(bgyy)
list_bgyy_num <- split(unlist_bgyy_num,ceiling(seq_along (unlist_bgyy_num)/len))

bgyz_num <- sapply(1:nrow(bgyz),function(x){as.numeric(strsplit(bgyz$bgy_z,' ')[[x]])})
unlist_bgyz_num <- unlist(bgyz_num)
unlist_bgyz_num <- unlist_bgyz_num[!is.na(unlist_bgyz_num)]
len <- length(unlist_bgyz_num)/nrow(bgyz)
list_bgyz_num <- split(unlist_bgyz_num,ceiling(seq_along (unlist_bgyz_num)/len))

										#mean and standard deviation calaulated for all type of measurement
mbacx_list <- lapply(list_bacx_num,mean)
title <- c("mean_body_x")
mbacx_df <- setNames(do.call(rbind.data.frame, mbacx_list), title)

mbacy_list <- lapply(list_bacy_num,mean)
title <- c("mean_body_y")
mbacy_df <- setNames(do.call(rbind.data.frame, mbacy_list), title)

mbacz_list <- lapply(list_bacz_num,mean)
title <- c("mean_body_z")
mbacz_df <- setNames(do.call(rbind.data.frame, mbacz_list), title)

mtacx_list <- lapply(list_tacx_num,mean)
title <- c("mean_total_x")
mtacx_df <- setNames(do.call(rbind.data.frame, mtacx_list), title)

mtacy_list <- lapply(list_tacy_num,mean)
title <- c("mean_total_y")
mtacy_df <- setNames(do.call(rbind.data.frame, mtacy_list), title)

mtacz_list <- lapply(list_tacz_num,mean)
title <- c("mean_total_z")
mtacz_df <- setNames(do.call(rbind.data.frame, mtacz_list), title)

mbgyx_list <- lapply(list_bgyx_num,mean)
title <- c("mean_gyro_x")
mbgyx_df <- setNames(do.call(rbind.data.frame, mbgyx_list), title)

mbgyy_list <- lapply(list_bgyy_num,mean)
title <- c("mean_gyro_y")
mbgyy_df <- setNames(do.call(rbind.data.frame, mbgyy_list), title)

mbgyz_list <- lapply(list_bgyz_num,mean)
title <- c("mean_gyro_z")
mbgyz_df <- setNames(do.call(rbind.data.frame, mbgyz_list), title)

sdbacx_list <- lapply(list_bacx_num,sd)
title <- c("sd_body_x")
sdbacx_df <- setNames(do.call(rbind.data.frame, sdbacx_list), title)

sdbacy_list <- lapply(list_bacy_num,sd)
title <- c("sd_body_y")
sdbacy_df <- setNames(do.call(rbind.data.frame, sdbacy_list), title)

sdbacz_list <- lapply(list_bacz_num,sd)
title <- c("sd_body_z")
sdbacz_df <- setNames(do.call(rbind.data.frame, sdbacz_list), title)

sdtacx_list <- lapply(list_tacx_num,sd)
title <- c("sd_total_x")
sdtacx_df <- setNames(do.call(rbind.data.frame, sdtacx_list), title)

sdtacy_list <- lapply(list_tacy_num,sd)
title <- c("sd_total_y")
sdtacy_df <- setNames(do.call(rbind.data.frame, sdtacy_list), title)

sdtacz_list <- lapply(list_tacz_num,sd)
title <- c("sd_total_z")
sdtacz_df <- setNames(do.call(rbind.data.frame, sdtacz_list), title)

sdbgyx_list <- lapply(list_bgyx_num,sd)
title <- c("sd_gyro_x")
sdbgyx_df <- setNames(do.call(rbind.data.frame, sdbgyx_list), title)

sdbgyy_list <- lapply(list_bgyy_num,sd)
title <- c("sd_gyro_y")
sdbgyy_df <- setNames(do.call(rbind.data.frame, sdbgyy_list), title)

sdbgyz_list <- lapply(list_bgyz_num,sd)
title <- c("sd_gyro_z")
sdbgyz_df <- setNames(do.call(rbind.data.frame, sdbgyz_list), title)

#mean and std dev files merged to form a single data frame
mfiles_list <- list(mbacx_df,mbacy_df,mbacz_df,mtacx_df,mtacy_df,mtacz_df,mbgyx_df,mbgyy_df,mbgyz_df)
moutput <- bind_cols(mfiles_list)
sdfiles_list <- list(sdbacx_df,sdbacy_df,sdbacz_df,sdtacx_df,sdtacy_df,sdtacz_df,sdbgyx_df,sdbgyy_df,sdbgyz_df)
sdoutput <- bind_cols(sdfiles_list)
output <- bind_cols(subject_id,activity_labels,moutput,sdoutput)

#creating a seperate tidy file
output$activity <- as.factor(output$activity)

#The following code changes the number representation of activities to their natural reference factors
levels(output$activity)[levels(output$activity)== 1 ] <- "WALKING"
levels(output$activity)[levels(output$activity)== 2 ] <- "WALKING_UPSTAIRS"
levels(output$activity)[levels(output$activity)== 3 ] <- "WALKING_DOWNSTAIRS"
levels(output$activity)[levels(output$activity)== 4 ] <- "SITTING"
levels(output$activity)[levels(output$activity)== 5 ] <- "STANDING"
levels(output$activity)[levels(output$activity)== 6 ] <- "LAYING"

#creating a text file of tidy data in the working directory ,for sharing.
write.table(output,"tidy_data.txt",sep="\t",row.names=FALSE)

}




