runAnalysisR <- function (FilePath){
   library (dplyr)
   # The zip file was downloadead localy. Enter the local pahth, in my case 
   # runAnalysisR ("/Users/tzhsami6/Downloads/UCI HAR Dataset")
   # prepare the file path for test and training
   myfileTest <- paste (FilePath, "test", sep="/")
   myfileTrain <- paste (FilePath, "train", sep="/")
   setwd(myfileTest)
      # read test record, including subject and descriptive movement variables
      X_test = read.table("X_test.txt")
      X_subject= read.table("subject_test.txt")
      X_test_label = read.table("y_test.txt") 
      # bring them to one test table together
      X_test$subject <- c (X_subject$V1) 
      X_test$label <- c (X_test_label$V1)

      # read train records  
      setwd(myfileTrain)
      X_train = read.table("X_train.txt")
      X_subject_train= read.table("subject_train.txt")
      X_label_train =   read.table("y_train.txt")
      # bring them to one train table together
      X_train$subject <- c(X_subject_train$V1)
      X_train$label <- c(X_label_train$V1)

      # Merge train and test records together
      # 1.Merges the training and the test sets to create one data set
      MergeTraintest <- rbind(X_train, X_test)

      #now we need to set the working directory to the source path 
      setwd(FilePath)
      # read the names to rename the merged dataframe 
      NamesMergeTraintest = read.table("features.txt")

      # make out of a factor a charchter
      NamesMergeTraintest$V3 <- as.character(NamesMergeTraintest$V2)
      # 4. Appropriately labels the data set with descriptive variable names.
      # rename columns 1-561 
      names (MergeTraintest) [1:561] <- c(NamesMergeTraintest$V3)
     
      # 2 Extracts only the measurements on the mean and standard deviation for each measurement.
      # create an expression to select columns (standard, deviation), but also the two clolumns 
      # for filtering the SUBJECT and MOVEMENT (label)
      expres <- "mean|std|subject|label"

      # select only the columun containing mean and std
      MergeTraintest2 <- MergeTraintest[ , grep (expres, names(MergeTraintest)) ]
      
      # 3. Uses descriptive activity names to name the activities in the data set
      # replace lable number with 
      #1 WALKING, 2 WALKING_UPSTAIRS, 3 WALKING_DOWNSTAIRS, 4 SITTING, 5 STANDING, 6 LAYING

      MergeTraintest2 <- mutate (MergeTraintest2,
      label = ifelse (as.character (MergeTraintest2$label)=="1","WALKING", 
                       ifelse (as.character (MergeTraintest2$label)=="2","WALKING_UPSTAIRS",
                       ifelse (as.character (MergeTraintest2$label)=="3","WALKING_DOWNSTAIRS",
                       ifelse (as.character (MergeTraintest2$label)=="4","SITTING",
                       ifelse (as.character (MergeTraintest2$label)=="5","STANDING",
                       ifelse (as.character (MergeTraintest2$label)=="6","LAYING","NO_LABEL")))))))
      
      # 5. From the data set in step 4, creates a second, independent tidy data set with 
      # the average of each variable for each activity and each subject.                 
      # MergeTraintest3 <-  aggregate(MergeTraintest2[, 1:79], list(MergeTraintest2$label3, MergeTraintest2$subject), mean)
      #  MergeTraintest3 <-  aggregate(MergeTraintest2[, -which(names(MergeTraintest2) %in% c("label3","subject"))], list(MergeTraintest2$label3, MergeTraintest2$subject), mean)
      # MergeTraintest3 <-  aggregate(MergeTraintest2[, -which(names(MergeTraintest2) %in% c("label3","subject"))], by= list(activity = MergeTraintest2$label3, subject=MergeTraintest2$subject), FUN=mean)
      # excol <- "label3|subject"
      # MergeTraintest3 <-  aggregate(MergeTraintest2[, -grep(excol,colnames(MergeTraintest2))], by= list(activity = MergeTraintest2$label3, subject=MergeTraintest2$subject), FUN=mean)
      # MergeTraintest3 <-  aggregate(MergeTraintest2[, 1:79], by= list(MergeTraintest2$label3, MergeTraintest2$subject), FUN=mean)
      MergeTraintest3 <- MergeTraintest2 %>%
        group_by(label, subject) %>%
        summarise_each(funs(mean))
    write.table(MergeTraintest3, file= "MergeTraintest3.txt" , row.name=FALSE)
      }

