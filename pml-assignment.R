library(caret)

# read the training and testing sets. Training is labeled as _orig because
# we will subset it later
training_orig = read.csv("pml-training.csv")
testing = read.csv("pml-testing.csv")

# Useful variables (most others have 19216 NA's or blank values)
myvars = c(#"user_name",
           #"new_window",
           #"num_window",
           "roll_belt",
           "pitch_belt",
           "yaw_belt",
           "total_accel_belt",
           "gyros_belt_x",
           "gyros_belt_y",
           "gyros_belt_z",
           "accel_belt_x",
           "accel_belt_y",
           "accel_belt_z",
           "magnet_belt_x",
           "magnet_belt_y",
           "magnet_belt_z",
           "roll_arm",
           "pitch_arm",
           "yaw_arm",
           "total_accel_arm",
           "gyros_arm_x",
           "gyros_arm_y",
           "gyros_arm_z",
           "accel_arm_x",
           "accel_arm_y",
           "accel_arm_z",
           "magnet_arm_x",
           "magnet_arm_y",
           "magnet_arm_z",
           "roll_dumbbell",
           "pitch_dumbbell",
           "yaw_dumbbell",
           "total_accel_dumbbell",
           "gyros_dumbbell_x",
           "gyros_dumbbell_y",
           "gyros_dumbbell_z",
           "accel_dumbbell_x",
           "accel_dumbbell_y",
           "accel_dumbbell_z",
           "magnet_dumbbell_x",
           "magnet_dumbbell_y",
           "magnet_dumbbell_z",
           "roll_forearm",
           "pitch_forearm",
           "yaw_forearm",
           "total_accel_forearm",
           "gyros_forearm_x",
           "gyros_forearm_y",
           "gyros_forearm_z",
           "accel_forearm_x",
           "accel_forearm_y",
           "accel_forearm_z",
           "magnet_forearm_x",
           "magnet_forearm_y",
           "magnet_forearm_z",
           "classe")

# subset the training set by only selecting the above variables
training = training_orig[myvars]

# setup the trainControl parameter to use cross-validation (and allow using all cpu cores)
tc = trainControl(method = "cv", allowParallel = TRUE)

# do the model trianing with a Random Forest method
# I chose a limited number of trees to speed it up and preprocessed the data
modFit = train(classe ~.,method="rf",data=training, importance=TRUE, proximity=TRUE,
               ntree=25, preProc=c("center", "scale"), trControl = tc)

# make a plot of error vs. # of trees
plot(modFit$finalModel,log="y")

# plot the variable importance
varImpPlot(modFit$finalModel)

# show the model summary with error estimate and confusion matrix
modFit$finalModel

# predict on the testing set and save in separate text files as suggested in the assignment
predTest = predict(modFit,newdata=testing)
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(predTest)
