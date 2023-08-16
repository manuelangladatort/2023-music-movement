################################################################################
# Analysis script: Random Forest
################################################################################

library(tidyverse)
library(readxl)
library(car)
library(caret)
library(party)

# data
Data_liking <- read_excel("data/Data_liking.xlsx") %>% 
  filter(Condition == 1) %>% 
  # normalize by participant
  group_by(Participant) %>% 
  mutate(Liking_audio_visual = scale(Liking_audio_visual),
         Liking_visual_only = scale(Liking_visual_only)
  )

# Data_liking_extralong <- read_excel("data/Data_liking_extralong.xlsx")

set.seed(321)

run_cforest_caret <- function(formula,data,ntree= 1000){
  library(caret)
  model.caret<- train(formula, data,
                      method = "cforest",
                      tuneLength = 1,
                      trControl = trainControl(##10-fold CF
                        method = "repeatedcv",
                        number = 10,
                        repeats = 10),
                      controls = cforest_unbiased(ntree = 1000))
  
  
  model.caret <- model.caret
  plot.caret <- plot(varImp(model.caret))
  return(list(model.caret,plot.caret))
}


################################################################################
# Random Forest: audio visual model
################################################################################

# choose variables
varaibles_solo = c("QoM_Solo", "Flu_Solo", "BR_Solo")
varaibles_acc = c("QoM_Acc", "Flu_Acc", "BR_Acc")


DTaudio.visual <- Data_liking %>%  dplyr::select(Liking_audio_visual, varaibles_acc, varaibles_solo)


# run the model
model_audio.visual <- run_cforest_caret(Liking_audio_visual ~ ., 
                              data = DTaudio.visual[,-1], ntree = 1000)

model_audio.visual  # R2= 0.27 (RMSE = .82)

## model tree
tree.audio.visual <- ctree(Liking_audio_visual ~ 
                                 QoM_Solo + Flu_Solo + BR_Solo +
                                 QoM_Acc + Flu_Acc + BR_Acc, 
                               data = DTaudio.visual[,-1])
plot(tree.audio.visual)
cor(predict(tree.audio.visual),DTaudio.visual$Liking_audio_visual)^2 # R2= 0.20


################################################################################
# Random Forest: visual only model
################################################################################

# choose variables
varaibles_solo = c("QoM_Solo", "Flu_Solo", "BR_Solo")
varaibles_acc = c("QoM_Acc", "Flu_Acc", "BR_Acc")


DTvisual <- Data_liking %>%  dplyr::select(Liking_visual_only, varaibles_solo, varaibles_acc)


# run the model
model_visual <- run_cforest_caret(Liking_visual_only ~ ., 
                                        data = DTvisual[,-1], ntree = 1000)

model_visual  # R2= 0.23 (RMSE = .82=5)

## model tree
tree.visual <- ctree(Liking_visual_only ~ 
                                 QoM_Solo + Flu_Solo + BR_Solo +
                                 QoM_Acc + Flu_Acc + BR_Acc, 
                               data = DTvisual[,-1])
plot(tree.visual)
cor(predict(tree.visual), DTvisual$Liking_visual_only)^2 # R2= 0.21
