################################################################################
# Analysis script: Multiple Regression
################################################################################

library(tidyverse)
library(readxl)
library(car)
library(caret)
library(ggpubr)
library(cowplot)

# data
Data_liking <- read_excel("data/Data_liking.xlsx") %>% 
  filter(Condition == 1) %>% 
  # normalize by participant
  group_by(Participant) %>% 
  mutate(Liking_audio_visual = scale(Liking_audio_visual),
         Liking_visual_only = scale(Liking_visual_only)
         )

# Data_liking_extralong <- read_excel("data/Data_liking_extralong.xlsx")


################################################################################
# Multiple regression: audio visual model
################################################################################

# choose variables
varaibles_solo = c("QoM_Solo", "Flu_Solo", "BR_Solo")
varaibles_acc = c("QoM_Acc", "Flu_Acc", "BR_Acc")


# a model with less variables works
DTaudio.visual <- Data_liking %>%  dplyr::select(Liking_audio_visual, varaibles_acc, varaibles_solo)

model_audio.visual <- lm(Liking_audio_visual ~ ., data = DTaudio.visual[, -1])
summary(model_audio.visual) # R2 = 22.82%

# assumptions
## 1. linearity
plot(model_audio.visual, which = 1) 
## 2. independence
durbinWatsonTest(model_audio.visual) # good but just: a value below 2 (positive autocorrelation). but p > .05
## 3. Normality
plot(model_audio.visual, which = 2) # roughly normally distributed but heavier-tailed ends
## 4.homoscedasticity: Breusch-Pagan test
ncvTest(model_audio.visual) # we fail to reject the null hypothesis and assume homoscedasticity 
# distributions fairly constant
ggplot(data = DTaudio.visual[,-1], aes(x = fitted(model_audio.visual), y = resid(model_audio.visual))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Residuals vs Fitted Values") +
  theme_minimal()

## 5.multiocollinearity
vif_values = vif(lm(Liking_audio_visual ~ ., data = DTaudio.visual[,-1])) 
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") 
abline(v = 5, lwd = 3, lty = 2) # two variables slightly above, I think it is fine for now


# plot signifincat predictors
a = ggplot(DTaudio.visual, aes(x = Liking_audio_visual, y = Flu_Solo)) +
  geom_point() +
  stat_cor(method="pearson") +
  geom_smooth(method = 'lm', color = 'red') + # Add a linear regression line
  xlab("Liking (AudioVisual)") +
  ylab("Fluidity (Solo)") +
  theme_bw()
b = ggplot(DTaudio.visual, aes(x = Liking_audio_visual, y = QoM_Acc)) +
  geom_point() +
  stat_cor(method="pearson") +
  geom_smooth(method = 'lm', color = 'red') + 
  xlab("Liking (AudioVisual") +
  ylab("Quantity of Motion (Accompanist)") +
  theme_bw()
c = ggplot(DTaudio.visual, aes(x = Liking_audio_visual, y = BR_Acc)) +
  geom_point() +
  stat_cor(method="pearson") +
  geom_smooth(method = 'lm', color = 'red') + 
  xlab("Liking (AudioVisual)") +
  ylab("Bounding Rectangle (Accompanist)") +
  theme_bw()

plot_grid(a, b, c, nrow = 1)

ggsave("audiovisual-sig-predictors.pdf", 
       height = 10, width = 22, units = "cm", dpi=300)


################################################################################
# Multiple regression: visual only model
################################################################################

# choose variables
varaibles_solo = c("QoM_Solo", "Flu_Solo", "BR_Solo")
varaibles_acc = c("QoM_Acc", "Flu_Acc", "BR_Acc")


# a model with less variables works
DTvisual <- Data_liking %>%  dplyr::select(Liking_visual_only, varaibles_solo, varaibles_acc)

model_visual <- lm(Liking_visual_only ~ ., data = DTvisual[, -1])
summary(model_visual) # R2 = 22%

# assumptions
## 1. linearity
plot(model_visual, which = 1) 
## 2. independence
durbinWatsonTest(model_visual) # good but just: a value below 2 (positive autocorrelation). but p > .05
## 3. Normality
plot(model_visual, which = 2) # roughly normally distributed but heavier-tailed ends
## 4.homoscedasticity: Breusch-Pagan test
ncvTest(model_visual) # we fail to reject the null hypothesis and assume homoscedasticity 
# distributions fairly constant
ggplot(data = DTvisual[,-1], aes(x = fitted(model_visual), y = resid(model_visual))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Residuals vs Fitted Values") +
  theme_minimal()

## 5.multiocollinearity
vif_values = vif(lm(Liking_visual_only ~ ., data = DTvisual[,-1])) 
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") 
abline(v = 5, lwd = 3, lty = 2) # two variables slightly above, I think it is fine for now


# plot signifincat predictors
a = ggplot(DTvisual, aes(x = Liking_visual_only, y = QoM_Solo)) +
  geom_point() +
  stat_cor(method="pearson") +
  geom_smooth(method = 'lm', color = 'red') + # Add a linear regression line
  xlab("Liking (Visual Only)") +
  ylab("Quantity of Motion (Solo)") +
  theme_bw()
b = ggplot(DTvisual, aes(x = Liking_visual_only, y = QoM_Acc)) +
  geom_point() +
  stat_cor(method="pearson") +
  geom_smooth(method = 'lm', color = 'red') + 
  xlab("Liking (Visual Only)") +
  ylab("Quantity of Motion (Accompanist)") +
  theme_bw()
c = ggplot(DTvisual, aes(x = Liking_visual_only, y = BR_Acc)) +
  geom_point() +
  stat_cor(method="pearson") +
  geom_smooth(method = 'lm', color = 'red') + 
  xlab("Liking (Visual Only)") +
  ylab("Bounding Rectangle (Accompanist)") +
  theme_bw()

plot_grid(a, b, c, nrow = 1)

ggsave("visualonly-sig-predictors.pdf", 
       height = 10, width = 22, units = "cm", dpi=300)
