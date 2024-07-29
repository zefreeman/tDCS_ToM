###..........................load packages.........................
library(tidyverse)
library(car)
library(rstatix)
library(ggplot2)
library(biotools)
library(effectsize)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                1. load data                              ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
home_directory <- Sys.getenv("HOME")
subdirectory <- "Documents/1_ToM_tDCS/data"
full_path <- file.path(home_directory, subdirectory)
setwd(full_path)
getwd()

dat <- read.csv("percent_correct.csv")
View(dat)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      2. add gender and age variables                     ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
genderdat <- read.csv("tDCS_accuracy-scores.csv")
View(genderdat)

dat <- dat %>%
  mutate(Age = genderdat$Age, 
         Gender = genderdat$Gender)

dat <- dat %>%
  select(1, Age, Gender, everything())


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##        3. create new variables for descriptives and assumptions          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# extract the group identifier from the filename and make new Group variable
dat$Group <- substr(dat$X, 1, 1)
dat$Group <- as.factor(dat$Group)

# calculate the mean and SD for each column grouped by stimulation type (Group)
grouped_means <- dat %>%
  group_by(Group) %>%
  summarise(across(RMET1:WCST2, mean))
as.data.frame(grouped_means)

grouped_sd <- dat %>%
  group_by(Group) %>%
  summarise(across(RMET1:WCST2, sd))
as.data.frame(grouped_sd)

# create difference variables by subtracting time 1 from time 2 accuracy
dat <- dat %>%
  mutate(
    RMET_diff = RMET2 - RMET1,
    SOD_diff = SOD2 - SOD1,
    WCST_diff = WCST2 - WCST1
  )

# perform Levene's test for each calculated difference variable
levene_RMET <- leveneTest(RMET_diff ~ Group, data = dat)
print(levene_RMET)

levene_SOD <- leveneTest(SOD_diff ~ Group, data = dat)
print(levene_SOD)

levene_WCST <- leveneTest(WCST_diff ~ Group, data = dat)
print(levene_WCST)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  4. plots                                ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# histograms for the difference variables
p1 <- ggplot(dat, aes(x = RMET_diff)) +
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
  labs(title = "Histogram of calculated RMET accuracy score",
           x = "RMET_diff", y = "Frequency")
print(p1)

p2 <- ggplot(dat, aes(x = SOD_diff)) +
  geom_histogram(binwidth = 5, color = "black", fill = "lightgreen") +
  labs(title = "Histogram of calculated SOD accuracy score",
           x = "SOD_diff", y = "Frequency")
print(p2)

p3 <- ggplot(dat, aes(x = WCST_diff)) +
  geom_histogram(binwidth = 5, color = "black", fill = "lightcoral") +
  labs(title = "Histogram of calculated WCST accuracy score",
           x = "WCST_diff", y = "Frequency")
print(p3)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##               5. 3x3 MANOVA with computed difference variables           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat$Group <- factor(dat$Group)

# MANOVA with gender as interaction term
anova_result <- manova(cbind(RMET_diff, SOD_diff, WCST_diff) ~ Group, data = dat)
summary.aov(anova_result)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                       6. one-way ANOVA of task results                   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyr)

# reshape data into long format for one-way ANOVA
dat_long <- dat %>%
  pivot_longer(cols = c(RMET_diff, SOD_diff, WCST_diff), 
               names_to = "Task", 
               values_to = "Score")

oneway_anova_result <- aov(Score ~ Task, data = dat_long)
summary.aov(oneway_anova_result)

# post-hoc test using Tukey's HSD
tukey_result <- TukeyHSD(oneway_anova_result)
print(tukey_result)