###..........................load packages.........................
library(tidyverse)
library(rstatix)
library(car)
library(ggplot2)
library(biotools)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                1. load data                              ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
home_directory <- Sys.getenv("HOME")
subdirectory <- "Documents/1_ToM_tDCS/data"
full_path <- file.path(home_directory, subdirectory)
setwd(full_path)
getwd()

datpres <- read.csv("percent_present_correct.csv")
View(datpres)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##        2. create new variables for descriptives and assumptions          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# extract the group identifier from the filename and make new Group variable
datpres$Group <- substr(datpres$X, 1, 1)
datpres$Group <- as.factor(datpres$Group)

# calculate the mean and SD for each column grouped by stimulation type (Group)
grouped_pres_means <- datpres %>%
  group_by(Group) %>%
  summarise(across(RMET1:WCST2, mean))
as.data.frame(grouped_pres_means)
print(grouped_pres_means)

grouped_pres_sd <- datpres %>%
  group_by(Group) %>%
  summarise(across(RMET1:WCST2, sd))
as.data.frame(grouped_pres_sd)
print(grouped_pres_sd)

# create difference variables by subtracting time 1 from time 2 accuracy
datpres <- datpres %>%
  mutate(
    RMET_diff = RMET2 - RMET1,
    SOD_diff = SOD2 - SOD1,
    WCST_diff = WCST2 - WCST1
  )

# perform Levene's test for each calculated difference variable
levene_RMET <- leveneTest(RMET_diff ~ Group, data = datpres)
print(levene_RMET)

levene_SOD <- leveneTest(SOD_diff ~ Group, data = datpres)
print(levene_SOD)

levene_WCST <- leveneTest(WCST_diff ~ Group, data = datpres)
print(levene_WCST)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  3. plots                                ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# histograms for the difference variables
p1pres <- ggplot(datpres, aes(x = RMET_diff)) +
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
  labs(title = "Histogram of calculated RMET accuracy score",
           x = "RMET_diff", y = "Frequency")
print(p1pres)

p2pres <- ggplot(datpres, aes(x = SOD_diff)) +
  geom_histogram(binwidth = 5, color = "black", fill = "lightgreen") +
  labs(title = "Histogram of calculated SOD accuracy score",
           x = "SOD_diff", y = "Frequency")
print(p2pres)

p3pres <- ggplot(datpres, aes(x = WCST_diff)) +
  geom_histogram(binwidth = 5, color = "black", fill = "lightcoral") +
  labs(title = "Histogram of calculated WCST accuracy score",
           x = "WCST_diff", y = "Frequency")
print(p3pres)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##               4. 3x3 MANOVA with computed difference variables           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
datpres$Group <- factor(datpres$Group)

anova_pres_result <- manova(cbind(RMET_diff, SOD_diff, WCST_diff) ~ Group, data = datpres)
summary.aov(anova_pres_result)
summary(anova_pres_result)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##         5. exploratory analysis with non-response trials excluded        ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nonresp <- read.csv("non-response_trials.csv")

# assign groups based on the first digit of the filenames
nonresp$Group <- ifelse(grepl("^1", nonresp$X), 1, ifelse(grepl("^2", nonresp$X), 2, 3))

# calculate the mean for each group
nonresp_means_sd <- nonresp %>%
  group_by(Group) %>%
  summarise(across(starts_with("RMET"), sd, na.rm = TRUE),
            across(starts_with("SOD"), sd, na.rm = TRUE),
            across(starts_with("WCST"), sd, na.rm = TRUE))

# Print the group means
print(nonresp_means_sd)
