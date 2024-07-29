
###...............................load packages................................
library(tidyverse)
library(rstatix)
library(openxlsx)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                1. load data                              ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
home_directory <- Sys.getenv("HOME")
subdirectory <- "/Documents/1_ToM_tDCS/data/"
full_path <- file.path(home_directory, subdirectory)
setwd(full_path)
getwd()

excel_file <- "tDCS_all-scores.xlsx"
wb <- openxlsx::loadWorkbook(file.path(full_path, excel_file))
num_sheets <- length(wb$worksheets)
print(num_sheets)

# loop through each sheet in workbook of all participant scores
for (i in 1:num_sheets) {
  # read the data from the current sheet
  sheet_data <- openxlsx::read.xlsx(wb, sheet = i)
  # generate the filename for the CSV file for each participant
  csv_filename <- paste0("sheet_", i, ".csv")
  # write new files
  write.csv(sheet_data, file = csv_filename, row.names = FALSE)
}

# define the mapping between old and new file names
old_names <- sprintf("sheet_%d.csv", 2:55)
new_names <- c(101:118, 201:218, 301:318)
file_mapping <- data.frame(
  old_name = old_names,
  new_name = paste0(new_names, ".csv")
)

# loop through each row in the mapping and rename the files to the
#  participant IDs
for (i in seq_len(nrow(file_mapping))) {
  old_file <- file_mapping$old_name[i]
  new_file <- file_mapping$new_name[i]
  file.rename(old_file, new_file)
}

# Check if renaming was successful
file.exists(file_mapping$new_name)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                2. clean data                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p_list <- list.files(pattern = "^\\d{3}\\.csv$")

# make dataframe to populate with number of missed responses in each task at
#  each timepoint
mtrx <- matrix(NA, 54, 6)
missing_responses <- as.data.frame(mtrx)
names(missing_responses) <- c("RMET_RT1", "SOD_RT1", "WCST_RT1",
                              "RMET_RT2", "SOD_RT2", "WCST_RT2")
row.names(missing_responses) <- (p_list)

# reshape each participant's csv file to only contain their in/correct scores
#  and reaction times
for (i in p_list) { 
  dat <- read.csv(i)
  
  # remove columns 7-14 and 21-27 inclusive. these were the % accuracy and
  # reaction time averages per trial as calculated in excel
  dat <- dat %>% select(-c(7:13, 20:26))

  # change the headers to ones which indicate task, timepoint, and reaction time
  names(dat)[1:12] <- c("RMET1", "SOD1", "WCST1",
                        "RMET2", "SOD2", "WCST2",
                        "RMET_RT1", "SOD_RT1",
                        "WCST_RT1", "RMET_RT2",
                        "SOD_RT2", "WCST_RT2")

  # count missing responses - number of 0s responses in the RTs columns
  zero_count <- sapply(dat[RTs], function(x) sum(x == 0, na.rm = TRUE))
  missing_responses[i, ] <- zero_count

  # read dat back in to the current participant's csv file
  write.csv(dat, i, row.names = FALSE)
  }

write.csv(missing_responses, "non-response_trials.csv", row.names=TRUE)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##            3. create participant accuracy scores for analysis            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# define column pairs used to check the influence of non-responses.
# ACs is accuracy, RTs is reaction time
ACs_straight_coded <- c("RMET1", "SOD1", "RMET2", "SOD2") # 1 is correct 
ACs_reverse_coded <- c("WCST1", "WCST2") # 1 is incorrect and 0 is correct
RTs <- c("RMET_RT1", "SOD_RT1", "WCST_RT1", "RMET_RT2", "SOD_RT2", "WCST_RT2")

# make dataframe for percent correct scores to go into
mtrx <- matrix(NA,54,6)
percent_correct <- as.data.frame(mtrx)
names(percent_correct) <- c("RMET1", "SOD1", "WCST1",
                            "RMET2", "SOD2", "WCST2")
row.names(percent_correct) <- (p_list)

# make a new dataframe with participant percent accuracy scores
for (i in p_list) { 
  dat <- read.csv(i)

  percent_correct_ones <- sapply(dat[ACs_straight_coded], function(x) {
  sum(x == 1, na.rm = TRUE) / length(na.omit(x)) * 100
})
percent_correct_zeros <- sapply(dat[ACs_reverse_coded], function(x) {
  sum(x == 0, na.rm = TRUE) / length(na.omit(x)) * 100
})

percent_combined <- c(
  percent_correct_ones["RMET1"],
  percent_correct_ones["SOD1"],
  percent_correct_zeros["WCST1"],
  percent_correct_ones["RMET2"],
  percent_correct_ones["SOD2"],
  percent_correct_zeros["WCST2"]
)

percent_correct[i, ] <- percent_combined
}
write.csv(percent_correct, "percent_correct.csv", row.names=TRUE)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##               4. accuracy scores without non-response trials             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# the same dataset, but only including the ones where they answered in time.
# make dataframe for percent of present correct scores to go into, i.e. no
# missing responses are counted in the percentages, as determined by 0s RT.
mtrx <- matrix(NA,54,6)
percent_present_correct <- as.data.frame(mtrx)
names(percent_present_correct) <- c("RMET1", "SOD1", "WCST1",
                                    "RMET2", "SOD2", "WCST2")
row.names(percent_present_correct) <- (p_list)

for (i in p_list) { 
  dat <- read.csv(i)

    percent_correct_rmetrt1 <- with(dat, {
    valid_indices <- RMET_RT1 != 0
    valid_rmet1 <- RMET1[valid_indices]
    sum(valid_rmet1 == 1, na.rm = TRUE) / length(na.omit(valid_rmet1)) * 100
    })

    percent_correct_sodrt1 <- with(dat, {
    valid_indices <- SOD_RT1 != 0
    valid_sod1 <- SOD1[valid_indices]
    sum(valid_sod1 == 1, na.rm = TRUE) / length(na.omit(valid_sod1)) * 100
    })

    percent_correct_wcstrt1 <- with(dat, {
    valid_indices <- WCST_RT1 != 0
    valid_wcst1 <- WCST1[valid_indices]
    sum(valid_wcst1 == 1, na.rm = TRUE) / length(na.omit(valid_wcst1)) * 100
    })

    percent_correct_rmetrt2 <- with(dat, {
    valid_indices <- RMET_RT2 != 0
    valid_rmet2 <- RMET2[valid_indices]
    sum(valid_rmet2 == 1, na.rm = TRUE) / length(na.omit(valid_rmet2)) * 100
    })

    percent_correct_sodrt2 <- with(dat, {
    valid_indices <- SOD_RT2 != 0
    valid_sod2 <- SOD2[valid_indices]
    sum(valid_sod2 == 1, na.rm = TRUE) / length(na.omit(valid_sod2)) * 100
    })

    percent_correct_wcstrt2 <- with(dat, {
    valid_indices <- WCST_RT2 != 0
    valid_wcst2 <- WCST2[valid_indices]
    sum(valid_wcst2 == 1, na.rm = TRUE) / length(na.omit(valid_wcst2)) * 100
    })

    percent_present_combined <- c(
        percent_correct_rmetrt1,
        percent_correct_sodrt1,
        percent_correct_wcstrt1,
        percent_correct_rmetrt2,
        percent_correct_sodrt2,
        percent_correct_wcstrt2
        )

percent_present_correct[i, ] <- percent_present_combined
}
write.csv(percent_present_correct, "percent_present_correct.csv", row.names=TRUE)
