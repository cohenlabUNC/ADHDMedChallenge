##### Create FSL Event Files for GNG Analyses ####
# August/September 2019, MEMitchell


### NEED TO GO BACK AND REMOVE COLUMN HEADERS & EXTRA COLUMNS



rm(list = ls())

#libraries
library(ggplot2)
library(dplyr)

#library(gtools)
#library(stringr)
#library(tidyr)
# Remember to set working directory to source file location first.
gng.dir = "/Users/mackenziemitchell/Google Drive/UNC Cohen Lab/Research Studies/ADHD Med Challenge/Task/Go_No_Go task/GNG task_JRC/data"
setwd(gng.dir)
print(getwd())



############ CREATE ONSET FILES FOR GNG tasks (both regular and reward) ############
filenames.gng = list.files(pattern=glob2rx("10*_GNGre*.csv"))
#filenames.gng

#only want to create onset files for sessions 2 and 3 (not 1 bc behavioral, nor for practice or unlabeled sessions), so restrict filenames.reg to only sessions 2 and 3
filenames.gng = filenames.gng[(sapply(filenames.gng, function (s) strsplit(s, '_')[[1]][2])) != 1 & (sapply(filenames.gng, function (s) strsplit(s, '_')[[1]][2])) != 'practice' & (sapply(filenames.gng, function (s) strsplit(s, '_')[[1]][2])) != 'practice1' & (sapply(filenames.gng, function (s) strsplit(s, '_')[[1]][2])) != 9999 & (sapply(filenames.gng, function (s) strsplit(s, '_')[[1]][2])) != 'practice2' & (sapply(filenames.gng, function (s) strsplit(s, '_')[[1]][2])) != 'practice day2' & (sapply(filenames.gng, function (s) strsplit(s, '_')[[1]][2])) != 'pracice']
filenames.gng

# keep only these files
filenames.gng = filenames.gng[c(1:3, 6:8, 9:16, 18:31, 33:55, 57:121, 135, 138, 139:150, 154:173, 175, 178:184, 189:201, 203, 204:217, 218, 221, 222:227, 229:230)] # this is outdated as of 11/10/19

filenames.gng = filenames.gng[c(202, 204, 208, 210, 213, 215, 217, 219)] #just to pull new GNG regular EVs to run 11/10/19


## NOTES:
# "10051_14_GNGreward_2017_Jul_25_1522.csv"  what; weird date
# "1012_25_GNGregular_2018_Nov_21_1028.csv" ignore bc empty
# 1023_2_GNGregular - notes? use the second one? looks like they got through the first run in the first file; how are scans sorted? how to align?
# 1045_3_GNGregular has MANY copies [122:135] - want 135 bc all others are empty
# 1045_3_GNGreward has 3 copies [136:138] - want 138 bc all others are empty
# 1050_3_GNGregular has multiple copies [151:154] - want 154 bc all others are empty
# 1059_2_GNGregular has multiple - want 175
# 1059_2_GNGreward - want 178 *incomplete
# 1062_3_GNGregular **two separate partial files, not including in this for onset files yet
# 1062_3_GNGreward **two separate partial files, not including in this for onset files yet
# 1068_2_GNGreward - want 203 bc first one is incomplete; ask for notes to confirm MRI scan structure (want to align with correct scan)
# 1076_2_GNGregular - want 218 (files fro Jul 15), 221
# 1081_2_GNGregular - want 229 But they are the same



# remove these participants bc the files are empty
#filenames.gng = filenames.gng[c(1:3, 6:16, 18:31, 33:121, 135, 138:150, 154:173, 175, 178:218, 221:227,229:230)]
# remove the first two 1050 3 reg files, first  1059 2 reg file, first two 1059 2 rew file, last two 1076 2 reg files, copies of 1081 2 reg
#1050 3 has files on two days

# 1062 has multiple incomplete versions; probably not the only one
# 1068 2 first rew is incomplete


# "1012_25_GNGregular_2018_Nov_21_1028.csv" what is this file?
# "1003__GNGregular_2017_Jun_16_1028.csv" what is this file?
# "1003_3.2_GNGregular_2017_Jun_16_1129.csv" what is this file?
# "10051_14_GNGreward_2017_Jul_25_1522.csv" what is this file?
# "1003__GNGreward_2017_Jun_16_1029.csv" what is this file?
# many duplicates (1) to clean up

numfiles = length(filenames.gng) 
numfiles # number of data files

names = sapply(filenames.gng, function (s) strsplit(s, '_')[[1]][1]) # pick participant ID out of filenames --  pulls the first string separated by a _
length(names) # number of participant files to be processed
length(unique(names)) # number of uniquely identified participants

session = sapply(filenames.gng, function (s) strsplit(s, '_')[[1]][2]) # pick session number out of filenames -- pulls the second string separated by _

gng_type = sapply(filenames.gng, function (s) strsplit(s, '_')[[1]][3])

output = data.frame()

for(i in 1:numfiles){
  id = sapply(filenames.gng[i], function (s) strsplit(s, '_')[[1]][1]) # create a variable that identifies the subject number
  session = sapply(filenames.gng[i], function (s) strsplit(s, '_')[[1]][2]) # create a variable that identifies the session
  id.session = paste0(id,'_',session) # create one variable that identifies the id and subject number together (used in the file name later)
  gng_type = sapply(filenames.gng[i], function (s) strsplit(s, '_')[[1]][3]) # create a variable that identifies the GNG task type
  print(paste0(i,' ', id,' ', session,' ', gng_type)) # print out the name as you loop through
  partic.specific = read.csv(filenames.gng[i], sep=",", na.strings=c("")) # read in the csv file for each participant
  
  
  # insert if statement for GNGregular
  if (gng_type == 'GNGregular'){
    # create base dataframe with all info needed for all GNGreg onset files
    partic.onset <- partic.specific %>% select(onsetTime_actual, stimDur_expected, trialtype, trial_response.corr, participant, session, run)
    partic.onset$weight = 1
    partic.onset <- partic.onset[!is.na(partic.onset$trialtype),]
    
    ## GNGreg Go_hit ##
    gohit.onset.1 = partic.onset[partic.onset$trialtype == "Go" & partic.onset$trial_response.corr == 1 & partic.onset$run == 1,]
        path = "~/Dropbox/GNG L1 + L2 processing/"
        gohit.onset.1$trialtype = NULL
        gohit.onset.1$trial_response.corr = NULL
        gohit.onset.1$participant = NULL
        gohit.onset.1$session = NULL
        gohit.onset.1$run = NULL
      write.table(gohit.onset.1, file.path(path, sprintf("%s_GNGreg_run1_Go_hit.txt", id.session)), row.names=F, col.names=F)
    gohit.onset.2 = partic.onset[partic.onset$trialtype == "Go" & partic.onset$trial_response.corr == 1 & partic.onset$run == 2,]
        gohit.onset.2$trialtype = NULL
        gohit.onset.2$trial_response.corr = NULL
        gohit.onset.2$participant = NULL
        gohit.onset.2$session = NULL
        gohit.onset.2$run = NULL
      write.table(gohit.onset.2, file.path(path, sprintf("%s_GNGreg_run2_Go_hit.txt", id.session)), row.names=F, col.names=F)
    
    ## GNGreg Go_omission ##
    goomiss.onset.1 = partic.onset[partic.onset$trialtype == "Go" & partic.onset$trial_response.corr == 0 & partic.onset$run == 1,]
        goomiss.onset.1$trialtype = NULL
        goomiss.onset.1$trial_response.corr = NULL
        goomiss.onset.1$participant = NULL
        goomiss.onset.1$session = NULL
        goomiss.onset.1$run = NULL
      write.table(goomiss.onset.1, file.path(path, sprintf("%s_GNGreg_run1_Go_omission.txt", id.session)), row.names=F, col.names=F)
    goomiss.onset.2 = partic.onset[partic.onset$trialtype == "Go" & partic.onset$trial_response.corr == 0 & partic.onset$run == 2,]
        goomiss.onset.2$trialtype = NULL
        goomiss.onset.2$trial_response.corr = NULL
        goomiss.onset.2$participant = NULL
        goomiss.onset.2$session = NULL
        goomiss.onset.2$run = NULL
      write.table(goomiss.onset.2, file.path(path, sprintf("%s_GNGreg_run2_Go_omission.txt", id.session)), row.names=F, col.names=F)
    
    ## GNGreg NoGo_correct_reject ##
    nogocorrrej.onset.1 = partic.onset[partic.onset$trialtype == "NoGo" & partic.onset$trial_response.corr == 1 & partic.onset$run == 1,]
        nogocorrrej.onset.1$trialtype = NULL
        nogocorrrej.onset.1$trial_response.corr = NULL
        nogocorrrej.onset.1$participant = NULL
        nogocorrrej.onset.1$session = NULL
        nogocorrrej.onset.1$run = NULL 
      write.table(nogocorrrej.onset.1, file.path(path, sprintf("%s_GNGreg_run1_NoGo_correct_reject.txt", id.session)), row.names=F, col.names=F)
    nogocorrrej.onset.2 = partic.onset[partic.onset$trialtype == "NoGo" & partic.onset$trial_response.corr == 1 & partic.onset$run == 2,]
        nogocorrrej.onset.2$trialtype = NULL
        nogocorrrej.onset.2$trial_response.corr = NULL
        nogocorrrej.onset.2$participant = NULL
        nogocorrrej.onset.2$session = NULL
        nogocorrrej.onset.2$run = NULL
      write.table(nogocorrrej.onset.2, file.path(path, sprintf("%s_GNGreg_run2_NoGo_correct_reject.txt", id.session)), row.names=F, col.names=F)
    
    ## GNGreg NoGo_commission ##
    nogocommission.onset.1 = partic.onset[partic.onset$trialtype == "NoGo" & partic.onset$trial_response.corr == 0 & partic.onset$run == 1,]
        nogocommission.onset.1$trialtype = NULL
        nogocommission.onset.1$trial_response.corr = NULL
        nogocommission.onset.1$participant = NULL
        nogocommission.onset.1$session = NULL
        nogocommission.onset.1$run = NULL
      write.table(nogocommission.onset.1, file.path(path, sprintf("%s_GNGreg_run1_NoGo_commission.txt", id.session)), row.names=F, col.names=F)
    nogocommission.onset.2 = partic.onset[partic.onset$trialtype == "NoGo" & partic.onset$trial_response.corr == 0 & partic.onset$run == 2,]
        nogocommission.onset.2$trialtype = NULL
        nogocommission.onset.2$trial_response.corr = NULL
        nogocommission.onset.2$participant = NULL
        nogocommission.onset.2$session = NULL
        nogocommission.onset.2$run = NULL
      write.table(nogocommission.onset.2, file.path(path, sprintf("%s_GNGreg_run2_NoGo_commission.txt", id.session)), row.names=F, col.names=F)      
    # end of GNGreg onset file creation
    
  } else {
    # create base dataframe with all info needed for all GNGrew onset files
    stim.partic.onset <- partic.specific %>% select(stimonsetTime_actual, stimDur_expected, trialtype, trial_response.corr, reward, participant, session, run)
    stim.partic.onset$weight = 1
    stim.partic.onset <- stim.partic.onset[!is.na(stim.partic.onset$trialtype),]
    
    rew.partic.onset <- partic.specific %>% select(rewonsetTime_actual, rewDur_expected, trialtype, trial_response.corr, reward, participant, session, run)
    rew.partic.onset$weight = 1
    rew.partic.onset <- rew.partic.onset[!is.na(rew.partic.onset$trialtype),]
    
    ## GNGrew Go_hit STIM ONSET ##
    stim.gohit.onset.1 = stim.partic.onset[stim.partic.onset$trialtype == "Go" & stim.partic.onset$trial_response.corr == 1 & stim.partic.onset$run == 1,]
        path = "~/Dropbox/GNG L1 + L2 processing/"
        stim.gohit.onset.1$trialtype = NULL
        stim.gohit.onset.1$trial_response.corr = NULL
        stim.gohit.onset.1$reward = NULL
        stim.gohit.onset.1$participant = NULL
        stim.gohit.onset.1$session = NULL
        stim.gohit.onset.1$run = NULL
    write.table(stim.gohit.onset.1, file.path(path, sprintf("%s_GNGrew_run1_Go_hit.txt", id.session)), row.names=F, col.names=F)
    stim.gohit.onset.2 = stim.partic.onset[stim.partic.onset$trialtype == "Go" & stim.partic.onset$trial_response.corr == 1 & stim.partic.onset$run == 2,]
        stim.gohit.onset.2$trialtype = NULL
        stim.gohit.onset.2$trial_response.corr = NULL
        stim.gohit.onset.2$reward = NULL
        stim.gohit.onset.2$participant = NULL
        stim.gohit.onset.2$session = NULL
        stim.gohit.onset.2$run = NULL
    write.table(stim.gohit.onset.2, file.path(path, sprintf("%s_GNGrew_run2_Go_hit.txt", id.session)), row.names=F, col.names=F)
    stim.gohit.onset.3 = stim.partic.onset[stim.partic.onset$trialtype == "Go" & stim.partic.onset$trial_response.corr == 1 & stim.partic.onset$run == 3,]
        stim.gohit.onset.3$trialtype = NULL
        stim.gohit.onset.3$trial_response.corr = NULL
        stim.gohit.onset.3$reward = NULL
        stim.gohit.onset.3$participant = NULL
        stim.gohit.onset.3$session = NULL
        stim.gohit.onset.3$run = NULL
    write.table(stim.gohit.onset.3, file.path(path, sprintf("%s_GNGrew_run3_Go_hit.txt", id.session)), row.names=F, col.names=F)
    stim.gohit.onset.4 = stim.partic.onset[stim.partic.onset$trialtype == "Go" & stim.partic.onset$trial_response.corr == 1 & stim.partic.onset$run == 4,]
        stim.gohit.onset.4$trialtype = NULL
        stim.gohit.onset.4$trial_response.corr = NULL
        stim.gohit.onset.4$reward = NULL
        stim.gohit.onset.4$participant = NULL
        stim.gohit.onset.4$session = NULL
        stim.gohit.onset.4$run = NULL
    write.table(stim.gohit.onset.4, file.path(path, sprintf("%s_GNGrew_run4_Go_hit.txt", id.session)), row.names=F, col.names=F)
    
    ## GNGrew Go_omission STIM ONSET ##
    stim.goomiss.onset.1 = stim.partic.onset[stim.partic.onset$trialtype == "Go" & stim.partic.onset$trial_response.corr == 0 & stim.partic.onset$run == 1,]
    stim.goomiss.onset.1$trialtype = NULL
    stim.goomiss.onset.1$trial_response.corr = NULL
    stim.goomiss.onset.1$reward = NULL
    stim.goomiss.onset.1$participant = NULL
    stim.goomiss.onset.1$session = NULL
    stim.goomiss.onset.1$run = NULL
    write.table(stim.goomiss.onset.1, file.path(path, sprintf("%s_GNGrew_run1_Go_omission.txt", id.session)), row.names=F, col.names=F)
    stim.goomiss.onset.2 = stim.partic.onset[stim.partic.onset$trialtype == "Go" & stim.partic.onset$trial_response.corr == 0 & stim.partic.onset$run == 2,]
    stim.goomiss.onset.2$trialtype = NULL
    stim.goomiss.onset.2$trial_response.corr = NULL
    stim.goomiss.onset.2$reward = NULL
    stim.goomiss.onset.2$participant = NULL
    stim.goomiss.onset.2$session = NULL
    stim.goomiss.onset.2$run = NULL
    write.table(stim.goomiss.onset.2, file.path(path, sprintf("%s_GNGrew_run2_Go_omission.txt", id.session)), row.names=F, col.names=F)
    stim.goomiss.onset.3 = stim.partic.onset[stim.partic.onset$trialtype == "Go" & stim.partic.onset$trial_response.corr == 0 & stim.partic.onset$run == 3,]
    stim.goomiss.onset.3$trialtype = NULL
    stim.goomiss.onset.3$trial_response.corr = NULL
    stim.goomiss.onset.3$reward = NULL
    stim.goomiss.onset.3$participant = NULL
    stim.goomiss.onset.3$session = NULL
    stim.goomiss.onset.3$run = NULL
    write.table(stim.goomiss.onset.3, file.path(path, sprintf("%s_GNGrew_run3_Go_omission.txt", id.session)), row.names=F, col.names=F)
    stim.goomiss.onset.4 = stim.partic.onset[stim.partic.onset$trialtype == "Go" & stim.partic.onset$trial_response.corr == 0 & stim.partic.onset$run == 4,]
    stim.goomiss.onset.4$trialtype = NULL
    stim.goomiss.onset.4$trial_response.corr = NULL
    stim.goomiss.onset.4$reward = NULL
    stim.goomiss.onset.4$participant = NULL
    stim.goomiss.onset.4$session = NULL
    stim.goomiss.onset.4$run = NULL
    write.table(stim.goomiss.onset.4, file.path(path, sprintf("%s_GNGrew_run4_Go_omission.txt", id.session)), row.names=F, col.names=F)
    
    ## GNGrew NoGo_correct_reject STIM ONSET ##
    stim.nogocorrrej.onset.1 = stim.partic.onset[stim.partic.onset$trialtype == "NoGo" & stim.partic.onset$trial_response.corr == 1 & stim.partic.onset$run == 1,]
    stim.nogocorrrej.onset.1$trialtype = NULL
    stim.nogocorrrej.onset.1$trial_response.corr = NULL
    stim.nogocorrrej.onset.1$reward = NULL
    stim.nogocorrrej.onset.1$participant = NULL
    stim.nogocorrrej.onset.1$session = NULL
    stim.nogocorrrej.onset.1$run = NULL
    write.table(stim.nogocorrrej.onset.1, file.path(path, sprintf("%s_GNGrew_run1_NoGo_correct_reject.txt", id.session)), row.names=F, col.names=F)
    stim.nogocorrrej.onset.2 = stim.partic.onset[stim.partic.onset$trialtype == "NoGo" & stim.partic.onset$trial_response.corr == 1 & stim.partic.onset$run == 2,]
    stim.nogocorrrej.onset.2$trialtype = NULL
    stim.nogocorrrej.onset.2$trial_response.corr = NULL
    stim.nogocorrrej.onset.2$reward = NULL
    stim.nogocorrrej.onset.2$participant = NULL
    stim.nogocorrrej.onset.2$session = NULL
    stim.nogocorrrej.onset.2$run = NULL
    write.table(stim.nogocorrrej.onset.2, file.path(path, sprintf("%s_GNGrew_run2_NoGo_correct_reject.txt", id.session)), row.names=F, col.names=F)
    stim.nogocorrrej.onset.3 = stim.partic.onset[stim.partic.onset$trialtype == "NoGo" & stim.partic.onset$trial_response.corr == 1 & stim.partic.onset$run == 3,]
    stim.nogocorrrej.onset.3$trialtype = NULL
    stim.nogocorrrej.onset.3$trial_response.corr = NULL
    stim.nogocorrrej.onset.3$reward = NULL
    stim.nogocorrrej.onset.3$participant = NULL
    stim.nogocorrrej.onset.3$session = NULL
    stim.nogocorrrej.onset.3$run = NULL
    write.table(stim.nogocorrrej.onset.3, file.path(path, sprintf("%s_GNGrew_run3_NoGo_correct_reject.txt", id.session)), row.names=F, col.names=F)
    stim.nogocorrrej.onset.4 = stim.partic.onset[stim.partic.onset$trialtype == "NoGo" & stim.partic.onset$trial_response.corr == 1 & stim.partic.onset$run == 4,]
    stim.nogocorrrej.onset.4$trialtype = NULL
    stim.nogocorrrej.onset.4$trial_response.corr = NULL
    stim.nogocorrrej.onset.4$reward = NULL
    stim.nogocorrrej.onset.4$participant = NULL
    stim.nogocorrrej.onset.4$session = NULL
    stim.nogocorrrej.onset.4$run = NULL
    write.table(stim.nogocorrrej.onset.4, file.path(path, sprintf("%s_GNGrew_run4_NoGo_correct_reject.txt", id.session)), row.names=F, col.names=F)
    
    ## GNGrew NoGo_commission STIM ONSET ##
    stim.nogocomission.onset.1 = stim.partic.onset[stim.partic.onset$trialtype == "NoGo" & stim.partic.onset$trial_response.corr == 0 & stim.partic.onset$run == 1,]
    stim.nogocomission.onset.1$trialtype = NULL
    stim.nogocomission.onset.1$trial_response.corr = NULL
    stim.nogocomission.onset.1$reward = NULL
    stim.nogocomission.onset.1$participant = NULL
    stim.nogocomission.onset.1$session = NULL
    stim.nogocomission.onset.1$run = NULL
    write.table(stim.nogocomission.onset.1, file.path(path, sprintf("%s_GNGrew_run1_NoGo_commission.txt", id.session)), row.names=F, col.names=F)
    stim.nogocomission.onset.2 = stim.partic.onset[stim.partic.onset$trialtype == "NoGo" & stim.partic.onset$trial_response.corr == 0 & stim.partic.onset$run == 2,]
    stim.nogocomission.onset.2$trialtype = NULL
    stim.nogocomission.onset.2$trial_response.corr = NULL
    stim.nogocomission.onset.2$reward = NULL
    stim.nogocomission.onset.2$participant = NULL
    stim.nogocomission.onset.2$session = NULL
    stim.nogocomission.onset.2$run = NULL
    write.table(stim.nogocomission.onset.2, file.path(path, sprintf("%s_GNGrew_run2_NoGo_commission.txt", id.session)), row.names=F, col.names=F)
    stim.nogocomission.onset.3 = stim.partic.onset[stim.partic.onset$trialtype == "NoGo" & stim.partic.onset$trial_response.corr == 0 & stim.partic.onset$run == 3,]
    stim.nogocomission.onset.3$trialtype = NULL
    stim.nogocomission.onset.3$trial_response.corr = NULL
    stim.nogocomission.onset.3$reward = NULL
    stim.nogocomission.onset.3$participant = NULL
    stim.nogocomission.onset.3$session = NULL
    stim.nogocomission.onset.3$run = NULL
    write.table(stim.nogocomission.onset.3, file.path(path, sprintf("%s_GNGrew_run3_NoGo_commission.txt", id.session)), row.names=F, col.names=F)
    stim.nogocomission.onset.4 = stim.partic.onset[stim.partic.onset$trialtype == "NoGo" & stim.partic.onset$trial_response.corr == 0 & stim.partic.onset$run == 4,]
    stim.nogocomission.onset.4$trialtype = NULL
    stim.nogocomission.onset.4$trial_response.corr = NULL
    stim.nogocomission.onset.4$reward = NULL
    stim.nogocomission.onset.4$participant = NULL
    stim.nogocomission.onset.4$session = NULL
    stim.nogocomission.onset.4$run = NULL
    write.table(stim.nogocomission.onset.4, file.path(path, sprintf("%s_GNGrew_run4_NoGo_commission.txt", id.session)), row.names=F, col.names=F)
    
    ####
    ## GNGrew Go_hit_rew REWARD ONSET ##
    rew.gohit.rew.onset.1 = rew.partic.onset[rew.partic.onset$trialtype == "Go" & rew.partic.onset$trial_response.corr == 1 & rew.partic.onset$reward == 1 & rew.partic.onset$run == 1,]
    rew.gohit.rew.onset.1$trialtype = NULL
    rew.gohit.rew.onset.1$trial_response.corr = NULL
    rew.gohit.rew.onset.1$reward = NULL
    rew.gohit.rew.onset.1$participant = NULL
    rew.gohit.rew.onset.1$session = NULL
    rew.gohit.rew.onset.1$run = NULL
    write.table(rew.gohit.rew.onset.1, file.path(path, sprintf("%s_GNGrew_run1_Go_hit_rew.txt", id.session)), row.names=F, col.names=F)
    rew.gohit.rew.onset.2 = rew.partic.onset[rew.partic.onset$trialtype == "Go" & rew.partic.onset$trial_response.corr == 1 & rew.partic.onset$reward == 1 & rew.partic.onset$run == 2,]
    rew.gohit.rew.onset.2$trialtype = NULL
    rew.gohit.rew.onset.2$trial_response.corr = NULL
    rew.gohit.rew.onset.2$reward = NULL
    rew.gohit.rew.onset.2$participant = NULL
    rew.gohit.rew.onset.2$session = NULL
    rew.gohit.rew.onset.2$run = NULL
    write.table(rew.gohit.rew.onset.2, file.path(path, sprintf("%s_GNGrew_run2_Go_hit_rew.txt", id.session)), row.names=F, col.names=F)
    rew.gohit.rew.onset.3 = rew.partic.onset[rew.partic.onset$trialtype == "Go" & rew.partic.onset$trial_response.corr == 1 & rew.partic.onset$reward == 1 & rew.partic.onset$run == 3,]
    rew.gohit.rew.onset.3$trialtype = NULL
    rew.gohit.rew.onset.3$trial_response.corr = NULL
    rew.gohit.rew.onset.3$reward = NULL
    rew.gohit.rew.onset.3$participant = NULL
    rew.gohit.rew.onset.3$session = NULL
    rew.gohit.rew.onset.3$run = NULL
    write.table(rew.gohit.rew.onset.3, file.path(path, sprintf("%s_GNGrew_run3_Go_hit_rew.txt", id.session)), row.names=F, col.names=F)
    rew.gohit.rew.onset.4 = rew.partic.onset[rew.partic.onset$trialtype == "Go" & rew.partic.onset$trial_response.corr == 1 & rew.partic.onset$reward == 1 & rew.partic.onset$run == 4,]
    rew.gohit.rew.onset.4$trialtype = NULL
    rew.gohit.rew.onset.4$trial_response.corr = NULL
    rew.gohit.rew.onset.4$reward = NULL
    rew.gohit.rew.onset.4$participant = NULL
    rew.gohit.rew.onset.4$session = NULL
    rew.gohit.rew.onset.4$run = NULL
    write.table(rew.gohit.rew.onset.4, file.path(path, sprintf("%s_GNGrew_run4_Go_hit_rew.txt", id.session)), row.names=F, col.names=F)
    
    ## GNGrew Go_hit_norew REWARD ONSET ## 
    rew.gohit.norew.onset.1 = rew.partic.onset[rew.partic.onset$trialtype == "Go" & rew.partic.onset$trial_response.corr == 1 & rew.partic.onset$reward == 0 & rew.partic.onset$run == 1,]
    rew.gohit.norew.onset.1$trialtype = NULL
    rew.gohit.norew.onset.1$trial_response.corr = NULL
    rew.gohit.norew.onset.1$reward = NULL
    rew.gohit.norew.onset.1$participant = NULL
    rew.gohit.norew.onset.1$session = NULL
    rew.gohit.norew.onset.1$run = NULL
    write.table(rew.gohit.norew.onset.1, file.path(path, sprintf("%s_GNGrew_run1_Go_hit_norew.txt", id.session)), row.names=F, col.names=F)
    rew.gohit.norew.onset.2 = rew.partic.onset[rew.partic.onset$trialtype == "Go" & rew.partic.onset$trial_response.corr == 1 & rew.partic.onset$reward == 0 & rew.partic.onset$run == 2,]
    rew.gohit.norew.onset.2$trialtype = NULL
    rew.gohit.norew.onset.2$trial_response.corr = NULL
    rew.gohit.norew.onset.2$reward = NULL
    rew.gohit.norew.onset.2$participant = NULL
    rew.gohit.norew.onset.2$session = NULL
    rew.gohit.norew.onset.2$run = NULL
    write.table(rew.gohit.norew.onset.2, file.path(path, sprintf("%s_GNGrew_run2_Go_hit_norew.txt", id.session)), row.names=F, col.names=F)
    rew.gohit.norew.onset.3 = rew.partic.onset[rew.partic.onset$trialtype == "Go" & rew.partic.onset$trial_response.corr == 1 & rew.partic.onset$reward == 0 & rew.partic.onset$run == 3,]
    rew.gohit.norew.onset.3$trialtype = NULL
    rew.gohit.norew.onset.3$trial_response.corr = NULL
    rew.gohit.norew.onset.3$reward = NULL
    rew.gohit.norew.onset.3$participant = NULL
    rew.gohit.norew.onset.3$session = NULL
    rew.gohit.norew.onset.3$run = NULL
    write.table(rew.gohit.norew.onset.3, file.path(path, sprintf("%s_GNGrew_run3_Go_hit_norew.txt", id.session)), row.names=F, col.names=F)
    rew.gohit.norew.onset.4 = rew.partic.onset[rew.partic.onset$trialtype == "Go" & rew.partic.onset$trial_response.corr == 1 & rew.partic.onset$reward == 0 & rew.partic.onset$run == 4,]
    rew.gohit.norew.onset.4$trialtype = NULL
    rew.gohit.norew.onset.4$trial_response.corr = NULL
    rew.gohit.norew.onset.4$reward = NULL
    rew.gohit.norew.onset.4$participant = NULL
    rew.gohit.norew.onset.4$session = NULL
    rew.gohit.norew.onset.4$run = NULL
    write.table(rew.gohit.norew.onset.4, file.path(path, sprintf("%s_GNGrew_run4_Go_hit_norew.txt", id.session)), row.names=F, col.names=F) 
    
    ## GNGrew Go_omission_norew REWARD ONSET ##
    rew.goomiss.norew.onset.1 = rew.partic.onset[rew.partic.onset$trialtype == "Go" & rew.partic.onset$trial_response.corr == 0 & rew.partic.onset$reward == 0 & rew.partic.onset$run == 1,]
    rew.goomiss.norew.onset.1$trialtype = NULL
    rew.goomiss.norew.onset.1$trial_response.corr = NULL
    rew.goomiss.norew.onset.1$reward = NULL
    rew.goomiss.norew.onset.1$participant = NULL
    rew.goomiss.norew.onset.1$session = NULL
    rew.goomiss.norew.onset.1$run = NULL
    write.table(rew.goomiss.norew.onset.1, file.path(path, sprintf("%s_GNGrew_run1_Go_omiss_norew.txt", id.session)), row.names=F, col.names=F)
    rew.goomiss.norew.onset.2 = rew.partic.onset[rew.partic.onset$trialtype == "Go" & rew.partic.onset$trial_response.corr == 0 & rew.partic.onset$reward == 0 & rew.partic.onset$run == 2,]
    rew.goomiss.norew.onset.2$trialtype = NULL
    rew.goomiss.norew.onset.2$trial_response.corr = NULL
    rew.goomiss.norew.onset.2$reward = NULL
    rew.goomiss.norew.onset.2$participant = NULL
    rew.goomiss.norew.onset.2$session = NULL
    rew.goomiss.norew.onset.2$run = NULL
    write.table(rew.goomiss.norew.onset.2, file.path(path, sprintf("%s_GNGrew_run2_Go_omiss_norew.txt", id.session)), row.names=F, col.names=F)
    rew.goomiss.norew.onset.3 = rew.partic.onset[rew.partic.onset$trialtype == "Go" & rew.partic.onset$trial_response.corr == 0 & rew.partic.onset$reward == 0 & rew.partic.onset$run == 3,]
    rew.goomiss.norew.onset.3$trialtype = NULL
    rew.goomiss.norew.onset.3$trial_response.corr = NULL
    rew.goomiss.norew.onset.3$reward = NULL
    rew.goomiss.norew.onset.3$participant = NULL
    rew.goomiss.norew.onset.3$session = NULL
    rew.goomiss.norew.onset.3$run = NULL
    write.table(rew.goomiss.norew.onset.3, file.path(path, sprintf("%s_GNGrew_run3_Go_omiss_norew.txt", id.session)), row.names=F, col.names=F)
    rew.goomiss.norew.onset.4 = rew.partic.onset[rew.partic.onset$trialtype == "Go" & rew.partic.onset$trial_response.corr == 0 & rew.partic.onset$reward == 0 & rew.partic.onset$run == 4,]
    rew.goomiss.norew.onset.4$trialtype = NULL
    rew.goomiss.norew.onset.4$trial_response.corr = NULL
    rew.goomiss.norew.onset.4$reward = NULL
    rew.goomiss.norew.onset.4$participant = NULL
    rew.goomiss.norew.onset.4$session = NULL
    rew.goomiss.norew.onset.4$run = NULL
    write.table(rew.goomiss.norew.onset.4, file.path(path, sprintf("%s_GNGrew_run4_Go_omiss_norew.txt", id.session)), row.names=F, col.names=F)
    
    ## GNGrew NoGo_correct_reject_rew REWARD ONSET ##
    rew.nogocorrrej.rew.onset.1 = rew.partic.onset[rew.partic.onset$trialtype == "NoGo" & rew.partic.onset$trial_response.corr == 1 & rew.partic.onset$reward == 5 & rew.partic.onset$run == 1,]
    rew.nogocorrrej.rew.onset.1$trialtype = NULL
    rew.nogocorrrej.rew.onset.1$trial_response.corr = NULL
    rew.nogocorrrej.rew.onset.1$reward = NULL
    rew.nogocorrrej.rew.onset.1$participant = NULL
    rew.nogocorrrej.rew.onset.1$session = NULL
    rew.nogocorrrej.rew.onset.1$run = NULL
    write.table(rew.nogocorrrej.rew.onset.1, file.path(path, sprintf("%s_GNGrew_run1_NoGo_correct_reject_rew.txt", id.session)), row.names=F, col.names=F)
    rew.nogocorrrej.rew.onset.2 = rew.partic.onset[rew.partic.onset$trialtype == "NoGo" & rew.partic.onset$trial_response.corr == 1 & rew.partic.onset$reward == 5 & rew.partic.onset$run == 2,]
    rew.nogocorrrej.rew.onset.2$trialtype = NULL
    rew.nogocorrrej.rew.onset.2$trial_response.corr = NULL
    rew.nogocorrrej.rew.onset.2$reward = NULL
    rew.nogocorrrej.rew.onset.2$participant = NULL
    rew.nogocorrrej.rew.onset.2$session = NULL
    rew.nogocorrrej.rew.onset.2$run = NULL
    write.table(rew.nogocorrrej.rew.onset.2, file.path(path, sprintf("%s_GNGrew_run2_NoGo_correct_reject_rew.txt", id.session)), row.names=F, col.names=F)
    rew.nogocorrrej.rew.onset.3 = rew.partic.onset[rew.partic.onset$trialtype == "NoGo" & rew.partic.onset$trial_response.corr == 1 & rew.partic.onset$reward == 5 & rew.partic.onset$run == 3,]
    rew.nogocorrrej.rew.onset.3$trialtype = NULL
    rew.nogocorrrej.rew.onset.3$trial_response.corr = NULL
    rew.nogocorrrej.rew.onset.3$reward = NULL
    rew.nogocorrrej.rew.onset.3$participant = NULL
    rew.nogocorrrej.rew.onset.3$session = NULL
    rew.nogocorrrej.rew.onset.3$run = NULL
    write.table(rew.nogocorrrej.rew.onset.3, file.path(path, sprintf("%s_GNGrew_run3_NoGo_correct_reject_rew.txt", id.session)), row.names=F, col.names=F)
    rew.nogocorrrej.rew.onset.4 = rew.partic.onset[rew.partic.onset$trialtype == "NoGo" & rew.partic.onset$trial_response.corr == 1 & rew.partic.onset$reward == 5 & rew.partic.onset$run == 4,]
    rew.nogocorrrej.rew.onset.4$trialtype = NULL
    rew.nogocorrrej.rew.onset.4$trial_response.corr = NULL
    rew.nogocorrrej.rew.onset.4$reward = NULL
    rew.nogocorrrej.rew.onset.4$participant = NULL
    rew.nogocorrrej.rew.onset.4$session = NULL
    rew.nogocorrrej.rew.onset.4$run = NULL
    write.table(rew.nogocorrrej.rew.onset.4, file.path(path, sprintf("%s_GNGrew_run4_NoGo_correct_reject_rew.txt", id.session)), row.names=F, col.names=F)
    
    ## GNGrew NoGo_commission_norew REWARD ONSET ##
    rew.nogocomission.rew.onset.1 = rew.partic.onset[rew.partic.onset$trialtype == "NoGo" & rew.partic.onset$trial_response.corr == 0 & rew.partic.onset$reward == 0 & rew.partic.onset$run == 1,]
    rew.nogocomission.rew.onset.1$trialtype = NULL
    rew.nogocomission.rew.onset.1$trial_response.corr = NULL
    rew.nogocomission.rew.onset.1$reward = NULL
    rew.nogocomission.rew.onset.1$participant = NULL
    rew.nogocomission.rew.onset.1$session = NULL
    rew.nogocomission.rew.onset.1$run = NULL
    write.table(rew.nogocomission.rew.onset.1, file.path(path, sprintf("%s_GNGrew_run1_NoGo_comission_norew.txt", id.session)), row.names=F, col.names=F)
    rew.nogocomission.rew.onset.2 = rew.partic.onset[rew.partic.onset$trialtype == "NoGo" & rew.partic.onset$trial_response.corr == 0 & rew.partic.onset$reward == 0 & rew.partic.onset$run == 2,]
    rew.nogocomission.rew.onset.2$trialtype = NULL
    rew.nogocomission.rew.onset.2$trial_response.corr = NULL
    rew.nogocomission.rew.onset.2$reward = NULL
    rew.nogocomission.rew.onset.2$participant = NULL
    rew.nogocomission.rew.onset.2$session = NULL
    rew.nogocomission.rew.onset.2$run = NULL
    write.table(rew.nogocomission.rew.onset.2, file.path(path, sprintf("%s_GNGrew_run2_NoGo_ccomission_norew.txt", id.session)), row.names=F, col.names=F)
    rew.nogocomission.rew.onset.3 = rew.partic.onset[rew.partic.onset$trialtype == "NoGo" & rew.partic.onset$trial_response.corr == 0 & rew.partic.onset$reward == 0 & rew.partic.onset$run == 3,]
    rew.nogocomission.rew.onset.3$trialtype = NULL
    rew.nogocomission.rew.onset.3$trial_response.corr = NULL
    rew.nogocomission.rew.onset.3$reward = NULL
    rew.nogocomission.rew.onset.3$participant = NULL
    rew.nogocomission.rew.onset.3$session = NULL
    rew.nogocomission.rew.onset.3$run = NULL
    write.table(rew.nogocomission.rew.onset.3, file.path(path, sprintf("%s_GNGrew_run3_NoGo_comission_norew.txt", id.session)), row.names=F, col.names=F)
    rew.nogocomission.rew.onset.4 = rew.partic.onset[rew.partic.onset$trialtype == "NoGo" & rew.partic.onset$trial_response.corr == 0 & rew.partic.onset$reward == 0 & rew.partic.onset$run == 4,]
    rew.nogocomission.rew.onset.4$trialtype = NULL
    rew.nogocomission.rew.onset.4$trial_response.corr = NULL
    rew.nogocomission.rew.onset.4$reward = NULL
    rew.nogocomission.rew.onset.4$participant = NULL
    rew.nogocomission.rew.onset.4$session = NULL
    rew.nogocomission.rew.onset.4$run = NULL
    write.table(rew.nogocomission.rew.onset.4, file.path(path, sprintf("%s_GNGrew_run4_NoGo_comission_norew.txt", id.session)), row.names=F, col.names=F)
  }

} 








