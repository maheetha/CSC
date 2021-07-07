#!/usr/bin/R
library("trotter")
args = commandArgs(trailingOnly=TRUE)
table = read.table(args[1], header = T, stringsAsFactors = F, sep = "\t", fill = T)

## subset by race if needed ###
white = subset(table, table$Race == 1)
black = subset(table, table$Race == 2)

#table = white or black depending on what you ned

## Create empty vectors to hold your values ##
ny_1 = c()
ny_2 = c()
ny_3 = c()
co_1 = c()
co_2 = c()
co_3 = c()
raw_1 = c()
raw_2 = c()
raw_3 = c()

ny_age_1 = c()
ny_age_2 = c()
ny_age_3 = c()
raw_age_1 = c()
raw_age_2 = c()
raw_age_3 = c()
co_age_1 = c()
co_age_2 = c()
co_age_3 = c()


# This is to create all possible combinations of patients
groupnum = as.numeric(as.character(args[2]))
list_of_indices = seq(1, nrow(table), by=1)
combs5 = cpv(groupnum, list_of_indices)
list_of_5s = seq(1, length(combs5))


for (m in seq(1:as.numeric(as.character(args[3])))){
  
  ny = ""
  raw = ""
  co = ""
  ny_age = ""
  raw_age = ""
  co_age = ""
  set.seed(m) ## setting seed and getting random sample of X number of combinations for all combinations possible
  mil_sample_5 = sample(list_of_5s, as.numeric(as.character(args[4])), replace = FALSE)
  
  
  for (x in mil_sample_5){
    
    
    outcomes = table$Outcomes[combs5[x]]
    
    #Accounting for whether the group is a mixed group of survivors and deceased
    mixed = ""
    if (sum(outcomes) != 0 & sum(outcomes) != groupnum){
      mixed = "Mixed"
    } else {
      mixed = "Not Mixed"
    }
    
    
    #Calculate which individual NY would have chosen for this combo of patients
    ny_scores = table$NY[combs5[x]] 
    min_score = min(ny_scores)
    thematch = which(ny_scores == min_score)
    
    if (length(thematch) == 1){
      outcome = outcomes[thematch]
      if (outcome == 1){
        ny = paste(ny, mixed, "\t","NonLottery\tSurvivor\n", sep = "")
      } else {
        ny = paste(ny, mixed, "\t","NonLottery\tDeceased\n", sep = "")
        
      }
    } else {
      win = sample(thematch, size = 1)
      outcome = outcomes[win]
      if (outcome == 1){
        ny =  paste(ny, mixed, "\t","Lottery\tSurvivor\n", sep = "")
      } else {
        ny = paste(ny, mixed, "\t","Lottery\tDeceased\n", sep = "")
        
      }
    }
    
    
    #Calculate which individual Raw Sofa would have chosen for this combo of patients
    raw_scores = table$SOFA[combs5[x]]
    min_score = min(raw_scores)
    thematch = which(raw_scores == min_score)
    if (length(thematch) == 1){
      outcome = outcomes[thematch]
      if (outcome == 1){
        raw = paste(raw, mixed, "\t","NonLottery\tSurvivor\n", sep = "")
      } else {
        raw = paste(raw, mixed, "\t","NonLottery\tDeceased\n", sep = "")
      }
    } else {
      win = sample(thematch, size = 1)
      outcome = outcomes[win]
      if (outcome == 1){
        raw = paste(raw, mixed, "\t","Lottery\tSurvivor\n", sep = "")
      } else {
        raw = paste(raw, mixed, "\t","Lottery\tDeceased\n", sep = "")
      }
    }
    
    
    #Calculate which individual Colorado would have chosen for this combo of patients
    co_scores = table$Colorado[combs5[x]]
    min_score = min(co_scores)
    thematch = which(co_scores == min_score)
    if (length(thematch) == 1){
      outcome = outcomes[thematch]
      if (outcome == 1){
        co = paste(co, mixed, "\t","NonLottery\tSurvivor\n", sep = "")
      } else {
        co = paste(co, mixed, "\t","NonLottery\tDeceased\n", sep = "")
      }
    } else {
      win = sample(thematch, size = 1)
      outcome = outcomes[win]
      if (outcome == 1){
        co = paste(co, mixed, "\t", "Lottery\tSurvivor\n", sep = "")
      } else {
        co = paste(co, mixed, "\t","Lottery\tDeceased\n", sep = "")
      }
    }
    
    #Calculate which individual New York with Age algorithm would have chosen for this combo of patients
    ny_age_scores = table$NY_Age[combs5[x]]
    min_score = min(ny_age_scores)
    thematch = which(ny_age_scores == min_score)
    if (length(thematch) == 1){
      outcome = outcomes[thematch]
      if (outcome == 1){
        ny_age = paste(ny_age, mixed, "\t", "NonLottery\tSurvivor\n", sep = "")
      } else {
        ny_age = paste(ny_age, mixed, "\t", "NonLottery\tDeceased\n", sep = "")
      }
    } else {
      win = sample(thematch, size = 1)
      outcome = outcomes[win]
      if (outcome == 1){
        ny_age = paste(ny_age, mixed, "\t", "Lottery\tSurvivor\n", sep = "")
      } else {
        ny_age = paste(ny_age, mixed, "\t", "Lottery\tDeceased\n", sep = "")
      }
    }
    
    #Calculate which individual Raw Sofa with Age algorithm would have chosen for this combo of patients
    raw_age_scores = table$SOFA_Age[combs5[x]]
    min_score = min(raw_age_scores)
    thematch = which(raw_age_scores == min_score)
    if (length(thematch) == 1){
      outcome = outcomes[thematch]
      if (outcome == 1){
        raw_age = paste(raw_age, mixed, "\t", "NonLottery\tSurvivor\n", sep = "")
      } else {
        raw_age = paste(raw_age, mixed, "\t", "NonLottery\tDeceased\n", sep = "")
      }
    } else {
      win = sample(thematch, size = 1)
      outcome = outcomes[win]
      if (outcome == 1){
        raw_age = paste(raw_age, mixed, "\t", "Lottery\tSurvivor\n", sep = "")
      } else {
        raw_age = paste(raw_age, mixed, "\t", "Lottery\tDeceased\n", sep = "")
      }
    }
    
    
    #Calculate which individual Colorado with Age algorithm would have chosen for this combo of patients
    co_age_scores = table$Colorado_Age[combs5[x]]
    min_score = min(co_age_scores)
    thematch = which(co_age_scores == min_score)
    if (length(thematch) == 1){
      outcome = outcomes[thematch]
      if (outcome == 1){
        co_age = paste(co_age, mixed, "\t", "NonLottery\tSurvivor\n", sep = "")
      } else {
        co_age = paste(co_age, mixed, "\t", "NonLottery\tDeceased\n", sep = "")
      }
    } else {
      win = sample(thematch, size = 1)
      outcome = outcomes[win]
      if (outcome == 1){
        co_age = paste(co_age, mixed, "\t", "Lottery\tSurvivor\n", sep = "")
      } else {
        co_age = paste(co_age, mixed, "\t", "Lottery\tDeceased\n", sep = "")
      }
    }
    
  }
  
  
  
  
  write.table(ny, "ny.txt", quote = F, row.names = F, col.names = F)
  write.table(co, "co.txt", quote = F, row.names = F, col.names = F)
  write.table(raw, "raw.txt", quote = F, row.names = F, col.names = F)
  write.table(ny_age, "ny_age.txt", quote = F, row.names = F, col.names = F)
  write.table(co_age, "co_age.txt", quote = F, row.names = F, col.names = F)
  write.table(raw_age, "raw_age.txt", quote = F, row.names = F, col.names = F)
  
  
  ####### Calculatee Percentages where algorithm chose without lottery and where algorithm chose survivor ######
  
  newyork = read.table("ny.txt", header = F, sep = "\t", stringsAsFactors = F)
  colorado = read.table("co.txt", header = F, sep = "\t", stringsAsFactors = F)
  rawsofa = read.table("raw.txt", header = F, sep = "\t", stringsAsFactors = F)
  newyorkage = read.table('ny_age.txt', header = F, sep = "\t", stringsAsFactors = F)
  coloradoage = read.table('co_age.txt', header = F, sep = "\t", stringsAsFactors = F)
  rawsofaage = read.table('raw_age.txt', header = F, sep = "\t", stringsAsFactors = F)
  
  num_mixed = nrow(subset(newyork, newyork$V1 == "Mixed")) ## denominator is number of mixed outcome groups
  mixed_newyork = subset(newyork, newyork$V1 == "Mixed") 
  mixed_colorado = subset(colorado, colorado$V1 == "Mixed")
  mixed_rawsofa = subset(rawsofa, rawsofa$V1 == "Mixed")
  mixed_newyorkage = subset(newyorkage, newyorkage$V1 == "Mixed")
  mixed_coloradoage = subset(coloradoage, coloradoage$V1 == "Mixed")
  mixed_rawsofaage = subset(rawsofaage, rawsofaage$V1 == "Mixed")
  
  
  ## Calculate percentages for New York Algorithm
  percent_nonlottery = nrow(subset(mixed_newyork, mixed_newyork$V2 != "Lottery"))/num_mixed
  mixed_not_lottery = subset(mixed_newyork, mixed_newyork$V2 != "Lottery")
  percent_survivor =  nrow(subset(mixed_not_lottery, mixed_not_lottery$V3 == "Survivor"))/nrow(mixed_not_lottery)
  percent_all = nrow(subset(mixed_not_lottery, mixed_not_lottery$V3 == "Survivor"))/nrow(mixed_newyork)
  percent_correct =  (nrow(subset(mixed_newyork, mixed_newyork$V3 == "Survivor")))/num_mixed
  
  ny_1 = c(ny_1, percent_nonlottery)
  ny_2 = c(ny_2, percent_survivor)
  ny_3 = c(ny_3, percent_correct)
  
  ## Calculate percentages for Colorado Algorithm
  
  percent_nonlottery = nrow(subset(mixed_colorado, mixed_colorado$V2 != "Lottery"))/num_mixed
  mixed_not_lottery = subset(mixed_colorado, mixed_colorado$V2 != "Lottery")
  percent_survivor =  nrow(subset(mixed_not_lottery, mixed_not_lottery$V3 == "Survivor"))/nrow(mixed_not_lottery)
  percent_all = nrow(subset(mixed_not_lottery, mixed_not_lottery$V3 == "Survivor"))/nrow(mixed_newyork)
  percent_correct =  (nrow(subset(mixed_colorado, mixed_colorado$V3 == "Survivor")))/num_mixed
  
  co_1 = c(co_1, percent_nonlottery)
  co_2 = c(co_2, percent_survivor)
  co_3 = c(co_3, percent_correct)
  
  ## Calculate percentages for Raw Sofa Algorithm
  percent_nonlottery = nrow(subset(mixed_rawsofa, mixed_rawsofa$V2 != "Lottery"))/num_mixed
  mixed_not_lottery = subset(mixed_rawsofa, mixed_rawsofa$V2 != "Lottery")
  percent_survivor =  nrow(subset(mixed_not_lottery, mixed_not_lottery$V3 == "Survivor"))/nrow(mixed_not_lottery)
  percent_all = nrow(subset(mixed_not_lottery, mixed_not_lottery$V3 == "Survivor"))/nrow(mixed_newyork)
  percent_correct =  (nrow(subset(mixed_rawsofa, mixed_rawsofa$V3 == "Survivor")))/num_mixed
  
  raw_1 = c(raw_1, percent_nonlottery)
  raw_2 = c(raw_2, percent_survivor)
  raw_3 = c(raw_3, percent_correct)
  
  ## Calculate percentages for New York with Age Algorithm
  percent_nonlottery = nrow(subset(mixed_newyorkage, mixed_newyorkage$V2 != "Lottery"))/num_mixed
  mixed_not_lottery = subset(mixed_newyorkage, mixed_newyorkage$V2 != "Lottery")
  percent_survivor =  nrow(subset(mixed_not_lottery, mixed_not_lottery$V3 == "Survivor"))/nrow(mixed_not_lottery)
  percent_all = nrow(subset(mixed_not_lottery, mixed_not_lottery$V3 == "Survivor"))/nrow(mixed_newyork)
  percent_correct =  (nrow(subset(mixed_newyorkage, mixed_newyorkage$V3 == "Survivor")))/num_mixed
  
  
  ny_age_1 = c(ny_age_1, percent_nonlottery)
  ny_age_2 = c(ny_age_2, percent_survivor)
  ny_age_3 = c(ny_age_3, percent_correct)
  
  ## Calculate percentages for Colorado with Age Algorithm
  percent_nonlottery = nrow(subset(mixed_coloradoage, mixed_coloradoage$V2 != "Lottery"))/num_mixed
  mixed_not_lottery = subset(mixed_coloradoage, mixed_coloradoage$V2 != "Lottery")
  percent_survivor =  nrow(subset(mixed_not_lottery, mixed_not_lottery$V3 == "Survivor"))/nrow(mixed_not_lottery)
  percent_all = nrow(subset(mixed_not_lottery, mixed_not_lottery$V3 == "Survivor"))/nrow(mixed_newyork)
  percent_correct =  (nrow(subset(mixed_coloradoage, mixed_coloradoage$V3 == "Survivor")))/num_mixed
  
  
  co_age_1 = c(co_age_1, percent_nonlottery)
  co_age_2 = c(co_age_2, percent_survivor)
  co_age_3 = c(co_age_3, percent_correct)
  
  ## Calculate percentages for Raw Sofa with Age Algorithm
  percent_nonlottery = nrow(subset(mixed_rawsofaage, mixed_rawsofaage$V2 != "Lottery"))/num_mixed
  mixed_not_lottery = subset(mixed_rawsofaage, mixed_rawsofaage$V2 != "Lottery")
  percent_survivor =  nrow(subset(mixed_not_lottery, mixed_not_lottery$V3 == "Survivor"))/nrow(mixed_not_lottery)
  percent_all = nrow(subset(mixed_not_lottery, mixed_not_lottery$V3 == "Survivor"))/nrow(mixed_newyork)
  percent_correct =  (nrow(subset(mixed_rawsofaage, mixed_rawsofaage$V3 == "Survivor")))/num_mixed
  
  raw_age_1 = c(raw_age_1, percent_nonlottery)
  raw_age_2 = c(raw_age_2, percent_survivor)
  raw_age_3 = c(raw_age_3, percent_correct)
  
}


final = paste("New York\t", (round(mean(ny_1, na.rm = T),3)*100), "\t", (round(quantile(ny_1,.025, na.rm = T),3)*100), "-", (round(quantile(ny_1,.975, na.rm=T),3)*100),
              "\t", (round(mean(ny_2, na.rm = T),3)*100), "\t", (round(quantile(ny_2,.025, na.rm = T),3)*100), "-", (round(quantile(ny_2,.975, na.rm = T),3)*100),
              "\t", (round(mean(ny_3, na.rm = T),3)*100), "\t", (round(quantile(ny_3,.025, na.rm = T),3)*100), "-", (round(quantile(ny_3,.975, na.rm = T),3)*100),
              "\n", "Colorado\t", (round(mean(co_1, na.rm = T),3)*100), "\t", (round(quantile(co_1,.025, na.rm = T),3)*100), "-", (round(quantile(co_1,.975, na.rm=T),3)*100),
              "\t", (round(mean(co_2, na.rm = T),3)*100), "\t", (round(quantile(co_2,.025, na.rm = T),3)*100), "-", (round(quantile(co_2,.975, na.rm = T),3)*100),
              "\t", (round(mean(co_3, na.rm = T),3)*100), "\t", (round(quantile(co_3,.025, na.rm = T),3)*100), "-", (round(quantile(co_3,.975, na.rm = T),3)*100),
              "\n", "Raw Sofa\t", (round(mean(raw_1, na.rm = T),3)*100), "\t", (round(quantile(raw_1,.025, na.rm = T),3)*100), "-", (round(quantile(raw_1,.975, na.rm=T),3)*100),
              "\t", (round(mean(raw_2, na.rm = T),3)*100), "\t", (round(quantile(raw_2,.025, na.rm = T),3)*100), "-", (round(quantile(raw_2,.975, na.rm = T),3)*100),
              "\t", (round(mean(raw_3, na.rm = T),3)*100), "\t", (round(quantile(raw_3,.025, na.rm = T),3)*100), "-", (round(quantile(raw_3,.975, na.rm = T),3)*100),
              "\n", "New York + Age\t", (round(mean(ny_age_1, na.rm = T),3)*100), "\t", (round(quantile(ny_age_1,.025, na.rm = T),3)*100), "-", (round(quantile(ny_age_1,.975, na.rm=T),3)*100),
              "\t", (round(mean(ny_age_2, na.rm = T),3)*100), "\t", (round(quantile(ny_age_2,.025, na.rm = T),3)*100), "-", (round(quantile(ny_age_2,.975, na.rm = T),3)*100),
              "\t", (round(mean(ny_age_3, na.rm = T),3)*100), "\t", (round(quantile(ny_age_3,.025, na.rm = T),3)*100), "-", (round(quantile(ny_age_3,.975, na.rm = T),3)*100),
              "\n", "Colorado + Age\t", (round(mean(co_age_1, na.rm = T),3)*100), "\t", (round(quantile(co_age_1,.025, na.rm = T),3)*100), "-", (round(quantile(co_age_1,.975, na.rm=T),3)*100),
              "\t", (round(mean(co_age_2, na.rm = T),3)*100), "\t", (round(quantile(co_age_2,.025, na.rm = T),3)*100), "-", (round(quantile(co_age_2,.975, na.rm = T),3)*100),
              "\t", (round(mean(co_age_3, na.rm = T),3)*100), "\t", (round(quantile(co_age_3,.025, na.rm = T),3)*100), "-", (round(quantile(co_age_3,.975, na.rm = T),3)*100),
              "\n", "Raw Sofa + Age\t", (round(mean(raw_age_1, na.rm = T),3)*100), "\t", (round(quantile(raw_age_1,.025, na.rm = T),3)*100), "-", (round(quantile(raw_age_1,.975, na.rm=T),3)*100),
              "\t", (round(mean(raw_age_2, na.rm = T),3)*100), "\t", (round(quantile(raw_age_2,.025, na.rm = T),3)*100), "-", (round(quantile(raw_age_2,.975, na.rm = T),3)*100),
              "\t", (round(mean(raw_age_3, na.rm = T),3)*100), "\t", (round(quantile(raw_age_3,.025, na.rm = T),3)*100), "-", (round(quantile(raw_age_3,.975, na.rm = T),3)*100),
              "\n", sep = "")

write.table(final, args[5], quote = F, sep = "\t", row.names = F)

