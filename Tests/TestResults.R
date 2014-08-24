rm(list = ls())
source('C://Users/Roux-Cil//Desktop//Masters File//Modgen//TsetseDynamics//Tests/test_helpers.R')
require(modgenTester)
odbcCloseAll()
sd <- "C:/Users/Roux-Cil/Desktop/Masters File/Modgen/TsetseDynamics"
td <- "C:/Users/Roux-Cil/Desktop/Masters File/Modgen/TsetseDynamics/Tests"
setwd(td)

all_results <- list_all_results(td)

db_channels <- list()
for (i in all_results){
  db_channel <- access_database(i)
  scenario_name <- gsub("\\(.*$", "", gsub("^.*/", "", i))
  db_channels[[scenario_name]] <- db_channel
}

Tsetse_cohort_Tester <- function(db_channel)
{
  require(plyr)
  require(sqldf)
  require(xtable)
  require(survival)
  require(survMisc)
  require(reshape2)
  out <- list()
  
  WD <- as.data.frame(new_RTable("Tsetse_Tester", db_channel))
  names(WD)[2] <- c('ActorID')
  WD$metrics <- factor(WD$metrics, labels = c('duration', 'unit', 'birth', 'first', 'sec', 'third', 'total'))
  
  Tsetse <- dcast(data = WD, formula = ActorID + gender ~ metrics, value.var = 'Value')
  Tsetse <- Tsetse[Tsetse$unit==1,]
  results_f <- sqldf('select min(birth), first-min(birth), sec-first from Tsetse where total>3')
  results_m <- sqldf("select min(birth) from Tsetse where gender == 'MALE'")
  
  out[['Reproduction']] <- ggplot(data = Tsetse) + geom_histogram(aes(x = birth, fill = gender), binwidth = 1) + 
    geom_histogram(aes(x = first), data = subset(Tsetse, first != 0), binwidth = 1) + 
    geom_histogram(aes(x = sec), data = subset(Tsetse, sec != 0), binwidth = 1) + 
    geom_histogram(aes(x = third), data = subset(Tsetse, third != 0), binwidth = 1) +
    annotate("text", x = 75, y = 950, label = paste("Male puparial period: ", 
                                                    round(results_m[1], 2) )) + 
    annotate("text", x = 75, y = 900, label = paste("Female puparial period: ", 
                                                    round(results_f[1], 2) )) + 
    annotate("text", x = 75, y = 850, label = paste("First larva: ", 
                                                    round(results_f[2], 2) ) )+ 
    annotate("text", x = 75, y = 800, label = paste("Inter-larval period: ", 
                                                    round(results_f[3], 2) ) ) + 
    xlim(0,100) +
    theme(legend.position = "bottom") +
    labs(x = 'Time')

  out[['larva_total']] <- ggplot(data = subset(Tsetse, gender == 'FEMALE'), aes(x = total)) + geom_histogram(binwidth = 1) + 
    xlim(0,30) + 
    labs(x = 'Number of pupa')
  
  out[['Fly_survival']] <- autoplot(survfit(Surv(duration) ~ gender, Tsetse))$plot
  
  out[['Between_age_death']] <- ggplot(data = Tsetse) + geom_histogram(aes(x = duration-floor(duration)), binwidth = 0.1)
  
  WD <- as.data.frame(new_RTable("Pupa_Tester", db_channel))
  names(WD)[2] <- c('ActorID')
  WD$metrics <- factor(WD$metrics, labels = c('duration', 'unit'))
  
  Pupa <- dcast(data = WD, formula = ActorID + gender ~ metrics, value.var = 'Value')
  Pupa <- Pupa[Pupa$unit==1,]
  
  #out[['Pupa_survival']] <- autoplot(survfit(Surv(duration) ~ gender, Pupa))$plot
  
  return(out)
}




