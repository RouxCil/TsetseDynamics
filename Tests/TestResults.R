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

Tsetse_cohort_Tester <- function(channel)
{
  require(plyr)
  require(sqldf)
  require(xtable)
  require(survival)
  require(survMisc)
  require(reshape2)
  out <- list()
  
  WD <- as.data.frame(new_RTable("Tsetse_Tester", channel))
  names(WD)[2] <- c('ActorID')
  WD$metrics <- factor(WD$metrics, labels = c('duration', 'unit', 'birth', 'first', 'sec', 'third', 'total'))
  
  Tsetse <- dcast(data = WD, formula = ActorID + gender ~ metrics, value.var = 'Value')
  Tsetse <- Tsetse[Tsetse$unit==1,]
  results_f <- sqldf('select min(birth), first-min(birth), sec-first from Tsetse where total>3')
  results_m <- sqldf("select min(birth) from Tsetse where gender == 'MALE'")
  
  mortality_AgePar <- as.data.frame(new_PTable("mortality_AgePar", channel))
  mortality_const <- as.data.frame(new_PTable("mortality_const", channel))
  mortality_TempPar <- as.data.frame(new_PTable("mortality_TempPar", channel))
  alpha <- as.data.frame(new_PTable("alpha", channel))
  Temp <- min(as.data.frame(new_PTable("DailyAvgTemperature", channel))[,2])
  
  m_mortality <- function(x)
  {
    surv <- exp(mortality_const[1,2]*( exp(-mortality_TempPar[1,2]*exp(alpha$Value*35)*x) - exp(mortality_AgePar[1,2]*x)))
    return(surv)
  }
  
  f_mortality <- function(x)
  {
    surv <- exp(mortality_const[2,2]*(exp(-mortality_TempPar[2,2]*exp(alpha$Value*35)*x) - exp(mortality_AgePar[2,2]*x)))
    return(surv)
  }

  
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
  
  out[['Random_start']] <- ggplot(data = Tsetse) + geom_histogram(aes(x = birth, fill = gender), binwidth = 1) + 
    xlim(0,50) +
    theme(legend.position = "bottom") +
    labs(x = 'Time')

  out[['larva_total']] <- ggplot(data = subset(Tsetse, gender == 'FEMALE'), aes(x = total)) + geom_histogram(binwidth = 1) + 
    xlim(0,30) + 
    labs(x = 'Number of pupa')
  
  out[['Fly_survival']] <- autoplot(survfit(Surv(duration) ~ gender, Tsetse))$plot + 
    stat_function(col = 'black', fun = function(x) exp(mortality_const[1,2]*(exp(-mortality_TempPar[1,2]*exp(alpha$Value*20)*x) - exp(mortality_AgePar[1,2]*x)))) + 
    stat_function(col = 'black', fun = function(x) exp(mortality_const[2,2]*(exp(-mortality_TempPar[2,2]*exp(alpha$Value*20)*x) - exp(mortality_AgePar[2,2]*x)))) 
   
   
  
  out[['Between_age_death']] <- autoplot(survfit(Surv(duration-floor(duration)) ~ gender, Tsetse), title = '', legLabs = c('F','M'), legTitle = 'Gender', xlab = 'Age')$plot
  
  WD <- as.data.frame(new_RTable("Pupa_Tester", channel))
  names(WD)[2] <- c('ActorID')
  WD$metrics <- factor(WD$metrics, labels = c('duration', 'unit'))
  
  Pupa <- dcast(data = WD, formula = ActorID + gender ~ metrics, value.var = 'Value')
  Pupa <- Pupa[Pupa$unit==1,]
  Pupa$status <- -1
  Pupa$status[Pupa$gender == 'MALE'] <- ifelse(round(Pupa$duration[Pupa$gender == 'MALE'], 4) == c(round(results_m[1],4)), 0, 1)
  Pupa$status[Pupa$gender == 'FEMALE'] <- ifelse(round(Pupa$duration[Pupa$gender == 'FEMALE'], 4) == c(round(results_f[1],4)), 0, 1)
  
  out[['Pupa_survival']] <- autoplot(survfit(Surv(duration, status) ~ gender, Pupa), title = '', legLabs = c('F','M'), legTitle = 'Gender')$plot + xlim(0,50) + ylim(0,1) + labs(x = 'Time', y = 'Survival')
  WD <- as.data.frame(new_RTable("Total", channel))
  plot_data <- subset(WD, Value != 0)
  
  out[['Population']] <- ggplot(plot_data, aes(x = report_time, y = Value, col = gender)) + geom_point()
  
#   m1 <- as.data.frame(new_RTable("Mark1", db_channel))
#   m2 <- as.data.frame(new_RTable("Mark2", db_channel))
#   m3 <- as.data.frame(new_RTable("Mark3", db_channel))
#   m4 <- as.data.frame(new_RTable("Mark4", db_channel))
#   m5 <- as.data.frame(new_RTable("Mark5", db_channel))
#   m6 <- as.data.frame(new_RTable("Mark6", db_channel))
#   m7 <- as.data.frame(new_RTable("Mark7", db_channel))
#   m8 <- as.data.frame(new_RTable("Mark8", db_channel))
#   m9 <- as.data.frame(new_RTable("Mark9", db_channel))
#   m10 <- as.data.frame(new_RTable("Mark10", db_channel))
#   All_marks <- rbind(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10)
#   
#   names(All_marks)[3] <- c('Week')
#   All_marks <- All_marks[All_marks$Value != 0,]
#   All_marks$Week <- as.numeric(substr(All_marks$Week, 6,8))
#   All_marks$mark <- factor(All_marks$mark, levels = c('week0','week1','week2','week3','week4','week5','week6','week7','week8','week9','week10','week11',
#                                                       'week12','week13','week14','week15','week16','week17','week18','week19','week20','week21','week22',
#                                                       'week23','week24','week25','week26','week27','week28','week29','week30','week31','week32','week33',
#                                                       'week34','week35','week36'), ordered = T)
#   
#   Total_marks <- dcast(data = All_marks, formula = Week + gender ~ mark, fun.aggregate = sum, value.var = 'Value')
#   Total_marks_f <- subset(Total_marks, gender == 'FEMALE')
#   Total_marks_m <- subset(Total_marks, gender == 'MALE')
#   
  
  return(out)
}




