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
  library(scales)
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
  Temp <- min(as.data.frame(new_PTable("DailyAvgTemperature", channel))[,2])
  
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
    xlim(0,55) +
    theme(legend.position = "bottom") +
    labs(x = 'Time')

  out[['larva_total']] <- ggplot(data = subset(Tsetse, gender == 'FEMALE' & total > 1), aes(x = total)) + geom_histogram(binwidth = 1) + 
    xlim(0,30) + 
    ylim(0,50) +
    labs(x = 'Number of pupa')
  
  out[['Fly_survival']] <- autoplot(survfit(Surv(duration) ~ gender, Tsetse), title = '', legLabs = c('F','M'), legTitle = 'Gender', xlab = 'Age')$plot + 
    xlim(0,300) + ylim(0,1) + labs(x = 'Time', y = 'Survival')
  
  WD <- as.data.frame(new_RTable("Pupa_Tester", channel))
  names(WD)[2] <- c('ActorID')
  WD$metrics <- factor(WD$metrics, labels = c('duration', 'unit'))
  
  Pupa <- dcast(data = WD, formula = ActorID + gender ~ metrics, value.var = 'Value')
  Pupa <- Pupa[Pupa$unit==1,]
  Pupa$status <- -1
  Pupa$status[Pupa$gender == 'MALE'] <- ifelse(round(Pupa$duration[Pupa$gender == 'MALE'], 4) == c(round(results_m[1],4)), 0, 1)
  Pupa$status[Pupa$gender == 'FEMALE'] <- ifelse(round(Pupa$duration[Pupa$gender == 'FEMALE'], 4) == c(round(results_f[1],4)), 0, 1)
  
  out[['Pupa_survival']] <- autoplot(survfit(Surv(duration, status) ~ gender, Pupa), title = '', legLabs = c('F','M'), legTitle = 'Gender')$plot + 
    xlim(0,50) + ylim(0,1) + labs(x = 'Time', y = 'Survival')
  
  return(out)
}  
  
Sim_Results <- function(channel)
{  
  out <- list()
  
  WD <- as.data.frame(new_RTable("Fly_Total", channel))
  plot_data <- subset(WD, Value != 0 & report_time != 0)
  
  out[['Population']] <- ggplot(plot_data, aes(x = report_time, y = Value, col = gender)) + geom_point()
  
  All_marks <- NULL
  for (i in 1:10)
  {
    WD <- as.data.frame(new_RTable(paste("Mark",i, sep = ''), channel))
    WD$nrmark <- names(WD)[2]
    names(WD)[2] <- 'mark'
    All_marks <- rbind(All_marks, WD)
  }
  
  All_marks$mark <- as.numeric(gsub('week', '', All_marks$mark))
  All_marks$Time <- gsub('min', '400', All_marks$Time)
  
  test <- sqldf("select * from All_marks where metrics like '%event%' and Value <> 0")
  
  plot_data <- ddply(.data = test, .variables = .(mark, gender), .fun = summarise, sum = sum(Value))
  out[['Flies_per_week']] <- ggplot(plot_data, aes(x = mark, y = sum, col = gender)) + geom_point() 
  
  test$group <- ifelse(test$nrmark == 'mark1', 'Unmarked', 'Marked')
  plot_data <- ddply(.data = test, .variables = .(mark, gender, group), .fun = summarise, sum = sum(Value))
  out[['Fly_per_marked']] <- ggplot(plot_data, aes(x = mark, y = sum, col = gender)) + geom_point() + facet_grid(group~.)
 
  test$nrmark <- as.numeric(gsub('mark', '', test$nrmark))
  plot_data <- ddply(.data = subset(test, group == 'Marked'), .variables = .(mark, group, gender), .fun = summarise, mean = sum(Value*(nrmark-1))/sum(Value))
  out[['released_marks']] <- ggplot(plot_data, aes(x = mark, y = mean, col = gender)) + geom_point()
  
#   Working_Data <- dcast(data = subset(All_marks, gender == 'MALE'), formula = Time ~ mark, fun.aggregate = sum, value.var = 'Value')
#   diag(Working_Data[,-1]) <- 0
#   Working_Data2 <- melt(Working_Data)
#   Working_Data2 <- Working_Data2[Working_Data2$value!=0,]
#   plot_Data <- ddply(.data = Working_Data2, .variables = .(Time), .fun = mutate, freq = value/sum(value),len = length(variable):1)
#   out[['Male_trend']] <- ggplot(plot_Data[134:249,], aes(x = len, y = freq)) +  
#     geom_point() +
#     ylim(0,1) +
#     geom_smooth(method = loess, se=FALSE) +
#     labs(y = 'Average number of markes for released flies', x = 'Week of experiment')
#   
#   Working_Data <- dcast(data = subset(All_marks, gender == 'FEMALE'), formula = Time ~ mark, fun.aggregate = sum, value.var = 'Value')
#   diag(Working_Data[,-1]) <- 0
#   Working_Data2 <- melt(Working_Data)
#   Working_Data2 <- Working_Data2[Working_Data2$value!=0,]
#   plot_Data <- ddply(.data = Working_Data2, .variables = .(Time), .fun = mutate, freq = value/sum(value),len = length(variable):1)
#   out[['Female_trend']] <- ggplot(plot_Data[288:587,], aes(x = len, y = freq)) +  
#     geom_point() +
#     ylim(0,1) +
#     geom_smooth(method = loess, se=FALSE) +
#     labs(y = 'Average number of markes for released flies', x = 'Week of experiment')
#   
  return(out)
}  
  





