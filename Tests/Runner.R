rm(list = ls())
source('C://Users/Roux-Cil//Desktop//Masters File//Modgen//TsetseDynamics//Tests/test_helpers.R')
library(modgenTester)
odbcCloseAll()
sd <- "C:/Users/Roux-Cil/Desktop/Masters File/Modgen/TsetseDynamics"
td <- "C:/Users/Roux-Cil/Desktop/Masters File/Modgen/TsetseDynamics/Tests"
setwd(td)

copy_model(sd, 'TsetseDynamicsD.exe', td)
copy_scenario(sd, 'Base.scex', 'Base(TsetseFlyCore).dat', td)
copy_scenario(sd, 'ConstantT.scex', 'ConstantT(TsetseFlyCore).dat', td)


scenarios_to_create <- list(
  '1' = list(test_folder = td, 
             base_scenario = 'ConstantT.scex', 
             base_scenario_dat = 'ConstantT(TsetseFlyCore).dat',
             scenario_name_sub = list(from = 'ConstantT', to = 'Extreme_Cold'), 
             scex_sub = list(), 
             dat_sub = list('1' = list(from = 'Switch_on_random_start = TRUE;',
                                       to = 'Switch_on_random_start = FALSE;'),
                            '2' = list(from = '35',
                                      to = '20')  ) ),
  '2' = list(test_folder = td, 
             base_scenario = 'ConstantT.scex', 
             base_scenario_dat = 'ConstantT(TsetseFlyCore).dat',
             scenario_name_sub = list(from = 'ConstantT', to = 'Extreme_Heat'), 
             scex_sub = list(), 
             dat_sub = list('1' = list(from = 'Switch_on_random_start = TRUE;',
                                       to = 'Switch_on_random_start = FALSE;'),
                            '2' = list(from = '35',
                                       to = '32')  ) ),
  '3' = list(test_folder = td, 
             base_scenario = 'ConstantT.scex', 
             base_scenario_dat = 'ConstantT(TsetseFlyCore).dat',
             scenario_name_sub = list(from = 'ConstantT', to = 'Extreme_Cold_with_R'), 
             scex_sub = list(), 
             dat_sub = list('1' = list(from = '35',
                                       to = '20')  ) ),
  '4' = list(test_folder = td, 
             base_scenario = 'ConstantT.scex', 
             base_scenario_dat = 'ConstantT(TsetseFlyCore).dat',
             scenario_name_sub = list(from = 'ConstantT', to = 'Extreme_Heat_with_R'), 
             scex_sub = list(), 
             dat_sub = list('1' = list(from = '35',
                                       to = '32')  ) )
)

for (i in scenarios_to_create){
  do.call(generate_scenario, i)
}
available_scenarios <- list_all_scenarios(td, FALSE)

library(foreach)
library(doSNOW)
ptm <- proc.time()
cl <- makeCluster(6)
clusterCall(cl, function() {
  library(modgenTester)
})
clusterExport(cl, 'td')
registerDoSNOW(cl)

rerun_models <- TRUE

if (rerun_models){
  foreach(scenario = available_scenarios) %dopar% {
    run_scenario(td, scenario$scex,
                 td, "TsetseDynamicsD.exe")
  }
}

stopCluster(cl)
all_results <- list_all_results(td)




