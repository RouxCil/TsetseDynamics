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
             scenario_name_sub = list(from = 'ConstantT', to = 'Step_20_r'), 
             scex_sub = list(), 
             dat_sub = list('1' = list(from = '28',
                                       to = '20'),
                            '2' = list(from = 'Switch_on_random_start = FALSE;',
                                       to = 'Switch_on_random_start = TRUE;') ) ),
  '2' = list(test_folder = td, 
             base_scenario = 'ConstantT.scex', 
             base_scenario_dat = 'ConstantT(TsetseFlyCore).dat',
             scenario_name_sub = list(from = 'ConstantT', to = 'Step_20'), 
             scex_sub = list(), 
             dat_sub = list('1' = list(from = '28',
                                       to = '20')) ),
  '3' = list(test_folder = td, 
             base_scenario = 'ConstantT.scex', 
             base_scenario_dat = 'ConstantT(TsetseFlyCore).dat',
             scenario_name_sub = list(from = 'ConstantT', to = 'Step_22'), 
             scex_sub = list(), 
             dat_sub = list('1' = list(from = '28',
                                       to = '22') ) ),
  '4' = list(test_folder = td, 
             base_scenario = 'ConstantT.scex', 
             base_scenario_dat = 'ConstantT(TsetseFlyCore).dat',
             scenario_name_sub = list(from = 'ConstantT', to = 'Step_24'), 
             scex_sub = list(), 
             dat_sub = list('1' = list(from = '28',
                                       to = '24') ) ),
  '5' = list(test_folder = td, 
             base_scenario = 'ConstantT.scex', 
             base_scenario_dat = 'ConstantT(TsetseFlyCore).dat',
             scenario_name_sub = list(from = 'ConstantT', to = 'Step_26'), 
             scex_sub = list(), 
             dat_sub = list('1' = list(from = '28',
                                       to = '26') ) ),
  '6' = list(test_folder = td, 
             base_scenario = 'ConstantT.scex', 
             base_scenario_dat = 'ConstantT(TsetseFlyCore).dat',
             scenario_name_sub = list(from = 'ConstantT', to = 'Step_28'), 
             scex_sub = list(), 
             dat_sub = list('1' = list(from = '28',
                                       to = '28') ) ),
  '7' = list(test_folder = td, 
             base_scenario = 'ConstantT.scex', 
             base_scenario_dat = 'ConstantT(TsetseFlyCore).dat',
             scenario_name_sub = list(from = 'ConstantT', to = 'Step_30'), 
             scex_sub = list(), 
             dat_sub = list('1' = list(from = '28',
                                       to = '30') ) ),
  '8' = list(test_folder = td, 
             base_scenario = 'ConstantT.scex', 
             base_scenario_dat = 'ConstantT(TsetseFlyCore).dat',
             scenario_name_sub = list(from = 'ConstantT', to = 'Step_32'), 
             scex_sub = list(), 
             dat_sub = list('1' = list(from = '28',
                                       to = '32') ) ),
  '9' = list(test_folder = td, 
             base_scenario = 'ConstantT.scex', 
             base_scenario_dat = 'ConstantT(TsetseFlyCore).dat',
             scenario_name_sub = list(from = 'ConstantT', to = 'Step_32_r'), 
             scex_sub = list(), 
             dat_sub = list('1' = list(from = '28',
                                       to = '32'),
                            '2' = list(from = 'Switch_on_random_start = FALSE;',
                                       to = 'Switch_on_random_start = TRUE;') ) )
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




