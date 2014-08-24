#' Adds a new test to the test_results list and prints output
#' @param test_results A list containing all the previously run tests
#' @param name The name of the new test
#' @param scenarios A character vector of the scenarios used in this test (optional)
#' @param result The result ot the current test (optional)
#' @param description A wordy description of the test (optional)
#' @export

new_test <- function(test_results, name, scenarios = NULL, result = NULL, description = NULL){
  anchor_link <- gsub(" ", "", name)
  test_results[[name]] <- list(name = name,
                               result = result,
                               scenarios = scenarios, 
                               anchor_link = anchor_link)
  cat(paste0('#### <a name="', anchor_link,'"></a>', name, "\n\n"))
  cat(paste0("Scenario(s): ", scenarios, "\n\n"))
  cat(paste0(description, '\n\n'))
  cat(paste0("[To Summary Table](#summary_tab_link)  \n\n"))
  return(test_results)
}

#' Adds a result to an already existing entry in the list of all tests
#' @param test_results A list containing all the previously run tests
#' @param name The name of the test the result must be attached to
#' @param result The result ot the current test (optional)
#' @export

new_result <- function(test_results, name, result){
  test_results[[name]][['result']] <- result
  cat("  \n\n")
  cat(paste0("**Result: ", result, "**  \n\n"))
  cat('---\n\n')
  return(test_results)
}

#' Prints a table summary of all the tests 
#' @param test_results A list containing all the previously run tests
#' @export
print_test_results <- function(test_results){
  result_vector <- table(sapply(test_results, `[[`, 'result'))
  cat("#### Summary of the Test Results \n\n")
  cat(paste0('|', as.character(paste(names(result_vector), collapse = '|', sep = '|')), '|\n'))
  cat(paste(paste(rep("|:-:", length(result_vector)), sep = "", collapse = ""), "|\n", sep = "", collapse = ""))
  cat(paste0('|', paste(as.character(result_vector), collapse = '|', sep = '|'), '| \n\n'))
  cat(paste0('#### <a name="summary_tab_link"></a>Table of the Test Results\n\n'))
  cat("|Name|Result|Scenarios|\n")
  cat("|:-:|:-:|:-:|\n")
  for (i in test_results){
    cat(paste0("|[", i$name, "](#", i$anchor_link, ")|", i$result, "|", paste(i$scenarios, collapse = ", "), " \n"))
  }
}

#' Returns the number of mothers that were created
#' @param db_channel The connection to the access database to use
#' @export

get_mother_population_size <- function(db_channel){
  DurationOfLife <- as.data.frame(new_RTable("MotherDurationOfLife", db_channel))
  return(subset(DurationOfLife, 
                metrics == "Population size", 
                select = 'Value')[1,1])
}

#' Returns the mother event details table with a time_to_event column added
#' The time to event column is computed based on the start_event and end_event inputs
#' @param db_channel The connection to the access database to use
#' @param start_event Time to event will consider this as the start of the risk period
#' @param end_event Time till the occurance of this event will be returned
#' @export

get_mother_time_to_event <- function(db_channel,
                                     start_event = 'birth',
                                     end_event = 'Death'){
  med <- as.data.frame(new_RTable("MotherEventDetails", db_channel))[,1:3]
  med <- dcast(med, `split( actor_id, ACTOR_ID )` ~ metrics)
  if (start_event == 'birth'){
    start_times <- rep(0, nrow(med))
  } else {
    col_name <- paste0("Age at ", start_event)
    stopifnot(col_name %in% names(med))
    start_times <- med[,col_name]
  }
  col_name <- paste0("Age at ", end_event)
  stopifnot(col_name %in% names(med))
  med$time_to_event <- med[,col_name] - start_times
  return(med)
}

