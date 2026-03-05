#' queue_add
#'
#' @param name The name of the project to be added to the queue
#' @param ... Any additional fields to be filled out
#'
#' @returns NULL
#' @export
#'
#' @examples queue_add('test',
#'           description = 'A description goes here',
#'           date_due = as.Date('1992-09-26'),
#'           something_else = "This doesn't work!")
#'
queue_add <- function(name,
                      date_added = Sys.Date(),
                      log_filepath = NULL,
                      biometric_lead = NULL,
                      user = Sys.getenv('USERNAME'), ...){
  queue <- queue_load()

  # check that project name is not used already
  if(name %in% queue$name) stop('Project name already exists in queue.')

  # create the log file for the new project
  if(is.null(log_filepath)){log_filepath <- paste0("project_logs/", name, ".txt")}
  log_file <- file(log_filepath, open = 'wt')
  writeLines(paste0(Sys.Date(), ': ', user, ' added project ', name, ' to queue.'),
             log_file)

  # unless otherwise specified, the user adding the project is assumed to be the biometric lead
  if(is.null(biometric_lead)){biometric_lead <- user}

  # initialize new row in queue
  new_entry <- data.frame(matrix(NA, nrow = 1, ncol = ncol(queue)))
  names(new_entry) <- colnames(queue)

  # update required fields
  new_entry$name <- as.character(name)
  new_entry$biometric_lead <- biometric_lead
  writeLines(paste0('--- biometric_lead', ' = ', biometric_lead),
             log_file) # record updates to log file
  new_entry$date_added <- date_added
  new_entry$log_filepath <- log_filepath
  new_entry$date_updated <- Sys.Date()

  # update optional fields
  dots <- list(...)
  for (field in names(dots)) {
    if(field %in% colnames(new_entry)){
      new_entry[,field] <- dots[[field]]
      writeLines(paste0('--- ', field, ' = ', dots[[field]]),
                 log_file) # record updates to log file
    } else{warning(paste0(field, ' is not a recognized field.'))}
  }
  close(log_file)

  # finally, update the queue
  queue <- queue |>
    rbind(new_entry)
  write.csv(queue, 'queue.csv', row.names = FALSE)
}
