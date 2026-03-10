#' queue_add
#'
#' @param name The name of the project to be updated in the queue
#' @param description A brief description of the project (optional)
#' @param status The current status of the project, e.g. 'waiting', 'active', or 'finished'
#' @param status_details Additional details regarding the status (optional)
#' @param PI The principal investigator for the project (optional)
#' @param biometric_lead The lead biometrician on the project
#' @param date_due The date by which the project is to be completed (optional)
#' @param resources The directory(s) containing resources such as code, data, and papers pertaining to the project
#' @param date_added The date the project was added to the biometric queue
#' @param user The user making changes to the queue
#' @param queue_dir The directory containing the queue.rds file
#'
#' @returns NULL
#' @export
#'
#' @examples queue_add('test',
#'           description = 'A description goes here',
#'           date_due = as.Date('1992-09-26'))
#'
queue_add <- function(name,
                      description = NULL, # optional fields
                      status = NULL,
                      status_details = NULL,
                      PI = NULL,
                      biometric_lead = Sys.getenv('USERNAME'),
                      date_due = NULL,
                      resources = NULL,
                      date_added = Sys.Date(), # required fields
                      user = Sys.getenv('USERNAME'),
                      queue_dir = 'S:/biometric/'){
  queue <- queue_load(queue_dir)

  # check that project name is not used already
  if(name %in% queue$name) stop('Project name already exists in queue.')

  # create the log file for the new project
  log_filepath <- paste0(queue_dir, "project_logs/", name, ".txt")
  log_file <- file(log_filepath, open = 'wt')
  writeLines(paste0(Sys.Date(), ': ', user, ' added project ', name, ' to queue.'),
             log_file)

  # unless otherwise specified, the user adding the project is assumed to be the biometric lead
  if(is.null(biometric_lead)){biometric_lead <- user}

  # initialize new row in queue
  new_entry <- data.frame(matrix(NA, nrow = 1, ncol = ncol(queue)))
  names(new_entry) <- colnames(queue)

  ## update required fields ##
  new_entry$name <- as.character(name)
  writeLines(paste0('--- biometric_lead updated: NA changed to ', biometric_lead),
             log_file)
  new_entry$date_added <- date_added
  new_entry$log_filepath <- log_filepath
  new_entry$date_updated <- Sys.Date()
  if(date_added != Sys.Date()){
    writeLines(paste0('--- date_added updated: NA changed to ', date_added),
               log_file)
  }
  close(log_file)
  queue <- rbind(queue, new_entry)
  queue_write(queue, queue_dir)

  ## update optional fields
  queue_update(name = name,
               description = description, # optional fields
               status = status,
               status_details = status_details,
               PI = PI,
               biometric_lead = biometric_lead,
               date_due = date_due,
               resources = resources,
               user = user,
               queue_dir = queue_dir)
}
