#' queue_update
#'
#' @param name The name of the project to be updated in the queue
#' @param description A brief description of the project (optional)
#' @param status The current status of the project, e.g. 'waiting', 'active', or 'finished'
#' @param status_details Additional details regarding the status (optional)
#' @param PI The principal investigator for the project (optional)
#' @param biometric_lead The lead biometrician on the project
#' @param date_due The date by which the project is to be completed (optional)
#' @param resources The directory(s) containing resources such as code, data, and papers pertaining to the project
#' @param user The user making changes to the queue
#' @param queue_dir The directory containing the queue.rds file
#'
#' @returns NULL
#' @export
#'
#' @examples queue_update('test',
#'           description = 'An updated description',
#'           PI = 'Harry Potter')
#'
queue_update <- function(name,
                         description = NULL,
                         status = NULL,
                         status_details = NULL,
                         PI = NULL,
                         biometric_lead = NULL,
                         date_due = NULL,
                         resources = NULL,
                         user = Sys.getenv('USERNAME'),
                         queue_dir = 'S:/biometric/'){
  queue <- queue_load(queue_dir)

  # check that project exists and is unique
  entry_row <- which(queue$name == name)
  if(length(entry_row) < 1) stop('Project does not exist in queue.')
  if(length(entry_row) > 1) stop('Duplicate projects exist in queue with the same name.')
  old_entry <- queue[entry_row,]
  new_entry <- old_entry

  # create log entry
  log_filepath <- old_entry$log_filepath[1]
  log_file <- file(log_filepath, open = 'a+')
  writeLines(paste0(Sys.Date(), ': ', user, ' updated project ', name, '.'),
             log_file)

  # update description
  if(!is.null(description)){
    new_entry$description <- description
    writeLines(paste0('--- description updated: \"', old_entry$description, '\" -- changed to -- \"',
                      new_entry$description, '\"'), log_file)
  }

  # update status
  if(!is.null(status)){
    if(!status %in% c('active', 'waiting', 'finished'))
      {warning('Status should be one of \"active\" \"waiting\" or \"finished\"')}
    new_entry$status <- status
    writeLines(paste0('--- status updated: \"', old_entry$status, '\" -- changed to -- \"',
                      new_entry$status,'\"'), log_file)
  }

  # update status_details
  if(!is.null(status_details)){
    new_entry$status_details <- status_details
    writeLines(paste0('--- status_details updated: \"', old_entry$status_details, '\" -- changed to -- \"',
                      new_entry$status_details,'\"'), log_file)
  }

  # update PI
  if(!is.null(PI)){
    new_entry$PI <- PI
    writeLines(paste0('--- PI updated: \"', old_entry$PI, '\" -- changed to -- \"',
                      new_entry$PI,'\"'), log_file)
  }

  # update biometric_lead
  if(!is.null(biometric_lead)){
    new_entry$biometric_lead <- biometric_lead
    writeLines(paste0('--- biometric_lead updated: \"', old_entry$biometric_lead, '\" -- changed to -- \"',
                      new_entry$biometric_lead,'\"'), log_file)
  }

  # update date_due
  if(!is.null(date_due)){
    new_entry$date_due <- date_due
    writeLines(paste0('--- date_due updated: \"', old_entry$date_due, '\" -- changed to -- \"',
                      new_entry$date_due,'\"'), log_file)
  }

  # update resources
  if(!is.null(resources)){
    new_entry$resources <- resources
    writeLines(paste0('--- resources updated: \"', old_entry$resources, '\" -- changed to -- \"',
                      new_entry$resources,'\"'), log_file)
  }

  queue[entry_row,] <- new_entry

  queue_write(queue, queue_dir)
  close(log_file)
}
