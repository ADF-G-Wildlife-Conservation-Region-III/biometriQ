#' queue_update
#'
#' @param name The name of the project to be updated in the queue
#' @param ... Any fields to be updated
#'
#' @returns NULL
#' @export
#'
#' @examples queue_update('test',
#'           description = 'An updated description',
#'           PI = 'Harry Potter',
#'           something_else = "This still doesn't work!")
#'
queue_update <- function(name, user = Sys.getenv('USERNAME'), ...){
  queue <- queue_load()

  # check that project exists and pull the row from queue
  if(!(name %in% queue$name)) stop('Project does not exist in queue.')
  old_entry <- queue[which(queue$name == name),]
  new_entry <- old_entry

  # create log entry
  log_filepath <- old_entry$log_filepath[1]
  log_file <- file(log_filepath, open = 'a+')
  writeLines(paste0(Sys.Date(), ': ', user, ' updated project ', name, '.'),
             log_file)

  # update fields
  dots <- list(...)
  for (field in names(dots)) {
    if(field %in% colnames(new_entry)){
      new_entry[,field] <- dots[[field]]
      writeLines(paste0('--- ', field, ' updated: \"', old_entry[,field], '\" -- changed to -- \"', new_entry[,field],'\"'),
                 log_file)
    } else{warning(paste0(field, ' is not a recognized field.'))}
  }

  queue[which(queue$name == name),] <- new_entry

  write.csv(queue, 'queue.csv', row.names = FALSE)
  close(log_file)
}
