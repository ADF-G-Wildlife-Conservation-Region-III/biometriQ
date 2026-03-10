#' queue_delete
#'
#' @param name The project to be permanently deleted from the queue
#' @param queue_dir The directory containing the queue.rds file
#' @param user The name of the user making the deletion
#'
#' @returns NULL
#' @export
#'
#' @examples queue_delete(name = 'test')
queue_delete <- function(name, user = Sys.getenv('USERNAME'), queue_dir = 'S:/biometric/'){
  queue <- queue_load(queue_dir)

  # check that project exists and is unique
  delete_row <- which(queue$name == name)
  if(length(delete_row) < 1) stop('Project does not exist in queue.')
  if(length(delete_row) > 1) stop('Duplicate projects exist in queue with the same name.')

  # make the user confirm
  confirm <- readline(prompt = paste0('Permanently delete the project \"', name,
                                      '\" from the queue? Type \"yes\" to confirm: '))
  if(confirm != 'yes'){
    cat('Deletion aborted.')
    return()
  }

  log_filepath <- queue$log_filepath[delete_row]
  log_file <- file(log_filepath, open = 'a+')
  writeLines(paste0(Sys.Date(), ': ', user, ' permanently deleted project ', name, '.'),
               log_file)
  close(log_file)
  queue <- queue[-delete_row,]
  queue_write(queue, queue_dir)
}
