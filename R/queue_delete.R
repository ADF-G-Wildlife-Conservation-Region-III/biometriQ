#' queue_delete
#'
#' @param name The project to be permanently deleted from the queue
#' @param user
#'
#' @returns NULL
#' @export
#'
#' @examples queue_delete('test')
queue_delete <- function(name, user = Sys.getenv('USERNAME')){
  queue <- queue_load()

  # check that project exists
  if(!(name %in% queue$name)) stop('Project does not exist in queue.')
  delete_row <- which(queue$name == name)

  # make the user confirm
  confirm <- readline(prompt = paste0('Permanently delete the project \"', name,
                                      '\" from the queue? Type \"yes\" to confirm: '))
  if(confirm != 'yes'){
    cat('Deletion aborted.')
    return()
  } else{
    queue <- queue[-delete_row,]
    write.csv(queue, 'queue.csv', row.names = FALSE)
  }
}
