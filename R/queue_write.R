#' Title
#'
#' @param queue_dir The directory containing the queue.rds file
#' @param queue The dataframe containing the updated queue
#'
#' @returns NULL
#' @export
#'
#' @examples queue <- queue_load()
#'           queue_write(queue)
queue_write <- function(queue, queue_dir = 'S:/biometric/'){
  require(utils)
  # to do: add some internal checks to make sure the file being written is similar to the old file
  queue_filepath <- paste0(queue_dir, 'queue.rds')
  csv_filepath <- paste0(queue_dir, 'queue.csv')

  utils::write.csv(queue, file = csv_filepath)
  saveRDS(queue, file = queue_filepath)
}
