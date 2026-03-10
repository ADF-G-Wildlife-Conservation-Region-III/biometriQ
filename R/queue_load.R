#' queue_load
#'
#' @param queue_dir The directory containing the queue.rds file
#'
#' @returns The current biometric queue as a data frame
#' @export
#'
#' @examples queue <- queue_load()
queue_load <- function(queue_dir = 'S:/biometric/'){
  queue_filepath = paste0(queue_dir, 'queue.rds')
  return(readRDS(queue_filepath))
}
