#' queue_load
#'
#' @returns The current biometric queue as a data frame
#' @export
#'
#' @examples queue <- queue_load()
queue_load <- function(){
  # read in queue, referencing metadata for correct column classes
  queue_classes <- as.character(read.csv('queue_classes.csv', header = TRUE))
  queue <- read.csv('queue.csv',
                    header = TRUE,
                    colClasses = queue_classes)

  return(queue)
}
