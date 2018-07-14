new_priority_queue <- function(config){
  targets <- c(ls(config$graph$targets), ls(config$graph$imports))
  ndeps <- lightly_parallelize(
    X = targets,
    FUN = function(target){
      length(upstream_jobs(target = target, graph = config$graph))
    },
    jobs = config$jobs_imports
  ) %>%
    unlist
  priority <- rep(Inf, length(targets))
  if ("priority" %in% colnames(config$plan)){
    priority <- config$plan$priority[match(targets, config$plan$target)]
    priority[is.na(priority)] <- Inf
  }
  data <- data.frame(
    targets = targets,
    ndeps = ndeps,
    priority = priority,
    stringsAsFactors = FALSE
  )
  R6_priority_queue$new(data)
}

# This is not actually a serious O(log n) priority queue
# based on a binary heap. It is a naive placeholder.
# The real priority queue will be
# https://github.com/dirmeier/datastructures
# once the CRAN version has decrease-key
# (https://github.com/dirmeier/datastructures/issues/4).
R6_priority_queue <- R6::R6Class(
  classname = "R6_priority_queue",
  public = list(
    data = NULL,
    initialize = function(data){
      self$data <- data
      self$sort()
    },
    size = function(){
      nrow(self$data)
    },
    empty = function(){
      self$size() < 1
    },
    list = function(){
      self$data$target
    },
    sort = function(){
      ndeps <- priority <- NULL
      self$data <- dplyr::arrange(self$data, ndeps, priority)
    },
    # Peek at the head node of the queue
    # if and only if its ndeps is 0.
    peek0 = function(){
      if (!self$empty() && self$data$ndeps[1] < 1){
        self$data$target[1]
      }
    },
    # Extract the head node of the queue
    # if and only if its ndeps is 0.
    pop0 = function(){
      if (!self$empty() && self$data$ndeps[1] < 1){
        out <- self$data$target[1]
        self$data <- self$data[-1, ]
        out
      }
    },
    # Get all the ready targets
    list0 = function(){
      if (!self$empty() && self$data$ndeps[1] < 1){
        self$data$target[self$data$ndeps < 1]
      }
    },
    remove = function(targets){
      self$data <- self$data[!(self$data$target %in% targets), ]
      invisible()
    },
    # This is all wrong and inefficient.
    # Needs the actual decrease-key algorithm
    decrease_key = function(targets){
      index <- self$data$target %in% targets
      self$data$ndeps[index] <- self$data$ndeps[index] - 1
      self$sort()
    }
  )
)
