#' @title Create the `igraph` dependency network of your project.
#' @description This function returns an igraph object representing how
#' the targets in your workflow plan data frame
#' depend on each other.
#' (`help(package = "igraph")`). To plot this graph, call
#' to [plot.igraph()] on your graph. See the online manual
#' for enhanced graph visualization functionality.
#' @export
#' @return An igraph object representing
#'   the workflow plan dependency network.
#' @inheritParams drake_config
#' @param sanitize_plan logical, deprecated. If you must,
#'   call `drake:::sanitize_plan()` to sanitize the plan
#'   and/or `drake:::sanitize_targets()` to sanitize the targets
#'   (or just get `plan` and `targets` and `graph` from
#'   [drake_config()]).
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' # Make the igraph network connecting all the targets and imports.
#' g <- build_drake_graph(my_plan)
#' class(g) # "igraph"
#' })
#' }
build_drake_graph <- function(
  plan = read_drake_plan(),
  targets = plan$target,
  envir = parent.frame(),
  verbose = drake::default_verbose(),
  jobs = 1,
  sanitize_plan = FALSE,
  console_log_file = NULL
){
  force(envir)
  if (sanitize_plan){
    warning(
      "The `sanitize_plan` argument to `build_drake_graph()` is deprecated.",
      call. = FALSE
    )
  }
  imports <- as.list(envir)
  unload_conflicts(
    imports = names(imports),
    targets = plan$target,
    envir = envir,
    verbose = verbose
  )
  import_names <- setdiff(names(imports), targets)
  imports <- imports[import_names]
  config <- list(verbose = verbose, console_log_file = console_log_file)
  console_many_targets(
    targets = names(imports),
    pattern = "connect",
    type = "import",
    config = config
  )
  import_deps <- lightly_parallelize(
    X = names(imports),
    FUN = function(name){
      import_dependencies(imports[[name]])
    },
    jobs = jobs
  ) %>%
    setNames(names(imports))
  console_many_targets(
    targets = plan$target,
    pattern = "connect",
    type = "target",
    config = config
  )
  command_deps <- lightly_parallelize(
    X = seq_len(nrow(plan)),
    FUN = function(i){
      command_dependencies(plan$command[i])
    },
    jobs = jobs
  ) %>%
    setNames(plan$target)
  exclude <- c(names(import_deps), plan$target)
  import_leaves <- setdiff(find_leaves(import_deps, jobs), exclude)
  target_leaves <- setdiff(find_leaves(command_deps, jobs), exclude)
  leaves <- union(import_leaves, target_leaves)
  for (leaf in leaves){
    import_deps[[leaf]] <- list()
  }
  list(
    imports = list2env(import_deps, parent = emptyenv(), hash = TRUE),
    targets = list2env(command_deps, parent = emptyenv(), hash = TRUE)
  ) %>%
    prune_drake_graph(jobs = jobs)
}

find_leaves <- function(dep_list, jobs){
  lightly_parallelize(
    dep_list,
    function(x){
      unlist(x[c("globals", "loadd", "readd")])
    },
    jobs = jobs
  ) %>%
    clean_dependency_list 
}

unload_conflicts <- function(imports, targets, envir, verbose){
  common <- intersect(imports, targets)
  if (verbose & length(common)){
    message(
      "Unloading targets from environment:\n",
      multiline_message(common), sep = ""
    )
  }
  remove(list = common, envir = envir)
}

#' @title Prune the dependency network of your project.
#' @export
#' @seealso [build_drake_graph()], [drake_config()],
#'   [make()]
#' @description `igraph` objects are used
#' internally to represent the dependency network of your workflow.
#' See `drake_config(my_plan)$graph` from the mtcars example.
#' @details For a supplied graph, take the subgraph of all combined
#' incoming paths to the vertices in `to`. In other words,
#' remove the vertices after `to` from the graph.
#' @return A pruned igraph object representing the dependency network
#'   of the workflow.
#' @param graph An igraph object to be pruned.
#' @param to Character vector, names of the vertices that draw
#'   the line for pruning. The pruning process removes all vertices
#'   downstream of `to`.
#' @param jobs Number of jobs for light parallelism (on non-Windows machines).
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' # Build the igraph object representing the workflow dependency network.
#' # You could also use drake_config(my_plan)$graph
#' graph <- build_drake_graph(my_plan)
#' # The default plotting is not the greatest,
#' # but you will get the idea.
#' # plot(graph) # nolint
#' # Prune the graph: that is, remove the nodes downstream
#' # from 'small' and 'large'
#' pruned <- prune_drake_graph(graph = graph, to = c("small", "large"))
#' # plot(pruned) # nolint
#' })
#' }
prune_drake_graph <- function(
  graph, to = ls(graph$targets), jobs = 1
){
  stage <- keep_these <- to
  while (length(stage)){
    stage <- lightly_parallelize(
      X = stage,
      FUN = upstream_jobs,
      graph = graph,
      jobs = jobs
    ) %>%
      unlist
    keep_these <- c(keep_these, stage)
  }
  remove_imports <- setdiff(ls(graph$imports), keep_these)
  remove_targets <- setdiff(ls(graph$targets), keep_these)
  if (length(remove_imports)){
    remove(list = remove_imports, envir = graph$imports)
  }
  if (length(remove_targets)){
    remove(list = remove_targets, envir = graph$targets)
  }
  graph
}

get_neighborhood <- function(graph, from, mode, order){
  if (!length(order)){
    order <- length(V(graph))
  }
  if (length(from)){
    from <- sanitize_nodes(nodes = from, choices = V(graph)$name)
    graph <- igraph::make_ego_graph(
      graph = graph,
      order = order,
      nodes = from,
      mode = mode
    ) %>%
      do.call(what = igraph::union)
  }
  graph
}

downstream_nodes <- function(from, graph, jobs){
  if (!length(from)){
    return(character(0))
  }
  lightly_parallelize(
    X = from,
    FUN = function(node){
      subcomponent(graph, v = node, mode = "out")$name
    },
    jobs = jobs
  ) %>%
    unlist() %>%
    unique() %>%
    sort()
}

leaf_nodes <- function(graph){
  is_leaf <- igraph::degree(graph, mode = "in") == 0
  V(graph)[is_leaf]$name
}

filter_upstream <- function(targets, graph){
  delete_these <- setdiff(V(graph)$name, targets)
  graph <- delete_vertices(graph = graph, v = delete_these)
  leaf_nodes(graph)
}

subset_graph <- function(graph, subset){
  if (!length(subset)){
    return(graph)
  }
  subset <- intersect(subset, V(graph)$name)
  igraph::induced_subgraph(graph = graph, vids = subset)
}

imports_graph <- function(config){
  delete_these <- intersect(config$plan$target, V(config$graph)$name)
  delete_vertices(config$graph, v = delete_these)
}

targets_graph <- function(config){
  delete_these <- setdiff(V(config$graph)$name, config$plan$target)
  delete_vertices(config$graph, v = delete_these)
}

connect_output_files <- function(commands_edges, output_files){
  if (!length(output_files)){
    return(commands_edges)
  }
  output_files <- utils::stack(output_files)
  output_files$ind <- as.character(output_files$ind)
  index <- match(commands_edges$from, table = output_files$values)
  commands_edges$from[is.finite(index)] <- output_files$ind[na.omit(index)]
  commands_edges
}
