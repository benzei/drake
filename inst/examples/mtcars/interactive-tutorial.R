# This script is meant to stand on its own, depending only on
# the included report.Rmd file.
# It is meant to walk you through the example step by step.
# The other files show you how to set up the example
# as a serious drake project.
# To run the project as a serious workflow, just run make.R.
# Then, read the output report.md file.
#
###############
### PURPOSE ###
###############
#
# The purpose of this example is to walk through
# drake's main functionality using a simple example
# data analysis workflow.
#
############
# QUESTION #
############
#
# Is there an association between the weight and the fuel efficiency of cars?
#
####################
# GENERAL APPROACH #
####################
#
# To answer the question, we use the `mtcars` dataset
# from the `datasets` package.
# The `mtcars` data originally came from the 1974 Motor Trend US magazine,
# and it contains design and performance data on 32 models of automobile.
# Type `?mtcars` for more information.
#
# `mtcars$mpg` is the fuel efficiency in miles per gallon,
# and `mtcars$wt` is the weight in tons. `mpg` and `wt`
# will become `x` and `y`, respectively.
#
# Since we only have 32 rows in the mtcars dataset,
# we will create bigger datasets by resampling
# the rows with replacement (bootstrapping).
# Then, we will apply a couple of regression models
# and ask how well we can use a car's weight
# to model its fuel efficiency.
#
###################################
### LOAD PACKAGES AND FUNCTIONS ###
###################################

# To skip to the "CHECK AND DEBUG WORKFLOW PLAN" section, just
# call load_mtcars_example().

library(knitr) # drake knows you loaded knitr.
library(drake)
pkgconfig::set_config("drake::strings_in_dots" = "literals")

clean() # remove any previous drake output

# Pick a random subset of n rows from a dataset
random_rows <- function(data, n){
  data[sample.int(n = nrow(data), size = n, replace = TRUE), ]
}

# Bootstrapped datasets from mtcars.
simulate <- function(n){
  # Pick a random set of cars to bootstrap from the mtcars data.
  data <- random_rows(data = mtcars, n = n)

  # x is the car's weight, and y is the fuel efficiency.
  data.frame(
    x = data$wt,
    y = data$mpg
  )
}

# Try a couple different regression models.

# Is fuel efficiency linearly related to weight?
reg1 <- function(d){
  lm(y ~ + x, data = d)
}

# Is fuel efficiency related to the SQUARE of the weight?
reg2 <- function(d){
  d$x2 <- d$x ^ 2
  lm(y ~ x2, data = d)
}

# At this point, please verify that a dynamic report
# called report.Rmd is in your working directory.

###############################
### CONSTRUCT WORKFLOW PLAN ###
###############################

# To skip to the "CHECK AND DEBUG WORKFLOW PLAN" section, just
# call load_mtcars_example().

# We write drake commands to generate our two bootstrapped datasets.
my_datasets <- drake_plan(
  small = simulate(48),
  large = simulate(64)
)

# Optionally, get replicates with expand_plan(my_datasets,
#   values = c("rep1", "rep2")).
# Bootstrapping involves randomness, so this is good practice
# in real life. But this is a miniaturized workflow,
# so we will not use replicates here.

# This is a wildcard template for generating more commands.
# These new commands will apply our regression models
# to each of the datasets in turn.
methods <- drake_plan(
  regression1 = reg1(dataset__),
  regression2 = reg2(dataset__)
)

# Here, we use the template to expand the `methods` template
# over the datasets we will analyze.
# Same as evaluate(methods, wildcard = "..dataset..",
#   values = my_datasets$target)
my_analyses <- plan_analyses(methods, datasets = my_datasets)

# Now, we summarize each regression fit of each bootstrapped dataset.
# We will look at these summaries to figure out if fuel efficiency
# and weight are related somehow.
# Again, this is a template. Later we will expand it over the
# available regression models.
summary_types <- drake_plan(
  summ = suppressWarnings(summary(analysis__$residuals)), # Summarize the RESIDUALS of the model fit. # nolint
  coef = suppressWarnings(summary(analysis__))$coefficients # Coefficinents with p-values # nolint
)

# Here, we expand the commands to summarize each analysis in turn.
# summaries() also uses evaluate(): once with expand = TRUE,
#   once with expand = FALSE
results <- plan_summaries(
  summary_types,
  my_analyses,
  my_datasets,
  gather = NULL
) # skip 'gather' (workflow my_plan is more readable)

# Use `knitr_in()` to tell drake to look for dependencies
# inside report.Rmd (targets referenced explicitly with loadd() and readd()
# in active code chunks).
# Use file_out() to tell drake that the target is a file.
# Drake knows to put report.md in the "target" column when it comes
# time to make().
report <- drake_plan(
  report = knit(knitr_in("report.Rmd"), file_out("report.md"), quiet = TRUE)
)

# Row order doesn't matter in the workflow my_plan.
my_plan <- rbind(report, my_datasets, my_analyses, results)


# For the commands you specify the free-form `...` argument,
# `drake_plan()` also supports tidy evaluation.
# For example, it supports quasiquotation with the `!!` argument.
# Use `tidy_evaluation = FALSE` or the `list` argument
# to suppress this behavior.

my_variable <- 5

drake_plan(
  a = !!my_variable,
  b = !!my_variable + 1,
  list = c(d = "!!my_variable")
)

drake_plan(
  a = !!my_variable,
  b = !!my_variable + 1,
  list = c(d = "!!my_variable"),
  tidy_evaluation = FALSE
)

# For instances of !! that remain in the workflow plan,
# make() will run these commands in tidy fashion,
# evaluating the !! operator using the environment you provided.

#####################################
### CHECK AND DEBUG WORKFLOW PLAN ###
#####################################

# Check for circularities, missing input files, etc.
check_plan(my_plan)

# Check the dependencies of individual functions and commands.
# See also deps_targets().
deps_code(reg1)
deps_code(my_plan$command[1])
deps_code(my_plan$command[nrow(my_plan)])

################################
### SINGLE PROCESS EXECUTION ###
################################

# Start off with a clean workspace (optional).
clean() # Cleans out the hidden cache in the .drake/ folder if it exists.

# Get a drake config list so you can use
# other utility functions
config <- drake_config(my_plan, verbose = FALSE)

# All the targets in the plan are "outdated" because we have not made them yet.
outdated(config)
# vis_drake_graph(my_plan) # Show how the pieces of your workflow are connected #nolint: optional
missed(config) # Nothing should be missing from your workspace.

# Run your project.
make(my_plan) # Return an updated config list
# The non-file dependencies of your last target are already loaded
# in your workspace.

# How long did each target take to build?
build_times()

ls() # Should contain the non-file dependencies of the last target(s).
progress() # See also in_progress()
outdated(config) # Everything is up to date
# vis_drake_graph(my_plan) # The red nodes from before turned green. #nolint: optional
# session() # get the sessionInfo() of the last call to make() #nolint: optional

# Since the p-value on x2 is so low,
# we can say that
readd(coef_regression2_large) # see also: loadd(), cached(), imported(), and built() # nolint

# Everything is up to date.
config <- make(my_plan)

# What if we want to explore a cubic term?
# What if we want to know if fuel efficiency is associated with weight cubed?
reg2 <- function(d){
  d$x3 <- d$x ^ 3
  lm(y ~ x3, data = d)
}
outdated(config) # The targets depending on reg2() are now out of date...
# vis_drake_graph(config) # ...which is indicated in the graph. #nolint: optional

make(my_plan) # Drake only runs targets that depend on reg2().

# For functions and my_plan$command,
# trivial changes like comments and whitespace are ignored.
reg2 <- function(d){
  d$x3 <- d$x ^ 3
    lm(y ~ x3, data = d) # I indented here.
}
outdated(config) # Everything is still up to date.

# Drake cares about nested functions too:
# nontrivial changes to `random_rows()` will propagate to `simulate()`
# and all the downstream targets.

random_rows <- function(data, n){
  n <- n + 1
  data[sample.int(n = nrow(data), size = n, replace = TRUE), ]
}
outdated(config)
make(my_plan)

#########################################
### NEED TO ADD MORE WORK ON THE FLY? ###
#########################################

# Just write more functions and add rows to your workflow plan.
# This function represents a null case.
# In other words, what would our methods discover if
# there were really no true relationship between weight and fuel efficiency?
# Our methods should detect no relationship.
new_simulation <- function(n){
  data.frame(x = rnorm(n), y = rnorm(n))
}

# Any R expression can be a command
# except for formulas and function definitions.
additions <- drake_plan(
  new_data = new_simulation(36) + sqrt(10)
)

# Add the new work
my_plan <- rbind(my_plan, additions)
config <- make(my_plan) # Only the new work is run.

# Clean up and start over next time.
# Use clean(small), clean(list = "large"), etc.
# to remove individual targets.
clean() # report.html and report.md are removed, but report.Rmd stays.

# Garbage collection
drake_gc() # Also consider clean(garbage_collection = TRUE)

###############################################
### ONE R SESSION WITH 2 PARALLEL PROCESSES ###
###############################################

clean()

config <- make(my_plan, jobs = 2) # parallelism == "parLapply" for Windows
# make(my_plan, parallelism = "mclapply", jobs = 2) # Not for Windows #nolint: optional
readd(coef_regression2_large) # see also: loadd(), cached()

# All up to date.
make(my_plan, jobs = 2)
clean() # Start over next time.

######################################
### PARALLEL COMPUTING WITH FUTURE ###
######################################

# The `future` and `future.batchtools` packages
# unlock a huge array of powerful parallel backends.
# Here is just a taste. You can find a list of
# future.batchtools backends at
# https://github.com/HenrikBengtsson/future.batchtools#choosing-batchtools-backend # nolint
# Note: the `jobs` does not apply to the "future_lapply" backend.
# Use `options(mc.cores = 4)` or something similar from ?future.options
# to cap the number of simultaneous jobs.
options(mc.cores = 2)
library(future)
future::plan(multicore) # Avoid drake::plan().
make(my_plan, parallelism = "future_lapply")
clean() # Erase the targets to start from scratch.

future::plan(multisession) # Use separate background R sessions.
make(my_plan, parallelism = "future_lapply")
clean()

if (require(future.batchtools)){ # More heavy-duty future-style parallel backends # nolint
  future::plan(batchtools_local)
  make(my_plan, parallelism = "future_lapply")
  clean()

  # Deploy targets with batchtools_local and use `future`-style
  # multicore parallism each individual target's command.
  future::plan(list(batchtools_local, multicore))
  make(my_plan, parallelism = "future_lapply")
  clean()
}
clean() # Start over next time

######################################################
### DISTRIBUTED COMPUTING: TWO PARALLEL R SESSIONS ###
######################################################

# Write a Makefile and execute it to spawn up to two
# R sessions at a time.
# Windows users need Rtools (https://cran.r-project.org/bin/windows/Rtools)
# Everyone else just needs Make (https://www.gnu.org/software/make)
# or an equivalent program.
make(my_plan, parallelism = "Makefile", jobs = 2) # build everything
readd(coef_regression2_large) # see also: loadd(), cached()

# Drake tells the Makefile what is already up to date.
make(my_plan, parallelism = "Makefile", jobs = 2)
clean() # Start over next time.

######################################################
### DISTRIBUTED COMPUTING: FOUR NODES ON A CLUSTER ###
### ONLY ATTEMPT ON A PROPER COMPUTING CLUSTER     ###
######################################################

# Use FALSE on regular local machines.
if (FALSE){
# if (TRUE){ # Only attempt this part on a proper computing cluster.

  # The file shell.sh tells the Makefile to submit jobs on a cluster.
  # You could write this file by hand if you want, or you can
  # generate a starter file with drake::shell_file().
  # You may have to change 'module load R' to a command that
  # loads a specific version of R.
  # Writes an example shell.sh and does a `chmod +x` so drake can execute it.

  shell_file()

  # In reality, you would put all your code in an R script
  # and then run it in the Linux/Mac terminal with
  # nohup nice -19 R CMD BATCH my_script.R &

  # Run up to four parallel jobs on the cluster or supercomputer,
  # depending on what is needed. These jobs could go to multiple
  # nodes for true distributed computing.
  make(my_plan, parallelism = "Makefile", jobs = 4, # build
    prepend = "SHELL=./shell.sh") # Generate shell.sh with shell_file().

  # Alternatively, users of SLURM (https://slurm.schedmd.com/)
  # can just point to `srun` and dispense with `shell.sh` altogether.
  # make(my_plan, parallelism = "Makefile", jobs = 4,
  #   prepend = "SHELL=srun")

  readd(coef_regression2_large) # see also: loadd(), cached()

  # Everything is up to date, so no jobs should be submitted.
  make(
    my_plan,
    parallelism = "Makefile",
    jobs = 4,
    prepend = "SHELL=./shell.sh"
  )
} # if(FALSE)

###########################
### CLEAN UP ALL OUTPUT ###
###########################

clean(destroy = TRUE) # Totally remove the hidden .drake/ cache.
unlink(
  c(
    ".future",
    "Makefile",
    "shell.sh",
    "STDIN.o*"
  ),
  recursive = TRUE
) # Clean up other files.
