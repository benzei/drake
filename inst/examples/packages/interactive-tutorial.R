# This file is an interactive tutorial that only depends
# on the included report.Rmd file.
# It is meant to walk you through the analysis step by step.
# The other files show how to set up this example
# as a serious drake project. Run make.R to deploy it
# as a serious workflow.
#
################
### OVERVIEW ###
################
#
# This small data analysis project explores some trends
# in R package downloads over time.
# The datasets are downloaded using the cranlogs package
# (https://github.com/metacran/cranlogs).

library(cranlogs)
cran_downloads(packages = "dplyr", when = "last-week")

# Above, each count is the number of times `dplyr`
# was downloaded from the RStudio CRAN mirror on the given day.
# To stay up to date with the latest download statistics,
# we need to refresh the data frequently.
# With `drake`, we can bring all our work up to date
# without restarting everything from scratch.
#
# This example is paired with a chapter of the user manual:
# https://ropenscilabs.github.io/drake-manual/example-packages.html

################
### ANALYSIS ###
################

# Drake knows about the packages you load with library() or require().

library(cranlogs)
library(drake)
library(dplyr)
library(ggplot2)
library(knitr)

# We want to explore the daily downloads from these packages.

package_list <- c(
  "knitr",
  "Rcpp",
  "ggplot2"
)

# We plan to use the cranlogs package.
# The data frames `older` and `recent` will
# contain the number of daily downloads for each package
# from the RStudio CRAN mirror.

data_plan <- drake_plan(
  recent = cran_downloads(packages = package_list, when = "last-month"),
  older = cran_downloads(
    packages = package_list,
    from = "2016-11-01",
    to = "2016-12-01"
  ),
  strings_in_dots = "literals"
)

# We want to summarize each set of
# download statistics a couple different ways.

output_types <- drake_plan(
  averages = make_my_table(dataset__),
  plot = make_my_plot(dataset__)
)

# We need to define functions to summarize
# and plot the data.

make_my_table <- function(downloads){
  group_by(downloads, package) %>%
    summarize(mean_downloads = mean(count))
}

make_my_plot <- function(downloads){
  ggplot(downloads) +
    geom_line(aes(x = date, y = count, group = package, color = package))
}

# Below, the targets `recent` and `older`
# each take turns substituting the `dataset__` wildcard.
# Thus, `output_plan` has four rows.

output_plan <- plan_analyses(
  plan = output_types,
  datasets = data_plan
)

# We plan to weave the results together
# in a dynamic knitr report.

report_plan <- drake_plan(
  knit(knitr_in("report.Rmd"), file_out("report.md"), quiet = TRUE)
)

# And we complete the workflow plan data frame by
# concatenating the results together.
# Drake analyzes the plan to figure out the dependency network,
# so row order does not matter.

whole_plan <- rbind(
  data_plan,
  output_plan,
  report_plan
)

# The latest download data needs to be refreshed every day, so we use
# triggers to force `recent` to always build.
# For more on triggers, see the guide on debugging and testing:
# https://ropenscilabs.github.io/drake-manual/debug.html # nolint

whole_plan$trigger <- "any" # default trigger
whole_plan$trigger[whole_plan$target == "recent"] <- "always"

# Now, we run the project to download the data and analyze it.
# The results will be summarized in the knitted report, `report.md`,
# but you can also read the results directly from the cache.

make(whole_plan)
readd(averages_recent)
readd(plot_recent)

# Because we used triggers, each make() rebuilds the `recent`
# data frame to get the latest download numbers for today.
# If the new data are the same as last time
# and nothing else changed,
# drake skips the other targets.

make(whole_plan)

# To visualize the build behavior, plot the dependency network.
# Target `recent` and everything depending on it is always
# out of date because of the `"always"` trigger.
# If you rerun the project tomorrow,
# the download counts will have been updated, so make()
# will refresh `averages_recent`, `plot_recent`, and
# `'report.md'`. Targets `averages_older` and `plot_older`
# are unaffected, so drake will skip them.

config <- drake_config(whole_plan)
vis_drake_graph(config)
