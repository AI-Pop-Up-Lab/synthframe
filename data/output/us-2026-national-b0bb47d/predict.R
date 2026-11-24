out <- Sys.getenv("VOTEFRAME_US_OUTPUT_DIR")
lines <- readLines("vignettes/vignette_us_2026.Rmd")
fence <- intToUtf8(rep(96L, 3L))
chunk <- function(name) {
  start <- which(lines == paste0(fence, "{r ", name, "}"))
  finish <- which(lines == fence & seq_along(lines) > start)[1L]
  parse(text = lines[seq.int(start + 1L, finish - 1L)])
}
for (name in c("inputs", "covariates-and-targets")) eval(chunk(name))
n_draws <- 125L
loadNamespace("gKRLS")

# Save each stage immediately after it returns, in addition to the full model.
for (model in c("pres_fit", "house_fit")) {
  assign(model, readRDS(file.path(out, paste0(model, ".rds"))))
}
for (name in c("presidential-extension", "house-extension")) eval(chunk(name))

# Reuse the vignette's prediction code. Save each state's comparison totals
# as it finishes so progress is visible and the results remain inspectable.
expressions <- chunk("joint-rake")
for (expr in expressions) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      identical(expr[[2]], as.name("summaries"))) break
  eval(expr)
}
stopifnot(!run_rake)
summary_dir <- file.path(out, "state-totals")
dir.create(summary_dir, showWarnings = FALSE)
all_states <- states
summaries <- lapply(names(all_states), function(state) {
  message("Predicting state ", state, " with ", n_draws, " draws.")
  states <<- all_states[state]
  result <- summarize_states()[[1L]]
  path <- file.path(summary_dir, paste0(state, ".rds"))
  saveRDS(result, paste0(path, ".tmp"))
  stopifnot(file.rename(paste0(path, ".tmp"), path))
  result
})
states <- all_states
names(summaries) <- names(states)
stopifnot(length(summaries) == 51L)
expressions <- chunk("target-plots")
for (expr in expressions[-length(expressions)]) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      identical(expr[[2]], as.name("figure_dir"))) {
    figure_dir <- file.path(out, "figures")
  } else eval(expr)
}
cat("Completed pre-rake comparisons for all 51 states and DC partitions.\n")
