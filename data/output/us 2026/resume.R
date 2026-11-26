out <- Sys.getenv("VOTEFRAME_US_OUTPUT_DIR", unset = file.path("data", "output", "us 2026"))
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
fitter <- readLines("R/fit_sequential_logit.R")
checkpoint <- which(fitter == "    fits[[outcome]] <- design")
stopifnot(length(checkpoint) == 1L)
fitter <- append(fitter, c(
  '    checkpoint_path <- file.path(dirname(log_file), paste0(extension_variable, "-stage-", i, ".rds"))',
  '    saveRDS(design, paste0(checkpoint_path, ".tmp"), compress = FALSE)',
  '    stopifnot(file.rename(paste0(checkpoint_path, ".tmp"), checkpoint_path))',
  '    system2("python3", shQuote(file.path(dirname(log_file), "update-resume.py")))'
), after = checkpoint)
eval(parse(text = fitter))

fit_chunks <- function(name, variable) {
  expressions <- chunk(name)
  for (expr in expressions) {
    # The published pre-fit report already contains the design diagnostics.
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        identical(expr[[2]], as.name("depth_support_dir"))) {
      depth_support_dir <<- NULL
    } else if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
               identical(expr[[2]], as.name(variable)) &&
               file.exists(file.path(out, paste0(variable, ".rds")))) {
      message("Loading saved fit ", variable)
      assign(variable, readRDS(file.path(out, paste0(variable, ".rds"))), envir = .GlobalEnv)
    } else {
      eval(expr, envir = .GlobalEnv)
    }
    if (exists(variable, envir = .GlobalEnv, inherits = FALSE)) break
  }
  model <- get(variable, envir = .GlobalEnv)
  path <- file.path(out, paste0(variable, ".rds"))
  if (!file.exists(path)) {
    saveRDS(model, paste0(path, ".tmp"), compress = FALSE)
    stopifnot(file.rename(paste0(path, ".tmp"), path))
    system2("python3", shQuote(file.path(out, "update-resume.py")))
  }
  print(model$fit_log[c("outcome", "iterations_used", "max_parameter_change", "status")])
}
fit_chunks("presidential-fit", "pres_fit")
fit_chunks("house-fit", "house_fit")
cat("Both national models saved.\n")
