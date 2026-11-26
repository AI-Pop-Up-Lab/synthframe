# Report milestones to the console and preserve the detailed trace in one log.
masterp_logger <- function(log_file, verbose) {
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("verbose must be TRUE or FALSE.")
  }
  if (is.null(log_file)) {
    directory <- file.path("data", "output", "masterp_logs")
    stamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
    log_file <- tempfile(paste0("masterp-", stamp, "-"), tmpdir = directory, fileext = ".log")
  }
  if (!is.character(log_file) || length(log_file) != 1L || is.na(log_file) || !nzchar(log_file)) {
    stop("log_file must be a single path.")
  }
  directory <- dirname(log_file)
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)
  path <- file.path(normalizePath(directory, mustWork = TRUE), basename(log_file))
  connection <- file(path, open = "at")
  solver_output <- NULL
  clock <- function() unname(proc.time()[["elapsed"]])

  write <- function(text, console = TRUE) {
    stamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    line <- paste(stamp, text)
    if (!is.null(solver_output)) flush(solver_output)
    writeLines(line, connection)
    flush(connection)
    if (console) cat(line, "\n", sep = "", file = stderr())
  }
  duration <- function(seconds) {
    if (seconds < 60) return(sprintf("%.1fs", seconds))
    sprintf("%dm %02ds", floor(seconds / 60), floor(seconds %% 60))
  }
  step <- function(label, expr) {
    write(paste("Started", label))
    started <- clock()
    result <- force(expr)
    write(paste("Finished", label, "in", duration(clock() - started)))
    result
  }
  progress <- function(label) {
    force(label)
    started <- clock()
    last_update <- -Inf
    function(done, total, detail = "") {
      if (done == 0L) {
        started <<- clock()
        last_update <<- -Inf
      }
      elapsed <- clock() - started
      status <- if (done == 0L) {
        "ETA pending"
      } else if (done < total) {
        paste("ETA", duration(elapsed * (total - done) / done))
      } else {
        paste("completed in", duration(elapsed))
      }
      console <- verbose || done <= 1L || done == total || elapsed - last_update >= 5
      write(paste0(label, " ", done, "/", total, " — ", status,
                   if (nzchar(detail)) paste0(" — ", detail)), console)
      if (console) last_update <<- elapsed
      invisible(NULL)
    }
  }
  capture_output <- function(expr) {
    # vglmer and SQUAREM write iteration traces to standard output.
    # Restore any caller-owned sink even when a solver stops with an error.
    sinks <- sink.number()
    output <- file(path, open = "at")
    solver_output <<- output
    on.exit({
      while (sink.number() > sinks) sink()
      close(output)
      solver_output <<- NULL
    }, add = TRUE)
    sink(output, split = verbose)
    force(expr)
  }
  run <- function(expr) {
    started <- clock()
    write(paste("MasterP started. Log file", path))
    result <- withCallingHandlers(
      force(expr),
      message = function(condition) {
        write(trimws(conditionMessage(condition)), console = verbose)
        tryInvokeRestart("muffleMessage")
      },
      warning = function(condition) write(paste("Warning:", conditionMessage(condition)), FALSE),
      interrupt = function(condition) {
        write(paste("MasterP interrupted after", duration(clock() - started)))
      },
      error = function(condition) {
        write(paste("MasterP failed after", duration(clock() - started), "—",
                    conditionMessage(condition)))
        # Record call names without printing survey data or large argument objects.
        calls <- vapply(sys.calls(), function(call) paste(deparse(call[[1L]]), collapse = " "),
                        character(1))
        write(paste("Call stack:", paste(calls, collapse = " → ")), FALSE)
      }
    )
    write(paste("MasterP finished in", duration(clock() - started)))
    result
  }
  list(path = path, write = write, step = step, progress = progress,
       capture_output = capture_output, run = run, close = function() close(connection))
}
