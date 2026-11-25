# Uniform sample of rows, loading only partitions selected by the sample.
sample_extended_frame <- function(frame, columns, partition_sizes = NULL, size = 100L) {
  if (is.data.frame(frame)) {
    rows <- sample.int(nrow(frame), min(size, nrow(frame)))
    return(frame[rows, columns, drop = FALSE])
  }

  stopifnot(length(frame) > 0L, length(frame) == length(partition_sizes),
            all(is.finite(partition_sizes) & partition_sizes > 0 &
                  partition_sizes == floor(partition_sizes)))
  boundaries <- c(0, cumsum(partition_sizes))
  rows <- sample.int(tail(boundaries, 1L), min(size, tail(boundaries, 1L)))
  partition <- findInterval(rows - 1, boundaries)
  positions <- split(seq_along(rows), partition)
  samples <- lapply(positions, function(index) {
    part <- partition[index[1L]]
    data <- readRDS(frame[[part]])
    stopifnot(nrow(data) == partition_sizes[part])
    data[rows[index] - boundaries[part], columns, drop = FALSE]
  })
  combined <- do.call(rbind, samples)
  combined[order(unlist(positions, use.names = FALSE)), , drop = FALSE]
}
