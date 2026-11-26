# Use base column indexing for frame inputs while retaining labels used for area joins.
as_plain_frame <- function(x, coerce = FALSE) {
  if (identical(class(x), "data.frame")) return(x)
  if (!is.data.frame(x)) return(if (coerce) as.data.frame(x) else x)
  row_names <- if (.row_names_info(x, 1L) > 0L) rownames(x)
  as.data.frame(x, row.names = row_names)
}
