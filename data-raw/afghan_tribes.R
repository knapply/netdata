target_url <- "https://sites.google.com/site/understandingdarknetworks/Home/appendix-1/Afghan%20Tribal%20Network.zip"

temp_dir <- tempdir()
temp_file <- tempfile(".zip")

download.file(target_url, destfile = temp_file)
unzip(temp_file, exdir = temp_dir)

all_files <- dir(temp_dir, pattern = ".csv", full.names = TRUE)

as_kv_vec <- function(x) {
  # vec <- c(...)
  names(x) <- seq_along(x) - 1L
  x
}

kv_tribal_order <- as_kv_vec("Super Tribe", "Tribe", "Sub-Tribe")
kv_religion <- as_kv_vec("Sunni", "Shia", "Other")
kv_ethnicity <- as_kv_vec("Pashtun", "Hazara", "Tajik", "Baluch", "Chahar Aimak", "Brahui")
stance_on_gov_kv <- as_kv_vec("Pro-Government", "Anti-Government", "Unknown")
stance_on_taliban <- as_kv_vec("Pro-TB", "Anti-TB", "Unknown")

raw_attrs <- data.table::fread(all_files[grepl("/Attributes\\.csv$", all_files)])
raw_attrs <- readLines(all_files[grepl("/Attributes\\.csv$", all_files)])

init_attrs <- do.call(
  rbind, lapply(lapply(raw_attrs[1:92], strsplit,  ","), unlist, use.names = FALSE)
)

cleaned_attrs <- init_attrs[-1L, -1L]
dimnames(cleaned_attrs) <- list(init_attrs[-1L , 1L], init_attrs[1L, -1L])

cbind(rownames(cleaned_attrs), cleaned_attrs)

kvs <- lapply(list(c("Super Tribe", "Tribe", "Sub-Tribe"),
                   c("Sunni", "Shia", "Other"),
                   c("Pashtun", "Hazara", "Tajik", "Baluch", "Chahar Aimak", "Brahui"),
                   c("Pro-Government", "Anti-Government", "Unknown"),
                   c("Pro-TB", "Anti-TB", "Unknown")),
              as_kv_vec)

`!!!` <- rlang::`!!!`
# cleaned_attrs[, 1] <- dplyr::recode(cleaned_attrs[, 1], !!!kvs)
cleaned_attrs <- as.matrix(
  mapply(function(x, y) dplyr::recode(x, !!!(y)),
         as.data.frame(cbind(rownames(cleaned_attrs), cleaned_attrs),
                             stringsAsFactors = FALSE),
         list(, kvs))
)


apply(cleaned_attrs, 2, list)

mapply(function(x, y) dplyr::recode(x, y),
       # apply(cleaned_attrs, 2, list),
       as.data.frame(cleaned_attrs, stringsAsFactors = FALSE),
       setNames(),
              as_kv_vec), colnames(cleaned_attrs))
       )

cleaned_attrs[ , "Tribal Order"] <- recode(
  cleaned_attrs[ , "Tribal Order"],
  as_kv_vec("Super Tribe", "Tribe", "Sub-Tribe")
)
recode


























#' Recode a `character` `matrix`.
#'
#' @param x `character` `matrix` with non-`NULL` `colnames()`.
#' @param kvs key-value pairs as named `list` (with names corresponding to `x`'s
#' `colnames()` with each element containing a named `vector`, the `names()` of which
#' corresponding to replacemets for those matched values. See example.
#'
#' @example
#' test_mat <- matrix(c("a", "b"), nrow = 3, ncol = 2, byrow = TRUE,
#'                    dimnames = list(NULL, c("col1", "col2")))
#'
#' key_values <- list(col1 = c("a2" = "a"), col2 = c("b2" = "b"))
#' #                  ^^^^     ^^^    ^^^
#' #        column-name  new-value    old-value
#'
#'  recode_col_chr(test_mat, list(col1 = c("a" = 4)))

# recode_chr_matrix <- function(x, kvs) {
#   col_names <- names(kvs)
#   for (i in seq_along(col_names)) {
#     target_col <- col_names[[i]]
#     if (!target_col %in% colnames(x)) next
#     x[, target_col] <- vapply(x[, target_col], function(i) {
#       if (!i %in% kvs[[target_col]]) return(i)
#       names(kvs[[target_col]][i == kvs[[target_col]]])
#     }, FUN.VALUE = typeof(x[, target_col]), USE.NAMES = FALSE)
#   }
#   x
# }
# test_mat <- matrix(c("a", "b"), nrow = 3, ncol = 2,
#                    byrow = TRUE,
#                    dimnames = list(NULL, c("col1", "col2")))
# recode_col(test_mat, list(col2 = c("3" = "b")))
