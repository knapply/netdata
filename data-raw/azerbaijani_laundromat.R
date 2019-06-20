readme <-
"Source: https://www.occrp.org/en/azerbaijanilaundromat/raw-data/

Description:
  The database below contains nearly 17,000 payments made to and from the four core
  UK-registered companies that made up the Azerbaijani Laundromat. Their bank accounts
  were held at the Estonian branch of Danske Bank and were obtained by Berlinske, an
  OCCRP partner. They cover the period form June 2012 until the end of 2014.

  The records provide a rare snapshot of where the money came from, where it went,
  who was involved and how it was spent. Not all details can be clearly seen, but the
  overall picture is crystal clear.

  The stories in the Azerbaijani Laundromat investigation are based in part on this data.
  OCCRP is sharing the full database here so that readers can do their own searches.

Cleaning Script:
  https://github.com/knapply/netdata/blob/master/data-raw/azerbaijani_laundromat.R"

library(readr)
library(jsonlite)
library(data.table)

target_url <- "https://cdn.occrp.org/projects/azerbaijanilaundromat/interactive/dkdata.json"

raw_data <- read_file(target_url)

parsed <- parse_json(raw_data)

col_names <- unique(unlist(lapply(parsed$data, names), use.names = FALSE))

init <- lapply(parsed$data, function(x) as.data.table(x[col_names]))

df <- rbindlist(init, fill = TRUE)

df[ , date := as.Date(date, format = "%Y-%m-%d")
    ][ , amount_eur := as.double(gsub("\\$|,", "", amount_eur))]

edges <- df[ , .(payer_name, beneficiary_name, date, purpose, amount_orig_currency,
                 amount_orig, amount_usd, amount_eur, investigation)]


payer_nodes <- df[ ,  names(df)[grepl("^payer", names(df))]
                   , with = FALSE]
setnames(payer_nodes, sub("^payer_?", "", names(payer_nodes))
         )[ , core := NULL]

beneficiary_nodes <- df[ ,  names(df)[grepl("^beneficiary", names(df))]
                         , with = FALSE]
setnames(beneficiary_nodes, sub("^beneficiary_?", "", names(beneficiary_nodes))
  )[ , core := NULL]

nodes <- unique(
  .rbind.data.table(payer_nodes, beneficiary_nodes)[ , -c("bank_country", "account")
     ][ , jurisdiction := ifelse(jurisdiction == "UNKNOWN", NA_character_, jurisdiction)
        ][ , node_type := type
           ][ , type := NULL]
)

g <- igraph::graph_from_data_frame(edges, directed = TRUE, vertices = nodes)


igraph::write_graph(
  g, file = "inst/data-files/azerbaijani_laundromat/azerbaijani_laundromat.graphml",
  format = "graphml"
)
fwrite(nodes, "inst/data-files/azerbaijani_laundromat/azerbaijani_laundromat-nodes.csv")
fwrite(edges, "inst/data-files/azerbaijani_laundromat/azerbaijani_laundromat-edges.csv")
write_lines(readme, "inst/data-files/azerbaijani_laundromat/README.txt")

sessionInfo()
# R version 3.6.0 (2019-04-26)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 17134)
#
# Matrix products: default
#
# locale:
# [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252
# [3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C
# [5] LC_TIME=English_United States.1252
#
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
#   [1] data.table_1.12.2 jsonlite_1.6      readr_1.3.1
#
# loaded via a namespace (and not attached):
# [1] Rcpp_1.0.1        packrat_0.5.0    crayon_1.3.4     R6_2.4.0
# [5] magrittr_1.5      pillar_1.4.1     rlang_0.3.4      curl_3.3
# [9] rstudioapi_0.9.0  tools_3.6.0      igraph_1.2.4.1   hms_0.4.2
# [13] compiler_3.6.0   pkgconfig_2.0.2  tibble_2.1.3

