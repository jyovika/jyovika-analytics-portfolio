# Downloads every grant from the ARC Data Portal API to data/grants/arc_grants.csv.
# Run once by hand (not on render): Rscript posts/grant-classification/download_grants.R
library(httr2)
library(dplyr)
library(purrr)
library(readr)

base_url  <- "https://dataportal.arc.gov.au/NCGP/API/grants"
page_size <- 1000

get_page <- function(page) {
  request(base_url) |>
    req_url_query(`page[size]` = page_size, `page[number]` = page) |>
    req_retry(max_tries = 3) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)
}

first   <- get_page(1)
n_pages <- first$meta$`total-pages`
message("Downloading ", first$meta$`total-size`, " grants over ", n_pages, " pages")

pages <- c(list(first), map(2:n_pages, \(p) { Sys.sleep(0.5); get_page(p) }, .progress = TRUE))

grants <- pages |>
  map(\(pg) pg$data$attributes) |>
  list_rbind() |>
  select(code,
         scheme          = `scheme-name`,
         start_year      = `funding-commencement-year`,
         organisation    = `announcement-admin-organisation`,
         funding         = `announced-funding-amount`,
         field_of_research = `primary-field-of-research`,
         summary         = `grant-summary`)

stopifnot(nrow(grants) == first$meta$`total-size`, !anyDuplicated(grants$code))

dir.create(here::here("data", "grants"), showWarnings = FALSE)
write_csv(grants, here::here("data", "grants", "arc_grants.csv"))
message("Saved ", nrow(grants), " grants")
