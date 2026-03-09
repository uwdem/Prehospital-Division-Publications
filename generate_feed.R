# generate_feed.R
# Queries the Scopus API for publications by specified authors (via AU-ID)
# and generates an RSS XML feed file.
#
# Required environment variable: SCOPUS_API_KEY
# Output: docs/feed.xml (served by GitHub Pages)

library(httr2)
library(xml2)
library(glue)
library(jsonlite)

`%||%` <- function(x, y) if (is.null(x)) y else x

# --- Configuration -----------------------------------------------------------

# Scopus Author IDs for your group (used for querying AND filtering authors)
author_map <- list(
  "57212495960" = "Spigner M.",
  "57031978800" = "Gussick M.",
  "57190754112" = "Lohmeier M.",
  "55314929900" = "Mancera M.",
  "56943490400" = "Genthe N.",
  "57201460258" = "Tschautscher C.",
  "55635899500" = "Bernardoni B.",
  "57207298867" = "Cathers D.",
  "57201897308" = "Newberry R.",
  "56012093400" = "Kim M.",
  "57221073727" = "Kim M."
)

author_ids <- names(author_map)

feed_title       <- "Prehospital Division Publications"
feed_description <- "Recent publications from the UW Prehospital Division."
feed_link        <- "https://uwdem.github.io/Prehospital-Division-Publications/feed.xml"
max_per_author   <- 25

api_key <- Sys.getenv("SCOPUS_API_KEY")
if (nchar(api_key) == 0) {
  stop("SCOPUS_API_KEY environment variable is not set.")
}

# --- Helper functions --------------------------------------------------------

safe_get <- function(entry, field, default = "") {
  val <- entry[[field]]
  if (is.null(val)) default else as.character(val)
}

xml_escape <- function(text) {
  text <- gsub("&",  "&amp;",  text, fixed = TRUE)
  text <- gsub("<",  "&lt;",   text, fixed = TRUE)
  text <- gsub(">",  "&gt;",   text, fixed = TRUE)
  text <- gsub("\"", "&quot;", text, fixed = TRUE)
  text <- gsub("'",  "&apos;", text, fixed = TRUE)
  text
}

# Extract UW group authors from entry's author list
# Extract ALL authors from entry's author list
get_all_authors <- function(entry) {
  authors <- entry[["author"]]
  
  if (is.null(authors)) {
    # Fallback if the specific author list isn't there
    return(entry[["dc:creator"]] %||% "Unknown Author")
  }

  # Scopus usually returns a list of author objects
  # We extract "authname" (e.g., "Spigner M.") from each
  all_names <- sapply(authors, function(a) a[["authname"]] %||% "")
  all_names <- all_names[nchar(all_names) > 0]
  
  if (length(all_names) == 0) {
    return(entry[["dc:creator"]] %||% "Unknown Author")
  }

  paste(all_names, collapse = ", ")
}

# --- Query Scopus API (one author at a time) ---------------------------------

query_author <- function(au_id) {
  query <- paste0("AU-ID(", au_id, ")")
  message(glue("  Querying {query}..."))

  resp <- request("https://api.elsevier.com/content/search/scopus") |>
    req_url_query(
      query = query,
      count = max_per_author,
      sort  = "-coverDate"
    ) |>
    req_headers(
      `X-ELS-APIKey` = api_key,
      Accept         = "application/json"
    ) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()

  status <- resp_status(resp)

  if (status != 200) {
    body <- resp_body_string(resp)
    warning(glue("AU-ID {au_id} returned status {status}: {body}"))
    return(list())
  }

  data <- resp_body_json(resp)
  entries <- data[["search-results"]][["entry"]]

  if (is.null(entries) || length(entries) == 0) {
    return(list())
  }
  if (length(entries) == 1 && !is.null(entries[[1]][["error"]])) {
    return(list())
  }

  message(glue("    Found {length(entries)} articles"))
  return(entries)
}

message("Querying Scopus API for each author...")
all_entries <- list()
for (au_id in author_ids) {
  entries <- query_author(au_id)
  all_entries <- c(all_entries, entries)
  Sys.sleep(0.5)
}

# Deduplicate by DOI or title
seen <- character(0)
unique_entries <- list()
for (entry in all_entries) {
  doi   <- entry[["prism:doi"]] %||% ""
  title <- entry[["dc:title"]]  %||% ""
  key   <- if (nchar(doi) > 0) doi else title

  if (nchar(key) > 0 && !(key %in% seen)) {
    seen <- c(seen, key)
    unique_entries <- c(unique_entries, list(entry))
  }
}

message(glue("Total unique articles: {length(unique_entries)}"))

# Sort by cover date (newest first)
dates <- sapply(unique_entries, function(e) {
  e[["prism:coverDate"]] %||% "1900-01-01"
})
unique_entries <- unique_entries[order(dates, decreasing = TRUE)]

# --- Build RSS XML ------------------------------------------------------------

items_xml <- vapply(unique_entries, function(entry) {
  title   <- xml_escape(safe_get(entry, "dc:title", "Untitled"))
  doi     <- safe_get(entry, "prism:doi", "")
  journal <- safe_get(entry, "prism:publicationName", "")
  eid     <- safe_get(entry, "eid", "")
  date    <- safe_get(entry, "prism:coverDate", "")

  # Get ALL author names instead of just the group
  all_authors <- get_all_authors(entry)

  # Build link
  if (nchar(eid) > 0) {
    link <- paste0("https://www.scopus.com/record/display.uri?eid=", eid, "&amp;origin=resultslist")
  } else if (nchar(doi) > 0) {
    link <- paste0("https://doi.org/", doi)
  } else {
    link <- safe_get(entry, "prism:url", "#")
  }

  # FORMATTING FIX: Use a standard <br /> and ensure no extra escaping
  # We put the Journal and DOI inside CDATA to protect the HTML
  desc_body <- ""
  if (nchar(journal) > 0) desc_body <- paste0(journal, "<br />")
  if (nchar(doi) > 0)     desc_body <- paste0(desc_body, "DOI: ", doi)

  # pubDate formatting
  pub_date <- ""
  if (nchar(date) > 0) {
    parsed_date <- tryCatch(as.Date(date), error = function(e) NA)
    if (!is.na(parsed_date)) {
      pub_date <- format(parsed_date, "%a, %d %b %Y 00:00:00 +0000")
    }
  }

  glue("    <item>
      <title>{title}</title>
      <link>{link}</link>
      <description><![CDATA[{desc_body}]]></description>
      <dc:creator><![CDATA[{all_authors}]]></dc:creator>
      <pubDate>{pub_date}</pubDate>
      <guid isPermaLink=\"true\">{link}</guid>
    </item>")
}, character(1))

build_date <- format(Sys.time(), "%a, %d %b %Y %H:%M:%S +0000", tz = "UTC")

rss_xml <- glue('<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom" xmlns:dc="http://purl.org/dc/elements/1.1/">
<channel>
    <title>{xml_escape(feed_title)}</title>
    <link>{feed_link}</link>
    <description>{xml_escape(feed_description)}</description>
    <lastBuildDate>{build_date}</lastBuildDate>
    <atom:link href="{feed_link}" rel="self" type="application/rss+xml"/>
{paste(items_xml, collapse = "\n")}
  </channel>
</rss>')

# --- Write Output -------------------------------------------------------------

dir.create("docs", showWarnings = FALSE)
writeLines(rss_xml, "docs/feed.xml")
message(glue("RSS feed written to docs/feed.xml with {length(unique_entries)} items."))
message("Done!")


