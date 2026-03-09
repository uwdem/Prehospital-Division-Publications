library(httr2)
library(xml2)
library(glue)

# --- Configuration -----------------------------------------------------------

# Scopus Author IDs
author_ids <- c(
  "57212495960",  # Michael Spigner
  "57031978800",  # Megan Gussick
  "57190754112",  # Michael Lohmeier
  "55314929900",  # Michael Mancera
  "56943490400",  # Nicholas Genthe
  "57201460258",  # Craig Tschautscher
  "55635899500",  # Brittney Bernardoni
  "57207298867",  # Drew Cathers
  "57201897308",  # Ryan Newberry
  "56012093400",  # Michael Kim (1)
  "57221073727"   # Michael Kim (2)
)

# Feed metadata
feed_title       <- "UW Prehospital Publications"
feed_description <- "Recent publications from the UW Prehospital Division"
feed_link        <- "https://github.com/uwdem/Prehospital-Division-Publications/feed.xml"
max_results      <- 50  # Number of recent articles to include.

# --- Build Scopus Query ------------------------------------------------------

# Construct the query string: AU-ID(xxx) OR AU-ID(yyy) OR ...
query <- paste(
  paste0("AU-ID(", author_ids, ")"),
  collapse = " OR "
)

api_key <- Sys.getenv("SCOPUS_API_KEY")
if (nchar(api_key) == 0) {
  stop("SCOPUS_API_KEY environment variable is not set.")
}

# --- Query Scopus API ---------------------------------------------------------

# The Scopus Search API returns article metadata.
# Documentation: https://dev.elsevier.com/documentation/ScopusSearchAPI.wadl

message("Querying Scopus API...")

resp <- request("https://api.elsevier.com/content/search/scopus") |>
  req_url_query(
    query   = query,
    count   = max_results,
    sort    = "-coverDate",   # newest first
    field   = "dc:title,dc:creator,prism:coverDate,prism:doi,prism:publicationName,dc:description,prism:url"
  ) |>
  req_headers(
    `X-ELS-APIKey` = api_key,
    Accept         = "application/json"
  ) |>
  req_perform()

if (resp_status(resp) != 200) {
  stop(glue("Scopus API returned status {resp_status(resp)}"))
}

data <- resp_body_json(resp)

entries <- data[["search-results"]][["entry"]]

if (is.null(entries) || length(entries) == 0) {
  message("No results returned from Scopus. Generating empty feed.")
  entries <- list()
}

message(glue("Retrieved {length(entries)} articles from Scopus."))

# --- Build RSS XML ------------------------------------------------------------

safe_get <- function(entry, field, default = "") {
  val <- entry[[field]]
  if (is.null(val)) default else as.character(val)
}

# Escape special XML characters
xml_escape <- function(text) {
  text <- gsub("&",  "&amp;",  text, fixed = TRUE)
  text <- gsub("<",  "&lt;",   text, fixed = TRUE)
  text <- gsub(">",  "&gt;",   text, fixed = TRUE)
  text <- gsub("\"", "&quot;", text, fixed = TRUE)
  text <- gsub("'",  "&apos;", text, fixed = TRUE)
  text
}

# Build each <item> block
items_xml <- vapply(entries, function(entry) {
  title   <- xml_escape(safe_get(entry, "dc:title", "Untitled"))
  creator <- xml_escape(safe_get(entry, "dc:creator", "Unknown Author"))
  date    <- safe_get(entry, "prism:coverDate", "")
  doi     <- safe_get(entry, "prism:doi", "")
  journal <- xml_escape(safe_get(entry, "prism:publicationName", ""))
  
  # Build article link from DOI if available, otherwise use Scopus URL
  if (nchar(doi) > 0) {
    link <- paste0("https://doi.org/", doi)
  } else {
    link <- safe_get(entry, "prism:url", "#")
  }
  
  # Build a description with author, journal, and date
  desc_parts <- c()
  if (nchar(creator) > 0) desc_parts <- c(desc_parts, paste0("Author: ", creator, " et al."))
  if (nchar(journal) > 0) desc_parts <- c(desc_parts, paste0("Journal: ", journal))
  if (nchar(date) > 0)    desc_parts <- c(desc_parts, paste0("Published: ", date))
  description <- xml_escape(paste(desc_parts, collapse = " | "))
  
  # Format date as RFC 822 for RSS (pubDate)
  pub_date <- ""
  if (nchar(date) > 0) {
    parsed_date <- tryCatch(
      as.Date(date),
      error = function(e) NA
    )
    if (!is.na(parsed_date)) {
      pub_date <- format(parsed_date, "%a, %d %b %Y 00:00:00 +0000")
    }
  }
  
  glue("    <item>
      <title>{title}</title>
      <link>{link}</link>
      <description>{description}</description>
      <author>{creator}</author>
      <pubDate>{pub_date}</pubDate>
      <guid isPermaLink=\"true\">{link}</guid>
    </item>")
}, character(1))

# Assemble the full RSS document
build_date <- format(Sys.time(), "%a, %d %b %Y %H:%M:%S +0000", tz = "UTC")

rss_xml <- glue('<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">
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
message(glue("RSS feed written to docs/feed.xml with {length(entries)} items."))
