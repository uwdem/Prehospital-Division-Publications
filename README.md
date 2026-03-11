Prehospital Division RSS Feed of Publications

Michael Spigner (mspigner@medicine.wisc.edu)

Problem: PubMed has built-in ability to generate an RSS feed from a search query. However, PubMed's ability to search by authors is limited because it's not possible to search author + institution pairs. Scopus, an Elsevier product, is another database with 92 million publications that has greater searchability. In Scopus, each author has their own unique ID that can be searched. Scopus does not have the ability to generate RSS feeds, so a workaround is needed to generate the feed from a search query.

Solution:
1. Use an R script to call the Scopus open access API using a defined set of author IDs for faculty in the prehospital division. Store the API key as a secret (encrypted) in GitHub.
2. Because of API usage limits, the script loops through authors one at a time and deduplicates by DOI or title (since multiple faculty can be on a single paper).
3. API response is formatted into an RSS XML.
4. YAML script used to to define GitHub Actions workflow, which relies on GitHub servers for script automation (no sensitive data in this script).
5. Script runs every Monday at 6:00 AM UTC: set up R, install dependencies, install packages, run the R script, and deploy the RSS XML to GitHub Pages from the gh-pages branch (feed.xml).

RSS Feed URL: https://uwdem.github.io/Prehospital-Division-Publications/feed.xml


This RSS feed is embedded in the new EMS Section website using the WordPress RSS Block in the UW Theme 2.0.
