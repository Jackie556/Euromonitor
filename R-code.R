# ---- 01_download.R ----
pkgs <- c("httr", "jsonlite", "readr")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
invisible(lapply(pkgs, library, character.only = TRUE))

dir.create("input", showWarnings = FALSE)

# ---------- 1) World Bank: Total Internet Users (automated, API) ----------
wb_url <- "https://api.worldbank.org/v2/country/all/indicator/IT.NET.USER.ZS"

get_page <- function(page) {
  r <- GET(wb_url, query = list(format = "json", per_page = 20000, page = page))
  stop_for_status(r)
  fromJSON(content(r, "text", encoding = "UTF-8"), flatten = TRUE)
}

first <- get_page(1)
pages <- first[[1]]$pages
wb <- first[[2]]
if (pages > 1) for (p in 2:pages) wb <- rbind(wb, get_page(p)[[2]])

# Save as returned by API (no cleaning)
write_csv(wb, "input/total_internet_user_raw.csv")
cat("World Bank rows saved:", nrow(wb), "\n")

# ---------- 2) ITU: Mobile Internet Users ----------
# Open the ITU DataHub (https://datahub.itu.int), find
# "Individuals using the Internet by mobile phone" (or the closest mobile-broadband indicator),
# copy the CSV download link, and paste it below.
itu_url  <- "D:/Euromonitor/Codes and plots/input/Individuals using the Internet by mobile phone_raw.csv" # <- paste direct CSV link here
itu_path <- "input/mobile_internet_raw.csv"







