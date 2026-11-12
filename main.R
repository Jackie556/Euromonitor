#Library


library(jsonlite)
library(readr)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(countrycode)
library(purrr)
library(ggplot2)





#Step-1 Data extraction----------------------------------

#Create folders

dir.create("input",  showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)


#World Bank: Total Internet Users (automated, via API)
# Full yearly history 2000-2025 for all countries


wb_url <- paste0(
  "https://api.worldbank.org/v2/country/all/indicator/IT.NET.USER.ZS",
  "?format=json&per_page=20000&date=2000:2025"
)

wb_json <- fromJSON(wb_url, flatten = TRUE)


# Element 1 = metadata, element 2 = data

cat("Pages:", wb_json[[1]]$pages, " Total records:", wb_json[[1]]$total, "\n")
wb_raw <- wb_json[[2]]

#raw file without any edits

write_csv(wb_raw, "input/total_internet_raw.csv")

# ITU: Mobile Internet Users

if (file.exists("mobile_internet_users.csv")) {
  file.copy("mobile_internet_users.csv",
            "input/mobile_internet_raw.csv",
            overwrite = TRUE)
}

# 5. Check that both raw files are saved
stopifnot(file.exists("input/total_internet_raw.csv"),
          file.exists("input/mobile_internet_raw.csv"))

# 6. Quick look
total_check  <- read_csv("input/total_internet_raw.csv",show_col_types = FALSE)
mobile_check <- read_csv("input/mobile_internet_raw.csv",show_col_types = FALSE)

dim(total_check);  head(total_check)
dim(mobile_check); head(mobile_check)


#Step-2: DATA CLEANING & HARMONISATION ----------------

## 2.1 Read raw files (read-only)


total_raw  <- read_csv("input/total_internet_raw.csv",  show_col_types = FALSE)
mobile_raw <- read_csv("input/mobile_internet_raw.csv",
                       col_types = cols(.default = col_character()),
                       name_repair = "minimal")


## 2.2 Total Internet Users (World Bank)


total_clean <- total_raw %>%
  transmute(
    iso3  = countryiso3code,
    year  = as.integer(date),                       # align year format
    total_internet_users = suppressWarnings(as.numeric(value)) # numeric
  ) %>%
  

# Keep real countries only: valid ISO3 code removes regional aggregates
# (World, High income, etc.)
  
filter(iso3 %in% codelist$iso3c, !is.na(iso3)) %>%
  
# Inconsistent: WB uses 0 as a placeholder for "no data" -> set to NA
  
mutate(total_internet_users = ifelse(total_internet_users <= 0, NA,
                                       total_internet_users))

# 2.3 Mobile Internet Users (ITU)
# Names that countrycode cannot match automatically


custom_names <- c(
  "Dominican Rep."   = "DOM",
  "Türkiye"          = "TUR",
  "Palestine"        = "PSE",
  "Hong Kong, China" = "HKG",
  "Macao, China"     = "MAC",
  "Czech Republic"   = "CZE",
  "Côte d'Ivoire"    = "CIV",
  "Cabo Verde"       = "CPV"
)

mobile_clean <- mobile_raw %>%
  pivot_longer(cols = -Economy, names_to = "year", values_to = "value_chr") %>%
  mutate(
    year = as.integer(year),                              # year as integer
    value_chr = str_trim(value_chr),
    value_chr = na_if(value_chr, "-"),                    # "-" means missing
    mobile_internet_users = suppressWarnings(
      as.numeric(str_remove(value_chr, "%"))              # "98.7%" -> 98.7
    ),
    iso3 = countrycode(Economy, "country.name", "iso3c",
                       custom_match = custom_names)        # standardise names
  )


# Check of every economy was matched


unmatched <- mobile_clean %>% filter(is.na(iso3)) %>% distinct(Economy)
print(unmatched)   # should be empty; if not, add them to custom_names

mobile_clean <- mobile_clean %>%
  filter(!is.na(iso3)) %>%
  select(iso3, year, mobile_internet_users)

#Merging on ISO3 and year


merged <- full_join(total_clean, mobile_clean, by = c("iso3", "year")) %>%
  mutate(country = countrycode(iso3, "iso3c", "country.name")) %>%  # one name set
  select(country, iso3, year, total_internet_users, mobile_internet_users) %>%
  arrange(country, year)


#Handled inconsistent values
#Duplicates on country-year

merged <- merged %>% distinct(iso3, year, .keep_all = TRUE)

# Values above 100% are impossible for a % of population -> cap at 100
#(flag them first so Task 3 can report them)

merged <- merged %>%
  mutate(
    flag_over100 = (total_internet_users > 100) | (mobile_internet_users > 100),
    total_internet_users  = pmin(total_internet_users, 100),
    mobile_internet_users = pmin(mobile_internet_users, 100)
  )


# Missing values are NOT filled in here. The two series have different
# coverage, so imputing would invent data. Missing stays NA and is handled
# in modelling (Task 4).

# Save the cleaned dataset

write_csv(merged, "output/clean_merged.csv")

#Quick checks


cat("Rows:", nrow(merged), " Countries:", n_distinct(merged$iso3), "\n")
cat("Years:", range(merged$year), "\n")
cat("Rows with both values:",
    sum(!is.na(merged$total_internet_users) & !is.na(merged$mobile_internet_users)), "\n")
cat("Capped (>100) rows:", sum(merged$flag_over100, na.rm = TRUE), "\n")
summary(merged)



#Step-3: DATA VALIDATION ----------


df <- read_csv("output/clean_merged.csv", show_col_types = FALSE)

#MISSING VALUES
#Overall counts

missing_summary <- df %>%
  summarise(
    rows                 = n(),
    missing_total        = sum(is.na(total_internet_users)),
    missing_mobile       = sum(is.na(mobile_internet_users)),
    both_missing         = sum(is.na(total_internet_users) & is.na(mobile_internet_users)),
    mobile_only_missing  = sum(!is.na(total_internet_users) & is.na(mobile_internet_users)),
    both_present         = sum(!is.na(total_internet_users) & !is.na(mobile_internet_users))
  )
print(missing_summary)


#Mobile coverage per country (how many observations each has)

mobile_coverage <- df %>%
  group_by(country, iso3) %>%
  summarise(n_mobile = sum(!is.na(mobile_internet_users)),
            first_year = suppressWarnings
            (min(year[!is.na(mobile_internet_users)])),
            last_year  = suppressWarnings
            (max(year[!is.na(mobile_internet_users)])),
            .groups = "drop") %>%
  filter(n_mobile > 0) %>% arrange(n_mobile)
print(table(mobile_coverage$n_mobile))      # distribution of obs per country


#Total series: years of coverage per country

total_coverage <- df %>%
  group_by(country) %>%
  summarise(n_total = sum(!is.na(total_internet_users)), .groups = "drop")
print(summary(total_coverage$n_total))


#Mobile countries with no matching total value in the same year

no_total_match <- df %>%
  filter(!is.na(mobile_internet_users), is.na(total_internet_users)) %>%
  select(country, year, mobile_internet_users)
print(no_total_match)

#UNREALISTIC VALUES
# Out of range: >100 (flagged in Step 2 before capping) or negative

out_of_range <- df %>%
  filter(flag_over100 %in% TRUE |
           total_internet_users < 0 | mobile_internet_users < 0) %>%
  select(country, year, total_internet_users, mobile_internet_users, flag_over100)
print(out_of_range)


#Implausible jumps in the total series: change > 15 pp in one year 


jumps_total <- df %>%
  filter(!is.na(total_internet_users)) %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(change = total_internet_users - lag(total_internet_users),
         gap_years = year - lag(year),
         annual_change = change / gap_years) %>%
  ungroup() %>%
  filter(abs(annual_change) > 15) %>%
  select(country, year, total_internet_users, annual_change)
print(jumps_total)


#Outliers in mobile: a point more than 15 pp away from the median
#of the same country's other mobile observations (needs >= 3 points)


mobile_outliers <- df %>%
  filter(!is.na(mobile_internet_users)) %>%
  group_by(country) %>%
  mutate(n_obs = n(),
         median_others = sapply(seq_along(mobile_internet_users),
                                function(i) median(mobile_internet_users[-i]))) %>%
  ungroup() %>%
  filter(n_obs >= 3, abs(mobile_internet_users - median_others) > 15) %>%
  select(country, year, mobile_internet_users, median_others)
print(mobile_outliers)


#Large year-on-year drops in mobile (usage rarely falls by more than 10 pp)


mobile_drops <- df %>%
  filter(!is.na(mobile_internet_users)) %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(change = mobile_internet_users - lag(mobile_internet_users),
         gap = year - lag(year)) %>%
  ungroup() %>%
  filter(!is.na(change), change < -10) %>%
  select(country, year, mobile_internet_users, change, gap)
print(mobile_drops)


#CONSTRAINT VIOLATIONS (mobile <= total)

violations <- df %>%
  filter(!is.na(total_internet_users), !is.na(mobile_internet_users),
         mobile_internet_users > total_internet_users) %>%
  mutate(gap_pp = mobile_internet_users - total_internet_users) %>%
  arrange(desc(gap_pp)) %>%
  select(country, year, total_internet_users, mobile_internet_users, gap_pp)

cat("Violations:", nrow(violations), "out of",
    sum(!is.na(df$total_internet_users) & !is.na(df$mobile_internet_users)),
    "comparable rows\n")
print(violations, n = 30)
print(summary(violations$gap_pp))

# Small gaps (< 2 pp) are likely rounding or estimation differences between
# ITU and the World Bank. Large gaps point to definitional or data errors.

#SAVE VALIDATION REPORTS

write_csv(missing_summary,  "output/validation_missing_summary.csv")
write_csv(mobile_coverage,  "output/validation_mobile_coverage.csv")
write_csv(out_of_range,     "output/validation_out_of_range.csv")
write_csv(mobile_outliers,  "output/validation_mobile_outliers.csv")
write_csv(violations,       "output/validation_constraint_violations.csv")

#VALIDATED DATASET FOR MODELLING

df_valid <- df %>%
  mutate(
# ITU value is read as % of Internet USERS 
#(see validation: ratio > 1 in most rows)
    # cap 100.17, 102.8, 100.39
    mobile_share_of_users = pmin(mobile_internet_users, 100),
    mobile_internet_users = total_internet_users * mobile_share_of_users / 100  # % of population
  )

stopifnot(all(df_valid$mobile_internet_users <= df_valid$total_internet_users,
              na.rm = TRUE))
write_csv(df_valid, "output/clean_validated.csv")




#Step-4: MODELLING & FORECASTING ---------


HORIZON <- 2030
PHI     <- 0.8   # damping: trend fades each year instead of running on forever
Z80     <- qnorm(0.90) # 80% interval


# Start from clean_merged.csv, NOT the old clean_validated.csv 
#(that one capped mobile at total)


d <- read_csv("output/clean_merged.csv", show_col_types = FALSE) %>%
  select(country, iso3, year, total_internet_users, mobile_internet_users) %>%
  filter(year <= 2025)

names_tbl <- d %>% distinct(iso3, country)


## 4.1 TOTAL INTERNET USERS (% pop): observed/interpolated,
#then damped logit trend to 2030


total_path <- function(df) {
  obs <- df %>% filter(!is.na(total_internet_users)) %>% arrange(year)
  if (nrow(obs) == 0) return(tibble(year = integer(), total = numeric(), total_source = character()))
  last_y <- max(obs$year)
  yrs    <- min(obs$year):HORIZON
  tot    <- rep(NA_real_, length(yrs))
  inside <- yrs <= last_y
  tot[inside] <- if (nrow(obs) >= 2) approx(obs$year, obs$total_internet_users, xout = yrs[inside])$y
  else obs$total_internet_users
  clipp  <- function(x) qlogis(pmin(pmax(x / 100, .001), .999))
  recent <- obs %>% filter(year >= last_y - 9)
  slope  <- if (nrow(recent) >= 4)
    max(unname(coef(lm(clipp(total_internet_users) ~ year, data = recent))[2]), 0) else 0
  base <- clipp(obs$total_internet_users[obs$year == last_y])
  h <- seq_len(HORIZON - last_y)
  if (length(h) > 0) tot[yrs > last_y] <- 100 * plogis(base + slope * PHI * (1 - PHI^h) / (1 - PHI))
  tibble(year = yrs, total = tot,
         total_source = ifelse(yrs %in% obs$year, "observed",
                               ifelse(yrs <= last_y, "interpolated", "forecast")))
}
tp <- d %>% group_by(iso3) %>% group_modify(~ total_path(.x)) %>% ungroup()



#4.2 MOBILE SHARE OF INTERNET USERS (0-1)


share <- d %>%
  filter(!is.na(mobile_internet_users)) %>%
  mutate(share = pmin(pmax(mobile_internet_users / 100, 0), 1)) %>%   # cap values >100
  arrange(iso3, year)


# Drop isolated spikes 
#(interior point >15pp away from interpolation of the others, e.g. Israel 2020)


flag_isolated <- function(yr, x, thr = 0.15) {
  n <- length(x); out <- rep(FALSE, n)
  if (n >= 3) for (i in 2:(n - 1))
    out[i] <- abs(x[i] - approx(yr[-i], x[-i], xout = yr[i])$y) > thr
  out
}
share <- share %>% group_by(iso3) %>% mutate(outlier = flag_isolated(year, share)) %>% ungroup()
print(share %>% filter(outlier) %>% select(country, year, share))
share <- share %>% filter(!outlier)

# Pooled logit model: country fixed effects + one common time trend

fit_df <- share %>% mutate(y = qlogis(pmin(pmax(share, .01), .995)), t = year - 2018)
fit    <- lm(y ~ factor(iso3) + t, data = fit_df)
slope_s <- unname(coef(fit)["t"]); sigma_s <- sigma(fit)
cat("Common trend (logit/yr):", round(slope_s, 3), " residual sd:", round(sigma_s, 3), "\n")

## 4.3 FORECAST: last observed logit share + damped common trend

last_obs <- fit_df %>% group_by(iso3) %>% slice_max(year, n = 1, with_ties = FALSE) %>%
  ungroup() %>% select(iso3, last_year = year, last_logit = y) %>% filter(last_year < HORIZON)


# 2a. one row per country-year after its last observation
fc0 <- tidyr::crossing(last_obs, year = 2000:HORIZON) %>%
  filter(year > last_year)
cat("2a rows:", nrow(fc0), "\n")

# 2b. forecast the logit share
fc1 <- fc0 %>%
  mutate(h = year - last_year,
         logit_s = last_logit + slope_s * PHI * (1 - PHI^h) / (1 - PHI),
         se = sigma_s * sqrt(h),
         share_hat = plogis(logit_s),
         share_lo  = plogis(logit_s - Z80 * se),
         share_hi  = plogis(logit_s + Z80 * se))
cat("2b rows:", nrow(fc1), "\n")

# 2c. join the total-users path
fc2 <- fc1 %>% left_join(tp, by = c("iso3", "year"))
cat("2c rows:", nrow(fc2), " missing total:", sum(is.na(fc2$total)), "\n")

# 2d. final forecast table

fc <- fc2 %>%
  filter(!is.na(total)) %>%
  transmute(iso3, year, type = "forecast",
            total_internet_users  = total,
            mobile_share_of_users = 100 * share_hat,
            mobile_internet_users = total * share_hat,
            mobile_lower80 = total * share_lo,
            mobile_upper80 = total * share_hi,
            total_source)
cat("fc rows:", nrow(fc), "\n")





hist <- share %>%
  left_join(tp, by = c("iso3", "year")) %>%
  filter(!is.na(total)) %>%
  transmute(iso3, year, type = "historical",
            total_internet_users  = total,
            mobile_share_of_users = 100 * share,
            mobile_internet_users = total * share,
            mobile_lower80 = NA_real_, mobile_upper80 = NA_real_,
            total_source)

out <- bind_rows(hist, fc) %>%
  left_join(names_tbl, by = "iso3") %>%
  select(country, iso3, year, type, total_internet_users, mobile_share_of_users,
         mobile_internet_users, mobile_lower80, mobile_upper80, total_source) %>%
  arrange(country, year)






#Step-5: VISUALISATION----------



out <- read_csv("output/mobile_internet_forecasts.csv", show_col_types = FALSE)

hist_df <- out %>% filter(type == "historical")
fc_df   <- out %>% filter(type == "forecast")


# Anchor = last actual point per country,
#so the forecast line starts where the actual ends

anchor <- hist_df %>%
  group_by(iso3) %>% slice_max(year, n = 1, with_ties = FALSE) %>% ungroup() %>%
  mutate(type = "forecast",
         mobile_lower80 = mobile_internet_users,
         mobile_upper80 = mobile_internet_users)

fc_line    <- bind_rows(anchor, fc_df) %>% arrange(iso3, year)
cutoff_df  <- anchor %>% select(country, iso3, year)          # forecast start per country
total_line <- out %>% select(country, iso3, year, total_internet_users)

cols  <- c("Actual mobile" = "black", "Forecast mobile" = "red",
           "Total internet users (upper bound)" = "grey50")
ltys  <- c("Actual mobile" = "solid", "Forecast mobile" = "dashed",
           "Total internet users (upper bound)" = "dotted")


## Page 1: overview of all countries

overview <- ggplot() +
  geom_line(data = hist_df, aes(year, mobile_internet_users, group = iso3),
            colour = "black", alpha = .35, linewidth = .3) +
  geom_line(data = fc_line, aes(year, mobile_internet_users, group = iso3),
            colour = "red", alpha = .45, linewidth = .3, linetype = "dashed") +
  geom_vline(xintercept = max(hist_df$year) + .5, colour = "grey70") +
  scale_x_continuous(breaks = seq(2018, 2030, 2)) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(title = "Mobile internet users (% of population): all countries",
       subtitle = "Black solid = actual (ITU share x World Bank total); red dashed = forecast to 2030",
       x = NULL, y = "% of population") +
  theme_minimal(base_size = 10)

## Country pages: 12 panels each

countries <- sort(unique(out$country))
pages <- split(countries, ceiling(seq_along(countries) / 12))

country_page <- function(pg) {
  ggplot() +
    geom_ribbon(data = filter(fc_line, country %in% pg),
                aes(year, ymin = mobile_lower80, ymax = mobile_upper80,
                    fill = "Forecast 80% band"), alpha = .2) +
    geom_line(data = filter(total_line, country %in% pg),
              aes(year, total_internet_users,
                  colour = "Total internet users (upper bound)",
                  linetype = "Total internet users (upper bound)")) +
    geom_line(data = filter(fc_line, country %in% pg),
              aes(year, mobile_internet_users,
                  colour = "Forecast mobile", linetype = "Forecast mobile"),
              linewidth = .8) +
    geom_line(data = filter(hist_df, country %in% pg),
              aes(year, mobile_internet_users,
                  colour = "Actual mobile", linetype = "Actual mobile"),
              linewidth = .8) +
    geom_point(data = filter(hist_df, country %in% pg),
               aes(year, mobile_internet_users, colour = "Actual mobile"), size = 1.6) +
    geom_vline(data = filter(cutoff_df, country %in% pg),
               aes(xintercept = year), colour = "grey80") +
    scale_colour_manual(name = NULL, values = cols) +
    scale_linetype_manual(name = NULL, values = ltys) +
    scale_fill_manual(name = NULL, values = c("Forecast 80% band" = "red")) +
    scale_x_continuous(breaks = c(2020, 2025, 2030)) +
    coord_cartesian(xlim = c(2018, 2030), ylim = c(0, 100)) +
    facet_wrap(~ country, ncol = 4) +
    labs(title = "Mobile internet users (% of population): actual vs forecast to 2030",
         subtitle = "Mobile never exceeds total (dotted line): mobile = total x share of users",
         x = NULL, y = "% of population") +
    theme_minimal(base_size = 9) +
    theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
}



#show plots

print(overview)                 # overview page

print(country_page(pages[[1]])) # first 12 countries
print(country_page(pages[[2]])) # next 12, and so on


#individual country wise


show_country <- function(name) print(country_page(name))
show_country("Lithuania")
show_country(c("Lithuania", "Latvia", "Estonia", "Poland"))


#Whole dataset

pdf("output/plots.pdf", width = 11.7, height = 8.3)
print(overview)
for (pg in pages) print(country_page(pg))
dev.off()

stopifnot(file.exists("output/plots.pdf"), file.size("output/plots.pdf") > 0)
cat("Saved output/plots.pdf with", length(pages) + 1, "pages\n")










