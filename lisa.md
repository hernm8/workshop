InterestRates\_\_Inflation
================
Lisa
2025-10-17

``` r
library(readxl)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(lubridate)
```

    ## Warning: package 'lubridate' was built under R version 4.4.3

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(ggplot2)
library(scales)
```

    ## Warning: package 'scales' was built under R version 4.4.3

``` r
library(tidyr)
```

## 

``` r
# ===== 1) File path =====
xlsx_path <- "InterestRates__InflationData_andGraphs 1960-2024.xlsx"

# ===== 2) Read; if headers look like ...1, ...2, force row 1 as headers =====
raw_try <- read_excel(xlsx_path, col_names = TRUE)
if (all(grepl("^\\.\\.\\.", names(raw_try)))) {
  raw0 <- read_excel(xlsx_path, col_names = FALSE)
  hdr  <- as.character(unlist(raw0[1, ]))
  hdr[is.na(hdr) | hdr == ""] <- paste0("col", which(is.na(hdr) | hdr == ""))
  names(raw0) <- hdr
  dat <- raw0[-1, , drop = FALSE]
} else {
  dat <- raw_try
}

stopifnot(ncol(dat) >= 2)  # need at least Year + Interest

# We will use the FIRST THREE COLUMNS ONLY: Year / Interest / Inflation
c1 <- names(dat)[1]                      # Year-like
c2 <- names(dat)[2]                      # Interest
c3 <- if (ncol(dat) >= 3) names(dat)[3] else NULL  # Inflation (optional)

# ===== 3) Helpers =====
to_year <- function(x){
  # Convert ONLY the first column (c1) to a 4-digit year from Date, serial, or "1/1/1960"
  if (inherits(x, "Date")) return(lubridate::year(x))
  if (is.numeric(x)) {
    out <- ifelse(x >= 1800 & x <= 2100, x,
                  lubridate::year(as.Date(x, origin = "1899-12-30")))
    return(as.integer(out))
  }
  x <- as.character(x)
  y <- suppressWarnings(lubridate::year(lubridate::mdy(x)))
  y[is.na(y)] <- suppressWarnings(lubridate::year(lubridate::ymd(x[is.na(y)])))
  as.integer(y)
}

to_num <- function(x){
  # Parse Interest / Inflation into numerics (handles "3.21", "2,345", "5.0%")
  if (is.numeric(x)) return(as.numeric(x))
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub(",", "", x, fixed = TRUE)     # remove thousands commas
  x <- gsub("%", "", x, fixed = TRUE)     # remove percent signs
  x <- gsub("[^0-9eE+\\-\\.]", "", x)     # keep digits, dot, minus, exponent
  suppressWarnings(as.numeric(x))
}

# ===== 4) Build clean annual dataset =====
year_raw <- dat[[c1]]
ir_raw   <- dat[[c2]]
infl_raw <- if (!is.null(c3)) dat[[c3]] else rep(NA_character_, nrow(dat))

df <- tibble::tibble(
  year          = to_year(year_raw),     # ONLY col1 → year
  interest_rate = to_num(ir_raw),        # col2 → numeric
  inflation     = to_num(infl_raw)       # col3 → numeric (may be NA)
) |>
  filter(!is.na(year)) |>
  # If you have multiple rows per year (e.g., monthly), collapse to annual means:
  group_by(year) |>
  summarise(
    interest_rate = if (all(is.na(interest_rate))) NA_real_ else mean(interest_rate, na.rm = TRUE),
    inflation     = if (all(is.na(inflation)))     NA_real_ else mean(inflation,     na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(year)

has_infl <- any(!is.na(df$inflation))

# ===== 5) Mark election vs non-election =====
election_years <- seq(1960, 2024, by = 4)
df <- df |>
  mutate(
    election_year = year %in% election_years,
    period = if_else(election_year, "Election years", "Non-election years")
  )
```

``` r
# ===== 6) All-years chart (dual axis if inflation exists) =====
if (has_infl) {
  sf <- max(df$interest_rate, na.rm = TRUE) / max(df$inflation, na.rm = TRUE)
  p_all <- ggplot(df, aes(x = year)) +
    geom_line(aes(y = interest_rate, color = "Interest rate"), linewidth = 1.1) +
    geom_line(aes(y = inflation * sf,  color = "Inflation"),   linewidth = 1.1, linetype = 2) +
    geom_point(data = subset(df, election_year),
               aes(y = interest_rate, color = "Interest rate"), size = 2) +
    scale_y_continuous(
      name = "Interest rate (%)",
      sec.axis = sec_axis(~ . / sf, name = "Inflation (%)")
    ) +
    scale_x_continuous(breaks = pretty(df$year, n = 16)) +
    scale_color_manual(NULL, values = c("Interest rate" = "#2C7FB8", "Inflation" = "#D95F0E")) +
    labs(title = "Interest vs Inflation — Annual",
         caption = "Dots mark U.S. presidential election years", x = "Year") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top", panel.grid.minor = element_blank())
} else {
  p_all <- ggplot(df, aes(x = year, y = interest_rate)) +
    geom_line(linewidth = 1.1) +
    geom_point(data = subset(df, election_year), size = 2) +
    scale_x_continuous(breaks = pretty(df$year, n = 16)) +
    labs(title = "Interest Rate — Annual",
         y = "Interest rate (%)", x = "Year",
         caption = "Dots mark U.S. presidential election years") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}
p_all
```

![](lisa_files/figure-gfm/viz-1-all-years-1.png)<!-- -->

``` r

``` r
# ===== 8) OPTIONAL: timelines faceted by period + distributions =====
p_facet <- ggplot(df, aes(x = year)) +
  geom_line(aes(y = interest_rate, color = "Interest rate"), linewidth = 1.05) +
  { if (has_infl) geom_line(aes(y = inflation, color = "Inflation"),
                            linewidth = 1.05, linetype = 2) else NULL } +
  facet_wrap(~ period, ncol = 1, scales = "free_y") +
  scale_color_manual(NULL, values = c("Interest rate" = "#2C7FB8", "Inflation" = "#D95F0E")) +
  scale_x_continuous(breaks = pretty(df$year, n = 12)) +
  labs(title = "Election vs Non-election — Timelines", x = "Year", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

p_boxes <- df |>
  pivot_longer(cols = c(interest_rate, inflation),
               names_to = "metric", values_to = "value") |>
  filter(!is.na(value)) |>
  ggplot(aes(x = period, y = value, fill = metric)) +
  geom_boxplot(alpha = 0.9, outlier.alpha = 0.4) +
  scale_fill_manual(NULL, labels = c("Inflation", "Interest rate"),
                    values = c("#D95F0E", "#2C7FB8")) +
  labs(title = "Election vs Non-election — Distributions", x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

p_facet
```

![](lisa_files/figure-gfm/viz-3-optional-1.png)<!-- -->

``` r
p_boxes
```

![](lisa_files/figure-gfm/viz-3-optional-2.png)<!-- -->
