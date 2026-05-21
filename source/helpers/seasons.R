# =============================================
#   Respiratory seasons
# =============================================
# Winter (Oct-Mar) is the respiratory peak; Summer (Apr-Sep) the
# off-peak. Labels follow the respiratory convention: "Winter 2022-23"
# spans Oct 2022 through Mar 2023.

# Label the respiratory season of a date.
#   season_of(as.Date("2023-02-01"))  # "Winter 2022-23"
#   season_of(as.Date("2023-07-01"))  # "Summer 2023"
season_of <- function(date) {
  mo <- lubridate::month(date)
  y0 <- if_else(mo >= 10L, lubridate::year(date), lubridate::year(date) - 1L)
  if_else(
    mo %in% c(10:12, 1:3),
    sprintf("Winter %d-%02d", y0, (y0 + 1L) %% 100L),
    sprintf("Summer %d", y0 + 1L)
  )
}

# Step back one year, keeping the same kind (Winter -> Winter, Summer -> Summer).
#   previous_season("Summer 2024")     # "Summer 2023"
#   previous_season("Winter 2024-25")  # "Winter 2023-24"
previous_season <- function(s) {
  yr <- as.integer(str_extract(s, "\\d{4}"))
  if_else(
    str_starts(s, "Summer"),
    sprintf("Summer %d", yr - 1L),
    sprintf("Winter %d-%02d", yr - 1L, yr %% 100L)
  )
}
