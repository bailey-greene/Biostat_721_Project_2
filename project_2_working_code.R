### Biostat 71
### Project 2
# Bailey Greene
# 2025.11.10
# Code Notes


# Libraries ---------------------------------------------------------------
library(tidyverse)
here::i_am("Greene_Bailey_Project2_Markdown.Rmd")
library(here)
here()


# Read In Data ------------------------------------------------------------

# all data sets are csv with no head or tail rows
# NA = NA
# date is in mmddyyy format
# some dates have multiple readings
AQ_2018 <- read_csv(here("data", "AQ_2018.csv"))
AQ_2019 <- read_csv(here("data", "AQ_2019.csv"))
AQ_2020 <- read_csv(here("data", "AQ_2020.csv"))


# Test Function Interior Code ---------------------------------------------

# create sample dataset
# using 2018 b/c it has neg value to test
AQ_SAMPLE <- AQ_2018

# renaming columns and reformatting date
AQ_SAMPLE <- AQ_SAMPLE |>
  # date and unit columns have concise names
  # pollutant value columns need to be adjusted
  rename(
    Daily_Max_8hr_CO = `Daily Max 8-hour CO Concentration`,
    Daily_Mean_PM2.5 = `Daily Mean PM2.5 Concentration`,
    Daily_Max_8hr_Ozone = `Daily Max 8-hour Ozone Concentration`
  ) |>
  # format date variable
  mutate(Date = mdy(Date))


# condensing data to one line per date
AQ_SAMPLE <- AQ_SAMPLE |>
  # remove complete duplicates to simplify averaging step
  distinct() |>
  # calculate daily averages and collapse to one row per date
  group_by(Date, units_CO, units_PM, units_O3) |>
  summarise(
    across(c(Daily_Max_8hr_CO, Daily_Mean_PM2.5, Daily_Max_8hr_Ozone),
           ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) 

# averaging converted NA to NaN
# negative values not accurate data
AQ_SAMPLE <- AQ_SAMPLE |>
# convert NaN back to NA and set negative values to zero in one step
  mutate(
    across(c(Daily_Max_8hr_CO, Daily_Mean_PM2.5, Daily_Max_8hr_Ozone),
         ~ case_when(
           is.nan(.x) ~ NA_real_,
           .x < 0 ~ 0,
           TRUE ~ .x
         ))
) 

# return columns to original order
AQ_SAMPLE <- AQ_SAMPLE |>
  select(Date, Daily_Max_8hr_CO, units_CO, Daily_Mean_PM2.5, units_PM, 
         Daily_Max_8hr_Ozone, units_O3)

# Build Function --------------------------------------------------------------

clean_air_quality <- function(df) {
  df_clean <- df |>
    # rename pollutant concentration columns
    rename(
      Daily_Max_8hr_CO = `Daily Max 8-hour CO Concentration`,
      Daily_Mean_PM2.5 = `Daily Mean PM2.5 Concentration`,
      Daily_Max_8hr_Ozone = `Daily Max 8-hour Ozone Concentration`
    ) |>
    # format date variable
    mutate(Date = mdy(Date)) |>
    # remove duplicate rows to simplify averaging
    distinct() |>
    # calculate daily averages and collapse to one row per date
    group_by(Date, units_CO, units_PM, units_O3) |>
    summarise(
      across(c(Daily_Max_8hr_CO, Daily_Mean_PM2.5, Daily_Max_8hr_Ozone),
             ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    # convert NaN back to NA and set negative values to zero
    mutate(
      across(c(Daily_Max_8hr_CO, Daily_Mean_PM2.5, Daily_Max_8hr_Ozone),
             ~ case_when(
               is.nan(.x) ~ NA_real_,
               .x < 0 ~ 0,
               TRUE ~ .x
             ))
    ) |>
    # return columns to original order
    select(Date, Daily_Max_8hr_CO, units_CO, Daily_Mean_PM2.5, units_PM, 
           Daily_Max_8hr_Ozone, units_O3)
  
  return(df_clean)
}


# Test Function -----------------------------------------------------------

# create new object to preserve original data
AQ_2018_Clean <- clean_air_quality(AQ_2018)
AQ_2019_Clean <- clean_air_quality(AQ_2019)
AQ_2020_Clean <- clean_air_quality(AQ_2020)

# combine clean data into a single dataset
AQ_All_Years <- bind_rows(AQ_2018_Clean, AQ_2019_Clean, AQ_2020_Clean)


# CO and O3 Plot ----------------------------------------------------------

# calculate monthly averages for CO
monthly_CO <- AQ_All_Years |>
  # convert date to month name
  mutate(Month = month(Date, label = TRUE, abbr = FALSE)) |>
  # group by month for analysis
  group_by(Month) |>
  # calculate mean, SE, and CI
  summarise(
    mean_conc = mean(Daily_Max_8hr_CO, na.rm = TRUE),
    se = sd(Daily_Max_8hr_CO, na.rm = TRUE) / sqrt(sum(!is.na(Daily_Max_8hr_CO))),
    ci_lower = mean_conc - 1.96 * se,
    ci_upper = mean_conc + 1.96 * se,
    .groups = "drop"
  )

# calculate monthly averages for Ozone
monthly_O3 <- AQ_All_Years |>
  # convert date into month name
  mutate(Month = month(Date, label = TRUE, abbr = FALSE)) |>
  # group by month for analysis
  group_by(Month) |>
  # calculate mean, se, and CI
  summarise(
    mean_conc = mean(Daily_Max_8hr_Ozone, na.rm = TRUE),
    se = sd(Daily_Max_8hr_Ozone, na.rm = TRUE) / sqrt(sum(!is.na(Daily_Max_8hr_Ozone))),
    ci_lower = mean_conc - 1.96 * se,
    ci_upper = mean_conc + 1.96 * se,
    .groups = "drop"
  )

# CO and O3 values are too far apart to appear on the same Y axis
# calculate scaling factor to place O3 values below CO visually
max_co <- max(monthly_CO$ci_upper, na.rm = TRUE)
max_o3 <- max(monthly_O3$ci_upper, na.rm = TRUE)
scale_factor <- (max_co * 0.6) / max_o3

# Add pollutant column and scale O3, then combine
monthly_CO <- monthly_CO |>
  mutate(Pollutant = "CO")
monthly_O3 <- monthly_O3 |>
  mutate(
    mean_conc = mean_conc * scale_factor,
    ci_lower = ci_lower * scale_factor,
    ci_upper = ci_upper * scale_factor,
    Pollutant = "Ozone"
  )

# combine monthly data for ggplot
monthly_combined <- bind_rows(monthly_CO, monthly_O3)

# create plot
ggplot(monthly_combined, aes(x = Month,
                             color = Pollutant, group = Pollutant)) +
  # confidence interval lines
  geom_linerange(aes(ymin = ci_lower, ymax = ci_upper),
                 linewidth = 0.8) +
  # point mean
  geom_point(aes(y = mean_conc), size = 2) +
  # apply colors to pollutants
  scale_color_manual(values = c("CO" = "darkorange3", "Ozone" = "steelblue4"),
                     labels = c("CO", expression(O[3]))) +
  # set y-axis scales
  scale_y_continuous(
    name = "CO Concentration (ppm)",
    sec.axis = sec_axis(~ . / scale_factor, name = expression(O[3]~"Concentration (ppm)"))
  ) +
  # title chart and note data limitations
  labs(
    title = expression(bold("Carbon Monoxide (CO) and Ozone ("*O[3]*") Concentrations (2018-2020)")),
    subtitle = "Monthly Average and 95% Confidence Intervals",
    x = "Month",
    caption = "Note: 2020 data is only through April"
  ) +
  # set background theme
  theme_minimal() +
  # # format axes and legend
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y.left = element_text(color = "darkorange3"),
    axis.text.y.left = element_text(color = "darkorange3"),
    axis.title.y.right = element_text(color = "steelblue4"),
    axis.text.y.right = element_text(color = "steelblue4"),
    legend.position = "right"
  )

# PM2.5 Plot --------------------------------------------------------------

# prepare PM2.5 data
pm25_data <- AQ_All_Years |>
  mutate(
    # add month as variable
    Month = month(Date, label = TRUE, abbr = FALSE),
    # add year as variable
    Year = factor(year(Date))
  ) |>
  # group by month and year
  group_by(Month, Year) |>
  # calculate mean concentration
  summarise(
    mean_conc = mean(Daily_Mean_PM2.5, na.rm = TRUE),
    .groups = "drop"
  )


# lines with mean points
# compare months over years and yearly trends
ggplot(pm25_data, aes(x = Month, y = mean_conc, color = Year, group = Year)) +
  # line for annual trends
  geom_line(linewidth = 1) +
  # point for monthly mean
  geom_point(size = 2) +
  # color by year
  scale_color_manual(values = c("2018" = "violetred3",
                                "2019" = "steelblue4",
                                "2020" = "goldenrod2"))+
  # add titles and labels
  labs(
    title = expression(bold("Particulate Matter (PM"[2.5]*") Concentrations (2018-2020)")),
    subtitle = "Monthly Average",
    x = "Month",
    y = expression("PM"[2.5]*" Concentration (ug/m"^3*")"),
    caption = "Note: 2020 data is only through April"
  ) +
  # set theme
  theme_minimal() +
  # finalize formatting
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

