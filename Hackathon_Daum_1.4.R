# Load required libraries
library(legislatoR)
library(tidyverse)
library(gt)
library(gtExtras)

# Step 1: Fetch data for Scottish legislature
sco_core <- get_core(legislature = "sco")
sco_political <- get_political(legislature = "sco")
sco_traffic <- get_traffic(legislature = "sco")

# Step 2: Filter for the 6th session
sco_political_6 <- sco_political %>%
  filter(session == 6) %>%
  select(pageid, party)

# Step 3: Join core and political data
sco_msp_6 <- sco_core %>%
  inner_join(sco_political_6, by = "pageid") %>%
  select(pageid, name, party)

# Step 4: Filter traffic data after May 13, 2021
sco_traffic_filtered <- sco_traffic %>%
  filter(pageid %in% sco_msp_6$pageid, as.Date(date) > as.Date("2021-05-13"))

# Step 5: Summarize traffic data
traffic_summary <- sco_traffic_filtered %>%
  group_by(pageid) %>%
  summarize(
    mean_traffic = mean(traffic, na.rm = TRUE),
    sd_traffic = sd(traffic, na.rm = TRUE)
  )

# Step 6: Create histograms for each MSP with binwidth = 1000
traffic_histograms <- sco_traffic_filtered %>%
  mutate(bin = cut(traffic, breaks = seq(0, max(traffic, na.rm = TRUE) + 1000, by = 1000))) %>%
  group_by(pageid, bin) %>%
  summarize(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = bin, values_from = count, values_fill = 0) %>%
  rowwise() %>%
  mutate(
    histogram = list(as.numeric(c_across(where(is.numeric))))  # Combine counts into a list
  ) %>%
  ungroup() %>%
  select(pageid, histogram)  # Select only relevant columns

# Step 7: Combine data into final table
final_data_with_hist <- sco_msp_6 %>%
  inner_join(traffic_summary, by = "pageid") %>%
  left_join(traffic_histograms, by = "pageid") %>%
  arrange(desc(mean_traffic)) %>%
  head(20)

# Step 8: Generate final table with barplots for traffic distribution
final_table <- final_data_with_hist %>%
  gt() %>%
  tab_header(
    title = "Top 20 Most Popular Scottish MSPs of the 6th Session",
    subtitle = "Based on Wikipedia Page Traffic After May 13, 2021"
  ) %>%
  cols_label(
    name = "MSP Name",
    party = "Party",
    mean_traffic = "Mean Traffic",
    sd_traffic = "Traffic SD",
    histogram = "Traffic Distribution"
  ) %>%
  data_color(
    columns = c(mean_traffic),
    fn = scales::col_numeric(
      palette = "Blues",
      domain = NULL
    )
  ) %>%
  gt_plt_bar(
    column = histogram,   # Use the histogram list column
    scaled = TRUE,        # Scales the bars proportionally
    fill = "purple"       # Bar color
  )

# Step 9: Display the final table
final_table
