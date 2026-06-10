library(conflicted)
library(tidyverse)
library(duckplyr)

conflict_prefer("filter", "dplyr")

set.seed(42)

customers <- read_csv("../../data/chapter-2/addresses.csv")
cat(nrow(customers), ncol(customers))
customers |> slice_head(n = 5)

customers |>
  summarise(across(everything(), \(x) sum(is.na(x))))

customers <- drop_na(customers)

summary(customers$total_spend)

cat(customers[[1, "address"]])

for (address in customers$address[1:5]) {
  cat(address, "\n\n")
}

customers <- customers |>
  mutate(address_clean = str_to_upper(address))

customers |>
  filter(str_detect(address_clean, "LONDON")) |>
  nrow()

customers |>
  filter(str_detect(address_clean, "LONDON,")) |>
  nrow()

customers <- customers |>
  mutate(
    address_lines = map_int(str_split(address_clean, "\n"), length)
  )

customers |>
  count(address_lines, sort = TRUE) |>
  arrange(n)

customers |>
  filter(address_lines == 1) |>
  select(address_clean)

customers |>
  filter(address_lines == 2) |>
  slice_sample(n = 5) |>
  select(address_clean)

cities <- read_csv("../../data/chapter-2/cities.csv", col_names = "city")

cities |> slice_head(n = 5)

countries_to_remove <- c("England", "Scotland", "Wales", "Northern Ireland")
cat(nrow(cities))
cities <- cities |>
  filter(city %notin% countries_to_remove)
cat(nrow(cities))

cities <- cities |>
  mutate(
    city = str_replace(city, r"(\*)", "")
  )

cities <- cities |>
  mutate(
    city = str_to_upper(city)
  )
cities |> slice_head(n = 5)

customers <- customers |> mutate(city = "OTHER")
for (cty in cities$city) {
  customers <- customers |>
    mutate(
      city = ifelse(
        str_detect(
          address_clean,
          str_c("\n", cty, ",")
        ),
        cty,
        city
      )
    )
}

customers |> slice_head(n = 5)

customers |>
  count(city, sort = TRUE) |>
  slice_head(n = 20)


sample_other <- customers |>
  filter(city == "OTHER") |>
  slice_sample(n = 5)

for (address in sample_other$address_clean) {
  cat(address, "\n\n")
}

cat(length(unique(customers$city)), "cities in customer data (including OTHER)")
cat(nrow(cities), "cities in city list")

setdiff(cities$city, customers$city)

customers |>
  filter(str_detect(address_clean, "\nHULL,"))

customers <- customers |>
  mutate(
    city = ifelse(str_detect(address_clean, "\nHULL,"), "NULL", city)
  )

top_20_spend <- customers |>
  group_by(city) |>
  summarise(
    total_spend = sum(total_spend)
  ) |>
  arrange(desc(total_spend)) |>
  slice_head(n = 20)

top_20_spend |>
  ggplot(aes(x = total_spend, y = reorder(city, total_spend))) +
  geom_bar(stat = "identity") +
  scale_x_continuous(
    labels = \(x) str_c("£", x / 1.0e6, "M")
  ) +
  labs(
    title = "Total customer spend by city",
    x = "Total spend",
    y = "City",
  )

cat("Total spend for all customers:")
cat(sum(customers$total_spend))

cat("Total spend for London customers:")
cat(customers |> filter(city == "LONDON") |> pull(total_spend) |> sum())

cat("Total spend outside London:")
cat(customers |> filter(city != "LONDON") |> pull(total_spend) |> sum())
cat("Total spend outside London (excluding OTHER):")
cat(customers |> filter(city %notin% c("LONDON", "OTHER")) |> pull(total_spend) |> sum())
