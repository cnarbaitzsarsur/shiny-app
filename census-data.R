
bstand2023 <- read.csv("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/thesis/shiny-app/data/excels/bevolkerungsstand-2023.csv", sep = ";")

bstand2023 <- bstand2023 %>%
  mutate(Goup.ages = as.numeric(sub("GALTEJ112-(\\d+)", "\\1", Goup.ages)))

bstand2023 <- bstand2023 %>%
  mutate(Census.area = as.numeric(sub("GRGEMAKT-(\\d+)", "\\1", Census.area)))

age_intervals <- c(0, 6, 19, 30, 45, 66, Inf)

# Group by district, age group, and summarize the total column
grouped_data <- bstand2023 %>%
  group_by(Census.area, Goup.ages = cut(Goup.ages, breaks = age_intervals)) %>%
  summarise(total_sum = sum(total))

# Create a new data frame with the desired columns
new_data <- data.frame(
  sex = grouped_data$sex,
  district = grouped_data$district,
  age_group = grouped_data$age_group,
  total = grouped_data$total_sum
)

# Print the new data frame
print(new_data)