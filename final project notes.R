library(dplyr)
library(ggplot2)
library(readr)
final_sample <- read_csv('final_sample')


# Selects all rows up to "Property Crime" variable and removes rest
cleaned_data <- final_sample %>%
  select(1:which(names(final_sample) == "Property Crime"))%>%
  filter(if_all(everything(), ~ . >= 0))
dim(final_sample)

#Finding each NA value left and cleaning
colSums(is.na(cleaned_data))

# (Alternatively) To avoid getting N/A in the “Violent Crime” or 
# “Property Crime” columns, you can create a new column that is the sum of what 
# is considered violent crime and property crime and after this, you can get 
# rid of the original “Violent Crime” and “Property Crime” columns.
# This will help us avoid having to delete whole rows
# just because they have N/A in that category.
# cleaned_data <- final_sample %>% mutate("Total Violent Crime" = Murder + Rape + 
#                                         Robbery + `Aggravated Assault`)
# cleaned_data <- cleaned_data %>% mutate("Total Property Crime" = Burglary + 
#                                        Theft + `Motor Vehicle Theft`)
# However, in this case, we will be using the first method of getting rid rows
# with N/A values entirely.


# Moving on to organizing the states by region.
names(cleaned_data$State)
length(unique(cleaned_data$State))

# There are 39 states (including Nationwide) used in this data

as.factor(cleaned_data$State)

table(cleaned_data$State)

# This shows how many times a state shows up in the data. 
# This shows that are actually 38 states used in the data since "Nationwide" 
# isn't a state.
# We can use this to organize the states into regions

# WEST ~> OR, HI, WA, CA, ID, CO, WY, NV, UT (9)
# SOUTHWEST ~> AZ, TX (2)
# MIDWEST ~> SD, NE, MN, MO, WI, IL, IN, MI, OH (9)
# SOUTHEAST ~> AR, LA, MS, TN, KY, VA, NC, FL, GA (9)
# NORTHEAST ~> PA, MD, DC, NJ, NY, CT, MA, NH, RI (9)
# NATIONWIDE ~> Nationwide (1)

length(unique(cleaned_data$Agency))

# Creates a data frame that matches states to their regions 
# (there was probably a faster way to do this but it’s whatever)
region_lookup <- data.frame(State = c("OR", "HI", "WA", "CA", "ID", "CO", "WY", 
                                      "NV", "UT", "AZ", "TX", "SD", "NE", "MN", 
                                      "MO", "WI", "IL", "IN", "MI", "OH", "AR", 
                                      "LA", "MS", "TN", "KY", "VA", "NC", "FL", 
                                      "GA", "PA", "MD", "DC", "NJ", "NY", "CT", 
                                      "MA", "NH", "RI"), 
                            Region = c(rep("West", 9), rep("Southwest", 2), 
                                       rep("Midwest", 9), rep("Southeast", 9), 
                                       rep("Northeast", 9))
)

# This adds a region column to the data (after you input the code above)
cleaned_data <- cleaned_data %>% left_join(region_lookup, by = "State")

# Shows how many times a region appears in the data set
# Although this does not show how many crimes each region has,
# it can lead us to infer which region has the most crime.
table(cleaned_data$Region)


# For the sake of the data visualizations, we're going to get rid of the
# Nationwide row.
cleaned_data <- cleaned_data[cleaned_data$Region != "N/A", ]

# Average Violent Crime
monthly_mean_violent_crime <- cleaned_data %>% 
  group_by(Region) %>% 
  summarise(monthly_mean_violent_crime = mean(`Violent Crime`, na.rm = FALSE))
yearly_mean_violent_crime <- cleaned_data %>% 
  group_by(Region) %>% 
  summarise(yearly_mean_violent_crime = 12*(mean(`Violent Crime`, na.rm = FALSE)))
# Violent crime by region data visualizations
ggplot(monthly_mean_violent_crime, aes(x = Region, 
                                       y = monthly_mean_violent_crime,
                                       fill = Region)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Monthly Mean Violent Crime by Region",
    x = "Region",
    y = "Mean Violent Crime"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(yearly_mean_violent_crime, aes(x = Region, y = yearly_mean_violent_crime, 
                                      fill = Region)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Yearly Mean Violent Crime by Region",
    x = "Region",
    y = "Mean Violent Crime"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Average Property Crime
monthly_mean_property_crime <- cleaned_data %>% 
  group_by(Region) %>% 
  summarise(monthly_mean_property_crime = mean(`Property Crime`, na.rm = FALSE))
yearly_mean_property_crime <- cleaned_data %>% 
  group_by(Region) %>% 
  summarise(yearly_mean_property_crime = 12*(mean(`Property Crime`, na.rm = FALSE)))
# Property Crime by region data visualization
ggplot(monthly_mean_property_crime, aes(x = Region, 
                                       y = monthly_mean_property_crime, 
                                       fill = Region)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Monthly Mean Property Crime by Region",
    x = "Region",
    y = "Mean Property Crime"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(yearly_mean_property_crime, aes(x = Region, 
                                      y = yearly_mean_property_crime, 
                                      fill = Region)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Yearly Mean Property Crime by Region",
    x = "Region",
    y = "Mean Property Crime"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Here's how to compare by year
# Violent Crime
mean_violent_crime_by_region_year <- cleaned_data %>%
  group_by(Region, Year) %>%
  summarize(yearly_mean_violent_crime = 12*(mean(`Violent Crime`, na.rm = FALSE)))
ggplot(mean_violent_crime_by_region_year, aes(x = Region, y = yearly_mean_violent_crime, fill = Region)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Mean Violent Crime by Region and Year",
    x = "Region",
    y = "Mean Violent Crime"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~ Year)

# Property Crime
mean_property_crime_by_region_year <- cleaned_data %>%
  group_by(Region, Year) %>%
  summarize(yearly_mean_property_crime = 12*(mean(`Property Crime`, na.rm = FALSE)))
ggplot(mean_property_crime_by_region_year, aes(x = Region, y = yearly_mean_property_crime, fill = Region)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Mean Property Crime by Region and Year",
    x = "Region",
    y = "Mean Property Crime"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~ Year)
