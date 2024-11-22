names(uscrimedata$State)
length(unique(uscrimedata$State))

# There are 39 states used in this data

sum(uscrimedata$State)
as.factor(uscrimedata$State)

table(uscrimedata$State)

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

length(unique(uscrimedata$Agency))

# "However, we would also be interested in categorizing our results by city, 
# state, and region. For instance, we can group by different regions 
# (Northeast, Southeast, Midwest, Southwest, and West) in the U.S. We can 
# then make comparisons about violent and non violent crimes in each region."

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
uscrimedata <- uscrimedata %>% left_join(region_lookup, by = "State")

# Shows how many times a region appears in the data set
# Although this does not show how many crimes each region has,
# it can lead us to infer which region has the most crime.
table(uscrimedata$Region)

# To avoid getting N/A in the “Violent Crime” or “Property Crime” columns,
# you can create a new column that is the sum of what is considered
# violent crime and property crime and after this, you can get rid of the
# original “Violent Crime” and “Property Crime” columns.
# This will help us avoid having to delete whole rows
# just because they have N/A in that category.

uscrimedata <- uscrimedata %>% mutate("Total Violent Crime" = Murder + Rape + 
                                        Robbery + `Aggravated Assault`)

uscrimedata <- uscrimedata %>% mutate("Total Property Crime" = Burglary + 
                                        Theft + `Motor Vehicle Theft`)


# Inputting the code below will generate tables for each region showing the 
# average amount of violent & property crime each year. There are 3 columns 
# because there are two methods of generating this data. By using the 
# 'Total Violent Crime' column and multiplying it by 12 or using the 
# 'Violent Crime_mvs_12' column (I think that column may be more accurate).

# Average Violent and Property Crime in the West
westviolentcrimebyyear1 <- uscrimedata %>% 
  filter(Region == "West") %>% 
  group_by(Year) %>% 
  summarize(Mean_Violent_Crime_1 = 12*(mean(`Total Violent Crime`, na.rm = TRUE)))
westviolentcrimebyyear2 <- uscrimedata %>% 
  filter(Region == "West") %>% 
  group_by(Year) %>% 
  summarize(Mean_Violent_Crime_2 = mean(`Violent Crime_mvs_12mo`, na.rm = TRUE))
westviolentcrime <- westviolentcrimebyyear1 %>% 
  left_join(westviolentcrimebyyear2, by = "Year")

westpropertycrimebyyear1 <- uscrimedata %>% 
  filter(Region == "West") %>% 
  group_by(Year) %>% 
  summarize(Mean_Property_Crime_1 = 12*(mean(`Total Property Crime`, na.rm = TRUE)))
westpropertycrimebyyear2 <- uscrimedata %>% 
  filter(Region == "West") %>% 
  group_by(Year) %>% 
  summarize(Mean_Property_Crime_2 = mean(`Property Crime_mvs_12mo`, na.rm = TRUE))
westpropertycrime <- westpropertycrimebyyear1 %>% 
  left_join(westpropertycrimebyyear2, by = "Year")

westcrimemean <- westviolentcrime %>% left_join(westpropertycrime, by = "Year")
View(westcrimemean)

# Southwest
swviolentcrimebyyear1 <- uscrimedata %>% 
  filter(Region == "Southwest") %>% 
  group_by(Year) %>% 
  summarize(Mean_Violent_Crime_1 = 12*(mean(`Total Violent Crime`, na.rm = TRUE)))
swviolentcrimebyyear2 <- uscrimedata %>% 
  filter(Region == "Southwest") %>% 
  group_by(Year) %>% 
  summarize(Mean_Violent_Crime_2 = mean(`Violent Crime_mvs_12mo`, na.rm = TRUE))
swviolentcrime <- swviolentcrimebyyear1 %>% 
  left_join(swviolentcrimebyyear2, by = "Year")

swpropertycrimebyyear1 <- uscrimedata %>% 
  filter(Region == "Southwest") %>% 
  group_by(Year) %>% 
  summarize(Mean_Property_Crime_1 = 12*(mean(`Total Property Crime`, na.rm = TRUE)))
swpropertycrimebyyear2 <- uscrimedata %>% 
  filter(Region == "Southwest") %>% 
  group_by(Year) %>% 
  summarize(Mean_Property_Crime_2 = mean(`Property Crime_mvs_12mo`, na.rm = TRUE))
swpropertycrime <- swpropertycrimebyyear1 %>% 
  left_join(swpropertycrimebyyear2, by = "Year")

swcrimemean <- swviolentcrime %>% left_join(swpropertycrime, by = "Year")
View(swcrimemean)

# Midwest
mwviolentcrimebyyear1 <- uscrimedata %>% 
  filter(Region == "Midwest") %>% 
  group_by(Year) %>% 
  summarize(Mean_Violent_Crime_1 = 12*(mean(`Total Violent Crime`, na.rm = TRUE)))
mwviolentcrimebyyear2 <- uscrimedata %>% 
  filter(Region == "Midwest") %>% 
  group_by(Year) %>% 
  summarize(Mean_Violent_Crime_2 = mean(`Violent Crime_mvs_12mo`, na.rm = TRUE))
mwviolentcrime <- mwviolentcrimebyyear1 %>% 
  left_join(mwviolentcrimebyyear2, by = "Year")

mwpropertycrimebyyear1 <- uscrimedata %>% 
  filter(Region == "Midwest") %>% 
  group_by(Year) %>% 
  summarize(Mean_Property_Crime_1 = 12*(mean(`Total Property Crime`, na.rm = TRUE)))
mwpropertycrimebyyear2 <- uscrimedata %>% 
  filter(Region == "Midwest") %>% 
  group_by(Year) %>% 
  summarize(Mean_Property_Crime_2 = mean(`Property Crime_mvs_12mo`, na.rm = TRUE))
mwpropertycrime <- mwpropertycrimebyyear1 %>% 
  left_join(mwpropertycrimebyyear2, by = "Year")

mwcrimemean <- mwviolentcrime %>% left_join(mwpropertycrime, by = "Year")
View(mwcrimemean)

# Southeast
seviolentcrimebyyear1 <- uscrimedata %>% 
  filter(Region == "Southeast") %>% 
  group_by(Year) %>% 
  summarize(Mean_Violent_Crime_1 = 12*(mean(`Total Violent Crime`, na.rm = TRUE)))
seviolentcrimebyyear2 <- uscrimedata %>% 
  filter(Region == "Southeast") %>% 
  group_by(Year) %>% 
  summarize(Mean_Violent_Crime_2 = mean(`Violent Crime_mvs_12mo`, na.rm = TRUE))
seviolentcrime <- seviolentcrimebyyear1 %>% 
  left_join(seviolentcrimebyyear2, by = "Year")

sepropertycrimebyyear1 <- uscrimedata %>% 
  filter(Region == "Southeast") %>% 
  group_by(Year) %>% 
  summarize(Mean_Property_Crime_1 = 12*(mean(`Total Property Crime`, na.rm = TRUE)))
sepropertycrimebyyear2 <- uscrimedata %>% 
  filter(Region == "Southeast") %>% 
  group_by(Year) %>% 
  summarize(Mean_Property_Crime_2 = mean(`Property Crime_mvs_12mo`, na.rm = TRUE))
sepropertycrime <- sepropertycrimebyyear1 %>% 
  left_join(sepropertycrimebyyear2, by = "Year")

secrimemean <- seviolentcrime %>% left_join(sepropertycrime, by = "Year")
View(secrimemean)

# Northeast
neviolentcrimebyyear1 <- uscrimedata %>% 
  filter(Region == "Northeast") %>% 
  group_by(Year) %>% 
  summarize(Mean_Violent_Crime = 12*(mean(`Total Violent Crime`, na.rm = TRUE)))
neviolentcrimebyyear2 <- uscrimedata %>% 
  filter(Region == "Northeast") %>% 
  group_by(Year) %>% 
  summarize(Mean_Violent_Crime = mean(`Violent Crime_mvs_12mo`, na.rm = TRUE))
neviolentcrime <- neviolentcrimebyyear1 %>% 
  left_join(neviolentcrimebyyear2, by = "Year")

nepropertycrimebyyear1 <- uscrimedata %>% 
  filter(Region == "Northeast") %>% 
  group_by(Year) %>% 
  summarize(Mean_Property_Crime_1 = 12*(mean(`Total Property Crime`, na.rm = TRUE)))
nepropertycrimebyyear2 <- uscrimedata %>% 
  filter(Region == "Northeast") %>% 
  group_by(Year) %>% 
  summarize(Mean_Property_Crime_2 = mean(`Property Crime_mvs_12mo`, na.rm = TRUE))
nepropertycrime <- nepropertycrimebyyear1 %>% 
  left_join(nepropertycrimebyyear2, by = "Year")

necrimemean <- neviolentcrime %>% left_join(nepropertycrime, by = "Year")
View(necrimemean)

# ^^ This will unfortunately take up a lot in your environment T-T

# But by using this, you can see how crime differs throughout the years.
# You can compare the values starting from 2018 to 2021.

# Categorizing which city has the most or least crime can be difficult because 
# there are so many cities. But if I can find a way to visualize that, I'll put 
# it here.