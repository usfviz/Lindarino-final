rm(list = ls())
cat('\014')

if (!require("tidyr")) install.packages("tidyr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("lubridate")) install.packages("lubridate")


aviation_data <- read.table("AviationData.txt",
                            header = TRUE, sep = "|", stringsAsFactors = FALSE,
                            blank.lines.skip = TRUE, quote = "", strip.white = TRUE,
                            comment.char = "")

lat_long_data <- read.csv("zip_codes_states.csv", header = TRUE, stringsAsFactors = FALSE)
lat_long_city_data <- lat_long_data %>%
  group_by(city, state) %>%
  summarise(latitude2 = mean(latitude), longitude2 = mean(longitude))
lat_long_city_data$city <- tolower(lat_long_city_data$city)

aviation_US <- aviation_data %>% filter(Country == "United States", Location != "") %>% select(-X)

States <- c("AL", "AK", "AZ", "AR", "CA",
            "CO", "CT", "DE", "FL", "GA",
            "HI", "ID", "IL", "IN", "IA",
            "KS", "KY", "LA", "ME", "MD",
            "MA", "MI", "MN", "MS", "MO",
            "MT", "NE", "NV", "NH", "NJ",
            "NM", "NY", "NC", "ND", "OH",
            "OK", "OR", "PA", "RI", "SC",
            "SD", "TN", "TX", "UT", "VT",
            "VA", "WA", "WV", "WI", "WY")

Full_States <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California",
                 "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
                 "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",
                 "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
                 "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
                 "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
                 "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
                 "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                 "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
                 "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

State_name_mapping <- data.frame(State_abbr = States, State_full_name = Full_States)

aviation_US$state <- sapply(aviation_US$Location, function(x) strsplit(x, split = ', ')[[1]][2])
aviation_US$city <- sapply(aviation_US$Location, function(x) tolower(strsplit(x, split = ', ')[[1]][1]))
aviation_US$state[aviation_US$state == "NYC"] <- "NY"
aviation_US <- aviation_US %>% filter(state %in% States)
aviation_US <- left_join(aviation_US, lat_long_city_data, by = c("city", "state"))



aviation_US$Event.Date <- as.Date(aviation_US$Event.Date, format = "%m/%d/%Y")
aviation_US$Publication.Date <- as.Date(aviation_US$Publication.Date, format = "%m/%d/%Y")
aviation_US$year <- year(aviation_US$Event.Date)

# write.csv(aviation_US, file = "/Users/lawrencebarrett/Documents/School/msan622/Final_Project/aviation_US.csv",
#           row.names = FALSE, fileEncoding = "utf-8")

aviation_long <- aviation_US %>% 
  select(state, year, Total.Fatal.Injuries, Total.Serious.Injuries, Total.Minor.Injuries, Total.Uninjured) %>% 
  filter(year >= 1982, year <= 2016) %>% 
  group_by(state, year) %>% 
  summarise(Total_Fatal_Injuries = sum(Total.Fatal.Injuries, na.rm = T),
            Total_Fatal_Injuries_Per_Incident = sum(Total.Fatal.Injuries, na.rm = T)/n(),
            Total_Serious_Injuries = sum(Total.Serious.Injuries, na.rm = T),
            Total_Serious_Injuries_Per_Incident = sum(Total.Serious.Injuries, na.rm = T)/n(),
            Total_Minor_Injuries = sum(Total.Minor.Injuries, na.rm = T),
            Total_Minor_Injuries_Per_Incident = sum(Total.Minor.Injuries, na.rm = T)/n(),
            Total_Uninjured = sum(Total.Uninjured, na.rm = T),
            Total_Uninjured_Per_Incident = sum(Total.Uninjured, na.rm = T)/n(),
            Total_Injuries = Total_Fatal_Injuries + Total_Serious_Injuries + Total_Minor_Injuries,
            Total_Injuries_Per_Incident = (Total_Fatal_Injuries + Total_Serious_Injuries + Total_Minor_Injuries)/n())

aviation_long$Total_Fatal_Injuries <- as.numeric(aviation_long$Total_Fatal_Injuries)
aviation_long$Total_Serious_Injuries <- as.numeric(aviation_long$Total_Serious_Injuries)
aviation_long$Total_Minor_Injuries <- as.numeric(aviation_long$Total_Minor_Injuries)
aviation_long$Total_Uninjured <- as.numeric(aviation_long$Total_Uninjured)
aviation_long$Total_Injuries <- as.numeric(aviation_long$Total_Injuries)

aviation_totals_per_state <- aviation_long %>% 
  group_by(state) %>% 
  summarise(Total_Fatal_Injuries = sum(Total_Fatal_Injuries),
            Total_Serious_Injuries = sum(Total_Serious_Injuries),
            Total_Minor_Injuries = sum(Total_Serious_Injuries),
            Total_Injuries = sum(Total_Injuries))

states <- geojsonio::geojson_read("us-states.json", what = "sp")


