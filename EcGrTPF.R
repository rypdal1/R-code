library(dplyr)
library(tidyr)
head(GDP.Per.Capita.17.22)
colnames(GDP.Per.Capita.17.22) <- c("Country", "Country Code", "Series Name", "Series Code", "2017", "2018", "2019", "2020", "2021", "2022")
head(GDP.Per.Capita.17.22)
gdp_data_fixed <- GDP.Per.Capita.17.22 %>%
  select("Country", "2017", "2018", "2019", "2020", "2021", "2022")
str(gdp_data_fixed)

gdp_data_fixed$`2022` <- as.numeric(gdp_data_fixed$`2022`)
str(gdp_data_fixed)

gdp_long <- gdp_data_fixed %>%
  pivot_longer(cols = '2017':'2022',
               names_to = "Year", 
               values_to = "GDP")

fifth_richest_poorest <- gdp_long %>%
  group_by(Year) %>%
  summarise(
    Fifth_Richest = Country[order(-GDP)][5],   
    Fifth_Poorest = Country[order(GDP)][5]      
  )

gdp_ratios <- data.frame(Year = character(), gdp_Ratio = numeric(), stringsAsFactors = FALSE)


for (i in 1:nrow(fifth_richest_poorest)) {
  
  year <- fifth_richest_poorest$Year[i]
  
  
  richest_country <- fifth_richest_poorest$Fifth_Richest[i]
  poorest_country <- fifth_richest_poorest$Fifth_Poorest[i]
  
  
  richest_gdp <- gdp_long$GDP[gdp_long$Country == richest_country & gdp_long$Year == year]
  poorest_gdp <- gdp_long$GDP[gdp_long$Country == poorest_country & gdp_long$Year == year]
  
  
  if (length(richest_gdp) > 0 && length(poorest_gdp) > 0) {
    ratio <- richest_gdp / poorest_gdp
  } else {
    ratio <- NA  
  }
  
  
  gdp_ratios <- rbind(gdp_ratios, data.frame(Year = year, gdp_Ratio = ratio))
}


print(gdp_ratios)

library(ggplot2)

ggplot(gdp_ratios, aes(x = Year, y = gdp_Ratio)) +
  geom_line(aes(group = 1), color = "blue") +  
  geom_point(color = "black", size = 3) +  
  labs(title = "GDP Ratio: 5th Richest vs. 5th Poorest Countries (2017-2022)",
       x = "Year",
       y = "GDP Ratio (5th Richest / 5th Poorest)") +
  theme_minimal()  
