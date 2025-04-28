######cleaning#######
# load libraries
library (readstata13)
library (readr)
library (dplyr)
library (stats)
library (visdat)
library (car)
library(ggplot2)
library(tidyverse)
library (tidyr)
library(showtext)
library(extrafont)
library(ggtext)
library(sf)
library(viridis)
library(RColorBrewer)
library(ggspatial)
library(leaflet)
library(plotly)
# read data

forest_data <- read_csv("Forestdata.csv")
# only select relevant variables

forest_data <- forest_data %>%
  select("Country Name", "1990 [YR1990]", "1991 [YR1991]", "1992 [YR1992]", 
         "1993 [YR1993]", "1994 [YR1994]", "1995 [YR1995]", "1996 [YR1996]", "1997 [YR1997]", 
         "1998 [YR1998]", "1999 [YR1999]", "2000 [YR2000]", "2001 [YR2001]", "2002 [YR2002]",
         "2003 [YR2003]", "2004 [YR2004]", "2005 [YR2005]", "2006 [YR2006]", "2007 [YR2007]",
         "2008 [YR2008]", "2009 [YR2009]", "2010 [YR2010]", "2011 [YR2011]", "2012 [YR2012]",
         "2013 [YR2013]", "2014 [YR2014]", "2015 [YR2015]", "2016 [YR2016]", "2017 [YR2017]",
         "2018 [YR2018]", "2019 [YR2019]", "2020 [YR2020]", "2021 [YR2021]")
# rename years

forest_data <- forest_data %>%
  rename("1990" = "1990 [YR1990]",
         "1991" = "1991 [YR1991]",
         "1992" = "1992 [YR1992]",
         "1993" = "1993 [YR1993]",
         "1994" = "1994 [YR1994]",
         "1995" = "1995 [YR1995]",
         "1996" = "1996 [YR1996]",
         "1997" = "1997 [YR1997]", 
         "1998" = "1998 [YR1998]", 
         "1999" = "1999 [YR1999]", 
         "2000" = "2000 [YR2000]",
         "2001" = "2001 [YR2001]",
         "2002" = "2002 [YR2002]",
         "2003" = "2003 [YR2003]", 
         "2004" = "2004 [YR2004]", 
         "2005" = "2005 [YR2005]",
         "2006" = "2006 [YR2006]",
         "2007" = "2007 [YR2007]",
         "2008" = "2008 [YR2008]",
         "2009" = "2009 [YR2009]",
         "2010" = "2010 [YR2010]", 
         "2011" = "2011 [YR2011]",
         "2012" = "2012 [YR2012]",
         "2013" = "2013 [YR2013]",
         "2014" = "2014 [YR2014]",
         "2015" = "2015 [YR2015]",
         "2016" = "2016 [YR2016]",
         "2017" = "2017 [YR2017]",
         "2018" = "2018 [YR2018]",
         "2019" = "2019 [YR2019]",
         "2020" = "2020 [YR2020]",
         "2021" = "2021 [YR2021]")

# remove missing values from variables

forest_data <-  forest_data %>% 
  filter(!is.na(`Country Name`))
forest_data <-  forest_data %>% 
  filter(!is.na(`1990`))
forest_data <-  forest_data %>% 
  filter(!is.na(`1992`))
forest_data <-  forest_data %>% 
  filter(!is.na(`1993`))
forest_data <-  forest_data %>% 
  filter(!is.na(`1994`))
forest_data <-  forest_data %>% 
  filter(!is.na(`1995`))
forest_data <-  forest_data %>% 
  filter(!is.na(`1996`))
forest_data <-  forest_data %>% 
  filter(!is.na(`1997`))
forest_data <-  forest_data %>% 
  filter(!is.na(`1998`))
forest_data <-  forest_data %>% 
  filter(!is.na(`1999`))
forest_data <-  forest_data %>% 
  filter(!is.na(`2000`))
forest_data <-  forest_data %>% 
  filter(!is.na(`2001`))
forest_data <-  forest_data %>% 
  filter(!is.na(`2002`))
forest_data <-  forest_data %>% 
  filter(!is.na(`2003`))
forest_data <-  forest_data %>% 
  filter(!is.na(`2004`))
forest_data <-  forest_data %>% 
  filter(!is.na(`2005`))
forest_data <-  forest_data %>% 
  filter(!is.na(`2006`))
forest_data <-  forest_data %>% 
  filter(!is.na(`2007`))
forest_data <-  forest_data %>% 
  filter(!is.na(`2008`))
forest_data <-  forest_data %>% 
  filter(!is.na(`2009`))
forest_data <-  forest_data %>% 
  filter(!is.na(`2010`))
forest_data <-  forest_data %>% 
  filter(!is.na(`2011`))
forest_data <-  forest_data %>% 
  filter(!is.na(`2012`))
forest_data <-  forest_data %>% 
  filter(!is.na(`2013`))
forest_data <-  forest_data %>% 
  filter(!is.na(`2014`))
forest_data <-  forest_data %>% 
  filter(!is.na(`2015`))
forest_data <-  forest_data %>% 
  filter(!is.na(`2016`))
forest_data <-  forest_data %>% 
  filter(!is.na(`2017`))
forest_data <-  forest_data %>% 
  filter(!is.na(`2018`))
forest_data <-  forest_data %>% 
  filter(!is.na(`2019`))
forest_data <-  forest_data %>% 
  filter(!is.na(`2020`))
forest_data <-  forest_data %>% 
  filter(!is.na(`2021`))
#check missing variable percentage

vis_miss(forest_data)
# filter country name variable to only countries, no territories/regions/world data

forest_data <- forest_data %>%
  filter(`Country Name` == "Afghanistan" |
           `Country Name` == "Albania" |
           `Country Name` == "Algeria" |
           `Country Name` == "Andorra" |
           `Country Name` == "Angola" |
           `Country Name` == "Antigua and Barbuda" |
           `Country Name` == "Argentina" |
           `Country Name` ==  "Armenia" |
           `Country Name` == "Australia" |
           `Country Name` == "Austria" |
           `Country Name` == "Azerbaijan" |
           `Country Name` == "Bahamas, The" |
           `Country Name` == "Bahrain" |
           `Country Name` == "Bangladesh" |
           `Country Name` == "Barbados" |
           `Country Name` == "Belarus" |
           `Country Name` == "Belgium" |
           `Country Name` == "Belize" |
           `Country Name` == "Benin" |
           `Country Name` == "Bhutan" |
           `Country Name` == "Bolivia" |
           `Country Name` == "Bosnia and Herzegovina" |
           `Country Name` == "Botswana" |
           `Country Name` == "Brazil" |
           `Country Name` == "Brunei Darussalam" |
           `Country Name` == "Bulgaria" |
           `Country Name` == "Burkina Faso" |
           `Country Name` == "Burundi" |
           `Country Name` == "Cabo Verde" |
           `Country Name` == "Cambodia" |
           `Country Name` == "Cameroon" |
           `Country Name` == "Canada" |
           `Country Name` == "Central African Republic" |
           `Country Name` == "Chad" |
           `Country Name` == "Chile" |
           `Country Name` == "China" |
           `Country Name` == "Colombia" |
           `Country Name` == "Comoros" |
           `Country Name` == "Congo, Dem. Rep." |
           `Country Name` == "Congo, Rep." |
           `Country Name` == "Costa Rica" |
           `Country Name` == "Cote d'Ivoire" |
           `Country Name` == "Croatia" |
           `Country Name` == "Cuba" |
           `Country Name` == "Cyprus" |
           `Country Name` == "Czechia" |
           `Country Name` == "Denmark" |
           `Country Name` == "Djibouti" |
           `Country Name` == "Dominica" |
           `Country Name` == "Dominican Republic" |
           `Country Name` == "Ecuador" |
           `Country Name` == "Egypt, Arab Rep." |
           `Country Name` == "El Salvador"|
           `Country Name` == "Equatorial Guinea" |
           `Country Name` == "Eritrea" |
           `Country Name` == "Estonia" |
           `Country Name` == "Eswatini" |
           `Country Name` == "Ethiopia" |
           `Country Name` == "Fiji" |
           `Country Name` == "Finland" |
           `Country Name` == "France" |
           `Country Name` == "Gabon" |
           `Country Name` == "Gambia, The" |
           `Country Name` == "Georgia" |
           `Country Name` == "Germany" |
           `Country Name` == "Ghana" |
           `Country Name` == "Greece" |
           `Country Name` == "Grenada" |
           `Country Name` == "Guatemala" |
           `Country Name` == "Guinea" |
           `Country Name` == "Guinea-Bissau" |
           `Country Name` == "Guyana" |
           `Country Name` == "Haiti" |
           `Country Name` == "Honduras" |
           `Country Name` == "Hungary" |
           `Country Name` == "Iceland" |
           `Country Name` == "India" |
           `Country Name` == "Indonesia" |
           `Country Name` == "Iran, Islamic Rep." |
           `Country Name` == "Iraq" |
           `Country Name` == "Ireland" |
           `Country Name` == "Israel" |
           `Country Name` == "Italy" |
           `Country Name` == "Jamaica" |
           `Country Name` == "Japan" |
           `Country Name` == "Jordan" |
           `Country Name` == "Kazakhstan" |
           `Country Name` == "Kenya" |
           `Country Name` == "Kiribati" |
           `Country Name` == "Korea, Dem. People's Rep." |
           `Country Name` == "Korea, Rep." |
           `Country Name` == "Kuwait" |
           `Country Name` == "Kyrgyz Republic" |
           `Country Name` == "Lao PDR" |
           `Country Name` == "Latvia" |
           `Country Name` == "Lebanon" |
           `Country Name` == "Lesotho" |
           `Country Name` == "Liberia" |
           `Country Name` == "Libya" |
           `Country Name` == "Liechtenstein" |
           `Country Name` == "Lithuania" |
           `Country Name` == "Luxembourg" |
           `Country Name` == "Madagascar" |
           `Country Name` == "Malawi" |
           `Country Name` == "Malaysia" |
           `Country Name` == "Maldives" |
           `Country Name` == "Mali" |
           `Country Name` == "Malta" |
           `Country Name` == "Marshall Islands" |
           `Country Name` == "Mauritania" |
           `Country Name` == "Mauritius" |
           `Country Name` == "Mexico" |
           `Country Name` == "Micronesia, Fed. Sts." |
           `Country Name` == "Moldova" |
           `Country Name` == "Monaco" |
           `Country Name` == "Mongolia" |
           `Country Name` == "Montenegro" |
           `Country Name` == "Morocco" |
           `Country Name` == "Mozambique" |
           `Country Name` == "Myanmar" |
           `Country Name` == "Namibia" |
           `Country Name` == "Nauru" |
           `Country Name` == "Nepal" |
           `Country Name` == "Netherlands" |
           `Country Name` == "New Zealand" |
           `Country Name` == "Nicaragua" |
           `Country Name` == "Niger" |
           `Country Name` == "Nigeria" |
           `Country Name` == "North Macedonia" |
           `Country Name` == "Norway" |
           `Country Name` == "Oman" |
           `Country Name` == "Pakistan" |
           `Country Name` == "Palau" |
           `Country Name` == "Panama" |
           `Country Name` == "Papua New Guinea" |
           `Country Name` == "Paraguay" |
           `Country Name` == "Peru" |
           `Country Name` == "Philippines" |
           `Country Name` == "Poland" |
           `Country Name` == "Portugal" |
           `Country Name` == "Qatar" |
           `Country Name` == "Romania" |
           `Country Name` == "Russian Federation" |
           `Country Name` == "Rwanda" |
           `Country Name` == "Samoa" |
           `Country Name` == "San Marino" |
           `Country Name` == "Sao Tome and Principe" |
           `Country Name` == "Saudi Arabia" |
           `Country Name` == "Senegal" |
           `Country Name` == "Serbia" |
           `Country Name` == "Seychelles" |
           `Country Name` == "Sierra Leone"|
           `Country Name` == "Singapore" |
           `Country Name` == "Slovak Republic" |
           `Country Name` == "Slovenia"|
           `Country Name` == "Solomon Islands" |
           `Country Name` == "Somalia" |
           `Country Name` == "South Africa" |
           `Country Name` == "South Sudan" |
           `Country Name` == "Spain" |
           `Country Name` == "Sri Lanka" |
           `Country Name` == "St. Kitts and Nevis" |
           `Country Name` == "St. Lucia" |
           `Country Name` == "St. Vincent and the Grenadines" |
           `Country Name` == "Sudan" |
           `Country Name` == "Suriname" |
           `Country Name` == "Sweden" |
           `Country Name` == "Switzerland" |
           `Country Name` == "Syrian Arab Republic" |
           `Country Name` == "Tajikistan" |
           `Country Name` == "Tanzania" |
           `Country Name` == "Thailand" |
           `Country Name` == "Timor-Leste" |
           `Country Name` == "Togo" |
           `Country Name` == "Tonga" |
           `Country Name` == "Trinidad and Tobago" |
           `Country Name` == "Tunisia" |
           `Country Name` == "Turkiye" |
           `Country Name` == "Turkmenistan" |
           `Country Name` == "Tuvalu" |
           `Country Name` == "Uganda" |
           `Country Name` == "Ukraine" |
           `Country Name` == "United Arab Emirates" |
           `Country Name` == "United Kingdom" |
           `Country Name` == "United States" |
           `Country Name` == "Uruguay" |
           `Country Name` == "Uzbekistan" |
           `Country Name` == "Vanuatu" |
           `Country Name` == "Venezuela, RB" |
           `Country Name` == "Viet Nam" |
           `Country Name` == "Yemen, Rep." |
           `Country Name` == "Zambia" |
           `Country Name` == "Zimbabwe")

# create new year variable and make years values instead of individual columns

forest_table <- forest_data %>% 
  gather(`1990`, `1991`, `1992`, 
         `1993`, `1994`, `1995`, `1996`, `1997`, 
         `1998`, `1999`, `2000`, `2001`, `2002`,
         `2003`, `2004`, `2005`, `2006`, `2007`,
         `2008`, `2009`, `2010`, `2011`, `2012`,
         `2013`, `2014`, `2015`, `2016`, `2017`,
         `2018`, `2019`, `2020`, `2021`, key = "year", value = "% of forested land")
# to deal with missing data - transpose df to use fill function to make any missing values represented by ".." have the most recent value

forest_transposed = t(forest_data)
forest_transposed[forest_transposed == ".."] <- NA
forest_transposed_df <- as.data.frame(forest_transposed)
forest_transposed_full <- forest_transposed_df %>% fill(8, 11, 16, 17, 22, 43, 46, 55, 56, 64, 87, 93, 95, 101, 102, 109, 113, 114, 117, 129, 133, 143, 150, 154, 155, 159, 170, 179, 182, 187, .direction = "up")
# check missing values

any(is.na(forest_transposed_full))
vis_miss(forest_transposed_full)
# transpose to swap rows and collumns

forest_full_table <- t(forest_transposed_full)
# transform matrix to dataframe i

forest_full_df <- as.data.frame(forest_full_table)

######## visualisation 1 ##########
# top 5 countries with the most total deforestation
# convert 1990 and 2021 columns to numeric so that they can be subtracted from each other 

final_forest_table <- forest_full_df %>%
  mutate(`1990` = as.numeric(`1990`), 
         `2021` = as.numeric(`2021`),
         difference = `2021` - `1990`)
# filter to top 5 countries 

top5 <- final_forest_table %>% 
  filter(`Country Name` == "Nicaragua" |
           `Country Name` == "Paraguay" |
           `Country Name` == "Gambia, The" |
           `Country Name` == "Cambodia" |
           `Country Name` == "Indonesia")
# change name of gambia

condition <- "Gambia, The"
replacement <- "Gambia"
top5$`Country Name` <- replace(top5$`Country Name`, top5$`Country Name` %in% condition, replacement)

#gather to have year variable
top5_time <- top5 %>% 
  gather(`1990`, `1991`, `1992`, 
         `1993`, `1994`, `1995`, `1996`, `1997`, 
         `1998`, `1999`, `2000`, `2001`, `2002`,
         `2003`, `2004`, `2005`, `2006`, `2007`,
         `2008`, `2009`, `2010`, `2011`, `2012`,
         `2013`, `2014`, `2015`, `2016`, `2017`,
         `2018`, `2019`, `2020`, `2021`, key = "year", value = "% of forested land")
# change % of forest to numeric
top5_time <- top5_time %>%
  mutate(`% of forested land` = as.numeric(`% of forested land`))
# add fonts

font_add_google("DM Sans", 
                family = "dm_sans")
showtext_auto()

# plot top 5 - line graph

vis1 <- top5_time %>% 
  ggplot() +
  geom_line(aes(x = year, y = `% of forested land`,
                color = `Country Name`,
                group = `Country Name`),
            size = 1) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 100),
                     breaks = seq(0, 100, 10),
                     labels = paste0(seq(0, 100, 10), "%")) +
  geom_text(x = "2021", y = 52,
            label = 'Indonesia',
            hjust = 1, size = 5,
            color = "#03bf7d", family = "dm_sans") +
  geom_text(x = "2017", y = 55,
            label = 'Cambodia',
            hjust = 1, size = 5,
            color = "#f7766d", family = "dm_sans") +
  geom_text(x = "2018", y = 39,
            label = 'Paraguay',
            hjust = 1, size = 5,
            color = "#e66bf3", family = "dm_sans") +
  geom_text(x = "2021", y = 32,
            label = 'Nicaragua',
            hjust = 1, size = 5,
            color = "#02b1f6", family = "dm_sans") +
  geom_text(x = "2021", y = 20,
            label = 'Gambia',
            hjust = 1, size = 5,
            color = "#a3a500", family = "dm_sans") +
  labs(x = "", y = "",
       title = "Top Deforested Countries",
       subtitle = "Trends of forested area as a percentage of total land <br>in 
       <span style = 'color:#f7766d;'> Cambodia, </span> <span style = 'color:#03bf7d;'> Indonesia, </span> 
       <span style = 'color:#e66bf3;'> Paraguay, </span> <span style = 'color:#02b1f6;'> Nicaragua, </span> and 
       <span style = 'color:#a3a500;'> Gambia. </span><br>Globally, these five countries have the greatest decline in forested land percentage") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0,
                                  family = "dm_sans",
                                  face = "bold",
                                  size = 35),
        plot.subtitle = element_markdown(family = "dm_sans",
                                     size = 20),
        axis.text.y = element_text(family = "dm_sans",
                                   size = 15),
        axis.text.x = element_text(family = "dm_sans",
                                   size = 10),
        panel.grid.major.x = element_blank(),
        legend.position = "none")


vis1

#save png
ggsave(file = "SD5813_Portfolio/Vis_1.png",
       dpi = 100,
       height = 7,
       width = 15)
# plot top 5 - bar chart - didn't end up using for the assignment

vis1_top5 <- top5 %>% 
  ggplot () +
  geom_col (aes(y = abs(`difference`),
                x = reorder(`Country Name`, `difference`, decreasing = TRUE)),
            fill = "#2E6F40",
            width = 0.85) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 100),
                     breaks = seq(0, 100, 10),
                     labels = paste0(seq(0, 100, 10), "%")) +
  labs(x = "", y = "",
       title = "Top 5 Deforested Countries",
       subtitle = "Total forest percentage lost from 1990-2021") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0,
                                  family = "dm_sans",
                                  face = "bold",
                                  size = 35),
        plot.subtitle = element_text(hjust = 0,
                                     family = "dm_sans",
                                     size = 25),
        axis.text = element_text(family = "dm_sans",
                                 size = 20),
        panel.grid.major.x = element_blank())


vis1_top5

##### visualisation 2 #####
# map of change in global % of forest with fill that depicts the %

#input shapefile

polygon_sf <- read_sf("world-administrative-boundaries.shp")
# check crs

st_crs(polygon_sf)

# rename polygon data to match forest data

polygon_sf <- polygon_sf %>%
  rename("Country Name" = "name")
# select only relevant variables

polygon_sf <- polygon_sf %>%
  select("Country Name", "geometry")
# rename countries which differ between two datasets

conditions <- c ("Antigua and Barbuda", "Bahamas, The", "Bosnia and Herzegovina", "Cabo Verde", 
                 "Congo, Dem. Rep.", "Congo, Rep.", "Cote d'Ivoire", "Czechia", "Egypt, Arab Rep.", 
                 "Gambia, The", "Iran, Islamic Rep.", "Korea, Dem. People's Rep.", "Korea, Rep.", "Kyrgyz Republic", 
                 "Lao PDR", "Libya", "Micronesia, Fed. Sts.", "Moldova", "North Macedonia", "Slovak Republic", 
                 "St. Kitts and Nevis", "St. Lucia", "St. Vincent and the Grenadines", "Turkiye", "Tanzania", 
                 "United Kingdom", "United States", "Venezuela, RB", "Viet Nam", "Yemen, Rep.")
replacements <- c("Antigua & Barbuda", "Bahamas", "Bosnia & Herzegovina", "Cape Verde", 
                  "Democratic Republic of the Congo", "Congo", "Côte d'Ivoire", 
                  "Czech Republic", "Egypt", "Gambia", "Iran (Islamic Republic of)", 
                  "Democratic People's Republic of Korea", "Republic of Korea", "Kyrgyzstan",
                  "Lao People's Democratic Republic", "Libyan Arab Jamahiriya", "Micronesia (Federated States of)",
                  "Moldova, Republic of", "The former Yugoslav Republic of Macedonia", 
                  "Slovakia", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines",
                  "Turkey", "United Republic of Tanzania", "U.K. of Great Britain and Northern Ireland", "United States of America",
                  "Venezuela", "Vietnam", "Yemen")
final_forest_table$`Country Name` <- replace(final_forest_table$`Country Name`, final_forest_table$`Country Name` %in% conditions, replacements)
# filter relevant forest data

forest_difference <- final_forest_table %>% 
  select("Country Name", "difference")
# merge polygon data with forest data

merged_data <- polygon_sf %>%
  left_join(forest_difference, by = "Country Name")

#check max and min of data, adjust breaks to have a better scale and create diverging colour palette

min(forest_difference$difference)
max(forest_difference$difference)
breaks <- c(-26, -20, -15, -10, -5, 0, 5, 10, 15, 20, 26)
palette <- (RColorBrewer::brewer.pal(length(breaks), "PuOr"))

# plot

vis2 <- ggplot() + 
  geom_sf(data=merged_data,aes(fill = difference)) +
  scale_fill_stepsn(colors = palette,
                    breaks = breaks,
                    limits = c(-26, 26),
                    values = scales::rescale(c(-26, -20, -15, -10, -5, 0, 5, 10, 15, 20, 26))) +
  labs(fill = "Change in percentage of <br> total forest",
       title = "Global Change in Forested Land",
       subtitle = "Percentage of total forest <span style = 'color:#803b08;'> **loss** </span> and <span style = 'color:#2d0a4b;'> **gain** </span> from 1990-2021") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0,
                                  family = "dm_sans",
                                  face = "bold",
                                  size = 35),
        plot.subtitle = element_markdown(hjust = 0,
                                         family = "dm_sans",
                                         size = 25),
        axis.text = element_text(family = "dm_sans",
                                 size = 10),
        legend.title = element_markdown(family = "dm_sans",
                                        size = 20),
        legend.text = element_text(family = "dm_sans",
                                   size = 10))+
  
  annotation_scale(location = "bl",
                   bar_cols = c("black", "white"))+
  annotation_north_arrow(
    location = "bl", 
    pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
    style = ggspatial::north_arrow_nautical(fill = c("black", "white"),
                                            line_col = "black"))
vis2

#save as png file

ggsave(file = "SD5813_Portfolio/Vis_2.png",
       dpi = 100,
       height = 7,
       width = 15)

######visualisation 3 ##########
#interactive difference plot on leaflet
# create colour palette

pal <- colorNumeric(
  palette = "PuOr",
  domain = merged_data$difference)
# remove missing values

merged_data <- merged_data %>% 
  filter(!is.na(`difference`))
# plot

leaflet(data = merged_data) %>%
  addTiles() %>%
  addPolygons(
    stroke = FALSE,
    smoothFactor = 0.2,
    fillOpacity = 0.9,
    color = ~pal(difference),
    label = ~paste0("Country: ", `Country Name`),
    popup = ~paste0(
      "<strong>Country: </strong>", `Country Name`, "<br>",
      "<strong>Change in forest cover (1990–2021): </strong>", round(difference, 2), "%"
    )
  ) %>%
  addLegend(
    pal = pal,
    values = ~difference,
    title = "Percentage of total forest <br> <span style = 'color:#803b08;'> loss </span> and <span style = 'color:#2d0a4b;'> gain </span> from 1990-2021",
    position = "bottomright") %>% 
  addScaleBar("bottomleft")
