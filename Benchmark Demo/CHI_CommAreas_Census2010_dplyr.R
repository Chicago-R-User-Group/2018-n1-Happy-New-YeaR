
library(dplyr)
library(tidyr)

library(microbenchmark)
library(ggplot2)

setwd('/path/to/working/directory')

#################
## IMPORT
#################
census_df <- read.csv('Data/CHI_CommAreas_Census2010.csv', skip=1, header=TRUE)


#################
## EXTRACT
#################

dplyr_extract_fct <- function() {
  race_df <<- census_df %>% select(., matches("Geog|Hispanic"))
  
  age_df <<- census_df %>% select(., matches("Geog|years"))
  
  gender_df <<- census_df %>% select(., matches("Geog|Population|Male|Female"))
  
  educ_df <<- read.csv('Data/CHI_CommAreas_Educ2010.csv', header=TRUE)
  
  hholds_df <<- census_df %>% select(., matches("Geog|Households"))
  
  housing_df <<- census_df %>% select(., matches("Geog|Total\\.Housing|Occupied|Vacant"))
  
  occupied_df <<- census_df %>% select(., matches("Geog|Owned|Renter"))
}

#################
## RESHAPE
#################

dplyr_reshape_fct <- function() {
  # RACE
  race_long_df <<- race_df %>% gather(., key="Race_Ethnicity", value="Population", -Geog, -GeogKey)
  
  # AGE
  age_df[gsub("Male..", "Age.", names(age_df)[grep("Male", names(age_df))])] <<- select(age_df, matches("^Male")) +
    select(age_df, matches("Female"))
  
  age_long_df <<- age_df %>% 
    select(., matches("Geog|^Age")) %>%
    gather(., key="Age_Group", value="Population", -Geog, -GeogKey)
  
  # GENDER
  gender_df$male <<- rowSums(select(select(gender_df, matches("Geog|(^Male)")), -Geog, -GeogKey))
  gender_df$female <<- rowSums(select(select(gender_df, matches("Geog|Female")), -Geog, -GeogKey))
  
  gender_long_df <<- gender_df %>% 
    select(., matches("Geog|(^male)|female", ignore.case=FALSE)) %>%
    gather(., key="Gender", value="Population", -Geog, -GeogKey)
  
  # EDUCATION
  educ_long_df <<- educ_df %>% 
    select(., matches("Geog|Total")) %>%
    gather(., key="Education_Level", value="Population", -Geog, -GeogKey)
  
  # HOUSEHOLD
  hholds_long_df <<-hholds_df
  
  # HOUSING UNITS
  housing_long_df <<- housing_df %>%
    select(-Occupied.Housing.Units.1) %>%
    gather(., key="Occupied_Status", value="Housing_Units", -Geog, -GeogKey)
  
  # OCCUPIED HOUSING
  occupied_long_df <<- occupied_df %>%
    gather(., key="Occupied_Type", value="Housing_Units", -Geog, -GeogKey)                             
}

#################
## SUMMARIZE
#################

dplyr_summarize_fct <- function() {
  # COMM AREA
  comm_aggdf <-  census_df %>%
    select(Total.Population) %>%
    summarise(mean_population = mean(Total.Population),
              median_population = median(Total.Population),
              min_population = min(Total.Population),
              max_population = max(Total.Population),
              total_population = sum(Total.Population))
  # RACE
  race_aggdf <- race_long_df %>%
    select(Race_Ethnicity, Population) %>%
    group_by(Race_Ethnicity) %>%
    summarise(mean_population = mean(Population),
              median_population = median(Population),
              min_population = min(Population),
              max_population = max(Population),
              total_population = sum(Population))
  
  # AGE
  age_aggdf <- age_long_df %>%
    select(Age_Group, Population) %>%
    group_by(Age_Group) %>%
    summarise(mean_population = mean(Population),
              median_population = median(Population),
              min_population = min(Population),
              max_population = max(Population),
              total_population = sum(Population))
  
  # GENDER
  gender_aggdf <- gender_long_df %>%
    select(Gender, Population) %>%
    group_by(Gender) %>%
    summarise(mean_population = mean(Population),
              median_population = median(Population),
              min_population = min(Population),
              max_population = max(Population),
              total_population = sum(Population))
  
  # EDUCATION
  educ_aggdf <- educ_long_df %>%
    select(Education_Level, Population) %>%
    group_by(Education_Level) %>%
    summarise(mean_population = mean(Population),
              median_population = median(Population),
              min_population = min(Population),
              max_population = max(Population),
              total_population = sum(Population))
  
  # HOUSEHOLDS
  hhold_aggdf <- hholds_long_df %>%
    summarise(mean_households = mean(Total.Households),
              median_households = median(Total.Households),
              min_households = min(Total.Households),
              max_households = max(Total.Households),
              total_households = sum(Total.Households))
  
  # HOUSING UNITS
  housing_aggdf <- housing_long_df %>%
    select(Occupied_Status, Housing_Units) %>%
    group_by(Occupied_Status) %>%
    summarise(mean_housing_units = mean(Housing_Units),
              median_housing_units = median(Housing_Units),
              min_housing_units = min(Housing_Units),
              max_housing_units = max(Housing_Units),
              total_housing_units = sum(Housing_Units))
  
  # OCCUPIED 
  occupied_aggdf <- occupied_long_df %>%
    select(Occupied_Type, Housing_Units) %>%
    group_by(Occupied_Type) %>%
    summarise(mean_housing_units = mean(Housing_Units),
              median_housing_units = median(Housing_Units),
              min_housing_units = min(Housing_Units),
              max_housing_units = max(Housing_Units),
              total_housing_units = sum(Housing_Units))
  
  list(comm_aggdf, race_aggdf, age_aggdf, gender_aggdf, educ_aggdf, hhold_aggdf, housing_aggdf, occupied_aggdf)
}

#################
## GRAPHING
#################

dplyr_graphing_fct <- function() {
  
  # COMM AREA
  comm_plot <- census_df %>%
    top_n(n=10, wt=Total.Population) %>%
    ggplot(aes(x=Geog, y=Total.Population, fill=factor(Geog))) + 
    geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
    guides(fill=FALSE) +
    labs(title="Top Ten Chicago Community Areas - Top Ten Population", 
         y="Total Population", x="Chicago Community Areas") +
    theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + 
    scale_fill_hue(l=45)
  
  # RACE
  plots <- race_long_df %>%
    group_by(Race_Ethnicity) %>%
    top_n(n=10, wt=Population) %>%
    do(plots=ggplot(., aes(x=Geog, y=Population, fill=factor(Geog))) + 
         geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
         guides(fill=guide_legend(title="Geog", ncol=15)) +
         labs(title=gsub("\\.", " ", paste("Top Ten Chicago Community Area ", unique(.$Race_Ethnicity), sep=" - ")), 
              y="Population", x="Chicago Community Areas") +
         theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + 
         scale_fill_hue(l=45))
  race_plots <- plots$plots
  
  # AGE
  plots <- age_long_df %>%
    group_by(Age_Group) %>%
    top_n(n=10, wt=Population) %>%
    do(plots=ggplot(., aes(x=Geog, y=Population, fill=factor(Geog))) + 
         geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
         guides(fill=guide_legend(title="Geog", ncol=15)) +
         labs(title=gsub("\\.", " ", paste("Top Ten Chicago Community Area ", unique(.$Age_Group), sep=" - ")), 
              y="Population", x="Chicago Community Areas") +
         theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + 
         scale_fill_hue(l=45))
  
  age_plots <- plots$plots
  
  # GENDER
  plots <- gender_long_df %>%
    group_by(Gender) %>%
    top_n(n=10, wt=Population) %>%
    do(plots=ggplot(., aes(x=Geog, y=Population, fill=factor(Geog))) + 
         geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
         guides(fill=guide_legend(title="Geog", ncol=15)) +
         labs(title=gsub("\\.", " ", paste("Top Ten Chicago Community Area ", unique(.$Gender), sep=" - ")), 
              y="Population", x="Chicago Community Areas") +
         theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + 
         scale_fill_hue(l=45))
  
  gender_plots <- plots$plots
  
  # EDUCATION
  plots <- educ_long_df %>%
    group_by(Education_Level) %>%
    top_n(n=10, wt=Population) %>%
    do(plots=ggplot(., aes(x=Geog, y=Population, fill=factor(Geog))) + 
         geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
         guides(fill=guide_legend(title="Geog", ncol=15)) +
         labs(title=gsub("\\.", " ", paste("Top Ten Chicago Community Area ", unique(.$Education_Level), sep=" - ")), 
              y="Population", x="Chicago Community Areas") +
         theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + 
         scale_fill_hue(l=45))
  
  educ_plots <- plots$plots
  
  # HOUSEHOLDS
  hhold_plot <- hholds_long_df %>%
    top_n(n=10, wt=Total.Households) %>%
    ggplot(aes(x=Geog, y=Total.Households, fill=factor(Geog))) + 
    geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
    guides(fill=FALSE) +
    labs(title="Top Ten Chicago Community Areas - Top Ten Households", 
         y="Total Households", x="Chicago Community Areas") +
    theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + 
    scale_fill_hue(l=45)
  
  # HOUSING
  plots <- housing_long_df %>%
    group_by(Occupied_Status) %>%
    top_n(n=10, wt=Housing_Units) %>%
    do(plots=ggplot(., aes(x=Geog, y=Housing_Units, fill=factor(Geog))) + 
         geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
         guides(fill=guide_legend(title="Geog", ncol=15)) +
         labs(title=gsub("\\.", " ", paste("Top Ten Chicago Community Area ", unique(.$Occupied_Status), sep=" - ")), 
              y="Housing_Units", x="Chicago Community Areas") +
         theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + 
         scale_fill_hue(l=45))
  
  housing_plots <- plots$plots
  
  # OCCUPIED HOUSING
  plots <- occupied_long_df %>%
    group_by(Occupied_Type) %>%
    top_n(n=10, wt=Housing_Units) %>%
    do(plots=ggplot(., aes(x=Geog, y=Housing_Units, fill=factor(Geog))) + 
         geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
         guides(fill=guide_legend(title="Geog", ncol=15)) +
         labs(title=gsub("\\.", " ", paste("Top Ten Chicago Community Area ", unique(.$Occupied_Type), sep=" - ")), 
              y="Housing_Units", x="Chicago Community Areas") +
         theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + 
         scale_fill_hue(l=45))
  
  occupied_plots <- plots$plots
  
  list(comm_plot, race_plots, age_plots, gender_plots, educ_plots, hhold_plot, housing_plots, occupied_plots)
}

