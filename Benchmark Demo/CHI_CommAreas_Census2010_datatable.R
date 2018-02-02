
library(data.table)

library(ggplot2)
library(microbenchmark)

setwd('/path/to/working/directory')

#################
## IMPORT
#################
census_dt <- fread(file='Data/CHI_CommAreas_Census2010.csv', sep=",", header=TRUE, skip=1)

#################
## EXTRACT
#################

dtable_extract_fct <- function() {
  
  race_dt <<- census_dt[, .SD, .SDcols = names(census_dt) %like% "Geog|Hispanic"]
  
  age_dt <<- census_dt[, .SD, .SDcols = names(census_dt) %like% "Geog|years"]
  
  gender_dt <<- census_dt[, .SD, .SDcols = names(census_dt) %like% "Geog|Population|Male|Female"]
  
  educ_dt <<- fread(file='Data/CHI_CommAreas_Educ2010.csv', sep=",", header=TRUE)
    
  hholds_dt <<- census_dt[, .SD, .SDcols = names(census_dt) %like% "Geog|Households"]
  
  housing_dt <<- census_dt[, .SD, .SDcols = names(census_dt) %like% "Geog|Total.Housing|Occupied|Vacant"]
  housing_dt <<- housing_dt[,unique(names(housing_dt)),with=FALSE]
  
  occupied_dt <<- census_dt[, .SD, .SDcols = names(census_dt) %like% "Geog|Owned|Renter"]
}


#################
## RESHAPE
#################

dtable_reshape_fct <- function() {
  
  # RACE
  race_long_dt <<- melt(race_dt, id.vars = c("Geog", "GeogKey"), 
                       variable.name = "Race_Ethnicity", value.name = "Population")
  
  # AGE
  age_dt[, gsub("Male", "Age", names(age_dt)[names(age_dt) %like% "Male"])] <<- 
    age_dt[, .SD, .SDcols = names(age_dt) %like% "Male"] +
    age_dt[, .SD, .SDcols = names(age_dt) %like% "Female"]
  
  age_dt <<- age_dt[, .SD, .SDcols = names(age_dt) %like% "Geog|Age"]
  
  age_long_dt <<- melt(age_dt, id.vars = c("Geog", "GeogKey"), 
                      variable.name = "Age_Group", value.name = "Population")
  
  # GENDER
  gender_dt <<- gender_dt[ ,male := rowSums(.SD), .SDcols = names(gender_dt) %like% "^Male"]
  gender_dt <<- gender_dt[ ,female := rowSums(.SD), .SDcols = names(gender_dt) %like% "Female"]
  
  all.equal(gender_dt$`Total Population`, gender_dt$male + gender_dt$female)
  
  gender_long_dt <<- melt(gender_dt[, .SD, .SDcols=names(gender_dt) %like% "Geog|^male|female"], 
                         id.vars = c("Geog", "GeogKey"), 
                         variable.name = "Gender", value.name = "Population")
  
  # EDUCATION
  educ_long_dt <<- melt(educ_dt[,.SD,.SDcols=names(educ_dt) %like% "Geog|^Total"], 
                       id.vars = c("Geog", "GeogKey"), 
                       variable.name = "Education_Level", value.name = "Population")
  
  ## HOUSEHOLD
  hholds_long_dt <<- hholds_dt
  
  ## HOUSING UNITS
  housing_long_dt <<- melt(housing_dt, id.vars = c("Geog", "GeogKey"),
                          variable.name = "Occupied_Status", value.name = "Housing_Units")
  
  ## OCCUPIED HOUSING
  occupied_long_dt <<- melt(occupied_dt, id.vars = c("Geog", "GeogKey"),
                           variable.name = "Occupied_Type", value.name = "Housing_Units")
}

#################
## SUMMARIZE
#################

dtable_summarize_fct <- function() {
  
  # COMM AREA
  comm_aggdt <- census_dt[, list(mean_population = mean(`Total Population`),
                                        median_population = median(`Total Population`),
                                        min_population = min(`Total Population`),
                                        max_population = max(`Total Population`),
                                        total_population = sum(`Total Population`)), ]
  
  # RACE
  setkey(race_long_dt, Race_Ethnicity)
  race_aggdt <- race_long_dt[, list(mean_popluation = mean(Population),
                                    median_population = median(Population),
                                    min_population = min(Population),
                                    max_population = max(Population),
                                    total_popluation = sum(Population)), by = Race_Ethnicity]
  
  # AGE
  setkey(age_long_dt, Age_Group)
  age_aggdt <- age_long_dt[, list(mean_popluation = mean(Population),
                                  median_population = median(Population),
                                  min_population = min(Population),
                                  max_population = max(Population),
                                  total_popluation = sum(Population)), by = Age_Group]
  
  # GENDER
  setkey(gender_long_dt, Gender)
  gender_aggdt <- gender_long_dt[, list(mean_popluation = mean(Population),
                                        median_population = median(Population),
                                        min_population = min(Population),
                                        max_population = max(Population),
                                        total_popluation = sum(Population)), by = Gender]
  
  # EDUCATION
  setkey(educ_long_dt, Education_Level)
  educ_aggdt <- educ_long_dt[, list(mean_popluation = mean(Population),
                                    median_population = median(Population),
                                    min_population = min(Population),
                                    max_population = max(Population),
                                    total_popluation = sum(Population)), by = Education_Level]
  
  # HOUSEHOLDS
  hholds_aggdt <- hholds_long_dt[, list(mean_households = mean(`Total Households`),
                                        median_households = median(`Total Households`),
                                        min_households = min(`Total Households`),
                                        max_households = max(`Total Households`),
                                        total_households = sum(`Total Households`)), ]
  
  # HOUSING
  setkey(housing_long_dt, Occupied_Status)
  housing_aggdt <- housing_long_dt[, list(mean_housing_units = mean(Housing_Units),
                                          median_housing_units = median(Housing_Units),
                                          min_housing_units = min(Housing_Units),
                                          max_housing_units = max(Housing_Units),
                                          total_housing_units = sum(Housing_Units)), by = Occupied_Status]

  # OCCUPIED
  setkey(occupied_long_dt, Occupied_Type)
  
  occupied_aggdt <- occupied_long_dt[, list(mean_housing_units = mean(Housing_Units),
                                            median_housing_units = median(Housing_Units),
                                            min_housing_units = min(Housing_Units),
                                            max_housing_units = max(Housing_Units),
                                            total_housing_units = sum(Housing_Units)), by = Occupied_Type]

  list(comm_aggdt, race_aggdt, age_aggdt, gender_aggdt, educ_aggdt, hholds_aggdt, housing_aggdt, occupied_aggdt)
}


#################
## GRAPHING
#################

dtable_graphing_fct <- function() {
  
  # COMM AREA
  setorder(census_dt, -`Total Population`)
  
  comm_plot <- ggplot(head(census_dt, 10), aes(x=Geog, y=`Total Population`, fill=factor(Geog))) + 
    geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
    guides(fill=guide_legend(title="Geog", ncol=15)) +
    labs(title="Top Ten Chicago Community Area - Total Population", 
         y="Total Population", x="Chicago Community Areas") +
    theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + 
    scale_fill_hue(l=45)

  # RACE
  setkey(race_long_dt, Race_Ethnicity)
  setorder(race_long_dt, Race_Ethnicity, -Population)
  
  grp_dt <- race_long_dt[, .SD[Population %in% head(Population, 10)], by = Race_Ethnicity]

  race_plots <- lapply(split(grp_dt, grp_dt$Race_Ethnicity), function(dt){
    grp <- dt$Race_Ethnicity[1]
    
    ggplot(dt, aes(x=Geog, y=Population, fill=factor(Geog))) + 
      geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
      guides(fill=guide_legend(title="Geog", ncol=15)) +
      labs(title=gsub("\\.", " ", paste("Top Ten Chicago Community Area ", grp, sep=" - ")), 
           y="Population", x="Chicago Community Areas") +
      theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + 
      scale_fill_hue(l=45)
  })
  
  # AGE
  setkey(age_long_dt, Age_Group)
  setorder(age_long_dt, Age_Group, -Population)
  
  grp_dt <- age_long_dt[, .SD[Population %in% head(Population, 10)], by = Age_Group]

  age_plots <- lapply(split(grp_dt, grp_dt$Age_Group), function(dt){
    grp <- dt$Age_Group[1]
    
    ggplot(dt, aes(x=Geog, y=Population, fill=factor(Geog))) + 
      geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
      guides(fill=guide_legend(title="Geog", ncol=15)) +
      labs(title=gsub("\\.", " ", paste("Top Ten Chicago Community Area ", grp, sep=" - ")), 
           y="Population", x="Chicago Community Areas") +
      theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + 
      scale_fill_hue(l=45)
  })
  
  # GENDER
  setkey(gender_long_dt, Gender)
  setorder(gender_long_dt, Gender, -Population)
  
  grp_dt <- gender_long_dt[, .SD[Population %in% head(Population, 10)], by = Gender]

  gender_plots <- lapply(split(grp_dt, grp_dt$Gender), function(dt){
    grp <- dt$Gender[1]
    
    ggplot(dt, aes(x=Geog, y=Population, fill=factor(Geog))) + 
      geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
      guides(fill=guide_legend(title="Geog", ncol=15)) +
      labs(title=gsub("\\.", " ", paste("Top Ten Chicago Community Area ", grp, sep=" - ")), 
           y="Population", x="Chicago Community Areas") +
      theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + 
      scale_fill_hue(l=45)
  })
  
  # EDUCATION
  setkey(educ_long_dt, Education_Level)
  setorder(educ_long_dt, Education_Level, -Population)
  
  grp_dt <- educ_long_dt[, .SD[Population %in% head(Population, 10)], by = Education_Level]

  educ_plots <- lapply(split(grp_dt, grp_dt$Education_Level), function(dt){
    grp <- dt$Education_Level[1]
    
    ggplot(dt, aes(x=Geog, y=Population, fill=factor(Geog))) + 
      geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
      guides(fill=guide_legend(title="Geog", ncol=15)) +
      labs(title=gsub("\\.", " ", paste("Top Ten Chicago Community Area ", grp, sep=" - ")), 
           y="Population", x="Chicago Community Areas") +
      theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + 
      scale_fill_hue(l=45)
  })
  
  # HOUSEHOLDS
  setorder(hholds_long_dt, -`Total Households`)
  
  hhold_plot <- ggplot(head(hholds_long_dt, 10), aes(x=Geog, y=`Total Households`, fill=factor(Geog))) + 
    geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
    guides(fill=guide_legend(title="Geog", ncol=15)) +
    labs(title="Top Ten Chicago Community Area - Total Households", 
         y="Total Households", x="Chicago Community Areas") +
    theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + 
    scale_fill_hue(l=45)
  
  # HOUSING UNITS
  setkey(housing_long_dt, Occupied_Status)
  setorder(housing_long_dt, Occupied_Status, -Housing_Units)
  
  grp_dt <- housing_long_dt[, .SD[Housing_Units %in% head(Housing_Units, 10)], by = Occupied_Status]

  housing_plots <- lapply(split(grp_dt, grp_dt$Occupied_Status), function(dt){
    grp <- dt$Occupied_Status[1]
    
    ggplot(dt, aes(x=Geog, y=Housing_Units, fill=factor(Geog))) + 
      geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
      guides(fill=guide_legend(title="Geog", ncol=15)) +
      labs(title=gsub("\\.", " ", paste("Top Ten Chicago Community Area ", grp, sep=" - ")), 
           y="Housing Units", x="Chicago Community Areas") +
      theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + 
      scale_fill_hue(l=45)
  })
  
  # OCCUPIED HOUSING
  setkey(occupied_long_dt, Occupied_Type)
  setorder(occupied_long_dt, Occupied_Type, -Housing_Units)
  
  grp_dt <- occupied_long_dt[, .SD[Housing_Units %in% head(Housing_Units, 10)], by = Occupied_Type]
  
  occupied_plots <- lapply(split(grp_dt, grp_dt$Occupied_Type), function(dt){
    grp <- dt$Occupied_Type[1]
    
    ggplot(dt, aes(x=Geog, y=Housing_Units, fill=factor(Geog))) + 
      geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
      guides(fill=guide_legend(title="Geog", ncol=15)) +
      labs(title=gsub("\\.", " ", paste("Top Ten Chicago Community Area ", grp, sep=" - ")), 
           y="Housing Units", x="Chicago Community Areas") +
      theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + 
      scale_fill_hue(l=45)
  })
  
  list(comm_plot, race_plots, age_plots, gender_plots, educ_plots, hhold_plot, housing_plots, occupied_plots)
}

