
library(ggplot2)
library(microbenchmark)

setwd('/path/to/working/directory')

#################
## IMPORT
#################
census_df <- read.csv('Data/CHI_CommAreas_Census2010.csv', skip=1, header=TRUE)


#################
## EXTRACT
#################

base_extract_fct <- function() {
  race_df <<- census_df[grep("Geog|Hispanic", names(census_df))]
    
  age_df <<- census_df[grep("Geog|years", names(census_df))]
    
  gender_df <<- census_df[grep("Geog|Total|Male|Female", names(census_df))]
  
  educ_df <<- read.csv('Data/CHI_CommAreas_Educ2010.csv', header=TRUE)
  educ_df <<- educ_df[grep("Geog|Total", names(educ_df))]
    
  hhold_df <<- census_df[grep("Geog|Household", names(census_df))]
    
  housing_units_df <<- census_df[grep("Geog|Total\\.Housing|Occupied|Vacant", names(census_df))]
  housing_units_df$Occupied.Housing.Units.1 <<- NULL 
  
  occupied_housing_df <<- census_df[grep("Geog|Owned|Renter", names(census_df))]
}

#################
## RESHAPE
#################

# RACE
base_reshape_fct <- function() {
  race_long_df <<- reshape(race_df, idvar=c("Geog", "GeogKey"), 
                          varying = c(3:10), v.names="Population",
                          timevar = "Race_Ethnicity", 
                          times = gsub("Not.Hispanic.or.Latino..", "", names(race_df)[c(3:10)]),
                          new.row.names = 1:1000,
                          direction = "long")
  
  # AGE
  age_df[gsub("Male..", "Age.", names(age_df)[grep("Male", names(age_df))])] <<-
    age_df[grep("Male", names(age_df))] + age_df[grep("Female", names(age_df))]
  
  age_long_df <<- reshape(age_df, idvar=c("Geog", "GeogKey"), 
                         varying = c(49:71), v.names="Population",
                         timevar = "Age_Group", 
                         times = names(age_df)[c(49:71)],
                         drop = names(age_df)[c(3:48)],
                         new.row.names = 1:2000,
                         direction = "long")
  
  # GENDER
  gender_df$Male_Total <<- apply(gender_df[grep("Male", names(gender_df))], 1, sum)
  gender_df$Female_Total <<- apply(gender_df[grep("Female", names(gender_df))], 1, sum)
  
  all.equal(gender_df$Male_Total + gender_df$Female_Total, gender_df$Total.Population)
  
  gender_long_df <<- reshape(gender_df, idvar=c("Geog", "GeogKey"), 
                            varying = c("Male_Total", "Female_Total"), v.names="Population",
                            timevar = "Gender", 
                            times = c("Male", "Female"),
                            drop = names(gender_df)[c(3:49)],
                            new.row.names = 1:2000,
                            direction = "long")
  
  
  # EDUCATION
  educ_long_df <<- reshape(educ_df, idvar=c("Geog", "GeogKey"), 
                            varying = c(4:8), v.names="Population",
                            timevar = "Education_Level", 
                            times = gsub("Total.", "", names(educ_df)[4:8]),
                            drop = names(educ_df)[3],
                            new.row.names = 1:2000,
                            direction = "long")
  
  # HOUSEHOLD
  hhold_long_df <<- hhold_df
  
  
  # HOUSING UNITS
  housing_units_long_df <<- reshape(housing_units_df, idvar=c("Geog", "GeogKey"), 
                                    varying = c("Occupied.Housing.Units", "Vacant.Housing.Units"), 
                                    v.names="Housing_Units",
                                    timevar = "Occupied_Status", 
                                    times = c("Occupied", "Vacant"),
                                    new.row.names = 1:2000,
                                    direction = "long")
  
  # OCCUPIED HOUSING
  occupied_housing_long_df <<- reshape(occupied_housing_df, idvar=c("Geog", "GeogKey"), 
                                      varying = c("Owned.free.and.clear", "Owned.with.a.mortgage.or.a.loan", "Renter.occupied"), 
                                      v.names="Housing_Units",
                                      timevar = "Occupied_Type", 
                                      times = c("Own", "Mortgage or Loan", "Rent"),
                                      new.row.names = 1:2000,
                                      direction = "long")
}

#################
## SUMMARIZE
#################

base_summarize_fct <- function() {
  # COMM AREA
  comm_aggdf <- with(census_df,
                          data.frame(mean_hhold = mean(Total.Population),
                                     median_hhold = median(Total.Population),
                                     min_hhold = min(Total.Population),
                                     max_hhold = max(Total.Population),
                                     total_hhold = sum(Total.Population)))               

  # RACE
  dfs <- sapply(c("mean", "median", "min", "max", "sum"), function(f) 
    setNames(aggregate(Population~Race_Ethnicity, race_long_df, FUN=f),
             c("race_ethnicity", paste(f, "population", sep="_"))), simplify = FALSE)
  
  race_aggdf <- Reduce(function(x,y) merge(x, y, by="race_ethnicity"), dfs)

  # AGE
  dfs <- sapply(c("mean", "median", "min", "max", "sum"), function(f) 
       setNames(aggregate(Population~Age_Group, age_long_df, FUN=f),
                          c("age_group", paste(f, "population", sep="_"))), simplify = FALSE)
  
  age_aggdf <- Reduce(function(x,y) merge(x, y, by="age_group"), dfs)

  # GENDER
  dfs <- sapply(c("mean", "median", "min", "max", "sum"), function(f) 
    setNames(aggregate(Population~Gender, gender_long_df, FUN=f),
             c("gender", paste(f, "households", sep="_"))), simplify = FALSE)
  
  gender_aggdf <- Reduce(function(x,y) merge(x, y, by="gender"), dfs)

  # EDUCATION
  dfs <- sapply(c("mean", "median", "min", "max", "sum"), function(f) 
    setNames(aggregate(Population~Education_Level,educ_long_df, FUN=f),
             c("education_level", paste(f, "population", sep="_"))), simplify = FALSE)
  
  educ_aggdf <- Reduce(function(x,y) merge(x, y, by="education_level"), dfs)
  
  # HOUSEHOLD
  hhold_aggdf <- with(hhold_long_df,
                      data.frame(mean_hhold = mean(Total.Households),
                                 median_hhold = median(Total.Households),
                                 min_hhold = min(Total.Households),
                                 max_hhold = max(Total.Households),
                                 total_hhold = sum(Total.Households)))     
  
  # HOUSING UNITS
  dfs <- sapply(c("mean", "median", "min", "max", "sum"), function(f) 
    setNames(aggregate(Housing_Units~Occupied_Status, housing_units_long_df, FUN=f),
             c("occupied_status", paste(f, "units", sep="_"))), simplify = FALSE)
  
  housing_aggdf <- Reduce(function(x,y) merge(x, y, by="occupied_status"), dfs)
  
  # OCCUPIED HOUSING
  dfs <- sapply(c("mean", "median", "min", "max", "sum"), function(f) 
    setNames(aggregate(Housing_Units~Occupied_Type, occupied_housing_long_df, FUN=f),
             c("occupied_type", paste(f, "units", sep="_"))), simplify = FALSE)
  
  occupied_aggdf <- Reduce(function(x,y) merge(x, y, by="occupied_type"), dfs)
  
  list(comm_aggdf, race_aggdf, age_aggdf, gender_aggdf, educ_aggdf, hhold_aggdf, housing_aggdf, occupied_aggdf)
}

#################
## GRAPHING
#################

base_graphing_fct <- function() {
  
    # COMM AREA
    census_df <- with(census_df, census_df[order(-Total.Population),])
    
    comm_plot <- ggplot(head(census_df, n=10), aes(x=Geog, y=Total.Population, fill=factor(Geog))) + 
      geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
      guides(fill=FALSE) +
      labs(title="Top Ten Chicago CAs - Top Ten Population", 
           y="Total Population", x="Chicago Community Areas") +
      theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + 
      scale_fill_hue(l=45)
  
    # RACE
    race_long_df <- with(race_long_df, race_long_df[order(Race_Ethnicity, -Population),])
    
    race_plots <- by(race_long_df, race_long_df$Race_Ethnicity, FUN=function(df) {
    
      df <- head(df, n=10)
      grp <- df$Race_Ethnicity[1]
             
      ggplot(df, aes(x=Geog, y=Population, fill=factor(Geog))) + 
             geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
             guides(fill=guide_legend(title="Geog", ncol=15)) +
             labs(title=gsub("\\.", " ", paste("Top Ten Chicago Community Area ", grp, sep=" - ")), 
                  y="Population", x="Chicago Community Areas") +
             theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + 
             scale_fill_hue(l=45)
    })
    
    # AGE
    age_long_df <- with(age_long_df, age_long_df[order(Age_Group, -Population),])
    
    age_plots <- by(age_long_df, age_long_df$Age_Group, FUN=function(df) {
      
      df <- head(df, n=10)
      grp <- df$Age_Group[1]
      
      ggplot(df, aes(x=Geog, y=Population, fill=factor(Geog))) + 
        geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
        guides(fill=FALSE) +
        labs(title=gsub("\\.", " ", paste("Top Ten Chicago CAs ", grp, sep=" - ")), 
             y="Population", x="Chicago Community Areas") +
        theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + 
        scale_fill_hue(l=45)
    })
    
    # GENDER
    gender_long_df <- with(gender_long_df, gender_long_df[order(Gender, -Population),])
    
    gender_plots <- by(gender_long_df, gender_long_df$Gender, FUN=function(df) {
      
      df <- head(df, n=10)
      grp <- df$Gender[1]
      
      ggplot(df, aes(x=Geog, y=Population, fill=factor(Geog))) + 
        geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
        guides(fill=FALSE) +
        labs(title=gsub("\\.", " ", paste("Top Ten Chicago CAs ", grp, sep=" - ")), 
             y="Population", x="Chicago Community Areas") +
        theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + 
        scale_fill_hue(l=45)
    })
    
    # EDUCATION
    educ_long_df <- with(educ_long_df, educ_long_df[order(Education_Level, -Population),])
    
    educ_plots <- by(educ_long_df, educ_long_df$Education_Level, FUN=function(df) {
      
      df <- head(df, n=10)
      grp <- df$Education_Level[1]
      
      ggplot(df, aes(x=Geog, y=Population, fill=factor(Geog))) + 
        geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
        guides(fill=FALSE) +
        labs(title=gsub("\\.", " ", paste("Top Ten Chicago CAs ", grp, sep=" - ")), 
             y="Population", x="Chicago Community Areas") +
        theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + 
        scale_fill_hue(l=45)
    })
    
    # HOUSEHOLDS
    hhold_long_df <- with(hhold_long_df, hhold_long_df[order(-Total.Households),])
    
    hhold_plot <- ggplot(head(hhold_long_df, n=10), aes(x=Geog, y=Total.Households, fill=factor(Geog))) + 
      geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
      guides(fill=FALSE) +
      labs(title="Top Ten Chicago CAs - Top Ten Households", 
           y="Total Households", x="Chicago Community Areas") +
      theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + 
      scale_fill_hue(l=45)
    
    # HOUSING UNITS
    housing_units_long_df <- with(housing_units_long_df, 
                                  housing_units_long_df[order(Occupied_Status, -Housing_Units),])
    
    housing_plots <- by(housing_units_long_df, housing_units_long_df$Occupied_Status, FUN=function(df) {
      
      df <- head(df, n=10)
      grp <- df$Occupied_Status[1]
      
      ggplot(df, aes(x=Geog, y=Housing_Units, fill=factor(Geog))) + 
        geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
        guides(fill=FALSE) +
        labs(title=gsub("\\.", " ", paste("Top Ten Chicago CAs ", grp, sep=" - ")), 
             y="Housing Units", x="Chicago Community Areas") +
        theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + 
        scale_fill_hue(l=45)
    })
    
    # OCCUPIED HOUSING
    occupied_housing_long_df <- with(occupied_housing_long_df, 
                                     occupied_housing_long_df[order(Occupied_Type, -Housing_Units),])
    
    occupied_plots <- by(occupied_housing_long_df, occupied_housing_long_df$Occupied_Type, FUN=function(df) {
      
      df <- head(df, n=10)
      grp <- df$Occupied_Type[1]
      
      ggplot(df, aes(x=Geog, y=Housing_Units, fill=factor(Geog))) + 
        geom_bar(stat="identity", position = position_dodge(width = 0.9)) +
        guides(fill=FALSE) +
        labs(title=gsub("\\.", " ", paste("Top Ten Chicago CAs ", grp, sep=" - ")), 
             y="Housing Units", x="Chicago Community Areas") +
        theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + 
        scale_fill_hue(l=45)
    })
    
    list(comm_plot, race_plots, age_plots, gender_plots, educ_plots, hhold_plot, housing_plots, occupied_plots)
}

