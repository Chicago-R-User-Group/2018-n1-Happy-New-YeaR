
library(jsonlite)

# https://www.meetup.com/meetup_api/docs/2/members/

setwd('/path/to/working/directory')

paths <- c("Data/CRUG_members1.json",
           "Data/CRUG_members2.json",
           "Data/CRUG_members3.json",
           "Data/CRUG_members4.json",
           "Data/CRUG_members5.json",
           "Data/CRUG_members6.json",
           "Data/CRUG_members7.json",
           "Data/CRUG_members8.json",
           "Data/CRUG_members9.json",
           "Data/CRUG_members10.json",
           "Data/CRUG_members11.json",
           "Data/CRUG_members12.json",
           "Data/CRUG_members13.json",
           "Data/CRUG_members14.json",
           "Data/CRUG_members15.json")

##############
### LAPPLY
##############

dfList <- lapply(paths, function(i){
  memberdata <- fromJSON(i)
  temp <- data.frame(id = memberdata$results$id,
                     name = memberdata$results$name,
                     bio = memberdata$results$bio,
                     link = memberdata$results$link,
                     lon = memberdata$results$lon,
                     lat = memberdata$results$lat,
                     city = memberdata$results$city,
                     state = memberdata$results$state,   
                     country = memberdata$results$country,
                     visited = memberdata$results$visited,
                     joined = memberdata$results$joined,
                     status = memberdata$results$status,
                     photo = memberdata$results$photo$highres_link,
                     stringsAsFactors = FALSE)
})

df <- do.call(rbind, dfList)

str(df)

head(df, 10)

# CRUG ORGANIZERS
df[grep("adam g|Justin Shea|Charlotte F|Irena|Paul Teetor|Troy H|Joe Cursio|Gene L", df$name),]

# RSVP EVENT MEMBERS
event_members <- fromJSON("Data/CRUG_eventmembers.json")

event_members_df <- data.frame(
  member_id = event_members$member$id,
  member_name = event_members$member$name,
  host = event_members$member$event_context$host,
  role = event_members$member$role,
  response = event_members$response,
  bio = event_members$member$bio,
  photo = event_members$member$photo$highres_link,
  stringsAsFactors = FALSE
)

head(event_members_df, 10) 

subset(event_members_df, host == TRUE | role == 'organizer' | role == 'coorganizer')

# MERGE FOR EVENT MEMBER INDICATOR
df <- merge(df, event_members_df[c("member_id", "response")], by.x="id", by.y="member_id", all.x=TRUE, stringsAsFactors=FALSE)

str(df)

# OPEN ORGANIZER PHOTO LINKS

# UPDATE JOE C PHOTO
df[df$id == 6498432, "photo"] <- 'https://secure.meetupstatic.com/photos/member/d/0/e/a/member_180473482.jpeg'

tryCatch(sapply(df[grep("adam g|Justin Shea|Charlotte F|Irena|Paul Teetor|Troy H|Joe Cursio|Gene L", df$name), "photo"], 
                browseURL), error = function(e) return(NA))

# OPEN RANDOM 5 EVENT MEMBERS LINKS
tryCatch(sapply(as.character(event_members_df[sample(nrow(event_members_df), 7), "photo"]), 
                browseURL), error = function(e) return(NA))


##############
### APPLY
##############
apply(df[which(df$response == "yes"), c("id", "name", "link", "city", "state", "country", "bio")], 1, 
      function(g) paste(g, collapse=", "))[1:5]


##############
### SAPPLY
##############
df$joined <- as.POSIXct(df$joined/1000, origin="1970-01-01")

df$joined_rank <- sapply(df$joined, function(i) sum(df$joined <= i))

df <- with(df, df[order(joined_rank),])

head(df[,c("name", "joined_rank", "status", "joined")], 10)

head(df[which(df$response == "yes"), c("name", "joined_rank", "status", "joined")], 10)

# CRUG ORGANIZERS
df[grep("adam g|Justin Shea|Charlotte F|Irena|Paul Teetor|Troy H|Joe Cursio|Gene L", df$name),
   c("name", "joined_rank", "status", "joined")]


##############
### VAPPLY
##############
df$visited <- as.POSIXct(df$visited/1000, origin="1970-01-01")

df$visited_rank <- vapply(df$visited, function(i) sum(df$visited >= i), integer(1))

df <- with(df, df[order(visited_rank),])

head(df[which(df$response == "yes"), c("name", "visited_rank", "status", "visited")], 10)

# ORGANIZERS
df[grep('adam g|Justin Shea|Irena|Charlotte F|Paul Teetor|Troy H|Joe Cursio|Gene L', df$name),
   c("name", "joined", "joined_rank", "visited", "visited_rank"),]


##############
### MAPPLY
##############
haversinef <- function(lon1, lat1) {
  R <- 6371 # Earth mean radius [km]
  kmconv <- 0.621371
  
  lat1Radians <- (lat1 / 180) * (4 * atan(1))
  lon1Radians <- (lon1 / 180) * (4 * atan(1))
  lat2Radians <- (41.9484 / 180) * (4 * atan(1))
  lon2Radians <- (-87.6553 / 180) * (4 * atan(1))
  
  AsinBase <- sin(sqrt(sin((lat1Radians - lat2Radians) / 2) ^ 2 + cos(lat1Radians) * cos(lat2Radians) * sin((lon1Radians - lon2Radians) / 2) ^ 2))
  DerivedAsin <- (AsinBase / sqrt(-AsinBase * AsinBase + 1))
  
  return (round(2 * DerivedAsin * (R * kmconv), 2))
  
}

df$WrigleyDistance <- mapply(haversinef, df$lon+0.0068, df$lat+0.0037)

df <- with(df, df[order(df$WrigleyDistance),])

head(df[which(df$response == "yes"), c("name", "lat", "lon", "city", "state", "country", "WrigleyDistance")], 10)

tail(df[, c("name", "lat", "lon", "city", "state", "country", "WrigleyDistance")], 10)

tail(df[which(df$response == "yes"), c("name", "lat", "lon", "city", "state", "country", "WrigleyDistance")], 10)

# ORGANIZERS
df[grep('adam g|Justin Shea|Irena|Charlotte F|Paul Teetor|Troy H|Joe Cursio|Gene L', df$name),
   c("name", "lat", "lon", "city", "state", "country", "WrigleyDistance"),]


##############
### MAP
##############
map_list <- Map(haversinef, df$lon+0.0068, df$lat+0.0037)

str(map_list[1:10])


##############
### TAPPLY
##############
citycount <- tapply(df$id, df$city, length)

citycount[order(-citycount)]

joinedyearcount <- tapply(df$id, format(df$joined, "%Y"), length)

joinedyearcount[order(-joinedyearcount)]


##############
### BY
##############
bylist <- by(df, df$city, FUN=function(i) i)

head(bylist$Chicago, 5)
head(bylist$`New York`, 5)
head(bylist$`San Francisco`, 5)
head(bylist$Seattle, 5)
head(bylist$Milwaukee, 5)


##############
### EAPPLY
##############
str(eapply(.GlobalEnv, length))


##############
### RAPPLY
##############
memberdata <- fromJSON("Data/CRUG_members5.json")

str(memberdata)

memberdata_type <- rapply(memberdata, function(x) typeof(x), how="replace")
                         
str(memberdata_type)
