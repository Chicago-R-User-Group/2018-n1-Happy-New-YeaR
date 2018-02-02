
# https://www.meetup.com/meetup_api/docs/2/members/
# https://www.meetup.com/meetup_api/docs/2/events/

setwd("/path/to/working/directory")

# DEFINING INPUTS 
api_key = "********************"        # RETRIEVE AT meetup.com/meetup_api/key/

offsets <- seq(0, 14)
paths <- c("CRUG_members1.json", "CRUG_members2.json", "CRUG_members3.json",
           "CRUG_members4.json", "CRUG_members5.json", "CRUG_members6.json",
           "CRUG_members7.json", "CRUG_members8.json", "CRUG_members9.json",
           "CRUG_members10.json", "CRUG_members11.json", "CRUG_members12.json",
           "CRUG_members13.json", "CRUG_members14.json", "CRUG_members15.json")

# MEMBERS DATA
get_meetup_json <- function(i, j) {
 download.file(paste0("https://api.meetup.com/2/members?offset=", i, "&format=json&group_urlname=",
                      "ChicagoRUG&key=", api_key, "&photo-host=public&page=500"), j)
}

# DOWNLOAD MEMERS DATA ITERATIVELY BY OFFSET
Map(get_meetup_json, offsets, paths)

# EVENT MEMBERS DATA
download.file(paste0("https://api.meetup.com/ChicagoRUG/events/245872598/rsvps?photo-host=",
                     "public&key=", api_key), "CRUG_eventmembers.json")


