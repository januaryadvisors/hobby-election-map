library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(sf)
library(geojsonio)
library(rmapshaper)
library(sp)

wgs84<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

#2020 data
data2020 <- readRDS(here::here("data", "data-2020-clean.rds"))

#Election data
df <- readxl::read_excel(here::here("data", "Updated Precinct Voting Data for Harris County.xlsx")) %>% 
  dplyr::select(-c(VAP_Anglo:`...13`)) %>% 
  mutate(
    DB_Dem = ifelse(DB_Dem==".", NA, as.numeric(DB_Dem)),
    DB_Rep = ifelse(DB_Rep==".", NA, as.numeric(DB_Rep)),
    `Sen/Pre_Dem` = ifelse(`Sen/Pre_Dem`==".", NA, as.numeric(`Sen/Pre_Dem`)),
    `Sen/Pre_Rep` = ifelse(`Sen/Pre_Rep`==".", NA, as.numeric(`Sen/Pre_Rep`))
  ) %>% 
  filter(Year!="2020") %>% 
  rbind(., data2020) %>% 
  mutate(
    DB_Dem_pct = ifelse(DB_Dem==0 & DB_Rep==0, NA, DB_Dem/(DB_Dem+DB_Rep)*100) ,
    DB_Rep_pct = ifelse(DB_Dem==0 & DB_Rep==0, NA, DB_Rep/(DB_Dem+DB_Rep)*100),
    `Sen/Pre_Dem_pct` = ifelse(`Sen/Pre_Dem`==0 & `Sen/Pre_Rep`==0, NA, `Sen/Pre_Dem`/(`Sen/Pre_Dem`+`Sen/Pre_Rep`)*100),
    `Sen/Pre_Rep_pct` = ifelse(`Sen/Pre_Dem`==0 & `Sen/Pre_Rep`==0, NA, `Sen/Pre_Rep`/(`Sen/Pre_Dem`+`Sen/Pre_Rep`)*100),
    
    db_diff = DB_Dem_pct - DB_Rep_pct,
    sp_diff = `Sen/Pre_Dem_pct` - `Sen/Pre_Rep_pct`,
    #year_array = (Year - 2000)/2,
    
    db_description = case_when(
      db_diff>0 ~ paste0("Democrats +", round(abs(db_diff)), " percentage points"),
      db_diff<0 ~ paste0("Republicans +", round(abs(db_diff)), " percentage points"),
      db_diff==0 ~ paste0("Equal numbers of Democrats and Republicans")
    ),
    
    sp_description = case_when(
      sp_diff>0 ~ paste0("Democrats +", round(abs(sp_diff)), " percentage points"),
      sp_diff<0 ~ paste0("Republicans +", round(abs(sp_diff)), " percentage points"),
      sp_diff==0 ~ paste0("Equal numbers of Democrats and Republicans")
    )
  ) %>% 
  dplyr::select(Pct, Year, db_diff, sp_diff, db_description, sp_description) %>% 
  gather(election, values, db_diff:sp_description) %>% 
  separate(election, into=c("election", "name"), sep="_") %>% 
  filter(!is.na(values)) %>% 
  ungroup() %>% 
  spread(name, values) %>% 
  mutate(
    diff=as.numeric(diff)
    ) %>% 
  arrange(Pct, election, Year) %>% 
  group_by(Pct, election) %>% 
  mutate(
    year_array = (Year - 2000)/2,
    #Different for presidential years
    year_array = case_when(
      Year==2000 & election=="sp" ~ 0,
      Year==2004 & election=="sp" ~ 1,
      Year==2008 & election=="sp" ~ 2,
      Year==2012 & election=="sp" ~ 3,
      Year==2016 & election=="sp" ~ 4,
      Year==2018 & election=="sp" ~ 5,
      Year==2020 & election=="sp" ~ 6,
      TRUE ~ year_array
    )
  )

  

#Precinct shapefile
sf <- read_sf(here::here("data", "Harris_County_Voting_Precincts.shp")) %>% 
  st_as_sf() %>% st_transform(., wgs84) %>% 
  mutate(Pct = as.numeric(PRECINCT)) %>% 
  dplyr::select(Pct, geometry)

#Merge and create main geojson
combined <- left_join(sf, df, by="Pct") %>% 
  geojson_json(.) %>% ms_simplify(.) 

geojson_write(combined, file = here::here("data", paste0("mapdata", ".json")))
geojson_write(combined, file = here::here(paste0("mapdata", ".json")))


#Create flyTo list w/ ID and centroids
centroids <- st_centroid(sf) %>%
  arrange(Pct) %>% 
  mutate(
    lon = unlist(map(geometry,1)),
    lat = unlist(map(geometry,2)),
    description = paste0("<strong>Precinct ", Pct, "</strong>")
  ) %>% 
  st_drop_geometry() %>% 
  write_csv(here::here("precinct_centroids.csv"))





