library(sf)
library(dplyr)

# set up ----

setwd("~/GIS_projects/nts_output")

avt <- st_read("data/avt_record_agg8.gpkg")

# filter out Dunkeld examples
unique(avt$Site_name)
avt <- avt %>% filter(Site_name != "Dun" & Site_name != "Dunkeld") 

# There is a problem, some surveys have recorded date only and some have recorded datetime only. 
# Make new column extracting date from datetime 
avt <- avt %>% 
  mutate(date2 = as.Date(Time))
# Where Date column is empty, copy in from date2 column
avt <- avt %>% 
  mutate(Date = as.Date(ifelse(is.na(Date), date2, Date)))
# Drop date2 column
avt <- subset(avt, select = -c(date2))
# No nulls
unique(avt$Date)

# Leaflet will want this cast to point definitively
avt <- st_cast(avt, "POINT")

# Don't want records where AVT field is empty
avt <- subset(avt, AVT != "NA")

# Don't want records where species field is empty
avt <- subset(avt, Species != "NA")

# Widest tree known has a girth of about 40m so
big <- avt %>% filter(Girth_m > 40)
summary(big$Girth_m)
big1 <- unique(big$Girth_m)
unique(big$Site_name)
# Looks like all these records from Lomond & Binns were recorded in cm not m
avt$Girth_m[c(big1)]
unique(big$Girth_m)
# yep that's them, divide them by 100
avt$Girth_m <- ifelse(avt$Girth_m > 40, avt$Girth_m/100, avt$Girth_m)
# better
avt$Girth_m[c(big1)]

# Make a new species column ----

avt$Species <- trimws(avt$Species)

#x <- avt %>%
#  group_by(Species) %>%
#  summarise(Count = n()) %>%
#  st_drop_geometry() %>%
#  arrange(desc(Count), by_group = FALSE) %>%
#  print(n = nrow(avt))

avt$Species2 <- ifelse(test = grepl("Q. Robur|Q.robur|Q. ROBUR|Q. robur|Peduncular oak*", avt$Species), yes = "Pedunculate oak", no = avt$Species)
avt$Species2 <- ifelse(test = grepl("Seq. giganteum|Seq.giganteum|Seq. Giganteum", avt$Species), yes = "Giant sequoia", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Ash?", avt$Species), yes = "Ash", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Elm?|*elm?", avt$Species), yes = "Elm", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Be3ch|Bee h|*likely beech", avt$Species), yes = "Beech", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Syc|Sycamre|*likely sycamore|Unclear, sycamore", avt$Species), yes = "Sycamore", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Birch b.pubecent|Downy Birch", avt$Species), yes = "Downy birch", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Ceder", avt$Species), yes = "Cedar", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Cherry?", avt$Species), yes = "Cherry", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Copper beach", avt$Species), yes = "Copper beech", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Eared willow h6brid", avt$Species), yes = "Eared willow", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("L8me|Lime?|Lyme", avt$Species), yes = "Lime", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Holm oak*", avt$Species), yes = "Holm oak", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Goat willow \\(*", avt$Species), yes = "Goat willow", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Horse chestnut*|Horse chesnut*", avt$Species), yes = "Horse chestnut", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Sweet chestnut*|Sweet chesnut*", avt$Species), yes = "Sweet chestnut", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Hawthorne", avt$Species), yes = "Hawthorn", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("*ype B", avt$Species), yes = "Lime", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("M9nkey*", avt$Species), yes = "Monkey puzzle", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Q. Petrea|Sessile Oak", avt$Species), yes = "Sessile oak", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Red wood", avt$Species), yes = "Redwood", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Rowan1|Rown|Rowqn", avt$Species), yes = "Rowan", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Turkey oak?", avt$Species), yes = "Turkey oak", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Eucalyptus gunnii?", avt$Species), yes = "Eucalyptus gunnii", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Ash?", avt$Species), yes = "Ash", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("unsure|Unsure*|Unknown|*Chris*|Unclear", avt$Species), yes = "Unsure", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("*maybe birch", avt$Species), yes = "Birch", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Abies alba?|Abies alba", avt$Species), yes = "Silver fir", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Apple, probable hybrid|Apple = *", avt$Species), yes = "Apple", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Scot pine|Scots Pine|Scots pine 2", avt$Species), yes = "Scots pine", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Oak2.2", avt$Species), yes = "Oak", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Salix alba", avt$Species), yes = "White willow", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Prunus serrulta 'Kanzan'", avt$Species), yes = "Cherry", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Eucalyptus gunnii", avt$Species), yes = "Cider gum", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Common walnut?", avt$Species), yes = "Walnut", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Alnus incana|Grey alder", avt$Species), yes = "Grey alder", no = avt$Species2)
avt$Species2 <- ifelse(test = grepl("Crab", avt$Species), yes = "Crab apple", no = avt$Species2)


#sp2 <- avt %>%
#  group_by(Species2) %>%
#  summarise(Count = n()) %>%
#  st_drop_geometry() %>%
#  arrange(desc(Count), by_group = FALSE) %>%
#  print(n = nrow(avt))

# Keep the original data in another column, and rename the clean data as Species
avt$species_original <- avt$Species

avt$Species <- avt$Species2

avt <- select(avt, -c(Species2))

# Clean up some columns
avt$Epiphytes <- str_replace_all(avt$Epiphytes, '\\{|\\}|"', '')
avt$Epiphytes <- str_replace_all(avt$Epiphytes, ',', ', ')

avt$Evidence_of <- str_replace_all(avt$Evidence_of, '\\{|\\}|"', '')
avt$Evidence_of <- str_replace_all(avt$Evidence_of, ',', ', ')

avt$Decaying_wood <- str_replace_all(avt$Decaying_wood, '\\{|\\}|"', '')
avt$Decaying_wood <- str_replace_all(avt$Decaying_wood, ',', ', ')

avt$Root_threat <- str_replace_all(avt$Root_threat, '\\{|\\}|"', '')
avt$Root_threat <- str_replace_all(avt$Root_threat, ',', ', ')

avt$Crown_threat <- str_replace_all(avt$Crown_threat, '\\{|\\}|"', '')
avt$Crown_threat <- str_replace_all(avt$Crown_threat, ',', ', ')

avt$Trunk_threat <- str_replace_all(avt$Trunk_threat, '\\{|\\}|"', '')
avt$Trunk_threat <- str_replace_all(avt$Trunk_threat, ',', ', ')

avt$Tree_threat <- str_replace_all(avt$Tree_threat, '\\{|\\}|"', '')
avt$Tree_threat <- str_replace_all(avt$Tree_threat, ',', ', ')

# Sort out recorders

avt$rc <- ifelse(test = grepl("*Binns*", avt$Site_name), yes = "Recorder1", no = avt$Recorder)
avt$rc <- ifelse(test = grepl("*Montrose*", avt$Site_name), yes = "Recorder1", no = avt$rc)
avt$rc <- ifelse(test = grepl("*Leith*", avt$Site_name), yes = "Recorder1", no = avt$rc)
avt$rc <- ifelse(test = grepl("*Fyvie*", avt$Site_name), yes = "Recorder2", no = avt$rc)
avt$rc <- ifelse(test = grepl("*Fraser*", avt$Site_name), yes = "Recorder2", no = avt$rc)
avt$rc <- ifelse(test = grepl("*Lomond*", avt$Site_name), yes = "Recorder3", no = avt$rc)
avt$rc <- ifelse(test = grepl("*Balma*", avt$Site_name), yes = "Recorder3", no = avt$rc)

avt$Recorder <- avt$rc

avt <- subset(avt, select = -c(rc))

# Drop 3D
avt <- st_zm(avt, drop=T, what='ZM')

# Write out csv
write.csv(avt, file = "data/output/avt_record_clean.csv", row.names = FALSE)

st_write(avt, "data/output/avt_record_clean.gpkg", append = FALSE)

# Tree groups
tg <- st_read("data/tree_group_agg2.gpkg")

tg <- subset(tg, select = -c(path, layer))

tg <- st_zm(tg, drop=T, what='ZM')

st_write(tg, "data/output/tree_group_clean.gpkg", append = FALSE)



