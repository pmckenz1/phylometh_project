site_specimen <- read.delim("data/site_specimen_data/Supplement_1.txt")
species_character <- read.delim("data/species_character/Supplement_3.txt")
genbank_info <- read.delim("data/genbank_accession/Supplement_2.txt")

rm(test)
plot(site_specimen$longitude,site_specimen$latitude,ylim = c(25,38),xlim = c(91,104))



sp

rupicola_chars <- species_character[(species_character$species == "rupicola"),]
roylei_chars <- species_character[(species_character$species == "roylei"),]



points(site_specimen$longitude[(as.character(site_specimen$species_epithet) == "roylei")],site_specimen$latitude[(as.character(site_specimen$species_epithet) == "roylei")],pch = 16, col="red")
points(site_specimen$longitude[(as.character(site_specimen$species_epithet) == "rupicola")],site_specimen$latitude[(as.character(site_specimen$species_epithet) == "rupicola")],pch = 16, col="green")

points.species <- function(speciesname,color) {
  points(site_specimen$longitude[(as.character(site_specimen$species_epithet) == speciesname)],site_specimen$latitude[(as.character(site_specimen$species_epithet) == speciesname)],pch = 16, col=color)
}

plot(site_specimen$longitude,site_specimen$latitude,ylim = c(25,38),xlim = c(91,104))
points.species("roylei","purple")
points.species("rupicola","green")


points.species("plicata","yellow")

points.species("anas","orange")
points.species("cheilanthifolia","red")


plot(site_specimen$longitude,site_specimen$latitude,ylim = c(25,38),xlim = c(91,104))
points.species("angustiloba","purple")
points.species("ingens","green")


points.species("trichocymba","yellow")

points.species("anas","orange")
points.species("cheilanthifolia","red")


###
#function for getting eco variables from latitude and longitude

library(rgbif)
library(htmltab)
for (i in 1:45) { 
  datatable <- read.csv(paste0("/Users/pmckenz1/Desktop/plant_list/Feb16_class/gbif_data/species",i,".csv"))
  if (ncol(datatable) > 1) {
    datatable$total_precip <- NA #create our new columns
    datatable$avg_tmax <- NA
    datatable$avg_tmin <- NA
    datatable$avg_sun <- NA
    datatable$elevation <- NA
    for (o in 1:nrow(datatable)) {
      if (datatable$longitude[o] != 0) {
        newlink <- paste0("http://geonetwork3.fao.org/aglw/climatex.php?xcoord=",datatable$longitude[o],"&ycoord=",datatable$latitude[o],"&dddms=dd")
        if (length(suppressMessages(htmltab(newlink,which=1))) > 4) {
          climate_table <- suppressMessages(htmltab(doc = newlink, which = 2)[c(-1,-14),]) #pull in a table
          datatable$total_precip[o] <- sum(as.numeric(climate_table$Prc.))
          datatable$avg_tmax[o] <- mean(as.numeric(climate_table$Tmp.max.))
          datatable$avg_tmin[o] <- mean(as.numeric(climate_table$Tmp.min.))
          datatable$avg_sun[o] <- mean(as.numeric(climate_table$Sun))
          datatable$elevation[o] <- attributes(suppressMessages(htmltab(doc = newlink, which = 1)[6]))$names[1]
          print(paste0(o,"/",nrow(datatable),"......species ",i))
        } }
    }
    write.csv(datatable,paste0("/Users/pmckenz1/Desktop/plant_list/Feb16_class/gbif_data_withclimate/species",i,".csv"),row.names = F)
  }
}

library(htmltab)
get_eco_info <- function(longitude, latitude) {
  newlink <- paste0("http://geonetwork3.fao.org/aglw/climatex.php?xcoord=",longitude,"&ycoord=",latitude,"&dddms=dd")
  if (length(suppressMessages(htmltab(newlink,which=1))) > 4) {
    climate_table <- suppressMessages(htmltab(doc = newlink, which = 2)[c(-1,-14),]) #pull in a table
  #  datatable$total_precip[o] <- sum(as.numeric(climate_table$Prc.))
  #  datatable$avg_tmax[o] <- mean(as.numeric(climate_table$Tmp.max.))
  #  datatable$avg_tmin[o] <- mean(as.numeric(climate_table$Tmp.min.))
  #  datatable$avg_sun[o] <- mean(as.numeric(climate_table$Sun))
     elevation <- attributes(suppressMessages(htmltab(doc = newlink, which = 1)[6]))$names[1]
  #  print(paste0(o,"/",nrow(datatable),"......species ",i))
     elevation <- gsub("m","",elevation)
     elevation <- gsub(" ","",elevation)
  }
  else {
    stop(paste0("no data for the location: Lon = ",longitude,", Lat = ",latitude))
  }
  return(list(climate_table,elevation))
}

eco_list <- list()
for (i in 1:50) {
  lon <- site_specimen$longitude[i]
  lat <- site_specimen$latitude[i]
  if (lon != 0 && lat != 0) {
    eco_list[[i]]<- get_eco_info(lon,lat)
  }
  print(i)
}

testing <- cbind.data.frame(site_specimen[1:50,],elevation = unlist(sapply(eco_list,head)[2,]),stringsAsFactors = FALSE)


testing[(testing$locality_id == unique(testing$locality_id)[10]),]$elevation

length(unique(testing$locality_id))
