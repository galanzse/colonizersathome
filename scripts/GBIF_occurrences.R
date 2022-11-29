
library(rgbif)
library(terra)
source("scripts/import traits.R")

# species
occurrences <- traits

# remove entries without epitet
occurrences <- occurrences[stringr::str_count(occurrences$species, " ")==1,]

# fixed names for gbif
occurrences$gbif_name <- occurrences$species
occurrences$gbif_name[occurrences$species=='Festuca bromoides'] <- 'Vulpia bromoides'
occurrences$gbif_name[occurrences$species=='Festuca myuros'] <- 'Vulpia myuros'
occurrences$gbif_name[occurrences$species=='Festuca perennis'] <- 'Lolium multiflorum'
occurrences$gbif_name[occurrences$species=='Molineriella minuta'] <- 'Molineriella'
occurrences$gbif_name[occurrences$species=='Stegia trimestris'] <- 'Malva trimestris'
occurrences$gbif_name[occurrences$species=='Tetragonolobus purpureus'] <- 'Lotus tetragonolobus'
occurrences$gbif_name[occurrences$species=='Chaetopogon fasciculatus'] <- 'Agrostis subspicata'
occurrences$gbif_name[occurrences$species=='Chamaemelum mixtum'] <- 'Cladanthus mixtus'
occurrences$gbif_name[occurrences$species=='Hedysarum coronarium'] <- 'Sulla coronaria'

# download occurrences from Spain in GBIF
occurrences$usageKey <- NA
occurrences$scientificName <- NA
occurrences$status <- NA
occurrences$confidence <- NA
occurrences$counts <- NA
occurrences_list <- list()
lm <- 30000
for (i in 1:nrow(occurrences)) {
  mysp <- name_backbone(name=occurrences$gbif_name[i]) # download backbone
  
  if (mysp$matchType != "NONE") {
    
    # add info
    occurrences$usageKey[i] <- mysp$usageKey
    occurrences$scientificName[i] <- mysp$scientificName
    occurrences$status[i] <- mysp$status
    occurrences$confidence[i] <- mysp$confidence
    occurrences$counts[i] <- occ_count(taxonKey=mysp$usageKey, country="ES", georeferenced=TRUE)
    
    # download occurrences
    mysp <- occ_data(taxonKey=mysp$usageKey, country="ES", limit=lm)
    occurrences_list[[i]] <- mysp$data %>% as.data.frame()
  }
  
  print(occurrences$gbif_name[i])
}

# check
sapply(occurrences_list, nrow)

# select the columns I need
mycolumns <- c('taxonKey', 'species', 'decimalLatitude', 'decimalLongitude')
occurrences_list_reduced <- occurrences_list
for (i in 1:length(occurrences_list_reduced)) {
  occurrences_list_reduced[[i]] <- occurrences_list_reduced[[i]][mycolumns] %>% na.omit()
}

# list to dataframe
myoccurrences_df <- do.call('rbind', myoccurrences_col)
dim(myoccurrences_df)

# some species were downloaded with different names
occurrences$gbif_name[!(occurrences$gbif_name %in% myoccurrences_df$species)]

# save
write.table(occurrences, 'results/occurrences.txt')
write.table(myoccurrences_df, "results/myoccurrences_df.txt")
save(myoccurrences_col, file="results/occurrences_list.RData")
save(occurrences_list_reduced, file="results/occurrences_list_reduced.RData")
