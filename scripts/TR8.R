
# download traits using the TR8 package

# install.packages('TR8',dependencies=TRUE)
library(TR8)
library(tidyverse)

# available traits
data("available_tr8")
available_tr8 %>% View()

# species
species <- tr_med$species
species[species=="Trifolium fragiferum/hybridum"] <- 'Trifolium hybridum'
species[species=="Lotus corniculatus/tenuis"] <- 'Lotus corniculatus'

ntraits <- c('poll_vect','dispersal','DispMode')
# 'poll_vect','poll_vect_B','poll_vect_fr'
# 'dispersal','dispersal_morphology','Breeding_sys','DispMode'

ntraits_db <- tr8(species_list=species, download_list=ntraits,
                    catminat_alternatives=TRUE, synonyms=TRUE, allow_persistent=F)

ntraits_db <- ntraits_db@results %>% as.data.frame()

ntraits_db$species <- rownames(ntraits_db)
ntraits_db$species[ntraits_db$species=="Trifolium hybridum"] <- 'Trifolium fragiferum/hybridum'
ntraits_db$species[ntraits_db$species=="Lotus corniculatus"] <- 'Lotus corniculatus/tenuis'


traits <- read_excel("data/traits.xlsx", sheet = "reproduction")
ntraits_db <- left_join(traits, ntraits_db, by="species")

write.table(ntraits_db, "ntraits_db.txt")
