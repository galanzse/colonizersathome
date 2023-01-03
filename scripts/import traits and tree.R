
library(tidyverse)
library(readxl)
library(ape)

# import functional traits
traits <- read_excel("data/traits.xlsx", sheet = "traits")
str(traits)

# california natives to exclude
california_sp <- traits %>% filter(region=='California' & origin=='native') %>% dplyr::select(species) %>% deframe()
traits <- traits[!(traits$species%in%california_sp),]

# average data
traits <- traits %>% group_by(species, family, growth_form, lifeform, origin, invasiveness) %>%
  summarise(SLA=median(SLA, na.rm=T),
            LDMC=median(LDMC, na.rm=T),
            SRL=median(SRL, na.rm=T),
            RDMC=median(RDMC, na.rm=T),
            dC13=median(dC13, na.rm=T),
            LNC=median(LNC, na.rm=T),
            height=median(height, na.rm=T),
            seed_weight=median(seed_weight, na.rm=T),
            onset_flowering=median(onset_flowering, na.rm=T),
            length_bloom=median(length_bloom, na.rm=T))

# add traits previously unused
traits2 <- read_excel("data/traits.xlsx", sheet = "traits2") %>%
  group_by(species) %>%
  summarise(LCC=mean(LCC, na.rm=T),
            # dN15=mean(dN15, na.rm=T),
            # SRA=mean(SRA, na.rm=T),
            RD=mean(RD, na.rm=T))
traits2 <- traits2[!(traits2$species%in%california_sp),]
traits2$species[which(!(traits2$species %in% traits$species))] # check names

# add reproductive traits
reproduction <- read_excel("data/traits.xlsx", sheet = "reproduction")
reproduction$species[which(!(reproduction$species %in% traits$species))] # check names
reproduction$numb_disp <- reproduction[,c("agochory","autochory","anemochory","hydrochory","zoochory")] %>% rowSums()
reproduction$cleistogamy <- NULL # seems like data is insufficient

# check and merge
traits <- merge(traits, traits2, by='species', all.x=T) 
traits <- merge(traits, reproduction, by='species', all.x=T)

# check variables
traits$origin <- as.factor(traits$origin); levels(traits$origin) <- c('colonizer','native')
traits$origin <- factor(traits$origin, levels=c('native','colonizer'))
traits$invasiveness <- as.factor(traits$invasiveness); levels(traits$invasiveness) <- c('invasive','native','naturalised')
traits$invasiveness <- factor(traits$invasiveness, levels=c('native','naturalised','invasive'))

table(traits$origin)
table(traits$invasiveness)

traits$pollination <- as.factor(traits$pollination); levels(traits$pollination) <- c('insect','selfed','wind')
traits$growth_form <- as.factor(traits$growth_form)
traits$lifeform <- as.factor(traits$lifeform)
traits$invasiveness <- as.factor(traits$invasiveness)
traits$onset_flowering <- as.integer(traits$onset_flowering)
traits$length_bloom <- as.integer(traits$length_bloom)
traits$numb_disp <- as.integer(traits$numb_disp)
traits$clonality <- as.factor(traits$clonality)
traits$self.compatible <- as.factor(traits$self.compatible)
traits$archaeophyte <- as.factor(traits$archaeophyte)
traits$agochory <- as.factor(traits$agochory)
traits$autochory <- as.factor(traits$autochory)
traits$anemochory <- as.factor(traits$anemochory)
traits$hydrochory <- as.factor(traits$hydrochory)
traits$zoochory <- as.factor(traits$zoochory)

# final dataset
str(traits)
rm(reproduction, traits2, california_sp)

# trait types
v_quantitative <- c("SLA","LDMC","SRL","RDMC","dC13","LNC","height","seed_weight","onset_flowering","length_bloom","LCC","RD","numb_disp") # "dN15","SRA",
v_qualitative <- c("lifeform","growth_form","self.compatible","archaeophyte","pollination","agochory","autochory","anemochory","hydrochory","zoochory")
  
# outliers
temp <- traits %>% dplyr::select(origin, all_of(v_quantitative)) %>% gather(all_of(v_quantitative), key='trait', value='value')
temp <- temp[!(is.na(temp$value)),]
ggplot(data=temp, aes(x=trait, y=value)) + geom_boxplot() + facet_wrap(~trait, scale="free") + xlab('')

traits$dC13[traits$dC13 > -26] <- NA # remove
# traits$dN15[traits$dN15 > 10] <- NA
traits$LDMC[traits$LDMC > 400] <- NA
traits$RDMC[traits$RDMC > 400] <- NA
traits$SLA[traits$SLA > 500] <- NA
traits$SRL[traits$SRL > 17000] <- NA
traits$height <- log(traits$height)
traits$seed_weight <- log(traits$seed_weight)

# pairs
pairs(traits[,v_quantitative], lower.panel=NULL)


# create new variable to match traits and tree species' names
traits$tree_species <- sub(" ", "_", traits$species)

# import tree
plant.tree <- read.tree("data/PhytoPhylo.tre")

# check names
table(traits$tree_species %in% plant.tree$tip.label)
traits$tree_species[!(traits$tree_species %in% plant.tree$tip.label)]

# add congeneric taxa in new variable
traits$tree_species[traits$tree_species=="Aegilops"] <- "Aegilops_speltoides"
traits$tree_species[traits$tree_species=="Agrostis_pourretii"] <- "Agrostis_castellana"
traits$tree_species[traits$tree_species=="Anacyclus_radiatus"] <- "Anacyclus_clavatus"
traits$tree_species[traits$tree_species=="Arenaria_hispanica"] <- "Arenaria_serpyllifolia"
traits$tree_species[traits$tree_species=="Arisarum_simorrhinum"] <- "Arisarum_vulgare"
traits$tree_species[traits$tree_species=="Asphodelus_ramosus"] <- "Asphodelus_albus"
traits$tree_species[traits$tree_species=="Astragalus"] <- "Astragalus_sparsus"
traits$tree_species[traits$tree_species=="Biserrula_pelecinus"] <- "Astragalus_pelecinus"
traits$tree_species[traits$tree_species=="Bellis_annua"] <- "Bellis_sylvestris"
traits$tree_species[traits$tree_species=="Biscutella"] <- "Biscutella_didyma"
traits$tree_species[traits$tree_species=="Biscutella_baetica"] <- "Biscutella_laevigata"
traits$tree_species[traits$tree_species=="Carduncellus_caeruleus"] <- "Carthamus_tinctorius"
traits$tree_species[traits$tree_species=="Carduus_bourgeanus"] <- "Carduus_pycnocephalus"
traits$tree_species[traits$tree_species=="Carduus_2"] <- "Carduus_defloratus"
traits$tree_species[traits$tree_species=="Carduus_7"] <- "Carduus_crispus"
traits$tree_species[traits$tree_species=="Carlina_racemosa"] <- "Carlina_acaulis"
traits$tree_species[traits$tree_species=="Centaurea_pullata"] <- "Centaurea_involucrata"
traits$tree_species[traits$tree_species=="Chaetopogon_fasciculatus"] <- "Agrostis_capillaris"
traits$tree_species[traits$tree_species=="Chamaemelum_mixtum"] <- "Chamaemelum_nobile"
traits$tree_species[traits$tree_species=="Coleostephus_myconis"] <- "Leucanthemum_vulgare"
traits$tree_species[traits$tree_species=="Daucus_muricatus"] <- "Daucus_glochidiatus"
traits$tree_species[traits$tree_species=="Diplotaxis"] <- "Diplotaxis_erucoides"
traits$tree_species[traits$tree_species=="Erodium_aethiopicum"] <- "Erodium_maritimum"
traits$tree_species[traits$tree_species=="Festuca_bromoides"] <- "Vulpia_bromoides"
traits$tree_species[traits$tree_species=="Festuca_myuros"] <- "Vulpia_microstachys"
traits$tree_species[traits$tree_species=="Festuca_perennis"] <- "Lolium_multiflorum"
traits$tree_species[traits$tree_species=="Stipellula_capensis"] <- "Juncus_capensis"
traits$tree_species[traits$tree_species=="Juncus"] <- "Juncus_trifidus"
traits$tree_species[traits$tree_species=="Lathyrus_angulatus"] <- "Lathyrus_niger"
traits$tree_species[traits$tree_species=="Logfia_gallica"] <- "Filago_gallica"
traits$tree_species[traits$tree_species=="Lysimachia_arvensis"] <- "Anagallis_arvensis"
traits$tree_species[traits$tree_species=="Medicago_doliata"] <- "Medicago_turbinata"
traits$tree_species[traits$tree_species=="Molineriella_minuta"] <- "Deschampsia_danthonioides"
traits$tree_species[traits$tree_species=="Ononis"] <- "Ononis_rotundifolia"
traits$tree_species[traits$tree_species=="Onobrychis_argentea"] <- "Onobrychis_humilis"
traits$tree_species[traits$tree_species=="Otospermum_glabrum"] <- "Matricaria_matricarioides"
traits$tree_species[traits$tree_species=="Phalaris_aquatica"] <- "Phalaris_coerulescens"
traits$tree_species[traits$tree_species=="Plantago_serraria"] <- "Plantago_crassifolia"
traits$tree_species[traits$tree_species=="Scorpiurus_vermiculatus"] <- "Scorpiurus_muricatus" 
traits$tree_species[traits$tree_species=="Silene_colorata"] <- "Silene_pusilla"
traits$tree_species[traits$tree_species=="Stegia_trimestris"] <- "Lavatera_trimestris"
traits$tree_species[traits$tree_species=="Tetragonolobus_purpureus"] <- "Lotus_tetragonolobus"
traits$tree_species[traits$tree_species=="Trifolium"] <- "Trifolium_wormskioldii"
traits$tree_species[traits$tree_species=="Galactites_tomentosa"] <- "Cirsium_helenioides"
traits$tree_species[traits$tree_species=="Capsella_rubella"] <- "Capsella_bursa-pastoris"

# prune tree
plant.tree <- keep.tip(phy=plant.tree, tip=traits$tree_species)

# plot
plant.tree2 <- plant.tree
plant.tree2$tip.label <- traits$species[match(plant.tree2$tip.label, traits$tree_species)] # original names
geo <- factor(traits$invasiveness[match(plant.tree2$tip.label, traits$species)]) # colors
mycol <- c('lightgreen','coral1','gold')[geo]
par(mar=c(0,0,0,0))
plot(plant.tree2,  tip.color=mycol, cex=0.35)
