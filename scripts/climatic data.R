
library(terra)
library(mapSpain)
library(geometry)
library(ggpubr)
source('scripts/import traits and tree.R')
occurrences <- read.csv("results/occurrences.txt", sep="")
myoccurrences_df <- read.csv("results/myoccurrences_df.txt", sep="")

# check names
traits$species[!(traits$species %in% myoccurrences_df$species)]

myoccurrences_df$species[myoccurrences_df$species=='Agrostis subspicata'] <- 'Chaetopogon fasciculatus'
myoccurrences_df$species[myoccurrences_df$species=='Cladanthus mixtus'] <- 'Chamaemelum mixtum'
myoccurrences_df$species[myoccurrences_df$species=='Lolium multiflorum'] <- 'Festuca perennis'
myoccurrences_df$species[myoccurrences_df$species=='Sulla coronaria'] <- 'Hedysarum coronarium'
myoccurrences_df$species[myoccurrences_df$species=='Malva trimestris'] <- 'Stegia trimestris'
myoccurrences_df$species[myoccurrences_df$species=='Lotus tetragonolobus'] <- 'Tetragonolobus purpureus'
myoccurrences_df$species[myoccurrences_df$species=='Festuca geniculata'] <- 'Vulpia geniculata'

# retain observations at the species level
myoccurrences_df <- myoccurrences_df %>% filter(species %in% traits$species)

# points
colnames(myoccurrences_df)[colnames(myoccurrences_df)=="decimalLatitude"] <- 'y'
colnames(myoccurrences_df)[colnames(myoccurrences_df)=="decimalLongitude"] <- 'x'
myoccurrences_pt <- vect(myoccurrences_df, geom=c('x','y'), 'epsg:4326')

# study area
andalucia_v <- esp_get_ccaa(ccaa='andalucia', epsg='4326') %>% vect() %>% as.polygons()
andalucia_r <- rast('C:/Users/user/Desktop/wc2.1_30s_bio/wc2.1_30s_bio_1.tif') %>% terra::crop(andalucia_v)

# filter points and redo dataframe
myoccurrences_pt <- myoccurrences_pt %>% terra::crop(andalucia_v)
myoccurrences_df <- myoccurrences_pt %>% as.data.frame(geom='xy')

# plot
plot(andalucia_r, legend=F)
lines(andalucia_v)
points(myoccurrences_pt)

# retain one observation per grid
myoccurrences_df$cell <- terra::extract(x=andalucia_r, y=myoccurrences_pt, cells=T)[,'cell']
myoccurrences_df <- myoccurrences_df %>% group_by(species, cell) %>% sample_n(1)

# traits
sppcounts <- myoccurrences_df$species %>% table() %>% as.data.frame()
colnames(sppcounts) <- c('species','counts')

# add info to trait table & plots
traits <- merge(traits, sppcounts, by='species', all.x=T)

# climatic data
climatic <- list.files('C:/Users/user/Desktop/wc2.1_30s_bio', full.names=T)
climatic <- rast(climatic) %>% terra::crop(andalucia_v)
climatic <- terra::extract(x=climatic, y=myoccurrences_pt, ID=F)
colnames(climatic) <- substr(colnames(climatic), 11, 16)
climatic$species <- myoccurrences_pt$species
# pairs(climatic[1:1000,], lower.panel=NULL)

# retain uncorrelated variables of mean, extreme and variation in temperature and precipitation
var_in <- c('species','bio_1','bio_5','bio_7','bio_12','bio_14','bio_15')
climatic <- climatic[,var_in] %>% na.omit()
climatic_pca <- prcomp(climatic[,-1], scale=T, center=T)
summary(climatic_pca)
climatic_pca <- climatic_pca$x[,1:3] %>% as.data.frame()
climatic_pca$species <- climatic$species

# mpd and convex hull of the climatic niche
traits$climatic_div <- NA
traits$climatic_ric <- NA
for (i in 1:nrow(traits)) {
  temp <- climatic_pca %>% filter(species == traits$species[i]) %>% dplyr::select(PC1,PC2,PC3)
  if (nrow(temp)>1) {
    traits$climatic_div[i] <- dist(temp) %>% mean()
    traits$climatic_ric[i] <- convhulln(temp, output.options=T)$vol
  }
}

# save
write.table(traits, 'results/trait_final.txt')

lm(log(traits$counts) ~ traits$origin) %>% aov() %>% TukeyHSD(conf.level=.95)
lm(traits$climatic_ric ~ traits$origin) %>% aov() %>% TukeyHSD(conf.level=.95)
lm(traits$climatic_div ~ traits$origin) %>% aov() %>% TukeyHSD(conf.level=.95)

lm(log(traits$counts) ~ traits$invasiveness) %>% aov() %>% TukeyHSD(conf.level=.95)
lm(traits$climatic_ric ~ traits$invasiveness) %>% aov() %>% TukeyHSD(conf.level=.95)
lm(traits$climatic_div ~ traits$invasiveness) %>% aov() %>% TukeyHSD(conf.level=.95)

temp <- traits[,c('origin','origin','invasiveness','counts','climatic_div','climatic_ric')] %>% na.omit()
temp <- temp %>% pivot_longer(4:6)
temp$invasiveness <- as.factor(temp$invasiveness)
temp$origin <- as.factor(temp$origin)
temp$name <- as.factor(temp$name)
temp$name <- factor(temp$name, levels=c("counts","climatic_ric","climatic_div"))
levels(temp$name) <- c('frequency','climatic niche richness','climatic niche diversity')

g1 <- ggplot(aes(y=value, fill=origin), data=temp) + geom_boxplot() +
  facet_wrap(~name, scales='free_y') + theme_classic() + xlab('') + ylab('') +
  theme(legend.position='top', legend.title=element_blank(), axis.text.x=element_blank()) +
  scale_fill_manual(values=c('lightgreen','coral1','gold'))
g2 <- ggplot(aes(y=value, fill=invasiveness), data=temp) + geom_boxplot() +
  facet_wrap(~name, scales='free_y') + theme_classic() + xlab('') + ylab('') +
  theme(legend.position='top', legend.title=element_blank(), axis.text.x=element_blank()) +
  scale_fill_manual(values=c('lightgreen','coral1','gold'))

ggarrange(g1, g2, nrow=2)
