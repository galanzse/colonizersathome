
library(tidyverse)
library(readxl)

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
            dN15=mean(dN15, na.rm=T),
            RD=mean(RD, na.rm=T),
            SRA=mean(SRA, na.rm=T))
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
v_quantitative <- c("SLA","LDMC","SRL","RDMC","dC13","LNC","height","seed_weight","onset_flowering","length_bloom","LCC","dN15","RD","SRA","numb_disp")
v_qualitative <- c("self.compatible","archaeophyte","pollination","agochory","autochory","anemochory","hydrochory","zoochory")
  
# outliers
temp <- traits %>% dplyr::select(origin, all_of(v_quantitative)) %>% gather(all_of(v_quantitative), key='trait', value='value')
temp <- temp[!(is.na(temp$value)),]
ggplot(data=temp, aes(x=trait, y=value)) + geom_boxplot() + facet_wrap(~trait, scale="free") + xlab('')

traits$dC13[traits$dC13 > -26] <- NA # remove
traits$dN15[traits$dN15 > 10] <- NA
traits$LDMC[traits$LDMC > 400] <- NA
traits$RDMC[traits$RDMC > 400] <- NA
traits$SLA[traits$SLA > 500] <- NA
traits$SRL[traits$SRL > 17000] <- NA
traits$height <- log(traits$height)
traits$seed_weight <- log(traits$seed_weight)

# pairs
pairs(traits[,v_quantitative], lower.panel=NULL)


# plots traits cuantitativos
temp <- traits %>% dplyr::select(origin, invasiveness, all_of(v_quantitative)) %>%
  gather(all_of(v_quantitative), key='trait', value='value') %>% na.omit()
temp$trait <- as.factor(temp$trait)
temp$trait <- factor(temp$trait,levels=c("onset_flowering","length_bloom","height","seed_weight",
                                         "SLA","LDMC","LCC","LNC","dC13","dN15",
                                         "SRL","SRA","RDMC","RD","numb_disp"))

# origin
ggplot(data=temp, aes(x=origin, y=value, fill=origin)) + geom_boxplot() +
  facet_wrap(~trait, scale="free_y") + xlab('') + theme_classic() +
  theme(legend.position='top', legend.title=element_blank(), axis.text.x=element_blank()) +
  scale_fill_manual(values=c('lightgreen','coral1'))
# sign
for (tr in v_quantitative) {
  temp <- na.omit(traits[,c(tr,'origin')])
  temp <- t.test(temp[temp$origin=='native',tr], temp[temp$origin=='colonizer',tr])
  print(paste('The p-value of', tr, 'is', round(temp$p.value,3)))
}

# invasiveness
ggplot(data=temp, aes(x=invasiveness, y=value, fill=invasiveness)) + geom_boxplot() +
  facet_wrap(~trait, scale="free") + xlab('') + theme_classic() +
  theme(legend.position='top', legend.title=element_blank(), axis.text.x=element_blank()) +
  scale_fill_manual(values=c('lightgreen','coral1','gold'))
# sign
for (tr in v_quantitative) {
  temp <- na.omit(traits[,c(tr,'invasiveness')])
  temp <- lm(temp[,tr] ~ temp$invasiveness) %>% anova()
  print(paste('The p-value of', tr, 'is', round(temp$`Pr(>F)`[1],3)))
}


# plots traits cualitativos
v_qualitative

table(traits$pollination, traits$invasiveness)[c('insect','wind'),] %>% chisq.test()

table(traits$self.compatible, traits$invasiveness) %>% chisq.test()
table(traits$archaeophyte, traits$invasiveness) %>% chisq.test()

table(traits$agochory, traits$invasiveness) %>% chisq.test()
table(traits$anemochory, traits$invasiveness) %>% chisq.test()
table(traits$autochory, traits$invasiveness) %>% chisq.test()
table(traits$hydrochory, traits$invasiveness) %>% chisq.test()
table(traits$zoochory, traits$invasiveness) %>% chisq.test()
