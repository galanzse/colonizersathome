
# trait comparisons
source('scripts/import traits and tree.R')
library(geiger)
library(pgirmess)

# plot quantitative traits
temp <- traits %>% dplyr::select(origin, invasiveness, all_of(v_quantitative)) %>%
  gather(all_of(v_quantitative), key='trait', value='value') %>% na.omit()
temp$trait <- as.factor(temp$trait)
temp$trait <- factor(temp$trait,levels=c("onset_flowering","length_bloom","height","seed_weight",
                                         "SLA","LDMC","CN","dC13",
                                         "SRL","RDMC","RD","numb_disp")) # "dN15","SRA",

# origin
ggplot(data=temp, aes(x=origin, y=value, fill=origin)) + geom_boxplot() +
  facet_wrap(~trait, scale="free") + xlab('') + ylab('') + theme_classic() +
  theme(legend.position='top', legend.title=element_blank(), axis.text.x=element_blank()) +
  scale_fill_manual(values=c('lightgreen','coral1'))

# sign
for (tr in v_quantitative) {
  temp2 <- na.omit(traits[,c(tr,'origin')])
  temp2 <- t.test(temp2[temp2$origin=='non-coloniser',tr], temp2[temp2$origin=='coloniser',tr])
  print(paste('The p-value of', tr, 'is', round(temp2$p.value,3)))
}

wilcox.test(traits$onset_flowering ~ traits$origin)
wilcox.test(traits$length_bloom ~ traits$origin)
wilcox.test(traits$numb_disp ~ traits$origin)

# invasiveness
ggplot(data=temp, aes(x=invasiveness, y=value, fill=invasiveness)) + geom_boxplot() +
  facet_wrap(~trait, scale="free") + xlab('') + ylab('') + theme_classic() +
  theme(legend.position='top', legend.title=element_blank(), axis.text.x=element_blank()) +
  scale_fill_manual(values=c('lightgreen','coral1','gold'))

# sign
for (tr in v_quantitative) {
  temp2 <- na.omit(traits[,c(tr,'invasiveness')])
  temp3 <- lm(temp2[,tr] ~ temp2$invasiveness) %>% anova()
  print(paste('The p-value of', tr, 'is', round(temp3$`Pr(>F)`[1],3)))
  print(lm(temp2[,tr] ~ temp2$invasiveness) %>% aov() %>% TukeyHSD(conf.level=.95))
}

kruskal.test(traits$onset_flowering ~ traits$invasiveness)
kruskal.test(traits$length_bloom ~ traits$invasiveness); kruskalmc(traits$length_bloom ~ traits$invasiveness)
kruskal.test(traits$numb_disp ~ traits$invasiveness); kruskalmc(traits$numb_disp ~ traits$invasiveness)

# plot qualitative traits
table(traits$lifeform, traits$invasiveness)[5,] %>% chisq.test()
table(traits$growth_form, traits$invasiveness)[5,] %>% chisq.test()
table(traits$pollination, traits$invasiveness)[2,] %>% chisq.test()
table(traits$self.compatible, traits$invasiveness) %>% chisq.test()
table(traits$archaeophyte, traits$invasiveness) %>% chisq.test()
table(traits$agochory, traits$invasiveness) %>% chisq.test()
table(traits$anemochory, traits$invasiveness) %>% chisq.test()
table(traits$autochory, traits$invasiveness) %>% chisq.test()
table(traits$hydrochory, traits$invasiveness) %>% chisq.test()
table(traits$zoochory, traits$invasiveness) %>% chisq.test()


# table of effect sizes, anovas and phylogenetically corrected Anovas

# table
phy_origin <- matrix(ncol=8, nrow=length(v_quantitative))
rownames(phy_origin) <- v_quantitative
colnames(phy_origin) <- c('F','dfgroup','dfresiduals','p','p.phy','non-coloniser','naturalised','invasive')

for (tr in v_quantitative) {
  # filter data
  temp <- traits[,c(tr,'invasiveness','tree_species')] %>% na.omit()
  colnames(temp) <- c('trait','invasiveness','tree_species')
  if (tr %in% c('onset_flowewing', 'length_bloom')) {  temp$trait <- log(temp$trait) }
  
  # format data
  dat <- temp$trait
  names(dat) <- temp$tree_species
  group <- temp$invasiveness
  names(group) <- temp$tree_species
  
  # run analysis
  print(tr)
  t1 <- aov.phylo(dat ~ group, keep.tip(plant.tree, temp$tree_species))
  t2 <- anova(t1)
  
  # save
  phy_origin[tr,'F'] <- t2$`F value`[1]
  phy_origin[tr,'dfgroup'] <- t2$Df[1]
  phy_origin[tr,'dfresiduals'] <- t2$Df[2]
  phy_origin[tr,'p'] <- t2$`Pr(>F)`[1]
  phy_origin[tr,'p.phy'] <- NA
  phy_origin[tr,'non-coloniser'] <- t1$coefficients['(Intercept)']
  phy_origin[tr,'naturalised'] <- sum(t1$coefficients[c(1,2)])
  phy_origin[tr,'invasive'] <- sum(t1$coefficients[c(1,3)])
  
}
