
# trait comparisons
source('scripts/import traits and tree.R')

# plot quantitative traits
temp <- traits %>% dplyr::select(archaeophyte, all_of(v_quantitative)) %>%
  gather(all_of(v_quantitative), key='trait', value='value') %>% na.omit()
temp$trait <- as.factor(temp$trait)
temp$trait <- factor(temp$trait,levels=c("onset_flowering","length_bloom","height","seed_weight",
                                         "SLA","LDMC","CN","dC13",
                                         "SRL","RDMC","RD","numb_disp"))
temp$archaeophyte <- as.factor(temp$archaeophyte)
levels(temp$archaeophyte) <- c('non-archaeophyte','archaeophyte')

# origin
ggplot(data=temp, aes(x=archaeophyte, y=value, fill=archaeophyte)) + geom_boxplot() +
  facet_wrap(~trait, scale="free") + xlab('') + ylab('') + theme_classic() +
  theme(legend.position='top', legend.title=element_blank(), axis.text.x=element_blank()) +
  scale_fill_manual(values=c('lightgreen','coral1'))
