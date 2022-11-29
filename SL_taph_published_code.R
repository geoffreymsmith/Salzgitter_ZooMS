#Salzgitter-Lebenstedt: R code for figures and tables ----

# This code will reproduce all figures and analyses from the paper:

# Ruebens et al., 2022:
# Neanderthal subsistence, taphonomy and chronology 
# at Salzgitter-Lebenstedt (Germany): 
# a multifaceted analyses of morphologically unidentifiable bone. 

##load libraries
##load libraries----
library(rstatix) #rstatix conflicts with tidyverse. best to load as required
library(tidyverse)
library(colorblindr)
library(patchwork)
library(scales)

##set default theme----
theme_set(theme_bw())
theme_default <- theme(axis.title = element_text(face = "bold",
                                                 size = 12))

##set colour scheme----
taxon_colours <-c('#b2182b','#ef8a62','#fddbc7','#d1e5f0','#67a9cf','#2166ac')

#read in total NSP data
SL_total_NSP <- read_csv("data/SL_total_NSP.csv")


# 1. Bone surface preservation----
## 1a. Bone surface readability----

#load data: readability data for SL
SL_read_mod <- read_csv("data/SL_read_mod.csv")

## 1b. chi-square on readability----
###prepare data: focus on main 4 taxa
read_chi_sq <- 
  SL_read_mod %>% 
  filter(`Barcode ID` == "Elephantidae"|
           `Barcode ID` == "Reindeer"|
           `Barcode ID` == "Bos/Bison"|
           `Barcode ID` == "Equidae"|
           `Barcode ID` == "unidentified") %>% 
  select(`Barcode ID`, type, read.mod) %>% 
  pivot_wider(names_from = type,
              values_from = read.mod)

###create data frame: row names = taxon
read_chi_sq <- data.frame(read_chi_sq, 
                          row.names = (read_chi_sq$`Barcode ID`))
###drop taxon column
read_chi_sq <- read_chi_sq %>% select(-Barcode.ID) %>% 
  select (low.readability, high.readability)

##chi square test: taxa vs read
SL_chisq_read <- 
  read_chi_sq %>% 
  chisq_test() 
#The data for SL indicate that weathering is dependent on Taxon
SL_chisq_read

##chi square tests: descriptive stats
SL_chisq_read_stats <-    
  chisq_descriptives(SL_chisq_read) %>% 
  rename(Barcode_ID = Var1, read= Var2) %>% 
  select(Barcode_ID, read, observed, std.resid) %>% 
  pivot_wider(names_from = read,
              values_from = c(observed, std.resid)) %>% 
  rename(obs_low_read=observed_low.readability,
         obs_high_read=observed_high.readability,
         AR_low_read = std.resid_low.readability,
         AR_high_read = std.resid_high.readability) %>% 
  select(Barcode_ID,obs_low_read, AR_low_read, obs_high_read) %>% 
  rename(AR = AR_low_read) %>% 
  add_row(Barcode_ID = "chi square",
          AR = 60.9)

## 1c. Figure 3: Bone readability----

#for labels on ggplot
SL_read_taxa <- 
        SL_read_mod %>% 
          mutate(`Barcode ID` = factor(`Barcode ID`,
                                         levels = c("Elephantidae",
                                                    "Equidae",
                                                    "Reindeer",
                                                    "Bos/Bison",
                                                    "unidentified")))
lbs = brk = levels(SL_read_taxa$`Barcode ID`)        
lbs[match("Bos/Bison", brk)] = expression(italic("Bos/Bison"))

#readability Figure 3
SL_bone_readability_figure <-  
  ggplot(SL_read_taxa,
         aes(x =`Barcode ID`,
             y = pc.read,
             fill = type))+
  geom_bar(position="dodge", stat="identity")+
  scale_x_discrete(breaks=brk, labels=lbs)+
  scale_fill_manual(name="type", 
                    values = taxon_colours)+
  labs(x= "Taxon", y = "% readability")+
  theme_default +
  theme(legend.title = element_blank())

SL_bone_readability_figure

#save and export
ggsave(SL_bone_readability_figure, filename = "output/Figure_3.tiff",
       dpi = 300, width = 20, height = 15, units = "cm")

## 1d. Figure 4: Bone surface weathering----
# weathering categories
weath.labels <- c(low.weath = "low weathering",
                  med.weath = "medium weathering",
                  heavy.weath = "high weathering")
#load in data: weathering
SL_weathering_mod <- read_csv("data/SL_weathering.csv")

##weathering general across assemblage
SL_weath_general_fig <- 
  SL_weathering_mod %>% 
mutate(Weathering = 
         factor(Weathering,
                levels = 
                  c("0",
                    "1",
                    "2",
                    "3",
                    "4",
                    "5"))) %>% 
  ggplot(aes(x=Weathering,
             y=pc,
             fill = Weathering))+
  geom_bar(stat="identity")+
  scale_fill_manual(values = taxon_colours)+
  labs(x= "weathering stage", y="% weathering")+
  scale_y_continuous(limits = c(0,60))+
  theme_default+
  theme(legend.position = "none")

SL_weath_general_fig

## weathering major taxa
#read in weathering by taxa data
SL_weath_raw_taxa <- read_csv("data/SL_weath_raw_taxa.csv")

##combine into modified categories low(0,1), medium(2,3), high(4,5)
SL_weath_taxa <- 
  SL_weath_raw_taxa %>%
  select(-pc) %>% 
  pivot_wider(names_from = Weathering, 
              values_from = NSP,
              values_fill = 0) %>% 
  mutate(low.weath = `0`+`1`) %>% 
  mutate(med.weath = `2` + `3`) %>% 
  mutate(heavy.weath = `4` + `5`) %>% 
  pivot_longer(!`Barcode ID`:`0`, 
               names_to = "type", 
               values_to = "weath.mod") %>% 
  select(-(`1`:`0`)) %>% 
  left_join(SL_total_NSP) %>% 
  mutate(pc = round(weath.mod/NSP*100, 1)) %>% 
  filter(`Barcode ID` == "Reindeer"|
           `Barcode ID` == "Elephantidae"|
           `Barcode ID` == "Bos/Bison" |
           `Barcode ID` == "Equidae"|
           `Barcode ID` == "unidentified") %>% 
  mutate(type = 
           factor(type,
                  labels = c("low weathering",
                             "medium weathering",
                             "high weathering"),
                  levels = c("low.weath",
                             "med.weath",
                             "heavy.weath"))) 

## Taxon vs. weathering
SL_weath_taxa_fig <-      
  ggplot(SL_weath_taxa,
         aes(x =`Barcode ID`,
             y = pc,
             fill = type))+
  geom_col(position = position_dodge2())+
  scale_x_discrete(breaks=brk, labels=lbs)+
  scale_fill_manual(name="type", values = c('#b2182b','#fddbc7','#2166ac')) +
  labs(x="Taxon", y = "%NSP")+
  theme_default+
  theme(legend.title = element_blank())

SL_weath_taxa_fig

## 1e. Figure 4:  general weathering plus species specific
SL_weath_combined <- 
  SL_weath_general_fig/SL_weath_taxa_fig+
  plot_annotation(tag_levels = 'a')

SL_weath_combined

##save and export
ggsave(plot = SL_weath_combined, filename = "output/Figure_4.tiff",
       dpi = 300, width = 20, height = 15, units = "cm")

## 1f. Chi-square: Taxa vs. weathering----
# read in data
SL_weath_mod_labels <- read_csv("data/SL_weath_mod_labels.csv")

#setup for chi-square analysis
SL_weath_chi_sq <- 
  SL_weath_mod_labels %>% 
  select(`Barcode ID`, type, weath.mod) %>% 
  pivot_wider(names_from = type,
              values_from = weath.mod) 

##prepare data: taxa = row names
SL_weath_chi_sq <- data.frame(SL_weath_chi_sq, row.names = (SL_weath_chi_sq$`Barcode ID`))

##drop taxa column  
SL_weath_chi_sq <- SL_weath_chi_sq %>% 
  select(-Barcode.ID) %>% 
  select (low.weathering,medium.weathering,high.weathering)

#low-medium weathering
SL_low_med_chisq <- 
  SL_weath_chi_sq %>% 
  select(-high.weathering) %>% 
  chisq_test() 
#significance so taxon is dependent on weathering stage
SL_low_med_chisq

#chi-square stats
SL_low_med_stats <- 
  SL_low_med_chisq %>% 
  chisq_descriptives() %>% 
  rename(Barcode_ID = Var1, weath= Var2) %>%  
  select(Barcode_ID, weath, observed, std.resid) %>% 
  pivot_wider(names_from = weath,
              values_from = c(observed, std.resid))%>% 
  select(1,2,5,3) %>% 
  rename(Observe_low = 2,
         AR_medium = 3,
         Observe_medium = 4)

#medium-high weathering
SL_med_high_chisq <- 
  SL_weath_chi_sq %>% 
  select(-low.weathering) %>% 
  chisq_test()

SL_med_high_chisq

#significance so taxon is dependent on weathering stage
SL_med_high_stats <- 
  SL_med_high_chisq %>% 
  chisq_descriptives() %>% 
  rename(Barcode_ID = Var1, weath= Var2) %>%  
  select(Barcode_ID, weath, observed, std.resid) %>% 
  pivot_wider(names_from = weath,
              values_from = c(observed, std.resid))%>% 
  select(1,5,3) %>% 
  rename(AR_high = 2,
         Observe_high = 3)

#Merge to form table
SL_weath_chi_sq_stats <- 
  SL_low_med_stats %>% 
  full_join(SL_med_high_stats, by = "Barcode_ID") %>% 
  add_row(Barcode_ID = "chi square",
          AR_medium = 16.1,
          AR_high = 18.4)


## 1g. Bone surface abrasion----

#read in data
SL_bone_abrasion <- read_csv("data/SL_bone_abrasion.csv")

#Create abrasion labels
abrasion.label <- labels <-  c(high.abrasion = "high abrasion", low.abrasion = "low abrasion")

##Tidy data wide version
SL_bone_abrasion_wide <- 
  SL_bone_abrasion %>% 
  select(`Barcode ID`, type, abras.mod, pc.abrasion) %>% 
  pivot_wider(names_from = type, values_from = c(abras.mod,pc.abrasion)) %>%
  rename(pc_low_abrasion = pc.abrasion_low.abrasion,
         pc_high_abrasion = pc.abrasion_high.abrasion,
         n_low_abrasion = abras.mod_low.abrasion,
         n_high_abrasion = abras.mod_high.abrasion) %>% 
  select(1,3,5,2,4)


## Figure: Taxon vs. bone abrasion
SL_bone_abrasion_fig <- 
  ggplot(SL_bone_abrasion,
         aes(x =`Barcode ID`,
             y = pc.abrasion,
             fill = type))+
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(name="type", values = taxon_colours,
                    labels = c("low abrasion", "high abrasion"))+
  scale_x_discrete(breaks=brk, labels=lbs)+
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,20))+
  labs(x= "Taxon", y = "% abrasion")+
  theme_default +
  theme(legend.title = element_blank())

SL_bone_abrasion_fig

# 2. Biomolecular preservation----
## 2a. read in data----
SL_ambic_acid_taxa_1105 <- read_csv("data/SL_ambic_acid_taxa_1105.csv")

SL_ambic_acid_taxa_1105 <- 
SL_ambic_acid_taxa_1105 %>% 
  mutate(`Barcode ID` = factor(`Barcode ID`,
                               levels = c("Elephantidae",
                                          "Equidae",
                                          "Reindeer",
                                          "Bos/Bison")))
  

#make labels Italics
lbs = brk = levels(SL_ambic_acid_taxa_1105$`Barcode ID`)        
lbs[match("Bos/Bison", brk)] = expression(italic("Bos/Bison"))


## 2b. Figure 5: box plot to compare deamidation of P1105----
SL_ambic_acid_taxa_1105_figure <- 
  ggplot(SL_ambic_acid_taxa_1105,
         aes(x=`Barcode ID`, y = value, fill = Protocol, color = Protocol))+
  geom_boxplot(alpha = 0.5,
               outlier.colour = NA)+
  geom_point(position = position_jitterdodge(jitter.width = 0.35), 
             alpha = 0.7)+
  scale_color_manual(values = c('#2166ac', 'black'), 
                     labels = c("Ambic", "Acid"))+
  scale_fill_manual(values = c('#2166ac', 'black'), 
                    labels = c("Ambic", "Acid"))+
  scale_x_discrete(breaks=brk, labels=lbs)+
  scale_y_continuous(limits = c(0.0,1.0))+
  labs(x = "Taxon", y = "\u03b11 508 deamidation")+
  theme_default+
  theme(legend.position = "bottom")

SL_ambic_acid_taxa_1105_figure

##save and export
ggsave(plot = SL_ambic_acid_taxa_1105_figure, 
       filename = "output/Figure_5.tiff",
       dpi = 300, width = 20, height = 15, units = "cm")

## 2c. wilcoxon test to compare taxa using Acid_P1105----
SL_wilcox_deamid_acid_taxa <- read_csv("data/SL_wilcox_deamid.csv")
# run wilcox test
SL_wilcox_deamid_acid_taxa <- 
  SL_wilcox_deamid_acid_taxa %>% 
  wilcox_test(Acid_P1105~Barcode_ID) %>% 
  add_significance() 

SL_wilcox_deamid_acid_taxa

# 3. Salzgitter taxa and fragmentation----
## 3a. SL summary statistics for bone length----

SL_frag_summ_stats <- read_csv("data/SL_frag_summ_stats.csv")

## 3b. SL bone fragment length by taxa----

#read in data
SL_main_taxa_length <- read_csv("data/SL_main_taxa_length.csv")

# modify data into size class groups
SL_taxa_size_class <- 
  SL_main_taxa_length %>% 
  mutate(length =
           cut(length,
               breaks = c(20,30,40,50,Inf),
               labels = c("2-3cm",
                          "3-4cm",
                          "4-5cm",
                          ">5cm"))) %>%
  group_by(`Barcode ID`, length) %>%   
  summarise(n=n()) %>% 
  mutate(total = sum(n),
         pc = round(n/total*100,1)) 

## 3c. Figure 6: Taxa fragment length size class
SL_taxa_size_class_figure <- 
  ggplot(SL_taxa_size_class,
         aes(x= `Barcode ID`,
             y= pc,
             fill = length))+
  geom_col(position = "dodge")+
  scale_x_discrete(breaks=brk, labels=lbs)+
  scale_fill_manual(name = "size class", 
                    values = taxon_colours)+
  labs(x="Taxon", y = "%NSP")+
  theme_default

SL_taxa_size_class_figure

##save and export
ggsave(plot = SL_taxa_size_class_figure, filename = "output/Figure_6.tiff",
       dpi = 300, width = 20, height = 15, units = "cm")

# 4. Salzgitter taxa and element portion
# read in data
SL_bone_element_taxon <- read_csv("data/SL_bone_element_taxon.csv")

## 4a. Figure 7: Plot taxa and bone element portions----
SL_bone_element_taxon_figure <- 
  SL_bone_element_taxon %>% 
  filter(`Barcode ID` != "unidentified" & `Barcode ID` != "Bovidae/Reindeer") %>%
  ggplot(aes(reorder(`Bone element`, desc(`Bone element`)),`%`,
             fill = `Barcode ID`))+
  geom_col()+
  coord_flip()+
  facet_grid(~`Barcode ID`)+
  scale_fill_manual(values = taxon_colours)+
  labs(x="Element portion", y= "%NSP")+
  scale_y_continuous(limits = c(0,100))+
  facet_grid(~`Barcode ID`)+
  theme(legend.position = "none")+
  theme_default

SL_bone_element_taxon_figure

##save and export
ggsave(plot = SL_bone_element_taxon_figure, filename = "output/Figure_7.tiff",
       dpi = 300, width = 29, height = 21, units = "cm")

# 5. Salzgitter: carnivore and human bone surface modification----

## 5a. Salzgitter bone surface modification general----
SL_BSM_gen <- read_csv("data/SL_BSM_gen.csv")

## 5b. Salzgitter specific carnivore modifications
SL_carniv_mod_specific <- read_csv("data/SL_carniv_mod_specific.csv")

## 5c. Salgitter specific human modifications
SL_hum_mod_specific <- read_csv("data/SL_hum_mod_specific.csv")

# 6. Salzgitter taxa and body size class

# read in data: this combines Staesche and Ruebens data
SL_BSC <- read_csv("data/SL_BSC.csv")

## 6a. Figure 10: BSC vs Barcode_ID faceted----
SL_BSC_fig_facet <- 
  ggplot(SL_BSC,
         aes(x=reorder(`Barcode ID`,
                       desc(`Barcode ID`)), 
             y=`%`, 
             fill = BSC,
             label = `%`))+
  geom_col()+
  scale_x_discrete(breaks=brk, labels=lbs)+
  coord_flip()+
  facet_grid(.~BSC)+
  labs(x="Taxon", y="% NSP")+
  scale_y_continuous(limits = c(0,100))+
  scale_fill_manual(name ="BSC", values=taxon_colours)+
  theme(legend.position = "none")+
  theme_default

#note that the labels were added afterwards

SL_BSC_fig_facet

###save and export
ggsave(plot = SL_BSC_fig_facet, filename = "output/Figure_10.tiff",
       dpi = 300, width = 20, height = 15, units = "cm")
