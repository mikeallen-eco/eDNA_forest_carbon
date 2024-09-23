### R Script used to analyze data for the article:
  ### "eDNA offers opportunities for improved biodiversity monitoring within forest carbon markets"

##### NOTE #####
# This R script is used for summarizing the literature reviewed in Allen et al. (2024) 
# ("eDNA offers opportunities for improved biodiversity monitoring within forest carbon markets") 
# and recreating all figures and analyses. It depends on 4 databases formatted as tab-separated text files:
# 1. eDNA_assessment_literature_database_20240923.tsv, 
# 2. FC_biodiversity_literature_database_20240923.tsv, 
# 3. FC_projects_database_20240923.tsv, and
# 4. all_FC_projects_with_areas_database_20240923.tsv
# Those data files are stored in the /data folder along with 3 metadata text files describing the 
# dataset and the data contained in each column. The script was created using using R version 4.4.0 
# and is subdivided into 12 sections (see Table of Contents below). After Section 1 is run to load 
# the packages and data, any section should be able to run independently of the others.
#####


##### TABLE OF CONTENTS #####

# SECTION 1: LOAD PACKAGES AND DATA
# SECTION 2: TALLY STATS FOR FIRST SECTION OF RESULTS
# SECTION 3: TALLY STATS FOR RESULTS - TAXONOMIC SCOPE
# SECTION 4: TALLY STATS FOR RESULTS - GEOGRAPHIC DISTRIBUTION
# SECTION 5: TALLY STATS FOR RESULTS - METHODOLOGICAL RIGOR & TRANSPARENCY
# SECTION 6: CREATE FIGURE 1 (LEFT-MIDDLE PANEL) - MAP THE FC PROJECTS
# SECTION 7: CREATE FIGURE 1 (RIGHT-MIDDLE PANEL) - MAP THE eDNA LITERATURE
# SECTION 8: CREATE FIGURE 1 (BOTTOM PANELS) - METHODOLOGICAL RIGOR & TRANSPARENCY
# SECTION 9: CREATE FIGURE 2 IN MANUSCRIPT (TAXA MONITORED)
# SECTION 10: CREATE FIGURE 3 IN MANUSCRIPT (NO. TAXA VS. REGIONS/METHODS)
# SECTION 11: CREATE FIGURE 4 - COMPARING RIGOR & TRANSPARENCY ACROSS ORGANIZATIONS
# SECTION 12: CREATE SUPPLEMENTARY FIGURE 3 (VERTEBRATES IN eDNA STUDIES)
# SECTION 13: PACKAGES AND VERSION INFORMATION

##### SECTION 1: LOAD PACKAGES AND DATA #####

# load libraries (see Section 13 at bottom of script for version information)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(data.table)
library(sf)
library(rnaturalearth)
'%notin%' <- Negate('%in%')
if(!dir.exists("figures")){dir.create("figures")} # create "figures" folder if it doesn't exist

# define locations of datafiles
eDNAlit_path <- "data/eDNA_assessment_literature_database_20240923.tsv" # eDNA articles
FCbiodiv_path <- "data/FC_biodiversity_literature_database_20240923.tsv" # forest carbon & biodiversity articles
FCproj <- "data/FC_projects_database_20240923.tsv" # forest carbon project info
FCall <- "data/all_FC_projects_with_areas_database_20240923.tsv"

# load forest carbon / biodiversity literature
fb <- fread(FCbiodiv_path, sep = "\t", header = T)

# load & format eDNA assessment literature data
e <- fread(eDNAlit_path) %>%
  mutate(vert = case_when(mammals==1|birds==1|amphibians==1|reptiles==1|fish==1 ~ 1,
                          TRUE ~ 0),
         year = as.numeric(year)) %>%
  rowwise() %>%
  mutate(numtaxgroups = sum(c_across(mammals:prokaryotes))) %>%
  ungroup()

# load & format forest carbon project data
p <- fread(FCproj) %>%
  # remove unneeded fields
  select(-taxa_notes, -methods_notes, -data_notes, -notes,
         -Proponent, -Distinctions, -Status) %>%
  # tally taxa and methods
  rowwise() %>%
  mutate(numtaxgroups = sum(c_across(mammals:prokaryotes)),
         numanimalgroups = sum(c_across(c(mammals, birds, amphibians, 
                                          reptiles, fish, invertebrates))),
         nummethods = sum(c_across(camtrap:incidental_obs))) %>%
  ungroup()

# load larger database of FC projects (including those not verifying biodiversity co-benefits)
pa <- fread(FCall) 


##### SECTION 2: TALLY STATS FOR FIRST SECTION OF RESULTS #####

table(fb$topic) # tally the various topics of the forest carbon / biodiversity

# summary statistics for the larger pool of 1323 forest carbon projects
nrow(pa) # 1323 total FC projects
sum(pa$area_ha) # 76306414 total hectares
sum(substr(pa$fcid,1,3)=="CCB"|substr(pa$fcid,1,2)=="PV"|
      substr(pa$fcid,1,2)=="GS"|substr(pa$fcid,1,2)=="SD") # 451 biodiversity-verifying FC projects
sum(pa[substr(pa$fcid,1,3)=="CCB"|substr(pa$fcid,1,2)=="PV"|
             substr(pa$fcid,1,2)=="GS"|substr(pa$fcid,1,2)=="SD",]$area_ha) 
  # 33011283 ha for biodiversity-verifying FC projects
nrow(p) # 129 FC projects included in review
sum(p$area_ha) # 6982585 ha for FC projects included in review

table(e$method) # tally the studies involving DNA/RNA metabarcoding, single-species approaches, etc.

table(e$hab_gen) # tally the habitats sampled by the eDNA studies 

# impacts evaluated in eDNA studies in forest habitats 
# (note: 'invasive species' and 'species interaction' lumped into one category)
eforest <- filter(e, grepl(x = hab_gen, pattern = "forest"))
table(eforest$impact)/nrow(eforest)


##### SECTION 3: TALLY STATS FOR RESULTS - TAXONOMIC SCOPE #####

# no. FC projects with some form of formal animal monitoring
(numanim <- p %>% 
   filter(numanimalgroups>0) %>%
   nrow())
numanim/nrow(p)
table(p$animal_mon) # 97 projects have some animal monitoring (i.e., 75% of 129)
# note: 4 projects don't say which animals, hence the difference between numanimals and table()

# get % of FC projects that monitor each taxonomic group
pa <- p %>%
  filter(animal_mon != "none")
someanimal <- nrow(pa)
sum(pa$birds)/nrow(pa)
sum(pa$mammals)/nrow(pa)
sum(pa$reptiles)/nrow(pa)
sum(pa$amphibians)/nrow(pa)
sum(pa$invertebrates)/nrow(pa)
sum(pa$fish)/nrow(pa)
sum(pa$numanimalgroups==0) # no specific animal taxa reported
sum(pa$numanimalgroups==0)/nrow(p) # % no specific animal taxa reported
summary(pa[pa$numanimalgroups>0,]$numanimalgroups)
hist(pa[pa$numanimalgroups>0,]$numanimalgroups)
summary(pa[pa$numtaxgroups>0,]$numtaxgroups)
hist(pa[pa$numtaxgroups>0,]$numtaxgroups)
summary(pa[pa$nummethods>0,]$nummethods)
hist(pa[pa$nummethods>0,]$nummethods)
length(pa[pa$nummethods>0,]$nummethods)

sum(p$plants)/nrow(p) # % FC projects discussing native forest community monitoring

# get % of eDNA studies that monitor each taxonomic group (terrestrial studies only)
eterr <- e %>%
  filter(grepl("forest", hab_gen) |
           grepl("grassland", hab_gen) |
           grepl("terrestrial", hab_gen) |
           grepl("cave", hab_gen))
100*sum(eterr$fungi)/nrow(eterr) # 51%
100*sum(eterr$prokaryotes)/nrow(eterr) # 40%
100*sum(eterr$invertebrates)/nrow(eterr) # 30%
100*sum(eterr$protists)/nrow(eterr) # 9%
100*sum(eterr$plants)/nrow(eterr) # 7%
100*sum(eterr$mammals)/nrow(eterr)
100*sum(eterr$reptiles)/nrow(eterr)
100*sum(eterr$birds)/nrow(eterr)
100*sum(eterr$fish)/nrow(eterr)
100*sum(eterr$amphibians)/nrow(eterr)


##### SECTION 4: TALLY STATS FOR RESULTS - GEOGRAPHIC DISTRIBUTION #####

# where are the FC projects that verify biodiversity
table(p$region)
table(p$region) / length(p$region)

# where are FC projects
table(p$trop_subtrop) # 111/129 = 86% in tropics or sub-tropics
table(p[p$trop_subtrop==1,]$region)/nrow(p) # % of (sub)tropical projects in each region
table(p[p$trop_subtrop==0,]$region)/nrow(p) # % of temperate projects in each region

# where are studies related to FC and biodiversity
table(fb$trop_subtrop) # 34/41 with sufficient location information = 83%

# where are studies involving (e)DNA for biodiversity monitoring
table(e$trop_sub) # 103/324 = 32% are tropical or subtropical
100*table(e[e$trop_sub==1,]$region)/nrow(e) # % of (sub)tropical projects in each region
100*table(e[e$trop_sub==0,]$region)/nrow(e) # % of temperate projects in each region


##### SECTION 5: TALLY STATS FOR RESULTS - METHODOLOGICAL RIGOR & TRANSPARENCY #####

# how many FC projects performed animal monitoring (formal and/or incidental)
table(p$animal_mon) 
# none      yes, incidental yes, methods unknown    yes, sample based 
# 32                   21                    6                   70 

# number of the sample-based FC project monitoring efforts that provided sample sizes
table(p$sample_based_n_provided) # 48/70 = 69%
table(p$sample_based_n_provided)/nrow(p) # 37% of all projects

# number of FC projects with some animal monitoring = 97
# 70/97 = 72% of animal-monitoring studies performed formal sample-based monitoring
# 21/97 = 22% used only unstandardized monitoring
# 6/97 = 6% didn't provide enough information to determine
(sum(p$formal_obs != 0))/nrow(p) # sample-based visual animal monitoring (transects, etc.); 49%
(sum(p$camtrap != 0))/nrow(p) # camera trap animal monitoring; 15%
(sum(p$traps != 0))/nrow(p) # trap-based animal monitoring; 8%
(sum(p$accous != 0))/nrow(p) # acoustic animal monitoring; 2%
(sum(p$DNA != 0))/nrow(p) # DNA animal monitoring; 2%
# number of survey methods used in FC projects: see section for creating Fig. 3

# mean sample sizes of FC project monitoring where reported
mean(as.numeric(p$formal_obs_n[as.numeric(p$formal_obs_n)>0]), na.rm = T) # 21 for visual surveys
mean(as.numeric(p$camtrap_n[as.numeric(p$camtrap_n)>0]), na.rm = T) # 27 for camera traps
mean(as.numeric(p$traps_n[as.numeric(p$traps_n)>0]), na.rm = T) # 34 for conventional traps

# number of FC projects that cite data occurring in unavailable reports
table(p$mon_results_in_unavail.report) # 28 projects

# average sample size of eDNA studies
mean(as.numeric(e$numsamp), na.rm = T)
summary(as.numeric(e[e$numsamp>1,]$numsamp), na.rm = T) # the one study where n = 1 was not considered "sample-based"

# tally substrates sampled in eDNA studies
tally(e %>%
        filter(hab_gen %in% c("forest", "grassland", "other terrestrial", 
                              "cave", "forest + freshwater", "cave", 
                              "grassland + freshwater")) %>%
        group_by(substrate))
# soil = 83/134 = 62%
# vegetation = 4/134 = 3%
# water = 2/134 = 1%
# air = 1/134 = 1%
# trap-collected invertebrates ('whole organism') = 24/134 = 18%
# organism diet = 14/134 = 10%
# organism microbiome = 13/134 = 10%

# number eDNA studies with data available
table(e$data_share)
234/nrow(e) # 234/324 = 72%

# where are eDNA data archived
table(e$raw_data_archive)
table(e$procdata_archive)


##### SECTION 6: CREATE FIGURE 1 (LEFT-MIDDLE PANEL) - MAP THE FC PROJECTS #####

# load world map
world <- rnaturalearth::ne_countries(returnclass = "sf")

# summarize count of FC projects by country, fixing name of Tanzania
pcountry <- p %>%
  group_by(country_area) %>%
  summarize(count = length(country_area)) %>%
  mutate(admin = case_when(country_area == "Tanzania" ~ "United Republic of Tanzania",
                           country_area == "Congo the Democratic Republic of the" ~
                             "Democratic Republic of the Congo",
                           country_area == "United States" ~ "United States of America",
                           country_area == "Timor-Leste" ~ "East Timor",
                           TRUE ~ country_area))

# join FC project count-by-country data to world map
worldp <- world %>%
  left_join(pcountry, by = c("admin"))

# make FC project lat/lon data into sf object for plotting points
psf <- p %>%
  select(ID, lat_approx, lon_approx) %>%
  st_as_sf(coords = c("lon_approx", "lat_approx"),
           crs = 4326)

# Deactivate s2 (for plotting)
sf::sf_use_s2(FALSE)

# make world map of FC projects
worldp %>%
  ggplot +
  geom_sf(aes(fill = count)) +
  geom_sf(aes(), 
          shape = "x", data = psf, color = "red") +
  scale_fill_viridis_b() +
  theme_bw() +
  labs(x = "", y = "", fill = "No.\nprojects") +  
  coord_sf(crs = st_crs("ESRI:54030"))

# Save Figure 1 (left middle panel)
# ggsave("figures/Figure1_left_middle_panel_FCmap.png", width = 6, height = 6, dpi = 600)


##### SECTION 7: CREATE FIGURE 1 (RIGHT-MIDDLE PANEL) - MAP THE eDNA LITERATURE #####

# load world map
world <- rnaturalearth::ne_countries(returnclass = "sf")

# summarize count of eDNA studies by country, fixing name of Tanzania & removing large-scale
ecountry <- e %>%
  mutate(admin = case_when(country_area == "Tanzania" ~ "United Republic of Tanzania",
                           country_area == "USA" ~ "United States of America",
                           country_area == "Hawaii (USA)" ~ "United States of America",
                           country_area == "Hawaii" ~ "United States of America",
                           country_area == "Mayotte (France)" ~ "France",
                           country_area == "UK" ~ "United Kingdom", 
                           country_area == "Czech Republic" ~ "Czechia", 
                           country_area == "Curacao" ~ "Netherlands",
                           TRUE ~ country_area)) %>%
  filter(country_area %notin% c("Amazon basin", "Atlantic Ocean",
                                "Bay of Biscay", "S. Atlantic / Antarctic Ocean",
                                "Mediterranean Sea", "Mekong River",
                                "Multiple", "South Pacific")) %>%
  group_by(admin) %>%
  summarize(count = length(admin))

# join eDNA study count-by-country data to world map
worlde <- world %>%
  left_join(ecountry, by = c("admin"))

# make eDNA study lat/lon data into sf object for plotting points
esf <- e %>%
  filter(!is.na(lat_approx),
         lat_approx != "NA") %>%
  select(paperid, lat_approx, lon_approx) %>%
  st_as_sf(coords = c("lon_approx", "lat_approx"),
           crs = 4326)

# Deactivate s2
sf::sf_use_s2(FALSE)

# make world map of eDNA studies
worlde %>%
  ggplot +
  geom_sf(aes(fill = count)) +
  geom_sf(aes(), 
          shape = "x", data = esf, color = "red") +
  scale_fill_viridis_b() +
  theme_bw() +
  labs(x = "", y = "", fill = "No.\nstudies") +  
  coord_sf(crs = st_crs("ESRI:54030"))

# Save Figure 1 (right middle panel)
# ggsave("figures/Figure1_right_middle_panel_eDNAmap.png", width = 6, height = 6, dpi = 600)


##### SECTION 8: CREATE FIGURE 1 (BOTTOM PANELS) - METHODOLOGICAL RIGOR & TRANSPARENCY #####

# gather rigor and transparency statistics for FC projects
table(p$animal_mon)/length(p$animal_mon) # 54% of all projects have replicated survey design
table(p$sample_based_n_provided) / nrow(p) # 37% have replicated design AND report sample size
tot_samps <- as.numeric(p$camtrap_n) + as.numeric(p$traps_n) + as.numeric(p$formal_obs_n)
length(tot_samps[tot_samps > 10 & !is.na(tot_samps)])/nrow(p) # 22% have those AND n > 10
table(p$raw_data_avail) / nrow(p) # 4% make underlying data available

# gather rigor and transparency statistics for eDNA studies
323/nrow(e) # 99.6% have replicated survey design (all except the single sample study)
sum(!is.na(as.numeric(e$numsamp)))/nrow(e) # 99.1% have replicated design AND report sample size
length(e[!is.na(e$numsamp) & e$numsamp > 10,]$numsamp) / nrow(e) # 94% have those AND n > 10
sum(e$data_share %in% 1) / nrow(e) # 72% make underlying data available 

# manually make data table for plotting based on those statistics
rigor_dat <- data.frame(label = rep(c(
  "Replicated\nsurvey design",
  "Sample size\nreported",
  "Sample\nsize >10",
  "Data\navailable"), 2),
  type = c(rep("FC", 4), rep("eDNA", 4)),
  val = c(54, 37, 22, 4, 99.7, 99, 94, 74),
  order = c(1, 2, 3, 4, 5, 6, 7, 8)) %>%
  mutate(label = forcats::fct_reorder(label, order, .desc = T))

# create rigor and transparency plot for FC projects
rigor_dat %>%
  filter(type == "FC") %>%
  ggplot() +
  geom_col(aes(x = val, y = label), fill = "black") +
  scale_x_continuous(limit = c(0,100)) +
  theme_bw() +
  theme(text = element_text(size = 30)) +
  labs(x = "% of projects", y = "")

# Save Figure 1 (left bottom panel)
# ggsave("figures/Figure1_left_bottom_panel_FCrigor.png", width = 7, height = 5, dpi = 600)

# create rigor and transparency plot for eDNA studies
rigor_dat %>%
  filter(type == "eDNA") %>%
  ggplot() +
  geom_col(aes(x = val, y = label), fill = "black") +
  scale_x_continuous(limit = c(0,100)) +
  theme_bw() +
  theme(text = element_text(size = 30)) +
  labs(x = "% of studies", y = "")

# Save Figure 1 (right bottom panel)
# ggsave("figures/Figure1_right_bottom_panel_eDNArigor.png", width = 7, height = 5, dpi = 600)


##### SECTION 9: CREATE FIGURE 2 IN MANUSCRIPT (TAXA MONITORED) #####

# format data for terrestrial eDNA studies
e.taxplot.terr <- e %>%
  filter(hab_gen %in% c("forest", "grassland", "other terrestrial", 
                        "cave", "forest + freshwater", "cave", 
                        "grassland + freshwater")) %>%
  select(mammals:prokaryotes) %>%
  pivot_longer(cols = mammals:prokaryotes) %>% 
  mutate(group = "eDNA studies") %>%
  group_by(group, name) %>%
  summarize(num = sum(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(name = fct_reorder(name, num),
         pct = 100*num/134) 

# format data for FC projects and combine with eDNA data for plotting
taxplot <- p %>%
  select(mammals:prokaryotes) %>%
  pivot_longer(cols = mammals:prokaryotes) %>% 
  mutate(group = "FC projects") %>%
  group_by(group, name) %>%
  summarize(num = sum(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(name = fct_reorder(name, num),
         pct = 100*num/nrow(p)) %>%
  bind_rows(e.taxplot.terr) %>%
  mutate(groupord = c(rep(1,10),rep(2,10)),
         group = fct_reorder(group, groupord),
         imagew = pct * 135 / 73.6)

# create Figure 2
taxplot %>%
  ggplot() +
  geom_col(aes(x = pct, y = name), fill = "black") +
  facet_wrap(~group) +
  labs(x = "% of projects or studies", y = "") +
  theme_bw() +
  theme(text = element_text(size = 14,
                            color = "black"))
# save Figure 2
# ggsave("figures/Figure2.png", dpi = 600, width = 6, height = 4)


##### SECTION 10: CREATE FIGURE 3 IN MANUSCRIPT (NO. TAXA VS. REGIONS/METHODS) #####

# format data for plotting number of taxa vs. number of primer regions
b.reg <- e %>%
  select(numtaxgroups, numregions, method) %>%
  mutate(type = "eDNA assessment",
         nummethods = 1) %>%
  filter(!is.na(numregions) & numregions != "NA") %>%
  filter(grepl(method, pattern = "metabar"))

(no_regions <- b.reg %>%
    ggplot() +
    geom_boxplot(aes(x = as.factor(numregions), y = numtaxgroups),
                 outlier.alpha = 0, color = "firebrick") +
    geom_point(aes(x = jitter(as.numeric(as.factor(numregions)), 0.3), y = numtaxgroups),
               color = "firebrick", 
               shape = 21, alpha = 0.75, size = 3) +
    scale_y_continuous(limits = c(0,7), breaks = 1:7) +
    labs(x = "No. genomic regions", y = "No. taxonomic groups") +
    theme_classic() +
    theme(text = element_text(size = 11)) +
    guides(color = "none") +
    theme(text = element_text(size = 14)))

# format data for plotting field methods vs. no. taxa, eDNA studies
e.meth <- e %>%
  mutate(nummethods = case_when(substrate %in% c("organism diet + vegetation", 
                                                 "organism diet + whole organism",
                                                 "sediment + organism diet",
                                                 "sediment + organism microbiome",
                                                 "sediment + whole organism",
                                                 "vegetation + organism diet",
                                                 "sediment + water",
                                                 "surface scrape + water",
                                                 "water + organism microbiome",
                                                 "water + sediment") ~ 2,
                                TRUE ~ 1))

# check "number of methods" classification
table(e.meth$substrate)
table(e.meth$nummethods)

# combine eDNA plot data with forest carbon project data
b <- e.meth %>%
  select(numtaxgroups, nummethods) %>%
  mutate(type = "eDNA") %>%
  bind_rows(filter(mutate(select(p, numtaxgroups, nummethods, animal_mon), 
                          type = "FCO",
                          nummethods = nummethods+1), animal_mon != "yes, methods unknown")) %>%
  filter(numtaxgroups>0) %>%
  select(-animal_mon)

# create Figure 3 (right panel)
(no_methods <- b %>%
    ggplot() +
    geom_boxplot(aes(x = as.factor(nummethods), y = numtaxgroups, 
                     color = type),
                 position = position_dodge(width = 1),
                 outlier.alpha = 0) +
    # needed to split up points due to jittering making x axis not factor and not dodge-able
    geom_point(aes(x = jitter(as.numeric(as.factor(nummethods)), 0.3)+.25, y = numtaxgroups,
                   color = type),
               position = position_dodge(width = 1), 
               shape = 21, alpha = 0.75, size = 3,
               data = filter(b, type == "FCO", nummethods <3)) +
    geom_point(aes(x = jitter(as.numeric(nummethods), 0.3), y = numtaxgroups,
                   color = type),
               position = position_dodge(width = 1), 
               shape = 21, alpha = 0.75, size = 3,
               data = filter(b, type == "FCO", nummethods >2)) +
    geom_point(aes(x = jitter(as.numeric(as.factor(nummethods)), 0.3)-.25, y = numtaxgroups,
                   color = type),
               position = position_dodge(width = 1), 
               shape = 21, alpha = 0.75, size = 3,
               data = filter(b, type == "eDNA", nummethods <3)) +
    scale_color_manual(values = c("firebrick", "olivedrab")) +
    scale_y_continuous(limits = c(0,7), breaks = 1:7) +
    labs(x = "No. field methods", y = "No. taxonomic groups") +
    theme_classic() +
    theme(text = element_text(size = 11)) +
    guides(color = "none") +
    theme(text = element_text(size = 14)))

# combine plots into a single plot with 2 panels
gg3 <- gridExtra::arrangeGrob(no_regions, no_methods, ncol = 2)
plot(gg3)

# save Figure 3
# ggsave("figures/Figure3.png", plot = gg3, dpi = 600, width = 6.5, height = 3)


##### SECTION 11: CREATE FIGURE 4 - COMPARING RIGOR & TRANSPARENCY ACROSS ORGANIZATIONS #####

# compare rigor of projects in 3 organizations

# subset the 3 main biodiversity co-benefit verification programs into separate databases
p_ccb <- p %>% filter(program == "CCB")
p_pv <- p %>% filter(program == "PV")
p_gs <- p %>% filter(program == "GS")

# calculate methodology stats for CCB projects
table(p_ccb$animal_mon)/length(p_ccb$animal_mon) # 85% with any animal mon, 76.25% with replicated
table(p_ccb$sample_based_n_provided) / nrow(p_ccb) # 57.5% have sample size
tot_samps_ccb <- as.numeric(p_ccb$camtrap_n) + as.numeric(p_ccb$traps_n) + as.numeric(p_ccb$formal_obs_n)
tot_samps_ccb <- tot_samps_ccb[tot_samps_ccb > 0]
tot_samps_ccb <- tot_samps_ccb[!is.na(tot_samps_ccb)]
sum(tot_samps_ccb >10) / nrow(p_ccb) # 35% have n>10
sum(p_ccb$raw_data_avail %in% "Y") / nrow(p_ccb)# 6.25% have data available

# calculate methodology stats for PlanVivo projects
table(p_pv$animal_mon)/length(p_pv$animal_mon) # 71.4% with any animal mon, 26.9% with replicated (additional 21.4 with methods unknown)
table(p_pv$sample_based_n_provided) / nrow(p_pv) # 7.7% have sample size
tot_samps_pv <- as.numeric(p_pv$camtrap_n) + as.numeric(p_pv$traps_n) + as.numeric(p_pv$formal_obs_n)
length(tot_samps_pv[tot_samps_pv > 0 & !is.na(tot_samps_pv)]) / nrow(p_pv) # 7.7% have n>10
# 0% have data available

# calculate methodology stats for GSIR projects
table(p_gs$animal_mon)/length(p_gs$animal_mon) # 40% with any animal mon, 10% with replicated (additional 15% with methods unknown)
table(p_gs$sample_based_n_provided) / nrow(p_gs) # 0% provide sample size
# 0% have n > 10
# 0% have data available

# manually make data table for plotting based on those statistics
fco_org_dat <- data.frame(label = rep(c(
  "Any animal\nmonitoring",
  "Replicated\nsurvey design",
  "Sample size\nreported",
  "Sample\nsize >10",
  "Data\navailable"), 3),
  type = c(rep("CCB", 5), rep("PlanVivo", 5), rep("Gold Standard", 5)),
  val = c(85, 76.25, 57.5, 35, 6.25, 
          71.4, 26.9, 7.7, 7.7, 0,
          40, 10, 0, 0, 0),
  order = 1:15,
  order2 = c(rep(1,5), rep(2,5), rep(3,5))) %>%
  mutate(label = forcats::fct_reorder(label, order, .desc = T),
         type = forcats::fct_reorder(type, order2, .desc = F))

# plot Figure 4
fco_org_dat %>%
  ggplot() +
  geom_col(aes(x = val, y = label), fill = "black") +
  facet_wrap(~type) +
  scale_x_continuous(limit = c(0,100)) +
  theme_bw() +
  theme(text = element_text(size = 16, color = "black")) +
  labs(x = "% of projects", y = "")

# save Figure 4
# ggsave("figures/Figure4.png", width = 9, height = 4, dpi =600)


##### SECTION 12: CREATE SUPPLEMENTARY FIGURE 3 (VERTEBRATES IN eDNA STUDIES) #####

# does representation of vertebrates change over time? (glm)
mod <- glm(vert ~ I(year-2012), data = e, family = "binomial")
summary(mod)

# make Fig S3 (left panel) - bar chart of % vertebrates over time
(vert <- e %>%
    group_by(year) %>%
    summarize(numvert = sum(vert),
              totpubs = length(vert),
              nonvert = totpubs-numvert,
              pctvert = 100*numvert/totpubs,
              .groups = "drop") %>%
    filter(year > 2016) %>%
    ggplot() +
    geom_col(aes(x = year, y = pctvert)) +
    geom_rect(aes(xmin = 2012.55, xmax = 2016.45, ymin = 0, ymax = 6.67), fill = "gray35") +
    geom_text(aes(x = year, y = 0.7, label = totpubs), color = "darkgray", size = 3) +
    geom_text(aes(x = 2014.5, y = 0.7, label = "n = 15"), color = "darkgray", size = 3) +
    theme_bw() +
    theme(text = element_text(size = 14)) +
    labs(x = "", y = "% of eDNA impact assessments\ninvolving vertebrates") + 
    scale_y_continuous(limits = c(0,35))
)

# make Fig S3 (right panel) - model predicted change of % vertebrates over time
(vertmodplot <- data.frame(
  year =  e$year,
  fit = predict(mod, type = "response"),
  lcl = plogis(predict(mod)-2*predict(mod, se.fit = T)$se.fit),
  ucl = plogis(predict(mod)+2*predict(mod, se.fit = T)$se.fit)) %>%
    distinct() %>%
    ggplot() +
    geom_point(aes(x = year, y = 100*fit), size = 4) +
    geom_errorbar(aes(x = year, ymin = 100*lcl, ymax = 100*ucl), linewidth = 1, width = 0) +
    scale_x_continuous(breaks = seq(2013, 2023, by = 2)) +
    theme_bw() +
    theme(text = element_text(size = 14)) +
    labs(x = "", y = "% of eDNA impact assessments\ninvolving vertebrates") +
    scale_y_continuous(limits = c(0,35)) + 
    labs(y = ""))

# arrange plots into 2 panels
gg2 <- gridExtra::arrangeGrob(vert, vertmodplot, ncol = 2)
plot(gg2)

# save Supplementary Figure 3
# ggsave("figures/FigureS3.png", plot = gg2, dpi = 600, width = 8, height = 5)


##### SECTION 13: PACKAGES AND VERSION INFORMATION #####

# Results of the sessionInfo() command:

# R version 4.4.0 (2024-04-24)
# Platform: aarch64-apple-darwin20
# Running under: macOS Sonoma 14.4
# 
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
# LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# time zone: America/New_York
# tzcode source: internal
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] sf_1.0-16         data.table_1.15.4 forcats_1.0.0     ggplot2_3.5.1     tidyr_1.3.1      
# [6] dplyr_1.1.4      
# 
# loaded via a namespace (and not attached):
# [1] utf8_1.2.4          generics_0.1.3      class_7.3-22        KernSmooth_2.23-24  digest_0.6.35      
# [6] magrittr_2.0.3      evaluate_0.24.0     grid_4.4.0          rnaturalearth_1.0.1 fastmap_1.2.0      
# [11] jsonlite_1.8.8      e1071_1.7-14        DBI_1.2.3           gridExtra_2.3       httr_1.4.7         
# [16] purrr_1.0.2         fansi_1.0.6         viridisLite_0.4.2   scales_1.3.0        textshaping_0.4.0  
# [21] codetools_0.2-20    cli_3.6.2           rlang_1.1.4         units_0.8-5         munsell_0.5.1      
# [26] withr_3.0.0         yaml_2.3.8          tools_4.4.0         colorspace_2.1-0    vctrs_0.6.5        
# [31] R6_2.5.1            proxy_0.4-27        lifecycle_1.0.4     classInt_0.4-10     ragg_1.3.2         
# [36] pkgconfig_2.0.3     terra_1.7-78        pillar_1.9.0        gtable_0.3.5        glue_1.7.0         
# [41] Rcpp_1.0.12         systemfonts_1.1.0   xfun_0.44           tibble_3.2.1        tidyselect_1.2.1   
# [46] rstudioapi_0.16.0   knitr_1.47          farver_2.1.2        htmltools_0.5.8.1   rmarkdown_2.27     
# [51] labeling_0.4.3      compiler_4.4.0 