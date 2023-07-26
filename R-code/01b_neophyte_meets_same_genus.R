source("R-code/00_preamble.R")

# aim ---------------------------------------------------------------------

# quantify how often a neophyte meets the same genus.
# a species being a neophyte in a region, may hybridize preferentially if 
# there is also the same genus present in the region newly colonized. 

# read and harmonize glonaf with powo -------------------------------------

# read glonaf data
glonaf_sp <- read_delim("Data/GLONAF/Taxon_x_List_GloNAF_vanKleunenetal2018Ecology.csv", 
                        delim = "\t", escape_double = FALSE, 
                        locale = locale(encoding = "utf-16"), 
                        trim_ws = TRUE)

# load harmonized data: here the glonaf genera match the accepted ones in powo
glonaf_higher_taxa_wcvp <- read_csv("Data/glonaf_higher_taxa_wcvp.csv")

# join with glonaf_sp
glonaf_sp <- left_join(glonaf_higher_taxa_wcvp %>% 
                         dplyr::select(standardized_name, genus_acc = genus), 
                       glonaf_sp)

# join with distribution data ---------------------------------------------

# read region ids for l3 areas
glonaf_reg <- read_csv("Data/GLONAF/Region_GloNAF_vanKleunenetal2018Ecology.csv")

glonaf_full <- full_join(glonaf_sp, glonaf_reg)

glonaf_tdwg3 <- glonaf_full %>% 
  dplyr::select(standardized_name, genus_acc, tdwg3) %>% 
  distinct

# there are 124,000 colonization events that led to a neophyte tdwg3 combination

# read powo data ----------------------------------------------------------

# read species list
kew_sp <- fread("Data/kew/checklist_names.txt", encoding = "UTF-8")

# kew species data
kew_gen <- kew_sp %>% 
  filter(taxon_rank == "Genus") %>% 
  filter(taxon_status == "Accepted") %>% 
  dplyr::select(genus, plant_name_id) %>% 
  distinct

# kew data on distribution
kew_dis <- fread("Data/kew/checklist_distribution.txt")
head(kew_dis)
nrow(kew_dis)

kew_gen_dis <- left_join(kew_gen, kew_dis)

kew_gen_dis <- kew_gen_dis %>% 
  filter(introduced == 0) %>% 
  dplyr::select(genus, plant_name_id, area_code_l3)


# join powo and glonaf data -----------------------------------------------
glonaf_kew_dis_comp <- left_join(glonaf_tdwg3, kew_gen_dis, 
                                 by = c("genus_acc" = "genus", 
                                        "tdwg3" = "area_code_l3"))

glonaf_kew_dis_comp %>% nrow

# get rid of the numeric tdwg3 regions, they are major ones
glonaf_kew_dis_comp <- glonaf_kew_dis_comp %>% 
  filter(!str_detect(tdwg3,"^\\s*[0-9]*\\s*$")) %>% 
  rename(tdwg3_neopyhte_occ = tdwg3, tdwg3_native_occ = plant_name_id)

# save this data frame, is useful for the analysis on how many of the genera
# with both neophytes and hybrids contribute to naturalizatione events.
write_csv(glonaf_kew_dis_comp, "Data/glonaf_kew_dis_comp.csv")

# how many species in this rather final df
glonaf_kew_dis_comp %>% dplyr::select(standardized_name) %>% distinct %>% nrow

# what's the overall rate
# full number of neophyte events
glonaf_kew_dis_comp %>% nrow

# full number of neophytes event with potential to meet species of same genus
glonaf_kew_dis_comp %>% filter(!is.na(tdwg3_native_occ)) %>% nrow

# in about 50% percent of the naturalization events, the neophyte could have
# met a species of the same genus
glonaf_kew_dis_comp %>% filter(!is.na(tdwg3_native_occ)) %>% nrow/
glonaf_kew_dis_comp %>% nrow


# how many species never met the same genus?
no_match <- glonaf_kew_dis_comp %>% 
  group_by(standardized_name) %>% 
  summarize(no_nats_events = n_distinct(tdwg3_neopyhte_occ), 
            no_nat_events_match = sum(!is.na(tdwg3_native_occ))) %>% 
  filter(no_nat_events_match == 0)

yes_match <- glonaf_kew_dis_comp %>% 
  group_by(standardized_name) %>% 
  summarize(no_nats_events = n_distinct(tdwg3_neopyhte_occ), 
            no_nat_events_match = sum(!is.na(tdwg3_native_occ))) %>% 
  filter(no_nat_events_match >= 1)

nrow(yes_match)/11987 
# 73% of the species found at least one time a match, 27% of neophytes never found
# the same genus.
