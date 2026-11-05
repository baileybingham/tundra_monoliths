########################################################
######## VISUALIZING LEAF GROWTH AND CANOPY HEIGHT #####
########          By Bailey Bingham               ######
########           Nov 5th, 2026                 ######
#######################################################

# ==============================================================================
# SET UP
# ==============================================================================

source(file.path("scripts","00_packages.R")) # load the packages listed in the 00_packages.R file under scripts

#Import cleaned above ground data from 2026
leaflg <- read_csv("data/leaflength_clean.csv") ##import cleaned leaf length file
canopy <- read_csv("data/canopyheight_clean.csv", ##import cleaned leaf length file
    col_types = cols(qne_species = col_character())) # read species columns as character, since it is reading it as lgl

#### Graphing leaf length for just Monolith 2, which we know had good graminoid growth ###

M02_LL <-leaflg %>%
  filter (monolith_id == "02") %>% # filter for just monolith 2.
     pivot_longer(                  # Pivot the leaf columns (l1, l2, l3) into a long format
        cols = c(l1, l2, l3), 
        names_to = "leaf_id", 
        values_to = "length_cm"
        ) %>%
  filter(!is.na(length_cm)) # Drop rows where there were no leaves measured. 

ggplot (M02_LL, aes (x=date, y=length_cm, group=leaf_id))+
  geom_line(aes(colour = leaf_id)) +
  geom_point(aes(colour = leaf_id)) +
  facet_grid(fxl_group ~ .) 


canopy <- canopy %>%
  mutate(maxheight = pmax(qne_h, qes_h, qsw_h, qwn_h, na.rm = TRUE))
  
  filter (monolith_id == "02") %>% # filter for just monolith 2.
  pivot_longer(                  
    cols = c(l1, l2, l3), 
    names_to = "leaf_id", 
    values_to = "length_cm"
  ) %>%
  filter(!is.na(length_cm)) # Drop rows where there were no leaves measured. 

