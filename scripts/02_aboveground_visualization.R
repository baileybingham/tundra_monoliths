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
leaflg <- read_csv("data/2026_leaflength_clean.csv") ##import cleaned leaf length file
canopy <- read_csv("data/2026_canopyheight_clean.csv", ##import cleaned leaf length file
    col_types = cols(qne_species = col_character())) # read species columns as character, since it is reading it as lgl

####FOCUS ON GRAMINOIDS AND FORBS, SINCE WE KNOW THEY GREW ###
gram_leaf <- leaflg %>%          
  filter(fxl_group == "graminoid") %>%
  pivot_longer(                  # Pivot the leaf columns (l1, l2, l3) into a long format
    cols = c(l1, l2, l3), 
    names_to = "leaf_id", 
    values_to = "length_cm"
  ) %>%
  filter(!is.na(length_cm)) # Drop rows where there were no leaves measured. 

forb_leaf <- leaflg %>%          
  filter(fxl_group == "forb") %>%
  pivot_longer(                  # Pivot the leaf columns (l1, l2, l3) into a long format
    cols = c(l1, l2, l3), 
    names_to = "leaf_id", 
    values_to = "length_cm"
  ) %>%
  filter(!is.na(length_cm)) # Drop rows where there were no leaves measured. 

gram_canopy <-canopy %>% 
  mutate(maxheight = pmax(qne_h, qes_h, qsw_h, qwn_h, na.rm = TRUE)) %>%
  filter(fxl_group == "graminoid")

forb_canopy <-canopy %>% 
  mutate(maxheight = pmax(qne_h, qes_h, qsw_h, qwn_h, na.rm = TRUE)) %>%
  filter(fxl_group == "forb")

#### Graphing graminoid leaf lengths  ###

ggplot (gram_leaf, aes (x=date, y=length_cm, group=monolith_id))+
  geom_line(aes(colour = monolith_id)) +
  geom_point(aes(colour = monolith_id)) +
  facet_grid(treatment ~ .) +
  scale_x_date(
    date_breaks = "5 days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#just monolith 2
M02_gram_canopy <- gram_canopy%>%
  filter(monolith_id =="M_02")

ggplot (M02_gram_canopy, aes (x=date, y=maxheight))+
  geom_line() +
  geom_point() +
  scale_x_date(
    date_breaks = "5 days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Graphing forb leaf lengths  ###

ggplot (leaflg, aes (x=date, y=l1, group=monolith_id))+
  geom_line() +
  geom_point() +
  facet_grid(treatment ~ fxl_group) +
  scale_x_date(
    date_breaks = "5 days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
