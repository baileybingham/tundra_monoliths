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
leaflg26 <- read_csv("data/2026_leaflength_clean.csv") ##import cleaned leaf length file
canopy26 <- read_csv("data/2026_canopyheight_clean.csv", ##import cleaned leaf length file
    col_types = cols(qne_species = col_character())) # read species columns as character, since it is reading it as lgl

#Import cleaned above ground data from 2025
gramleaf25 <- read_csv("data/2025_gramleaflength_clean.csv")%>% ##import cleaned leaf length file
  rename(length_cm = avg_height_cm)
head(gramleaf25)

#### 2026 FOCUS ON GRAMINOIDS AND FORBS, SINCE WE KNOW THEY GREW ###
gramleaf26 <- leaflg26 %>%          
  filter(fxl_group == "graminoid") %>%
  pivot_longer(                  # Pivot the leaf columns (l1, l2, l3) into a long format
    cols = c(l1, l2, l3), 
    names_to = "leaf_id", 
    values_to = "length_cm"
  ) %>%
  filter(!is.na(length_cm))%>%  # Drop rows where there were no leaves measured. 
  select (date, sim_date, exp_day, monolith_id, treatment, leaf_id, length_cm)

forbleaf26 <- leaflg26 %>%          
  filter(fxl_group == "forb") %>%
  pivot_longer(                  # Pivot the leaf columns (l1, l2, l3) into a long format
    cols = c(l1, l2, l3), 
    names_to = "leaf_id", 
    values_to = "length_cm"
  ) %>%
  filter(!is.na(length_cm)) # Drop rows where there were no leaves measured. 

gramcanopy26 <-canopy26 %>% 
  mutate(maxheight = pmax(qne_h, qes_h, qsw_h, qwn_h, na.rm = TRUE)) %>%
  filter(fxl_group == "graminoid")

forbcanopy26 <-canopy26 %>% 
  mutate(maxheight = pmax(qne_h, qes_h, qsw_h, qwn_h, na.rm = TRUE)) %>%
  filter(fxl_group == "forb")

# =============================================================================
#                           VISUALIZING DATA 
# =============================================================================
#### Graphing 2026 graminoid leaf lengths  ###

ggplot (gramleaf26, aes (x=sim_date, y=length_cm, group=monolith_id))+
  geom_line(aes(colour = monolith_id)) +
  geom_point(aes(colour = monolith_id)) +
  facet_grid(treatment ~ .) +
  scale_x_date(
    date_breaks = "5 days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Graphing 2025 graminoid leaf lengths  ###
ggplot (gramleaf25, aes (x=sim_date, y=length_cm, group=monolith_id))+
  geom_line(aes(colour = monolith_id)) +
  geom_point(aes(colour = monolith_id)) +
  facet_grid(treatment ~ .) +
  scale_x_date(
    date_breaks = "5 days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#just monolith 2
M02_gramcanopy <- gramcanopy26%>%
  filter(monolith_id =="M_02")

ggplot (M02_gramcanopy, aes (x=date, y=maxheight))+
  geom_line() +
  geom_point() +
  scale_x_date(
    date_breaks = "5 days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ==============================================================================
#                    COMBINING 2025 AND 2026
# ==============================================================================
bothyrs <- bind_rows(
  gramleaf25 %>% mutate(year = "2025"),
  gramleaf26 %>% mutate(year = "2026")
) %>%
  mutate(
    sim_date = as.Date(format(sim_date, "2000-%m-%d")), #set sim year to 2000, because the year is arbitrary,
    treatment = factor(treatment, levels = c("con", "hw", "ext"))
  )

# average leaf length per monolith per date first (2026 has multiple leaves per monolith)
per_monolith <- bothyrs %>%
  group_by(year, treatment, monolith_id, sim_date) %>%
  summarise(length_cm = mean(length_cm, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.nan(length_cm))

summ <- per_monolith %>%
  group_by(year, treatment, sim_date) %>%
  summarise(
    mean = mean(length_cm),
    se   = sd(length_cm) / sqrt(n()),
    .groups = "drop"
  )

treatment_cols <- c(
  con = "#4C72B0",   # blue
  hw  = "#DD8452",   # orange
  ext = "#55A868"    # green
)

ggplot(summ, aes(sim_date, mean, colour = treatment, linetype = year, shape = year)) +
  geom_vline(xintercept = as.Date("2000-08-14"),
             linetype = "dashed", colour = "gold",
             linewidth = 0.8) +
  geom_line() +
  geom_point(size = 2) +
 # geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 1) +
  scale_colour_manual(values = treatment_cols,
                      labels = c(con = "Control", hw = "Heat wave", ext = "Extended")
  ) +
  scale_x_date(date_labels = "%b %d", breaks= "1 weeks", limits =c(as.Date("2000-06-27"), NA)) +
  labs(x = "Simulated date", y = "Average Graminoid Leaf Length (cm)",
       colour = "Treatment", linetype = "Year", shape = "Year")+
  theme_minimal()

# ===========================================================================
# RELATIVE GROWTH
# ===========================================================================
#Since at least some of the differences between 2025 and 2026 are due to different
# measurements, I want to look at growth relative to a starting point. 

ref_dates <- tibble::tribble(
  ~year, ~treatment, ~ref_date,
  "2025", "con",      as.Date("2000-07-29"),
  "2025", "hw",       as.Date("2000-07-29"),
  "2025", "ext",      as.Date("2000-07-31"),
  "2026", "con",      as.Date("2000-07-28"),
  "2026", "hw",       as.Date("2000-07-28"),
  "2026", "ext",      as.Date("2000-07-28")
) %>%
  mutate(treatment = factor(treatment, levels = levels(summ$treatment)))

relative_avg <- summ %>%
  left_join(ref_dates, by = c("year", "treatment")) %>%
  group_by(year, treatment) %>%
  mutate(
    ref_mean    = mean[sim_date == first(ref_date)],
    mean_rel    = mean / ref_mean,        # proportion of baseline (1 = no change)
    se_rel      = se / ref_mean,
    mean_change = mean - ref_mean         # change in cm (0 = no change)
  ) %>%
  ungroup()

ggplot(relative_avg, aes(sim_date, mean_change, colour = treatment, linetype = year, shape = year)) +
  geom_vline(xintercept = as.Date("2000-08-14"),
             linetype = "dashed", colour = "gold",
             linewidth = 0.8) +
  geom_hline(yintercept = 0,
             linetype = "dashed", colour = "black",
             linewidth = 0.8) +
  geom_line() +
  geom_point(size = 2) +
  # geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 1) +
  scale_colour_manual(values = treatment_cols,
                      labels = c(con = "Control", hw = "Heat wave", ext = "Extended")
  ) +
  scale_x_date(date_labels = "%b %d", breaks= "1 weeks", limits =c(as.Date("2000-07-23"), NA)) +
  coord_cartesian(ylim = c(-5, 15), expand = FALSE)+
  labs(x = "Simulated date", y = "Graminoid Leaf Length (cm) relative to sim July end",
       colour = "Treatment", linetype = "Year", shape = "Year")+
  theme_minimal()
