##### Cleaning Above Ground Measurements #####
#### By Bailey Bingham 
#### Nov 2, 2026

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ==============================================================================
#                            2026 DATA CLEANING
# ==============================================================================

source(file.path("scripts","00_packages.R")) # load the packages listed in the 00_packages.R file under scripts

#Import 2026 raw data
above26 <- read_csv("raw_data/2026/Above-ground-measurements.csv", ##import raw above ground measurement file,
                  skip = 3, #skip the first 3 rows since they include metadata
                  na = c("", "NA", "N/A", "na", "n/a", "NaN", "-", "#N/A", " "), #write all of these variations as na
                  col_types = cols(.default = col_character()), #read everything as text so typos like "0,32" or "o.40" are not lost
                  name_repair = tolower)%>%  #change heading names to lowercase
  mutate(
    monolith_id = str_pad(monolith_id, width = 2, side = "left", pad = "0"), #make sure all monolith IDs are two digits
    monolith_id = paste0("M_", monolith_id))
names(above26)

# ==============================================================================
# DATA TIDYING
# ==============================================================================
height_cols <- c("qne_h", "qes_h", "qsw_h", "qwn_h") # height of the tallest plant in each quadrant (cm)
leaf_cols   <- c("l1", "l2", "l3") # length of three marked leaves (cm)
count_cols  <- c("buds", "anthesis", "wilt")  # counts per pot (whole numbers)
# All the columns that should contain numbers. Used for conversion and checks.
num_cols    <- c(height_cols, leaf_cols, count_cols, "senes") # "senes" is the % of canopy showing senescence (0-100)

##### Find typos in numeric columns #####

bad_numeric <- above26 %>%
  mutate(row = row_number()) %>%   
   pivot_longer(
    cols      = all_of(num_cols),      # stack all the numeric columns together, so everything that should be a number is in one place. 
    names_to  = "variable",           
    values_to = "raw"                  
  ) %>%
  filter(
    !is.na(raw),                                   # filter out NAs
    is.na((as.numeric(raw)))       # list all cells that cannot be converted to a number
  ) %>%
  select(row, date, monolith_id, fxl_group, variable, raw)

head (bad_numeric) # inspect each one, e.g. "0,32", "o.40", "~5"  
bad_numeric_count <-count(bad_numeric, raw, sort = TRUE)   # count the number of time that that text occurs where there should be numbers. 
head(bad_numeric_count)

#-----------------------------------------------------------------------------
##### Fix typos in numeric columns ####

clean_num <- function(x) { #create a function to clean the data
  x_trim <- str_trim(x)    # remove stray spaces at either end of entries
  
  case_when(
    # any entry starting with "bud" ("bud", "Bud", "buds", "budding") -> 0.001
    str_detect(x_trim, regex("^bud", ignore_case = TRUE)) ~ 0.001,
    
    # everything else: repair typos, keep only plain numbers, convert to numeric.
       .default = x_trim %>%
      str_replace(",", ".") %>%                  # decimal comma: "0,32" -> "0.32"
      str_replace("^[oO](?=[.,]?\\d)", "0") %>%  # leading letter o: "o.40" -> "0.40"
      str_extract("^-?\\d*\\.?\\d+$") %>%        # NA unless it is a plain number
      parse_double()                             # readr: text -> number (NA stays NA)
  )
}

#-----------------------------------------------------------------------------
# Create a function that writes the old text into a new column. 
status_note <- function(x) {
  x_trim <- str_trim(x)
  
  case_when(
    str_detect(x_trim, regex("fully\\s*senesc", ignore_case = TRUE)) ~ "fully_senesced",
    str_detect(x_trim, regex("^bud", ignore_case = TRUE))            ~ "bud",
    .default = NA_character_
  )
}

above26_clean <- above26 %>%
  #make notes of all statuses that are in the leaf numeric columns. 
  mutate(across(all_of(leaf_cols), status_note, .names = "{.col}_status")) %>%
  # Convert any typoes to numbers and tidy the id columns.
  mutate(
    date      = ymd(date),                       # read date as date (YYYYMMDD)
    across(all_of(num_cols), clean_num)         # apply clean_num() function to every numeric column
  )

# Note that if plant leaves were too small to measure, or a bud was present, we noted them as 0.001, 
# which is the smallest possible measurement that could be taken on the calipers. 

# Where observer wrote bud, we also converted to 0.001 to show presence. 

#-----------------------------------------------------------------------------
##### Check that no real data was deleted during cleaning ####

lost_values <- tibble(
  variable = num_cols,
  before   = map_int(num_cols, ~ sum(!is.na(above26[[.x]]))),
  after    = map_int(num_cols, ~ sum(!is.na(above26_clean[[.x]]))),
  senesced = map_int(num_cols, ~ sum(str_detect(above26[[.x]],
                                                regex("fully\\s*senesc", ignore_case = TRUE)), na.rm = TRUE))
) %>%
  mutate(unexplained_loss = before - after - senesced)
head(lost_values)
# all should be zero, showing that nothing was lost between the before and after file. 

# Tally of the leaf notes
above26_clean %>%
  pivot_longer(ends_with("_status"), names_to = "leaf", values_to = "status") %>%
  count(fxl_group, status)
#not sure why there is a forb with a 'bud' status. To deal with later. 

# ==============================================================================
#Descriptive statistics to look for errors in where the decimal was placed
# ==============================================================================

summary (above26_clean)

# ==============================================================================
# Add treatment to the data
# ==============================================================================
# Monolith treatments #
treatments<- tibble(
  monolith_id = c("M_01", "M_02", "M_03", "M_04", "M_05", "M_06", "M_07", "M_08", "M_09", "M_10", 
         "M_11", "M_12", "M_13", "M_14", "M_15", "M_16", "M_17", "M_18", "M_19", "M_20", "M_21"),
  treatment = c("ext", "hw", "hw", "con", "con", "ext", "ext", "ext", "con", "ext", 
                "hw", "hw", "hw", "con", "con", "ext", "con", "ext", "hw", "con", "hw")
  )

above26_clean <-above26_clean %>%
  left_join(treatments, by = "monolith_id")

# ==============================================================================
# Add simulated date and experimental day to data for easier comparison to other years
# ==============================================================================
actual_start    <- as.Date("2026-07-17")
simulated_start <- as.Date("2026-05-23") # this is the date that we were simulating the light and temp conditions for. Year is arbitrary.

above26_clean2 <- above26_clean %>%
  mutate(
    exp_day = as.numeric(date - actual_start) + 1, #calculate the experimental day.
    sim_date = simulated_start + (date - actual_start) #calculate the simulated date
  )

head(above26_clean2)
# ==============================================================================
# Separate into leaf length data and canopy height data
# ==============================================================================

#LEAF LENGTHS#
#remove dates before August 13th, since no leaf length measurements were taken before that date
leaflg26 <- select (above26_clean2, date, sim_date, exp_day, monolith_id, treatment, fxl_group, starts_with ("l")) %>%
  filter(date >= mdy("August 13, 2026"))

write_csv (leaflg26, "data/2026_leaflength_clean.csv")

#CANOPY HEIGHT#
#remove dates before August 13th. There were baseline measurements taken on Aug 6th, but they were mostly dead biomass. 
canopy26 <- select (above26_clean2, date, sim_date, exp_day, monolith_id, treatment, fxl_group, starts_with ("q"), notes) %>%
  filter(date >= mdy("August 13, 2026"))

write_csv (canopy26, "data/2026_canopyheight_clean.csv")
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ==============================================================================
#                          2025 DATA CLEANING
# ==============================================================================
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Import 2025 raw data
#note that this year we only collected the length of the longest thing, which was always a graminoid. 
#this data reflects the longest graminoid leaves, measured weekly. 
above25 <- read_csv("raw_data/2025/2025_graminoid_longleaf.csv", ##import raw long leaf measurement file,
                    na = c("", "NA", "N/A", "na", "n/a", "NaN", "-", "#N/A", " "), #write all of these variations as na
                    col_types = cols(.default = col_character()), #read everything as text so typos like "0,32" or "o.40" are not lost
                    name_repair = tolower)%>%  #change heading names to lowercase
  mutate(
    monolith_id = str_pad(monolith_id, width = 2, side = "left", pad = "0"), #make sure all monolith IDs are two digits
    monolith_id = paste0("M_", monolith_id))
names(above25)

##### Find typos in numeric columns #####
bad_numeric <- above25 %>%
  filter(
    !is.na(avg_height_cm),                                   # filter out NAs
    is.na((as.numeric(avg_height_cm)))       # list all cells that cannot be converted to a number
  ) %>%
  select(date, monolith_id, avg_height_cm)

head (bad_numeric) # inspect each one, e.g. "0,32", "o.40", "~5"  #if nothing, all lines are numeric. 
bad_numeric_count <-count(bad_numeric, avg_height_cm, sort = TRUE)   # count the number of time that that text occurs where there should be numbers. 
head(bad_numeric_count)

## recreate clean_num function and apply
#-----------------------------------------------------------------------------
##### Fix typos in numeric columns ####

clean_num <- function(x) { #create a function to clean the data
  x_trim <- str_trim(x)    # remove stray spaces at either end of entries
  
  case_when(
    # any entry starting with "bud" ("bud", "Bud", "buds", "budding") -> 0.001
    str_detect(x_trim, regex("^bud", ignore_case = TRUE)) ~ 0.001,
    
    # everything else: repair typos, keep only plain numbers, convert to numeric.
    .default = x_trim %>%
      str_replace(",", ".") %>%                  # decimal comma: "0,32" -> "0.32"
      str_replace("^[oO](?=[.,]?\\d)", "0") %>%  # leading letter o: "o.40" -> "0.40"
      str_extract("^-?\\d*\\.?\\d+$") %>%        # NA unless it is a plain number
      parse_double()                             # readr: text -> number (NA stays NA)
  )
}

above25_clean <- above25 %>%
  mutate(
    date      = dmy(date),                              # read date as date (YYYYMMDD)
    avg_height_cm = clean_num (avg_height_cm))         # apply clean_num() function to leaf lengths
  

# ==============================================================================
#Descriptive statistics to look for errors in where the decimal was placed
# ==============================================================================

summary (above25_clean)

# ==============================================================================
# Add treatment to the data
# ==============================================================================
# Monolith treatments #
treatments<- tibble(
  monolith_id = c("M_01", "M_02", "M_03", "M_04", "M_05", "M_06", "M_07", "M_08", "M_09", "M_10", 
                  "M_11", "M_12", "M_13", "M_14", "M_15", "M_16", "M_17", "M_18", "M_19", "M_20", "M_21"),
  treatment = c("ext", "hw", "hw", "con", "con", "ext", "ext", "ext", "con", "ext", 
                "hw", "hw", "hw", "con", "con", "ext", "con", "ext", "hw", "con", "hw")
)

above25_clean <-above25_clean %>%
  left_join(treatments, by = "monolith_id")

# ==============================================================================
# Add simulated date and experimental day to data for easier comparison to other years
# ==============================================================================
actual_start25 <- as.Date("2025-04-11")  # start of the experiment in 2025
actual_aut25_control<- as.Date("2025-06-27") #actual date we decreased light for control and hw
actual_aut25_ext <- as.Date("2025-07-16") #actual date we decreased light for ext.
sim_aut25_start <- as.Date("2025-08-14") # this is the approx date where light begins to decrease below 24hrs on QHI. Year is arbitrary.

above25_clean2 <- above25_clean %>%
  mutate(
    # experimental day: days since the start of the experiment (day 1 = start)
    exp_day = as.numeric(date - actual_start25) + 1,
    
    # each treatment's own light-decrease date
    actual_aut_start = case_when(
      treatment == "ext" ~ actual_aut25_ext,
      TRUE               ~ actual_aut25_control   # control and heat wave
    ),
    
    # simulated date: 14 Aug + days since that treatment's light decrease began
    sim_date = sim_aut25_start + as.numeric(date - actual_aut_start)
  )

head(above25_clean2)


#reorder columns and print
final25 <- select (above25_clean2, date, sim_date, exp_day, monolith_id, treatment, avg_height_cm)

write_csv(final25,"data/2025_gramleaflength_clean.csv") 
