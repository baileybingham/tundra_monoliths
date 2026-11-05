##### Cleaning Above Ground Measurements #####
#### By Bailey Bingham 
#### Nov 2, 2026

# ==============================================================================
# SET UP
# ==============================================================================

source(file.path("scripts","00_packages.R")) # load the packages listed in the 00_packages.R file under scripts

#Import raw data
above <- read_csv("raw_data/Above-ground-measurements.csv", ##import raw above ground measurement file,
                  skip = 3, #skip the first 3 rows since they include metadata
                  na = c("", "NA", "N/A", "na", "n/a", "NaN", "-", "#N/A", " "), #write all of these variations as na
                  col_types = cols(.default = col_character()), #read everything as text so typos like "0,32" or "o.40" are not lost
                  name_repair = tolower)%>%  #change heading names to lowercase
  mutate(
    monolith_id = str_pad(monolith_id, width = 2, side = "left", pad = "0") #make sure all monolith IDs are two digits
  )

names(above)

# ==============================================================================
# DATA TIDYING
# ==============================================================================
height_cols <- c("qne_h", "qes_h", "qsw_h", "qwn_h") # height of the tallest plant in each quadrant (cm)
leaf_cols   <- c("l1", "l2", "l3") # length of three marked leaves (cm)
count_cols  <- c("buds", "anthesis", "wilt")  # counts per pot (whole numbers)
# All the columns that should contain numbers. Used for conversion and checks.
num_cols    <- c(height_cols, leaf_cols, count_cols, "senes") # "senes" is the % of canopy showing senescence (0-100)

##### Find typos in numeric columns #####

bad_numeric <- above %>%
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
view(bad_numeric_count)

#-----------------------------------------------------------------------------
##### Fix typos in numeric columns ####

clean_num <- function(x) { #create a function to clean the data
  x_trim <- str_trim(x)    # remove stray spaces at either end of entries
  
  case_when(
    # any entry starting with "bud" ("bud", "Bud", "buds", "budding") -> 0.001
    str_detect(x_trim, regex("^bud", ignore_case = TRUE)) ~ 0.001,
    
    # everything else: repair typos, keep only plain numbers, convert.
    # "fully senesced" is not a plain number, so it becomes NA here.
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

above_clean <- above %>%
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
  before   = map_int(num_cols, ~ sum(!is.na(above[[.x]]))),
  after    = map_int(num_cols, ~ sum(!is.na(above_clean[[.x]]))),
  senesced = map_int(num_cols, ~ sum(str_detect(above[[.x]],
                                                regex("fully\\s*senesc", ignore_case = TRUE)), na.rm = TRUE))
) %>%
  mutate(unexplained_loss = before - after - senesced)
view(lost_values)
# all should be zero, showing that nothing was lost between the before and after file. 

# Tally of the leaf notes
above_clean %>%
  pivot_longer(ends_with("_status"), names_to = "leaf", values_to = "status") %>%
  count(fxl_group, status)
#not sure why there is a forb with a 'bud' status. To deal with later. 

# ==============================================================================
# Separate into leaf length data and canopy height data
# ==============================================================================

#LEAF LENGTHS#
#remove dates before August 13th, since no leaf length measurements were taken before that date
leaflg <- select (above_clean, date, monolith_id, fxl_group, starts_with ("l")) %>%
  filter(date >= mdy("August 13, 2026"))

#write_csv (leaflg, "data/leaflength_clean.csv")

#CANOPY HEIGHT#
#remove dates before August 13th. There were baseline measurements taken on Aug 6th, but they were mostly dead biomass. 
canopy <- select (above_clean, date, monolith_id, fxl_group, starts_with ("q"), notes) %>%
  filter(date >= mdy("August 13, 2026"))

#write_csv (canopy, "data/canopyheight_clean.csv")
