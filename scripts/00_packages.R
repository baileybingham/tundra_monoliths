pckg.list <- c("data.table","tidyverse","sf","mapview","ggspatial")


for(i in pckg.list){
  ihavethepckgalready <- require(i,character.only = T) # Load the package, if i don't have it, install it
  if(!ihavethepckgalready){install.packages(i)
    require(i,character.only = T) 
  }
}