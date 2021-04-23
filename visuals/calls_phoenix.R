# Load libraries.
library(readr)
library(janitor)
library(dplyr)
library(stringr)
library(treemapify)
library(ggplot2)
library(tidyr)


# Useful function.
`%nin%` <- Negate(`%in%`)

# # Download open calls-for-service data for 2020 in Phoenix.
# download.file(url = "https://www.phoenixopendata.com/dataset/64a60154-3b2d-4583-8fb5-6d5e1b469c28/resource/3c0ae3ec-456f-45f4-801d-b8d6699ba32e/download/callsforsrvc2020.csv",
#               destfile = "data/cfs_phoenix_2021.csv")
# 
# # Download open crime data in Phoenix.
# download.file(url = "https://www.phoenixopendata.com/dataset/cc08aace-9ca9-467f-b6c1-f0879ab1a358/resource/0ce3411a-2fc6-4302-a33f-167f68608a20/download/crimestat.csv",
#               destfile = "data/crime_phoenix.csv")

# Load.
cfs_df <- read_csv("data/cfs_phoenix_2021.csv", col_types = cols(INCIDENT_NUM = col_character()))
# cri_df <- read_csv("data/crime_phoenix.csv", col_types = cols(`INC NUMBER` = col_character()))

# Frequencies. Pull out those that are 'infrequent' (arbitary <= 20 counts per year).
infreqs <- cfs_df %>% 
  group_by(FINAL_CALL_TYPE) %>% 
  summarise(counts = n()) %>% 
  ungroup() %>% 
  arrange(counts) %>% # visual check
  filter(counts <= 20) %>% 
  pull(FINAL_CALL_TYPE)

# Remove these call types from the individual records.
cfs_sub_df <- cfs_df %>% 
  filter(FINAL_CALL_TYPE %nin% infreqs)

# Now we are dealing with slightly less categories.
length(unique(cfs_sub_df$FINAL_CALL_TYPE))

# Proportion frequencies.
freqs_df <- cfs_sub_df %>% 
  drop_na(FINAL_CALL_TYPE) %>% 
  group_by(FINAL_CALL_TYPE) %>% 
  summarise(counts = n()) %>% 
  mutate(total = nrow(cfs_sub_df),
         props = 100*(counts/total)) %>% 
  ungroup()

# Visualize it.
ggplot(data = freqs_df,
       mapping = aes(area = props, label = FINAL_CALL_TYPE)) +
  geom_treemap(fill = "white", colour = "black", size = 2) +
  geom_treemap_text()
