
# # Download open calls-for-service data for 2020 in Phoenix.
# download.file(url = "https://www.phoenixopendata.com/dataset/64a60154-3b2d-4583-8fb5-6d5e1b469c28/resource/3c0ae3ec-456f-45f4-801d-b8d6699ba32e/download/callsforsrvc2020.csv",
#               destfile = "data/cfs_phoenix_2021.csv")

# Load.
cfs_df <- read_csv("data/cfs_phoenix_2021.csv", col_types = cols(INCIDENT_NUM = col_character()))
# cri_df <- read_csv("data/crime_phoenix.csv", col_types = cols(`INC NUMBER` = col_character()))

# Frequencies. Pull out those that are 'infrequent' (arbitary <= 20 counts per year).
infreqs <- cfs_df %>% 
  group_by(FINAL_CALL_TYPE) %>% 
  summarise(counts = n()) %>% 
  ungroup() %>% 
  arrange(counts) %>% # visual check
  filter(counts <= 100) %>% 
  pull(FINAL_CALL_TYPE)

# Remove these call types from the individual records.
# cfs_sub_df <- cfs_df %>% 
#   filter(FINAL_CALL_TYPE %nin% infreqs)

# Code these call types as 'other'
cfs_sub_df <- cfs_df %>%
  mutate(FINAL_CALL_TYPE2 = if_else(FINAL_CALL_TYPE %in% infreqs,
                                    "OTHER",
                                    FINAL_CALL_TYPE))


# Now we are dealing with slightly less categories.
length(unique(cfs_sub_df$FINAL_CALL_TYPE2))

# Proportion frequencies.
freqs_df <- cfs_sub_df %>% 
  drop_na(FINAL_CALL_TYPE2) %>% 
  filter()
  group_by(FINAL_CALL_TYPE2, DISP_CODE) %>% 
  summarise(counts = n()) %>% 
  mutate(total = nrow(cfs_sub_df),
         props = 100*(counts/total)) %>% 
  ungroup()

# Visualize it.
ggplot(data = freqs_df,
       mapping = aes(area = props, label = FINAL_CALL_TYPE2, group = DISP_CODE)) +
  geom_treemap(fill = "white", colour = "black", size = 2) +
  geom_treemap_text()
