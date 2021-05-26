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

# 2016-2021 911 calls for service from Detroit open data portal.
# https://data.detroitmi.gov/datasets/911-calls-for-service?geometry=-86.035%2C42.028%2C-80.808%2C42.738

# Direct download link.
# download.file(url = "https://opendata.arcgis.com/datasets/4f49eb825f564efa9a23cd103c4ba13b_0.csv",
#               destfile = "data/detroit_16_21.csv")

# Load.
detroit_df <- read_csv("data/detroit_16_21.csv")

# Date checks.
detroit_df %>% 
  slice(1:5) %>% 
  select(call_timestamp)

# Subset for 2019. We want pre-COVID.
detroit19_df <- detroit_df %>% 
  filter(str_detect(call_timestamp, "2019/"))

# Date checks.
detroit19_df %>% 
  slice(1:5) %>% 
  select(call_timestamp)

detroit19_df %>% 
  mutate(year = as.character(str_extract_all(call_timestamp, "^.{4}"))) %>% 
  group_by(year) %>% 
  summarise(counts = n())

# Remove original to free memory if needed.
rm(detroit_df)

# Missings.
sum(is.na(detroit19_df$calldescription))
sum(is.na(detroit19_df$totaltime))

# Remove missings, zeros and negatives.
detroit19_sub_df <- detroit19_df %>% 
  drop_na(totaltime) %>% 
  filter(totaltime != 0, totaltime >= 0)

# What's the distribution now?
ggplot(data = detroit19_sub_df) +
  geom_histogram(mapping = aes(x = totaltime), bins = 100)

# Outliers are massively skewing this. For now, arbitrary cut-off.
detroit19_sub_no_df <- detroit19_sub_df %>% 
  filter(totaltime < 300)

# What's the distribution now?
ggplot(data = detroit19_sub_no_df) +
  geom_histogram(mapping = aes(x = totaltime), bins = 100)

# What take the most time?
freqs_df <- detroit19_sub_no_df %>% 
  group_by(calldescription) %>% 
  summarise(tt_cd = sum(totaltime)) %>% 
  arrange(tt_cd)
  
# Calculate sum times by category, filter out admin/role categories.
times_df <- detroit19_sub_df %>% 
  filter(!str_detect(calldescription, "REPORT|RPT"),
         calldescription != "REMARKS",
         calldescription != "START OF SHIFT INFORMATION",
         calldescription != "UNKNOWN PROBLEM",
         # calldescription != "SPECIAL ATTENTION",
         calldescription != "VIRTUAL SPECIAL ATTENTION",
         calldescription != "EMPLOYEE CALL IN / TIME OFF",
         calldescription != "CALL BACK DESK") %>%
  mutate(calldescription2 = if_else(str_detect(calldescription, "DV"),
                                   true = "DOMESTIC INCIDENT",
                                   false = calldescription)) %>% 
  group_by(calldescription2) %>% 
  summarise(sum_time = sum(totaltime),
            sum_counts = n()) %>% 
  mutate(total_time     = sum(sum_time),
         total_counts   = sum(sum_counts),
         prop_time      = round(sum_time/total_time, 6),
         prop_counts    = sum_counts/total_counts) %>% 
  ungroup() 
  
# Flag other types of incident.
times_df <- times_df %>%
  mutate(type = if_else(condition = str_detect(calldescription2, "EMS|DRUG OD|SERIOUS INJURIES|HEMORRHAGE|FAINTING ALRT|INHALATION|SEIZURE|MENTAL|WELL BEING|SUICIDE|MEDICAL|HOSPITAL|SICK|OVERDOSE|BREATHING"),
                        true = "health",
                        false = calldescription2),
         type = if_else(condition = str_detect(type, "SPECIAL ATTENTION|TRANSPORT PRISONER|TRANSPORT PRISONER-OTH AGT|ESCORT|SAFEWALK|BE ON THE LOOK OUT|INVESTIGATE|BUILDING CHECK"),
                        true = "proactive",
                        false = type),
         type = if_else(condition = str_detect(type, "SMOKING VIOLATIONS|EXPLOSION|DEAD PERSON OBSERVED|SUSPICIOUS PACKAGE|SMOKE INVESTIGATION|YOUTH LOITERING/CONGREGATING|SCHOOL CROSSING|SCHOOL THREATS I/P|SCHOOL THREATS J/H & RPT|MALICIOUS DESTRUCTION|DISTURBANCE|VANDALISM|PARKING|NOISE|LITTERING"),
                        true = "quality of life",
                        false = type),
         type = if_else(condition = str_detect(type, "PBT TEST|HAZARDOUS CONDITIONS|VERIFIED ALR|TOWING DETAIL|TRAFFIC|UTO X - BLDG / DWELL|AUTO X OR PED X - INJURIES|AUTO X UNK INJ / IMPAIRED|ENTERING AUTO|MOTOR"),
                        true = "traffic",
                        false = type),
         type = if_else(condition = str_detect(type, "TEMPERATURE ALARM|CURFEW VIOLATION|CITIZEN RADIO PATROL IN TROUBL|PEACE OFFICER DETAIL|RESIDENTIAL STRUCTURE FIRE|OTHR OUTSIDE STRUCTURE FIRE|FIRE ALARM|FIRE ALARMS|EXTRICATION / ENTRAPMENT|ELEVATOR ENTRAPMENT|ALARM UNKNOWN CAUSE|ALARM MISUSE|ALARM MALFUNCTION|ANIMAL|ASSIST OTHER|ASSIST CITIZEN|MISSING|FOUND PERSON|SENIOR CITIZEN ASSIS|HOME ALONE|POWER LINES|GRASS  FIRE|VEHICLE FIRE|WSPD - FIRES|COMMERCIAL STRUCTURE FIRE"),
                        true = "community",
                        false = type),
         # type = if_else(condition = str_detect(type, "PERSONNEL IN TROUBLE|VIP THREATS I/P|PURSUIT - VEHICLE OR FOOT|PPO VIOLATION|HOLD UP ALARM AND MOW|PANIC / DURESS ALARM|WSPD - BURGLARY ALARM|ATM  ALARM|BANK ALARM|HPPD BURG ALRM|HPPD BURG ALRM  W/ MOW|AUTO X HIT|BUS BOARDING|K-9 DEPLOYMENT|RECOVER AUTO|LEWD AND LASCIVIOUS|ASSIST PERSONNEL|HOLDING PERSON|EXTORTION|UDAA|KIDNAPPING|DV |NARCOTICS|MOLESTATION|FRAUD|BOMB|ARSON|SHOTS|SHOOTING|STABBED|STAB|ROBBERY|ASSAULT|RAPE|WEAPON|BURGLARY|PROPERTY|BREAKING|LARCENY|CHILD /  ADULT|RAID - EXECUTE SEARCH WARRANT|WNTD WRRNT"),
         #                true = "crime",
         #                false = type),
         type = if_else(condition = str_detect(type, "DOMESTIC INCIDENT|PERSONNEL IN TROUBLE|VIP THREATS I/P|PURSUIT - VEHICLE OR FOOT|PPO VIOLATION|HOLD UP ALARM AND MOW|PANIC / DURESS ALARM|WSPD - BURGLARY ALARM|ATM  ALARM|BANK ALARM|HPPD BURG ALRM|HPPD BURG ALRM  W/ MOW|AUTO X HIT|BUS BOARDING|K-9 DEPLOYMENT|RECOVER AUTO|LEWD AND LASCIVIOUS|ASSIST PERSONNEL|HOLDING PERSON|EXTORTION|UDAA|KIDNAPPING|DV |NARCOTICS|MOLESTATION|FRAUD|BOMB|ARSON|SHOTS|SHOOTING|STABBED|STAB|ROBBERY|ASSAULT|RAPE|WEAPON|BURGLARY|PROPERTY|BREAKING|LARCENY|CHILD /  ADULT|RAID - EXECUTE SEARCH WARRANT|WNTD WRRNT"),
                        true = "crime",
                        false = type),
         type_comp = if_else(condition = type %in% calldescription2 | str_detect(type, "FIRE ALARM|ALARM MALFUNCTION|ALARM MISUSE|ALARM UNKNOWN CAUSE"),
                             true = "unclassified",
                             false = type))

# Reaggregate with new coding.
times2_df <- times_df %>% 
  group_by(calldescription2) %>% 
  mutate(prop_time = sum(prop_time)) %>% 
  distinct(calldescription2, prop_time, type_comp) %>% 
  ungroup()


# Descriptive stats.
times2_df %>% 
  group_by(type_comp) %>% 
  summarise(type_prop_time  = sum(prop_time))

# Graphic counts.
tree_gg <- ggplot(data = times_df,
       mapping = aes(area = prop_time, label = calldescription2, subgroup = type_comp, fill = type_comp)) +
  scale_fill_brewer(palette = "Greys") + 
  geom_treemap(colour = "black", size = 1, alpha = 0.5) +
  geom_treemap_text(min.size = 1) +
  geom_treemap_subgroup_text(padding.y = unit(0.2, "cm"), colour = "black") +
  geom_treemap_subgroup_border(colour = "black", size = 2) + 
  labs(title = "Total deployed time in Detroit (2019) | Proportional breakdown",
       subtitle = "Deployment time = response time + time on scene") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5))

# Save.
ggsave(plot = tree_gg, filename = "visuals/tree_time_gg.png", height = 30, width = 20, unit = "cm")
