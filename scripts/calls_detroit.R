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
# ggplot(data = detroit19_sub_df) +
#   geom_histogram(mapping = aes(x = totaltime), bins = 100)

# Outliers are massively skewing this. For now, arbitrary cut-off.
detroit19_sub_no_df <- detroit19_sub_df %>% 
  filter(totaltime < 300)

# What's the distribution now?
# ggplot(data = detroit19_sub_no_df) +
#   geom_histogram(mapping = aes(x = totaltime), bins = 100)

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
                                   false = calldescription),
         calldescription2 = if_else(str_detect(calldescription2, "MENTAL"),
                                    true = "MENTAL HEALTH",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "SUICIDE"),
                                    true = "SUICIDE INCIDENT OR THREAT",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "EMS"),
                                    true = "EMS",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "BURGLARY"), # captures small alarm category
                                    true = "BURGLARY",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "BURGLARY"), # captures small alarm category
                                    true = "BURGLARY",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "HIT & RUN|HIT& RUN"),
                                    true = "HIT & RUN",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "LOST PROPERTY|RECOVERED / FOUND PROPERTY"),
                                    true = "LOST PROPERTY",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "AUTO X"),
                                    true = "AUTO ACCIDENT",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "ASSAULT AND BATTERY"),
                                    true = "ASSAULT AND BATTERY",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "ASSAULT DANGEROUSOR SERIOUS|ASSAULT OR SEX ASSAULT DELTA"),
                                    true = "OTHER ASSAULT",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "FELONIOUS ASSAULT"),
                                    true = "FELONIOUS ASSAULT",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "OVERDOSE|DRUG OD"),
                                    true = "OVERDOSE",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "ROBBERY ARMED"),
                                    true = "ARMED ROBBERY",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "ROBBERY NOT ARMED"),
                                    true = "UNARMED ROBBERY",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "ANIMAL BITE|VICIOUS ANIMAL"),
                                    true = "VICIOUS ANIMAL OR BITE",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "MISSING|FOUND"),
                                    true = "MISSING OR FOUND PERSON",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "VERIFIED ALR|ALR PT"),
                                    true = "LICENCE REVOCATION",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "PBT TEST"),
                                    true = "BREATH TEST",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "ALRM"),
                                    true = "BURGLARY ALARMS",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "ARSON"),
                                    true = "ARSON",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "FRAUD"),
                                    true = "FRAUD",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "SHOTS"),
                                    true = "SHOTS FIRED",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "SHOOTING|SHOT STAB"), # shooting/cutting/pent wound
                                    true = "SHOOTING, STABBING OR PENETRATING WOUND",
                                    false = calldescription2),
         # calldescription2 = if_else(str_detect(calldescription2, "RAPE|MOLESTATION"),
         #                            true = "RAPE OR MOLESTATION",
         #                            false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "ADULT ABUSE"),
                                    true = "CHILD OR ADULT ABUSE",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "NARCOTICS"),
                                    true = "NARCOTICS",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "LARCENY"),
                                    true = "LARCENY",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "RAPE"),
                                    true = "RAPE",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "RAID - EXECUTE SEARCH WARRANT"),
                                    true = "EXECUTE SEARCH WARRANT",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "LEWD AND LASCIVIOUS"),
                                    true = "LEWD AND LASCIVIOUS CONDUCT",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "UDAA"),
                                    true = "UDAA",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "AID MOTORIST"), # include child locked in car
                                    true = "AID MOTORIST",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "BREAKING AND ENTERING AUTO I/P"),
                                    true = "BREAK & ENTER AUTO",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "PPO VIOLATION"), 
                                    true = "PPO VIOLATION",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "PANIC|HOLD UP"), 
                                    true = "PANIC, DURESS OR HOLD-UP ALARM",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "WNTD WRRNT"), 
                                    true = "WANTED WARRANT",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "MEDICAL ALARM"), 
                                    true = "MEDICAL ALARM",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "BANK ALARM|ATM ALARM"), 
                                    true = "BANK OR ATM ALARM",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "FIRE ALARM"), # combine real and tests
                                    true = "FIRE ALARM",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "ALARM MALFUNCTION|ALARM UNKNOWN"), # combine real and tests
                                    true = "OTHER ALARMS",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "RUBBISH LITTERING I/P"), # combine real and tests
                                    true = "LITTERING",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "VIP THREATS I/P"), # combine real and tests
                                    true = "VIP THREATS",
                                    false = calldescription2)) %>% 
  group_by(calldescription2) %>% 
  summarise(sum_time = sum(totaltime),
            sum_counts = n()) %>% 
  mutate(total_time     = sum(sum_time),
         total_counts   = sum(sum_counts),
         prop_time      = round(sum_time/total_time, 6),
         prop_counts    = sum_counts/total_counts) %>% 
  ungroup() 
  
# Flag other types of incident.
times_agg_df <- times_df %>%
  mutate(type = if_else(condition = str_detect(calldescription2, "EMS|OVERDOSE|SERIOUS INJURIES|HEMORRHAGE|FAINTING ALRT|INHALATION|SEIZURE|MENTAL HEALTH|WELL BEING|SUICIDE INCIDENT OR THREAT|MEDICAL ALARM|HOSPITAL|SICK|BREATHING"),
                        true = "health",
                        false = calldescription2),
         type = if_else(condition = str_detect(type, "WANTED WARRANT|EXECUTE SEARCH WARRANT|BAIT PROPERTY DEPLOYMENT|SPECIAL ATTENTION|TRANSPORT PRISONER|TRANSPORT PRISONER-OTH AGT|ESCORT|SAFEWALK|BE ON THE LOOK OUT|INVESTIGATE|BUILDING CHECK"),
                        true = "proactive",
                        false = type),
         type = if_else(condition = str_detect(type, "SMOKING VIOLATIONS|EXPLOSION|DEAD PERSON OBSERVED|SUSPICIOUS PACKAGE|SMOKE INVESTIGATION|YOUTH LOITERING/CONGREGATING|SCHOOL CROSSING|SCHOOL THREATS I/P|SCHOOL THREATS J/H & RPT|MALICIOUS DESTRUCTION|DISTURBANCE|VANDALISM|PARKING|NOISE|LITTERING"),
                        true = "quality of life",
                        false = type),
         type = if_else(condition = str_detect(type, "UDAA|BREATH TEST|LICENCE REVOCATION|AUTO ACCIDENT|HAZARDOUS CONDITIONS|TOWING DETAIL|TRAFFIC|ENTERING AUTO|MOTOR"),
                        true = "traffic",
                        false = type),
         type = if_else(condition = str_detect(type, "FIRE ALARM|MISSING OR FOUND PERSON|PROPERTY DAMAGE NON-CRIMINAL|LOST PROPERTY|TEMPERATURE ALARM|CURFEW VIOLATION|CITIZEN RADIO PATROL IN TROUBL|PEACE OFFICER DETAIL|RESIDENTIAL STRUCTURE FIRE|OTHR OUTSIDE STRUCTURE FIRE|FIRE ALARM|FIRE ALARMS|EXTRICATION / ENTRAPMENT|ELEVATOR ENTRAPMENT|ALARM UNKNOWN CAUSE|ALARM MISUSE|ALARM MALFUNCTION|ANIMAL|ASSIST CITIZEN|SENIOR CITIZEN ASSIS|HOME ALONE|POWER LINES|GRASS  FIRE|VEHICLE FIRE|WSPD - FIRES|COMMERCIAL STRUCTURE FIRE"),
                        true = "community",
                        false = type),
         type = if_else(condition = str_detect(type, "BANK OR ATM ALARM|BREAK & ENTER AUTO|PANIC, DURESS OR HOLD-UP ALARM|SHOOTING, STABBING OR PENETRATING WOUND|RAPE|BURGLARY ALARMS|ROBBERY|ASSAULT AND BATTERY|FELONIOUS ASSAULT|DOMESTIC INCIDENT|PERSONNEL IN TROUBLE|VIP THREATS I/P|PURSUIT - VEHICLE OR FOOT|PPO VIOLATION|HOLD UP ALARM AND MOW|ATM  ALARM|BANK ALARM|HPPD BURG ALRM|HPPD BURG ALRM  W/ MOW|HIT & RUN|BUS BOARDING|K-9 DEPLOYMENT|RECOVER AUTO|LEWD AND LASCIVIOUS|HOLDING PERSON|EXTORTION|KIDNAPPING|DV |NARCOTICS|MOLESTATION|FRAUD|BOMB|ARSON|SHOTS|STABBED|STAB|OTHER ASSAULT|WEAPON|BURGLARY|BREAKING|LARCENY|CHILD OR ADULT ABUSE|WNTD WRRNT"),
                        true = "crime",
                        false = type),
         type_comp = if_else(condition = type %in% calldescription2 | str_detect(type, "OTHER ALARMS|ASSIST PERSONNEL|ASSIST OTHER|FIRE ALARM|ALARM MALFUNCTION|ALARM MISUSE|ALARM UNKNOWN CAUSE"),
                             true = "unclassified",
                             false = type),
         calldescription2 = if_else(prop_time < 0.0005, true  = "OTHER", false = calldescription2)) %>% 
  group_by(calldescription2, type_comp) %>% 
  summarise(prop_time = sum(prop_time)) %>% 
  ungroup()

# Descriptive stats.
times_agg_df %>% 
  group_by(type_comp) %>% 
  summarise(type_prop_time  = sum(prop_time)) %>% 
  ungroup()

# Graphic counts.
tree_gg <- ggplot(data = times_agg_df,
       mapping = aes(area = prop_time, label = calldescription2, subgroup = type_comp, fill = type_comp)) +
  scale_fill_brewer(palette = "Greys") +
  geom_treemap(colour = "black", size = 1, alpha = 0.5) +
  geom_treemap_text(min.size = 1) +
  geom_treemap_subgroup_text(padding.y = unit(0.2, "cm"), colour = "black") +
  geom_treemap_subgroup_border(colour = "black", size = 2) + 
  labs(title = "Deployed police time: Proportional breakdown in Detroit.",
       subtitle = "Deployment time = response time + time on scene",
       caption = "Code and data sources available at https://github.com/langtonhugh/demand_viz.") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 32),
        plot.subtitle = element_text(hjust = 0.5, size = 22),
        plot.caption = element_text(size = 16))

# Save.
ggsave(plot = tree_gg, filename = "visuals/tree_time_gg.png", height = 60, width = 40, unit = "cm")

