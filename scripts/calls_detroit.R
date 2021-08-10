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

# Subset for 2019. We want pre-COVID.
detroit19_df <- detroit_df %>% 
  filter(str_detect(call_timestamp, "2019/"))

# Date checks.
detroit19_df %>% 
  mutate(year = as.character(str_extract_all(call_timestamp, "^.{4}"))) %>% 
  group_by(year) %>% 
  summarise(counts = n())

# Check time variables variables.
detroit19_df %>% 
  select(calldescription, dispatchtime, traveltime, totalresponsetime, time_on_scene, totaltime)

# Totaltime = totalresponsetime + time_on_scene
# Makes sense.

# Missings in key variables.
sum(is.na(detroit19_df$calldescription))
sum(is.na(detroit19_df$totaltime)) # ~121k

# Remove missings, zeros and negatives.
detroit19_sub_df <- detroit19_df %>% 
  drop_na(totaltime) %>% 
  filter(totaltime >0)

# What is the distribution of total time?
ggplot(data = detroit19_sub_df) +
  geom_histogram(mapping = aes(x = totaltime), bins = 30)

# First, filter out calldescriptions which are admin, completely unknown or do not involve deployment.
# We retain 'REPORT' (or similar e.g. RPT).
detroit19_deploy_df <- detroit19_sub_df %>%
  filter(calldescription != "REMARKS", 
         calldescription != "START OF SHIFT INFORMATION",
         calldescription != "UNKNOWN PROBLEM",
         calldescription != "VIRTUAL SPECIAL ATTENTION",
         calldescription != "EMPLOYEE CALL IN / TIME OFF",
         calldescription != "CALL BACK DESK")

# How many categories remain?
length(unique(detroit19_deploy_df$calldescription)) # 211

# Create df to consult names for suitable recoding.
calldesc_df <- tibble(calldescription = unique(detroit19_deploy_df$calldescription)) %>% 
  arrange(calldescription)

# Recode raw calldescription into broader categories and rename to short characters as appropriate.
detroit19_deploy_df <- detroit19_deploy_df %>% 
  mutate(calldescription2 = if_else(str_detect(calldescription, "DV")                                      ,"DOMESTIC INCIDENT"          , calldescription),
         calldescription2 = if_else(str_detect(calldescription, "MENTAL")                                  ,"MENTAL HEALTH"              , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "SUICIDE")                                 ,"SUICIDE OR SUICIDE THREAT"  , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "BURGLARY")                                ,"BURGLARY"                   , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "HIT & RUN|HIT& RUN")                      ,"HIT & RUN"                  , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "LOST PROPERTY|RECOVERED / FOUND PROPERTY"),"LOST PROPERTY"              , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "AUTO X")                                  ,"AUTO ACCIDENT"              , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "ASSAULT AND BATTERY")                     ,"ASSAULT & BATTERY"          , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "ASSAULT DANGEROUSOR SERIOUS|ASSAULT OR")  ,"OTHER ASSAULT"              , calldescription2),
         calldescription2 = if_else(str_detect(calldescription,"FELONIOUS ASSAULT")                        ,"FELONIOUS ASSAULT"          , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "OVERDOSE|DRUG OD")                        ,"OVERDOSE"                   , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "ROBBERY ARMED")                           ,"ARMED ROBBERY"              , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "ROBBERY NOT ARMED")                       ,"UNARMED ROBBERY"            , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "ANIMAL")                                  ,"ANIMAL INCIDENT"            , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "MISSING|FOUND PERSON")                    ,"MISSING OR FOUND PERSON"    , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "ARSON")                                   ,"ARSON"                      , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "FRAUD")                                   ,"FRAUD"                      , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "SHOTS")                                   ,"SHOTS"                      , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "SHOOTING|SHOT STAB")                      ,"SHOOTING OR STABBING WOUND" , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "NARCOTICS")                               ,"NARCOTICS"                  , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "LARCENY")                                 ,"LARCENY"                    , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "RAPE")                                    ,"RAPE"                       , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "LEWD AND LASCIVIOUS")                     ,"LEWD & LASCIVIOUS CONDUCT"  , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "UDAA")                                    ,"UDAA"                       , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "AID MOTORIST")                            ,"AID MOTORIST"               , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "BREAKING AND|BREAKING &")                 ,"BREAK & ENTER AUTO"         , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "PPO VIOLATION")                           ,"PPO VIOLATION"              , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "WNTD WRRNT|EXECUTE SEARCH WARRANT")       ,"WARRANT"                    , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "THREATS REPORT|VIP THREATS")              ,"THREATS"                    , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "MALICIOUS DESTRUCTION")                   ,"MALICIOUS DESTRUCTION"      , calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "TRANSPORT PRISONER")                     ,"TRANSPORT PRISONER"         , calldescription2),
         calldescription2 = if_else(calldescription == "PEACE OFFICER DETAIL"                              ,"PEACE OFFICER"              , calldescription2),
         calldescription2 = if_else(calldescription == "BAIT PROPERTY DEPLOYMENT"                          ,"BAIT"                       , calldescription2),
         calldescription2 = if_else(calldescription == "VERIFIED ALR / PERSON W/O CODE"                    ,"LICENCE REVOCATION"         , calldescription2),
         calldescription2 = if_else(calldescription == "PBT TEST"                                          ,"BREATH TEST"                , calldescription2),
         calldescription2 = if_else(calldescription == "CHILD /  ADULT ABUSE|CHILD / ADULT ABUSE REPORT"   ,"CHILD OR ADULT ABUSE"       , calldescription2),
         calldescription2 = if_else(calldescription == "MEDICAL ALARM OR UNK PROB"                         ,"MEDICAL ALARM"              , calldescription2),
         calldescription2 = if_else(calldescription == "RUBBISH LITTERING I/P"                             ,"LITTERING"                  , calldescription2))

# Create new df to consult recoding.
calldesc2_df <- detroit19_deploy_df %>% 
  select(calldescription, calldescription2) %>% 
  distinct() %>% 
  arrange(calldescription)
  

         # Calculate sum times by category, filter out admin/role categories.
times_df <- detroit19_sub_df %>% 
  mutate(calldescription2 = if_else(str_detect(calldescription2, "EMS"),
                                    true = "EMS",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "VERIFIED ALR|ALR PT"),
                                    true = "LICENCE REVOCATION",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "ALRM"),
                                    true = "BURGLARY ALARMS",
                                    false = calldescription2),
         calldescription2 = if_else(str_detect(calldescription2, "PANIC|HOLD UP"), 
                                    true = "PANIC, DURESS \nOR HOLD-UP ALARM",
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




         calldescription2 = if_else(str_detect(calldescription2, "HOLDING PERSON"), # combine real and tests
                                    true = "HOLDING \nPERSON",
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
         type = if_else(condition = str_detect(type, "TRANSPORT PRISONER|WARRANT|BAIT|SPECIAL ATTENTION|ESCORT|SAFEWALK|BE ON THE LOOK OUT|INVESTIGATE|BUILDING CHECK"),
                        true = "proactive",
                        false = type),
         type = if_else(condition = str_detect(type, "PUBLIC TRANSPORT|SMOKING VIOLATIONS|EXPLOSION|DEAD PERSON OBSERVED|SUSPICIOUS PACKAGE|SMOKE INVESTIGATION|YOUTH LOITERING/CONGREGATING|SCHOOL CROSSING|SCHOOL THREATS I/P|SCHOOL THREATS J/H & RPT|DISTURBANCE|VANDALISM|PARKING|NOISE|LITTERING"),
                        true = "quality of life",
                        false = type),
         type = if_else(condition = str_detect(type, "UDAA|BREATH TEST|LICENCE REVOCATION|AUTO ACCIDENT|HAZARDOUS \nCONDITIONS|TOWING DETAIL|TRAFFIC|ENTERING AUTO|MOTOR"),
                        true = "traffic",
                        false = type),
         type = if_else(condition = str_detect(type, "HOME ALONE|FIRE ALARM|MISSING OR FOUND PERSON|PROPERTY DAMAGE NON-CRIMINAL|LOST PROPERTY|TEMPERATURE ALARM|CURFEW VIOLATION|CITIZEN RADIO PATROL IN TROUBL|PEACE OFFICER DETAIL|RESIDENTIAL STRUCTURE FIRE|OTHR OUTSIDE STRUCTURE FIRE|FIRE ALARM|FIRE ALARMS|EXTRICATION / ENTRAPMENT|ELEVATOR ENTRAPMENT|ALARM UNKNOWN CAUSE|ALARM MISUSE|ALARM MALFUNCTION|ANIMAL|ASSIST CITIZEN|SENIOR CITIZEN ASSIS|HOME ALONE|POWER LINES|GRASS  FIRE|VEHICLE FIRE|WSPD - FIRES|COMMERCIAL STRUCTURE FIRE"),
                        true = "community",
                        false = type),
         type = if_else(condition = str_detect(type, "MALICIOUS DESTRUCTION|BANK OR ATM ALARM|BREAK & ENTER AUTO|PANIC, DURESS \nOR HOLD-UP ALARM|SHOOTING, STABBING OR PENETRATING \nWOUND|RAPE|BURGLARY ALARMS|ROBBERY|ASSAULT & BATTERY|FELONIOUS ASSAULT|DOMESTIC INCIDENT|PERSONNEL IN TROUBLE|VIP THREATS I/P|PURSUIT - VEHICLE OR FOOT|PPO VIOLATION|HOLD UP ALARM AND MOW|ATM  ALARM|BANK ALARM|HPPD BURG ALRM|HPPD BURG ALRM  W/ MOW|HIT & RUN|BUS BOARDING|K-9 DEPLOYMENT|RECOVER AUTO|LEWD & LASCIVIOUS|HOLDING \nPERSON|EXTORTION|KIDNAPPING|DV |NARCOTICS|MOLESTATION|FRAUD|BOMB|ARSON|SHOTS|STABBED|STAB|OTHER ASSAULT|WEAPON|BURGLARY|BREAKING|LARCENY|CHILD OR ADULT ABUSE|WNTD WRRNT"),
                        true = "crime",
                        false = type),
         type_comp = if_else(condition = type %in% calldescription2 | str_detect(type, "OTHER ALARMS|ASSIST PERSONNEL|ASSIST OTHER|FIRE ALARM|ALARM MALFUNCTION|ALARM MISUSE|ALARM UNKNOWN CAUSE"),
                             true = "unclassified",
                             false = type),
         calldescription2 = if_else(prop_time < 0.001, true  = "OTHER", false = calldescription2)) %>% 
  group_by(calldescription2, type_comp) %>% 
  summarise(prop_time = sum(prop_time)) %>% 
  ungroup()

# Descriptive stats. 
times_agg_df %>% 
  group_by(type_comp) %>% 
  summarise(type_prop_time  = sum(prop_time)) %>% 
  ungroup()

# Remove unclassified (!!!).
# times_agg_df <- times_agg_df %>% 
#   filter(type_comp != "unclassified")

# Arrange to examine.
times_agg_df <- times_agg_df %>% 
  arrange(type_comp, prop_time)

# Proportional breakdown graphic.
ggplot(data = times_agg_df,
       mapping = aes(area = prop_time, 
                     label = calldescription2,
                     subgroup = type_comp)) +
  geom_treemap(fill = "snow", colour = "lightgrey", size = 2, alpha = 0.5) +
  geom_treemap_text(family = "serif", padding.y = unit(0.3, "cm"), grow = FALSE) +
  geom_treemap_subgroup_text(padding.y = unit(0.3, "cm"), colour = "black", size = 48, family = "serif", place = "bottomleft") +
  geom_treemap_subgroup_border(colour = "dodgerblue2", size = 4) +
  theme(legend.position = "none",
        panel.border  = element_rect(colour = "dodgerblue2", fill = "transparent", size = 2.5)) -> tree_gg

# Save.
ggsave(plot = tree_gg, filename = "visuals/tree_time_gg.png",
       height = 40, width = 60, unit = "cm", dpi = 300)
