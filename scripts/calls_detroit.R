# Load libraries.
library(readr)
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)
library(lubridate)
library(treemapify)
library(ggplot2)
library(cowplot)

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
detroit_df <- detroit_df %>% 
  filter(str_detect(call_timestamp, "2019"))

# Date checks.
detroit_df %>% 
  mutate(year = as.character(str_extract_all(call_timestamp, "^.{4}"))) %>% 
  group_by(year) %>% 
  summarise(counts = n()) %>% 
  ungroup()

# Check time variables variables.
detroit_df %>% 
  select(calldescription, dispatchtime, traveltime, totalresponsetime, time_on_scene, totaltime)

# Totaltime = totalresponsetime + time_on_scene
# Makes sense.

# Missings in key variables.
sum(is.na(detroit_df$calldescription))
sum(is.na(detroit_df$totaltime)) # ~121k

# Remove missings, zeros and negatives.
detroit19_sub_df <- detroit_df %>% 
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
         calldescription2 = if_else(str_detect(calldescription, "HIT & RUN|HIT& RUN|H&R")                  ,"HIT & RUN"                  , calldescription2),
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
         calldescription2 = if_else(str_detect(calldescription, "SHOOTING|SHOT STAB|SHOT OR STABBED")      ,"SHOOTING OR STABBING WOUND" , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "BOMB THREAT")                             ,"BOMB THREAT"                , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "NARCOTICS")                               ,"NARCOTICS"                  , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "LARCENY")                                 ,"LARCENY"                    , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "RAPE")                                    ,"RAPE"                       , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "LEWD AND LASCIVIOUS")                     ,"LEWD & LASCIVIOUS CONDUCT"  , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "UDAA")                                    ,"UDAA"                       , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "AID MOTORIST")                            ,"AID MOTORIST"               , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "BREAKING AND|BREAKING &")                 ,"BREAK & ENTER AUTO"         , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "PPO VIOLATION")                           ,"PPO VIOLATION"              , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "WNTD WRRNT|EXECUTE SEARCH WARRANT")       ,"WARRANT"                    , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "MALICIOUS DESTRUCTION")                   ,"MALICIOUS DESTRUCTION"      , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "TRANSPORT PRISONER")                      ,"TRANSPORT PRISONER"         , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "EXTORTION")                               ,"EXTORTION"                  , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "KIDNAPPING")                              ,"KIDNAPPING"                 , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "TRAFFIC INCIDENT|TRAFFIC- MINOR INJURIES"),"TRAFFIC INCIDENT"           , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "RECOVER AUTO")                            ,"RECOVER AUTO"               , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "MOLESTATION")                             ,"MOLESTATION"                , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "PERSON W/  A WEAPON")                     ,"PERSON WITH WEAPON"         , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "BURGLARY") & !str_detect(calldescription, "ALARM"),"BURGLARY"          , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "FIRE|SMOKE INVESTIGATION")     & !str_detect(calldescription, "ALARM|FIRED"),"FIRE"        , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "FIRE ALARM")                              ,"FIRE ALARM"                , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "CHILD /  ADULT ABUSE|CHILD / ADULT ABUSE REPORT") ,"CHILD OR ADULT ABUSE", calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "SCHOOL THREATS|THREATS REPORT|VIP THREATS")       ,"THREATS"             , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "ENTRAPMENT")                                      ,"ENTRAPMENT"          , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "DISTURBANCE")                                     ,"DISTURBANCE"         , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "BURGLARY ALARM|BURG ALRM")                        ,"BURGLAR ALARM"       , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "SCRAP STRIP")                                     ,"SCRAP METAL THEFT"   , calldescription2),
         calldescription2 = if_else(calldescription == "PEACE OFFICER DETAIL"                              ,"PEACE OFFICER"              , calldescription2),
         calldescription2 = if_else(calldescription == "MISCELLANEOUS TRAFFIC"                             ,"MISC TRAFFIC"               , calldescription2),
         calldescription2 = if_else(calldescription == "MISCELLANEOUS ACCIDENT"                            ,"MISC ACCIDENT"              , calldescription2),
         calldescription2 = if_else(calldescription == "HARASSMENT REPORT"                                 ,"HARASSMENT"                 , calldescription2),
         calldescription2 = if_else(calldescription == "BAIT PROPERTY DEPLOYMENT"                          ,"BAIT"                       , calldescription2),
         calldescription2 = if_else(calldescription == "VERIFIED ALR / PERSON W/O CODE"                    ,"LICENCE REVOCATION"         , calldescription2),
         calldescription2 = if_else(calldescription == "PBT TEST"                                          ,"BREATH TEST"                , calldescription2),
         calldescription2 = if_else(calldescription == "RUBBISH LITTERING I/P"                             ,"LITTERING"                  , calldescription2),
         calldescription2 = if_else(calldescription == "YOUTH LOITERING/CONGREGATING"                      ,"YOUTH LOITERING"            , calldescription2),
         calldescription2 = if_else(calldescription == "BACKGROUND/LEIN CHK / LIVESCAN"                    ,"BACKGROUND CHECK"           , calldescription2),
         calldescription2 = if_else(calldescription == "PURSUIT - VEHICLE OR FOOT"                         ,"PURSUIT"                    , calldescription2),
         calldescription2 = if_else(calldescription %in% c("ABNORMAL BREATHING","ATYPICAL SEIZURE OR HX OR CVA","BREATHING PROBLEMS DELTA","CO, INHALATION OR HAZMAT DELTA","FAINTING ALRT W/HX OR ABD PAIN","MEDICAL ALARM OR UNK PROB","OB HEMORRHAGE LABOR OR BIRTH","SERIOUS HEMORRHAGE","SERIOUS INJURIES"),"OTHER MEDICAL EMERGENCY", calldescription2),
         calldescription2 = if_else(calldescription %in% c("ATM  ALARM","BANK ALARM")                      ,"ATM OR BANK ALARM"          , calldescription2),
         calldescription2 = if_else(calldescription %in% c("ALARM MALFUNCTION","ALARM MISUSE")             ,"ALARM MALFUNCTION OR MISUSE", calldescription2),
         calldescription2 = if_else(calldescription %in% c("HOLD UP ALARM AND MOW","PANIC / DURESS ALARM","PORTABLE ALARM SYSTEM"),"PANIC ALARM", calldescription2),
         calldescription2 = if_else(calldescription %in% c("ALARM UNKNOWN CAUSE","TEMPERATURE ALARM")            ,"ALARM OTHER"                           , calldescription2),
         calldescription2 = if_else(calldescription %in% c("DPD DEMS DFD N TRO","ASSIST PERSONNEL")              ,"ASSIST EMERGENCY PERSONNEL"            , calldescription2),
         calldescription2 = if_else(calldescription %in% c("CITIZEN RADIO PATROL IN TROUBL","ASSIST CITIZEN","SENIOR CITIZEN ASSIST")    ,"ASSIST CITIZEN", calldescription2),
         calldescription2 = if_else(calldescription %in% c("INVESTIGATE PERSON", "INVESTIGATE YOUTH(S)")    ,"INVESTIGATE PERSON(S)"     , calldescription2),
         calldescription2 = if_else(calldescription %in% c("EMERGENCY STANDBY", "BE ON THE LOOK OUT")       ,"STAND-BY"                  , calldescription2))

# How many unique categories after recoding?
length(unique(detroit19_deploy_df$calldescription2)) # 103

# Create new df to consult recoding.
calldesc2_df <- detroit19_deploy_df %>% 
  select(calldescription, calldescription2) %>% 
  distinct() %>% 
  arrange(calldescription)

# Aggregate.
detroit_19_times_df <- detroit19_deploy_df %>% 
  group_by(calldescription2) %>% 
  summarise(sum_time = sum(totaltime),
            sum_counts = n()) %>% 
  mutate(total_time     = sum(sum_time),
         total_counts   = sum(sum_counts),
         prop_time      = 100*(round(sum_time/total_time, 6)),
         prop_counts    = 100*(round(sum_counts/total_counts, 6)) ) %>% 
  ungroup() 

# Check missings.
sum(is.na(detroit_19_times_df$prop_time)) # 0

# Base graphic.
detroit_19_times_df %>%
  filter(prop_time > 0.1) %>% 
  ggplot(mapping = aes(area = prop_time, label = calldescription2)) +
  geom_treemap(fill = "snow") +
  geom_treemap_text()

# Define the category types into vectors for clarity. Used later for recoding.
health_vec    <- c("MENTAL HEALTH",
                   "OTHER MEDICAL EMERGENCY",
                   "OVERDOSE",
                   "SUICIDE OR SUICIDE THREAT",
                   "WELL BEING CHECK")

proactive_vec <- c("BACKGROUND CHECK",
                   "BAIT",
                   "BUILDING CHECK",
                   "STAND-BY",
                   "ESCORT",
                   "INVESTIGATE AUTO",
                   "INVESTIGATE PERSON(S)",
                   "SPECIAL ATTENTION",
                   "TRANSPORT PRISONER",
                   "WARRANT")

qol_vec       <- c("DEAD PERSON OBSERVED",
                   "DISTURBANCE",
                   "EXPLOSION",
                   "FIRE",
                   "FIRE ALARM",
                   "LITTERING",
                   "MISC ACCIDENT",
                   "PARKING COMPLAINT",
                   "PROPERTY DAMAGE NON-CRIMINAL",
                   "SMOKING VIOLATIONS",
                   "SUSPICIOUS PACKAGE",
                   "YOUTH LOITERING")

traffic_vec   <- c("AID MOTORIST",
                   "AUTO ACCIDENT",
                   "BREATH TEST",
                   "HAZARDOUS CONDITIONS",
                   "LICENCE REVOCATION",
                   "MISC TRAFFIC",
                   "TOWING DETAIL",
                   "TRAFFIC INCIDENT",
                   "TRAFFIC STOP")

comm_vec      <- c("ALARM MALFUNCTION OR MISUSE",
                   "ALARM OTHER",
                   "ANIMAL INCIDENT",
                   "ASSIST CITIZEN",
                   "CHILD(REN) HOME ALONE",
                   "CURFEW VIOLATION",
                   "ENTRAPMENT",
                   "LOST PROPERTY",
                   "MISSING OR FOUND PERSON",
                   "NOISE COMPLAINT",
                   "PEACE OFFICER",
                   "SAFEWALK",
                   "SCHOOL CROSSING")

crime_vec     <- c("ARMED ROBBERY",
                   "ARSON",
                   "ASSAULT & BATTERY",
                   "ATM OR BANK ALARM",
                   "BOMB THREAT",
                   "BREAK & ENTER AUTO",
                   "BURGLAR ALARM",
                   "BURGLARY",
                   "CHILD OR ADULT ABUSE",
                   "DOMESTIC INCIDENT",
                   "EXTORTION",
                   "FELONIOUS ASSAULT",
                   "FRAUD",
                   "HARASSMENT",
                   "HIT & RUN",
                   "HOLDING PERSON",
                   "KIDNAPPING",
                   "LARCENY",
                   "LEWD & LASCIVIOUS CONDUCT",
                   "MALICIOUS DESTRUCTION",
                   "MOLESTATION",
                   "NARCOTICS",
                   "OTHER ASSAULT",
                   "PANIC ALARM",
                   "PERSON WITH WEAPON",
                   "PPO VIOLATION",
                   "RAPE",
                   "RECOVER AUTO",
                   "SCRAP METAL THEFT",
                   "SHOOTING OR STABBING WOUND",
                   "SHOTS",
                   "THREATS",
                   "UDAA",
                   "UNARMED ROBBERY")

unclas_vec    <- c("ADMIT OR E/E",
                   "ALR PT DISABLED / TIMEZONE CHG",
                   "ASSIST EMERGENCY PERSONNEL",
                   "ASSIST OTHER",
                   "BLUE LIGHT PHONE MALF",
                   "BUS BOARDING",
                   "DDOT TROUBLE",
                   "DPDA",
                   "HANGUP CALLS",
                   "INFORMATION/NON-CRIMINAL RPT",
                   "K-9 DEPLOYMENT",
                   "MT EMS-ENTRY",
                   "MT EMS-TRO/ENTRY",
                   "NOTIFICATION(S) MADE",
                   "ONE OVER THE WHEEL",
                   "OVER THE WHEEL",
                   "PERSONNEL IN TROUBLE",
                   "POWER LINES",
                   "PURSUIT",
                   "WRKABLE ARRST/OBV OR EXP DEATH")

# Add broad category.
detroit_19_times_df <- detroit_19_times_df %>% 
  mutate(type = if_else(calldescription2 %in% comm_vec     , "community"      , NULL),
         type = if_else(calldescription2 %in% crime_vec    , "crime"          , type),
         type = if_else(calldescription2 %in% health_vec   , "health"         , type),
         type = if_else(calldescription2 %in% proactive_vec, "proactive"      , type),
         type = if_else(calldescription2 %in% qol_vec      , "quality of life", type),
         type = if_else(calldescription2 %in% traffic_vec  , "traffic"        , type),
         type = if_else(calldescription2 %in% unclas_vec   , "unclassified"   , type)) %>% 
  arrange(type)

# Check missings.
sum(is.na(detroit_19_times_df)) # 0

# Create 'other' categories, aggregate.
detroit_19_times_agg_df <- detroit_19_times_df %>%
  mutate(calldescription2 = if_else(prop_time < 0.1, true  = "OTHER", false = calldescription2)) %>% 
  group_by(calldescription2, type) %>% 
  summarise(prop_time  = sum(prop_time),
            prop_count = sum(prop_counts)) %>% 
  ungroup() %>% 
  arrange(type)

# Check missings.
sum(is.na(detroit_19_times_agg_df)) # 0

# Descriptive stats. 
detroit_19_times_agg_df %>% 
  group_by(type) %>% 
  summarise(type_prop_count  = sum(prop_count),
            type_prop_time   = sum(prop_time)) %>% 
  ungroup()

# Demand time graphic.
detroit_19_times_agg_df %>%
  filter(type != "unclassified") %>%
ggplot(mapping = aes(area = prop_time, label = calldescription2, subgroup = type)) +
  geom_treemap(fill = "snow", colour = "lightgrey", size = 2, alpha = 0.5) +
  geom_treemap_text(padding.y = unit(0.3, "cm"), grow = FALSE, family = "serif", ) +
  geom_treemap_subgroup_text(padding.y = unit(0.3, "cm"), , place = "bottomleft", colour = "black", size = 48, family = "serif") +
  geom_treemap_subgroup_border(colour = "dodgerblue2", size = 4) +
  theme(legend.position = "none",
        panel.border  = element_rect(colour = "dodgerblue2", fill = "transparent", size = 2.5)) 

# Save.
ggsave(filename = "visuals/demand_time.png", height = 40, width = 60, unit = "cm", dpi = 300)

# For further descriptive statsitics, we join the new categories back with the raw data.
detroit19_deploy_df <- detroit19_deploy_df %>% 
  left_join(detroit_19_times_df)

# Check missings after join.
sum(is.na(detroit19_deploy_df$prop_counts))

# Recreate other category by type.
detroit19_deploy_df <- detroit19_deploy_df %>%
  mutate(calldescription2 = if_else(prop_time < 0.1, true  = "OTHER", false = calldescription2))

# Investigate missings in timestamps.
sum(is.na(detroit19_deploy_df$call_timestamp)) # 0

# Create date class, round to nearest hour, create day of week label.
detroit19_deploy_df <- detroit19_deploy_df %>% 
  mutate(call_timestamp_l  = as_datetime(call_timestamp),
         call_timestamp_lr = round_date(call_timestamp_l, "hour")) %>% 
  separate(col = call_timestamp_lr, into = c("date_lr", "time_lr"), sep = " ", remove = FALSE) %>%
  mutate(week_day = wday(date_lr, label = TRUE, abbr = FALSE),
         week_day = fct_relevel(week_day, "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

# Create small example of detroit19_deploy_df for manual viewing.
mini_df <- detroit19_deploy_df %>% 
  slice(1:100)

# Aggregate by date and hour.
dh_agg_df <- detroit19_deploy_df %>% 
  group_by(date_lr, time_lr, week_day, type) %>% 
  summarise(call_count = n()) %>% 
  ungroup() %>% 
  group_by(time_lr, week_day, type) %>% 
  summarise(mean_count = mean(call_count)) %>% 
  ungroup()

# Split data frame into list by type.
dh_agg_list <- group_split(dh_agg_df, type)

# Heatmap graphic.
dh_agg_hm_list <- lapply(dh_agg_list, function(x){
  ggplot(data = x) +
    geom_tile(mapping = aes(x = time_lr, y = week_day, fill = mean_count)) +
    theme_void() +
    theme(legend.position = "none")
})

# Arrange graphic.
plot_grid(plotlist = dh_agg_hm_list, ncol = 1)

# Investigate missings in coordinates.

# Load in boundaries of Detroit.

# Create 100x100 metre grid cell over the city.

# Aggregate points to cells by broad category.

# Spatial patterning graphics.


