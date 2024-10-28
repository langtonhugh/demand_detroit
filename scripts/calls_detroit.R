# Load libraries.
library(ggspatial)
library(nngeo)
library(cowplot)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(forcats)
library(stringr)
library(lubridate)
library(treemapify)
library(ggplot2)
library(sf)

# Useful function.
`%nin%` <- Negate(`%in%`)

# 2016-2021 911 calls for service from Detroit open data portal.
# https://data.detroitmi.gov/datasets/911-calls-for-service?geometry=-86.035%2C42.028%2C-80.808%2C42.738

# Direct download link.
# download.file(url = "https://opendata.arcgis.com/datasets/4f49eb825f564efa9a23cd103c4ba13b_0.csv",
#               destfile = "data/detroit_16_21.csv")

# Load.
detroit_df <- read_csv("data/detroit_16_21.csv")

# Initial filter.
sum(is.na(detroit_df$officerinitiated)) # 0

detroit_df <- detroit_df %>% 
  filter(str_detect(call_timestamp, "2019"), # 2019 only.
         officerinitiated == "No")           # exclude officer initiated calls.

# Date checks.
detroit_df %>% 
  mutate(year = as.character(str_extract_all(call_timestamp, "^.{4}"))) %>% 
  group_by(year) %>% 
  summarise(counts = n()) %>% 
  ungroup()

# Check time variables variables. 
detroit_df %>% 
  select(calldescription, dispatchtime, traveltime, totalresponsetime, time_on_scene, totaltime)

# Missings in key variables.
sum(is.na(detroit_df$calldescription))
sum(is.na(detroit_df$totaltime)) 
sum(is.na(detroit_df$time_on_scene)) # some overlap with above

# Unique id.
length(unique(detroit_df$incident_id)) # 419193. Same as the number of rows.

# Remove missings, zeros and negatives.
detroit19_sub_df <- detroit_df %>% 
  # drop_na(totaltime, time_on_scene) %>%        # note the overlap.
  drop_na(traveltime, time_on_scene) %>%         # note the overlap.
  filter(traveltime >=0, time_on_scene >0) %>%   # has to be time on scene, but no travel time is ok.
  mutate(totaltime = traveltime + time_on_scene) # this overwrite the original measure.

# Check.
nrow(detroit19_sub_df) # 293025

# First, filter out calldescriptions which are admin, completely unknown or do not appear to
# involve deployment.
detroit19_deploy_df <- detroit19_sub_df %>%
  filter(calldescription != "REMARKS", 
         calldescription != "START OF SHIFT INFORMATION",
         calldescription != "UNKNOWN PROBLEM",
         calldescription != "VIRTUAL SPECIAL ATTENTION", 
         calldescription != "EMPLOYEE CALL IN / TIME OFF",
         calldescription != "CALL BACK DESK")

# How many categories remain?
length(unique(detroit19_deploy_df$calldescription)) # 207

# Create df to consult names for suitable recoding.
calldesc_df <- tibble(calldescription = unique(detroit19_deploy_df$calldescription)) %>% 
  arrange(calldescription)

# Recode raw calldescription into broader categories and rename to short characters as appropriate.
# We also remove 'warrant' and 'transport prisoner' are considered not officer initiated.
detroit19_deploy_df <- detroit19_deploy_df %>% 
  mutate(calldescription2 = if_else(str_detect(calldescription, "DV")                                      ,"DOMESTIC INCIDENT"          , calldescription),
         calldescription2 = if_else(str_detect(calldescription, "MENTAL")                                  ,"MENTAL HEALTH"              , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "SUICIDE")                                 ,"SUICIDE OR SUICIDE THREAT"  , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "HIT & RUN|HIT& RUN|H&R")                  ,"HIT AND RUN"                  , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "LOST PROPERTY|RECOVERED / FOUND PROPERTY"),"LOST PROPERTY"              , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "AUTO X")                                  ,"AUTO ACCIDENT"              , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "ASSAULT AND BATTERY")                     ,"ASSAULT AND BATTERY"          , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "ASSAULT DANGEROUSOR SERIOUS|ASSAULT OR")  ,"OTHER ASSAULT"              , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "FELONIOUS ASSAULT")                       ,"FELONIOUS ASSAULT"          , calldescription2),
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
         calldescription2 = if_else(str_detect(calldescription, "LEWD AND LASCIVIOUS")                     ,"LEWD AND LASCIVIOUS CONDUCT"  , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "UDAA")                                    ,"UDAA"                       , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "AID MOTORIST")                            ,"AID MOTORIST"               , calldescription2),
         calldescription2 = if_else(str_detect(calldescription, "BREAKING AND|BREAKING &")                 ,"BREAK AND ENTER AUTO"         , calldescription2),
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
         calldescription2 = if_else(calldescription %in% c("EMERGENCY STANDBY", "BE ON THE LOOK OUT")       ,"STAND-BY"                  , calldescription2)) %>% 
  filter(calldescription2 != "WARRANT" & calldescription2 != "TRANSPORT PRISONER") 

# How many unique categories after recoding?
length(unique(detroit19_deploy_df$calldescription2)) # 99

# Create new df to consult recoding.
calldesc2_df <- detroit19_deploy_df %>% 
  select(calldescription, calldescription2) %>% 
  distinct() %>% 
  arrange(calldescription)

# Save for reader reference.
# calldesc2_df %>% 
#   rename(calldescription_original = calldescription,
#          calldescription_new      = calldescription2) %>% 
# write_csv(file = "results/categorisation_summary.csv")

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
                   "SPECIAL ATTENTION")
                   # "TRANSPORT PRISONER",
                   # "WARRANT")

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
                   "ASSAULT AND BATTERY",
                   "ATM OR BANK ALARM",
                   "BOMB THREAT",
                   "BREAK AND ENTER AUTO",
                   "BURGLAR ALARM",
                   "BURGLARY",
                   "CHILD OR ADULT ABUSE",
                   "DOMESTIC INCIDENT",
                   "EXTORTION",
                   "FELONIOUS ASSAULT",
                   "FRAUD",
                   "HARASSMENT",
                   "HIT AND RUN",
                   "HOLDING PERSON",
                   "KIDNAPPING",
                   "LARCENY",
                   "LEWD AND LASCIVIOUS CONDUCT",
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
# We now also remove prisoner and warrant calls, as these are deemed to officer-initiated.
detroit_19_times_df <- detroit_19_times_df %>% 
  mutate(type = if_else(calldescription2 %in% comm_vec     , "community"      , NA),
         type = if_else(calldescription2 %in% crime_vec    , "crime"          , type),
         type = if_else(calldescription2 %in% health_vec   , "health"         , type),
         type = if_else(calldescription2 %in% proactive_vec, "proactive"      , type),
         type = if_else(calldescription2 %in% qol_vec      , "quality of life", type),
         type = if_else(calldescription2 %in% traffic_vec  , "traffic"        , type),
         type = if_else(calldescription2 %in% unclas_vec   , "unclassified"   , type)) %>% 
  arrange(type)

# Check missings.
sum(is.na(detroit_19_times_df)) # 0

# Create 'other' categories for incident <0.2% deployed time, then aggregate.
detroit_19_times_agg_df <- detroit_19_times_df %>%
  mutate(calldescription2 = if_else(prop_time < 0.2, true  = "OTHER", false = calldescription2)) %>% 
  group_by(calldescription2, type) %>% 
  summarise(freq            = sum(sum_counts),
            prop_time       = sum(prop_time),
            sum_time        = sum(sum_time), # optional
            prop_count      = sum(prop_counts)) %>% 
  ungroup() %>% 
  arrange(type)

# Check missings.
sum(is.na(detroit_19_times_agg_df)) # 0

# How many incidents in total?
sum(detroit_19_times_agg_df$freq) # 258786

# Descriptive stats. 
des_stats_df <- detroit_19_times_agg_df %>% 
  group_by(type) %>% 
  summarise(`Count`      = sum(freq),
            `Count (%)`  = round(sum(prop_count), 2),
            `Total deployed time (%)`   = round(sum(prop_time ), 2)) %>% 
            # `Total deployed time (shifts)` = round((sum(sum_time)/60)/7.5, 0) )  %>%
  rename(`Demand type` = type) %>% 
  ungroup()

# Save.
write_csv(x = des_stats_df, file = "results/table1_des_stats_total_time.csv")

# Create categorical colour scheme for future use. Colourblind friendly.
# col_vec <- c("#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84")

# Save data used for proportional breakdown.
write_csv(x = detroit_19_times_agg_df, file = "results/prop_breakdown_total_time.csv")

# Create label variable with line break between words. This for visual purposes only.
detroit_19_times_agg_df <- detroit_19_times_agg_df %>% 
  mutate(calldescription2_labs = str_replace_all(string = calldescription2, pattern = " ", replacement = "\n"))

length(unique(detroit_19_times_agg_df$calldescription2)) 

# Create main colour.
viridis_1 <- viridis::viridis(5)[2]

# Demand time graphic.
time_gg <- detroit_19_times_agg_df %>%
  filter(type != "unclassified") %>%
  ggplot(mapping = aes(area = prop_time, label = calldescription2_labs, subgroup = type)) +
  geom_treemap(fill = "snow", colour = "darkgrey", size = 2, alpha = 0.5) +
  geom_treemap_text(padding.y = unit(0.3, "cm"), grow = FALSE) +
  geom_treemap_subgroup_border(colour = viridis_1, size = 4) +
  theme_void() +
  theme(panel.border  = element_rect(colour = viridis_1, fill = "transparent", size = 1.5))

# Portrait version annotations.
time_ann_gg <- time_gg +
  theme(plot.margin = unit(c(1.5,1.5,1.5,1.5), "cm")) +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1), clip = "off") +
  annotate(geom = "text", label = "crime"          , colour = viridis_1, 
           size = 9, x = 0.22, y = 1.065) +
  annotate(geom = "text", label = "quality of life", colour = viridis_1, 
           size = 9, x = 0.75, y = -0.065) +
  annotate(geom = "text", label = "health"      , colour = viridis_1, 
           size = 9, x = 0.59, y = 1.065) +
  annotate(geom = "text", label = "proactive"      , colour = viridis_1, 
           size = 9, x = 0.88, y = 1.065) +
  annotate(geom = "text", label = "traffic"         , colour = viridis_1,
           size = 9, x = 1.07, y = 0.53, angle = -90) +
  annotate(geom = "text", label = "community"         , colour = viridis_1,
           size = 9, x = 1.07, y = 0.77, angle = -90) +
  annotate(geom = "text", label = "PPO = Personal Protection Order",
           size = 4, x = -0.045, y = -0.03, hjust = 0) +
  annotate(geom = "text", label = "UDAA = Unlawfully Driving Away of an Automobile",
           size = 4, x = -0.045, y = -0.04, hjust = 0)

# Save portrait version (tt = total tiome)
ggsave(filename = "visuals/fig1_time_total_time.png", height = 48, width = 40, unit = "cm", dpi = 300)

# For further descriptive statistics, we join the new categories back with the raw data.
detroit19_deploy_df <- detroit19_deploy_df %>% 
  left_join(detroit_19_times_df)

# Check missings after join.
sum(is.na(detroit19_deploy_df$prop_counts))

# Create other category again if needed.
detroit19_deploy_df <- detroit19_deploy_df %>%
  mutate(calldescription2 = if_else(prop_time < 0.2, true  = "OTHER", false = calldescription2))

# Investigate missings in timestamps.
sum(is.na(detroit19_deploy_df$call_timestamp)) # 0

# Create date class, round to nearest hour, create day of week label.
detroit19_deploy_df <- detroit19_deploy_df %>% 
  mutate(call_timestamp_l  = as_datetime(call_timestamp),
         call_timestamp_lr = round_date(call_timestamp_l, "hour")) %>% 
  separate(col = call_timestamp_lr, into = c("date_lr", "time_lr"), sep = " ", remove = FALSE) %>%
  mutate(week_day = wday(date_lr, label = TRUE, abbr = FALSE),
         week_day = fct_relevel(week_day, "Sunday","Saturday","Friday","Thursday","Wednesday","Tuesday","Monday"))

# Create small example of detroit19_deploy_df for manual viewing.
mini_df <- detroit19_deploy_df %>% 
  slice(1:100)

# Check categories again.
length(unique(detroit19_deploy_df$calldescription2)) # 52

# Aggregate by date and hour.
dh_agg_df <- detroit19_deploy_df %>% 
  group_by(date_lr, time_lr, week_day, type) %>% 
  summarise(call_count = n()) %>% 
  ungroup() %>% 
  group_by(time_lr, week_day, type) %>% 
  summarise(mean_count = mean(call_count)) %>% 
  ungroup() %>% 
  filter(type != "unclassified")

# Split data frame into list by type.
dh_agg_list <- group_split(dh_agg_df, type)

# Heatmap graphic.
dh_agg_hm_list <- lapply(dh_agg_list, function(x){
  ggplot(data = x) +
    geom_tile(mapping = aes(x = time_lr, y = week_day, fill = mean_count)) +
    scale_x_discrete(labels = 1:24) +
    # scale_fill_continuous(guide = "colourbar", low = "snow", high = "dodgerblue3",
    #                       breaks = scales::pretty_breaks(n = 3)) +
    scale_fill_viridis_c(guide = "colourbar", breaks = scales::pretty_breaks(n = 3)) +
    guides(fill = guide_colourbar(barwidth = 0.5, barheight = 4)) +
    labs(fill = NULL, x = NULL, y = NULL) +
    theme_minimal() +
    theme(legend.text = element_text(size = 5),
          axis.text   = element_text(size = 6), 
          legend.text.align = 0.5)
})

# Arrange and annotate graphic.
time_heat_gg <- plot_grid(plotlist = dh_agg_hm_list,
                          ncol = 1,
                          labels = unique(dh_agg_df$type),
                          label_size = 8, label_fontface = "plain",
                          hjust = 0.5, label_x = 0.5,
                          scale = 0.9) +
  theme(plot.margin = unit(c(0,0,0.2,0), "cm")) +
  annotate(geom = "text", label = "hours of the day",
           x = 0.5, y = 0, size = 2)

# Save.
ggsave(filename = "visuals/fig3_time_heat_total_time.png", height = 20, width = 14, unit = "cm", dpi = 300)

# Investigate missings in coordinates.
sum(is.na(detroit19_deploy_df$latitude))  # 0
sum(is.na(detroit19_deploy_df$longitude)) # 0

# Unique values.
duplicate_coords_df <- detroit19_deploy_df %>% 
  distinct(latitude, longitude)

length(unique(detroit19_deploy_df$longitude)) # 16324
length(unique(detroit19_deploy_df$latitude))  # 16324
length(unique(duplicate_coords_df$longitude)) # 16324
length(unique(duplicate_coords_df$latitude))  # 16324

# Check sample of incidents.
set.seed(1612)

detroit_sample_sf <- detroit19_deploy_df %>% 
  filter(type == "traffic") %>% 
  sample_n(size = 10000) %>% 
  st_as_sf(coords = c(x = "longitude", y = "latitude"), crs = 4326) %>% 
  st_transform(2253)

# We get spurious coordinates. Clip needed.
# ggplot(data = detroit_sample_sf) +
#   geom_sf()

# Save csv for exploration in QGIS. I know retrospectively that there is a spurious hotspot,
# which is ~wasteland, likely due to unknown locations being geocoded to a specific street.
# detroit_sample_sf %>% 
#   as_tibble() %>% 
#   write_csv(file = "data/detroit_sample.csv")

# Check categories again.
length(unique(detroit19_deploy_df$calldescription2)) # 52

# Check counts total.
nrow(detroit19_deploy_df) # 258786

# This makes it clear that many incidents for which the location is not known are geocoded
# to a specific arbitrary location (-83.111560213, 42.3003668800001).

# What percentage of incidents have a *known* location? Defined as known zipcode. This leaves only
# those with complete coordinates and excluded 'unknown' location.
detroit19_deploy_known_df <- detroit19_deploy_df %>% 
  drop_na(zip_code)

nrow(detroit19_deploy_known_df)/nrow(detroit19_deploy_df) # 98% are 'known'. Proceed with these.

# Load in nhood boundaries of Detroit. Shapefile downloaded from https://data.detroitmi.gov/datasets/current-city-of-detroit-neighborhoods/explore?location=42.352721%2C-83.099208%2C11.13.
detroit_sf <- st_read("data/Current_City_of_Detroit_Neighborhoods.shp")

# Transform CRS.
detroit_sf <- detroit_sf %>% 
  st_transform(2253)

# Create cfs sf object and transform.
detroit19_deploy_known_sf <- detroit19_deploy_known_df %>% 
  st_as_sf(coords = c(x = "longitude", y = "latitude"), crs = 4326) %>% 
  st_transform(2253) 

# Dissolve nhood boundaries as best we can.
diss_df <- detroit_sf %>%
  mutate(n = 1) %>%
  group_by(n) %>%
  summarise(detroit = 1) %>%
  ungroup()

# Dissolve into council boundaries.
council_sf <- detroit_sf %>%
  group_by(council_di) %>% 
  summarise(detroit = 1) %>% 
  ungroup()

# Remove holes. Note the legitimate hole for Highland Park + Hamtramck.
detroit_uni_sf <- st_remove_holes(diss_df, max_area = 10000) # 10000 will keep the parks, 0 for blank.
council_sf <- st_remove_holes(council_sf, max_area = 0) 

# Clip incident points to the Detroit boundary.
detroit19_deploy_clip_sf <- detroit19_deploy_known_sf %>% 
  st_intersection(detroit_uni_sf)

# Create 1000x1000ft grid over Detroit.
detroit_grid_sf <- detroit_uni_sf %>% 
  # st_buffer(dist = 1000) %>% # To check if grids miss incidents near boundary.
  st_make_grid(cellsize = 1000) %>% 
  st_as_sf() 

# Plot.
# ggplot() +
#   geom_sf(data = detroit_grid_sf) +
#   geom_sf(data = detroit_uni_sf, colour = "red", fill = "transparent")

# Clip the grid to the Detroit boundary.
detroit_grid_sf <- detroit_grid_sf %>% 
  st_intersection(detroit_uni_sf)

# Plot.
# ggplot() +
#   geom_sf(data = detroit_grid_sf) +
#   geom_sf(data = detroit_uni_sf, colour = "red", fill = "transparent")

# Check counts used in maps. Lower due to incomplete coordinates.
sum(detroit19_deploy_clip_sf$n) # 246913

# Split incident sf object into list.
detroit19_deploy_clip_list <- detroit19_deploy_clip_sf %>% 
  filter(type != "unclassified") %>% 
  group_split(type)

# Create list of the duplicate grid sf objects to match. Not an ideal approach but it works.
grids_list <- rep(list(detroit_grid_sf), 6) # run this instead if the below produced length of 18.
# grids_list <- list(detroit_grid_sf, detroit_grid_sf, detroit_grid_sf,
#                    detroit_grid_sf, detroit_grid_sf, detroit_grid_sf)


# Create point (incidents) to polygon (grids) function.
p2p_fun <- function(x, y){
  x %>% 
    st_as_sf() %>% 
    mutate(call_count = lengths(st_intersects(x, y)))
}

# Run p2p through lists of incidents and duplicate grids.
detroit19_grid_list <- map2(grids_list, detroit19_deploy_clip_list, p2p_fun)

# Check counts for each demand type. Note these counts as slightly lower than
# the Table 1 counts because we lost 2% due to incomplete coordinates.
# This confirms that the list remains in alphabetical order.
lapply(detroit19_grid_list, function(x){sum(x$call_count)})

# Save specific maps for exploration in QGIS (optional).
names(detroit19_grid_list) <- unique(dh_agg_df$type)

# for (i in 1:length(detroit19_grid_list)) {
#   st_write(obj = detroit19_grid_list[[i]],
#            dsn = paste("results/", names(detroit19_grid_list[i]), "_map.shp", sep = ""))
# }

# Generate maps of incident counts by type.
grid_maps_list <- lapply(detroit19_grid_list, function(x){
  ggplot() +
    geom_sf(data = x, mapping = aes(fill = call_count), colour = "transparent") +
    # geom_sf(data = detroit_uni_sf, fill = NA, colour = "grey78") +
    scale_fill_viridis_c(guide = "colourbar", n.breaks = 2, alpha = 0.9,
                          limits = c(0,max(x$call_count))) +
    labs(fill = NULL) +
    guides(fill = guide_colourbar(barwidth = 9, barheight = 0.6, draw.ulim = FALSE,
                                  ticks.colour = "black", ticks.linewidth = 0.5)) +
    theme_void() +
    theme(legend.text = element_text(size = 11),
          legend.position = c(0.7,0.24),
          legend.direction = "horizontal",
          legend.box = "horizontal")
})

# Adjust quality of life limits due to rounded max, and annotate north arrow and scale.
# Note that this replaces an existing element and replaces previous fill layer.
grid_maps_list[[5]] <- grid_maps_list[[5]] +
  scale_fill_viridis_c(guide = "colourbar", n.breaks = 2, alpha = 0.9,
                        limits = c(0,20+max(detroit19_grid_list[[5]]$call_count))) +
  annotation_scale(pad_x = unit(1, "cm"), pad_y = unit(0.1, "cm"), line_width = 1, text_cex = 1, style = "ticks") +
  annotation_north_arrow(pad_x = unit(2.8, "cm"), pad_y = unit(1.5, "cm"),
                         style = north_arrow_fancy_orienteering)

# Annotate select maps with key locations. Again, this replaces the existing elements and fill layer.

# community
grid_maps_list[[1]] <- grid_maps_list[[1]] +
  annotate(geom = "text"    , x = 13467040, y = 322652, label = "WSU campus & Midtown", size = 4, colour = "snow") +
  annotate(geom = "curve" , x = 13467040, y = 320672, xend = 13471640, yend = 314242, size = 0.7,
           arrow = arrow(length = unit(0.01, "npc")), curvature = 0.3, colour = "snow") +
  scale_fill_viridis_c(guide = "colourbar", n.breaks = 2, alpha = 0.9) 


# crime
grid_maps_list[[2]] <- grid_maps_list[[2]] +
  annotate(geom = "text"  , x = 13465040, y = 324802, label = "Henry Ford Hospital", size = 4, colour = "snow") +
  annotate(geom = "curve" , x = 13465040, y = 322952, xend = 13469040, yend = 318852, size = 0.7,
           arrow = arrow(length = unit(0.01, "npc")), curvature = 0.3, colour = "snow") +
  annotate(geom = "text"    , x = 13497168, y = 337022, label = "Ascension St. John Hospital", size = 4, colour = "snow") +
  annotate(geom = "curve" , x = 13511168, y = 337062, xend = 13515568, yend = 337562, size = 0.7,
           arrow = arrow(length = unit(0.01, "npc")), curvature = 0.1, colour = "snow") +
  annotate(geom = "text"  , x = 13434648, y = 331826, label = "DMC Sinai Grace Hospital", size = 4, colour = "snow") +
  annotate(geom = "curve" , x = 13437648, y = 333049, xend = 13442046, yend = 336525, size = 0.7,
           arrow = arrow(length = unit(0.01, "npc")), curvature = -0.2, colour = "snow")

# health. label not included.
# grid_maps_list[[3]] <- grid_maps_list[[3]] +
#   annotate(geom = "text"  , x = 13481089, y = 322116, label = "Mental health service facility", size = 4) +
#   annotate(geom = "curve" , x = 13482240, y = 320016, xend = 13485816, yend = 317571, size = 0.7,
#            arrow = arrow(length = unit(0.01, "npc")), curvature = 0.3) +
#   scale_fill_continuous(guide = "colourbar", low = "snow", high = "dodgerblue3", breaks = c(0,30,60)) 

# quality of life. Note that we also slightly adjust the scale due to rounding making the breaks odd.
grid_maps_list[[5]] <- grid_maps_list[[5]] +
  scale_fill_viridis_c(guide = "colourbar", n.breaks = 2, alpha = 0.9,
                        limits = c(0,1.2*+max(detroit19_grid_list[[5]]$call_count))) +
  annotate(geom = "text"    , x = 13490040, y = 301652, label = "Downtown", size = 4) +
  annotate(geom = "curve" , x = 13484240, y = 301752, xend = 13481040, yend = 304752, size = 0.7,
           arrow = arrow(length = unit(0.01, "npc")), curvature = -0.3) +
  annotation_scale(pad_x = unit(1, "cm"), pad_y = unit(0.1, "cm"), line_width = 1, text_cex = 1, style = "ticks") +
  annotation_north_arrow(pad_x = unit(2.8, "cm"), pad_y = unit(1.5, "cm"),
                         style = north_arrow_fancy_orienteering)

# Arrange maps.
maps_gg <-  plot_grid(plotlist = grid_maps_list,
                      ncol = 2,
                      labels = unique(dh_agg_df$type),
                      label_size = 16, label_fontface = "plain",
                      hjust = 0.5, label_x = 0.5,
                      scale = 1.05)
# Save.
ggsave(filename = "visuals/fig5_maps_counts.png", height = 48, width = 40, unit = "cm", dpi = 300)

# Save workspace.
# save.image(file = "data/workspce_total_time.RData")
