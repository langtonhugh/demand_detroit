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

# Check time variables variables. Totaltime = totalresponsetime + time_on_scene.
# Note that some missings still have a total time allocated.
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
  drop_na(totaltime, time_on_scene) %>%  # note the overlap.
  filter(totaltime >0, time_on_scene >0)

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

# Save for reader reference. No need to save again.
# calldesc2_df %>% 
#   rename(calldescription_original = calldescription,
#          calldescription_new      = calldescription2) %>% 
# write_csv(file = "results/categorisation_summary.csv")

# Aggregate.
detroit_19_times_df <- detroit19_deploy_df %>% 
  group_by(calldescription2) %>% 
  summarise(sum_time = sum(time_on_scene),
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

# Create 'other' categories for incident <0.2% deployed time, then aggregate.
detroit_19_times_agg_df <- detroit_19_times_df %>%
  mutate(calldescription2 = if_else(prop_time < 0.2, true  = "OTHER", false = calldescription2)) %>% 
  group_by(calldescription2, type) %>% 
  summarise(freq       = sum(sum_counts),
            prop_time  = sum(prop_time),
            prop_count = sum(prop_counts)) %>% 
  ungroup() %>% 
  arrange(type)

# Check missings.
sum(is.na(detroit_19_times_agg_df)) # 0

# How many incidents in total?
sum(detroit_19_times_agg_df$freq) # 258773


# Descriptive stats. 
des_stats_df <- detroit_19_times_agg_df %>% 
  group_by(type) %>% 
  summarise(`Count`      = sum(freq),
            `Count (%)`  = round(sum(prop_count), 2),
            `Time on scene (%)`   = round(sum(prop_time ), 2)) %>%
  rename(`Demand type` = type) %>% 
  ungroup()

# Save.
write_csv(x = des_stats_df, file = "results/table1_des_stats_tos.csv")

# Create categorical colour scheme for future use. Colourblind friendly.
# col_vec <- c("#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84")

# Save data used for proportional breakdown.
write_csv(x = detroit_19_times_agg_df, file = "results/prop_breakdown_tos.csv")

# Create label variable with line break between words. This for visual purposes only.
detroit_19_times_agg_df <- detroit_19_times_agg_df %>% 
  mutate(calldescription2_labs = str_replace_all(string = calldescription2, pattern = " ", replacement = "\n"))

length(unique(detroit_19_times_agg_df$calldescription2)) # 50

# Demand time graphic.
time_gg <- detroit_19_times_agg_df %>%
  filter(type != "unclassified") %>%
  ggplot(mapping = aes(area = prop_time, label = calldescription2_labs, subgroup = type)) +
  geom_treemap(fill = "snow", colour = "darkgrey", size = 2, alpha = 0.5) +
  geom_treemap_text(padding.y = unit(0.3, "cm"), grow = FALSE) +
  geom_treemap_subgroup_border(colour = "dodgerblue4", size = 4) +
  theme_void() +
  theme(panel.border  = element_rect(colour = "dodgerblue4", fill = "transparent", size = 1.5))

# Portrait version annotations.
time_ann_gg <- time_gg +
  theme(plot.margin = unit(c(1.5,1.5,1.5,1.5), "cm")) +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1), clip = "off") +
  annotate(geom = "text", label = "crime"          , colour = "dodgerblue4", 
           size = 9, x = 0.22, y = 1.065) +
  annotate(geom = "text", label = "traffic", colour = "dodgerblue4", 
           size = 9, x = 0.8, y = -0.065) +
  annotate(geom = "text", label = "health"      , colour = "dodgerblue4", 
           size = 9, x = 0.645, y = 1.065) +
  annotate(geom = "text", label = "proactive"      , colour = "dodgerblue4", 
           size = 9, x = 0.9, y = 1.065) +
  annotate(geom = "text", label = "quality of life"         , colour = "dodgerblue4",
           size = 9, x = 1.07, y = 0.51, angle = -90) +
  annotate(geom = "text", label = "community"         , colour = "dodgerblue4",
           size = 9, x = 1.07, y = 0.78, angle = -90) +
  annotate(geom = "text", label = "PPO = Personal Protection Order",
           size = 4, x = -0.045, y = -0.03, hjust = 0) +
  annotate(geom = "text", label = "UDAA = Unlawfully Driving Away of an Automobile",
           size = 4, x = -0.045, y = -0.04, hjust = 0)

# Save portrait version.
ggsave(filename = "visuals/fig1_time_port_tos.png", height = 48, width = 40, unit = "cm", dpi = 300)

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
length(unique(detroit19_deploy_df$calldescription2)) # 50

# We use the same incidents for counts/time on scene, so there's no need to recreate the heatmap.
# If run, it will just create an idenical visual.

# Having said that: we run this chunk because it gives us labels later on!
# # Aggregate by date and hour.
dh_agg_df <- detroit19_deploy_df %>%
  group_by(date_lr, time_lr, week_day, type) %>%
  summarise(call_count = n()) %>%
  ungroup() %>%
  group_by(time_lr, week_day, type) %>%
  summarise(mean_count = mean(call_count)) %>%
  ungroup() %>%
  filter(type != "unclassified")
# 
# # Split data frame into list by type.
# dh_agg_list <- group_split(dh_agg_df, type)
# 
# # Heatmap graphic.
# dh_agg_hm_list <- lapply(dh_agg_list, function(x){
#   ggplot(data = x) +
#     geom_tile(mapping = aes(x = time_lr, y = week_day, fill = mean_count)) +
#     scale_x_discrete(labels = 1:24) +
#     scale_fill_continuous(guide = "colourbar", low = "snow", high = "dodgerblue3",
#                           breaks = scales::pretty_breaks(n = 3)) +
#     guides(fill = guide_colourbar(barwidth = 0.5, barheight = 4)) +
#     labs(fill = NULL, x = NULL, y = NULL) +
#     theme_minimal() +
#     theme(legend.text = element_text(size = 5),
#           axis.text   = element_text(size = 6), 
#           legend.text.align = 0.5)
# })
# 
# # Arrange and annotate graphic.
# time_heat_gg <- plot_grid(plotlist = dh_agg_hm_list,
#                           ncol = 1,
#                           labels = unique(dh_agg_df$type),
#                           label_size = 8, label_fontface = "plain",
#                           hjust = 0.5, label_x = 0.5,
#                           scale = 0.9) +
#   theme(plot.margin = unit(c(0,0,0.2,0), "cm")) +
#   annotate(geom = "text", label = "hours of the day",
#            x = 0.5, y = 0, size = 2)
# 
# # Save.
# ggsave(filename = "visuals/fig2_time_heat_tos.png", height = 20, width = 20, unit = "cm", dpi = 300)

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
ggplot(data = detroit_sample_sf) +
  geom_sf()

# Save csv for exploration in QGIS. I know retrospectively that there is a spurious hotspot,
# which is ~wasteland, likely due to unknown locations being geocoded to a specific street.
# detroit_sample_sf %>% 
#   as_tibble() %>% 
#   write_csv(file = "data/detroit_sample.csv")

# Check categories again.
length(unique(detroit19_deploy_df$calldescription2)) # 50

# Check counts total.
nrow(detroit19_deploy_df) # 258773

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

# Remove holes. Note the legitimate hole for Highland Park + Hamtramck.
detroit_uni_sf <- st_remove_holes(diss_df, max_area = 0) # 10000 will keep the parks.

# Clip incident points to the Detroit boundary.
detroit19_deploy_clip_sf <- detroit19_deploy_known_sf %>% 
  st_intersection(detroit_uni_sf)

# Create 1000x1000ft grid over Detroit.
detroit_grid_sf <- detroit_uni_sf %>% 
  # st_buffer(dist = 1000) %>% # To check if grids miss incidents near boundary.
  st_make_grid(cellsize = 1000) %>% 
  st_as_sf() %>% 
  mutate(grid_id = 1:nrow(.))

# Check counts used in maps. Lower due to incomplete coordinates.
sum(detroit19_deploy_clip_sf$n) # 246971

# Split incident sf object into list.
detroit19_deploy_clip_list <- detroit19_deploy_clip_sf %>% 
  filter(type != "unclassified") %>% 
  group_split(type)

# Create some describe stats for the histogram.
# Explore distributions. Noting the spike at 30 minutes. Outliers removed just for this.
means_df <- detroit19_deploy_clip_sf %>% 
  as_tibble() %>% 
  group_by(type) %>% 
  summarise(mean_type   = round(mean(time_on_scene)  , 2),
            median_type = round(median(time_on_scene), 2),
            min_type    = round(min(time_on_scene)   , 2),
            max_type    = round(max(time_on_scene)   , 2),
            sd_type     = round(sd(time_on_scene)    , 2) ) %>% 
  ungroup() %>% 
  select(type, mean_type, median_type, min_type, max_type, sd_type) %>% 
  filter(type != "unclassified")

# Plot histogram.
histo_gg <- ggplot() +
  geom_histogram(data = filter(detroit19_deploy_clip_sf, type != "unclassified"),
                 mapping = aes(x = time_on_scene), bins = 100, fill = "dodgerblue3") +
  geom_vline(data = means_df, mapping = aes(xintercept = mean_type)  , linetype = "dotted") +
  geom_vline(data = means_df, mapping = aes(xintercept = median_type), linetype = "dotted") +
  geom_vline(data = means_df, mapping = aes(xintercept = max_type)   , linetype = "dotted") +
  facet_wrap(~ type, scales = "free_y", ncol = 1) +
  labs(y = NULL, x = "Minutes spent on scene") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 14) ) 

# Find relative size of y-axis in the facet, in order to add the descriptive stats. This might be done
# better with a loop on each plot, but this works for now.
build_gg <- ggplot_build(histo_gg)     
build_gg <- build_gg$data              
build_gg <- as.data.frame(build_gg[1])

means_df <- build_gg %>% 
  group_by(PANEL) %>% 
  summarise(max_y = max(ymax)) %>% 
  ungroup() %>% 
  mutate(type = unique(dh_agg_df$type)) %>% 
  left_join(means_df)

# Add descriptive stats to histogram.
histo_comp_gg <- histo_gg +
  geom_text(data = means_df, mapping = aes(label = paste("Mean:"  , mean_type, sep = " ")  , x = 600, y = max_y*0.7), hjust = 0, size = 6) +
  geom_text(data = means_df, mapping = aes(label = paste("Median:", median_type, sep = " "), x = 600, y = max_y*0.6), hjust = 0, size = 6) +
  geom_text(data = means_df, mapping = aes(label = paste("SD:"    , sd_type    , sep = " "), x = 600, y = max_y*0.5), hjust = 0, size = 6) +
  geom_text(data = means_df, mapping = aes(label = paste("Min:"   , min_type   , sep = " "), x = 600, y = max_y*0.4), hjust = 0, size = 6) +
  geom_text(data = means_df, mapping = aes(label = paste("Max:"   , max_type   , sep = " "), x = 600, y = max_y*0.3), hjust = 0, size = 6) +
  geom_text(data = means_df, mapping = aes(label = "Mean"  , x = mean_type  , y = max_y*0.9), angle = 90, size = 3, vjust = 1.1) +
  geom_text(data = means_df, mapping = aes(label = "Median", x = median_type, y = max_y*0.9), angle = 90, size = 3, vjust = 1.1) +
  geom_text(data = means_df, mapping = aes(label = "Max."  , x = max_type   , y = max_y*0.9), angle = 90, size = 3, vjust = 1.1) 

# Save.
ggsave(plot = histo_comp_gg, filename = "visuals/fig4_histogram_mins.png", height = 20, width = 16)

# Create list of the duplicate grid sf objects to match. Not an ideal approach but it works.
grids_list <- list(detroit_grid_sf, detroit_grid_sf, detroit_grid_sf,
                   detroit_grid_sf, detroit_grid_sf, detroit_grid_sf)

# Create point (incidents) to polygon (grids) function.
p2p_fun <- function(x, y){
  x %>% 
    st_join(y) %>% 
    mutate(deployed_time = sum(time_on_scene)) %>% 
    group_by(grid_id) %>% 
    summarise(resolve_time = sum(time_on_scene)) %>% 
    ungroup() %>% 
    replace_na(list(resolve_time = 0)) %>% 
    mutate(resolve_time_hours = resolve_time/60)
}

# Run p2p through lists of incidents and duplicate grids.
detroit19_grid_list <- map2(grids_list, detroit19_deploy_clip_list, p2p_fun)

# Save specific maps for exploration in QGIS (optional).
names(detroit19_grid_list) <- unique(dh_agg_df$type)

for (i in 1:length(detroit19_grid_list)) {
  st_write(obj = detroit19_grid_list[[i]],
           dsn = paste("results/", names(detroit19_grid_list[i]), "_tos_map.shp", sep = ""), delete_dsn = T)
}

# Generate maps of incident counts by type.
grid_maps_list <- lapply(detroit19_grid_list, function(x){
  ggplot() +
    geom_sf(data = x, mapping = aes(fill = resolve_time_hours), colour = "transparent") +
    geom_sf(data = detroit_uni_sf, fill = "transparent") +
    scale_fill_continuous(guide = "colourbar", low = "snow", high = "dodgerblue3", n.breaks = 3) + 
    labs(fill = NULL) +
    guides(fill = guide_colourbar(barwidth = 9, barheight = 0.6, draw.ulim = FALSE,
                                  ticks.colour = "black", ticks.linewidth = 2)) +
    theme_void() +
    theme(legend.text = element_text(size = 11),
          legend.position = c(0.7,0.24),
          legend.direction = "horizontal",
          legend.box = "horizontal")
})

# Distribution across grids. Heavily skewed.
# histos_agg_list <- lapply(detroit19_grid_list, function(x){
#   ggplot(data = x) +
#     geom_histogram(mapping = aes(x = resolve_time_hours), bins = 60) +
#     theme_minimal() +
#     labs(x = NULL)
# }  )
# 
# # # Create list of the raw incidents for each type.
# detroit19_deploy_clip_sf <- detroit19_deploy_clip_sf %>%
#   filter(type != "unclassified") %>%
#   mutate(tos_hours = time_on_scene/60)
# 
# # Group into list.
# detroit19_deploy_clip_list <- detroit19_deploy_clip_sf %>%
#   group_split(type)
# 
# # Identify max.
# max_vec <- max(detroit19_deploy_clip_sf$tos_hours) 

# Distributions of time on scene, raw incidents, by type.
# histos_inc_list <- lapply(detroit19_deploy_clip_list, function(x) {
#   ggplot(data = x) +
#     geom_histogram(mapping = aes(x = tos_hours), bins = 60) +
#     scale_x_continuous(limits = c(0,max_vec)) +
#     # geom_vline(xintercept = mean(x$time_on_scene), colour = "red", linetype = "dotted") +
#     theme_minimal() +
#     labs(x = NULL)
# })

# Function to a add histograms to maps.
# histo_fun <- function(x, y) {
#   ggdraw() +
#     draw_plot(x) +
#     draw_plot(y, width = 0.25, height = 0.17, x = 0.07, y = 0.15)
# }

# Add to each map.
# maphisto_list <- map2(grid_maps_list, histos_agg_list, histo_fun)

# Annotate select maps with key locations.

# community
grid_maps_list[[1]] <- grid_maps_list[[1]] +
  annotate(geom = "text"    , x = 13467040, y = 322652, label = "WSU campus & Midtown", size = 4) +
  annotate(geom = "curve" , x = 13467040, y = 320672, xend = 13471640, yend = 314242, size = 0.7,
           arrow = arrow(length = unit(0.01, "npc")), curvature = 0.3) +
  scale_fill_continuous(guide = "colourbar", low = "snow", high = "dodgerblue3", breaks = c(0,75,150)) 
  

# crime
grid_maps_list[[2]] <- grid_maps_list[[2]] +
  annotate(geom = "text"    , x = 13477040, y = 324802, label = "Henry Ford Hospital", size = 4) +
  annotate(geom = "curve" , x = 13473240, y = 322952, xend = 13470640, yend = 318852, size = 0.7,
           arrow = arrow(length = unit(0.01, "npc")), curvature = -0.3) +
  annotate(geom = "text"    , x = 13497168, y = 337022, label = "Ascension St. John Hospital", size = 4) +
  annotate(geom = "curve" , x = 13511168, y = 337062, xend = 13515568, yend = 337562, size = 0.7,
           arrow = arrow(length = unit(0.01, "npc")), curvature = 0.1) +
  annotate(geom = "text"  , x = 13434648, y = 331826, label = "DMC Sinai Grace Hospital", size = 4) +
  annotate(geom = "curve" , x = 13437648, y = 333049, xend = 13442046, yend = 336525, size = 0.7,
           arrow = arrow(length = unit(0.01, "npc")), curvature = -0.2)

# health. label not included.
# grid_maps_list[[3]] <- grid_maps_list[[3]] +
#   annotate(geom = "text"  , x = 13481089, y = 322116, label = "Mental health service facility", size = 4) +
#   annotate(geom = "curve" , x = 13482240, y = 320016, xend = 13485816, yend = 317571, size = 0.7,
#            arrow = arrow(length = unit(0.01, "npc")), curvature = 0.3) +
#   scale_fill_continuous(guide = "colourbar", low = "snow", high = "dodgerblue3", breaks = c(0,30,60)) 

# quality of life
grid_maps_list[[5]] <- grid_maps_list[[5]] +
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
ggsave(filename = "visuals/fig3_maps_tos.png", height = 48, width = 40, unit = "cm", dpi = 300)
