# Data Cleaning and Merging 
library(tidyverse)

# Paths so we can consistently amend data in the same way
bp_dir <- "TBP"
game_dir <- "TG"
out_dir <- "pitcher_data"
ind_dir <- file.path(out_dir, "individual")

dir.create(ind_dir, recursive = TRUE, showWarnings = FALSE)
bp_files <- list.files(bp_dir, pattern = "\\.csv$", full.names = TRUE)
game_files <- list.files(game_dir, pattern = "\\.csv$", full.names = TRUE)


# Try and create consistent column names 
bp_cols <- names(read_csv(bp_files[1], n_max = 0, show_col_types = FALSE))
game_cols <- names(read_csv(game_files[1], n_max = 0, show_col_types = FALSE))
shared_cols <- intersect(bp_cols, game_cols) %>%
  setdiff(c("AwayTeamForeignID"))

# Columns in games that aren't consistently in BP for some reason 
game_context_cols <- c("PitchCall", "Batter", "BatterSide",
                       "Inning", "Balls", "Strikes",
                       "PAofInning", "PitchofPA",
                       "KorBB", "TaggedHitType", "PlayResult", "OutsOnPlay")


# BP Files
bp_data <- map(bp_files, \(f) read_csv(f, show_col_types = FALSE) %>%
                 mutate(SourceFile = basename(f))) %>%
  keep(\(df) is.character(df$Pitcher)) %>%
  map(\(df) {
    # a consistent way to track sessions and dates easier? 
    if (!"GameID" %in% names(df)) {
      df$GameID <- paste0(format(df$Date, "%Y%m%d"), "-WildHealthField-BP")
    }
    df %>% # try to deal with inconsistent naming strategies in BP vs Game
      rename(any_of(c(EffectiveVelo = "EffVelocity"))) %>%
      select(any_of(c(shared_cols, "EffectiveVelo", "GameID", "SourceFile"))) %>%
      mutate(SessionType = "LiveBP")
  }) %>%
  bind_rows() %>%  # inconsistent pitcher name spelling lol
  mutate(Pitcher = str_replace(Pitcher, "\\s*,\\s*", ", ")) %>% 
  mutate(Pitcher = recode(Pitcher, "Broaddus, Landon" = "Broaddus, Landen"))

# Game Files
game_data <- map(game_files, \(f) read_csv(f, show_col_types = FALSE) %>%
                   mutate(SourceFile = basename(f))) %>%
  map(\(df) df %>%
        filter(PitcherTeam == "TRA_UNI") %>%
        select(any_of(c(shared_cols, "EffectiveVelo", game_context_cols, "SourceFile", 
                        "GameID"))) %>%
        mutate(SessionType = "Game")) %>%
  bind_rows() %>% 
  mutate(Pitcher = str_replace(Pitcher, "\\s*,\\s*", ", ")) %>% 
  mutate(Pitcher = recode(Pitcher, "Broaddus, Landon" = "Broaddus, Landen"))

game_data <- game_data %>%
  mutate(Pitcher = if_else(
    SourceFile == "Mar_6_26_G.csv" & Inning == 6,
    "Dotson, Kemper",
    Pitcher
  ))

# Let's get non-Transy pitcher data 
## I tried to use this for something else, but feels like it could be useful here!
opponent_data <- map(game_files, \(f) read_csv(f, show_col_types = FALSE)) %>%
  map(\(df) df %>% 
        filter(PitcherTeam != "TRA_UNI") %>%
        select(Pitcher, TaggedPitchType, HorzBreak, InducedVertBreak,
               RelSpeed, SpinRate, SpinAxis)) %>%
  bind_rows

# Let's check them
dim(bp_data)
glimpse(bp_data)
dim(game_data)
glimpse(game_data)

# Now we can get it into a combined file
all_pitchers <- bind_rows(bp_data, game_data) %>%
  filter(!is.na(Pitcher), Pitcher != "") %>%
  arrange(Pitcher, Date, Time)
write_csv(all_pitchers, file.path(out_dir, "all_transy_pitchers.csv"))

# And now a per-pitcher file
all_pitchers %>%
  group_by(Pitcher) %>%
  group_split() %>%
  walk(~ {
    safe_name <- str_replace_all(.x$Pitcher[1], "[^A-Za-z0-9]", "_") %>%
      str_replace_all("_+", "_") %>% str_remove("_$")
    write_csv(.x, file.path(ind_dir, paste0(safe_name, ".csv")))
  })

# I guess we should do the same for the hitting side as well
bp_dir <- "TBP"
game_dir <- "TG"
out_dir_bat <- "batter_data"

game_batting <- map(game_files, \(f) read_csv(f, show_col_types = FALSE)) %>%
  map(\(df) df %>%
        filter(BatterTeam == "TRA_UNI") %>%
        select(any_of(c("Batter", "BatterSide", "Date", "Time",
                        "AutoPitchType", "TaggedPitchType",
                        "PitchCall", "TaggedHitType", "AutoHitType",
                        "PlayResult", "ExitSpeed", "Angle", "Distance", "Bearing",
                        "PlateLocHeight", "PlateLocSide", "Direction",
                        "RelSpeed", "SpinRate", "HorzBreak", 
                        "InducedVertBreak", "GameID", "PitchofPA",
                        "ContactPositionX", "ContactPositionY",
                        "Strikes", "Balls", "Outs",
                        "ContactPositionZ"))) %>%
        mutate(SessionType = "Game")) %>%
  bind_rows()

bp_batting <- map(bp_files, \(f) read_csv(f, show_col_types = FALSE)) %>%
  keep(\(df) "Batter" %in% names(df) & is.character(df$Batter)) %>%
  map(\(df) df %>%
        select(any_of(c("Batter", "BatterSide", "Date", "Time",
                        "AutoPitchType", "TaggedPitchType",
                        "PitchCall", "TaggedHitType", "AutoHitType",
                        "ExitSpeed", "Angle", "Distance", "Bearing",
                        "PlateLocHeight", "PlateLocSide", "PitchofPA",
                        "RelSpeed", "SpinRate", "Direction",
                        "ContactPositionX", "ContactPositionY",
                        "Strikes", "Balls", "Outs",
                        "ContactPositionZ"))) %>%
        mutate(SessionType = "LiveBP")) %>%
  bind_rows()

transy_batting %>%
  group_by(SessionType) %>%
  summarise(
    n = n(),
    pitchofpa_present = sum(!is.na(PitchofPA)),
    pitchofpa_pct = mean(!is.na(PitchofPA))
  )


all_transy_batting <- bind_rows(game_batting, bp_batting) %>%
  filter(!is.na(Batter), Batter != "") %>%
  mutate(Batter = str_replace(Batter, "\\s*,\\s*", ", ")) %>%
  arrange(Batter, Date, Time)

write_csv(all_transy_batting, file.path(out_dir_bat, "all_transy_batting.csv"))

# I forgot the coach didn't give us the best BP data for hitters. 
# we lose a lot of data if we use the game_batting 
# but we can't do any reliable per-batter analysis. 
# After looking, the Mar_11_26_BP file is fine. 
# Let's use the game batting + this session 
all_transy_batting %>% 
  group_by(Batter) %>% 
  count()

# sample size is quite a bit smaller 
game_batting %>%
  filter(!is.na(Batter), Batter != "") %>%
  count(Batter) %>%
  arrange(desc(n))

transy_batting %>%
  filter(!is.na(Batter), Batter != "") %>%
  count(Batter) %>%
  arrange(desc(n))

# Correction log 
## Let's add the Mar 11 file 
Mar_11_26_BP <- Mar_11_26_BP %>% 
  select(any_of(c("Batter", "BatterSide", "Date", "Time",
                  "AutoPitchType", "TaggedPitchType",
                  "PitchCall", "TaggedHitType", "AutoHitType",
                  "ExitSpeed", "Angle", "Distance", "Bearing",
                  "PlateLocHeight", "PlateLocSide", "PitchofPA",
                  "RelSpeed", "SpinRate", "Direction", "PlayResult",
                  "ContactPositionX", "ContactPositionY",
                  "Strikes", "Balls", "Outs",
                  "ContactPositionZ"))) %>%
  mutate(SessionType = "LiveBP")

Mar_19_26_BP <- Mar_19_26_BP %>% 
  select(any_of(c("Batter", "BatterSide", "Date", "Time",
                  "AutoPitchType", "TaggedPitchType", 
                  "PitchCall", "TaggedHitType", "AutoHitType",
                  "ExitSpeed", "Angle", "Distance", "Bearing",
                  "PlateLocHeight", "PlateLocSide", "PitchofPA",
                  "RelSpeed", "SpinRate", "Direction",
                  "ContactPositionX", "ContactPositionY",
                  "Strikes", "Balls", "Outs",
                  "ContactPositionZ")))

Mar_19_26_BP <- Mar_19_26_BP %>% 
  select(any_of(c("Batter", "BatterSide", "Date", "Time",
                  "AutoPitchType", "TaggedPitchType",
                  "PitchCall", "TaggedHitType", "AutoHitType",
                  "ExitSpeed", "Angle", "Distance", "Bearing",
                  "PlateLocHeight", "PlateLocSide", "PitchofPA",
                  "RelSpeed", "SpinRate", "Direction", "PlayResult",
                  "ContactPositionX", "ContactPositionY",
                  "Strikes", "Balls", "Outs",
                  "ContactPositionZ"))) %>%
  mutate(SessionType = "LiveBP")

Mar_26_26_BP <- Mar_26_26_BP %>% 
  select(any_of(c("Batter", "BatterSide", "Date", "Time",
                  "AutoPitchType", "TaggedPitchType",
                  "PitchCall", "TaggedHitType", "AutoHitType",
                  "ExitSpeed", "Angle", "Distance", "Bearing",
                  "PlateLocHeight", "PlateLocSide", "PitchofPA",
                  "RelSpeed", "SpinRate", "Direction", "PlayResult",
                  "ContactPositionX", "ContactPositionY",
                  "Strikes", "Balls", "Outs",
                  "ContactPositionZ"))) %>%
  mutate(SessionType = "LiveBP")



transy_batting <- bind_rows(game_batting, Mar_11_26_BP, Mar_19_26_BP, Mar_26_26_BP) %>%
  filter(!is.na(Batter), Batter != "") %>%
  mutate(Batter = str_replace(Batter, "\\s*,\\s*", ", ")) %>%
  arrange(Batter, Date, Time)
write_csv(transy_batting, file = "transy_game_batting.csv")


## March 6th: 6th inning is incorrectly attributed to Drew. 
## Manually adjusting this to the correct pitcher, Kemper Dotson 
game_data <- game_data %>%
  mutate(Pitcher = if_else(
    SourceFile == "Mar_6_26_G.csv" & Inning == 6,
    "Dotson, Kemper",
    Pitcher
  ))

## Above pitcher script will need to be re-run after this 

## Bigger issue 
# Mar_3_26_BP2 is completely invalid. Inconsistent data, the two pitchers 
# have to be incorrect. 
