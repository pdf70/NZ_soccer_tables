# Filename: "NZ_soccer_tables.R"

# Reads in data from wikipedia of history of all New Zealand National Soccer League tables
# Note that the format of the input data may change as people change wikipedia entries

# Team colours sourced from https://imagecolorpicker.com/en.

# Retrieve previous work from:
#setwd(output_path) 
#load(file = "nzsl_tables_raw.Rdata")     # list - "tables"
#load(file="nzsl_tables.Rdata")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Libraries & directories

# Set directory paths
path = "C:/Users/fallo/OneDrive/Documents/Pete/R-files"
input_path = paste(path, "/Input", sep="")
output_path = paste(path, "/R_output", sep="")
setwd(path)

# Specify packages (libraries) that are used
library(lubridate)
library(tidyverse)
library(scales)
library(janitor)
library(rvest)    # Reading tables from a web page


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Parameters 

# From 1970 to 2023
end_yr = c(seq(1970, 1998, by = 1), rep(1999,2), seq(2000, 2003, by = 1), seq(2005, 2021, by = 1),seq(2021, 2023, by = 1) )
start_yr = end_yr - 1
seasons = case_when(
  end_yr <= 1996 ~ substr(end_yr,1,4),
  end_yr > 1996 & end_yr <= 1998 ~ paste(start_yr, "-", substr(end_yr,3,4), sep = ""),
  end_yr >= 2004 & end_yr <= 2021 ~ paste(start_yr, "-", substr(end_yr,3,4), sep = ""),
  TRUE ~ substr(end_yr,1,4))
seasons[53] = "2021"

no_teams_finals = c(rep(0,23), rep(4,6), rep(1,2), rep(4,6), rep(3,2), rep(4,13), rep(2,length(seasons)-52))    
no_teams_finals[which(seasons == "2019-20")] = 0
no_teams_finals[which(seasons == "1997-98")] = 3
no_teams_finals[which(seasons == "2005-06")] = 5

# Note: need to update this line each year to value of table number in wikipedia for latest season
wiki_table_no = c(rep(1,23), rep(4,3), rep(1,4), 2, 1, rep(2,3), rep(1,4), rep(2,13), 19, 23, 22)
wiki_table_no[which(seasons == "1971")] = 2
wiki_table_no[which(seasons %in% c("2011-12", "2019-20"))] = 3

wiki_name = c(rep("_New_Zealand_National_Soccer_League", 23),
              rep("_New_Zealand_Superclub_League", 3),
              rep("_National_Summer_Soccer_League", 3),
              rep("_New_Zealand_island_soccer_leagues", 2),
              rep("_New_Zealand_National_Soccer_League", 4),
              rep("_New_Zealand_Football_Championship", 17),
              rep("_New_Zealand_National_League", length(seasons)-52))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Functions 
make_graph_nzsl = function(team_abbrev) {
  data_for_graph = nzsl_tables %>% 
    filter(abbrev == team_abbrev)
  
  max_teams_in_season = max(data_for_graph$count_teams_div)
  start_yr = min(data_for_graph$season)
  end_yr = max(data_for_graph$season)
  min_yr = min(data_for_graph$yr_for_xaxis)
  max_yr = max(data_for_graph$yr_for_xaxis)
  
  league_name = case_when(                           
    max_yr <= 1992 ~ "NZ National Soccer League",
    min_yr >= 1992 & max_yr <= 1995 ~ "NZ Superclub League",
    min_yr >= 1996 & max_yr <= 1998 ~ "NZ Summer Soccer League",
    min_yr >= 2000 & max_yr <= 2003 ~ "NZ National Soccer League",
    min_yr >= 2004 & max_yr <= 2020 ~ "NZ Football Championship",
    min_yr >= 2021 ~ "NZ National  League",
    TRUE ~ "NZ National Football Leagues")
  team_name = data_for_graph$current_name[1]
  sth_isl = min(sum(data_for_graph$conference == "South"), 1)
  
  #Breaks for background rectangles, other formatting
  # Update these values whenever the no. of teams in the league changes
  rects = data.frame(xstart = c(-Inf, 1970.5, 1976.5, 1986.5, 1992.5, 1995.5, 1997.5, 1998.5, 1999.5, 2003.5,
                                2013.5, 2014.5, 2015.5, 2019.5, 2020.5, 2021.5), 
                     xend = c(1970.5, 1976.5, 1986.5, 1992.5, 1995.5, 1997.5, 1998.5, 1999.5, 2003.5, 2013.5, 
                              2014.5, 2015.5, 2019.5, 2020.5, 2021.5, Inf),
                     ystart = c(rep(14,16)), 
                     yend = c(8, 10, 12, 14, 8, 10, 11, 12 - 4 * sth_isl, 10, 8, 9, 8, 10, 8, 6, 10))
  x_intercepts = data_for_graph$yr_for_xaxis[(data_for_graph$yr_for_xaxis %% 5) == 0]
  x_intercepts = x_intercepts[!(x_intercepts ==max_yr)]
  
  # Graph of league position
  graph_1 = ggplot(data_for_graph, aes(x = yr_for_xaxis, y = Pos, group=nzsl_stint)) +
    geom_line(linewidth=1.15, colour = data_for_graph$team_colours[1]) +
    geom_point(aes(colour=as.factor(champion), size = as.factor(champion))) +
    scale_colour_manual(values = c(data_for_graph$second_colour[1], data_for_graph$champ_colour[1])) +
    scale_size_manual(values = c(2,4)) +
    
    # axes
    geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = Inf, ymax = yend+0.1),  # 0.1 for margin
              fill = "white", alpha = 1.0, inherit.aes = FALSE) +
    scale_y_continuous(trans = "reverse", expand = c(0,0.1), breaks= pretty_breaks()) +
    scale_x_continuous(breaks= pretty_breaks()) +
    coord_cartesian(xlim = c(min_yr, max_yr), ylim = c(max_teams_in_season, 1)) +
    geom_vline(xintercept=x_intercepts,  linetype="dotted") +
    theme(panel.border = element_rect(fill=NA)) +
    
    # titles
    ggtitle(paste("Position of", team_name, "in", league_name, "from", start_yr, "to", end_yr)) + 
    theme(plot.title = element_text(lineheight=1.0, face="bold", hjust = 0.5)) +
    labs(x="Year", y="Position") +
    theme(axis.title = element_text(face = "bold")) +
    theme(plot.margin=unit(c(0.5,1,1.5,1.2),"cm")) +
    theme(legend.position = "none") +
    
    # horizontal lines for number of finals teams (approximated as 4 in years pre-1993 when no finals were held)
    # ignored exceptions where there were 2, 3 or 5 teams in finals for just one year
    # geom_step doesn't show years when teams were not in highest division
    {if(min_yr<2006)geom_segment(aes(x = min(yr_for_xaxis), xend = min(max_yr,2005.5), y = 4.5, yend = 4.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if((min_yr<2006)&(max_yr>=2006))geom_segment(aes(x = 2005.5, xend = 2005.5, y = 4.5, yend = 3.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if((max_yr>=2006)&(min_yr<2009))geom_segment(aes(x = 2005.5, xend = 2008.5, y = 3.5, yend = 3.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if((min_yr<2009)&(max_yr>=2009))geom_segment(aes(x = 2008.5, xend = 2008.5, y = 3.5, yend = 4.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if((max_yr>=2009)&(min_yr<2021))geom_segment(aes(x = 2008.5, xend = 2020.5, y = 4.5, yend = 4.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if((min_yr<2021)&(max_yr>=2021))geom_segment(aes(x = 2020.5, xend = 2020.5, y = 2.5, yend = 4.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if(max_yr>=2021)geom_segment(aes(x = max(2020.5,min_yr), xend = max(yr_for_xaxis), y = 2.5, yend = 2.5), linetype="dotted", colour = "black", linewidth = 1)}
  
  graph_1
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read in external data
# read all league tables in one loop
# to read a league table manually, see code at bottom, read_html("https://en.wikipedia.org/wiki/1970_New_Zealand_National_Soccer_League")
tables = list()
for (j in 1:length(seasons)) {
  table = read_html(paste("https://en.wikipedia.org/wiki/", seasons[j], wiki_name[j], sep = ""))
  tables_wiki <- table %>%
    html_nodes(".wikitable") %>%
    html_table(fill = TRUE)
  
  tables[[j]] <- tables_wiki[[wiki_table_no[j]]]  %>% # added to my list
    mutate(season_no = j, season = seasons[j])
  
  if (j%%5==0) print(paste("season = ", seasons[j], sep="")) 
}

# Review headers in each of the tables - need consistency of names for combining tables
headers_all = c()
for (j in 1) {
  header_fmt1 = colnames(tables[[j]])
  headers_all = rbind(header_fmt1, headers_all)
}
for (j in 9) {
  header_fmt2 = colnames(tables[[j]])
  headers_all = rbind(header_fmt2, headers_all)
}
for (j in 17) {
  header_fmt3 = colnames(tables[[j]])
  headers_all = rbind(header_fmt3, headers_all)
}
for (j in 27) {
  header_fmt4 = colnames(tables[[j]])
  headers_all = rbind(header_fmt4, headers_all)
}

header_fmt1 = colnames(tables[[1]])
header_fmt2 = colnames(tables[[9]])
header_fmt3 = colnames(tables[[17]]) %>%
  str_replace("Qualification or relegation", "Qualification")
header_fmt4 = colnames(tables[[27]]) %>%
  str_replace("Qualification or relegation", "Qualification")

for (j in 1:8) {
  colnames(tables[[j]]) = header_fmt1              # 1970 to 1977 seasons used goal ratio
}
for (j in 9:26) {  
  colnames(tables[[j]]) = header_fmt2              # goal diff, no Qualif. column
}
for (j in c(17, 29:31, 33:length(seasons))) {
  colnames(tables[[j]]) = header_fmt3              # latest format - 1986, 1997-98 seasons onwards
}
for (j in c(27:28, 32)) {
  colnames(tables[[j]]) = header_fmt4              # exception - 1996, 1996-97 & 2000 seasons with BP
}

# convert from list to data frame
tables_all_fmt1 = do.call(rbind, lapply(tables[c(1:8)], as.data.frame))
tables_all_fmt2 = do.call(rbind, lapply(tables[c(9:16, 18:26)], as.data.frame))
tables_all_fmt3 = do.call(rbind, lapply(tables[c(17, 29:31, 33:length(seasons))], as.data.frame))
tables_all_fmt4 = do.call(rbind, lapply(tables[c(27:28, 32)], as.data.frame))

tables_all_fmt1_adj = tables_all_fmt1 %>%
  mutate(GD = GF - GA,
         Qualification = "None") %>%
  select(Pos:GA, GD, GR:Pts, Qualification, season_no:season)

tables_all_fmt2_adj = tables_all_fmt2 %>%
  mutate(GR = "n/a",
         Qualification = "None") %>%
  select(Pos:GD, GR, Pts, Qualification, season_no:season)

tables_all_fmt3_adj = tables_all_fmt3 %>%
  mutate(GR = "n/a") %>%
  select(Pos:GD, GR, Pts, Qualification, season_no:season)

tables_all_fmt4_adj = tables_all_fmt4 %>%
  mutate(GR = "n/a") %>%
  select(Pos:GD, GR, Pts, Qualification, season_no:season)

tables_all = rbind(tables_all_fmt1_adj, tables_all_fmt2_adj, tables_all_fmt3_adj, tables_all_fmt4_adj) %>%
  arrange(season_no, Pos)

# read in premiers & runners-up
table_winners_superclub_raw = read_html("https://en.wikipedia.org/wiki/New_Zealand_Superclub_League") %>%
  html_nodes(".wikitable") %>%
  html_table(fill = TRUE)

table_winners_superclub = table_winners_superclub_raw[[1]] %>%
  select(Year, Winners, "Runners-up") %>%
  rename("Runners_up" = "Runners-up") %>%
  mutate(Year = str_replace(Year, "\\[.*\\]", ""))

table_winners_summer_raw = read_html("https://en.wikipedia.org/wiki/National_Summer_Soccer_League") %>%
  html_nodes(".wikitable") %>%
  html_table(fill = TRUE)

table_winners_summer = table_winners_summer_raw[[1]] %>%
  select(Year, Winners, "Runners-up") %>%
  rename("Runners_up" = "Runners-up") %>%
  mutate(Year = ifelse(Year == "1996", Year, paste(Year - 1, "-", substr(Year,3,4), sep = "")))

table_winners_championship_raw = read_html("https://en.wikipedia.org/wiki/New_Zealand_Football_Championship") %>%
  html_nodes(".wikitable") %>%
  html_table(fill = TRUE)

table_winners_championship = table_winners_championship_raw[[3]] %>%
  row_to_names(1) %>%
  select(c(1,6,8)) %>%
  rename("Runners_up" = "Runners-up") %>%
  mutate(Season = str_replace(Season, "-", "-"))
colnames(table_winners_championship) = colnames(table_winners_summer)


table_winners_nat_league_raw = read_html("https://en.wikipedia.org/wiki/New_Zealand_National_League") %>%
  html_nodes(".wikitable") %>%
  html_table(fill = TRUE)

table_winners_nat_league = table_winners_nat_league_raw[[3]] %>%
  row_to_names(1) %>%
  select(Season, Champions, "Runners-up") %>%
  rename("Runners_up" = "Runners-up") %>%
  mutate(Season = str_replace(Season, "\\[.*\\]", ""))
colnames(table_winners_nat_league) = colnames(table_winners_summer)

table_winners_other = rbind(table_winners_superclub, table_winners_summer, table_winners_championship,
                            table_winners_nat_league) 

table_other = data.frame(seasons, no_teams_finals) %>%
  mutate(season_year_end = as.numeric(substr(seasons, 1, 4)) + ifelse(nchar(seasons) <= 4, 0, 1),
         season_for_join = case_when(
           nchar(seasons) == 7 & substr(seasons,1,3) %in% c("200", "201", "202") ~ season_year_end - 1,
           TRUE ~ season_year_end)) %>%
  distinct() %>%
  left_join(table_winners_other, by = c("seasons" = "Year"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read in input files
setwd(input_path)
nzsl_teams = read_csv("nzsl_teams.csv")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Select relevant data, and then data manipulations
nzsl_tables = tables_all %>% 
  mutate(yr_end = case_when(
           season_no <= 27 ~ as.numeric(substr(season, 1, 4)),
           season_no %in% c(28, 29, 36:52) ~ as.numeric(substr(season, 1, 4)) + 1,   
           TRUE ~ as.numeric(substr(season, 1, 4))),
         yr_for_xaxis = ifelse(season_no %in% c(36:52), yr_end - 1, yr_end),
         Team = str_replace(Team, "\\[.*\\]", ""),            # remove text inside square brackets
         champion = ifelse(substr(Team, nchar(Team) - 2, nchar(Team)) == "(C)", 1, 0),
         champion = ifelse(substr(Team, nchar(Team) - 5, nchar(Team)) == "(C, Q)", 1, champion),
         premiers = ifelse(Pos == 1 & !(yr_end == 1999), 1, 0),
         conference_winners = ifelse(Pos == 1 & yr_end == 1999, 1, 0),
         relegated = ifelse(substr(Team, nchar(Team) - 2, nchar(Team)) == "(R)", 1, 0),
         Team = str_replace(Team, " \\(C\\)", ""),            # to get consistency in team name
         Team = str_replace(Team, " \\(R\\)", ""), 
         Team = str_replace(Team, "\\(R\\)", ""),
         Team = str_replace(Team, " \\(A\\)", ""), 
         Team = str_replace(Team, "\\(A\\)", ""),
         Team = str_replace(Team, " \\(Q\\)", ""),
         Team = str_replace(Team, " \\(C, Q\\)", ""),
         Team = ifelse(Team == "Auckland City" & season == "1972", paste(Team, season, sep = " "), Team),
         Pts = as.numeric(str_replace(Pts, "\\[.*\\]", "")),
         pts_per_win = case_when(
           yr_end <= 1982 ~ 2,
           yr_end >= 1996 & yr_end <= 1997 ~ 4,
           TRUE ~ 3),
         pts_per_draw = 1,
         pts_bonus = ifelse(season == "2000", Pts - (pts_per_win * W + D), 0),    
         pts_WPen = ifelse(season %in% c("1996", "1996-97"), Pts - (pts_per_win * W + D), 0),  
         pts_deducted = Pts - (pts_per_win * W + pts_per_draw * D + pts_bonus + pts_WPen),
         max_avail_pts = Pld * (pts_per_win + ifelse(season == "2000", 1, 0)),    
         pts_achieved_perc = Pts / max_avail_pts,
         goal_diff = GF - GA,
         GD_prefix = substr(GD,1,1),
         GD_sign = case_when(
           GD_prefix == "+" ~ 1,
           GD_prefix == "-" ~ -1,
           GD_prefix == "0" ~ 1,
           TRUE ~ -1),
         GD_numeric = ifelse(GD_prefix == "0", 0, ifelse(season_no <= 8, as.numeric(GD),
                                                         as.numeric(substr(GD,2,nchar(GD))) * GD_sign)),
         GD_check = GD_numeric - goal_diff,
         goals_per_game = round(GF / Pld, 2)) %>%
  left_join(table_other, by = c("yr_for_xaxis" = "season_for_join")) %>%
  mutate(finals = ifelse(Pos <= no_teams_finals, 1, 0),
         winner = ifelse(is.na(Winners), 0, ifelse(Team == Winners, 1, 0)),
         champion = case_when(
           yr_end %in% c(1993, 1994, 1995) ~ winner,
           TRUE ~ champion),
         runners_up = ifelse(Team == Runners_up, 1, 0),
         runners_up = case_when(
           yr_end == 1999 & Team == "Dunedin Technical" ~ 1,
           yr_end == 2000 & Team == "University-Mount Wellington" ~ 1,
           yr_end == 2001 & Team == "Miramar Rangers" ~ 1,
           yr_end == 2002 & Team == "Napier City Rovers" ~ 1,
           yr_end == 2003 & Team == "East Auckland" ~ 1,
           TRUE ~ runners_up),
         across(where(is.numeric), ~replace(., is.na(.), 0)),
         gf_years = ifelse(yr_end >= 1993 & !(yr_end == 2020), 1, 0),
         grand_finalist = (champion + runners_up) * gf_years) %>%
#         missed_gf = (1 - grand_finalist) * gf_years,
#         finals_years = ifelse(yr_end %in% c(1977, 1981, 1983), 0, 1),
#         no_teams_finals_chart = ifelse(finals_years == 0, 4, no_teams_finals),
#         missed_finals = (1 - finals) * finals_years) %>%
  group_by(season_no) %>%
  mutate(count_teams = n(),
         wooden_spoon = ifelse(Pos == max(Pos), 1, 0),
         wooden_spoon = ifelse(Pld == 0, 0, wooden_spoon)) %>%
  ungroup() %>%
  mutate(conference = case_when(
    season_no == 30 ~ "North",     
    season_no == 31 ~ "South",     
    TRUE ~ "National"),                     
    count_teams_div = count_teams) %>%    
  select(Pos:champion, runners_up, premiers:relegated, finals:grand_finalist, count_teams, 
         conference:count_teams_div, wooden_spoon, pts_per_win:goals_per_game, yr_end)

# Create a table of team names, including history & past team name changes
teams = as_tibble(unique(nzsl_tables$Team))
colnames(teams) = c("previous_name")
teams = teams %>% 
  mutate(current_name = previous_name) %>%
  mutate(current_name = case_when(                            # to get consistency of team names
    previous_name == "Blockhouse Bay" ~ "Bay Olympic",
    previous_name == "Hungaria" ~ "Wellington City",
    previous_name == "Auckland City 1972" ~ "Mount Albert-Ponsonby",
    previous_name == "Wellington Diamond United" ~ "Wellington United",
    previous_name == "Hawke's Bay United" ~ "Napier City Rovers",
    previous_name == "Waikato FC" ~ "WaiBOP United",
    previous_name == "Otago United" ~ "Southern United",
    previous_name == "Wellington Phoenix" ~ "Wellington Phoenix Reserves",
    TRUE ~ current_name))

teams_all = left_join(teams, nzsl_teams, by = c("current_name" = "current_name"))

nzsl_tables_all = left_join(nzsl_tables, teams_all, by = c("Team" = "previous_name"))

# Add additional information of previous season's finishing position
nzsl_tables = nzsl_tables_all %>%
  arrange(current_name, season_no) %>%
  mutate(prev_pos = ifelse(current_name == lag(current_name), lag(Pos), NA)) %>%
  mutate(next_pos = ifelse(current_name == lead(current_name), lead(Pos), NA)) %>%
  arrange(season_no, Pos) %>%
  mutate(pos_diff = ifelse(is.na(prev_pos), NA, -(Pos - prev_pos)),
         pos_abs_diff = abs(pos_diff),
         row_number = row_number(),
         # Need team abbrev, year of relegation for teams that returned later
         releg_yr_1 = case_when(
           abbrev == "WES" ~ 1971,
           abbrev == "CAV" ~ 1973,
           abbrev == "WDU" ~ 1974,
           abbrev == "GIS" ~ 1976,
           abbrev == "DCI" ~ 1977,
           abbrev %in% c("MEL", "WAT", "WOO") ~ 1978,
           abbrev %in% c("RED", "EAS") ~ 1979,
           abbrev == "NEL" ~ 1980,
           abbrev == "RAN" ~ 1981,
           abbrev == "DTE" ~ 1983,
           abbrev == "NAP" ~ 1985,
           abbrev == "PAP" ~ 1989,
           abbrev %in% c("CHR", "MAN", "MMA", "MWE") ~ 1992,
           abbrev %in% c("MIR", "WKU") ~ 1993,
           abbrev %in% c("CEN", "CTE", "WOL") ~ 1994,
           abbrev == "NSU" ~ 1999,
           abbrev == "AUC" ~ 2020,
           TRUE ~ 2099),
         releg_yr_2 = case_when(
           abbrev == "CAV" ~ 1977,
           abbrev == "WOO" ~ 1981,
           abbrev == "MEL" ~ 1982,
           abbrev %in% c("NEL", "RED") ~ 1988,
           abbrev == "WDU" ~ 1992,
           abbrev %in% c("WES", "WOL") ~ 1999,
           abbrev == "MIR" ~ 2003,
           abbrev %in% c("EAS", "NAP") ~ 2020,
           TRUE ~ 2099),
         releg_yr_3 = case_when(
           abbrev == "NEL" ~ 1993,
           abbrev %in% c("CAV", "MEL") ~ 1999,
           TRUE ~ 2099),
         nzsl_stint = case_when(
           yr_for_xaxis > releg_yr_3 ~ 4,
           yr_for_xaxis > releg_yr_2 ~ 3,
           yr_for_xaxis > releg_yr_1 ~ 2,
           TRUE ~ 1)) %>%
  ungroup() %>%
  arrange(season_no, Pos)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Analysis of NZSL tables data
# Make all-time league table
nzsl_all_time_league_table = group_by(nzsl_tables, current_name) %>%
  summarise(count = n(),
            Total_Pld = sum(Pld),
            Total_W = sum(W),
            Total_D = sum(D),
            Total_L = sum(L),
            Total_Bon = sum(pts_bonus),
            Total_WPen = sum(pts_WPen),
            Total_Ded = sum(pts_deducted),
            Total_GF = sum(GF),
            Total_GA = sum(GA),
            Total_GD = sum(goal_diff),
            Total_Pts = sum(Pts),
            pts_per_game = round(sum(Pts) / sum(Pld), 2),
            win_perc = round(Total_W / Total_Pld * 100, 2),
            count_champions = sum(champion),
            count_runners_up = sum(runners_up),
            count_premiers = sum(premiers),
            conference_winners = sum(conference_winners),
            count_finals = sum(finals),
            count_1st = sum(Pos == 1),
            count_2nd = sum(Pos == 2),
            count_3rd = sum(Pos == 3),
            count_4th = sum(Pos == 4),
            best = min(Pos),
            count_spoon = sum(wooden_spoon),
            count_relegated = sum(relegated),
            count_gf = sum(grand_finalist),
            first_season = min(season),
            last_season = max(season)) %>%
  arrange(desc(Total_Pts), desc(Total_GD), desc(Total_GF))

# champions by final position
champions = filter(nzsl_tables, champion == 1)
champions_by_Pos = group_by(champions, Pos) %>%
  summarise(count = n())

# totals by season
season_totals = group_by(nzsl_tables, season) %>%
  summarise(count = n(),
            Total_Pld = sum(Pld),
            Total_W = sum(W),
            Total_D = sum(D),
            Total_L = sum(L),
            Total_GF = sum(GF),
            Total_GA = sum(GA),
            Total_GD = sum(goal_diff),
            Total_Pts = sum(Pts),
            max_ave_goals_scored_team = max(goals_per_game),
            min_ave_goals_scored_team = min(goals_per_game)) %>%
  mutate(ave_goals_scored_game = round(Total_GF / (0.5 * Total_Pld), 1))

season_totals_div = group_by(nzsl_tables, season, conference) %>%
  summarise(count = n(),
            Total_Pld = sum(Pld),
            Total_W = sum(W),
            Total_D = sum(D),
            Total_L = sum(L),
            Total_GF = sum(GF),
            Total_GA = sum(GA),
            Total_GD = sum(goal_diff),
            Total_Pts = sum(Pts))

title_race_totals = group_by(nzsl_tables, season, yr_end) %>%
  summarise(count = n(),
            Total_Pts_1 = sum(Pts[Pos == 1]),
            Total_Pts_2 = sum(Pts[Pos == 2]),
            Total_GD_1 = sum(goal_diff[Pos == 1]),
            Total_GD_2 = sum(goal_diff[Pos == 2]),
            Total_GF_1 = sum(GF[Pos == 1]),
            Total_GF_2 = sum(GF[Pos == 2])) %>%
  mutate(margin_pts = Total_Pts_1 - Total_Pts_2,
         margin_GD = Total_GD_1 - Total_GD_2,
         margin_GF = Total_GF_1 - Total_GF_2)

# totals by club
club_records = group_by(nzsl_tables, current_name) %>%
  summarise(highest_GF = max(GF),
            lowest_GF = min(GF),
            highest_GA = max(GA),
            lowest_GA = min(GA),
            highest_Pts = max(Pts),
            lowest_Pts = min(Pts))

# Records for each team in a season
highest_GF_team = club_records %>%
  left_join(nzsl_tables, by = c("current_name" = "current_name",
                               "highest_GF" = "GF")) %>%
  select(current_name, highest_GF, Pld, season)

lowest_GF_team = club_records %>%
  left_join(nzsl_tables, by = c("current_name" = "current_name",
                               "lowest_GF" = "GF")) %>%
  select(current_name, lowest_GF, Pld, season)

highest_GA_team = club_records %>%
  left_join(nzsl_tables, by = c("current_name" = "current_name",
                               "highest_GA" = "GA")) %>%
  select(current_name, highest_GA, Pld, season)

lowest_GA_team = club_records %>%
  left_join(nzsl_tables, by = c("current_name" = "current_name",
                               "lowest_GA" = "GA")) %>%
  select(current_name, lowest_GA, Pld, season)

highest_Pts_team = club_records %>%
  left_join(nzsl_tables, by = c("current_name" = "current_name",
                               "highest_Pts" = "Pts")) %>%
  select(current_name, highest_Pts, Pld, season)

lowest_Pts_team = club_records %>%
  left_join(nzsl_tables, by = c("current_name" = "current_name",
                               "lowest_Pts" = "Pts")) %>%
  select(current_name, lowest_Pts, Pld, season)

# Records for a single season - not adjusted for no. of games
# most & least points
most_pts_season = arrange(nzsl_tables, desc(Pts)) %>%
  select(season, Team, Pld, Pts)
head(most_pts_season, 5)

least_pts_season = arrange(nzsl_tables, Pts) %>%
  select(season, Team, Pld, Pts)
head(least_pts_season, 5)

# most & least wins
most_wins_season = arrange(nzsl_tables, desc(W)) %>%
  select(season, Team, Pld, W)
head(most_wins_season, 5)

least_wins_season = arrange(nzsl_tables, W) %>%
  filter(Pld > 0) %>%
  select(season, Team, Pld, W)
head(least_wins_season, 5)

# most & least losses
most_losses_season = arrange(nzsl_tables, desc(L)) %>%
  select(season, Team, Pld, L)
head(most_losses_season, 5)

least_losses_season = arrange(nzsl_tables, L) %>%
  filter(Pld > 0) %>%
  select(season, Team, Pld, L)
head(least_losses_season, 5)

# most & least draws
most_draws_season = arrange(nzsl_tables, desc(D)) %>%
  select(season, Team, Pld, D)
head(most_draws_season, 5)

least_draws_season = arrange(nzsl_tables, D) %>%
  filter(Pld > 0) %>%
  select(season, Team, Pld, D)
head(least_draws_season, 5)

# most & least goals scored
most_goals_season = arrange(nzsl_tables, desc(GF)) %>%
  select(season, Team, Pld, GF)
head(most_goals_season, 5)

least_goals_season = arrange(nzsl_tables, GF) %>%
  filter(Pld > 0) %>%
  select(season, Team, Pld, GF)
head(least_goals_season, 5)

# most & least goals conceded
most_goals_against_season = arrange(nzsl_tables, desc(GA)) %>%
  select(season, Team, Pld, GA)
head(most_goals_against_season, 5)

least_goals_against_season = arrange(nzsl_tables, GA) %>%
  filter(Pld > 0) %>%
  select(season, Team, Pld, GA)
head(least_goals_against_season, 5)

# best & worst goal difference
best_goals_diff_season = arrange(nzsl_tables, desc(goal_diff)) %>%
  select(season, Team, Pld, goal_diff)
head(best_goals_diff_season, 5)

worst_goals_diff_season = arrange(nzsl_tables, goal_diff) %>%
  select(season, Team, Pld, goal_diff)
head(worst_goals_diff_season, 5)

# highest & lowest points achieved percentage
highest_pts_perc_season = arrange(nzsl_tables, desc(pts_achieved_perc)) %>%
  select(season, Team, Pld, Pts, max_avail_pts, pts_achieved_perc)
head(highest_pts_perc_season, 5)

lowest_pts_perc_season = arrange(nzsl_tables, pts_achieved_perc) %>%
  select(season, Team, Pld, Pts, max_avail_pts, pts_achieved_perc)
head(lowest_pts_perc_season, 5)

# most points to not win the league
most_pts_not_premiers_season = arrange(nzsl_tables, desc(Pts)) %>%
  filter(premiers == 0) %>%
  select(season, Team, Pld, Pts) 
head(most_pts_not_premiers_season, 5)

# least points to win the league
least_pts_premiers_season = arrange(nzsl_tables, Pts) %>%
  filter(premiers == 1) %>%
  select(season, Team, Pld, Pts)
head(least_pts_premiers_season, 5)

# biggest & smallest winning margin in league
most_winning_margin_season = title_race_totals %>%
  arrange(desc(margin_pts), desc(margin_GD), desc(margin_GF)) %>%
  left_join(nzsl_tables, by = c("season" = "season")) %>%
  filter(Pos == 1) %>%
  select(season, Team, margin_pts, margin_GD, margin_GF)
head(most_winning_margin_season, 5)

least_winning_margin_season = title_race_totals %>%
  arrange(margin_pts, margin_GD, margin_GF) %>%
  left_join(nzsl_tables, by = c("season" = "season")) %>%
  filter(Pos == 1) %>%
  select(season, Team, margin_pts, margin_GD, margin_GF)
head(least_winning_margin_season, 5)

# highest movement in final position
highest_mvmt_up_season = arrange(nzsl_tables, desc(pos_diff)) %>%
  select(season, Team, Pos, prev_pos, pos_diff)
head(highest_mvmt_up_season, 5)

highest_mvmt_down_season = arrange(nzsl_tables, pos_diff) %>%
  select(season, Team, Pos, prev_pos, pos_diff)
head(highest_mvmt_down_season, 5)


# lowest position to champion in one season
prev_pos_champion = nzsl_tables %>%
  filter(champion == 1) %>%
  select(season, Team, prev_pos) %>%
  arrange(desc(prev_pos), season)
head(prev_pos_champion, 5)

# lowest position after being champion in one season
next_pos_champion = nzsl_tables %>%
  filter(champion == 1) %>%
  select(season, Team, next_pos) %>%
  arrange(desc(next_pos), season)
head(next_pos_champion, 5)


# volatility of position from year to year
pos_changes = nzsl_tables %>%
  group_by(current_name) %>%
  summarise(count_seasons = n(),
            total_pos_diff = sum(pos_abs_diff, na.rm = TRUE)) %>%
  mutate(ave_mvmt = total_pos_diff / (count_seasons - 1)) %>%
  arrange(desc(ave_mvmt))
pos_changes


# no. of teams in finals
finals_teams = nzsl_tables %>% 
#  filter(str_detect(tolower(Qualification), pattern = "finals") |
#           str_detect(tolower(Qualification), pattern = "play-offs")) %>% 
  group_by(season, yr_end) %>% 
  summarise(finals_teams = max(Pos))

# list of all team abbreviations
teams_unique = unique(nzsl_tables$abbrev)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# checks on data for consistency
error_check_pts = nzsl_tables %>% 
  filter(!Pts == (pts_per_win * W + pts_per_draw * D))    

error_check_pld = nzsl_tables %>%
  filter(!Pld == (W + D + L))

error_check_results = season_totals %>%
  filter(!Total_W == Total_L)

error_check_gd_season = season_totals %>%
  filter(!Total_GD == 0)

error_check_gd = nzsl_tables %>%
  filter(!(GD_check == 0))

error_check_pos = group_by(nzsl_tables, season, conference) %>%
  summarise(count = n(),
            sum_pos = sum(Pos)) %>%
  mutate(exp_sum_pos = count * (count + 1) / 2,
         pos_diff = sum_pos - exp_sum_pos) %>%   # error if calculated difference (pos_diff) is not zero
  filter(!(pos_diff == 0))

error_sorted_pos = nzsl_tables %>%
  arrange(season_no, desc(Pts), desc(goal_diff), desc(GF)) %>%
  mutate(sorted_row_number = row_number(),
         row_no_diff = row_number - sorted_row_number) %>%
  filter(!(row_no_diff == 0)) %>%
  filter(season_no >= 9)

error_sorted_pos_ratio = nzsl_tables %>%
  filter(!is.na(GR)) %>%
  arrange(season_no, desc(Pts), desc(GR)) %>%
  mutate(sorted_row_number = row_number(),
         row_no_diff = row_number - sorted_row_number) %>%
  filter(!(row_no_diff == 0)) 

check_identical_pos = nzsl_tables %>%
  group_by(season_no, Pts, goal_diff, GF) %>%
  summarise(count_seasons = n()) %>%
  filter(count_seasons > 1)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# run function to produce graph for a specific team
make_graph_nzsl("BAY")     # Bay Olympic
make_graph_nzsl("EAS")     # Eastern Suburbs - graph is better split in two
make_graph_nzsl("CHR")     # Christchurch United - graph is better split in two
make_graph_nzsl("MWE")     # Mount Wellington
make_graph_nzsl("STO")     # Stop Out
make_graph_nzsl("GIS")     # Gisborne City
#make_graph_nzsl("WCI")    # Hungaria / Wellington City (pre-Wellington United)
make_graph_nzsl("WDU")     # Wellington Diamond United
#make_graph_nzsl("WES")    # Western Suburbs
#make_graph_nzsl("CAV")    # Caversham
#make_graph_nzsl("MAP")    # Mount Albert-Ponsonby / Auckland City (1972)
#make_graph_nzsl("NBR")    # New Brighton
make_graph_nzsl("NSU")     # North Shore United
#make_graph_nzsl("MEL")    # Melville United
#make_graph_nzsl("NEL")    # Nelson United
make_graph_nzsl("DCI")     # Dunedin City
#make_graph_nzsl("COU")    # Courier Rangers
#make_graph_nzsl("WAT")    # Waterside Karori
#make_graph_nzsl("WOO")    # Woolston WMC
make_graph_nzsl("MAN")     # Manurewa  - graph is better split in two
#make_graph_nzsl("RED")    # Red Sox Manawatu

# new teams from 1980
#make_graph_nzsl("RAN")    # Rangers
make_graph_nzsl("MIR")     # Miramar Rangers - graph is better split in two
#make_graph_nzsl("TAK")    # Takapuna City
make_graph_nzsl("NAP")     # Napier City Rovers / Hawke's Bay United
#make_graph_nzsl("ECB")    # East Coast Bays
#make_graph_nzsl("INV")    # Invercargill Thistle
#make_graph_nzsl("PAP")    # Papatoetoe
#make_graph_nzsl("DTE")    # Dunedin Technical
#make_graph_nzsl("UNI")    # Auckland University
#make_graph_nzsl("MMA")    # Mount Maunganui
#make_graph_nzsl("HUT")    # Hutt Valley United
#make_graph_nzsl("WKU")    # Waikato United
make_graph_nzsl("WTC")     # Waitakere City
#make_graph_nzsl("NPR")    # New Plymouth Rangers

# new teams from Superclub era 1993-95
make_graph_nzsl("CEN")     # Central United
#make_graph_nzsl("WNG")    # Wanganui East Athletic
#make_graph_nzsl("WOL")    # Wellington Olympic
#make_graph_nzsl("ROS")    # Roslyn-Wakari
#make_graph_nzsl("CTE")    # Christchurch Technical

# new teams from Summer League
#make_graph_nzsl("NSB")    # Nelson Suburbs
#make_graph_nzsl("TOT")    # Team Otago
#make_graph_nzsl("LHC")    # Lower Hutt City

# new teams from Island Leagues 1999
#make_graph_nzsl("MET")    # Metro
#make_graph_nzsl("STH")    # Southland United
#make_graph_nzsl("NHE")    # Northern Hearts
#make_graph_nzsl("MAR")    # Marlborough

# new teams from NZ NSL 2000-03
#make_graph_nzsl("UMW")    # University-Mount Wellington
#make_graph_nzsl("WTE")    # Woolston Technical
#make_graph_nzsl("TCU")    # Tauranga City United
make_graph_nzsl("CAN")     # Canterbury United
#make_graph_nzsl("EAU")    # East Auckland

# new teams from NZ Football Championship 2004-21
make_graph_nzsl("AUC")     # Auckland City
make_graph_nzsl("WTU")     # Waitakere United
make_graph_nzsl("WBP")     # Waikato FC / WaiBOP United
make_graph_nzsl("TWE")     # Team Wellington
make_graph_nzsl("SOU")     # Otago United / Southern United
make_graph_nzsl("YHM")     # YoungHeart Manawatu
#make_graph_nzsl("U20")    # Wanderers SC
make_graph_nzsl("WPR")     # Wellington Phoenix Reserves
#make_graph_nzsl("TAS")    # Tasman United
#make_graph_nzsl("HAM")    # Hamilton Wanderers

# new teams from NZ National League 2021-
#make_graph_nzsl("CAS")    # Cashmere Technical
#make_graph_nzsl("SEL")    # Selwyn United
#make_graph_nzsl("AUN")    # Auckland United
#make_graph_nzsl("BIR")    # Birkenhead United
#make_graph_nzsl("PET")    # Petone


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# export file to csv format
names(nzsl_all_time_league_table) <- gsub(x = names(nzsl_all_time_league_table), pattern = "_", replacement = " ") 

setwd(output_path)
save(tables, file = "nzsl_tables_raw.Rdata")
save(nzsl_tables, file = "nzsl_tables.Rdata")
write.csv(nzsl_tables, file = "nzsl_tables_full.csv")
write.csv(nzsl_all_time_league_table, file = "nzsl_all_time_league_table.csv")
write.csv(season_totals, file = "nzsl_season_totals.csv")
#write.csv(error_check_pts, file = "nzsl_error_check_pts.csv")
setwd(path) 


# export multiple graphs
for (i in 1:length(teams_unique)) {
  make_graph_nzsl(teams_unique[i])
  setwd(output_path)
  ggsave(paste("performance_chart_nzsl_", teams_unique[i], ".png", sep=""))
  ggsave(paste("performance_chart_nzsl_", teams_unique[i], ".svg", sep=""))
}
setwd(path)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# End


# To do:
# Graph
# - vertical lines for 1993-95 (Superclub) & 1996-98 (Summer League), 1999 (Islands)

# Alternative graphs where timeline is better split in two
#  - EAS, CHR, MAN, MIR


# Check Wikipedia data against ultimatenzsoccer.com or RSSSF


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Test
# read one league table manually
table_test = read_html("https://en.wikipedia.org/wiki/1992_New_Zealand_National_Soccer_League")
table_test = read_html("https://en.wikipedia.org/wiki/1995_New_Zealand_Superclub_League")

table_test = read_html("https://en.wikipedia.org/wiki/1996_National_Summer_Soccer_League")
table_test = read_html("https://en.wikipedia.org/wiki/1997-98_National_Summer_Soccer_League")
table_test = read_html("https://en.wikipedia.org/wiki/1999_New_Zealand_island_soccer_leagues")

table_test = read_html("https://en.wikipedia.org/wiki/2009-10_New_Zealand_Football_Championship")
table_test = read_html("https://en.wikipedia.org/wiki/New_Zealand_Football_Championship")

table_test = read_html("https://en.wikipedia.org/wiki/2023_New_Zealand_National_League")
tables_all_test <- table_test %>%
  html_nodes(".wikitable") %>%
  html_table(fill = TRUE)
table_yyyymm = tables_all_test[[22]]
table_yyyymm

