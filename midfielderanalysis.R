install.packages("worldfootballR")
install.packages("fmsb")
library(worldfootballR)
library(fmsb)
library(dplyr)
library(ggplot2)
library(remotes)
remotes::install_github("ricardo-bion/ggradar")
library(ggradar)


pos_stats <- fb_big5_advanced_season_stats(season_end_year = 2023, stat_type = "possession", team_or_player = "player")
shoot_stats <- fb_big5_advanced_season_stats(season_end_year = 2023, stat_type = "shooting", team_or_player = "player")
pass_stats <- fb_big5_advanced_season_stats(season_end_year = 2023, stat_type = "passing", team_or_player = "player")
def_stats <- fb_big5_advanced_season_stats(season_end_year = 2023, stat_type = "defense", team_or_player = "player")
gca_stats <- fb_big5_advanced_season_stats(season_end_year = 2023, stat_type = "gca", team_or_player = "player")

stats <- inner_join(shoot_stats, pass_stats, by = c("Player", "Squad"))
stats <- inner_join(stats, pos_stats, by = c("Player", "Squad"))
stats <- inner_join(stats, def_stats, by = c("Player", "Squad"))
stats <- inner_join(stats, gca_stats, by = c("Player", "Squad"))

stats <- stats %>%
  filter(grepl("MF", Pos.x)) %>%
  filter(Mins_Per_90.x >= 19, !is.na(SoT_percent_Standard)) %>%
  select(team = Squad, comp = Comp.x, player = Player, goals = Gls_Standard, sotpct = SoT_percent_Standard, goalssot = G_per_SoT_Standard, npxg = npxG_Expected, npg_xg = 'np:G_minus_xG_Expected', progdist = PrgDist_Total, cmpshort = Cmp_Short, cmpshortpct = Cmp_percent_Short, cmpmed = Cmp_Medium, cmpmedpct = Cmp_percent_Medium, cmplong = Cmp_Long, cmplongpct = Cmp_percent_Long, ast = Ast, xag = xAG, xa = xA_Expected, a_xag = A_minus_xAG_Expected, key = KP, finalthird = Final_Third, progpass = PrgP, touches = Touches_Touches, deftouches = `Def 3rd_Touches`, midtouches = `Mid 3rd_Touches`, atttouches = `Att 3rd_Touches`, attake = Att_Take, succtake = Succ_percent_Take, carr = Carries_Carries, progdistcarr = PrgDist_Carries, progcarr = PrgC_Carries, finalthirdcarr = Final_Third_Carries, progrec = PrgR_Receiving, defthirdtkl = `Def 3rd_Tackles`, midthirdtkl = `Mid 3rd_Tackles`, attthirdtkl = `Att 3rd_Tackles`, attchal = Att_Challenges, tklpct = Tkl_percent_Challenges, blks = Blocks_Blocks, int = Int, sca90 = SCA90_SCA, gca90 = GCA90_GCA)

stats$goalssot <- ifelse(is.na(stats$goalssot), 0, stats$goalssot)

shooting_cols <- stats[,4:8]
passing_cols <- stats[,9:22]
prog_cols <- stats[,23:33]
def_cols <- stats[,34:40]
crea_cols <- stats[,41:42]

pca_shooting <- prcomp(shooting_cols, scale = TRUE)
weights_shooting <- pca_shooting$rotation[,1]
weighted_avg_shooting <- rowSums(shooting_cols * weights_shooting)
stats$shooting <- weighted_avg_shooting

pca_passing <- prcomp(passing_cols, scale = TRUE)
weights_passing <- pca_passing$rotation[,1]
weighted_avg_passing <- rowSums(passing_cols * weights_passing)
stats$passing <- weighted_avg_passing

pca_prog <- prcomp(prog_cols, scale = TRUE)
weights_prog <- pca_prog$rotation[,1]
weighted_avg_prog <- rowSums(prog_cols * weights_prog)
stats$prog <- weighted_avg_prog

pca_def <- prcomp(def_cols, scale = TRUE)
weights_def <- pca_def$rotation[,1]
weighted_avg_def <- rowSums(def_cols * weights_def)
stats$def <- weighted_avg_def

pca_crea <- prcomp(crea_cols, scale = TRUE)
weights_crea <- pca_crea$rotation[,1]
weighted_avg_crea <- rowSums(crea_cols * weights_crea)
stats$crea <- weighted_avg_crea402 

compiled <- stats %>%
  select(player, team, comp, shooting, passing, prog, def, crea)

compiled[,4:8] <- as.data.frame(apply(compiled[,4:8], 2, function(x) rank(x) / length(x) * 100))

compiled[,4:8] <- round(compiled[,4:8], 2)

prem <- subset(compiled, compiled$player == "Kevin De Bruyne" | compiled$player == "Martin Ødegaard" | compiled$player == "Bruno Fernandes" | compiled$player == "Rodri" | compiled$player == "Casemiro") %>%
  group_by(player) %>%
  select(player, Shooting = shooting, Passing = passing, Progression = prog, Defense = def, Creation = crea)

premradar <- prem %>%
  ggradar(
    legend.position = "bottom",
    base.size = 50,
    font.radar = "roboto",
    grid.label.size = 6,
    axis.label.size = 6,
    group.point.size = 3,
    grid.max = 100,
    grid.mid = 50,
    grid.min = 0,
    plot.title = "Premier League Midfielder Style Analysis",
  ) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

premradar

laliga <- subset(compiled, compiled$player == "Toni Kroos" | compiled$player == "Frenkie de Jong" | compiled$player == "Pedri" | compiled$player == "Eduardo Camavinga" | compiled$player == "Antoine Griezmann") %>%
  group_by(player) %>%
  select(player, Shooting = shooting, Passing = passing, Progression = prog, Defense = def, Creation = crea)

laligaradar <- laliga %>%
  ggradar(
    legend.position = "bottom",
    base.size = 50,
    font.radar = "roboto",
    grid.label.size = 6,
    axis.label.size = 6,
    group.point.size = 3,
    grid.max = 100,
    grid.mid = 50,
    grid.min = 0,
    plot.title = "La Liga Midfielder Style Analysis",
  ) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

laligaradar

seriea <- subset(compiled, compiled$player == "Ademola Lookman" | compiled$player == "Paulo Dybala" | compiled$player == "Stanislav Lobotka" | compiled$player == "Rafael Leão" | compiled$player == "Sandro Tonali") %>%
  group_by(player) %>%
  select(player, Shooting = shooting, Passing = passing, Progression = prog, Defense = def, Creation = crea)

seriearadar <- seriea %>%
  ggradar(
    legend.position = "bottom",
    base.size = 50,
    font.radar = "roboto",
    grid.label.size = 6,
    axis.label.size = 6,
    group.point.size = 3,
    grid.max = 100,
    grid.mid = 50,
    grid.min = 0,
    plot.title = "Serie A Midfielder Style Analysis",
  ) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

seriearadar

bundesliga <- subset(compiled, compiled$player == "Joshua Kimmich" | compiled$player == "Jude Bellingham" | compiled$player == "Christopher Nkunku" | compiled$player == "Jamal Musiala" | compiled$player == "Dominik Szoboszlai") %>%
  group_by(player) %>%
  select(player, Shooting = shooting, Passing = passing, Progression = prog, Defense = def, Creation = crea)

bundesligaradar <- bundesliga %>%
  ggradar(
    legend.position = "bottom",
    base.size = 50,
    font.radar = "roboto",
    grid.label.size = 6,
    axis.label.size = 6,
    group.point.size = 3,
    grid.max = 100,
    grid.mid = 50,
    grid.min = 0,
    plot.title = "Bundesliga Midfielder Style Analysis",
  ) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

bundesligaradar

ligueone <- subset(compiled, compiled$player == "Lionel Messi" | compiled$player == "Marco Verratti" | compiled$player == "Alexis Sánchez" | compiled$player == "Mattéo Guendouzi" | compiled$player == "Seko Fofana") %>%
  group_by(player) %>%
  select(player, Shooting = shooting, Passing = passing, Progression = prog, Defense = def, Creation = crea)

ligueoneradar <- ligueone %>%
  ggradar(
    legend.position = "bottom",
    base.size = 50,
    font.radar = "roboto",
    grid.label.size = 6,
    axis.label.size = 6,
    group.point.size = 3,
    grid.max = 100,
    grid.mid = 50,
    grid.min = 0,
    plot.title = "Ligue One Midfielder Style Analysis",
  ) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

ligueoneradar


