year_draft <- 2026
recruteur <- "EP"

# Fonctions
source("src/find_player.R")
source("src/picked_player.R")
source("src/get_stat.R")
source("src/get_merge_historique.R")
source("src/show_players.R")
# Params
source(file.path("params", year_draft, "draft_rules.R"))

# données de projection et historique
projections <- read.csv2(file.path("data", year_draft, "fantrax_proj.csv"), sep = ",") |>
  mutate(salaire = as.numeric(gsub(",", "", Salary)))

proj <- merge_historique(projections, history_year = 3)


# Afficher les joueurs
View(proj |>
       filter(available == 1 & FPts > 20 & Position == "D") |>
       arrange(pts_salaire, FPts) |>
       select(Player, Team, Position, Rookie, FPts, Salary, pts_salaire, an1, an2, an3, mean_histo)
)

View(proj |>
       filter(available == 1 & salaire <=3340000 & Position == "G") |>
       arrange(pts_salaire, FPts) |>
       select(Player, Team, Position, Rookie, FPts, Salary, pts_salaire, an1, an2, an3, mean_histo)
)
View(proj |>
       filter(available == 1 & salaire <=2590000 & Position == "D") |>
       arrange(pts_salaire, FPts) |>
       select(Player, Team, Position, Rookie, FPts, Salary, pts_salaire, an1, an2, an3, mean_histo)
)
#low budget
View(proj |>
       filter(available == 1 & FPts > 30 & an2 + an3 == 0 & salaire < 2000000) |>
       arrange(pts_salaire, FPts) |>
       select(Player, Team, Position, Rookie, FPts, Salary, pts_salaire, an1, an2, an3, mean_histo)
)

find_player(proj, "evans", 1)
proj %>% filter(Player == "Morgan Rielly") %>% select(Player, Team, Status, FPts)
proj %>% filter(Player == "Frederik Andersen") %>% select(Player, Team, Status, FPts)
proj %>% filter(Player == "Ilya Sorokin") %>% select(Player, Team, Status, FPts)
proj %>% filter(Player == "Ryker Evans") %>% select(Player, Team, Status, FPts)


View(score_players(proj, 0.35, 0.5, 0.15))# 177F, 30D $, 37G

# Attribuer les joueurs pendant le draft
id <- find_player(proj, "mcd", 1)
proj %>% filter(ID == id) %>% select(Player, Team, Status, FPts)

#Assigner le joueur
proj <- picked_player(proj, id)

# Stats du draft
pool_status(proj)
roster_status(proj, "LG")
proj %>% filter(roster == "EP")

# Résultats du draft
stock_res <- proj %>% 
  filter(available == 0) %>% 
  summarise(FPts = sum(FPts), cash = masse_salariale - sum(salaire), npick = n(), .by = roster) %>% 
  arrange(desc(FPts))
stock_res


# to_pick <- score_players(proj, 0.5, 0.25, 0.25)
# 
# proj <- picked_player(proj, to_pick$ID[1])
# 
# 1:10 (10)
# 11 1
# 12:18 6
# 19:39
# 40

# Dans mes 2 premiers picks, c all in ou un gros steal
# Steals
# Macklin Celebrini
# Beckett Sennecke
# Matthew Schaefer
# Brandon Bussi (Goaler, pas garantie numero 1, projection Fantrax trop optimiste)

### attaquants
# a 160, on est a 42 points

### goaler
# a 32 G, on est a 42 points
# Brandon Bussi 71 point est un steal
# je pense pas que c'est pressant

### Def
# a 80 defenseurs, on est dans les 25 points
# 
# Matthew Schaefer
# Shayne Gostisbehere
# 
# 6 defenseurs en haut de 70 points
# 2 rentables en haut de 50 points


# en bas de 100k / pts,
# 6 def, dont 4 qui sortent du lot
# 74 attaquants
# 20 goalers


#faire des buckets des joueurs
#percentile par position
#Faire un code pour avoir le classement du prédit avec les stats que je faisais en EXCEL
# Pourrait ajouter round et pick dans fonction picked_player
# Ajouter conditions dans find_player pour laisser agrument vide

# Ceux qui ont des meilleurs défenseurs semblent plus se démaruqer parce qu'ils ont trouvé des attaquants pour compenser
# 
# Masse salariale:
# Je pense que c'est pas bon de hold l'argent, tu te ramasses avec des picks cher qui produisent pas tant que ca
# J'ai une preference pour waste ton money au début, sauf si recru à 70 points et que tu waste après
# Je pense que c'est bon de mettre un filtre sur les points minimums en regardant les points par salaire
# Se ramasser a waste 7M pour un defenseur a 40 points c'est pas payant quand hold a fin et pas toutes tes defs
# 
# En oubliant la valeur du pointage, je pense que le prio selon les choix sont: Def, attaquants et Goaler
# 
# Goaler
# Le goaler ne semble pas être urgent, il y en a beaucoup dans les mêmes points.
# y'a environ 14 goalers à 6M et moins qui sont potables
# faut juste s'assurer d'avoir au moins 6M pour un gros goaler
# 
# Défenseurs
# y'a environ 10 défenseurs qui sont rentables en bas de 100k points/salaire dans le format 2025 pour faire au moins 40 points
# Y'a quelques défenseurs qui countent vraiment pas cher a 30 points qui valent la peine
# Je pense que si les defenseurs sont payants, tu privélégie pour avoir dans les tops 10, sinon t'attends pour les low budget en flambant ton cash avant sur les attaquants
# soit low budget ou mets bcp plus de salaire par defenseur que par F
# 
# Attaquants:
#   Y'a quelques attaquants en haut des 100 points qui valent la peine, meme si pas le plus rentable, y'en a peu et ca vaut la peine
# Ceux qui privéligient le points par salaire se ramassent avec trop de cash a fin pour des picks pas rentable. Semble mieux de mettre un filtre pour prendr epoints/salaire mais avec un minimum de points
# Prendre une recrue a avec ben des points pour flober tout de suite apres sur un mega pick est smart move
# je pense que c bon de pas considérer de banc, va avoir au moins un blessé
