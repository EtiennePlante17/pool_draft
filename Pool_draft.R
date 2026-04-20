source("src/find_player.R")
source("src/picked_player.R")
source("src/get_stat.R")
source("src/get_merge_historique.R")

source("params/draft_rules.R")

year_draft <- 2025

projections <- read.csv2(
  file.path("data", year_draft, "fantrax_proj.csv"), 
  sep = ";")

proj <- merge_historique(projections, history_year = 3)

proj <- picked_player(proj, "Connor McDavid", "AP")
find_player(proj, "EDM", "F", "C")

projections %>%
  filter(Player == "Elias Pettersson")

historique_an1 %>%
  filter(Player == "Elias Pettersson")
historique_an2 %>%
  filter(Player == "Elias Pettersson")

roster_status(proj, "EP")

View(proj |>
  filter(available == 1 & FPts > 30) |>
  arrange(pts_salaire, FPts)
)



## Faire un quarto pour les résultats du pool versus le prédit
# mettre la citation de Mike en entré

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
