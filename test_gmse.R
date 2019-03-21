# Adrian Bach
# tests GMSE

#### init ####
rm(list=ls())

# wd
setwd("Desktop/Thèse/GitKraken/tests_gmse/")
getwd()

#### tests ####

sim1 <- gmse_apply()
sim1
head(sim1)

# from help
sim <- gmse(lambda = 0.3, land_ownership = TRUE, time_max = 10, plotting = TRUE)
#head(sim)

# from article
sim_1 <- gmse(land_ownership = TRUE, stakeholders = 2, observe_type = 0,
            res_death_K = 2000, manage_target = 1000, RESOURCE_ini = 1000,
            user_budget = 1000, manager_budget = 1000, res_consume = 1,
            scaring = TRUE, plotting = T, time_max = 10)

names(sim_1)
sim_1$action[[1]]  # action array at first time step
# par defaut le premier layer est le manager
# les autres sont les users

sim_1$observation[[3]]
sim_1$land
sim_1$paras
sim_1$agents[[2]]

# gmse apply = 1 time step
# gmse = simul sur plusieurs ts

# tentative de modif de du rapport des budgets
# deux fois plus
sim_budget <- gmse(land_ownership = TRUE, stakeholders = 2, observe_type = 0,
              res_death_K = 2000, manage_target = 1000, RESOURCE_ini = 1000,
              user_budget = 1000, manager_budget = 2000, res_consume = 1,
              scaring = TRUE, plotting = T, time_max = 10)

names(sim_budget)

#### extraire des mesures ####

# desactiver le plot
sim_budget2 <- gmse(land_ownership = TRUE, stakeholders = 2, observe_type = 0,
                    res_death_K = 2000, manage_target = 1000, RESOURCE_ini = 1000,
                    user_budget = 1000, manager_budget = 2000, res_consume = 1,
                    scaring = TRUE, plotting = F, time_max = 10)

# avec gmse_table
budget_table <- gmse_table(gmse_sim = sim_budget2)
budget_table
# colonne "cost_unused" -> jamais max (ce serait 220 on dirait)

# on va checker comment ca se comporte quand le manager agit tous les deux ts 
sim_freq2 <- gmse(land_ownership = TRUE, stakeholders = 2, observe_type = 0,
                    res_death_K = 2000, manage_target = 1000, RESOURCE_ini = 1000,
                    user_budget = 1000, manager_budget = 1000, res_consume = 1,
                    scaring = TRUE, plotting = F, time_max = 10, manage_freq = 2)
freq2_table <- gmse_table(gmse_sim = sim_freq2)
freq2_table
# not to update the policy = apply the same costs as previous timestep

## que se passe-t-il si manager n'agit jaja
sim_freq_jaja <- gmse(land_ownership = TRUE, stakeholders = 2, observe_type = 0,
                      res_death_K = 2000, manage_target = 1000, RESOURCE_ini = 1000,
                      user_budget = 1000, manager_budget = 1000, res_consume = 1,
                      scaring = TRUE, plotting = T, time_max = 10, manage_freq = 10)
(freq_jaja_table <- gmse_table(sim_freq_jaja))

# possibilite de choper que le dernier time step avec gmse_table(, all_time = F)

#### replicats de simulation ####

## manager n'agit jamais
(gmse_rep_jaja <- gmse_replicates(replicates = 10, time_max = 10, plotting = F, stakeholders = 2, manage_freq = 11))
# extinction most of le temps
# pas agir = pas estimer la pop non plus!

## effet d'un ecart de budget
# mm budget
gmse_rep_same <- gmse_replicates(replicates = 10, time_max = 10, plotting = F, stakeholders = 2)
gmse_rep_same

# deux fois plus de budget pour les managers
gmse_rep_twice <- gmse_replicates(replicates = 10, time_max = 10, plotting = F, stakeholders = 2, manager_budget = 2000)
gmse_rep_twice

# Attention: seul les valeurs du dernier timestep sont presentees
# parfait pour les mesures de finalite genre la proba d'extinction sur la duree de la finalite, l'ecart a la target apres n timestep, final crop yield
## IMPORTANT: ça change vraiment pas masse la policy, car un changement de 10 est suffisant pour assurer la conservation
## Aussi, si manager agit pas, pas d'estimation de la pop !! attention a ca cdm

#### recuperer des moyennes sur tous les timesteps ####
# init vecteurs de la longueur du nombre de replicats pour acceuillir les moyennes, sd, et IC
# voire un tableau de resultats ncol = nb de mesures d'interet x3, nrow = nb de replicats
# boucle for sur les replicats
# simuls
# recup gmse_table
# faire des differences pour obtinir la variation par rapport a une mesure
# moyenner, stats
# ajouter les mesures a la bonne place dans le tableau

# nb de replicats
nbrep <- 5
target <- 1000
budget <- 1000

# on va mesurer la variabilite moyenne de la pop par rapport a la target au cours des simus (from gmse_table$crop_yield)
# variabilite dans le crop yield total (mean + var + ic from gmse_table$crop_yield)
# ecart max de yield entre les users (from sim$agents[,17]) PLUS TARD, ON COMMENCE SIMPLE

# nb de mesures a relever: deux calculs + pop
nbmesures <- 3

# meme budget 
facteur <- 1

# init table
res_f1 <- matrix(nrow = nbrep, ncol = nbmesures*3, dimnames = list(c(1:nbrep),c("pop_mean","pop_sd","pop_95ci","dev_mean","dev_sd","dev_95ci","yield_mean","yield_sd","yield_95ci")))

# loop
for (i in 1:nbrep) {
  sim <- gmse(time_max = 10, stakeholders = 2, land_ownership = T, manage_target = target, user_budget = budget, manager_budget = facteur*budget)
  table <- gmse_table(sim)
  ts <- sqrt(max(table[,1]))
  
  # mean pop
  res_f1[i,1] <- mean(table[,2])
  res_f1[i,2] <- sd(table[,2])
  res_f1[i,3] <- 1.86*res_f1[i,2]/sqrt(ts)
  res_f1[i,7] <- mean(table[,8])
  res_f1[i,8] <- sd(table[,8])
  res_f1[i,9] <- 1.86*res_f1[i,8]/sqrt(ts)
  
  # deviation from target
  dev <- abs(table[,2]-target)
  res_f1[i,4] <- mean(dev)
  res_f1[i,5] <- sd(dev)
  res_f1[i,6] <- 1.86*res_f1[i,5]/sqrt(ts)
  
}
res_f1

# avec un plus grand budget pour les 
facteur <- 2

# init table
res_f2 <- matrix(nrow = nbrep, ncol = nbmesures*3, dimnames = list(c(1:nbrep),c("pop_mean","pop_sd","pop_95ci","dev_mean","dev_sd","dev_95ci","yield_mean","yield_sd","yield_95ci")))

# loop
for (i in 1:nbrep) {
  sim <- gmse(time_max = 10, stakeholders = 2, land_ownership = T, manage_target = target, user_budget = budget, manager_budget = facteur*budget)
  table <- gmse_table(sim)
  ts <- sqrt(max(table[,1]))
  
  # mean pop
  res_f2[i,1] <- mean(table[,2])
  res_f2[i,2] <- sd(table[,2])
  res_f2[i,3] <- 1.86*res_f2[i,2]/sqrt(ts)
  res_f2[i,7] <- mean(table[,8])
  res_f2[i,8] <- sd(table[,8])
  res_f2[i,9] <- 1.86*res_f2[i,8]/sqrt(ts)
  
  # deviation from target
  dev <- abs(table[,2]-target)
  res_f2[i,4] <- mean(dev)
  res_f2[i,5] <- sd(dev)
  res_f2[i,6] <- 1.86*res_f2[i,5]/sqrt(ts)
  
}
res_f2

# premier plot des resultats
par(mfrow=c(1,1))
boxplot(res_f1[,1],res_f2[,1])
boxplot(res_f1[,1+3],res_f2[,1+3])
boxplot(res_f1[,1+6],res_f2[,1+6])

# bizarre, plus de variabilite avec un plus grand ecart entre les budgets
# parfois, le revenu des users est plus bas quand la diff de budget est plus grande