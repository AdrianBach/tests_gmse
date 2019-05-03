# Adrian Bach
# tests GMSE

#### init ####
rm(list=ls())

## wd
#setwd("Desktop/Thèse/GitKraken/tests_gmse/")
setwd("Documents/GitKraken/tests_gmse/")
getwd()

## packages

# gmse
library("GMSE", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")

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

# Attention: seules les valeurs du dernier timestep sont presentees
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
  
  # final year's mean yield 
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

# avec un plus grand budget pour les managers
facteur <- 1.1

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
  
  # final year's mean yield 
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

facteur <- 1.2
# init table
res_f3 <- matrix(nrow = nbrep, ncol = nbmesures*3, dimnames = list(c(1:nbrep),c("pop_mean","pop_sd","pop_95ci","dev_mean","dev_sd","dev_95ci","yield_mean","yield_sd","yield_95ci")))

# loop
for (i in 1:nbrep) {
  sim <- gmse(time_max = 10, stakeholders = 2, land_ownership = T, manage_target = target, user_budget = budget, manager_budget = facteur*budget)
  table <- gmse_table(sim)
  ts <- sqrt(max(table[,1]))
  
  # mean pop
  res_f3[i,1] <- mean(table[,2])
  res_f3[i,2] <- sd(table[,2])
  res_f3[i,3] <- 1.86*res_f3[i,2]/sqrt(ts)
  
  # final year's mean yield 
  res_f3[i,7] <- mean(table[,8])
  res_f3[i,8] <- sd(table[,8])
  res_f3[i,9] <- 1.86*res_f3[i,8]/sqrt(ts)
  
  # deviation from target
  dev <- abs(table[,2]-target)
  res_f3[i,4] <- mean(dev)
  res_f3[i,5] <- sd(dev)
  res_f3[i,6] <- 1.86*res_f3[i,5]/sqrt(ts)
  
}
res_f3

# premier plot des resultats
par(mfrow=c(1,1))
boxplot(res_f1[,1],res_f2[,1],res_f3[,1])
boxplot(res_f1[,1+3],res_f2[,1+3],res_f3[,1+3])
boxplot(res_f1[,1+6],res_f2[,1+6],res_f3[,1+6])

# bizarre, plus de variabilite avec un plus grand ecart entre les budgets
# parfois, le revenu des users est plus bas quand la diff de budget est plus grande

###################################################################################

# fini d'ajouter l'action threshold, on va tester ca tsai
simtest_at <- gmse(land_ownership = TRUE, stakeholders = 2, observe_type = 0,
                   res_death_K = 2000, manage_target = 1000, RESOURCE_ini = 1000,
                   user_budget = 1000, manager_budget = 1000, res_consume = 1,
                   scaring = F, plotting = T, time_max = 20, action_thres = 0.05, budget_bonus = 0.1)

# check si ca fonctionne la con de ca
simtest_at$paras[,106:108]

plot_gmse_effort(simtest_at)

# ok bah feu aux simulations
# je commence par qui?
# les trucs pour le poster
# une simul qui montre bien ce qu'il se passe
# une figure qui montre l'effet sur la qualidad
# du coup variable proxy de la qualidad de la conservation avec une du revenu des usagers pour differents thresholds, dont 0 correspondant a agir a chaque fois

# pour les trucs au cours du temps y a des chances qu'il faille aller dans le code gmse.R
# tu peux checker la fonction qui sert a plotter A FAIRE A FAIRE A FAIRE A FAIRE

# un vecteur avec des valeurs de AT
at <- seq(0,0.10,0.05)

# un pour le budget bonus
bb <- seq(0,0.2,0.1)

# un nombre de ts
ts <- 10

# un nombre de replicats
rep <- 5

# un budget initial
bud_ini <- 1000

# initial resources
res_ini <- 1000

# manager target
man_tar <- 1000

# une structure pour acceuillir les resultats
# est-ce qu'on ferait pas un objet avec n = at*bb couches
columns <- c("rep", "at", "bb", "init_budg", "init_res", "extinct", "act_dev", "final yield", "max_diff_yield", "inac_ts") 
results <- array(data=NA, dim = c(rep, length(columns), length(at)*length(bb)), dimnames = list(NULL,columns,NULL))
print(results)
# ok le tableau est pret fo le remplir mtn

# preparer un tableau pour les stats
avrg_columns <- c("rep", "at", "bb", "init_budg", "init_res", "ext_prob", "act_dev", "sd", "95ci", "final yield", "sd", "95ci", "max_diff_yield", "sd", "95ci", "inac_ts", "sd", "95ci")
avrg_results <- matrix(data = NA, nrow = length(at)*length(bb), ncol = length(avrg_columns), dimnames = list(NULL,avrg_columns))
print(avrg_results)
# ok c'est pret

# bon maintenant faut balancer les simuls

# initialize a param combo tracker
param_set <- 1

# sim loop
for (i in 1:length(at)) {
  for (j in 1:length(bb)) {
    for (k in 1:rep) {
      # lancer simul
      sim <- gmse(land_ownership = TRUE, stakeholders = 3, observe_type = 0, manage_target = man_tar, RESOURCE_ini = res_ini,
                  user_budget = bud_ini, manager_budget = bud_ini,
                  scaring = TRUE, plotting = F, time_max = ts, action_thres = at[i], budget_bonus = bb[j])
      
      # last time step
      final_ts <- dim(sim$paras)[1]
      
      # ecrire dans results les infos correspondantes
      # replicate
      results[k,1,param_set] <- k
      
      # at value
      results[k,2,param_set] <- at[i]
      
      # bb value
      results[k,3,param_set] <- bb[j]
      
      # initial budget
      results[k,4,param_set] <- bud_ini
      
      # initial resource pop
      results[k,5,param_set] <- res_ini
      
      # has extinction occured?
      results[k,7,param_set] <- ifelse(dim(sim$resource[[final_ts]])[1] < 20, 1, 0)
      
      # actual pop deviation from target
      #results[k,6,param_set] <- tab[dim(tab)[1],2]/man_tar - 1
      results[k,6,param_set] <- dim(sim$resource[[final_ts]])[1]/man_tar - 1
      
      # total final yield
      results[k,8,param_set] <- sum(sim$agents[[final_ts]][,16])
      
      # maximal difference between users yield
      results[k,9,param_set] <- round((max(sim$agents[[final_ts]][,16]) - min(sim$agents[[final_ts]][-1,16]))/max(sim$agents[[final_ts]][,16]),2)
      
      # timesteps spend inactive?
      results[k,10,param_set] <- length(sim$paras[,107])-sum(sim$paras[,107])
    }
    
    # increment tracker
    param_set <- param_set + 1
  }
}
results

# tableau de stats
for (i in 1:dim(results)[3]) {
  avrg_results[i,1] <- dim(results)[1]
  for (j in 2:5) {
    avrg_results[i,j] <- results[1,j,i]
  }
  avrg_results[i,6] <- round(sum(results[,6,i])/dim(results)[1],2)
  zz <- 0
  for (k in 7:dim(results)[2]) {
    avrg_results[i,k+zz] <- mean(results[,k,i])
    avrg_results[i,k+zz+1] <- sd(results[,k,i])
    avrg_results[i,k+zz+2] <- 1.86*avrg_results[i,k+zz+1]/sqrt(rep)
    zz <- zz + 2
  }
}
avrg_results

