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

#### tests ####
# fini d'ajouter l'action threshold, on va tester ca tsai
simtest_at <- gmse(land_ownership = TRUE, stakeholders = 2, observe_type = 0,
                   res_death_K = 2000, manage_target = 1000, RESOURCE_ini = 1000,
                   user_budget = 1000, manager_budget = 1000,
                   scaring = F, plotting = T, time_max = 20, action_thres = 0.1, budget_bonus = 0.1, lambda = 0.25)

# check si ca fonctionne la con de ca
simtest_at$paras[,106:108]

plot_gmse_effort(simtest_at)

#### premiere figure ####
# simul avec action threshold
w_at1 <- simtest_at
w_at1 <- gmse(land_ownership = TRUE, stakeholders = 2, observe_type = 0,
                   res_death_K = 2000, manage_target = 1000, RESOURCE_ini = 1000,
                   user_budget = 1000, manager_budget = 1000,
                   scaring = F, plotting = T, time_max = 20, action_thres = 0.1, budget_bonus = 0.1, lambda = 0.27)

# save manager activity
w_at1_polup <- w_at1$paras[,107]
w_at1_polup

# save actions
tmax <- dim(w_at1$paras)[1]
w_at1_act <- rep(NA, tmax)
for (i in 1:tmax) {
  w_at1_act[i] <- w_at1$action[[i]][1,9,2] + w_at1$action[[i]][1,9,3]
}
w_at1_act

# without at
wo_at1 <- gmse(land_ownership = TRUE, stakeholders = 2, observe_type = 0,
             res_death_K = 2000, manage_target = 1000, RESOURCE_ini = 1000,
             user_budget = 1000, manager_budget = 1000,
             scaring = F, plotting = T, time_max = 20, action_thres = 0.0, budget_bonus = 0.1, lambda = 0.275)

# save manager activity
wo_at1_polup <- wo_at1$paras[,107]
wo_at1_polup

# save actions
tmax <- dim(wo_at1$paras)[1]
wo_at1_act <- rep(NA, tmax)
for (i in 1:tmax) {
  wo_at1_act[i] <- wo_at1$action[[i]][1,9,2] + wo_at1$action[[i]][1,9,3]
}
wo_at1_act

# petit plot des fams

#### ok bah feu aux simulations ####
# je commence par qui?
# les trucs pour le poster
# une simul qui montre bien ce qu'il se passe
# une figure qui montre l'effet sur la qualidad
# du coup variable proxy de la qualidad de la conservation avec une du revenu des usagers pour differents thresholds, dont 0 correspondant a agir a chaque fois

# pour les trucs au cours du temps y a des chances qu'il faille aller dans le code gmse.R
# tu peux checker la fonction qui sert a plotter A FAIRE A FAIRE A FAIRE A FAIRE

#### simul ####

# un vecteur avec des valeurs de AT
at <- seq(0,0.5,0.1)

# un pour le budget bonus
bb <- seq(0,0.1,0.1)

# un nombre de ts
ts <- 10

# un nombre de replicats
rep <- 30

# un budget initial
bud_ini <- 1000

# initial resources
res_ini <- 1000

# manager target
man_tar <- 1000

# growth rate
lbd <- 0.27

# une structure pour acceuillir les resultats
# est-ce qu'on ferait pas un objet avec n = at*bb couches
columns <- c("rep", "at", "bb", "init_budg", "init_res", "lambda", "extinct", "act_dev", "final yield", "max_diff_yield", "inac_ts") 
results_batch2 <- array(data=NA, dim = c(rep, length(columns), length(at)*length(bb)), dimnames = list(NULL,columns,NULL))
print(results_batch2)
# ok le tableau est pret fo le remplir mtn

# preparer un tableau pour les stats
avrg_columns <- c("rep", "at", "bb", "init_budg", "init_res", "lambda", "ext_prob", "act_dev", "act_dev_sd", "act_dev_95ci", "fin_yield", "fin_yield_sd", "fin_yield_95ci", "max_diff_yield", "max_diff_yield_sd", "max_diff_yield_95ci", "inac_ts", "inac_ts_sd", "inac_ts_95ci")
avrg_results_batch2 <- matrix(data = NA, nrow = length(at)*length(bb), ncol = length(avrg_columns), dimnames = list(NULL,avrg_columns))
print(avrg_results_batch2)
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
                  user_budget = bud_ini, manager_budget = bud_ini, lambda = lbd,
                  scaring = F, plotting = F, time_max = ts, action_thres = at[i], budget_bonus = bb[j])
      
      # last time step
      final_ts <- length(which(sim$paras[,1] != 0))
      
      # ecrire dans results les infos correspondantes
      # replicate
      results_batch2[k,1,param_set] <- k
      
      # at value
      results_batch2[k,2,param_set] <- at[i]
      
      # bb value
      results_batch2[k,3,param_set] <- bb[j]
      
      # initial budget
      results_batch2[k,4,param_set] <- bud_ini
      
      # initial resource pop
      results_batch2[k,5,param_set] <- res_ini
      
      # growth rate
      results_batch2[k,6, param_set] <- lbd
      
      # has extinction occured?
      #results_batch2[k,6,param_set] <- ifelse(dim(sim$resource[[final_ts-1]])[1] < 20, 1, 0)
      results_batch2[k,7,param_set] <- ifelse(final_ts < dim(sim$paras)[1], 1, 0)
      
      if (results_batch2[k,7,param_set] != 0) {
        # actual pop deviation from target
        #results_batch2[k,6,param_set] <- tab[dim(tab)[1],2]/man_tar - 1
        results_batch2[k,8,param_set] <- abs(dim(sim$resource[[final_ts-1]])[1]/man_tar - 1)
        
        # total final yield
        results_batch2[k,9,param_set] <- sum(sim$agents[[final_ts-1]][,16])
        
        # maximal difference between users yield
        results_batch2[k,10,param_set] <- round((max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16]),2)
        
        # timesteps spend inactive?
        results_batch2[k,11,param_set] <- round(final_ts-sum(sim$paras[,107]),1)
      }
      else {
        # actual pop deviation from target in absolute values
        results_batch2[k,8,param_set] <- abs(dim(sim$resource[[final_ts]])[1]/man_tar - 1)
      
        # total final yield
        results_batch2[k,9,param_set] <- sum(sim$agents[[final_ts]][,16])
      
        # maximal difference between users yield
        results_batch2[k,10,param_set] <- round((max(sim$agents[[final_ts]][,16]) - min(sim$agents[[final_ts]][-1,16]))/max(sim$agents[[final_ts]][,16]),2)
      
        # timesteps spend inactive?
        results_batch2[k,11,param_set] <- round(length(sim$paras[,107])-sum(sim$paras[,107]),1)
      }
    }
    
    # increment tracker
    param_set <- param_set + 1
  }
}
#results_batch2

# tableau de stats
for (i in 1:dim(results_batch2)[3]) {
  avrg_results_batch2[i,1] <- dim(results_batch2)[1]
  for (j in 2:6) {
    avrg_results_batch2[i,j] <- results_batch2[1,j,i]
  }
  avrg_results_batch2[i,7] <- round(sum(results_batch2[,7,i])/dim(results_batch2)[1],2)
  zz <- 0
  for (k in 8:dim(results_batch2)[2]) {
    avrg_results_batch2[i,k+zz] <- mean(results_batch2[,k,i])
    avrg_results_batch2[i,k+zz+1] <- sd(results_batch2[,k,i])
    avrg_results_batch2[i,k+zz+2] <- 1.86*avrg_results_batch2[i,k+zz+1]/sqrt(rep)
    zz <- zz + 2
  }
}
avrg_results_batch2

write.csv(avrg_results_batch2, file = "batch2.csv", row.names = F)

#### trace des figures ####
library(gplots)
library(ggplot2)

# changer nom colonnes
# colnames(avrg_results_large) <- c("rep", "at", "bb", "init_budg", "init_res", "ext_prob", "act_dev", "act_dev_sd", "act_dev_95ci", "fin_yield", "fin_yield_sd", "fin_yield_95ci", "max_diff_yield", "max_diff_yield_sd", "max_diff_yield_95ci", "inac_ts", "inac_ts_sd", "inac_ts_95ci")

# deviation de la pop reelle en fonction du action threshold
fig1_tab <- subset(avrg_results_batch2, bb == 0.1)
#fig1_tab$at <- as.factor(fig1_tab$at)

fig1_tab <- fig1_tab[-c(2,4,6,8,10,11),]

plot(fig1_tab[,2], fig1_tab[,7], type = "p", pch = 16, xlab = "Action threshold value", ylab = "Actual pop deviation from target")

plotCI(x = fig1_tab[,2], y = fig1_tab[,7]*100, uiw = fig1_tab[,8]*100/2, pch = 16, gap=0, xlab = "Action threshold value", ylab = "Actual pop deviation from target")

p1 <-  ggplot(as.data.frame(fig1_tab), aes(x=as.factor(at), y=act_dev)) + 
  geom_bar(position=position_dodge(), stat="identity", colour="black", fill="light green") +
  geom_errorbar(aes(ymin=act_dev, ymax=act_dev+act_dev_sd),
                width=.2,
                position=position_dodge(.9)) +
  xlab("Action Threshold Value (% of population target)") +
  ylab("Actual population deviation from target")
p1

p2 <-  ggplot(as.data.frame(fig1_tab), aes(x=as.factor(at), y=fin_yield/100)) + 
  geom_bar(position=position_dodge(), stat="identity", colour="black", fill="orange1") +
  geom_errorbar(aes(ymin=fin_yield/100, ymax=fin_yield/100+fin_yield_sd/100),
                width=.2,
                position=position_dodge(.9)) +
  xlab("Action Threshold Value (% of population target)") +
  ylab("Total final users yield (??)")
p2

# comment on les mets sur le meme graph ???


library(grid)
library(dplyr)

#' Create the two plots.
plot1 <- as.data.frame(fig1_tab) %>%
  select(as.factor(at), act_dev, act_dev_sd) %>%
  na.omit() %>%
  ggplot() +
  geom_errorbar(aes(x = at, ymin=act_dev*100-act_dev_sd*100/2, ymax=act_dev*100+act_dev_sd*100/2),
                width=.01, colour = "grey2") +
  geom_point(aes(x = at, y = act_dev*100), size = 5, alpha = 1, colour="black", fill = "light green", stroke = 1, shape = 21) +
  #ylab("Actual pop deviation from target (%)") +
  #xlab("Action Threshold Value (% of population target)")
  theme_gray(base_size = 15) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.line.y = element_line(size = 1, colour = "grey50"))

plot2 <- as.data.frame(fig1_tab) %>%
  select(as.factor(at), fin_yield, fin_yield_sd) %>%
  na.omit() %>%
  ggplot() +
  geom_errorbar(aes(x = at, ymin=fin_yield/100-fin_yield_sd/100/2, ymax=fin_yield/100+fin_yield_sd/100/2),
                width=.01,
                colour="grey2") +
  geom_point(aes(x = at, y = fin_yield/100), size = 5, alpha = 1, colour="black", fill = "orange1", stroke = 1, shape = 21) +
  #ylab("Users final total yield (in k??)") +
  #xlab("Action threshold value (% of pop target)") +
  theme_gray(base_size = 15) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.line = element_line(size = 1, colour = "grey50"))
  #theme(axis.title.x = "Action threshold value (% of pop target)")

grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))

#### batch 3 ####

#### simul ####

# un vecteur avec des valeurs de AT
at <- seq(0,0.5,0.1)

# un pour le budget bonus
bb <- seq(0,0.1,0.1)

# un nombre de ts
ts <- 20

# un nombre de replicats
rep <- 30

# un budget initial
bud_ini <- 1000

# initial resources
res_ini <- 1000

# manager target
man_tar <- 1000

# growth rate
lbd <- 0.27

# une structure pour acceuillir les resultats
# est-ce qu'on ferait pas un objet avec n = at*bb couches
columns <- c("rep", "at", "bb", "init_budg", "init_res", "lambda", "extinct", "act_dev", "final yield", "max_diff_yield", "inac_ts") 
results_batch3 <- array(data=NA, dim = c(rep, length(columns), length(at)*length(bb)), dimnames = list(NULL,columns,NULL))
print(results_batch3)
# ok le tableau est pret fo le remplir mtn

# preparer un tableau pour les stats
avrg_columns <- c("rep", "at", "bb", "init_budg", "init_res", "lambda", "ext_prob", "act_dev", "act_dev_sd", "act_dev_95ci", "fin_yield", "fin_yield_sd", "fin_yield_95ci", "max_diff_yield", "max_diff_yield_sd", "max_diff_yield_95ci", "inac_ts", "inac_ts_sd", "inac_ts_95ci")
avrg_results_batch3 <- matrix(data = NA, nrow = length(at)*length(bb), ncol = length(avrg_columns), dimnames = list(NULL,avrg_columns))
print(avrg_results_batch3)
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
                  user_budget = bud_ini, manager_budget = bud_ini, lambda = lbd,
                  scaring = F, plotting = F, time_max = ts, action_thres = at[i], budget_bonus = bb[j])
      
      # last time step
      final_ts <- length(which(sim$paras[,1] != 0))
      
      # ecrire dans results les infos correspondantes
      # replicate
      results_batch3[k,1,param_set] <- k
      
      # at value
      results_batch3[k,2,param_set] <- at[i]
      
      # bb value
      results_batch3[k,3,param_set] <- bb[j]
      
      # initial budget
      results_batch3[k,4,param_set] <- bud_ini
      
      # initial resource pop
      results_batch3[k,5,param_set] <- res_ini
      
      # growth rate
      results_batch3[k,6, param_set] <- lbd
      
      # has extinction occured?
      results_batch3[k,7,param_set] <- ifelse(final_ts < dim(sim$paras)[1], 1, 0)
      
      if (results_batch3[k,7,param_set] != 0) {
        # actual pop deviation from target
        results_batch3[k,8,param_set] <- abs(dim(sim$resource[[final_ts-1]])[1]/man_tar - 1)
        
        # total final yield
        results_batch3[k,9,param_set] <- sum(sim$agents[[final_ts-1]][,16])
        
        # maximal difference between users yield
        results_batch3[k,10,param_set] <- round((max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16]),2)
        
        # timesteps spend inactive?
        results_batch3[k,11,param_set] <- round(final_ts-sum(sim$paras[,107]),1)
      }
      else {
        # actual pop deviation from target in absolute values
        results_batch3[k,8,param_set] <- abs(dim(sim$resource[[final_ts]])[1]/man_tar - 1)
        
        # total final yield
        results_batch3[k,9,param_set] <- sum(sim$agents[[final_ts]][,16])
        
        # maximal difference between users yield
        results_batch3[k,10,param_set] <- round((max(sim$agents[[final_ts]][,16]) - min(sim$agents[[final_ts]][-1,16]))/max(sim$agents[[final_ts]][,16]),2)
        
        # timesteps spend inactive?
        results_batch3[k,11,param_set] <- round(length(sim$paras[,107])-sum(sim$paras[,107]),1)
      }
    }
    
    # increment tracker
    param_set <- param_set + 1
  }
}

# tableau de stats
for (i in 1:dim(results_batch3)[3]) {
  avrg_results_batch3[i,1] <- dim(results_batch3)[1]
  for (j in 2:6) {
    avrg_results_batch3[i,j] <- results_batch3[1,j,i]
  }
  avrg_results_batch3[i,7] <- round(sum(results_batch3[,7,i])/dim(results_batch3)[1],2)
  zz <- 0
  for (k in 8:dim(results_batch3)[2]) {
    avrg_results_batch3[i,k+zz] <- mean(results_batch3[,k,i])
    avrg_results_batch3[i,k+zz+1] <- sd(results_batch3[,k,i])
    avrg_results_batch3[i,k+zz+2] <- 1.86*avrg_results_batch3[i,k+zz+1]/sqrt(rep)
    zz <- zz + 2
  }
}
avrg_results_batch3

write.csv(avrg_results_batch3, file = "batch3.csv", row.names = F)

#### trace des figures ####

# deviation de la pop reelle en fonction du action threshold
figb3_tab <- subset(avrg_results_batch3, bb == 0.1)

#' Create the two plots.
plot1 <- as.data.frame(figb3_tab) %>%
  select(as.factor(at), act_dev, act_dev_sd) %>%
  na.omit() %>%
  ggplot() +
  geom_errorbar(aes(x = at, ymin=act_dev*100-act_dev_sd*100/2, ymax=act_dev*100+act_dev_sd*100/2),
                width=.01, colour = "grey2") +
  geom_point(aes(x = at, y = act_dev*100), size = 5, alpha = 1, colour="black", fill = "light green", stroke = 1, shape = 21) +
  ylab("Actual pop deviation from target (%)") +
  #xlab("Action Threshold Value (% of population target)")
  theme_gray(base_size = 15) +
  theme(axis.title.x = element_blank(), axis.line.y = element_line(size = 1, colour = "grey50"))

plot2 <- as.data.frame(figb3_tab) %>%
  select(as.factor(at), fin_yield, fin_yield_sd) %>%
  na.omit() %>%
  ggplot() +
  geom_errorbar(aes(x = at, ymin=fin_yield/100-fin_yield_sd/100/2, ymax=fin_yield/100+fin_yield_sd/100/2),
                width=.01,
                colour="grey2") +
  geom_point(aes(x = at, y = fin_yield/100), size = 5, alpha = 1, colour="black", fill = "orange1", stroke = 1, shape = 21) +
  ylab("Users final total yield (in k??)") +
  xlab("Action threshold value (% of pop target)") +
  theme_gray(base_size = 15) +
  theme(axis.line = element_line(size = 1, colour = "grey50"))
#theme(axis.title.x = "Action threshold value (% of pop target)")

grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))

# without labels and big features

plot1 <- as.data.frame(figb3_tab) %>%
  select(as.factor(at), act_dev, act_dev_sd) %>%
  na.omit() %>%
  ggplot() +
  geom_errorbar(aes(x = at, ymin=act_dev*100-act_dev_sd*100/2, ymax=act_dev*100+act_dev_sd*100/2),
                width=.01,colour = "grey2") +
  geom_point(aes(x = at, y = act_dev*100), size = 7, alpha = 1, colour="black", fill = "light green", stroke = 1, shape = 21)+
  #ylab("Users final total yield (in k??)") +
  #xlab("Action threshold value (% of pop target)") +
  theme_gray(base_size = 20) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.line = element_line(size = 1, colour = "grey50"))

plot2 <- as.data.frame(figb3_tab) %>%
  select(as.factor(at), fin_yield, fin_yield_sd) %>%
  na.omit() %>%
  ggplot() +
  geom_errorbar(aes(x = at, ymin=fin_yield/100-fin_yield_sd/100/2, ymax=fin_yield/100+fin_yield_sd/100/2),
                width=.01,
                colour="grey2") +
  geom_point(aes(x = at, y = fin_yield/100), size = 5, alpha = 1, colour="black", fill = "orange1", stroke = 1, shape = 21) +
  ylab("Users final total yield (in k??)") +
  xlab("Action threshold value (% of pop target)") +
  theme_gray(base_size = 15) +
  theme(axis.line = element_line(size = 1, colour = "grey50"))
#theme(axis.title.x = "Action threshold value (% of pop target)")

grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))

#### batch 4 : narrow down action threshold values ####

#### simul ####

# un vecteur avec des valeurs de AT
at <- seq(0,0.2,0.05)

# un pour le budget bonus
bb <- seq(0,0.1,0.1)

# un nombre de ts
ts <- 20

# un nombre de replicats
rep <- 30

# un budget initial
bud_ini <- 1000

# initial resources
res_ini <- 1000

# manager target
man_tar <- 1000

# growth rate
lbd <- 0.27

# une structure pour acceuillir les resultats
# est-ce qu'on ferait pas un objet avec n = at*bb couches
columns <- c("rep", "at", "bb", "init_budg", "init_res", "lambda", "extinct", "act_dev", "final yield", "max_diff_yield", "inac_ts") 
results_batch4 <- array(data=NA, dim = c(rep, length(columns), length(at)*length(bb)), dimnames = list(NULL,columns,NULL))

# preparer un tableau pour les stats
avrg_columns <- c("rep", "at", "bb", "init_budg", "init_res", "lambda", "ext_prob", "act_dev", "act_dev_sd", "act_dev_95ci", "fin_yield", "fin_yield_sd", "fin_yield_95ci", "max_diff_yield", "max_diff_yield_sd", "max_diff_yield_95ci", "inac_ts", "inac_ts_sd", "inac_ts_95ci")
avrg_results_batch4 <- matrix(data = NA, nrow = length(at)*length(bb), ncol = length(avrg_columns), dimnames = list(NULL,avrg_columns))

# bon maintenant faut balancer les simuls

# initialize a param combo tracker
param_set <- 1

# sim loop
for (i in 1:length(at)) {
  for (j in 1:length(bb)) {
    for (k in 1:rep) {
      # lancer simul
      sim <- gmse(land_ownership = TRUE, stakeholders = 3, observe_type = 0, manage_target = man_tar, RESOURCE_ini = res_ini,
                  user_budget = bud_ini, manager_budget = bud_ini, lambda = lbd,
                  scaring = F, plotting = F, time_max = ts, action_thres = at[i], budget_bonus = bb[j])
      
      # last time step
      final_ts <- length(which(sim$paras[,1] != 0))
      
      # ecrire dans results les infos correspondantes
      # replicate
      results_batch4[k,1,param_set] <- k
      
      # at value
      results_batch4[k,2,param_set] <- at[i]
      
      # bb value
      results_batch4[k,3,param_set] <- bb[j]
      
      # initial budget
      results_batch4[k,4,param_set] <- bud_ini
      
      # initial resource pop
      results_batch4[k,5,param_set] <- res_ini
      
      # growth rate
      results_batch4[k,6, param_set] <- lbd
      
      # has extinction occured?
      results_batch4[k,7,param_set] <- ifelse(final_ts < dim(sim$paras)[1], 1, 0)
      
      if (results_batch4[k,7,param_set] != 0) {
        # actual pop deviation from target
        results_batch4[k,8,param_set] <- abs(dim(sim$resource[[final_ts-1]])[1]/man_tar - 1)
        
        # total final yield
        results_batch4[k,9,param_set] <- sum(sim$agents[[final_ts-1]][,16])
        
        # maximal difference between users yield
        results_batch4[k,10,param_set] <- round((max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16]),2)
        
        # timesteps spend inactive?
        results_batch4[k,11,param_set] <- round(final_ts-sum(sim$paras[,107]),1)
      }
      else {
        # actual pop deviation from target in absolute values
        results_batch4[k,8,param_set] <- abs(dim(sim$resource[[final_ts]])[1]/man_tar - 1)
        
        # total final yield
        results_batch4[k,9,param_set] <- sum(sim$agents[[final_ts]][,16])
        
        # maximal difference between users yield
        results_batch4[k,10,param_set] <- round((max(sim$agents[[final_ts]][,16]) - min(sim$agents[[final_ts]][-1,16]))/max(sim$agents[[final_ts]][,16]),2)
        
        # timesteps spend inactive?
        results_batch4[k,11,param_set] <- round(length(sim$paras[,107])-sum(sim$paras[,107]),1)
      }
    }
    
    # increment tracker
    param_set <- param_set + 1
  }
}

# tableau de stats
for (i in 1:dim(results_batch4)[3]) {
  avrg_results_batch4[i,1] <- dim(results_batch4)[1]
  for (j in 2:6) {
    avrg_results_batch4[i,j] <- results_batch4[1,j,i]
  }
  avrg_results_batch4[i,7] <- round(sum(results_batch4[,7,i])/dim(results_batch4)[1],2)
  zz <- 0
  for (k in 8:dim(results_batch4)[2]) {
    avrg_results_batch4[i,k+zz] <- mean(results_batch4[,k,i])
    avrg_results_batch4[i,k+zz+1] <- sd(results_batch4[,k,i])
    avrg_results_batch4[i,k+zz+2] <- 1.86*avrg_results_batch4[i,k+zz+1]/sqrt(rep)
    zz <- zz + 2
  }
}
View(avrg_results_batch4)

write.csv(avrg_results_batch4, file = "batch4.csv", row.names = F)

#### trace des figures ####

# deviation de la pop reelle en fonction du action threshold
figb4_tab <- subset(avrg_results_batch4, bb == 0.1)

#' Create the two plots.
plot1 <- as.data.frame(figb4_tab) %>%
  select(as.factor(at), act_dev, act_dev_sd) %>%
  na.omit() %>%
  ggplot() +
  geom_errorbar(aes(x = at, ymin=act_dev*100-act_dev_sd*100/2, ymax=act_dev*100+act_dev_sd*100/2),
                width=.01, colour = "grey2") +
  geom_point(aes(x = at, y = act_dev*100), size = 5, alpha = 1, colour="black", fill = "light green", stroke = 1, shape = 21) +
  ylab("Actual pop deviation from target (%)") +
  #xlab("Action Threshold Value (% of population target)")
  theme_gray(base_size = 15) +
  theme(axis.title.x = element_blank(), axis.line.y = element_line(size = 1, colour = "grey50"))

plot2 <- as.data.frame(figb4_tab) %>%
  select(as.factor(at), fin_yield, fin_yield_sd) %>%
  na.omit() %>%
  ggplot() +
  geom_errorbar(aes(x = at, ymin=fin_yield/100-fin_yield_sd/100/2, ymax=fin_yield/100+fin_yield_sd/100/2),
                width=.01,
                colour="grey2") +
  geom_point(aes(x = at, y = fin_yield/100), size = 5, alpha = 1, colour="black", fill = "orange1", stroke = 1, shape = 21) +
  ylab("Users final total yield (in k??)") +
  xlab("Action threshold value (% of pop target)") +
  theme_gray(base_size = 15) +
  theme(axis.line = element_line(size = 1, colour = "grey50"))
#theme(axis.title.x = "Action threshold value (% of pop target)")

grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))

# without labels and big features

plot1 <- as.data.frame(figb4_tab) %>%
  select(as.factor(at), act_dev, act_dev_sd) %>%
  na.omit() %>%
  ggplot() +
  geom_errorbar(aes(x = at, ymin=act_dev*100-act_dev_sd*100/2, ymax=act_dev*100+act_dev_sd*100/2),
                width=.01,colour = "grey2") +
  geom_point(aes(x = at, y = act_dev*100), size = 7, alpha = 1, colour="black", fill = "light green", stroke = 1, shape = 21)+
  #ylab("Users final total yield (in k??)") +
  #xlab("Action threshold value (% of pop target)") +
  theme_gray(base_size = 20) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.line = element_line(size = 1, colour = "grey50"))

plot2 <- as.data.frame(figb4_tab) %>%
  select(as.factor(at), fin_yield, fin_yield_sd) %>%
  na.omit() %>%
  ggplot() +
  geom_errorbar(aes(x = at, ymin=fin_yield/100-fin_yield_sd/100/2, ymax=fin_yield/100+fin_yield_sd/100/2),
                width=.01,
                colour="grey2") +
  geom_point(aes(x = at, y = fin_yield/100), size = 5, alpha = 1, colour="black", fill = "orange1", stroke = 1, shape = 21) +
  #ylab("Users final total yield (in k??)") +
  #xlab("Action threshold value (% of pop target)") +
  theme_gray(base_size = 15) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.line = element_line(size = 1, colour = "grey50"))

grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))

#### batch5 : poster figure ####

#### simul ####

# un vecteur avec des valeurs de AT
at <- seq(0,0.2,0.05)

# un pour le budget bonus
bb <- seq(0,0.1,0.1)

# un nombre de ts
ts <- 20

# un nombre de replicats
rep <- 50

# un budget initial
bud_ini <- 1000

# initial resources
res_ini <- 1000

# manager target
man_tar <- 1000

# growth rate
lbd <- 0.3

# une structure pour acceuillir les resultats
# est-ce qu'on ferait pas un objet avec n = at*bb couches
columns <- c("rep", "at", "bb", "init_budg", "init_res", "lambda", "extinct", "act_dev", "final yield", "max_diff_yield", "inac_ts") 
results_batch5 <- array(data=NA, dim = c(rep, length(columns), length(at)*length(bb)), dimnames = list(NULL,columns,NULL))

# preparer un tableau pour les stats
avrg_columns <- c("rep", "at", "bb", "init_budg", "init_res", "lambda", "ext_prob", "act_dev", "act_dev_sd", "act_dev_95ci", "fin_yield", "fin_yield_sd", "fin_yield_95ci", "max_diff_yield", "max_diff_yield_sd", "max_diff_yield_95ci", "inac_ts", "inac_ts_sd", "inac_ts_95ci")
avrg_results_batch5 <- matrix(data = NA, nrow = length(at)*length(bb), ncol = length(avrg_columns), dimnames = list(NULL,avrg_columns))

# bon maintenant faut balancer les simuls

# initialize a param combo tracker
param_set <- 1

# sim loop
for (i in 1:length(at)) {
  for (j in 1:length(bb)) {
    for (k in 1:rep) {
      # lancer simul
      sim <- gmse(land_ownership = TRUE, stakeholders = 3, observe_type = 0, manage_target = man_tar, RESOURCE_ini = res_ini,
                  user_budget = bud_ini, manager_budget = bud_ini, lambda = lbd,
                  scaring = F, plotting = F, time_max = ts, action_thres = at[i], budget_bonus = bb[j])
      
      # last time step
      final_ts <- length(which(sim$paras[,1] != 0))
      
      # ecrire dans results les infos correspondantes
      # replicate
      results_batch5[k,1,param_set] <- k
      
      # at value
      results_batch5[k,2,param_set] <- at[i]
      
      # bb value
      results_batch5[k,3,param_set] <- bb[j]
      
      # initial budget
      results_batch5[k,4,param_set] <- bud_ini
      
      # initial resource pop
      results_batch5[k,5,param_set] <- res_ini
      
      # growth rate
      results_batch5[k,6, param_set] <- lbd
      
      # has extinction occured?
      results_batch5[k,7,param_set] <- ifelse(final_ts < dim(sim$paras)[1], 1, 0)
      
      if (results_batch5[k,7,param_set] != 0) {
        # actual pop deviation from target
        results_batch5[k,8,param_set] <- abs(dim(sim$resource[[final_ts-1]])[1]/man_tar - 1)
        
        # total final yield
        results_batch5[k,9,param_set] <- sum(sim$agents[[final_ts-1]][,16])
        
        # maximal difference between users yield
        results_batch5[k,10,param_set] <- round((max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16]),2)
        
        # timesteps spend inactive?
        results_batch5[k,11,param_set] <- round(final_ts-sum(sim$paras[,107]),1)
      }
      else {
        # actual pop deviation from target in absolute values
        results_batch5[k,8,param_set] <- abs(dim(sim$resource[[final_ts]])[1]/man_tar - 1)
        
        # total final yield
        results_batch5[k,9,param_set] <- sum(sim$agents[[final_ts]][,16])
        
        # maximal difference between users yield
        results_batch5[k,10,param_set] <- round((max(sim$agents[[final_ts]][,16]) - min(sim$agents[[final_ts]][-1,16]))/max(sim$agents[[final_ts]][,16]),2)
        
        # timesteps spend inactive?
        results_batch5[k,11,param_set] <- round(length(sim$paras[,107])-sum(sim$paras[,107]),1)
      }
    }
    
    # increment tracker
    param_set <- param_set + 1
  }
}

# tableau de stats
for (i in 1:dim(results_batch5)[3]) {
  avrg_results_batch5[i,1] <- dim(results_batch5)[1]
  for (j in 2:6) {
    avrg_results_batch5[i,j] <- results_batch5[1,j,i]
  }
  avrg_results_batch5[i,7] <- round(sum(results_batch5[,7,i])/dim(results_batch5)[1],2)
  zz <- 0
  for (k in 8:dim(results_batch5)[2]) {
    avrg_results_batch5[i,k+zz] <- mean(results_batch5[,k,i])
    avrg_results_batch5[i,k+zz+1] <- sd(results_batch5[,k,i])
    avrg_results_batch5[i,k+zz+2] <- 1.86*avrg_results_batch5[i,k+zz+1]/sqrt(rep)
    zz <- zz + 2
  }
}
View(avrg_results_batch5)

write.csv(avrg_results_batch5, file = "batch5.csv", row.names = F)

#### trace des figures ####

# deviation de la pop reelle en fonction du action threshold
figb5_tab <- subset(avrg_results_batch5)

#' Create the two plots.
plot1 <- as.data.frame(figb5_tab) %>%
  select(as.factor(at), act_dev, act_dev_sd) %>%
  na.omit() %>%
  ggplot() +
  geom_errorbar(aes(x = at, ymin=act_dev*100-act_dev_sd*100/2, ymax=act_dev*100+act_dev_sd*100/2),
                width=.01, colour = "grey2") +
  geom_point(aes(x = at, y = act_dev*100), size = 5, alpha = 1, colour="black", fill = "light green", stroke = 1, shape = 21) +
  ylab("Actual pop deviation from target (%)") +
  #xlab("Action Threshold Value (% of population target)")
  theme_gray(base_size = 15) +
  theme(axis.title.x = element_blank(), axis.line.y = element_line(size = 1, colour = "grey50"))

plot2 <- as.data.frame(figb5_tab) %>%
  select(as.factor(at), fin_yield, fin_yield_sd) %>%
  na.omit() %>%
  ggplot() +
  geom_errorbar(aes(x = at, ymin=fin_yield/100-fin_yield_sd/100/2, ymax=fin_yield/100+fin_yield_sd/100/2),
                width=.01,
                colour="grey2") +
  geom_point(aes(x = at, y = fin_yield/100), size = 5, alpha = 1, colour="black", fill = "orange1", stroke = 1, shape = 21) +
  ylab("Users final total yield (in k??)") +
  xlab("Action threshold value (% of pop target)") +
  theme_gray(base_size = 15) +
  theme(axis.line = element_line(size = 1, colour = "grey50"))
#theme(axis.title.x = "Action threshold value (% of pop target)")

grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))

# without labels and big features

plot1 <- as.data.frame(figb5_tab) %>%
  select(as.factor(at), as.factor(bb), act_dev, act_dev_sd) %>%
  na.omit() %>%
  ggplot() +
  geom_errorbar(aes(x = at, ymin=act_dev*100-act_dev_sd*100/2, ymax=act_dev*100+act_dev_sd*100/2, group = bb),
                width=.02, colour = "grey2") +
  geom_point(aes(x = at, y = act_dev*100, fill = as.factor(bb)),
             size = 7, alpha = 1, colour="black", stroke = 1, shape = 21,
             position = position_dodge(width = 0.05)) +
  #facet_wrap(~as.factor(at)) +
  #ylab("Users final total yield (in k??)") +
  #xlab("Action threshold value (% of pop target)") +
  theme_gray(base_size = 20) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position='none',
        axis.line.y = element_line(size = 1, colour = "grey50"))
plot1

plot2 <- as.data.frame(figb5_tab) %>%
  select(as.factor(at), fin_yield, fin_yield_sd) %>%
  na.omit() %>%
  ggplot() +
  geom_errorbar(aes(x = at, ymin=fin_yield/100-fin_yield_sd/100/2, ymax=fin_yield/100+fin_yield_sd/100/2),
                width=.01,
                colour="grey2") +
  geom_point(aes(x = at, y = fin_yield/100), size = 5, alpha = 1, colour="black", fill = "orange1", stroke = 1, shape = 21) +
  #ylab("Users final total yield (in k??)") +
  #xlab("Action threshold value (% of pop target)") +
  theme_gray(base_size = 15) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.line = element_line(size = 1, colour = "grey50"))

grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))

# nvlle tentative
plotbb1 <- ggplot(as.data.frame(figb5_tab), aes(x=as.factor(at), y=act_dev*100, group=as.factor(bb), fill = as.factor(bb))) +
           #scale_colour_manual(name="", 
           #                    bb = c("0.0"="yellow", "0.1"="orange"))
           geom_errorbar(aes(ymin=act_dev*100-act_dev_sd*100/2, ymax=act_dev*100+act_dev_sd*100/2, group = as.factor(bb)),  
                position=position_dodge(0.6),
                colour = "grey40", width=0.4) +
           geom_point(size = 10, alpha = 1, colour="black", stroke = 1, shape = 21,
                position = position_dodge(width = 0.6)) +
  theme_gray(base_size = 30) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position='none',
        axis.line.y = element_line(size = 1, colour = "grey50"))

plotbb2 <- ggplot(as.data.frame(figb5_tab), aes(x=as.factor(at), y=fin_yield/100, group=as.factor(bb), fill = as.factor(bb))) +
  #scale_colour_manual(name="", 
  #                    bb = c("0.0"="yellow", "0.1"="orange"))
  geom_errorbar(aes(ymin=fin_yield/100-fin_yield_sd/100/2, ymax=fin_yield/100+fin_yield_sd/100/2, group = as.factor(bb)),  
                position=position_dodge(0.6),
                colour = "grey40", width=0.4) +
  geom_point(size = 10, alpha = 1, colour="black", stroke = 1, shape = 21,
             position = position_dodge(width = 0.6)) +
  theme_gray(base_size = 40) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position='none',
        axis.line = element_line(size = 1, colour = "grey50"))

grid.newpage()
grid.draw(rbind(ggplotGrob(plotbb1), ggplotGrob(plotbb2), size = "last"))

#### batch5 : poster figure ####

#### simul ####

# un vecteur avec des valeurs de AT
at <- seq(0,0.2,0.05)

# un pour le budget bonus
bb <- seq(0,0.1,0.1)

# un nombre de ts
ts <- 20

# un nombre de replicats
rep <- 50

# un budget initial
bud_ini <- 1000

# initial resources
res_ini <- 1000

# manager target
man_tar <- 1000

# growth rate
lbd <- 0.27

# une structure pour acceuillir les resultats
# est-ce qu'on ferait pas un objet avec n = at*bb couches
columns <- c("rep", "at", "bb", "init_budg", "init_res", "lambda", "extinct", "act_dev", "final yield", "max_diff_yield", "inac_ts") 
results_batch6 <- array(data=NA, dim = c(rep, length(columns), length(at)*length(bb)), dimnames = list(NULL,columns,NULL))

# preparer un tableau pour les stats
avrg_columns <- c("rep", "at", "bb", "init_budg", "init_res", "lambda", "ext_prob", "act_dev", "act_dev_sd", "act_dev_95ci", "fin_yield", "fin_yield_sd", "fin_yield_95ci", "max_diff_yield", "max_diff_yield_sd", "max_diff_yield_95ci", "inac_ts", "inac_ts_sd", "inac_ts_95ci")
avrg_results_batch6 <- matrix(data = NA, nrow = length(at)*length(bb), ncol = length(avrg_columns), dimnames = list(NULL,avrg_columns))

# bon maintenant faut balancer les simuls

# initialize a param combo tracker
param_set <- 1

# sim loop
for (i in 1:length(at)) {
  for (j in 1:length(bb)) {
    for (k in 1:rep) {
      # lancer simul
      sim <- gmse(land_ownership = TRUE, stakeholders = 3, observe_type = 0, manage_target = man_tar, RESOURCE_ini = res_ini,
                  user_budget = bud_ini, manager_budget = bud_ini, lambda = lbd,
                  scaring = F, plotting = F, time_max = ts, action_thres = at[i], budget_bonus = bb[j])
      
      # last time step
      final_ts <- length(which(sim$paras[,1] != 0))
      
      # ecrire dans results les infos correspondantes
      # replicate
      results_batch6[k,1,param_set] <- k
      
      # at value
      results_batch6[k,2,param_set] <- at[i]
      
      # bb value
      results_batch6[k,3,param_set] <- bb[j]
      
      # initial budget
      results_batch6[k,4,param_set] <- bud_ini
      
      # initial resource pop
      results_batch6[k,5,param_set] <- res_ini
      
      # growth rate
      results_batch6[k,6, param_set] <- lbd
      
      # has extinction occured?
      results_batch6[k,7,param_set] <- ifelse(final_ts < dim(sim$paras)[1], 1, 0)
      
      if (results_batch6[k,7,param_set] != 0) {
        # actual pop deviation from target
        results_batch6[k,8,param_set] <- abs(dim(sim$resource[[final_ts-1]])[1]/man_tar - 1)
        
        # total final yield
        results_batch6[k,9,param_set] <- sum(sim$agents[[final_ts-1]][,16])
        
        # maximal difference between users yield
        results_batch6[k,10,param_set] <- round((max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16]),2)
        
        # timesteps spend inactive?
        results_batch6[k,11,param_set] <- round(final_ts-sum(sim$paras[,107]),1)
      }
      else {
        # actual pop deviation from target in absolute values
        results_batch6[k,8,param_set] <- abs(dim(sim$resource[[final_ts]])[1]/man_tar - 1)
        
        # total final yield
        results_batch6[k,9,param_set] <- sum(sim$agents[[final_ts]][,16])
        
        # maximal difference between users yield
        results_batch6[k,10,param_set] <- round((max(sim$agents[[final_ts]][,16]) - min(sim$agents[[final_ts]][-1,16]))/max(sim$agents[[final_ts]][,16]),2)
        
        # timesteps spend inactive?
        results_batch6[k,11,param_set] <- round(length(sim$paras[,107])-sum(sim$paras[,107]),1)
      }
    }
    
    # increment tracker
    param_set <- param_set + 1
  }
}

# tableau de stats
for (i in 1:dim(results_batch6)[3]) {
  avrg_results_batch6[i,1] <- dim(results_batch6)[1]
  for (j in 2:6) {
    avrg_results_batch6[i,j] <- results_batch6[1,j,i]
  }
  avrg_results_batch6[i,7] <- round(sum(results_batch6[,7,i])/dim(results_batch6)[1],2)
  zz <- 0
  for (k in 8:dim(results_batch6)[2]) {
    avrg_results_batch6[i,k+zz] <- mean(results_batch6[,k,i])
    avrg_results_batch6[i,k+zz+1] <- sd(results_batch6[,k,i])
    avrg_results_batch6[i,k+zz+2] <- 1.86*avrg_results_batch6[i,k+zz+1]/sqrt(rep)
    zz <- zz + 2
  }
}
View(avrg_results_batch6)

write.csv(avrg_results_batch6, file = "batch6.csv", row.names = F)

#### trace des figures ####

# deviation de la pop reelle en fonction du action threshold
figb6_tab <- avrg_results_batch6

# without labels and big features

plotbb1 <- ggplot(as.data.frame(figb6_tab), aes(x=as.factor(at), y=act_dev*100, group=as.factor(bb), fill = as.factor(bb))) +
  #scale_colour_manual(name="", 
  #                    bb = c("0.0"="yellow", "0.1"="orange"))
  geom_errorbar(aes(ymin=act_dev*100-act_dev_sd*100/2, ymax=act_dev*100+act_dev_sd*100/2, group = as.factor(bb)),  
                position=position_dodge(0.6),
                colour = "grey40", width=0.4) +
  geom_point(size = 6, alpha = 1, colour="black", stroke = 1, shape = 21,
             position = position_dodge(width = 0.6)) +
  theme_gray(base_size = 50) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position='none',
        axis.line.y = element_line(size = 1, colour = "grey50"))

plotbb2 <- ggplot(as.data.frame(figb6_tab), aes(x=as.factor(at), y=fin_yield/100, group=as.factor(bb), fill = as.factor(bb))) +
  #scale_colour_manual(name="", 
  #                    bb = c("0.0"="yellow", "0.1"="orange"))
  geom_errorbar(aes(ymin=fin_yield/100-fin_yield_sd/100/2, ymax=fin_yield/100+fin_yield_sd/100/2, group = as.factor(bb)),  
                position=position_dodge(0.6),
                colour = "grey40", width=0.4) +
  geom_point(size = 6, alpha = 1, colour="black", stroke = 1, shape = 21,
             position = position_dodge(width = 0.6)) +
  theme_gray(base_size = 50) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position='none',
        axis.line = element_line(size = 1, colour = "grey50"))

grid.newpage()
grid.draw(rbind(ggplotGrob(plotbb1), ggplotGrob(plotbb2), size = "last"))
