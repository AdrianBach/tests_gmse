# Adrian Bach
# tests GMSE

#### init ####
rm(list=ls())

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


# gmse apply = 1 time step
# gmse = simul sur plusieurs ts

# tentative de modif de la 
