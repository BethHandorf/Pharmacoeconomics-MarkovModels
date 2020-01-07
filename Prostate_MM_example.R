############################################
# Example Markov model for first-line treatment of metastatic prostate cancer
# Adapted from Abiraterone Acetate (AA) strategy described in:
#   Ramamurthy C, Handorf EA, Correa AF, Beck JR, Geynisman DM. 
#   Cost-effectiveness of abiraterone versus docetaxel in the treatment of metastatic 
#   hormone naïve prostate cancer. Urologic Oncology: Seminars and Original Investigations 
#   2019 Oct 1 (Vol. 37, No. 10, pp. 688-695). Elsevier.

library(heemod)

###### Set probabilities
par_mod <- define_parameters( 
  Pr_SD_to_PD = 0.0208, # Monthly probability of progression from stable disease 
                        # (estimated from LATITUDE trial results)
  Pr_SD_to_Death = 0.00164, # Monthly probability of death from progression 
                            # (SSA actuarial life tables - 68 y/o male probability of death 2016)
  Pr_PD_to_Death = 0.0205, # Monthly probability of death from progression 
                           # (estimated from MAINSAIL trial results)
  
  Pr_FAT = 0.003, # Probabilty of fatigue in first 6 months 
                  # (estimated from LATITUDE trial results)
  Pr_SD_to_SDFAT = ifelse(model_time<=6, 0.003, 0) # After 6 months, assume zero probability of fatigue
)


###### Define transition matrix

#Possible states:
#1) SD: M1 disease (no progression)
#2) SD_FAT: No progression, fatigue
#3) PD: Progressed disease
#4) Death

matAA<-define_transition(
  state_names=c("SD","SD_FAT","PD","Death"),
  C, Pr_SD_to_SDFAT, Pr_SD_to_PD, Pr_SD_to_Death, #SD
  0, C,              Pr_SD_to_PD, Pr_SD_to_Death, #SD_FAT
  0, 0,              C,           Pr_PD_to_Death, #PD
  0, 0,              0,           C               #Death
)


###### Define states, assign value

     # Costs, QALYs accrued per cycle in each state would be added here 
     # for full cost-effectiveness analysis

state_SD<-define_state(
  SurvMo = 1
)

state_SD_FAT<-define_state(
  SurvMo = 1
)

state_PD<-define_state(
  SurvMo = 1
)

state_Death<-define_state(
  SurvMo = 0
)


###### Set up AA stragegy

strat_AA <-define_strategy(
  transition=matAA,
  
  SD = state_SD,
  SD_FAT = state_SD_FAT,
  PD = state_PD,
  Death = state_Death
)


###### Run the model
res_mod <- run_model(
  parameters=par_mod,
  AA=strat_AA,   # Only 1 stragegy considered here
  cycles=60,     # 5 years, 1 month cycles
  effect=SurvMo, # Effect in months of life
  method="life-table"
)

#Plots of state counts by markov cycle
plot1<-plot(res_mod, type="counts", panel="by_state",free_y=TRUE)
#Re-order the facets of the graph
plot1[['data']]$state_names<-factor(plot1[['data']]$state_names,levels=c("SD","SD_FAT","PD","Death"))
print(plot1)
