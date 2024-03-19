
#------------------------------------------------------------
# single path formulae
#------------------------------------------------------------

#' Compute Expectancy Violation (EV)
#'
#' @param threat_expectancy The anticipated threat before exposure, on a scale from 0 to 1
#' @param threat_occurence The actually occuring threat during exposure, on a scale from 0 to 1

compute_ev <- function(threat_expectancy, threat_occurence){
  ev <- threat_expectancy - threat_occurence
  return(ev)
}


#' Compute Expectancy Change (EC)
#'
#' @param ev The expectancy violation, on a scale from -1 to 1
#' @param a The learn rate, the extent to which Expectancy Violation is transformed into Expectancy Change, on a scale from 0 to 1

compute_ec <- function(ev, a){
  ec <- a*ev
  return(ec)
}

#' Compute the adjusted Threat Expectancy
#'
#' @param threat_expectancy The anticipated threat before exposure, on a scale from 0 to 1
#' @param ec The Expectancy Change, on a scale from -1 to 1
compute_threat_expectancy <- function(threat_expectancy, ec){
  threat_expectancy2 <- threat_expectancy - ec
  return(threat_expectancy2)
}

#------------------------------------------------------------
# Simulation
#------------------------------------------------------------


#' Compute updated person after one exposure-session
#'
#' @param threat_occurence The actual occurence of threat, on a scale from 0 to 1
#' @param person Name vector c(threat_expectancy, a) where threat_expectancy is
#'  the initial threat expectancy and a is the learnrate of that person
#' 
#' @return updated person vector after one iteration
exposure_iteration <- function(threat_occurence, person) {
  ev <- compute_ev(threat_occurence, person$threat_expectancy)
  ec <- compute_ec(ev, person$a)
  threat_expectancy <- compute_threat_expectancy(threat_expectancy = person$threat_expectancy, ec = ec)
  
  person$threat_expectancy <-threat_expectancy
  person
}

#' Compute updated person after several exposure-sessionsd
#'
#' @param num_iterations The number of exposure-sessions the person will go through
#' @param threat_occurence The actual occurence of threat, on a scale from 0 to 1
#' @param person Name vector c(threat_expectancy, a) where threat_expectancy is
#'  the initial threat expectancy and a is the learnrate of that person
#' 
#' @return updated person vector after complete intervention
exposure_sessions <- function(num_iterations, person, threat_occurence) {
  for (i in seq_len(num_iterations)) {
    person <- exposure_iteration(threat_occurence, person)
  }
  
  person
}

#------------------------------------------------------------
# Helper Functions
#------------------------------------------------------------

generate_group <- function(n) {
  data.frame(
    id = seq_len(n),
    a = rbeta(n, shape1=8.7, shape2=3.5),
    threat_expectancy = rbeta(n, shape1=3.5, shape2=3.5)
  )
}

#------------------------------------------------------------
# Test
#------------------------------------------------------------

library(dplyr)


# Do a simulated study with n=200 (overall)
#------------------------------------------------

threat_occurence <- 0.7
intervention_iterations <- 1
n <- 200 # overall sample size (control + experimental group)

group <- generate_group(n)

# every other person is assigned to the treatment-condition, every remaining person is assigned to the control-condition
group <- group |>
  mutate(condition = ifelse(id %% 2 == 0, "treatment", "control"))



group <- group %>%
  mutate(
    threat_expectancy_t2 = 
      ifelse(
        condition == "treatment",  # if a person is part of the treatment-condition, the exposure_sessions-function is executed
        exposure_sessions(
          intervention_iterations,
          person = list(threat_expectancy = threat_expectancy, a = a),
          threat_occurence = threat_occurence)$threat_expectancy,
        threat_expectancy
      )
  )



# groups difference after intervention: treatment effect
t1 <- t.test(group$threat_expectancy_t2 ~ group$condition, alternative = "greater")
t1

library(compute.es)
ES <- tes(t1$statistic, n/2, n/2, level = 95, verbose=FALSE)
ES$d


install.packages('ggrain')
library(ggrain)
ggplot(group, aes(x=condition, y=threat_expectancy_t2)) + geom_rain() + ggtitle(paste0("Effect size: d = ", round(ES$d, 2)))


