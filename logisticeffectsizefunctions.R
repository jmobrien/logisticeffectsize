# converting log-odds (from b's) to probabilities: 
logodds2prob <- function(x){exp(x)/(1+exp(x))}

# Comparing effect sizes of different effect b's, given a common intercept b:
LogisticEffectSizeCompare <- function(intercept_b, overall_b, within_b){
  statistics <- 
  tibble(
    Pb = logodds2prob(intercept_b),
    Po = logodds2prob(intercept_b + overall_b),
    `∆Po` = Po - Pb,
    `∆Po_prop` = `∆Po`/Pb,
    Pw = logodds2prob(intercept_b + within_b),
    `∆Pw` = Pw - Pb,
    `∆Pw_prop` = `∆Pw`/Pb,
    `∆Pchange` = `∆Pw`-`∆Po`,
    `∆Pchange_prop` = `∆Pchange`/`∆Pw`,
    `∆Pchange_%` = round(`∆Pchange_prop`*100, 2)
  )
  
  markers <- 
  tibble(
    Category = c("BASELINE", rep("OVERALL", 3), 
                 rep("WITHIN", 3), 
                 rep("COMPARE", 3)),
    Component = c("success probability (Pb):",
                  "Prob @ +1SD predictor (Po):",
                  "Increase in success prob (Po-Pb):",
                  "prop. inc. in success prob (∆Po/Pb):",
                  "Prob @ +1SD predictor (Pw):",
                  "Increase in success prob (Pw-Pb):",
                  "prop. inc. in success prob (∆Pw/Pb):",
                  "difference in ∆-prob (∆Pw-∆Po):",
                  "proportional diff ∆-prob (∆Pw-∆Po)/∆Pw:",
                  "% change effect size:"
  )) 
      
  out <- 
    bind_cols(
      markers,
      pivot_longer(statistics, everything(), 
                   names_to = "statistic",
                   values_to = "value")
    )
  return(out)
}

#Looking at this with self-control #'s:
LogisticEffectSizeCompare(intercept_b = -1.62, 
                          overall_b = .26, 
                          within_b = .28)
