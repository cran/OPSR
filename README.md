
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OPSR <img src="man/figures/logo.svg" align="right" height="139" alt="" />

<!-- badges: start -->
<!-- badges: end -->

Estimates ordinal probit switching regression (OPSR) models - a Heckman
type selection model with an ordinal selection and continuous outcomes.
Different model specifications are allowed for each treatment/regime.

## Installation

Install from CRAN:

``` r
install.packages("OPSR")
```

You can install the development version of `OPSR` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dheimgartner/OPSR")
```

## Example

`OPSR` can be used whenever the ordinal treatment is not assigned
exogenously but self-selected and one is interested in the treatment
effect on a continuous outcome. The motivating example is telework
frequency (conceptually, the treatment) and vehicle miles driven (the
outcome of interest). We assume that two distinct processes lead people
to choose a certain telework frequency and how mobile they are. Further
and most importantly, the possibility of selection on unobservables
exists. I.e., unobserved factors (as part of the errors of the two
processes) might influence both the ordinal and continuous outcome. This
leads to error correlation which needs to be accounted for in the
modeling effort in spirit of Heckman.

``` r
library(OPSR)
#> 
#> Please cite the 'OPSR' package as:
#> Heimgartner, D. and X. Wang (2024) OPSR: A package for estimating ordinal probit switching regression models in R. tbc.
#> 
#> Wang, X. and P. L. Mokhtarian (2024) Examining the treatment effect of teleworking on vehicle-miles driven: Applying an ordered probit selection model and incorporating the role of travel stress, Transportation Research Part A, 186, 104072, doi:10.1016/j.tra.2024.104072.
#> 
#> If you have questions, suggestions, or comments regarding the 'OPSR' package, please open an issue on https://github.com/dheimgartner/OPSR
#> 
#> To see these entries in BibTeX format, use 'citation('OPSR')'
```

``` r

f <-
  ## ordinal and continuous outcome
  twing_status | vmd_ln ~
  ## selection model
  edu_2 + edu_3 + hhincome_2 + hhincome_3 +
  flex_work + work_fulltime + twing_feasibility +
  att_proactivemode + att_procarowning +
  att_wif + att_proteamwork +
  att_tw_effective_teamwork + att_tw_enthusiasm + att_tw_location_flex |
  ## outcome model NTW
  female + age_mean + age_mean_sq +
  race_black + race_other +
  vehicle + suburban + smalltown + rural +
  work_fulltime +
  att_prolargehouse + att_procarowning +
  region_waa |
  ## outcome model NUTW
  edu_2 + edu_3 + suburban + smalltown + rural +
  work_fulltime +
  att_prolargehouse + att_proactivemode + att_procarowning |
  ## outcome model UTW
  female + hhincome_2 + hhincome_3 +
  child + suburban + smalltown + rural +
  att_procarowning +
  region_waa

fit <- opsr(f, telework_data, printLevel = 0)
texreg::screenreg(fit, beside = TRUE, include.pseudoR2 = TRUE, include.R2 = TRUE)
#> 
#> ===============================================================================================
#>                            Structural    Selection     Outcome 1     Outcome 2     Outcome 3   
#> -----------------------------------------------------------------------------------------------
#> kappa1                         1.15 ***                                                        
#>                               (0.17)                                                           
#> kappa2                         2.38 ***                                                        
#>                               (0.18)                                                           
#> sigma1                         1.18 ***                                                        
#>                               (0.05)                                                           
#> sigma2                         1.24 ***                                                        
#>                               (0.07)                                                           
#> sigma3                         1.43 ***                                                        
#>                               (0.04)                                                           
#> rho1                           0.07                                                            
#>                               (0.10)                                                           
#> rho2                           0.13                                                            
#>                               (0.07)                                                           
#> rho3                           0.30 ***                                                        
#>                               (0.07)                                                           
#> edu_2                                        0.24                        0.22                  
#>                                             (0.14)                      (0.34)                 
#> edu_3                                        0.40 **                     0.67 *                
#>                                             (0.13)                      (0.33)                 
#> hhincome_2                                   0.09                                      0.47    
#>                                             (0.11)                                    (0.26)   
#> hhincome_3                                   0.28 *                                    0.30    
#>                                             (0.11)                                    (0.25)   
#> flex_work                                    0.28 **                                           
#>                                             (0.10)                                             
#> work_fulltime                                0.25 *        0.44 ***      0.71 ***              
#>                                             (0.10)        (0.13)        (0.17)                 
#> twing_feasibility                            0.13 ***                                          
#>                                             (0.01)                                             
#> att_proactivemode                            0.08 *                     -0.18 *                
#>                                             (0.04)                      (0.08)                 
#> att_procarowning                            -0.08          0.12          0.17          0.25 ***
#>                                             (0.04)        (0.07)        (0.09)        (0.06)   
#> att_wif                                      0.12 **                                           
#>                                             (0.04)                                             
#> att_proteamwork                              0.09 *                                            
#>                                             (0.04)                                             
#> att_tw_effective_teamwork                    0.32 ***                                          
#>                                             (0.04)                                             
#> att_tw_enthusiasm                            0.09 *                                            
#>                                             (0.04)                                             
#> att_tw_location_flex                         0.08 *                                            
#>                                             (0.04)                                             
#> (Intercept)                                                3.74 ***      2.42 ***      2.38 ***
#>                                                           (0.28)        (0.39)        (0.29)   
#> female                                                    -0.21 *                     -0.37 ***
#>                                                           (0.11)                      (0.11)   
#> age_mean                                                   0.01 **                             
#>                                                           (0.00)                               
#> age_mean_sq                                               -0.00                                
#>                                                           (0.00)                               
#> race_black                                                -0.39                                
#>                                                           (0.24)                               
#> race_other                                                -0.02                                
#>                                                           (0.18)                               
#> vehicle                                                    0.13 **                             
#>                                                           (0.05)                               
#> suburban                                                   0.01          0.45 *        0.29 *  
#>                                                           (0.16)        (0.17)        (0.14)   
#> smalltown                                                  0.41 *        0.22          0.31    
#>                                                           (0.18)        (0.29)        (0.28)   
#> rural                                                      0.49 *        0.82 *        0.85 ** 
#>                                                           (0.23)        (0.32)        (0.33)   
#> att_prolargehouse                                          0.19 ***      0.16 *                
#>                                                           (0.05)        (0.08)                 
#> region_waa                                                -0.24 *                     -0.27 *  
#>                                                           (0.11)                      (0.11)   
#> child                                                                                  0.19 ** 
#>                                                                                       (0.06)   
#> -----------------------------------------------------------------------------------------------
#> AIC                         7189.51       7189.51       7189.51       7189.51       7189.51    
#> BIC                         7490.10       7490.10       7490.10       7490.10       7490.10    
#> Log Likelihood             -3538.76      -3538.76      -3538.76      -3538.76      -3538.76    
#> Pseudo R^2 (EL)                0.49          0.49          0.49          0.49          0.49    
#> Pseudo R^2 (MS)                0.46          0.46          0.46          0.46          0.46    
#> R^2                            0.24          0.24          0.24          0.24          0.24    
#> Num. obs.                   1584          1584          1584          1584          1584       
#> ===============================================================================================
#> *** p < 0.001; ** p < 0.01; * p < 0.05
```
