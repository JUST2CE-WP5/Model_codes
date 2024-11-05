### SCENARIOS

# 1) reduction in consumption level: reduction of propensities to consume?
# 2) change in consumption composition (e.g. higher share of services)
# increase consumption share of services, decrease consumption share of manufacturing; increase final investment of manufacturing
# 3) extension of products lifetime by increasing the share of durable goods
# reduce depreciation rate delta for firms; reduce Percentage of durable consumption goods discarded zeta_dc for households


# CE PRACTICES BY HOUSEHOLDS AND FIRMS
if (shock == 1 && i == t.shock) 
{
  Z1_alpha1 <- 1.55 * reduction * Z1_alpha1 
  Z1_alpha2 <- 1.55 * reduction * Z1_alpha2 
  Z1_alpha3 <- 1.55 * reduction * Z1_alpha3 
}

if (shock == 2 && i == 2) 
{
  Z1_beta[1 , t.shock : nPeriods] <- reduction * Z1_beta[1 , t.shock : nPeriods]
  Z1_beta[3 , t.shock : nPeriods] <- 1 - colSums(Z1_beta[1 : 2 , t.shock : nPeriods])
}

if (shock == 3 && i == t.shock) Z1_delta <- reduction * Z1_delta

if (shock == 4 && i == t.shock) Z1_ce <- 1

if (shock == 5 && i == t.shock) Z1_zeta1 <- Z1_zeta2 <- Z1_zeta3 <- Z1_zeta1 * reduction

if (shock == 7 && i == 2) Z1_mu_mat[ , t.shock : nPeriods] <- 1.5 * reduction * Z1_mu_mat[ , t.shock : nPeriods]

if (shock == 8 && i == 2) Z1_zeta_dc[ , t.shock : nPeriods] <- reduction * Z1_zeta_dc[ , t.shock : nPeriods]

if (shock == 9 && i == 2) Z1_eta_en[ , t.shock : nPeriods] <- Z1_eta_en[ , t.shock : nPeriods] / reduction

# DIRECT CE POLICIES BY THE GOVERNMENT

if (shock == 11 && i == t.shock) 
{
  Z1_g0[ , t.shock]<- 25
  #Z1_g[ , t.shock : nPeriods] <- Z1_g0[ , t.shock : nPeriods] / reduction # (2 - reduction)
  Z1_ce <- 1
}

if (shock == 12 && i == 2) 
{
  Z1_sigma[5 , t.shock : nPeriods] <- 1 - reduction
  Z1_sigma[1 : 3, t.shock : nPeriods] <- reduction * Z1_sigma[1 : 3, t.shock : nPeriods]
}

# INDIRECT EFFECTS ON CE FROM OTHER PRACTICES AND POLICIES

if (shock == 21 && i == t.shock) Z1_theta_c <- 1.15 * Z1_theta_c