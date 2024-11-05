################################################################################

#CREATE TABLES FOR 2-AREA MODELS

#Upload libraries
library(knitr)

#Choose a period (note: 75 = shock starting period)
# yr=2

################################################################################
################################################################################
################################################################################


# # Z1_e_s[1,1] <- Z2_e_s[1,1] <- Z1_lf[1,1] <- Z2_lf[1,1] <- 0
# Z1_e_s[1,1] <- Z1_e_h_Z1[1,1] <- Z1_k[1,1] <- 100

#Create BS matrix
BS <- function(res, yr)
{
  if (res != 0)
  {
  sim <- res$simulation
  # Aggregate Variables
  var.labels <- rownames(sim$variables)
  variable.names <- unique(sapply(strsplit(var.labels, split = '-', fixed = T), function (tmp) tmp[1]))
  for(i in which(!grepl('-', var.labels))) assign(var.labels[i], matrix(sim$variables[i, ], nrow = 1, ncol = nPeriods))
  
  # Industry Variables
  industry.vars <- unique(sapply(strsplit(grep('-', var.labels, value = T), split = '-', fixed = T), function (tmp) tmp[1]))
  
  for(i in 1 : length(industry.vars))
  {
    assign(industry.vars[i], 
           matrix(sim$variables[grep('-', var.labels)[(10 * (i - 1) + 1) : (10 * i)], ], 
                  nrow = 10, ncol = nPeriods))
  }
  }
#Create row names for BS matrix
rownames <-c( "Money",
              "Advances",
              "Deposits",
              "Loans",
              "Area 1 bills",
              "Area 2 bills",
              "Area 1 shares",
              "Area 2 shares",
              "Capital stock",
              "Net financial wealth",
              "Total")

################################################################################

#Create entries for Z1 BS

#Create household aggregates
Z1_Households <- c(round(Z1_h_h[yr], digits = 2),
                0,
                round(Z1_mh[yr], digits = 2),
                round(-Z1_lh[yr], digits = 2),
                round(Z1_b_h_Z1[yr], digits = 2),
                round(Z1_b_h_Z2[yr], digits = 2),
                round(Z1_e_h_Z1[yr], digits = 2),
                round(Z1_e_h_Z2[yr], digits = 2),
                0,
                round(-Z1_v[yr], digits = 2),
                round(Z1_h_h[yr]+Z1_mh[yr]+Z1_b_h_Z1[yr]+Z1_b_h_Z2[yr]-Z1_v[yr]+
                                          +Z1_e_h_Z1[yr]+Z1_e_h_Z2[yr]-Z1_lh[yr], digits = 0)
)                                                                    

#Create table of results
Z1_HouseDataBS<-as.data.frame(Z1_Households,row.names=rownames)
# kable(Z1_HouseDataBS) #Unload # kableExtra to use this

#Create firms aggregates
Z1_Firms <- c(0,                                                                    
           0,
           0,
           round(-Z1_lf[yr], digits = 2),
           0,
           0,
           round(-Z1_e_s[yr], digits = 2),
           0,
           round(Z1_k[yr]*Z1_pid[yr], digits = 2),
           0,
           round(-Z1_lf[yr]+Z1_k[yr]*Z1_pid[yr]-Z1_e_s[yr], digits = 2)
)                                                                    

#Create table of results
Z1_FirmsDataBS<-as.data.frame(Z1_Firms,row.names=rownames)
# kable(Z1_FirmsDataBS) #Unload # kableExtra to use this

#Create government aggregates
Z1_Government <- c(0,
                0,
                0,
                0,
                round(-Z1_b_s[yr], digits = 2),
                0,
                0,
                0,
                0,
                round(Z1_b_s[yr], digits = 2),
                0
)                                                                    

#Create table of results
Z1_GovDataBS<-as.data.frame(Z1_Government,row.names=rownames)
# kable(Z1_GovDataBS) #Unload # kableExtra to use this

#Create banks aggregates
Z1_Banks       <- c(0,
                 round(-Z1_a_d[yr], digits = 2),
                 round(-Z1_mh[yr], digits = 2),
                 round(Z1_ls[yr], digits = 2),
                 round(Z1_b_b[yr], digits = 2),
                 0,
                 0,
                 0,
                 0,
                 0,
                 round(-Z1_a_d[yr]-Z1_mh[yr]+Z1_ls[yr]+Z1_b_b[yr], digits = 2)
)                                                                    

#Create table of results
Z1_BanksDataBS<-as.data.frame(Z1_Banks,row.names=rownames)
# kable(Z1_BanksDataBS) #Unload # kableExtra to use this

#Create CB aggregates
Z1_CentralBank <- c(round(-Z1_h_s[yr], digits = 2),
                 round(Z1_a_s[yr], digits = 2),
                 0,
                 0,
                 round(Z1_b_cb[yr], digits = 2),   # <-------------------------- check
                 round(Z1_b_cb_d_Z2[yr], digits = 2),
                 0,
                 0,
                 0,
                 0,
                 round(-Z1_h_s[yr]+Z1_a_s[yr]+Z1_b_cb[yr]+Z1_b_cb_d_Z2[yr], digits = 2)
)                                                                    

#Create table of results
Z1_CBDataBS<-as.data.frame(Z1_CentralBank,row.names=rownames)
# kable(Z1_CBDataBS) #Unload # kableExtra to use this

################################################################################

#Create entries for Z2 BS

#Create household aggregates
Z2_Households <- c(round(Z2_xr[yr]*Z2_h_h[yr], digits = 2),
                   0,
                   round(Z2_xr[yr]*Z2_mh[yr], digits = 2),
                   round(-Z2_xr[yr]*Z2_lh[yr], digits = 2),
                   round(Z2_xr[yr]*Z2_b_h_Z1[yr], digits = 2),
                   round(Z2_xr[yr]*Z2_b_h_Z2[yr], digits = 2),
                   round(Z2_xr[yr]*Z2_e_h_Z1[yr], digits = 2),
                   round(Z2_xr[yr]*Z2_e_h_Z2[yr], digits = 2),
                   0,
                   round(Z2_xr[yr]*(-Z2_v[yr]), digits = 2),
                   round(Z2_xr[yr]*(Z2_h_h[yr]+Z2_mh[yr]+Z2_b_h_Z1[yr]+Z2_b_h_Z2[yr]-Z2_v[yr]+
                                                        +Z2_e_h_Z1[yr]+Z2_e_h_Z2[yr]-Z2_lh[yr]), digits = 0)
                   
)                                                                    

#Create table of results
Z2_HouseDataBS<-as.data.frame(Z2_Households,row.names=rownames)
# kable(Z2_HouseDataBS) #Unload # kableExtra to use this

#Create firms aggregates
Z2_Firms <- c(0,                                                                    
              0,
              0,
              round(Z2_xr[yr]*(-Z2_lf[yr]), digits = 2),
              0,
              0,
              0,
              round(Z2_xr[yr]*(-Z2_e_s[yr]), digits = 2),
              round(Z2_xr[yr]*(Z2_k[yr]*Z2_pid[yr]), digits = 2),
              0,
              round(Z2_xr[yr]*(-Z2_lf[yr]+Z2_k[yr]*Z2_pid[yr]-Z2_e_s[yr]), digits = 2)
)                                                                    

#Create table of results
Z2_FirmsDataBS<-as.data.frame(Z2_Firms,row.names=rownames)
# kable(Z2_FirmsDataBS) #Unload # kableExtra to use this

#Create government aggregates
Z2_Government <- c(0,
                   0,
                   0,
                   0,
                   0,
                   round(Z2_xr[yr]*(-Z2_b_s[yr]), digits = 2),
                   0,
                   0,
                   0,
                   round(Z2_xr[yr]*(Z2_b_s[yr]), digits = 2),
                   0
)                                                                    

#Create table of results
Z2_GovDataBS<-as.data.frame(Z2_Government,row.names=rownames)
# kable(Z2_GovDataBS) #Unload # kableExtra to use this

#Create banks aggregates
Z2_Banks       <- c(0,
                    round(Z2_xr[yr]*(-Z2_a_d[yr]), digits = 2),
                    round(Z2_xr[yr]*(-Z2_mh[yr]), digits = 2),
                    round(Z2_xr[yr]*Z2_ls[yr], digits = 2),
                    0,
                    round(Z2_xr[yr]*Z2_b_b[yr], digits = 2),
                    0,
                    0,
                    0,
                    0,
                    round(Z2_xr[yr]*(-Z2_a_d[yr]-Z2_mh[yr]+Z2_ls[yr]+Z2_b_b[yr]), digits = 2)
)                                                                    

#Create table of results
Z2_BanksDataBS<-as.data.frame(Z2_Banks,row.names=rownames)
# kable(Z2_BanksDataBS) #Unload # kableExtra to use this

#Create CB aggregates
Z2_CentralBank <- c(round(Z2_xr[yr]*(-Z2_h_s[yr]), digits = 2),
                    round(Z2_xr[yr]*(Z2_a_s[yr]), digits = 2),
                    0,
                    0,
                    0,
                    round(Z2_xr[yr]*(Z2_b_cb[yr]), digits = 2), 
                    0,
                    0,
                    0,
                    0,
                    round(Z2_xr[yr]*(-Z2_h_s[yr]+Z2_a_s[yr]+Z2_b_cb[yr]), digits = 2)
)                                                                    

#Create table of results
Z2_CBDataBS<-as.data.frame(Z2_CentralBank,row.names=rownames)
# kable(Z2_CBDataBS) #Unload # kableExtra to use this

################################################################################

#Create total aggregates
Total <- c( 0,
            0,
            0,
            round(Z1_ls[yr]-Z1_lh[yr]-Z1_lf[yr]+Z2_xr[yr]*(Z2_ls[yr]-Z2_lh[yr]-Z2_lf[yr]), digits=2),
            round(Z1_b_h_Z1[yr]-Z1_b_s[yr]+Z1_b_b[yr]+Z1_b_cb[yr]+Z2_xr[yr]*Z2_b_h_Z1[yr], digits=2),
            round(Z2_b_h_Z2[yr]-Z2_b_s[yr]+Z2_b_b[yr]+Z2_b_cb[yr]+Z1_xr[yr]*Z1_b_h_Z2[yr] + Z1_b_cb_s_Z2[yr], digits=2),
            round(Z1_e_h_Z1[yr]-Z1_e_s[yr]+Z2_xr[yr]*Z2_e_h_Z1[yr], digits=2),
            round(Z2_e_h_Z2[yr]-Z2_e_s[yr]+Z1_xr[yr]*Z1_e_h_Z2[yr], digits=2),
            
            round(Z1_k[yr]*Z1_pid[yr] + Z2_xr[yr]*(Z2_k[yr]*Z2_pid[yr]), digits=2),
            round(-Z1_v[yr] + Z1_b_s[yr] - Z2_xr[yr]*(Z2_v[yr] - Z2_b_s[yr]), digits = 2),
            
            round(Z1_ls[yr]-Z1_lh[yr]-Z1_lf[yr]+Z2_xr[yr]*(Z2_ls[yr]-Z2_lh[yr]-Z2_lf[yr]) +
                  Z1_b_h_Z1[yr]-Z1_b_s[yr]+Z1_b_b[yr]+Z1_b_cb[yr] + Z2_xr[yr]*Z2_b_h_Z1[yr]+
                  Z2_b_h_Z2[yr]-Z2_b_s[yr]+Z2_b_b[yr]+Z2_b_cb[yr] + Z1_xr[yr]*Z1_b_h_Z2[yr] + Z1_b_cb_s_Z2[yr]+
                  Z1_k[yr]*Z1_pid[yr] + Z2_xr[yr]*(Z2_k[yr]*Z2_pid[yr])+
                  -Z1_v[yr] + Z1_b_s[yr] - Z2_xr[yr]*(Z2_v[yr] - Z2_b_s[yr]) +
                  + Z1_e_h_Z1[yr]-Z1_e_s[yr]+Z2_xr[yr]*Z2_e_h_Z1[yr] + Z2_e_h_Z2[yr]-Z2_e_s[yr]+Z1_xr[yr]*Z1_e_h_Z2[yr], digits = 2)
                  
)                                                                    

#Create table of results
TotDataBS<-as.data.frame(Total,row.names=rownames)
# kable(TotDataBS) #Unload # kableExtra to use this

#Create xr column
xr <- c( round(Z1_xr[yr], digits=4),
            round(Z1_xr[yr], digits=4),
            round(Z1_xr[yr], digits=4),
            round(Z1_xr[yr], digits=4),
            round(Z1_xr[yr], digits=4),
            round(Z1_xr[yr], digits=4),
            round(Z1_xr[yr], digits=4),
            round(Z1_xr[yr], digits=4),
            round(Z1_xr[yr], digits=4),
            round(Z1_xr[yr], digits=4),
            paste("")
)        

#Create BS matrix
BS_Matrix<-cbind(Z1_HouseDataBS,Z1_FirmsDataBS,Z1_GovDataBS,Z1_BanksDataBS,Z1_CBDataBS,xr,
                 Z2_HouseDataBS,Z2_FirmsDataBS,Z2_GovDataBS,Z2_BanksDataBS,Z2_CBDataBS,TotDataBS)
# kable(BS_Matrix) #Unload # kableExtra to use this
return(BS_Matrix)
}
################################################################################
################################################################################

#Create TFM matrix
TFM <- function (res, yr)
{
  if (res != 0)
  {
  sim <- res$simulation
  # Aggregate Variables
  var.labels <- rownames(sim$variables)
  variable.names <- unique(sapply(strsplit(var.labels, split = '-', fixed = T), function (tmp) tmp[1]))
  for(i in which(!grepl('-', var.labels))) assign(var.labels[i], matrix(sim$variables[i, ], nrow = 1, ncol = nPeriods))
  
  # Industry Variables
  industry.vars <- unique(sapply(strsplit(grep('-', var.labels, value = T), split = '-', fixed = T), function (tmp) tmp[1]))
  
  for(i in 1 : length(industry.vars))
  {
    assign(industry.vars[i], 
           matrix(sim$variables[grep('-', var.labels)[(10 * (i - 1) + 1) : (10 * i)], ], 
                  nrow = 10, ncol = nPeriods))
  }
  }
#Create row names for TFM matrix
rownames <-c( "Consumption",
              "Investment",
              "Government spending",
              "Export of Area 1",
              "Import of Area 1",
              "[Value added]",
              "Wage bill",
              "Corporate profit",
              "Amortization",
              "Bank profit",
              "CB profit",
              "Income tax revenue",
              "VAT revenue",
              "Tariffs revenue",
              "Interests on deposits",
              "Interests on loans",
              "Interests on Area 1 bills",
              "Interests on Area 2 bills",
              "Change in money stock",
              "Change in advances",
              "Change in deposits",
              "Change in loans",
              "Change in Area 1 bills",
              "Change in Area 2 bills",
              "Change in Area 1 shares",
              "Change in Area 2 shares",
              "Revaluation effects",
              "Total"
)


################################################################################

#Create entries for Z1 TFM

#Create household aggregates
Z1_Households <- c(round(-Z1_c[yr]*Z1_pa[yr], digits = 2),                                                                    
                0,
                0,
                0,
                0,
                0,
                round(Z1_wb[yr], digits = 2),
                round(Z1_div[yr], digits = 2),
                0,
                round(Z1_f_b[yr], digits = 2),
                0,
                round(-Z1_t[yr], digits = 2),
                0,
                0,
                round(Z1_rm[yr-1]*Z1_mh[yr-1], digits = 2),
                round(-Z1_rh[yr-1]*Z1_lh[yr-1], digits = 2),
                round(Z1_rb[yr-1] * Z1_b_s_Z1[yr-1], digits = 2),
                round(Z2_rb[yr-1] * Z1_b_h_Z2[yr-1], digits = 2),
                round(-Z1_h_h[yr]+Z1_h_h[yr-1], digits = 2),
                0,
                round(-Z1_mh[yr]+Z1_mh[yr-1], digits = 2),
                round(Z1_lh[yr]-Z1_lh[yr-1], digits = 2),
                round(-Z1_b_h_Z1[yr]+Z1_b_h_Z1[yr-1], digits = 2),
                round(-Z1_b_h_Z2[yr]+Z1_b_h_Z2[yr-1], digits = 2),
                
                round(-Z1_e_h_Z1[yr]+Z1_e_h_Z1[yr-1], digits = 2),
                round(-Z1_e_h_Z2[yr]+Z1_e_h_Z2[yr-1], digits = 2),
                
                round(Z2_xr[yr]*(Z1_b_s_Z2[yr]+Z1_e_s_Z2[yr])*(Z2_xr[yr]-Z2_xr[yr-1]), digits = 2),
                
                round(-Z1_c[yr]*Z1_pa[yr]+Z1_wb[yr]+Z1_div[yr]+Z1_f_b[yr]-Z1_t[yr]+
                      +Z1_rm[yr-1]*Z1_mh[yr-1]+
                      -Z1_rh[yr-1]*Z1_lh[yr-1]
                      +Z1_rb[yr-1]*Z1_b_s_Z1[yr-1]+
                      +Z2_rb[yr-1]*Z1_b_h_Z2[yr-1]+
                      -Z1_h_h[yr]+Z1_h_h[yr-1]+
                      -Z1_mh[yr]+Z1_mh[yr-1]+
                      +Z1_lh[yr]-Z1_lh[yr-1]
                      -Z1_b_h_Z1[yr]+Z1_b_h_Z1[yr-1]+
                      -Z1_b_h_Z2[yr]+Z1_b_h_Z2[yr-1]+
                      -Z1_e_h_Z1[yr]+Z1_e_h_Z1[yr-1]+
                      -Z1_e_h_Z2[yr]+Z1_e_h_Z2[yr-1]+
                      +Z2_xr[yr]*(Z1_b_s_Z2[yr]+Z1_e_s_Z2[yr])*(Z2_xr[yr]-Z2_xr[yr-1])
                      , digits = 2)
)

#Create table of results
Z1_HouseDataTFM<-as.data.frame(Z1_Households,row.names=rownames)
# kable(Z1_HouseDataTFM)

#Create firms aggregates (current account)
Z1_Firms <- c(round(Z1_c[yr]*Z1_pa[yr], digits = 2),                                                                    
           round(Z1_id[yr]*Z1_pid[yr], digits = 2),
           round(Z1_g[yr]*Z1_pg[yr], digits = 2),
           round(Z1_nex[,yr], digits = 2),
           round(-Z1_nimp[,yr], digits = 2),
           paste("[",round(Z1_yn[yr], digits = 2),"]"),
           round(-Z1_wb[yr], digits = 2),
           round(-Z1_f_f[yr], digits = 2),
           round(-Z1_af[yr], digits = 2),
           0,
           0,
           0,
           round(-Z1_vat_rev[yr], digits = 2),
           round(-Z2_xr[yr]*Z2_tar_rev[yr], digits = 2),
           0,
           round(-Z1_rl[yr-1]*Z1_lf[yr-1], digits = 2),
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           round(Z1_c[yr]*Z1_pa[yr] + Z1_id[yr]*Z1_pid[yr] + Z1_g[yr]*Z1_pg[yr] + Z1_nex[,yr] - Z1_nimp[,yr] +
                 -Z1_wb[yr] - Z1_f_f[yr] - Z1_af[yr] - Z1_rl[yr-1]*Z1_lf[yr-1] - Z1_vat_rev[yr]  - Z2_xr[yr]*Z2_tar_rev[yr], digits = 2)
)

#Create table of results
Z1_FirmsDataTFM<-as.data.frame(Z1_Firms,row.names=rownames)
# kable(Z1_FirmsDataTFM)

#Create capital aggregates
Z1_Capital <- c(0,                                                                    
             round(-Z1_id[yr]*Z1_pid[yr], digits = 2),
             0,
             0,
             0,
             0,
             0,
             0,
             round(Z1_af[yr], digits = 2),
             round(Z1_f_f_u[yr], digits = 2),
             0,
             0,
             0,
             0,
             0,
             0,
             0,
             0,
             0,
             0,
             0,
             round(Z1_lf[yr]-Z1_lf[yr-1], digits = 2),
             0,
             0,
             round(Z1_e_s[yr]-Z1_e_s[yr-1], digits = 2),
             0,
             0,
             round(-Z1_id[yr]*Z1_pid[yr] + Z1_af[yr] + (Z1_lf[yr]-Z1_lf[yr-1]) +
                   +Z1_f_f_u[yr]+Z1_e_s[yr]-Z1_e_s[yr-1], digits = 2)
)

#Create table of results
Z1_CapitalDataTFM<-as.data.frame(Z1_Capital,row.names=rownames)
# kable(Z1_CapitalDataTFM)

#Create government aggregates
Z1_Government <- c( 0,
                 0,
                 round(-Z1_g[yr]*Z1_pg[yr], digits = 2),
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 round(Z1_f_cb[yr], digits = 2),
                 round(Z1_t[yr], digits = 2),
                 round(Z1_vat_rev[yr], digits = 2),
                 round(Z1_tar_rev[yr], digits = 2),
                 0,
                 0,
                 round(-Z1_rb[yr-1]*Z1_b_s[yr-1], digits = 2), 
                 0,
                 0,
                 0,
                 0,
                 0,
                 round(Z1_b_s[yr]-Z1_b_s[yr-1] - Z2_b_s_Z1[yr]*(Z1_xr[yr]-Z1_xr[yr-1]) , digits = 2),
                 0,
                 
                 0, #round(-Z2_b_s_Z1[yr]*(Z1_xr[yr]-Z1_xr[yr-1]) , digits = 2),
                 0,
                 0,
                 round(-Z1_g[yr]*Z1_pg[yr]+Z1_f_cb[yr]+Z1_t[yr]-Z1_rb[yr-1]*Z1_b_s[yr-1]+(Z1_b_s[yr]-Z1_b_s[yr-1]) + Z1_vat_rev[yr] + Z1_tar_rev[yr]
                       #-Z2_b_s_Z1[yr]*(Z1_xr[yr]-Z1_xr[yr-1])
                       , digits = 2)
)

#Create table of results
Z1_GovDataTFM<-as.data.frame(Z1_Government,row.names=rownames)
# kable(Z1_GovDataTFM)

#Create central bank aggregates
Z1_CentralBank <- c( 0,                                                                    
                  0,
                  0,
                  0,
                  0,
                  0,
                  0,
                  0,
                  0,
                  0,
                  round(-Z1_f_cb[yr], digits = 2),
                  0,
                  0,
                  0,
                  0,
                  0,
                  round(Z1_rb[yr-1] * Z1_b_cb[yr-1], digits = 2),
                  round(Z2_rb[yr-1] * Z1_b_cb_s_Z2[yr-1] * Z2_xr[yr], digits = 2),
                  round(Z1_h_s[yr]-Z1_h_s[yr-1], digits = 2),
                  round(-Z1_a_s[yr]+Z1_a_s[yr-1], digits = 2),
                  0,
                  0,
                  round(-Z1_b_cb[yr]+Z1_b_cb[yr-1], digits = 2),
                  round(-Z1_b_cb_d_Z2[yr]+Z1_b_cb_d_Z2[yr-1], digits = 2),
                  
                  0, #round(Z2_xr[yr]*Z1_b_cb_s_Z2[yr]*(Z2_xr[yr]-Z2_xr[yr-1]), digits = 2),
                  0,
                  0,
                  
                  round(-Z1_f_cb[yr]+Z1_rb[yr-1] * Z1_b_cb[yr-1]+Z2_rb[yr-1] * Z1_b_cb_s_Z2[yr-1] * Z2_xr[yr]+
                        +(Z1_h_s[yr]-Z1_h_s[yr-1])
                        -Z1_a_s[yr]+Z1_a_s[yr-1]
                        -(Z1_b_cb[yr]-Z1_b_cb[yr-1])
                        -(Z1_b_cb_d_Z2[yr]-Z1_b_cb_d_Z2[yr-1])
                        #+Z2_xr[yr]*Z1_b_cb_s_Z2[yr]*(Z2_xr[yr]-Z2_xr[yr-1])
                        , digits = 2)
)

#Create table of results
Z1_CBDataTFM<-as.data.frame(Z1_CentralBank,row.names=rownames)
# kable(Z1_CBDataTFM)

#Create banks aggregates
Z1_Banks       <- c( 0,                                                                    
                  0,
                  0,
                  0,
                  0,
                  0,
                  0,
                  0,
                  0,
                  round(-Z1_f_b[yr], digits = 2),
                  0,
                  0,
                  0,
                  0,
                  round(-Z1_rm[yr-1]*Z1_mh[yr-1], digits = 2),
                  round(Z1_rl[yr-1]*Z1_lf[yr-1]+Z1_rh[yr-1]*Z1_lh[yr-1], digits = 2),
                  round(Z1_rb[yr-1]*Z1_b_b[yr-1], digits = 2),
                  0,
                  0,
                  round(Z1_a_d[yr]-Z1_a_d[yr-1], digits = 2),
                  round(Z1_mh[yr]-Z1_mh[yr-1], digits = 2),
                  round(-Z1_ls[yr]+Z1_ls[yr-1] , digits = 2),
                  round(-Z1_b_b[yr]+Z1_b_b[yr-1], digits = 2),
                  0,
                  0,
                  0,
                  0,
                  round(-Z1_f_b[yr] - Z1_rm[yr-1]*Z1_mh[yr-1] + Z1_rl[yr-1]*Z1_lf[yr-1] + Z1_rh[yr-1]*Z1_lh[yr-1] + Z1_rb[yr-1]*Z1_b_b[yr-1] + (Z1_a_d[yr]-Z1_a_d[yr-1])
                          - (-Z1_mh[yr]+Z1_mh[yr-1] + Z1_ls[yr]-Z1_ls[yr-1]  + Z1_b_b[yr]-Z1_b_b[yr-1] )  , digits = 2)
)

#Create table of results
Z1_BanksDataTFM<-as.data.frame(Z1_Banks,row.names=rownames)
# kable(Z1_BanksDataTFM)

################################################################################

#Create entries for Z2 TFM

#Create household aggregates
Z2_Households <- c(round(Z2_xr[yr]*(-Z2_c[yr]*Z2_pa[yr]), digits = 2),                                                                    
                   0,
                   0,
                   0,
                   0,
                   0,
                   round(Z2_xr[yr]*(Z2_wb[yr]), digits = 2),
                   round(Z2_xr[yr]*(Z2_div[yr]), digits = 2),
                   0,
                   round(Z2_xr[yr]*(Z2_f_b[yr]), digits = 2),
                   0,
                   round(Z2_xr[yr]*(-Z2_t[yr]), digits = 2),
                   0,
                   0,
                   round(Z2_xr[yr]*(Z2_rm[yr-1]*Z2_mh[yr-1]), digits = 2),
                   round(-Z2_xr[yr]*(Z2_rh[yr-1]*Z2_lh[yr-1]), digits = 2),
                   round(Z1_rb[yr-1] * Z2_b_s_Z1[yr-1], digits = 2),
                   round(Z2_xr[yr]*Z2_rb[yr-1] * Z2_b_h_Z2[yr-1], digits = 2),
                   round(Z2_xr[yr]*(-Z2_h_h[yr]+Z2_h_h[yr-1]), digits = 2),
                   0,
                   round(Z2_xr[yr]*(-Z2_mh[yr]+Z2_mh[yr-1]), digits = 2),
                   round(Z2_xr[yr]*(Z2_lh[yr]-Z2_lh[yr-1]), digits = 2),
                   round(-Z2_b_s_Z1[yr]+Z2_b_s_Z1[yr-1], digits = 2),
                   
                   round(Z2_xr[yr]*(-Z2_b_h_Z2[yr]+Z2_b_h_Z2[yr-1]), digits = 2),
                   
                   round(-Z2_e_s_Z1[yr]+Z2_e_s_Z1[yr-1], digits = 2),
                   
                   round(Z2_xr[yr]*(-Z2_e_h_Z2[yr]+Z2_e_h_Z2[yr-1]), digits = 2),
                   
                   round((Z2_b_s_Z1[yr]+Z2_e_s_Z1[yr])*(Z1_xr[yr]-Z1_xr[yr-1]), digits = 2),
                   
                   round(Z2_xr[yr]*(-Z2_c[yr]*Z2_pa[yr]+Z2_wb[yr]+Z2_div[yr]+Z2_f_b[yr]-Z2_t[yr]+
                         +Z2_rm[yr-1]*Z2_mh[yr-1]+
                         -Z2_rh[yr-1]*Z2_lh[yr-1]+
                         +Z1_rb[yr-1] * Z2_b_h_Z1[yr-1]+
                         +Z2_rb[yr-1] * Z2_b_h_Z2[yr-1]+
                         -Z2_h_h[yr]+Z2_h_h[yr-1]+
                         -Z2_mh[yr]+Z2_mh[yr-1]+
                         +Z2_lh[yr]-Z2_lh[yr-1]+
                         -Z2_b_h_Z1[yr]+Z2_b_h_Z1[yr-1]+
                         -Z2_b_h_Z2[yr]+Z2_b_h_Z2[yr-1]+
                         -Z2_e_h_Z1[yr]+Z2_e_h_Z1[yr-1]+
                         -Z2_e_h_Z2[yr]+Z2_e_h_Z2[yr-1])+   
                         +(Z2_b_s_Z1[yr]+Z2_e_s_Z1[yr])*(Z1_xr[yr]-Z1_xr[yr-1]), digits = 2)
)

#Create table of results
Z2_HouseDataTFM<-as.data.frame(Z2_Households,row.names=rownames)
# kable(Z2_HouseDataTFM)

#Create firms aggregates (current account)
Z2_Firms <- c(round(Z2_xr[yr]*Z2_c[yr]*Z2_pa[yr], digits = 2),                                                                    
              round(Z2_xr[yr]*Z2_id[yr]*Z2_pid[yr], digits = 2),
              round(Z2_xr[yr]*Z2_g[yr]*Z2_pg[yr], digits = 2),
              round(Z2_xr[yr]*(-Z2_nimp[,yr]), digits = 2),
              round(Z2_xr[yr]*Z2_nex[,yr], digits = 2),
              paste("[",round(Z2_xr[yr]*Z2_yn[yr], digits = 2),"]"),
              round(-Z2_xr[yr]*Z2_wb[yr], digits = 2),
              round(-Z2_xr[yr]*Z2_f_f[yr], digits = 2),
              round(-Z2_xr[yr]*Z2_af[yr], digits = 2),
              0,
              0,
              0,
              round(-Z2_xr[yr]*Z2_vat_rev[yr], digits = 2),
              round(-Z1_tar_rev[yr], digits = 2),               #Note: already expressed in Z1 currency
              0,
              round(-Z2_xr[yr]*Z2_rl[yr-1]*Z2_lf[yr-1], digits = 2),
              0,
              0,
              0,
              0,
              0,
              0,
              0,
              0,
              0,
              0,
              0,
              round(Z2_xr[yr]*(Z2_c[yr]*Z2_pa[yr] + Z2_id[yr]*Z2_pid[yr] + Z2_g[yr]*Z2_pg[yr] + Z2_nex[,yr] - Z2_nimp[,yr] +
                    - Z2_wb[yr] -  Z2_f_f[yr] - Z2_af[yr] - Z2_rl[yr-1]*Z2_lf[yr-1] - Z2_vat_rev[yr] ) - Z1_tar_rev[yr], digits = 2)
)

#Create table of results
Z2_FirmsDataTFM<-as.data.frame(Z2_Firms,row.names=rownames)
# kable(Z2_FirmsDataTFM)

#Create capital aggregates
Z2_Capital <- c(0,                                                                    
                round(-Z2_xr[yr]*Z2_id[yr]*Z2_pid[yr], digits = 2),
                0,
                0,
                0,
                0,
                0,
                0,
                round(Z2_xr[yr]*Z2_af[yr], digits = 2),
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                round(Z2_xr[yr]*(Z2_lf[yr]-Z2_lf[yr-1]), digits = 2),
                0,
                0,
                0,
                round(Z2_xr[yr]*(Z2_e_s[yr]-Z2_e_s[yr-1]), digits = 2),
                0,
                round(Z2_xr[yr]*(-Z2_id[yr]*Z2_pid[yr] + Z2_af[yr] + (Z2_lf[yr]-Z2_lf[yr-1])) +
                     + Z2_xr[yr]*(Z2_e_s[yr]-Z2_e_s[yr-1]), digits = 2)
)

#Create table of results
Z2_CapitalDataTFM<-as.data.frame(Z2_Capital,row.names=rownames)
# kable(Z2_CapitalDataTFM)

#Create government aggregates
Z2_Government <- c( 0,
                    0,
                    round(-Z2_xr[yr]*Z2_g[yr]*Z2_pg[yr], digits = 2),
                    0,
                    0,
                    0,
                    0,
                    0,
                    0,
                    0,
                    round(Z2_xr[yr]*Z2_f_cb[yr], digits = 2),
                    round(Z2_xr[yr]*Z2_t[yr], digits = 2),
                    round(Z2_xr[yr]*Z2_vat_rev[yr], digits = 2),
                    round(Z2_xr[yr]*Z2_tar_rev[yr], digits = 2),
                    0,
                    0,
                    0,
                    round(-Z2_xr[yr]*Z2_rb[yr-1]*Z2_b_s[yr-1], digits = 2), 
                    0,
                    0,
                    0,
                    0,
                    0,
                    round(Z2_xr[yr]*(Z2_b_s[yr]-Z2_b_s[yr-1]) - Z1_b_s_Z2[yr]*(Z2_xr[yr]-Z2_xr[yr-1]) - Z1_b_cb_s_Z2[yr]*(Z2_xr[yr]-Z2_xr[yr-1]) , digits = 2),
                    
                    0, #round(-(Z1_b_s_Z2[yr]*(Z2_xr[yr]-Z2_xr[yr-1]) + Z1_b_cb_s_Z2[yr]*(Z2_xr[yr]-Z2_xr[yr-1])) , digits = 2),
                    0,
                    0,
                    
                    round(Z2_xr[yr]*(-Z2_g[yr]*Z2_pg[yr]+Z2_f_cb[yr]+Z2_t[yr]-Z2_rb[yr-1]*Z2_b_s[yr-1]+(Z2_b_s[yr]-Z2_b_s[yr-1]) + Z2_vat_rev[yr] + Z2_tar_rev[yr])
                          #-(Z1_b_s_Z2[yr]*(Z2_xr[yr]-Z2_xr[yr-1])+Z1_b_cb_s_Z2[yr]*(Z2_xr[yr]-Z2_xr[yr-1]))
                          , digits = 2)
)

#Create table of results
Z2_GovDataTFM<-as.data.frame(Z2_Government,row.names=rownames)
# kable(Z2_GovDataTFM)

#Create central bank aggregates
Z2_CentralBank <- c( 0,                                                                    
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     round(-Z2_xr[yr]*Z2_f_cb[yr], digits = 2),
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     round(Z2_xr[yr]*Z2_rb[yr-1]*Z2_b_cb[yr-1], digits = 2),
                     round(Z2_xr[yr]*(Z2_h_s[yr]-Z2_h_s[yr-1]), digits = 2),
                     round(Z2_xr[yr]*(-Z2_a_s[yr]+Z2_a_s[yr-1]), digits = 2),
                     0,
                     0,
                     0,
                     round(Z2_xr[yr]*(-Z2_b_cb[yr]+Z2_b_cb[yr-1]), digits = 2),
                     0,
                     0,
                     0,
                     round(Z2_xr[yr]*(-Z2_f_cb[yr]+Z2_rb[yr-1]*Z2_b_cb[yr-1]+
                             +(Z2_h_s[yr]-Z2_h_s[yr-1])
                             + Z2_xr[yr]*(-Z2_a_s[yr]+Z2_a_s[yr-1])
                           -(Z2_b_cb[yr]-Z2_b_cb[yr-1])) , digits = 2)
)

#Create table of results
Z2_CBDataTFM<-as.data.frame(Z2_CentralBank,row.names=rownames)
# kable(Z2_CBDataTFM)

#Create banks aggregates
Z2_Banks       <- c( 0,                                                                    
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     0,
                     round(-Z2_xr[yr]*Z2_f_b[yr], digits = 2),
                     0,
                     0,
                     0,
                     0,
                     round(-Z2_xr[yr]*Z2_rm[yr-1]*Z2_mh[yr-1], digits = 2),
                     round(Z2_xr[yr]*(Z2_rl[yr-1]*Z2_lf[yr-1]+Z2_rh[yr-1]*Z2_lh[yr-1]), digits = 2),
                     0,
                     round(Z2_xr[yr]*Z2_rb[yr-1]*Z2_b_b[yr-1], digits = 2),
                     0,
                     round(Z2_xr[yr]*(Z2_a_d[yr]-Z2_a_d[yr-1]), digits = 2),
                     round(Z2_xr[yr]*(Z2_mh[yr]-Z2_mh[yr-1]), digits = 2),
                     round(Z2_xr[yr]*(-Z2_ls[yr]+Z2_ls[yr-1]), digits = 2),
                     0,
                     round(Z2_xr[yr]*(-Z2_b_b[yr]+Z2_b_b[yr-1]), digits = 2),
                     0,
                     0,
                     0,
                     round(Z2_xr[yr]*(-Z2_f_b[yr] - Z2_rm[yr-1]*Z2_mh[yr-1] + Z2_rl[yr-1]*Z2_lf[yr-1] + Z2_rh[yr-1]*Z2_lh[yr-1] + Z2_rb[yr-1]*Z2_b_b[yr-1] +
                           + Z2_xr[yr]*(Z2_a_d[yr]-Z2_a_d[yr-1]) 
                           - (-Z2_mh[yr]+Z2_mh[yr-1] + Z2_ls[yr]-Z2_ls[yr-1] + Z2_b_b[yr]-Z2_b_b[yr-1]) )  , digits = 2)
)

#Create table of results
Z2_BanksDataTFM<-as.data.frame(Z2_Banks,row.names=rownames)
# kable(Z2_BanksDataTFM)

################################################################################

#Create total aggregates
Total_TFM  <- c( 0,                                                                    
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 round( Z1_rb[yr-1] * Z1_b_h_Z1[yr-1]+
                       -Z1_rb[yr-1] * Z1_b_s[yr-1]+
                        Z1_rb[yr-1] * Z1_b_cb[yr-1]+
                        Z1_rb[yr-1] * Z1_b_b[yr-1]+
                        Z1_rb[yr-1] * Z2_b_s_Z1[yr-1], digits=2),
                 
                 round( Z2_rb[yr-1] * Z1_b_s_Z2[yr-1] +
                        Z2_rb[yr-1] * Z1_b_cb_s_Z2[yr-1]+
                        Z2_rb[yr-1] * Z2_b_s_Z2[yr-1]+
                       -Z2_rb[yr-1] * Z2_b_s[yr-1]+
                        Z2_rb[yr-1] * Z2_b_cb[yr-1]+
                        Z2_rb[yr-1] * Z2_b_b[yr-1], digits=2),
                 0,
                 0,
                 0,
                 0,
                 round( -Z1_b_h_Z1[yr] + Z1_b_h_Z1[yr-1] +
                        +Z1_b_s[yr] - Z1_b_s[yr-1] +
                        -Z1_b_cb[yr] + Z1_b_cb[yr-1] +
                        -Z1_b_b[yr] + Z1_b_b[yr-1] +
                        -Z2_b_s_Z1[yr] + Z2_b_s_Z1[yr-1], digits=2),
                 
                 round( -Z2_b_h_Z2[yr] + Z2_b_h_Z2[yr-1] +
                        +Z2_b_s[yr] - Z2_b_s[yr-1] +
                        -Z2_b_cb[yr] + Z2_b_cb[yr-1] +
                        -Z2_b_b[yr] + Z2_b_b[yr-1] +
                        -Z1_b_s_Z2[yr] + Z1_b_s_Z2[yr-1] +
                        -Z1_b_cb_s_Z2[yr] + Z1_b_cb_s_Z2[yr-1], digits=2),
                 
                 round( -Z1_e_h_Z1[yr] + Z1_e_h_Z1[yr-1] +
                        +Z1_e_s[yr] - Z1_e_s[yr-1] +
                        -Z2_e_s_Z1[yr] + Z2_e_s_Z1[yr-1], digits=2),
                 
                 round( -Z2_e_h_Z2[yr] + Z2_e_h_Z2[yr-1] +
                        +Z2_e_s[yr] - Z2_e_s[yr-1] +
                        -Z1_e_s_Z2[yr] + Z1_e_s_Z2[yr-1], digits=2),
                 
                 round( Z2_xr[yr]*Z1_b_s_Z2[yr]*(Z2_xr[yr]-Z2_xr[yr-1])+
                        Z2_b_s_Z1[yr]*(Z1_xr[yr]-Z1_xr[yr-1]), digits = 2),
                 
                 round( Z1_rb[yr-1] * Z1_b_h_Z1[yr-1]+
                          -Z1_rb[yr-1] * Z1_b_s[yr-1]+
                          Z1_rb[yr-1] * Z1_b_cb[yr-1]+
                          Z1_rb[yr-1] * Z1_b_b[yr-1]+
                          Z1_rb[yr-1] * Z2_b_s_Z1[yr-1]+
                          Z2_rb[yr-1] * Z1_b_s_Z2[yr-1] +
                          Z2_rb[yr-1] * Z1_b_cb_s_Z2[yr-1]+
                          Z2_rb[yr-1] * Z2_b_s_Z2[yr-1]+
                          -Z2_rb[yr-1] * Z2_b_s[yr-1]+
                          Z2_rb[yr-1] * Z2_b_cb[yr-1]+
                          Z2_rb[yr-1] * Z2_b_b[yr-1]
                        -Z1_b_h_Z1[yr] + Z1_b_h_Z1[yr-1] +
                          +Z1_b_s[yr] - Z1_b_s[yr-1] +
                          -Z1_b_cb[yr] + Z1_b_cb[yr-1] +
                          -Z1_b_b[yr] + Z1_b_b[yr-1] +
                          -Z2_b_s_Z1[yr] + Z2_b_s_Z1[yr-1]
                        -Z2_b_h_Z2[yr] + Z2_b_h_Z2[yr-1] +
                          +Z2_b_s[yr] - Z2_b_s[yr-1] +
                          -Z2_b_cb[yr] + Z2_b_cb[yr-1] +
                          -Z2_b_b[yr] + Z2_b_b[yr-1] +
                          -Z1_b_s_Z2[yr] + Z1_b_s_Z2[yr-1] +
                          -Z1_b_cb_s_Z2[yr] + Z1_b_cb_s_Z2[yr-1] +
                          +Z2_xr[yr]*Z1_b_s_Z2[yr]*(Z2_xr[yr]-Z2_xr[yr-1])+
                          +Z2_b_s_Z1[yr]*(Z1_xr[yr]-Z1_xr[yr-1])
                        
                        -Z1_e_h_Z1[yr] + Z1_e_h_Z1[yr-1] +
                          +Z1_e_s[yr] - Z1_e_s[yr-1] +
                          -Z2_e_s_Z1[yr] + Z2_e_s_Z1[yr-1]
                        
                        -Z2_e_h_Z2[yr] + Z2_e_h_Z2[yr-1] +
                          +Z2_e_s[yr] - Z2_e_s[yr-1] +
                          -Z1_e_s_Z2[yr] + Z1_e_s_Z2[yr-1]
                        
                        , digits=2)
                        )

#Create table of results
TotDataTFM<-as.data.frame(Total_TFM,row.names=rownames)
# kable(TotDataTFM)

#Create xr column
xr <- c( round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         round(Z1_xr[yr], digits=4),
         paste("")
)        


#Create table of results
xrTFM<-as.data.frame(xr,row.names=rownames)
# kable(xrTFM)

#Create TFM matrix
TFM_Matrix<-cbind(Z1_HouseDataTFM,Z1_FirmsDataTFM,Z1_CapitalDataTFM,Z1_GovDataTFM,Z1_BanksDataTFM,Z1_CBDataTFM,xr,Z2_HouseDataTFM,Z2_FirmsDataTFM,Z2_CapitalDataTFM,Z2_GovDataTFM,Z2_BanksDataTFM,Z2_CBDataTFM,TotDataTFM)
# kable(TFM_Matrix) #Unload # kableExtra to use this

################################################################################
################################################################################
################################################################################

#Create html version of tables

# #Create titles
# if (shock==1){title <- paste("increase in propensity to import of Area 2")}
# if (shock==2){title <- paste("devaluation of Area 1 currency")}
# if (shock==3){title <- paste("increase in propensity to import of Area 2 and quasi-floating exchange rate")}
# if (shock==4){title <- paste("CE innovation in Area 1 with fixed exchange rate")}
# if (shock==5){title <- paste("CE innovation in Area 1 with floating exchange rate")}
# caption1 <- paste("Table 3. Balance sheet (OPEN) in period", yr) #"after",title
# caption2 <- paste("Table 2. Transactions-flow matrix (OPEN) in period ",yr) #,"after",title

return(TFM_Matrix)
}

#See MRIO Table Code for IO matrix
MRIO <- function(res, yr)
{
  sim <- res$simulation
  # Aggregate Variables
  var.labels <- rownames(sim$variables)
  variable.names <- unique(sapply(strsplit(var.labels, split = '-', fixed = T), function (tmp) tmp[1]))
  for(i in which(!grepl('-', var.labels))) assign(var.labels[i], matrix(sim$variables[i, ], nrow = 1, ncol = nPeriods))
  
  # Industry Variables
  industry.vars <- unique(sapply(strsplit(grep('-', var.labels, value = T), split = '-', fixed = T), function (tmp) tmp[1]))
  
  for(i in 1 : length(industry.vars))
  {
    assign(industry.vars[i], 
           matrix(sim$variables[grep('-', var.labels)[(10 * (i - 1) + 1) : (10 * i)], ], 
                  nrow = 10, ncol = nPeriods))
  }
  
  rownames <-c( "Agriculture in Area 1",
                "Manufacturing in Area 1",
                "Services in Area 1",
                "Waste manag. in Area 1",
                "Recycling in Area 1",
                "Agriculture in Area 2",
                "Manufacturing in Area 2",
                "Services in Area 2",
                "Waste manag. in Area 2",
                "Recycling in Area 2",
                "Value added",
                " ~ Compensation of employees",
                " ~ G.O. surplus and mixed incomes",
                "Taxes on production",
                "Output [row sum]",
                "Output"
  )
  
  ################################################################################
  
  #Define global real output
  #x=matrix(data=c(0,0,0,0,0,0,0,0),nrow=8,ncol=nPeriods)
  #x[,] = Z1_x[,] + Z2_x[,]
  
  #Define global real demand
  #d=matrix(data=c(0,0,0,0,0,0,0,0),nrow=8,ncol=nPeriods)
  #d[,] = Z1_d[,] + Z2_d[,]
  
  ################################################################################
  
  # AREA 1
  
  #Create manufacturing industry in Area 1
  Z1_Agri <- c(round(x[1,yr]*a11[,yr]*Z1_p[1,yr], digits = 2),                                                                    
               round(x[1,yr]*a21[,yr]*Z1_p[2,yr], digits = 2),
               round(x[1,yr]*a31[,yr]*Z1_p[3,yr], digits = 2),
               round(x[1,yr]*a41[,yr]*Z1_p[4,yr], digits = 2),
               round(x[1,yr]*a51[,yr]*Z1_p[5,yr], digits = 2),                                                                    
               round(x[1,yr]*a61[,yr]*Z2_xr[,yr]*Z2_p[6,yr], digits = 2),
               round(x[1,yr]*a71[,yr]*Z2_xr[,yr]*Z2_p[7,yr], digits = 2),
               round(x[1,yr]*a81[,yr]*Z2_xr[,yr]*Z2_p[8,yr], digits = 2),
               round(x[1,yr]*a91[,yr]*Z2_xr[,yr]*Z2_p[9,yr], digits = 2),
               round(x[1,yr]*a101[,yr]*Z2_xr[,yr]*Z2_p[10,yr], digits = 2),
               "",
               round(Z1_wb1[,yr],digits = 2),
               round(Z1_va1[,yr]-Z1_wb1[,yr],digits = 2),
               0,
               round(x[1,yr]*a11[,yr]*Z1_p[1,yr]+
                       x[1,yr]*a21[,yr]*Z1_p[2,yr]+
                       x[1,yr]*a31[,yr]*Z1_p[3,yr]+
                       x[1,yr]*a41[,yr]*Z1_p[4,yr]+
                       x[1,yr]*a51[,yr]*Z1_p[5,yr]+
                       x[1,yr]*a61[,yr]*Z2_xr[,yr]*Z2_p[6,yr]+
                       x[1,yr]*a71[,yr]*Z2_xr[,yr]*Z2_p[7,yr]+
                       x[1,yr]*a81[,yr]*Z2_xr[,yr]*Z2_p[8,yr]+
                       x[1,yr]*a91[,yr]*Z2_xr[,yr]*Z2_p[9,yr]+
                       x[1,yr]*a101[,yr]*Z2_xr[,yr]*Z2_p[10,yr]+
                       Z1_va1[,yr] , digits = 2),
               round(x[1,yr]*Z1_p[1,yr] , digits = 2)
  )
  
  x[6,i]*a61[,i]+ x[6,i]*a71[,i]+ x[6,i]*a81[,i]+ x[6,i]*a91[,i]+ x[6,i]*a101[,i]+
    x[7,i]*a62[,i]+ x[7,i]*a72[,i]+ x[7,i]*a82[,i]+ x[7,i]*a92[,i]+ x[7,i]*a102[,i]+ 
    x[8,i]*a63[,i]+ x[8,i]*a73[,i]+ x[8,i]*a83[,i]+ x[8,i]*a93[,i]+ x[8,i]*a103[,i]+
    x[9,i]*a64[,i]+ x[9,i]*a74[,i]+ x[9,i]*a84[,i]+ x[9,i]*a94[,i]+ x[9,i]*a104[,i]+
    x[10,i]*a65[,i]+ x[10,i]*a75[,i]+ x[10,i]*a85[,i]+ x[10,i]*a95[,i]+ x[10,i]*a105[,i]
  
  
  #Create table of results
  IO_Z1_Agri<-as.data.frame(Z1_Agri,row.names=rownames)
  kable(IO_Z1_Agri)
  
  #Create agriculture in Area 1
  Z1_Manufacturing <- c(round(x[2,yr]*a12[,yr]*Z1_p[1,yr], digits = 2),                                                                    
                        round(x[2,yr]*a22[,yr]*Z1_p[2,yr], digits = 2),
                        round(x[2,yr]*a32[,yr]*Z1_p[3,yr], digits = 2),
                        round(x[2,yr]*a42[,yr]*Z1_p[4,yr], digits = 2),
                        round(x[2,yr]*a52[,yr]*Z1_p[5,yr], digits = 2),
                        round(x[2,yr]*a62[,yr]*Z2_xr[,yr]*Z2_p[6,yr], digits = 2),
                        round(x[2,yr]*a72[,yr]*Z2_xr[,yr]*Z2_p[7,yr], digits = 2),
                        round(x[2,yr]*a82[,yr]*Z2_xr[,yr]*Z2_p[8,yr], digits = 2),
                        round(x[2,yr]*a92[,yr]*Z2_xr[,yr]*Z2_p[9,yr], digits = 2), 
                        round(x[2,yr]*a102[,yr]*Z2_xr[,yr]*Z2_p[10,yr], digits = 2), 
                        "",
                        round(Z1_wb2[,yr],digits = 2),
                        round(Z1_va2[,yr]-Z1_wb2[,yr],digits = 2),
                        0,
                        round(x[2,yr]*a12[,yr]*Z1_p[1,yr]+
                                x[2,yr]*a22[,yr]*Z1_p[2,yr]+
                                x[2,yr]*a32[,yr]*Z1_p[3,yr]+
                                x[2,yr]*a42[,yr]*Z1_p[4,yr]+
                                x[2,yr]*a52[,yr]*Z1_p[5,yr]+
                                x[2,yr]*a62[,yr]*Z2_xr[,yr]*Z2_p[6,yr]+
                                x[2,yr]*a72[,yr]*Z2_xr[,yr]*Z2_p[7,yr]+
                                x[2,yr]*a82[,yr]*Z2_xr[,yr]*Z2_p[8,yr]+
                                x[2,yr]*a92[,yr]*Z2_xr[,yr]*Z2_p[9,yr]+
                                x[2,yr]*a102[,yr]*Z2_xr[,yr]*Z2_p[10,yr]+
                                Z1_va2[,yr] , digits = 2),
                        round(x[2,yr]*Z1_p[2,yr] , digits = 2)
  )
  
  
  #Create table of results
  IO_Z1_Manufacturing<-as.data.frame(Z1_Manufacturing,row.names=rownames)
  kable(IO_Z1_Manufacturing)
  
  
  #Create services in Area 1
  Z1_Serv       <- c(round(x[3,yr]*a13[,yr]*Z1_p[1,yr], digits = 2),                                                                    
                     round(x[3,yr]*a23[,yr]*Z1_p[2,yr], digits = 2),
                     round(x[3,yr]*a33[,yr]*Z1_p[3,yr], digits = 2),
                     round(x[3,yr]*a43[,yr]*Z1_p[4,yr], digits = 2),
                     round(x[3,yr]*a53[,yr]*Z1_p[5,yr], digits = 2),
                     round(x[3,yr]*a63[,yr]*Z2_xr[,yr]*Z2_p[6,yr], digits = 2),
                     round(x[3,yr]*a73[,yr]*Z2_xr[,yr]*Z2_p[7,yr], digits = 2),
                     round(x[3,yr]*a83[,yr]*Z2_xr[,yr]*Z2_p[8,yr], digits = 2),
                     round(x[3,yr]*a93[,yr]*Z2_xr[,yr]*Z2_p[9,yr], digits = 2),
                     round(x[3,yr]*a103[,yr]*Z2_xr[,yr]*Z2_p[10,yr], digits = 2),                                                                    
                     "",
                     round(Z1_wb3[,yr],digits = 2),
                     round(Z1_va3[,yr]-Z1_wb3[,yr],digits = 2),
                     0,
                     round(x[3,yr]*a13[,yr]*Z1_p[1,yr]+
                             x[3,yr]*a23[,yr]*Z1_p[2,yr]+
                             x[3,yr]*a33[,yr]*Z1_p[3,yr]+
                             x[3,yr]*a43[,yr]*Z1_p[4,yr]+
                             x[3,yr]*a53[,yr]*Z1_p[5,yr]+
                             x[3,yr]*a63[,yr]*Z2_xr[,yr]*Z2_p[6,yr]+
                             x[3,yr]*a73[,yr]*Z2_xr[,yr]*Z2_p[7,yr]+
                             x[3,yr]*a83[,yr]*Z2_xr[,yr]*Z2_p[8,yr]+
                             x[3,yr]*a93[,yr]*Z2_xr[,yr]*Z2_p[9,yr]+
                             x[3,yr]*a103[,yr]*Z2_xr[,yr]*Z2_p[10,yr]+
                             Z1_va3[,yr] , digits = 2),
                     round(x[3,yr]*Z1_p[3,yr] , digits = 2)
  )
  
  #Create table of results
  IO_Z1_Serv<-as.data.frame(Z1_Serv,row.names=rownames)
  kable(IO_Z1_Serv)
  
  
  #Create waste management industry in Area 1
  Z1_Waste   <- c(round(x[4,yr]*a14[,yr]*Z1_p[1,yr], digits = 2),                                                                    
                  round(x[4,yr]*a24[,yr]*Z1_p[2,yr], digits = 2),
                  round(x[4,yr]*a34[,yr]*Z1_p[3,yr], digits = 2),
                  round(x[4,yr]*a44[,yr]*Z1_p[4,yr], digits = 2),
                  round(x[4,yr]*a54[,yr]*Z1_p[5,yr], digits = 2),
                  round(x[4,yr]*a64[,yr]*Z2_xr[,yr]*Z2_p[6,yr], digits = 2),
                  round(x[4,yr]*a74[,yr]*Z2_xr[,yr]*Z2_p[7,yr], digits = 2),
                  round(x[4,yr]*a84[,yr]*Z2_xr[,yr]*Z2_p[8,yr], digits = 2),
                  round(x[4,yr]*a94[,yr]*Z2_xr[,yr]*Z2_p[9,yr], digits = 2),
                  round(x[4,yr]*a104[,yr]*Z2_xr[,yr]*Z2_p[10,yr], digits = 2),                                                                    
                  "",
                  round(Z1_wb4[,yr],digits = 2),
                  round(Z1_va4[,yr]-Z1_wb4[,yr],digits = 2),
                  0,
                  round(x[4,yr]*a14[,yr]*Z1_p[1,yr]+
                          x[4,yr]*a24[,yr]*Z1_p[2,yr]+
                          x[4,yr]*a34[,yr]*Z1_p[3,yr]+
                          x[4,yr]*a44[,yr]*Z1_p[4,yr]+
                          x[4,yr]*a54[,yr]*Z1_p[5,yr]+
                          x[4,yr]*a64[,yr]*Z2_xr[,yr]*Z2_p[6,yr]+
                          x[4,yr]*a74[,yr]*Z2_xr[,yr]*Z2_p[7,yr]+
                          x[4,yr]*a84[,yr]*Z2_xr[,yr]*Z2_p[8,yr]+
                          x[4,yr]*a94[,yr]*Z2_xr[,yr]*Z2_p[9,yr]+
                          x[4,yr]*a104[,yr]*Z2_xr[,yr]*Z2_p[10,yr]+
                          Z1_va4[,yr] , digits = 2),
                  round(x[4,yr]*Z1_p[4,yr] , digits = 2)
  )
  
  #Create table of results
  IO_Z1_Waste<-as.data.frame(Z1_Waste,row.names=rownames)
  kable(IO_Z1_Waste)
  
  
  #Create recycling industry in Area 1
  Z1_Recyc   <- c(round(x[5,yr]*a15[,yr]*Z1_p[1,yr], digits = 2),                                                                    
                  round(x[5,yr]*a25[,yr]*Z1_p[2,yr], digits = 2),
                  round(x[5,yr]*a35[,yr]*Z1_p[3,yr], digits = 2),
                  round(x[5,yr]*a45[,yr]*Z1_p[4,yr], digits = 2),
                  round(x[5,yr]*a55[,yr]*Z1_p[5,yr], digits = 2),
                  round(x[5,yr]*a65[,yr]*Z2_xr[,yr]*Z2_p[6,yr], digits = 2),
                  round(x[5,yr]*a75[,yr]*Z2_xr[,yr]*Z2_p[7,yr], digits = 2),
                  round(x[5,yr]*a85[,yr]*Z2_xr[,yr]*Z2_p[8,yr], digits = 2),
                  round(x[5,yr]*a95[,yr]*Z2_xr[,yr]*Z2_p[9,yr], digits = 2),
                  round(x[5,yr]*a105[,yr]*Z2_xr[,yr]*Z2_p[10,yr], digits = 2),                                                                    
                  "",
                  round(Z1_wb5[,yr],digits = 2),
                  round(Z1_va5[,yr]-Z1_wb5[,yr],digits = 2),
                  0,
                  round(x[5,yr]*a15[,yr]*Z1_p[1,yr]+
                          x[5,yr]*a25[,yr]*Z1_p[2,yr]+
                          x[5,yr]*a35[,yr]*Z1_p[3,yr]+
                          x[5,yr]*a45[,yr]*Z1_p[4,yr]+
                          x[5,yr]*a55[,yr]*Z1_p[5,yr]+
                          x[5,yr]*a65[,yr]*Z2_xr[,yr]*Z2_p[6,yr]+
                          x[5,yr]*a75[,yr]*Z2_xr[,yr]*Z2_p[7,yr]+
                          x[5,yr]*a85[,yr]*Z2_xr[,yr]*Z2_p[8,yr]+
                          x[5,yr]*a95[,yr]*Z2_xr[,yr]*Z2_p[9,yr]+
                          x[5,yr]*a105[,yr]*Z2_xr[,yr]*Z2_p[10,yr]+
                          Z1_va5[,yr] , digits = 2),
                  round(x[5,yr]*Z1_p[5,yr] , digits = 2)
  )
  
  #Create table of results
  IO_Z1_Recyc<-as.data.frame(Z1_Recyc,row.names=rownames)
  kable(IO_Z1_Recyc)
  
  
  ################################################################################
  
  # AREA 2
  
  #Create manufacturing industry in Area 2
  Z2_Agri <- c(round(x[6,yr]*a16[,yr]*Z1_p[1,yr], digits = 2),                                                                    
               round(x[6,yr]*a26[,yr]*Z1_p[2,yr], digits = 2),
               round(x[6,yr]*a36[,yr]*Z1_p[3,yr], digits = 2),
               round(x[6,yr]*a46[,yr]*Z1_p[4,yr], digits = 2),
               round(x[6,yr]*a56[,yr]*Z1_p[5,yr], digits = 2),
               round(x[6,yr]*a66[,yr]*Z2_xr[,yr]*Z2_p[6,yr], digits = 2),
               round(x[6,yr]*a76[,yr]*Z2_xr[,yr]*Z2_p[7,yr], digits = 2),
               round(x[6,yr]*a86[,yr]*Z2_xr[,yr]*Z2_p[8,yr], digits = 2),
               round(x[6,yr]*a96[,yr]*Z2_xr[,yr]*Z2_p[9,yr], digits = 2),                                                                    
               round(x[6,yr]*a106[,yr]*Z2_xr[,yr]*Z2_p[10,yr], digits = 2),                                                                    
               "",
               round(Z2_wb1[,yr]*Z2_xr[,yr],digits = 2),
               round((Z2_va1[,yr]-Z2_wb1[,yr])*Z2_xr[,yr],digits = 2),
               0,
               round(x[6,yr]*a16[,yr]*Z1_p[1,yr]+
                       x[6,yr]*a26[,yr]*Z1_p[2,yr]+
                       x[6,yr]*a36[,yr]*Z1_p[3,yr]+
                       x[6,yr]*a46[,yr]*Z1_p[4,yr]+
                       x[6,yr]*a56[,yr]*Z1_p[5,yr]+
                       x[6,yr]*a66[,yr]*Z2_xr[,yr]*Z2_p[6,yr]+
                       x[6,yr]*a76[,yr]*Z2_xr[,yr]*Z2_p[7,yr]+
                       x[6,yr]*a86[,yr]*Z2_xr[,yr]*Z2_p[8,yr]+
                       x[6,yr]*a96[,yr]*Z2_xr[,yr]*Z2_p[9,yr]+
                       x[6,yr]*a106[,yr]*Z2_xr[,yr]*Z2_p[10,yr]+
                       Z2_va1[,yr]*Z2_xr[,yr] , digits = 2),   
               round(x[6,yr]*Z2_p[6,yr]*Z2_xr[,yr], digits = 2)
  )
  
  
  #Create table of results
  IO_Z2_Agri<-as.data.frame(Z2_Agri,row.names=rownames)
  kable(IO_Z2_Agri)
  
  #Create agriculture in Area 2
  Z2_Manufacturing  <- c(round(x[7,yr]*a17[,yr]*Z1_p[1,yr], digits = 2),                                                                    
                         round(x[7,yr]*a27[,yr]*Z1_p[2,yr], digits = 2),
                         round(x[7,yr]*a37[,yr]*Z1_p[3,yr], digits = 2),
                         round(x[7,yr]*a47[,yr]*Z1_p[4,yr], digits = 2),
                         round(x[7,yr]*a57[,yr]*Z1_p[5,yr], digits = 2),                                                                    
                         round(x[7,yr]*a67[,yr]*Z2_xr[,yr]*Z2_p[6,yr], digits = 2),
                         round(x[7,yr]*a77[,yr]*Z2_xr[,yr]*Z2_p[7,yr], digits = 2),
                         round(x[7,yr]*a87[,yr]*Z2_xr[,yr]*Z2_p[8,yr], digits = 2),
                         round(x[7,yr]*a97[,yr]*Z2_xr[,yr]*Z2_p[9,yr], digits = 2),
                         round(x[7,yr]*a107[,yr]*Z2_xr[,yr]*Z2_p[10,yr], digits = 2),
                         "",
                         round(Z2_wb2[,yr]*Z2_xr[,yr],digits = 2),
                         round((Z2_va2[,yr]-Z2_wb2[,yr])*Z2_xr[,yr],digits = 2),
                         0,
                         round(x[7,yr]*a17[,yr]*Z1_p[1,yr]+
                                 x[7,yr]*a27[,yr]*Z1_p[2,yr]+
                                 x[7,yr]*a37[,yr]*Z1_p[3,yr]+
                                 x[7,yr]*a47[,yr]*Z1_p[4,yr]+
                                 x[7,yr]*a57[,yr]*Z1_p[5,yr]+
                                 x[7,yr]*a67[,yr]*Z2_xr[,yr]*Z2_p[6,yr]+
                                 x[7,yr]*a77[,yr]*Z2_xr[,yr]*Z2_p[7,yr]+
                                 x[7,yr]*a87[,yr]*Z2_xr[,yr]*Z2_p[8,yr]+
                                 x[7,yr]*a97[,yr]*Z2_xr[,yr]*Z2_p[9,yr]+
                                 x[7,yr]*a107[,yr]*Z2_xr[,yr]*Z2_p[10,yr]+
                                 Z2_va2[,yr]*Z2_xr[,yr] , digits = 2),    
                         round(x[7,yr]*Z2_p[7,yr]*Z2_xr[,yr] , digits = 2)
  )
  
  #Create table of results
  IO_Z2_Manufacturing<-as.data.frame(Z2_Manufacturing,row.names=rownames)
  kable(IO_Z2_Manufacturing)
  
  #Create services in Area 2
  Z2_Serv       <- c(round(x[8,yr]*a18[,yr]*Z1_p[1,yr], digits = 2),                                                                    
                     round(x[8,yr]*a28[,yr]*Z1_p[2,yr], digits = 2),
                     round(x[8,yr]*a38[,yr]*Z1_p[3,yr], digits = 2),
                     round(x[8,yr]*a48[,yr]*Z1_p[4,yr], digits = 2),
                     round(x[8,yr]*a58[,yr]*Z2_p[5,yr], digits = 2),                                                                    
                     round(x[8,yr]*a68[,yr]*Z2_xr[,yr]*Z2_p[6,yr], digits = 2),
                     round(x[8,yr]*a78[,yr]*Z2_xr[,yr]*Z2_p[7,yr], digits = 2),
                     round(x[8,yr]*a88[,yr]*Z2_xr[,yr]*Z2_p[8,yr], digits = 2),
                     round(x[8,yr]*a98[,yr]*Z2_xr[,yr]*Z2_p[9,yr], digits = 2),
                     round(x[8,yr]*a108[,yr]*Z2_xr[,yr]*Z2_p[10,yr], digits = 2),
                     "",
                     round(Z2_wb3[,yr]*Z2_xr[,yr],digits = 2),
                     round((Z2_va3[,yr]-Z2_wb3[,yr])*Z2_xr[,yr],digits = 2),
                     0,
                     round(x[8,yr]*a18[,yr]*Z1_p[1,yr]+
                             x[8,yr]*a28[,yr]*Z1_p[2,yr]+
                             x[8,yr]*a38[,yr]*Z1_p[3,yr]+
                             x[8,yr]*a48[,yr]*Z1_p[4,yr]+
                             x[8,yr]*a58[,yr]*Z1_p[5,yr]+
                             x[8,yr]*a68[,yr]*Z2_xr[,yr]*Z2_p[6,yr]+
                             x[8,yr]*a78[,yr]*Z2_xr[,yr]*Z2_p[7,yr]+
                             x[8,yr]*a88[,yr]*Z2_xr[,yr]*Z2_p[8,yr]+
                             x[8,yr]*a98[,yr]*Z2_xr[,yr]*Z2_p[9,yr]+
                             x[8,yr]*a108[,yr]*Z2_xr[,yr]*Z2_p[10,yr]+
                             Z2_va3[,yr]*Z2_xr[,yr] , digits = 2), 
                     round(x[8,yr]*Z2_p[8,yr]*Z2_xr[,yr] , digits = 2)
  )
  
  #Create table of results
  IO_Z2_Serv<-as.data.frame(Z2_Serv,row.names=rownames)
  kable(IO_Z2_Serv)
  
  
  #Create waste management industry in Area 2
  Z2_Waste   <- c(round(x[9,yr]*a19[,yr]*Z1_p[1,yr], digits = 2),                                                                    
                  round(x[9,yr]*a29[,yr]*Z1_p[2,yr], digits = 2),
                  round(x[9,yr]*a39[,yr]*Z1_p[3,yr], digits = 2),
                  round(x[9,yr]*a49[,yr]*Z1_p[4,yr], digits = 2),
                  round(x[9,yr]*a59[,yr]*Z2_p[5,yr], digits = 2),                                                                    
                  round(x[9,yr]*a69[,yr]*Z2_xr[,yr]*Z2_p[6,yr], digits = 2),
                  round(x[9,yr]*a79[,yr]*Z2_xr[,yr]*Z2_p[7,yr], digits = 2),
                  round(x[9,yr]*a89[,yr]*Z2_xr[,yr]*Z2_p[8,yr], digits = 2),
                  round(x[9,yr]*a99[,yr]*Z2_xr[,yr]*Z2_p[9,yr], digits = 2),
                  round(x[9,yr]*a109[,yr]*Z2_xr[,yr]*Z2_p[10,yr], digits = 2),
                  "",
                  round(Z2_wb4[,yr]*Z2_xr[,yr],digits = 2),
                  round((Z2_va4[,yr]-Z2_wb4[,yr])*Z2_xr[,yr],digits = 2),
                  0,
                  round(x[9,yr]*a19[,yr]*Z1_p[1,yr]+
                          x[9,yr]*a29[,yr]*Z1_p[2,yr]+
                          x[9,yr]*a39[,yr]*Z1_p[3,yr]+
                          x[9,yr]*a49[,yr]*Z1_p[4,yr]+
                          x[9,yr]*a59[,yr]*Z1_p[5,yr]+
                          x[9,yr]*a69[,yr]*Z2_xr[,yr]*Z2_p[6,yr]+
                          x[9,yr]*a79[,yr]*Z2_xr[,yr]*Z2_p[7,yr]+
                          x[9,yr]*a89[,yr]*Z2_xr[,yr]*Z2_p[8,yr]+
                          x[9,yr]*a99[,yr]*Z2_xr[,yr]*Z2_p[9,yr]+
                          x[9,yr]*a109[,yr]*Z2_xr[,yr]*Z2_p[10,yr]+
                          Z2_va4[,yr]*Z2_xr[,yr] , digits = 2), 
                  round(x[9,yr]*Z2_p[9,yr]*Z2_xr[,yr] , digits = 2)
  )
  
  #Create table of results
  IO_Z2_Waste<-as.data.frame(Z2_Waste,row.names=rownames)
  kable(IO_Z2_Waste)
  
  
  #Create recycling industry in Area 2
  Z2_Recyc   <- c(round(x[10,yr]*a110[,yr]*Z1_p[1,yr], digits = 2),                                                                    
                  round(x[10,yr]*a210[,yr]*Z1_p[2,yr], digits = 2),
                  round(x[10,yr]*a310[,yr]*Z1_p[3,yr], digits = 2),
                  round(x[10,yr]*a410[,yr]*Z1_p[4,yr], digits = 2),
                  round(x[10,yr]*a510[,yr]*Z1_p[5,yr], digits = 2),                                                                    
                  round(x[10,yr]*a610[,yr]*Z2_xr[,yr]*Z2_p[6,yr], digits = 2),
                  round(x[10,yr]*a710[,yr]*Z2_xr[,yr]*Z2_p[7,yr], digits = 2),
                  round(x[10,yr]*a810[,yr]*Z2_xr[,yr]*Z2_p[8,yr], digits = 2),
                  round(x[10,yr]*a910[,yr]*Z2_xr[,yr]*Z2_p[9,yr], digits = 2),
                  round(x[10,yr]*a1010[,yr]*Z2_xr[,yr]*Z2_p[10,yr], digits = 2),
                  "",
                  round(Z2_wb5[,yr]*Z2_xr[,yr],digits = 2),
                  round((Z2_va5[,yr]-Z2_wb5[,yr])*Z2_xr[,yr],digits = 2),
                  0,
                  round(x[10,yr]*a110[,yr]*Z1_p[1,yr]+
                          x[10,yr]*a210[,yr]*Z1_p[2,yr]+
                          x[10,yr]*a310[,yr]*Z1_p[3,yr]+
                          x[10,yr]*a410[,yr]*Z1_p[4,yr]+
                          x[10,yr]*a510[,yr]*Z1_p[5,yr]+
                          x[10,yr]*a610[,yr]*Z2_xr[,yr]*Z2_p[6,yr]+
                          x[10,yr]*a710[,yr]*Z2_xr[,yr]*Z2_p[7,yr]+
                          x[10,yr]*a810[,yr]*Z2_xr[,yr]*Z2_p[8,yr]+
                          x[10,yr]*a910[,yr]*Z2_xr[,yr]*Z2_p[9,yr]+
                          x[10,yr]*a1010[,yr]*Z2_xr[,yr]*Z2_p[10,yr]+
                          Z2_va5[,yr]*Z2_xr[,yr] , digits = 2), 
                  round(x[10,yr]*Z2_p[10,yr]*Z2_xr[,yr] , digits = 2)
  )
  
  #Create table of results
  IO_Z2_Recyc<-as.data.frame(Z2_Recyc,row.names=rownames)
  kable(IO_Z2_Recyc)
  
  ################################################################################
  
  
  #Create final demand column 
  Z1_D_shares<-as.matrix(Z1_d[,yr]/sum(Z1_d[,yr]))
  Z1_p_df<-t(Z1_D_shares)%*%Z1_p[,yr]
  
  Z1_F_Demand   <- c(round(Z1_d[1,yr]*Z1_p[1,yr] , digits = 2),                                                                    
                     round(Z1_d[2,yr]*Z1_p[2,yr] , digits = 2),
                     round(Z1_d[3,yr]*Z1_p[3,yr] , digits = 2),
                     round(Z1_d[4,yr]*Z1_p[4,yr] , digits = 2),
                     round(Z1_d[5,yr]*Z1_p[5,yr] , digits = 2),                                                                    
                     round(Z1_d[6,yr]*Z2_p[6,yr]*Z2_xr[,yr] , digits = 2),
                     round(Z1_d[7,yr]*Z2_p[7,yr]*Z2_xr[,yr] , digits = 2),
                     round(Z1_d[8,yr]*Z2_p[8,yr]*Z2_xr[,yr] , digits = 2),
                     round(Z1_d[9,yr]*Z2_p[9,yr]*Z2_xr[,yr] , digits = 2),
                     round(Z1_d[10,yr]*Z2_p[10,yr]*Z2_xr[,yr] , digits = 2),
                     "",
                     "",
                     "",
                     "",
                     round(sum(Z1_d[,yr])*Z1_p_df,digits=2),
                     ""
  )
  
  #Create table of results
  IO_Z1_F_Demand<-as.data.frame(Z1_F_Demand,row.names=rownames)
  kable(IO_Z1_F_Demand)
  
  
  #Create final demand column 
  Z2_D_df<-as.data.frame(Z2_d)
  Z2_D<- as.data.frame(colSums(Z2_D_df))
  
  Z2_D_shares<-as.matrix(Z2_d[,yr]/sum(Z2_d[,yr]))
  Z2_p_df<-t(Z2_D_shares)%*%Z2_p[,yr]
  
  Z2_F_Demand   <- c(round(Z2_d[1,yr]*Z2_p[1,yr] , digits = 2),                                                                    
                     round(Z2_d[2,yr]*Z2_p[2,yr] , digits = 2),
                     round(Z2_d[3,yr]*Z2_p[3,yr] , digits = 2),
                     round(Z2_d[4,yr]*Z2_p[4,yr] , digits = 2),
                     round(Z2_d[5,yr]*Z2_p[5,yr] , digits = 2),                                                                    
                     round(Z2_d[6,yr]*Z2_p[6,yr]*Z2_xr[,yr] , digits = 2),
                     round(Z2_d[7,yr]*Z2_p[7,yr]*Z2_xr[,yr] , digits = 2),
                     round(Z2_d[8,yr]*Z2_p[8,yr]*Z2_xr[,yr] , digits = 2),
                     round(Z2_d[9,yr]*Z2_p[9,yr]*Z2_xr[,yr] , digits = 2),
                     round(Z2_d[10,yr]*Z2_p[10,yr]*Z2_xr[,yr] , digits = 2),
                     "",
                     "",
                     "",
                     "",
                     round(sum(Z2_d[,yr])*Z2_p_df,digits=2),
                     ""
  )
  
  
  #Create table of results
  IO_Z2_F_Demand<-as.data.frame(Z2_F_Demand,row.names=rownames)
  kable(IO_Z2_F_Demand)
  
  
  
  #Create final demand column 
  Z1_C_Demand   <- c(round(Z1_c[1,yr]*Z1_beta[1,yr]*Z1_p[1,yr] , digits = 2),                                                                    
                     round(Z1_c[1,yr]*Z1_beta[2,yr]*Z1_p[2,yr] , digits = 2),
                     round(Z1_c[1,yr]*Z1_beta[3,yr]*Z1_p[3,yr] , digits = 2),
                     round(Z1_c[1,yr]*Z1_beta[4,yr]*Z1_p[4,yr] , digits = 2),
                     round(Z1_c[1,yr]*Z1_beta[5,yr]*Z1_p[5,yr] , digits = 2),                                                                    
                     round(Z1_c[1,yr]*Z1_beta[6,yr]*Z1_p[6,yr]*Z1_xr[,yr] , digits = 2),
                     round(Z1_c[1,yr]*Z1_beta[7,yr]*Z1_p[7,yr]*Z1_xr[,yr] , digits = 2),
                     round(Z1_c[1,yr]*Z1_beta[8,yr]*Z1_p[8,yr]*Z1_xr[,yr] , digits = 2),
                     round(Z1_c[1,yr]*Z1_beta[9,yr]*Z1_p[9,yr]*Z1_xr[,yr] , digits = 2),
                     round(Z1_c[1,yr]*Z1_beta[10,yr]*Z1_p[10,yr]*Z1_xr[,yr] , digits = 2),
                     "",
                     "",
                     "",
                     "",
                     round(Z1_c[1,yr]*Z1_pa[1,yr],digits = 2),
                     ""
  )
  
  #Create table of results
  IO_Z1_C_Demand<-as.data.frame(Z1_C_Demand,row.names=rownames)
  kable(IO_Z1_C_Demand)
  
  
  
  #Create final demand column 
  Z2_C_Demand   <- c(round(Z2_c[1,yr]*Z2_beta[1,yr]*Z2_p[1,yr]*Z2_xr[,yr] , digits = 2),                                                                    
                     round(Z2_c[1,yr]*Z2_beta[2,yr]*Z2_p[2,yr]*Z2_xr[,yr] , digits = 2),
                     round(Z2_c[1,yr]*Z2_beta[3,yr]*Z2_p[3,yr]*Z2_xr[,yr] , digits = 2),
                     round(Z2_c[1,yr]*Z2_beta[4,yr]*Z2_p[4,yr]*Z2_xr[,yr] , digits = 2),
                     round(Z2_c[1,yr]*Z2_beta[5,yr]*Z2_p[5,yr]*Z2_xr[,yr] , digits = 2),                                                                    
                     round(Z2_c[1,yr]*Z2_beta[6,yr]*Z2_p[6,yr] , digits = 2),
                     round(Z2_c[1,yr]*Z2_beta[7,yr]*Z2_p[7,yr] , digits = 2),
                     round(Z2_c[1,yr]*Z2_beta[8,yr]*Z2_p[8,yr] , digits = 2),
                     round(Z2_c[1,yr]*Z2_beta[9,yr]*Z2_p[9,yr] , digits = 2),
                     round(Z2_c[1,yr]*Z2_beta[10,yr]*Z2_p[10,yr] , digits = 2),
                     "",
                     "",
                     "",
                     "",
                     round(Z2_c[1,yr]*Z2_pa[1,yr],digits = 2),
                     ""
  )
  
  #Create table of results
  IO_Z2_C_Demand<-as.data.frame(Z2_C_Demand,row.names=rownames)
  kable(IO_Z2_C_Demand)
  
  #Create final demand column 
  Z1_G_Demand   <- c(round(Z1_g[1,yr]*Z1_sigma[1,yr]*Z1_p[1,yr] , digits = 2),                                                                    
                     round(Z1_g[1,yr]*Z1_sigma[2,yr]*Z1_p[2,yr] , digits = 2),
                     round(Z1_g[1,yr]*Z1_sigma[3,yr]*Z1_p[3,yr] , digits = 2),
                     round(Z1_g[1,yr]*Z1_sigma[4,yr]*Z1_p[4,yr] , digits = 2),
                     round(Z1_g[1,yr]*Z1_sigma[5,yr]*Z1_p[5,yr] , digits = 2),                                                                    
                     round(Z1_g[1,yr]*Z1_sigma[6,yr]*Z1_p[6,yr]*Z1_xr[,yr] , digits = 2),
                     round(Z1_g[1,yr]*Z1_sigma[7,yr]*Z1_p[7,yr]*Z1_xr[,yr] , digits = 2),
                     round(Z1_g[1,yr]*Z1_sigma[8,yr]*Z1_p[8,yr]*Z1_xr[,yr] , digits = 2),
                     round(Z1_g[1,yr]*Z1_sigma[9,yr]*Z1_p[9,yr]*Z1_xr[,yr] , digits = 2),
                     round(Z1_g[1,yr]*Z1_sigma[10,yr]*Z1_p[10,yr]*Z1_xr[,yr] , digits = 2),
                     "",
                     "",
                     "",
                     "",
                     round(Z1_g[1,yr]*Z1_pg[1,yr],digits = 2),
                     ""
  )
  
  #Create table of results
  IO_Z1_G_Demand<-as.data.frame(Z1_G_Demand,row.names=rownames)
  kable(IO_Z1_G_Demand)
  
  #Create final demand column 
  Z2_G_Demand   <- c(round(Z2_g[1,yr]*Z2_sigma[1,yr]*Z2_p[1,yr]*Z2_xr[,yr] , digits = 2),                                                                    
                     round(Z2_g[1,yr]*Z2_sigma[2,yr]*Z2_p[2,yr]*Z2_xr[,yr] , digits = 2),
                     round(Z2_g[1,yr]*Z2_sigma[3,yr]*Z2_p[3,yr]*Z2_xr[,yr] , digits = 2),
                     round(Z2_g[1,yr]*Z2_sigma[4,yr]*Z2_p[4,yr]*Z2_xr[,yr] , digits = 2),
                     round(Z2_g[1,yr]*Z2_sigma[5,yr]*Z2_p[5,yr]*Z2_xr[,yr] , digits = 2),                                                                    
                     round(Z2_g[1,yr]*Z2_sigma[6,yr]*Z2_p[6,yr] , digits = 2),
                     round(Z2_g[1,yr]*Z2_sigma[7,yr]*Z2_p[7,yr] , digits = 2),
                     round(Z2_g[1,yr]*Z2_sigma[8,yr]*Z2_p[8,yr] , digits = 2),
                     round(Z2_g[1,yr]*Z2_sigma[9,yr]*Z2_p[9,yr] , digits = 2),
                     round(Z2_g[1,yr]*Z2_sigma[10,yr]*Z2_p[10,yr] , digits = 2),
                     "",
                     "",
                     "",
                     "",
                     round(Z2_g[1,yr]*Z2_pg[1,yr],digits = 2),
                     ""
  )
  
  #Create table of results
  IO_Z2_G_Demand<-as.data.frame(Z2_G_Demand,row.names=rownames)
  kable(IO_Z2_G_Demand)
  
  #Create final demand column 
  Z1_id_Demand   <- c(round(Z1_id[1,yr]*Z1_iota[1,yr]*Z1_p[1,yr] , digits = 2),                                                                    
                      round(Z1_id[1,yr]*Z1_iota[2,yr]*Z1_p[2,yr] , digits = 2),
                      round(Z1_id[1,yr]*Z1_iota[3,yr]*Z1_p[3,yr] , digits = 2),
                      round(Z1_id[1,yr]*Z1_iota[4,yr]*Z1_p[4,yr] , digits = 2),
                      round(Z1_id[1,yr]*Z1_iota[5,yr]*Z1_p[5,yr] , digits = 2),                                                                    
                      round(Z1_id[1,yr]*Z1_iota[6,yr]*Z1_p[6,yr]*Z1_xr[,yr] , digits = 2),
                      round(Z1_id[1,yr]*Z1_iota[7,yr]*Z1_p[7,yr]*Z1_xr[,yr] , digits = 2),
                      round(Z1_id[1,yr]*Z1_iota[8,yr]*Z1_p[8,yr]*Z1_xr[,yr] , digits = 2),
                      round(Z1_id[1,yr]*Z1_iota[9,yr]*Z1_p[9,yr]*Z1_xr[,yr] , digits = 2),
                      round(Z1_id[1,yr]*Z1_iota[10,yr]*Z1_p[10,yr]*Z1_xr[,yr] , digits = 2),
                      "",
                      "",
                      "",
                      "",
                      round(Z1_id[1,yr]*Z1_pid[1,yr],digits = 2),
                      ""
  )
  
  #Create table of results
  IO_Z1_id_Demand<-as.data.frame(Z1_id_Demand,row.names=rownames)
  kable(IO_Z1_id_Demand)
  
  #Create investment final demand column 
  Z2_id_Demand   <- c(round(Z2_id[1,yr]*Z2_iota[1,yr]*Z2_p[1,yr]*Z2_xr[,yr] , digits = 2),                                                                    
                      round(Z2_id[1,yr]*Z2_iota[2,yr]*Z2_p[2,yr]*Z2_xr[,yr] , digits = 2),
                      round(Z2_id[1,yr]*Z2_iota[3,yr]*Z2_p[3,yr]*Z2_xr[,yr] , digits = 2),
                      round(Z2_id[1,yr]*Z2_iota[4,yr]*Z2_p[4,yr]*Z2_xr[,yr] , digits = 2),
                      round(Z2_id[1,yr]*Z2_iota[5,yr]*Z2_p[5,yr]*Z2_xr[,yr] , digits = 2),                                                                    
                      round(Z2_id[1,yr]*Z2_iota[6,yr]*Z2_p[6,yr] , digits = 2),
                      round(Z2_id[1,yr]*Z2_iota[7,yr]*Z2_p[7,yr] , digits = 2),
                      round(Z2_id[1,yr]*Z2_iota[8,yr]*Z2_p[8,yr] , digits = 2),
                      round(Z2_id[1,yr]*Z2_iota[9,yr]*Z2_p[9,yr] , digits = 2),
                      round(Z2_id[1,yr]*Z2_iota[10,yr]*Z2_p[10,yr] , digits = 2),
                      "",
                      "",
                      "",
                      "",
                      round(Z2_id[1,yr]*Z2_pid[1,yr],digits = 2),
                      ""
  )
  
  #Create table of results
  IO_Z2_id_Demand<-as.data.frame(Z2_id_Demand,row.names=rownames)
  kable(IO_Z2_id_Demand)
  
  #Create Export final demand column 
  Z1_nex_Demand  <- c(round(Z2_eta_rev[1,yr]*Z1_rex[,yr]*Z1_p[1,yr] , digits = 2),                                                                    
                      round(Z2_eta_rev[2,yr]*Z1_rex[,yr]*Z1_p[2,yr] , digits = 2),
                      round(Z2_eta_rev[3,yr]*Z1_rex[,yr]*Z1_p[3,yr], digits = 2),
                      round(Z2_eta_rev[4,yr]*Z1_rex[,yr]*Z1_p[4,yr], digits = 2),
                      round(Z2_eta_rev[5,yr]*Z1_rex[,yr]*Z1_p[5,yr], digits = 2),                                                                    
                      round(Z2_eta_rev[6,yr]*Z1_rex[,yr]*Z1_p[6,yr] , digits = 2),
                      round(Z2_eta_rev[7,yr]*Z1_rex[,yr]*Z1_p[7,yr] , digits = 2),
                      round(Z2_eta_rev[8,yr]*Z1_rex[,yr]*Z1_p[8,yr] , digits = 2),
                      round(Z2_eta_rev[9,yr]*Z1_rex[,yr]*Z1_p[9,yr] , digits = 2),
                      round(Z2_eta_rev[10,yr]*Z1_rex[,yr]*Z1_p[10,yr] , digits = 2),
                      "",
                      "",
                      "",
                      "",
                      round(Z1_rex[,yr]*(Z1_pim[,yr]/Z2_xr[,i]),digits = 2),
                      ""
  )
  
  #Create table of results
  IO_Z1_nex_Demand<-as.data.frame(Z1_nex_Demand,row.names=rownames)
  kable(IO_Z1_nex_Demand)
  
  #Create Export final demand column 
  Z2_nex_Demand   <- c(round(Z1_eta_rev[1,yr]*Z2_rex[,yr]*Z2_p[1,yr], digits = 2),                                                                    
                       round(Z1_eta_rev[2,yr]*Z2_rex[,yr]*Z2_p[2,yr], digits = 2),
                       round(Z1_eta_rev[3,yr]*Z2_rex[,yr]*Z2_p[3,yr], digits = 2),
                       round(Z1_eta_rev[4,yr]*Z2_rex[,yr]*Z2_p[4,yr], digits = 2),
                       round(Z1_eta_rev[5,yr]*Z2_rex[,yr]*Z2_p[5,yr], digits = 2),                                                                    
                       round(Z1_eta_rev[6,yr]*Z2_rex[,yr]*Z2_p[6,yr], digits = 2),
                       round(Z1_eta_rev[7,yr]*Z2_rex[,yr]*Z2_p[7,yr], digits = 2),
                       round(Z1_eta_rev[8,yr]*Z2_rex[,yr]*Z2_p[8,yr] , digits = 2),
                       round(Z1_eta_rev[9,yr]*Z2_rex[,yr]*Z2_p[9,yr] , digits = 2),
                       round(Z1_eta_rev[10,yr]*Z2_rex[,yr]*Z2_p[10,yr], digits = 2),
                       "",
                       "",
                       "",
                       "",
                       round(Z2_rex[,yr]*(Z2_pim[,yr]/Z1_xr[,i]),digits = 2),
                       ""
  )
  
  #Create table of results
  IO_Z2_nex_Demand<-as.data.frame(Z2_nex_Demand,row.names=rownames)
  kable(IO_Z2_nex_Demand)
  
  #Create Export final demand column 
  Z1_nimp_Demand <- c(round(-Z1_eta[1,yr]*Z1_imp[,yr]*Z1_p[1,yr], digits = 2),                                                                    
                      round(-Z1_eta[2,yr]*Z1_imp[,yr]*Z1_p[2,yr], digits = 2),
                      round(-Z1_eta[3,yr]*Z1_imp[,yr]*Z1_p[3,yr], digits = 2),
                      round(-Z1_eta[4,yr]*Z1_imp[,yr]*Z1_p[4,yr], digits = 2),
                      round(-Z1_eta[5,yr]*Z1_imp[,yr]*Z1_p[5,yr], digits = 2),                                                                    
                      round(-Z1_eta[6,yr]*Z1_imp[,yr]*Z1_p[6,yr], digits = 2),
                      round(-Z1_eta[7,yr]*Z1_imp[,yr]*Z1_p[7,yr], digits = 2),
                      round(-Z1_eta[8,yr]*Z1_imp[,yr]*Z1_p[8,yr], digits = 2),
                      round(-Z1_eta[9,yr]*Z1_imp[,yr]*Z1_p[9,yr], digits = 2),
                      round(-Z1_eta[10,yr]*Z1_imp[,yr]*Z1_p[10,yr], digits = 2),
                      "",
                      "",
                      "",
                      "",
                      round(-Z1_imp[,yr]*Z1_pim[,yr],digits = 2),
                      ""
  )
  
  #Create table of results
  IO_Z1_nimp_Demand<-as.data.frame(Z1_nimp_Demand,row.names=rownames)
  kable(IO_Z1_nimp_Demand)
  
  #Create import final demand column 
  Z2_nimp_Demand   <- c(round(-Z2_eta[1,yr]*Z2_imp[,yr]*Z2_p[1,yr], digits = 2),                                                                    
                        round(-Z2_eta[2,yr]*Z2_imp[,yr]*Z2_p[2,yr], digits = 2),
                        round(-Z2_eta[3,yr]*Z2_imp[,yr]*Z2_p[3,yr], digits = 2),
                        round(-Z2_eta[4,yr]*Z2_imp[,yr]*Z2_p[4,yr], digits = 2),
                        round(-Z2_eta[5,yr]*Z2_imp[,yr]*Z2_p[5,yr], digits = 2),                                                                    
                        round(-Z2_eta[6,yr]*Z2_imp[,yr]*Z2_p[6,yr], digits = 2),
                        round(-Z2_eta[7,yr]*Z2_imp[,yr]*Z2_p[7,yr], digits = 2),
                        round(-Z2_eta[8,yr]*Z2_imp[,yr]*Z2_p[8,yr], digits = 2),
                        round(-Z2_eta[9,yr]*Z2_imp[,yr]*Z2_p[9,yr], digits = 2),
                        round(-Z2_eta[10,yr]*Z2_imp[,yr]*Z2_p[10,yr], digits = 2),
                        "",
                        "",
                        "",
                        "",
                        round(-Z2_imp[,yr]*Z2_pim[,yr],digits = 2),
                        ""
  )
  
  #Create table of results
  IO_Z2_nimp_Demand<-as.data.frame(Z2_nimp_Demand,row.names=rownames)
  kable(IO_Z2_nimp_Demand)
  
  #Create total output column
  Output_col_sum   <- c(round( x[1,yr]*a11[,yr]*Z1_p[1,yr]+
                                 x[2,yr]*a12[,yr]*Z1_p[1,yr]+  
                                 x[3,yr]*a13[,yr]*Z1_p[1,yr]+
                                 x[4,yr]*a14[,yr]*Z1_p[1,yr]+
                                 x[5,yr]*a15[,yr]*Z1_p[1,yr]+
                                 x[6,yr]*a16[,yr]*Z1_p[1,yr]+
                                 x[7,yr]*a17[,yr]*Z1_p[1,yr]+
                                 x[8,yr]*a18[,yr]*Z1_p[1,yr]+
                                 x[9,yr]*a19[,yr]*Z1_p[1,yr]+   
                                 x[10,yr]*a110[,yr]*Z1_p[1,yr]+
                                 d[1,yr]*Z1_p[1,yr]             , digits = 2),                                                                    
                        
                        round( x[1,yr]*a21[,yr]*Z1_p[2,yr]+
                                 x[2,yr]*a22[,yr]*Z1_p[2,yr]+  
                                 x[3,yr]*a23[,yr]*Z1_p[2,yr]+
                                 x[4,yr]*a24[,yr]*Z1_p[2,yr]+
                                 x[5,yr]*a25[,yr]*Z1_p[2,yr]+
                                 x[6,yr]*a26[,yr]*Z1_p[2,yr]+
                                 x[7,yr]*a27[,yr]*Z1_p[2,yr]+
                                 x[8,yr]*a28[,yr]*Z1_p[2,yr]+
                                 x[9,yr]*a29[,yr]*Z1_p[2,yr]+
                                 x[8,yr]*a210[,yr]*Z1_p[2,yr]+
                                 d[2,yr]*Z1_p[2,yr]             , digits = 2),
                        
                        round( x[1,yr]*a31[,yr]*Z1_p[3,yr]+
                                 x[2,yr]*a32[,yr]*Z1_p[3,yr]+  
                                 x[3,yr]*a33[,yr]*Z1_p[3,yr]+
                                 x[4,yr]*a34[,yr]*Z1_p[3,yr]+
                                 x[5,yr]*a35[,yr]*Z1_p[3,yr]+
                                 x[6,yr]*a36[,yr]*Z1_p[3,yr]+
                                 x[7,yr]*a37[,yr]*Z1_p[3,yr]+
                                 x[8,yr]*a38[,yr]*Z1_p[3,yr]+
                                 x[9,yr]*a39[,yr]*Z1_p[3,yr]+  
                                 x[10,yr]*a310[,yr]*Z1_p[3,yr]+
                                 d[3,yr]*Z1_p[3,yr]             , digits = 2),
                        
                        round( x[1,yr]*a41[,yr]*Z1_p[4,yr]+
                                 x[2,yr]*a42[,yr]*Z1_p[4,yr]+  
                                 x[3,yr]*a43[,yr]*Z1_p[4,yr]+
                                 x[4,yr]*a44[,yr]*Z1_p[4,yr]+
                                 x[5,yr]*a45[,yr]*Z1_p[4,yr]+
                                 x[6,yr]*a46[,yr]*Z1_p[4,yr]+
                                 x[7,yr]*a47[,yr]*Z1_p[4,yr]+
                                 x[8,yr]*a48[,yr]*Z1_p[4,yr]+
                                 x[9,yr]*a49[,yr]*Z1_p[4,yr]+
                                 x[10,yr]*a410[,yr]*Z1_p[4,yr]+
                                 d[4,yr]*Z1_p[4,yr]             , digits = 2),
                        
                        round( x[1,yr]*a51[,yr]*Z1_p[5,yr]+
                                 x[2,yr]*a52[,yr]*Z1_p[5,yr]+  
                                 x[3,yr]*a53[,yr]*Z1_p[5,yr]+
                                 x[4,yr]*a54[,yr]*Z1_p[5,yr]+
                                 x[5,yr]*a55[,yr]*Z1_p[5,yr]+
                                 x[6,yr]*a56[,yr]*Z1_p[5,yr]+
                                 x[7,yr]*a57[,yr]*Z1_p[5,yr]+
                                 x[8,yr]*a58[,yr]*Z1_p[5,yr]+
                                 x[9,yr]*a59[,yr]*Z1_p[5,yr]+
                                 x[10,yr]*a510[,yr]*Z1_p[5,yr]+
                                 d[5,yr]*Z2_p[5,yr]             , digits = 2),                                                                    
                        
                        round( x[1,yr]*a61[,yr]*Z2_p[6,yr]*Z2_xr[,yr]+
                                 x[2,yr]*a62[,yr]*Z2_p[6,yr]*Z2_xr[,yr]+  
                                 x[3,yr]*a63[,yr]*Z2_p[6,yr]*Z2_xr[,yr]+
                                 x[4,yr]*a64[,yr]*Z2_p[6,yr]*Z2_xr[,yr]+
                                 x[5,yr]*a65[,yr]*Z2_p[6,yr]*Z2_xr[,yr]+
                                 x[6,yr]*a66[,yr]*Z2_p[6,yr]*Z2_xr[,yr]+
                                 x[7,yr]*a67[,yr]*Z2_p[6,yr]*Z2_xr[,yr]+
                                 x[8,yr]*a68[,yr]*Z2_p[6,yr]*Z2_xr[,yr]+
                                 x[9,yr]*a69[,yr]*Z2_p[6,yr]*Z2_xr[,yr]+
                                 x[10,yr]*a610[,yr]*Z2_p[6,yr]*Z2_xr[,yr]+
                                 d[6,yr]*Z2_p[6,yr]*Z2_xr[,yr]   , digits = 2),
                        
                        round( x[1,yr]*a71[,yr]*Z2_p[7,yr]*Z2_xr[,yr]+
                                 x[2,yr]*a72[,yr]*Z2_p[7,yr]*Z2_xr[,yr]+  
                                 x[3,yr]*a73[,yr]*Z2_p[7,yr]*Z2_xr[,yr]+
                                 x[4,yr]*a74[,yr]*Z2_p[7,yr]*Z2_xr[,yr]+
                                 x[5,yr]*a75[,yr]*Z2_p[7,yr]*Z2_xr[,yr]+
                                 x[6,yr]*a76[,yr]*Z2_p[7,yr]*Z2_xr[,yr]+
                                 x[7,yr]*a77[,yr]*Z2_p[7,yr]*Z2_xr[,yr]+
                                 x[8,yr]*a78[,yr]*Z2_p[7,yr]*Z2_xr[,yr]+
                                 x[9,yr]*a79[,yr]*Z2_p[7,yr]*Z2_xr[,yr]+
                                 x[10,yr]*a710[,yr]*Z2_p[7,yr]*Z2_xr[,yr]+
                                 d[7,yr]*Z2_p[7,yr]*Z2_xr[,yr]   , digits = 2),
                        
                        round( x[1,yr]*a81[,yr]*Z2_p[8,yr]*Z2_xr[,yr]+
                                 x[2,yr]*a82[,yr]*Z2_p[8,yr]*Z2_xr[,yr]+  
                                 x[3,yr]*a83[,yr]*Z2_p[8,yr]*Z2_xr[,yr]+
                                 x[4,yr]*a84[,yr]*Z2_p[8,yr]*Z2_xr[,yr]+
                                 x[5,yr]*a85[,yr]*Z2_p[8,yr]*Z2_xr[,yr]+
                                 x[6,yr]*a86[,yr]*Z2_p[8,yr]*Z2_xr[,yr]+
                                 x[7,yr]*a87[,yr]*Z2_p[8,yr]*Z2_xr[,yr]+
                                 x[8,yr]*a88[,yr]*Z2_p[8,yr]*Z2_xr[,yr]+
                                 x[9,yr]*a89[,yr]*Z2_p[8,yr]*Z2_xr[,yr]+
                                 x[10,yr]*a810[,yr]*Z2_p[8,yr]*Z2_xr[,yr]+
                                 d[8,yr]*Z2_p[8,yr]*Z2_xr[,yr]   , digits = 2),
                        
                        round( x[1,yr]*a91[,yr]*Z2_p[9,yr]*Z2_xr[,yr]+
                                 x[2,yr]*a92[,yr]*Z2_p[9,yr]*Z2_xr[,yr]+  
                                 x[3,yr]*a93[,yr]*Z2_p[9,yr]*Z2_xr[,yr]+
                                 x[4,yr]*a94[,yr]*Z2_p[9,yr]*Z2_xr[,yr]+
                                 x[5,yr]*a95[,yr]*Z2_p[9,yr]*Z2_xr[,yr]+
                                 x[6,yr]*a96[,yr]*Z2_p[9,yr]*Z2_xr[,yr]+
                                 x[7,yr]*a97[,yr]*Z2_p[9,yr]*Z2_xr[,yr]+
                                 x[8,yr]*a98[,yr]*Z2_p[9,yr]*Z2_xr[,yr]+
                                 x[9,yr]*a99[,yr]*Z2_p[9,yr]*Z2_xr[,yr]+
                                 x[10,yr]*a910[,yr]*Z2_p[9,yr]*Z2_xr[,yr]+
                                 d[9,yr]*Z2_p[9,yr]*Z2_xr[,yr]   , digits = 2),
                        
                        round( x[1,yr]*a101[,yr]*Z2_p[10,yr]*Z2_xr[,yr]+
                                 x[2,yr]*a102[,yr]*Z2_p[10,yr]*Z2_xr[,yr]+  
                                 x[3,yr]*a103[,yr]*Z2_p[10,yr]*Z2_xr[,yr]+
                                 x[4,yr]*a104[,yr]*Z2_p[10,yr]*Z2_xr[,yr]+
                                 x[5,yr]*a105[,yr]*Z2_p[10,yr]*Z2_xr[,yr]+
                                 x[6,yr]*a106[,yr]*Z2_p[10,yr]*Z2_xr[,yr]+
                                 x[7,yr]*a107[,yr]*Z2_p[10,yr]*Z2_xr[,yr]+
                                 x[8,yr]*a108[,yr]*Z2_p[10,yr]*Z2_xr[,yr]+
                                 x[9,yr]*a109[,yr]*Z2_p[10,yr]*Z2_xr[,yr]+
                                 x[10,yr]*a1010[,yr]*Z2_p[10,yr]*Z2_xr[,yr]+
                                 d[10,yr]*Z2_p[10,yr]*Z2_xr[,yr]   , digits = 2),
                        
                        "",
                        "",
                        "",
                        "",
                        "",
                        ""
  )
  
  #Create table of results
  IO_Output_col_sum<-as.data.frame(Output_col_sum,row.names=rownames)
  kable(IO_Output_col_sum)
  
  
  #Create total output column
  Output_model      <- c(round( x[1,yr]*Z1_p[1,yr], digits = 2),                                                                    
                         round( x[2,yr]*Z1_p[2,yr], digits = 2),
                         round( x[3,yr]*Z1_p[3,yr], digits = 2),
                         round( x[4,yr]*Z1_p[4,yr], digits = 2),
                         round( x[5,yr]*Z1_p[5,yr], digits = 2),                                                                    
                         round( x[6,yr]*Z2_p[6,yr]*Z2_xr[,yr], digits = 2),
                         round( x[7,yr]*Z2_p[7,yr]*Z2_xr[,yr], digits = 2),
                         round( x[8,yr]*Z2_p[8,yr]*Z2_xr[,yr], digits = 2),
                         round( x[9,yr]*Z2_p[9,yr]*Z2_xr[,yr], digits = 2),
                         round( x[10,yr]*Z2_p[10,yr]*Z2_xr[,yr], digits = 2),
                         "",
                         "",
                         "",
                         "",
                         "",
                         ""
  )
  
  
  #Create table of results
  IO_Output_model<-as.data.frame(Output_model,row.names=rownames)
  kable(IO_Output_model)
  
  #Create IO matrix
  IO_Matrix<-cbind(IO_Z1_Agri,IO_Z1_Manufacturing,IO_Z1_Serv,IO_Z1_Waste,IO_Z1_Recyc,
                   IO_Z2_Agri,IO_Z2_Manufacturing,IO_Z2_Serv,IO_Z2_Waste,IO_Z2_Recyc,
                   IO_Z1_C_Demand,IO_Z1_G_Demand,IO_Z1_id_Demand,IO_Z1_nex_Demand,IO_Z1_nimp_Demand,IO_Z1_F_Demand,
                   IO_Z2_C_Demand,IO_Z2_G_Demand,IO_Z2_id_Demand,IO_Z2_nex_Demand,IO_Z2_nimp_Demand,IO_Z2_F_Demand,
                   IO_Output_col_sum,IO_Output_model)
  return(IO_Matrix)
}