# New Markup Determination and Reproduction Prices Fixed
# Reproduction Prices and Market Prices are Equal
# pa and pim include vat and tariffs, respectively
# 5 February
run.model <- function (initial)
{
  # INITIALIZATION
  # Matrices
  A <- as.matrix(initial$A.matrix)
  I <- initial$I
  
  # Parameters
  for (i in 1 : nrow(initial$parameters)) assign(rownames(initial$parameters)[i], initial$parameters$value[i])
  
  # Aggregate Variables
  var.labels <- rownames(initial$variables)
  variable.names <- unique(sapply(strsplit(var.labels, split = '-', fixed = T), function (tmp) tmp[1]))
  for(i in which(!grepl('-', var.labels))) assign(var.labels[i], matrix(initial$variables[i, 't1'], nrow = 1, ncol = nPeriods))
  
  # Industry Variables
  industry.vars <- unique(sapply(strsplit(grep('-', var.labels, value = T), split = '-', fixed = T), function (tmp) tmp[1]))
  
  for(i in 1 : length(industry.vars))
  {
    assign(industry.vars[i], 
           matrix(initial$variables[grep('-', var.labels)[(dim(A)[1] * (i - 1) + 1) : (dim(A)[1] * i)], 't1'], 
                  nrow = dim(A)[1], ncol = nPeriods))
  }
  
  # Load "a" and 'b' values from the "A.mat" and 'B.mat' Excel worksheets, not the "variables" worksheet
  for(i in 1 : 10) for (j in 1 : 10) assign(paste0('a', i, j), matrix(data = as.numeric(initial$A.matrix[i,j]), nrow=1, ncol=nPeriods))
  for(i in 1 : 10) for (j in 1 : 10) assign(paste0('b', i, j), matrix(data = as.numeric(initial$B.matrix[i,j]), nrow=1, ncol=nPeriods))
  
  # MARKUP DETERMINATION SO MARKET PRICES ARE UNITY
  Z1_p = Z1_p_t <- matrix(data = c(1,1,1,1,1,0,0,0,0,0), nrow = 10, ncol = nPeriods) 
  Z2_p = Z2_p_t <- matrix(data = c(0,0,0,0,0,1,1,1,1,1), nrow = 10, ncol = nPeriods)
  
  w <- c(Z1_w[1 : 5, 1], Z2_w[6 : 10, 1])
  pr <- c(Z1_pr[1 : 5, 1], Z2_pr[6 : 10, 1])
  kappa <- c(Z1_kappa[1 : 5, 1], Z2_kappa[6 : 10, 1])
  delta <- rep(c(Z1_delta, Z2_delta), each = 5)
  foo <- (1 - w / pr) / (1 + kappa * delta)
  markup <- foo / colSums(A) - 1
  
  # w / pr + colSums(A) * (1 + markup) * (1 + kappa * delta) # prices should be 1 for each sector
  
  Z1_mu = matrix(data=c(markup[1 : 5],0,0,0,0,0),nrow=10,ncol=nPeriods)
  Z2_mu = matrix(data=c(0,0,0,0,0, markup[6 : 10]),nrow=10,ncol=nPeriods)
  
  rm(w, pr, kappa, delta, foo, markup) # remove auxiliary variables
  
  # INITIALIZE BALANCE SHEET  
  i <- 1
  Z1_b_s[,i] <- initial$BS['Z1_bs0',]
  Z1_b_h_Z1[,i] <- Z1_b_s_Z1[,i] <- initial$BS['Z1_b_s_Z1_0', ]
  Z1_a_s[1,1] <- Z1_a_d[1,1] <- initial$BS['Z1_a_s0', ]
  Z1_e_s[1,1] <- Z1_e_h_Z1[1,1] <- initial$BS['Z1_e_s0',]
  Z1_ls[1,1] <- initial$BS['Z1_ls0', ]
  Z1_lh[1,1] <- initial$BS['Z1_lh0', ]
  Z1_mh[1,1] <- initial$BS['Z1_mh0', ]
  Z1_b_h_Z2[1,1] <- initial$BS['Z1_b_h_Z2_0',]
  Z1_b_cb_d_Z2[1,1] <- Z1_b_cb_s_Z2[1,1] <- initial$BS['Z1_b_cb_d_Z20', ]
  
  Z1_lf[1,1] <- Z1_ls[1,1] - Z1_lh[1,1]
  Z1_b_b[1,1] <- Z1_a_d[1,1] + Z1_mh[1,1] - Z1_ls[1,1] 
  Z1_b_cb[1,1] <- Z1_b_s[1,1] - Z1_b_b[1,1] - Z1_b_h_Z1[1,1]
  Z1_h_h[1,1] <- Z1_h_s[1,1] <- Z1_b_cb[1,1] + Z1_a_s[1,1] + Z1_b_cb_d_Z2[1,1]
  Z1_v[1,1] <- Z1_b_h_Z1[1,1] + Z1_e_h_Z1[1,1] + Z1_mh[1,1] + Z1_h_h[1,1] + Z1_b_h_Z2[1,1] - Z1_lh[1,1]
  Z1_k[1,1] <- Z1_e_s[1,1] + Z1_lf[1,1]
  
  Z2_b_s[,i] <- initial$BS['Z2_bs0',]
  Z2_b_h_Z2[,i] <- Z2_b_s_Z2[,i] <- initial$BS['Z2_b_s_Z2_0',]
  Z2_a_s[1,1] <- Z2_a_d[1,1] <- initial$BS['Z2_a_s0',]
  Z2_e_s[1,1] <- Z2_e_h_Z2[1,1] <- initial$BS['Z2_e_s0',]
  Z2_ls[1,1] <- initial$BS['Z2_ls0',]
  Z2_lh[1,1] <- initial$BS['Z2_lh0',]
  Z2_mh[1,1] <- initial$BS['Z2_mh0',]
  
  Z2_lf[1,1] <- Z2_ls[1,1] - Z2_lh[1,1]
  Z2_b_b[1,1] <- Z2_a_d[1,1] + Z2_mh[1,1] - Z2_ls[1,1] 
  Z2_b_cb[1,1] <- Z2_b_s[1,1] - Z2_b_b[1,1] - Z2_b_h_Z2[1,1] - Z1_b_cb_d_Z2[1,1] - Z1_b_h_Z2[1,1]
  Z2_h_h[1,1] <- Z2_h_s[1,1] <- Z2_b_cb[1,1] + Z2_a_s[1,1]
  Z2_v[1,1] <- Z2_b_h_Z2[1,1] + Z2_e_h_Z2[1,1] + Z2_mh[1,1] + Z2_h_h[1,1] - Z2_lh[1,1]
  Z2_k[1,1] <- Z2_e_s[1,1] + Z2_lf[1,1]
  
  Z1_g[ ,i] <- Z1_gg0
  Z2_g[ ,i] <- Z2_gg0
  Z1_rb[ ,i] = Z1_r_star[,i] + Z1_mu_b[,i] 
  Z1_rm[,i] = Z1_r_star[,i] + Z1_mu_m[,i]  #NEW
  Z1_rb[,i] = Z1_r_star[,i] + Z1_mu_b[,i]  #NEW
  Z1_rl[,i] = Z1_r_star[,i] + Z1_mu_l[,i]  #NEW
  Z1_rh[,i] = Z1_r_star[,i] + Z1_mu_h[,i]
  Z2_rb[ ,i] = Z2_r_star[,i] + Z2_mu_b[,i] 
  Z2_rm[,i] = Z2_r_star[,i] + Z2_mu_m[,i]  #NEW
  Z2_rb[,i] = Z2_r_star[,i] + Z2_mu_b[,i]  #NEW
  Z2_rl[,i] = Z2_r_star[,i] + Z2_mu_l[,i]  #NEW
  Z2_rh[,i] = Z2_r_star[,i] + Z2_mu_h[,i]
  
  ### FOR LOOP
  last.iteration <- consistency.error <- c()
  
  #Start the production in t=2
  for (i in 2:nPeriods){
  
    ## SCENARIOS
    source(paste0(directory, 'functions/scenario_selection.R'), local = TRUE)
    
    # Define iterations for converging to simultaneous solution
    simiter <- array(NA, dim = c(length(var.labels), 100), dimnames = list(var.labels, paste0('iter', 1 : 100)))
    for (iterations in 1 : max.iterations){
      
      #MODEL EQUATIONS FOR AREA 1
      
      #MATRIX OF TECHNICAL COEFFICIENTS
      
      #Define technical coefficients (note: a = initial coefficients; b = target coefficients)
      a11[,i] = a11[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b11[,i-1] - a11[,i-1])
      a12[,i] = a12[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b12[,i-1] - a12[,i-1])
      a13[,i] = a13[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b13[,i-1] - a13[,i-1])
      a14[,i] = a14[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b14[,i-1] - a14[,i-1])
      a15[,i] = a15[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b15[,i-1] - a15[,i-1])
      
      a21[,i] = a21[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b21[,i-1] - a21[,i-1])
      a22[,i] = a22[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b22[,i-1] - a22[,i-1])
      a23[,i] = a23[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b23[,i-1] - a23[,i-1])
      a24[,i] = a24[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b24[,i-1] - a24[,i-1])
      a25[,i] = a25[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b25[,i-1] - a25[,i-1])
      
      a31[,i] = a31[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b31[,i-1] - a31[,i-1])
      a32[,i] = a32[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b32[,i-1] - a32[,i-1])
      a33[,i] = a33[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b33[,i-1] - a33[,i-1])
      a34[,i] = a34[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b34[,i-1] - a34[,i-1])
      a35[,i] = a35[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b35[,i-1] - a35[,i-1])
      
      a41[,i] = a41[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b41[,i-1] - a41[,i-1])
      a42[,i] = a42[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b42[,i-1] - a42[,i-1])
      a43[,i] = a43[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b43[,i-1] - a43[,i-1])
      a44[,i] = a44[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b44[,i-1] - a44[,i-1])
      a45[,i] = a45[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b45[,i-1] - a45[,i-1])
      
      a51[,i] = a51[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b51[,i-1] - a51[,i-1])
      a52[,i] = a52[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b52[,i-1] - a52[,i-1])
      a53[,i] = a53[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b53[,i-1] - a53[,i-1])
      a54[,i] = a54[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b54[,i-1] - a54[,i-1])
      a55[,i] = a55[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b55[,i-1] - a55[,i-1])
     
      a61[,i] = a61[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b61[,i-1] - a61[,i-1])
      a62[,i] = a62[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b62[,i-1] - a62[,i-1])
      a63[,i] = a63[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b63[,i-1] - a63[,i-1])
      a65[,i] = a65[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b65[,i-1] - a65[,i-1])
      
      a71[,i] = a71[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b71[,i-1] - a71[,i-1])
      a72[,i] = a72[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b72[,i-1] - a72[,i-1])
      a73[,i] = a73[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b73[,i-1] - a73[,i-1])
      a74[,i] = a74[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b74[,i-1] - a74[,i-1])
      a75[,i] = a75[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b75[,i-1] - a75[,i-1])
      
      a81[,i] = a81[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b81[,i-1] - a81[,i-1])
      a82[,i] = a82[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b82[,i-1] - a82[,i-1])
      a83[,i] = a83[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b83[,i-1] - a83[,i-1])
      a84[,i] = a84[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b84[,i-1] - a84[,i-1])
      a85[,i] = a85[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b85[,i-1] - a85[,i-1])
      
      a91[,i] = a91[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b91[,i-1] - a91[,i-1])
      a92[,i] = a92[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b92[,i-1] - a92[,i-1])
      a93[,i] = a93[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b93[,i-1] - a93[,i-1])
      a94[,i] = a94[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b94[,i-1] - a94[,i-1])
      a95[,i] = a95[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b95[,i-1] - a95[,i-1])
      
      a101[,i] = a101[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b101[,i-1] - a101[,i-1])
      a102[,i] = a102[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b102[,i-1] - a102[,i-1])
      a103[,i] = a103[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b103[,i-1] - a103[,i-1])
      a104[,i] = a104[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b104[,i-1] - a104[,i-1])
      a105[,i] = a105[,i-1] + Z1_ce*Z1_gamma_A[,i-1]*(b105[,i-1] - a105[,i-1])
      
      #Attribute values to matrix A of Area 1 industries
      A[1]  = a11[,i]
      A[11]  = a12[,i]
      A[21]  = a13[,i]
      A[31]  = a14[,i]
      A[41]  = a15[,i]
      
      A[2]  = a21[,i]
      A[12] = a22[,i]
      A[22] = a23[,i]
      A[32] = a24[,i]
      A[42] = a25[,i]
      
      A[3] = a31[,i]
      A[13] = a32[,i]
      A[23] = a33[,i]
      A[33] = a34[,i]
      A[43] = a35[,i]
      
      A[4] = a41[,i]
      A[14] = a42[,i]
      A[24] = a43[,i]
      A[34] = a44[,i]
      A[44] = a45[,i]
      
      A[5] = a51[,i]
      A[15] = a52[,i]
      A[25] = a53[,i]
      A[35] = a54[,i]
      A[45] = a55[,i]
      
      A[6] = a61[,i]
      A[16] = a62[,i]
      A[26] = a63[,i]
      A[36] = a64[,i]
      A[46] = a65[,i]
      
      A[7] = a71[,i]
      A[17] = a72[,i]
      A[27] = a73[,i]
      A[37] = a74[,i]
      A[47] = a75[,i]
      
      A[8] = a81[,i]
      A[18] = a82[,i]
      A[28] = a83[,i]
      A[38] = a84[,i]
      A[48] = a85[,i]
      
      A[9] = a91[,i]
      A[19] = a92[,i]
      A[29] = a93[,i]
      A[39] = a94[,i]
      A[49] = a95[,i]
      
      A[10] = a101[,i]
      A[20] = a102[,i]
      A[30] = a103[,i]
      A[40] = a104[,i]
      A[50] = a105[,i]

      #Speed of adjustment to CE
      Z1_gamma_A[,i] = Z1_gammaA0 + t(Z1_gammaA1[,i]) %*% (Z1_sigma[,i]*Z1_g[,i-1])
      
      #HOUSEHOLDS
      
        #Share of services in household consumption (growing) <--- JBF      
        #Z1_beta[3,i] = Z1_beta[3,i-1] + Z1_beta31*Z1_ydw[,i-1]/Z1_p[3,i-1] + Z1_beta32*Z1_ydc[,i-1]/Z1_p[3,i-1]
        # if(Z1_beta[3,i-1]< 1-Z1_beta10-0.01) {Z1_beta[3,i] = Z1_beta[3,i-1] + Z1_beta31*Z1_ydw[,i-1]/Z1_p[3,i-1] + Z1_beta32*Z1_ydc[,i-1]/Z1_p[3,i-1]
        # }else{Z1_beta[3,i] = 1-Z1_beta10-0.01}
        # 
        # #Share of agricultural goods in household consumption (shrinking) 
        # Z1_beta[2,i] = 1 - Z1_beta[1,i] - Z1_beta[3,i]
        # 
        # #Share of manufacturing goods in household consumption (constant) 
        # Z1_beta[1,i] = Z1_beta10
        
        #Total real consumption (ignoring inflation tax)
        if (i<=2) {Z1_c[,i] = Z1_alpha1*Z1_ydw[,i] + Z1_alpha2*Z1_ydc[,i] + Z1_alpha3*Z1_v[,i-1]
        }else{Z1_c[,i] = Z1_alpha1*Z1_ydw[,i]/Z1_pa[,i-1] + Z1_alpha2*Z1_ydc[,i]/Z1_pa[,i-1] + Z1_alpha3*Z1_v[,i-1]/Z1_pa[,i-1]}
      
        #Disposable income of domestic households #NEW 
        #Z1_yd[,i] = Z1_div[,i] + Z1_wb[,i] + Z1_rb[,i-1] * Z1_b_s_Z1[,i-1] + Z1_rm[,i-1] * Z1_mh[,i-1] - Z1_rh[,i-1] * Z1_lh[,i-1] - Z1_t[,i] + Z1_f_b[,i] + Z2_rb[,i-1] * Z1_b_s_Z2[,i-1] * Z2_xr[,i-1] + (Z2_xr[,i]-Z2_xr[,i-1])*(Z1_b_s_Z2[,i-1]+Z1_e_s_Z2[,i-1]) 
        if (Z1_e_s[,i-1]==0 || Z2_e_s[,i-1]==0){Z1_yd[,i] = (1-Z1_omega)*(Z1_yn[,i] - Z1_wb[,i] - Z1_af[,i] - Z1_rl[,i-1] * Z1_lf[,i-1]) + Z1_wb[,i] + Z1_rb[,i-1] * Z1_b_s_Z1[,i-1] + Z1_rm[,i-1] * Z1_mh[,i-1] - Z1_rh[,i-1] * Z1_lh[,i-1] - Z1_t[,i] + Z1_f_b[,i] + Z2_rb[,i-1] * Z1_b_s_Z2[,i-1] * Z2_xr[,i-1] + (Z2_xr[,i]-Z2_xr[,i-1])*(Z1_b_s_Z2[,i-1]+Z1_e_s_Z2[,i-1])
        }else{Z1_yd[,i] = ((1-Z1_omega)*(Z1_yn[,i] - Z1_wb[,i] - Z1_af[,i] - Z1_rl[,i-1] * Z1_lf[,i-1])*Z1_e_h_Z1[,i-1]/Z1_e_s[,i-1] + Z2_xr[,i]*(1-Z2_omega)*(Z2_yn[,i] - Z2_wb[,i] - Z2_af[,i] - Z2_rl[,i-1] * Z2_lf[,i-1])*Z1_e_s_Z2[,i-1]/Z2_e_s[,i-1]) + Z1_wb[,i] + Z1_rb[,i-1] * Z1_b_s_Z1[,i-1] + Z1_rm[,i-1] * Z1_mh[,i-1] - Z1_rh[,i-1] * Z1_lh[,i-1] - Z1_t[,i] + Z1_f_b[,i] + Z2_rb[,i-1] * Z1_b_s_Z2[,i-1] * Z2_xr[,i-1] + (Z2_xr[,i]-Z2_xr[,i-1])*(Z1_b_s_Z2[,i-1]+Z1_e_s_Z2[,i-1]) }
        
        #Disposable incomes by industry and social class
        if(Z1_va[,i]>0){
      
          #Disposable income by industry (j=1,2,3,4,5)
          Z1_yd_j[,i] = Z1_yd[,i] * Z1_va_j[,i]/Z1_va[,i]
          
          #Disposable income of workers by industry (j=1,2,3,4,5) 
          Z1_ydw_j[,i] = Z1_wb_j[,i] * (1 - Z1_theta_w[,i])     
          
          #Disposable income of capitalists by industry (j=1,2,3,4,5)
          Z1_ydc_j[,i] = Z1_yd_j[,i] - Z1_ydw_j[,i]
          
          #Total disposable income of workers
          Z1_ydw[,i] = t(Z1_ydw_j[,i]) %*% (I_col[,i])    
      
          #Total disposable income of capitalists
          Z1_ydc[,i] = t(Z1_ydc_j[,i]) %*% (I_col[,i])    
      
        }
      
        #Net wealth of households
        Z1_v[,i] = Z1_v[,i-1] + Z1_yd[,i] - Z1_c[,i]*Z1_pa[,i]        
      
      
      #PRODUCTION FIRMS
      
        #Final demand vector in real terms 
        Z1_d[,i] = Z1_beta[,i]*Z1_c[,i] + Z1_sigma[,i]*Z1_g[,i] + Z1_iota[,i]*Z1_id[,i] + Z2_eta_rev[,i]*Z1_rex[,i] - Z1_eta[,i]*Z1_imp[,i] 
        Z1_fd[,i] = sum(Z1_d[,i])
      
        #Real output
        Z1_x[,i] = solve(I-A) %*% Z1_d[,i]
        
        #Value of gross output
        Z1_y[,i] = t(Z1_p[,i]) %*% Z1_x[,i]   
      
        #Net income (= total value added) net of VAT and tariffs #NEW
        Z1_yn[,i] = Z1_c[,i]*Z1_pa[,i] + Z1_id[,i]*Z1_pid[,i] + Z1_g[,i]*Z1_pg[,i] + Z1_nex[,i] - Z1_nimp[,i] - Z1_vat_rev[,i] - Z2_xr[,i]*Z2_tar_rev[,i]
        
        #Corporate profit
        Z1_f_f[,i] = Z1_yn[,i] - Z1_wb[,i] - Z1_af[,i] - Z1_rl[,i-1] * Z1_lf[,i-1] 
        
        #Retained profit
        Z1_f_f_u[,i] = Z1_omega*Z1_f_f[,i] #NEW
        
        #Value added in manufacturing industry 
        Z1_va1[,i] = x[1,i]*Z1_p[1,i] - ( x[1,i]*a11[,i]*Z1_p[1,i]
                                         +x[1,i]*a21[,i]*Z1_p[2,i]
                                         +x[1,i]*a31[,i]*Z1_p[3,i]
                                         +x[1,i]*a41[,i]*Z1_p[4,i]
                                         +x[1,i]*a51[,i]*Z1_p[5,i]
                                         +x[1,i]*a61[,i]*Z2_xr[,i]*Z2_p[6,i]
                                         +x[1,i]*a71[,i]*Z2_xr[,i]*Z2_p[7,i]
                                         +x[1,i]*a81[,i]*Z2_xr[,i]*Z2_p[8,i]
                                         +x[1,i]*a91[,i]*Z2_xr[,i]*Z2_p[9,i]
                                         +x[1,i]*a101[,i]*Z2_xr[,i]*Z2_p[10,i])
        
        #Value added in agriculture 
        Z1_va2[,i] = x[2,i]*Z1_p[2,i] - ( x[2,i]*a12[,i]*Z1_p[1,i]
                                         +x[2,i]*a22[,i]*Z1_p[2,i]
                                         +x[2,i]*a32[,i]*Z1_p[3,i]
                                         +x[2,i]*a42[,i]*Z1_p[4,i]
                                         +x[2,i]*a52[,i]*Z1_p[5,i]
                                         +x[2,i]*a62[,i]*Z2_xr[,i]*Z2_p[6,i]
                                         +x[2,i]*a72[,i]*Z2_xr[,i]*Z2_p[7,i]
                                         +x[2,i]*a82[,i]*Z2_xr[,i]*Z2_p[8,i]
                                         +x[2,i]*a92[,i]*Z2_xr[,i]*Z2_p[9,i]
                                         +x[2,i]*a102[,i]*Z2_xr[,i]*Z2_p[10,i])
        
        #Value added in services 
        Z1_va3[,i] = x[3,i]*Z1_p[3,i] - ( x[3,i]*a13[,i]*Z1_p[1,i]
                                         +x[3,i]*a23[,i]*Z1_p[2,i]
                                         +x[3,i]*a33[,i]*Z1_p[3,i]
                                         +x[3,i]*a43[,i]*Z1_p[4,i]
                                         +x[3,i]*a53[,i]*Z1_p[5,i]
                                         +x[3,i]*a63[,i]*Z2_xr[,i]*Z2_p[6,i]
                                         +x[3,i]*a73[,i]*Z2_xr[,i]*Z2_p[7,i]
                                         +x[3,i]*a83[,i]*Z2_xr[,i]*Z2_p[8,i]
                                         +x[3,i]*a93[,i]*Z2_xr[,i]*Z2_p[9,i]
                                         +x[3,i]*a103[,i]*Z2_xr[,i]*Z2_p[10,i])
        
        #Value added in waste management industry 
        Z1_va4[,i] = x[4,i]*Z1_p[4,i] - ( x[4,i]*a14[,i]*Z1_p[1,i]
                                         +x[4,i]*a24[,i]*Z1_p[2,i]
                                         +x[4,i]*a34[,i]*Z1_p[3,i]
                                         +x[4,i]*a44[,i]*Z1_p[4,i]
                                         +x[4,i]*a54[,i]*Z1_p[5,i]
                                         +x[4,i]*a64[,i]*Z2_xr[,i]*Z2_p[6,i]
                                         +x[4,i]*a74[,i]*Z2_xr[,i]*Z2_p[7,i]
                                         +x[4,i]*a84[,i]*Z2_xr[,i]*Z2_p[8,i]
                                         +x[4,i]*a94[,i]*Z2_xr[,i]*Z2_p[9,i]
                                         +x[4,i]*a104[,i]*Z2_xr[,i]*Z2_p[10,i])
        
        #Value added in recycling industry 
        Z1_va5[,i] = x[5,i]*Z1_p[5,i] - ( x[5,i]*a15[,i]*Z1_p[1,i]
                                         +x[5,i]*a25[,i]*Z1_p[2,i]
                                         +x[5,i]*a35[,i]*Z1_p[3,i]
                                         +x[5,i]*a45[,i]*Z1_p[4,i]
                                         +x[5,i]*a55[,i]*Z1_p[5,i]
                                         +x[5,i]*a65[,i]*Z2_xr[,i]*Z2_p[6,i]
                                         +x[5,i]*a75[,i]*Z2_xr[,i]*Z2_p[7,i]
                                         +x[5,i]*a85[,i]*Z2_xr[,i]*Z2_p[8,i]
                                         +x[5,i]*a95[,i]*Z2_xr[,i]*Z2_p[9,i]
                                         +x[5,i]*a105[,i]*Z2_xr[,i]*Z2_p[10,i])
        
        #Value added by industry
        Z1_va_j[1,i] = Z1_va1[,i]
        Z1_va_j[2,i] = Z1_va2[,i]
        Z1_va_j[3,i] = Z1_va3[,i]
        Z1_va_j[4,i] = Z1_va4[,i]
        Z1_va_j[5,i] = Z1_va5[,i]
        
        #Total value added of domestic industry 
        Z1_va[,i] = Z1_va1[,i] + Z1_va2[,i] + Z1_va3[,i] + Z1_va4[,i] + Z1_va5[,i]
        
      
        #INVESTMENT: PURCHASE OF NEW FIXED CAPITAL
        
        #Target fixed capital #as ratio to outputs (alternative calculations) <--- JBF
        #if(i<=5){Z1_kt[,i] = 0}
        #else{Z1_kt[,i] = (t(Z1_p[,i-1]) %*% (Z1_kappa[,i] * Z1_x[,i-1])) / Z1_pid[,i-1]}
        Z1_kt[,i] = (t(Z1_p[,i-1]) %*% (Z1_kappa[,i] * (Z1_x[,i-1]+Z2_x[,i-1]))) / Z1_pid[,i-1]
        
        #Real investment in fixed capital
        Z1_id[,i] = Z1_gamma*(Z1_kt[,i] - Z1_k[,i-1]) + Z1_da[,i] + Z1_id0[,i]
        # Z1_gdef[,i] = Z1_g[,i]*Z1_pg[,i] - Z1_t[,i] + Z1_rb[,i-1] * Z1_b_s[,i-1] - Z1_f_cb[,i] - Z1_vat_rev[,i] - Z1_tar_rev[,i]
        # Z1_g[,i]= Z1_g[,i-1]*(1 + Z1_g_g) + Z1_g0[,i]
        # Z1_g[, i] = Z1_ggg * (0.8 - Z1_bs[,i -1] / Z1_va[,i -1]) + Z1_g0[,i]
        
        #Depreciation allowances in real terms 
        Z1_da[,i] = Z1_delta * Z1_k[,i-1]
        
        #Fixed capital stock
        Z1_k[,i] = Z1_k[,i-1] + Z1_id[,i] - Z1_da[,i]
        
        #Amortization funds
        Z1_af[,i] = Z1_da[,i] * Z1_pid[,i-1] - Z1_k[,i]*(Z1_pid[,i]-Z1_pid[,i-1])
        
        #Corporate demand for bank loans
        Z1_lf[,i] = Z1_lf[,i-1] + Z1_id[,i] * Z1_pid[,i] - Z1_af[,i] - Z1_f_f_u[,i] - (Z1_e_s[,i]-Z1_e_s[,i-1]) #NEW
      
        #Quantity of shares issued by private firms of Area 1
        Z1_e_s[,i] = Z1_e_h_Z1[,i] + Z2_xr[,i]*Z2_e_h_Z1[,i]  #NEW
        
        #Supply of Z1 shares to Z2's households
        Z2_e_s_Z1[,i] = Z2_e_h_Z1[,i] * Z2_xr[,i] #NEW
        
        #Percentage return rate on shares issued by private firms of Area 1
        Z1_r_e[,i] = (1-Z1_omega)*Z1_f_f[,i]/Z1_e_s[,i]  #NEW
        
        #Dividends obtained by households of Area 1 #NEW_NEW
        if (Z1_e_s[,i-1]==0 || Z2_e_s[,i-1]==0){Z1_div[,i] = (1-Z1_omega)*(Z1_yn[,i] - Z1_wb[,i] - Z1_af[,i] - Z1_rl[,i-1] * Z1_lf[,i-1])
        }else{Z1_div[,i] = (1-Z1_omega)*(Z1_yn[,i] - Z1_wb[,i] - Z1_af[,i] - Z1_rl[,i-1] * Z1_lf[,i-1])*Z1_e_h_Z1[,i-1]/Z1_e_s[,i-1] + (1-Z2_omega)*(Z2_yn[,i] - Z2_wb[,i] - Z2_af[,i] - Z2_rl[,i-1] * Z2_lf[,i-1])*Z1_e_h_Z2[,i-1]/Z2_e_s[,i-1]}
        
        
      #COMMERCIAL BANKS
        
        #Supply of bank loans #NEW
        Z1_ls[,i] = Z1_lf[,i] + Z1_lh[,i]
        
        #Supply of bank deposits (on demand)
        Z1_ms[,i] = Z1_mh[,i]   
        
        #Holdings of bills and demand for advances
        if(is.na(Z1_ms[,i]) | is.na(Z1_ls[,i])) stop('Model Diverged')
        if (Z1_ms[,i] > Z1_ls[,i]){
                             Z1_b_b[,i] = Z1_ms[,i] - Z1_ls[,i] 
                             Z1_a_d[,i] = 0 } else{
                             Z1_a_d[,i] = Z1_ls[,i] - Z1_ms[,i] 
                             Z1_b_b[,i] = 0 }
        
        #Bank profit #NEW
        Z1_f_b[,i] = Z1_rl[,i-1] * Z1_lf[,i-1] + Z1_rb[,i-1] * Z1_b_b[,i-1] - Z1_rm[,i-1] * Z1_ms[,i-1] + Z1_rh[,i-1] * Z1_lh[,i-1]
        
        
      #GOVERNMENT AND CENTRAL BANK
      
        #Government spending 
        Z1_g[,i]= Z1_g[,i-1]*(1 + Z1_g_g) + Z1_g0[,i]
        
        #Income tax payments --- CHANGED ON 08/08/2023
        Z1_t[,i] = Z1_theta_w[,i]*Z1_wb[,i] + Z1_theta_c[,i]*(Z1_div[,i] + Z1_rb[,i-1] * Z1_b_s_Z1[,i-1] +  Z1_rm[,i-1] * Z1_mh[,i-1] + Z2_rb[,i-1] * Z1_b_s_Z2[,i-1] * Z2_xr[,i-1])
        
        #Income tax payment by industry (j=1,2,3,4,5)
        Z1_t_j[,i] = Z1_t[,i] * Z1_va_j[,i]/Z1_va[,i]
      
        #Total VAT revenue #NEW
        Z1_vat_rev[,i] = t((Z1_p[,i] * Z1_vat[,i]) / (I_col[,i] + Z1_vat[,i])) %*% (Z1_beta[,i]*Z1_c[,i])
        
        #Total import tariffs revenue #NEW <--- JBF/OVC 12/2/24 taking into account prices are always unity
        #Z1_tar_rev[,i] = t((Z2_xr[,i] * Z2_p[,i] * Z1_tar[,i]) / (I_col[,i] + Z1_tar[,i])) %*% (Z1_eta[,i]*Z1_imp[,i])
        # Z1_tar_rev[,i] = t((Z2_xr[,i] * Z2_p[,i] * Z1_tar[,i]) / (I_col[,i] + Z1_tar[,i])) %*% (Z1_eta[,i]*Z1_tot_imp[,i])
        Z1_tar_rev[,i] = t((Z2_xr[,i] * I_col[,i] * Z1_tar[,i]) / (I_col[,i] + Z1_tar[,i])) %*% (Z1_eta[,i]*Z1_tot_imp[,i])
        
        #Government deficit #NEW
        Z1_gdef[,i] = Z1_g[,i]*Z1_pg[,i] - Z1_t[,i] + Z1_rb[,i-1] * Z1_b_s[,i-1] - Z1_f_cb[,i] - Z1_vat_rev[,i] - Z1_tar_rev[,i]
        
        #Supply of government bills (government debt)
        Z1_b_s[,i] = Z1_b_s[,i-1] + Z1_gdef[,i]

        #CB advances: supply
        Z1_a_s[,i] = Z1_a_d[,i]
      
        #Supply of cash money
        Z1_h_s[,i] = Z1_h_h[,i] #- Z1_a_s[,i]  
     
        #Price of foreign reserves 
        #Z1_p_or[,i] = Z2_p_or[,i] * Z2_xr[,i]
        
        #Supply of Z1 bills to Z1's households
        Z1_b_s_Z1[,i] = Z1_b_h_Z1[,i]
        
        #Supply of Z1 bills to Z2's households
        Z2_b_s_Z1[,i] = Z2_b_h_Z1[,i] * Z2_xr[,i]
        
        #Central bank profit
        Z1_f_cb[,i] = Z1_rb[,i-1] * Z1_b_cb[,i-1] + Z2_rb[,i-1] * Z1_b_cb_s_Z2[,i-1] * Z2_xr[,i-1]
        
        #Z2 bills held by Z1 central bank - Note: unnecessary equation 
        Z1_b_cb_d_Z2[,i] = Z1_b_cb_s_Z2[,i] * Z2_xr[,i]
        
        #Interest rate on Z1 area deposits
        Z1_rm[,i] = Z1_r_star[,i] + Z1_mu_m[,i]  #NEW
        
        #Interest rate on Z1 area bills
        Z1_rb[,i] = Z1_r_star[,i] + Z1_mu_b[,i]  #NEW
        
        #Interest rate on Z1 area loans to firms
        Z1_rl[,i] = Z1_r_star[,i] + Z1_mu_l[,i]  #NEW
        
        #Interest rate on Z1 area loans to households
        Z1_rh[,i] = Z1_r_star[,i] + Z1_mu_h[,i]  #NEW
        
        
      #LABOUR MARKET
        
        #Employment generated by domestic final demand
        Z1_n[,i] = t(Z1_x[,i]) %*% (I_col[,i]/Z1_pr[,i])   
        
        #Employment generated by domestic final demand by domestic (rows 1 to 5) and foreign (rows 6 to 10) industry
        Z1_n_j[,i] = Z1_x[,i]/Z1_pr[,i]
        
        #Female employment by domestic industry <--- JBF
        #Z1_nf_j[,i] = Z1_n_j[,i] * (Z1_rho[,i] + Z2_rho[,i])
        Z1_nf_j[,i] = (Z1_n_j[,i] + Z2_n_j[,i]) * Z1_rho[,i]    
        
        #Wage bill by domestic industry <--- JBF
        #Z1_wb_j[,i] = Z1_w[,i] * Z1_n_j[,i]    
        Z1_wb_j[,i] = Z1_w[,i] * Z1_n_j[,i] + Z1_w[,i] * Z2_n_j[,i]   
        
        #Total wage bill (before taxes) <--- JBF
        #Z1_wb[,i] = t(Z1_n_j[,i]) %*% (Z1_w[,i]*I_col[,i])
        #Z1_wb[,i] = t(Z1_n_j[,i]) %*% Z1_w[,i] #NEW
        Z1_wb[,i] = t(Z1_n_j[,i]) %*% Z1_w[,i] + t(Z2_n_j[,i]) %*% Z1_w[,i] #NEW
        
      #PORTFOLIO CHOICES OF DOMESTIC HOUSEHOLDS
      
        #Z1's domestic households demand for Z1 bills
        Z1_b_h_Z1[,i] = Z1_lambda10*Z1_v[,i] + Z1_lambda11*Z1_rb[,i-1]*Z1_v[,i] - Z1_lambda12*(Z2_rb[,i-1]+(Z2_xr[,i]-Z2_xr[,i-1])/Z2_xr[,i-1])*Z1_v[,i] - Z1_lambda13*Z1_rm[,i-1]*Z1_v[,i] - Z1_lambda14*Z1_yd[,i] #- Z1_lambda15*Z1_r_e[,i] - Z1_lambda16*(Z2_r_e[,i]+(Z2_xr[,i]-Z2_xr[,i-1])/Z2_xr[,i-1])   #NEW
        
        #Z1's domestic households demand for Z2 bills
        Z1_b_h_Z2[,i] = Z1_lambda20*Z1_v[,i] - Z1_lambda21*Z1_rb[,i-1]*Z1_v[,i] + Z1_lambda22*(Z2_rb[,i-1]+(Z2_xr[,i]-Z2_xr[,i-1])/Z2_xr[,i-1])*Z1_v[,i] - Z1_lambda23*Z1_rm[,i-1]*Z1_v[,i] - Z1_lambda24*Z1_yd[,i] #- Z1_lambda25*Z1_r_e[,i] - Z1_lambda26*(Z2_r_e[,i]+(Z2_xr[,i]-Z2_xr[,i-1])/Z2_xr[,i-1]) #NEW  
        
        #Z1's domestic households demand for Z1 shares (to be expanded including other parameters)
        Z1_e_h_Z1[,i] = Z1_lambda30*Z1_v[,i] # +...  #NEW
        
        #Z1's domestic households demand for Z2 shares (to be expanded including other parameters)
        Z1_e_h_Z2[,i] = Z1_lambda40*Z1_v[,i] # +...  #NEW
        
        #Cash held by domestic households
        Z1_h_h[,i] = Z1_lambdac * Z1_c[,i-1] * Z1_pa[,i-1]       
      
        #Households demand for personal loans #NEW --- CHANGED ON 10/08/2023
        Z1_lh[,i] = Z1_lh[,i-1] * (1-Z1_rep) + max(Z1_c[,i]*Z1_pa[,i]-Z1_yd[,i], Z1_psi * ( t(Z1_p[,i]) %*% Z1_dc[,i] - ( t(Z1_p[,i-1]) %*% Z1_dc[,i-1] ) ) )       
      
        #Domestic households demand for deposits #NEW
        Z1_mh[,i] = Z1_v[,i] - Z1_b_h_Z1[,i] - Z1_h_h[,i] - Z1_b_h_Z2[,i] - Z1_e_h_Z1[,i] - Z1_e_h_Z2[,i] + Z1_lh[,i]
      
        
      #PRICES AND PRODUCTION FUNCTION 
      
        ## Reproduction prices including capital depreciation 
        # for(iter in 1 : 10){
        # Z1_p_t[1,i]  =  ((                                Z1_p_t[2,i]*a21[,i]+Z1_p_t[3,i]*a31[,i]+Z1_p_t[4,i]*a41[,i]+Z1_p_t[5,i]*a51[,i]+
        #                    Z2_xr[,i]*(Z2_p_t[6,i]*a61[,i]+Z2_p_t[7,i]*a71[,i]+Z2_p_t[8,i]*a81[,i]+Z2_p_t[9,i]*a91[,i]+Z2_p_t[10,i]*a101[,i]) )*(1+Z1_mu[1,i])*(1 + Z1_kappa[1,i]*Z1_delta) + Z1_w[1,i]/Z1_pr[1,i]) /(1 - a11[,i]*(1+Z1_mu[1,i])*(1 + Z1_kappa[1,i]*Z1_delta))
        # 
        # Z1_p_t[2,i]  =  ((            Z1_p_t[1,i]*a12[,i]                    +Z1_p_t[3,i]*a32[,i]+Z1_p_t[4,i]*a42[,i]+Z1_p_t[5,i]*a52[,i]+
        #                    Z2_xr[,i]*(Z2_p_t[6,i]*a62[,i]+Z2_p_t[7,i]*a72[,i]+Z2_p_t[8,i]*a82[,i]+Z2_p_t[9,i]*a92[,i]+Z2_p_t[10,i]*a102[,i]) )*(1+Z1_mu[2,i])*(1 + Z1_kappa[2,i]*Z1_delta) + Z1_w[2,i]/Z1_pr[2,i]) /(1 - a22[,i]*(1+Z1_mu[2,i])*(1 + Z1_kappa[2,i]*Z1_delta))
        # 
        # Z1_p_t[3,i]  =  ((            Z1_p_t[1,i]*a13[,i]+Z1_p_t[2,i]*a23[,i]                    +Z1_p_t[4,i]*a43[,i]+Z1_p_t[5,i]*a53[,i]+
        #                    Z2_xr[,i]*(Z2_p_t[6,i]*a63[,i]+Z2_p_t[7,i]*a73[,i]+Z2_p_t[8,i]*a83[,i]+Z2_p_t[9,i]*a93[,i]+Z2_p_t[10,i]*a103[,i]) )*(1+Z1_mu[3,i])*(1 + Z1_kappa[3,i]*Z1_delta) + Z1_w[3,i]/Z1_pr[3,i]) /(1 - a33[,i]*(1+Z1_mu[3,i])*(1 + Z1_kappa[3,i]*Z1_delta))
        # 
        # Z1_p_t[4,i]  =  ((            Z1_p_t[1,i]*a14[,i]+Z1_p_t[2,i]*a24[,i]+Z1_p_t[3,i]*a34[,i]                    +Z1_p_t[5,i]*a54[,i]+                      
        #                    Z2_xr[,i]*(Z2_p_t[6,i]*a64[,i]+Z2_p_t[7,i]*a74[,i]+Z2_p_t[8,i]*a84[,i]+Z2_p_t[9,i]*a94[,i]+Z2_p_t[10,i]*a104[,i]) )*(1+Z1_mu[4,i])*(1 + Z1_kappa[4,i]*Z1_delta) + Z1_w[4,i]/Z1_pr[4,i]) /(1 - a44[,i]*(1+Z1_mu[4,i])*(1 + Z1_kappa[4,i]*Z1_delta))
        # 
        # Z1_p_t[5,i]  =  ((            Z1_p_t[1,i]*a15[,i]+Z1_p_t[2,i]*a25[,i]+Z1_p_t[3,i]*a35[,i]+Z1_p_t[4,i]*a45[,i]+                      
        #                    Z2_xr[,i]*(Z2_p_t[6,i]*a65[,i]+Z2_p_t[7,i]*a75[,i]+Z2_p_t[8,i]*a85[,i]+Z2_p_t[9,i]*a95[,i]+Z2_p_t[10,i]*a105[,i]) )*(1+Z1_mu[5,i])*(1 + Z1_kappa[5,i]*Z1_delta) + Z1_w[5,i]/Z1_pr[5,i]) /(1 - a55[,i]*(1+Z1_mu[5,i])*(1 + Z1_kappa[5,i]*Z1_delta) )
        # }
        
        #Vector of domestic potential outputs (note: matter and energy to be introduced later on)  
        Z1_x_star[,i] = Z1_pr[,i]*Z1_pop_j[,i] #pmin( pr[,i]*pop_j[,i], ...  )
        
        
        #Vector of market prices including VAT # --- CHANGED ON 11/08/2023  <--- JBF
        #Z1_p[,i] = (Z1_p_t[,i] + Z1_gamma_x[,i]*( Z1_x[,i-1] - Z1_x_star[,i-1]))*(1+Z1_vat[,i]+Z2_tar[,i])
        #Z1_p[,i] = (Z1_p_t[,i] + Z1_gamma_x[,i]*( x[,i]*Z1_unitary[,i] - Z1_x_star[,i-1]))*(1+Z1_vat[,i]+Z2_tar[,i])
        #x_star[,i]= Z1_x_star[,i] + Z2_x_star[,i]
        #Z1_p[,i] = (Z1_p_t[,i] + Z1_gamma_x[,i]*Z1_unitary[,i]*(x[,i] - x_star[,i-1]))*(1+Z1_vat[,i]+Z2_tar[,i])
        Z1_p[,i] = Z1_p_t[,i] # *(1+Z1_vat[,i]+Z2_tar[,i])
        
        #Mark-up in recycling industry<- Needs to be turned off for the calibration
        #if (Z1_ce==1){ Z1_mu[5,i] = Z1_mu[5,i-1] + Z1_gamma_mu * (((Z1_mu[1,i-1]+Z1_mu[2,i-1]+Z1_mu[3,i-1]+Z1_mu[4,i-1])/4) - Z1_mu[5,i-1])     
        #}else{ Z1_mu[5,i] = 0}
        
        #Average price of domestic consumption
        Z1_pa[,i] = t(Z1_p[,i] + Z1_vat[,i]) %*% Z1_beta[,i]
      
        #Average price of investment
        Z1_pid[,i] = t(Z1_p[,i]) %*% Z1_iota[,i]
        
        #Average price of government spending 
        Z1_pg[,i] = t(Z1_p[,i]) %*% Z1_sigma[,i]
      
        #Average price of import of Area 1 #NEW ----- CHECK TARIFFS!
        Z1_pim[,i] = t(Z1_p[,i] + Z1_tar[,i]) %*% Z1_eta[,i] * Z2_xr[,i]
        
        
      #WASTE AND EMISSIONS
        ####WASTE AND EMISSIONS####
        #Waste produced by domestic manufacturing industry (stock) net of recycling 
        #Z1_wa1[,i] = Z1_x[1,i] * Z1_zeta1 + Z1_wa1[,i-1] - Z1_x[1,i] * a51[,i] 
        
        #Waste produced by domestic agriculture (stock) net of recycling
        #Z1_wa2[,i] = Z1_x[2,i] * Z1_zeta2 + Z1_wa2[,i-1] - Z1_x[2,i] * a52[,i]
        
        #Waste produced by domestic services (stock) net of recycling
        #Z1_wa3[,i] = Z1_x[3,i] * Z1_zeta3 + Z1_wa3[,i-1] - Z1_x[3,i] * a53[,i]
        
        #Waste produced by waste management (stock) net of recycling
        #Z1_wa4[,i] = Z1_x[4,i] * Z1_zeta4 + Z1_wa4[,i-1] - Z1_x[4,i] * a54[,i]
        
        #Waste produced by domestic manufacturing industry (stock) net of recycling <--- JBF
        Z1_wa1[,i] = (Z1_x[1,i] + Z2_x[1,i]) * Z1_zeta1 + Z1_wa1[,i-1] - (Z1_x[1,i] * a51[,i] + Z1_x[1,i] * a101[,i] + Z2_x[1,i] * a51[,i] + Z2_x[1,i] * a101[,i]) 
        
        #Waste produced by domestic agriculture (stock) net of recycling <--- JBF
        Z1_wa2[,i] = (Z1_x[2,i] + Z2_x[2,i]) * Z1_zeta2 + Z1_wa2[,i-1] - (Z1_x[2,i] * a52[,i] + Z1_x[2,i] * a102[,i] + Z2_x[2,i] * a52[,i] + Z2_x[2,i] * a102[,i])
        
        #Waste produced by domestic services (stock) net of recycling <--- JBF
        Z1_wa3[,i] = (Z1_x[3,i] + Z2_x[3,i]) * Z1_zeta3 + Z1_wa3[,i-1] - (Z1_x[3,i] * a53[,i] + Z1_x[3,i] * a103[,i] + Z2_x[3,i] * a53[,i] + Z2_x[3,i] * a103[,i])
        
        #Waste produced by waste management (stock) net of recycling <--- JBF
        Z1_wa4[,i] = (Z1_x[4,i] + Z2_x[4,i]) * Z1_zeta4 + Z1_wa4[,i-1] - (Z1_x[4,i] * a54[,i] + Z1_x[4,i] * a104[,i] + Z2_x[4,i] * a54[,i] + Z2_x[4,i] * a104[,i])
        
        #Total domestic waste (stock) net of recycling
        Z1_wa[,i] = Z1_wa1[,i] + Z1_wa2[,i] + Z1_wa3[,i] + Z1_wa4[,i]
      
        #Annual CO2 emissions by domestic industry <--- JBF
        #Z1_emis_j[,i] = Z1_x[,i] * Z1_eps[,i] * Z1_beta_e
        Z1_emis_j[,i] = (Z1_x[,i] + Z2_x[,i]) * Z1_eps[,i] * Z1_beta_e
        
        #Annual CO2 emissions of the domestic economy --- CHANGED ON 08/08/2023 <--- JBF
        #Z1_emis[,i] = t(Z1_x[,i]) %*% ((I_col[,i]-Z1_eta_en[,i]) * Z1_eps[,i] * Z1_beta_e) 
        Z1_emis[,i] = t(Z1_x[,i] + Z2_x[,i]) %*% ((I_col[,i]-Z1_eta_en[,i]) * Z1_eps[,i] * Z1_beta_e) 
        
        #C02 concentration using alternative method --- CHANGED ON 01/08/2023
          
        #Cumulative CO2 emissions due to Area 1 --- CHANGED ON 01/08/2023
        Z1_co2_cum[,i] = Z1_co2_cum[,i-1] + Z1_emis[,i]
        
      
      ##########################################################################        
                
      #AUXILIARY CALCULATIONS AND DEFINITIONS <--- JBF
        
        #Interest payments on bills by industry
        Z1_int_j[,i] = Z1_va_j[,i]-Z1_yd_j[,i]-Z1_t_j[,i]  
        
        #Interest payments on bills in manufacturing industry
        Z1_int1[,i] = Z1_va1[,i]-Z1_yd1[,i]-Z1_t1[,i]
        
        #Interest payments on bills in agriculture
        Z1_int2[,i] = Z1_va2[,i]-Z1_yd2[,i]-Z1_t2[,i]
        
        #Interest payments on bills in services
        Z1_int3[,i] = Z1_va3[,i]-Z1_yd3[,i]-Z1_t3[,i]
        
        #Interest payments on bills in waste management
        Z1_int4[,i] = Z1_va4[,i]-Z1_yd4[,i]-Z1_t4[,i]
        
        #Interest payments on bills in recycling
        Z1_int5[,i] = Z1_va5[,i]-Z1_yd5[,i]-Z1_t5[,i]
        
        #Annual CO2 emissions of domestic manufacturing industry 
        Z1_emis1[,i] = Z1_emis_j[1,i] + Z2_emis_j[1,i]
        
        #Annual CO2 emissions of agriculture
        Z1_emis2[,i] = Z1_emis_j[2,i] + Z2_emis_j[2,i] 
        
        #Annual CO2 emissions of domestic services
        Z1_emis3[,i] = Z1_emis_j[3,i] + Z2_emis_j[3,i] 
        
        #Annual CO2 emissions of waste management
        Z1_emis4[,i] = Z1_emis_j[4,i] + Z2_emis_j[4,i] 
        
        #Annual CO2 emissions of recycling
        Z1_emis5[,i] = Z1_emis_j[5,i] + Z2_emis_j[5,i] 
        
        #Employment in domestic manufacturing industry 
        Z1_n1[,i] = Z1_n_j[1,i] + Z2_n_j[1,i] 
        
        #Employment in domestic agriculture
        Z1_n2[,i] = Z1_n_j[2,i] + Z2_n_j[2,i] 
        
        #Employment in domestic services
        Z1_n3[,i] = Z1_n_j[3,i] + Z2_n_j[3,i]  
        
        #Employment in domestic waste management
        Z1_n4[,i] = Z1_n_j[4,i] + Z2_n_j[4,i]
        
        #Employment in domestic waste recycling industry
        Z1_n5[,i] = Z1_ce * (Z1_n_j[5,i] + Z2_n_j[5,i])
        
        #Taxes paid by manufacturing industry
        Z1_t1[,i] = Z1_t[,i] * Z1_va1[,i]/Z1_va[,i]
        
        #Taxes paid by agriculture
        Z1_t2[,i] = Z1_t[,i] * Z1_va2[,i]/Z1_va[,i]
        
        #Taxes paid by services
        Z1_t3[,i] = Z1_t[,i] * Z1_va3[,i]/Z1_va[,i]
        
        #Taxes paid by waste management
        Z1_t4[,i] = Z1_t[,i] * Z1_va4[,i]/Z1_va[,i]
        
        #Taxes paid by waste recycling
        Z1_t5[,i] = Z1_t[,i] * Z1_va5[,i]/Z1_va[,i]
        
        #Disposable income in manufacturing industry 
        Z1_yd1[,i] = Z1_yd_j[1,i] + Z1_yd_j[6,i]
        
        #Disposable income in agriculture 
        Z1_yd2[,i] = Z1_yd_j[2,i] + Z1_yd_j[7,i] 
        
        #Disposable income in services 
        Z1_yd3[,i] = Z1_yd_j[3,i] + Z1_yd_j[8,i] 
        
        #Disposable income in waste management 
        Z1_yd4[,i] = Z1_yd_j[4,i] + Z1_yd_j[9,i] 
        
        #Disposable income in recycling 
        Z1_yd5[,i] = Z1_yd_j[5,i] + Z1_yd_j[10,i] 
        
        #Disposable income of workers in manufacturing industry 
        Z1_yd1w[,i] = Z1_ydw_j[1,i] + Z1_ydw_j[6,i]
        
        #Disposable income of capitalists in manufacturing industry 
        Z1_yd1c[,i] = Z1_ydc_j[1,i] + Z1_ydc_j[6,i] 
        
        #Disposable income of workers in agriculture 
        Z1_yd2w[,i] = Z1_ydw_j[2,i] + Z1_ydw_j[7,i] 
        
        #Disposable income of capitalists in agriculture 
        Z1_yd2c[,i] = Z1_ydc_j[2,i] + Z1_ydc_j[7,i] 
        
        #Disposable income of workers in services 
        Z1_yd3w[,i] = Z1_ydw_j[3,i] + Z1_ydw_j[8,i] 
        
        #Disposable income of capitalists in services 
        Z1_yd3c[,i] = Z1_ydc_j[3,i] + Z1_ydc_j[8,i] 
        
        #Disposable income of workers in waste management 
        Z1_yd4w[,i] = Z1_ydw_j[4,i] + Z1_ydw_j[9,i] 
        
        #Disposable income of capitalists in waste management 
        Z1_yd4c[,i] = Z1_ydc_j[4,i] + Z1_ydc_j[9,i] 
        
        #Disposable income of workers in recycling 
        Z1_yd5w[,i] = Z1_ydw_j[5,i] + Z1_ydw_j[10,i] 
        
        #Disposable income of capitalists in recycling 
        Z1_yd5c[,i] = Z1_ydc_j[5,i] + Z1_ydc_j[10,i] 
        
        #Female employees in manufacturing industry
        Z1_n1f[,i] = Z1_nf_j[1,i] + Z2_nf_j[1,i]
        
        #Female employees in agriculture
        Z1_n2f[,i] = Z1_nf_j[2,i] + Z2_nf_j[2,i] 
        
        #Female employees in services
        Z1_n3f[,i] = Z1_nf_j[3,i] + Z2_nf_j[3,i] 
        
        #Female employees in waste management
        Z1_n4f[,i] = Z1_nf_j[4,i] + Z2_nf_j[4,i]
        
        #Female employees in recycling
        Z1_n5f[,i] = Z1_nf_j[5,i] + Z2_nf_j[5,i]
        
        #Wage bill paid in manufacturing industry (before taxes)
        Z1_wb1[,i] = Z1_wb_j[1,i] + Z2_wb_j[1,i] 
        
        #Wage bill paid in agriculture (before taxes)
        Z1_wb2[,i] = Z1_wb_j[2,i] + Z2_wb_j[2,i] 
        
        #Wage bill paid in services (before taxes)
        Z1_wb3[,i] = Z1_wb_j[3,i] + Z2_wb_j[3,i] 
        
        #Wage bill paid in waste management (before taxes)
        Z1_wb4[,i] = Z1_wb_j[4,i] + Z2_wb_j[4,i] 
        
        #Wage bill paid in recycling (before taxes)
        Z1_wb5[,i] = Z1_wb_j[5,i] + Z2_wb_j[5,i]  
        
        #Industry-related demands for inputs --- CHECK AND REDEFINE
        #Z1_d11[,i] = Z1_x[1,i]*a11[,i]
        #Z1_d12[,i] = Z1_x[1,i]*a12[,i]     
        #Z1_d13[,i] = Z1_x[1,i]*a13[,i] 
        #Z1_d14[,i] = Z1_x[1,i]*a14[,i] 
        
        #Z1_d21[,i] = Z1_x[2,i]*a21[,i] 
        #Z1_d22[,i] = Z1_x[2,i]*a22[,i]
        #Z1_d23[,i] = Z1_x[2,i]*a23[,i] 
        #Z1_d24[,i] = Z1_x[2,i]*a24[,i] 
        
        #Z1_d31[,i] = Z1_x[3,i]*a31[,i] 
        #Z1_d32[,i] = Z1_x[3,i]*a32[,i]
        #Z1_d33[,i] = Z1_x[3,i]*a33[,i] 
        #Z1_d34[,i] = Z1_x[3,i]*a34[,i] 
        
        #Z1_d41[,i] = Z1_x[4,i]*a41[,i] 
        #Z1_d42[,i] = Z1_x[4,i]*a42[,i]
        #Z1_d43[,i] = Z1_x[4,i]*a43[,i] 
        #Z1_d44[,i] = Z1_x[4,i]*a44[,i] 
        
        ##########################################################################
        
        #MODEL EQUATIONS FOR AREA 2
        
        #MATRIX OF TECHNICAL COEFFICIENTS
        
        #Define technical coefficients (note: a = initial coefficients; b = target coefficients)
        a16[,i] = a16[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b16[,i-1] - a16[,i-1])
        a17[,i] = a17[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b17[,i-1] - a17[,i-1])
        a18[,i] = a18[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b18[,i-1] - a18[,i-1])
        a19[,i] = a19[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b19[,i-1] - a19[,i-1])
        a110[,i] = a110[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b110[,i-1] - a110[,i-1])
        
        a26[,i] = a26[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b26[,i-1] - a26[,i-1])
        a27[,i] = a27[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b27[,i-1] - a27[,i-1])
        a28[,i] = a28[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b28[,i-1] - a28[,i-1])
        a29[,i] = a29[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b29[,i-1] - a29[,i-1])
        a210[,i] = a210[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b210[,i-1] - a210[,i-1])
        
        a36[,i] = a36[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b36[,i-1] - a36[,i-1])
        a37[,i] = a37[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b37[,i-1] - a37[,i-1])
        a38[,i] = a38[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b38[,i-1] - a38[,i-1])
        a39[,i] = a39[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b39[,i-1] - a39[,i-1])
        a310[,i] = a310[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b310[,i-1] - a310[,i-1])
        
        a46[,i] = a46[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b46[,i-1] - a46[,i-1])
        a47[,i] = a47[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b47[,i-1] - a47[,i-1])
        a48[,i] = a48[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b48[,i-1] - a48[,i-1])
        a49[,i] = a49[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b49[,i-1] - a49[,i-1])
        a410[,i] = a410[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b410[,i-1] - a410[,i-1])
        
        a56[,i] = a56[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b56[,i-1] - a56[,i-1])
        a57[,i] = a57[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b57[,i-1] - a57[,i-1])
        a58[,i] = a58[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b58[,i-1] - a58[,i-1])
        a59[,i] = a59[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b59[,i-1] - a59[,i-1])
        a510[,i] = a510[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b510[,i-1] - a510[,i-1])
        
        a66[,i] = a66[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b66[,i-1] - a66[,i-1])
        a67[,i] = a67[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b67[,i-1] - a67[,i-1])
        a68[,i] = a68[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b68[,i-1] - a68[,i-1])
        a69[,i] = a69[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b69[,i-1] - a69[,i-1])
        a610[,i] = a610[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b610[,i-1] - a610[,i-1])
        
        a76[,i] = a76[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b76[,i-1] - a76[,i-1])
        a77[,i] = a77[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b77[,i-1] - a77[,i-1])
        a78[,i] = a78[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b78[,i-1] - a78[,i-1])
        a79[,i] = a79[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b79[,i-1] - a79[,i-1])
        a710[,i] = a710[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b710[,i-1] - a710[,i-1])
        
        a86[,i] = a86[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b86[,i-1] - a86[,i-1])
        a87[,i] = a87[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b87[,i-1] - a87[,i-1])
        a88[,i] = a88[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b88[,i-1] - a88[,i-1])
        a89[,i] = a89[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b89[,i-1] - a89[,i-1])
        a810[,i] = a810[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b810[,i-1] - a810[,i-1])
        
        a96[,i] = a96[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b96[,i-1] - a96[,i-1])
        a97[,i] = a97[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b97[,i-1] - a97[,i-1])
        a98[,i] = a98[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b98[,i-1] - a98[,i-1])
        a99[,i] = a99[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b99[,i-1] - a99[,i-1])
        a910[,i] = a910[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b910[,i-1] - a910[,i-1])
        
        a1010[,i] = a1010[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b1010[,i-1] - a1010[,i-1])
        a106[,i] = a106[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b106[,i-1] - a106[,i-1])
        a107[,i] = a107[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b107[,i-1] - a107[,i-1])
        a108[,i] = a108[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b108[,i-1] - a108[,i-1])
        a109[,i] = a109[,i-1] + Z2_ce*Z2_gamma_A[,i-1]*(b109[,i-1] - a109[,i-1])
        
        #Attribute values to matrix A of Area 2 industries
        A[51] = a16[,i]
        A[61] = a17[,i]
        A[71] = a18[,i]
        A[81] = a19[,i]
        A[91] = a110[,i]
        
        A[52] = a26[,i]
        A[62] = a27[,i]
        A[72] = a28[,i]
        A[82] = a29[,i]
        A[92] = a210[,i]
        
        A[53] = a36[,i]
        A[63] = a37[,i]
        A[73] = a38[,i]
        A[83] = a39[,i]
        A[93] = a310[,i]
        
        A[54] = a46[,i]
        A[64] = a47[,i]
        A[74] = a48[,i]
        A[84] = a49[,i]
        A[94] = a410[,i]
        
        A[55] = a56[,i]
        A[65] = a57[,i]
        A[75] = a58[,i]
        A[85] = a59[,i]
        A[95] = a510[,i]
        
        A[56] = a66[,i]
        A[66] = a67[,i]
        A[76] = a68[,i]
        A[86] = a69[,i]
        A[96] = a610[,i]
        
        A[57] = a76[,i]
        A[67] = a77[,i]
        A[77] = a78[,i]
        A[87] = a79[,i]
        A[97] = a710[,i]
        
        A[58] = a86[,i]
        A[68] = a87[,i]
        A[78] = a88[,i]
        A[88] = a89[,i]
        A[98] = a810[,i]
        
        A[59] = a96[,i]
        A[69] = a97[,i]
        A[79] = a98[,i]
        A[89] = a99[,i]
        A[99] = a910[,i]
        
        A[60] = a106[,i]
        A[70] = a107[,i]
        A[80] = a108[,i]
        A[90] = a109[,i]
        A[100] = a1010[,i]
        
        
        #Speed of adjustment to CE
        Z2_gamma_A[,i] = Z2_gammaA0 + t(Z2_gammaA1[,i]) %*% (Z2_sigma[,i]*Z2_g[,i-1])
        
        
        #HOUSEHOLDS
        
        #Share of services in household consumption (growing) <--- JBF      
        #Z2_beta[8,i] = Z2_beta[8,i-1] + Z2_beta81*Z2_ydw[,i-1]/Z2_p[8,i-1] + Z2_beta82*Z2_ydc[,i-1]/Z2_p[8,i-1]
        if(Z2_beta[8,i-1]< 1-Z2_beta60-0.01) {Z2_beta[8,i] = Z2_beta[8,i-1] + Z2_beta81*Z2_ydw[,i-1]/Z2_p[8,i-1] + Z2_beta82*Z2_ydc[,i-1]/Z2_p[8,i-1]
        }else{Z1_beta[8,i] = 1-Z2_beta60-0.01}
        
        #Share of agricultural goods in household consumption (shrinking) 
        Z2_beta[7,i] = 1 - Z2_beta[6,i] - Z2_beta[8,i]
        
        #Share of manufacturing goods in household consumption (constant) 
        Z2_beta[6,i] = Z2_beta60
        
        #Total real consumption (ignoring inflation tax)
        if (i<=2) {Z2_c[,i] = Z2_alpha1*Z2_ydw[,i] + Z2_alpha2*Z2_ydc[,i] + Z2_alpha3*Z2_v[,i-1]
        }else{Z2_c[,i] = Z2_alpha1*Z2_ydw[,i]/Z2_pa[,i-1] + Z2_alpha2*Z2_ydc[,i]/Z2_pa[,i-1] + Z2_alpha3*Z2_v[,i-1]/Z2_pa[,i-1]}
        
        #Disposable income of domestic households #NEW
        #Z2_yd[,i] = Z2_div[,i] + Z2_wb[,i] + Z2_rb[,i-1] * Z2_b_s_Z2[,i-1] + Z2_rm[,i-1] * Z2_mh[,i-1] - Z2_rh[,i-1] * Z2_lh[,i-1] - Z2_t[,i] + Z2_f_b[,i] + Z1_rb[,i-1] * Z2_b_s_Z1[,i-1] * Z1_xr[,i-1] + (Z1_xr[,i]-Z1_xr[,i-1])*(Z2_b_s_Z1[,i-1]+Z2_e_s_Z1[,i-1])
        if (Z1_e_s[,i-1]==0 || Z2_e_s[,i-1]==0){Z2_yd[,i] = (1-Z2_omega)*(Z2_yn[,i] - Z2_wb[,i] - Z2_af[,i] - Z2_rl[,i-1] * Z2_lf[,i-1]) + Z2_wb[,i] + Z2_rb[,i-1] * Z2_b_s_Z2[,i-1] + Z2_rm[,i-1] * Z2_mh[,i-1] - Z2_rh[,i-1] * Z2_lh[,i-1] - Z2_t[,i] + Z2_f_b[,i] + Z1_rb[,i-1] * Z2_b_s_Z1[,i-1] * Z1_xr[,i-1] + (Z1_xr[,i]-Z1_xr[,i-1])*(Z2_b_s_Z1[,i-1]+Z2_e_s_Z1[,i-1])
        }else{Z2_yd[,i] = ((1-Z2_omega)*(Z2_yn[,i] - Z2_wb[,i] - Z2_af[,i] - Z2_rl[,i-1] * Z2_lf[,i-1])*Z2_e_h_Z2[,i-1]/Z2_e_s[,i-1] + Z1_xr[,i]*(1-Z1_omega)*(Z1_yn[,i] - Z1_wb[,i] - Z1_af[,i] - Z1_rl[,i-1] * Z1_lf[,i-1])*Z2_e_s_Z1[,i-1]/Z1_e_s[,i-1]) + Z2_wb[,i] + Z2_rb[,i-1] * Z2_b_s_Z2[,i-1] + Z2_rm[,i-1] * Z2_mh[,i-1] - Z2_rh[,i-1] * Z2_lh[,i-1] - Z2_t[,i] + Z2_f_b[,i] + Z1_rb[,i-1] * Z2_b_s_Z1[,i-1] * Z1_xr[,i-1] + (Z1_xr[,i]-Z1_xr[,i-1])*(Z2_b_s_Z1[,i-1]+Z2_e_s_Z1[,i-1]) }
        
        #Disposable incomes by industry and social class
        if(Z2_va[,i]>0){
          
          #Disposable income by industry (j=1,2,3,4,5)
          Z2_yd_j[,i] = Z2_yd[,i] * Z2_va_j[,i]/Z2_va[,i]
          
          #Disposable income of workers by industry (j=1,2,3,4,5) --- CHANGED ON 01/08/2023
          Z2_ydw_j[,i] = Z2_wb_j[,i] * (1 - Z2_theta_w[,i])     
          
          #Disposable income of capitalists by industry (j=1,2,3,4,5)
          Z2_ydc_j[,i] = Z2_yd_j[,i] - Z2_ydw_j[,i]
          
          #Total disposable income of workers
          Z2_ydw[,i] = t(Z2_ydw_j[,i]) %*% (I_col[,i])    
          
          #Total disposable income of capitalists
          Z2_ydc[,i] = t(Z2_ydc_j[,i]) %*% (I_col[,i])    
          
        }
        
        #Net wealth of households
        Z2_v[,i] = Z2_v[,i-1] + Z2_yd[,i] - Z2_c[,i]*Z2_pa[,i]              
        
        
        #PRODUCTION FIRMS
        
        #Final demand vector in real terms 
        Z2_d[,i] = Z2_beta[,i]*Z2_c[,i] + Z2_sigma[,i]*Z2_g[,i] + Z2_iota[,i]*Z2_id[,i] + Z1_eta_rev[,i]*Z2_rex[,i] - Z2_eta[,i]*Z2_imp[,i] 
        Z2_fd[,i] = sum(Z2_d[,i])
        
        #Real output
        Z2_x[,i] = solve(I-A) %*% Z2_d[,i]
        
        #Value of gross output
        Z2_y[,i] = t(Z2_p[,i]) %*% Z2_x[,i]   
        
        #Net income (= total value added) net of VAT and tariffs #NEW
        Z2_yn[,i] = Z2_c[,i]*Z2_pa[,i] + Z2_id[,i]*Z2_pid[,i] + Z2_g[,i]*Z2_pg[,i] + Z2_nex[,i] - Z2_nimp[,i] - Z2_vat_rev[,i] - Z1_xr[,i]*Z1_tar_rev[,i]
        
        #Corporate profit
        Z2_f_f[,i] = Z2_yn[,i] - Z2_wb[,i] - Z2_af[,i] - Z2_rl[,i-1] * Z2_lf[,i-1]
        
        #Retained profit
        Z2_f_f_u[,i] = Z2_omega*Z2_f_f[,i] #NEW
        
        #Value added in manufacturing industry 
        Z2_va1[,i] = (x[6,i]*Z2_p[6,i] - Z1_xr[,i]*( x[6,i]*a16[,i]*Z1_p[1,i]
                                                   +x[6,i]*a26[,i]*Z1_p[2,i]
                                                   +x[6,i]*a36[,i]*Z1_p[3,i]
                                                   +x[6,i]*a46[,i]*Z1_p[4,i]
                                                   +x[6,i]*a56[,i]*Z1_p[5,i] )
                                                - ( x[6,i]*a66[,i]*Z2_p[6,i]
                                                   +x[6,i]*a76[,i]*Z2_p[7,i]
                                                   +x[6,i]*a86[,i]*Z2_p[8,i]
                                                   +x[6,i]*a96[,i]*Z2_p[9,i]
                                                   +x[6,i]*a106[,i]*Z2_p[10,i] ))
        
        #Value added in agriculture 
        Z2_va2[,i] = (x[7,i]*Z2_p[7,i] - Z1_xr[,i]*( x[7,i]*a17[,i]*Z1_p[1,i]
                                                   +x[7,i]*a27[,i]*Z1_p[2,i]
                                                   +x[7,i]*a37[,i]*Z1_p[3,i]
                                                   +x[7,i]*a47[,i]*Z1_p[4,i]
                                                   +x[7,i]*a57[,i]*Z1_p[5,i] )
                                                - ( x[7,i]*a67[,i]*Z2_p[6,i]
                                                   +x[7,i]*a77[,i]*Z2_p[7,i]
                                                   +x[7,i]*a87[,i]*Z2_p[8,i]
                                                   +x[7,i]*a97[,i]*Z2_p[9,i]
                                                   +x[7,i]*a107[,i]*Z2_p[10,i] ))
        
        #Value added in services 
        Z2_va3[,i] = (x[8,i]*Z2_p[8,i] - Z1_xr[,i]*( x[8,i]*a18[,i]*Z1_p[1,i]
                                                   +x[8,i]*a28[,i]*Z1_p[2,i]
                                                   +x[8,i]*a38[,i]*Z1_p[3,i]
                                                   +x[8,i]*a48[,i]*Z1_p[4,i]
                                                   +x[8,i]*a58[,i]*Z1_p[5,i] )
                                                - ( x[8,i]*a68[,i]*Z2_p[6,i]
                                                   +x[8,i]*a78[,i]*Z2_p[7,i]
                                                   +x[8,i]*a88[,i]*Z2_p[8,i]
                                                   +x[8,i]*a98[,i]*Z2_p[9,i]
                                                   +x[8,i]*a108[,i]*Z2_p[10,i]))
        

        #Value added in waste management 
        Z2_va4[,i] = (x[9,i]*Z2_p[9,i] - Z1_xr[,i]*( x[9,i]*a19[,i]*Z1_p[1,i]
                                                    +x[9,i]*a29[,i]*Z1_p[2,i]
                                                    +x[9,i]*a39[,i]*Z1_p[3,i]
                                                    +x[9,i]*a49[,i]*Z1_p[4,i]
                                                    +x[9,i]*a59[,i]*Z1_p[5,i] )
                                                 - ( x[9,i]*a69[,i]*Z2_p[6,i]
                                                    +x[9,i]*a79[,i]*Z2_p[7,i]
                                                    +x[9,i]*a89[,i]*Z2_p[8,i]
                                                    +x[9,i]*a99[,i]*Z2_p[9,i]
                                                    +x[9,i]*a109[,i]*Z2_p[10,i]))
        
        #Value added in recycling 
        Z2_va5[,i] = (x[10,i]*Z2_p[10,i] - Z1_xr[,i]*( x[10,i]*a110[,i]*Z1_p[1,i]
                                                     +x[10,i]*a210[,i]*Z1_p[2,i]
                                                     +x[10,i]*a310[,i]*Z1_p[3,i]
                                                     +x[10,i]*a410[,i]*Z1_p[4,i]
                                                     +x[10,i]*a510[,i]*Z1_p[5,i] )
                                                  - ( x[10,i]*a610[,i]*Z2_p[6,i]
                                                     +x[10,i]*a710[,i]*Z2_p[7,i]
                                                     +x[10,i]*a810[,i]*Z2_p[8,i]
                                                     +x[10,i]*a910[,i]*Z2_p[9,i]
                                                     +x[10,i]*a1010[,i]*Z2_p[10,i]))
        
        #Value added by industry
        Z2_va_j[6,i] = Z2_va1[,i]
        Z2_va_j[7,i] = Z2_va2[,i]
        Z2_va_j[8,i] = Z2_va3[,i]
        Z2_va_j[9,i] = Z2_va4[,i]
        Z2_va_j[10,i] = Z2_va5[,i]
        
        #Total value added of domestic industry 
        Z2_va[,i] = Z2_va1[,i] + Z2_va2[,i] + Z2_va3[,i] + Z2_va4[,i] + Z2_va5[,i]
        
        
        #INVESTMENT: PURCHASE OF NEW FIXED CAPITAL
        
        #Target fixed capital as ratio to outputs (alternative calculations) <--- JBF
        #if(i<=5){Z2_kt[,i] = 0}
        #else{Z2_kt[,i] = (t(Z2_p[,i-1]) %*% (Z2_kappa[,i] * Z2_x[,i-1])) / Z2_pid[,i-1]}
        Z2_kt[,i] = (t(Z2_p[,i-1]) %*% (Z2_kappa[,i] * (Z2_x[,i-1]+Z1_x[,i-1]))) / Z2_pid[,i-1]
        
        #Real investment in fixed capital
        Z2_id[,i] = Z2_gamma*(Z2_kt[,i] - Z2_k[,i-1]) + Z2_da[,i] + Z2_id0[,i]
        
        #Depreciation allowances in real terms 
        Z2_da[,i] = Z2_delta * Z2_k[,i-1]
        
        #Fixed capital stock
        Z2_k[,i] = Z2_k[,i-1] + Z2_id[,i] - Z2_da[,i]
        
        #Amortization funds
        Z2_af[,i] = Z2_da[,i] * Z2_pid[,i-1] - Z2_k[,i]*(Z2_pid[,i]-Z2_pid[,i-1])
        
        #Corporate demand for bank loans
        Z2_lf[,i] = Z2_lf[,i-1] + Z2_id[,i] * Z2_pid[,i] - Z2_af[,i] - Z2_f_f_u[,i]- (Z2_e_s[,i]-Z2_e_s[,i-1]) #NEW
        
        #Quantity of shares issued by private firms of Area 2
        Z2_e_s[,i] = Z2_e_h_Z2[,i] + Z1_xr[,i]*Z1_e_h_Z2[,i]  #NEW
        
        #Supply of Z2 shares to Z1's households
        Z1_e_s_Z2[,i] = Z1_e_h_Z2[,i] * Z1_xr[,i] #NEW
        
        #Percentage return rate on shares issued by private firms of Area 2
        Z2_r_e[,i] = (1-Z2_omega)*Z2_f_f[,i]/Z2_e_s[,i]  #NEW
        
        #Dividends obtained by households of Area 2 #NEW_NEW
        if (Z1_e_s[,i-1]==0 || Z2_e_s[,i-1]==0){Z2_div[,i] = (1-Z2_omega)*(Z2_yn[,i] - Z2_wb[,i] - Z2_af[,i] - Z2_rl[,i-1] * Z2_lf[,i-1])
        }else{Z2_div[,i] = (1-Z2_omega)*(Z2_yn[,i] - Z2_wb[,i] - Z2_af[,i] - Z2_rl[,i-1] * Z2_lf[,i-1])*Z2_e_h_Z2[,i-1]/Z2_e_s[,i-1] + (1-Z1_omega)*(Z1_yn[,i] - Z1_wb[,i] - Z1_af[,i] - Z1_rl[,i-1] * Z1_lf[,i-1])*Z2_e_h_Z1[,i-1]/Z1_e_s[,i-1]}
        
        
        #COMMERCIAL BANKS
        
        #Supply of bank loans #NEW
        Z2_ls[,i] = Z2_lf[,i] + Z2_lh[,i]
        
        #Supply of bank deposits (on demand)
        Z2_ms[,i] = Z2_mh[,i]  
        
        #Holdings of bills and demand for advances
        if(is.na(Z2_ms[,i]) | is.na(Z2_ls[,i])) stop('Model Diverged')
        if (Z2_ms[,i] > Z2_ls[,i]){
          Z2_b_b[,i] = Z2_ms[,i] - Z2_ls[,i] 
          Z2_a_d[,i] = 0 }else{
          Z2_a_d[,i] = Z2_ls[,i] - Z2_ms[,i] 
          Z2_b_b[,i] = 0 }
        
        
        #Bank profit #NEW
        Z2_f_b[,i] = Z2_rl[,i-1] * Z2_lf[,i-1] + Z2_rb[,i-1] * Z2_b_b[,i-1] - Z2_rm[,i-1] * Z2_ms[,i-1] + Z2_rh[,i-1] * Z2_lh[,i-1]
        
        
        #GOVERNMENT AND CENTRAL BANK
        
        #Government spending
        Z2_g[,i]= Z2_g[,i-1]*(1 + Z2_g_g) + Z2_g0[,i]
        
        #Income tax payments --- CHANGED ON 08/08/2023
        Z2_t[,i] = Z2_theta_w[,i]*Z2_wb[,i] + Z2_theta_c[,i]*(Z2_div[,i] + Z2_rb[,i-1] * Z2_b_s_Z2[,i-1] + Z2_rm[,i-1] * Z2_mh[,i-1] + Z1_rb[,i-1] * Z2_b_s_Z1[,i-1] * Z1_xr[,i-1])
       
        #Income tax payment by industry (j=1,2,3,4,5)
        Z2_t_j[,i] = Z2_t[,i] * Z2_va_j[,i]/Z2_va[,i]
        
        #Total VAT revenue #NEW
        Z2_vat_rev[,i] = t((Z2_p[,i] * Z2_vat[,i]) / (I_col[,i] + Z2_vat[,i])) %*% (Z2_beta[,i]*Z2_c[,i])
        
        #Total import tariffs revenue #NEW <--- JBF/OVC 12/2/24
        #Z2_tar_rev[,i] = t((Z1_xr[,i] * Z1_p[,i] * Z2_tar[,i]) / (I_col[,i] + Z2_tar[,i])) %*% (Z2_eta[,i]*Z2_imp[,i])
        # Z2_tar_rev[,i] = t((Z1_xr[,i] * Z1_p[,i] * Z2_tar[,i]) / (I_col[,i] + Z2_tar[,i])) %*% (Z2_eta[,i]*Z2_tot_imp[,i])
        Z2_tar_rev[,i] = t((Z1_xr[,i] * I_col[,i] * Z2_tar[,i]) / (I_col[,i] + Z2_tar[,i])) %*% (Z2_eta[,i]*Z2_tot_imp[,i])
        
        #Government deficit #NEW
        Z2_gdef[,i] = Z2_g[,i]*Z2_pg[,i] - Z2_t[,i] + Z2_rb[,i-1] * Z2_b_s[,i-1] - Z2_f_cb[,i] - Z2_vat_rev[,i] - Z2_tar_rev[,i]
        
        #Supply of government bills (government debt)
        Z2_b_s[,i] = Z2_b_s[,i-1] + Z2_gdef[,i]
        
        #CB advances: supply
        Z2_a_s[,i] = Z2_a_d[,i]
        
        #Supply of cash money
        Z2_h_s[,i] = Z2_h_h[,i] 
        
        #Price of foreign reserves (exogenous)
        #Z2_p_or[,i] = 1
        
        #Supply of Z2 bills to Z2's households
        Z2_b_s_Z2[,i] = Z2_b_h_Z2[,i]
        
        #Central bank profit
        Z2_f_cb[,i] = Z2_rb[,i-1] * Z2_b_cb[,i-1]
        
        #Interest rate on Z2 area deposits
        Z2_rm[,i] = Z2_r_star[,i] + Z2_mu_m[,i]  #NEW
        
        #Interest rate on Z1 area bills
        Z2_rb[,i] = Z2_r_star[,i] + Z2_mu_b[,i]  #NEW
        
        #Interest rate on Z1 area loans to firms
        Z2_rl[,i] = Z2_r_star[,i] + Z2_mu_l[,i]  #NEW
        
        #Interest rate on Z1 area loans to households
        Z2_rh[,i] = Z2_r_star[,i] + Z2_mu_h[,i]  #NEW
        
        #######################################################################
        
        
        #LABOUR MARKET
        
        #Domestic labour force by industry (to be amended later on) #NEW
        #if(i<=100){Z2_pop_j[,i] = Z2_n_j[,i]}
        #else{Z2_pop_j[,i] = Z2_pop_j[,i-1] + Z2_gamma_pop[,i]*(Z2_n_j[,i-1]-Z2_pop_j[,i-1]) }
        
        #Employment generated by domestic final demand
        Z2_n[,i] = t(Z2_x[,i]) %*% (I_col[,i]/Z2_pr[,i]) 
        
        #Employment generated by domestic final demand by foreign (rows 1 to 5) and domestic (rows 6 to 10) industry
        Z2_n_j[,i] = Z2_x[,i]/Z2_pr[,i] 
        
        #Female employment by domestic industry <--- JBF
        #Z2_nf_j[,i] = Z2_n_j[,i] * (Z2_rho[,i] + Z1_rho[,i])    
        Z2_nf_j[,i] = (Z2_n_j[,i] + Z1_n_j[,i]) * Z2_rho[,i]
        
        #Wage bill by domestic industry <--- JBF
        #Z2_wb_j[,i] = Z2_w[,i] * Z2_n_j[,i]    
        Z2_wb_j[,i] = Z2_w[,i] * Z2_n_j[,i] + Z2_w[,i] * Z1_n_j[,i]   
        
        #Total wage bill (before taxes) <--- JBF
        #Z2_wb[,i] = t(Z2_n_j[,i]) %*% (Z2_w[,i]*I_col[,i])
        #Z2_wb[,i] = t(Z2_n_j[,i]) %*% Z2_w[,i] #NEW
        Z2_wb[,i] = t(Z2_n_j[,i]) %*% Z2_w[,i] + t(Z1_n_j[,i]) %*% Z2_w[,i]#NEW
        
        #PORTFOLIO CHOICES OF DOMESTIC HOUSEHOLDS
        
        #Z2's domestic households demand for Z2 bills
        Z2_b_h_Z2[,i] = Z2_lambda10*Z2_v[,i] - Z2_lambda11*(Z1_rb[,i-1]+(Z1_xr[,i]-Z1_xr[,i-1])/Z1_xr[,i-1])*Z2_v[,i] + Z2_lambda12*Z2_rb[,i-1]*Z2_v[,i] - Z2_lambda13*Z2_rm[,i-1]*Z2_v[,i] - Z2_lambda14*Z2_yd[,i] #- Z2_lambda15*(Z1_r_e[,i]+(Z1_xr[,i]-Z1_xr[,i-1])/Z1_xr[,i-1]) - Z2_lambda16*Z2_r_e[,i]   #NEW
        
        #Z2's domestic households demand for Z1 bills
        Z2_b_h_Z1[,i] = Z2_lambda20*Z2_v[,i] + Z2_lambda21*(Z1_rb[,i-1]+(Z1_xr[,i]-Z1_xr[,i-1])/Z1_xr[,i-1])*Z2_v[,i] - Z2_lambda22*Z2_rb[,i-1]*Z2_v[,i] - Z2_lambda23*Z2_rm[,i-1]*Z2_v[,i] - Z2_lambda24*Z2_yd[,i] #- Z2_lambda25*(Z1_r_e[,i]+(Z1_xr[,i]-Z1_xr[,i-1])/Z1_xr[,i-1]) - Z2_lambda26*Z2_r_e[,i]   #NEW
        
        #Z2's domestic households demand for Z1 shares (to be expanded including other parameters)
        Z2_e_h_Z1[,i] = Z2_lambda30*Z2_v[,i] # +...  #NEW
        
        #Z2's domestic households demand for Z2 shares (to be expanded including other parameters)
        Z2_e_h_Z2[,i] = Z2_lambda40*Z2_v[,i] # +...  #NEW
        
        #Cash held by domestic households
        Z2_h_h[,i] = Z2_lambdac * Z2_c[,i-1] * Z2_pa[,i-1]       
        
        #Households demand for personal loans #NEW --- CHANGED ON 10/08/2023
        Z2_lh[,i] = Z2_lh[,i-1] * (1-Z2_rep) + max(Z2_c[,i]*Z2_pa[,i]-Z2_yd[,i], Z2_psi * ( t(Z2_p[,i]) %*% Z2_dc[,i] - ( t(Z2_p[,i-1]) %*% Z2_dc[,i-1] ) ) )
        
        #Domestic households demand for deposits
        Z2_mh[,i] = Z2_v[,i] - Z2_b_h_Z2[,i] - Z2_h_h[,i] - Z2_b_h_Z1[,i] - Z2_e_h_Z1[,i] - Z2_e_h_Z2[,i] + Z2_lh[,i] #NEW
        
        
        #PRICES AND PRODUCTION FUNCTION 
        
        ## Reproduction prices including capital depreciation 
        # for(iter in 1 : 10) {
        # Z2_p_t[6,i]  =  ((                               +Z2_p_t[7,i]*a76[,i]+Z2_p_t[8,i]*a86[,i]+Z2_p_t[9,i]*a96[,i]+Z2_p_t[10,i]*a106[,i]+
        #                    Z1_xr[,i]*(Z1_p_t[1,i]*a16[,i]+Z1_p_t[2,i]*a26[,i]+Z1_p_t[3,i]*a36[,i]+Z1_p_t[4,i]*a46[,i]+Z1_p_t[5,i]*a56[,i])  )*(1+Z2_mu[6,i])*(1 + Z2_kappa[6,i]*Z2_delta) + Z2_w[6,i]/Z2_pr[6,i]) /(1 - a66[,i]*(1+Z2_mu[6,i])*(1 + Z2_kappa[6,i]*Z2_delta) )
        # 
        # Z2_p_t[7,i]  =  ((            Z2_p_t[6,i]*a67[,i]                    +Z2_p_t[8,i]*a87[,i]+Z2_p_t[9,i]*a97[,i]+Z2_p_t[10,i]*a107[,i]+
        #                    Z1_xr[,i]*(Z1_p_t[1,i]*a17[,i]+Z1_p_t[2,i]*a27[,i]+Z1_p_t[3,i]*a37[,i]+Z1_p_t[4,i]*a47[,i]+Z1_p_t[5,i]*a57[,i])  )*(1+Z2_mu[7,i])*(1 + Z2_kappa[7,i]*Z2_delta) + Z2_w[7,i]/Z2_pr[7,i]) /(1 - a77[,i]*(1+Z2_mu[7,i])*(1 + Z2_kappa[7,i]*Z2_delta))
        # 
        # Z2_p_t[8,i]  =  ((            Z2_p_t[6,i]*a68[,i]+Z2_p_t[7,i]*a78[,i]+                   +Z2_p_t[9,i]*a98[,i]+Z2_p_t[10,i]*a108[,i]+               
        #                    Z1_xr[,i]*(Z1_p_t[1,i]*a18[,i]+Z1_p_t[2,i]*a28[,i]+Z1_p_t[3,i]*a38[,i]+Z1_p_t[4,i]*a48[,i]+Z1_p_t[5,i]*a58[,i])  )*(1+Z2_mu[8,i])*(1 + Z2_kappa[8,i]*Z2_delta) + Z2_w[8,i]/Z2_pr[8,i]) /(1 - a88[,i]*(1+Z2_mu[8,i])*(1 + Z2_kappa[8,i]*Z2_delta))
        # 
        # Z2_p_t[9,i]  =  ((            Z2_p_t[6,i]*a69[,i]+Z2_p_t[7,i]*a79[,i]+Z2_p_t[8,i]*a89[,i]+                   +Z2_p_t[10,i]*a109[,i]+               
        #                    Z1_xr[,i]*(Z1_p_t[1,i]*a19[,i]+Z1_p_t[2,i]*a29[,i]+Z1_p_t[3,i]*a39[,i]+Z1_p_t[4,i]*a49[,i]+Z1_p_t[5,i]*a59[,i])  )*(1+Z2_mu[9,i])*(1 + Z2_kappa[9,i]*Z2_delta) + Z2_w[9,i]/Z2_pr[9,i]) /(1 - a99[,i]*(1+Z2_mu[9,i]))*(1 + Z2_kappa[9,i]*Z2_delta)
        # 
        # Z2_p_t[10,i]  =  ((            Z2_p_t[6,i]*a610[,i]+Z2_p_t[7,i]*a710[,i]+Z2_p_t[8,i]*a810[,i]+Z2_p_t[9,i]*a910[,i]+               
        #                    Z1_xr[,i]*(Z1_p_t[1,i]*a110[,i]+Z1_p_t[2,i]*a210[,i]+Z1_p_t[3,i]*a310[,i]+Z1_p_t[4,i]*a410[,i]+Z1_p_t[5,i]*a510[,i])  )*(1+Z2_mu[10,i])*(1 + Z2_kappa[10,i]*Z2_delta) + Z2_w[10,i]/Z2_pr[10,i]) /(1 - a1010[,i]*(1+Z2_mu[10,i]))*(1 + Z2_kappa[10,i]*Z2_delta)
        # }
        
        #Vector of domestic potential outputs (note: matter and energy to be introduced later on)  
        Z2_x_star[,i] = Z2_pr[,i]*Z2_pop_j[,i] #pmin( pr[,i]*pop_j[,i], ...  )
        
        #Vector of market prices including VAT # --- CHANGED ON 11/08/2023 
        #Z2_p[,i] = (Z2_p_t[,i] + Z2_gamma_x[,i]*( Z2_x[,i-1] - Z2_x_star[,i-1]))*(1+Z2_vat[,i]+Z1_tar[,i])
        #Z2_p[,i] = (Z2_p_t[,i] + Z2_gamma_x[,i]*( x[,i-1]*Z2_unitary[,i] - Z2_x_star[,i-1]))*(1+Z2_vat[,i]+Z1_tar[,i])
        #Z2_p[,i] = (Z2_p_t[,i] + Z2_gamma_x[,i]*Z2_unitary[,i]*(x[,i-1] - x_star[,i-1]))*(1+Z2_vat[,i]+Z1_tar[,i])
        Z2_p[,i] = Z2_p_t[,i] # *(1+Z2_vat[,i]+Z1_tar[,i])
        
        #Mark-up in recycling industry<-Needed to be turned off for model 
        #if (Z2_ce==1){ Z2_mu[10,i] = Z2_mu[10,i-1] + Z2_gamma_mu * (((Z2_mu[6,i-1]+Z2_mu[7,i-1]+Z2_mu[8,i-1]+Z2_mu[9,i-1])/4) - Z2_mu[10,i-1])     
        #}else{ Z2_mu[10,i] = 0}
        
        #Average price of domestic consumption
        Z2_pa[,i] = t(Z2_p[,i] + Z2_vat[,i]) %*% Z2_beta[,i]
        
        #Average price of investment
        Z2_pid[,i] = t(Z2_p[,i]) %*% Z2_iota[,i]
        
        #Average price of government spending 
        Z2_pg[,i] = t(Z2_p[,i]) %*% Z2_sigma[,i]
        
        #Average price of import of Area 2
        Z2_pim[,i] = t(Z2_p[,i] + Z2_tar[,i]) %*% Z2_eta[,i] * Z1_xr[,i]
        
        #WASTE AND EMISSIONS
        
        #WASTE AND EMISSIONS
        
        #Waste produced by domestic manufacturing industry (stock) net of recycling 
        #Z2_wa1[,i] = Z2_x[6,i] + Z1_x[6,i] * Z2_zeta1 + Z2_wa1[,i-1] - Z2_x[6,i] * a106[,i] 
        
        #Waste produced by domestic agriculture (stock) net of recycling
        #Z2_wa2[,i] = Z2_x[7,i] + Z1_x[7,i] * Z2_zeta2 + Z2_wa2[,i-1] - Z2_x[7,i] * a107[,i]
        
        #Waste produced by domestic services (stock) net of recycling
        #Z2_wa3[,i] = Z2_x[8,i] + Z1_x[8,i] * Z2_zeta3 + Z2_wa3[,i-1] - Z2_x[8,i] * a108[,i]
        
        #Waste produced by waste management (stock) net of recycling
        #Z2_wa4[,i] = Z2_x[9,i] + Z1_x[9,i] * Z2_zeta4 + Z2_wa4[,i-1] - Z2_x[9,i] * a109[,i]
        
        #Waste produced by domestic manufacturing industry (stock) net of recycling <--- JBF
        Z2_wa1[,i] = (Z2_x[6,i] + Z1_x[6,i]) * Z2_zeta1 + Z2_wa1[,i-1] - (Z2_x[6,i] * a106[,i] + Z2_x[6,i] * a56[,i] + Z1_x[6,i] * a106[,i] + Z1_x[6,i] * a56[,i]) 
        
        #Waste produced by domestic agriculture (stock) net of recycling <--- JBF
        Z1_wa2[,i] = (Z1_x[2,i] + Z2_x[2,i]) * Z1_zeta2 + Z1_wa2[,i-1] - (Z2_x[7,i] * a107[,i] + Z2_x[7,i] * a57[,i] + Z1_x[7,i] * a107[,i] + Z1_x[7,i] * a57[,i])
        
        #Waste produced by domestic services (stock) net of recycling <--- JBF
        Z1_wa3[,i] = (Z1_x[3,i] + Z2_x[3,i]) * Z1_zeta3 + Z1_wa3[,i-1] - (Z2_x[8,i] * a108[,i] + Z2_x[8,i] * a58[,i] + Z1_x[8,i] * a108[,i] + Z1_x[8,i] * a58[,i])
        
        #Waste produced by waste management (stock) net of recycling <--- JBF
        Z1_wa4[,i] = (Z1_x[4,i] + Z2_x[4,i]) * Z1_zeta4 + Z1_wa4[,i-1] - (Z2_x[9,i] * a109[,i] + Z2_x[9,i] * a59[,i] + Z1_x[9,i] * a109[,i] + Z1_x[9,i] * a59[,i])
        
        #Total domestic waste (stock) net of recycling
        Z2_wa[,i] = Z2_wa1[,i] + Z2_wa2[,i] + Z2_wa3[,i] + Z2_wa4[,i]
        
        #Annual CO2 emissions by domestic industry <--- JBF
        #Z2_emis_j[,i] = Z2_x[,i] * Z2_eps[,i] * Z2_beta_e
        Z2_emis_j[,i] = (Z2_x[,i] + Z1_x[,i]) * Z2_eps[,i] * Z2_beta_e
        
        #Annual CO2 emissions of the domestic economy --- CHANGED ON 08/08/2023 <--- JBF
        #Z2_emis[,i] = t(Z2_x[,i]) %*% ((I_col[,i]-Z2_eta_en[,i]) * Z2_eps[,i] * Z2_beta_e) 
        Z2_emis[,i] = t(Z2_x[,i] + Z1_x[,i]) %*% ((I_col[,i]-Z2_eta_en[,i]) * Z2_eps[,i] * Z2_beta_e) 
        
        #C02 concentration using alternative method --- CHANGED ON 01/08/2023
        
        #Cumulative CO2 emissions in Area 2 --- CHANGED ON 01/08/2023
        Z2_co2_cum[,i] = Z2_co2_cum[,i-1] + Z2_emis[,i] 
        
        #Atmospheric temperature --- CHANGED ON 01/08/2023
        temp[,i] = (1/(1-fnc))*tcre*(Z1_co2_cum[,i] + Z2_co2_cum[,i])
        
        
        ##########################################################################        
        
        #AUXILIARY CALCULATIONS AND DEFINITIONS <--- JBF
        
        #Define global real output
        x[,i] = Z1_x[,i] + Z2_x[,i]
        Z1_go[,i] = sum(x[1 : 5, i])
        Z2_go[,i] = sum(x[6 : 10, i])
        
        #Define global real demand
        d[,i] = Z1_d[,i] + Z2_d[,i]
        
        #Interest payments on bills by industry
        Z2_int_j[,i] = Z2_va_j[,i]-Z2_yd_j[,i]-Z2_t_j[,i]  
        
        #Interest payments on bills in manufacturing industry
        Z2_int1[,i] = Z2_va1[,i]-Z2_yd1[,i]-Z2_t1[,i]
        
        #Interest payments on bills in agriculture
        Z2_int2[,i] = Z2_va2[,i]-Z2_yd2[,i]-Z2_t2[,i]
        
        #Interest payments on bills in services
        Z2_int3[,i] = Z2_va3[,i]-Z2_yd3[,i]-Z2_t3[,i]
        
        #Interest payments on bills in waste management
        Z2_int4[,i] = Z2_va4[,i]-Z2_yd4[,i]-Z2_t4[,i]
        
        #Interest payments on bills in recycling
        Z2_int5[,i] = Z2_va5[,i]-Z2_yd5[,i]-Z2_t5[,i]
        
        #Annual CO2 emissions of domestic manufacturing industry 
        Z2_emis1[,i] = Z2_emis_j[6,i] + Z1_emis_j[6,i] 
        
        #Annual CO2 emissions of agriculture
        Z2_emis2[,i] = Z2_emis_j[7,i] + Z1_emis_j[7,i] 
        
        #Annual CO2 emissions of domestic services
        Z2_emis3[,i] = Z2_emis_j[8,i] + Z1_emis_j[8,i] 
        
        #Annual CO2 emissions of waste management
        Z2_emis4[,i] = Z2_emis_j[9,i] + Z1_emis_j[9,i] 
        
        #Annual CO2 emissions of recycling
        Z2_emis5[,i] = Z2_emis_j[10,i] + Z1_emis_j[10,i] 
        
        #Employment in domestic manufacturing industry 
        Z2_n1[,i] = Z2_n_j[6,i] + Z1_n_j[6,i] 
        
        #Employment in domestic agriculture
        Z2_n2[,i] = Z2_n_j[7,i] + Z1_n_j[7,i] 
        
        #Employment in domestic services
        Z2_n3[,i] = Z2_n_j[8,i] + Z1_n_j[8,i] 
        
        #Employment in waste management
        Z2_n4[,i] = Z2_n_j[9,i] + Z1_n_j[9,i] 
        
        #Employment in domestic recycling
        Z2_n5[,i] = Z2_ce * (Z2_n_j[10,i] + Z1_n_j[10,i])
        
        #Taxes paid by manufacturing industry
        Z2_t1[,i] = Z2_t[,i] * Z2_va1[,i]/Z2_va[,i]
        
        #Taxes paid by agriculture
        Z2_t2[,i] = Z2_t[,i] * Z2_va2[,i]/Z2_va[,i]
        
        #Taxes paid by services
        Z2_t3[,i] = Z2_t[,i] * Z2_va3[,i]/Z2_va[,i]
        
        #Taxes paid by waste management
        Z2_t4[,i] = Z2_t[,i] * Z2_va4[,i]/Z2_va[,i]
        
        #Taxes paid by recycling
        Z2_t5[,i] = Z2_t[,i] * Z2_va5[,i]/Z2_va[,i]
        
        #Disposable income in manufacturing industry 
        Z2_yd1[,i] = Z2_yd_j[6,i] + Z2_yd_j[1,i] 
        
        #Disposable income in agriculture 
        Z2_yd2[,i] = Z2_yd_j[7,i] + Z2_yd_j[2,i] 
        
        #Disposable income in services 
        Z2_yd3[,i] = Z2_yd_j[8,i] + Z2_yd_j[3,i] 
        
        #Disposable income in waste management 
        Z2_yd4[,i] = Z2_yd_j[9,i] + Z2_yd_j[4,i] 
        
        #Disposable income in recycling 
        Z2_yd5[,i] = Z2_yd_j[10,i] + Z2_yd_j[5,i] 
        
        #Disposable income of workers in manufacturing industry 
        Z2_yd1w[,i] = Z2_ydw_j[6,i] + Z2_ydw_j[1,i] 
        
        #Disposable income of capitalists in manufacturing industry 
        Z2_yd1c[,i] = Z2_ydc_j[6,i] + Z2_ydc_j[1,i] 
        
        #Disposable income of workers in agriculture 
        Z2_yd2w[,i] = Z2_ydw_j[7,i] + Z2_ydw_j[2,i] 
        
        #Disposable income of capitalists in agriculture 
        Z2_yd2c[,i] = Z2_ydc_j[7,i] + Z2_ydc_j[2,i] 
        
        #Disposable income of workers in services 
        Z2_yd3w[,i] = Z2_ydw_j[8,i] + Z2_ydw_j[3,i] 
        
        #Disposable income of capitalists in services 
        Z2_yd3c[,i] = Z2_ydc_j[8,i] + Z2_ydc_j[3,i] 
        
        #Disposable income of workers in waste management 
        Z2_yd4w[,i] = Z2_ydw_j[9,i] + Z2_ydw_j[4,i] 
        
        #Disposable income of capitalists in waste management 
        Z2_yd4c[,i] = Z2_ydc_j[9,i] + Z2_ydc_j[4,i] 
        
        #Disposable income of workers in recycling 
        Z2_yd5w[,i] = Z2_ydw_j[10,i] + Z2_ydw_j[5,i] 
        
        #Disposable income of capitalists in recycling 
        Z2_yd5c[,i] = Z2_ydc_j[10,i] + Z2_ydc_j[5,i] 
        
        #Female employees in manufacturing industry
        Z2_n1f[,i] = Z2_nf_j[6,i] + Z1_nf_j[6,i] 
        
        #Female employees in agriculture
        Z2_n2f[,i] = Z2_nf_j[7,i] + Z1_nf_j[7,i]
        
        #Female employees in services
        Z2_n3f[,i] = Z2_nf_j[8,i] + Z1_nf_j[8,i]
        
        #Female employees in waste management
        Z2_n4f[,i] = Z2_nf_j[9,i] + Z1_nf_j[9,i]
        
        #Female employees in recycling
        Z2_n5f[,i] = Z2_nf_j[10,i] + Z1_nf_j[10,i]
        
        #Wage bill paid in manufacturing industry (before taxes)
        Z2_wb1[,i] = Z2_wb_j[6,i] + Z1_wb_j[6,i] 
        
        #Wage bill paid in agriculture (before taxes)
        Z2_wb2[,i] = Z2_wb_j[7,i] + Z1_wb_j[7,i]
        
        #Wage bill paid in services (before taxes)
        Z2_wb3[,i] = Z2_wb_j[8,i] + Z1_wb_j[8,i]
        
        #Wage bill paid in waste management (before taxes)
        Z2_wb4[,i] = Z2_wb_j[9,i] + Z1_wb_j[9,i]
        
        #Wage bill paid in recycling (before taxes)
        Z2_wb5[,i] = Z2_wb_j[10,i] + Z1_wb_j[10,i]
        
        ##########################################################################
        
        #BALANCE OF PAYMENTS ENTRIES
        
        #Z1's real import from Z2 --- CHANGED ON 11/08/2023 
        if (i<10){
        Z1_imp[,i] = exp(Z1_mu0 + Z1_mu2 * log(Z1_yd[,i-1]))}else{
        if(Z1_yd[,i-1] < 0) {stop('Model diverged')} else {
        Z1_imp[,i] = exp(Z1_mu0 - Z1_mu1*(log(Z1_pim[,i-1])-log(Z1_pa[,i-1])) + Z1_mu2 * log(Z1_yd[,i-1]/Z1_pa[,i-1]))}}
        
        #Z1's real intermediate imports from Z2 --- ADDED ON 08/09/2023 (JBF_OVC) <--- JBF
        Z1_M_TOT_int [,i]=           
          x[1,i]*a61[,i]+ x[1,i]*a71[,i]+ x[1,i]*a81[,i]+ x[1,i]*a91[,i]+ x[1,i]*a101[,i]+
          x[2,i]*a62[,i]+ x[2,i]*a72[,i]+ x[2,i]*a82[,i]+ x[2,i]*a92[,i]+ x[2,i]*a102[,i]+ 
          x[3,i]*a63[,i]+ x[3,i]*a73[,i]+ x[3,i]*a83[,i]+ x[3,i]*a93[,i]+ x[3,i]*a103[,i]+
          x[4,i]*a64[,i]+ x[4,i]*a74[,i]+ x[4,i]*a84[,i]+ x[4,i]*a94[,i]+ x[4,i]*a104[,i]+
          x[5,i]*a65[,i]+ x[5,i]*a75[,i]+ x[5,i]*a85[,i]+ x[5,i]*a95[,i]+ x[5,i]*a105[,i]
        
        #Z1's real TOTAL import from Z2 --- CHANGED ON 25/08/2023 
        Z1_tot_imp[,i]= Z1_imp[,i] + Z1_M_TOT_int [,i]
        
        #Z2's real import from Z1 --- CHANGED ON 11/08/2023  
        if (i<10){
        Z2_imp[,i] = exp(Z2_mu0 + Z2_mu2 *log(Z2_yd[,i-1]))}else{
        Z2_imp[,i] = exp(Z2_mu0 - Z2_mu1*(log(Z2_pim[,i-1])-log(Z2_pa[,i-1])) + Z2_mu2 *log(Z2_yd[,i-1]/Z2_pa[,i-1]))}
        
        #Z1's nominal import from Z2   
        #Z1_nimp[,i] = Z1_pim[,i] * Z1_imp[,i] 
        
        #Z2's nominal import from Z1  
        #Z2_nimp[,i] = Z2_pim[,i] * Z2_imp[,i]   
        
        #Z2's real intermediate imports from Z1 --- ADDED ON 08/09/2023 (JBF_OVC) <--- JBF
        Z2_M_TOT_int [,i]=
          x[6,i]*a16[,i]+ x[6,i]*a26[,i]+ x[6,i]*a36[,i]+ x[6,i]*a46[,i]+ x[6,i]*a56[,i]+
          x[7,i]*a17[,i]+ x[7,i]*a27[,i]+ x[7,i]*a37[,i]+ x[7,i]*a47[,i]+ x[7,i]*a57[,i]+ 
          x[8,i]*a18[,i]+ x[8,i]*a28[,i]+ x[8,i]*a38[,i]+ x[8,i]*a48[,i]+ x[8,i]*a58[,i]+
          x[9,i]*a19[,i]+ x[9,i]*a29[,i]+ x[9,i]*a39[,i]+ x[9,i]*a49[,i]+ x[9,i]*a59[,i]+
          x[10,i]*a110[,i]+ x[10,i]*a210[,i]+ x[10,i]*a310[,i]+ x[10,i]*a410[,i]+ x[10,i]*a510[,i]
        
        #Z2's real TOTAL import from Z1 --- CHANGED ON 25/08/2023 <--- JBF
        Z2_tot_imp[,i]= Z2_imp[,i] + Z2_M_TOT_int [,i]
        
        #Z1's nominal import from Z2  --- CHANGED ON 08/09/2023 <--- JBF
        #Z1_nimp[,i] = Z1_pim[,i] * Z1_imp[,i] 
        Z1_nimp[,i] = Z1_pim[,i] * Z1_tot_imp[,i] 
        
        #Z2's nominal import from Z1  --- CHANGED ON 08/09/2023 <--- JBF
        #Z2_nimp[,i] = Z2_pim[,i] * Z2_imp[,i]
        Z2_nimp[,i] = Z2_pim[,i] * Z2_tot_imp[,i]   
        
        #Z1's real export to Z2 <--- JBF
        Z1_rex[,i] = Z2_imp[,i]
        # Z1_rex[,i] = Z2_tot_imp[,i]
        
        #Z2's real export to Z1 <--- JBF
        Z2_rex[,i] = Z1_imp[,i]
        # Z2_rex[,i] = Z1_tot_imp[,i]
        
        #Z1's nominal export to Z2
        Z1_nex[,i] = Z2_nimp[,i] * Z2_xr[,i]
        
        #Z2's nominal export to Z1
        Z2_nex[,i] = Z1_nimp[,i] * Z1_xr[,i]
        
        #Trade balance of Z1  
        Z1_tb[,i] = Z1_nex[,i] - Z1_nimp[,i]
        
        #Trade balance of Z2  
        Z2_tb[,i] = Z2_nex[,i] - Z2_nimp[,i]
        
        
        #Current account balance of Z1 #NEW
        if (Z1_e_s[,i-1]==0 || Z2_e_s[,i-1]==0){Z1_cab[,i] = Z1_tb[,i] + Z2_rb[,i-1] * Z1_b_s_Z2[,i-1] * Z2_xr[,i-1] - Z1_rb[,i-1] * Z2_b_s_Z1[,i-1] + Z2_rb[,i-1] * Z1_b_cb_s_Z2[,i-1] * Z2_xr[,i-1]
        }else{Z1_cab[,i] = (Z1_tb[,i] + Z2_rb[,i-1] * Z1_b_s_Z2[,i-1] * Z2_xr[,i-1] - Z1_rb[,i-1] * Z2_b_s_Z1[,i-1] + Z2_rb[,i-1] * Z1_b_cb_s_Z2[,i-1] * Z2_xr[,i-1] +   
                        + Z2_xr[,i]*(1-Z2_omega)*Z2_f_f[,i]*Z1_e_s_Z2[,i-1]/Z2_e_s[,i-1] - (1-Z1_omega)*Z1_f_f[,i]*Z2_e_s_Z1[,i-1]/Z1_e_s[,i-1])}
        
        #Current account balance of Z2 #NEW
        if (Z1_e_s[,i-1]==0 || Z2_e_s[,i-1]==0){Z2_cab[,i] = Z2_tb[,i] + Z1_rb[,i-1] * Z2_b_s_Z1[,i-1] * Z1_xr[,i-1] - Z2_rb[,i-1] * Z1_b_s_Z2[,i-1] - Z2_rb[,i-1] * Z1_b_cb_s_Z2[,i-1]
        }else{Z2_cab[,i] = (Z2_tb[,i] + Z1_rb[,i-1] * Z2_b_s_Z1[,i-1] * Z1_xr[,i-1] - Z2_rb[,i-1] * Z1_b_s_Z2[,i-1] - Z2_rb[,i-1] * Z1_b_cb_s_Z2[,i-1] +    
                   + Z1_xr[,i]*(1-Z1_omega)*Z1_f_f[,i]*Z2_e_s_Z1[,i-1]/Z1_e_s[,i-1] - (1-Z2_omega)*Z2_f_f[,i]*Z1_e_s_Z2[,i-1]/Z2_e_s[,i-1])}
        
        #Financial account balance of Z1, net of official transactions
        Z1_kabp[,i] = ((Z2_b_s_Z1[,i]-Z2_b_s_Z1[,i-1]) - (Z1_b_s_Z2[,i]-Z1_b_s_Z2[,i-1]) * Z2_xr[,i] +
                    + (Z2_e_s_Z1[,i]-Z2_e_s_Z1[,i-1]) - (Z1_e_s_Z2[,i]-Z1_e_s_Z2[,i-1]) * Z2_xr[,i])    #NEW
        
        #Financial account balance of Z2, net of official transactions
        Z2_kabp[,i] = ((Z1_b_s_Z2[,i]-Z1_b_s_Z2[,i-1]) - (Z2_b_s_Z1[,i]-Z2_b_s_Z1[,i-1]) * Z1_xr[,i] +
                    + (Z1_e_s_Z2[,i]-Z1_e_s_Z2[,i-1]) - (Z2_e_s_Z1[,i]-Z2_e_s_Z1[,i-1]) * Z1_xr[,i])    #NEW
        
        #Net accumulation of financial assets (NAFA) of Z1  
        Z1_nafa[,i] = Z1_gdef[,i] + Z1_cab[,i]
        
        #Net accumulation of financial assets (NAFA) of Z2  
        Z2_nafa[,i] = Z2_gdef[,i] + Z2_cab[,i]
        
        #Quantity of reserves of z2
        #Z2_or[,i] = 0
        
        #Quantity of reserves of z1
        #Z1_or[,i] = Z2_or[,i]
        
        
        ##########################################################################
        
        ##########################################################################
        
        # EXCHANGE RATE CLOSURES
        
        #Note 1: equation numbers are taken from Godley & Lavoie (2007, pp. 460-464)
        
        #Note 2: we must consider also bills held by commercial banks
        
        #Exchange rate: value of Z2 currency in terms of Z1
        Z2_xr[,i] = 1/Z1_xr[,i]
        
        #Z1 CB holdings of bills ------------------------------------ 12.84 --- CHANGED ON 04/08/2023 (ADVANCES ADDED)
        Z1_b_cb[,i] = Z1_b_cb[,i-1] + (Z1_h_s[,i]-Z1_h_s[,i-1]) - + (Z1_a_s[,i]-Z1_a_s[,i-1]) - (Z1_b_cb_s_Z2[,i]-Z1_b_cb_s_Z2[,i-1])*Z2_xr[,i] - (Z1_or[,i]-Z1_or[,i-1])*Z1_p_or[,i] 
        
        #Z2 CB holdings of bills ------------------------------------ 12.83 --- CHANGED ON 04/08/2023 (ADVANCES ADDED)
        Z2_b_cb[,i] = Z2_h_s[,i] - Z2_or[,i]*Z2_p_or[,i] - Z2_a_s[,i]
        
        ################################################################################################
        
        #1) FIXED EXCHANGE RATE CLOSURE --- CHANGED ON 01/08/2023
        
        #Supply of Z2 bills to Z1's households --------------------------------------------------- 12.89F 
        Z1_b_s_Z2[,i] = Z1_b_h_Z2[,i] * Z1_xr[,i]
        
        #Supply of Z2 bills to Z1 central bank --------------------------------------------------- 12.90F 
        Z1_b_cb_s_Z2[,i] = Z2_b_s[,i] - Z1_b_s_Z2[,i] - Z2_b_s_Z2[,i] - Z2_b_cb[,i] - Z2_b_b[,i]
        
        #Exchange rate: value of Z1 currency in terms of Z2 (exogenous) -------------------------- 12.91F 
        #Z1_xr[,i] = 1
        
        ################################################################################################
        
        #2) FLOATING EXCHANGE RATE CLOSURE
        
        #Exchange rate: value of Z1 currency in terms of Z2 (endogenous) ------------------------ 12.89FL
        #Z1_xr[,i] = Z1_b_s_Z2[,i]/Z1_b_h_Z2[,i] 
        
        #Supply of Z2 bills to Z1's households -------------------------------------------------- 12.90FL
        #Z1_b_s_Z2[,i] = Z2_b_s[,i] - Z2_b_s_Z2[,i] - Z2_b_cb[,i] - Z2_b_b[,i] 
        
        #Supply of Z2 bills to Z1 central bank -------------------------------------------------- 12.91FL
        #Z1_b_cb_s_Z2[,i] = 0
        
        #Z1 CB holdings of bills ---------------------------------------------------------------- 12.84FL 
        #Z1_b_cb[,i] = Z1_b_cb[,i-1] + (Z1_h_s[,i]-Z1_h_s[,i-1])
        
        
        ################################################################################################
        
        #3) MIXED REGIME: QUASI-FLOATING --- CHANGED ON 01/08/2023
        if (i>75 && (shock==3 || shock==5) ){
        
        #Exchange rate: value of Z1 currency in terms of Z2 (endogenous)
        Z1_xr[,i] = Z1_xr[,i-1] + par_xr*Z1_cab[,i-1]/Z1_yn[,i-1]
        
        } 
        
        ##########################################################################
        
        #Hidden equation used for consistency check --------------------------------------------- 12.82A
        # Z1_b_cb[,i] = Z1_b_s[,i] - Z1_b_s_Z1[,i] - Z2_b_s_Z1[,i] - Z1_b_b[,i]
        
        ##########################################################################        
        
        #ECOSYSTEM EQUATIONS #NEW
        
        #Ecosystem equations for Area 1
        
        #Extraction of matter of Area 1
        Z1_x_mat[,i] = t(Z1_mu_mat[,i]) %*% Z1_x[,i]                                       #Production of material goods
        Z1_mat[,i] = Z1_x_mat[,i] - Z1_rec[,i]                                             #Extraction of matter 
        Z1_rec[,i] = Z1_rho_dis[,i]*Z1_dis[,i] + Z1_mu_mat[5,i]*x[5,i]                     #Recycled matter (of socioeconomic stock + industrial waste)
        Z1_dis[,i] = t(Z1_mu_mat[,i]) %*% (Z1_zeta_dc[,i-1] * Z1_dc[,i-1])                 #Discarded socioeconomic stock
        Z1_dc[,i] = Z1_dc[,i-1] + Z1_beta[,i]*Z1_c[,i] - Z1_zeta_dc[,i-1] * Z1_dc[,i-1]    #Stock of durable consumption goods
        Z1_kh[,i] = Z1_kh[,i-1] + Z1_x_mat[,i] - Z1_dis[,i]                                #Socioeconomic stock
        #Z1_wa[,i] = ...                                                                   #Waste (to be redefined?)
        
        #Use of energy of Area 1
        Z1_en[,i] = t(Z1_eps_en[,i]) %*% Z1_x[,i]                                          #Energy required for production
        Z1_ren[,i] = t(Z1_eps_en[,i]) %*% (Z1_eta_en[,i]*Z1_x[,i])                         #Renewable energy at the end of the period
        Z1_nen[,i] = Z1_en[,i] - Z1_ren[,i]                                                #Non-renewable energy
        #Z1_emis[,i] = ...                                                                 #CO2 emissions (to be redefined?)
        
        #Extraction of matter of Area 2
        Z2_x_mat[,i] = t(Z2_mu_mat[,i]) %*% Z2_x[,i]                                       #Production of material goods
        Z2_mat[,i] = Z2_x_mat[,i] - Z2_rec[,i]                                             #Extraction of matter 
        Z2_rec[,i] = Z2_rho_dis[,i]*Z2_dis[,i] + Z2_mu_mat[5,i]*x[5,i]                     #Recycled matter (of socioeconomic stock + industrial waste)
        Z2_dis[,i] = t(Z2_mu_mat[,i]) %*% (Z2_zeta_dc[,i-1] * Z2_dc[,i-1])                 #Discarded socioeconomic stock
        Z2_dc[,i] = Z2_dc[,i-1] + Z2_beta[,i]*Z2_c[,i] - Z2_zeta_dc[,i-1] * Z2_dc[,i-1]    #Stock of durable consumption goods
        Z2_kh[,i] = Z2_kh[,i-1] + Z2_x_mat[,i] - Z2_dis[,i]                                #Socioeconomic stock
        #Z2_wa[,i] = ...                                                                   #Waste (to be redefined?)
        
        #Use of energy of Area 2
        Z2_en[,i] = t(Z2_eps_en[,i]) %*% Z2_x[,i]                                          #Energy required for production
        Z2_ren[,i] = t(Z2_eps_en[,i]) %*% (Z2_eta_en[,i]*Z2_x[,i])                         #Renewable energy at the end of the period
        Z2_nen[,i] = Z2_en[,i] - Z2_ren[,i]                                                #Non-renewable energy
        #Z2_emis[,i] = ...                                                                 #CO2 emissions (to be redefined?)
        
        #Global use and depletion of matter and energy
        kmat[,i] = kmat[,i-1] + conv_mat[,i] - Z1_mat[,i] - Z2_mat[,i]                     #Global stock of material reserves
        conv_mat[,i] = sigma_mat*res_mat[,i]                                               #Material resources converted to reserves 
        res_mat[,i] = res_mat[,i-1] - conv_mat[,i]                                         #Global stock of material resources
        ken[,i] = ken[,i-1] + conv_en[,i] - Z1_nen[,i] - Z2_nen[,i]                        #Stock of energy reserves
        conv_en[,i] = sigma_en*res_en[,i]                                                  #Energy resources converted to reserves 
        res_en[,i] = res_en[,i-1] - conv_en[,i]                                            #Stock of energy resources
        
        #LABOUR FORCE AND IMMIGRATION EQUATIONS
        if(i<=75){Z1_pop_j[,i] = Z1_n_j[,i]
        }else{Z1_pop_j[,i] = Z1_pop_j[,i-1]*(1+Z1_g_pop[,i]) + Z1_imm[,i] - Z2_imm[,i]}     #Labour force in Area 1 (by industry)  
        
        if(i<=75){Z2_pop_j[,i] = Z2_n_j[,i]
        }else{Z2_pop_j[,i] = Z2_pop_j[,i-1]*(1+Z1_g_pop[,i]) + Z2_imm[,i] - Z1_imm[,i]}     #Labour force in Area 2 (by industry)
        
        if(Z1_pop_j[5,i]==0){Z1_un[5,i] = 0
        }else{Z1_un[,i] = 1 - Z1_n_j[,i]/Z1_pop_j[,i]}                                      #Industry-specific unemployment rate in Area 1
        
        if(Z2_pop_j[5,i]==0){Z1_un[5,i] = 0
        }else{Z2_un[,i] = 1 - Z2_n_j[,i]/Z2_pop_j[,i]}                                      #Industry-specific unemployment rate in Area 2
        
        Z1_imm[,i] = (gamma_imm_10[,i]*Z2_pop_j[,i-1] + gamma_imm_11[,i]*Z2_un[,i-1] +     #Immigration in Area 1: labour-force moving from Area 2 to Area 1
                   + gamma_imm_12[,i]*(Z1_w[,i-1]-Z2_w[,i-1]))
          
        Z2_imm[,i] = (gamma_imm_20[,i]*Z1_pop_j[,i-1] + gamma_imm_21[,i]*Z1_un[,i-1] +     #Immigration in Area 2: labour-force moving from Area 2 to Area 2
                   + gamma_imm_22[,i]*(Z2_w[,i-1]-Z1_w[,i-1]))
        
        Z1_rho[,i] = Z1_parw0[,i] - Z1_parw1[,i] * (Z1_w[,i] - Z1_w[,i-1])                 #Percentage of female workers by industry in Area 1 --- CHANGED ON 01/08/2023
        
        Z2_rho[,i] = Z2_parw0[,i] - Z2_parw1[,i] * (Z2_w[,i] - Z2_w[,i-1])                 #Percentage of female workers by industry in Area 1 --- CHANGED ON 01/08/2023 

        
        # CONSISTENCY CHECK
        error <- 0.5 * (( (Z1_b_cb[,i] - ( Z1_b_s[,i] - Z1_b_s_Z1[,i] - Z1_b_b[,i] - Z2_b_s_Z1[,i])) )^2 + ( Z1_or[,i]-Z1_or[,i-1]+(Z2_or[,i]-Z2_or[,i-1]) )^2)
        
        # CHECK CONVERGENCE
        # Optimize Number of Iterations
        iter.list <- lapply(variable.names, function (tmp) eval(as.symbol(tmp))[ , i])
        simiter[ , iterations] <- unlist(iter.list)
        simiter[is.infinite(simiter[,iterations]), iterations] <- NA
        
        # Gauss-Seidel Score Function and Check Consistency
        score <- 1
        if(iterations > 4)
        {
          score <- (simiter[, iterations] - simiter[, iterations - 1]) / simiter[, iterations - 1]
          score[is.na(score)] <- 0
        }
        if(sum(score < tolerance, na.rm = T) == length(var.labels) & error < consistency.threshold)
        {
          last.iteration[i] <- iterations
          consistency.error[i] <- error
          break
        }
    }
    
    # BREAK MODEL RUN IF ASSETS ARE NEGATIVE OR STOCK-FLOW INCONSISTENT
    negative.assets <- as.numeric(Z1_b_s[,i] < 0) + as.numeric(Z2_b_s[,i] < 0)
    # negative.assets <- as.numeric(Z1_b_cb[,i] < 0) + as.numeric(Z2_b_cb[,i]<0) + as.numeric(Z1_b_b[,i] < 0) +
    #   as.numeric(Z2_b_b[,i] < 0) + as.numeric(Z1_b_cb_s_Z2[,i]<0) + as.numeric(Z1_b_s[,i] < 0) + as.numeric(Z2_b_s[,i] < 0)
      # as.numeric(Z2_b_s[ ,i]<0) + as.numeric(Z2_b_s_Z2[,i]<0) + # as.numeric(Z2_mh[,i]<0) + as.numeric(Z2_h_h[,i] < 0) +
      # as.numeric(Z2_b_b[,i]<0) + as.numeric(Z2_b_cb[,i] < 0)
    
    # negative.assets <- as.numeric(Z1_b_s[ ,i]<0) + as.numeric(Z1_b_s_Z1[,i]<0) + # as.numeric(Z1_mh[,i]<0) + as.numeric(Z1_h_h[,i] < 0) + 
    #   as.numeric(Z1_b_b[,i]<0) + as.numeric(Z1_b_cb[,i] < 0) + 
    #   as.numeric(Z2_b_s[ ,i]<0) + as.numeric(Z2_b_s_Z2[,i]<0) + # as.numeric(Z2_mh[,i]<0) + as.numeric(Z2_h_h[,i] < 0) + 
    #   as.numeric(Z2_b_b[,i]<0) + as.numeric(Z2_b_cb[,i] < 0)
     # negative.assets <- as.numeric(Z1_b_s[ ,i]<0) + as.numeric(Z1_b_s_Z1[,i]<0) + as.numeric(Z1_mh[,i]<0) + as.numeric(Z1_h_h[,i] < 0) + 
     #  as.numeric(Z1_b_b[,i]<0) + as.numeric(Z1_b_cb[,i] < 0) + 
     #  as.numeric(Z2_b_s[ ,i]<0) + as.numeric(Z2_b_s_Z2[,i]<0) + as.numeric(Z2_mh[,i]<0) + as.numeric(Z2_h_h[,i] < 0) + 
     #  as.numeric(Z2_b_b[,i]<0) + as.numeric(Z2_b_cb[,i] < 0)

    # if (negative.assets | error > consistency.threshold) break
    if (error > consistency.threshold) break
  }
  simulation <- initial
  variable.list <- lapply(variable.names, function (tmp) eval(as.symbol(tmp)))
  simulation$variables <- do.call(rbind, variable.list)
  dimnames(simulation$variables) <- list(var.labels, paste0('t', 1 : nPeriods))

  target.values <- target.table
  
  if (length(target.variables) > 1)
  {
    target.values[1, ] <- c(Z1_c[,i], Z1_id[,i], Z1_g[,i], Z1_rex[,i], 
                            Z1_imp[,i], Z2_M_TOT_int[,i], Z1_M_TOT_int[,i], 
                            sum(Z1_d[,i]), Z1_va[,i], sum(x[1 : 5,i]), Z1_gdef[,i], Z1_b_s[,i] / Z1_va[,i], Z1_b_s[, i])[target.indices]
    target.values[2, ] <- c(Z2_c[,i], Z2_id[,i], Z2_g[,i], Z2_rex[,i], 
                            Z2_imp[,i], Z1_M_TOT_int[,i], Z2_M_TOT_int[,i], 
                            sum(Z2_d[,i]), Z2_va[,i], sum(x[6 : 10,i]), Z2_gdef[,i], Z2_b_s[,i] / Z2_va[,i], Z2_b_s[, i])[target.indices]
  } else {
    target.values[1] <- c(Z1_c[,i], Z1_id[,i], Z1_g[,i], Z1_rex[,i], 
                          Z1_imp[,i], Z2_M_TOT_int[,i], Z1_M_TOT_int[,i], 
                          sum(Z1_d[,i]), Z1_va[,i], sum(x[1 : 5,i]), Z1_gdef[,i], Z1_b_s[,i] / Z1_va[,i], Z1_b_s[, i])[target.indices]
    target.values[2] <- c(Z2_c[,i], Z2_id[,i], Z2_g[,i], Z2_rex[,i], 
                          Z2_imp[,i], Z1_M_TOT_int[,i], 
                          Z2_M_TOT_int[,i], sum(Z2_d[,i]), 
                          Z2_va[,i], sum(x[6 : 10,i]), Z2_gdef[,i], Z2_b_s[,i] / Z2_va[,i], Z2_b_s[, i])[target.indices]
  }
  target.ratios <- target.values / target.table
  # target.ratios <- c(Z1_b_s[ ,i], Z1_b_s_Z1[,i], Z1_mh[,i], Z1_h_h[,i],
  #                    Z1_b_b[,i], Z1_b_cb[,i],
  #                    Z2_b_s[ ,i], Z2_b_s_Z2[,i], Z2_mh[,i], Z2_h_h[,i],
  #                    Z2_b_b[,i], Z2_b_cb[,i], i, error)
  # target.ratios <- setNames(target.ratios, c('Z1_b_s', 'Z1_b_s_Z1', 'Z1_mh', 'Z1_h_h', 'Z1_b_b', 'Z1_b_cb', 'Z2_b_s', 'Z2_b_s_Z2', 'Z2_mh', 'Z2_h_h', 'Z2_b_b', 'Z2_b_cb', 'i', 'error'))
  # 
  fitness <- sum((target.ratios - 1) ^ 2) / prod(dim(target.table))
  # fitness <- error
  # fitness <- error ^ 2 + negative.assets ^ 2 # - i ^ 2 # + sum((target.ratios - 1) ^ 2) / prod(dim(target.table))
  # print (c(fitness, i))
  # fitness <- -i
  
  return(list(initial = initial, simulation = simulation, values = target.values, ratios = target.ratios, fitness = fitness, 
              period = i, negative.assets = negative.assets, error = error))
}
