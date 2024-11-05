#### AUXILIARY FUNCTIONS ####
#### LOAD INITIAL VALUES ####
load.init <- function(identif)
{
  initial.values <- list(variables = openxlsx :: read.xlsx(identif, sheet = 'variables', rowNames = TRUE), 
                         parameters = openxlsx :: read.xlsx(identif, sheet = 'parameters', rowNames = TRUE), 
                         BS = openxlsx :: read.xlsx(identif, sheet = 'BS', rowNames = TRUE), 
                         A.matrix = openxlsx :: read.xlsx(identif, sheet = 'A.matrix', rowNames = TRUE), 
                         B.matrix = openxlsx :: read.xlsx(identif, sheet = 'B.matrix', rowNames = TRUE), 
                         I = as.matrix(openxlsx :: read.xlsx(identif, sheet = 'I', rowNames = TRUE)))
  var.labels <- rownames(initial.values$variables)
  variable.names <- unique(sapply(strsplit(var.labels, split = '-', fixed = T), function (tmp) tmp[1]))
  return (initial.values)
}

# Auxiliary Function that returns Instrument Values
return.instr.vals <- function (res)
{
  return(dplyr :: bind_rows(list(variable = data.frame(label = instruments$var, 
                                                       value = res$simulation$variables[instruments$var, 't1'], row.names = NULL),
                                 parameter = data.frame(label = instruments$par, 
                                                        value = res$simulation$parameters[instruments$par, ], row.names = NULL),
                                 BS = data.frame(label = instruments$BS, 
                                                 value = res$initial$BS[instruments$BS, ], row.names = NULL)),
                            .id = 'item'))
}

#### LOAD TARGET VALUES IN WIDE AND LONG FORMAT ####
load.target <- function (target.file)
{
  target.table.0 <- openxlsx :: read.xlsx(target.file, sheet = 'Target', rowNames = TRUE) / 10000 # WIDE FORMAT
  target.set <- data.frame(area = rep(c('Z1', 'Z2'), times = dim(target.table.0)[2]),
                           var.label = rep(c('c', 'id', 'g', 'rex', 'imp', 'X_TOT_int', 'M_TOT_int', 'fd', 'va', 'go', 'gdef', 'debt.gdp', 'b_s'), each = 2),
                           value = unlist(target.table.0))
  target.set$var <- paste0(target.set$area, '_', target.set$var)
  target.set$type <- setNames(variable.table$type, variable.table$label)[target.set$var.label]
  target.set$variable <- setNames(variable.table$name, variable.table$label)[target.set$var.label]
  target.set$unit <- setNames(variable.table$unit, variable.table$label)[target.set$var.label]
  target.set$name <- ifelse(target.set$unit == '', 
                            paste0(target.set$variable, '\n(', target.set$var.label, ')'),
                            paste0(target.set$variable, '\n(', target.set$var.label,', in ', target.set$unit, ')'))
  # LONG FORMAT
  rownames(target.set) <- target.set$var 
  
  return(list(wide = target.table.0, long = target.set))
}

#### RANDOM SEARCH ALGORITHM ####
random.search <- function(initial, max.iter, std.dev)
{
  results <- run.model(initial)
  
  universe <- list() # universe is a list that saves each run of the model for each of its elements
  universe[[1]] <- list(initial = results$initial,
                        instruments = return.instr.vals(results),# instrument values
                        simulation = results$simulation,         # simulation values
                        target = results$values,                 # values of target variables
                        ratio = results$ratios,                  # target/actual ratios (should be 1)
                        fitness = results$fitness)  # fitness value (a sum of the ratios - 1)
  iters <- k <- 1
  
  print(c(iters, k, universe[[k]]$fitness))
  while (iters < max.iter)
  {
    # Adjust Parameters and Run Model
    sim <- universe[[k]]$initial
    sim$variables[instruments$var, 't1'] <- rnorm(length(instruments$var), 1, std.dev) * sim$variables[instruments$var, 't1']
    sim$parameters[instruments$par, ] <- rnorm(length(instruments$par), 1, std.dev) * sim$parameters[instruments$par, ]
    sim$BS[instruments$BS, ] <- rnorm(length(instruments$BS), 1, std.dev) * sim$BS[instruments$BS, ]
    
    # Boundary Conditions so Parameters are Meaningful
    if(sim$parameters['Z1_alpha1', ] > 1) sim$parameters['Z1_alpha1', ] <- .99
    if(sim$parameters['Z1_alpha2', ] > 1) sim$parameters['Z1_alpha2', ] <- .99
    if(sim$parameters['Z1_alpha3', ] > 1) sim$parameters['Z1_alpha3', ] <- .99
    
    if(sim$parameters['Z2_alpha1', ] > 1) sim$parameters['Z2_alpha1', ] <- .99
    if(sim$parameters['Z2_alpha2', ] > 1) sim$parameters['Z2_alpha2', ] <- .99  
    if(sim$parameters['Z2_alpha3', ] > 1) sim$parameters['Z2_alpha3', ] <- .99
    
    if(sim$parameters['Z1_alpha2', ] > sim$parameters['Z1_alpha1', ]) sim$parameters['Z1_alpha2', ] <- sim$parameters['Z1_alpha1', ]
    if(sim$parameters['Z1_alpha3', ] > sim$parameters['Z1_alpha2', ]) sim$parameters['Z1_alpha3', ] <- sim$parameters['Z1_alpha2', ]
    
    if(sim$parameters['Z2_alpha2', ] > sim$parameters['Z2_alpha1', ]) sim$parameters['Z2_alpha2', ] <- sim$parameters['Z2_alpha1', ]
    if(sim$parameters['Z2_alpha3', ] > sim$parameters['Z2_alpha2', ]) sim$parameters['Z2_alpha3', ] <- sim$parameters['Z2_alpha2', ]
    
    if(sum(sim$parameters[paste0('Z1_lambda', 1 : 4, '0'), ]) > 1) 
      sim$parameters[paste0('Z1_lambda', 1 : 4, '0'), ] <- sim$parameters[paste0('Z1_lambda', 1 : 4, '0'), ] / sum(sim$parameters[paste0('Z1_lambda', 1 : 4, '0'), ])
    if(sum(sim$parameters[paste0('Z2_lambda', 1 : 4, '0'), ]) > 1) 
      sim$parameters[paste0('Z2_lambda', 1 : 4, '0'), ] <- sim$parameters[paste0('Z2_lambda', 1 : 4, '0'), ] / sum(sim$parameters[paste0('Z2_lambda', 1 : 4, '0'), ])
    
    results <- run.model(sim)
    
    # If New Fitness is Lower, Save Parameters
    if (universe[[k]]$fitness >= results$fitness)
    {
      # if (!sum(results$simulation$variables['Z1_b_s', ] < 0) & !sum(results$simulation$variables['Z2_b_s', ] < 0)) # only accept if public debt is positive
      # {
      k <- k + 1
      universe[[k]] <- list(initial = sim,
                            instruments = return.instr.vals(results),
                            simulation = results$simulation,
                            target = results$values,
                            ratio = results$ratios,
                            fitness = results$fitness)
      # }
    }
    print(c(iters <- iters + 1, k, universe[[k]]$fitness, results$fitness))
  }
  return (universe)
}


#### SAVE SIMULATION VALUES ####
save.sim <- function (tit.le)
{
  variable.list <- lapply(variable.names, function (tmp) eval(as.symbol(tmp)))
  res.vars <- do.call(rbind, variable.list)
  dimnames(res.vars) <- list(var.labels, paste0('t', 1 : nPeriods))
  write.csv(res.vars, file = paste0('results_', tit.le, '.csv'), row.names = TRUE)
}

#### SHOCK LONG ####

shock.long <- function(baseline, scenario)
{
  res <- baseline
  if (is.numeric(scenario)) {scen <- scenario} else {scen <- baseline.run$simulation$parameters['shock', ]}
  sh.ock <- rownames(scenario.table)[scen]
  
  # Aggregate
  plot.labels <- apply(expand.grid(c('Z1', 'Z2'), variable.table$label[variable.table$dimension == 'aggregate']), 1, paste, collapse="_")
  selected <- plot.labels[plot.labels %in% rownames(res$simulation$variables)]
  # foo <- baseline$simulation$variables[selected, ] - scenario.list[[scen]]$simulation$variables[selected, ]
  # selected <- names(which(rowSums(foo != 0) > 0))
  dt <- res$simulation$variables[selected, ] %>% melt(varnames = c('var', 't'))
  dt$time <- as.numeric(substring(dt$t, 2))
  dt$area <- substr(dt$var, 1, 2)
  dt$variable <- substring(dt$var, 4)
  dt$name <- paste0(setNames(variable.table$name, variable.table$label)[dt$variable], '\n(', dt$variable,')')
  dt$type <- setNames(variable.table$type, variable.table$label)[dt$variable]
  dt$sector <- 'aggregate'
  dt$scenario <- 'baseline'
  dt$shock <- 0
  df <- dt
  
  # Industry
  plot.labels <- apply(expand.grid(c('Z1', 'Z2'), variable.table$label[variable.table$dimension == 'industry']), 1, paste, collapse="_")
  plot.labels <- apply(expand.grid(plot.labels, as.character(1 : 10)), 1, paste, collapse="-")
  selected <- plot.labels[plot.labels %in% rownames(res$simulation$variables)]
  # foo <- baseline$simulation$variables[selected, ] - scenario.list[[scen]]$simulation$variables[selected, ]
  # selected <- names(which(rowSums(foo != 0) > 0))
  dt <- res$simulation$variables[selected, ] %>% melt(varnames = c('var', 't'))
  dt$time <- as.numeric(substring(dt$t, 2))
  dt$area <- substr(dt$var, 1, 2)
  dt$variable <- sapply(strsplit(substring(dt$var, 4), split = '-'), function (x) x[1])
  dt$name <- paste0(setNames(variable.table$name, variable.table$label)[dt$variable], '\n(', dt$variable, ')')
  dt$sector <- sapply(strsplit(substring(dt$var, 4), split = '-'), function (x) x[2])
  
  dt <- dt %>% filter((dt$area == 'Z1' & as.numeric(dt$sector) < 6) | (dt$area == 'Z2' & as.numeric(dt$sector) > 5))
  dt$sector <- ifelse(as.numeric(dt$sector) > 5, as.character(as.numeric(dt$sector) - 5), dt$sector)
  dt$type <- setNames(variable.table$type, variable.table$label)[dt$variable]
  dt$scenario <- 'baseline'
  dt$shock <- 0
  
  df <- rbind(df, dt)
  
  if (is.numeric(scenario)) {res <- scenario.list[[scen]]} else {res <- scenario}
  
  # Aggregate
  plot.labels <- apply(expand.grid(c('Z1', 'Z2'), variable.table$label[variable.table$dimension == 'aggregate']), 1, paste, collapse="_")
  selected <- plot.labels[plot.labels %in% rownames(res$simulation$variables)]
  # foo <- baseline$simulation$variables[selected, ] - scenario.list[[scen]]$simulation$variables[selected, ]
  # selected <- names(which(rowSums(foo != 0) > 0))
  dt <- res$simulation$variables[selected, ] %>% melt(varnames = c('var', 't'))
  if(!length(selected)) return (0)
  dt$time <- as.numeric(substring(dt$t, 2))
  dt$area <- substr(dt$var, 1, 2)
  dt$variable <- substring(dt$var, 4)
  dt$name <- paste0(setNames(variable.table$name, variable.table$label)[dt$variable], '\n(', dt$variable,')')
  dt$type <- setNames(variable.table$type, variable.table$label)[dt$variable]
  dt$sector <- 'aggregate'
  dt$scenario <- 'shock'
  dt$shock <- scen
  df <- rbind(df, dt)
  
  # Industry
  plot.labels <- apply(expand.grid(c('Z1', 'Z2'), variable.table$label[variable.table$dimension == 'industry']), 1, paste, collapse="_")
  plot.labels <- apply(expand.grid(plot.labels, as.character(1 : 10)), 1, paste, collapse="-")
  selected <- plot.labels[plot.labels %in% rownames(res$simulation$variables)]
  # foo <- baseline$simulation$variables[selected, ] - scenario.list[[scen]]$simulation$variables[selected, ]
  # selected <- names(which(rowSums(foo != 0) > 0))
  dt <- res$simulation$variables[selected, ] %>% melt(varnames = c('var', 't'))
  dt$time <- as.numeric(substring(dt$t, 2))
  dt$area <- substr(dt$var, 1, 2)
  dt$variable <- sapply(strsplit(substring(dt$var, 4), split = '-'), function (x) x[1])
  dt$name <- paste0(setNames(variable.table$name, variable.table$label)[dt$variable], '\n(', dt$variable, ')')
  dt$sector <- sapply(strsplit(substring(dt$var, 4), split = '-'), function (x) x[2])
  
  dt <- dt %>% filter((dt$area == 'Z1' & as.numeric(dt$sector) < 6) | (dt$area == 'Z2' & as.numeric(dt$sector) > 5))
  dt$sector <- ifelse(as.numeric(dt$sector) > 5, as.character(as.numeric(dt$sector) - 5), dt$sector)
  dt$type <- setNames(variable.table$type, variable.table$label)[dt$variable]
  dt$scenario <- 'shock'
  dt$shock <- scen
  
  df <- rbind(df, dt)
  
  df$sector[df$sector != 'aggregate'] <- sectors[as.numeric(df$sector[df$sector != 'aggregate'])]
  df$sector <- factor(df$sector, levels = c(sectors, 'aggregate'))
  
  return (df)
}