library(reshape2)
library(dplyr)
library(ggplot2)

directory <- "code/REV_MODEL_26OCT/"
variable.table <- read.csv(paste0(directory, 'data/Variable_Definitions.csv'))
scenario.list <- read.csv(paste0(directory, 'data/Scenario_List.csv'))

#### LOAD FUNCTIONS ####
source(paste0(directory, 'functions/1_Model_Function_scenarios_Feb27.R')) # model
source(paste0(directory, 'functions/auxiliary.R')) # load.init, load.target, return.instr.val
source(paste0(directory, 'functions/plot_variable_function_Feb9.R')) # plotting functions 
source(paste0(directory, 'functions/5A_Tables_Function_Feb12.R')) # table functions (BS and TFM)

#### LOAD TARGET VALUES IN WIDE AND LONG FORMAT ####
tmp <- load.target(paste0(directory, 'data/target_values.xlsx'))
target.table.0 <- tmp$wide
target.set <- tmp$long


################## ALL SCENARIOS
init <- load.init(paste0(directory, 'data/initial_state_Feb25.xlsx'))
init$parameters['shock', ] <- 0
baseline <- run.model(init)

sce.narios <- scenario.list$Shock.Number # c(1 : 4, 7 : 9, 11 : 12, 21)
sce.list <- list()
for (scen in 1 : length(sce.narios))
{
  init$parameters['shock', ] <- sce.narios[scen]
  sce.list[[scen]] <- run.model(init)
}


# CONSTRUCTION OF THE DATA FOR VISUALIZATION

for (scen in 1 : length(sce.narios))
{
  
# BASELINE
res <- baseline

# Aggregate
plot.labels <- apply(expand.grid(c('Z1', 'Z2'), variable.table$label[variable.table$dimension == 'aggregate']), 1, paste, collapse="_")
selected <- plot.labels[plot.labels %in% rownames(res$simulation$variables)]
foo <- baseline$simulation$variables[selected, ] - sce.list[[scen]]$simulation$variables[selected, ]
selected <- names(which(rowSums(foo != 0) > 0))
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
foo <- baseline$simulation$variables[selected, ] - sce.list[[scen]]$simulation$variables[selected, ]
selected <- names(which(rowSums(foo != 0) > 0))
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

res <- sce.list[[scen]]
  # Aggregate
  plot.labels <- apply(expand.grid(c('Z1', 'Z2'), variable.table$label[variable.table$dimension == 'aggregate']), 1, paste, collapse="_")
  selected <- plot.labels[plot.labels %in% rownames(res$simulation$variables)]
  foo <- baseline$simulation$variables[selected, ] - sce.list[[scen]]$simulation$variables[selected, ]
  selected <- names(which(rowSums(foo != 0) > 0))
  dt <- res$simulation$variables[selected, ] %>% melt(varnames = c('var', 't'))
  dt$time <- as.numeric(substring(dt$t, 2))
  dt$area <- substr(dt$var, 1, 2)
  dt$variable <- substring(dt$var, 4)
  dt$name <- paste0(setNames(variable.table$name, variable.table$label)[dt$variable], '\n(', dt$variable,')')
  dt$type <- setNames(variable.table$type, variable.table$label)[dt$variable]
  dt$sector <- 'aggregate'
  dt$scenario <- 'shock'
  dt$shock <- sce.narios[scen]
  df <- rbind(df, dt)

  # Industry
  plot.labels <- apply(expand.grid(c('Z1', 'Z2'), variable.table$label[variable.table$dimension == 'industry']), 1, paste, collapse="_")
  plot.labels <- apply(expand.grid(plot.labels, as.character(1 : 10)), 1, paste, collapse="-")
  selected <- plot.labels[plot.labels %in% rownames(res$simulation$variables)]
  foo <- baseline$simulation$variables[selected, ] - sce.list[[scen]]$simulation$variables[selected, ]
  selected <- names(which(rowSums(foo != 0) > 0))
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
  dt$shock <- sce.narios[scen]

  df <- rbind(df, dt)
  sh.ock <- scenario.list$Shock.Number[scen]
  pdf(paste0(directory, 'figures/scenario', sh.ock, '.pdf'))
  for (i in unique(df$type))
  {
    shock.title <- scenario.list$Shock[scen]
    print(filter(df, type == i & (shock == 0 | shock == sh.ock) & sector == 'aggregate') %>% ggplot(aes(x = time, y = value, color = area, linetype = scenario)) +
            facet_wrap(~ name, scales = 'free') + geom_line() +
            geom_vline(xintercept = baseline$simulation$parameters['t.shock', ], linetype = 'dashed', linewidth = .4) + 
            # geom_hline(data = filter(target.set, type == i), mapping = aes(yintercept = value), linetype = 'dashed', linewidth = .5) +
            labs(title = paste0('Shock: ', shock.title), subtitle = paste0(i, ', aggregate')))
  }
  for (i in unique(df$type[df$sector != 'aggregate']))
  {
    print(df %>% filter(type == i & (shock == 0 | shock == sh.ock) & sector != 'aggregate') %>% ggplot(aes(x = time, y = value, color = sector, linetype = scenario)) + 
            facet_grid(name ~ area, scales = 'free') + geom_line() + theme(strip.text.y.right = element_text(angle = 0)) +
            geom_vline(xintercept = baseline$simulation$parameters['t.shock', ], linetype = 'dashed', linewidth = .4) + 
            labs(title = paste0('Shock: ', shock.title), subtitle = paste0(i, ', industry')))
  }
  dev.off()
}

# i <- 'households'
# sh.ock <- 8
# filter(df, type == i & shock == sh.ock & sector == 'aggregate')
