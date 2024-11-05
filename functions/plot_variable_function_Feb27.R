library(ggplot2)
library(reshape2)
library(dplyr)

# PRODUCE A PDF PLOT WITH ALL VARIABLE CATEGORIES IN THE FOLDER FIGURES
# ARGUMENTS SUCH AS RESULTS AND RUN.NAME, e.g. plot.run(results, 'stable')
plot.run <- function(res, run.name)
{
  plot.labels <- apply(expand.grid(c('Z1', 'Z2'), variable.table$label[variable.table$dimension == 'aggregate']), 1, paste, collapse="_")
  dt <- res$simulation$variables[plot.labels[plot.labels %in% rownames(res$simulation$variables)], ] %>% melt(varnames = c('var', 't'))
  dt$time <- as.numeric(substring(dt$t, 2))
  dt$area <- substr(dt$var, 1, 2)
  dt$variable <- paste0(setNames(variable.table$name, variable.table$label)[substring(dt$var, 4)], '\n(', substring(dt$var, 4),')')
  dt$type <- setNames(variable.table$type, variable.table$label)[substring(dt$var, 4)]
  
  pdf(paste0(directory, 'figures/variable_plots_', run.name, '.pdf'), width = 10)
  
  plot.ratio(res)
  
  for (i in unique(dt$type))
  {
    print(filter(dt, type == i) %>% ggplot(aes(x = time, y = value, color = area)) + 
    facet_wrap(~ variable, scales = 'free') + geom_line() + 
      geom_hline(data = filter(target.set, type == i), mapping = aes(yintercept = value), linetype = 'dashed', linewidth = .5) +
      ggtitle(paste0(i, ', Aggregate')))
  }
  
  plot.labels <- apply(expand.grid(c('Z1', 'Z2'), variable.table$label[variable.table$dimension == 'industry']), 1, paste, collapse="_")
  plot.labels <- apply(expand.grid(plot.labels, as.character(1 : 10)), 1, paste, collapse="-")
  dt <- res$simulation$variables[plot.labels[plot.labels %in% rownames(res$simulation$variables)], ] %>% melt(varnames = c('var', 't'))
  dt$time <- as.numeric(substring(dt$t, 2))
  dt$area <- substr(dt$var, 1, 2)
  dt$variable <- sapply(strsplit(substring(dt$var, 4), split = '-'), function (x) x[1])
  dt$unit <- setNames(variable.table$unit, variable.table$label)[dt$variable]
  dt$name <- ifelse(dt$unit == '', 
                    paste0(setNames(variable.table$name, variable.table$label)[dt$variable], '\n(', dt$variable,')'),
                    paste0(setNames(variable.table$name, variable.table$label)[dt$variable], '\n(', dt$variable,', in ', dt$unit, ')'))
  dt$sector <- sapply(strsplit(substring(dt$var, 4), split = '-'), function (x) x[2])
  
  dt <- dt %>% filter((dt$area == 'Z1' & as.numeric(dt$sector) < 6) | (dt$area == 'Z2' & as.numeric(dt$sector) > 5))
  dt$sector <- ifelse(as.numeric(dt$sector) > 5, as.character(as.numeric(dt$sector) - 5), dt$sector)
  dt$type <- setNames(variable.table$type, variable.table$label)[dt$variable]
  
  for (i in unique(dt$type))
  {
    print(dt %>% filter(type == i) %>% ggplot(aes(x = time, y = value, color = sector)) + 
            facet_grid(name ~ area, scales = 'free') + geom_line() + ggtitle(paste0(i, ', Industry')) + theme(strip.text.y.right = element_text(angle = 0)))
  }
  dev.off()
}

plot.ratio <- function(res)
{
  dt <- target.set
  nPeriods <- res$simulation$parameters['nPeriods', ]
  selected <- target.set$var[target.set$var %in% rownames(res$simulation$variables)]
  dt[selected, 'ratio'] <- res$simulation$variables[selected, nPeriods] / target.set[selected, 'value']
  if(sum(is.na(dt[c('Z1_fd', 'Z2_fd'), 'ratio']))) dt[c('Z1_fd', 'Z2_fd'), 'ratio'] <- c(sum(res$simulation$variables[paste0('Z1_d-', 1 : 5), nPeriods]), sum(res$simulation$variables[paste0('Z2_d-', 6 : 10), nPeriods])) / target.set[c('Z1_fd', 'Z2_fd'), 'value']
  if(sum(is.na(dt[c('Z1_go', 'Z2_go'), 'ratio']))) dt[c('Z1_go', 'Z2_go'), 'ratio'] <- c(sum(res$simulation$variables[paste0('x-', 1 : 5), nPeriods]), sum(res$simulation$variables[paste0('x-', 6 : 10), nPeriods])) / target.set[c('Z1_go', 'Z2_go'), 'value']
  if(sum(is.na(dt[c('Z1_debt.gdp', 'Z2_gebt.gdp'), 'ratio']))) dt[c('Z1_debt.gdp', 'Z2_debt.gdp'), 'ratio'] <- res$simulation$variables[c('Z1_b_s', 'Z2_b_s'), nPeriods] / res$simulation$variables[c('Z1_va', 'Z2_va'), nPeriods] / target.set[c('Z1_debt.gdp', 'Z2_debt.gdp'), 'value']
  # dt[c('Z1_fd', 'Z2_fd'), 'ratio'] <- dt[c('Z1_fd', 'Z2_fd'), 'ratio'] 
  # dt[c('Z1_go', 'Z2_go'), 'ratio'] <- dt[c('Z1_go', 'Z2_go'), 'ratio'] / target.set[c('Z1_go', 'Z2_go'), 'value']
  # dt[c('Z1_debt.gdp', 'Z2_debt.gdp'), 'ratio'] <- dt[c('Z1_debt.gdp', 'Z2_debt.gdp'), 'ratio'] / target.set[c('Z1_debt.gdp', 'Z2_debt.gdp'), 'value']
  
  print(ggplot(filter(dt, !is.na(dt$ratio)), aes(x = variable, y = ratio, fill = area)) + 
          geom_bar(stat = 'identity', position = 'dodge') + facet_wrap(~ variable, scales = 'free') + 
          geom_hline(yintercept = 1, linetype = 'dashed', linewidth = .5) + ggtitle('Goodness-of-Fit Ratios'))
  
  # return(list(ratio = setNames(dt$ratio, rownames(dt)), fitness = sum((dt$ratio - 1) ^ 2, na.rm = T) / sum(!is.na(dt$ratio))))
}
# plot.labels <- c(grep('Z1_b_', rownames(results$simulation$variables), value = T),
#                  grep('Z2_b_', rownames(results$simulation$variables), value = T))

# DISPLAY THE PLOT OF A PARTICULAR CATEGORY, AGGREGATE-LEVEL
# ARGUMENTS results and category, e.g. plot.type(results, 'govt.central.bank')
plot.type <- function (res, category)
{
  plot.labels <- apply(expand.grid(c('Z1', 'Z2'), variable.table$label[variable.table$dimension == 'aggregate']), 1, paste, collapse="_")
  dt <- res$simulation$variables[plot.labels[plot.labels %in% rownames(res$simulation$variables)], ] %>% melt(varnames = c('var', 't'))
  dt$time <- as.numeric(substring(dt$t, 2))
  dt$area <- substr(dt$var, 1, 2)
  dt$variable <- substring(dt$var, 4)
  dt$unit <- setNames(variable.table$unit, variable.table$label)[dt$variable]
  dt$name <- ifelse(dt$unit == '', 
                    paste0(setNames(variable.table$name, variable.table$label)[dt$variable], '\n(', dt$variable,')'),
                    paste0(setNames(variable.table$name, variable.table$label)[dt$variable], '\n(', dt$variable,', in ', dt$unit, ')'))
  dt$type <- setNames(variable.table$type, variable.table$label)[dt$variable]
  dt$area <- as.factor(dt$area)
  levels(dt$area) <- countries
  
  excl.vars <- c('x_mat', 'gamma_A')
  print(dt %>% filter(type == category & !variable %in% excl.vars) %>% ggplot(aes(x = time, y = value, color = area)) +
          geom_hline(data = filter(target.set, type == category), mapping = aes(yintercept = value), linetype = 'dashed', linewidth = .5) +
          facet_wrap(~ name, scales = 'free') + geom_line() + ggtitle(paste0('Aggregate ', toTitleCase(gsub('.', ' ', category, fixed = TRUE)), ' Indicators')))
}

# DISPLAY THE PLOT OF A PARTICULAR CATEGORY, INDUSTRY-LEVEL
# ARGUMENTS results and category, e.g. plot.type(results, 'govt.central.bank')
plot.type.k <- function (res, category)
{
  plot.labels <- apply(expand.grid(c('Z1', 'Z2'), variable.table$label[variable.table$dimension == 'industry']), 1, paste, collapse="_")
  plot.labels <- apply(expand.grid(plot.labels, as.character(1 : 10)), 1, paste, collapse="-")
  dt <- res$simulation$variables[plot.labels[plot.labels %in% rownames(res$simulation$variables)], ] %>% melt(varnames = c('var', 't'))
  dt$time <- as.numeric(substring(dt$t, 2))
  dt$area <- substr(dt$var, 1, 2)
  dt$variable <- sapply(strsplit(substring(dt$var, 4), split = '-'), function (x) x[1])
  dt$unit <- setNames(variable.table$unit, variable.table$label)[dt$variable]
  dt$name <- ifelse(dt$unit == '', 
                    paste0(setNames(variable.table$name, variable.table$label)[dt$variable], '\n(', dt$variable,')'),
                    paste0(setNames(variable.table$name, variable.table$label)[dt$variable], '\n(', dt$variable,', in ', dt$unit, ')'))
  dt$sector <- sapply(strsplit(substring(dt$var, 4), split = '-'), function (x) x[2])
  
  dt <- dt %>% filter((dt$area == 'Z1' & as.numeric(dt$sector) < 6) | (dt$area == 'Z2' & as.numeric(dt$sector) > 5))
  dt$sector <- ifelse(as.numeric(dt$sector) > 5, as.character(as.numeric(dt$sector) - 5), dt$sector)
  dt$sector <- sectors[as.numeric(dt$sector)]
  dt$type <- setNames(variable.table$type, variable.table$label)[dt$variable]
  dt$area <- as.factor(dt$area)
  levels(dt$area) <- countries
  
  print(dt %>% filter(type == category) %>% ggplot(aes(x = time, y = value, color = sector)) + 
          facet_grid(name ~ area, scales = 'free') + geom_line() + ggtitle(paste0('Industry-level ', toTitleCase(gsub('.', ' ', category, fixed = TRUE)), ' Indicators'))
        + theme(strip.text.y.right = element_text(angle = 0)))
}

plot.vars <- function (res, vars)
{
  plot.labels <- apply(expand.grid(c('Z1', 'Z2'), vars), 1, paste, collapse="_")
  dt <- res$simulation$variables[plot.labels[plot.labels %in% rownames(res$simulation$variables)], ] %>% melt(varnames = c('var', 't'))
  dt$time <- as.numeric(substring(dt$t, 2))
  dt$area <- substr(dt$var, 1, 2)
  dt$variable <- substring(dt$var, 4)
  dt$unit <- setNames(variable.table$unit, variable.table$label)[dt$variable]
  dt$name <- ifelse(dt$unit == '', 
                    paste0(setNames(variable.table$name, variable.table$label)[dt$variable], '\n(', dt$variable,')'),
                    paste0(setNames(variable.table$name, variable.table$label)[dt$variable], '\n(', dt$variable,', in ', dt$unit, ')'))
  dt$name <- factor(dt$name, levels = unique(dt$name))
  dt$type <- setNames(variable.table$type, variable.table$label)[dt$variable]
  dt$area <- as.factor(dt$area)
  levels(dt$area) <- countries
  
  print(dt %>% ggplot(aes(x = time, y = value, color = area)) + 
          facet_wrap(~ name, scales = 'free', labeller = labeller(facet_var = as_labeller(vars))) + geom_line() + 
          geom_hline(data = filter(target.set, var.label %in% vars), mapping = aes(yintercept = value), linetype = 'dashed', linewidth = .5) +
          ggtitle('Selected Aggregate Macroeconomic Indicators')) 
}

plot.instr <- function (universe)
{
  instr <- dplyr :: bind_rows(lapply(universe, function (x) cbind(x$instruments, fitness = x$fitness)), .id = 'iteration')
  instr$iteration <- as.numeric(instr$iteration)
  instr$area <- substr(instr$label, 1, 2)
  instr$variable <- substring(instr$label, 4)
  uni.fit <- sapply(universe, function (x) x$fitness)

  mean.instr <- instr[instr$fitness == -75, ] %>% group_by(label) %>% summarize(value = mean(value)) %>% as.data.frame
  mean.instr$area <- substr(mean.instr$label, 1, 2)
  mean.instr$variable <- substring(mean.instr$label, 4)
  print(instr %>% ggplot(aes(x = iteration, y = value, color = area)) + 
          geom_rect(aes(xmin = which(uni.fit == -75)[1], xmax = length(uni.fit), ymin = -Inf, ymax = Inf), alpha = 0.1, color = NA, fill = 'lightgreen') + 
          geom_line(size = 1.1) + 
    facet_wrap(variable ~ ., scales = 'free_y') + xlab('iteration') + 
      labs(title = 'Instrument Variables', subtitle = 'Dashed horizontal line is the stock-flow consistent average') + 
    geom_hline(data = mean.instr, mapping = aes(yintercept = value, color = area), linetype = 'dashed'))
  
  return(list(instruments = instr, mean = mean.instr))
}

plot.uni <- function (universe)
{
  dt.list <- list()
  for(k in 1 : length(universe))
  { 
    dt <- target.set
    res <- universe[[k]]
    nPeriods <- res$simulation$parameters['nPeriods', ]
    selected <- target.set$var[target.set$var %in% rownames(res$simulation$variables)]
    dt[selected, 'ratio'] <- res$simulation$variables[selected, nPeriods] / target.set[selected, 'value']
    dt[c('Z1_fd', 'Z2_fd'), 'ratio'] <- c(sum(res$simulation$variables[paste0('Z1_d-', 1 : 5), nPeriods]), sum(res$simulation$variables[paste0('Z2_d-', 6 : 10), nPeriods]))
    dt[c('Z1_go', 'Z2_go'), 'ratio'] <- c(sum(res$simulation$variables[paste0('x-', 1 : 5), nPeriods]), sum(res$simulation$variables[paste0('x-', 6 : 10), nPeriods]))
    dt[c('Z1_debt.gdp', 'Z2_debt.gdp'), 'ratio'] <- res$simulation$variables[c('Z1_b_s', 'Z2_b_s'), nPeriods] / res$simulation$variables[c('Z1_va', 'Z2_va'), nPeriods]
    dt[c('Z1_fd', 'Z2_fd'), 'ratio'] <- dt[c('Z1_fd', 'Z2_fd'), 'ratio'] / target.set[c('Z1_fd', 'Z2_fd'), 'value']
    dt[c('Z1_go', 'Z2_go'), 'ratio'] <- dt[c('Z1_go', 'Z2_go'), 'ratio'] / target.set[c('Z1_go', 'Z2_go'), 'value']
    dt[c('Z1_debt.gdp', 'Z2_debt.gdp'), 'ratio'] <- dt[c('Z1_debt.gdp', 'Z2_debt.gdp'), 'ratio'] / target.set[c('Z1_debt.gdp', 'Z2_debt.gdp'), 'value']
    
    dt.list[[k]] <- na.omit(dt)
  }
  foo <- bind_rows(dt.list, .id = 'iter')
  foo$iter <- as.numeric(foo$iter)
  print(ggplot(foo, aes(x = iter, y = ratio, color = variable)) + theme(strip.text.y.right = element_text(angle = 0)) + 
          geom_line(linewidth = 1.1, show.legend = FALSE) + facet_grid(name ~ area, scales = 'free') +
          geom_hline(yintercept = 1, linetype = 'dashed', linewidth = .5) + ggtitle('Goodness-of-Fit Ratios'))
}


#### PLOT SHOCK

plot.shock <- function(baseline, scenario, category)
{
  df <- shock.long(baseline, scenario)
  shock.title <- scenario.table$Shock[scenario]

  print(filter(df, type == category & sector == 'aggregate') %>% ggplot(aes(x = time, y = value, color = area, linetype = scenario)) +
          facet_wrap(~ name, scales = 'free') + geom_line() +
          geom_vline(xintercept = baseline$simulation$parameters['t.shock', ], linetype = 'dashed', linewidth = .4) + 
          # geom_hline(data = filter(target.set, type == i), mapping = aes(yintercept = value), linetype = 'dashed', linewidth = .5) +
          labs(title = paste0('Shock: ', shock.title), subtitle = paste0(category, ', aggregate. Vertical dashed line indicates shock time')))

  if(nrow(df %>% filter(type == category & sector != 'aggregate')) > 0)
  {
    print(df %>% filter(type == category & sector != 'aggregate') %>% ggplot(aes(x = time, y = value, color = sector, linetype = scenario)) + 
          facet_grid(name ~ area, scales = 'free') + geom_line() + theme(strip.text.y.right = element_text(angle = 0)) +
          geom_vline(xintercept = baseline$simulation$parameters['t.shock', ], linetype = 'dashed', linewidth = .4) + 
          labs(title = paste0('Shock: ', shock.title), subtitle = paste0(category, ', industry. Vertical dashed line indicates shock time')))
  }
# return (list(baseline = df[df$shock == 0, ], shock = df[df$shock == scen, ]))
}

#### PlOT SHOCK SUMMARY

plot.shock.summary <- function (baseline, scenario)
{
  df <- shock.long(baseline, scenario)
  shock.title <- scenario.table$Shock[scenario]
  
  info <- df[df$shock == 0 & df$time == 1, ]
  info$diff <- df$value[df$shock == scenario & df$time == max(df$time)] - df$value[df$shock == 0 & df$time == max(df$time)]
  
  info$name <- gsub('\\n', ' ', info$name)
  if (nrow(info %>% filter(abs(diff) > 1)))
  {
  info %>% filter(abs(diff) > 1) %>% ggplot(aes(y = reorder(name, diff), x = diff, fill = area, alpha = sector)) + xlab('') + ylab('') +
    geom_bar(stat = 'identity') + facet_grid(type ~ ., scales = 'free') +
    geom_vline(xintercept = 0, linetype = 'dashed', linewidth = .4) + theme(strip.text.y.right = element_text(angle = 0 )) +
    labs(title = paste0('Shock: ', shock.title), subtitle = 'Shock-Baseline Difference, Last Period. Only Absolute Values Larger than 1')
  } else {print('No variables with absolute difference larger than 1.')}
}

#### PlOT SHOCK PERCENT

plot.shock.prcnt <- function (baseline, scenario)
{
  df <- shock.long(baseline, scenario)
  shock.title <- scenario.table$Shock[scenario]
  
  info <- df[df$shock == 0 & df$time == max(df$time), ]
  info$diff <- (df$value[df$shock == scenario & df$time == max(df$time)] / df$value[df$shock == 0 & df$time == max(df$time)] - 1) * 100
  info <- info[abs(info$value) > .1, ]
  
  info$name <- gsub('\\n', ' ', info$name)
  if (nrow(info %>% filter(abs(diff) > .1)))
  {
    info %>% filter(abs(diff) > .1) %>% ggplot(aes(y = reorder(name, diff), x = diff, fill = area, alpha = sector)) + xlab('') + ylab('') +
      geom_bar(stat = 'identity') + facet_grid(type ~ ., scales = 'free') +
      geom_vline(xintercept = 0, linetype = 'dashed', linewidth = .4) + theme(strip.text.y.right = element_text(angle = 0 )) +
      labs(title = paste0('Shock: ', shock.title), subtitle = 'Shock-Baseline Percent Difference, Last Period. Only Absolute Values Larger than 0.1')
  } else {print('No variables with absolute percentage difference larger than 0.1.')}
}