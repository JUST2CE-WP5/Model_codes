init <- load.init(paste0(directory, 'data/initial_state_Feb25.xlsx'))
init$parameters['shock', ] <- 0
baseline <- run.model(init)

init$parameters['shock', ] <- sh.ock <- 1
shocked <- run.model(init)

# BASELINE
res <- baseline

# Aggregate
plot.labels <- apply(expand.grid(c('Z1', 'Z2'), variable.table$label[variable.table$dimension == 'aggregate']), 1, paste, collapse="_")
dt <- res$simulation$variables[plot.labels[plot.labels %in% rownames(res$simulation$variables)], ] %>% melt(varnames = c('var', 't'))
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
dt <- res$simulation$variables[plot.labels[plot.labels %in% rownames(res$simulation$variables)], ] %>% melt(varnames = c('var', 't'))
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

res <- shocked

# Aggregate
plot.labels <- apply(expand.grid(c('Z1', 'Z2'), variable.table$label[variable.table$dimension == 'aggregate']), 1, paste, collapse="_")
dt <- res$simulation$variables[plot.labels[plot.labels %in% rownames(res$simulation$variables)], ] %>% melt(varnames = c('var', 't'))
dt$time <- as.numeric(substring(dt$t, 2))
dt$area <- substr(dt$var, 1, 2)
dt$variable <- substring(dt$var, 4)
dt$name <- paste0(setNames(variable.table$name, variable.table$label)[dt$variable], '\n(', dt$variable,')')
dt$type <- setNames(variable.table$type, variable.table$label)[dt$variable]
dt$sector <- 'aggregate'
dt$scenario <- 'shock'
dt$shock <- sh.ock
df <- rbind(df, dt)

# Industry
plot.labels <- apply(expand.grid(c('Z1', 'Z2'), variable.table$label[variable.table$dimension == 'industry']), 1, paste, collapse="_")
plot.labels <- apply(expand.grid(plot.labels, as.character(1 : 10)), 1, paste, collapse="-")
dt <- res$simulation$variables[plot.labels[plot.labels %in% rownames(res$simulation$variables)], ] %>% melt(varnames = c('var', 't'))
dt$time <- as.numeric(substring(dt$t, 2))
dt$area <- substr(dt$var, 1, 2)
dt$variable <- sapply(strsplit(substring(dt$var, 4), split = '-'), function (x) x[1])
dt$name <- paste0(setNames(variable.table$name, variable.table$label)[dt$variable], '\n(', dt$variable, ')')
dt$sector <- sapply(strsplit(substring(dt$var, 4), split = '-'), function (x) x[2])

dt <- dt %>% filter((dt$area == 'Z1' & as.numeric(dt$sector) < 6) | (dt$area == 'Z2' & as.numeric(dt$sector) > 5))
dt$sector <- ifelse(as.numeric(dt$sector) > 5, as.character(as.numeric(dt$sector) - 5), dt$sector)
dt$type <- setNames(variable.table$type, variable.table$label)[dt$variable]
dt$scenario <- 'shock'
dt$shock <- sh.ock

df <- rbind(df, dt)

pdf(paste0(directory, 'figures/scenario', sh.ock, '.pdf'))
for (i in unique(df$type))
{
  shock.title <- scenario.list$Shock[which(scenario.list$Shock.Number == sh.ock)]
  print(filter(df, type == i & (shock == 0 | shock == sh.ock) & sector == 'aggregate') %>% ggplot(aes(x = time, y = value, color = area, linetype = scenario)) +
          facet_wrap(~ name, scales = 'free') + geom_line() +
          geom_hline(data = filter(target.set, type == i), mapping = aes(yintercept = value), linetype = 'dashed', linewidth = .5) +
          labs(title = paste0('Shock: ', shock.title), subtitle = paste0(i, ', aggregate')))
}
for (i in unique(df$type[df$sector != 'aggregate']))
{
  print(df %>% filter(type == i & (shock == 0 | shock == sh.ock) & sector != 'aggregate') %>% ggplot(aes(x = time, y = value, color = sector, linetype = scenario)) + 
          facet_grid(name ~ area, scales = 'free') + geom_line() + theme(strip.text.y.right = element_text(angle = 0)) +
          labs(title = paste0('Shock: ', shock.title), subtitle = paste0(i, ', industry')))
}
dev.off()