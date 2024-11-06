This is the official repository for the input-output stock-flow consistent models developed in Work Package 5 of the JUST2CE project. The progamming language/environment we use is `R`. New codes are going to be released throughout 2024 and 2025.


---
title: "Circular Economy Experiments of the SFC Model - JBF Code"
author: "Oriol Vallès Codina"
date: "`r Sys.Date()`"
output:
  # word_document: default
  # keep_tex: true

  # pdf_document:
  #   toc: true
  #   toc_depth: 3
  #   number_sections: true
  html_document:
    toc: TRUE
    toc_float: TRUE
format:
  html:
    self-contained:
      true
---

# Initialization 

```{r setup, warning = FALSE, message = FALSE, exclude = TRUE}
knitr::opts_chunk$set(echo = TRUE, fig.width = 9, fig.height = 10)

library(reshape2)
library(dplyr)
library(ggplot2)
library(printr)
library(kableExtra)
library(flextable)
library(tools)

sectors <- c('Manufacturing', 'Agriculture', 'Services', 'Waste', 'Recycling')
countries <- c('EU', 'RoW')
directory <- '' # directory <- "G:/.shortcut-targets-by-id/17a5070DCsX5nmgKQ8AQdb9R86eOkcJFj/WP5 material/Experiments/deliverable2/"
variable.table <- read.csv(paste0(directory, 'data/Variable_Definitions.csv'))
scenario.table <- read.csv(paste0(directory, 'data/Scenario_List.csv'), row.names = 1)

#### LOAD FUNCTIONS ####
source(paste0(directory, 'functions/1_Model_rev (JBF_Revised Feb_with public Investment)_adapted.R')) # model
source(paste0(directory, 'functions/auxiliary.R')) # load.init, load.target, return.instr.val
source(paste0(directory, 'functions/plot_variable_function_Feb27.R')) # plotting functions 
#source(paste0(directory, 'functions/5A_Tables (D5.2, 2024).R')) # table functions (BS and TFM)

#### LOAD TARGET VALUES IN WIDE AND LONG FORMAT ####
tmp <- load.target(paste0(directory, 'data/target_values.xlsx'))
target.set <- tmp$long
target.table.0 <- tmp$wide

target.variables <- colnames(target.table.0)
target.indices <- colnames(target.table.0) %in% target.variables
target.table <- target.table.0[ , target.indices]

#### Load Baseline and Scenarios
#baseline <- readRDS(paste0(directory, 'data/baseline_Mar5_JBF.RDS'))
#scenario.list <- readRDS(paste0(directory, 'data/scenario_list_Mar5_JBF.RDS'))
```

# Results

[…] Graphs GDP, Gross Output, Employment (with gender breakdown), CAB, Government deficit and debt to GDP ratio. 

## Baseline

```{r baseline.plots}

# Load Initial Values
init <- load.init(paste0(directory, 'data/initial_values_may20_jbf.xlsx'))
baseline <- run.model(init)
selected.vars <- c('n', 'c', 'ineq', 'va', 'cab', 'nf', 'gdef', 'tb', 'go')

# BASELINE PLOT
pdf(paste0(directory, 'figures/baseline_doublecheck_nov5.pdf'), width = 8, height = 6)
plot.vars(baseline, selected.vars)
plot.type.k(baseline, 'labor.market')
plot.type(baseline, 'ecological')
dev.off()

plot.vars(baseline, selected.vars)
plot.type.k(baseline, 'labor.market')
plot.type(baseline, 'ecological')
```

## Scenarios

```{r scenario.plots}
# SCENARIOS PLOTS
# Pre-saved list
# It just saves them into a list, it does not print them as pdf

#### Load Scenarios
scenario.list <- readRDS(paste0(directory, 'data/scenario_list_Mar5_JBF.RDS'))

scenario.plots <- list()
for (i in 1 : nrow(scenario.table))
{
  # pdf(paste0(directory, 'figures/shock_', i, '_3_NEWRUN.pdf'), width = 10, height = 6)
  
  scenario.plots[[i]] <- list(shock.title = shock.title <- paste0('Shock ', i, ': ', scenario.table$Shock[i]))  
  
  df <- shock.long(baseline, i)
  df$area <- as.factor(df$area)
  levels(df$area) <- countries

  # Selected Vars
  scenario.plots[[i]]$selected.vars <- filter(df, variable %in% selected.vars & sector == 'aggregate') %>% 
    ggplot(aes(x = time, y = value, color = area, linetype = scenario)) + facet_wrap(~ name, scales = 'free') + geom_line() + 
    geom_vline(xintercept = baseline$simulation$parameters['t.shock', ], linetype = 'dashed', linewidth = .4) +
    labs(title = shock.title, subtitle = 'Selected Aggregate Macroeconomic Indicators. Vertical dashed line indicates shock time')
  
 # Labor Market
  category <- 'labor.market'
  scenario.plots[[i]]$labor.market <- df %>% filter(type == category & sector != 'aggregate') %>% 
    ggplot(aes(x = time, y = value, color = sector, linetype = scenario)) + geom_line() + facet_grid(name ~ area, scales = 'free') + 
    theme(strip.text.y.right = element_text(angle = 0)) +
    geom_vline(xintercept = baseline$simulation$parameters['t.shock', ], linetype = 'dashed', linewidth = .4) + 
    labs(title = shock.title, subtitle = 'Industry-level Labour Market Indicators. Vertical dashed line indicates shock time')

 # Ecological
  category <- 'ecological'
  excl.vars <- c('x_mat', 'gamma_A')

  scenario.plots[[i]]$ecological <- filter(df, !variable %in% excl.vars & type == category & sector == 'aggregate') %>% 
    ggplot(aes(x = time, y = value, color = area, linetype = scenario)) +
    facet_wrap( ~ name, scales = 'free') + geom_line() +
    geom_vline(xintercept = baseline$simulation$parameters['t.shock', ], linetype = 'dashed', linewidth = .4) + 
    labs(title = shock.title, subtitle = 'Aggregate Ecological Indicators. Vertical dashed line indicates shock time')
  
  # Print Plots
  # print(scenario.plots[[i]]$selected.vars)
  # print(scenario.plots[[i]]$labor.market)
  # print(scenario.plots[[i]]$ecological)
  
  # dev.off()
}
```

## Specific Baseline

```{r baseline.run}
recycling.init <- load.init(paste0(directory, 'data/initial_values_mar21_jbf.xlsx'))
recycling.init$parameters['shock', ] <- 0
baseline.run <- run.model(recycling.init)

plot.vars(baseline.run, selected.vars)
```

## Specific Shock

```{r specific.shock.run}
recycling.init$parameters['shock', ] <- 4
recycling.run <- run.model(recycling.init)

# Higher Recycling Rate
i <- 4
shock.title <- paste0('Shock ', i, ': ', scenario.table$Shock[i])
  
df <- shock.long(baseline.run, recycling.run)
df$area <- as.factor(df$area)
levels(df$area) <- countries

# Selected Vars
p.selected.vars <- filter(df, variable %in% selected.vars & sector == 'aggregate') %>% 
    ggplot(aes(x = time, y = value, color = area, linetype = scenario)) + facet_wrap(~ name, scales = 'free') + geom_line() + 
    geom_vline(xintercept = baseline$simulation$parameters['t.shock', ], linetype = 'dashed', linewidth = .4) +
    labs(title = shock.title, subtitle = 'Selected Aggregate Macroeconomic Indicators. Vertical dashed line indicates shock time')

# Labor Market
category <- 'labor.market'
p.labor.market <- df %>% filter(type == category & sector != 'aggregate') %>% 
  ggplot(aes(x = time, y = value, color = sector, linetype = scenario)) + geom_line() + facet_grid(name ~ area, scales = 'free') + 
  theme(strip.text.y.right = element_text(angle = 0)) +
  geom_vline(xintercept = baseline$simulation$parameters['t.shock', ], linetype = 'dashed', linewidth = .4) + 
  labs(title = shock.title, subtitle = 'Industry-level Labour Market Indicators. Vertical dashed line indicates shock time')

# Ecological
category <- 'ecological'
excl.vars <- c('x_mat', 'gamma_A')
p.ecological <- filter(df, !variable %in% excl.vars & type == category & sector == 'aggregate') %>% 
  ggplot(aes(x = time, y = value, color = area, linetype = scenario)) +
  facet_wrap( ~ name, scales = 'free') + geom_line() +
  geom_vline(xintercept = baseline$simulation$parameters['t.shock', ], linetype = 'dashed', linewidth = .4) + 
  labs(title = shock.title, subtitle = 'Aggregate Ecological Indicators. Vertical dashed line indicates shock time')

# Print Plots
print(p.selected.vars)
print(p.labor.market)
print(p.ecological)

pdf(paste0(directory, 'figures/recycling_shock_nov5.pdf'), width = 10, height = 8)
print(p.selected.vars)
print(p.labor.market)
print(p.ecological)
dev.off()
```

# Scenario Analysis

Once the model baseline is empirically calibrated, CE policies can be simulated and their impact on economic, social, and ecological indicators. A table of CE experiments that can be run in the model is shown in table `scenario.table`, divided into three categories: private practices by households and firms, direct government policies, and indirect CE effects from other practices and policies. All shocks except indicated otherwise are run on area 1, Europe, with a common shock intensity $S$.

```{r scenario.analysis}
foo <- list()
for (k in 1 : nrow(scenario.table))
{
  df <- shock.long(baseline, k)
  info <- df[df$shock == 0 & df$time == max(df$time), ]
  info$scenario <- scenario.table$Shock[k]
  info$base.value <- df$value[df$shock == 0 & df$time == max(df$time)]
  info$shock.value <- df$value[df$shock != 0 & df$time == max(df$time)]
  info$diff <- info$shock.value - info$base.value
  info$prcnt <- (info$shock.value / info$base.value - 1) * 100
  info$shock <- k

  foo[[k]] <- info
}
foo <- bind_rows(foo, .id = 'shock.name')

foo$name <- gsub('\\n', ' ', foo$name)
foo$shock.type <- setNames(scenario.table$Type, scenario.table$Shock)[foo$shock]
foo$shock.name <- scenario.table$Shock[foo$shock]
foo$shock.name <- factor(foo$shock.name, levels = unique(foo$shock.name))
foo$shock.num[foo$shock < 9] <- paste0('1.', foo$shock[foo$shock < 9])
foo$shock.num[foo$shock > 8] <- paste0('2.', foo$shock[foo$shock > 8] - 8)
foo$shock.num[foo$shock > 10] <- paste0('3.', foo$shock[foo$shock > 10] - 10)
foo$prcnt[is.infinite(foo$prcnt)] <- NA

# Shock-Baseline Absolute Difference, Last Period by Shock
p.abs <- na.omit(foo) %>% filter(abs(diff) > 10) %>% ggplot(aes(y = reorder(name, diff), x = diff, fill = shock.num, alpha = sector)) +
  xlab('') + ylab('') + geom_bar(stat = 'identity') + facet_grid(type ~ area, scales = 'free') +
  geom_vline(xintercept = 0, linetype = 'dashed', linewidth = .5, color = gray(0.25, 0.75)) +
  labs(title = 'Shock-Baseline Absolute Difference, Last Period by Shock',
       subtitle = 'Only absolute values larger than 10 are shown.') +
  theme(legend.position="bottom") + guides(fill = guide_legend(nrow = 3, byrow = TRUE), alpha = guide_legend(nrow = 3, byrow = TRUE)) +
  theme(strip.text.y.right = element_text(angle = 0 ))

# Shock-Baseline Percentage Difference, Last Period by Shock
p.prcnt <- na.omit(foo) %>% filter(abs(prcnt) > 0.1 & prcnt > -500) %>% ggplot(aes(y = reorder(name, prcnt), x = prcnt, fill = shock.name, alpha = sector)) +
  xlab('') + ylab('') + geom_bar(stat = 'identity') + facet_grid(type ~ area, scales = 'free') +
  geom_hline(yintercept = 0, linetype = 'dashed', linewidth = .4) +
  labs(title = 'Shock-Baseline Percentage Difference, Last Period by Shock') +
  theme(legend.position="bottom") + guides(fill = guide_legend(nrow = 5, byrow = TRUE), alpha = guide_legend(nrow = 4, byrow = TRUE)) +
  xlim(-800, 1000) + theme(strip.text.y.right = element_text(angle = 0 ))

pdf(paste0(directory, 'figures/byshock1jbf_3b.pdf'), width = 12, height = 14)
p.abs
dev.off()


pdf(paste0(directory, 'figures/byshock2jbf_2.pdf'), width = 14, height = 16)
p.prcnt
dev.off()

p.abs
p.prcnt
```

## Scenario Table

```{r results = 'asis'}
kable(scenario.table[ , 1 : 2], row.names = FALSE, booktabs = T, format = 'html') %>% pack_rows(index = table(scenario.table$Type)) %>% kable_styling()
```

## Scenario Analysis of Selected Variables

```{r scenario.selected}
# Function to replace spaces based on threshold
replace_spaces <- function(text, threshold) {
  if (nchar(text) > threshold) {
    # If the number of characters is greater than the threshold, replace the third space
    text <- gsub("^(\\S+\\s+\\S+\\s+\\S+)", "\\1\n", text)
  } else {
    # If the number of characters is less than the threshold, replace the second space
    text <- gsub("^(\\S+\\s+\\S+)", "\\1\n", text)
  }
  text <- gsub("\n ", "\n", text)
  return(text)
}

foo$name <- gsub(' \\(', '\n\\(', foo$name)
foo$area <- as.factor(foo$area)
levels(foo$area) <- countries
levels(foo$shock.name) <- sapply(levels(foo$shock.name), replace_spaces, threshold = 35)
levels(foo$shock.name)[10] <- "More Selective Govt Spending\ntowards Recycling Efficiency"
levels(foo$shock.name)[nchar(levels(foo$shock.name)) < 29] <- gsub('\\n', ' ', levels(foo$shock.name)[nchar(levels(foo$shock.name)) < 29])
levels(foo$shock.name)[c(2, 6, 7, 10)] <- c('Change in Consumption\nComposition towards Services',
                                            "Lower Extraction (or Conversion)\nRate of Matter",
                                            "Lower Discarding Rate\nof Socio-Economic Stock",
                                            "More Selective Govt Spending\ntowards Recycling Efficiency")

selected <- c('va', 'n', 'nf', 'ineq', 'emis', 'mat')
p.selected <- foo %>% filter(variable %in% selected) %>% ggplot(aes(y = shock.name, x = prcnt, fill = area)) + geom_bar(stat = 'identity', position = position_dodge()) + facet_grid(~ name, scales = 'free') +
  theme(strip.text.y.right = element_text(angle = 0), legend.position = 'bottom') + scale_y_discrete(limits = rev) + ylab('')

pdf(paste0(directory, 'figures/some_variables_by_shock_nov5.pdf'), height = 7, width = 10.5)
p.selected
dev.off()

p.selected
```

