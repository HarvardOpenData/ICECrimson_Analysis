source('styleguide.R')
library(stringr)
ethnicity <- read.csv("crimson_ice_responses.csv")
agree_disagree_levels <- c("Strongly agree",
                           "Somewhat agree",
                           "Neither agree nor disagree",
                           "Somewhat disagree",
                           "Strongly disagree",
                           "I don't know enough to say")
color_scheme <- c("#485976",
                  "#88c2d2",
                  "#eea241",
                  "#fc3839",
                  "#8c1717",
                  "#5e5e5e")

make_graph <- function(x_var, title) {
  n = length(ethnicity[,x_var])
  props <- as.data.frame(table(ethnicity[x_var])/n)
  props$lower <- props$Freq - 1.96 * sqrt(props$Freq * (1-props$Freq) / n)
  props$lower[props$lower<0] <- 0
  props$upper <- props$Freq + 1.96 * sqrt(props$Freq * (1-props$Freq) / n)
  dodge <- position_dodge(width=0.9)
  props$Var1 <- factor(props$Var1,levels = agree_disagree_levels)
  p <- ggplot(data = props, aes(Var1, Freq)) +
    geom_col(position = dodge, fill = color_scheme) +
    geom_errorbar(aes(ymin = lower, ymax = upper), position = dodge, width = 0.25) +
    labs(title=title, subtitle = "N = 118",caption="Error Bars represent 95% CIs") +
    xlab("Response") +
    ylab("Proportion") + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    theme_hodp() +
    theme(legend.position="none",
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14))
  return(p)
}

# Correct to call ICE
correct <- make_graph("crimson_ice.crimson_correct",
                      "Do you agree with The Crimson's decision to ask ICE for comment?")
correct
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

# Safety of undocumented students
safety <- make_graph("crimson_ice.undermined_safety",
                     "Do you agree that The Crimson undermined undocumented students' safety?")
safety
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

# Obligation to call ICE
obligation <- make_graph("crimson_ice.comment_obligation",
                        "Do you agree that The Crimson had an obligation to ask ICE for comment?")
obligation
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

# Boycott of Crimson
boycott <- make_graph("crimson_ice.boycott",
                      "Do you agree with student organizations' boycott of The Crimson?")
boycott
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))


# ICE deportations
deportations <- read.csv("ice_deportations.csv")
g <- ggplot(data = deportations, aes(x = factor(year), y = number)) +
  geom_bar(stat = "identity", fill = monochrome[1]) +
  labs(title="ICE deportations in Boston Area of Responsibility") +
  xlab("Year") +
  ylab("Number of deportations") + 
  theme_hodp() +
  theme(legend.position="none",
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))
g
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))


# Attemp to lump likert scale values
make_graph_lumped <- function(x_var, title) {
  n = length(ethnicity[,x_var])
  props <- as.data.frame(table(ethnicity[x_var])/n)
  agree <- sum(props$Freq[3],props$Freq[5])
  disagree <- sum(props$Freq[4],props$Freq[6])
  neither <- sum(props$Freq[1],props$Freq[2])
  props <- data.frame(Var1 = c('agree', 'disagree', 'neither/uninformed'), 
                      Freq = c(agree, disagree, neither))
  
  props$lower <- props$Freq - 1.96 * sqrt(props$Freq * (1-props$Freq) / n)
  props$lower[props$lower<0] <- 0
  props$upper <- props$Freq + 1.96 * sqrt(props$Freq * (1-props$Freq) / n)
  props$Var1 <- factor(props$Var1,levels = c('agree', 'disagree', 'neither/uninformed'))
  dodge <- position_dodge(width=0.9)
  color_scheme <-  c("#485976","#eea241","#fc3839")
  p <- ggplot(data = props, aes(Var1, Freq)) +
    geom_col(position = dodge, fill = color_scheme) +
    geom_errorbar(aes(ymin = lower, ymax = upper), position = dodge, width = 0.25) +
    labs(title=title, subtitle = "N = 118",caption="Error Bars represent 95% CIs") +
    xlab("Response") +
    ylab("Proportion") + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    theme_hodp() +
    theme(legend.position="none",
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14))
  return(p)
}
correct <- make_graph_lumped("crimson_ice.crimson_correct",
                      "Do you agree with The Crimson's decision to ask ICE for comment?")
correct

# Safety of undocumented students
safety <- make_graph_lumped("crimson_ice.undermined_safety",
                     "Do you agree that The Crimson undermined undocumented students' safety?")
safety

# Obligation to call ICE
obligation <- make_graph_lumped("crimson_ice.comment_obligation",
                         "Do you agree that The Crimson had an obligation to ask ICE for comment?")
obligation

# Boycott of Crimson
boycott <- make_graph_lumped("crimson_ice.boycott",
                      "Do you agree with student organizations' boycott of The Crimson?")
boycott


