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
  ggplot(data = ethnicity, aes(factor({{x_var}},
                                      levels = agree_disagree_levels))) +
    geom_bar(aes(y=..count../sum(..count..),
                 fill = factor({{x_var}},
                               levels = agree_disagree_levels))) +
    scale_fill_manual(values = color_scheme) +
    labs(title=title) +
    xlab("Response") +
    ylab("Proportion") + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    theme_hodp() +
    theme(legend.position="none",
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14))
}

# Correct to call ICE
correct <- make_graph(crimson_ice.crimson_correct,
                      "Do you agree with The Crimson's decision to ask ICE for comment?")
correct
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

# Safety of undocumented students
safety <- make_graph(crimson_ice.undermined_safety,
                     "Do you agree that The Crimson undermined undocumented students' safety?")
safety
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

# Obligation to call ICE
obligation <- make_graph(crimson_ice.comment_obligation,
                        "Do you agree that The Crimson had an obligation to ask ICE for comment?")
obligation
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

# Boycott of Crimson
boycott <- make_graph(crimson_ice.boycott,
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
