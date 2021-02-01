## Helper functions for BSL negation analysis
# 19/06/2020
# Bodo Winter

# Function for easily calculating proportions:

proportion <- function(x) round(x / sum(x), 2)

# Function for easily calculating reportable percentages:

percentage <- function(x) str_c(round(x / sum(x), 2) * 100, '%')

# Rounded proportions table:

prop_tab <- function(x) round(prop.table(x), 2)

# Rounded proportions table:

rowprop_tab <- function(x) round(prop.table(x, 1), 2)

# ggplot2 theme:

theme_BSL <- function() {
  theme_minimal(base_size = 16) %+replace% 
    theme(
      legend.position = '',
      axis.title.y = element_text(size = 20,
                                  margin = margin(r = 20, l = 0,
                                                  t = 0, b = 0),
                                  angle = 90,
                                  face = 'bold'),
      axis.title = element_text(size = 20,
                                face = 'bold'),
      axis.text.x = element_text(face = 'bold',
                                 color = 'black',
                                 size = 12,
                                 vjust = 1),
      plot.title = element_text(size = 20,
                           face = 'bold',
                           margin = margin(r = 0, l = 0,
                                           t = 0, b = 10),
                           hjust = 0),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = 'grey80')
    )
}


# ggplot2 theme, with 45 degree rotated x-axis text:

theme_BSL_45 <- function() {
  theme_minimal(base_size = 16) %+replace% 
    theme(
      legend.position = '',
      axis.title.y = element_text(size = 20,
                                  margin = margin(r = 20, l = 0,
                                                  t = 0, b = 0),
                                  angle = 90,
                                  face = 'bold'),
      axis.title = element_text(size = 20,
                                face = 'bold'),
      axis.text.x = element_text(face = 'bold',
                                 color = 'black',
                                 size = 12,
                                 vjust = 1,
                                 hjust = 1,
                                 angle = 45),
      plot.title = element_text(size = 20,
                                face = 'bold',
                                margin = margin(r = 0, l = 0,
                                                t = 0, b = 10),
                                hjust = 0),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = 'grey80')
    )
}