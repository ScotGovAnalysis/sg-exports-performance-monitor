# CREATING GIFS
library(ggplot2)     # Used for creating plots.
library(gganimate)   # Extends the ggplot2 package with the frame aesthetic.
library(tweenr)      # Makes the animation smooth.
library(magick)      # Takes a set of images and coverts them into a .gif format.
library(gifski)
library(plyr)
library(dplyr)
library(magrittr)
#devtools::install_github("ellisp/ggflags")
library(ggflags)
library(countrycode)
library(grid)

# VARIABLES
SECTORS <- read.csv("./Base Datatables/SECTORS.csv")
variable_choices <- c("All", "International","RUK")

for (i in seq_len(length(variable_choices))) {
  SetValue <- variable_choices[i]

  SetRank <- case_when(
    variable_choices[i] == "All" ~ "RankAll",
    variable_choices[i] == "International" ~ "Rank_int",
    variable_choices[i] == "RUK" ~ "Rank_ruk"
  )

  # CHOOSING DATA SET

  DD <- SECTORS %>%
    mutate(Value = SECTORS[,c(as.character(SetValue))]) %>%
    mutate(Rank = SECTORS[,c(as.character(SetRank))]) %>%
    group_by(Year) %>%
    top_n(n = 25, wt = Value) %>%
    mutate(Rank = Rank * 120) %>%
    ungroup() %>%
    mutate(textHover = paste(dollar(Value, accuracy = 1, scale = 1, prefix = "£"), "million"))

  # PLOT
  p <- ggplot(DD) +
    geom_col(aes(x = Rank, y = Value), width = 100, fill = "azure3", color = "black") + # Columns
    labs(title = '{closest_state}', x = "", y = "Value (£ million)",
         caption = paste("Source: Export Statistics Scotland", Yr,"\n",
                         "Estimates are rounded to the nearest £5 million.")
         ) + # Labels
    theme_minimal() + # Theme
    geom_text(aes(x = Rank, y = -600, label = Sector), hjust = 1) + # Names
    geom_text(aes(x = Rank, y = Value + 200, label = as.character(textHover)), hjust = 0, color = "black") + # Values
    scale_y_continuous(labels = scales::comma, expand = expand_scale(add = c(10, 2000))) + # Format y-axis values
    scale_x_reverse( expand = expand_scale(add = c(-1,-1))) + # Highest values on top
    coord_flip(clip = "off", expand = TRUE) + # Flip
    transition_states(Year, transition_length = 4, state_length = 1, wrap = TRUE) + # Animate
    theme(
      plot.title = element_text(hjust = 0, size = 20),
      plot.margin = margin(0,2,0,7,"cm"),
      axis.text.y  = element_blank()
    )

  # ANIMATION
  sectorAnimate <- animate(p, fps = 30, duration = 30, width = 800, height = 600)

  # Saving GIF File
  anim_save(sectorAnimate, filename = paste("./www/SectorAnimation",variable_choices[i],".gif", sep = ""))
}
