# CREATING GIFS
library(ggplot2)     # Used for creating plots.
library(gganimate)   # Extends the ggplot2 package with the frame aesthetic.
library(tweenr)      # Makes the animation smooth.
library(magick)      # Takes a set of images and coverts them into a .gif format.
library(gifski)
library(dplyr)
library(magrittr)
#devtools::install_github("ellisp/ggflags")
library(grid)

# VARIABLES
SetValue <- "All"
#SetValue <- "International"
#SetValue <- "RUK"	

SetRank <- "RankAll"
#SetRank <- "RankInternational"
#SetRank <- "RankRUK"

# CHOOSING DATA SET
SECTORS <- read.csv("SECTORS.csv")
DD <- SECTORS %>%
    mutate(Value = SECTORS[,c(as.character(SetValue))]) %>%
    mutate(Rank = SECTORS[,c(as.character(SetRank))]) %>%
    group_by(Year) %>%
    filter() %>%
    top_n(n = 25, wt = Value) %>%
    mutate(Rank = Rank * 120) %>%
    ungroup()

# PLOT
p <- ggplot(DD) +
        geom_col(aes(x = Rank, y = Value), width = 100, fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(title='{closest_state}', x = "", y = "Value (£ million)") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank, y = -600, label = Sector), hjust = 1) + # Names
        geom_text(aes(x = Rank, y = Value + 200, label = as.character(Value)), hjust = 0, color = "black") + # Values  
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        transition_states(Year, transition_length = 4, state_length = 1) + # Animate
        theme(
            plot.title = element_text(hjust = 0, size = 20),
            plot.margin = margin(0,2,0,7,"cm"),
            axis.text.y  = element_blank()
        )
p

# ANIMATION
animate(p, fps = 30, duration = 30, width = 800, height = 600)

