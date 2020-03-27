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

# DATA
COUNTRIES <- read.csv("./Base Datatables/COUNTRIES.csv")
D <- COUNTRIES %>%
  group_by(Year) %>%
  top_n(n = 25, wt = Total) %>%
  mutate(Rank = Rank * 120) %>%
  ungroup() %>%
  arrange(Year, -Total) %>%
  mutate(textHover = paste(dollar(Total, accuracy = 1, scale = 1, prefix = "£"), "million"))
D$Code <- countrycode(D$Country, "country.name", "iso2c") %>% tolower()


# PLOT
p <- ggplot(D) +
  geom_col(aes(x = Rank, y = Total), width = 100, fill = "azure3", color = "black") + # Columns
  labs(title = '{closest_state}', x = "", y = "Value (£ million)",
       caption = paste("Source: Export Statistics Scotland", Yr,"\n",
                       "Estimates are rounded to the nearest £5 million.")
       ) + # Labels
  theme_minimal() + # Theme
  geom_text(aes(x = Rank, y = -600, label = Country), hjust = 1) + # Names
  geom_text(aes(x = Rank, y = Total + 200, label = as.character(textHover)), hjust = 0, color = "black") + # Values
  geom_flag(aes(x = Rank, y = -300,  country = Code), size = 10) + # Flags
  scale_y_continuous(labels = scales::comma, expand = expand_scale(add = c(10, 2000))) + # Format y-axis values
  scale_x_reverse( expand = expand_scale(add = c(-1,-1))) + # Highest values on top
  coord_flip(clip = "off", expand = TRUE) + # Flip
  transition_states(Year, transition_length = 4, state_length = 1, wrap = TRUE) + # Animate
  theme(
    plot.title = element_text(hjust = 0, size = 20),
    plot.margin = margin(0,2,0,3,"cm"),
    axis.text.y  = element_blank()
  )

# ANIMATION
country_animate <- animate(p, fps = 30, duration = 30, width = 800, height = 600)
# Saving GIF File
anim_save(country_animate, filename = "./www/CountryAnimation.gif")

