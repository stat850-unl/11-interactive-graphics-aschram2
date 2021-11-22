# Animated and Interactive Graphics

## Instructions

Use the [TidyTuesday Cocktail Data](https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-05-26/readme.md) to create a shiny applet containing the following:

- A visual exploration of the cocktail data
- A way for a user to search through cocktail recipes by ingredient

Your applet should make use of reactivity in both components, but the specifics of what you do and how you do it are up to you.

Once you're satisfied with your applet, upload it to [ShinyApps.io](https://www.shinyapps.io/) and edit this README to contain a link to your applet.'

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')

# Link Below
https://aschram2.shinyapps.io/11-interactive-graphics-aschram2/








#cocktail_cat <- data.frame(cocktails$category)
#drink <- cocktails$drink_thumb

#library(imager)
#im <- load.image(drink)
#plot(im)
# plot(cocktails$category)
