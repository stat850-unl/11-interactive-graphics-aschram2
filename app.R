library(shiny)
library(tidyverse)
library(dplyr)
library(shinydashboard)
library(DT)
library(imager)
library(ggplot2)

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')
cocktail_ingredients <- data.frame(cocktails$ingredient)
drink <- cocktails$drink_thumb

plotType <- function(number_of_ingredients, type) {
  switch(type,
         Histogram = hist(number_of_ingredients),
         Barplot = barplot(number_of_ingredients),
         Boxplot = boxplot(number_of_ingredients))
}


ui <- fluidPage(
  titlePanel("Cocktail Recipes"),
  textInput('search2', "Search 2"),
  DTOutput('cocktails'),
  selectInput(inputId="Columns",
              label = "Columns",
             choices = c("alcoholic" = "alcoholic",
              "category" = "category",
              "glass" = "glass",
              "iba" = "iba")),
  plotOutput('plot2'),
  bootstrapPage(
    radioButtons("pType", "Choose plot type:",
                 list("Histogram", "Barplot", "Boxplot")),
    plotOutput('plot')
  )
)


  server <- function(input, output) {

      output$cocktails <- DT::renderDataTable({
        DT::datatable(cocktails, filter = list(position = "top"), escape = FALSE)
      })
      output$plot <- renderPlot({
        plotType(cocktails$ingredient_number, input$pType)
      })
      output$plot2 <- renderPlot({
        ggplot(cocktails,aes(x=.data[[input$Columns]]))+geom_bar()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
      })
  }

shinyApp(ui, server)
