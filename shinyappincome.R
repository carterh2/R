# Author: Carter Hogan
library(shiny)
library(ggplot2)
library(carData)
library(dplyr)
attach(SLID)
SLID <- na.omit(SLID)

ui <- fluidPage(
  titlePanel("Wages by Education and Gender"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "age", label = h3("Age Range"), min = 16, max = 69, value = c(16,69)
        ),
      selectInput(
        inputId = "sex",
        label = h3("Gender"),
        choices = list("Male","Female")
      )
    ), mainPanel(
      fluidRow(
        column(2, align="right",
               plotOutput(outputId = "plot1", 
                          width  = "500px",height = "400px"),            
               plotOutput(outputId = "plot2", width  = "500px",height = "400px") 
               )
        )
      
    )
  )
)

server = function(input, output) {
  output$plot1 = renderPlot({
    ggplot(SLID, aes(x = education, y = wages)) + geom_point() + labs(
      title = "Scatterplot with Regression of Education on Wages (All Ages, Both Male and Female)") +geom_smooth(
        method='lm', formula= y~x) + xlab("Education") +ylab("Wages") 
    })
  output$plot2 = renderPlot({ 
    df = SLID %>% filter(sex == input$sex, age >= input$age[1]  & age <= input$age[2])
    ggplot(df, aes(x = education, y = wages)) + geom_point() + labs(
                                                                    title = "Scatterplot with Regression of Education on Wages User-Adjusted") +geom_smooth(
                                                                      method='lm', formula= y~x) +xlab("Education") +ylab("Wages")
    })
}


shinyApp(ui = ui, server = server)
  