library(shiny)
library(ggplot2)

 #load data
 data(diamonds)

 data<-diamonds
 min_price<- round(min(data$price))
 max_price<- round(max(data$price))

# Define UI for application that plots random distributions 
shinyUI(
  navbarPage("Diamonds Browser", inverse = FALSE, collapsable = FALSE,
   tabPanel("Data Visulisation",
    fluidRow(

      sidebarPanel(
        selectInput(inputId = "plot_type",
          label="Plot Type:", 
          choices= c("scatter plot","box plot"),
          selected="scatter plot"),
        sliderInput("price_range", "Price Range:",
          min = min_price, max = max_price, value = c(min_price,max_price)),
        selectInput(inputId = "x",label = "X axis",
          choices = c("carat","cut","color","clarity", "depth","table", "price"),
          selected = "carat"),
        selectInput(inputId = "y",
          label = "Y axis",
          choices = c("carat","cut","color","clarity", "depth","table", "price"),
          selected = "price"), 
        selectInput(inputId = "z",
          label = "Colour axis",
          choices = c("carat","cut","color","clarity", "depth","table", "price"),
          selected = "cut")
        ),

  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot"))
  )
    ,
    fluidRow(

      )
    ),
   tabPanel("Detailed Report",
    sidebarPanel(
      checkboxGroupInput('show_vars', 'Columns in diamonds to show:', names(diamonds),
       selected = names(diamonds)),
      helpText('This page lists all diamonds for you to browse with every aspect of details.You can select/unselect variables to display. The table columns can be sorted, filterd or even searched by keyword.')
      ),
    mainPanel(
      tabsetPanel(id="diamonds_data",
        tabPanel('Fair', dataTableOutput('mytable1')),
        tabPanel('Good', dataTableOutput('mytable2')),
        tabPanel('Very Good', dataTableOutput('mytable3')),
        tabPanel('Premium', dataTableOutput('mytable4')),
        tabPanel('Ideal', dataTableOutput('mytable5'))
        )
      )

    ),
   tabPanel("Download Data",
    sidebarPanel(
      selectInput("download_dataset", "Choose a dataset:", 
        choices = c("Fair", "Good", "Very Good","Premium","Ideal","All"),selected="ALL"),
      downloadButton('downloadData', 'Download')
      ),
    mainPanel(
      helpText("Preview:"),
      tableOutput('table')
      )

    ),
   tabPanel("Documentation",
    helpText('Diamond Browser is a shiny app which allows you to browse diamonds in different ways through a web page.'),
    helpText('The functions of the app includes: '),
    helpText('1. Data Visulisation'),
    helpText('2. Detailed Report'),
    helpText('3. Data Download'),
    helpText('Data Visulisation allows users to visualise our diamond stock.User can specify the attributes in x-axis, y-axis and grouping colour in a scatter chart or box plot to see how price changes over different characteristics of the diamonds. '),
    helpText('"Detailed Report" lists all diamonds for users to browse with every aspect of details.Users can select/unselect variables to display. The table columns can be sorted, filterd or even searched by keyword.'),
    helpText('Data Download allows users to download selected subset of diamonds data as .csv files.'),

     #p(em("Documentation:",a("Diamonds Product Report",href="./readme.html"))),
      p(em("Github Documentation:",a("Data-Science-Data-Products",href="https://github.com/dexterwang/DataScienceJohnHopkinsUni/tree/master/Data%20Products/Week4%20Project/readme.md"))),
      p(em("Source Code:",a("Data-Science-Data-Products",href="https://github.com/dexterwang/DataScienceJohnHopkinsUni/tree/master/Data%20Products/Week4%20Project/")))

    )

   )

  )

