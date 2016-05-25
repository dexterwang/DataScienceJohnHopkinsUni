library(shiny)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {

  #load libraries
library(shiny)
library(ggplot2)

  #load data
  data(diamonds)
  diamonds$clarity<-as.factor(diamonds$clarity)
  diamonds$cut<-as.factor(diamonds$cut)
  diamonds$color<-as.factor(diamonds$color)

  dataset <- reactive({
    subset(diamonds,price >= as.numeric(input$price_range[1]) & price <= as.numeric(input$price_range[2]))
    })
  #data<- diamonds

  sliderValues <- reactive({input$price_range[1]})


  #render plots
  output$distPlot <- renderPlot({

    data<-dataset()
    g<-ggplot(data,aes_string(x=input$x,y=input$y,col=input$z,group=1))
    if(input$plot_type=="scatter plot"){
      g+geom_point()
      }else if(input$plot_type=="box plot"){
        ggplot(data,aes_string(x=input$x,y=input$y,col=input$z))+geom_boxplot()
      }
        #else if (input$plot_type=="bar chart"){
        #ggplot(data,aes_string(x=input$x,group=input$y))+geom_bar(aes(fill = input$z), position = "dodge",stat='identity')
        #}

        })

  output$text_out <- renderPrint({

    sliderValues()
    })

  #render detailed data tables.
  output$mytable1 = renderDataTable({
    diamonds[which(diamonds$cut=='Fair'), input$show_vars, drop = FALSE]
    },options = list(bSortClasses = TRUE, aLengthMenu = c(5, 10, 20), iDisplayLength = 15))

  output$mytable2 = renderDataTable({
    diamonds[which(diamonds$cut=='Good'), input$show_vars, drop = FALSE]
    },options = list(bSortClasses = TRUE, aLengthMenu = c(5, 10, 20), iDisplayLength = 15))

  output$mytable3 = renderDataTable({
    diamonds[which(diamonds$cut=='Very Good'), input$show_vars, drop = FALSE]
    },options = list(bSortClasses = TRUE, aLengthMenu =c(5, 10, 20), iDisplayLength = 15))

  output$mytable4 = renderDataTable({
    diamonds[which(diamonds$cut=='Premium'), input$show_vars, drop = FALSE]
    },options = list(bSortClasses = TRUE, aLengthMenu = c(5, 10, 20), iDisplayLength = 15))

  output$mytable5 = renderDataTable({
    diamonds[which(diamonds$cut=='Ideal'), input$show_vars, drop = FALSE]
    },options = list(bSortClasses = TRUE, aLengthMenu = c(5, 10, 20), iDisplayLength = 15))


  datasetInput <- reactive({
    switch(input$download_dataset,
           "Fair" = diamonds[which(diamonds$cut=='Fair'),],
           "Good" = diamonds[which(diamonds$cut=='Good'),],
           "Very Good" = diamonds[which(diamonds$cut=='Very Good'),],
           "Premium" = diamonds[which(diamonds$cut=='Premium'),],
           "Ideal" = diamonds[which(diamonds$cut=='Ideal'),],
           "All" = diamonds
           )
  })
  
  output$table <- renderTable({
    head(datasetInput(),10)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$dataset, '.csv', sep='') },
    content = function(file) {
      write.csv(datasetInput(), file,row.names=FALSE)
    }
  )


  })