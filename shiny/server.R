shinyServer(function(input, output) {
  
  #------------------------------ Input Data ------------------------------ 
  
  # Subset string
  output$subsetStr <- renderUI({
    textInput('subsetStr', "Subset String:", value = TRUE)
  })
  
  # Data import:
  Dataset <- eventReactive(input$subsetButton,
                      
    {
    # Load the default input data and subset
    load("evalData_random.RData")
    if (length(input$subsetStr) == 0)
      return(random.df)
    else
      return(subset(random.df, 
                    subset=eval(isolate(parse(text=input$subsetStr)))))
  })
  
  # Select variables dynamicially:
  output$varselect <- renderUI({
    
    if (identical(Dataset(), '') || 
        identical(Dataset(),data.frame()))
      return(NULL)
    
    # Variable selection:    
    selectInput("vars", "Variables to Display:",
                names(Dataset()),
                selected = c("Date","Course","Instructor",
                             "Question.STR","Response",
                             "Response.n","Sample.n"),
                multiple = TRUE, selectize = TRUE)            
  })
  
  # Show table:
  output$table <- renderDataTable({
    if (is.null(input$vars) || 
        length(input$vars)==0) 
      return(NULL)
    else
      return(Dataset()[,input$vars,drop=FALSE])
  }, 
  options = list(lengthMenu = c(10, 30, 50), 
  pageLength = 5))
  
  #------------------------------ Plot Data ------------------------------ 
  
  # Select question
  output$question <- renderUI({
    
    if (identical(Dataset(), '') || 
        identical(Dataset(),
        data.frame()))
      return(NULL)
    
    # Variable selection:    
    selectInput("qvar", 
                "Select Question:",
                choices = as.character(unique(Dataset()[["Question.STR"]])),
                selectize = TRUE)            
  })
  
  # Select grouping variable
  output$group <- renderUI({
    
    if (identical(Dataset(), '') || 
        identical(Dataset(),
                  data.frame()))
      return(NULL)
    
    # Variable selection:    
    selectInput("gvar", 
                "Select Grouping Variable:",
                choices = c("Course","Course.Prefix","Course.Suffix",
                            "Date","Instructor.Random"),
                selected="Course.Prefix",
                selectize = TRUE)            
  })
  
  output$plot <- renderPlot({
    
    library(ggplot2)
    library(plyr)
    library(scales)
    
    if (identical(Dataset(), '') || 
        identical(Dataset(),data.frame()) ||
        is.null(input$gvar) ||
        is.null(input$qvar))
      return(NULL)
    
    # Aggregate
    subset.df <- subset(Dataset(),
                        Question.STR==input$qvar)
    subset.df[[input$gvar]] <- as.factor(subset.df[[input$gvar]]) 
    in.text1 <- paste0("aggregate(cbind(Sample.n,Response.n)~",
                      input$gvar,
                      "+Response,data=subset.df,FUN=sum)")
    agg.df <- eval(parse(text=in.text1))
    agg.df$Percent <- with(agg.df, Response.n / Sample.n)
    in.text2 <- paste0("ggplot(agg.df, aes(x=",
                       input$gvar,
                       ",y=Percent,fill=Response)) + 
                        coord_flip() + 
                        geom_bar(position = \"fill\", stat=\"identity\") + 
                        scale_y_continuous(labels = percent_format())")
    agg.gg <- eval(parse(text=in.text2))
    print(agg.gg)
  })

})