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
    order.df$Instructor <- order.df$Instructor.Random
    if (length(input$subsetStr) == 0)
      return(order.df)
    else
      return(subset(order.df, 
                    subset=eval(isolate(parse(text=input$subsetStr)))))
  })
  
  # Select filter variable dynamicially:
  output$filterselect <- renderUI({
    
    if (is.null(Dataset()))
      return(NULL)
    
    # Variable selection:    
    selectInput("fvar", "Filter variable to use:",
                c("None", "Course", "Course.Prefix","Course.Suffix","Date",
                  "Instructor","Question.STR","Response"),
                selected = "None", selectize = TRUE)            
  })
  
  # Select filter value dynamicially:
  output$filtervalue <- renderUI({
    
    if (is.null(Dataset()) ||
        is.null(input$fvar))
      return(NULL)
    
    # Variable selection:
    if (input$fvar != "None") {
      inValues <- unique(as.character(Dataset()[[input$fvar]]))
      selectInput("fval", "Filter value to use:", 
                  c("None", inValues[order(inValues)]),
                  selected = "None", selectize = TRUE) 
    }
  })
  
  # Select variables dynamicially:
  output$varselect <- renderUI({
    
    if (is.null(Dataset()))
      return(NULL)
    
    # Variable selection:    
    selectInput("vars", "Variables to Display:",
                names(Dataset())[order(names(Dataset()))],
                selected = c("Date","Course","Instructor",
                             "Question.STR","Response",
                             "Response.n","Sample.n"),
                multiple = TRUE, selectize = TRUE)            
  })
  
  # Show table:
  output$table <- renderDataTable({
    
    if (is.null(Dataset()) ||
        is.null(input$vars) || 
        length(input$vars)==0 ||
        is.null(input$fvar) ||
        is.null(input$fval)) 
      return(NULL)
    
    else
      if (input$fvar != "None" && input$fval != "None")
        return(subset(Dataset(),
                      subset=eval(isolate(parse(text=
                                                paste0("as.character(",
                                                  input$fvar, ")==\"", 
                                                  input$fval, "\"",
                                                  "&",
                                                  input$subsetStr
                                                  )))))[,input$vars,drop=FALSE])
      else 
        return(Dataset()[,input$vars,drop=FALSE])
    
  }, 
  options = list(lengthMenu = c(10, 30, 50), 
  pageLength = 5))
  
  #------------------------------ Plot Data ------------------------------ 
  
  # Select question
  output$question <- renderUI({
    
    if (is.null(Dataset()))
      return(NULL)
    
    # Variable selection:    
    selectInput("qvar", 
                "Select Question:",
                choices = as.character(unique(Dataset()[["Question.STR"]])),
                selectize = TRUE)            
  })
  
  # Select grouping variable
  output$group <- renderUI({
    
    if (is.null(Dataset()))
      return(NULL)
    
    # Variable selection:    
    selectInput("gvar", 
                "Select Grouping Variable:",
                choices = c("Course","Course.Prefix","Course.Suffix",
                            "Date","Instructor"),
                selected="Course.Prefix",
                selectize = TRUE)            
  })
  
  # Select sorting value dynamicially:
  output$sortvalue <- renderUI({
    
    if (is.null(Dataset()) ||
        is.null(input$qvar))
      return(NULL)
    
    # Variable selection: 
    idx <- Dataset()[["Question.STR"]] == input$qvar
    subData <- Dataset()[idx,]
    inValues <- unique(as.character(subData$Response))
    selectInput("sval", "Select which factor to order by:",
                c("None",
                  inValues[order(inValues)]),
                selected = "None", selectize = TRUE)            
  })
  
  output$plot <- renderPlot({
    
    library(ggplot2)
    library(plyr)
    library(scales)
    
    if (is.null(Dataset()) ||
        is.null(input$gvar) ||
        is.null(input$qvar) ||
        is.null(input$fvar) ||
        is.null(input$sval))
      return(NULL)
    
    # Subset
    if (input$fvar != "None" && !is.na(input$fval))
      if (input$fval != "None")
        subDataset <- subset(Dataset(),
                      subset=eval(isolate(parse(text=
                                                  paste0("as.character(",
                                                         input$fvar, ")==\"", 
                                                         input$fval, "\"",
                                                         "&",
                                                         input$subsetStr
                                                  )))))
      else
        subDataset <- Dataset()
    else 
      subDataset <- Dataset()
    
    # Structure and Aggregate
    subset.df <- subset(subDataset,
                        Question.STR==input$qvar)
    subset.df[[input$gvar]] <- as.factor(subset.df[[input$gvar]]) 
    in.text1 <- paste0("aggregate(cbind(Sample.n,Response.n)~",
                      input$gvar,
                      "+Response,data=subset.df,FUN=sum)")
    agg.df <- eval(parse(text=in.text1))
    agg.df$Percent <- with(agg.df, Response.n / Sample.n)
    
    
    # Sort and plot
    if (input$sval !="None") {
      sort.df <- agg.df
      # Arrange levels
      levels <- levels(sort.df$Response)
      ordering.df <- data.frame(Response=c(input$sval, 
                                           setdiff(levels,input$sval)),
                                Ranking=1:length(levels(sort.df$Response)))
      sort.df <- merge(sort.df,ordering.df,by="Response")
      sort.df <- sort.df[order(sort.df$Ranking),]
      # Sort
      percent.df <- subset(sort.df, subset=Response==input$sval)
      ordering <- order(percent.df$Percent)
      sort.df[[input$gvar]] <- factor(sort.df[[input$gvar]],
                                      levels(sort.df[[input$gvar]])[ordering])
    } else {
      sort.df <- agg.df
    }
    in.text2 <- paste0("ggplot(sort.df, aes(x=",
                       input$gvar,
                       ",y=Percent,fill=Response)) + 
                        coord_flip() + 
                        geom_bar(position = \"fill\", stat=\"identity\") + 
                        scale_y_continuous(labels = percent_format()) +
                        + scale_fill_gradient(low=\"#CEF6CE\",high=\"#0B610B\")")
    sort.gg <- eval(parse(text=in.text2))
    print(sort.gg)
  })

})