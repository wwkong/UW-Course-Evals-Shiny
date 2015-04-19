shinyUI(fluidPage(
  
  # Header:
  titlePanel("Shiny - UW Course Evaluations",
             title="Analysis of UW Course Evaluations"),
  
  # Sub-header
  fluidRow(column(12,p("Coded by William Kong. All rights reserved."))),
  
  # Input in sidepanel:
  sidebarPanel(
    
    
#------------------------------ Input Data ------------------------------ 
    
    # Variable selection:
    conditionalPanel(
      condition="input.conditionedPanels==1",
      htmlOutput("varselect")),

    # Filter Variable:
    conditionalPanel(
      condition="input.conditionedPanels==1",
      htmlOutput("filterselect")),

    # Filter Value:
    conditionalPanel(
      condition="input.conditionedPanels==1",
      htmlOutput("filtervalue")),
    
    # Subset String:
    conditionalPanel(
      condition="input.conditionedPanels==1",
      htmlOutput("subsetStr")),

    # Submit Subset
    conditionalPanel(
      condition="input.conditionedPanels==1",
      actionButton("subsetButton","Reload Data")),
    
#------------------------------ Plot Data ------------------------------ 

    # Question selection:
    conditionalPanel(
      condition="input.conditionedPanels==2",
      htmlOutput("question")),

    # Group selection:
    conditionalPanel(
      condition="input.conditionedPanels==2",
      htmlOutput("group")),
    
    # Sorting Value:
    conditionalPanel(
      condition="input.conditionedPanels==2",
      htmlOutput("sortvalue"))
    
  ),

# Main Panel
mainPanel(
  
  tabsetPanel(
    tabPanel("Input", 
             dataTableOutput("table"), 
             value=1),
    tabPanel("Plot", 
             plotOutput("plot", clickId = 'scatterPosn'),
             value=2),
    id="conditionedPanels"
  )
)


))