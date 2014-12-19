#### ui script

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  
  # Application title
  titlePanel("Party Word Clouds"),
  
  p("Jeremy Yang"),
  br(),
  br(),
  
  
  # Sidebar with a slider input for the number of bins
  
  
  fluidRow(
    
    column(1,p("")),
    column(2,h5("Democrat Manifesto:")),
    
    column(2,
           plotOutput('plot1')),
    
    column(1,p("")),
    column(2,h5("Republican Manifesto:")),
    
    column(2,
           plotOutput('plot2'))
  ),
  
  fluidRow(
    
    column(1,p("")),
    column(2,h5("Democrat Speech:")),
    column(2,
           plotOutput('plot3')),
    
    column(1,p("")),
    column(2,h5("Republican Speech:")),
    column(2,
           plotOutput('plot4'))
  ),
  
  br(),
  br(),
  hr(),
  
  fluidRow(
    
    column(5,p("")),
    column(2,
           selectInput("select1",label=h5("select a year:"),choices=as.character(seq(1956,2012,4)),selected="2012")),
    column(5,p(""))
    
    
    
  )
  
  
))
