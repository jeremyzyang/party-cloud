#### ui script

shinyUI(fluidPage(
  
  titlePanel("Party Word Clouds"), 
  h5('Yang/Singh'),
  br(),
  h4('Extract party manifesto and presidential speech text from American Presidency Project and compute word clouds for a chosen year.'),
  
  
  fluidRow(
    
    column(5),
    column(2,
           selectInput("select1",label=h4("Select A Year:"),choices=as.character(seq(1956,2012,4)),selected="2012")),
    column(5)),
  
  hr(),
  
  fluidRow(
    
    column(2,h5("Democrat Manifesto")),
    
    column(4,
           plotOutput('plot1')),
    
    column(4,
           plotOutput('plot2')),
    column(2,h5("Republican Manifesto"))
  ),
  
  fluidRow(
    
    column(2,h5("Democrat Speech")),
    column(4,
           plotOutput('plot3')),
    column(4,
           plotOutput('plot4')),
    column(2,h5("Republican Speech"))
    
  )  
    
  )
  
  
)
