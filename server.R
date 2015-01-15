library(shiny)
library(tm)
library(wordcloud)
library(XML)
library(SnowballC)
library(shinyapps)

HtmltoTxt <- function (url)                           
{ library(XML) 
  
  html <- htmlTreeParse(url,useInternal=TRUE)          
  text <- unlist(xpathApply(html,'//p',xmlValue)) 
  text <- gsub('\\n',' ',text)
  text <- paste(text,collapse=' ')
  return(text)
}

# 1956-2012: democrat party manifesto  
add <- "http://www.presidency.ucsb.edu/ws/index.php?pid=29"
manifesto.d <- "placeholder"
for (i in 1:13) 
{ manifesto.d[i] <- paste(add,i+600,sep="")}
manifesto.d[13]
url.2008 <- "http://www.presidency.ucsb.edu/ws/index.php?pid=78283"
url.2012 <- "http://www.presidency.ucsb.edu/ws/index.php?pid=101962"
manifesto.d <- c(manifesto.d,url.2008,url.2012) 

# 1956-2012: republican party manifesto
add2 <- "http://www.presidency.ucsb.edu/ws/index.php?pid=25"
manifesto.r <- "placeholder"
for (j in 1:13)
{ manifesto.r[j] <- paste(add2,j+837,sep="")}
manifesto.r[13]
url.2008 <- "http://www.presidency.ucsb.edu/ws/index.php?pid=78545"
url.2012 <- "http://www.presidency.ucsb.edu/ws/index.php?pid=101961"
manifesto.r <- c(manifesto.r,url.2008,url.2012)

# 1956-2012: democrat presidential speech
speech.d <- c("http://www.presidency.ucsb.edu/ws/index.php?pid=101968",
             "http://www.presidency.ucsb.edu/ws/index.php?pid=78284",
             "http://www.presidency.ucsb.edu/ws/index.php?pid=25971",
             "http://www.presidency.ucsb.edu/ws/index.php?pid=25963",
             "http://www.presidency.ucsb.edu/ws/index.php?pid=53253",
             "http://www.presidency.ucsb.edu/ws/index.php?pid=25958",
             "http://www.presidency.ucsb.edu/ws/index.php?pid=25961",
             "http://www.presidency.ucsb.edu/ws/index.php?pid=25972",
             "http://www.presidency.ucsb.edu/ws/index.php?pid=44909",
             "http://www.presidency.ucsb.edu/ws/index.php?pid=25953",
             "http://www.presidency.ucsb.edu/ws/index.php?pid=25967",
             "http://www.presidency.ucsb.edu/ws/index.php?pid=25964",
             "http://www.presidency.ucsb.edu/ws/index.php?pid=26467",
             "http://www.presidency.ucsb.edu/ws/index.php?pid=25966",
             "http://www.presidency.ucsb.edu/ws/index.php?pid=75172")
speech.d <- rev(speech.d)

# 1956-2012: republican presidential speech 
speech.r <- c("http://www.presidency.ucsb.edu/ws/index.php?pid=101966",
              "http://www.presidency.ucsb.edu/ws/index.php?pid=78576",
              "http://www.presidency.ucsb.edu/ws/index.php?pid=72727",
              "http://www.presidency.ucsb.edu/ws/index.php?pid=25954",
              "http://www.presidency.ucsb.edu/ws/index.php?pid=25960",
              "http://www.presidency.ucsb.edu/ws/index.php?pid=21352",
              "http://www.presidency.ucsb.edu/ws/index.php?pid=25955",
              "http://www.presidency.ucsb.edu/ws/index.php?pid=40290",
              "http://www.presidency.ucsb.edu/ws/index.php?pid=25970",
              "http://www.presidency.ucsb.edu/ws/index.php?pid=6281",
              "http://www.presidency.ucsb.edu/ws/index.php?pid=3537",
              "http://www.presidency.ucsb.edu/ws/index.php?pid=25968",
              "http://www.presidency.ucsb.edu/ws/index.php?pid=25973",
              "http://www.presidency.ucsb.edu/ws/index.php?pid=25974",
              "http://www.presidency.ucsb.edu/ws/index.php?pid=10583")
speech.r <- rev(speech.r)

# read data
html <- c(manifesto.d,speech.d,manifesto.r,speech.r)
text <- 'placeholder'

for (k in 1:60){
  text[k] <- HtmltoTxt(html[k])
}

data <- Corpus(VectorSource(text))
summary(data)

# server function

shinyServer(function(input, output) { 
  
  cloud.d <- reactive({
    
    year <- (as.numeric(input$select1)-1952)/4    
    
    wordcloud <- tm_map(data[year],content_transformer(removePunctuation))
    wordcloud <- tm_map(wordcloud,content_transformer(tolower)) 
    wordcloud <- tm_map(wordcloud,content_transformer(removeNumbers))
    wordcloud <- tm_map(wordcloud,content_transformer(removeWords),stopwords("english"))
    wordcloud <- tm_map(wordcloud,content_transformer(removeWords),c('may','will'))
    wordcloud <- tm_map(wordcloud,content_transformer(stripWhitespace))
    wordcloud <- tm_map(wordcloud,content_transformer(stemDocument))
    
    dtm <- DocumentTermMatrix(wordcloud)
    wordlist.d <- colSums(as.matrix(dtm))    
    return(wordlist.d)
    
  })
  
  cloud.r <- reactive({
    
    year <- (as.numeric(input$select1)-1952)/4 + 30    
    
    wordcloud <- tm_map(data[year],content_transformer(removePunctuation))
    wordcloud <- tm_map(wordcloud,content_transformer(tolower)) 
    wordcloud <- tm_map(wordcloud,content_transformer(removeNumbers))
    wordcloud <- tm_map(wordcloud,content_transformer(removeWords),stopwords("english"))
    wordcloud <- tm_map(wordcloud,content_transformer(removeWords),c('may','will'))
    wordcloud <- tm_map(wordcloud,content_transformer(stripWhitespace))
    wordcloud <- tm_map(wordcloud,content_transformer(stemDocument))
    
    dtm <- DocumentTermMatrix(wordcloud)
    wordlist.r <- colSums(as.matrix(dtm))
    
    return(wordlist.r)
    
  })
  
  cloud.d.s <- reactive({
    
    year <- (as.numeric(input$select1)-1952)/4 + 15 
    
    wordcloud <- tm_map(data[year],content_transformer(removePunctuation))
    wordcloud <- tm_map(wordcloud,content_transformer(tolower)) 
    wordcloud <- tm_map(wordcloud,content_transformer(removeNumbers))
    wordcloud <- tm_map(wordcloud,content_transformer(removeWords),stopwords("english"))
    wordcloud <- tm_map(wordcloud,content_transformer(removeWords),c('may','will'))
    wordcloud <- tm_map(wordcloud,content_transformer(stripWhitespace))
    wordcloud <- tm_map(wordcloud,content_transformer(stemDocument))
    
    dtm <- DocumentTermMatrix(wordcloud)
    wordlist.d.s <- colSums(as.matrix(dtm))
    
    return(wordlist.d.s)
    
  })
  
  cloud.r.s <- reactive({
    
    year <- (as.numeric(input$select1)-1952)/4 + 45    
    
    wordcloud <- tm_map(data[year],content_transformer(removePunctuation))
    wordcloud <- tm_map(wordcloud,content_transformer(tolower)) 
    wordcloud <- tm_map(wordcloud,content_transformer(removeNumbers))
    wordcloud <- tm_map(wordcloud,content_transformer(removeWords),stopwords("english"))
    wordcloud <- tm_map(wordcloud,content_transformer(removeWords),c('may','will'))
    wordcloud <- tm_map(wordcloud,content_transformer(stripWhitespace))
    wordcloud <- tm_map(wordcloud,content_transformer(stemDocument))
    
    dtm <- DocumentTermMatrix(wordcloud)
    wordlist.r.s <- colSums(as.matrix(dtm))
    
    return(wordlist.r.s)
    
  })
  
  
  # check it's manifesto or speech
  
  
  output$plot1 <- renderPlot({
    wordcloud(names(cloud.d()),cloud.d(),scale=c(5,.5),max.word=100,rot.per=.3,color=brewer.pal(6,'Dark2')) 
  })
  
  output$plot2 <- renderPlot({
    wordcloud(names(cloud.r()),cloud.r(),scale=c(5,.5),max.word=100,rot.per=.3,color=brewer.pal(6,'Dark2'))
  })  
  
  output$plot3 <- renderPlot({
    wordcloud(names(cloud.d.s()),cloud.d.s(),scale=c(5,.5),max.word=100,rot.per=.3,color=brewer.pal(6,'Dark2')) 
  })
  
  output$plot4 <- renderPlot({
    wordcloud(names(cloud.r.s()),cloud.r.s(),scale=c(5,.5),max.word=100,rot.per=.3,color=brewer.pal(6,'Dark2'))
  })
  
  
})    
