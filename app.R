#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
# Download data and unzip
system("curl -s http://users.csc.tntech.edu/~elbrown/access_log.bz2 | bunzip2 > mylog.log")
# Read in the data, this gives a nice data frame, no need to do .csv
df = read.table('mylog.log', fill = TRUE, header = FALSE, stringsAsFactors = FALSE,numerals = "no.loss")
# Add column names
colnames(df)=c('host','ident','authuser','date','time_zone','request','status','bytes','Webpage','Browser')
# Work with date, time and time zone
df$date=strptime(df$date,"[%d/%b/%Y:%H:%M:%S")
df$time_zone = as.numeric(substring(df$time_zone,1,3))
# Generate a standard date and time to do comparison
df$SD_dateNtime = df$date + df$time_zone*60^2
# Extract the standard date from the last field
df$SD_date =  as.Date(substring(as.character(df$SD_dateNtime),1,10))

# Make status numeric 
df$status = as.numeric(df$status)
# Remove row with wrong status
df = df[-which(is.na(df$status)),]
# Make bytes numeric
df$bytes = as.numeric(df$bytes)

# Created for bytes investigation
dff = df
dff = dff[-which(is.na(dff$bytes)),]


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  # titlePanel("Log file explorer"),
  
  # Sidebar with a slider input for number of bins
  navbarPage("Log File Explorer by Beichen Su",
             tabPanel("Time Series Plots",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("select_3", label = h3("Select a status(Update!)"), 
                                      choices = list("None" = 0, "200 OK" = 200, "404 Not Found" = 404, "500 Internal Server Error" = 500), 
                                      selected = 0),
                          selectInput("select_1", label = h3("Select a time break for request histogram"), 
                                      choices = list("days" = "days", "weeks" = "weeks", "months" = "months"), 
                                      selected = "days"),
                          actionButton('go',
                                       'update')
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("Frequency")
                        )
                      )
             ),
             
             tabPanel("Status",
                      numericInput("num", label = h3("Enter a status code to see counts(eg. 200, 404, 500)"), value = 200),
                      # Show a plot of the generated distribution
                      mainPanel(
                        verbatimTextOutput("value"),
                        plotOutput("Status")
                      )
                      
             ),
             tabPanel("Data returned in bytes",
                      column(3,numericInput("min", label = h3("Enter the minimum number of bytes"), value = 0)
                      ),
                      column(4,actionButton('goo',
                                            'update')),
                      mainPanel(
                        plotOutput("Data_size")
                      )
             )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  value = reactiveValues(mydata = df)
  
  # Defined the dataset based on users' behavior
  
  observeEvent(input$go,{
    if (input$select_3 == 0) {
      temp = df
    } else {
      temp = df[which(df$status == input$select_3),]
    }
    value$mydata = temp
  })
  
  valueB = reactiveValues(mydata = dff)
  observeEvent(input$goo,{
    tempp = dff
    tempp = tempp[which(tempp$bytes >= input$min),]
    valueB$mydata = tempp
  })
  
  output$Frequency <- renderPlot({
    hist(value$mydata$SD_date,breaks = input$select_1, freq = TRUE, xlab = "Date", ylab = "Frequency", main = "Traffic to sites")
  })
  
  output$Data_size <- renderPlot({
    plot(valueB$mydata$SD_date,valueB$mydata$bytes,xlab =  "Date", ylab = "Data size(bytes)", type = 'l')
  })
  
  output$Status <- renderPlot({
    library(ggplot2)
    ggplot(data=df, aes(x=format(df$status))) + geom_bar() + xlab('Status') + ylab('Count') 
  })
  
  output$value <- renderPrint({length(which(df$status == input$num))
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

