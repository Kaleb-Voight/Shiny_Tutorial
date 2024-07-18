#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Shiny LM Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),

            actionButton("go", "Plot Linear Data"),
            
            textOutput("modelSummary"),
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("lmPlot"),
           tableOutput("contents")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    lmdata <- reactiveValues()
    
    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })

    observeEvent(input$go,{
        update_lm()
    })
    
    update_lm <- function(){
        lmdata$model <- lm(y ~ x, data = dataInput())
    }
    
    output$distPlot <- renderPlot({
        plot(dataInput()$x, dataInput()$y,
             main = "Scatter Plot",
             xlab = "X Data", 
             ylab = "Y Data")
    
    })
    
    output$lmPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y,
        main = "Linear Model Plot",
        xlab = "X Data", 
        ylab = "Y Data")
        abline(lmdata$model)
        
        slope <- coef(lmdata$model)[2]
        intercept <- coef(lmdata$model)[1]
        
    })
    
    output$modelSummary <- renderText({
    req(lmdata$model)
    
    slope <- coef(lmdata$model)[2]
    intercept <- coef(lmdata$model)[1]
    correlation <- cor(dataInput()$x, dataInput()$y)
    
    paste("Slope:", round(slope, 3),
          ", Intercept:", round(intercept, 2),
          ", Correlation:", round(correlation, 4))
    })
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
