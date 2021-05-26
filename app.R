## libraries ----
suppressPackageStartupMessages({
    library(shiny)
    library(shinyjs)
    library(shinydashboard)
    library(ggplot2)
})

## functions ----

# source("R/func.R") # put long functions in external files

# display debugging messages in R if local, 
# or in the console log if remote
debug_msg <- function(...) {
    is_local <- Sys.getenv('SHINY_PORT') == ""
    txt <- paste(...)
    if (is_local) {
        message(txt)
    } else {
        shinyjs::logjs(txt)
    }
}

## tabs ----

# you can put complex tabs in separate files and source them
#source("ui/main_tab.R")
#source("ui/info_tab.R")

## main_tab ----
main_tab <- tabItem(
    tabName = "main_tab",
    p("This app will teach you about distributions.")
)

## unif_tab ----
unif_tab <- tabItem(
    tabName = "unif_tab",
    h2("Uniform Distribution"),
    fluidRow(
        column(width = 4,
               numericInput("unif_n", "N", value = 10, min = 1, max = 10000, step = 1),
               numericInput("unif_min", "Minimum", value = 0),
               numericInput("unif_max", "Maximum", value = 1),
               actionButton("unif_submit", "Simulate")
        ),
        column(width = 8,
               plotOutput("unif_plot")
        )
    )
    )
    

## normal_tab ----
normal_tab <- tabItem(
    tabName = "normal_tab",
    h2("Normal Distribution"),
    fluidRow(
        column(width = 4,
                numericInput(inputId = "normal_n", label = "N", value = 10, 
                             min = 1, max = 10000, step = 1),
                numericInput(inputId = "normal_mean", label = "Mean", value = 0),
                numericInput(inputId = "normal_sd", label = "Standard deviation", value = 1),
                actionButton(inputId = "normal_submit", label = "Simulate")
               ),
        column(width = 8,
               plotOutput("normal_plot"))
))


# if the header and/or sidebar get too complex, 
# put them in external files and uncomment below 
# source("ui/header.R") # defines the `header`
# source("ui/sidebar.R") # defines the `sidebar`


## UI ----
ui <- dashboardPage(
    skin = "red",
    # header, # if sourced above
    dashboardHeader(title = "Simulation"),
    # sidebar, # if sourced above
    dashboardSidebar(
        # https://fontawesome.com/icons?d=gallery&m=free
        sidebarMenu(
            id = "tabs",
            menuItem("Main", tabName = "main_tab",
                     icon = icon("home")),
            menuItem("Uniform", tabName = "unif_tab",
                     icon = icon("ruler-horizontal")),
            menuItem("Normal", tabName = "normal_tab",
                     icon = icon("bell"))
        )
    ),
    dashboardBody(
        shinyjs::useShinyjs(),
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"), # links to www/custom.css
            tags$script(src = "custom.js") # links to www/custom.js
        ),
        tabItems(
            main_tab,
            unif_tab,
            normal_tab
        )
    )
)


## server ----
server <- function(input, output, session) {
    ## unif_submit ----
    observeEvent(input$unif_submit, {
        debug_msg("unif_submit", input$unif_submit)
        # input check
        is_numeric <- is.numeric(input$unif_n)
        is_int <- as.integer(input$unif_n) == input$unif_n
        is_pos <- input$unif_n >= 1
        
        if(!is_numeric || !is_int || !is_pos){
            shiny::showNotification("Bad n")
            return()
        }
        
        debug_msg("unif_min", input$unif_min)
        # input check
        is_numeric <- is.numeric(input$unif_min)
        if(!is_numeric){
            shiny::showNotification("Bad min")
            return()
        }
        # simulate data
        data <- runif(
            n = input$unif_n,
            min =  input$unif_min,
            max = input$unif_max
        )
        
        df <- data.frame(
            x = data
        )
        # draw plot
        
        p <- ggplot(df, aes(x = x)) +
                geom_histogram(bins = 20) +
                xlim(input$unif_min, input$unif_max)
        
        output$unif_plot <- renderPlot(p)
    })
    
    ## normal_submit ----
    observeEvent(input$normal_submit, {
        debug_msg("normal_submit", input$normal_submit)
        
        # simulate data
        data <- rnorm(
            n = input$normal_n,
            mean =  input$normal_mean,
            sd = input$normal_sd
        )
        
        df <- data.frame(
            x = data
        )
        # draw plot
        
        xmin <- input$normal_mean - (4*input$normal_sd)
        xmax <- input$normal_mean + (4*input$normal_sd)
        
        p <- ggplot(df, aes(x = x)) +
            geom_density() +
            stat_function(fun = dnorm, n = 101, color = "red", 
                          args = list(mean = input$normal_mean,
                                      sd = input$normal_sd)) +
            scale_x_continuous(n.breaks = 9, limits = c(xmin, xmax))
        
        output$normal_plot <- renderPlot(p)
    })
} 

shinyApp(ui, server)