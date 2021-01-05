#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(collapsibleTree)
library(waffle)
library(randomcoloR)
library(plotly)
library(countrycode)

# Data from https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-08-18/readme.md
plants <- read.csv("plants.csv")
threats <- read.csv("threats.csv")
actions <- read.csv("actions.csv")

countries <- sort(unique(plants$country))

ui <- fluidPage(
    dashboardPage(
        skin="green",
        dashboardHeader(
            title="Plant Extinction"
        ),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Overview", tabName = "Overview"),
                menuItem("Explore", tabName = "Explore"),
                menuItem("Map", tabName = "Map"),
                menuItem("Threats", tabName = "Threats"),
                menuItem("Actions", tabName = "Actions"),
                menuItem("Tree", tabName = "Tree"),
                menuItem("Acknowledgements", tabName = "Acknowledgements")
            )
        ),
        dashboardBody(
            tabItems(
                tabItem(
                    tabName="Overview",
                    fluidRow(
                        box(plotOutput("extinction_by_country", height="650px"), width=12)
                    ),
                    fluidRow(
                        box(plotOutput("extinction_by_continent")),
                        box(plotOutput("groups"))
                    )
                    
                ),
                tabItem(
                    tabName="Explore",
                    DT::dataTableOutput("table")
                    
                ),
                tabItem(
                    tabName="Map",
                    box(plotlyOutput("map", height="500px"), width=12, height="600px")
                ),
                tabItem(
                    p("Please note that a species may correspond to multiple threats."),
                    tabName="Threats",
                    fluidRow(
                        box(selectInput("threat_country",
                                        "Choose a country:",
                                        countries)
                        )
                    ),
                    fluidRow(box(plotOutput("threat_waffle")))
                ),
                tabItem(
                    p("Please note that a species may correspond to multiple actions."),
                    tabName="Actions",
                    fluidRow(
                        box(selectInput("action_country", "Choose a country:",
                                        countries
                        )
                        )
                    ),
                    fluidRow(
                        box(plotOutput("action_waffle"))
                    )
                    
                    
                ),
                tabItem(
                    tabName="Tree",
                    p("Note that this may take some time to load."),
                    collapsibleTreeOutput("tree", height="700px")
                ),
                tabItem(
                    tabName="Acknowledgements",
                    h1("Acknowledgements"),
                    p("Thank you to those who made this dataset available: "),
                    tags$ul(
                        tags$li("IUCN Red List (iucnredlist.org)"), 
                        tags$li("Florent Lavergne (behance.net/gallery/98304453/Infographic-Plants-in-Danger)"), 
                        tags$li("Tidy Tuesday (github.com/rfordatascience/tidytuesday)")
                    )
                )
            )
        )
    )
    
)

server <- function(input, output) {
    output$table <- DT::renderDataTable({
        threats_merge <- threats %>%
            filter(threatened == 1) %>%
            group_by(binomial_name) %>%
            summarise(threat = paste0(threat_type, collapse = ", "))
        
        actions_merge <- actions %>%
            filter(action_taken == 1) %>%
            group_by(binomial_name) %>%
            summarise(action = paste0(action_type, collapse = ", "))
        
        dt <- plants %>% left_join(threats_merge, by="binomial_name")  %>%
            left_join(actions_merge, by="binomial_name") %>%
            select(binomial_name, country, continent, group, year_last_seen, threat, action, red_list_category)
        
        dt
    })
    
    output$tree <- renderCollapsibleTree({
        n_continent <- length(unique(plants$continent))
        n_country <- dim(unique(plants[,c("country","continent")]))[1]
        n_group <- dim(unique(plants[,c("country","continent","group")]))[1]
        n_binomial_name <- dim(unique(plants[,c("country","continent","group","binomial_name")]))[1]
        
        fill <- c("#234d20",
                  rep("#36802d", n_continent),
                  rep("#77ab59", n_country),
                  rep("#c9df8a", n_group),
                  rep("#f0f7da", n_binomial_name)
        )
        
        collapsibleTree(
            plants,
            hierarchy = c("continent", "country", "group", "binomial_name"),
            zoomable = FALSE,
            root="Plant Geography",
            fill=fill
        )
    })
    
    output$extinction_by_country<- renderPlot({
        plants %>%
            count(continent, country) %>%
            ggplot(aes(y=tidytext::reorder_within(country, n, continent), x=n, fill=as.factor(n))) +
            geom_col() +
            tidytext::scale_y_reordered() +
            scale_colour_gradient2() +
            facet_wrap(~continent, ncol=2, scales="free") +
            labs(y="country", x="n of extinct plant species") +
            theme(legend.position = "none")
        
    })
    
    output$extinction_by_continent <- renderPlot({
        plants %>% count(continent) %>%
            mutate(continent = fct_reorder(continent, n)) %>%
            ggplot(aes(x=continent, y=n, fill=as.factor(n))) +
            geom_col() +
            scale_colour_gradient2()+
            coord_flip() +
            labs(x="continent", y="n of extinct plant species") +
            scale_fill_brewer(palette = "Greens") +
            theme(legend.position = "none")
    })
    
    output$groups <- renderPlot({
        plants %>% count(group) %>%
            mutate(group = fct_reorder(group, n, .desc=TRUE)) %>%
            ggplot(aes(x=group, y=n, fill=as.factor(n))) +
            geom_col() +
            scale_colour_gradient2()+
            labs(x="group", y="n of extinct plant species") +
            scale_fill_brewer(palette = "Greens") +
            theme(legend.position = "none")
    })
    
    output$threat_waffle <- renderPlot({
        th <- threats %>%
            filter(threatened == 1, country == input$threat_country) %>%
            count(threat_type) %>%
            mutate(threat_type = paste0(threat_type, " (n = ", n, ")"))
        
        df <- data.frame(t(th$n))
        colnames(df) <- th$threat_type
        
        n <- length(df)
        palette <- distinctColorPalette(n)
        
        waffle(df, rows=round(sqrt(sum(df)) / 1.5), size=0.7,
               colors = palette,
               title="Threats Waffle Chart",  
               xlab="Number of species associated with threat") 
    })
    
    output$action_waffle <- renderPlot({
        th <- actions %>%
            filter(action_taken == 1, country == input$action_country) %>%
            count(action_type) %>%
            mutate(action_type = paste0(action_type, " (n = ", n, ")"))
        
        df <- data.frame(t(th$n))
        colnames(df) <- th$action_type
        
        n <- length(df)
        palette <- distinctColorPalette(n)
        
        waffle(df, rows=round(sqrt(sum(df)) / 1.5), size=0.7,
               colors = palette,
               title="Actions Waffle Chart",  
               xlab="Number of species associated with action") 
    })
    
    output$map <- renderPlotly({
        plants$ctry_cd <- countrycode(plants$country, origin = 'country.name', destination = 'iso3c') 
        
        to_map <- plants %>%
            count(ctry_cd, country) 
        
        fig <- plot_ly(to_map, type='choropleth', locations=to_map$ctry_cd, z=to_map$n, text=to_map$country, colorscale="Inferno")
        fig <- fig %>% colorbar(title = "# extinct plant species")
        fig
        })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
