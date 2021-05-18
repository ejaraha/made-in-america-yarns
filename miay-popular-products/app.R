library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(lubridate)

# get data
data_denorm <- read.csv("https://raw.githubusercontent.com/sjaraha/made-in-america-yarns/master/miay-popular-products/denormalized.csv", stringsAsFactors = FALSE)

# define theme for plots
theme_style <- theme(plot.title = element_text(face = "bold", size = 15),
               plot.subtitle = element_text(face = "plain", size = 13),
               axis.title = element_text(size = 13, face="italic"),
               axis.text.y = element_blank(),
               axis.text.x = element_text(size = 13),
               legend.text = element_text(size = 13),
               legend.title = element_text(face = "bold", size = 13),
               legend.position = "right",
               panel.background = element_rect(fill = "slategray3"),
               panel.grid.major = element_line(colour = NA),
               panel.grid.minor = element_line(colour = NA),
               axis.ticks = element_line(color=NA),
               axis.title.y = element_blank(),
               axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

# define labels for plots
plot_labels <- labs(title = "Frequency of Each Product in the Top-Product-Ranking",
                    y = "number of times ranked as a top product")

# define UI 
ui <- fluidPage(
    navbarPage(
        theme = shinytheme("spacelab"),
        title = 'MiAY Popular Products',
        # define layout
        sidebarLayout(
            # space for variables
            sidebarPanel(
                h4("Filters:"), 
                actionLink(inputId = "reset",
                           label = "Reset."),
                actionLink(inputId = "apply",
                           label = "Apply."),
                br(),
                br(),
                dateRangeInput(
                    inputId = "date_range",
                    label = "date range:",
                    start = paste(year(Sys.Date()), "1", "1", sep="-"),
                    end = max(data_denorm$order_date),
                    max = max(data_denorm$order_date),
                    min = min(data_denorm$order_date)
                ),
                selectInput(
                    inputId = "customer_type",
                    label = "customer type:",
                    choices = unique(data_denorm$customer_type),
                    selected = unique(data_denorm$customer_type),
                    multiple = TRUE,
                    selectize = FALSE,
                    width = NULL,
                    size = 3
                ),
                selectInput(
                    inputId = "customer_usage",
                    label = "customer usage:",
                    choices= unique(data_denorm$meta.yarn_usage),
                    selected = unique(data_denorm$meta.yarn_usage),
                    multiple = TRUE,
                    selectize = FALSE,
                    width = NULL,
                    size = 3
                ),
                selectInput(
                    inputId = "yarn_fiber",
                    label = "yarn fiber:",
                    choices= unique(data_denorm$fiber),
                    selected= unique(data_denorm$fiber),
                    multiple = TRUE,
                    selectize = FALSE,
                    width = NULL,
                    size = 3
                ),
                selectInput(
                    inputId = "yarn_weight",
                    label = "yarn weight:",
                    choices= unique(data_denorm$yarn_weight),
                    selected = unique(data_denorm$yarn_weight),
                    multiple = TRUE,
                    selectize = FALSE,
                    width = NULL,
                    size = 3
                ),
                selectInput(
                    inputId = "yarn_effect",
                    label = "yarn effect:",
                    choices= unique(data_denorm$effect),
                    selected = unique(data_denorm$effect),
                    multiple = TRUE,
                    selectize = FALSE,
                    width = NULL,
                    size = 3
                ),
                h4("Product Granularity:"),
                checkboxInput(inputId = "include_variations", label= "include variations?", value=FALSE),
                h4("Scope:"),
                radioButtons(
                    inputId = "interval",
                    label = "top products by ___:",
                    choices = c("week", "month", "year"),
                    selected = "month",
                    width = NULL,
                    choiceNames = NULL,
                    choiceValues = NULL
                )
            ),
    
            # space for plot
            mainPanel(
               verbatimTextOutput("filter_stats"),
               plotOutput("top_prod_plot") ,
               verbatimTextOutput("test2"),
               br(),
               p(h4("About This Tool")),
               br(),
               p("This tool can be used to ", strong("identify popular products")," for specific subgroups and date ranges."),
               br(),
               p("The ",strong("bar chart"), " shows the number of times each product appears in the ", tags$u("top three"), " for the chosen ", strong("scope"), " (week, month, or year).
                 The top three products (for the chosen filters) are first ranked by the number of orders placed for each product, then ranked by the quantity sold of each product."),
               br(),
               p("The ", strong("filters"), " section allows you to analyze a specific subset of all available data. 
                 If you choose 'metallic' from the \"yarn fiber\" filter and set the \"date range\" filter to '2021-01-01 to 2021-01-31', then the bar chart will only show results for metallic products in January 2021.",
                 em("Notes on filters: 'not applicable' indicates non-yarn items (textile bobbins) or group items (mystery box). 'unassigned' marks products that have not yet been assigned an attribute.")),
               br(),
               p("The ", strong("product granularity"), " section allows you to rank products at the product level (ex. american lamb yarn) or the variation level (ex. american lamb yarn - wineglass)."),
               br(),
               p("The ", strong("scope"), " section allows you to choose the scope of the ranking. If ", strong("scope"), " is 'week' and \"date range\" is '2021-01-01 to 2021-01-31', then the ", tags$u("top three"), " products will be calculated for each week in January 2021.
                 Then, the ",strong("bar chart"), " will display the number of times top products appear in the top ranking."),
               br(),br(),br()

        )
    )
))

# define server logic
server <- function(input, output) {
    
    # INITIALIZE
    ## data_denorm only filtered by date (for calculating % of orders that fulfill the filters)
    df_date <- reactiveValues(data = data_denorm)
    ## data_denorm filtered by date and selected filters
    df_filter <- reactiveValues(data = data_denorm)
    ## format for printing dates
    human_date <- stamp("March 19, 1995")
    
    # load function
    # integer breaks for y axis
    #-------------------------------------------------------------
    # A function factory for getting integer y-axis values.
    # https://www.r-bloggers.com/2019/11/setting-axes-to-integer-values-in-ggplot2/
    integer_breaks <- function(n = 5, ...) {
        fxn <- function(x) {
            breaks <- floor(pretty(x, n, ...))
            names(breaks) <- attr(breaks, "labels")
            breaks
        }
        return(fxn)
    }
    
    
    # FILTER
    observeEvent(input$apply, 
                 {df_date$data <- data_denorm %>%
                     # filter sidebar selections
                     filter(order_date >= input$date_range[1],
                            order_date <= input$date_range[2])
                     
                     df_filter$data <- data_denorm %>%
                     # filter sidebar selections
                     filter(order_date >= input$date_range[1],
                            order_date <= input$date_range[2],
                            customer_type %in% input$customer_type,
                            meta.yarn_usage %in% input$customer_usage,
                            fiber %in% input$yarn_fiber,
                            yarn_weight %in% input$yarn_weight,
                            effect %in% input$yarn_effect)
                     from <- input$date_range[1]
                                    
                 })
    
    observeEvent(input$reset,
                 {df_filter$data <- data_denorm
                 df_date$data <- data_denorm
                 updateDateRangeInput(inputId = "date_range",
                                      start = paste(year(Sys.Date()), "1", "1", sep="-"),
                                      end = max(data_denorm$order_date),
                                      max = max(data_denorm$order_date),
                                      min = min(data_denorm$order_date))
                 updateSelectInput(inputId = "customer_type",
                                   choices = unique(data_denorm$customer_type),
                                   selected = unique(data_denorm$customer_type))
                 updateSelectInput(inputId = "customer_usage",
                                   choices = unique(data_denorm$meta.yarn_usage),
                                   selected = unique(data_denorm$meta.yarn_usage))
                 updateSelectInput(inputId = "yarn_fiber", 
                                   choices = unique(data_denorm$fiber),
                                   selected = unique(data_denorm$fiber))
                 updateSelectInput(inputId = "yarn_weight",
                                    choices = unique(data_denorm$yarn_weight),
                                    selected = unique(data_denorm$yarn_weight))
                 updateSelectInput(inputId = "yarn_effect",
                                   choices = unique(data_denorm$effect),
                                   selected = unique(data_denorm$effect))
                 updateCheckboxInput(inputId = "include_variations",
                                     label = "include variations?",
                                     value = FALSE)
                 })
    
    output$filter_stats <- renderText({
        
        paste(
            # number of orders placed in the date range
            as.character(nrow(distinct(df_date$data["order_id"]))),
              " orders were placed between ", 
            # start date
              human_date(ymd(input$date_range[1])),
              " and ", 
            # end date
              human_date(ymd(input$date_range[2])),
              ".","\n", 
            as.character(as.integer(nrow(distinct(df_filter$data["order_id"])))),
            " of those orders (",
            # percentage of orders in the date range that fulfill the filters
              as.character(as.integer(nrow(distinct(df_filter$data["order_id"]))/nrow(distinct(df_date$data["order_id"]))*100)),
              "%) fulfill the filters (in the side panel), and are plotted below.",
              "\n", "\n",
            # coupons used in the date range
              "Also, the following coupons were used during that time:",
              "\n", 
              unique(df_filter$data["coupon"] %>% filter(is.na(coupon)==FALSE)), 
              sep="")
    })
    
    
    
    # PLOT
    output$top_prod_plot <- renderPlot({
        
        ## PRODUCT / WEEK
        if(input$include_variations == FALSE &
           input$interval == "week"){
            df <- df_filter$data %>% 
                # get order-level data and all necessary fields
                distinct(order_id, product_id, order_year, order_week, name, color, quantity) %>%
                # get item-level info
                group_by(order_year, order_week, product_id) %>%
             # n_orders = orders placed by year/mo
             summarize(n_orders = length(unique(order_id)),
                       # sum_quantity = number purchased per period
                       sum_quantity = sum(quantity),
                       # need this so "name" won't be dropped
                       name = max(name),
                       # change grouping
                       .groups = "drop") %>%  
            # rank
            group_by(order_year, order_week) %>%
            arrange(desc(n_orders, sum_quantity)) %>%
            mutate("rank" = 1:n()) %>% 
            filter(rank %in% c(1,2,3)) %>% 
            ungroup() %>%
            count(name) 
            
            # order by number of times in top three
            df_plot <- df %>% 
                arrange(n) %>%
                mutate(name=factor(name, levels=name, ordered = TRUE))

            # plot
            df_plot %>% ggplot(aes(x=name, y=n, label = name)) +
                geom_col(fill="lightskyblue4") +
                geom_text(hjust= 0, nudge_y = -df_plot$n+.05, colour="white") +
                coord_flip() + 
                theme_style +
                plot_labels +
                scale_y_continuous(expand = c(0,0), breaks=integer_breaks())
            
            
            
        ## PRODUCT / MONTH
        }else if(input$include_variations == FALSE &
              input$interval == "month"){
            df <- df_filter$data %>% 
                # get order-level data and all necessary fields
                distinct(order_id, product_id, order_year, order_month, name, color, quantity) %>%
                # get item-level info
                group_by(order_year, order_month, product_id) %>%
                # n_orders = orders placed by year/mo
                summarize(n_orders = length(unique(order_id)),
                          # sum_quantity = number purchased per period
                          sum_quantity = sum(quantity),
                          # need this so "name" won't be dropped
                          name = max(name),
                          # change grouping
                          .groups = "drop") %>%
                # rank
                group_by(order_year, order_month) %>%
                arrange(desc(n_orders, sum_quantity)) %>%
                mutate("rank" = 1:n()) %>% 
                filter(rank %in% c(1,2,3)) %>% 
                ungroup() %>%
                count(name)
            
            # order by number of times in top three
            df_plot <- df %>% 
                arrange(n) %>%
                mutate(name=factor(name, levels=name, ordered = TRUE))

            df_plot %>%
                ggplot(aes(x=name, y=n, label = name)) +
                geom_col(fill="lightskyblue4") +
                geom_text(hjust= 0, nudge_y = -df_plot$n+.05, colour="white") +
                coord_flip() + 
                theme_style +
                plot_labels +
                scale_y_continuous(expand = c(0,0), breaks=integer_breaks())
            
            # PRODUCT / YEAR
        } else if(input$include_variations == FALSE &
                  input$interval == "year"){
            df <- df_filter$data %>% 
                # get order-level data and all necessary fields
                distinct(order_id, product_id, order_year, name, color, quantity) %>%
                # get item-level info
                group_by(order_year, product_id) %>%
                # n_orders = orders placed by year/mo
                summarize(n_orders = length(unique(order_id)),
                          # sum_quantity = number purchased per period
                          sum_quantity = sum(quantity),
                          # need this so "name" won't be dropped
                          name = max(name),
                          # change grouping
                          .groups = "drop") %>%      
                # rank
                group_by(order_year) %>%
                arrange(desc(n_orders, sum_quantity)) %>%
                mutate("rank" = 1:n()) %>% 
                filter(rank %in% c(1,2,3)) %>% 
                ungroup() %>%
                count(name)
            
            # order by number of times in top three
            df_plot <- df %>% 
                arrange(n) %>%
                mutate(name=factor(name, levels=name, ordered = TRUE))
            df_plot %>%
                ggplot(aes(x=name, y=n, label = name)) +
                geom_col(fill="lightskyblue4") +
                geom_text(hjust= 0, nudge_y = -df_plot$n+.05, colour="white") +
                coord_flip() + 
                theme_style +
                plot_labels +
                scale_y_continuous(expand = c(0,0), breaks=integer_breaks())
        ## VARIATION / WEEK
        }else if(input$include_variations == TRUE &
              input$interval == "week"){
            df <- df_filter$data %>% 
                # get order-level data and all necessary fields
                mutate("label" = paste(name, color, sep= " - ")) %>%
                distinct(order_id, product_id,variation_id, order_year, order_week, label, quantity) %>%
                # get item-level info
                group_by(order_year, order_week, product_id, variation_id) %>%
                # n_orders = orders placed by year/mo
                summarize(n_orders = length(unique(order_id)),
                          # sum_quantity = number purchased per period
                          sum_quantity = sum(quantity),
                          # need this so "name" won't be dropped
                          label = max(label),
                          # change grouping
                          .groups = "drop") %>%     
                # rank
                group_by(order_year, order_week) %>%
                arrange(desc(n_orders, sum_quantity)) %>%
                mutate("rank" = 1:n()) %>% 
                filter(rank %in% c(1,2,3)) %>% 
                ungroup() %>%
                count(label) 
            
            # order by number of times in top three
            df_plot <- df %>% 
                arrange(n) %>%
                mutate(label=factor(label, levels=label, ordered = TRUE))
            df_plot %>%
                ggplot(aes(x=label, y=n, label = label)) +
                geom_col(fill="lightskyblue4") +
                geom_text(hjust= 0, nudge_y = -df_plot$n+.05, colour="white") +
                coord_flip() + 
                theme_style +
                plot_labels +
                scale_y_continuous(expand = c(0,0), breaks=integer_breaks())
        ## VARIATION / MONTH
        }else if(input$include_variations == TRUE &
                 input$interval == "month"){
            df <- df_filter$data %>% 
                # get order-level data and all necessary fields
                mutate("label" = paste(name, color, sep= " - ")) %>%
                distinct(order_id, product_id,variation_id, order_year, order_month, label, quantity) %>%
                # get item-level info
                group_by(order_year, order_month, product_id, variation_id) %>%
                # n_orders = orders placed by year/mo
                summarize(n_orders = length(unique(order_id)),
                          # sum_quantity = number purchased per period
                          sum_quantity = sum(quantity),
                          # need this so "name" won't be dropped
                          label = max(label),
                          # change grouping
                          .groups = "drop") %>%  
                # rank
                group_by(order_year, order_month) %>%
                arrange(desc(n_orders, sum_quantity)) %>%
                mutate("rank" = 1:n()) %>% 
                filter(rank %in% c(1,2,3)) %>% 
                ungroup() %>%
                count(label)
            
            # order by number of times in top three
            df_plot <- df %>% 
                arrange(n) %>%
                mutate(label=factor(label, levels=label, ordered = TRUE))
            df_plot %>%
                ggplot(aes(x=label, y=n, label = label)) +
                geom_col(fill="lightskyblue4") +
                geom_text(hjust= 0, nudge_y = -df_plot$n+.05, colour="white") +
                coord_flip() + 
                theme_style +
                plot_labels +
                scale_y_continuous(expand = c(0,0), breaks=integer_breaks())
            
        # VARIATION / YEAR
        }else if(input$include_variations == TRUE &
                 input$interval == "year"){
            df <- df_filter$data %>% 
                # get order-level data and all necessary fields
                mutate("label" = paste(name, color, sep= " - ")) %>%
                distinct(order_id, product_id,variation_id, order_year, label, quantity) %>%
                # get item-level info
                group_by(order_year, product_id, variation_id) %>%
                # n_orders = orders placed by year/mo
                summarize(n_orders = length(unique(order_id)),
                          # sum_quantity = number purchased per period
                          sum_quantity = sum(quantity),
                          # need this so "name" won't be dropped
                          label = max(label),
                          # change grouping
                          .groups = "drop") %>%  
                # rank
                group_by(order_year) %>%
                arrange(desc(n_orders, sum_quantity)) %>%
                mutate("rank" = 1:n()) %>% 
                filter(rank %in% c(1,2,3)) %>% 
                ungroup() %>%
                count(label) 
            
            # order by number of times in top three
            df_plot <- df %>% 
                arrange(n) %>%
                mutate(label=factor(label, levels=label, ordered = TRUE))
            df_plot %>%
                ggplot(aes(x=label, y=n, label = label)) +
                geom_col(fill="lightskyblue4") +
                geom_text(hjust= 0, nudge_y = -df_plot$n+.05, colour="white") +
                coord_flip() + 
                theme_style +
                plot_labels +
                scale_y_continuous(expand = c(0,0), breaks=integer_breaks())

        }
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
