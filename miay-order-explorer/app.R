library(shiny)
library(shinythemes)
library(ggplot2)

# to do: test row counts and groupings against wpAdmin

setwd("C:/Users/sjara/git/made-in-america-yarns/data")
source("C:/Users/sjara/git/made-in-america-yarns/functions.R")



data_denorm <- read.csv("denormalized.csv", stringsAsFactors = FALSE) %>%
    mutate("order_year" = year(order_date),
           "order_month" = month(order_date),
           "order_week" = week(order_date)) %>%
    # replace NA values in usage and effect with "unassigned" so that filtering will work
    mutate(across(c(meta.yarn_usage, effect), ~case_when(is.na(.x)==TRUE ~"unassigned",
                                                         TRUE ~ as.character(.x)))) %>%
    # handle strange missing data
    filter(is.na(quantity)==FALSE) 
    # mutate(effect = rep_len(c("test", "testing", "123"), 3248))

# Define UI 
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
                h4("Variations:"),
                checkboxInput(inputId = "include_variations", label= "include variations?", value=FALSE),
                h4("Popularity Ranking:"),
                radioButtons(
                    inputId = "interval",
                    label = "calculate top products by ____:",
                    choices = c("week", "month", "year"),
                    selected = "week",
                    width = NULL,
                    choiceNames = NULL,
                    choiceValues = NULL
                )
                
            ),
    
            # space for plot
            mainPanel(
               verbatimTextOutput("filter_stats"),
               plotOutput("top_prod_plot") ,
               verbatimTextOutput("test2")
        )
    )
))

# Define server logic
server <- function(input, output) {
    
    # INITIALIZE
    ## data_denorm only filtered by date (for calculating % of orders that fulfill the filters)
    df_date <- reactiveValues(data = data_denorm)
    ## data_denorm filtered by date and selected filters
    df_filter <- reactiveValues(data = data_denorm)
    ## format for printing dates
    human_date <- stamp("March 19, 1995")
    
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
                                    
                 })
    
    observeEvent(input$reset,
                 {df_filter$data <- data_denorm
                 df_date$data <- data_denorm
                 updateDateRangeInput(inputId = "date_range",
                                      start = paste(year(Sys.Date()), month(Sys.Date()), "1", sep="-"),
                                      end = Sys.Date())
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
            # percentage of orders in the date range that fulfill the filters
              as.character(as.integer(nrow(distinct(df_filter$data))/nrow(distinct(df_date$data))*100)),
              "% of those orders fulfill the filters and are plotted below.",
              "\n", "\n",
            # coupons used in the date range
              "The following coupons were used during that time:",
              "\n", 
              unique(df_filter$data["coupon"]), 
              sep="")
    })
    
    
    
    # PLOT
    output$top_prod_plot <- renderPlot({
        
        # CALCULATE
        
        ## PRODUCT / WEEK
        if(input$include_variations == FALSE &
           input$interval == "week"){
            df <- df_filter$data %>% 
                # get order-level data and all necessary fields
                distinct(order_id, product_id, order_year, order_week, name, quantity) %>%
                # get item-level info
                group_by(order_year, order_week, product_id) %>%
             # n_orders = orders placed by year/mo
             summarize(n_orders = n(),
                       # sum_quantity = number purchased per period
                       sum_quantity = sum(quantity),
                       # need this so "name" won't be dropped
                       name = max(name),
                       # change grouping
                       .groups = "drop") %>%      
            group_by(order_year, order_week) %>%
            # rank n_orders and sum_quantity (1=most orders/largest quantity)
            mutate("rank_orders" = dense_rank(desc(n_orders)),
                   "rank_quantity" = dense_rank(desc(sum_quantity))) %>%
            # get top three items
            filter(rank_orders %in% c(1,2,3)) %>%
            # for repeat rank_orders, choose the record(s) with the largest quantity ordered
            group_by(order_year, order_week, rank_orders, .add=TRUE) %>%
            filter(rank_quantity == min(rank_quantity)) %>%
            # count how many times each name appears in the top
            ungroup() %>%
            count(name)
            
            # order by number of times in top three
            df_plot <- df %>% 
                arrange(n) %>%
                mutate(name=factor(name, levels=name, ordered = TRUE))
            df_plot %>%
                ggplot(aes(x=name, y=n)) +
                geom_col() +
                coord_flip()
        ## PRODUCT / MONTH
        }else if(input$include_variations == FALSE &
              input$interval == "month"){
            df <- df_filter$data %>% 
                # get order-level data and all necessary fields
                distinct(order_id, product_id, order_year, order_month, name, quantity) %>%
                # get item-level info
                group_by(order_year, order_month, product_id) %>%
                # n_orders = orders placed by year/mo
                summarize(n_orders = n(),
                          # sum_quantity = number purchased per period
                          sum_quantity = sum(quantity),
                          # need this so "name" won't be dropped
                          name = max(name),
                          # change grouping
                          .groups = "drop") %>%      
                group_by(order_year, order_month) %>%
                # rank n_orders and sum_quantity (1=most orders/largest quantity)
                mutate("rank_orders" = dense_rank(desc(n_orders)),
                       "rank_quantity" = dense_rank(desc(sum_quantity))) %>%
                # get top three items
                filter(rank_orders %in% c(1,2,3)) %>%
                # for repeat rank_orders, choose the record(s) with the largest quantity ordered
                group_by(order_year, order_month, rank_orders, .add=TRUE) %>%
                filter(rank_quantity == min(rank_quantity)) %>%
                # count how many times each name appears in the top
                ungroup() %>%
                count(name)
            
            # order by number of times in top three
            df_plot <- df %>% 
                arrange(n) %>%
                mutate(name=factor(name, levels=name, ordered = TRUE))
            
            df_plot %>%
                ggplot(aes(x=name, y=n)) +
                geom_col() +
                coord_flip()
            ## PRODUCT / YEAR
        } else if(input$include_variations == FALSE &
                  input$interval == "year"){
            df <- df_filter$data %>% 
                # get order-level data and all necessary fields
                distinct(order_id, product_id, order_year, name, quantity) %>%
                # get item-level info
                group_by(order_year, product_id) %>%
                # n_orders = orders placed by year/mo
                summarize(n_orders = n(),
                          # sum_quantity = number purchased per period
                          sum_quantity = sum(quantity),
                          # need this so "name" won't be dropped
                          name = max(name),
                          # change grouping
                          .groups = "drop") %>%      
                group_by(order_year) %>%
                # rank n_orders and sum_quantity (1=most orders/largest quantity)
                mutate("rank_orders" = dense_rank(desc(n_orders)),
                       "rank_quantity" = dense_rank(desc(sum_quantity))) %>%
                # get top three items
                filter(rank_orders %in% c(1,2,3)) %>%
                # for repeat rank_orders, choose the record(s) with the largest quantity ordered
                group_by(order_year, rank_orders, .add=TRUE) %>%
                filter(rank_quantity == min(rank_quantity)) %>%
                # count how many times each name appears in the top
                ungroup() %>%
                count(name)
            
            # order by number of times in top three
            df_plot <- df %>% 
                arrange(n) %>%
                mutate(name=factor(name, levels=name, ordered = TRUE))
            df_plot %>%
                ggplot(aes(x=name, y=n)) +
                geom_col() +
                coord_flip()
        ## VARIATION / WEEK
        }else if(input$include_variations == TRUE &
              input$interval == "week"){
            df <- df_filter$data %>% 
                # get order-level data and all necessary fields
                mutate("label" = paste(name, color, sep= "-")) %>%
                distinct(order_id, product_id,variation_id, order_year, order_week, label, quantity) %>%
                # get item-level info
                group_by(order_year, order_week, product_id, variation_id) %>%
                # n_orders = orders placed by year/mo
                summarize(n_orders = n(),
                          # sum_quantity = number purchased per period
                          sum_quantity = sum(quantity),
                          # need this so "name" won't be dropped
                          label = max(label),
                          # change grouping
                          .groups = "drop") %>%      
                group_by(order_year, order_week) %>%
                # rank n_orders and sum_quantity (1=most orders/largest quantity)
                mutate("rank_orders" = dense_rank(desc(n_orders)),
                       "rank_quantity" = dense_rank(desc(sum_quantity))) %>%
                # get top three items
                filter(rank_orders %in% c(1,2,3)) %>%
                # for repeat rank_orders, choose the record(s) with the largest quantity ordered
                group_by(order_year, order_week, rank_orders, .add=TRUE) %>%
                filter(rank_quantity == min(rank_quantity)) %>%
                # count how many times each name appears in the top
                ungroup() %>%
                count(label)
            
            # order by number of times in top three
            df_plot <- df %>% 
                arrange(n) %>%
                mutate(label=factor(label, levels=label, ordered = TRUE))
            df_plot %>%
                ggplot(aes(x=label, y=n)) +
                geom_col() +
                coord_flip()
        ## VARIATION / MONTH
        }else if(input$include_variations == TRUE &
                 input$interval == "month"){
            df <- df_filter$data %>% 
                # get order-level data and all necessary fields
                mutate("label" = paste(name, color, sep= "-")) %>%
                distinct(order_id, product_id,variation_id, order_year, order_month, label, quantity) %>%
                # get item-level info
                group_by(order_year, order_month, product_id, variation_id) %>%
                # n_orders = orders placed by year/mo
                summarize(n_orders = n(),
                          # sum_quantity = number purchased per period
                          sum_quantity = sum(quantity),
                          # need this so "name" won't be dropped
                          label = max(label),
                          # change grouping
                          .groups = "drop") %>%      
                group_by(order_year, order_month) %>%
                # rank n_orders and sum_quantity (1=most orders/largest quantity)
                mutate("rank_orders" = dense_rank(desc(n_orders)),
                       "rank_quantity" = dense_rank(desc(sum_quantity))) %>%
                # get top three items
                filter(rank_orders %in% c(1,2,3)) %>%
                # for repeat rank_orders, choose the record(s) with the largest quantity ordered
                group_by(order_year, order_month, rank_orders, .add=TRUE) %>%
                filter(rank_quantity == min(rank_quantity)) %>%
                # count how many times each name appears in the top
                ungroup() %>%
                count(label)
            
            # order by number of times in top three
            df_plot <- df %>% 
                arrange(n) %>%
                mutate(label=factor(label, levels=label, ordered = TRUE))
            df_plot %>%
                ggplot(aes(x=label, y=n)) +
                geom_col() +
                coord_flip()
            
        # VARIATION / YEAR
        }else if(input$include_variations == TRUE &
                 input$interval == "year"){
            df <- df_filter$data %>% 
                # get order-level data and all necessary fields
                mutate("label" = paste(name, color, sep= "-")) %>%
                distinct(order_id, product_id,variation_id, order_year, label, quantity) %>%
                # get item-level info
                group_by(order_year, product_id, variation_id) %>%
                # n_orders = orders placed by year/mo
                summarize(n_orders = n(),
                          # sum_quantity = number purchased per period
                          sum_quantity = sum(quantity),
                          # need this so "name" won't be dropped
                          label = max(label),
                          # change grouping
                          .groups = "drop") %>%      
                group_by(order_year) %>%
                # rank n_orders and sum_quantity (1=most orders/largest quantity)
                mutate("rank_orders" = dense_rank(desc(n_orders)),
                       "rank_quantity" = dense_rank(desc(sum_quantity))) %>%
                # get top three items
                filter(rank_orders %in% c(1,2,3)) %>%
                # for repeat rank_orders, choose the record(s) with the largest quantity ordered
                group_by(order_year, rank_orders, .add=TRUE) %>%
                filter(rank_quantity == min(rank_quantity)) %>%
                # count how many times each name appears in the top
                ungroup() %>%
                count(label)
            
            # order by number of times in top three
            df_plot <- df %>% 
                arrange(n) %>%
                mutate(label=factor(label, levels=label, ordered = TRUE))
            df_plot %>%
                ggplot(aes(x=label, y=n)) +
                geom_col() +
                coord_flip()
        }
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
