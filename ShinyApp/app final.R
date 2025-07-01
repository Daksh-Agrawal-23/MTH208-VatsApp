# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(bslib)
library(DT)
library(tidyr)
library(shinyWidgets)
library(imager)
library(magick)

# # Set up base directory
base_dir <- ".."
setwd(base_dir)

# Load and set up category directories
categories <- list.dirs("Data", full.names = FALSE, recursive = FALSE)
categ_ <- list.dirs("Data", full.names = FALSE, recursive = FALSE)
categ_dir <- list.dirs("Data", full.names = TRUE, recursive = FALSE)
categories <- lapply(categories, function(x) gsub("_", " ", x))
categories <- unlist(categories)
categ_ <- unlist(categ_)

# Define UI with modern theme
ui <- fluidPage(
  theme = bs_theme(bootswatch = "solar"),  # Apply modern dark theme
  
  titlePanel("Google Play Store Analysis"),
  
  tabsetPanel(
    # Home Tab
    tabPanel("Home", 
             h2("Project Description"),
             p("This project provides insights into Google Play Store data, analyzing apps based on metrics like ratings, downloads, and categories across two regions: the US and India.")
    ),
    
    # Data Surfer Tab
    tabPanel("Data Surfer", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("country", "Select Country:", choices = c("US", "India")),
                 selectInput("category", "Select Category:", choices = categories),
                 selectInput("file_type", "Select File Type:", choices = c(
                   "Top Selling Paid" = "topselling_paid",
                   "Top Selling Free" = "topselling_free",
                   "Top Grossing" = "topgrossing"
                 )),
                 selectInput("metric", "Select Metric:", choices = c("rating", "downloads", "price", "category"))
               ),
               mainPanel(
                 plotOutput("distPlot"),
                 DTOutput("summaryTable")
               )
             )
    ),
    
    # Trend Compare Tab
    tabPanel("Trend Compare", 
             sidebarLayout(
               sidebarPanel(
                 # File 1 Inputs
                 selectInput("country_1", "Select Country for File 1:", choices = c("US", "India")),
                 selectInput("category_1", "Select Category for File 1:", choices = categories),
                 selectInput("file_type_1", "Select File Type for File 1:", choices = c(
                   "Top Selling Paid" = "topselling_paid",
                   "Top Selling Free" = "topselling_free",
                   "Top Grossing" = "topgrossing"
                 )),
                 
                 # File 2 Inputs
                 selectInput("country_2", "Select Country for File 2:", choices = c("US", "India")),
                 selectInput("category_2", "Select Category for File 2:", choices = categories),
                 selectInput("file_type_2", "Select File Type for File 2:", choices = c(
                   "Top Selling Paid" = "topselling_paid",
                   "Top Selling Free" = "topselling_free",
                   "Top Grossing" = "topgrossing"
                 )),
                 
                 # Common Metric Input
                 selectInput("metric_compare", "Select Metric for Comparison:", choices = c("rating", "downloads", "price", "category"))
               ),
               mainPanel(
                 plotOutput("comparePlot"),
                 DTOutput("compareTable")
               )
             )
    ),
    
    tabPanel("Icon Analysis",
             sidebarLayout(
               sidebarPanel(
                 fileInput("upload_icon", "Upload App Icon", accept = c("image/png", "image/jpeg")),
                 selectInput("icon_country", "Select Country:", choices = c("US", "India")),
                 selectInput("icon_category", "Select Category:", choices = categories),
                 selectInput("icon_file_type", "Select File Type:", choices = c(
                   "Top Selling Paid" = "topselling_paid",
                   "Top Selling Free" = "topselling_free",
                   "Top Grossing" = "topgrossing"
                 )),
                 actionButton("analyze_icon", "Analyze Icon")
               ),
               mainPanel(
                 plotOutput("icon_histogram"),
                 h3("Uploaded Icon Preview"),
                 imageOutput("uploaded_icon_preview"),
                 h3("Expected Metrics for Uploaded Icon"),
                 tableOutput("icon_prediction"),
                 imageOutput("closest_icon_preview")
               )
             )
    ),
    
    
    tabPanel("App Search",
             sidebarPanel(
               # Filters for App Search
               selectInput("filt_country", "Select Country:", choices = c("US", "India")),
               selectInput("filt_category", "Select Category:", choices = categories),
               selectInput("filt_file_type", "Select File Type:", choices = c(
                 "Top Selling Paid" = "topselling_paid",
                 "Top Selling Free" = "topselling_free",
                 "Top Grossing" = "topgrossing"
               )),
               selectInput("search_category", "Sub-Category", choices = NULL, selected = NULL, multiple = TRUE),
               selectInput("search_author", "Author", choices = NULL, selected = NULL, multiple = TRUE),
               selectInput("search_downloads", "Downloads", choices = NULL, selected = NULL, multiple = TRUE),
               sliderInput("search_rating", "Rating", min = 0, max = 5, value = c(0, 50)/10),
               sliderInput("search_price", "Price Range", min = 0, max = 1000, value = c(0, 1000))
             ),
             mainPanel(
               DTOutput("app_search_table")  # Table output for displaying search results
             )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Helper to load CSV data
  load_data <- function(country, category, file_type) {
    category_underscore <- categ_[categories == category]
    country_code <- ifelse(country == "US", "us", "in")
    file_path <- file.path(base_dir, categ_dir[categ_ == category_underscore], 
                           paste0(country_code, "_", file_type, "_", category_underscore, ".csv"))
    if (file.exists(file_path)) {
      data <- read.csv(file_path)
    } else {
      data <- data.frame()
    }
    data
  }
  load_image_from_url_with_magick <- function(url) {
    # Load the image from the URL using magick
    img_magick <- image_read(url)
    
    # Convert the image to a raw format that imager can read
    temp_file <- tempfile(fileext = ".png")
    image_write(img_magick, path = temp_file, format = "png")
    
    # Load the image with imager from the temporary PNG file
    image <- load.image(temp_file)
    
    # Clean up the temporary file
    unlink(temp_file)
    
    return(image)
  }
  
  
  observe({
    metric_choices <- if (input$file_type == "topselling_paid") {
      c("rating", "downloads", "price", "category")
    } else {
      c("rating", "downloads", "category")
    }
    updateSelectInput(session, "metric", choices = metric_choices)
  })
  
  observe({
    metric_compare_choices <- if (input$file_type_1 == "topselling_paid" && input$file_type_2 == "topselling_paid") {
      c("rating", "downloads", "price", "category")
    } else {
      c("rating", "downloads", "category")
    }
    updateSelectInput(session, "metric_compare", choices = metric_compare_choices)
  })
  
  search_data <- reactive({
    category_underscore <- categ_[categories == input$filt_category]
    country_code <- ifelse(input$filt_country == "US", "us", "in")
    file_path <- file.path(base_dir, categ_dir[categ_ == category_underscore], 
                           paste0(country_code, "_", input$filt_file_type, "_", category_underscore, ".csv"))
    data <- read.csv(file_path)
    
    # Convert 'downloads' to categorical factor
    if ("downloads" %in% names(data)) {
      data$downloads <- factor(data$downloads, levels = unique(data$downloads[order(nchar(data$downloads), data$downloads)]))
    }
    
    # Convert 'category' to factor
    if ("category" %in% names(data)) {
      data$category <- factor(data$category)
    }
    data
  })
  
  observe({
    data=search_data()
    updateSelectInput(session, "search_category", choices = unique(data$category))
    updateSelectInput(session, "search_author", choices = unique(data$author))
    updateSelectInput(session, "search_downloads", choices = unique(data$downloads))
    updateSliderInput(session, "search_price", min = 0, max = max(data$extracted_price), value = c(0, max(data$extracted_price)))
  })
  # 
  # Reactive filtering for the App Search tab
  filtered_app_data <- reactive({
    data <- search_data()
    
    # Apply filters conditionally based on the presence of extracted_price
    filtered_data <- data %>%
      filter(
        (is.null(input$search_category) | category %in% input$search_category),
        (is.null(input$search_author) | author %in% input$search_author),
        (is.null(input$search_downloads) | downloads %in% input$search_downloads),
        rating >= input$search_rating[1],
        rating <= input$search_rating[2]
      )
    
    # Additional filter if `extracted_price` exists in `data`
    if ("extracted_price" %in% colnames(data)) {
      filtered_data <- filtered_data %>%
        filter(
          extracted_price >= input$search_price[1],
          extracted_price <= input$search_price[2]
        )
    }
    
    filtered_data
  })

  # Render the filtered app search results in a table
  output$app_search_table <- renderDT({
    datatable(filtered_app_data())
  })
  
  
  # Reactive expression to load the selected file
  selected_data <- reactive({
    category_underscore <- categ_[categories == input$category]
    country_code <- ifelse(input$country == "US", "us", "in")
    file_path <- file.path(base_dir, categ_dir[categ_ == category_underscore], 
                           paste0(country_code, "_", input$file_type, "_", category_underscore, ".csv"))
    data <- read.csv(file_path)
    
    # Convert 'downloads' to categorical factor
    if ("downloads" %in% names(data)) {
      data$downloads <- factor(data$downloads, levels = unique(data$downloads[order(nchar(data$downloads), data$downloads)]))
    }
    
    # Convert 'category' to factor
    if ("category" %in% names(data)) {
      data$category <- factor(data$category)
    }
    data
  })
  
  # Plot distribution based on selected metric
  output$distPlot <- renderPlot({
    data <- selected_data()
    
    # Customize x-axis label based on selected metric
    x_label <- switch(input$metric,
                      "rating" = "Rating",
                      "downloads" = "Downloads",
                      "price" = "Price",
                      "category" = "Category")
    
    # General plot aesthetics for theme consistency with solar background
    plot_theme <- theme(
      plot.title = element_text(face = "bold", size = 16, color = "#E3F2FD"),
      axis.title = element_text(face = "bold", size = 14, color = "#E3F2FD"),
      axis.text = element_text(size = 12, color = "#B0BEC5"),
      axis.line = element_line(color = "#455A64"),
      panel.background = element_rect(fill = "#002b36"),  # Solar dark background
      plot.background = element_rect(fill = "#042f39", color = "transparent"),  # Same as panel background
      panel.grid.major = element_line(color = "#37474F", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(10, 10, 10, 10)
    )
    
    # Plot based on selected metric
    if (input$metric == "rating") {
      ggplot(data, aes(x = rating)) +
        geom_histogram(binwidth = 0.2, fill = "#FF7043", color = "#455A64") +
        plot_theme +
        labs(title = paste("Distribution of", input$metric, "for", input$category, "in", input$country),
             x = x_label)
      
    } else if (input$metric == "downloads") {
      ggplot(data, aes(x = downloads)) +
        geom_bar(fill = "#81C784", color = "#455A64") +
        plot_theme +
        labs(title = paste("Distribution of", input$metric, "for", input$category, "in", input$country),
             x = x_label)
      
    } else if (input$metric == "price") {
      ggplot(data, aes(x = extracted_price)) +
        geom_histogram(fill = "#FFEB3B", color = "#455A64") +
        plot_theme +
        labs(title = paste("Distribution of", input$metric, "for", input$category, "in", input$country),
             x = x_label)
      
    } else if (input$metric == "category") {
      ggplot(data, aes(x = category)) +
        geom_bar(fill = "#64B5F6", color = "#455A64") +
        plot_theme +
        labs(title = paste("Distribution of", input$metric, "for", input$category, "in", input$country),
             x = x_label)
    }
  })
  
  # Render summary table with category and download details
  output$summaryTable <- DT::renderDataTable({
    data <- selected_data()
    
    # Handle different metrics and render the summary stats
    if (input$metric == "rating" || input$metric == "price") {
      num_var = ifelse(input$metric == "rating", "rating", "extracted_price")
      summary_stats <- data %>%
        summarise(
          Average = mean(get(num_var), na.rm = TRUE),
          Median = median(get(num_var), na.rm = TRUE),
          Max = max(get(num_var), na.rm = TRUE),
          Min = min(get(num_var), na.rm = TRUE)
        )
      
      # Format columns for rating and price metrics
      summary_stats %>%
        DT::datatable(options = list(
          pageLength = 5,
          autoWidth = TRUE,
          searching = TRUE,
          ordering = TRUE,
          lengthMenu = c(5, 10, 15, 20)
        )) %>%
        formatStyle(
          columns = c("Average", "Median", "Max", "Min"), 
          backgroundColor = styleEqual(c("Average", "Median", "Max", "Min"), c("#455A64", "#37474F", "#263238", "#212121")),
          color = styleEqual(c("Average", "Median", "Max", "Min"), c("#FFEB3B", "#81C784", "#FF7043", "#64B5F6"))
        )
      
    } else if (input$metric == "downloads") {
      summary_stats <- data %>%
        group_by(downloads) %>%
        summarise(Count = n()) %>%
        arrange(
          desc(nchar(as.character(downloads))),
          desc(downloads)
        )
      
      summary_stats <- summary_stats %>%
        mutate(Example_Apps = sapply(downloads, function(download) {
          apps <- data %>%
            filter(downloads == download) %>%
            pull(title) %>%
            unique()
          
          app_list <- if (length(apps) > 3) {
            paste(head(apps, 3), collapse = ", ")
          } else {
            paste(apps, collapse = ", ")
          }
          
          if (length(apps) > 3) {
            paste(app_list, "...")
          } else {
            app_list
          }
        }))
      
      # Format columns for downloads summary
      summary_stats %>%
        DT::datatable(options = list(
          pageLength = 5,
          autoWidth = TRUE,
          searching = TRUE,
          ordering = TRUE,
          lengthMenu = c(5, 10, 20, 50)
        )) %>%
        formatStyle(
          columns = c("downloads", "Count", "Example_Apps"),
          backgroundColor = styleEqual(c("downloads", "Count", "Example_Apps"), c("#455A64", "#37474F", "#263238")),
          color = styleEqual(c("downloads", "Count", "Example_Apps"), c("#FFEB3B", "#81C784", "#FF7043"))
        )
      
    } else if (input$metric == "category") {
      # Handle "category" metric: frequency table for categories
      category_summary <- data %>%
        group_by(category) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count))  # Sort by frequency
      
      # Render the category frequency table
      category_summary %>%
        DT::datatable(options = list(
          pageLength = 5,
          autoWidth = TRUE,
          searching = TRUE,
          ordering = TRUE,
          lengthMenu = c(5, 10, 20, 50)
        )) %>%
        formatStyle(
          columns = c("category", "Count"),
          backgroundColor = styleEqual(c("category", "Count"), c("#455A64", "#37474F")),
          color = styleEqual(c("category", "Count"), c("#FFEB3B", "#81C784"))
        )
    }
  })
  
  # Reactive expressions to load data for Trend Compare Tab
  selected_data_1 <- reactive({
    category_underscore_1 <- categ_[categories == input$category_1]
    country_code_1 <- ifelse(input$country_1 == "US", "us", "in")
    file_path_1 <- file.path(base_dir, categ_dir[categ_ == category_underscore_1], 
                             paste0(country_code_1, "_", input$file_type_1, "_", category_underscore_1, ".csv"))
    data_1 <- read.csv(file_path_1)
    
    if ("downloads" %in% names(data_1)) {
      data_1$downloads <- factor(data_1$downloads, levels = unique(data_1$downloads[order(nchar(data_1$downloads), data_1$downloads)]))
    }
    
    if ("category" %in% names(data_1)) {
      data_1$category <- factor(data_1$category)
    }
    data_1
  })
  
  selected_data_2 <- reactive({
    category_underscore_2 <- categ_[categories == input$category_2]
    country_code_2 <- ifelse(input$country_2 == "US", "us", "in")
    file_path_2 <- file.path(base_dir, categ_dir[categ_ == category_underscore_2], 
                             paste0(country_code_2, "_", input$file_type_2, "_", category_underscore_2, ".csv"))
    data_2 <- read.csv(file_path_2)
    
    if ("downloads" %in% names(data_2)) {
      data_2$downloads <- factor(data_2$downloads, levels = unique(data_2$downloads[order(nchar(data_2$downloads), data_2$downloads)]))
    }
    
    if ("category" %in% names(data_2)) {
      data_2$category <- factor(data_2$category)
    }
    data_2
  })
  
  # Plot comparison between the two datasets (Trend Compare Tab)
  output$comparePlot <- renderPlot({
    data_1 <- selected_data_1()
    data_2 <- selected_data_2()
    
    if (input$file_type_1 == "topselling_paid" && input$file_type_2 == "topselling_paid") {
      # Apply conversion if one of the datasets is from India
      if (input$country_1 != input$country_2) {
        
        # If data_1 is from India, adjust its price
        if (input$country_1 == "India") {
          data_1$extracted_price <- data_1$extracted_price / 84
        }
        
        # If data_2 is from India, adjust its price
        if (input$country_2 == "India") {
          data_2$extracted_price <- data_2$extracted_price / 84
        }
      }
    }
    
    # Standardize columns between data_1 and data_2
    common_cols <- intersect(names(data_1), names(data_2))
    data_1 <- data_1[, common_cols, drop = FALSE]
    data_2 <- data_2[, common_cols, drop = FALSE]
    
    # Add a 'group' column to distinguish between datasets
    data_1$group <- "File 1"
    data_2$group <- "File 2"
    
    combined_data <- rbind(data_1, data_2)
    
    x_label <- switch(input$metric_compare,
                      "rating" = "Rating",
                      "downloads" = "Downloads",
                      "price" = "Price",
                      "category" = "Category")
    
    plot_theme <- theme(
      plot.title = element_text(face = "bold", size = 16, color = "#E3F2FD"),
      axis.title = element_text(face = "bold", size = 14, color = "#E3F2FD"),
      axis.text = element_text(size = 12, color = "#B0BEC5"),
      axis.line = element_line(color = "#455A64"),
      panel.background = element_rect(fill = "#002b36"),
      plot.background = element_rect(fill = "#042f39", color = "transparent"),
      panel.grid.major = element_line(color = "#37474F", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(10, 10, 10, 10),
      
      # Legend adjustments
      legend.position = "top",  # Position at the top
      legend.title = element_text(face = "bold", size = 12, color = "#E3F2FD"),
      legend.text = element_text(size = 10, color = "#B0BEC5"),
      legend.background = element_rect(fill = "#042f39", color = "transparent"),
      legend.key = element_rect(fill = "#002b36", color = "transparent")
    )
    
    # Plot based on selected metric
    if (input$metric_compare == "rating") {
      ggplot(combined_data, aes_string(x = input$metric_compare, fill = "group")) +
        geom_histogram(binwidth = 0.2, position = position_dodge(width = 0.1), color = "#455A64") +
        plot_theme +
        labs(title = paste("Comparison of", input$metric_compare, "for Selected Files"),
             x = x_label)
      
    } else if (input$metric_compare == "downloads" || input$metric_compare == "category") {
      ggplot(combined_data, aes_string(x = input$metric_compare, fill = "group")) +
        geom_bar(position = position_dodge(), color = "#455A64", width = 0.25) +
        plot_theme +
        labs(title = paste("Comparison of", input$metric_compare, "for Selected Files"),
             x = x_label)
      
    } else if (input$metric_compare == "price") {
      ggplot(combined_data, aes_string(x = "extracted_price", fill = "group")) +
        geom_histogram(position = position_dodge(width = 0.2), color = "#455A64") +
        plot_theme +
        labs(title = paste("Comparison of", input$metric_compare, "for Selected Files"),
             x = x_label)
    }
  })
  
  
  
  # Render comparison table for Trend Compare
  output$compareTable <- DT::renderDataTable({
    data_1 <- selected_data_1()
    data_2 <- selected_data_2()
    
    if (input$metric_compare == "rating" || input$metric_compare == "price") {
      # Numerical summary for rating or price
      num_var <- ifelse(input$metric_compare == "rating", "rating", "extracted_price")
      
      # Summarize data for File 1
      summary_1 <- data_1 %>%
        summarise(
          Average = mean(get(num_var), na.rm = TRUE),
          Median = median(get(num_var), na.rm = TRUE),
          Max = max(get(num_var), na.rm = TRUE),
          Min = min(get(num_var), na.rm = TRUE)
        ) %>%
        mutate(File = "File 1")
      
      # Summarize data for File 2
      summary_2 <- data_2 %>%
        summarise(
          Average = mean(get(num_var), na.rm = TRUE),
          Median = median(get(num_var), na.rm = TRUE),
          Max = max(get(num_var), na.rm = TRUE),
          Min = min(get(num_var), na.rm = TRUE)
        ) %>%
        mutate(File = "File 2")
      
      # Combine summaries for display
      combined_summary <- rbind(summary_1, summary_2)
      
      # Render table
      combined_summary %>%
        select(File, Average, Median, Max, Min) %>%
        DT::datatable(options = list(
          pageLength = 2,
          autoWidth = TRUE,
          searching = FALSE,
          ordering = TRUE,
          lengthMenu = c(2, 5, 10)
        ))
      
    } else if (input$metric_compare == "downloads" || input$metric_compare == "category") {
      # Categorical summary for downloads or category
      cat_var <- ifelse(input$metric_compare == "downloads", "downloads", "category")
      
      # Summarize data for File 1
      summary_1 <- data_1 %>%
        group_by_at(cat_var) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count)) %>%
        mutate(File = "File 1")
      
      # Summarize data for File 2
      summary_2 <- data_2 %>%
        group_by_at(cat_var) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count)) %>%
        mutate(File = "File 2")
      
      # Combine summaries for display
      combined_summary <- summary_1 %>%
        full_join(summary_2, by = cat_var, suffix = c("_File1", "_File2")) %>%
        rename(Class = !!cat_var) %>%
        mutate(across(starts_with("Count"), ~ replace_na(.x, 0)))  # Ensure NAs are 0 for Counts
      
      # Render table
      if (cat_var=="downloads"){
        combined_summary=combined_summary %>%
        arrange(
          desc(nchar(as.character(Class))),
          desc(Class)
        )
      }
      combined_summary %>%
        select(Class, Count_File1, Count_File2) %>%
        DT::datatable(options = list(
          pageLength = 5,
          autoWidth = TRUE,
          searching = TRUE,
          ordering = TRUE,
          lengthMenu = c(5, 10, 20)
        ))
    }
    })
    
  # Reactive function to load the uploaded image and calculate its histogram
  icon_data <- reactive({
    req(input$upload_icon)
    icon <- load.image(input$upload_icon$datapath)
    
    # Calculate color histogram for uploaded icon
    icon_hist <- color_histogram(icon)
    
    list(icon = icon, icon_hist = icon_hist)
  }) 
  output$uploaded_icon_preview <- renderImage({
    icon_data <- icon_data()
    list(src = input$upload_icon$datapath, contentType = 'image/png', width = 100, height = 100)
  }, deleteFile = FALSE)
  
  # Helper function to calculate color histograms
  color_histogram <- function(image) {
    # Normalize pixel values to 0–1 range
    image <- as.cimg(image) / max(image)
    
    as.data.frame(image) %>%
      gather(channel, value, -x, -y) %>%
      mutate(value = pmin(1, pmax(0, value))) %>%  # Ensure values are within 0 and 1
      group_by(channel) %>%
      summarize(histogram = list(hist(value, plot = FALSE, breaks = seq(0, 1, 0.01))$counts)) %>%
      ungroup()
  }
  
  # Plot histogram of the uploaded image
  output$icon_histogram <- renderPlot({
    icon_hist <- icon_data()$icon_hist
    
    # Combine histograms into a single data frame
    hist_df <- bind_rows(
      lapply(1:nrow(icon_hist), function(i) {
        data.frame(Channel = icon_hist$channel[i], Bin = 1:length(icon_hist$histogram[[i]]), 
                   Frequency = icon_hist$histogram[[i]])
      })
    )
    
    plot_theme <- theme(
      plot.title = element_text(face = "bold", size = 16, color = "#E3F2FD"),
      axis.title = element_text(face = "bold", size = 14, color = "#E3F2FD"),
      axis.text = element_text(size = 12, color = "#B0BEC5"),
      axis.line = element_line(color = "#455A64"),
      panel.background = element_rect(fill = "#002b36"),
      plot.background = element_rect(fill = "#042f39", color = "transparent"),
      panel.grid.major = element_line(color = "#37474F", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(10, 10, 10, 10),
      
      # Legend adjustments
      legend.position = "top",  # Position at the top
      legend.title = element_text(face = "bold", size = 12, color = "#E3F2FD"),
      legend.text = element_text(size = 10, color = "#B0BEC5"),
      legend.background = element_rect(fill = "#042f39", color = "transparent"),
      legend.key = element_rect(fill = "#002b36", color = "transparent")
    )
    
    # Plot using ggplot2
    ggplot(hist_df, aes(x = Bin, y = log(Frequency), fill = Channel)) +
      geom_bar(stat = "identity", position = "dodge") +
      plot_theme+
      labs(title = "Color Histogram of Uploaded Icon", x = "Intensity Bin", y = "log of Frequency") +
      scale_fill_manual(values = c("#FFEB3B", "#64B5F6"))
  })
  
  # Event for icon analysis
  observeEvent(input$analyze_icon, {
    # Load data for the selected category
    icon_analysis_data <- load_data(input$icon_country, input$icon_category, input$icon_file_type)
    
    # Calculate histograms for each icon in the selected category
    histograms <- lapply(icon_analysis_data$thumbnail, function(icon_url) {
      # Load the icon image from URL using the magick-based helper function
      icon <- tryCatch(
        load_image_from_url_with_magick(icon_url),
        error = function(e) NULL  # Handle error if image loading fails
      )
      
      if (!is.null(icon)) {
        color_histogram(icon)
      } else {
        NULL  # Skip if loading failed
      }
    })
    
    # Remove NULL entries from histograms if any URLs failed to load
    histograms <- histograms[!sapply(histograms, is.null)]
    
    # Find the closest histogram
    uploaded_hist <- icon_data()$icon_hist
    distances <- sapply(histograms, function(hist) {
      sum((unlist(uploaded_hist$histogram) - unlist(hist$histogram))^2)
    })
    print(distances)
    closest_index <- which.min(distances)
    closest_icon <- icon_analysis_data$thumbnail[closest_index]
    
    # Extract information of the closest icon
    closest_icon_data <- icon_analysis_data[closest_index, ]
    print(closest_icon_data)
    # Prediction based on closest icon's actual values
    if (input$icon_file_type=="topselling_paid"){ 
    prediction <- data.frame(
      Metric = c("Expected Rating", "Expected Downloads", "Expected Price"),
      Value = c(
        closest_icon_data$rating,
        closest_icon_data$downloads,
        closest_icon_data$extracted_price
      )
    )
    }
    else{
      prediction <- data.frame(
        Metric = c("Expected Rating", "Expected Downloads", "Expected Price"),
        Value = c(
          closest_icon_data$rating,
          closest_icon_data$downloads,
          "Not Applicable for App Type (unpaid)"
        )
      )
    }
    
    output$icon_prediction <- renderTable({
      prediction
    })
    print(closest_icon)
    output$closest_icon_preview <- renderImage({
      list(src = closest_icon, contentType = 'image/png', width = 100, height = 100)
    }, deleteFile = FALSE)
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
