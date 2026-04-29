library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)

# Data preparation
data <- read.csv("Amazon-final.csv")
data$Price <- as.numeric(gsub("[$,]", "", data$Price))
data$Rating_count <- as.numeric(gsub(",", "", data$Rating_count))
data$Total_Purchases <- data$Purchase_August + data$Purchase_September + data$Purchase_October

# UI
ui <- fluidPage(
  titlePanel("Inventory Management Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Filters"),
      selectInput("brand", "Select Brand", choices = c("All", unique(data$Brand))),
      sliderInput("price_range", "Price Range:", 
                  min = min(data$Price, na.rm = TRUE), 
                  max = max(data$Price, na.rm = TRUE), 
                  value = c(min(data$Price, na.rm = TRUE), max(data$Price, na.rm = TRUE)))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Overview", 
                 plotOutput("ratingDist"),
                 plotOutput("monthlyPurchases")),
        tabPanel("Correlation", 
                 plotOutput("correlationHeatmap")),
        tabPanel("Brand Analysis", 
                 plotOutput("brandCounts"),
                 textOutput("topBrand")),
        tabPanel("Inventory Analysis",
                 verbatimTextOutput("inventoryStats"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Filtered Data
  filtered_data <- reactive({
    data %>%
      filter((Brand == input$brand | input$brand == "All") &
               Price >= input$price_range[1] & Price <= input$price_range[2])
  })
  
  # Rating Distribution
  output$ratingDist <- renderPlot({
    ggplot(filtered_data(), aes(x = Stars)) +
      geom_histogram(binwidth = 0.1, fill = "dodgerblue", color = "black") +
      labs(title = "Distribution of Product Ratings (Stars)", x = "Stars", y = "Count") +
      theme_minimal()
  })
  
  # Monthly Purchases
  output$monthlyPurchases <- renderPlot({
    monthly_purchases <- colSums(filtered_data()[, c("Purchase_August", "Purchase_September", "Purchase_October")], na.rm = TRUE)
    monthly_df <- data.frame(Month = names(monthly_purchases), Purchases = monthly_purchases)
    
    ggplot(monthly_df, aes(x = Month, y = Purchases, fill = Month)) +
      geom_bar(stat = "identity") +
      labs(title = "Monthly Purchases", x = "Months", y = "Total Purchases") +
      theme_minimal()
  })
  
  # Correlation Heatmap
  output$correlationHeatmap <- renderPlot({
    numeric_data <- filtered_data()[, c("Stars", "Rating_count", "Price", "Purchase_August", "Purchase_September", "Purchase_October", "In_stock")]
    correlation_matrix <- cor(numeric_data, use = "complete.obs")
    heatmap <- melt(correlation_matrix)
    
    ggplot(heatmap, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
      labs(title = "Correlation Heatmap with Values", x = "Variables", y = "Variables") +
      theme_minimal()
  })
  
  # Brand Analysis
  output$brandCounts <- renderPlot({
    brand_counts <- filtered_data() %>%
      group_by(Brand) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count))
    
    ggplot(brand_counts, aes(x = reorder(Brand, -Count), y = Count, fill = Brand)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Count of Products by Brand", x = "Brand", y = "Count") +
      theme_minimal()
  })
  
  # Top Brand
  output$topBrand <- renderText({
    brand_total_purchases <- filtered_data() %>%
      group_by(Brand) %>%
      summarise(Total_Purchases = sum(Total_Purchases, na.rm = TRUE)) %>%
      arrange(desc(Total_Purchases))
    
    if (nrow(brand_total_purchases) > 0) {
      top_brand <- brand_total_purchases[1, ]
      paste("Top Brand: ", top_brand$Brand, " | Total Purchases: ", top_brand$Total_Purchases)
    } else {
      "No data available for the selected filters."
    }
  })
  
  # Inventory Stats
  output$inventoryStats <- renderText({
    avg_demand <- mean(filtered_data()$Total_Purchases, na.rm = TRUE)
    std_dev_demand <- sd(filtered_data()$Total_Purchases, na.rm = TRUE)
    lead_time <- 2  # Lead time in months
    z_factor <- 1.65  # Z-factor for 95% service level
    safety_stock <- z_factor * std_dev_demand * sqrt(lead_time)
    reorder_point <- avg_demand * lead_time + safety_stock
    
    paste("Safety Stock:", round(safety_stock, 2), "\nReorder Point:", round(reorder_point, 2))
  })
}

# Run the app
shinyApp(ui = ui, server = server)

