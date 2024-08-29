library(tidyverse)
library(shiny)
library(shinythemes)

mcdonalds_raw <- read_csv("data/mcdonaldata_UK.csv")

menu_data <- mcdonalds_raw |> 
  mutate(calories = as.numeric(str_remove(product_calories, "kcal: "))) |> 
  rename_all(str_to_lower)

# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
      body {background-color: #f0f8ff;} /* Light blue background */
      .well {border: 0px solid #DA291C; padding: 10px; background-color: #fff3cd;} /* Emphasized instructions box */
      .panel-heading {background-color: #FFC72C; color: #DA291C; font-weight: bold;} /* Styling for panel headings */
      .panel-body {background-color: #ffffff;} /* White background inside panels */
      .shiny-plot-output {background-color: #f0f8ff; border: 1px solid #f0f8ff;} /* Light blue background for chart with red border */
    "))
  ),
  titlePanel("McDonald's Menu Visualization"),
  fluidRow(
    column(12,
           div(class = "well", 
               h5("Explore the nutritional content of McDonald's menu items. 
                  Use the dropdowns below to select the variables for the x and y axes, 
                  and click on a point in the chart to see more details about the selected item."),
               HTML("This example developed by Adi from <a href='https://www.sarid-ins.com'>Sarid Research</a>. 
               We are not affiliated in any way with McDonald's.
               The Data's source comes from <a href='https://www.kaggle.com/datasets/danilchurko/mcdonalds-uk-menu-dataset'>Kaggle</a>.")
           )
    )
  ),
  br(),
  fluidRow(
    column(6, 
           selectInput("xvar", "X-axis:", 
                       choices = c("Calories" = "calories", 
                                   "Protein" = "protein", 
                                   "Carbs" = "carbs", 
                                   "Salt" = "salt", 
                                   "Fat" = "fat", 
                                   "Saturates" = "saturates", 
                                   "Sugars" = "sugars"),
                       selected = "calories")
    ),
    column(6, 
           selectInput("yvar", "Y-axis:", 
                       choices = c("Calories" = "calories", 
                                   "Protein" = "protein", 
                                   "Carbs" = "carbs", 
                                   "Salt" = "salt", 
                                   "Fat" = "fat", 
                                   "Saturates" = "saturates", 
                                   "Sugars" = "sugars"),
                       selected = "protein")
    )
  ),
  fluidRow(
    column(8, 
           plotOutput("menuPlot", 
                      click = clickOpts(id = "plot_click"),
                      hover = hoverOpts(id = "plot_hover"))
    ),
    column(4,
           uiOutput("click_info")
    )
  ),
  br(),
  fluidRow(
    column(12,
      uiOutput("product_iframe")
    )
  ),
)



# Server
server <- function(input, output, session) {
  
  # Reactive values to store the currently hovered and clicked points
  hovered_point <- reactiveVal(NULL)
  selected_point <- reactiveVal(NULL)
  
  # Render the plot with point highlighting on click and label on hover
  output$menuPlot <- renderPlot({
    base_plot <- ggplot(menu_data, aes(x = !!sym(input$xvar), y = !!sym(input$yvar))) +
      geom_point(color = "#DA291C", size = 4) +  # McDonald's red color for markers
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "#D3D3D3", linetype = "dashed"),
        text = element_text(color = "black"),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16)
      ) +
      labs(x = input$xvar, y = input$yvar) + 
      ggtitle("The McDonald's menu, in numbers")
    
    # Highlight the selected point
    if (!is.null(selected_point())) {
      base_plot <- base_plot +
        geom_point(data = menu_data %>% filter(product_name == selected_point()), 
                   color = "#001aff", size = 5) +
        geom_text(data = menu_data %>% filter(product_name == selected_point()), 
                  aes(label = product_name), vjust = -1, color = "#001aff")
    }
    
    # Add label for the hovered point
    if (!is.null(hovered_point())) {
      base_plot <- base_plot +
        geom_text(data = menu_data %>% filter(product_name == hovered_point()), 
                  aes(label = product_name), vjust = -1, color = "#ff6600")
    }
    
    base_plot
  })
  
  # Handle click event to highlight the clicked point
  observeEvent(input$plot_click, {
    click <- input$plot_click
    point <- nearPoints(menu_data, click, xvar = input$xvar, yvar = input$yvar, threshold = 10, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) > 0) {
      # Store the name of the clicked point
      selected_point(point$product_name)
    } else {
      # If clicked outside any point, clear the selection
      selected_point(NULL)
    }
  })

  # Handle hover event to show the label
  observeEvent(input$plot_hover, {
    hover <- input$plot_hover
    point <- nearPoints(menu_data, hover, xvar = input$xvar, yvar = input$yvar, threshold = 10, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) > 0) {
      # Store the name of the hovered point
      hovered_point(point$product_name)
    } else {
      # If not hovering over any point, clear the hover
      hovered_point(NULL)
    }
  })

  # Display details in the well panel upon clicking a point
  output$click_info <- renderUI({
    point_name <- selected_point()
    if (is.null(point_name)) return(NULL)
    
    point <- menu_data %>% filter(product_name == point_name)
    
    wellPanel(
      style = "background-color: #FFC72C; color: #DA291C;",
      h5(strong(point$product_name)),
      p(paste("Calories:", point$calories)),
      p(paste("Protein:", point$protein, "g")),
      p(paste("Carbs:", point$carbs, "g")),
      p(paste("Sugars:", point$sugars, "g")),
      p(paste("Fat:", point$fat, "g")),
      p(paste("Saturates:", point$saturates, "g")),
      p(paste("Salt:", point$salt, "g")),
      p(paste("Price:", point$product_price, "GBP"))
    )
  })
  
  # Render iframe for product details
  output$product_iframe <- renderUI({
    point_name <- selected_point()
    if (is.null(point_name)) return(NULL)
    
    point <- menu_data %>% filter(product_name == point_name)
    
    tags$iframe(
      src = point$url_for_product,
      height = "400px",
      width = "100%",
      frameborder = 0
    )
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
