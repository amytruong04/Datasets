library(shiny)
library(readxl)

# Load the dataset
jk_pickle <- read_excel("jk_pickle.xlsx")

# Ensure relevant columns are numeric
jk_pickle$`Handle Length (in)` <- as.numeric(jk_pickle$`Handle Length (in)`)
jk_pickle$`Firepower (0-100)` <- as.numeric(jk_pickle$`Firepower (0-100)`)
jk_pickle$`Static Weight (oz)` <- as.numeric(jk_pickle$`Static Weight (oz)`)

# Convert Shape to a factor to prevent selection errors
jk_pickle$Shape <- as.factor(jk_pickle$Shape)

# Convert Price to Numeric (Removes "$" and ",")
jk_pickle$Price <- as.numeric(gsub("[$,]", "", as.character(jk_pickle$Price)))

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("The Perfect Paddle"),
  
  # Sidebar with dropdown and sliders for filtering
  sidebarLayout(
    sidebarPanel(
      # Dropdown menu for Shape selection
      selectInput("shape",
                  "Select Paddle Shape:",
                  choices = unique(jk_pickle$Shape),
                  selected = unique(jk_pickle$Shape)[1]),  # Default to first shape
      
      # Slider for Handle Length
      sliderInput("handle_length",
                  "Select Handle Length Range:",
                  min = min(jk_pickle$`Handle Length (in)`, na.rm = TRUE),
                  max = max(jk_pickle$`Handle Length (in)`, na.rm = TRUE),
                  value = range(jk_pickle$`Handle Length (in)`, na.rm = TRUE)),
      
      # Slider for Firepower
      sliderInput("firepower",
                  "Select Firepower Range:",
                  min = min(jk_pickle$`Firepower (0-100)`, na.rm = TRUE),
                  max = max(jk_pickle$`Firepower (0-100)`, na.rm = TRUE),
                  value = range(jk_pickle$`Firepower (0-100)`, na.rm = TRUE)),
      
      # Slider for Static Weight
      sliderInput("static_weight",
                  "Select Static Weight Range:",
                  min = min(jk_pickle$`Static Weight (oz)`, na.rm = TRUE),
                  max = max(jk_pickle$`Static Weight (oz)`, na.rm = TRUE),
                  value = range(jk_pickle$`Static Weight (oz)`, na.rm = TRUE)),
      
      # Slider for Price Range
      sliderInput("price_range",
                  "Select Price Range ($):",
                  min = min(jk_pickle$Price, na.rm = TRUE),
                  max = max(jk_pickle$Price, na.rm = TRUE),
                  value = range(jk_pickle$Price, na.rm = TRUE), pre = "$")
    ),
    
    # Show a table of the selected filtered data
    mainPanel(
      tableOutput("filteredTable")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$filteredTable <- renderTable({
    # Filter data based on selected Shape, Handle Length, Firepower, Static Weight, and Price
    filtered_data <- subset(jk_pickle, 
                            Shape == input$shape &  # Filter by selected Shape
                              `Handle Length (in)` >= input$handle_length[1] & 
                              `Handle Length (in)` <= input$handle_length[2] &
                              `Firepower (0-100)` >= input$firepower[1] & 
                              `Firepower (0-100)` <= input$firepower[2] &
                              `Static Weight (oz)` >= input$static_weight[1] & 
                              `Static Weight (oz)` <= input$static_weight[2] &
                              !is.na(Price) &  # Ensure Price is Not NA
                              Price >= input$price_range[1] & 
                              Price <= input$price_range[2])  # Filter by price range
    
    # Display filtered table
    filtered_data
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

