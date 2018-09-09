
library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel(h3("Student performance monitoring")),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "data_type",
                   label = "Upload files or use toy data?",
                   choices = c("Upload file","Toy data"),
                   selected = "Upload file",
                   inline = T),
      conditionalPanel(condition = "input.data_type == 'Upload file'",
                       fileInput(inputId = "input_percentiles",
                                 label = "Choose percentile (csv file)",
                                 accept = ".csv",
                                 buttonLabel = "Browse",
                                 placeholder = "No file selected"),
                       fileInput(inputId = "input_student_performance",
                                 label = "Upload student's performance (csv file)",
                                 accept = ".csv",
                                 buttonLabel = "Browse",
                                 placeholder = "No file selected")),
      checkboxInput("show_percentiles",
                    "Show percentiles"),
      sliderInput("input_percentile_goal",
                   "Select percentile for final goal",
                   min = 1,
                   max = 99,
                   value = 20),
      checkboxInput("show_smooth_line",
                    "Show performance progress smooth line"),
      textInput("performance_metric",
                "Fill in the performance's measurement unit"),
      textInput("plot_title",
                "Fill in plot's title"),
      downloadButton("export_plot",
                     "Export plot")
    ),
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("plot1",
                  height = "600px")
    )
  )
))
