
library(shiny)

shinyUI(fluidPage(
  # Title
  titlePanel(h3("Student performance monitoring")),
  
  sidebarLayout(
    sidebarPanel(
      tags$style(".well {background-color:#bfdcf2;}"),
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
      numericInput("input_goal",
                   "Set performance goal",
                   value = NA),
      checkboxInput("show_smooth_line",
                    "Show performance progress smooth line"),
      textInput("plot_title",
                "Fill in plot's title"),
      textInput("performance_metric",
                "Fill in the y-axis label (Performance's measurement unit)"),
      textInput("period_unit",
                "Fill in the x-axis label (Period's unit)"),
      downloadButton("export_plot",
                     "Export plot"),
    width = 3),
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("plot1",
                  height = "600px")
    )
  )
))
