
library(shiny)
library(ggplot2)
library(ggthemes)
library(data.table)

shinyServer(function(input, output) {
  
  # Read percentile data
  percentile_data <- reactive({
    if (input$data_type == "Upload file") {
      inFile <- input$input_percentiles
      if (is.null(inFile)) { return(NULL) }    
      dataFile <- read.csv(inFile$datapath)
    } else if (input$data_type == "Toy data") {
      dataFile <- read.csv("data/percentiles.csv") 
    }
    return(dataFile)
  }) 
  
  # Read student performance data
  student_data <- reactive({
    if (input$data_type == "Upload file") {
      inFile <- input$input_student_performance
      if (is.null(inFile)) { return(NULL) }    
      dataFile <- read.csv(inFile$datapath)
    } else if (input$data_type == "Toy data") {
      dataFile <- read.csv("data/student_performance.csv") 
    }
    return(dataFile)
  })
  
  # Create the plot
  output_plot <- reactive({
    # percentiles <- read.csv("data/percentiles.csv") 
    # student_perf <- read.csv("data/student_performance.csv")
    percentiles <- percentile_data()
    student_perf <- student_data()
    
    g <- ggplot()
    
    # Check if data ara available
    if (!is.null(percentiles) & !is.null(student_perf)) {
      # Extract the first performance
      start_perf <- student_perf$Performance1[1]
      
      # Find the minimum of each percentile
      percentiles$minPerformance <- dplyr::lead(percentiles$Performance)
      
      # Replace NAs with 0s
      percentiles$minPerformance[is.na(percentiles$minPerformance)] <- 0
      
      # for (i in 1:(nrow(percentiles)-1)) {
      #   percentiles$minPerformance[i] <- percentiles$Performance[i+1]
      # }
      # 
      # percentiles$minPerformance <- ifelse(percentiles$Percentile == 1,
      #                                      percentiles$Performance,
      #                                      percentiles$minPerformance)
      
      #percentiles <- subset(percentiles, percentiles$Percentile > 1)
      
      # Create all combinations of periods and percentiles
      period_percentiles <- expand.grid(Period = min(student_perf$Period):max(student_perf$Period), Percentile = percentiles$Percentile)
      
      # Add performance limits
      period_percentiles <- merge(period_percentiles,
                                  percentiles,
                                  by = "Percentile",
                                  all.x = T)
      
      period_percentiles$PeriodPerformance <- start_perf + period_percentiles$Period*(period_percentiles$Performance-start_perf)/max(student_perf$Period)
      
      #percentiles2 <- subset(percentiles2, percentiles2$Performance >= start_perf)
      
      # count numer of percentile values
      num_of_percentiles <- length(percentiles$Percentile)
      
      # Create a palette based on the number of percentiles
      percentile_colors <- colorRampPalette(c('red','green'))(num_of_percentiles)
      c <- 1
      
      if (input$show_percentiles) {
        # Loop over percentile values
        for (p in sort(percentiles$Percentile)) {
          # Create the percentile area
          g <- g + geom_ribbon(data = subset(period_percentiles, period_percentiles$Percentile == p),
                               aes(x = Period,
                                   ymax = Performance,
                                   ymin = minPerformance),
                               alpha = .3,
                               fill = percentile_colors[c],
                               color = "grey")
          # Add the percentile label
          g <- g + annotate("label",
                            x = max(student_perf$Period) + 1,
                            y = percentiles$Performance[percentiles$Percentile == p],
                            label = paste0(p, "%"))
          # Go to the next color
          c <- c + 1
        }
      }

      # if (input$show_percentiles) {
      #   if (!is.null(subset(percentiles2, percentiles2$Percentile == 99))) {
      #     g <- g + geom_ribbon(data = subset(percentiles2, percentiles2$Percentile == 99),
      #                          aes(x = Week,
      #                              ymax = Performance,
      #                              ymin = minPerformance),
      #                          alpha = .5,
      #                          fill = "darkolivegreen1",
      #                          color = "grey")
      #     g <- g + geom_label(aes(label = "99%",
      #                             x = 8.2,
      #                             y = max(subset(percentiles2, percentiles2$Percentile == 99)$Performance)))
      #   }
      #   if (!is.null(subset(percentiles2, percentiles2$Percentile == 95))) {
      #     g <- g + geom_ribbon(data = subset(percentiles2, percentiles2$Percentile == 95),
      #                          aes(x = Week,
      #                              ymax = Performance,
      #                              ymin = minPerformance),
      #                          alpha = .5,
      #                          fill = "darkolivegreen2",
      #                          color = "grey")
      #     g <- g + geom_label(aes(label = "95%",
      #                             x = 8.2,
      #                             y = max(subset(percentiles2, percentiles2$Percentile == 95)$Performance)))
      #     
      #   }
      #   if (!is.null(subset(percentiles2, percentiles2$Percentile == 90))) {
      #     g <- g + geom_ribbon(data = subset(percentiles2, percentiles2$Percentile == 90),
      #                          aes(x = Week,
      #                              ymax = Performance,
      #                              ymin = minPerformance),
      #                          alpha = .5,
      #                          fill = "darkolivegreen3",
      #                          color = "grey")
      #     g <- g + geom_label(aes(label = "90%",
      #                             x = 8.2,
      #                             y = max(subset(percentiles2, percentiles2$Percentile == 90)$Performance)))
      #     
      #   }
      #   if (!is.null(subset(percentiles2, percentiles2$Percentile == 80))) {
      #     g <- g + geom_ribbon(data = subset(percentiles2, percentiles2$Percentile == 80),
      #                          aes(x = Week,
      #                              ymax = Performance,
      #                              ymin = minPerformance),
      #                          alpha = .5,
      #                          fill = "gold",
      #                          color = "grey")
      #     g <- g + geom_label(aes(label = "80%",
      #                             x = 8.2,
      #                             y = max(subset(percentiles2, percentiles2$Percentile == 80)$Performance)))
      #   }
      #   if (!is.null(subset(percentiles2, percentiles2$Percentile == 70))) {
      #     g <- g + geom_ribbon(data = subset(percentiles2, percentiles2$Percentile == 70),
      #                          aes(x = Week,
      #                              ymax = Performance,
      #                              ymin = minPerformance),
      #                          alpha = .5,
      #                          fill = "gold1",
      #                          color = "grey")
      #     g <- g + geom_label(aes(label = "70%",
      #                             x = 8.2,
      #                             y = max(subset(percentiles2, percentiles2$Percentile == 70)$Performance)))
      #   }
      #   if (!is.null(subset(percentiles2, percentiles2$Percentile == 60))) {
      #     g <- g + geom_ribbon(data = subset(percentiles2, percentiles2$Percentile == 60),
      #                          aes(x = Week,
      #                              ymax = Performance,
      #                              ymin = minPerformance),
      #                          alpha = .5,
      #                          fill = "gold2",
      #                          color = "grey")
      #     g <- g + geom_label(aes(label = "60%",
      #                             x = 8.2,
      #                             y = max(subset(percentiles2, percentiles2$Percentile == 60)$Performance)))
      #   }
      #   if (!is.null(subset(percentiles2, percentiles2$Percentile == 50))) {
      #     g <- g + geom_ribbon(data = subset(percentiles2, percentiles2$Percentile == 50),
      #                          aes(x = Week,
      #                              ymax = Performance,
      #                              ymin = minPerformance),
      #                          alpha = .5,
      #                          fill = "darkorange",
      #                          color = "grey")
      #     g <- g + geom_label(aes(label = "50%",
      #                             x = 8.2,
      #                             y = max(subset(percentiles2, percentiles2$Percentile == 50)$Performance)))
      #   }
      #   if (!is.null(subset(percentiles2, percentiles2$Percentile == 40))) {
      #     g <- g + geom_ribbon(data = subset(percentiles2, percentiles2$Percentile == 40),
      #                          aes(x = Week,
      #                              ymax = Performance,
      #                              ymin = minPerformance),
      #                          alpha = .5,
      #                          fill = "darkorange1",
      #                          color = "grey")
      #     g <- g + geom_label(aes(label = "40%",
      #                             x = 8.2,
      #                             y = max(subset(percentiles2, percentiles2$Percentile == 40)$Performance)))
      #   }
      #   if (!is.null(subset(percentiles2, percentiles2$Percentile == 30))) {
      #     g <- g + geom_ribbon(data = subset(percentiles2, percentiles2$Percentile == 30),
      #                          aes(x = Week,
      #                              ymax = Performance,
      #                              ymin = minPerformance),
      #                          alpha = .5,
      #                          fill = "darkorange2",
      #                          color = "grey")
      #     g <- g + geom_label(aes(label = "30%",
      #                             x = 8.2,
      #                             y = max(subset(percentiles2, percentiles2$Percentile == 30)$Performance)))
      #   }
      #   if (!is.null(subset(percentiles2, percentiles2$Percentile == 20))) {
      #     g <- g + geom_ribbon(data = subset(percentiles2, percentiles2$Percentile == 20),
      #                          aes(x = Week,
      #                              ymax = Performance,
      #                              ymin = minPerformance),
      #                          alpha = .5,
      #                          fill = "orangered",
      #                          color = "grey")
      #     g <- g + geom_label(aes(label = "20%",
      #                             x = 8.2,
      #                             y = max(subset(percentiles2, percentiles2$Percentile == 20)$Performance)))
      #   }
      #   if (!is.null(subset(percentiles2, percentiles2$Percentile == 10))) {
      #     g <- g + geom_ribbon(data = subset(percentiles2, percentiles2$Percentile == 10),
      #                          aes(x = Week,
      #                              ymax = Performance,
      #                              ymin = minPerformance),
      #                          alpha = .5,
      #                          fill = "orangered1",
      #                          color = "grey")
      #     g <- g + geom_label(aes(label = "10%",
      #                             x = 8.2,
      #                             y = max(subset(percentiles2, percentiles2$Percentile == 10)$Performance)))
      #   }
      #   if (!is.null(subset(percentiles2, percentiles2$Percentile == 5))) {
      #     g <- g + geom_ribbon(data = subset(percentiles2, percentiles2$Percentile == 5),
      #                          aes(x = Week,
      #                              ymax = Performance,
      #                              ymin = minPerformance),
      #                          alpha = .5,
      #                          fill = "orangered2",
      #                          color = "grey")
      #     g <- g + geom_label(aes(label = "5%",
      #                             x = 8.2,
      #                             y = max(subset(percentiles2, percentiles2$Percentile == 5)$Performance)))
      #   }
      #   if (!is.null(subset(percentiles2, percentiles2$Percentile == 1))) {
      #     g <- g + geom_ribbon(data = subset(percentiles2, percentiles2$Percentile == 1),
      #                          aes(x = Week,
      #                              ymax = Performance,
      #                              ymin = minPerformance),
      #                          alpha = .5,
      #                          fill = "orangered3",
      #                          color = "grey")
      #     g <- g + geom_label(aes(label = "1%",
      #                             x = 8.2,
      #                             y = max(subset(percentiles2, percentiles2$Percentile == 1)$Performance)))
      #   }
      # 
      # }
      
      # Transform student performance data from wide to long
      if ("Method" %in% names(student_perf)) {
        student_perf_long <- melt(student_perf, 
                                  id.vars = c("Period","Method"),
                                  measure.vars = names(student_perf)[grepl("Performance",names(student_perf))])
      } else {
        student_perf_long <- melt(student_perf, 
                                  id.vars = "Period")
      }
      
      # Delete "Performance" subststring
      student_perf_long$variable <- gsub("Performance","",student_perf_long$variable)
      
      # Draw student's performance
      if ("Method" %in% names(student_perf)) {
        g <- g + geom_point(data = subset(student_perf_long, !is.na(student_perf_long$Method)),
                            aes(x = Period,
                                y = value,
                                shape = Method,
                                fill = variable),
                            color = "black",
                            size = 4,
                            position = position_jitter(w = 0.1, h = 0))
        
        g <- g + scale_shape_manual(values = 21:25, name = "Method")
      } else {
        if (length(names(student_perf)[grepl("Performance", names(student_perf))]) == 1) {
          g <- g + geom_point(data = student_perf_long,
                              aes(x = Period,
                                  y = value),
                              color = "black",
                              fill = "red",
                              shape = 21,
                              size = 3)
        } else {
          g <- g + geom_point(data = student_perf_long,
                              aes(x = Period,
                                  y = value,
                                  fill = variable),
                              color = "black",
                              shape = 21,
                              size = 3,
                              position = position_jitter(w = 0.1, h = 0))
        }
      }

      # Draw a horizontal line on the initial performance
      g <- g + geom_hline(data = student_perf,
                          yintercept = start_perf,
                          linetype = "dashed",
                          color = "red")
      
      if (!input$show_percentiles) {
        g <- g + geom_label(data = student_perf,
                            x = max(student_perf$Period),
                            y = start_perf,
                            label = start_perf)
      }
      
      # Get the goal
      goal <- as.numeric(input$input_goal)
      # if (goal != 1 & goal != 99) {
      #   goal <- 5*round(goal/5)
      # }
      
      # Draw goal line
      if (goal >= start_perf & !is.na(goal)) {
        g <- g + geom_segment(aes(x = min(student_perf$Period),
                                  xend = max(student_perf$Period),
                                  y = start_perf,
                                  yend = goal))
      }

      
      # Draw goal line
      # if (percentiles$Performance[percentiles$Percentile == goal] > start_perf) {
      #   
      #   goal_data <- subset(percentiles2, percentiles2$Percentile == goal)
      #   
      #   g <- g + geom_line(data = goal_data,
      #                      aes(x = Week,
      #                          y = WeekPerformance,
      #                          group = factor(Percentile)))
      #   
      #   if (!input$show_percentiles) {
      #     g <- g + geom_hline(data = goal_data,
      #                         yintercept = max(goal_data$WeekPerformance),
      #                         linetype = "dashed",
      #                         color = "blue")
      #     
      #     g <- g + geom_label(data = goal_data,
      #                       x = 8,
      #                       y = max(goal_data$WeekPerformance),
      #                       label = max(goal_data$WeekPerformance))
      #   }
      # }
      
      # Draw a smooth line
      if (input$show_smooth_line) {
        mean_performances <- setDT(student_perf_long)[, list(mean_performance = mean(value, na.rm = T)),
                                                      by = Period]
        
        g <- g + geom_smooth(data = mean_performances,
                             aes(x = Period,
                                 y = mean_performance),
                             method='lm',
                             formula= y~x)
      }
      # + scale_x_continuous(breaks = 0:8)
      g <- g + scale_fill_discrete(name="Time") + scale_x_continuous(breaks = min(student_perf$Period):max(student_perf$Period))
      
      #if (input$show_percentiles) {
      g <- g + scale_y_continuous(breaks = pretty(percentiles$Performance, 10))
      #} #else {
        #g <- g + scale_y_continuous(breaks = pretty(student_perf_long$value, 10))
      #}
      g <- g + xlab(input$period_unit) + ylab(input$performance_metric)
      g <- g + theme_bw()
      
      if (nchar(input$plot_title) > 0) {
        g <- g + ggtitle(input$plot_title)
      }
      
      if ("Method" %in% names(student_perf)) {
        g <- g + guides(fill = guide_legend(override.aes = list(shape = 21)))
      }
    }

    g
  })
  
  output$plot1 <- renderPlot({
    print(output_plot())
  })
  
  output$export_plot <- downloadHandler(
    filename = function() { paste('performance_plot.png', sep='') },
    content = function(file) {
      ggsave(file, plot = output_plot(), device = "png")
    }
  )
  
})
