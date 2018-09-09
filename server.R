
library(shiny)
library(ggplot2)
library(ggthemes)
library(data.table)

shinyServer(function(input, output) {
  
  percentile_data <- reactive({
    inFile <- input$input_percentiles
    if (is.null(inFile)) { return(NULL) }    
    dataFile <- read.csv(inFile$datapath)
    #percentiles <- read.csv("/Users/NikosKousiadis/Desktop/Fluency_boys_3rd.csv") 
    return(dataFile)
  }) 
  
  student_data <- reactive({
    inFile <- input$input_student_performance
    if (is.null(inFile)) { return(NULL) }    
    dataFile <- read.csv(inFile$datapath)
    #student_perf <- read.csv("/Users/NikosKousiadis/Desktop/student_performance2.csv") 
    return(dataFile)
  })
  
  output_plot <- reactive({
    percentiles <- percentile_data()
    student_perf <- student_data()
    
    g <- ggplot()
    
    # Draw percentiles
    if (!is.null(percentiles) & !is.null(student_perf)) {
      start_perf <- student_perf$Performance1[1]
      
      for (i in 1:(nrow(percentiles)-1)) {
        percentiles$minPerformance[i] <- percentiles$Performance[i+1]
      }
      
      percentiles$minPerformance <- ifelse(percentiles$Percentile == 1,
                                           percentiles$Performance,
                                           percentiles$minPerformance)
      
      #percentiles <- subset(percentiles, percentiles$Percentile > 1)
      
      percentiles2 <- expand.grid(Week = 0:8, Percentile = percentiles$Percentile)
      percentiles2 <- merge(percentiles2,
                            percentiles,
                            by = "Percentile",
                            all.x = T)
      percentiles2$WeekPerformance <- start_perf + percentiles2$Week*(percentiles2$Performance-start_perf)/8
      
      #percentiles2 <- subset(percentiles2, percentiles2$Performance >= start_perf)
      
      if (input$show_percentiles) {
        if (!is.null(subset(percentiles2, percentiles2$Percentile == 99))) {
          g <- g + geom_ribbon(data = subset(percentiles2, percentiles2$Percentile == 99),
                               aes(x = Week,
                                   ymax = Performance,
                                   ymin = minPerformance),
                               alpha = .5,
                               fill = "darkolivegreen1",
                               color = "grey")
          g <- g + geom_label(aes(label = "99%",
                                  x = 8.2,
                                  y = max(subset(percentiles2, percentiles2$Percentile == 99)$Performance)))
        }
        if (!is.null(subset(percentiles2, percentiles2$Percentile == 95))) {
          g <- g + geom_ribbon(data = subset(percentiles2, percentiles2$Percentile == 95),
                               aes(x = Week,
                                   ymax = Performance,
                                   ymin = minPerformance),
                               alpha = .5,
                               fill = "darkolivegreen2",
                               color = "grey")
          g <- g + geom_label(aes(label = "95%",
                                  x = 8.2,
                                  y = max(subset(percentiles2, percentiles2$Percentile == 95)$Performance)))
          
        }
        if (!is.null(subset(percentiles2, percentiles2$Percentile == 90))) {
          g <- g + geom_ribbon(data = subset(percentiles2, percentiles2$Percentile == 90),
                               aes(x = Week,
                                   ymax = Performance,
                                   ymin = minPerformance),
                               alpha = .5,
                               fill = "darkolivegreen3",
                               color = "grey")
          g <- g + geom_label(aes(label = "90%",
                                  x = 8.2,
                                  y = max(subset(percentiles2, percentiles2$Percentile == 90)$Performance)))
          
        }
        if (!is.null(subset(percentiles2, percentiles2$Percentile == 80))) {
          g <- g + geom_ribbon(data = subset(percentiles2, percentiles2$Percentile == 80),
                               aes(x = Week,
                                   ymax = Performance,
                                   ymin = minPerformance),
                               alpha = .5,
                               fill = "gold",
                               color = "grey")
          g <- g + geom_label(aes(label = "80%",
                                  x = 8.2,
                                  y = max(subset(percentiles2, percentiles2$Percentile == 80)$Performance)))
        }
        if (!is.null(subset(percentiles2, percentiles2$Percentile == 70))) {
          g <- g + geom_ribbon(data = subset(percentiles2, percentiles2$Percentile == 70),
                               aes(x = Week,
                                   ymax = Performance,
                                   ymin = minPerformance),
                               alpha = .5,
                               fill = "gold1",
                               color = "grey")
          g <- g + geom_label(aes(label = "70%",
                                  x = 8.2,
                                  y = max(subset(percentiles2, percentiles2$Percentile == 70)$Performance)))
        }
        if (!is.null(subset(percentiles2, percentiles2$Percentile == 60))) {
          g <- g + geom_ribbon(data = subset(percentiles2, percentiles2$Percentile == 60),
                               aes(x = Week,
                                   ymax = Performance,
                                   ymin = minPerformance),
                               alpha = .5,
                               fill = "gold2",
                               color = "grey")
          g <- g + geom_label(aes(label = "60%",
                                  x = 8.2,
                                  y = max(subset(percentiles2, percentiles2$Percentile == 60)$Performance)))
        }
        if (!is.null(subset(percentiles2, percentiles2$Percentile == 50))) {
          g <- g + geom_ribbon(data = subset(percentiles2, percentiles2$Percentile == 50),
                               aes(x = Week,
                                   ymax = Performance,
                                   ymin = minPerformance),
                               alpha = .5,
                               fill = "darkorange",
                               color = "grey")
          g <- g + geom_label(aes(label = "50%",
                                  x = 8.2,
                                  y = max(subset(percentiles2, percentiles2$Percentile == 50)$Performance)))
        }
        if (!is.null(subset(percentiles2, percentiles2$Percentile == 40))) {
          g <- g + geom_ribbon(data = subset(percentiles2, percentiles2$Percentile == 40),
                               aes(x = Week,
                                   ymax = Performance,
                                   ymin = minPerformance),
                               alpha = .5,
                               fill = "darkorange1",
                               color = "grey")
          g <- g + geom_label(aes(label = "40%",
                                  x = 8.2,
                                  y = max(subset(percentiles2, percentiles2$Percentile == 40)$Performance)))
        }
        if (!is.null(subset(percentiles2, percentiles2$Percentile == 30))) {
          g <- g + geom_ribbon(data = subset(percentiles2, percentiles2$Percentile == 30),
                               aes(x = Week,
                                   ymax = Performance,
                                   ymin = minPerformance),
                               alpha = .5,
                               fill = "darkorange2",
                               color = "grey")
          g <- g + geom_label(aes(label = "30%",
                                  x = 8.2,
                                  y = max(subset(percentiles2, percentiles2$Percentile == 30)$Performance)))
        }
        if (!is.null(subset(percentiles2, percentiles2$Percentile == 20))) {
          g <- g + geom_ribbon(data = subset(percentiles2, percentiles2$Percentile == 20),
                               aes(x = Week,
                                   ymax = Performance,
                                   ymin = minPerformance),
                               alpha = .5,
                               fill = "orangered",
                               color = "grey")
          g <- g + geom_label(aes(label = "20%",
                                  x = 8.2,
                                  y = max(subset(percentiles2, percentiles2$Percentile == 20)$Performance)))
        }
        if (!is.null(subset(percentiles2, percentiles2$Percentile == 10))) {
          g <- g + geom_ribbon(data = subset(percentiles2, percentiles2$Percentile == 10),
                               aes(x = Week,
                                   ymax = Performance,
                                   ymin = minPerformance),
                               alpha = .5,
                               fill = "orangered1",
                               color = "grey")
          g <- g + geom_label(aes(label = "10%",
                                  x = 8.2,
                                  y = max(subset(percentiles2, percentiles2$Percentile == 10)$Performance)))
        }
        if (!is.null(subset(percentiles2, percentiles2$Percentile == 5))) {
          g <- g + geom_ribbon(data = subset(percentiles2, percentiles2$Percentile == 5),
                               aes(x = Week,
                                   ymax = Performance,
                                   ymin = minPerformance),
                               alpha = .5,
                               fill = "orangered2",
                               color = "grey")
          g <- g + geom_label(aes(label = "5%",
                                  x = 8.2,
                                  y = max(subset(percentiles2, percentiles2$Percentile == 5)$Performance)))
        }
        if (!is.null(subset(percentiles2, percentiles2$Percentile == 1))) {
          g <- g + geom_ribbon(data = subset(percentiles2, percentiles2$Percentile == 1),
                               aes(x = Week,
                                   ymax = Performance,
                                   ymin = minPerformance),
                               alpha = .5,
                               fill = "orangered3",
                               color = "grey")
          g <- g + geom_label(aes(label = "1%",
                                  x = 8.2,
                                  y = max(subset(percentiles2, percentiles2$Percentile == 1)$Performance)))
        }

      }
      
      if ("Method" %in% names(student_perf)) {
        student_perf_long <- melt(student_perf, 
                                  id.vars = c("Week","Method"),
                                  measure.vars = names(student_perf)[grepl("Performance",names(student_perf))])
      } else {
        student_perf_long <- melt(student_perf, 
                                  id.vars = "Week")
      }
      
      student_perf_long$variable <- gsub("Performance","",student_perf_long$variable)
      
      # Draw student's performance
      if ("Method" %in% names(student_perf)) {
        g <- g + geom_point(data = student_perf_long,
                            aes(x = Week,
                                y = value,
                                shape = Method,
                                fill = variable),
                            color = "black",
                            #shape = 21,
                            size = 3,
                            position = position_jitter(w = 0.1, h = 0))
        
        g <- g + scale_shape_manual(values = 21:25, name = "Τεχνική")
      } else {
        if (length(names(student_perf)[grepl("Performance", names(student_perf))]) == 1) {
          g <- g + geom_point(data = student_perf_long,
                              aes(x = Week,
                                  y = value),
                              color = "black",
                              fill = "red",
                              shape = 21,
                              size = 3)
        } else {
          g <- g + geom_point(data = student_perf_long,
                              aes(x = Week,
                                  y = value,
                                  fill = variable),
                              color = "black",
                              shape = 21,
                              size = 3,
                              position = position_jitter(w = 0.1, h = 0))
        }
      }

      g <- g + geom_hline(data = student_perf,
                          yintercept = start_perf,
                          linetype = "dashed",
                          color = "red")
      
      if (!input$show_percentiles) {
        g <- g + geom_label(data = student_perf,
                            x = 8,
                            y = start_perf,
                            label = start_perf)
      }
      
      goal <- as.numeric(input$input_percentile_goal)
      if (goal != 1 & goal != 99) {
        goal <- 5*round(goal/5)
      }
      
      # Draw goal line
      if (percentiles$Performance[percentiles$Percentile == goal] > start_perf) {
        
        goal_data <- subset(percentiles2, percentiles2$Percentile == goal)
        
        g <- g + geom_line(data = goal_data,
                           aes(x = Week,
                               y = WeekPerformance,
                               group = factor(Percentile)))
        
        if (!input$show_percentiles) {
          g <- g + geom_hline(data = goal_data,
                              yintercept = max(goal_data$WeekPerformance),
                              linetype = "dashed",
                              color = "blue")
          
          g <- g + geom_label(data = goal_data,
                            x = 8,
                            y = max(goal_data$WeekPerformance),
                            label = max(goal_data$WeekPerformance))
        }
      }
      
      if (input$show_smooth_line) {
        mean_performances <- setDT(student_perf_long)[, list(mean_performance = mean(value, na.rm = T)),
                                                      by = Week]
        
        g <- g + geom_smooth(data = mean_performances,
                             aes(x = Week,
                                 y = mean_performance),
                             method='lm',
                             formula= y~x)
      }
      # + scale_x_continuous(breaks = 0:8)
      g <- g + scale_fill_discrete(name="Φορά") + scale_x_continuous(breaks = 1:length(student_perf))
      
      #if (input$show_percentiles) {
        g <- g + scale_y_continuous(breaks = pretty(percentiles$Performance, 10))
      #} #else {
        #g <- g + scale_y_continuous(breaks = pretty(student_perf_long$value, 10))
      #}
      g <- g + xlab("Εβδομάδα") + ylab(paste0("Επίδοση (",input$performance_metric,")"))
      g <- g + theme_solarized()
      
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
