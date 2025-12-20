library(shiny)
library(ggplot2)
library(lmtest)
library(datasets)

# Data Preparation
sb_raw <- Seatbelts
sb_data <- as.data.frame(sb_raw)
month_labels <- paste0(1:12, ": ", month.abb)
sb_data$Month <- factor(cycle(sb_raw), labels = month_labels)
sb_data$Time <- 1:nrow(sb_data) 

shinyServer(function(input, output) {
    
    # --- Tab 1: Exploratory Regression  ---
    output$regPlot <- renderPlot({
        p <- ggplot(sb_data, aes_string(x = input$predictor, y = "DriversKilled")) +
            geom_point(alpha = 0.6, color = "steelblue") +
            theme_minimal() + 
            theme(axis.text = element_text(color = "black", size = 10))
        if (input$show_lm) p <- p + geom_smooth(method = "lm", color = "firebrick")
        p
    })
    
    output$regSummary <- renderPrint({
        summary(lm(as.formula(paste("DriversKilled ~", input$predictor)), data = sb_data))
    })
    
    output$residualPlotMain <- renderPlot({
        fit <- lm(as.formula(paste("DriversKilled ~", input$predictor)), data = sb_data)
        
        res_data <- data.frame(
            Fitted = predict(fit),
            Residuals = residuals(fit)
        )
        
        ggplot(res_data, aes(x = Fitted, y = Residuals)) +
            geom_point(alpha = 0.6) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
            geom_smooth(method = "loess", color = "blue", se = FALSE, size = 0.5) +
            theme_minimal() + 
            theme(axis.text = element_text(color = "black", size = 10)) +
            labs(title = "Residuals vs Fitted Values (DriversKilled)",
                 x = "Predicted Values",
                 y = "Residuals (Errors)")
    })
    
    
    # --- Tab 2: Front vs. Rear Comparison  ---
    output$plotFront <- renderPlot({
        f <- ggplot(sb_data, aes_string(x = input$predictor, y = "front")) +
            geom_point(color = "darkgreen") + theme_minimal() + 
            theme(axis.text = element_text(color = "black", size = 10))
        if (input$show_lm) f <- f + geom_smooth(method = "lm")
        f
    })
    
    output$plotRear <- renderPlot({
        r <- ggplot(sb_data, aes_string(x = input$predictor, y = "rear")) +
            geom_point(color = "purple")  + theme_minimal() + 
            theme(axis.text = element_text(color = "black", size = 10))
        if (input$show_lm) r <- r + geom_smooth(method = "lm")
        r
    })
    
    output$sumFront <- renderPrint({
        summary(lm(as.formula(paste("front ~", input$predictor)), data = sb_data))
    })
    
    output$sumRear <- renderPrint({
        summary(lm(as.formula(paste("rear ~", input$predictor)), data = sb_data))
    })
    
    # --- Tab 3: Seasonality & Residuals Analysis ---
    model_seasonal <- reactive({
        f <- if(input$use_seasonality) paste("DriversKilled ~", input$predictor, "+ Month")
        else paste("DriversKilled ~", input$predictor)
        lm(as.formula(f), data = sb_data)
    })
    
    output$seasonalPlot <- renderPlot({
        s <- ggplot(sb_data, aes_string(x = input$predictor, y = "DriversKilled", color = "Month")) +
            geom_point(size = 3, alpha = 1) + 
            theme_minimal() + 
            theme(axis.text = element_text(color = "black", size = 10)) +
            labs( title = "Seasonal Trends and Regression Slopes",
                subtitle = "Separate trend lines calculated for each month (if \"Individual Monthly Trends\" is selected in the sidebar)",
                x = input$predictor,
                y = "Drivers Killed",
                color = "Month (Index: Name)"  ) +
            theme( legend.position = "right",
                legend.text = element_text(size = 10),
                legend.title = element_text(face = "bold")) +
            guides(color = guide_legend(override.aes = list(alpha = 1, size = 4)))
        
        if (input$show_lm) { 
          if (input$trend_type == "global") {  
            s <- s + geom_smooth(method = "lm", aes(group = 1), color = "black", size = 1.2)
          } else {                             
            s <- s + geom_smooth(method = "lm", se = FALSE, size = 1)
          }
        }
        s
    })
    
    output$dwTest <- renderPrint({
        dwtest(model_seasonal())
    })
    
    output$residualPlot <- renderPlot({
        res_data <- data.frame(Fitted = predict(model_seasonal()), Res = residuals(model_seasonal()))
        ggplot(res_data, aes(x = Fitted, y = Res)) +
            geom_point() + geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
            geom_smooth(method = "loess", color = "blue", se = FALSE, size = 0.5) +
            theme_minimal() + 
            theme(axis.text = element_text(color = "black", size = 10)) + 
            labs(title = "Residuals vs Fitted Values (DriversKilled)", x = "Predicted Values",
                                   y = "Residuals (Errors)")
    })
    
    # --- Tab 4: Predictor Tool ---
    master_model <- reactive({
        lm(DriversKilled ~ PetrolPrice + Month + law + kms + Time, data = sb_data)
    })
    
    output$predictionResult <- renderText({ 
        selected_month_string <- paste0(input$pred_month_num, ": ", month.abb[input$pred_month_num]) 
        law_val <- as.numeric(input$pred_law_bool)
        
        input_data <- data.frame(
            PetrolPrice = input$pred_petrol, 
            Month = factor(selected_month_string, levels = month_labels),
            law = law_val,
            kms = input$pred_kms,
            Time = max(sb_data$Time) 
        )
        
        pred_val <- predict(master_model(), newdata = input_data)

        paste(round(max(0, pred_val), 0), "Fatalities")
    })
    
    
    accuracy_metrics <- reactive({
        mod <- master_model()
        resids <- residuals(mod)
 
        mae <- mean(abs(resids))

        r2 <- summary(mod)$adj.r.squared
        
        list(mae = round(mae, 1), r2 = round(r2, 3))
    })
    
    output$rSquaredText <- renderText({
        paste(accuracy_metrics()$r2 * 100, "%")
    })

    output$maeText <- renderText({
        paste("±", accuracy_metrics()$mae, "deaths")
    })
    
    # --- Tab 5: About Data --- 
    output$varTable <- renderTable({
        data.frame(
            Variable = c("DriversKilled", "drivers", "front", "rear", "kms", "PetrolPrice", "law"),
            Description = c("Number of car drivers killed",
                            "Drivers killed or seriously injured",
                            "Front seat passengers killed or seriously injured",
                            "Rear seat passengers killed or seriously injured",
                            "Distance driven, in millions of kilometers, in UK that month",
                            "Price of petrol (gasoline) in pounds per litre",
                            "Binary: 1 if the law was in effect, 0 otherwise (=1 for February 1983 onwards).")
        )
    })
    
    output$timeSeriesPlot <- renderPlot({
        sb_data$Date <- seq(as.Date("1969-01-01"), by = "month", length.out = nrow(sb_data))
        law_start_date <- sb_data$Date[170] 
        
        ggplot(sb_data, aes(x = Date, y = DriversKilled)) +
            geom_line(aes(color = factor(law))) +
            geom_point(aes(color = factor(law)), alpha = 0.6) +
            geom_vline(xintercept = as.numeric(law_start_date), 
                       linetype = "dashed", color = "red", size = 1.2) +
            annotate("text", x = law_start_date + 95, y = 180, 
                     label = "1983 Seatbelts\n    Law Starts", color = "red", fontface = "bold", hjust = 0) +
            theme_minimal() + 
            theme(axis.text = element_text(color = "black", size = 10)) +
            labs(title = "Monthly Driver Fatalities (1969 - 1984)",
                 x = "Year", y = "Drivers Killed",
                 color = "Drivers Killed\n(Before/After\nSeatbelts Law)" ) +
            theme(
                legend.position = "right",
                legend.text = element_text(size = 10),
                legend.title = element_text(face = "bold")) +
            scale_color_discrete(labels = c("Before the Law", "After the Law"))
    })
})