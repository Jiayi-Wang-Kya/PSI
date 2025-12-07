library(shiny)
library(bslib)
library(ggplot2)
library(waffle)
library(dplyr)
library(tidyr)

# Define UI
ui <- fluidPage(
    # Custom CSS for a fun, colorful look
    tags$head(
        tags$style(HTML("
            body { 
                background-image: url('bg.jpg');
                background-size: 50%; 
                background-size: 300px;
                background-size: contain;
                background-size: cover; 
            }
            .title { color: #ffe0b2; font-size: 28px; font-weight: bold; text-align: center; margin-bottom: 20px; font-family: 'Comic Sans MS', cursive, sans-serif; text-shadow: 2px 2px 4px rgba(0,0,0,0.3); }
            .well { background-color: #e8f5e9; border-color: #81c784; border-radius: 15px; }
            .btn-primary { background-color: #ff7043; border-color: #ff5722; }
            .btn-primary:hover { background-color: #ff5722; border-color: #f4511e; }
            .slider-label { font-weight: bold; color: #0277bd; }
            .result-box { border-radius: 10px; padding: 15px; margin-top: 10px; background-color: #fff8e1; border: 2px dashed #ffca28; }
            .download-btn { margin-top: 10px; background-color: #4caf50; border-color: #388e3c; }
            .download-btn:hover { background-color: #388e3c; border-color: #2e7d32; }
        "))
    ),
    
    # Title
    div(class = "title", "Pain Score Explorer: Normal Distribution Adventure!"),
    
    # Sidebar with controls
    sidebarLayout(
        sidebarPanel(
            width = 3,
            div(class = "slider-label", "Number of Patients:"),
            sliderInput("n", "", min = 10, max = 100, value = 30, step = 5),
            
            div(class = "slider-label", "Pain Score (Mean):"),
            sliderInput("mean", "", min = 0, max = 10, value = 5, step = 0.5),
            
            div(class = "slider-label", "Variability (Standard Deviation):"),
            sliderInput("sd", "", min = 0.5, max = 3, value = 1, step = 0.1),
            
            actionButton("generate", "Generate New Data!", icon = icon("refresh"))
        ),
        
        # Main panel with visualizations
        mainPanel(
            width = 9,
            tabsetPanel(
                tabPanel("Introduction", 
                         div(class = "result-box", 
                             h3("Welcome to the Pain Score Explorer!", style = "color: #5e35b1;"),
                             p("This app simulates pain scores for a group of patients based on a normal distribution. Adjust the parameters to see how the scores change!"),
                             p("Use the sliders to set the number of patients, mean pain score, and variability (standard deviation). Click 'Generate New Data' to refresh the results.")
                         ),
                         fluidRow(
                             column(width = 6,
                                    div(class = "result-box", 
                                        h4("What is Mean? ", style = "color: #5e35b1;"),
                                        #img(src='folder-math.png', align = "right", height),
                                        p("The mean represents the average pain score across all patients."),
                                        p("A higher mean indicates greater overall pain in the group."),
                                        p("In real studies, this helps us understand the central tendency of pain reported."),
                                        withMathJax(),
                                        p("Formula: ", "\\( \\bar{x} = \\frac{1}{n} \\sum_{i=1}^{n} x_i \\)", 
                                          style = "font-style: italic;"))
                                    ),
                             column(width = 6,
                                    div(class = "result-box", 
                                        h4("What is Standard Deviation?", style = "color: #5e35b1;"),
                                        #img(src='folder-math.png', align = "right"),
                                        p("Standard deviation measures how spread out the pain scores are."),
                                        p("A higher standard deviation means more variability between patients."),
                                        p("Lower values indicate most patients report similar pain levels."),
                                        p("Formula: ", tags$span("\\( \\sigma = \\sqrt{\\frac{1}{n} \\sum_{i=1}^{n} (x_i - \\bar{x})^2} \\)", 
                                          style = "font-style: italic;"))
                                    )
                             )
                         )
                ),
                tabPanel("Pain Picture", 
                                 div(class = "result-box", 
                                         h3("Each person's pain score is shown as a face:", style = "color: #5e35b1;"),
                                         plotOutput("painPictogram", height = "450px")
                                 )
                ),
                tabPanel("Histogram", 
                                 div(class = "result-box",
                                         h3("Distribution of Pain Scores:", style = "color: #ffffffff;"),
                                         plotOutput("histogram", height = "450px"),
                                         downloadButton("downloadHistogram", "Download Histogram", class = "download-btn")
                                 )
                )
            )
        )
    ))

# Define server logic
server <- function(input, output, session) {
    # Reactive data generation
    pain_data <- eventReactive(input$generate | is.null(input$generate), {
        set.seed(sample(1:1000, 1))  # Random seed for different results each time
        pain_scores <- rnorm(input$n, mean = input$mean, sd = input$sd)
        pain_scores <- pmax(0, pmin(10, pain_scores))  # Constrain between 0 and 10
        return(pain_scores)
    })
    
    # Pictogram output
    output$painPictogram <- renderPlot({
        scores <- pain_data()
        rounded_scores <- round(scores)
        
        # Create a data frame for plotting
        pain_df <- data.frame(
            id = 1:length(scores),
            score = scores,
            rounded_score = rounded_scores
        )
        
        # Define face types and colors based on pain level
        face_types <- c("😀", "🙂", "😐", "😕", "☹️", "😖", "😫", "😩", "🤕", "😭", "🥵")
        
        # Plot
        ggplot(pain_df, aes(x = (id-1) %% 10, y = (id-1) %/% 10, fill = score)) +
            geom_tile(color = "white", size = 0.5) +
            geom_text(aes(label = face_types[rounded_score + 1]), size = 8) +
            scale_fill_gradient2(low = "#27c267", mid = "#ffeb3b", high = "#d50000", 
                                                     midpoint = 5, name = "Pain Level") +
            theme_minimal() +
            theme(
                legend.position = "bottom",
                plot.title = element_text(hjust = 0.5, size = 18, color = "#5e35b1"),
                plot.subtitle = element_text(hjust = 0.5, size = 14, color = "#7e57c2"),
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                panel.grid = element_blank()
            ) +
            labs(
                title = paste0("Pain Scores for ", length(scores), " Patients"),
                subtitle = paste0("Mean: ", round(input$mean, 1), " | Standard Deviation: ", round(input$sd, 1))
            ) +
            coord_equal()
    })
    
    # Histogram output
    output$histogram <- renderPlot({
        scores <- pain_data()
        
        # Create data frame for histogram
        hist_df <- data.frame(score = scores)
        
        ggplot(hist_df, aes(x = score)) +
            geom_histogram(aes(y = after_stat(density), fill = "Histogram"), 
                          bins = 15, color = "white", alpha = 0.7) +
            geom_density(aes(color = "Density Curve"), linewidth = 1.5) +
            geom_vline(xintercept = mean(scores), color = "black", linewidth = 1.5, linetype = "dashed") +
            geom_text(aes(x = mean(scores), y = 0, label = paste("Mean =", round(mean(scores), 2))), 
                                color = "black", hjust = -0.2, vjust = -0.5, size = 5) +
            scale_fill_manual(name = "", values = c("Histogram" = "#42a5f5")) +
            scale_color_manual(name = "", values = c("Density Curve" = "#d81b60")) +
            theme_minimal() +
            theme(
                plot.title = element_text(hjust = 0.5, size = 18, color = "#0277bd"),
                plot.subtitle = element_text(hjust = 0.5, size = 14, color = "#0277bd"),
                axis.title = element_text(size = 14, color = "#0277bd"),
                axis.text = element_text(size = 12),
                panel.grid.major = element_line(color = "#e0e0e0"),
                panel.grid.minor = element_blank(),
                legend.position = "bottom"
            ) +
            labs(
                title = "Distribution of Pain Scores",
                subtitle = paste0("Mean: ", round(input$mean, 1), " | SD: ", round(input$sd, 1), 
                                                    " | Sample Size: ", length(scores)),
                x = "Pain Score (0-10)",
                y = "Frequency"
            ) +
            scale_x_continuous(limits = c(0, 10))
    })

    output$downloadHistogram <- downloadHandler(
        filename = function() {
            paste("pain_histogram_", Sys.Date(), ".png", sep = "")
        },
        content = function(filename) {
            ggsave(filename, plot = output$histogram(), width = 8, height = 6, dpi = 300)
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)

