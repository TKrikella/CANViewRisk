library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(png)
library(grid)
library(patchwork)
library(shinycssloaders)

ui <- navbarPage(
  title = "CANViewRisk",
  id = "tabs",
  theme = shinytheme("flatly"),
  
  header = tags$head(
    tags$style(HTML("
      .navbar-brand { cursor: default; pointer-events: none; }
      .sidebar { background-color: #f8f9fa; }
      body { font-size: 15px; }
      /* Prevent CSS from forcing capitalization or small-caps */
      * { text-transform: none !important; font-variant: normal !important; }
      .plot-title-display { 
        text-align: center; 
        font-weight: bold; 
        font-size: 24px; 
        margin-top: 10px;
        margin-bottom: 0px; 
        color: #2c3e50;
      }
    "))
  ),
  
  # ------------------
  # Tab 1: Introduction & Case Study
  # ------------------
  tabPanel("How to Use This Tool", value = "info",
           fluidRow(
             column(1),
             column(10,
                    br(),
                    h3("Welcome to CANViewRisk"),
                    p("Welcome to CANViewRisk, a Shiny application developed by Canadian statisticians 
                      to help visualize risk using best-practice methods. We allow users to develop 
                      icon arrays (ie. pictographs) to visualize absolute risks in the form of frequencies. Icon arrays 
                      have been shown to be an effective visualization for those with lower numeracy; 
                      conveying risks in terms of absolute risk instead of relative risk reduces 
                      misinterpretation and prevents exaggerated perceptions of benefit or harm."),
                    
                    p("This application is organized into four tabs. The first is this introduction 
                      tab, which includes helpful definitions and examples. The 'Single Absolute Risk' 
                      tab allows you to input a single value to generate a visualization. The 
                      'Single Relative Risk' tab calculates the change between a baseline risk and 
                      a second outcome based on a relative increase, displaying them as two icon arrays. 
                      Finally, the 'Compare Two Absolute Risks' tab allows for a side-by-side comparison 
                      of two independent risks. In all three visualization tabs, you can export 
                      your icon arrays as a PDF."),
                    
                    hr(),
                    
                    h3("Case Study: The Power of Absolute Risk Visualization"),
                    p("Consider the landmark WOSCOPS study (Shepherd et al., 1995) which investigated the 
                      effect of statins on heart disease. The study reported a 31% relative risk reduction 
                      in heart attacks. However, rounding to the nearest whole number for clarity, we can 
                      see the absolute impact per 100 people:"),
                    
                    fluidRow(
                      column(6, 
                             wellPanel(
                               tags$b("The Baseline Risk"),
                               p("In the placebo group, the risk was ~8% (8 out of 100)."),
                               withSpinner(plotOutput("case_study_plot1", height = "300px"), color = "#2c3e50")
                             )
                      ),
                      column(6, 
                             wellPanel(
                               tags$b("The Reduced Risk"),
                               p("With the drug, the risk dropped to ~5% (5 out of 100)."),
                               withSpinner(plotOutput("case_study_plot2", height = "300px"), color = "#2c3e50")
                             )
                      )
                    ),
                    
                    p(tags$i("Interpretation:"), "While a 31% reduction sounds massive, the icon array shows that for every 100 people 
                      taking the medication, about 3 fewer people will have a heart attack. This visual 
                      helps patients and clinicians weigh the benefits against potential side effects 
                      without being misled by large relative percentages."),
                    
                    hr(),
                    
                    h3("Key Citations & Evidence"),
                    tags$ul(
                      tags$li(
                        tags$b("The WOSCOPS Study: "), 
                        "Shepherd, J., et al. (1995). Prevention of Coronary Heart Disease with Pravastatin in Men with Hypercholesterolemia. ", 
                        tags$i("New England Journal of Medicine"), ", 333(20), 1301-1307."
                      ),
                      tags$li(
                        tags$b("Best-Practice Methods in Risk Communication: "), 
                        "Spiegelhalter, D. (2017). Risk and Uncertainty Communication.", 
                        tags$i("Annual Review of Statistics and Its Application.")
                      )
                    ),
                    
                    hr(), 
                    
                    h3("Notes"), 
                    p("(1) The exported PDFs are high-resolution vector graphics and have better quality than the preview images shown in this application."),
                    p("(2) The icon arrays may take several moments to generate. If you see the loading spinner or progress bar, the application is working and will display your plot shortly."),
                    
                    hr(), 
                    
                    h3("Contact Us"), 
                    p("To report any issues with the web application, please contact krikella@yorku.ca.")
             ), 
             column(1)
           )
  ),
  
  # ------------------
  # Tab 2: Single Absolute Risk
  # ------------------
  tabPanel("Single Absolute Risk", value = "abs",
           sidebarLayout(
             sidebarPanel(
               h4("Visualization Settings"),
               textInput("user_title1", "Main Plot Title:", value = "Insert title here"),
               sliderInput("total_n1", "Total Population (Icons):", min = 10, max = 100, value = 100, step = 10),
               hr(),
               h4("Risk Input"),
               numericInput("direct_abs", "Absolute Risk (%)", value = 8, min = 0, max = 100, step = 0.1),
               br(),
               downloadButton("downloadAbs", "Export Pictograph as pdf", class = "btn-primary")
             ),
             mainPanel(
               div(class = "plot-title-display", textOutput("title_out1")),
               withSpinner(plotOutput("single_plot", height = "600px"), color = "#2c3e50")
             )
           )
  ),
  
  # ------------------
  # Tab 3: Relative Change
  # ------------------
  tabPanel("Single Relative Risk", value = "comp",
           sidebarLayout(
             sidebarPanel(
               h4("Visualization Settings"),
               textInput("user_title2", "Main Plot Title:", value = "Insert title here"),
               sliderInput("total_n2", "Total Population (Icons):", min = 10, max = 100, value = 100, step = 10),
               hr(),
               h4("Risk Calculation"),
               numericInput("baseline", "Baseline Risk (%)", value = 8, min = 0, max = 100, step = 0.1),
               radioButtons("rel_direction", "Direction of Change:",
                            choices = list("Increase (+)" = "inc", "Decrease (-)" = "dec"),
                            selected = "dec", inline = TRUE),
               numericInput("pct_change", "Relative Change (%)", value = 31, min = 0, step = 0.1),
               br(),
               downloadButton("downloadRel", "Export Comparison as pdf", class = "btn-primary")
             ),
             mainPanel(
               div(class = "plot-title-display", textOutput("title_out2")),
               withSpinner(plotOutput("comp_plot", height = "600px"), color = "#2c3e50"),
               wellPanel(
                 h4("Calculation Summary"),
                 htmlOutput("comp_text")
               )
             )
           )
  ),
  
  # ------------------
  # Tab 4: Compare Two Risks
  # ------------------
  tabPanel("Compare Two Absolute Risks", value = "dual",
           sidebarLayout(
             sidebarPanel(
               h4("Visualization Settings"),
               textInput("user_title3", "Main Plot Title:", value = "Insert title here"),
               sliderInput("total_n3", "Total Population (Icons):", min = 10, max = 100, value = 100, step = 10),
               hr(),
               h4("Condition A"),
               textInput("label_a", "Label:", value = "Condition A"),
               numericInput("risk1", "Risk (%)", value = 2, min = 0, max = 100, step = 0.1),
               hr(),
               h4("Condition B"),
               textInput("label_b", "Label:", value = "Condition B"),
               numericInput("risk2", "Risk (%)", value = 8, min = 0, max = 100, step = 0.1),
               br(),
               downloadButton("downloadDual", "Export Comparison as pdf", class = "btn-primary")
             ),
             mainPanel(
               div(class = "plot-title-display", textOutput("title_out3")),
               withSpinner(plotOutput("dual_plot", height = "600px"), color = "#2c3e50")
             )
           )
  )
)

server <- function(input, output, session) {
  
  # Titles
  output$title_out1 <- renderText({ input$user_title1 })
  output$title_out2 <- renderText({ input$user_title2 })
  output$title_out3 <- renderText({ input$user_title3 })
  
  # Case Study Plots for the Introduction Tab
  # We use manually rounded values (8 and 5) for the clearest possible example
  output$case_study_plot1 <- renderPlot({
    make_icon_plot(100, 8, "Placebo (Baseline)", 8)
  })
  
  output$case_study_plot2 <- renderPlot({
    make_icon_plot(100, 5, "Statin (Treatment)", 5)
  })
  
  # Icon plotting function
  make_icon_plot <- function(n, count, label_prefix, pct, 
                             img_coloured = "www/coloured.png",
                             img_outline  = "www/outline.png",
                             ncol = 10) {
    
    img_col <- readPNG(img_coloured)
    img_out <- readPNG(img_outline)
    
    nrow <- ceiling(n / ncol)
    grid_data <- expand.grid(x = 1:ncol, y = nrow:1) %>%
      slice(1:n) %>%
      arrange(desc(y), x) %>%
      mutate(id = row_number(),
             is_risk = id <= floor(count))
    
    p <- ggplot(grid_data, aes(x, y)) +
      coord_fixed() +
      scale_x_continuous(limits = c(0.5, ncol + 0.5)) +
      scale_y_continuous(limits = c(0.5, nrow + 0.5)) +
      theme_void() +
      labs(subtitle = paste0(label_prefix, ": ", pct, "% (", round(count, 1), " out of ", n, ")")) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        
        plot.subtitle = element_text(hjust = 0.5, size = 16, color = "#7f8c8d", margin = margin(t = 10, b = 20)),
        plot.margin = margin(10, 10, 10, 10)
      )
    
    padding <- 0.48
    icons <- mapply(function(x, y, is_risk) {
      img <- if(is_risk) img_col else img_out
      annotation_custom(rasterGrob(img, interpolate = TRUE),
                        xmin = x - padding, xmax = x + padding,
                        ymin = y - padding, ymax = y + padding)
    }, grid_data$x, grid_data$y, grid_data$is_risk, SIMPLIFY = FALSE)
    
    p + icons
  }
  
  # Render plots
  output$single_plot <- renderPlot({
    cnt <- (input$direct_abs / 100) * input$total_n1
    make_icon_plot(input$total_n1, cnt, "Absolute Risk", input$direct_abs)
  })
  
  output$comp_plot <- renderPlot({
    # Determine the multiplier based on direction
    mult <- if(input$rel_direction == "inc") {
      1 + (input$pct_change / 100)
    } else {
      1 - (input$pct_change / 100)
    }
    
    b_cnt <- (input$baseline / 100) * input$total_n2
    n_risk <- input$baseline * mult
    
    # Ensure risk doesn't go below 0
    n_risk <- max(0, n_risk)
    
    n_cnt <- (n_risk / 100) * input$total_n2
    
    p1 <- make_icon_plot(input$total_n2, b_cnt, "Baseline", input$baseline)
    p2 <- make_icon_plot(input$total_n2, n_cnt, "New Risk", round(n_risk, 2))
    p1 + p2
  })
  
  output$comp_text <- renderUI({
    mult <- if(input$rel_direction == "inc") {
      1 + (input$pct_change / 100)
    } else {
      1 - (input$pct_change / 100)
    }
    
    n_risk <- max(0, input$baseline * mult)
    direction_text <- if(input$rel_direction == "inc") "relative increase" else "relative decrease"
    
    HTML(paste0("A baseline risk of <b>", input$baseline, "%</b> with a <b>", input$pct_change, 
                "% ", direction_text, "</b> results in a new absolute risk of <b>", round(n_risk, 2), "%</b>."))
  })
  
  output$dual_plot <- renderPlot({
    cnt1 <- (input$risk1 / 100) * input$total_n3
    cnt2 <- (input$risk2 / 100) * input$total_n3
    p1 <- make_icon_plot(input$total_n3, cnt1, input$label_a, input$risk1)
    p2 <- make_icon_plot(input$total_n3, cnt2, input$label_b, input$risk2)
    p1 + p2
  })
  
  # Downloads
  output$downloadAbs <- downloadHandler(
    filename = function() { "absolute_risk.pdf" },
    content = function(file) {
      # This adds the progress notification at the top of the screen
      withProgress(message = 'Generating PDF', detail = 'This may take a moment...', value = 0, {
        
        incProgress(0.3, detail = "Calculating plot...") # Move bar to 30%
        cnt <- (input$direct_abs / 100) * input$total_n1
        p <- make_icon_plot(input$total_n1, cnt, "Absolute Risk", input$direct_abs)
        p <- p + labs(title = input$user_title1) + 
          theme(plot.title = element_text(hjust=0.5, size=20))
        
        incProgress(0.6, detail = "Rendering icons...") # Move bar to 90%
        ggsave(file, plot = p, width = 8, height = 7, bg = "white")
        
        incProgress(0.1, detail = "Finished!") # Finish
      })
    }
  )
  
  # Download for Tab 3: Single Relative Risk
  output$downloadRel <- downloadHandler(
    filename = function() { "relative_risk.pdf" },
    content = function(file) {
      withProgress(message = 'Preparing Comparison PDF', detail = 'Rendering arrays...', value = 0, {
        
        incProgress(0.2)
        mult <- if(input$rel_direction == "inc") 1 + (input$pct_change / 100) else 1 - (input$pct_change / 100)
        b_cnt <- (input$baseline / 100) * input$total_n2
        n_risk <- max(0, input$baseline * mult)
        n_cnt <- (n_risk / 100) * input$total_n2
        
        incProgress(0.4, detail = "Creating visualizations...")
        p1 <- make_icon_plot(input$total_n2, b_cnt, "Baseline", input$baseline)
        p2 <- make_icon_plot(input$total_n2, n_cnt, "New Risk", round(n_risk, 2))
        
        combined <- p1 + p2 + 
          plot_annotation(title = input$user_title2, 
                          theme = theme(plot.title = element_text(hjust=0.5, size=20)))
        
        incProgress(0.3, detail = "Saving file...")
        ggsave(file, plot = combined, width = 14, height = 7, bg = "white")
      })
    }
  )
  
  # Download for Tab 4: Compare Two Absolute Risks
  output$downloadDual <- downloadHandler(
    filename = function() { "dual_comparison.pdf" },
    content = function(file) {
      withProgress(message = 'Preparing Comparison PDF', detail = 'Rendering arrays...', value = 0, {
        
        incProgress(0.3)
        cnt1 <- (input$risk1 / 100) * input$total_n3
        cnt2 <- (input$risk2 / 100) * input$total_n3
        
        incProgress(0.3, detail = "Building side-by-side plot...")
        p1 <- make_icon_plot(input$total_n3, cnt1, input$label_a, input$risk1)
        p2 <- make_icon_plot(input$total_n3, cnt2, input$label_b, input$risk2)
        
        combined <- p1 + p2 + 
          plot_annotation(title = input$user_title3, 
                          theme = theme(plot.title = element_text(hjust=0.5, size=20)))
        
        incProgress(0.3, detail = "Saving file...")
        ggsave(file, plot = combined, width = 14, height = 7, bg = "white")
      })
    }
  )

}
shinyApp(ui, server)
