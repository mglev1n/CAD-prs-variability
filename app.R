library(dplyr)
library(ggplot2)
library(patchwork)
# library(stringr)
library(tidyr)
# library(lubridate)
library(forcats)

# Define the list of models
model_list <- c(
    "PGS000010", "PGS000011", "PGS000012", "PGS000013", "PGS000018", "PGS000019", "PGS000057",
    "PGS000058", "PGS000059", "PGS000200", "PGS000296", "PGS000329", "PGS000337", "PGS000349",
    "PGS000710", "PGS000746", "PGS000747", "PGS000748", "PGS000749", "PGS000798", "PGS000818",
    "PGS000899", "PGS000962", "PGS001048", "PGS001314", "PGS001315", "PGS001316", "PGS001317",
    "PGS001355", "PGS001780", "PGS001839", "PGS002048", "PGS002244", "PGS002262", "PGS002775",
    "PGS002776", "PGS002777", "PGS002778", "PGS002809", "PGS003355", "PGS003356", "PGS003438",
    "PGS003446", "PGS003725", "PGS003726", "PGS003866", "PGS_LDP2Auto", "PGS_prscsx"
)

df_ntile_norm <- read.csv("CAD_ref_ntile.csv")

pgs_dates <- read.csv("PGS_dates.csv") %>%
  mutate(PGS_date = as.Date(PGS_date, format = "%m/%d/%Y"),
         PGS_year = as.integer(format(PGS_date, "%Y"))) %>%
  mutate(PGS_year = as.integer(paste0("20", PGS_year))) %>%
  arrange(PGS_date) %>%
  arrange(PGS_date) %>%
  mutate(PGS_ID = forcats::fct_inorder(PGS_ID))

custom_theme <- theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    text = element_text(family = "sans", size = 14),
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    strip.placement = "outside",  # Move strips outside
    panel.border = element_rect(fill = NA, color = "grey70")
  )

# Define a custom color palette inspired by JAMA colors
jama_colors <- c(
    "#374E55", "#DF8F44", "#00A1D5", "#B24745", "#79AF97", 
    "#6A6599", "#80796B", "#0073C2", "#EFC000", "#868686", 
    "#CD534C", "#7AA6DC", "#003C67", "#8F7700", "#594F4F"
)

# Function to generate more colors if needed
extend_jama_colors <- function(n) {
    if (n <= length(jama_colors)) {
        return(jama_colors[1:n])
    }
    colorRampPalette(jama_colors)(n)
}

# Custom CSS for JAMA-inspired styling with improved navbar readability
jama_css <- "
  body {
    background-color: #f8f8f8;
    color: #333333;
    font-family: Arial, sans-serif;
  }
  .container-fluid {
    max-width: 1200px;
    margin: 0 auto;
  }
  .well {
    background-color: #ffffff;
    border: 1px solid #dddddd;
    border-radius: 4px;
    box-shadow: 0 1px 1px rgba(0, 0, 0, 0.05);
  }
  .btn-default {
    color: #ffffff;
    background-color: #0072B5;
    border-color: #0072B5;
  }
  .btn-default:hover {
    background-color: #005a8e;
    border-color: #005a8e;
  }
  .form-control {
    border-color: #dddddd;
  }
  .selectize-input {
    border-color: #dddddd;
  }
  .shiny-notification {
    background-color: #0072B5;
    color: #ffffff;
  }
  .navbar {
    background-color: #B24745FF;
    border-color: #B24745FF;
  }
  .navbar-brand {
    color: #ffffff !important;
  }
  .navbar-nav > li > a {
    color: #ffffff !important;
    font-weight: bold;
  }
  .navbar-nav > li > a:hover,
  .navbar-nav > li > a:focus {
    background-color: #803332 !important;
    color: #ffffff !important;
  }
  .navbar-nav > .active > a,
  .navbar-nav > .active > a:hover,
  .navbar-nav > .active > a:focus {
    color: #ffffff !important;
    background-color: #B24745FF !important;
  }
  .citation {
    background-color: #f8f8f8;
    border-left: 5px solid #0072B5;
    padding: 15px;
    margin: 20px 0;
    font-size: 0.9em;
    font-style: italic;
  }
  .citation strong {
    font-style: normal;
  }
  .tab-pane {
    background-color: #ffffff;
    padding: 20px;
    box-shadow: 0 1px 3px rgba(0, 0, 0, 0.12), 0 1px 2px rgba(0, 0, 0, 0.24);
  }
  code {
    background-color: #f0f0f0;
    border-radius: 3px;
    font-family: Consolas, Monaco, 'Andale Mono', 'Ubuntu Mono', monospace;
    font-size: 0.9em;
    padding: 2px 4px;
  }
"

# UI for Shiny App
ui <- fluidPage(
    tags$head(
        HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=G-QK378MNTCR'></script>
<script>
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', 'G-QK378MNTCR');
</script>"),
        # includeHTML("google-analytics.html"),
        tags$style(HTML(jama_css))
    ),
    navbarPage(
        "CAD PRS Variability",
        tabPanel(
            "Plot",
            p("Ancestry-normalized polygenic risk scores (PRS) for coronary artery disease (CAD) are plotted for a random selection of individuals from the 1000 Genomes + HGDP reference panel. Scores are arranged on the x-axis by year of publication. For each individual, the y-axis shows the percentile rank of each CAD PRS within the reference panel. A boxplot summarizes the distribution of scores for each individual."),
            sidebarLayout(
              sidebarPanel(
                width = 3,
                actionButton("plot_button", "Plot PRS Variability", class = "btn-block"),
                hr(),
                checkboxInput("show_advanced", "Show Advanced Options", FALSE),
                conditionalPanel(
                  condition = "input.show_advanced == true",
                  selectInput("selected_models",
                              label = "Select CAD PRS",
                              choices = model_list,
                              selected = model_list,
                              multiple = TRUE
                  ),
                  numericInput("seed_input",
                               label = "Set Random Seed (leave empty for random)",
                               value = NA
                  ),
                  sliderInput("sample_size_input",
                              label = "Select number of individuals",
                              min = 1,
                              max = 10,
                              value = 5,
                              step = 1
                  ),
                  # Add a new range slider for year selection
                  sliderInput("year_range",
                              label = "Select Year Range",
                              min = min(pgs_dates$PGS_year),
                              max = max(pgs_dates$PGS_year),
                              value = c(min(pgs_dates$PGS_year), max(pgs_dates$PGS_year)),
                              step = 1,
                              sep = ""
                  )
                )
              ),
              mainPanel(
                width = 9,
                textOutput("year_range_text"),
                plotOutput("score_plot", height = "600px")
              )
            )
        ),
        tabPanel(
            "About",
            fluidRow(
                column(
                    12,
                    h2("About CAD PRS Variability"),
                    p(HTML("This application visualizes the variability of Coronary Artery Disease (CAD) Polygenic Risk Scores (PRS) across different individuals from the 1000 Genomes + HGDP Reference Panel. CAD PRS weights were obtained from the <a href = 'https://www.pgscatalog.org/'>PGS Catalog</a>, and were used to calculate the PRS for each individual using <a href = 'https://pgsc-calc.readthedocs.io/en/latest/', target='_blank'><code>pgsc_calc</code></a>. The Z_norm2 approach was applied to normalize risk scores and their variance across population groups using PCA.")),                    
                    h3("Data Source:"),
                    p(
                        "The PRS used in this application are sourced from the ",
                        tags$a(href = "https://www.pgscatalog.org/", "PGS Catalog"),
                        "focusing on those related to Coronary Artery Disease."
                    ),
                    h3("How to Use:"),
                    tags$ol(
                        tags$li("Navigate to the 'Plot' tab."),
                        tags$li("Click 'Plot PRS Variability' to generate a visualization."),
                        tags$li("Use 'Advanced Options' to customize the plot:"),
                        tags$ul(
                            tags$li("Select specific CAD PRS models to include."),
                            tags$li("Set a random seed for reproducibility."),
                            tags$li("Adjust the number of individuals to display.")
                        )
                    ),
                    h3("Manuscript Citation:"),
                    div(
                        class = "citation",
                        p(strong("Population Performance and Individual Agreement of Coronary Artery Disease Polygenic Risk Scores")),
                        p("Sarah A. Abramowitz, Kristin Boulier, Karl Keat, Katie M. Cardone, Manu Shivakumar, John DePaolo, Renae Judy, Dokyoon Kim, Daniel J. Rader, Marylyn Ritchie, Benjamin F. Voight, Bogdan Pasaniuc, Michael G. Levin*, Scott M. Damrauer*"),
                        p("* jointly supervised"),
                        p("doi: ", a(href = "https://doi.org/10.1101/2024.07.25.24310931", "https://doi.org/10.1101/2024.07.25.24310931", target="_blank"))
                    ),
                    h3("About the Developers:"),
                    p("This application was developed by Sarah Abramowitz and Michael Levin at the University of Pennsylvania. Application source code is available on ", tags$a(href = "https://github.com/mglev1n/CAD-prs-variability", "GitHub.", target="_blank"), "For more information or to report issues, please contact ", tags$a(href = "mailto:Michael.Levin@pennmedicine.upenn.edu", "Michael.Levin@pennmedicine.upenn.edu.", target="_blank")),
                )
            )
        )
    )
)

# Server logic
# Server logic
server <- function(input, output, session) {
  current_seed <- reactiveVal(NULL)
  
  selected_years <- reactive({
    if (is.null(input$selected_models)) {
      pgs_dates$PGS_year
    } else {
      pgs_dates %>%
        filter(PGS_ID %in% input$selected_models) %>%
        pull(PGS_year)
    }
  })
  
  observe({
    updateSliderInput(session, "year_range",
                      min = min(selected_years()),
                      max = max(selected_years()),
                      value = c(min(selected_years()), max(selected_years()))
    )
  })
  
  generate_plot <- function() {
    model_selection <- if (is.null(input$selected_models)) model_list else input$selected_models
    
    filtered_pgs_dates <- pgs_dates %>%
      filter(PGS_year >= input$year_range[1] & PGS_year <= input$year_range[2])
    
    model_selection <- intersect(model_selection, filtered_pgs_dates$PGS_ID)
    
    seed_value <- if (!is.na(input$seed_input) && !is.null(input$seed_input)) {
      input$seed_input
    } else {
      as.integer(runif(1) * 1e6)
    }
    
    current_seed(seed_value)
    set.seed(seed_value)
    
    if (is.na(input$seed_input) || is.null(input$seed_input)) {
      showNotification(paste("Using a randomly generated seed:", seed_value), type = "message")
    }
    
    ntile_list <- paste("ntile_", model_selection, sep = "")
    sample_size <- ifelse(is.null(input$sample_size_input), 5, input$sample_size_input)
    random_ntile <- sample_n(df_ntile_norm, sample_size) %>% select(IID, all_of(ntile_list))
    
    melt_random_ntile <- random_ntile %>%
      pivot_longer(cols = -IID, names_to = "variable", values_to = "value") %>%
      mutate(variable = gsub("ntile_", "", variable)) %>%
      left_join(filtered_pgs_dates, by = c("variable" = "PGS_ID"))
    
    point_plot <- ggplot(data = melt_random_ntile, aes(x = variable, y = value, fill = IID, group = IID)) +
      geom_hline(yintercept = 50, linetype = "dashed") +
      geom_point(size = 3, shape = 21) +
      facet_grid(rows = vars(IID), cols = vars(PGS_year), switch = "y", scales = "free_x", space = "free_x") +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      labs(x = "CAD PRS Ordered by Year of Publication", y = "Percentile", caption = paste("Seed:", seed_value)) +
      custom_theme +
      scale_fill_manual(values = extend_jama_colors(length(unique(melt_random_ntile$IID))), guide = "none") +
      theme(
        strip.text.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5),
        panel.spacing.x = unit(0, "lines")
      )
    
    beeswarm_plot <- melt_random_ntile %>%
      ggplot(aes(y = value, x = "a", fill = IID)) +
      geom_jitter(aes(fill = IID), shape = 21, width = 0.2, height = 0, size = 2, alpha = 0.5) +
      geom_boxplot(width = 0.3, outlier.shape = NA, fill = "white", linewidth = 1) +
      geom_hline(yintercept = 50, linetype = "dashed") +
      facet_grid(rows = vars(IID), scales = "free_x") +
      scale_fill_manual(values = extend_jama_colors(length(unique(melt_random_ntile$IID))), guide = "none") +
      # scale_color_manual(values = extend_jama_colors(length(unique(melt_random_ntile$IID))), guide = "none") +
      labs(x = "", y = NULL) +
      custom_theme +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
    
    combined_plot <- point_plot + beeswarm_plot +
      plot_layout(ncol = 2, widths = c(9, 1)) +
      plot_annotation(
        theme = theme(plot.margin = margin(5.5, 5.5, 25, 20))
      ) &
      theme(plot.margin = margin(5.5, 0, 5.5, 0))
    
    return(combined_plot)
  }
  
  observeEvent(input$plot_button, {
    output$score_plot <- renderPlot({
      generate_plot()
    })
  })
  
  # output$year_range_text <- renderText({
  #   paste("Showing scores from", input$year_range[1], "to", input$year_range[2])
  # })
  
  # Automatically generate plot on app startup
  observe({
    req(initial_load())
    output$score_plot <- renderPlot({
      generate_plot()
    })
  })
  
  # Set initial_load to TRUE after a short delay
  initial_load <- reactiveVal(FALSE)
  session$onFlushed(function() {
    initial_load(TRUE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)


# vroom::vroom("CAD_ref_ntile.csv") %>%
#   select(IID, contains("ntile")) %>%
#   pivot_longer(contains("ntile"), names_to = "variable") %>%
#   mutate(variable = str_replace(variable, "ntile_", "")) %>%
#   # Get 25 random IIDs
#   group_by(IID) %>%
#   slice_sample(n = 1) %>%  # Take one row per IID to make sampling faster
#   ungroup() %>%
#   slice_sample(n = 50) %>%  # Randomly select 25 IIDs
#   select(IID) %>%  # Keep only the IID column
#   # Join back with the original data to get all rows for these IIDs
#   inner_join(
#     vroom::vroom("CAD_ref_ntile.csv") %>%
#       select(IID, contains("ntile")) %>%
#       pivot_longer(contains("ntile"), names_to = "variable") %>%
#       mutate(variable = str_replace(variable, "ntile_", "")),
#     by = "IID"
#   ) %>%
#   ggplot(aes(x = IID, y = value), x = "a") +
#   geom_jitter(width = 0.2, height = 0, size = 2, alpha = 0.5, aes(color = IID)) +
#   # ggdist::geom_swarm(dotsize = 1.5, color = "black", alpha = 0.5) +
#   geom_boxplot(width = 0.15, outlier.shape = NA, fill = "white", linewidth = 1) +
#   geom_hline(yintercept = 50, linetype = "dashed") +
#   facet_wrap(~IID, scales = "free_x", nrow = 10, ncol = 5) +
#   scale_color_manual(values = extend_jama_colors(50), guide = "none") +
#   # ggsci::scale_fill_jama(guide = "none") +
#   labs(x = "", y = "Percentile") +
#   scale_y_continuous(labels = scales::percent_format(scale = 1)) +
#   theme_minimal() +
#   theme(
#     panel.background = element_rect(fill = "white"),
#     text = element_text(family = "Arial", size = 14),
#     plot.title = element_text(size = 16, hjust = 0.5),
#     plot.subtitle = element_text(size = 14, hjust = 0.5),
#     plot.margin = margin(20, 20, 20, 20),
#     axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5) # Updated x-axis text settings
#   ) +
#   theme(
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank()
#   )
