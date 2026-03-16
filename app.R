#Sys.setlocale("LC_ALL", "English_United States.1252")
#Sys.setenv(LANG = "en_US.UTF-8")
#Sys.setlocale("LC_ALL", "C.UTF-8")  
#options(encoding = "UTF-8")
#rsconnect::deployApp()

library(shiny)
library(DT)
library(ggplot2)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(Spectra)
library(MsBackendMgf)
library(MetaboCoreUtils)
library(plotly)
library(enviPat)
library(shinycssloaders)
library(Rdisop)
library(InterpretMSSpectrum)
library(MsCoreUtils)

data(isotopes, package = "enviPat")

ui <- fluidPage(
  useShinyjs(),
  
  tags$head(tags$style(HTML("
    /* 1. Make tabs wrap into several lines */
    .nav-tabs {
      display: flex;
      flex-wrap: wrap;
      border-bottom: none;
    }
    .nav-tabs > li {
      margin-bottom: 5px; /* Space between rows */
    }
    .nav-tabs > li > a {
      font-size: 20px !important; /* Adjusted size for better wrapping */
      font-weight: bold !important;
      padding: 10px 20px !important; /* Smaller padding to fit more per line */
      margin-right: 5px;
      border: 1px solid #ddd !important;
      border-radius: 4px !important;
    }
    .nav-tabs > li.active > a {
      background-color: #0066cc !important;
      color: white !important;
    }
    
    /* Shrink the box to fit the content and reduce padding */
.highlight-mini {
  background-color: #FFFFFF;
  border: 1px solid #ccc;
  color: black;
  padding: 5px 10px;
  border-radius: 6px;
  font-weight: bold;
  display: inline-block; /* This makes the box only as wide as the text */
  margin-top: 5px;
  box-shadow: 1px 1px 3px rgba(0,0,0,0.1);
}

/* Reduce spacing between lines in the mass error panels */
.mass-err-label {
  margin-bottom: 2px;
  font-size: 13px;
  color: #555;
}
    
    /* Footer and other existing styles */
    .app-footer { position: fixed; left:0; right:0; bottom:0; text-align:center; 
                  font-size:12px; opacity:0.75; padding:8px; background: rgba(255,255,255,0.8);
                  border-top: 1px solid #ddd; z-index: 9999; }
    body { padding-bottom: 45px; }
  "))),
  
tags$head(tags$style(HTML("
  .app-footer { position: fixed; left:0; right:0; bottom:0; 
                text-align:center; font-size:12px; opacity:0.75;
                padding:8px; background: rgba(255,255,255,0.8);
                border-top: 1px solid #ddd; z-index: 9999; }
  body { padding-bottom: 45px; }
  
    .progress.shiny-file-input-progress {
      height: 22px !important;        /* Makes the bar thicker */
      margin-top: 10px !important;    /* Adds space above the bar */
      border-radius: 6px !important;  /* Rounded corners */
      background-color: #f5f5f5;      /* Background of the empty bar */
    }
    
    .progress.shiny-file-input-progress .progress-bar {
      line-height: 22px !important;   /* Centers the text vertically */
      font-size: 14px !important;     /* Larger 'Upload complete' text */
      font-weight: bold !important;
      background-color: #0066cc !important; /* Matches your header color */
    }
    /* ---------------------------------------- */
  
"))),

tags$head(
  tags$title("qry4ms"),
  tags$link(rel = "icon", type = "image/png",
            href = "https://raw.githubusercontent.com/plyush1993/qry4ms/main/qry4ms.png")
),

div(
  class = "app-footer",
  HTML('Created by: Ivan Plyushchenko &nbsp;|&nbsp;
       <a href="https://github.com/plyush1993/qry4ms" target="_blank">GitHub repository</a>')
),
  
div(
  style = "
    width: 100%;
    display: flex;
    align-items: center;
    justify-content: center;
    margin-bottom: 20px;
  ",
  
  tags$img(
    src = 'https://raw.githubusercontent.com/plyush1993/qry4ms/main/qry4ms.png',
    height = '150px',
    style = 'margin-right: 20px;'
  ),
  
  div(
    style = '
      font-size: 32px;
      font-weight: 900;
      color: #0066cc;
      text-align: center;
    ',
    "Making query for MS1/MS2 data"
  )
),
  
  tags$head(tags$style(HTML("
    .shiny-output-error-validation {
      color: #000 !important;
      font-size: 18px !important;
      font-weight: 800 !important;
      padding: 12px;
    }
    .highlight {
      background-color: #FFFFFF;
      border: 2px solid black;
      color: black;
      padding: 8px;
      border-radius: 8px;
      font-weight: bold;
    }
  "))),
  
  theme = shinytheme("flatly"), 
  setBackgroundColor(color = c("azure", "azure"), gradient = "linear", direction = "bottom"),
  
  sidebarLayout(
    sidebarPanel(
      h4("General settings"),
      radioButtons("plot_engine", "Plot Style:", 
                   choices = c("Static" = "static", "Interactive" = "plotly"), 
                   inline = TRUE),
      textInput("compound_name", "Compound name / file basename:",
                value = "Compound_X"),
      numericInput("parent_mass", "Parent mass (m/z):",
                   value = 270.144073, min = 0, step = 0.0001),
      numericInput("charge", "Charge:",
                   value = 1, step = 1),
      
      hr(),
      
      h4("MS1 filtering"),
      checkboxInput("filter_ms1",
                    "Enable MS1 filtering (parent mass +/- tolerance)",
                    value = FALSE),

      conditionalPanel(
        "input.filter_ms1 == true",
        numericInput("ms1_tol",
                     "MS1 tolerance around parent mass (+/- Da):",
                     value = 10, min = 0, step = 1)
      ),
      checkboxInput("filter_ms1_pct",
              "Filter MS1 by relative intensity (%)",
              value = FALSE),
     conditionalPanel(
              "input.filter_ms1_pct == true",
           numericInput("ms1_pct",
               "Keep MS1 peaks ≥ this % of max MS1 intensity:",
               value = 1, min = 0, max = 100, step = 1)
         ),
      
      hr(),
      h4("MS1 spectrum input"),
      radioButtons(
        "ms1_input_type", "MS1 input type:",
        choices = c("Paste text" = "paste", "Upload txt/csv/tsv" = "upload"),
        selected = "paste"
      ),
      conditionalPanel(
        "input.ms1_input_type == 'paste'",
        tags$small("Paste two columns: m/z intensity, separated by space or tab, one peak per line."),
        textAreaInput("ms1_text", NULL, rows = 8, placeholder = "e.g.\n270.1440 12345\n269.1402 5678")
      ),
      conditionalPanel(
        "input.ms1_input_type == 'upload'",
        fileInput("ms1_file", "Upload MS1 file:",
                  accept = c(".txt", ".csv", ".tsv"))
      ),
      
      hr(),
      h4("MS2 filtering"),
      checkboxInput("filter_ms2",
                    "Filter MS2 by m/z (<= parent mass + tolerance)",
                    value = FALSE),

      conditionalPanel(
        "input.filter_ms2 == true",
        numericInput("ms2_tol",
                     "MS2 tolerance above parent mass (Da):",
                     value = 10, min = 0, step = 1)
      ),
      
      checkboxInput("filter_ms2_pct",
                    "Filter MS2 by relative intensity (%)",
                    value = FALSE),
      conditionalPanel(
        "input.filter_ms2_pct == true",
        numericInput("ms2_pct",
                     "Keep peaks ≥ this % of max MS2 intensity:",
                     value = 1, min = 0, max = 100, step = 1)
      ),
      
      hr(),
      h4("MS2 spectrum input"),
      radioButtons(
        "ms2_input_type", "MS2 input type:",
        choices = c("Paste text" = "paste", "Upload txt/csv/tsv" = "upload"),
        selected = "paste"
      ),
      conditionalPanel(
        "input.ms2_input_type == 'paste'",
        tags$small("Paste two columns: m/z intensity, separated by space or tab, one peak per line."),
        textAreaInput("ms2_text", NULL, rows = 8, placeholder = "e.g.\n150.0712 5000\n120.0550 3000")
      ),
      conditionalPanel(
        "input.ms2_input_type == 'upload'",
        fileInput("ms2_file", "Upload MS2 file:",
                  accept = c(".txt", ".csv", ".tsv"))
      ),
      
     hr(),
      h4("Reference Spectra"),
      tags$small("Upload reference spectrum as CSV with columns 'mz' and 'intensity'."),
      fileInput("spectra_file", "", accept = ".csv"),
     
      hr(),
      downloadButton("download_ms", "Download .ms file", class = "btn-info"),
     br(), br(),
     downloadButton("download_mgf", "Download .mgf file", class = "btn-success")
    ),
    
        mainPanel(
        tabsetPanel(
        tabPanel("MS1 spectrum",
                 uiOutput("ms1_plot_ui")%>% withSpinner(color="#0066cc"), 
                 br(), br(),  
                 DTOutput("ms1_table")                      
        ),
        tabPanel("MS2 spectrum",
                 uiOutput("ms2_plot_ui")%>% withSpinner(color="#0066cc"), 
                 br(), br(),  
                 DTOutput("ms2_table")
        ),
        tabPanel("Mirror & Similarity",
        br(),
         # Custom CSS to fix the alignment and spacing
         tags$head(
           tags$style(HTML("
             .inline-input .form-group { display: flex; align-items: center; margin-bottom: 5px; }
             .inline-input label { margin-right: 10px; margin-bottom: 0; white-space: nowrap; min-width: 90px; }
             .stats-box { min-width: 120px !important; }
             .sim-container { display: flex; justify-content: space-around; align-items: center; }
           "))
         ),
         wellPanel(
           fluidRow(
             # Column 1: Source and PPM
             column(3, 
                    radioButtons("mirror_source", "Use MS2:", choices = c("Filtered" = "filtered", "Raw" = "raw"), inline = TRUE),
                    numericInput("sim_ppm", "Match Tol (ppm):", value = 10, min = 0)
             ),
             # Column 2: Weights and Stats (Fixed Labels)
             column(4, class = "inline-input",
                    numericInput("sim_m", "m (mz weight):", value = 0, step = 0.1),
                    numericInput("sim_n", "n (int weight):", value = 0.5, step = 0.1),
                    div(style="display:flex; align-items:center;",
                        tags$b("Matched/Total: ", style="margin-right:10px;"),
                        div(class = "highlight-mini stats-box", textOutput("match_stats", inline = TRUE))
                    )
             ),
             actionButton(inputId = "btn", 
                label = "?", 
                class = "btn-primary btn-s"),
          bsTooltip("btn", 
          title = "<b>For Dot Product calculation via <em>MsCoreUtils</em> package</b><br>Most commonly m = 0 and n = 0.5 are used<br>NIST Library Search uses m = 3 and n = 0.6<br>MassBank uses m = 2 and n = 0.5<br>", "right", trigger = "click", options = list(container = "body")
          ),
             # Column 3 & 4: Scores (Closer together)
             column(5,
                    div(class = "sim-container",
                        div(style="text-align:center;",
                            div(tags$b("GNPS Score")),
                            div(class = "highlight-mini", style="border-color: #104E8B; width: 80px;", textOutput("sim_gnps"))
                        ),
                        div(style="text-align:center;",
                            div(tags$b("Dot Product")),
                            div(class = "highlight-mini", style="border-color: #8B1A1A; width: 80px;", textOutput("sim_dot"))
                        )
                    )
             )
           ),
          tags$small("Calculation is based on the MsCoreUtils R package.")
         ),
         uiOutput("mirror_plot_ui") %>% withSpinner(color="#0066cc")
        ),
        tabPanel("Raw .ms", verbatimTextOutput("preview_ms")),
        tabPanel("Mass & Pattern",
                 br(),
                 wellPanel(
                   fluidRow(
                     column(3, textInput("envipat_formula", "Formula:", value = "C15H10O7")),
                     column(3, selectInput("envipat_adduct", "Adduct & Polarity:", 
                                           choices = c("[M+H]+", "M+", "[M+Na]+", "[M+K]+", "M-", "[M-H]-", "[M+Cl]-"), 
                                           selected = "[M+H]+")),
                     column(3, numericInput("envipat_threshold", "Rel. Abundance Threshold (%):", 
                                            value = 0.1, min = 0, step = 0.1)),
                     column(3, actionButton("calc_envipat", "Run", 
                                            class = "btn-info", 
                                            style = "margin-top: 25px; width: 100%; font-weight: bold;"))
                   ),
                   tags$small("Note: Type standard element symbols (e.g., C6H12O6). Case sensitive. Calculation is based on the envipat R package.")
                 ),
                 fluidRow(
                   column(12, 
                     div(style = "display: flex; align-items: center; justify-content: center; margin-bottom: 15px;",
                         tags$b("Monoisotopic m/z: ", style = "margin-right: 10px; font-size: 18px; color: #2c3e50;"),
                         textInput("mono_mass_out", label = NULL, value = "", width = "150px"),
                         actionButton("copy_mass", "Copy", icon = icon("clipboard"), 
                                      style = "margin-left: 10px; margin-bottom: 15px; background-color: #2c3e50; color: white;")
                     )
                   )
                 ),
                 plotlyOutput("envipat_plot", height = "500px") %>% withSpinner(),
                 br(),
                 DTOutput("envipat_table")
        ),
        tabPanel("Adduct Calculator",
         br(),
         wellPanel(
           radioButtons("input_type", "Parent mass is:",
                        choices = c("Neutral mass (M)" = "neutral",
                                    "Adduct (M+H / M-H)" = "adduct"),
                        inline = TRUE),
          helpText("Parent mass is defined in the left sidebar. Calculation is based on the MetaboCoreUtils R package.")
         ),
         fluidRow(
           column(12, 
                  div(style = "display: flex; align-items: center; justify-content: center; margin-bottom: 15px;",
                      tags$b("Calculated Neutral Mass (M): ", style = "margin-right: 10px; font-size: 18px; color: #0066cc;"),
                      textInput("neutral_mass_display", label = NULL, value = "", width = "150px"),
                      actionButton("copy_neutral_mass", "Copy", icon = icon("clipboard"), 
                                   style = "margin-left: 10px; margin-bottom: 15px; background-color: #0066cc; color: white;")
                  )
           )
         ),
         DTOutput("adduct_table") %>% withSpinner()
),

tabPanel("Formula Finder",
         br(),
         wellPanel(
           fluidRow(
             column(3, 
                div(style = "display: flex; align-items: flex-end;",
                  numericInput("rdisop_mass", "Neutral Mass:", value = 180.063388, step = 0.0001))
              ),
             column(3, numericInput("rdisop_ppm", "PPM Tolerance:", value = 5, min = 0.1)),
             column(6, textInput("rdisop_elements_custom", "Allowed Elements (comma separated):", 
                                 value = "C, H, N, O"))
           ),
           tags$small("Calculation is based on the Rdisop R package."),
           br(),
           br(),
           actionButton("run_rdisop", "Generate Formulas", class = "btn-primary", width = "100%")
         ),
         DTOutput("rdisop_table") %>% withSpinner()
),

tabPanel("Interpret MS1",
br(),
         wellPanel(
           fluidRow(
             column(8, 
                    p(tags$b("About:"), "This algorithm evaluates MS1 clusters to propose the most likely neutral mass and identifies adducts/isotopes automatically. Calculation is based on the InterpretMSSpectrum R package."),
             ),
             column(2, 
                    actionButton("run_findmain", "Run", 
                                 class = "btn-warning", style = "margin-top:25px; width:100%; font-weight:bold;"))
           )
         ),
         fluidRow(
           column(12, 
                  h4("Proposed Adducts & Neutral Mass"),
                  plotOutput("fm_plot_main", height = "400px") %>% withSpinner(color="#0066cc")
           )
         ),
         hr(),
         h4("Calculation Summary"),
         DTOutput("fm_summary_table")
),

  tabPanel("Mass Error",
         br(),
         fluidRow(
           # Panel A: Range Calculator
           column(6, 
                  wellPanel(
                    style = "border-left: 5px solid #0066cc; padding: 15px;",
                    h4("1. Tolerance Range Calculator"),
                    fluidRow(
                      column(6, numericInput("range_m", "Target m/z:", value = 270.14407, step = 0.0001)),
                      column(6, numericInput("range_ppm", "Tolerance (ppm):", value = 5, min = 0))
                    ),
                    hr(style = "margin: 10px 0;"),
                    # Tightened outputs
                    div(class = "mass-err-label", tags$b("Lower Bound:")),
                    div(class = "highlight-mini", textOutput("res_lower", inline = TRUE)),
                    
                    div(class = "mass-err-label", style="margin-top:10px;", tags$b("Upper Bound:")),
                    div(class = "highlight-mini", textOutput("res_upper", inline = TRUE)),
                    
                    div(class = "mass-err-label", style="margin-top:10px;", tags$b("Total Delta (Da):")),
                    div(class = "highlight-mini", style="border-color: #0066cc;", textOutput("res_delta", inline = TRUE))
                  )
           ),
           # Panel B: PPM Error Calculator
           column(6, 
                  wellPanel(
                    style = "border-left: 5px solid #e74c3c; padding: 15px;",
                    h4("2. Specific PPM Error"),
                    numericInput("obs_mass", "Observed m/z:", value = 270.14407, step = 0.0001),
                    numericInput("theo_mass", "Theoretical m/z:", value = 270.14352, step = 0.0001),
                    hr(style = "margin: 10px 0;"),
                    div(style = "text-align: center;",
                        div(class = "mass-err-label", tags$b("Calculated Error:")),
                        div(class = "highlight-mini", 
                            style = "border: 2px solid #e74c3c; font-size: 18px;", 
                            textOutput("res_ppm_err", inline = TRUE)
                        )
                    )
                  )
           )
         )
)


      )
    )
  )
)

server <- function(input, output, session) {
  
  parse_spectrum <- function(input_type, text_value, file_input, label = "MS") {

    if (input_type == "paste") {
      validate(
        need(
          !is.null(text_value) && nzchar(trimws(text_value)),
          paste0("No ", label, " spectrum provided. Please paste m/z and intensity values.")
        )
      )
      con <- textConnection(text_value)
      on.exit(close(con), add = TRUE)
      df <- tryCatch(
        read.table(con, header = FALSE),
        error = function(e) NULL
      )
    } else {
      validate(
        need(
          !is.null(file_input),
          paste0("No ", label, " file uploaded. Please upload a txt/csv/tsv file.")
        )
      )
      df <- tryCatch(
        read.table(file_input$datapath, header = FALSE),
        error = function(e) NULL
      )
    }
    
    validate(
      need(!is.null(df), paste0("Unable to read ", label, " spectrum. Check file format.")),
      need(ncol(df) >= 2, paste0(label, " spectrum must have at least 2 columns: m/z and intensity."))
    )
    
    df <- df[, 1:2, drop = FALSE]
    colnames(df) <- c("mz", "intensity")
    
    validate(
      need(nrow(df) > 0, paste0(label, " spectrum is empty. Check input or file."))
    )
    
    df
  }

  ms1_data <- reactive({
    parse_spectrum(input$ms1_input_type, input$ms1_text, input$ms1_file, label = "MS1")
  })
  
  ms2_data <- reactive({
    parse_spectrum(input$ms2_input_type, input$ms2_text, input$ms2_file, label = "MS2")
  })
  
ms1_filtered <- reactive({
  df <- ms1_data()
  
  if (isTRUE(input$filter_ms1)) {
    req(input$parent_mass)
    
    tol <- input$ms1_tol
    if (is.null(tol) || is.na(tol) || tol < 0) tol <- 0
    
    center <- input$parent_mass
    lower  <- max(0, center - tol)
    upper  <- center + tol
    
    df <- df[df$mz >= lower & df$mz <= upper, , drop = FALSE]
  }
  
  if (isTRUE(input$filter_ms1_pct) && nrow(df) > 0) {
    pct <- input$ms1_pct
    if (is.null(pct) || is.na(pct) || pct < 0) pct <- 0
    if (pct > 100) pct <- 100
    
    max_int <- max(df$intensity, na.rm = TRUE)
    thr <- max_int * pct / 100
    
    df <- df[df$intensity >= thr, , drop = FALSE]
  }
  
  validate(
    need(
      nrow(df) > 0,
      "MS1 filtering removed all peaks. Try relaxing the m/z tolerance or % intensity threshold, or disable MS1 filters."
    )
  )
  
  df
})
  
  ms2_filtered <- reactive({
    df <- ms2_data()
    
    if (isTRUE(input$filter_ms2)) {
      req(input$parent_mass)
      
      tol <- input$ms2_tol
      if (is.null(tol) || is.na(tol) || tol < 0) tol <- 0
      
      max_mz <- input$parent_mass + tol
      df <- df[df$mz <= max_mz, , drop = FALSE]
    }
    
    if (isTRUE(input$filter_ms2_pct) && nrow(df) > 0) {
      pct <- input$ms2_pct
      if (is.null(pct) || is.na(pct) || pct < 0) pct <- 0
      if (pct > 100) pct <- 100
      
      max_int <- max(df$intensity, na.rm = TRUE)
      thr <- max_int * pct / 100
      
      df <- df[df$intensity >= thr, , drop = FALSE]
    }
    
    validate(
      need(
        nrow(df) > 0,
        "MS2 filtering removed all peaks. Try lowering the % threshold or tolerance, or disable MS2 filters."
      )
    )
    
    df
  })

    lib_ms2_data <- reactive({
    validate(
      need(input$spectra_file,
           "Upload a reference MS2 CSV (with columns 'mz' and 'intensity') in the sidebar.")
    )
    
    df <- tryCatch(
      read.csv(input$spectra_file$datapath, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
    
    validate(
      need(!is.null(df), "Unable to read reference MS2 CSV. Check the file format."),
      need(all(c("mz", "intensity") %in% names(df)),
           "Reference CSV must contain columns named 'mz' and 'intensity'.")
    )
    
    df <- df[, c("mz", "intensity"), drop = FALSE]
    df$mz        <- suppressWarnings(as.numeric(df$mz))
    df$intensity <- suppressWarnings(as.numeric(df$intensity))
    
    df <- df[is.finite(df$mz) & is.finite(df$intensity), , drop = FALSE]
    validate(
      need(nrow(df) > 0, "Reference MS2 spectrum is empty after cleaning. Check the CSV.")
    )
    
    df
  })

  mirror_data_prep <- reactive({
    df_s <- ms2_for_mirror()
    validate(need(nrow(df_s) > 0, "MS2 spectrum has no peaks to show in mirror plot."))
    df_ref <- lib_ms2_data()
    
    max_s   <- max(df_s$intensity,   na.rm = TRUE)
    max_ref <- max(df_ref$intensity, na.rm = TRUE)
    validate(
      need(is.finite(max_s)   && max_s   > 0, "MS2 intensities are zero or NA."),
      need(is.finite(max_ref) && max_ref > 0, "Reference MS2 intensities are zero or NA.")
    )
    
    df_s$rel   <- 100 * df_s$intensity   / max_s
    df_ref$rel <- 100 * df_ref$intensity / max_ref
    df_ref$rel <- -df_ref$rel
    
    df_s$type   <- "Sample"
    df_ref$type <- "Reference"
    
    rbind(df_s[, c("mz", "intensity", "rel", "type")], 
          df_ref[, c("mz", "intensity", "rel", "type")])
  })
    
  output$mirror_plot <- renderPlot({
  df_s <- ms2_for_mirror()
  validate(
    need(nrow(df_s) > 0, "MS2 spectrum has no peaks to show in mirror plot.")
  )
  
  df_s$mz        <- suppressWarnings(as.numeric(df_s$mz))
  df_s$intensity <- suppressWarnings(as.numeric(df_s$intensity))
  df_s <- df_s[is.finite(df_s$mz) & is.finite(df_s$intensity), , drop = FALSE]
  validate(
    need(nrow(df_s) > 0, "MS2 spectrum is empty after cleaning (NA / non-numeric values removed).")
  )
  
  df_ref <- lib_ms2_data()
  
  max_s   <- max(df_s$intensity,   na.rm = TRUE)
  max_ref <- max(df_ref$intensity, na.rm = TRUE)
  
  validate(
    need(is.finite(max_s)   && max_s   > 0, "MS2 intensities are zero or NA."),
    need(is.finite(max_ref) && max_ref > 0, "Reference MS2 intensities are zero or NA.")
  )
  
  df_s$rel   <- 100 * df_s$intensity   / max_s
  df_ref$rel <- 100 * df_ref$intensity / max_ref
  
  df_ref$rel <- -df_ref$rel
  
  df_s$type   <- "Sample"
  df_ref$type <- "Reference"
  
  df_all <- rbind(df_s, df_ref)
  
  p <- ggplot(df_all, aes(x = mz, xend = mz, y = 0, yend = rel, color = type)) +
    geom_segment(size = 0.8) +
    scale_color_manual(
      name   = "Spectrum",
      values = c(
        "Sample"   = "#104E8B",
        "Reference" = "#8B1A1A"
      )
    ) +
    scale_y_continuous(
      "Relative intensity (%)",
      labels = function(x) abs(x)
    ) +
    labs(
      x = "m/z",
      title = "Mirror spectrum"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "bottom")
  
  p
})

  ms_file_text <- reactive({
    df1 <- ms1_filtered()
    df2 <- ms2_filtered()
    
    validate(
      need(nrow(df1) > 0, "Cannot generate .ms file: MS1 spectrum is empty."),
      need(nrow(df2) > 0, "Cannot generate .ms file: MS2 spectrum is empty.")
    )
    
    req(input$compound_name, input$parent_mass, input$charge)
    
    ms1_lines <- apply(df1, 1, function(r) paste(r[1], r[2]))
    ms2_lines <- apply(df2, 1, function(r) paste(r[1], r[2]))
    
    lines <- c(
      paste0(">compound ", input$compound_name),
      paste0(">parentmass ", input$parent_mass),
      paste0(">charge ", input$charge),
      ">ms1",
      ms1_lines,
      ">ms2",
      ms2_lines
    )
    paste(lines, collapse = "\n")
  })
  
  # --- UI RENDERING SWITCHES ---
  output$ms1_plot_ui <- renderUI({
    if (input$plot_engine == "plotly") plotlyOutput("ms1_plot_plotly", height = "300px")
    else plotOutput("ms1_plot_static", height = "300px")
  })
  
  output$ms2_plot_ui <- renderUI({
    if (input$plot_engine == "plotly") plotlyOutput("ms2_plot_plotly", height = "300px")
    else plotOutput("ms2_plot_static", height = "300px")
  })
  
  output$mirror_plot_ui <- renderUI({
    if (input$plot_engine == "plotly") plotlyOutput("mirror_plot_plotly", height = "600px")
    else plotOutput("mirror_plot_static", height = "600px")
  })

  # --- MS1 PLOTS ---
  output$ms1_plot_static <- renderPlot({
    df <- ms1_filtered()
    validate(need(nrow(df) > 0, "No MS1 peaks to plot."))
    ggplot(df, aes(x = mz, y = intensity)) +
      geom_segment(aes(xend = mz, y = 0, yend = intensity)) +
      labs(x = "m/z", y = "Intensity", title = "MS1 spectrum") +
      theme_minimal(base_size = 16)
  })
  
  output$ms1_plot_plotly <- renderPlotly({
    df <- ms1_filtered()
    validate(need(nrow(df) > 0, "No MS1 peaks to plot."))
    
    df$mz <- as.numeric(df$mz)
    df$intensity <- as.numeric(df$intensity)
    
    df$hover_txt <- paste0("<b>m/z:</b> ", round(df$mz, 4), "<br><b>Int:</b> ", round(df$intensity, 1))
    
    plot_ly(df) %>%
      add_segments(x = ~mz, xend = ~mz, y = 0, yend = ~intensity, line = list(color = "#2c3e50"), hoverinfo="none") %>%
      add_markers(x = ~mz, y = ~intensity, text = ~hover_txt, hoverinfo = "text", marker = list(color = "#e74c3c", size = 4)) %>%
      layout(title = "MS1 spectrum", xaxis = list(title = "m/z", tickformat=".4f"), yaxis = list(title = "Intensity"), showlegend = FALSE, hovermode = "closest")
  })

  # --- MS2 PLOTS ---
  output$ms2_plot_static <- renderPlot({
    df <- ms2_filtered()
    validate(need(nrow(df) > 0, "No MS2 peaks to plot."))
    ggplot(df, aes(x = mz, y = intensity)) +
      geom_segment(aes(xend = mz, y = 0, yend = intensity)) +
      labs(x = "m/z", y = "Intensity", title = "MS2 spectrum") +
      theme_minimal(base_size = 16)
  })
  
  output$ms2_plot_plotly <- renderPlotly({
    df <- ms2_filtered()
    validate(need(nrow(df) > 0, "No MS2 peaks to plot."))
    
    df$mz <- as.numeric(df$mz)
    df$intensity <- as.numeric(df$intensity)
    
    df$hover_txt <- paste0("<b>m/z:</b> ", round(df$mz, 4), "<br><b>Int:</b> ", round(df$intensity, 1))
    
    plot_ly(df) %>%
      add_segments(x = ~mz, xend = ~mz, y = 0, yend = ~intensity, line = list(color = "#2c3e50"), hoverinfo="none") %>%
      add_markers(x = ~mz, y = ~intensity, text = ~hover_txt, hoverinfo = "text", marker = list(color = "#e74c3c", size = 4)) %>%
      layout(title = "MS2 spectrum", xaxis = list(title = "m/z", tickformat=".4f"), yaxis = list(title = "Intensity"), showlegend = FALSE, hovermode = "closest")
  })

  # --- MIRROR PLOT PREPARATION (Shared between static and plotly) ---
  ms2_for_mirror <- reactive({
    if (is.null(input$mirror_source) || input$mirror_source == "filtered") {
      ms2_filtered()
    } else {
      ms2_data()
    }
  })
  
  mirror_data_prep <- reactive({
    df_s <- ms2_for_mirror()
    validate(need(nrow(df_s) > 0, "MS2 spectrum has no peaks to show in mirror plot."))
    df_ref <- lib_ms2_data()
    
    max_s   <- max(df_s$intensity,   na.rm = TRUE)
    max_ref <- max(df_ref$intensity, na.rm = TRUE)
    validate(
      need(is.finite(max_s)   && max_s   > 0, "MS2 intensities are zero or NA."),
      need(is.finite(max_ref) && max_ref > 0, "Reference MS2 intensities are zero or NA.")
    )
    
    df_s$rel   <- 100 * df_s$intensity   / max_s
    df_ref$rel <- 100 * df_ref$intensity / max_ref
    df_ref$rel <- -df_ref$rel
    
    df_s$type   <- "Sample"
    df_ref$type <- "Reference"
    
    # If reference lacks rt, make sure it binds cleanly
    if(!"rt" %in% colnames(df_ref)) df_ref$rt <- NA
    if(!"rt" %in% colnames(df_s)) df_s$rt <- NA
    
    rbind(df_s[, c("mz", "intensity", "rt", "rel", "type")], 
          df_ref[, c("mz", "intensity", "rt", "rel", "type")])
  })

  # --- MIRROR PLOTS ---
  output$mirror_plot_static <- renderPlot({
    df_all <- mirror_data_prep()
    ggplot(df_all, aes(x = mz, xend = mz, y = 0, yend = rel, color = type)) +
      geom_segment(size = 0.8) +
      scale_color_manual(name = "Spectrum", values = c("Sample" = "#104E8B", "Reference" = "#8B1A1A")) +
      scale_y_continuous("Relative intensity (%)", labels = abs) +
      labs(x = "m/z", title = "Mirror spectrum") +
      theme_minimal(base_size = 16) + theme(legend.position = "bottom")
  })
  
  output$mirror_plot_plotly <- renderPlotly({
    df_all <- mirror_data_prep()
    
    df_all$mz <- as.numeric(df_all$mz)
    df_all$intensity <- as.numeric(df_all$intensity)
    df_all$rel <- as.numeric(df_all$rel)
    
    df_s <- df_all[df_all$type == "Sample", ]
    df_ref <- df_all[df_all$type == "Reference", ]
    
    df_s$hover_txt <- paste0("<b>m/z:</b> ", round(df_s$mz, 4), 
                             "<br><b>Rel Int:</b> ", round(df_s$rel, 1), "%",
                             "<br><b>Abs Int:</b> ", round(df_s$intensity, 1))
                             
    df_ref$hover_txt <- paste0("<b>m/z:</b> ", round(df_ref$mz, 4), 
                               "<br><b>Rel Int:</b> ", round(abs(df_ref$rel), 1), "%",
                               "<br><b>Abs Int:</b> ", round(df_ref$intensity, 1))
    
    plot_ly() %>%
      add_segments(data = df_s, x = ~mz, xend = ~mz, y = 0, yend = ~rel, line = list(color = "#104E8B"), hoverinfo="none", name="Sample") %>%
      add_markers(data = df_s, x = ~mz, y = ~rel, marker = list(color = "#104E8B", size = 4),
                  text = ~hover_txt, hoverinfo = "text", name = "Sample") %>%
      add_segments(data = df_ref, x = ~mz, xend = ~mz, y = 0, yend = ~rel, line = list(color = "#8B1A1A"), hoverinfo="none", name="Reference") %>%
      add_markers(data = df_ref, x = ~mz, y = ~rel, marker = list(color = "#8B1A1A", size = 4),
                  text = ~hover_txt, hoverinfo = "text", name = "Reference") %>%
      layout(title = "Mirror spectrum", xaxis = list(title = "m/z"), 
             yaxis = list(title = "Relative intensity (%)", tickmode = "array", tickvals = seq(-100, 100, 25), ticktext = abs(seq(-100, 100, 25))),
             hovermode = "closest")
  })
  
  output$ms1_table <- renderDT({
    df <- ms1_filtered()
    validate(
      need(nrow(df) > 0, "No MS1 peaks to show in table.")
    )
    
    datatable(
      df,
      extensions = c("Buttons"),
      rownames  = FALSE,
      options   = list(
        dom = "Blfrtip",
        pageLength = 10,
        lengthMenu = list(
          c(10, 50, 100, -1),
          c("10", "50", "100", "All")
        ),
        buttons = list(
          list(
            extend = "copy",
            text   = "Copy All",
            title  = NULL,
            header = FALSE,
            exportOptions = list(
              modifier = list(page = "all")
            )
          ),
          list(
            extend = "csvHtml5",
            text   = "Download CSV",
            filename = JS(
              "function() {
                 var base = $('#compound_name').val() || 'compound';
                 return base + '_MS1';
               }"
            ),
            exportOptions = list(
              modifier = list(page = "all")
            )
          )
        )
      )
    )
  }, server = FALSE)
  
  output$ms2_table <- renderDT({
    df <- ms2_filtered()
    validate(
      need(nrow(df) > 0, "No MS2 peaks to show in table.")
    )
    
    datatable(
      df,
      extensions = c("Buttons"),
      rownames  = FALSE,
      options   = list(
        dom = "Blfrtip",
        pageLength = 10,
        lengthMenu = list(
          c(10, 50, 100, -1),
          c("10", "50", "100", "All")
        ),
        buttons = list(
          list(
            extend = "copy",
            text   = "Copy All",
            title  = NULL,
            header = FALSE,
            exportOptions = list(
              modifier = list(page = "all")
            )
          ),
          list(
            extend = "csvHtml5",
            text   = "Download CSV",
            filename = JS(
              "function() {
                 var base = $('#compound_name').val() || 'compound';
                 return base + '_MS2';
               }"
            ),
            exportOptions = list(
              modifier = list(page = "all")
            )
          )
        )
      )
    )
  }, server = FALSE)
  
  output$preview_ms <- renderText({
    ms_file_text()
  })
  
  output$download_ms <- downloadHandler(
    filename = function() {
      nm <- input$compound_name
      if (is.null(nm) || !nzchar(nm)) nm <- "compound"
      paste0(nm, ".ms")
    },
    content = function(file) {
      txt <- ms_file_text()
      writeLines(txt, file)
    }
  )
  
 output$download_mgf <- downloadHandler(
    filename = function() {
      nm <- input$compound_name
      if (is.null(nm) || !nzchar(nm)) nm <- "compound"
      paste0(nm, ".mgf")
    },
    content = function(file) {
      df1 <- ms1_filtered()
      df2 <- ms2_filtered()
      
      if (!is.data.frame(df1) || nrow(df1) == 0) {
        stop("Cannot generate MGF: MS1 spectrum is empty.")
      }
      if (!is.data.frame(df2) || nrow(df2) == 0) {
        stop("Cannot generate MGF: MS2 spectrum is empty.")
      }
      
      sp_df <- S4Vectors::DataFrame(
        msLevel         = c(1L, 2L),
        rtime           = c(NA_real_, NA_real_),
        precursorMz     = c(as.numeric(input$parent_mass), as.numeric(input$parent_mass)),
        precursorCharge = c(as.integer(input$charge), as.integer(input$charge)),
        TITLE           = c(paste0(input$compound_name, "_MS1"), paste0(input$compound_name, "_MS2")),
        FEATURE_ID      = c(input$compound_name, input$compound_name) 
      )
      
      sp_df$mz <- list(df1$mz, df2$mz)
      sp_df$intensity <- list(df1$intensity, df2$intensity)
      
      sps <- Spectra::Spectra(sp_df)
      
      Spectra::export(
        sps,
        MsBackendMgf::MsBackendMgf(),
        file = file
      )
    }
  )
  
  observe({
    ms1_ok <- tryCatch({
      df1 <- ms1_filtered()
      nrow(df1) > 0
    }, error = function(e) {
      FALSE
    })
    
    ms2_ok <- tryCatch({
      df2 <- ms2_filtered()
      nrow(df2) > 0
    }, error = function(e) {
      FALSE
    })
    
    ms_ok <- tryCatch({
      ms_file_text()
      TRUE
    }, error = function(e) {
      FALSE
    })
    
    valid_ms <- ms1_ok && ms2_ok && ms_ok
    valid_mgf <- ms1_ok && ms2_ok
    
    if (valid_ms) {
      shinyjs::enable("download_ms")
    } else {
      shinyjs::disable("download_ms")
    }
    
    if (valid_mgf) {
      shinyjs::enable("download_mgf")
    } else {
      shinyjs::disable("download_mgf")
    }
    
  })
  
  # envipat
  envipat_results <- reactive({
  req(input$envipat_formula, input$envipat_adduct)
  
  base_formula <- trimws(input$envipat_formula)
  adduct <- input$envipat_adduct
  threshold <- input$envipat_threshold

  validate(need(nzchar(base_formula), "Please enter a valid chemical formula."))
  checked_base <- enviPat::check_chemform(isotopes, base_formula)
  validate(
    need(checked_base$warning == FALSE, 
         paste("Invalid chemical formula:", checked_base$new_formula))
  )
  
  if (adduct == "[M+H]+") {
    form_adj <- enviPat::mergeform(base_formula, "H1"); charge <- 1
  } else if (adduct == "[M+Na]+") {
    form_adj <- enviPat::mergeform(base_formula, "Na1"); charge <- 1
  } else if (adduct == "[M+K]+") {
    form_adj <- enviPat::mergeform(base_formula, "K1"); charge <- 1
  } else if (adduct == "M+") {
    form_adj <- base_formula; charge <- 1
  } else if (adduct == "[M-H]-") {
    validate(need(grepl("H", base_formula), "No Hydrogen to lose!")); form_adj <- enviPat::subform(base_formula, "H1"); charge <- -1
  } else if (adduct == "[M+Cl]-") {
    form_adj <- enviPat::mergeform(base_formula, "Cl1"); charge <- -1
  } else if (adduct == "M-") {
    form_adj <- base_formula; charge <- -1
  } else { stop("Adduct not supported.") }
  
  checked_form <- enviPat::check_chemform(isotopes, form_adj)
  validate(need(!checked_form$warning, "Invalid chemical formula."))
  
  pattern <- enviPat::isopattern(isotopes, checked_form$new_formula, threshold = threshold, plotit = FALSE, verbose = FALSE)
  iso_df <- as.data.frame(pattern[[1]])
  
  # --- ELECTRON & ADDUCT CORRECTION ---
  mass_electron <- 0.0005485799
  iso_df$mz_adduct <- (iso_df$`m/z` - (charge * mass_electron)) / abs(charge)
  iso_df$abundance_pct <- iso_df$abundance
  
  # Return everything needed for plot, table, and UI update
  list(df = iso_df, mono = checked_form$monoisotopic_mass, base = base_formula, add = adduct)
}) %>% bindEvent(input$calc_envipat)
  
  output$envipat_plot <- renderPlotly({
  res <- envipat_results()
  updateTextInput(session, "mono_mass_out", value = sprintf("%.6f", res$mono))
  
  plot_ly(res$df) %>%
    add_segments(
      x = ~mz_adduct, xend = ~mz_adduct,
      y = 0, yend = ~abundance_pct,
      line = list(color = "#2c3e50", width = 2),
      showlegend = FALSE, hoverinfo = "none"
    ) %>%
    add_markers(
      x = ~mz_adduct, y = ~abundance_pct,
      marker = list(color = "#e74c3c", size = 8),
      text = ~paste0("<b>m/z:</b> ", round(mz_adduct, 5), "<br>",
                     "<b>Rel. Abundance:</b> ", round(abundance_pct, 2), "%"),
      hoverinfo = "text", name = "Isotopologue"
    ) %>%
    layout(
      title = list(text = paste0("<b>Theoretical Isotopic Pattern</b><br>",
                                 "Formula: ", res$base, " | Adduct: ", res$add, "<br>",
                                 "Monoisotopic m/z: ", round(res$mono, 5)), font = list(size = 14)),
      xaxis = list(title = "<b>m/z</b>", zeroline = FALSE, tickformat = ".4f"),
      yaxis = list(title = "<b>Relative Abundance (%)</b>", range = c(0, 105), zeroline = TRUE),
      hovermode = "closest", plot_bgcolor = "white", paper_bgcolor = "white"
    )
})
  
  output$envipat_table <- renderDT({
  datatable(envipat_results()$df, 
            extensions = 'Buttons',
            rownames = FALSE,
            options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'))) %>%
    formatRound(columns = names(envipat_results()$df), digits = 5)
})
  
  observeEvent(input$copy_mass, {
    req(input$mono_mass_out) # Make sure there is actually a number to copy
    
    # Run a quick JavaScript command to select and copy the text
    shinyjs::runjs("
      var copyText = document.getElementById('mono_mass_out');
      copyText.select();
      document.execCommand('copy');
    ")
    
    # Pop up a temporary success message in the bottom right corner!
    showNotification("Monoisotopic mass copied to clipboard!", type = "message", duration = 3)
  })
  
  # Reactive to calculate the neutral mass (M)
  # 1. Update the reactive to also update the UI display
  neutral_mass <- reactive({
    req(input$parent_mass, input$charge)
    
    m_val <- if (input$input_type == "neutral") {
      as.numeric(input$parent_mass)
    } else {
      # Back-calculate M from the assumed [M+H]+ or [M-H]-
      add_type <- if (input$charge > 0) "[M+H]+" else "[M-H]-"
      as.numeric(MetaboCoreUtils::mz2mass(input$parent_mass, add_type))
    }
    
    # Update the text input field automatically
    updateTextInput(session, "neutral_mass_display", value = sprintf("%.6f", m_val))
    
    return(m_val)
  })

  # 2. Add the Copy Logic for the Adduct Tab
  observeEvent(input$copy_neutral_mass, {
    req(input$neutral_mass_display)
    
    # Use shinyjs to select and copy
    shinyjs::runjs("
      var copyText = document.getElementById('neutral_mass_display');
      copyText.select();
      document.execCommand('copy');
    ")
    
    showNotification("Neutral mass copied to clipboard!", type = "message", duration = 3)
  })

  # Generate the Adduct Table with custom sorting
  output$adduct_table <- renderDT({
    req(input$parent_mass, input$charge)
    
    m <- neutral_mass()
    pol <- if (input$charge > 0) "positive" else "negative"
    
    # 1. Get all adduct names
    adds <- MetaboCoreUtils::adductNames(polarity = pol)
    
    # 2. Calculate m/z values (returns a matrix)
    res_matrix <- MetaboCoreUtils::mass2mz(m, adduct = adds)
    
    # 3. Parse adduct properties safely
    # Extract 'n' (number of Ms): look for digit immediately after '['
    mult_raw <- gsub("^\\[(\\d+)?M.*", "\\1", adds)
    mult <- as.numeric(mult_raw)
    mult[mult_raw == "" | is.na(mult)] <- 1  # Default to 1M if empty or NA
    
    # Extract charge 'z': look for digits before the final + or -
    # We use a capture group that handles both [M+H]+ (empty) and [M+2H]2+ (2)
    z_raw <- gsub("^.*\\](\\d+)?[+-]$", "\\1", adds)
    z_val <- as.numeric(z_raw)
    z_val[z_raw == "" | is.na(z_val)] <- 1 # Default to charge 1 if empty or NA
    
    # 4. Build the data frame
    df_adducts <- data.frame(
      Adduct = adds,
      mz = as.numeric(res_matrix[1, ]),
      z = z_val,
      n = mult,
      stringsAsFactors = FALSE
    )
    
    # 5. Advanced Sorting:
    # First by Number of Ms (n), then by Charge (z), then by m/z
    df_adducts <- df_adducts[order(df_adducts$n, df_adducts$z, df_adducts$mz), ]
    
    datatable(
    df_adducts,
    extensions = 'Buttons', # Added
    colnames = c("Adduct Type", "Calculated m/z", "Charge (z)", "Molecules (n)"),
    rownames = FALSE,
    options = list(
      dom = 'Bfrtip',        # Changed from '<"top"f>rt<"bottom"lp>'
      buttons = c('copy', 'csv', 'excel'), # Added
      pageLength = 20, 
      columnDefs = list(list(className = 'dt-center', targets = 1:2))
    ),
    # ... (keep your caption) ...
  ) %>% formatRound(columns = "mz", digits = 6)
    
  })
  
  # --- Rdisop: Formula Finder Logic ---

rdisop_results <- eventReactive(input$run_rdisop, {
  req(input$rdisop_mass, input$rdisop_elements_custom)
  
  # Parse the comma-separated string into a clean vector
  elem_vec <- strsplit(input$rdisop_elements_custom, ",")[[1]]
  elem_vec <- trimws(elem_vec) # Remove extra spaces
  elem_vec <- elem_vec[elem_vec != ""] # Remove empty strings
  
  # Initialize elements dynamically
  elem_list <- tryCatch({
    initializeElements(elem_vec)
  }, error = function(e) {
    showNotification("Invalid element symbol detected. Please use standard symbols (e.g., Cl, Fe, Br).", type = "error")
    return(NULL)
  })
  
  req(elem_list)
  
  # Decompose mass
  res <- decomposeMass(input$rdisop_mass, ppm = input$rdisop_ppm, elements = elem_list)
  
  validate(need(!is.null(res$formula), "No formulas found within this tolerance and element set."))
  
  # Build and sort Table
  df <- data.frame(
    Formula   = res$formula,
    ExactMass = res$exactmass,
    Error_PPM = abs((res$exactmass - input$rdisop_mass) / input$rdisop_mass) * 1e6,
    stringsAsFactors = FALSE
  )
  df[order(df$Error_PPM), ]
})

output$rdisop_table <- renderDT({
  datatable(rdisop_results(), 
            extensions = 'Buttons', # Added
            rownames = FALSE, 
            options = list(
              dom = 'Bfrtip',        # Added (B = Buttons, f = Search)
              buttons = c('copy', 'csv', 'excel'), # Added
              pageLength = 10, scrollX = TRUE)) %>%
    formatRound(columns = c("ExactMass", "Error_PPM"), digits = 5)
})

# 1. Execute findMAIN algorithm on button click
  fm_results <- eventReactive(input$run_findmain, {
    df <- ms1_filtered()
    validate(need(nrow(df) > 0, "No MS1 data found. Please paste or upload MS1 peaks first."))
    
    # findMAIN requires a two-column matrix (mz, intensity)
    spec_matrix <- as.matrix(df[, c("mz", "intensity")])
    
    # Run the core algorithm from InterpretMSSpectrum
    fmr <- InterpretMSSpectrum::findMAIN(spec_matrix)
    return(fmr)
  })

  # 2. Render the findMAIN diagnostic plot
  output$fm_plot_main <- renderPlot({
    req(fm_results())
    # The plot method for findMAIN objects shows the mass clusters and proposed M+H / Adducts
    plot(fm_results())
  })

  # 3. Render the summary table (Neutral Mass, Score, Adducts)
  output$fm_summary_table <- renderDT({
    req(fm_results())
    
    # summary() on a findMAIN object returns a data frame of the top hypotheses
    summ <- as.data.frame(print(fm_results()))
    
    if ("intensity" %in% colnames(summ)) {
      summ <- summ[order(summ$intensity, decreasing = TRUE), ]
    }
    
    datatable(
    summ, 
    extensions = 'Buttons', # Added
    rownames = FALSE,
    options = list(
      dom = 'Bfrtip',        # Added
      buttons = c('copy', 'csv', 'excel'), # Added
      scrollX = TRUE, 
      pageLength = 10,
      columnDefs = list(list(className = 'dt-center', targets = "_all"))
    )
  )
  })
  
  output$res_lower <- renderText({
    req(input$range_m, input$range_ppm)
    sprintf("%.6f", input$range_m * (1 - input$range_ppm / 1e6))
  })
  
  output$res_upper <- renderText({
    req(input$range_m, input$range_ppm)
    sprintf("%.6f", input$range_m * (1 + input$range_ppm / 1e6))
  })
  
  output$res_delta <- renderText({
    req(input$range_m, input$range_ppm)
    sprintf("%.6f", (input$range_m * (input$range_ppm / 1e6)) * 2)
  })
  
  # 2. Specific PPM Error Calculation
  output$res_ppm_err <- renderText({
    req(input$obs_mass, input$theo_mass)
    err <- ((input$obs_mass - input$theo_mass) / input$theo_mass) * 1e6
    paste0(round(err, 3), " ppm")
  })
  
  # Sync for Formula Finder
observeEvent(input$sync_rdisop, {
  updateNumericInput(session, "rdisop_mass", value = input$parent_mass)
})

observe({
  updateNumericInput(session, "rdisop_mass", value = input$parent_mass)
  updateNumericInput(session, "range_m", value = input$parent_mass)
})

# --- Similarity Calculations ---
  
  output$sim_gnps <- renderText({
    req(ms2_for_mirror(), lib_ms2_data(), input$sim_ppm)
    
    x <- as.matrix(ms2_for_mirror()[, c("mz", "intensity")])
    y <- as.matrix(lib_ms2_data()[, c("mz", "intensity")])
    
    # Direct mapping for GNPS as requested
    map <- MsCoreUtils::join_gnps(x[, 1], y[, 1], tolerance = 0, ppm = input$sim_ppm)
    
    if (length(map[[1]]) == 0) return("0.000")
    
    # Calculate GNPS only on matched peaks
    score <- MsCoreUtils::gnps(x[map[[1]], , drop=FALSE], y[map[[2]], , drop=FALSE])
    
    sprintf("%.3f", if(is.na(score)) 0 else score)
  })
  
  output$sim_dot <- renderText({
    req(ms2_for_mirror(), lib_ms2_data(), input$sim_m, input$sim_n, input$sim_ppm)
    
    x <- as.matrix(ms2_for_mirror()[, c("mz", "intensity")])
    y <- as.matrix(lib_ms2_data()[, c("mz", "intensity")])
    
    # Alignment required for dot product to avoid "length" warning
    joined <- MsCoreUtils::join(x[, 1], y[, 1], tolerance = 0, ppm = input$sim_ppm, type = "outer")
    
    # Reconstruct vectors (filling missing intensities with 0)
    x_int <- x[joined$x, 2]; x_int[is.na(x_int)] <- 0
    y_int <- y[joined$y, 2]; y_int[is.na(y_int)] <- 0
    mz_aligned <- rowMeans(cbind(x[joined$x, 1], y[joined$y, 1]), na.rm = TRUE)
    
    score <- MsCoreUtils::ndotproduct(cbind(mz_aligned, x_int), 
                                      cbind(mz_aligned, y_int), 
                                      m = input$sim_m, n = input$sim_n)
    
    sprintf("%.3f", if(is.na(score)) 0 else score)
  })

  # --- Match Stats (using closest) ---
  output$match_stats <- renderText({
    req(ms2_for_mirror(), lib_ms2_data(), input$sim_ppm)
    
    samp_mz <- ms2_for_mirror()$mz
    lib_mz <- lib_ms2_data()$mz
    
    # Find the index of the closest library peak for each sample peak
    # tolerance = 0 because we are using ppm exclusively
    closest_idx <- MsCoreUtils::closest(samp_mz, lib_mz, tolerance = 0, ppm = input$sim_ppm)
    
    # Count how many sample peaks successfully found a match (!is.na)
    # We use unique() on the matches if you want to count how many LIB peaks were hit
    matched <- sum(!is.na(closest_idx))
    total <- length(lib_mz) 
    
    paste0(matched, " / ", total)
  })

  
}

shinyApp(ui, server)