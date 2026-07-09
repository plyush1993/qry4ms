#' @import shiny
#' @import shinythemes
#' @import shinyjs
#' @import ggplot2
#' @import DT
#' @import shinyWidgets
#' @import shinyBS
#' @import shinycssloaders
#' @import plotly
app_ui <- function() {
fluidPage(
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
            href = "www/qry4ms.png")
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
    src = 'www/qry4ms.png',
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
      h4("Reference Spectra"),
      tags$small("Upload reference spectrum as txt/csv/tsv with m/z and intensity."),
      fileInput("spectra_file", "", accept = c(".csv", ".txt", ".tsv")),

      hr(),
      downloadButton("download_ms", "Download .ms file", class = "btn-success"),
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
       radioButtons(
         "mirror_source",
         "Use MS2:",
         choices = c("Filtered" = "filtered", "Raw" = "raw"),
         inline = TRUE
       ),

       radioButtons(
         "sim_tol_unit",
         "Match tolerance unit:",
         choices = c("ppm" = "ppm", "Da" = "da"),
         selected = "ppm",
         inline = TRUE
       ),

       conditionalPanel(
         condition = "input.sim_tol_unit == 'ppm'",
         numericInput(
           "sim_ppm",
           "Match Tol (ppm):",
           value = 10,
           min = 0,
           step = 1
         )
       ),

       conditionalPanel(
         condition = "input.sim_tol_unit == 'da'",
         numericInput(
           "sim_da",
           "Match Tol (Da):",
           value = 0.01,
           min = 0,
           step = 0.001
         )
       ),
       prettyCheckbox(
         "mirror_highlight_matches",
         "Highlight matched peaks",
         value = FALSE,
         shape = "curve",
         status = "primary",
    animation = "pulse"
       )
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
         uiOutput("mirror_plot_ui") %>% withSpinner(color="#0066cc"),
        ),
        tabPanel("Raw .ms", verbatimTextOutput("preview_ms")),
        tabPanel("Mass & Pattern",
                 br(),
                 wellPanel(
                   fluidRow(
                     column(3, textInput("envipat_formula", "Formula:", value = "C15H10O7")),
                     column(3, selectInput("envipat_adduct", "Select Adduct:",
            choices = list(
              "Positive Adducts" = c(
                "[M+H]+"     = "[M+H]+",
                "[M+Na]+"    = "[M+Na]+",
                "[M+K]+"     = "[M+K]+",
                "[M+NH4]+"   = "[M+NH4]+",
                "M+"         = "M+",
                "[M+2H]2+"   = "[M+2H]2+",
                "[2M+H]+"    = "[2M+H]+",
                "[M+2Na]2+"  = "[M+2Na]2+",
                "[M+2K]2+"   = "[M+2K]2+",
                "[M+H+K]2+"  = "[M+H+K]2+",
                "[M+H+Na]2+" = "[M+H+Na]2+",
                "[M+ACN+H]+" = "[M+ACN+H]+"
              ),
              "Negative Adducts" = c(
                "[M-H]-"     = "[M-H]-",
                "[M+Cl]-"    = "[M+Cl]-",
                "[M+Br]-"    = "[M+Br]-",
                "M-"         = "M-",
                "[M-2H]2-"   = "[M-2H]2-",
                "[2M-H]-"    = "[2M-H]-",
                "[M+FA-H]-"  = "[M+FA-H]-",
                "[M+Hac-H]-" = "[M+Hac-H]-"
              )
            ))),
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
                        choices = c("Adduct (M+H / M-H)" = "adduct",
                                    "Neutral mass (M)" = "neutral"),
                        inline = TRUE),
          helpText("Parent mass & Charge are defined in the left sidebar. Calculation is based on the MetaboCoreUtils R package.")
         ),
         fluidRow(
           column(12,
                  div(style = "display: flex; align-items: center; justify-content: center; margin-bottom: 15px;",
                      tags$b("Calculated Neutral Mass (M): ", style = "margin-right: 10px; font-size: 18px; color: #2c3e50;"),
                      textInput("neutral_mass_display", label = NULL, value = "", width = "150px"),
                      actionButton("copy_neutral_mass", "Copy", icon = icon("clipboard"),
                                   style = "margin-left: 10px; margin-bottom: 15px; background-color: #2c3e50; color: white;")
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
           div(style = "text-align: center;",
            actionButton("run_rdisop", "Generate Formulas", class = "btn-info", width = "30%")
            )
         ),
         DTOutput("rdisop_table") %>% withSpinner()
),

tabPanel("Interpret MS1",
br(),
         wellPanel(
           fluidRow(
             column(4,
                    p(tags$b("About:"), "This algorithm evaluates MS1 specta clusters to propose the most likely neutral mass and identifies adducts/isotopes automatically. Calculation is based on the InterpretMSSpectrum R package."),
             ),
             column(3,
                    radioButtons("fm_ionmode", "Ion Mode:",
                                 choices = c("Positive" = "positive", "Negative" = "negative"),
                                 inline = TRUE)
             ),
             column(2,
                    numericInput("fm_ppm", "PPM Tol:", value = 5, min = 0.1, step = 1)
             ),
             column(2,
                    numericInput("fm_abs", "Da Tol:", value = 0.01, min = 0.000001, step = 0.001)
             ),
             column(2,
                    div(style = "text-align: center;", actionButton("run_findmain", "Run",
                                 class = "btn-info", style = "margin-top:25px; width:100%; font-weight:bold;"))
             )
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
  column(
    6,
    radioButtons(
      "range_tol_unit",
      "Tolerance unit:",
      choices = c("ppm" = "ppm", "Da" = "da"),
      selected = "ppm",
      inline = TRUE
    ),

    conditionalPanel(
      condition = "input.range_tol_unit == 'ppm'",
      numericInput("range_ppm", "Tolerance (ppm):", value = 5, min = 0)
    ),

    conditionalPanel(
      condition = "input.range_tol_unit == 'da'",
      numericInput("range_da", "Tolerance (Da):", value = 0.01, min = 0, step = 0.001)
    )
  )
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
                    h4("2. Specific Mass Error"),
                    numericInput("obs_mass", "Observed m/z:", value = 270.14407, step = 0.0001),
                    numericInput("theo_mass", "Theoretical m/z:", value = 270.14352, step = 0.0001),
                    radioButtons(
  "err_unit",
  "Error unit:",
  choices = c("ppm" = "ppm", "Da" = "da"),
  selected = "ppm",
  inline = TRUE
),
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
}
