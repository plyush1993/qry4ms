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
library(Spectra)
library(MsBackendMgf)
#library(plotly)

ui <- fluidPage(
  useShinyjs(),
  
tags$head(tags$style(HTML("
  .app-footer { position: fixed; left:0; right:0; bottom:0; 
                text-align:center; font-size:12px; opacity:0.75;
                padding:8px; background: rgba(255,255,255,0.8);
                border-top: 1px solid #ddd; z-index: 9999; }
  body { padding-bottom: 45px; }
"))),

tags$head(
  tags$title("ms_for_sirius"),
  tags$link(rel = "icon", type = "image/png",
            href = "https://raw.githubusercontent.com/plyush1993/ms_for_sirius/main/ms_for_sirius_logo.png")
),

div(
  class = "app-footer",
  HTML('Created by: Ivan Plyushchenko &nbsp;|&nbsp;
       <a href="https://github.com/plyush1993/ms_for_sirius" target="_blank">GitHub repository</a>')
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
    src = 'https://raw.githubusercontent.com/plyush1993/ms_for_sirius/main/ms_for_sirius_logo.png',
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
    "MS1/MS2 .ms File Generator for SIRIUS"
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
  
  tags$head(
    tags$style(HTML("
      .nav-tabs > li > a {
        font-size: 20px !important;
        font-weight: bold !important;
        padding: 12px 18px !important;
      }
      .nav-tabs > li.active > a {
        font-size: 22px !important;
      }
    "))
  ),
  
  theme = shinytheme("flatly"), 
  setBackgroundColor(color = c("azure", "azure"), gradient = "linear", direction = "bottom"),
  
  sidebarLayout(
    sidebarPanel(
      h4("General settings"),
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
                 plotOutput("ms1_plot", height = "300px"),  
                 br(), br(),  
                 DTOutput("ms1_table")                      
        ),
        tabPanel("MS2 spectrum",
                 plotOutput("ms2_plot", height = "300px"),  
                 br(), br(),  
                 DTOutput("ms2_table")
        ),
        tabPanel("Mirror plot",
                 br(),
                 radioButtons(
                   "mirror_source", "Use MS2:",
                   choices = c("Filtered MS2 (after filters)" = "filtered",
                               "Raw MS2 (before filters)"     = "raw"),
                   inline = TRUE
                 ),
                 tags$small(
                   "Reference spectrum is taken from the CSV uploaded in the sidebar (columns: 'mz', 'intensity')."
                 ),
                 br(), br(),
                 plotOutput("mirror_plot", height = "600px")
        ),
        tabPanel("Raw .ms text", verbatimTextOutput("preview_ms"))
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

  ms2_for_mirror <- reactive({
    if (is.null(input$mirror_source) || input$mirror_source == "filtered") {
      ms2_filtered()
    } else {
      ms2_data()
    }
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
    theme_minimal(base_size = 12) +
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
  
  output$ms1_plot <- renderPlot({
    df <- ms1_filtered()
    validate(
      need(nrow(df) > 0, "No MS1 peaks to plot.")
    )
    p <- ggplot(df, aes(x = mz, y = intensity)) +
      geom_segment(aes(xend = mz, y = 0, yend = intensity)) +
      labs(x = "m/z", y = "Intensity", title = "MS1 spectrum") +
      theme_minimal(base_size = 12)
    #ggplotly(p)
    p
  })
  
  output$ms2_plot <- renderPlot({
    df <- ms2_filtered()
    validate(
      need(nrow(df) > 0, "No MS2 peaks to plot.")
    )
    p <- ggplot(df, aes(x = mz, y = intensity)) +
      geom_segment(aes(xend = mz, y = 0, yend = intensity)) +
      labs(x = "m/z", y = "Intensity", title = "MS2 spectrum") +
      theme_minimal(base_size = 12)
    #ggplotly(p)
    p
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
    
    valid <- ms1_ok && ms2_ok && ms_ok
    
    if (valid) {
      shinyjs::enable("download_ms")
    } else {
      shinyjs::disable("download_ms")
    }
    
    valid2 <- ms2_ok
    
    if (valid2) {
      shinyjs::enable("download_mgf")
    } else {
      shinyjs::disable("download_mgf")
    }
    
  })
  
}


shinyApp(ui, server)
