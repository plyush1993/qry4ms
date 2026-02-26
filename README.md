# ms_for_sirius <img src="ms_for_sirius_logo.png" align="right" height="180" width="160">

### Description :bookmark_tabs:
The [Shiny App](https://shiny.posit.co/) for generating an individual .ms or .mgf file from user-provided MS spectra in MS1 & MS2 levels for a single compound. Spectra can be provided in .txt or pasted directly from the clipboard. The obtained .ms and .mgf files are suitable for processing in [SIRIUS](https://bio.informatik.uni-jena.de/software/sirius/), .mgf - [NIST MS Search](https://chemdata.nist.gov/mass-spc/ms-search/), [MetaboScape](https://www.bruker.com/en/products-and-solutions/mass-spectrometry/ms-software/metaboscape.html). It is also possible to filter by precursor mass and relative abundance, which provides a tidy formatting for the search query in MS fragmentation libraries. Also, there is an option to build a mirror plot from the reference spectra.

### Launch the App :rocket:
Shiny deployment:<br>
[**`https://plyush1993.shinyapps.io/ms_for_sirius/`**](https://plyush1993.shinyapps.io/ms_for_sirius/) <br><br>
Run locally:
```r
cat("Checking required packages (auto-installing if missing)\n")
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load("shiny", "DT", "ggplot2", "shinythemes", "shinyWidgets", "shinyjs", "BiocManager")
if (!requireNamespace("Spectra", quietly = TRUE)) BiocManager::install("Spectra")
if (!requireNamespace("MsBackendMgf", quietly = TRUE)) BiocManager::install("MsBackendMgf")

source("https://raw.githubusercontent.com/plyush1993/ms_for_sirius/refs/heads/main/app.R")
shiny::shinyApp(ui, server)
```
<br>

> [!IMPORTANT]
>The [App's script](https://github.com/plyush1993/Metabocano/blob/main/app.R) was compiled using [R version 4.1.2](https://cran.r-project.org/bin/windows/base/old/4.1.2/) 
<br>

### Contact :mailbox_with_mail:
Please send any comment, suggestion or question you may have to the author (Dr. Ivan Plyushchenko):  
<div> 
  <a href="mailto:plyushchenko.ivan@gmail.com"><img src="https://img.shields.io/badge/-4a9edc?style=for-the-badge&logo=gmail" height="28" alt="Email" /></a>
  <a href="https://github.com/plyush1993"><img src="https://img.shields.io/static/v1?style=for-the-badge&message=%20&color=181717&logo=GitHub&logoColor=FFFFFF&label=" height="28" alt="GH" /></a>
  <a href="https://orcid.org/0000-0003-3883-4695"><img src="https://img.shields.io/badge/-A6CE39?style=for-the-badge&logo=ORCID&logoColor=white" height="28" alt="ORCID" /></a>
</div>
