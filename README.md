# qry4ms <img src="qry4ms.png" align="right" height="180" width="160">

### Description :bookmark_tabs:
The [`Shiny App`](https://shiny.posit.co/) for making an MS query: 
- Generates an individual .ms or .mgf file from user-provided MS spectra in MS1 & MS2 levels for a single compound. Spectra can be provided in .txt or pasted directly from the clipboard.
- Generated .ms / .mgf files were tested in [`SIRIUS`](https://bio.informatik.uni-jena.de/software/sirius/), and [`NIST MS Search`](https://chemdata.nist.gov/mass-spc/ms-search/).
- Filters by precursor mass and relative abundance, which provides a tidy formatting for the search query in MS fragmentation libraries.
- Static/Interactive MS1 & MS2 spectra.
- Builds a mirror plot from the reference spectra.
- Computes Isotopic Pattern Distribution and Monoisotopic Mass for chemical formula based on [`envipat`](https://cran.r-project.org/web/packages/enviPat/index.html).
- Calculates Adducts Map based on [`MetaboCoreUtils`](https://www.bioconductor.org/packages/release/bioc/html/MetaboCoreUtils.html).
- Generates Molecular Formula based on [`Rdisop`](https://bioconductor.org/packages/release/bioc/html/Rdisop.html).
- Interprets MS1 spectra based on [`InterpretMSSpectrum`](https://cran.r-project.org/web/packages/InterpretMSSpectrum/index.html).
- Calculates Mass Error.

### Launch the App :rocket:
Shiny deployment:<br>
[**`https://plyush1993.shinyapps.io/qry4ms/`**](https://plyush1993.shinyapps.io/qry4ms/) <br><br>
Run locally:
```r
cat("Checking required packages (auto-installing if missing)\n")
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load("shiny", "DT", "ggplot2", "shinythemes", "shinyWidgets", "shinyjs", "shinycssloaders", "BiocManager", "plotly", "enviPat", "InterpretMSSpectrum")
if (!requireNamespace("Spectra", quietly = TRUE)) BiocManager::install("Spectra")
if (!requireNamespace("MsBackendMgf", quietly = TRUE)) BiocManager::install("MsBackendMgf")
if (!requireNamespace("MetaboCoreUtils", quietly = TRUE)) BiocManager::install("MetaboCoreUtils")
if (!requireNamespace("MsCoreUtils", quietly = TRUE)) BiocManager::install("MsCoreUtils")
if (!requireNamespace("Rdisop", quietly = TRUE)) BiocManager::install("Rdisop")

source("https://raw.githubusercontent.com/plyush1993/qry4ms/refs/heads/main/app.R")
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
