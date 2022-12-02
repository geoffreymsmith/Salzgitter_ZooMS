[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

# Salzgitter ZooMS analysis and Taphonomy.

This repository contains data and code used for analyses in:

> Ruebens et. al. (2021) Neanderthal subsistence, taphonomy and chronology at Salzgitter-Lebenstedt (Germany): a multifaceted analysis of morphologically unidentifiable bone. 

### Requirements
To run the code, you will need to have an up-to-date version of [R](https://www.r-project.org/) and [RStudio](https://rstudio.com/) installed on your computer and a few CRAN packages (see below). 

All code and analyses were run using R version 4.0.2 (R Core Team 2020) on a Windows 10 operating system. 

```
# Install required packages
install.packages(c("tidyverse", "patchwork", "rstatix", "colorblindr","scales"))


The following versions of these R packages were used: 
tidyverse_1.3.2   (Wickham et al. 2019)
patchwork_1.1.2 (Pedersen 2020)
rstatix_0.7.0 (Kassambara 2021)
colorblindr_0.1.0 (McWhite and Wilke. 2022)
scales_1.2.1  (Wickham and Seidel. 2022)

```
### Repository structure
`data/`: This contains all the data files necessary to replicate the analysis and produce the figures and tables in the manuscript. 

`output/`: contains output of all figures from manuscript as .tiff format.

`SL_taph_published code.R`: R script with the code necessary to run and reproduce the analysis and figures.

`readme.me` - readme file for this repository.

### Files contained in this repository

1) `data/`

  * `SL_ambic_acid_taxa_1105.csv` - peptide 1105 deamidation data for species from Salzgitter for both acid and ambic extractions.
  * `SL_bone_abrasion.csv` - bone abrasion data of major taxa from subdivided by low (0%, <50%) and high (>50%, 100%) abrasion.
  * `SL_bone_element_taxon.csv` - major taxa subdivided by body part (cranial, axial, forelimb, hindlimb, foot).
  * `SL_BSC.csv` - comparison of body size class (BSC) assignment to ZooMS identified specimens.
  * `SL_BSM_gen.csv` - percentage of major taxa  with carnivore and human modifications.
  * `SL_carniv_mod_specific.csv` - specific carnivore modifications on ZooMS identified specimens.
  * `SL_frag_summ_stats.csv` - summary statistics for fragment bone length (mean and standard deviation) for major species.
  * `SL_hum_mod_specific.csv` - percentage of specific human bone surface modificatios for major species.
  * `SL_main_taxa_length.csv` - ZooMS identified species and bone length.
  * `SL_read_mod.csv` - bone surface readability by major species and subdivided into low (0%, <50%) and high (>50%, 100%) readability.
  * `SL_total_NSP.csv` - total number of specimens by species from Salzgitter analysis.
  * `SL_weath_mod_labels.csv` - % of weathered bone by taxa for statistcal analysis.
  * `SL_weath_raw_taxa.csv` - % of weathered bone by different weathering stages and major species.
  * `SL_weathering.csv` -  bone weathering data for all specimens from Salzgitter.
  * `SL_wilcox_deamid.csv` - raw data including species and deamidation data to run wilcox test.

2) `output/` 

  * `Figure_3.tiff` - Figure 3: bone readability by major taxa.
  * `Figure_4.tiff` - Figure 4: bone weathering subdivied at assemblage level and by major taxon.
  * `Figure_5.tiff` - Figure 5: deamidation subdivided by taxa and extraction protocol (Ambic, Acid).
  * `Figure_6.tiff` - Figure 6: bone fragment length subdivided by major taxa.
  * `Figure_7.tiff` - Figure 7: major taxa subdivided by bone element portion (cranial, axial, forelimb, hindlimb, foot),
  * `Figure_8.tiff` - Figure 8: major taxa and recorded body size class.

### References

Alboukadel Kassambara (2021).rstatix: Pipe-Friendly Framework
for Basic Statistical Tests. R package version 0.7.0.
https://CRAN.R-project.org/package=rstatix


Thomas Lin Pedersen (2020).patchwork: The Composer of Plots.R package version 1.1.1.  https://CRAN.R-project.org/package=patchwork

R Core Team (2020). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna,Austria. URL
https://www.R-project.org/.

RStudio Team (2021). RStudio: Integrated Development Environment for R. RStudio, PBC, Boston, MA. http://www.rstudio.com/.

  McWhite C, Wilke C (2022). _colorblindr: Simulate colorblindness in R figures_. R package version 0.1.0, <https://github.com/clauswilke/colorblindr>.

Hadley Wickham and Jennifer Bryan (2019). readxl: Read Excel Files. R package version 1.3.1.
https://CRAN.R-project.org/package=readxl

Hadley Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686,     https://doi.org/10.21105/joss.01686

Hadley Wickham (2016). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York. https://ggplot2.tidyverse.org

 Wickham H, Seidel D (2022). _scales: Scale Functions for Visualization_. R package

### Licenses

**Text and figures :**
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Data :** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/)
attribution requested in reuse

**Code :** 

Copyright 2022 Geoff Smith

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
