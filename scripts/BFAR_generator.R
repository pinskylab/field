#' ---
#' title: Annual Report for Gratuitous Permit No. FBP-0079-14
#' author: Malin Pinsky
#' date: 2018
#' output:
#'    word_document
#' ---

#'
#' ### Introduction
#' Under the project “Effects of low population density on reef fish
#' connectivity,” we conducted fieldwork and collected anemonefish samples in
#' Leyte from `r min(fish$date)` to `r max(fish$date)`. The primary objective was to collect
#' tissue samples (fin clips) from individuals of Amphiprion clarkii at each of
#' `r nrow(bfar)` locations. In this season, we sampled `r nrow(bfar)` sites in Albuera and Bay Bay City, Leyte.
#'
#' We are now conducting laboratory analysis of the collected specimens. We are
#' using genotyping-by-sequencing methods to genotype each specimen at a number
#' of Single Nucleotide Polymorphisms (SNPs). This will allow us to match
#' parents and offspring and to identify when we recapture the same fish in a
#' different field season. This information will allow us to determine whether
#' small populations are self-persistent or whether they rely on surrounding
#' populations for persistence (network persistence).
#'
#'### Inventory
#' Note: All samples are tissue clips from the caudal fin.
knitr::kable(bfar)

map_path <- "/Users/macair/Documents/field/data/leyte_map.png"
knitr::include_graphics(map_path)

#'<!--run bfar_data_generator.R first in order to generate the data for this report -->
#' #'<!-- rmarkdown::render('/Users/macair/Documents/field/scripts/BFAR_generator.R') -->
