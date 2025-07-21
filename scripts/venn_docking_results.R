#!/usr/bin/env Rscript

# Generate Venn diagrams for docking result overlaps

suppressPackageStartupMessages({
  if (!requireNamespace("VennDiagram", quietly = TRUE)) {
    stop("Package 'VennDiagram' is required")
  }
})

library(VennDiagram)

script_dir <- dirname(normalizePath(sys.frame(1)$ofile))
root <- normalizePath(file.path(script_dir, ".."))
docking_dir <- file.path(root, "Docking Results")

files <- list(
  Atorva = list(
    PharmaMapper = file.path(docking_dir, "PharmaMapper", "AtorvoPharmMapper.csv"),
    SuperPred    = file.path(docking_dir, "SuperPre", "AtorvaSuperPred.csv"),
    Swiss        = file.path(docking_dir, "Swiss", "AtorvaSwiss.csv")
  ),
  Rosuva = list(
    PharmaMapper = file.path(docking_dir, "PharmaMapper", "RosuvaPharmMapper.csv"),
    SuperPred    = file.path(docking_dir, "SuperPre", "RosuvaSuperPred.csv"),
    Swiss        = file.path(docking_dir, "Swiss", "RosuvaSwiss.csv")
  )
)

read_uniprot <- function(path) {
  dat <- read.csv(path, check.names = FALSE)
  col <- grep("Uni", names(dat), ignore.case = TRUE, value = TRUE)[1]
  if (is.na(col)) stop("No UniProt column in ", path)
  ids <- trimws(as.character(dat[[col]]))
  unique(ids[ids != ""])
}

figures_dir <- file.path(root, "Figures")
if (!dir.exists(figures_dir)) dir.create(figures_dir)

for (drug in names(files)) {
  sets <- lapply(files[[drug]], read_uniprot)
  out_path <- file.path(figures_dir, paste0(drug, "_venn.svg"))
  venn.diagram(
    x = sets,
    category.names = names(sets),
    filename = out_path,
    imagetype = "svg",
    height = 320,
    width = 400,
    output = TRUE
  )
  message("Wrote ", out_path)
}

