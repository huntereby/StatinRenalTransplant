#' Generate a structural similarity dendrogram for a set of drugs.
#'
#' This script queries PubChem for canonical SMILES strings, converts them to
#' molecular fingerprints, computes pairwise Tanimoto similarities, and draws a
#' dendrogram to visualise the clustering of drugs by structural similarity.
#'
#' Required packages: webchem, rcdk, fingerprint, dplyr, tibble, purrr, readr
suppressPackageStartupMessages({
  library(webchem)
  library(rcdk)
  library(fingerprint)
  library(dplyr)
  library(tibble)
  library(purrr)
  library(readr)
})

drug_names <- c(
  "Acetylcysteine", "Acrivastine", "Albendazole", "Alitretinoin",
  "Amidate", "Atorvastatin", "Azathioprine", "Azelastine",
  "Bambuterol", "Benfluorex", "Berberine", "Bifonazole",
  "Cabergoline", "Captopril", "Cariprazine", "Cefdinir",
  "Chlorpheniramine", "Chlorthalidone", "Cilastatin", "Cilostazol",
  "Clarithromycin", "Crotamiton", "Dacomitinib", "Danazol",
  "Deferasirox", "Diazoxide", "Dichlorphenamide", "Dicumarol",
  "Dihydroergotamine", "Doxorubicin", "Erythromycin Ethylsuccinate",
  "Ethionamide", "Etodolac", "Evodiamine", "Famciclovir",
  "Fenipentol", "Fludarabine Phosphate", "Flunisolide", "Formoterol",
  "Gabapentin", "Gefitinib", "Glyburide", "Glycocholic acid",
  "Ifenprodil", "Ilomastat", "Infigratinib", "Ipratropium",
  "Isoflupredone Acetate", "Lacosamide", "Lapatinib", "Lasalocid",
  "Latanoprost", "Lenalidomide", "Lonafarnib", "Loperamide",
  "Loratadine", "Lorazepam", "Mafenide", "Medetomidine",
  "Melperone", "Menadione", "Mesoridazine", "Methyldopa",
  "Metolazone", "Midodrine", "Nabumetone", "Nicardipine",
  "Nimodipine", "Noscapine", "Ofloxacin", "Omeprazole",
  "Ondansetron", "Opipramol", "Oxfendazole", "Oxibendazole",
  "Paroxetine", "Pazopanib", "Penciclovir", "Pentoxifylline",
  "Perphenazine", "Phenformin", "Pimobendan", "Pinacidil",
  "Pindolol", "Piperacillin", "Piperidolate", "Ponatinib",
  "Primidone", "Propranolol", "Pyrilamine", "Quinidine Gluconate",
  "Racecadotril", "Regorafenib", "Retinol", "Salsolinol",
  "Sertaconazole", "Sertindole", "Sirolimus", "sn-Glycero-3-phosphocholine",
  "Tadalafil", "Testosterone", "Thiabendazole", "Tibolone",
  "Tivozanib", "Tofacitinib", "Topiramate", "Triamcinolone Diacetate",
  "Trifluoperazine", "Troglitazone", "Ursolic acid", "Valproic acid",
  "Vardenafil", "Vemurafenib", "Vilazodone Hydrochloride",
  "Vinpocetine", "Zileuton"
)

# Helper to fetch canonical SMILES -------------------------------------------------

fetch_smiles <- function(names) {
  cid_tbl <- webchem::get_cid(names, from = "name", first = TRUE, match = "first")

  valid_cids <- cid_tbl$cid[!is.na(cid_tbl$cid)]
  prop_tbl <- if (length(valid_cids) > 0) {
    out <- webchem::pc_prop(valid_cids, properties = "CanonicalSMILES")
    if (is.null(out)) {
      tibble(CID = numeric(0), CanonicalSMILES = character(0))
    } else {
      out
    }
  } else {
    tibble(CID = numeric(0), CanonicalSMILES = character(0))
  }

  smiles_tbl <- cid_tbl |>
    mutate(
      CanonicalSMILES = prop_tbl$CanonicalSMILES[match(cid, prop_tbl$CID)],
      CanonicalSMILES = na_if(CanonicalSMILES, "")
    )

  smiles_tbl |>
    select(query, cid, CanonicalSMILES) |>
    rename(drug = query)
}

smiles_df <- fetch_smiles(drug_names)

missing_smiles <- smiles_df |> filter(is.na(CanonicalSMILES))
if (nrow(missing_smiles) > 0) {
  warning(
    "No canonical SMILES were found for the following entries: ",
    paste(missing_smiles$drug, collapse = ", ")
  )
}

valid_smiles <- smiles_df |> filter(!is.na(CanonicalSMILES))

# Convert to molecules ------------------------------------------------------------

parse_to_fingerprint <- function(smiles) {
  mol <- rcdk::parse.smiles(smiles)
  if (length(mol) == 0 || is.null(mol[[1]])) {
    return(NULL)
  }
  fp <- rcdk::get.fingerprint(mol[[1]], type = "extended")
  fp
}

fingerprints <- purrr::map(valid_smiles$CanonicalSMILES, parse_to_fingerprint)

valid_idx <- !purrr::map_lgl(fingerprints, is.null)
if (!all(valid_idx)) {
  warning(
    "Unable to compute fingerprints for: ",
    paste(valid_smiles$drug[!valid_idx], collapse = ", ")
  )
}

fingerprints <- fingerprints[valid_idx]
drug_labels <- valid_smiles$drug[valid_idx]

if (length(fingerprints) < 2) {
  stop("At least two valid fingerprints are required to compute a dendrogram.")
}

# Similarity and clustering -------------------------------------------------------

sim_matrix <- fingerprint::fp.sim.matrix(fingerprints, method = "tanimoto")
dimnames(sim_matrix) <- list(drug_labels, drug_labels)

dist_matrix <- as.dist(1 - sim_matrix)
hc <- hclust(dist_matrix, method = "average")

# Save dendrogram -----------------------------------------------------------------

fig_path <- file.path("Figures", "drug_tanimoto_dendrogram.png")
png(fig_path, width = 1600, height = 900, res = 120)
plot(hc, main = "Drug structural similarity (Tanimoto)", xlab = "", sub = "",
     cex = 0.7)
dev.off()

message("Dendrogram saved to ", fig_path)

# Export similarity matrix --------------------------------------------------------

sim_path <- file.path("Generated Data", "drug_tanimoto_similarity.csv")

tibble::as_tibble(sim_matrix, rownames = "drug") |>
  readr::write_csv(sim_path)

message("Pairwise Tanimoto similarity matrix saved to ", sim_path)
