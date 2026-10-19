##### HistoneMod: Curated Histone Mark Annotation #####
# Site-level annotation of histone post-translational modifications against a
# small curated reference table, plus mark-class summaries and plots.
#
# The curation in `inst/extdata/histone_marks.csv` is based on general reviews
# of histone modification biology (citations are intentionally kept here rather
# than in the CSV):
#   Allis CD, Jenuwein T. The molecular hallmarks of epigenetic control.
#     Nat Rev Genet. 2016;17(8):487-500. doi:10.1038/nrg.2016.59
#   Kouzarides T. Chromatin modifications and their function.
#     Cell. 2007;128(4):693-705. doi:10.1016/j.cell.2007.02.005
#   Bannister AJ, Kouzarides T. Regulation of chromatin by histone modifications.
#     Cell Res. 2011;21(3):381-395. doi:10.1038/cr.2011.22
#   Strahl BD, Allis CD. The language of covalent histone modifications.
#     Nature. 2000;403(6765):41-45. doi:10.1038/47412
#
# Mark classes are coarse functional labels:
#   active     - associated with transcribed chromatin / active enhancers
#   repressive - associated with silenced chromatin / heterochromatin
#   poised     - associated with bivalent or primed chromatin
#   other      - context-dependent marks and unmodified states

# Columns expected in the curated mark table.
.hm_mark_columns <- c(
  "protein", "residue", "position", "mod", "canonical", "class",
  "writer", "eraser", "reader", "notes"
)

# Allowed mark classes; the order is also the default plotting order.
.hm_mark_classes <- c("active", "repressive", "poised", "other")

# Variant histone families that inherit shared marks from their parent family.
# Only H3.3 -> H3 is used by default: H3.3 differs from H3.1/H3.2 at only a few
# residues, so the tail marks curated for H3 apply. H2A.Z and macroH2A are not
# inherited because their tails/C-termini differ more substantially.
.hm_variant_parent <- c("H3.3" = "H3")

# Internal: small built-in table used only when the packaged CSV is unavailable
# (for example when the package is sourced without its inst/extdata files).
.hm_fallback_marks <- function() {
  data.frame(
    protein = c("H3", "H3", "H3", "H3", "H3", "H4", "H4", "H2A", "H2B", "H3"),
    residue = c("K", "K", "K", "K", "K", "K", "K", "K", "K", "S"),
    position = c(4L, 4L, 4L, 9L, 27L, 5L, 16L, 119L, 120L, 10L),
    mod = c("me1", "me2", "me3", "me3", "me3", "ac", "ac", "ub", "ub", "ph"),
    canonical = c(
      "H3K4me1", "H3K4me2", "H3K4me3", "H3K9me3", "H3K27me3",
      "H4K5ac", "H4K16ac", "H2AK119ub", "H2BK120ub", "H3S10ph"
    ),
    class = c(
      "active", "active", "active", "repressive", "repressive",
      "active", "active", "repressive", "active", "other"
    ),
    writer = NA_character_,
    eraser = NA_character_,
    reader = NA_character_,
    notes = "Built-in fallback row used when the packaged CSV is unavailable",
    stringsAsFactors = FALSE
  )
}

# Internal: normalise modification tokens (case-insensitive, with aliases).
# Empty or missing values become "un" when `missing_to_un = TRUE` (site tables),
# and stay NA otherwise (curated mark tables).
.hm_normalise_mod <- function(mod, missing_to_un = TRUE) {
  mod <- tolower(trimws(as.character(mod)))
  out <- rep(NA_character_, length(mod))
  if (length(mod) == 0L) return(out)

  missing <- is.na(mod) | !nzchar(mod)
  if (missing_to_un) out[missing] <- "un"

  aliases <- c(
    "un" = "un", "unmodified" = "un", "none" = "un",
    "me" = "me1", "me1" = "me1", "mono" = "me1", "monomethyl" = "me1",
    "methyl" = "me1", "methylation" = "me1",
    "me2" = "me2", "di" = "me2", "dimethyl" = "me2", "dimethylation" = "me2",
    "me3" = "me3", "tri" = "me3", "trimethyl" = "me3", "trimethylation" = "me3",
    "ac" = "ac", "kac" = "ac", "acetyl" = "ac", "acetylation" = "ac",
    "ph" = "ph", "p" = "ph", "phospho" = "ph", "phosphorylation" = "ph",
    "ub" = "ub", "ubiq" = "ub", "ubiquitin" = "ub",
    "ubiquitination" = "ub", "ubiquitylation" = "ub",
    "su" = "su", "sumo" = "su", "sumoylation" = "su"
  )

  hit <- match(mod, names(aliases))
  known <- !is.na(hit)
  out[known] <- unname(aliases[hit[known]])
  # Unknown non-empty tokens are kept as-is (lower case) so that custom
  # modifications can be supplied by the user in their own mark table.
  unknown <- !missing & !known
  out[unknown] <- mod[unknown]
  out
}

# Internal: coerce a marks data.frame into the standard layout and types.
.hm_normalise_marks <- function(marks) {
  marks <- as.data.frame(marks, stringsAsFactors = FALSE)

  for (col in .hm_mark_columns) {
    if (!col %in% names(marks)) marks[[col]] <- NA
  }
  for (col in setdiff(.hm_mark_columns, "position")) {
    value <- trimws(as.character(marks[[col]]))
    value[!is.na(value) & !nzchar(value)] <- NA_character_
    marks[[col]] <- value
  }
  marks$position <- suppressWarnings(as.integer(as.character(marks$position)))
  marks$residue <- toupper(marks$residue)
  has_mod <- !is.na(marks$mod)
  marks$mod[has_mod] <- .hm_normalise_mod(marks$mod[has_mod], missing_to_un = FALSE)
  marks$class <- tolower(marks$class)

  marks[, .hm_mark_columns, drop = FALSE]
}

# Internal: validate a user-supplied mark table and normalise it.
.hm_validate_marks <- function(marks) {
  if (!is.data.frame(marks)) {
    stop("`marks` must be a data.frame of curated histone marks.", call. = FALSE)
  }
  required <- c("protein", "position", "mod", "canonical", "class")
  missing_cols <- setdiff(required, names(marks))
  if (length(missing_cols) > 0L) {
    stop(
      "`marks` is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  .hm_normalise_marks(marks)
}

# Internal: order the observed classes with the four standard classes first.
.hm_class_levels <- function(observed) {
  observed <- unique(as.character(observed))
  observed <- observed[!is.na(observed)]
  c(intersect(.hm_mark_classes, observed), sort(setdiff(observed, .hm_mark_classes)))
}

# Internal: parse site strings.
# Supports compact names ("H3K4me3", "H3K4", "H4K5ac", "H2A.ZK4ac",
# "macroH2A1K5ac") and the peptide-note style used by the demo data
# ("H3_3-8_K4_me3", "H4_4-17_K5_ac").
.hm_parse_site <- function(x) {
  x <- as.character(x)
  n <- length(x)
  protein <- rep(NA_character_, n)
  residue <- rep(NA_character_, n)
  position <- rep(NA_integer_, n)
  mod <- rep(NA_character_, n)

  # Demo peptide notes use underscores ("H3_3_8_K4_me3"), while the demo
  # protein column uses a dash ("H3_3-8"); accept both separators.
  note_pattern <- paste0(
    "^([A-Za-z][A-Za-z0-9.]*)_([0-9]+)[-_]([0-9]+)_",
    "([A-Za-z])([0-9]+)(?:_([A-Za-z0-9]+))?$"
  )
  # Compact names start with a histone-like protein prefix so that names such
  # as "macroH2A1K5ac" are not split in the middle of the protein name.
  compact_pattern <- paste0(
    "^((?:macroH2A[0-9.]*|H[0-9][A-Za-z0-9.]*?))",
    "([A-Za-z])([0-9]+)([A-Za-z0-9]*)$"
  )

  is_note <- !is.na(x) & grepl(note_pattern, x, perl = TRUE, ignore.case = TRUE)
  is_compact <- !is.na(x) & !is_note &
    grepl(compact_pattern, x, perl = TRUE, ignore.case = TRUE)

  if (any(is_note)) {
    values <- x[is_note]
    matched <- regmatches(values, regexec(note_pattern, values, perl = TRUE, ignore.case = TRUE))
    protein[is_note] <- vapply(
      matched, function(v) if (length(v) >= 2L) v[[2L]] else NA_character_, character(1)
    )
    residue[is_note] <- vapply(
      matched, function(v) if (length(v) >= 5L) v[[5L]] else NA_character_, character(1)
    )
    position[is_note] <- vapply(
      matched,
      function(v) if (length(v) >= 6L) suppressWarnings(as.integer(v[[6L]])) else NA_integer_,
      integer(1)
    )
    mod[is_note] <- vapply(
      matched, function(v) if (length(v) >= 7L) v[[7L]] else NA_character_, character(1)
    )
  }

  if (any(is_compact)) {
    values <- x[is_compact]
    matched <- regmatches(
      values,
      regexec(compact_pattern, values, perl = TRUE, ignore.case = TRUE)
    )
    protein[is_compact] <- vapply(
      matched, function(v) if (length(v) >= 2L) v[[2L]] else NA_character_, character(1)
    )
    residue[is_compact] <- vapply(
      matched, function(v) if (length(v) >= 3L) v[[3L]] else NA_character_, character(1)
    )
    position[is_compact] <- vapply(
      matched,
      function(v) if (length(v) >= 4L) suppressWarnings(as.integer(v[[4L]])) else NA_integer_,
      integer(1)
    )
    mod[is_compact] <- vapply(
      matched, function(v) if (length(v) >= 5L) v[[5L]] else NA_character_, character(1)
    )
  }

  data.frame(
    protein = protein,
    residue = residue,
    position = position,
    mod = mod,
    stringsAsFactors = FALSE
  )
}

# Internal: find site-table columns by normalised (case/separator-insensitive)
# name. Returns a named list of column names or NULL per role.
.hm_resolve_site_columns <- function(nms) {
  normalised <- tolower(gsub("[^A-Za-z0-9]", "", nms))
  pick <- function(candidates) {
    index <- which(normalised %in% candidates)
    if (length(index) == 0L) NULL else nms[index[1L]]
  }
  list(
    site = pick(c("site", "sitename", "sitestring")),
    protein = pick(c("protein", "proteinname", "histone", "histoneprotein")),
    residue = pick(c("residue", "aa", "aminoacid", "residueoneletter")),
    position = pick(c("position", "pos", "residuenumber", "siteposition")),
    mod = pick(c("mod", "modification", "modificationtype", "modtype", "ptm"))
  )
}

# Internal: use the explicit value where present and fall back to the parsed
# value for missing/empty entries.
.hm_pick_value <- function(explicit, fallback) {
  if (is.null(explicit)) return(as.character(fallback))
  explicit <- as.character(explicit)
  fallback <- as.character(fallback)
  blank <- is.na(explicit) | !nzchar(trimws(explicit))
  explicit[blank] <- fallback[blank]
  explicit
}

# Internal: match protein/residue/position/mod combinations against the mark
# table. Sites with a residue are matched on the full key first. Sites without a
# residue, and mark tables curated without a residue, are matched on
# protein + position + mod.
.hm_match_marks <- function(protein, residue, position, mod, marks) {
  n <- length(protein)
  out <- rep(NA_integer_, n)
  if (n == 0L) return(out)

  usable <- !is.na(protein) & !is.na(position) & !is.na(mod)
  if (!any(usable)) return(out)

  mark_usable <- !is.na(marks$protein) & !is.na(marks$position) & !is.na(marks$mod)
  if (!any(mark_usable)) return(out)

  key_full <- function(protein, residue, position, mod) {
    paste(protein, ifelse(is.na(residue), "", residue), position, mod, sep = "\r")
  }
  key_no_residue <- function(protein, position, mod) {
    paste(protein, position, mod, sep = "\r")
  }

  mark_has_residue <- mark_usable & !is.na(marks$residue)
  mark_without_residue <- mark_usable & is.na(marks$residue)

  mark_key_full <- rep(NA_character_, nrow(marks))
  mark_key_full[mark_has_residue] <- key_full(
    marks$protein[mark_has_residue],
    marks$residue[mark_has_residue],
    marks$position[mark_has_residue],
    marks$mod[mark_has_residue]
  )

  mark_key_no_residue <- rep(NA_character_, nrow(marks))
  mark_key_no_residue[mark_usable] <- key_no_residue(
    marks$protein[mark_usable],
    marks$position[mark_usable],
    marks$mod[mark_usable]
  )

  mark_key_no_residue_only <- rep(NA_character_, nrow(marks))
  mark_key_no_residue_only[mark_without_residue] <- key_no_residue(
    marks$protein[mark_without_residue],
    marks$position[mark_without_residue],
    marks$mod[mark_without_residue]
  )

  # 1. Exact match including the residue.
  with_residue <- usable & !is.na(residue)
  if (any(with_residue)) {
    out[with_residue] <- match(
      key_full(
        protein[with_residue],
        residue[with_residue],
        position[with_residue],
        mod[with_residue]
      ),
      mark_key_full
    )
  }

  # 2. Marks curated without a residue still match sites that carry one.
  remaining_with_residue <- usable & is.na(out) & !is.na(residue)
  if (any(remaining_with_residue)) {
    out[remaining_with_residue] <- match(
      key_no_residue(
        protein[remaining_with_residue],
        position[remaining_with_residue],
        mod[remaining_with_residue]
      ),
      mark_key_no_residue_only
    )
  }

  # 3. Sites without a residue match any mark by protein + position + mod.
  without_residue <- usable & is.na(out) & is.na(residue)
  if (any(without_residue)) {
    out[without_residue] <- match(
      key_no_residue(
        protein[without_residue],
        position[without_residue],
        mod[without_residue]
      ),
      mark_key_no_residue
    )
  }

  out
}

#' Curated histone mark reference table
#'
#' Loads the curated histone post-translational modification table shipped in
#' `inst/extdata/histone_marks.csv`. Each row describes one mark
#' (protein + residue + position + modification) with its canonical name, a
#' coarse functional class (`"active"`, `"repressive"`, `"poised"` or
#' `"other"`) and the best-known writer, eraser and reader proteins. Unknown
#' writer/eraser/reader entries are stored as `NA` rather than guessed.
#'
#' Unmodified states (`mod = "un"`) are included so that unmodified peptides
#' from quantitative workflows (for example the `_un` peptides in the demo
#' data) can be annotated as well; they are classified as `"other"`. The
#' `macroH2A` row is a documentation row: it carries no residue/position
#' because macroH2A is a variant histone rather than a single mark.
#'
#' The curation is based on general reviews of histone modification biology
#' (Allis & Jenuwein 2016, Nat Rev Genet; Kouzarides 2007, Cell). The citations
#' are deliberately kept out of the CSV itself.
#'
#' @return A `data.frame` with the columns `protein`, `residue`, `position`
#'   (integer), `mod`, `canonical`, `class`, `writer`, `eraser`, `reader` and
#'   `notes`. Returns a small built-in fallback table only if the packaged CSV
#'   cannot be found.
#'
#' @references
#' Allis CD, Jenuwein T. The molecular hallmarks of epigenetic control.
#' *Nat Rev Genet* 17:487-500 (2016). \doi{10.1038/nrg.2016.59}
#'
#' Kouzarides T. Chromatin modifications and their function.
#' *Cell* 128:693-705 (2007). \doi{10.1016/j.cell.2007.02.005}
#'
#' @export
histone_marks <- function() {
  path <- tryCatch(
    system.file("extdata", "histone_marks.csv", package = "HistoneMod"),
    error = function(e) ""
  )
  unavailable <- length(path) != 1L || is.na(path) || !nzchar(path) ||
    !file.exists(path)

  if (unavailable) {
    return(.hm_normalise_marks(.hm_fallback_marks()))
  }

  marks <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )

  missing_cols <- setdiff(.hm_mark_columns, names(marks))
  if (length(missing_cols) > 0L) {
    stop(
      "Packaged histone_marks.csv is missing column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  .hm_normalise_marks(marks)
}

#' Normalise histone protein names
#'
#' Maps common histone protein labels to a small canonical vocabulary
#' (`"H3"`, `"H3.3"`, `"H4"`, `"H2A"`, `"H2A.Z"`, `"H2B"`, `"macroH2A"`).
#' Input is case-insensitive and whitespace-tolerant. Residue ranges appended to
#' demo-style names such as `"H3_3-8"` or `"H4_4-17"` are removed before
#' matching, so those names map to `"H3"` and `"H4"`. Histone variant prefixes
#' are preserved when detectable: `"H3.3"` stays `"H3.3"` and `"H2A.Z"` stays
#' `"H2A.Z"`, while the replication-dependent variants `"H3.1"` and `"H3.2"`
#' are folded into `"H3"`. `"H2A.X"` is treated as `"H2A"` because the curated
#' H2A tail marks are shared.
#'
#' @param x A character (or factor) vector of histone protein names.
#'
#' @return A character vector of the same length as `x` with canonical protein
#'   names. Unrecognised or missing values are returned as `NA_character_`.
#'
#' @export
normalise_histone_protein <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (!is.character(x)) {
    x <- tryCatch(
      as.character(x),
      error = function(e) {
        stop("`x` must be a character vector of histone protein names.",
             call. = FALSE)
      }
    )
  }

  out <- rep(NA_character_, length(x))
  if (length(x) == 0L) return(out)

  key <- trimws(x)
  # Drop demo-style residue ranges such as "_3-8", "_3_8" or " 4-17". A leading
  # underscore/space is required so that the H3.3 variant suffix is preserved.
  key <- sub("[_ ][0-9]+[._\u2013-][0-9]+$", "", key)
  key <- trimws(key)
  key <- sub("[._ ]+$", "", key)
  lowered <- tolower(key)

  is_macro <- grepl("^macroh2a", lowered)
  is_h2az <- grepl("^h2a[._]?z", lowered)
  is_h2ax <- grepl("^h2a[._]?x", lowered)
  is_h33 <- grepl("^h3[._]?3", lowered)

  out[which(is_macro)] <- "macroH2A"
  out[which(is_h2az)] <- "H2A.Z"
  out[which(is_h2ax & !is_h2az)] <- "H2A"
  out[which(is_h33)] <- "H3.3"
  out[which(grepl("^h3", lowered) & !is_h33)] <- "H3"
  out[which(grepl("^h4", lowered))] <- "H4"
  out[which(grepl("^h2a", lowered) & !is_h2az & !is_h2ax)] <- "H2A"
  out[which(grepl("^h2b", lowered))] <- "H2B"

  out[is.na(x)] <- NA_character_
  out
}

#' Annotate histone modification sites
#'
#' Matches a per-site table against the curated mark table returned by
#' [histone_marks()] (or a user-supplied table) and appends the canonical mark
#' name, functional class, writer, eraser, reader and a `matched` flag.
#'
#' `sites` may either provide the components explicitly (`protein`,
#' `position` and `mod`, with an optional one-letter `residue`) or a `site`
#' column containing strings such as `"H3K4me3"`, `"H4K5ac"` or `"H3K4"`. The
#' demo-style peptide-note names (`"H3_3_8_K4_me3"`, `"H3_3-8_K4_me3"`,
#' `"H4_4_17_K5_ac"`) are parsed too. Explicit columns take precedence over
#' parsed values; missing entries are filled from the parsed `site` string. An
#' empty modification is interpreted as the unmodified state (`"un"`).
#'
#' Matching is case-insensitive and based on the normalised protein, residue,
#' position and modification. Protein names are normalised with
#' [normalise_histone_protein()]. Marks not found in the table are returned
#' with `NA` annotation and `matched = FALSE`; a single summary warning (rather
#' than one warning per row) reports how many sites could not be matched.
#' H3.3 sites inherit shared H3 marks when no H3.3-specific row exists.
#'
#' @param sites A `data.frame` with a `site` column and/or `protein`,
#'   `position`, `mod` (and optionally `residue`) columns, or a character vector
#'   of site names.
#' @param marks A curated mark table, by default [histone_marks()].
#'
#' @return `sites` with the additional columns `canonical`, `class`, `writer`,
#'   `eraser`, `reader` (character; `NA` for unmatched sites) and `matched`
#'   (logical).
#'
#' @export
annotate_sites <- function(sites, marks = histone_marks()) {
  if (is.character(sites)) {
    sites <- data.frame(site = sites, stringsAsFactors = FALSE)
  }
  if (!is.data.frame(sites)) {
    stop(
      "`sites` must be a data.frame or a character vector of site names.",
      call. = FALSE
    )
  }
  marks <- .hm_validate_marks(marks)

  data <- as.data.frame(sites, stringsAsFactors = FALSE)
  # Re-annotating a previously annotated table should not duplicate columns.
  data <- data[
    ,
    setdiff(names(data), c("canonical", "class", "writer", "eraser", "reader", "matched")),
    drop = FALSE
  ]

  columns <- .hm_resolve_site_columns(names(data))
  if (is.null(columns$site) && is.null(columns$protein) &&
      is.null(columns$position)) {
    stop(
      "`sites` must contain a `site` column or protein/position/mod columns.",
      call. = FALSE
    )
  }

  site_values <- if (is.null(columns$site)) {
    rep(NA_character_, nrow(data))
  } else {
    as.character(data[[columns$site]])
  }
  parsed <- .hm_parse_site(site_values)

  get_column <- function(column) {
    if (is.null(column)) NULL else data[[column]]
  }

  protein_raw <- .hm_pick_value(get_column(columns$protein), parsed$protein)
  residue_raw <- .hm_pick_value(get_column(columns$residue), parsed$residue)
  position_raw <- .hm_pick_value(get_column(columns$position), parsed$position)
  mod_raw <- .hm_pick_value(get_column(columns$mod), parsed$mod)

  protein <- normalise_histone_protein(protein_raw)
  residue <- toupper(trimws(residue_raw))
  residue[!is.na(residue) & !grepl("^[A-Z]$", residue)] <- NA_character_
  position <- suppressWarnings(as.integer(position_raw))
  mod <- .hm_normalise_mod(mod_raw)

  index <- .hm_match_marks(protein, residue, position, mod, marks)

  # H3.3 inherits shared H3 tail marks when no H3.3-specific row matched.
  inherits_parent <- is.na(index) & !is.na(protein) &
    protein %in% names(.hm_variant_parent)
  if (any(inherits_parent)) {
    parent <- unname(.hm_variant_parent[protein[inherits_parent]])
    index[inherits_parent] <- .hm_match_marks(
      parent,
      residue[inherits_parent],
      position[inherits_parent],
      mod[inherits_parent],
      marks
    )
  }

  data$canonical <- marks$canonical[index]
  data$class <- marks$class[index]
  data$writer <- marks$writer[index]
  data$eraser <- marks$eraser[index]
  data$reader <- marks$reader[index]
  data$matched <- !is.na(index)

  n_unmatched <- sum(!data$matched)
  if (n_unmatched > 0L) {
    display <- if (!is.null(columns$site)) {
      site_values
    } else {
      paste0(
        ifelse(is.na(protein), "?", protein),
        ifelse(is.na(residue), "?", residue),
        ifelse(is.na(position), "?", as.character(position)),
        ifelse(is.na(mod) | mod == "un", "", mod)
      )
    }
    examples <- unique(display[!data$matched])
    examples <- examples[!is.na(examples) & nzchar(examples)]
    examples <- utils::head(examples, 5L)
    warning(
      n_unmatched, " of ", nrow(data),
      " site(s) could not be matched to a curated histone mark",
      if (length(examples) > 0L) {
        paste0(" (e.g. ", paste(examples, collapse = ", "), ")")
      } else {
        ""
      },
      ". Unmatched sites are returned with NA annotation.",
      call. = FALSE
    )
  }

  data
}

#' Summarise site values by histone mark class
#'
#' Annotates a per-site data set with [annotate_sites()] and aggregates a
#' numeric value (for example site occupancy or relative abundance) by group
#' and mark class. The result contains the mean, standard deviation, number of
#' observations, number of distinct sites and standard error for every observed
#' group x class combination. Sites that cannot be matched to the curated table
#' are dropped (with the summary warning issued by [annotate_sites()]).
#'
#' @param data A `data.frame` with a `site` column or `protein`/`position`/
#'   `mod` columns, a group column and a numeric value column.
#' @param value_col Name of the numeric value column.
#' @param group_col Name of the grouping column, `"Group"` by default.
#' @param marks Curated mark table, by default [histone_marks()].
#' @param na.rm Logical; remove missing values before computing the summaries
#'   (default `TRUE`).
#'
#' @return A `data.frame` with columns `Group`, `class`, `mean`, `sd`, `n`
#'   (number of values), `n_sites` (number of distinct annotated marks) and
#'   `se` (standard error), ordered by group and mark class.
#'
#' @export
summarise_by_class <- function(data, value_col = "Percentage",
                               group_col = "Group",
                               marks = histone_marks(), na.rm = TRUE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }

  value_name <- .hm_find_column(names(data), value_col)
  if (is.null(value_name)) {
    stop(
      "Column '", value_col, "' not found in `data`. Available columns: ",
      paste(names(data), collapse = ", "),
      call. = FALSE
    )
  }
  group_name <- .hm_find_column(names(data), group_col)
  if (is.null(group_name)) {
    stop(
      "Column '", group_col, "' not found in `data`. Available columns: ",
      paste(names(data), collapse = ", "),
      call. = FALSE
    )
  }

  raw_value <- data[[value_name]]
  value <- suppressWarnings(as.numeric(as.character(raw_value)))
  coerced_to_na <- !is.na(raw_value) & is.na(value) &
    nzchar(trimws(as.character(raw_value)))
  if (any(coerced_to_na)) {
    stop("Column '", value_name, "' must be numeric.", call. = FALSE)
  }

  annotated <- annotate_sites(data, marks = marks)
  group_values <- as.character(annotated[[group_name]])

  keep <- !is.na(annotated$class)
  if (any(!keep)) {
    annotated <- annotated[keep, , drop = FALSE]
    group_values <- group_values[keep]
    value <- value[keep]
  }
  missing_group <- is.na(group_values) | !nzchar(trimws(group_values))
  if (any(missing_group)) {
    warning(
      sum(missing_group),
      " row(s) with a missing group were dropped from the class summary.",
      call. = FALSE
    )
    annotated <- annotated[!missing_group, , drop = FALSE]
    group_values <- group_values[!missing_group]
    value <- value[!missing_group]
  }

  empty <- data.frame(
    Group = character(0),
    class = character(0),
    mean = numeric(0),
    sd = numeric(0),
    n = integer(0),
    n_sites = integer(0),
    se = numeric(0),
    stringsAsFactors = FALSE
  )
  if (nrow(annotated) == 0L) return(empty)

  groups <- unique(group_values)
  classes <- .hm_class_levels(annotated$class)
  rows <- list()

  for (group in groups) {
    for (class in classes) {
      index <- which(group_values == group & annotated$class == class)
      if (length(index) == 0L) next

      values <- value[index]
      n_obs <- sum(!is.na(values))
      if (n_obs == 0L) next

      mean_value <- mean(values, na.rm = na.rm)
      sd_value <- if (n_obs > 1L) stats::sd(values, na.rm = na.rm) else NA_real_
      se_value <- if (!is.na(sd_value) && n_obs > 0L) {
        sd_value / sqrt(n_obs)
      } else {
        NA_real_
      }

      rows[[length(rows) + 1L]] <- data.frame(
        Group = group,
        class = class,
        mean = mean_value,
        sd = sd_value,
        n = as.integer(if (isTRUE(na.rm)) n_obs else length(values)),
        n_sites = as.integer(length(unique(annotated$canonical[index]))),
        se = se_value,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0L) return(empty)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# Internal: case/separator-insensitive column lookup.
.hm_find_column <- function(nms, name) {
  normalised <- tolower(gsub("[^A-Za-z0-9]", "", nms))
  wanted <- tolower(gsub("[^A-Za-z0-9]", "", as.character(name)[1L]))
  index <- which(normalised == wanted)
  if (length(index) == 0L) NULL else nms[index[1L]]
}

#' Plot histone mark class summaries
#'
#' Draws a grouped bar chart of mean values per group and mark class with
#' standard-error error bars, using a viridis fill palette and the same clean
#' theme as the other HistoneMod plots. The function accepts either a per-site
#' table (which is summarised with [summarise_by_class()] first) or the summary
#' table returned by [summarise_by_class()].
#'
#' @param data A per-site `data.frame` or a summary `data.frame` returned by
#'   [summarise_by_class()].
#' @param value_col Name of the numeric value column when `data` is a per-site
#'   table.
#' @param group_col Name of the grouping column, `"Group"` by default.
#' @param marks Curated mark table, by default [histone_marks()].
#' @param group_order Optional character vector giving the display order of
#'   groups. Unlisted groups are appended in order of appearance.
#' @param class_order Optional character vector giving the display order of
#'   mark classes.
#' @param palette Viridis palette name: one of `"viridis"`, `"magma"`,
#'   `"plasma"`, `"inferno"`, `"cividis"`, `"rocket"`, `"mako"` or `"turbo"`.
#' @param error_bar Character; `"se"` (default) or `"sd"` error bars.
#' @param ylab Optional y-axis label; defaults to the value column name for
#'   per-site input and `"Mean value"` for summary input.
#' @param legend_position Position of the class legend (default `"right"`).
#'
#' @return A `ggplot` object.
#'
#' @export
plot_mark_classes <- function(data, value_col = "Percentage",
                              group_col = "Group",
                              marks = histone_marks(),
                              group_order = NULL,
                              class_order = NULL,
                              palette = "viridis",
                              error_bar = c("se", "sd"),
                              ylab = NULL,
                              legend_position = "right") {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  error_bar <- match.arg(error_bar)
  palette <- match.arg(
    palette,
    c("viridis", "magma", "plasma", "inferno", "cividis", "rocket", "mako", "turbo")
  )

  is_summary <- all(c("class", "mean") %in% names(data)) &&
    any(c("Group", group_col) %in% names(data))

  if (is_summary) {
    summary <- as.data.frame(data, stringsAsFactors = FALSE)
    group_name <- if ("Group" %in% names(summary)) "Group" else group_col
    if (!group_name %in% names(summary)) {
      stop(
        "Summary `data` must contain a 'Group' column or the `group_col` column.",
        call. = FALSE
      )
    }
    summary$Group <- as.character(summary[[group_name]])
    summary$class <- as.character(summary$class)
    summary$mean <- as.numeric(summary$mean)
    summary$sd <- if ("sd" %in% names(summary)) as.numeric(summary$sd) else NA_real_
    summary$n <- if ("n" %in% names(summary)) as.numeric(summary$n) else NA_real_
    summary$n_sites <- if ("n_sites" %in% names(summary)) {
      as.numeric(summary$n_sites)
    } else {
      NA_real_
    }
    summary$se <- if ("se" %in% names(summary)) {
      as.numeric(summary$se)
    } else {
      ifelse(!is.na(summary$n) & summary$n > 0, summary$sd / sqrt(summary$n), NA_real_)
    }
    if (is.null(ylab)) ylab <- "Mean value"
  } else {
    summary <- summarise_by_class(
      data,
      value_col = value_col,
      group_col = group_col,
      marks = marks
    )
    if (is.null(ylab)) ylab <- value_col
  }

  if (nrow(summary) == 0L) {
    return(
      ggplot2::ggplot() +
        ggplot2::geom_text(
          ggplot2::aes(x = 0, y = 0, label = "No annotated sites to plot"),
          size = 5
        ) +
        ggplot2::theme_void()
    )
  }

  summary$err <- switch(
    error_bar,
    se = summary$se,
    sd = summary$sd
  )

  observed_groups <- unique(summary$Group)
  if (is.null(group_order)) {
    group_levels_out <- observed_groups
  } else {
    if (!is.character(group_order)) {
      stop("`group_order` must be a character vector of group names.", call. = FALSE)
    }
    group_levels_out <- c(
      intersect(group_order, observed_groups),
      setdiff(observed_groups, group_order)
    )
  }
  summary$Group <- factor(summary$Group, levels = group_levels_out)

  observed_classes <- unique(summary$class)
  if (is.null(class_order)) {
    class_levels_out <- .hm_class_levels(observed_classes)
  } else {
    if (!is.character(class_order)) {
      stop("`class_order` must be a character vector of mark classes.", call. = FALSE)
    }
    class_levels_out <- c(
      intersect(class_order, observed_classes),
      setdiff(.hm_class_levels(observed_classes), class_order)
    )
  }
  summary$class <- factor(summary$class, levels = class_levels_out)

  p <- ggplot2::ggplot(
    summary,
    ggplot2::aes(x = Group, y = mean, fill = class)
  ) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.9), width = 0.8) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean - err, ymax = mean + err),
      width = 0.2,
      position = ggplot2::position_dodge(width = 0.9),
      na.rm = TRUE
    ) +
    ggplot2::scale_fill_viridis_d(option = palette) +
    ggplot2::labs(x = "Group", y = ylab, fill = "Mark class") +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.border = ggplot2::element_rect(
        color = "black", fill = NA, linewidth = 0.8, linejoin = "mitre"
      ),
      axis.title = ggplot2::element_text(size = 14),
      axis.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 12),
      legend.text = ggplot2::element_text(size = 10),
      legend.position = legend_position
    )

  if (length(observed_groups) > 4L) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }

  p
}
