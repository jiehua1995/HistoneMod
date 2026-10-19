# Tests for R/sites.R: site parsing, intensity-based occupancy and
# combinatorial modification cross-talk analysis.

# ---- helpers -------------------------------------------------------------

.hm_site_extdata <- function(file) {
  candidates <- c(
    system.file("extdata", file, package = "HistoneMod"),
    file.path("inst", "extdata", file),
    file.path("..", "..", "inst", "extdata", file),
    file.path("..", "..", "..", "inst", "extdata", file)
  )
  candidates <- candidates[nzchar(candidates)]
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0L) {
    testthat::skip(paste("Bundled extdata file not found:", file))
  }
  found[[1L]]
}

.hm_site_demo_tables <- function() {
  list(
    ms1 = read_skyline_csv(.hm_site_extdata("MS1_demo.csv")),
    sample = read_skyline_csv(.hm_site_extdata("samples_demo.csv"))
  )
}

# Build a minimal MS1 table. All arguments except `modified` and `areas` are
# recycled to the number of areas.
.hm_ms1 <- function(replicate = "A_rep1", protein = "H4_4-17",
                    sequence = "GKGGKGLGKGGAKR", note = "H4_4_17",
                    modified = NULL, areas, isotope = "light",
                    start = NULL, end = NULL) {
  n <- length(areas)
  out <- data.frame(
    Replicate.Name = rep(replicate, length.out = n),
    Protein.Name = rep(protein, length.out = n),
    Peptide.Sequence = rep(sequence, length.out = n),
    Peptide.Note = rep(note, length.out = n),
    Total.Area.MS1 = as.numeric(areas),
    Isotope.Label.Type = rep(isotope, length.out = n),
    stringsAsFactors = FALSE
  )
  if (!is.null(modified)) {
    out$Peptide.Modified.Sequence.Unimod.Ids <- rep(modified, length.out = n)
  }
  if (!is.null(start)) out$Peptide.Start <- rep(start, length.out = n)
  if (!is.null(end)) out$Peptide.End <- rep(end, length.out = n)
  out
}

.hm_sample <- function(replicates, groups) {
  data.frame(
    Replicate.Name = as.character(replicates),
    Group = as.character(groups),
    Replicate.No = as.integer(seq_along(replicates)),
    stringsAsFactors = FALSE
  )
}

# Modified sequence of H4 4-17 with acetylated lysines at the requested
# positions (K5, K8, K12 and/or K16).
.hm_h4_modified <- function(marks = character(0)) {
  sequence <- strsplit("GKGGKGLGKGGAKR", "", fixed = TRUE)[[1L]]
  index <- c(K5 = 2L, K8 = 5L, K12 = 9L, K16 = 13L)[marks]
  out <- ""
  for (i in seq_along(sequence)) {
    out <- paste0(out, sequence[i])
    if (i %in% index) out <- paste0(out, "(unimod:1)")
  }
  out
}

.hm_h4_table <- function(state_marks, areas, replicate = "A_rep1") {
  .hm_ms1(
    replicate = replicate,
    modified = vapply(state_marks, .hm_h4_modified, character(1)),
    areas = areas
  )
}

# Two H3 backbones that both cover H3K4: H3 3-8 and H3 3-10.
.hm_h3_two_backbones <- function() {
  rbind(
    .hm_ms1(
      protein = "H3", sequence = "TKQTAR", note = "H3_3_8",
      modified = c("", "TK(unimod:37)QTAR"), areas = c(100, 100)
    ),
    .hm_ms1(
      protein = "H3", sequence = "TKQTARKSTG", note = "H3_3_10",
      modified = c("", "TK(unimod:1)QTARKSTG"), areas = c(100, 300)
    )
  )
}

# ---- parse_peptide_sites -------------------------------------------------

test_that("parse_peptide_sites parses the demo UniMod annotation", {
  tables <- .hm_site_demo_tables()
  sites <- parse_peptide_sites(tables$ms1)

  expect_s3_class(sites, "data.frame")
  expect_identical(
    names(sites)[1:12],
    c("row_id", "Protein.Name", "Peptide.Sequence", "Peptide.Note", "start",
      "end", "residue", "position", "site", "mod", "is_modified",
      "is_unmodified")
  )
  expect_equal(nrow(sites), nrow(tables$ms1))
  expect_identical(sort(unique(sites$row_id)), seq_len(nrow(tables$ms1)))
  expect_true(all(sites$source == "unimod"))

  un_rows <- sites[sites$Peptide.Note == "H3_3_8_K4_un", , drop = FALSE]
  expect_equal(nrow(un_rows), 20L)
  expect_true(all(!un_rows$is_modified))
  expect_true(all(un_rows$is_unmodified))
  expect_true(all(un_rows$is_derivatisation))
  expect_true(all(un_rows$mod == "propionyl"))
  expect_true(all(un_rows$site == "H3K4"))
  expect_true(all(un_rows$position == 4))
  expect_true(all(un_rows$start == 3 & un_rows$end == 8))
  expect_true(all(un_rows$position_type == "protein"))

  me3_rows <- sites[sites$Peptide.Note == "H3_3_8_K4_me3", , drop = FALSE]
  expect_true(all(me3_rows$is_modified))
  expect_true(all(me3_rows$mod == "me3"))
  expect_true(all(me3_rows$site == "H3K4"))

  ac_rows <- sites[sites$mod == "ac", , drop = FALSE]
  expect_equal(nrow(ac_rows), 40L)
  expect_true(all(ac_rows$site %in% c("H4K5", "H4K12")))
  expect_equal(sort(unique(ac_rows$position)), c(5, 12))
  expect_true(all(ac_rows$start == 4 & ac_rows$end == 17))
})

test_that("parse_peptide_sites falls back to the Peptide.Note grammar", {
  ms1 <- .hm_ms1(
    protein = c("H3", "H3", "H3", "H3", "H3.3"),
    sequence = "KQTARKSTGGKAPR",
    note = c("H3_9_17_K8K14_unun", "H3_9_17_K9me3K14ac",
             "H3_9_17_un", "H3_K4_me3", "H3.3_9_17_K9me3"),
    areas = c(1, 1, 1, 1, 1)
  )
  ms1 <- ms1[!names(ms1) %in% "Peptide.Modified.Sequence.Unimod.Ids"]
  sites <- parse_peptide_sites(ms1)

  expect_true(all(sites$source %in% c("note", "none")))
  expect_true(any(sites$source == "note"))

  unun <- sites[sites$Peptide.Note == "H3_9_17_K8K14_unun", , drop = FALSE]
  expect_equal(nrow(unun), 2L)
  expect_identical(unun$residue, c("K", "K"))
  expect_equal(unun$position, c(8, 14))
  expect_identical(unun$mod, c("un", "un"))
  expect_true(all(!unun$is_modified & unun$is_unmodified))
  expect_true(all(unun$start == 9 & unun$end == 17))
  expect_true(all(unun$position_type == "protein"))

  inline <- sites[sites$Peptide.Note == "H3_9_17_K9me3K14ac", , drop = FALSE]
  expect_equal(inline$position, c(9, 14))
  expect_identical(inline$mod, c("me3", "ac"))
  expect_true(all(inline$is_modified))

  # A ranged note without any site token yields one unmodified placeholder row.
  empty_note <- sites[sites$Peptide.Note == "H3_9_17_un", , drop = FALSE]
  expect_equal(nrow(empty_note), 1L)
  expect_true(empty_note$is_unmodified)
  expect_true(is.na(empty_note$site))
  expect_true(empty_note$start == 9 && empty_note$end == 17)

  # Notes without a range use peptide-relative positions.
  relative <- sites[sites$Peptide.Note == "H3_K4_me3", , drop = FALSE]
  expect_equal(relative$position, 4)
  expect_equal(relative$site, "H3K4")
  expect_identical(relative$position_type, "peptide")

  # Protein names with dots are kept in the site label.
  dotted <- sites[sites$Peptide.Note == "H3.3_9_17_K9me3", , drop = FALSE]
  expect_equal(dotted$site, "H3.3K9")
})

test_that("parse_peptide_sites handles multi-mod notes and repeated tokens", {
  ms1 <- .hm_ms1(
    protein = c("H3", "H4", "H4"),
    sequence = c("KQTARKSTGGKAPR", "GKGGKGLGKGGAKR", "GKGGKGLGKGGAKR"),
    note = c("H3_9_17_K9K14_me3ac", "H4_4_17_K5K8K12K16_acacacac",
             "H4_4_17_K5_ac"),
    areas = c(1, 1, 1)
  )
  ms1 <- ms1[!names(ms1) %in% "Peptide.Modified.Sequence.Unimod.Ids"]
  sites <- parse_peptide_sites(ms1)

  combined <- sites[sites$Peptide.Note == "H3_9_17_K9K14_me3ac", ,
                    drop = FALSE]
  expect_equal(combined$position, c(9, 14))
  expect_identical(combined$mod, c("me3", "ac"))

  tetra <- sites[sites$Peptide.Note == "H4_4_17_K5K8K12K16_acacacac", ,
                 drop = FALSE]
  expect_equal(tetra$position, c(5, 8, 12, 16))
  expect_identical(tetra$mod, rep("ac", 4L))
  expect_identical(tetra$site, c("H4K5", "H4K8", "H4K12", "H4K16"))

  single <- sites[sites$Peptide.Note == "H4_4_17_K5_ac", , drop = FALSE]
  expect_equal(single$position, 5)
  expect_identical(single$mod, "ac")
})

test_that("parse_peptide_sites maps UniMod ids and supports extensions", {
  ms1 <- data.frame(
    Replicate.Name = "A_rep1",
    Protein.Name = "H3",
    Peptide.Note = NA_character_,
    Peptide.Modified.Sequence.Unimod.Ids = c(
      "TK(unimod:37)QTAR", "TK(unimod:58)QTAR",
      "TK(unimod:21)QTAR", "TK(unimod:99)QTAR",
      "TK(unimod:500)QTAR", "(unimod:21)TKQTAR"
    ),
    Total.Area.MS1 = 1,
    Isotope.Label.Type = "light",
    stringsAsFactors = FALSE
  )
  sites <- parse_peptide_sites(
    ms1,
    unimod_map = data.frame(unimod = 99, mod = "crotonyl")
  )
  expect_identical(
    sites$mod,
    c("me3", "propionyl", "ph", "crotonyl", "unimod:500", "ph")
  )
  expect_identical(sites$matched, c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE))
  expect_identical(
    sites$is_derivatisation,
    c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  expect_identical(sites$is_modified, c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(sites$position, c(2, 2, 2, 2, 2, NA))
  expect_true(all(sites$site[1:5] == "H3K2"))

  # A named character vector is accepted as well.
  sites2 <- parse_peptide_sites(ms1, unimod_map = c("99" = "crotonyl"))
  expect_identical(sites2$mod[4], "crotonyl")

  expect_error(
    parse_peptide_sites(ms1, unimod_map = data.frame(id = 1, mark = "ac")),
    "columns 'unimod' and 'mod'"
  )
})

test_that("parse_peptide_sites prefers Peptide.Start/Peptide.End columns", {
  ms1 <- .hm_ms1(
    protein = "H4", note = "H4_K5_ac", areas = 1, start = 10, end = 23
  )
  # The note token is peptide-relative when the note has no range.
  sites <- parse_peptide_sites(ms1)
  expect_equal(sites$start, 10)
  expect_equal(sites$end, 23)
  expect_equal(sites$position, 14)
  expect_equal(sites$site, "H4K14")
  expect_identical(sites$position_type, "protein")

  # With UniMod annotation the peptide index is translated via the start column.
  ms2 <- .hm_ms1(
    protein = "H3", sequence = "KQTARKSTGGKAPR", note = "H3_pep",
    modified = "TK(unimod:37)QTAR", areas = 1, start = 10, end = 23
  )
  sites2 <- parse_peptide_sites(ms2)
  expect_equal(sites2$position, 11)
  expect_identical(sites2$position_type, "protein")
})

test_that("parse_peptide_sites is robust to missing and empty annotation", {
  ms1 <- .hm_ms1(
    protein = c("H3", "H4"),
    note = c("some_random_peptide", NA_character_),
    modified = c(NA_character_, ""),
    areas = c(1, 1)
  )
  sites <- parse_peptide_sites(ms1)
  expect_equal(nrow(sites), 2L)
  expect_true(all(sites$is_unmodified))
  expect_true(all(is.na(sites$site)))
  expect_identical(sites$mod, c("un", "un"))
  expect_identical(sites$source, c("none", "none"))

  empty <- parse_peptide_sites(ms1[0, , drop = FALSE])
  expect_equal(nrow(empty), 0L)
  expect_true(all(c("row_id", "site", "is_modified") %in% names(empty)))

  expect_error(parse_peptide_sites("not a table"), "must be a data.frame")
})

# ---- compute_site_occupancy ---------------------------------------------

test_that("compute_site_occupancy computes the expected demo occupancies", {
  tables <- .hm_site_demo_tables()
  occ <- compute_site_occupancy(tables$ms1, tables$sample)

  expect_s3_class(occ, "data.frame")
  expect_true(all(c("Replicate.Name", "Group", "Protein.Name", "site",
                    "residue", "position", "backbone", "occupancy",
                    "n_states", "total_intensity", "complete_backbone",
                    "note", "reason") %in% names(occ)))

  h3 <- occ[occ$site == "H3K4" & occ$Replicate.Name == "SampleA_rep1", ,
            drop = FALSE]
  expect_equal(nrow(h3), 1L)
  expect_equal(h3$occupancy, 1680000000 / (4320000000 + 1680000000))
  expect_equal(h3$total_intensity, 6000000000)
  expect_equal(h3$n_states, 2)
  expect_true(h3$complete_backbone)
  expect_true(is.na(h3$note))
  expect_equal(h3$Group, "SampleA")

  h4_k5 <- occ[occ$site == "H4K5" & occ$Replicate.Name == "SampleA_rep1", ,
               drop = FALSE]
  h4_k12 <- occ[occ$site == "H4K12" & occ$Replicate.Name == "SampleA_rep1", ,
                drop = FALSE]
  expect_equal(h4_k5$occupancy, 1870000000 / 5500000000)
  expect_equal(h4_k12$occupancy, 3630000000 / 5500000000)
  expect_equal(h4_k5$total_intensity, h4_k12$total_intensity)
  expect_false(h4_k5$complete_backbone)
  expect_match(h4_k5$note, "no measured unmodified state")
  expect_true(all(occ$occupancy >= 0 & occ$occupancy <= 1, na.rm = TRUE))
})

test_that("occupancy sums to one over sites when all states are single-mod", {
  # A zero-area unmodified state keeps the backbone complete while every
  # remaining state carries exactly one modification.
  ms1 <- .hm_h4_table(
    list(character(0), "K5", "K8", "K12", "K16"),
    areas = c(0, 100, 200, 300, 400)
  )
  sample <- .hm_sample("A_rep1", "A")
  occ <- compute_site_occupancy(ms1, sample)

  expect_true(all(occ$complete_backbone))
  expect_equal(occ$n_states, rep(5L, nrow(occ)))
  expect_equal(sum(occ$occupancy), 1)
  expect_equal(
    occ$occupancy[match(c("H4K5", "H4K8", "H4K12", "H4K16"), occ$site)],
    c(0.1, 0.2, 0.3, 0.4)
  )
})

test_that("occupancy includes the unmodified state in the denominator", {
  ms1 <- .hm_h4_table(
    list(character(0), "K5", "K8"),
    areas = c(100, 100, 200)
  )
  occ <- compute_site_occupancy(ms1, .hm_sample("A_rep1", "A"))
  expect_equal(
    occ$occupancy[match(c("H4K5", "H4K8"), occ$site)],
    c(100 / 400, 200 / 400)
  )
  expect_equal(sum(occ$occupancy), 300 / 400)
  expect_equal(occ$n_states, rep(3L, nrow(occ)))
})

test_that("occupancy handles a missing unmodified state explicitly", {
  ms1 <- .hm_ms1(
    protein = "H3", sequence = "KQTARKSTG", note = "H3_9_17",
    modified = c("K(unimod:37)QTARKSTG", "KQTARK(unimod:1)STG"),
    areas = c(200, 50)
  )
  sample <- .hm_sample("A_rep1", "A")

  occ <- compute_site_occupancy(ms1, sample)
  expect_false(any(occ$complete_backbone))
  expect_match(occ$note[1L], "assumes unmeasured states are zero")
  expect_equal(
    occ$occupancy[match(c("H3K9", "H3K14"), occ$site)],
    c(200 / 250, 50 / 250)
  )

  strict <- compute_site_occupancy(ms1, sample, require_unmodified = TRUE)
  expect_true(all(is.na(strict$occupancy)))
  expect_match(strict$reason[1L], "unmodified state")
})

test_that("occupancy handles zero areas and duplicated rows", {
  ms1 <- .hm_h4_table(list(character(0), "K5"), areas = c(0, 0))
  zero <- compute_site_occupancy(ms1, .hm_sample("A_rep1", "A"))
  expect_equal(nrow(zero), 1L)
  expect_true(is.na(zero$occupancy))
  expect_match(zero$reason, "non-positive total")

  # A zero-area state is still counted as a state.
  ms2 <- .hm_h4_table(list(character(0), "K5"), areas = c(100, 0))
  occ2 <- compute_site_occupancy(ms2, .hm_sample("A_rep1", "A"))
  expect_equal(occ2$occupancy, 0)
  expect_equal(occ2$n_states, 2L)
  expect_true(occ2$complete_backbone)

  # Duplicated rows are summed rather than double-counted as states.
  ms3 <- rbind(
    .hm_h4_table(list(character(0)), areas = 100),
    .hm_h4_table(list(character(0)), areas = 100),
    .hm_h4_table("K5", areas = 100)
  )
  occ3 <- compute_site_occupancy(ms3, .hm_sample("A_rep1", "A"))
  expect_equal(occ3$total_intensity, 300)
  expect_equal(occ3$occupancy, 1 / 3)
  expect_equal(occ3$n_states, 2L)
})

test_that("occupancy uses light peptides only", {
  ms1 <- .hm_h4_table(list(character(0), "K5"), areas = c(100, 100))
  heavy <- ms1
  heavy$Isotope.Label.Type <- "heavy"
  heavy$Total.Area.MS1 <- c(1e9, 1e9)
  combined <- rbind(ms1, heavy)

  occ <- compute_site_occupancy(combined, .hm_sample("A_rep1", "A"))
  expect_equal(occ$total_intensity, 200)
  expect_equal(occ$occupancy, 0.5)
})

test_that("backbone_detail = FALSE aggregates overlapping backbones", {
  ms1 <- .hm_h3_two_backbones()
  sample <- .hm_sample("A_rep1", "A")

  detail <- compute_site_occupancy(ms1, sample)
  expect_equal(nrow(detail), 2L)
  expect_equal(
    detail$occupancy[match(c("H3_3-8", "H3_3-10"), detail$backbone)],
    c(0.5, 0.75)
  )
  expect_equal(
    detail$total_intensity[match(c("H3_3-8", "H3_3-10"), detail$backbone)],
    c(200, 400)
  )

  aggregated <- compute_site_occupancy(ms1, sample, backbone_detail = FALSE)
  expect_equal(nrow(aggregated), 1L)
  expect_equal(aggregated$site, "H3K4")
  expect_equal(aggregated$n_backbones, 2L)
  expect_equal(aggregated$n_states, 4L)
  expect_equal(aggregated$total_intensity, 600)
  # Intensity-weighted mean: (0.5 * 200 + 0.75 * 400) / 600.
  expect_equal(aggregated$occupancy, 400 / 600)
  expect_true(aggregated$complete_backbone)
  expect_true(is.na(aggregated$backbone))
})

test_that("occupancy falls back to the Peptide.Note annotation", {
  tables <- .hm_site_demo_tables()
  without_unimod <- tables$ms1[
    , setdiff(names(tables$ms1), "Peptide.Modified.Sequence.Unimod.Ids"),
    drop = FALSE
  ]
  with_unimod <- compute_site_occupancy(tables$ms1, tables$sample)
  without <- compute_site_occupancy(without_unimod, tables$sample)

  key <- c("Replicate.Name", "Protein.Name", "site")
  with_unimod <- with_unimod[order(with_unimod$Replicate.Name,
                                   with_unimod$Protein.Name,
                                   with_unimod$site), , drop = FALSE]
  without <- without[order(without$Replicate.Name, without$Protein.Name,
                           without$site), , drop = FALSE]
  expect_equal(without[[key[1L]]], with_unimod[[key[1L]]])
  expect_equal(without$occupancy, with_unimod$occupancy)
  expect_equal(without$complete_backbone, with_unimod$complete_backbone)
})

test_that("compute_site_occupancy validates its inputs and empty input", {
  tables <- .hm_site_demo_tables()

  expect_error(compute_site_occupancy("x", tables$sample), "data.frame")
  expect_error(
    compute_site_occupancy(tables$ms1, "x"), "data.frame"
  )
  expect_error(
    compute_site_occupancy(
      tables$ms1[, setdiff(names(tables$ms1), "Total.Area.MS1")],
      tables$sample
    ),
    "missing required"
  )
  expect_error(
    compute_site_occupancy(tables$ms1,
                           tables$sample[, setdiff(names(tables$sample),
                                                   "Group")]),
    "'Group'"
  )

  no_heavy <- tables$ms1
  no_heavy$Isotope.Label.Type <- "heavy"
  empty <- compute_site_occupancy(no_heavy, tables$sample)
  expect_equal(nrow(empty), 0L)
  expect_true("occupancy" %in% names(empty))
})

# ---- compute_crosstalk ---------------------------------------------------

test_that("compute_crosstalk derives intensity-weighted pattern frequencies", {
  tables <- .hm_site_demo_tables()
  ct <- compute_crosstalk(tables$ms1, sample = tables$sample)

  expect_type(ct, "list")
  expect_identical(
    sort(names(ct)),
    sort(c("patterns", "cooccurrence", "sites", "settings"))
  )
  expect_s3_class(ct$patterns, "data.frame")
  expect_s3_class(ct$cooccurrence, "data.frame")
  expect_s3_class(ct$sites, "data.frame")
  expect_true(all(c("sample", "group", "protein", "pattern", "sites",
                    "frequency", "group_frequency", "n_sites",
                    "n_samples") %in% names(ct$patterns)))
  expect_true(all(c("site_a", "site_b", "log_odds_ratio", "n",
                    "conditional_a_given_b",
                    "conditional_b_given_a") %in% names(ct$cooccurrence)))
  expect_true(is.list(ct$settings))

  # Frequencies sum to one per sample x protein.
  by_sample <- stats::aggregate(
    ct$patterns$frequency,
    by = list(sample = ct$patterns$sample, protein = ct$patterns$protein),
    FUN = sum
  )
  expect_equal(by_sample$x, rep(1, nrow(by_sample)))

  a1 <- ct$patterns[
    ct$patterns$sample == "SampleA_rep1" & ct$patterns$protein == "H3_3-8", ,
    drop = FALSE
  ]
  expect_equal(a1$frequency[a1$pattern == "-"],
               4320000000 / (4320000000 + 1680000000))
  expect_equal(a1$frequency[a1$pattern == "H3K4me3"],
               1680000000 / (4320000000 + 1680000000))

  h3_group <- ct$patterns[
    ct$patterns$protein == "H3_3-8" & ct$patterns$pattern == "-" &
      ct$patterns$group == "SampleA", ,
    drop = FALSE
  ]
  expect_equal(unique(h3_group$group_frequency),
               mean(c(4320 / 6000, 4284 / 6300, 4060 / 5800, 4331 / 6100)))

  # H4 K5 and K12 are mutually exclusive in the demo data.
  h4_pairs <- ct$cooccurrence[ct$cooccurrence$protein == "H4_4-17", ,
                              drop = FALSE]
  expect_equal(nrow(h4_pairs), 5L)
  expect_true(all(h4_pairs$a == 0))
  expect_true(all(h4_pairs$log_odds_ratio < 0))
  expect_true(all(abs(h4_pairs$a + h4_pairs$b + h4_pairs$c +
                        h4_pairs$d - 1) < 1e-8))
  expect_equal(unique(h4_pairs$n), 4L)
})

test_that("compute_crosstalk supports multi-mod patterns and positive logOR", {
  ms1 <- .hm_h4_table(
    list(character(0), "K5", "K8", c("K5", "K8")),
    areas = c(100, 50, 50, 100)
  )
  ms1 <- rbind(ms1, {
    copy <- ms1
    copy$Replicate.Name <- "A_rep2"
    copy
  })
  sample <- .hm_sample(c("A_rep1", "A_rep2"), c("A", "A"))
  ct <- compute_crosstalk(ms1, sample = sample)

  pattern_freq <- ct$patterns[
    ct$patterns$sample == "A_rep1" & ct$patterns$protein == "H4_4-17", ,
    drop = FALSE
  ]
  expected <- c("-" = 1 / 3, "H4K5ac" = 1 / 6, "H4K8ac" = 1 / 6,
                "H4K5ac|H4K8ac" = 1 / 3)
  expect_equal(
    pattern_freq$frequency[match(names(expected), pattern_freq$pattern)],
    unname(expected)
  )
  expect_equal(pattern_freq$group_frequency, pattern_freq$frequency)
  expect_equal(pattern_freq$n_sites[
    pattern_freq$pattern == "H4K5ac|H4K8ac"
  ], 2L)
  expect_equal(pattern_freq$sites[pattern_freq$pattern == "H4K5ac|H4K8ac"],
               "H4K5ac,H4K8ac")

  pair <- ct$cooccurrence[ct$cooccurrence$protein == "H4_4-17", ,
                          drop = FALSE]
  expect_equal(nrow(pair), 1L)
  expect_equal(pair$site_a, "H4K5ac")
  expect_equal(pair$site_b, "H4K8ac")
  expect_equal(pair$a, 1 / 3)
  expect_equal(pair$b, 1 / 6)
  expect_equal(pair$c, 1 / 6)
  expect_equal(pair$d, 1 / 3)
  expect_equal(pair$log_odds_ratio, log(1.5625))
  expect_gt(pair$log_odds_ratio, 0)
  expect_equal(pair$conditional_a_given_b, 2 / 3)
  expect_equal(pair$conditional_b_given_a, 2 / 3)
  expect_equal(pair$n, 2L)
})

test_that("compute_crosstalk reports negative logOR for exclusive sites", {
  ms1 <- .hm_h4_table(
    list(character(0), "K5", "K8"),
    areas = c(100, 100, 100)
  )
  ct <- compute_crosstalk(ms1, sample = .hm_sample("A_rep1", "A"))
  pair <- ct$cooccurrence[ct$cooccurrence$protein == "H4_4-17", ,
                          drop = FALSE]
  expect_equal(nrow(pair), 1L)
  expect_equal(pair$a, 0)
  expect_equal(pair$b, 1 / 3)
  expect_equal(pair$c, 1 / 3)
  expect_equal(pair$d, 1 / 3)
  expect_equal(pair$log_odds_ratio, log(0.6))
  expect_lt(pair$log_odds_ratio, 0)
  expect_equal(pair$conditional_a_given_b, 0)
  expect_equal(pair$conditional_b_given_a, 0)

  # Sorted pattern labels follow protein position order.
  expect_true("-" %in% ct$patterns$pattern)
})

test_that("compute_crosstalk marginal site frequencies are consistent", {
  ms1 <- .hm_h4_table(
    list(character(0), "K5", "K8", c("K5", "K8")),
    areas = c(100, 50, 50, 100)
  )
  ct <- compute_crosstalk(ms1, sample = .hm_sample("A_rep1", "A"))
  sites <- ct$sites[ct$sites$sample == "A_rep1", , drop = FALSE]

  k5 <- sites$frequency[sites$site == "H4K5ac"]
  k8 <- sites$frequency[sites$site == "H4K8ac"]
  expect_equal(k5, 1 / 6 + 1 / 3)
  expect_equal(k8, 1 / 6 + 1 / 3)
  patterns <- ct$patterns[ct$patterns$sample == "A_rep1", , drop = FALSE]
  expect_equal(sum(patterns$frequency[grepl("H4K5ac", patterns$pattern)]), k5)
})

test_that("compute_crosstalk supports proteins with a single site", {
  ms1 <- .hm_ms1(
    protein = "H3", sequence = "TKQTAR", note = "H3_3_8",
    modified = c("TK(unimod:37)QTAR", "TK(unimod:58)QTAR"),
    areas = c(100, 100)
  )
  ct <- compute_crosstalk(ms1, sample = .hm_sample("A_rep1", "A"))
  expect_equal(nrow(ct$cooccurrence), 0L)
  expect_true(all(c("site_a", "site_b", "log_odds_ratio",
                    "conditional_a_given_b") %in%
                    names(ct$cooccurrence)))
  expect_equal(nrow(ct$patterns), 2L)
})

test_that("compute_crosstalk filters proteins and ignores heavy peptides", {
  tables <- .hm_site_demo_tables()
  only_h3 <- compute_crosstalk(
    tables$ms1, protein = "H3_3-8", sample = tables$sample
  )
  expect_equal(unique(only_h3$patterns$protein), "H3_3-8")
  expect_equal(nrow(only_h3$cooccurrence), 0L)

  expect_error(
    compute_crosstalk(tables$ms1, protein = "not-a-protein",
                      sample = tables$sample),
    "was not found"
  )

  heavy <- tables$ms1
  heavy$Isotope.Label.Type <- "heavy"
  heavy$Total.Area.MS1 <- 1e12
  combined <- rbind(tables$ms1, heavy)
  with_heavy <- compute_crosstalk(combined, sample = tables$sample)
  reference <- compute_crosstalk(tables$ms1, sample = tables$sample)
  expect_equal(
    with_heavy$patterns$frequency,
    reference$patterns$frequency
  )
})

test_that("compute_crosstalk handles missing groups and zero intensities", {
  tables <- .hm_site_demo_tables()
  expect_error(compute_crosstalk(tables$ms1), "Group")
  expect_error(
    compute_crosstalk(
      tables$ms1[, setdiff(names(tables$ms1), "Replicate.Name")],
      sample = tables$sample
    ),
    "Replicate.Name"
  )

  # A Group column in the data is sufficient when no sample table is given.
  with_group <- merge(
    tables$ms1,
    tables$sample[, c("Replicate.Name", "Group")],
    by = "Replicate.Name"
  )
  from_column <- compute_crosstalk(with_group)
  from_sample <- compute_crosstalk(tables$ms1, sample = tables$sample)
  from_column <- from_column$patterns[order(
    from_column$patterns$sample, from_column$patterns$protein,
    from_column$patterns$pattern
  ), , drop = FALSE]
  from_sample <- from_sample$patterns[order(
    from_sample$patterns$sample, from_sample$patterns$protein,
    from_sample$patterns$pattern
  ), , drop = FALSE]
  expect_equal(from_column$frequency, from_sample$frequency)

  zero <- .hm_h4_table(list(character(0), "K5"), areas = c(0, 0))
  expect_warning(
    empty <- compute_crosstalk(zero, sample = .hm_sample("A_rep1", "A")),
    "excluded from cross-talk"
  )
  expect_equal(nrow(empty$patterns), 0L)
  expect_true(all(c("sample", "pattern", "frequency") %in%
                    names(empty$patterns)))
  expect_true(is.list(empty$settings))
})

# ---- plotting ------------------------------------------------------------

test_that("plot_occupancy returns grouped ggplot bars", {
  tables <- .hm_site_demo_tables()
  occ <- compute_site_occupancy(tables$ms1, tables$sample)

  p <- plot_occupancy(occ)
  expect_s3_class(p, "ggplot")
  expect_equal(sort(unique(as.character(p$data$Group))),
               sort(unique(tables$sample$Group)))
  expect_equal(unique(as.character(p$data$site)),
               c("H3K4", "H4K5", "H4K12"))
  expect_true(all(p$data$mean_occupancy >= 0 &
                    p$data$mean_occupancy <= 1))

  p_order <- plot_occupancy(occ, group_order = c("SampleC", "SampleA"))
  expect_s3_class(p_order, "ggplot")
  expect_equal(levels(p_order$data$Group)[1:2], c("SampleC", "SampleA"))

  p_facet <- plot_occupancy(occ, facet = TRUE, error_bar = TRUE,
                            color_palette = "magma")
  expect_s3_class(p_facet, "ggplot")
  expect_s3_class(p_facet$facet, "FacetWrap")

  p_limits <- plot_occupancy(occ, y_limits = c(0, 1))
  expect_s3_class(p_limits, "ggplot")

  expect_error(plot_occupancy(data.frame(site = "H3K4")), "missing required")
  expect_s3_class(
    plot_occupancy(data.frame(site = character(0), occupancy = numeric(0),
                              Group = character(0))),
    "ggplot"
  )
})

test_that("plot_occupancy accepts aggregated occupancy output", {
  ms1 <- .hm_h3_two_backbones()
  aggregated <- compute_site_occupancy(
    ms1, .hm_sample("A_rep1", "A"), backbone_detail = FALSE
  )
  p <- plot_occupancy(aggregated)
  expect_s3_class(p, "ggplot")
  expect_equal(p$data$mean_occupancy, 400 / 600)
})

test_that("plot_crosstalk draws pattern bars", {
  tables <- .hm_site_demo_tables()
  ct <- compute_crosstalk(tables$ms1, sample = tables$sample)

  p <- plot_crosstalk(ct, type = "patterns")
  expect_s3_class(p, "ggplot")
  expect_true(all(c("pattern", "frequency", "group") %in% names(p$data)))
  expect_true(all(p$data$frequency >= 0))

  p_top <- plot_crosstalk(ct, type = "patterns", top_n = 2)
  expect_s3_class(p_top, "ggplot")
  expect_equal(length(levels(p_top$data$pattern)), 2L)

  p_order <- plot_crosstalk(ct, type = "patterns", top_n = 3,
                            group_order = c("SampleC", "SampleA"))
  expect_equal(levels(p_order$data$group)[1:2], c("SampleC", "SampleA"))

  p_facet <- plot_crosstalk(ct, type = "patterns", facet = TRUE,
                            color_palette = "plasma")
  expect_s3_class(p_facet, "ggplot")
  expect_s3_class(p_facet$facet, "FacetWrap")

  # A plain patterns data.frame is accepted too.
  p_df <- plot_crosstalk(ct$patterns, type = "patterns")
  expect_s3_class(p_df, "ggplot")

  expect_error(plot_crosstalk(ct, type = "patterns", top_n = 0),
               "positive integer")
})

test_that("plot_crosstalk draws a co-occurrence logOR heatmap", {
  tables <- .hm_site_demo_tables()
  ct <- compute_crosstalk(tables$ms1, sample = tables$sample)

  p <- plot_crosstalk(ct, type = "cooccurrence")
  expect_s3_class(p, "ggplot")
  expect_true(all(c("site_a", "site_b", "log_odds_ratio") %in%
                    names(p$data)))
  expect_true(any(p$data$log_odds_ratio < 0))

  # Only one group: no facet.
  single <- compute_crosstalk(
    tables$ms1[tables$ms1$Replicate.Name %in% c("SampleA_rep1",
                                                "SampleA_rep2"), ],
    sample = tables$sample
  )
  p_single <- plot_crosstalk(single, type = "cooccurrence")
  expect_s3_class(p_single, "ggplot")

  # A plain co-occurrence data.frame is accepted too.
  p_df <- plot_crosstalk(ct$cooccurrence, type = "cooccurrence")
  expect_s3_class(p_df, "ggplot")

  expect_error(
    plot_crosstalk(data.frame(a = 1), type = "cooccurrence"),
    "site_a, site_b, log_odds_ratio"
  )
  expect_error(
    plot_crosstalk(ct, type = "not-a-type")
  )
})

test_that("plot_crosstalk returns placeholders for empty results", {
  empty <- list(
    patterns = data.frame(
      sample = character(0), group = character(0), protein = character(0),
      pattern = character(0), sites = character(0), frequency = numeric(0),
      group_frequency = numeric(0), n_sites = integer(0),
      n_samples = integer(0), stringsAsFactors = FALSE
    ),
    cooccurrence = data.frame(
      group = character(0), protein = character(0), site_a = character(0),
      site_b = character(0), a = numeric(0), b = numeric(0), c = numeric(0),
      d = numeric(0), log_odds_ratio = numeric(0), n = integer(0),
      conditional_a_given_b = numeric(0),
      conditional_b_given_a = numeric(0), n_samples = integer(0),
      stringsAsFactors = FALSE
    )
  )
  expect_s3_class(plot_crosstalk(empty, type = "patterns"), "ggplot")
  expect_s3_class(plot_crosstalk(empty, type = "cooccurrence"), "ggplot")
})
