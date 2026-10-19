##### HistoneMod: Site-level Analysis #####
# Parsing of Skyline peptide modifications into a long site table, per-site
# intensity-based occupancy and combinatorial modification cross-talk analysis.
#
# These functions form the site-level analysis layer (HistoneMod 0.8). They
# only use base R plus the packages that HistoneMod already imports
# (ggplot2, viridis); no new hard dependency is introduced.

# Column names used inside ggplot2 non-standard evaluation in this file.
# Declaring them here keeps R CMD check free of "no visible binding" notes.
utils::globalVariables(c(
  "site", "mean_occupancy", "se_occupancy", "pattern", "group",
  "site_a", "site_b", "log_odds_ratio", "label"
))

# ------------------------- Internal helpers -------------------------------

# Default UniMod -> histone mark mapping used by parse_peptide_sites().
# UniMod 58 (propionyl) is a derivatisation tag used by several Skyline
# workflows to improve ionisation of unmodified lysines; it is mapped so that
# it can be recognised, but it is not treated as a histone mark.
.hm_default_unimod_map <- function() {
  data.frame(
    unimod = c(1, 21, 34, 36, 37, 58),
    mod = c("ac", "ph", "me1", "me2", "me3", "propionyl"),
    stringsAsFactors = FALSE
  )
}

# Build the effective UniMod map: the package default extended (and overridden
# on duplicated ids) by a user-supplied data.frame or named character vector.
.hm_unimod_map <- function(unimod_map = NULL) {
  base <- .hm_default_unimod_map()

  if (is.null(unimod_map)) {
    return(base)
  }

  if (is.data.frame(unimod_map)) {
    if (!all(c("unimod", "mod") %in% names(unimod_map))) {
      stop(
        "`unimod_map` must be a data.frame with columns 'unimod' and 'mod'.",
        call. = FALSE
      )
    }
    extra <- data.frame(
      unimod = as.character(unimod_map$unimod),
      mod = as.character(unimod_map$mod),
      stringsAsFactors = FALSE
    )
  } else if (is.character(unimod_map) && !is.null(names(unimod_map))) {
    extra <- data.frame(
      unimod = as.character(names(unimod_map)),
      mod = as.character(unname(unimod_map)),
      stringsAsFactors = FALSE
    )
  } else {
    stop(
      "`unimod_map` must be a data.frame with columns 'unimod' and 'mod' ",
      "or a named character vector.",
      call. = FALSE
    )
  }

  ok <- !is.na(extra$unimod) & nzchar(trimws(extra$unimod)) &
    !is.na(extra$mod) & nzchar(trimws(extra$mod))
  extra <- extra[ok, , drop = FALSE]
  extra$unimod <- as.character(suppressWarnings(as.numeric(extra$unimod)))
  extra <- extra[!is.na(extra$unimod), , drop = FALSE]
  extra$mod <- trimws(extra$mod)

  keep <- !as.character(base$unimod) %in% extra$unimod
  combined <- rbind(
    data.frame(
      unimod = as.character(base$unimod[keep]),
      mod = as.character(base$mod[keep]),
      stringsAsFactors = FALSE
    ),
    extra
  )
  combined[!duplicated(combined$unimod), , drop = FALSE]
}

# Expand a repeated modification token, e.g. "unun" -> c("un", "un") and
# "acacacac" -> four "ac" tokens. Tokens that are not an exact repetition of a
# shorter motif are returned unchanged.
.hm_expand_mod_token <- function(token) {
  token <- as.character(token)[1]
  if (is.na(token) || !nzchar(token)) {
    return(character(0))
  }
  n <- nchar(token)
  if (n >= 4L) {
    for (p in seq_len(floor(n / 2))) {
      if (n %% p != 0L) next
      base <- substr(token, 1L, p)
      if (identical(paste(rep(base, n / p), collapse = ""), token)) {
        return(rep(base, n / p))
      }
    }
  }
  token
}

# Split the text following a site token into individual modification tokens.
# Concatenated and repeated tokens are supported: "me3ac" -> c("me3", "ac"),
# "unun" -> c("un", "un"), "acacacac" -> four "ac" tokens.
.hm_split_mod_tokens <- function(tail) {
  if (is.na(tail) || !nzchar(tail)) {
    return(character(0))
  }
  parts <- unlist(strsplit(tail, "_", fixed = TRUE), use.names = FALSE)
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) {
    return(character(0))
  }
  out <- character(0)
  for (part in parts) {
    tokens <- regmatches(part, gregexpr("[a-z]+[0-9]*", part, perl = TRUE))[[1L]]
    if (length(tokens) == 0L) tokens <- part
    for (token in tokens) {
      out <- c(out, .hm_expand_mod_token(token))
    }
  }
  out
}

# Parse the SITES part of a Peptide.Note string.
# Grammar: one or more uppercase residue + integer position tokens, each
# optionally followed by a modification token, either inline ("K9me3K14ac") or
# after an underscore ("K8K14_unun", "K4_me3"). A trailing modification token
# may be repeated to describe several sites ("unun", "acacacac").
# Returns a data.frame with residue, position (as written) and mod.
.hm_parse_note_sites <- function(sites_str) {
  empty <- data.frame(
    residue = character(0),
    position = numeric(0),
    mod = character(0),
    stringsAsFactors = FALSE
  )
  if (length(sites_str) != 1L || is.na(sites_str) || !nzchar(sites_str)) {
    return(empty)
  }

  matches <- gregexpr("[A-Z][0-9]+", sites_str, perl = TRUE)[[1]]
  if (length(matches) == 1L && matches[1] == -1L) {
    return(empty)
  }
  widths <- attr(matches, "match.length")
  n_sites <- length(matches)
  tokens <- substring(sites_str, matches, matches + widths - 1L)
  residues <- substr(tokens, 1L, 1L)
  positions <- suppressWarnings(as.numeric(substring(tokens, 2L)))
  positions[!is.finite(positions)] <- NA_real_

  tails <- character(n_sites)
  for (i in seq_len(n_sites)) {
    tail_start <- matches[i] + widths[i]
    tail_end <- if (i < n_sites) matches[i + 1L] - 1L else nchar(sites_str)
    tails[i] <- if (tail_start <= tail_end) {
      substr(sites_str, tail_start, tail_end)
    } else {
      ""
    }
    tails[i] <- gsub("^_+|_+$", "", tails[i])
  }

  mods <- vector("list", n_sites)
  if (n_sites > 1L) {
    for (i in seq_len(n_sites - 1L)) {
      if (nzchar(tails[i])) {
        toks <- .hm_split_mod_tokens(tails[i])
        if (length(toks) > 0L) mods[[i]] <- toks[1L]
      }
    }
  }

  last_tokens <- .hm_split_mod_tokens(tails[n_sites])
  unassigned <- which(vapply(mods, is.null, logical(1)))
  if (length(last_tokens) > 0L && length(unassigned) > 0L) {
    if (length(last_tokens) == length(unassigned)) {
      for (k in seq_along(unassigned)) mods[[unassigned[k]]] <- last_tokens[k]
    } else if (length(last_tokens) == 1L) {
      for (k in seq_along(unassigned)) mods[[unassigned[k]]] <- last_tokens[1L]
    } else {
      unique_tokens <- unique(last_tokens)
      if (length(unique_tokens) == 1L) {
        for (k in seq_along(unassigned)) mods[[unassigned[k]]] <- unique_tokens[1L]
      } else {
        for (k in seq_len(min(length(last_tokens), length(unassigned)))) {
          mods[[unassigned[k]]] <- last_tokens[k]
        }
      }
    }
  }

  mods <- vapply(
    mods,
    function(x) if (is.null(x) || length(x) == 0L || is.na(x[1L])) "un" else x[1L],
    character(1)
  )

  data.frame(
    residue = residues,
    position = positions,
    mod = mods,
    stringsAsFactors = FALSE
  )
}

# Parse a Peptide.Note string. Returns protein, start, end, the parsed site
# table, and whether the note followed either supported grammar.
.hm_parse_peptide_note <- function(note) {
  out <- list(
    protein = NA_character_,
    start = NA_real_,
    end = NA_real_,
    matched = FALSE,
    sites = data.frame(
      residue = character(0),
      position = numeric(0),
      mod = character(0),
      stringsAsFactors = FALSE
    )
  )
  if (length(note) != 1L || is.na(note) || !nzchar(trimws(note))) {
    return(out)
  }
  note <- trimws(note)

  strict <- regmatches(note, regexec("^([^_]+)_([0-9]+)_([0-9]+)_?(.*)$", note))[[1]]
  if (length(strict) == 5L) {
    out$protein <- strict[2L]
    out$start <- as.numeric(strict[3L])
    out$end <- as.numeric(strict[4L])
    out$sites <- .hm_parse_note_sites(strict[5L])
    out$matched <- TRUE
    return(out)
  }

  loose <- regmatches(note, regexec("^([^_]+)_(.+)$", note))[[1]]
  if (length(loose) == 3L) {
    sites <- .hm_parse_note_sites(loose[3L])
    if (nrow(sites) > 0L) {
      out$protein <- loose[2L]
      out$sites <- sites
      out$matched <- TRUE
    }
  }
  out
}

# Canonical protein prefix used in site labels (H3, H4, H3.3, ...).
.hm_protein_prefix <- function(protein_name, note_protein = NA_character_) {
  if (length(note_protein) == 1L && !is.na(note_protein) && nzchar(note_protein)) {
    return(trimws(note_protein))
  }
  x <- as.character(protein_name)[1]
  if (is.na(x) || !nzchar(trimws(x))) {
    return(NA_character_)
  }
  x <- trimws(x)
  stripped <- sub("[_-][0-9]+[_-][0-9]+$", "", x)
  if (nzchar(stripped)) stripped else x
}

# Walk a modified sequence such as "TK(unimod:37)QTAR" and return one row per
# modification with the residue, its 1-based peptide index and the UniMod id.
.hm_parse_unimod_sequence <- function(x) {
  empty <- data.frame(
    residue = character(0),
    peptide_index = numeric(0),
    unimod = numeric(0),
    stringsAsFactors = FALSE
  )
  if (length(x) != 1L || is.na(x) || !nzchar(x)) {
    return(empty)
  }
  chars <- strsplit(x, "", fixed = TRUE)[[1L]]
  n <- length(chars)
  if (n == 0L) {
    return(empty)
  }

  residues <- character(0)
  indices <- numeric(0)
  ids <- numeric(0)
  aa_index <- 0
  last_residue <- NA_character_
  last_index <- NA_real_
  i <- 1L
  while (i <= n) {
    ch <- chars[i]
    if (identical(ch, "(")) {
      close <- which(chars[i:n] == ")")
      if (length(close) == 0L) {
        i <- i + 1L
        next
      }
      close <- close[1L] + i - 1L
      inside <- if (close > i + 1L) {
        paste0(chars[(i + 1L):(close - 1L)], collapse = "")
      } else {
        ""
      }
      found <- regmatches(
        inside,
        gregexpr("[Uu][Nn][Ii][Mm][Oo][Dd]:[0-9]+", inside, perl = TRUE)
      )[[1L]]
      if (length(found) > 0L) {
        for (token in found) {
          ids <- c(ids, suppressWarnings(as.numeric(sub("^[^:]*:", "", token))))
          residues <- c(residues, last_residue)
          indices <- c(indices, last_index)
        }
      }
      i <- close + 1L
    } else if (grepl("^[A-Za-z]$", ch)) {
      aa_index <- aa_index + 1
      last_residue <- toupper(ch)
      last_index <- aa_index
      i <- i + 1L
    } else {
      i <- i + 1L
    }
  }

  data.frame(
    residue = residues,
    peptide_index = indices,
    unimod = ids,
    stringsAsFactors = FALSE
  )
}

# Pick an explicit column name or the first of several candidates. All site
# parsing columns are optional: an absent column is reported as NA and the
# corresponding annotation source is simply unavailable.
.hm_pick_col <- function(data, col, candidates = character(0), label = "column") {
  if (!is.null(col) && length(col) == 1L && !is.na(col) && nzchar(col) &&
      col %in% names(data)) {
    return(col)
  }
  hits <- candidates[candidates %in% names(data)]
  if (length(hits) > 0L) hits[1L] else NA_character_
}

# Column values or a vector of NA when the column is absent.
.hm_col_or_na <- function(data, col, n) {
  if (length(col) != 1L || is.na(col) || !col %in% names(data)) {
    return(rep(NA, n))
  }
  data[[col]]
}

# First finite scalar of the supplied values.
.hm_first_finite <- function(...) {
  values <- list(...)
  for (value in values) {
    if (length(value) == 1L && !is.na(value) && is.finite(value)) {
      return(as.numeric(value))
    }
  }
  NA_real_
}

# Empty long site table with the documented column layout.
.hm_empty_site_table <- function() {
  data.frame(
    row_id = integer(0),
    Protein.Name = character(0),
    Peptide.Sequence = character(0),
    Peptide.Note = character(0),
    start = numeric(0),
    end = numeric(0),
    residue = character(0),
    position = numeric(0),
    site = character(0),
    mod = character(0),
    is_modified = logical(0),
    is_unmodified = logical(0),
    position_type = character(0),
    protein = character(0),
    peptide_index = numeric(0),
    unimod = numeric(0),
    matched = logical(0),
    is_derivatisation = logical(0),
    source = character(0),
    stringsAsFactors = FALSE
  )
}

# One row of the long site table.
.hm_site_record <- function(row_id, protein_name, sequence, note, start, end,
                            residue, position, site, mod, is_modified,
                            is_unmodified, position_type, protein,
                            peptide_index, unimod, matched, is_derivatisation,
                            source) {
  data.frame(
    row_id = as.integer(row_id),
    Protein.Name = as.character(protein_name),
    Peptide.Sequence = as.character(sequence),
    Peptide.Note = as.character(note),
    start = as.numeric(start),
    end = as.numeric(end),
    residue = as.character(residue),
    position = as.numeric(position),
    site = as.character(site),
    mod = as.character(mod),
    is_modified = as.logical(is_modified),
    is_unmodified = as.logical(is_unmodified),
    position_type = as.character(position_type),
    protein = as.character(protein),
    peptide_index = as.numeric(peptide_index),
    unimod = as.numeric(unimod),
    matched = as.logical(matched),
    is_derivatisation = as.logical(is_derivatisation),
    source = as.character(source),
    stringsAsFactors = FALSE
  )
}

# Site label from protein prefix, residue and (protein or peptide) position.
.hm_site_label <- function(protein, residue, position) {
  if (is.na(protein) || !nzchar(protein) ||
      is.na(residue) || !nzchar(residue) || is.na(position)) {
    return(NA_character_)
  }
  paste0(protein, residue, format(position, scientific = FALSE, trim = TRUE))
}

# Core parser: one input row can produce zero or more long site rows, but every
# input row produces at least one row (an unmodified placeholder) so that the
# long table covers the full intensity denominator.
.hm_parse_site_table <- function(ms1, unimod_map, derivatisation_mods,
                                 protein_col, sequence_col, note_col,
                                 modified_col, start_col, end_col) {
  n <- nrow(ms1)
  map <- .hm_unimod_map(unimod_map)
  map_ids <- as.character(map$unimod)
  map_mods <- as.character(map$mod)

  derivatisation_mods <- tolower(trimws(as.character(derivatisation_mods)))
  derivatisation_mods <- derivatisation_mods[
    !is.na(derivatisation_mods) & nzchar(derivatisation_mods)
  ]

  protein_values <- as.character(.hm_col_or_na(ms1, protein_col, n))
  sequence_values <- as.character(.hm_col_or_na(ms1, sequence_col, n))
  note_values <- as.character(.hm_col_or_na(ms1, note_col, n))
  modified_values <- as.character(.hm_col_or_na(ms1, modified_col, n))
  start_values <- suppressWarnings(as.numeric(.hm_col_or_na(ms1, start_col, n)))
  end_values <- suppressWarnings(as.numeric(.hm_col_or_na(ms1, end_col, n)))

  records <- vector("list", n)
  for (i in seq_len(n)) {
    note_info <- .hm_parse_peptide_note(note_values[i])
    start <- .hm_first_finite(start_values[i], note_info$start)
    end <- .hm_first_finite(end_values[i], note_info$end)
    if (is.na(end) && !is.na(start) && !is.na(sequence_values[i]) &&
        nzchar(sequence_values[i])) {
      end <- start + nchar(sequence_values[i]) - 1
    }
    prefix <- .hm_protein_prefix(
      protein_values[i],
      if (isTRUE(note_info$matched)) note_info$protein else NA_character_
    )
    if (is.na(prefix)) prefix <- "unknown"

    tokens <- if (!is.na(modified_values[i]) && nzchar(trimws(modified_values[i]))) {
      .hm_parse_unimod_sequence(modified_values[i])
    } else {
      .hm_parse_unimod_sequence(NA_character_)
    }

    rows <- list()

    if (nrow(tokens) > 0L) {
      for (j in seq_len(nrow(tokens))) {
        id <- tokens$unimod[j]
        hit <- match(as.character(id), map_ids)
        matched <- !is.na(hit)
        mod <- if (matched) map_mods[hit] else paste0("unimod:", id)
        is_deriv <- matched && tolower(mod) %in% derivatisation_mods
        is_mod <- !is_deriv

        residue <- tokens$residue[j]
        peptide_index <- tokens$peptide_index[j]
        if (is.na(peptide_index)) {
          position <- NA_real_
          position_type <- NA_character_
        } else if (!is.na(start)) {
          position <- start + peptide_index - 1
          position_type <- "protein"
        } else {
          position <- peptide_index
          position_type <- "peptide"
        }

        rows[[length(rows) + 1L]] <- .hm_site_record(
          row_id = i,
          protein_name = protein_values[i],
          sequence = sequence_values[i],
          note = note_values[i],
          start = start,
          end = end,
          residue = residue,
          position = position,
          site = .hm_site_label(prefix, residue, position),
          mod = mod,
          is_modified = is_mod,
          is_unmodified = !is_mod,
          position_type = position_type,
          protein = prefix,
          peptide_index = peptide_index,
          unimod = id,
          matched = matched,
          is_derivatisation = is_deriv,
          source = "unimod"
        )
      }
    } else if (nrow(note_info$sites) > 0L) {
      note_start <- note_info$start
      for (j in seq_len(nrow(note_info$sites))) {
        residue <- note_info$sites$residue[j]
        token_position <- note_info$sites$position[j]
        mod <- tolower(trimws(note_info$sites$mod[j]))
        is_un <- is.na(mod) || !nzchar(mod) || grepl("^un+$", mod)
        is_deriv <- !is_un && mod %in% derivatisation_mods
        is_mod <- !is_un && !is_deriv

        if (is.na(token_position)) {
          position <- NA_real_
          peptide_index <- NA_real_
          position_type <- NA_character_
        } else if (!is.na(note_start)) {
          # Positions in a ranged note are absolute protein positions.
          position <- token_position
          peptide_index <- token_position - note_start + 1
          position_type <- "protein"
        } else if (!is.na(start)) {
          # No range in the note: the token is peptide-relative.
          position <- start + token_position - 1
          peptide_index <- token_position
          position_type <- "protein"
        } else {
          position <- token_position
          peptide_index <- token_position
          position_type <- "peptide"
        }

        rows[[length(rows) + 1L]] <- .hm_site_record(
          row_id = i,
          protein_name = protein_values[i],
          sequence = sequence_values[i],
          note = note_values[i],
          start = start,
          end = end,
          residue = residue,
          position = position,
          site = .hm_site_label(prefix, residue, position),
          mod = if (is_un) "un" else mod,
          is_modified = is_mod,
          is_unmodified = !is_mod,
          position_type = position_type,
          protein = prefix,
          peptide_index = peptide_index,
          unimod = NA_real_,
          matched = TRUE,
          is_derivatisation = is_deriv,
          source = "note"
        )
      }
    }

    if (length(rows) == 0L) {
      rows[[1L]] <- .hm_site_record(
        row_id = i,
        protein_name = protein_values[i],
        sequence = sequence_values[i],
        note = note_values[i],
        start = start,
        end = end,
        residue = NA_character_,
        position = NA_real_,
        site = NA_character_,
        mod = "un",
        is_modified = FALSE,
        is_unmodified = TRUE,
        position_type = NA_character_,
        protein = prefix,
        peptide_index = NA_real_,
        unimod = NA_real_,
        matched = TRUE,
        is_derivatisation = FALSE,
        source = "none"
      )
    }

    records[[i]] <- do.call(rbind, rows)
  }

  if (n == 0L) {
    return(.hm_empty_site_table())
  }
  out <- do.call(rbind, records)
  rownames(out) <- NULL
  out
}

# Empty occupancy table with the documented column layout.
.hm_empty_occupancy <- function() {
  data.frame(
    Replicate.Name = character(0),
    Group = character(0),
    Protein.Name = character(0),
    site = character(0),
    residue = character(0),
    position = numeric(0),
    backbone = character(0),
    occupancy = numeric(0),
    n_states = integer(0),
    total_intensity = numeric(0),
    complete_backbone = logical(0),
    note = character(0),
    reason = character(0),
    position_type = character(0),
    stringsAsFactors = FALSE
  )
}

# Empty cross-talk patterns/sites tables.
.hm_empty_patterns <- function() {
  data.frame(
    sample = character(0),
    group = character(0),
    protein = character(0),
    pattern = character(0),
    sites = character(0),
    frequency = numeric(0),
    group_frequency = numeric(0),
    n_sites = integer(0),
    n_samples = integer(0),
    stringsAsFactors = FALSE
  )
}

.hm_empty_cooccurrence <- function() {
  data.frame(
    group = character(0),
    protein = character(0),
    site_a = character(0),
    site_b = character(0),
    a = numeric(0),
    b = numeric(0),
    c = numeric(0),
    d = numeric(0),
    log_odds_ratio = numeric(0),
    n = integer(0),
    conditional_a_given_b = numeric(0),
    conditional_b_given_a = numeric(0),
    n_samples = integer(0),
    stringsAsFactors = FALSE
  )
}

.hm_empty_site_frequencies <- function() {
  data.frame(
    sample = character(0),
    group = character(0),
    protein = character(0),
    site = character(0),
    frequency = numeric(0),
    group_frequency = numeric(0),
    n_samples = integer(0),
    stringsAsFactors = FALSE
  )
}

# Sort site labels such as "H4K5ac" by protein prefix, residue position and
# label. Labels without a parseable position are placed last.
.hm_order_site_labels <- function(labels) {
  labels <- as.character(labels)
  position <- suppressWarnings(as.numeric(
    sub("^.*[A-Za-z]([0-9]+)[^0-9]*$", "\\1", labels, perl = TRUE)
  ))
  order(is.na(position), position, labels)
}

# Per-row backbone metadata: protein prefix, start/end, sequence, the site span
# written in the note and a human-readable backbone label plus an internal key.
.hm_row_backbone_meta <- function(parsed, ms1) {
  n <- nrow(ms1)
  ids <- seq_len(n)
  groups <- split(parsed, factor(parsed$row_id, levels = ids))

  first_non_na <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0L) NA else x[1L]
  }

  note_protein <- vapply(groups, function(d) {
    first_non_na(d$protein)
  }, character(1))
  start <- vapply(groups, function(d) {
    as.numeric(first_non_na(d$start))
  }, numeric(1))
  end <- vapply(groups, function(d) {
    as.numeric(first_non_na(d$end))
  }, numeric(1))
  span <- vapply(groups, function(d) {
    ok <- !is.na(d$residue) & !is.na(d$position)
    if (!any(ok)) return("unknown")
    paste(unique(paste0(d$residue[ok], d$position[ok])), collapse = "_")
  }, character(1))

  sequence <- as.character(.hm_col_or_na(ms1, "Peptide.Sequence", n))
  protein_name <- as.character(.hm_col_or_na(ms1, "Protein.Name", n))

  prefix <- note_protein
  needs_prefix <- is.na(prefix) | !nzchar(prefix)
  if (any(needs_prefix)) {
    prefix[needs_prefix] <- vapply(
      protein_name[needs_prefix],
      .hm_protein_prefix,
      character(1),
      note_protein = NA_character_
    )
  }
  prefix[is.na(prefix) | !nzchar(prefix)] <- "unknown"

  sequence_clean <- ifelse(is.na(sequence), "", sequence)
  has_range <- !is.na(start) & !is.na(end)
  has_sequence <- nzchar(sequence_clean)

  label <- ifelse(
    has_range,
    paste0(prefix, "_", start, "-", end),
    ifelse(has_sequence, paste0(prefix, "_", sequence_clean),
           paste0(prefix, "_", span))
  )
  key <- ifelse(
    has_range,
    paste(prefix, start, end, sequence_clean, sep = "|"),
    label
  )

  data.frame(
    row_id = ids,
    protein_prefix = prefix,
    start = start,
    end = end,
    sequence = sequence_clean,
    span = span,
    backbone = label,
    backbone_key = key,
    stringsAsFactors = FALSE
  )
}

# ------------------------- Exported functions -----------------------------

#' Parse Skyline peptide modifications into a long site table
#'
#' Converts modification annotation in an MS1 peptide table into one long-format
#' row per peptide state x modification, which is the input used by
#' [compute_site_occupancy()] and [compute_crosstalk()].
#'
#' Two annotation sources are supported, tried in this order:
#' \enumerate{
#'   \item the Skyline `Peptide.Modified.Sequence.Unimod.Ids` column (for
#'     example `"TK(unimod:37)QTAR"`). UniMod identifiers are unambiguous and
#'     are mapped with `unimod_map`;
#'   \item the `Peptide.Note` grammar `PROTEIN_START_END_SITES` (for example
#'     `"H3_3_8_K4_me3"`, `"H3_9_17_K8K14_unun"` or
#'     `"H4_4_17_K5K8_acac"`), used when the UniMod column is absent or empty
#'     for a row.
#' }
#'
#' Default UniMod mapping: 1 = acetyl (`ac`), 21 = phospho (`ph`), 34 =
#' methyl (`me1`), 36 = dimethyl (`me2`), 37 = trimethyl (`me3`) and 58 =
#' propionyl. Propionyl is a derivatisation tag, not a histone mark: a state
#' carrying only derivatisation tags is reported with `is_modified = FALSE`,
#' `is_unmodified = TRUE` and `is_derivatisation = TRUE`. Unknown UniMod ids are
#' kept with `mod = "unimod:<id>"`, `matched = FALSE` and are counted as
#' modifications. A `unimod_map` data.frame can extend or override the defaults.
#'
#' `Peptide.Note` grammar details: `PROTEIN` is everything before the first
#' underscore (it may contain dots, e.g. `H3.3`), `START`/`END` are the first
#' and last protein position of the peptide, and `SITES` contains one or more
#' residue + position tokens (e.g. `K4`, `K8K14`), each optionally followed by a
#' modification token, either inline (`K9me3K14ac`) or after an underscore
#' (`K4_me3`). A trailing modification token may be repeated to describe several
#' sites (`K8K14_unun`, `K5K8_acac`). `un` means unmodified.
#'
#' Positions are taken from `Peptide.Start`/`Peptide.End` (or `Start`/`End`)
#' columns when present, otherwise from the `START`/`END` part of `Peptide.Note`,
#' otherwise they stay peptide-relative. The `position_type` column records which
#' convention was used (`"protein"` or `"peptide"`). `site` labels are built from
#' the protein prefix found in the note (or derived from `Protein.Name` by
#' stripping a trailing range such as `_3-8`), the residue and the position, e.g.
#' `H3K4`.
#'
#' @param ms1 A data.frame with peptide-level MS1 measurements. Column names are
#'   normalised (whitespace to dots) before use.
#' @param unimod_map Optional data.frame with columns `unimod` and `mod`
#'   extending/overriding the default UniMod mapping. A named character vector
#'   (names are UniMod ids) is also accepted.
#' @param derivatisation_mods Character vector of modification names that are
#'   derivatisation tags rather than histone marks. Defaults to `"propionyl"`.
#' @param protein_col,sequence_col,note_col,modified_col Column names for the
#'   protein name, unmodified peptide sequence, `Peptide.Note` and UniMod id
#'   columns. Columns that are absent are treated as missing (for
#'   `sequence_col` and `modified_col` this simply disables that source).
#' @param start_col,end_col Optional column names with the peptide start/end
#'   protein positions. When `NULL` (the default) `"Peptide.Start"`/`"Start"`
#'   and `"Peptide.End"`/`"End"` are used when present.
#'
#' @return A long data.frame with columns `row_id` (1-based row index in `ms1`),
#'   `Protein.Name`, `Peptide.Sequence`, `Peptide.Note`, `start`, `end`,
#'   `residue`, `position`, `site`, `mod`, `is_modified`, `is_unmodified`,
#'   `position_type`, `protein` (site prefix), `peptide_index`, `unimod`,
#'   `matched`, `is_derivatisation` and `source` (`"unimod"`, `"note"` or
#'   `"none"`). Every input row contributes at least one output row; a state
#'   without usable modification annotation yields one unmodified row with
#'   `mod = "un"` and missing `residue`/`position`/`site`.
#'
#' @examples
#' \dontrun{
#' ms1 <- read_skyline_csv("MS1.csv")
#' sites <- parse_peptide_sites(ms1)
#' head(sites)
#' }
#' @export
parse_peptide_sites <- function(ms1,
                                unimod_map = NULL,
                                derivatisation_mods = "propionyl",
                                protein_col = "Protein.Name",
                                sequence_col = "Peptide.Sequence",
                                note_col = "Peptide.Note",
                                modified_col = "Peptide.Modified.Sequence.Unimod.Ids",
                                start_col = NULL,
                                end_col = NULL) {
  if (!is.data.frame(ms1)) {
    stop("`ms1` must be a data.frame.", call. = FALSE)
  }
  ms1 <- .hm_normalise_table(ms1)
  if (nrow(ms1) == 0L) {
    return(.hm_empty_site_table())
  }

  protein_col <- .hm_pick_col(ms1, protein_col, label = "protein_col")
  sequence_col <- .hm_pick_col(ms1, sequence_col, label = "sequence_col")
  note_col <- .hm_pick_col(ms1, note_col, label = "note_col")
  modified_col <- .hm_pick_col(ms1, modified_col, label = "modified_col")
  start_col <- .hm_pick_col(
    ms1, start_col, candidates = c("Peptide.Start", "Start"),
    label = "start_col"
  )
  end_col <- .hm_pick_col(
    ms1, end_col, candidates = c("Peptide.End", "End"),
    label = "end_col"
  )

  .hm_parse_site_table(
    ms1 = ms1,
    unimod_map = unimod_map,
    derivatisation_mods = derivatisation_mods,
    protein_col = protein_col,
    sequence_col = sequence_col,
    note_col = note_col,
    modified_col = modified_col,
    start_col = start_col,
    end_col = end_col
  )
}

#' Compute intensity-based occupancy per histone modification site
#'
#' Estimates how much of a peptide backbone carries a given modification. For
#' every light-labelled peptide state the area in `Total.Area.MS1` is used. A
#' backbone is the combination of `Protein.Name`, `Peptide.Sequence`,
#' `start` and `end`; all states of a backbone measured in the same replicate
#' form the intensity denominator, so unmodified states are included. For each
#' site covered by the backbone, `occupancy` is the summed intensity of the
#' states carrying that site as a modification divided by the total backbone
#' intensity. A peptide state carrying several modifications contributes to each
#' of its sites.
#'
#' Sites are parsed with [parse_peptide_sites()], so both UniMod annotation and
#' the `Peptide.Note` grammar are supported. Derivatisation tags (for example
#' propionyl) count as unmodified states, not as histone marks.
#'
#' When a backbone has no measured unmodified state the occupancy is still
#' computed from the measured states, but `complete_backbone` is `FALSE` and
#' `note` explains that unmeasured states are assumed to be zero. Set
#' `require_unmodified = TRUE` to instead return `NA` occupancy (with `reason`)
#' for such backbones. Missing or non-positive total intensities also yield `NA`
#' occupancy with a `reason`.
#'
#' With `backbone_detail = FALSE` the estimates of all backbones covering the
#' same protein and site are combined per replicate with an
#' intensity-weighted mean, and `n_backbones` reports how many backbones
#' contributed. With `backbone_detail = TRUE` (the default) one row per
#' replicate x backbone x site is returned.
#'
#' @param ms1 A data.frame with peptide-level MS1 measurements. Must contain
#'   `Replicate.Name`, `Protein.Name`, `Peptide.Note` and `area_col`; an
#'   `Isotope.Label.Type` column is used to keep light-labelled rows only.
#' @param sample A data.frame with sample metadata, containing `Replicate.Name`
#'   and `Group`.
#' @param backbone_detail Logical; when `TRUE` (default) return one row per
#'   replicate x backbone x site, when `FALSE` aggregate backbones covering the
#'   same site with an intensity-weighted mean.
#' @param require_unmodified Logical; when `TRUE`, occupancy is `NA` for
#'   backbones without a measured unmodified state. Defaults to `FALSE`.
#' @param area_col Name of the numeric area column (default
#'   `"Total.Area.MS1"`).
#' @param unimod_map Optional UniMod extension map passed to
#'   [parse_peptide_sites()].
#'
#' @return A data.frame with columns `Replicate.Name`, `Group`, `Protein.Name`,
#'   `site`, `residue`, `position`, `backbone`, `occupancy` (0-1),
#'   `n_states`, `total_intensity`, `complete_backbone`, `note`, `reason` and
#'   `position_type`. With `backbone_detail = FALSE` the result instead has one
#'   row per replicate x protein x site, `backbone` is `NA` and an extra
#'   `n_backbones` column gives the number of backbones that contributed.
#'
#' @examples
#' \dontrun{
#' tables <- list(
#'   ms1 = read_skyline_csv("MS1.csv"),
#'   sample = read_skyline_csv("samples.csv")
#' )
#' occ <- compute_site_occupancy(tables$ms1, tables$sample)
#' }
#' @export
compute_site_occupancy <- function(ms1, sample,
                                   backbone_detail = TRUE,
                                   require_unmodified = FALSE,
                                   area_col = "Total.Area.MS1",
                                   unimod_map = NULL) {
  backbone_detail <- isTRUE(backbone_detail)
  require_unmodified <- isTRUE(require_unmodified)

  if (!is.data.frame(ms1)) {
    stop("`ms1` must be a data.frame.", call. = FALSE)
  }
  if (!is.data.frame(sample)) {
    stop("`sample` must be a data.frame.", call. = FALSE)
  }
  ms1 <- .hm_normalise_table(ms1)
  sample <- .hm_normalise_table(sample)

  required_ms1 <- c("Replicate.Name", "Protein.Name", "Peptide.Note", area_col)
  missing_ms1 <- setdiff(required_ms1, names(ms1))
  if (length(missing_ms1) > 0L) {
    stop(
      "`ms1` is missing required column(s): ",
      paste(missing_ms1, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (!"Replicate.Name" %in% names(sample)) {
    stop("`sample` must contain a 'Replicate.Name' column.", call. = FALSE)
  }
  if (!"Group" %in% names(sample)) {
    stop("`sample` must contain a 'Group' column.", call. = FALSE)
  }

  sample$Replicate.Name <- as.character(sample$Replicate.Name)
  sample$Group <- as.character(sample$Group)
  sample <- sample[!duplicated(sample$Replicate.Name), , drop = FALSE]
  group_map <- stats::setNames(sample$Group, sample$Replicate.Name)

  if ("Isotope.Label.Type" %in% names(ms1)) {
    isotope <- tolower(trimws(as.character(ms1$Isotope.Label.Type)))
    ms1 <- ms1[!is.na(isotope) & isotope == "light", , drop = FALSE]
  }

  ms1$Replicate.Name <- as.character(ms1$Replicate.Name)
  valid <- !is.na(ms1$Replicate.Name) & nzchar(trimws(ms1$Replicate.Name))
  ms1 <- ms1[valid, , drop = FALSE]
  if (nrow(ms1) == 0L) {
    return(.hm_empty_occupancy())
  }
  area <- suppressWarnings(as.numeric(as.character(ms1[[area_col]])))

  parsed <- parse_peptide_sites(ms1, unimod_map = unimod_map)
  meta <- .hm_row_backbone_meta(parsed, ms1)

  original_protein <- as.character(ms1$Protein.Name)
  protein_out <- ifelse(
    is.na(original_protein) | !nzchar(trimws(original_protein)),
    meta$protein_prefix,
    original_protein
  )

  rows <- data.frame(
    row_id = seq_len(nrow(ms1)),
    Replicate.Name = ms1$Replicate.Name,
    Group = unname(group_map[ms1$Replicate.Name]),
    area = area,
    Protein.Name = protein_out,
    backbone = meta$backbone,
    backbone_key = meta$backbone_key,
    stringsAsFactors = FALSE
  )

  modified_key <- if ("Peptide.Modified.Sequence.Unimod.Ids" %in% names(ms1)) {
    as.character(ms1[["Peptide.Modified.Sequence.Unimod.Ids"]])
  } else {
    rep("", nrow(ms1))
  }
  note_key <- as.character(ms1$Peptide.Note)
  state_key <- paste(note_key, modified_key, sep = "|")
  bad_state <- (is.na(note_key) | !nzchar(trimws(note_key))) &
    (is.na(modified_key) | !nzchar(trimws(modified_key)))
  state_key[bad_state] <- paste0("row_", rows$row_id[bad_state])
  rows$state_key <- state_key

  modified_ids <- unique(parsed$row_id[parsed$is_modified])
  rows$modified_row <- rows$row_id %in% modified_ids
  rows$rkey <- paste(rows$Replicate.Name, rows$backbone_key, sep = "\r")

  sum_area <- function(values) {
    if (length(values) == 0L || all(is.na(values))) NA_real_ else
      sum(values, na.rm = TRUE)
  }

  totals <- stats::aggregate(
    rows$area,
    by = list(rkey = rows$rkey),
    FUN = sum_area
  )
  names(totals)[2L] <- "total_intensity"

  complete <- stats::aggregate(
    !rows$modified_row,
    by = list(rkey = rows$rkey),
    FUN = any
  )
  names(complete)[2L] <- "complete_backbone"

  n_states <- stats::aggregate(
    rows$state_key,
    by = list(rkey = rows$rkey),
    FUN = function(values) length(unique(values))
  )
  names(n_states)[2L] <- "n_states"

  site_rows <- parsed[
    !is.na(parsed$site),
    c("row_id", "site", "residue", "position", "position_type"),
    drop = FALSE
  ]
  site_rows <- merge(site_rows, rows[, c("row_id", "rkey")], by = "row_id")
  coverage <- unique(site_rows[, c("rkey", "site", "residue", "position",
                                   "position_type"), drop = FALSE])

  modified_rows <- parsed[
    parsed$is_modified & !is.na(parsed$site),
    c("row_id", "site"),
    drop = FALSE
  ]
  if (nrow(modified_rows) > 0L) {
    modified_rows <- merge(
      modified_rows,
      rows[, c("row_id", "rkey", "area")],
      by = "row_id"
    )
    numerator <- stats::aggregate(
      modified_rows$area,
      by = list(rkey = modified_rows$rkey, site = modified_rows$site),
      FUN = function(values) {
        if (length(values) == 0L || all(is.na(values))) 0 else
          sum(values, na.rm = TRUE)
      }
    )
    names(numerator)[3L] <- "modified_intensity"
  } else {
    numerator <- data.frame(
      rkey = character(0),
      site = character(0),
      modified_intensity = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  base <- unique(rows[, c("rkey", "Replicate.Name", "Group", "backbone",
                          "backbone_key", "Protein.Name"), drop = FALSE])
  out <- merge(coverage, base, by = "rkey")
  out$total_intensity <- totals$total_intensity[match(out$rkey, totals$rkey)]
  out$complete_backbone <- complete$complete_backbone[
    match(out$rkey, complete$rkey)
  ]
  out$n_states <- n_states$n_states[match(out$rkey, n_states$rkey)]

  num_key <- paste(numerator$rkey, numerator$site, sep = "\r")
  out$modified_intensity <- numerator$modified_intensity[
    match(paste(out$rkey, out$site, sep = "\r"), num_key)
  ]
  out$modified_intensity[is.na(out$modified_intensity)] <- 0

  valid_total <- !is.na(out$total_intensity) & out$total_intensity > 0
  out$occupancy <- rep(NA_real_, nrow(out))
  out$occupancy[valid_total] <- out$modified_intensity[valid_total] /
    out$total_intensity[valid_total]
  finite_occupancy <- !is.na(out$occupancy)
  out$occupancy[finite_occupancy] <- pmin(
    pmax(out$occupancy[finite_occupancy], 0), 1
  )

  out$reason <- rep(NA_character_, nrow(out))
  out$reason[!valid_total] <- "missing or non-positive total backbone intensity"
  if (require_unmodified) {
    incomplete <- !out$complete_backbone
    out$occupancy[incomplete] <- NA_real_
    out$reason[incomplete] <- "unmodified state of the backbone was not measured"
  }
  out$note <- rep(NA_character_, nrow(out))
  out$note[!out$complete_backbone] <- paste0(
    "Backbone has no measured unmodified state; occupancy assumes ",
    "unmeasured states are zero."
  )

  detail <- out[, c(
    "Replicate.Name", "Group", "Protein.Name", "site", "residue", "position",
    "backbone", "occupancy", "n_states", "total_intensity",
    "complete_backbone", "note", "reason", "position_type"
  ), drop = FALSE]

  if (backbone_detail) {
    ordering <- order(
      detail$Replicate.Name, detail$Protein.Name,
      detail$position, detail$site, detail$backbone
    )
    detail <- detail[ordering, , drop = FALSE]
    rownames(detail) <- NULL
    attr(detail, "backbone_detail") <- TRUE
    attr(detail, "area_col") <- area_col
    return(detail)
  }

  grouping_key <- paste(
    detail$Replicate.Name, detail$Group, detail$Protein.Name,
    detail$site, sep = "\r"
  )
  pieces <- split(detail, grouping_key)
  aggregated <- lapply(pieces, function(part) {
    valid <- !is.na(part$occupancy) & !is.na(part$total_intensity) &
      part$total_intensity > 0
    denominator <- sum(part$total_intensity[valid], na.rm = TRUE)
    occupancy <- if (denominator > 0) {
      sum(part$occupancy[valid] * part$total_intensity[valid],
          na.rm = TRUE) / denominator
    } else {
      NA_real_
    }
    total <- if (all(is.na(part$total_intensity))) NA_real_ else
      sum(part$total_intensity, na.rm = TRUE)
    incomplete <- any(!part$complete_backbone, na.rm = TRUE)
    data.frame(
      Replicate.Name = part$Replicate.Name[1L],
      Group = part$Group[1L],
      Protein.Name = part$Protein.Name[1L],
      site = part$site[1L],
      residue = part$residue[1L],
      position = part$position[1L],
      backbone = NA_character_,
      occupancy = occupancy,
      n_backbones = length(unique(part$backbone)),
      n_states = sum(part$n_states, na.rm = TRUE),
      total_intensity = total,
      complete_backbone = !incomplete,
      note = if (incomplete) {
        paste0(
          "At least one backbone has no measured unmodified state; ",
          "occupancy assumes unmeasured states are zero."
        )
      } else {
        NA_character_
      },
      reason = if (is.na(occupancy)) {
        if (denominator > 0) NA_character_ else
          "no backbone with a valid occupancy estimate"
      } else {
        NA_character_
      },
      position_type = part$position_type[1L],
      stringsAsFactors = FALSE
    )
  })
  aggregated <- do.call(rbind, aggregated)
  if (is.null(aggregated)) {
    aggregated <- .hm_empty_occupancy()
    aggregated$n_backbones <- integer(0)
  }
  ordering <- order(
    aggregated$Replicate.Name, aggregated$Protein.Name,
    aggregated$position, aggregated$site
  )
  aggregated <- aggregated[ordering, , drop = FALSE]
  rownames(aggregated) <- NULL
  aggregated <- aggregated[, c(
    "Replicate.Name", "Group", "Protein.Name", "site", "residue", "position",
    "backbone", "occupancy", "n_backbones", "n_states", "total_intensity",
    "complete_backbone", "note", "reason", "position_type"
  ), drop = FALSE]
  attr(aggregated, "backbone_detail") <- FALSE
  attr(aggregated, "area_col") <- area_col
  aggregated
}

#' Compute combinatorial modification patterns and site co-occurrence
#'
#' Derives, for every peptide state, the set of modified sites it carries and
#' aggregates the resulting modification patterns across samples and groups.
#' Site labels include the mark, e.g. `H4K5ac`; the pattern string sorts the
#' labels by protein position and joins them with `"|"` (for example
#' `"H4K5ac|H4K8ac"`). A state without any histone mark uses the pattern `"-"`.
#'
#' Intensities are weighted within each sample and protein: a row contributes
#' `Total.Area.MS1 / sum(Total.Area.MS1 of that protein in that sample)`. Pattern
#' frequencies therefore sum to one per sample x protein; `group_frequency` is
#' the mean frequency per group (samples without the pattern count as zero, so
#' `group_frequency` also sums to one per group x protein). Derivatisation tags
#' such as propionyl are not histone marks and do not create pattern labels.
#'
#' Co-occurrence is computed from the group-level pattern frequencies for every
#' pair of modified sites within the same protein: `a` = frequency of patterns
#' containing both sites, `b` = only the first, `c` = only the second,
#' `d` = neither. The log odds ratio uses the Haldane-Anscombe correction
#' (0.5 added to each cell), the conditional probabilities are
#' `P(site_a | site_b)` and `P(site_b | site_a)`, and `n` is the number of
#' samples in the group. Positive log odds ratios indicate co-occurrence,
#' negative values mutual exclusivity.
#'
#' @param data A data.frame with peptide-level MS1 measurements. It must contain
#'   `Replicate.Name`, `Protein.Name`, `Peptide.Note` and `area_col`, plus
#'   either a `Group` column or a `sample` table.
#' @param protein Optional single protein name (a value of `Protein.Name`) used
#'   to restrict the analysis.
#' @param sample Optional sample metadata data.frame with `Replicate.Name` and
#'   `Group`.
#' @param area_col Name of the numeric area column (default
#'   `"Total.Area.MS1"`).
#' @param unimod_map Optional UniMod extension map passed to
#'   [parse_peptide_sites()].
#'
#' @return A list with elements:
#'   \describe{
#'     \item{patterns}{per sample data.frame with `sample`, `group`, `protein`,
#'       `pattern`, `sites`, `frequency`, `group_frequency`, `n_sites` and
#'       `n_samples`.}
#'     \item{cooccurrence}{data.frame with `group`, `protein`, `site_a`,
#'       `site_b`, the 2x2 frequencies `a`, `b`, `c`, `d`,
#'       `log_odds_ratio`, `n`, `conditional_a_given_b`,
#'       `conditional_b_given_a` and `n_samples`.}
#'     \item{sites}{per sample marginal site frequencies with `sample`, `group`,
#'       `protein`, `site`, `frequency`, `group_frequency` and `n_samples`.}
#'     \item{settings}{list describing the analysis settings.}
#'   }
#'   Empty results (for example one site per protein) contain zero-row tables
#'   with the same columns.
#'
#' @examples
#' \dontrun{
#' tables <- list(
#'   ms1 = read_skyline_csv("MS1.csv"),
#'   sample = read_skyline_csv("samples.csv")
#' )
#' ct <- compute_crosstalk(tables$ms1, sample = tables$sample)
#' ct$patterns
#' }
#' @export
compute_crosstalk <- function(data, protein = NULL, sample = NULL,
                              area_col = "Total.Area.MS1",
                              unimod_map = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  data <- .hm_normalise_table(data)
  if (!"Replicate.Name" %in% names(data)) {
    stop("`data` must contain a 'Replicate.Name' column.", call. = FALSE)
  }

  if (!is.null(sample)) {
    if (!is.data.frame(sample)) {
      stop("`sample` must be a data.frame.", call. = FALSE)
    }
    sample <- .hm_normalise_table(sample)
    if (!"Replicate.Name" %in% names(sample)) {
      stop("`sample` must contain a 'Replicate.Name' column.", call. = FALSE)
    }
    if (!"Group" %in% names(sample)) {
      stop("`sample` must contain a 'Group' column.", call. = FALSE)
    }
    sample$Replicate.Name <- as.character(sample$Replicate.Name)
    sample$Group <- as.character(sample$Group)
    sample <- sample[!duplicated(sample$Replicate.Name), , drop = FALSE]
    data$Group <- sample$Group[
      match(as.character(data$Replicate.Name), sample$Replicate.Name)
    ]
  }

  required <- c("Replicate.Name", "Protein.Name", "Peptide.Note", "Group",
                area_col)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop(
      "`data` is missing required column(s): ",
      paste(missing, collapse = ", "),
      ". Supply a `sample` table with Replicate.Name and Group, or add a Group column.",
      call. = FALSE
    )
  }

  if (!is.null(protein)) {
    if (!is.character(protein) || length(protein) != 1L || is.na(protein) ||
        !nzchar(trimws(protein))) {
      stop("`protein` must be a single non-empty protein name.", call. = FALSE)
    }
    protein <- trimws(protein)
    available <- unique(as.character(data$Protein.Name))
    if (!protein %in% available) {
      stop(
        "Protein '", protein, "' was not found in `Protein.Name`. Available: ",
        paste(utils::head(available, 10L), collapse = ", "), ".",
        call. = FALSE
      )
    }
    data <- data[as.character(data$Protein.Name) == protein, , drop = FALSE]
  }

  light_only <- FALSE
  if ("Isotope.Label.Type" %in% names(data)) {
    isotope <- tolower(trimws(as.character(data$Isotope.Label.Type)))
    data <- data[!is.na(isotope) & isotope == "light", , drop = FALSE]
    light_only <- TRUE
  }

  data$Replicate.Name <- as.character(data$Replicate.Name)
  data$Group <- as.character(data$Group)
  data$Protein.Name <- as.character(data$Protein.Name)
  valid <- !is.na(data$Replicate.Name) & nzchar(trimws(data$Replicate.Name)) &
    !is.na(data$Group)
  data <- data[valid, , drop = FALSE]
  if (nrow(data) == 0L) {
    return(list(
      patterns = .hm_empty_patterns(),
      cooccurrence = .hm_empty_cooccurrence(),
      sites = .hm_empty_site_frequencies(),
      settings = list(
        proteins = character(0), groups = character(0), area_col = area_col,
        weighting = "row intensity / total protein intensity",
        pattern_separator = "|", light_only = light_only, n_samples = NULL
      )
    ))
  }
  data$Protein.Name[is.na(data$Protein.Name) | !nzchar(trimws(data$Protein.Name))] <- "unknown"
  area <- suppressWarnings(as.numeric(as.character(data[[area_col]])))

  n <- nrow(data)
  row_ids <- seq_len(n)

  totals_key <- paste(data$Replicate.Name, data$Protein.Name, sep = "\r")
  totals <- stats::aggregate(
    area,
    by = list(key = totals_key),
    FUN = function(values) {
      if (length(values) == 0L || all(is.na(values))) NA_real_ else
        sum(values, na.rm = TRUE)
    }
  )
  total_values <- totals[[2L]][match(totals_key, totals$key)]
  weight <- area / total_values
  # Rows whose protein total is missing or non-positive cannot be weighted and
  # are excluded; genuinely zero-area rows simply receive weight zero.
  usable <- !is.na(weight) & is.finite(weight)
  n_unusable <- sum(!usable)
  if (n_unusable > 0L) {
    warning(
      n_unusable, " row(s) with a missing or non-positive total protein ",
      "intensity were excluded from cross-talk frequencies.",
      call. = FALSE
    )
  }
  if (!any(usable)) {
    return(list(
      patterns = .hm_empty_patterns(),
      cooccurrence = .hm_empty_cooccurrence(),
      sites = .hm_empty_site_frequencies(),
      settings = list(
        proteins = character(0), groups = character(0), area_col = area_col,
        weighting = "row intensity / total protein intensity",
        pattern_separator = "|", light_only = light_only, n_samples = NULL
      )
    ))
  }

  parsed <- parse_peptide_sites(data, unimod_map = unimod_map)
  modified <- parsed[parsed$is_modified & !is.na(parsed$site), , drop = FALSE]
  if (nrow(modified) > 0L) {
    ordering <- order(
      modified$row_id,
      ifelse(is.na(modified$position), Inf, modified$position),
      modified$residue,
      modified$mod
    )
    modified <- modified[ordering, , drop = FALSE]
    labels <- paste0(modified$site, modified$mod)
    label_list <- split(labels, factor(modified$row_id, levels = row_ids))
  } else {
    label_list <- vector("list", n)
  }
  label_list <- lapply(label_list, function(values) {
    if (is.null(values)) character(0) else unique(values)
  })

  pattern <- vapply(label_list, function(values) {
    if (length(values) == 0L) "-" else paste(values, collapse = "|")
  }, character(1))
  sites_string <- vapply(label_list, function(values) {
    if (length(values) == 0L) "" else paste(values, collapse = ",")
  }, character(1))
  n_sites <- vapply(label_list, length, integer(1))

  rows <- data.frame(
    sample = data$Replicate.Name,
    group = data$Group,
    protein = data$Protein.Name,
    pattern = pattern,
    sites = sites_string,
    n_sites = as.integer(n_sites),
    weight = weight,
    area = area,
    row_id = row_ids,
    usable = usable,
    stringsAsFactors = FALSE
  )
  rows <- rows[rows$usable, , drop = FALSE]
  rows$row_key <- paste(rows$sample, rows$group, rows$protein, rows$pattern,
                        sep = "\r")

  first <- rows[!duplicated(rows$row_key), , drop = FALSE]
  frequency <- stats::aggregate(
    rows$weight,
    by = list(row_key = rows$row_key),
    FUN = sum
  )
  patterns <- data.frame(
    sample = first$sample,
    group = first$group,
    protein = first$protein,
    pattern = first$pattern,
    sites = first$sites,
    n_sites = first$n_sites,
    frequency = frequency[[2L]][match(first$row_key, frequency$row_key)],
    stringsAsFactors = FALSE
  )

  unique_samples <- unique(rows[, c("sample", "group", "protein")])
  n_samples <- stats::aggregate(
    unique_samples$sample,
    by = list(group = unique_samples$group, protein = unique_samples$protein),
    FUN = function(values) length(unique(values))
  )
  names(n_samples)[3L] <- "n_samples"

  group_key <- paste(patterns$group, patterns$protein, patterns$pattern,
                     sep = "\r")
  group_sum <- stats::aggregate(
    patterns$frequency,
    by = list(group_key = group_key),
    FUN = sum
  )
  pattern_ns_key <- paste(patterns$group, patterns$protein, sep = "\r")
  ns_key <- paste(n_samples$group, n_samples$protein, sep = "\r")
  patterns$n_samples <- n_samples$n_samples[
    match(pattern_ns_key, ns_key)
  ]
  patterns$group_frequency <- group_sum[[2L]][
    match(group_key, group_sum$group_key)
  ] / patterns$n_samples
  patterns <- patterns[, c(
    "sample", "group", "protein", "pattern", "sites", "frequency",
    "group_frequency", "n_sites", "n_samples"
  ), drop = FALSE]
  ordering <- order(patterns$sample, patterns$protein,
                    -patterns$frequency, patterns$pattern)
  patterns <- patterns[ordering, , drop = FALSE]
  rownames(patterns) <- NULL

  site_rows <- if (nrow(modified) > 0L) {
    data.frame(
      row_id = modified$row_id,
      site = paste0(modified$site, modified$mod),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(row_id = integer(0), site = character(0),
               stringsAsFactors = FALSE)
  }
  if (nrow(site_rows) > 0L) {
    site_rows$sample <- rows$sample[match(site_rows$row_id, rows$row_id)]
    site_rows$group <- rows$group[match(site_rows$row_id, rows$row_id)]
    site_rows$protein <- rows$protein[match(site_rows$row_id, rows$row_id)]
    site_rows$weight <- rows$weight[match(site_rows$row_id, rows$row_id)]
    site_rows <- site_rows[
      !is.na(site_rows$weight) & site_rows$weight > 0, , drop = FALSE
    ]
  }
  if (nrow(site_rows) > 0L) {
    site_key <- paste(site_rows$sample, site_rows$group, site_rows$protein,
                      site_rows$site, sep = "\r")
    site_rows$site_key <- site_key
    site_first <- site_rows[!duplicated(site_key), , drop = FALSE]
    site_frequency <- stats::aggregate(
      site_rows$weight,
      by = list(site_key = site_key),
      FUN = sum
    )
    sites <- data.frame(
      sample = site_first$sample,
      group = site_first$group,
      protein = site_first$protein,
      site = site_first$site,
      frequency = site_frequency[[2L]][
        match(site_first$site_key, site_frequency$site_key)
      ],
      stringsAsFactors = FALSE
    )
    sites_group_key <- paste(sites$group, sites$protein, sites$site, sep = "\r")
    sites_group_sum <- stats::aggregate(
      sites$frequency,
      by = list(group_key = sites_group_key),
      FUN = sum
    )
    sites_ns_key <- paste(sites$group, sites$protein, sep = "\r")
    sites$n_samples <- n_samples$n_samples[match(sites_ns_key, ns_key)]
    sites$group_frequency <- sites_group_sum[[2L]][
      match(sites_group_key, sites_group_sum$group_key)
    ] / sites$n_samples
    sites <- sites[, c("sample", "group", "protein", "site", "frequency",
                       "group_frequency", "n_samples"), drop = FALSE]
    ordering <- order(sites$sample, sites$protein, sites$site)
    sites <- sites[ordering, , drop = FALSE]
    rownames(sites) <- NULL
  } else {
    sites <- .hm_empty_site_frequencies()
  }

  # Group-level pattern table used for the co-occurrence analysis.
  group_patterns <- unique(patterns[, c(
    "group", "protein", "pattern", "n_sites", "group_frequency", "n_samples"
  ), drop = FALSE])
  cooccurrence <- .hm_build_cooccurrence(group_patterns)
  cooccurrence <- cooccurrence[, c(
    "group", "protein", "site_a", "site_b", "a", "b", "c", "d",
    "log_odds_ratio", "n", "conditional_a_given_b",
    "conditional_b_given_a", "n_samples"
  ), drop = FALSE]
  rownames(cooccurrence) <- NULL

  settings <- list(
    proteins = unique(patterns$protein),
    groups = unique(patterns$group),
    area_col = area_col,
    weighting = "row intensity / total protein intensity",
    pattern_separator = "|",
    light_only = light_only,
    n_samples = unique(patterns[, c("group", "protein", "n_samples")])
  )

  list(
    patterns = patterns,
    cooccurrence = cooccurrence,
    sites = sites,
    settings = settings
  )
}

# Build the co-occurrence table from group-level pattern frequencies.
.hm_build_cooccurrence <- function(group_patterns) {
  if (nrow(group_patterns) == 0L) {
    return(.hm_empty_cooccurrence())
  }
  results <- list()
  for (grp in unique(group_patterns$group)) {
    for (prot in unique(group_patterns$protein[group_patterns$group == grp])) {
      part <- group_patterns[
        group_patterns$group == grp & group_patterns$protein == prot, ,
        drop = FALSE
      ]
      label_lists <- strsplit(part$pattern, "|", fixed = TRUE)
      all_labels <- unique(unlist(label_lists, use.names = FALSE))
      all_labels <- all_labels[nzchar(all_labels) & all_labels != "-"]
      all_labels <- all_labels[.hm_order_site_labels(all_labels)]
      if (length(all_labels) < 2L) next

      n_value <- unique(part$n_samples)
      n_value <- if (length(n_value) == 0L) NA_integer_ else
        as.integer(n_value[1L])

      pairs <- utils::combn(all_labels, 2L, simplify = FALSE)
      for (pair in pairs) {
        site_a <- pair[1L]
        site_b <- pair[2L]
        has_a <- vapply(label_lists, function(values) site_a %in% values,
                        logical(1))
        has_b <- vapply(label_lists, function(values) site_b %in% values,
                        logical(1))
        a <- sum(part$group_frequency[has_a & has_b], na.rm = TRUE)
        b <- sum(part$group_frequency[has_a & !has_b], na.rm = TRUE)
        c <- sum(part$group_frequency[!has_a & has_b], na.rm = TRUE)
        d <- max(0, 1 - a - b - c)
        log_odds_ratio <- log(
          ((a + 0.5) * (d + 0.5)) / ((b + 0.5) * (c + 0.5))
        )
        conditional_a_given_b <- if ((a + c) > 0) a / (a + c) else NA_real_
        conditional_b_given_a <- if ((a + b) > 0) a / (a + b) else NA_real_
        results[[length(results) + 1L]] <- data.frame(
          group = grp,
          protein = prot,
          site_a = site_a,
          site_b = site_b,
          a = a,
          b = b,
          c = c,
          d = d,
          log_odds_ratio = log_odds_ratio,
          n = n_value,
          conditional_a_given_b = conditional_a_given_b,
          conditional_b_given_a = conditional_b_given_a,
          n_samples = n_value,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(results) == 0L) {
    return(.hm_empty_cooccurrence())
  }
  out <- do.call(rbind, results)
  ordering <- order(out$group, out$protein, out$site_a, out$site_b)
  out <- out[ordering, , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Shared clean package theme for the site-level plots.
.hm_site_theme <- function(base_size = 12) {
  ggplot2::theme_minimal(base_family = "sans", base_size = base_size) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.border = ggplot2::element_rect(
        color = "black", fill = NA, linewidth = 0.8, linejoin = "mitre"
      ),
      axis.title = ggplot2::element_text(size = base_size + 2),
      axis.text = ggplot2::element_text(size = base_size),
      legend.text = ggplot2::element_text(size = base_size),
      legend.title = ggplot2::element_text(size = base_size)
    )
}

# ggplot placeholder used whenever there is nothing to draw.
.hm_site_placeholder <- function(text) {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text", x = 0, y = 0, label = text, size = 5, colour = "grey30"
    ) +
    ggplot2::theme_void()
}

# viridis fill scale for discrete groups.
.hm_site_fill_scale <- function(color_palette = "viridis") {
  color_palette <- match.arg(
    color_palette,
    c("viridis", "magma", "plasma", "inferno", "cividis")
  )
  ggplot2::scale_fill_viridis_d(option = color_palette)
}

#' Plot site occupancy
#'
#' Draws one grouped bar per site and group from the output of
#' [compute_site_occupancy()]. Occupancy values are averaged within
#' site x group (across replicates and, when `backbone_detail = TRUE`, across
#' backbones) before plotting, so both output shapes of
#' [compute_site_occupancy()] can be passed directly.
#'
#' @param data A data.frame returned by [compute_site_occupancy()] with at least
#'   the columns `site` and `occupancy`. A `Group` column is used for the fill
#'   scale; when absent all rows are shown as one group.
#' @param group_order Optional character vector giving the display order of
#'   groups. Groups not listed are appended in order of appearance.
#' @param facet Logical; when `TRUE`, groups are drawn in separate facets
#'   (`facet_wrap`) instead of side-by-side bars.
#' @param color_palette viridis palette option, one of `"viridis"`, `"magma"`,
#'   `"plasma"`, `"inferno"` or `"cividis"`.
#' @param error_bar Logical; add standard-error error bars to the bars.
#' @param y_limits Optional numeric vector of length 2 used as the y-axis range.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' occ <- compute_site_occupancy(ms1, sample)
#' plot_occupancy(occ, group_order = c("SampleC", "SampleA"))
#' }
#' @export
plot_occupancy <- function(data,
                           group_order = NULL,
                           facet = FALSE,
                           color_palette = "viridis",
                           error_bar = FALSE,
                           y_limits = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  required <- c("site", "occupancy")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop(
      "`data` is missing required column(s): ",
      paste(missing, collapse = ", "),
      ". Use the output of compute_site_occupancy().",
      call. = FALSE
    )
  }
  facet <- isTRUE(facet)
  error_bar <- isTRUE(error_bar)

  dat <- data
  if (!"Group" %in% names(dat)) dat$Group <- "All"
  dat$Group <- as.character(dat$Group)
  dat$occupancy <- suppressWarnings(as.numeric(as.character(dat$occupancy)))
  dat <- dat[!is.na(dat$occupancy), , drop = FALSE]

  if (nrow(dat) == 0L) {
    return(.hm_site_placeholder("No occupancy data to plot"))
  }

  site_info <- unique(dat[, intersect(
    c("site", "Protein.Name", "position"), names(dat)
  ), drop = FALSE])
  if (!"Protein.Name" %in% names(site_info)) site_info$Protein.Name <- ""
  if (!"position" %in% names(site_info)) site_info$position <- NA_real_
  site_info$position <- suppressWarnings(as.numeric(site_info$position))
  ordering <- order(
    site_info$Protein.Name,
    ifelse(is.na(site_info$position), Inf, site_info$position),
    site_info$site
  )
  site_levels <- unique(as.character(site_info$site[ordering]))

  summary <- stats::aggregate(
    dat$occupancy,
    by = list(site = dat$site, Group = dat$Group),
    FUN = function(values) {
      values <- values[!is.na(values)]
      c(mean = mean(values), sd = if (length(values) > 1L) stats::sd(values)
        else NA_real_, n = length(values))
    }
  )
  summary <- data.frame(
    site = as.character(summary$site),
    Group = as.character(summary$Group),
    mean_occupancy = summary$x[, "mean"],
    sd_occupancy = summary$x[, "sd"],
    n = summary$x[, "n"],
    stringsAsFactors = FALSE
  )
  summary$se_occupancy <- summary$sd_occupancy / sqrt(pmax(summary$n, 1))
  summary$site <- factor(summary$site, levels = site_levels)
  summary$Group <- factor(
    summary$Group,
    levels = group_levels(summary$Group, group_order)
  )
  summary <- summary[order(summary$site, summary$Group), , drop = FALSE]

  p <- ggplot2::ggplot(
    summary,
    ggplot2::aes(x = site, y = mean_occupancy, fill = Group)
  ) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.9)) +
    ggplot2::labs(x = "Site", y = "Occupancy", fill = "Group") +
    .hm_site_fill_scale(color_palette) +
    .hm_site_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  if (error_bar) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean_occupancy - se_occupancy,
                   ymax = mean_occupancy + se_occupancy),
      width = 0.2,
      position = ggplot2::position_dodge(width = 0.9),
      na.rm = TRUE
    )
  }
  if (facet) {
    p <- p +
      ggplot2::facet_wrap(~Group) +
      ggplot2::theme(legend.position = "none")
  }
  if (!is.null(y_limits) && length(y_limits) == 2L && all(is.finite(y_limits))) {
    p <- p + ggplot2::coord_cartesian(ylim = sort(as.numeric(y_limits)))
  }
  p
}

#' Plot modification cross-talk patterns or site co-occurrence
#'
#' Visualises the output of [compute_crosstalk()] in one of two ways:
#' \itemize{
#'   \item `type = "patterns"`: bars of the most frequent modification patterns
#'     per group (dodged by group, or faceted with `facet = TRUE`), based on
#'     the group-level pattern frequencies;
#'   \item `type = "cooccurrence"`: a tile heatmap of the log odds ratio for
#'     each pair of sites, with numeric values printed in the tiles.
#' }
#' A patterns or co-occurrence data.frame can also be passed directly. Both
#' plots use a viridis palette and the package theme.
#'
#' @param x The list returned by [compute_crosstalk()], or the `patterns` /
#'   `cooccurrence` data.frame extracted from it.
#' @param type Plot type: `"patterns"` or `"cooccurrence"`.
#' @param top_n Number of patterns (ordered by group-level frequency) shown in
#'   the patterns plot.
#' @param group_order Optional character vector giving the display order of
#'   groups.
#' @param facet Logical; for `type = "patterns"` facets are drawn per group
#'   instead of dodged bars.
#' @param color_palette viridis palette option, one of `"viridis"`, `"magma"`,
#'   `"plasma"`, `"inferno"` or `"cividis"`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' ct <- compute_crosstalk(ms1, sample = sample)
#' plot_crosstalk(ct, type = "patterns", top_n = 8)
#' plot_crosstalk(ct, type = "cooccurrence")
#' }
#' @export
plot_crosstalk <- function(x,
                           type = c("patterns", "cooccurrence"),
                           top_n = 10,
                           group_order = NULL,
                           facet = FALSE,
                           color_palette = "viridis") {
  type <- match.arg(type)
  facet <- isTRUE(facet)
  top_n <- suppressWarnings(as.integer(top_n))
  if (length(top_n) != 1L || is.na(top_n) || top_n < 1L) {
    stop("`top_n` must be a positive integer.", call. = FALSE)
  }

  is_result_list <- is.list(x) && !is.data.frame(x) &&
    any(c("patterns", "cooccurrence") %in% names(x))

  if (type == "patterns") {
    dat <- if (is_result_list) x[["patterns"]] else x
    if (!is.data.frame(dat)) {
      stop(
        "`x` must be the list returned by compute_crosstalk() or a data.frame.",
        call. = FALSE
      )
    }
    if (!all(c("pattern", "frequency") %in% names(dat)) &&
        !"group_frequency" %in% names(dat)) {
      stop(
        "Pattern data must contain at least the columns 'pattern' and ",
        "'frequency' (or 'group_frequency').",
        call. = FALSE
      )
    }

    if (!"group" %in% names(dat)) dat$group <- "All"
    dat$group <- as.character(dat$group)
    frequency <- if ("group_frequency" %in% names(dat)) {
      as.numeric(dat$group_frequency)
    } else {
      as.numeric(dat$frequency)
    }
    group_patterns <- unique(data.frame(
      pattern = as.character(dat$pattern),
      group = dat$group,
      frequency = frequency,
      stringsAsFactors = FALSE
    ))
    group_patterns <- group_patterns[!is.na(group_patterns$frequency), ,
                                     drop = FALSE]
    if (nrow(group_patterns) == 0L) {
      return(.hm_site_placeholder("No cross-talk patterns to plot"))
    }

    overall <- stats::aggregate(
      group_patterns$frequency,
      by = list(pattern = group_patterns$pattern),
      FUN = function(values) mean(values, na.rm = TRUE)
    )
    overall <- overall[order(-overall$x, overall$pattern), , drop = FALSE]
    selected <- utils::head(as.character(overall$pattern), top_n)
    overall$pattern <- factor(overall$pattern, levels = rev(overall$pattern))
    pattern_levels <- rev(as.character(overall$pattern)[
      overall$pattern %in% selected
    ])

    plot_dat <- group_patterns[group_patterns$pattern %in% selected, ,
                               drop = FALSE]
    grid <- expand.grid(
      pattern = selected,
      group = unique(group_patterns$group),
      stringsAsFactors = FALSE
    )
    grid$frequency <- plot_dat$frequency[
      match(paste(grid$pattern, grid$group, sep = "\r"),
            paste(plot_dat$pattern, plot_dat$group, sep = "\r"))
    ]
    grid$frequency[is.na(grid$frequency)] <- 0
    grid$pattern <- factor(grid$pattern, levels = pattern_levels)
    grid$group <- factor(
      grid$group,
      levels = group_levels(grid$group, group_order)
    )

    p <- ggplot2::ggplot(
      grid,
      ggplot2::aes(x = pattern, y = frequency, fill = group)
    ) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.9)) +
      ggplot2::labs(
        x = "Modification pattern", y = "Frequency", fill = "Group"
      ) +
      .hm_site_fill_scale(color_palette) +
      .hm_site_theme() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )
    if (facet) {
      p <- p +
        ggplot2::facet_wrap(~group) +
        ggplot2::theme(legend.position = "none")
    }
    return(p)
  }

  dat <- if (is_result_list) x[["cooccurrence"]] else x
  if (!is.data.frame(dat)) {
    stop(
      "`x` must be the list returned by compute_crosstalk() or a data.frame.",
      call. = FALSE
    )
  }
  required <- c("site_a", "site_b", "log_odds_ratio")
  missing <- setdiff(required, names(dat))
  if (length(missing) > 0L) {
    stop(
      "Co-occurrence data must contain the columns ",
      paste(required, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (nrow(dat) == 0L) {
    return(.hm_site_placeholder("No site pairs available for co-occurrence"))
  }

  dat$site_a <- as.character(dat$site_a)
  dat$site_b <- as.character(dat$site_b)
  dat$log_odds_ratio <- suppressWarnings(as.numeric(dat$log_odds_ratio))
  dat$label <- ifelse(
    is.finite(dat$log_odds_ratio),
    formatC(dat$log_odds_ratio, format = "f", digits = 2),
    "NA"
  )
  if (!"group" %in% names(dat)) dat$group <- "All"
  dat$group <- as.character(dat$group)
  if (!"protein" %in% names(dat)) dat$protein <- ""
  dat$protein <- as.character(dat$protein)

  # Mirror the pairs so the tile heatmap shows a full matrix.
  mirror <- dat
  mirror$site_a <- dat$site_b
  mirror$site_b <- dat$site_a
  tile_dat <- rbind(dat, mirror)

  panels <- unique(tile_dat[, c("group", "protein"), drop = FALSE])
  facet_title <- NULL
  if (nrow(panels) > 1L && length(unique(panels$group)) > 1L) {
    facet_title <- "group"
  } else if (length(unique(panels$protein)) > 1L) {
    facet_title <- "protein"
  }
  if (nrow(panels) > 1L && length(unique(panels$group)) > 1L &&
      length(unique(panels$protein)) > 1L) {
    tile_dat$.panel <- paste(tile_dat$group, tile_dat$protein, sep = " | ")
    facet_title <- ".panel"
  }

  p <- ggplot2::ggplot(
    tile_dat,
    ggplot2::aes(x = site_a, y = site_b, fill = log_odds_ratio)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 3) +
    ggplot2::scale_fill_viridis_c(
      option = match.arg(
        color_palette,
        c("viridis", "magma", "plasma", "inferno", "cividis")
      ),
      na.value = "grey85"
    ) +
    ggplot2::labs(
      x = NULL, y = NULL, fill = "log OR"
    ) +
    .hm_site_theme() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  if (!is.null(facet_title)) {
    formula <- stats::as.formula(paste("~", facet_title))
    p <- p + ggplot2::facet_wrap(formula, scales = "free")
  }
  p
}
