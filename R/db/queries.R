# Generates a SQL OR clause that matches species in both abbreviated and full-binomial
# forms without requiring a lookup table:
#   c1: direct LIKE (e.g. stored 'E. coli', search 'E. coli')
#   c2: computed abbreviation from stored full name (e.g. stored 'Escherichia coli', search 'E. coli')
#   c3: abbreviated form of the search term, when term is a full binomial
#       (e.g. stored 'E. coli', search 'Escherichia coli')
# Sync dataset_conditions for the given dataset_ids (or all datasets if NULL).
# Must be called by every import script after writing to well_metadata.
qres_sync_conditions <- function(con, dataset_ids = NULL) {
  if (!is.null(dataset_ids) && length(dataset_ids) > 0L) {
    ph <- paste(rep('?', length(dataset_ids)), collapse = ',')
    DBI::dbExecute(con,
      sprintf("DELETE FROM dataset_conditions WHERE dataset_id IN (%s)", ph),
      params = as.list(dataset_ids))
    DBI::dbExecute(con, sprintf("
      INSERT INTO dataset_conditions
        (dataset_id, species, strain, medium_type, perturbation_type, perturbation, signal_type)
      SELECT DISTINCT p.dataset_id, w.species, w.strain,
             w.medium_type, w.perturbation_type, w.perturbation,
             COALESCE(rmr.signal_type, drr.signal_type)
      FROM well_metadata w
      JOIN plate p ON w.plate_id = p.plate_id
      LEFT JOIN raw_measurement_registry rmr ON rmr.dataset_id = p.dataset_id
      LEFT JOIN dose_response_registry   drr ON drr.dataset_id = p.dataset_id
      WHERE p.dataset_id IN (%s)", ph),
      params = as.list(dataset_ids))
  } else {
    DBI::dbExecute(con, "DELETE FROM dataset_conditions")
    DBI::dbExecute(con, "
      INSERT INTO dataset_conditions
        (dataset_id, species, strain, medium_type, perturbation_type, perturbation, signal_type)
      SELECT DISTINCT p.dataset_id, w.species, w.strain,
             w.medium_type, w.perturbation_type, w.perturbation,
             COALESCE(rmr.signal_type, drr.signal_type)
      FROM well_metadata w
      JOIN plate p ON w.plate_id = p.plate_id
      LEFT JOIN raw_measurement_registry rmr ON rmr.dataset_id = p.dataset_id
      LEFT JOIN dose_response_registry   drr ON drr.dataset_id = p.dataset_id")
  }
  tryCatch(qres_backfill_experiment_type(con), error = function(e) NULL)
  invisible(NULL)
}

.species_sql_clause <- function(term, col = "coalesce(dc.species,'')") {
  safe   <- qres_sql_safe(tolower(trimws(term)))
  c1     <- sprintf("lower(%s) LIKE lower('%%%s%%') ESCAPE '\\'", col, safe)
  a_col  <- sprintf(
    "lower(substr(%s,1,1)||'. '||trim(substr(%s,instr(%s,' ')+1)))", col, col, col)
  c2     <- sprintf("%s LIKE lower('%%%s%%') ESCAPE '\\'", a_col, safe)
  parts  <- strsplit(trimws(term), "\\s+")[[1]]
  clauses <- c(c1, c2)
  if (length(parts) >= 2 && nchar(parts[1]) > 2 && grepl("^[A-Za-z]", parts[1])) {
    abbrev <- qres_sql_safe(tolower(paste0(substr(parts[1],1,1), '. ',
                                            paste(parts[-1], collapse = ' '))))
    clauses <- c(clauses, sprintf("lower(%s) LIKE lower('%%%s%%') ESCAPE '\\'", col, abbrev))
  }
  paste0('(', paste(clauses, collapse = ' OR '), ')')
}

# Genus names (lower case) for which the search also matches abbreviated species
# ("Escherichia" → "E. coli"). Full binomials are matched via search_expr anyway.
.PHAMIG_GENERA <- c(
  'escherichia', 'mycobacterium', 'saccharomyces', 'bacillus', 'staphylococcus',
  'streptococcus', 'pseudomonas', 'salmonella', 'klebsiella', 'vibrio', 'enterococcus',
  'enterobacter', 'acinetobacter', 'listeria', 'lactobacillus', 'lactiplantibacillus',
  'clostridium', 'clostridioides', 'campylobacter', 'helicobacter', 'shigella',
  'yersinia', 'serratia', 'proteus', 'aeromonas', 'brevundimonas', 'caulobacter',
  'corynebacterium', 'streptomyces', 'bacteroides', 'bifidobacterium', 'myxococcus',
  'porphyromonas', 'candida', 'schizosaccharomyces', 'trypanosoma', 'neisseria',
  'haemophilus', 'legionella', 'burkholderia', 'agrobacterium', 'rhizobium',
  'sphingomonas', 'sphingobacterium', 'erwinia', 'ochrobactrum', 'brucella')

qres_search_datasets <- function(con, search = NULL) {
  requireNamespace('DBI')

  # Embargo filter: only show datasets that are released OR have no embargo record.
  # Legacy datasets (imported before embargo feature) have no embargo row → shown.
  base_sql <- "
    SELECT d.dataset_id, dc.species, dc.strain, dc.medium_type,
           dc.perturbation_type, dc.perturbation,
           p.temperature_c, d.doi, d.year, d.authors, d.raw_file, d.experiment_type,
           p.plate_reader,
           CASE WHEN rmr.path IS NOT NULL THEN 1 ELSE 0 END AS has_raw,
           COALESCE(rmr.signal_type,      drr.signal_type)      AS signal_type,
           COALESCE(rmr.measurement_unit, drr.measurement_unit) AS measurement_unit
    FROM dataset d
    LEFT JOIN plate p USING(dataset_id)
    LEFT JOIN dataset_conditions dc USING(dataset_id)
    LEFT JOIN raw_measurement_registry rmr USING(dataset_id)
    LEFT JOIN dose_response_registry drr USING(dataset_id)
    LEFT JOIN upload_embargo ue USING(dataset_id)
    WHERE (ue.released = TRUE OR ue.dataset_id IS NULL)
  "

  if (is.null(search) || !nzchar(trimws(search %||% ''))) {
    return(DBI::dbGetQuery(con, paste(base_sql, "ORDER BY p.temperature_c")))
  }

  # Split into tokens — each token must appear independently (AND logic).
  # This lets "E. coli glucose" find datasets where species=E.coli AND perturbation=Glucose,
  # even though those fields are not adjacent in the concatenated search string.
  tokens <- strsplit(tolower(trimws(search)), "\\s+")[[1]]
  tokens <- tokens[nzchar(tokens)]
  tokens <- vapply(tokens, qres_sql_safe, character(1L))

  search_expr <- "lower(
        coalesce(d.title,'')               || ' ' || coalesce(d.doi,'')            || ' ' ||
        coalesce(d.authors,'')             || ' ' || coalesce(dc.species,'')        || ' ' ||
        coalesce(substr(dc.species,1,1)||'. '||trim(substr(dc.species,instr(dc.species,' ')+1)),'') || ' ' ||
        coalesce(dc.strain,'')             || ' ' || coalesce(dc.medium_type,'')    || ' ' ||
        coalesce(p.plate_reader,'')        || ' ' || coalesce(dc.perturbation_type,'') || ' ' ||
        coalesce(dc.perturbation,'')       || ' ' ||
        coalesce(CAST(p.temperature_c AS VARCHAR),'') || ' ' ||
        coalesce(CAST(d.year AS VARCHAR),'') || ' ' ||
        coalesce(rmr.signal_type, drr.signal_type, '') || ' ' ||
        coalesce(rmr.measurement_unit, drr.measurement_unit, '')
      )"
  # For each token, build a LIKE clause on the full concat string.
  # For tokens that are a genus name, ALSO add a fallback matching abbreviated
  # species like "M. tuberculosis" when the user types "Mycobacterium", or
  # "E. coli" when they type "Escherichia". Restricted to known genera: any
  # 5+ letter word used to qualify, so "evolution" or "erythromycin" matched
  # every E. coli dataset.
  make_token_clause <- function(tok) {
    base <- sprintf("%s LIKE '%%%s%%' ESCAPE '\\'", search_expr, tok)
    if (tok %in% .PHAMIG_GENERA) {
      fl <- substr(tok, 1L, 1L)
      genus_clause <- sprintf("lower(coalesce(dc.species,'')) LIKE '%s.%%'", fl)
      paste0("(", base, " OR ", genus_clause, ")")
    } else {
      paste0("(", base, ")")
    }
  }
  token_clauses <- paste(
    vapply(tokens, make_token_clause, character(1L)),
    collapse = " AND ")

  DBI::dbGetQuery(con,
    paste0(base_sql, " AND ", token_clauses, " ORDER BY p.temperature_c")
  )
}

# Search by publication metadata: author, year, DOI/PubMed, device
qres_search_by_publication <- function(con, author = NULL, year_min = NULL,
                                        year_max = NULL, doi = NULL,
                                        device = NULL) {
  clauses <- character(0)

  if (!is.null(author) && nzchar(trimws(author %||% ''))) {
    clauses <- c(clauses, sprintf(
      "lower(coalesce(d.authors,'')) LIKE lower('%%%s%%') ESCAPE '\\'",
      qres_sql_safe(trimws(author))))
  }
  if (!is.null(year_min) && is.finite(as.numeric(year_min %||% NA))) {
    clauses <- c(clauses, sprintf("d.year >= %d", as.integer(year_min)))
  }
  if (!is.null(year_max) && is.finite(as.numeric(year_max %||% NA))) {
    clauses <- c(clauses, sprintf("d.year <= %d", as.integer(year_max)))
  }
  if (!is.null(doi) && nzchar(trimws(doi %||% ''))) {
    clauses <- c(clauses, sprintf(
      "lower(coalesce(d.doi,'') || ' ' || coalesce(d.source_url,'')) LIKE lower('%%%s%%') ESCAPE '\\'",
      qres_sql_safe(trimws(doi))))
  }
  if (!is.null(device) && nzchar(trimws(device %||% ''))) {
    clauses <- c(clauses, sprintf(
      "lower(coalesce(p.plate_reader,'') || ' ' || coalesce(p.spectrophotometer,'')) LIKE lower('%%%s%%') ESCAPE '\\'",
      qres_sql_safe(trimws(device))))
  }

  where <- if (length(clauses) > 0)
    paste('WHERE', paste(clauses, collapse = ' AND ')) else ''

  # Prepend embargo filter; if WHERE already starts the clause, use AND
  embargo_clause <- "(ue.released = TRUE OR ue.dataset_id IS NULL)"
  full_where <- if (nchar(where) == 0)
    paste('WHERE', embargo_clause)
  else
    paste(where, 'AND', embargo_clause)

  DBI::dbGetQuery(con, sprintf("
    SELECT d.dataset_id, d.title, d.authors, d.year, d.doi, d.source_url,
           dc.species, dc.strain, dc.medium_type,
           dc.perturbation_type, dc.perturbation,
           p.temperature_c, p.plate_reader, p.spectrophotometer,
           CASE WHEN rmr.path IS NOT NULL THEN 1 ELSE 0 END AS has_raw
    FROM dataset d
    LEFT JOIN plate p USING(dataset_id)
    LEFT JOIN dataset_conditions dc USING(dataset_id)
    LEFT JOIN raw_measurement_registry rmr USING(dataset_id)
    LEFT JOIN upload_embargo ue USING(dataset_id)
    %s
    ORDER BY d.year DESC, d.dataset_id
  ", full_where))
}

qres_get_datasets_by_ids <- function(con, ids) {
  if (length(ids) == 0) return(data.frame())
  ph <- paste(rep('?', length(ids)), collapse = ', ')
  DBI::dbGetQuery(con, sprintf("
    SELECT d.dataset_id, dc.species, dc.strain, p.temperature_c,
           dc.medium_type, dc.perturbation_type, dc.perturbation, d.authors, d.experiment_type
    FROM dataset d
    LEFT JOIN plate p USING(dataset_id)
    LEFT JOIN dataset_conditions dc USING(dataset_id)
    WHERE d.dataset_id IN (%s)", ph),
    params = as.list(ids))
}

qres_get_distinct_authors <- function(con) {
  res <- DBI::dbGetQuery(con, "SELECT authors FROM dataset WHERE authors IS NOT NULL")
  parts <- unlist(strsplit(res$authors, ','))
  parts <- trimws(parts[nzchar(trimws(parts))])
  parts <- tools::toTitleCase(tolower(parts))
  sort(unique(parts))
}

qres_get_distinct_strains <- function(con, species) {
  clause <- .species_sql_clause(species, col = "coalesce(species,'')")
  res <- DBI::dbGetQuery(con,
    sprintf("SELECT DISTINCT strain FROM well_metadata WHERE %s AND strain IS NOT NULL ORDER BY strain",
            clause))
  trimws(res$strain[nzchar(trimws(res$strain))])
}

qres_get_distinct_perturbation_types <- function(con) {
  res <- DBI::dbGetQuery(con,
    "SELECT DISTINCT perturbation_type FROM well_metadata WHERE perturbation_type IS NOT NULL ORDER BY perturbation_type")
  res$perturbation_type[nzchar(res$perturbation_type)]
}

qres_get_distinct_perturbations <- function(con, perturb_type) {
  res <- DBI::dbGetQuery(con,
    "SELECT DISTINCT perturbation FROM well_metadata WHERE lower(perturbation_type) = lower(?) AND perturbation IS NOT NULL ORDER BY perturbation",
    params = list(perturb_type))
  res$perturbation[nzchar(res$perturbation)]
}

qres_filter_datasets <- function(con, species = NULL, strain = NULL,
                                  temp_min = NULL, temp_max = NULL,
                                  medium = NULL, perturbation_type = NULL,
                                  perturbation = NULL, year_min = NULL, year_max = NULL,
                                  author = NULL, registered_only = TRUE) {
  clauses <- character(0)

  if (!is.null(species) && nzchar(species)) {
    clauses <- c(clauses, .species_sql_clause(species))
  }
  if (!is.null(strain) && nzchar(strain)) {
    clauses <- c(clauses, sprintf(
      "lower(coalesce(dc.strain,'')) LIKE lower('%%%s%%') ESCAPE '\\'", qres_sql_safe(strain)))
  }
  if (!is.null(temp_min) && is.finite(as.numeric(temp_min %||% NA_real_))) {
    clauses <- c(clauses, sprintf("p.temperature_c >= %s", as.numeric(temp_min)))
  }
  if (!is.null(temp_max) && is.finite(as.numeric(temp_max %||% NA_real_))) {
    clauses <- c(clauses, sprintf("p.temperature_c <= %s", as.numeric(temp_max)))
  }
  if (!is.null(medium) && nzchar(medium)) {
    clauses <- c(clauses, sprintf(
      "lower(coalesce(dc.medium_type,'')) LIKE lower('%%%s%%') ESCAPE '\\'", qres_sql_safe(medium)))
  }
  if (!is.null(perturbation_type) && nzchar(perturbation_type) && perturbation_type != 'all') {
    # perturbation_type is an enum — validate, don't sanitise via string interpolation
    ptype_safe <- if (perturbation_type %in% c('chemical', 'genetic', 'temperature', 'evolution', 'none'))
      perturbation_type else stop('Invalid perturbation_type: ', perturbation_type)
    clauses <- c(clauses, sprintf(
      "lower(coalesce(dc.perturbation_type,'')) LIKE lower('%%%s%%') ESCAPE '\\'", ptype_safe))
  }
  if (!is.null(perturbation) && nzchar(perturbation)) {
    temp_val <- suppressWarnings(as.numeric(perturbation))
    if (!is.na(temp_val) &&
        (is.null(perturbation_type) || perturbation_type %in% c('all', 'temperature'))) {
      # Numeric value: temperature match. temp_val is coerced to number — safe.
      clauses <- c(clauses, sprintf(
        "(lower(coalesce(dc.perturbation,'')) LIKE lower('%%%s%%') ESCAPE '\\' OR ABS(p.temperature_c - %s) <= 0.5)",
        qres_sql_safe(perturbation), temp_val))
    } else {
      clauses <- c(clauses, sprintf(
        "lower(coalesce(dc.perturbation,'')) LIKE lower('%%%s%%') ESCAPE '\\'", qres_sql_safe(perturbation)))
    }
  }
  if (!is.null(year_min) && is.finite(as.numeric(year_min %||% NA_real_))) {
    clauses <- c(clauses, sprintf("d.year >= %d", as.integer(year_min)))
  }
  if (!is.null(year_max) && is.finite(as.numeric(year_max %||% NA_real_))) {
    clauses <- c(clauses, sprintf("d.year <= %d", as.integer(year_max)))
  }
  if (!is.null(author) && nzchar(author)) {
    clauses <- c(clauses, sprintf(
      "lower(coalesce(d.authors,'')) LIKE lower('%%%s%%') ESCAPE '\\'", qres_sql_safe(author)))
  }
  if (isTRUE(registered_only)) {
    clauses <- c(clauses, "rmr.path IS NOT NULL")
  }

  clauses <- c(clauses, "(ue.released = TRUE OR ue.dataset_id IS NULL)")
  where   <- paste('WHERE', paste(clauses, collapse = ' AND '))

  DBI::dbGetQuery(con, sprintf("
    SELECT d.dataset_id, dc.species, dc.strain, dc.medium_type,
           dc.perturbation_type, dc.perturbation,
           p.temperature_c, d.doi, d.year, d.authors, d.experiment_type, rmr.path AS raw_path
    FROM dataset d
    LEFT JOIN plate p USING(dataset_id)
    LEFT JOIN dataset_conditions dc USING(dataset_id)
    LEFT JOIN raw_measurement_registry rmr USING(dataset_id)
    LEFT JOIN upload_embargo ue USING(dataset_id)
    %s
    ORDER BY p.temperature_c
  ", where))
}

qres_get_raw_registry <- function(con, dataset_id) {
  DBI::dbGetQuery(con, 'SELECT * FROM raw_measurement_registry WHERE dataset_id = ?',
                  params = list(dataset_id))
}

qres_get_distinct_species <- function(con) {
  res <- DBI::dbGetQuery(con, "SELECT DISTINCT species FROM well_metadata WHERE species IS NOT NULL ORDER BY species")
  res$species
}

# Named character vector: value = species name (used in SQL); name = display label
# showing unique strain count when >1 strain exists, e.g. "E. coli (3 strains)".
qres_get_species_with_counts <- function(con, author = NULL) {
  if (!is.null(author) && nzchar(trimws(author))) {
    author_safe <- qres_sql_safe(trimws(author))
    sql <- sprintf(
      "SELECT w.species,
              COUNT(DISTINCT CASE WHEN w.strain IS NOT NULL AND trim(w.strain) != '' THEN w.strain END) AS n_strains
       FROM well_metadata w
       WHERE w.species IS NOT NULL
         AND w.plate_id IN (
           SELECT p.plate_id FROM plate p
           JOIN dataset d USING(dataset_id)
           WHERE lower(coalesce(d.authors,'')) LIKE lower('%%%s%%') ESCAPE '\\')
       GROUP BY w.species ORDER BY w.species", author_safe)
  } else {
    sql <- "SELECT species,
                   COUNT(DISTINCT CASE WHEN strain IS NOT NULL AND trim(strain) != '' THEN strain END) AS n_strains
            FROM well_metadata WHERE species IS NOT NULL
            GROUP BY species ORDER BY species"
  }
  res <- DBI::dbGetQuery(con, sql)
  if (nrow(res) == 0) return(setNames(character(0), character(0)))
  lbl <- ifelse(res$n_strains > 1,
    paste0(res$species, ' (', res$n_strains, ' strains)'),
    res$species)
  setNames(res$species, lbl)
}

qres_get_temp_range <- function(con) {
  res <- DBI::dbGetQuery(con, "SELECT MIN(temperature_c) AS tmin, MAX(temperature_c) AS tmax FROM plate")
  c(res$tmin, res$tmax)
}

# Returns well_ids flagged as blanks for the given dataset(s).
qres_get_blank_wells <- function(con, dataset_ids) {
  if (length(dataset_ids) == 0) return(character(0))
  placeholders <- paste(rep('?', length(dataset_ids)), collapse = ', ')
  res <- DBI::dbGetQuery(con,
    sprintf('SELECT well_id FROM well_metadata WHERE dataset_id IN (%s) AND is_blank = TRUE',
            placeholders),
    params = as.list(dataset_ids))
  res$well_id
}
