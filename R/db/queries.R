qres_search_datasets <- function(con, search = NULL) {
  requireNamespace('DBI')

  # Embargo filter: only show datasets that are released OR have no embargo record.
  # Legacy datasets (imported before embargo feature) have no embargo row → shown.
  base_sql <- "
    SELECT d.dataset_id, w.species, w.strain, w.medium_type,
           w.perturbation_type, w.perturbation,
           p.temperature_c, d.doi, d.year, d.authors, d.raw_file,
           p.plate_reader,
           CASE WHEN rmr.path IS NOT NULL THEN 1 ELSE 0 END AS has_raw
    FROM dataset d
    LEFT JOIN plate p USING(dataset_id)
    LEFT JOIN (
      SELECT plate_id, species, strain, medium_type, perturbation_type, perturbation
      FROM well_metadata
      GROUP BY plate_id, species, strain, medium_type, perturbation_type, perturbation
    ) w USING(plate_id)
    LEFT JOIN raw_measurement_registry rmr USING(dataset_id)
    LEFT JOIN upload_embargo ue USING(dataset_id)
    WHERE (ue.released = TRUE OR ue.dataset_id IS NULL)
  "

  if (is.null(search) || !nzchar(trimws(search %||% ''))) {
    return(DBI::dbGetQuery(con, paste(base_sql, "ORDER BY p.temperature_c")))
  }

  pattern <- paste0('%', tolower(trimws(qres_sql_safe(search))), '%')
  DBI::dbGetQuery(con,
    paste0(base_sql, "
      AND lower(
        coalesce(d.title,'')              || ' ' || coalesce(d.doi,'')           || ' ' ||
        coalesce(d.authors,'')            || ' ' || coalesce(w.species,'')       || ' ' ||
        coalesce(w.strain,'')             || ' ' || coalesce(w.medium_type,'')   || ' ' ||
        coalesce(p.plate_reader,'')       || ' ' || coalesce(w.perturbation_type,'') || ' ' ||
        coalesce(w.perturbation,'')       || ' ' ||
        coalesce(CAST(p.temperature_c AS VARCHAR),'') || ' ' ||
        coalesce(CAST(d.year AS VARCHAR),'')
      ) LIKE ?
      ORDER BY p.temperature_c"),
    params = list(pattern)
  )
}

# Search by publication metadata: author, year, DOI/PubMed, device
qres_search_by_publication <- function(con, author = NULL, year_min = NULL,
                                        year_max = NULL, doi = NULL,
                                        device = NULL) {
  clauses <- character(0)

  if (!is.null(author) && nzchar(trimws(author %||% ''))) {
    clauses <- c(clauses, sprintf(
      "lower(coalesce(d.authors,'')) LIKE lower('%%%s%%')",
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
      "lower(coalesce(d.doi,'') || ' ' || coalesce(d.source_url,'')) LIKE lower('%%%s%%')",
      qres_sql_safe(trimws(doi))))
  }
  if (!is.null(device) && nzchar(trimws(device %||% ''))) {
    clauses <- c(clauses, sprintf(
      "lower(coalesce(p.plate_reader,'') || ' ' || coalesce(p.spectrophotometer,'')) LIKE lower('%%%s%%')",
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
           w.species, w.strain, w.medium_type,
           w.perturbation_type, w.perturbation,
           p.temperature_c, p.plate_reader, p.spectrophotometer,
           CASE WHEN rmr.path IS NOT NULL THEN 1 ELSE 0 END AS has_raw
    FROM dataset d
    LEFT JOIN plate p USING(dataset_id)
    LEFT JOIN (
      SELECT plate_id, species, strain, medium_type, perturbation_type, perturbation
      FROM well_metadata
      GROUP BY plate_id, species, strain, medium_type, perturbation_type, perturbation
    ) w USING(plate_id)
    LEFT JOIN raw_measurement_registry rmr USING(dataset_id)
    LEFT JOIN upload_embargo ue USING(dataset_id)
    %s
    ORDER BY d.year DESC, d.dataset_id
  ", full_where))
}

qres_get_distinct_authors <- function(con) {
  res <- DBI::dbGetQuery(con, "SELECT authors FROM dataset WHERE authors IS NOT NULL")
  parts <- unlist(strsplit(res$authors, ','))
  sort(unique(trimws(parts[nzchar(trimws(parts))])))
}

qres_get_distinct_strains <- function(con, species) {
  res <- DBI::dbGetQuery(con,
    "SELECT DISTINCT strain FROM well_metadata WHERE lower(species) = lower(?) AND strain IS NOT NULL ORDER BY strain",
    params = list(species))
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
    clauses <- c(clauses, sprintf(
      "lower(coalesce(w.species,'')) LIKE lower('%%%s%%')", qres_sql_safe(species)))
  }
  if (!is.null(strain) && nzchar(strain)) {
    clauses <- c(clauses, sprintf(
      "lower(coalesce(w.strain,'')) LIKE lower('%%%s%%')", qres_sql_safe(strain)))
  }
  if (!is.null(temp_min) && is.finite(as.numeric(temp_min %||% NA_real_))) {
    clauses <- c(clauses, sprintf("p.temperature_c >= %s", as.numeric(temp_min)))
  }
  if (!is.null(temp_max) && is.finite(as.numeric(temp_max %||% NA_real_))) {
    clauses <- c(clauses, sprintf("p.temperature_c <= %s", as.numeric(temp_max)))
  }
  if (!is.null(medium) && nzchar(medium)) {
    clauses <- c(clauses, sprintf(
      "lower(coalesce(w.medium_type,'')) LIKE lower('%%%s%%')", qres_sql_safe(medium)))
  }
  if (!is.null(perturbation_type) && nzchar(perturbation_type) && perturbation_type != 'all') {
    # perturbation_type is an enum — validate, don't sanitise via string interpolation
    ptype_safe <- if (perturbation_type %in% c('chemical', 'genetic', 'temperature', 'none'))
      perturbation_type else stop('Invalid perturbation_type: ', perturbation_type)
    clauses <- c(clauses, sprintf(
      "lower(coalesce(w.perturbation_type,'')) LIKE lower('%%%s%%')", ptype_safe))
  }
  if (!is.null(perturbation) && nzchar(perturbation)) {
    temp_val <- suppressWarnings(as.numeric(perturbation))
    if (!is.na(temp_val) &&
        (is.null(perturbation_type) || perturbation_type %in% c('all', 'temperature'))) {
      # Numeric value: temperature match. temp_val is coerced to number — safe.
      clauses <- c(clauses, sprintf(
        "(lower(coalesce(w.perturbation,'')) LIKE lower('%%%s%%') OR ABS(p.temperature_c - %s) <= 0.5)",
        qres_sql_safe(perturbation), temp_val))
    } else {
      clauses <- c(clauses, sprintf(
        "lower(coalesce(w.perturbation,'')) LIKE lower('%%%s%%')", qres_sql_safe(perturbation)))
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
      "lower(coalesce(d.authors,'')) LIKE lower('%%%s%%')", qres_sql_safe(author)))
  }
  if (isTRUE(registered_only)) {
    clauses <- c(clauses, "rmr.path IS NOT NULL")
  }

  clauses <- c(clauses, "(ue.released = TRUE OR ue.dataset_id IS NULL)")
  where   <- paste('WHERE', paste(clauses, collapse = ' AND '))

  DBI::dbGetQuery(con, sprintf("
    SELECT d.dataset_id, w.species, w.strain, w.medium_type,
           w.perturbation_type, w.perturbation,
           p.temperature_c, d.doi, d.year, d.authors, rmr.path AS raw_path
    FROM dataset d
    LEFT JOIN plate p USING(dataset_id)
    LEFT JOIN (
      SELECT plate_id, species, strain, medium_type, perturbation_type, perturbation
      FROM well_metadata
      GROUP BY plate_id, species, strain, medium_type, perturbation_type, perturbation
    ) w USING(plate_id)
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
