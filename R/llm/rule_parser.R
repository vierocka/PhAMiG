# Rule-based natural language parser for PhaMiG search queries.
# Free alternative to Claude NL→SQL. Extracts structured filters from text.
# Claude is used as fallback when this parser returns no filters and the API key is set.
#
# Usage:
#   filters <- qres_parse_nl_query("E. coli at 37°C in LB with rifampicin")
#   # → list(species="E. coli", temp_min=37, temp_max=37, medium="LB", perturbation="rifampicin")

# ─── Known vocabulary ────────────────────────────────────────────────────────

.NL_SPECIES <- list(
  'E. coli'             = c('e\\.?\\s*coli', 'escherichia\\s*(coli)?', 'ecoli'),
  'M. tuberculosis'     = c('m\\.?\\s*tuberculosis', 'mycobacterium(\\s+tuberculosis)?', '\\bmtb\\b', 'm\\.?tb'),
  'S. cerevisiae'       = c('s\\.?\\s*cerevisiae', 'saccharomyces(\\s+cerevisiae)?', 'yeast', 'baker.s yeast'),
  'B. subtilis'         = c('b\\.?\\s*subtilis', 'bacillus(\\s+subtilis)?'),
  'S. typhimurium'      = c('salmonella', 's\\.?\\s*typhimurium'),
  'K. pneumoniae'       = c('klebsiella', 'k\\.?\\s*pneumoniae'),
  'P. aeruginosa'       = c('pseudomonas', 'p\\.?\\s*aeruginosa'),
  'S. aureus'           = c('staphylococcus', 's\\.?\\s*aureus'),
  'A. tumefaciens'      = c('agrobacterium', 'a\\.?\\s*tumefaciens')
)

.NL_MEDIA <- c('lb', 'tsa', 'mh', 'mops', 'm9', 'r2a', 'tsb', 'bhi',
               'synthetic', 'minimal', 'rich', 'nutrient', 'marine',
               'glucose', 'glycerol', 'succinate', 'acetate')

.NL_PERTURB_TYPES <- list(
  'temperature' = c('temperature', 'temp', 'thermal', 'heat', 'cold'),
  'chemical'    = c('chemical', 'antibiotic', 'drug', 'stressor', 'compound',
                    'perturbation', 'treatment', 'dose', 'concentration', 'mg/l',
                    'microg', '\\bmic\\b', 'inhibit', 'resistan'),
  'genetic'     = c('genetic', 'mutation', 'knockout', 'knockin', 'deletion',
                    'overexpression', 'plasmid', 'delta', '\\bΔ\\b', 'knock.out')
)

# Common antibiotic and drug names
.NL_DRUGS <- c('rifampicin', 'rifampin', 'ampicillin', 'amoxicillin',
               'ciprofloxacin', 'tetracycline', 'chloramphenicol', 'kanamycin',
               'gentamicin', 'trimethoprim', 'sulfamethoxazole', 'vancomycin',
               'erythromycin', 'azithromycin', 'meropenem', 'imipenem',
               'colistin', 'polymyxin', 'streptomycin', 'tobramycin',
               'fusidic acid', 'fosfomycin', 'nitrofurantoin', 'linezolid',
               'norfloxacin', 'ofloxacin', 'levofloxacin', 'nalidixic acid',
               'glucose', 'nacl', 'salt', 'acid', 'h2o2', 'ethanol', 'cuso4')

# ─── Main parser ─────────────────────────────────────────────────────────────

#' Parse a plain-text growth curve query into structured filters.
#' Returns a named list compatible with qres_filter_datasets() and qres_search_by_publication().
#' Returns NULL if no meaningful filters could be extracted.
qres_parse_nl_query <- function(text) {
  if (!nzchar(trimws(text %||% ''))) return(NULL)
  txt <- tolower(trimws(text))

  filters <- list()

  # ── Species ────────────────────────────────────────────────────────────────
  for (canonical in names(.NL_SPECIES)) {
    patterns <- .NL_SPECIES[[canonical]]
    if (any(sapply(patterns, function(p) grepl(p, txt, perl = TRUE)))) {
      filters$species <- canonical
      break
    }
  }

  # ── Temperature ────────────────────────────────────────────────────────────
  # Patterns: "37°C", "37 degrees", "at 37", "30-37", "between 30 and 37"
  # Range: "between X and Y", "X to Y", "X-Y", "X–Y"
  # Try range patterns in priority order; stop at first match.
  temp_found <- FALSE

  # "between X and Y °C" / "between X and Y degrees"
  range_m <- regmatches(txt, regexpr(
    'between\\s+(\\d+(?:\\.\\d+)?)\\s*(?:and|to)\\s*(\\d+(?:\\.\\d+)?)',
    txt, perl = TRUE))
  if (length(range_m) > 0) {
    nums <- as.numeric(regmatches(range_m, gregexpr('\\d+(?:\\.\\d+)?', range_m, perl = TRUE))[[1]])
    if (length(nums) == 2) {
      filters$temp_min <- min(nums); filters$temp_max <- max(nums); temp_found <- TRUE
    }
  }

  # "X and Y degrees/°C"  (e.g. "35 and 38 degree Celsius", "in 30 and 42°C")
  if (!temp_found) {
    multi_m <- regmatches(txt, regexpr(
      '(\\d+(?:\\.\\d+)?)\\s+and\\s+(\\d+(?:\\.\\d+)?)\\s*(?:°c|°|degrees?|celsius|fahrenheit)',
      txt, perl = TRUE))
    if (length(multi_m) > 0) {
      nums <- as.numeric(regmatches(multi_m, gregexpr('\\d+(?:\\.\\d+)?', multi_m, perl = TRUE))[[1]])
      if (length(nums) == 2 && max(nums) <= 100) {
        filters$temp_min <- min(nums); filters$temp_max <- max(nums); temp_found <- TRUE
      }
    }
  }

  # "X to Y °C" / "X-Y °C" (dash or en-dash)
  if (!temp_found) {
    dash_m <- regmatches(txt, regexpr(
      '(\\d+(?:\\.\\d+)?)\\s*(?:to|[-–])\\s*(\\d+(?:\\.\\d+)?)\\s*(?:°c|°|degrees?|celsius)?',
      txt, perl = TRUE))
    if (length(dash_m) > 0) {
      nums <- as.numeric(regmatches(dash_m, gregexpr('\\d+(?:\\.\\d+)?', dash_m, perl = TRUE))[[1]])
      if (length(nums) == 2 && max(nums) <= 100) {
        filters$temp_min <- min(nums); filters$temp_max <- max(nums); temp_found <- TRUE
      }
    }
  }

  # Single value: "at 37°C", "37 degrees", "temperature of 37"
  if (!temp_found) {
    single_m <- regmatches(txt, regexpr(
      '(?:at|@|temp(?:erature)?\\s+(?:of)?\\s*)?(\\d+(?:\\.\\d+)?)\\s*(?:°c|°|degrees?|celsius)',
      txt, perl = TRUE))
    if (length(single_m) > 0) {
      val <- as.numeric(regmatches(single_m,
             gregexpr('\\d+(?:\\.\\d+)?', single_m, perl = TRUE))[[1]][1])
      if (is.finite(val) && val >= 0 && val <= 100) {
        filters$temp_min <- val - 0.5; filters$temp_max <- val + 0.5
      }
    }
  }

  # ── Year ───────────────────────────────────────────────────────────────────
  year_m <- regmatches(txt, gregexpr('\\b(19|20)\\d{2}\\b', txt, perl = TRUE))[[1]]
  if (length(year_m) == 1) {
    yr <- as.integer(year_m[1])
    filters$year_min <- yr; filters$year_max <- yr
  } else if (length(year_m) == 2) {
    yr <- as.integer(year_m)
    filters$year_min <- min(yr); filters$year_max <- max(yr)
  }

  # ── Medium ─────────────────────────────────────────────────────────────────
  for (med in .NL_MEDIA) {
    if (grepl(paste0('\\b', med, '\\b'), txt, perl = TRUE)) {
      filters$medium <- toupper(med)
      break
    }
  }

  # ── Perturbation type ──────────────────────────────────────────────────────
  for (ptype in names(.NL_PERTURB_TYPES)) {
    patterns <- .NL_PERTURB_TYPES[[ptype]]
    if (any(sapply(patterns, function(p) grepl(p, txt, perl = TRUE)))) {
      filters$perturbation_type <- ptype
      break
    }
  }

  # ── Perturbation name (drug / compound) ────────────────────────────────────
  for (drug in .NL_DRUGS) {
    if (grepl(drug, txt, fixed = TRUE)) {
      filters$perturbation <- drug
      break
    }
  }

  # ── Author ─────────────────────────────────────────────────────────────────
  # Patterns: "by Smith", "Smith lab", "Smith et al", or just a standalone
  # capitalised word preceded by "author" or "by"
  auth_m <- regmatches(txt, regexpr(
    '(?:by|author|lab|group)\\s+([a-z][a-z]+(?:\\s+[a-z][a-z]+)?)',
    txt, perl = TRUE))
  if (length(auth_m) > 0) {
    author_word <- sub('^(?:by|author|lab|group)\\s+', '', auth_m, perl = TRUE)
    if (nzchar(author_word)) filters$author <- author_word
  }

  if (length(filters) == 0) return(NULL)
  filters
}

#' Try rule-based NL parsing; fall back to Claude if no filters found and API available.
#' Returns same structure as qres_filter_datasets filters (list).
qres_smart_parse <- function(text) {
  filters <- qres_parse_nl_query(text)
  if (!is.null(filters)) return(list(source = 'rule', filters = filters))

  # Fallback: Claude (if available)
  if (qres_llm_available()) {
    action <- tryCatch(qres_nl_to_action(text), error = function(e) NULL)
    if (!is.null(action)) return(list(source = 'llm', action = action))
  }

  list(source = 'keyword', filters = NULL)
}
