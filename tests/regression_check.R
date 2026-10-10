# tests/regression_check.R
# ──────────────────────────────────────────────────────────────────────────────
# Regression check for PhaMiG: new features and how they interact with the main
# functions. Run from the project root after every change:
#
#   Rscript tests/regression_check.R              # uses data/qres.duckdb (read-only)
#   Rscript tests/regression_check.R --db=path    # another database
#   Rscript tests/regression_check.R --app        # also start the app and fetch its page
#
# Exit status 1 if any check fails. Read-only on the database except for the
# search-observer test, which runs the app's own startup migration (idempotent).
# ──────────────────────────────────────────────────────────────────────────────

args    <- commandArgs(TRUE)
`%||%`  <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a
db_path <- sub('^--db=', '', grep('^--db=', args, value = TRUE)[1] %||% 'data/qres.duckdb')
run_app <- '--app' %in% args
# If the app (e.g. in RStudio) holds the DuckDB lock, test a snapshot copy instead
lock_ok <- tryCatch({ c0 <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE); DBI::dbDisconnect(c0, shutdown = TRUE); TRUE },
                    error = function(e) FALSE)
if (!lock_ok) {
  snap <- file.path(tempdir(), 'qres_snapshot.duckdb')
  file.copy(db_path, snap, overwrite = TRUE)
  if (file.exists(paste0(db_path, '.wal'))) file.copy(paste0(db_path, '.wal'), paste0(snap, '.wal'), overwrite = TRUE)
  cat(sprintf('NOTE: %s is locked by another process (app running?) — testing a snapshot copy\n', db_path))
  db_path <- snap
}
Sys.setenv(QRES_DB = db_path)   # the app server (testServer / --app) uses the same database

results <- data.frame(area = character(), check = character(), ok = logical(), detail = character())
check <- function(area, name, expr) {
  out <- tryCatch(expr, error = function(e) structure(FALSE, detail = paste('ERROR:', conditionMessage(e))))
  ok  <- isTRUE(as.logical(out))
  det <- attr(out, 'detail') %||% ''
  results[nrow(results) + 1, ] <<- list(area, name, ok, det)
  cat(sprintf('[%s] %-10s %s%s\n', if (ok) 'PASS' else 'FAIL', area, name, if (nzchar(det)) paste0('  — ', det) else ''))
  invisible(ok)
}
with_detail <- function(ok, detail) structure(ok, detail = detail)

# ── 1. Static: parse, source, undefined functions ────────────────────────────
app_lines <- readLines('app.R', warn = FALSE)
sources   <- regmatches(app_lines, regexpr("R/[^']+\\.R", app_lines))
importers <- list.files('R/import', '_import\\.R$', full.names = TRUE)
all_files <- unique(c('app.R', sources, importers, 'R/import/generic_csv.R', 'tests/regression_check.R'))
bad_parse <- Filter(function(f) inherits(tryCatch(parse(f), error = function(e) e), 'error'), all_files)
check('static', sprintf('all %d R files parse', length(all_files)),
      with_detail(length(bad_parse) == 0, paste(bad_parse, collapse = ', ')))
env <- globalenv()
bad_src <- character(0)
for (f in unique(c(sources, importers))) tryCatch(sys.source(f, envir = env), error = function(e) bad_src <<- c(bad_src, f))
check('static', 'all modules and importers source', with_detail(length(bad_src) == 0, paste(bad_src, collapse = ', ')))
code  <- paste(app_lines, collapse = '\n')
calls <- unique(sub('\\s*\\($', '', regmatches(code, gregexpr(
  "(?<![A-Za-z0-9_.$:])(qres_[A-Za-z0-9_]+|phamig_[A-Za-z0-9_]+|\\.[a-z][A-Za-z0-9_]+)\\s*\\(", code, perl = TRUE))[[1]]))
defined <- unique(sub('\\s*<-.*', '', regmatches(code, gregexpr("[.A-Za-z_][A-Za-z0-9_.]*\\s*<-\\s*(function|reactive)", code))[[1]]))
js_names <- c('.on', '.removeClass', '.addClass', '.css', '.index')
undef <- setdiff(calls[!vapply(calls, exists, logical(1))], c(defined, js_names))
check('static', 'app.R calls no undefined functions', with_detail(length(undef) == 0, paste(undef, collapse = ', ')))

# ── 2. Database integrity ────────────────────────────────────────────────────
con <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
q   <- function(sql) DBI::dbGetQuery(con, sql)
n_ds <- q('SELECT count(*) n FROM dataset')$n
cat(sprintf('           database: %s — %d datasets\n', db_path, n_ds))
reg  <- q('SELECT dataset_id, path, n_rows FROM raw_measurement_registry')
paths <- unlist(lapply(reg$path, function(p) tryCatch(.resolve_raw_paths(p), error = function(e) paste0('MISSING:', p))))
check('db', 'every registered raw file exists', with_detail(!any(grepl('^MISSING:', paths)),
      paste(head(grep('^MISSING:', paths, value = TRUE), 3), collapse = ', ')))
orph <- q('SELECT count(*) n FROM dataset d WHERE NOT EXISTS (SELECT 1 FROM dataset_conditions c WHERE c.dataset_id = d.dataset_id)')$n
check('db', 'every dataset is in dataset_conditions (searchable)', with_detail(orph == 0, sprintf('%d missing', orph)))
nopl <- q('SELECT count(*) n FROM dataset d WHERE NOT EXISTS (SELECT 1 FROM plate p WHERE p.dataset_id = d.dataset_id)')$n
check('db', 'every dataset has a plate row', with_detail(nopl == 0, sprintf('%d without plate', nopl)))
bad_pt <- q("SELECT DISTINCT perturbation_type FROM well_metadata WHERE perturbation_type NOT IN ('chemical','genetic','temperature','evolution','none')")$perturbation_type
check('db', 'perturbation_type uses the controlled vocabulary', with_detail(length(bad_pt) == 0, paste(bad_pt, collapse = ', ')))
et <- q("SELECT experiment_type, count(*) n FROM dataset GROUP BY 1")
check('db', 'every dataset has a valid experiment_type',
      with_detail(all(et$experiment_type %in% c('growth_curve', 'evolution', 'endpoint', 'competition')) && sum(et$n) == n_ds,
                  paste(et$experiment_type, et$n, sep = '=', collapse = ', ')))
tc_tbl <- 'transfer_cycle' %in% DBI::dbListTables(con)
check('db', 'transfer_cycle table exists', tc_tbl)
if (tc_tbl) {
  orph_tc <- q('SELECT count(*) n FROM transfer_cycle t WHERE NOT EXISTS (SELECT 1 FROM well_metadata w WHERE w.well_id = t.well_id)')$n
  check('db', 'transfer_cycle rows point to existing wells', with_detail(orph_tc == 0, sprintf('%d orphan rows', orph_tc)))
  evo_ds <- q("SELECT DISTINCT dataset_id FROM well_metadata WHERE perturbation_type = 'evolution'")$dataset_id
  no_tc  <- setdiff(evo_ds, q('SELECT DISTINCT dataset_id FROM transfer_cycle')$dataset_id)
  check('db', 'every evolution dataset has transfer cycles', with_detail(length(no_tc) == 0, paste(head(no_tc, 3), collapse = ', ')))
}
# load a sample of datasets per source (+ all multi-file ones) through the app reader
set.seed(1)
prefix <- sub('_.*', '', reg$dataset_id)
pick <- unique(c(unlist(lapply(split(reg$dataset_id, prefix), function(x) sample(x, min(3, length(x))))),
                 reg$dataset_id[grepl(';', reg$path)]))
load_fail <- character(0); row_mis <- character(0)
for (did in pick) {
  d <- tryCatch(qres_get_raw_curves(con, did), error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) { load_fail <- c(load_fail, did); next }
  nr <- reg$n_rows[reg$dataset_id == did]
  if (!is.na(nr) && nrow(d) != nr) row_mis <- c(row_mis, did)
}
check('db', sprintf('%d sampled datasets load via qres_get_raw_curves()', length(pick)),
      with_detail(length(load_fail) == 0, paste(head(load_fail, 3), collapse = ', ')))
check('db', 'loaded row counts match the registry', with_detail(length(row_mis) == 0, paste(head(row_mis, 3), collapse = ', ')))

# every data source must give analysis results (catches legacy Parquet layouts)
for (f in list.files('R/plugins', '\\.R$', full.names = TRUE)) if (!grepl('conflicted|omniplate', f)) sys.source(f, envir = env)
no_metrics <- character(0)
for (src in unique(prefix)) {
  did <- reg$dataset_id[prefix == src][1]
  d <- tryCatch(qres_get_raw_curves(con, did), error = function(e) NULL)
  if (is.null(d)) { no_metrics <- c(no_metrics, paste0(did, ' (load)')); next }
  if ('cycle' %in% names(d)) d <- qres_split_transfer_cycles(d[d$well_id %in% unique(d$well_id)[1:2] & d$cycle %in% 1:3, ])
  d <- d[d$well_id %in% unique(d$well_id)[1:min(6, length(unique(d$well_id)))], ]
  for (pl in c('model_free', 'gcplyr')) {
    m <- tryCatch(suppressMessages(qres_compare_plugin_outputs(d, plugins = pl)), error = function(e) NULL)
    if (is.null(m) || !any(is.finite(m$metric_value))) no_metrics <- c(no_metrics, paste0(did, '/', pl))
  }
}
check('analysis', sprintf('every source (%d) gives metrics with model_free and gcplyr', length(unique(prefix))),
      with_detail(length(no_metrics) == 0, paste(no_metrics, collapse = ', ')))

# ── 3. Search functions (as used by the Search page) ─────────────────────────
nd <- function(term) length(unique(qres_search_datasets(con, term)$dataset_id))
for (ex in regmatches(app_lines, regexpr("fillSearch\\('[^']+'\\)", app_lines))) {
  term <- sub("fillSearch\\('([^']+)'\\)", '\\1', ex)
  n <- nd(term)
  if (term == 'yeast ethanol') check('search', sprintf('example "%s" stays the negative example', term), with_detail(n == 0, sprintf('%d hits', n)))
  else check('search', sprintf('example "%s" has hits', term), with_detail(n > 0, sprintf('%d hits', n)))
}
n_evo <- length(unique(q("SELECT DISTINCT dataset_id FROM well_metadata WHERE perturbation_type = 'evolution'")$dataset_id))
check('search', '"evolution" returns only evolution datasets (no E. coli flood)',
      with_detail(nd('evolution') == n_evo, sprintf('%d hits vs %d evolution datasets', nd('evolution'), n_evo)))
check('search', 'filter perturbation_type = evolution',
      with_detail(length(unique(qres_filter_datasets(con, perturbation_type = 'evolution', registered_only = FALSE)$dataset_id)) == n_evo, ''))
check('search', 'genus synonym: "Escherichia" == "E. coli"', nd('Escherichia') == nd('E. coli') && nd('E. coli') > 0)
check('search', 'underscore is literal (SW_c00003)', with_detail(nd('SW_c00003') > 0 && nd('SWXc00003') == 0, sprintf('%d hits', nd('SW_c00003'))))
check('search', 'backslash / quote input does not error', is.numeric(nd('ends\\')) && is.numeric(nd("O'Brien")))
check('search', 'empty search returns every dataset', with_detail(nd('') == n_ds, sprintf('%d of %d', nd(''), n_ds)))

# ── 4. Evolution helpers ─────────────────────────────────────────────────────
if (tc_tbl && n_evo > 0) {
  did <- q("SELECT dataset_id FROM transfer_cycle LIMIT 1")$dataset_id
  d <- qres_get_raw_curves(con, did)
  check('evolution', 'evolution Parquet has a cycle column', 'cycle' %in% names(d))
  s <- qres_split_transfer_cycles(d)
  t0 <- tapply(s$time_h, s$well_id, min)
  check('evolution', 'split cycles: every cycle curve starts at t = 0', all(abs(t0) < 1e-9))
  check('evolution', 'split cycles: no data lost', nrow(s) == nrow(d))
  check('evolution', 'split is a no-op for batch data', identical(qres_split_transfer_cycles(qres_get_raw_curves(con, reg$dataset_id[1])),
                                                                   qres_get_raw_curves(con, reg$dataset_id[1])))
}
DBI::dbDisconnect(con, shutdown = TRUE)

# ── 5. Search observer in the real server (LLM stubbed) ─────────────────────
suppressMessages(library(shiny))
llm_calls <- 0
obs <- tryCatch({
  out <- list()
  testServer(shinyAppDir('.'), {
    assign('qres_nl_to_action', function(...) { llm_calls <<- llm_calls + 1; NULL }, envir = globalenv())
    k <- 0
    for (term in c('glucose', 'E. coli', 'ampicillin', '')) {
      k <- k + 1; session$setInputs(search = term); session$setInputs(run_search = k)
      out[[if (nzchar(term)) term else '(empty)']] <<- length(unique(dataset_results()$dataset_id))
    }
  })
  out
}, error = function(e) e)
srv2 <- tryCatch({
  out <- list()
  testServer(shinyAppDir('.'), {
    assign('qres_nl_to_action', function(...) NULL, envir = globalenv())
    session$setInputs(search = 'E. coli'); session$setInputs(run_search = 1)
    out$all <<- length(unique(display_results()$dataset_id))
    session$setInputs(search_type_click = 'evolution'); out$evo <<- length(unique(display_results()$dataset_id))
    out$evo_types <<- unique(display_results()$experiment_type)
    session$setInputs(search_type_click = 'evolution'); out$back <<- length(unique(display_results()$dataset_id))
    session$setInputs(search_type_click = 'evolution', select_all_results = TRUE)
    out$sel_all <<- length(analyze_candidate_ids())
    session$setInputs(go_to_analyze = 1); out$sent <<- length(analyze_selected_ids())
    session$setInputs(search_type_click = 'evolution', select_all_results = FALSE)
    evo_did <- DBI::dbGetQuery(con, "SELECT dataset_id FROM dataset WHERE experiment_type = 'evolution' LIMIT 1")$dataset_id
    if (length(evo_did)) {
      analyze_selected_ids(c(evo_did, 'aida_cond00001')); analyse_mode_rv('B')
      out$batch_ids <<- comp_active_ids(); out$evo_ids <<- evo_ids()
      session$setInputs(evo_strains = character(0), evo_conditions = character(0), evo_log_dose = TRUE, evo_cycle_range = c(1, 5))
      out$plots <<- vapply(c('evo_dose_plot', 'evo_od_plot', 'evo_cycle_plot'),
                          function(o) !is.null(tryCatch(output[[o]]$src, error = function(e) NULL)), logical(1))
    }
  })
  out
}, error = function(e) e)
if (inherits(srv2, 'error')) check('server', 'type cards / evolution panel run', with_detail(FALSE, conditionMessage(srv2))) else {
  check('server', 'type card "Evolution" filters and toggles back',
        with_detail(srv2$evo == n_evo && identical(srv2$evo_types, 'evolution') && srv2$back == srv2$all,
                    sprintf('all %d, evolution %d, back %d', srv2$all, srv2$evo, srv2$back)))
  check('server', '"Select all results" sends every current result to Analyse',
        with_detail(identical(srv2$sel_all, n_evo) && identical(srv2$sent, n_evo),
                    sprintf('candidates %s, sent %s, expected %d', format(srv2$sel_all), format(srv2$sent), n_evo)))
  if (!is.null(srv2$plots)) {
    check('server', 'evolution datasets kept out of the batch pipeline',
          with_detail(identical(srv2$batch_ids, 'aida_cond00001') && length(srv2$evo_ids) == 1, ''))
    check('server', 'evolution panel plots render (trajectory, OD, per cycle)',
          with_detail(all(srv2$plots), paste(names(srv2$plots)[!srv2$plots], collapse = ', ')))
  }
}
if (inherits(obs, 'error')) check('server', 'search observer runs', with_detail(FALSE, conditionMessage(obs))) else {
  check('server', 'keyword hits found without asking the LLM',
        with_detail(llm_calls == 0 && all(unlist(obs[c('glucose', 'E. coli', 'ampicillin')]) > 0),
                    sprintf('LLM calls %d; %s', llm_calls, paste(names(obs), unlist(obs), sep = '=', collapse = ', '))))
  check('server', 'empty search shows the repository overview', with_detail(obs[['(empty)']] == n_ds, sprintf('%d', obs[['(empty)']])))
}

# ── 5b. Security and data lifecycle (2026-10-10 fixes) ──────────────────────
mk <- tempfile('pwned_')
bad_inputs <- c(sprintf('rep(1:2, 2); file.create("%s")', mk), sprintf('rep(file.create("%s"), 2)', mk),
                'rep(get("system")("id"), 1)', 'rep(1:9999, 9999)', '1:100000000')
check('security', 'replicate pattern is parsed, never evaluated',
      with_detail(all(vapply(bad_inputs, function(x) length(.parse_replicate_seq(x)) == 0, logical(1))) && !file.exists(mk),
                  if (file.exists(mk)) 'code was executed' else ''))
check('security', 'replicate pattern: valid notations unchanged',
      identical(.parse_replicate_seq('rep(1:12, 8)'), rep(1:12, 8)) &&
      identical(.parse_replicate_seq('rep(1:4, each = 24)'), rep(1:4, each = 24)) &&
      identical(.parse_replicate_seq('rep(c(1,2), 48)'), rep(c(1L, 2L), 48)) &&
      identical(.parse_replicate_seq('1:3, 7'), c(1:3, 7L)))
check('security', 'raw SQL from the LLM is not executed',
      'error' %in% names(qres_execute_action(con, list(sql = 'SELECT 1 AS x'))$data))
r_code <- unlist(lapply(c('app.R', list.files('R', '\\.R$', recursive = TRUE, full.names = TRUE)) |>
                          Filter(f = function(f) !grepl('conflicted copy|_old_flat', f)), readLines, warn = FALSE))
check('security', 'no eval(parse()) and no unescaped DataTables',
      with_detail(!any(grepl('eval\\s*\\(\\s*parse', r_code)) && !any(grepl('escape\\s*=\\s*FALSE', r_code)),
                  paste(head(grep('eval\\s*\\(\\s*parse|escape\\s*=\\s*FALSE', r_code, value = TRUE), 2), collapse = ' | ')))
# delete / reopen on a scratch copy: migration adds needs_correction, no orphan wells remain
scratch <- file.path(tempdir(), 'qres_lifecycle.duckdb'); file.copy(db_path, scratch, overwrite = TRUE)
lc <- tryCatch({
  c2 <- DBI::dbConnect(duckdb::duckdb(), scratch); on.exit(DBI::dbDisconnect(c2, shutdown = TRUE), add = TRUE)
  qres_migrate_schema(c2)
  has_nc <- 'needs_correction' %in% names(DBI::dbGetQuery(c2, 'SELECT * FROM upload_embargo LIMIT 0'))
  orph <- function() DBI::dbGetQuery(c2, 'SELECT count(*) n FROM well_metadata w LEFT JOIN plate p USING(plate_id) WHERE p.plate_id IS NULL')$n
  o0 <- orph()
  ids <- DBI::dbGetQuery(c2, "SELECT DISTINCT p.dataset_id FROM well_metadata w JOIN plate p USING(plate_id)
                              WHERE w.dataset_id IS NULL ORDER BY 1 LIMIT 2")$dataset_id   # older imports
  res <- vapply(seq_along(ids), function(i) {
    key <- qres_register_embargo(c2, ids[i], 'test@example.org', 'passphrase123', 'immediate')
    key <- if (is.list(key)) key$access_key %||% key[[1]] else key
    fn  <- if (i == 1) qres_delete_dataset else qres_reopen_dataset
    isTRUE(fn(c2, ids[i], 'test@example.org', key, 'passphrase123', parquet_dir = tempdir())$ok)
  }, logical(1))
  nc <- DBI::dbGetQuery(c2, 'SELECT needs_correction FROM upload_embargo WHERE dataset_id = ?', list(ids[2]))$needs_correction
  list(has_nc = has_nc, ok = all(res) && length(ids) == 2, new_orph = orph() - o0, nc = isTRUE(nc))
}, error = function(e) e)
if (inherits(lc, 'error')) check('lifecycle', 'delete / reopen', with_detail(FALSE, conditionMessage(lc))) else {
  check('lifecycle', 'migration adds upload_embargo.needs_correction', lc$has_nc)
  check('lifecycle', 'delete + reopen leave no orphan wells', with_detail(lc$ok && lc$new_orph == 0, sprintf('%d new orphans', lc$new_orph)))
  check('lifecycle', 'reopen sets needs_correction', lc$nc)
}
unlink(scratch)

# ── 6. App start (optional) ──────────────────────────────────────────────────
if (run_app) {
  port <- 48000 + sample(999, 1); log <- tempfile(fileext = '.log')
  # start in the background and remember the PID (R rewrites spaces in its command
  # line as '~+~', so pattern-based pkill is unreliable)
  pid <- system(sprintf("Rscript -e \"shiny::runApp('.', port = %d, launch.browser = FALSE)\" > %s 2>&1 & echo $!",
                        port, log), intern = TRUE)
  ok <- FALSE
  for (i in 1:60) { Sys.sleep(1); ok <- isTRUE(tryCatch(nchar(paste(readLines(sprintf('http://127.0.0.1:%d/', port), warn = FALSE), collapse = '')) > 1000, error = function(e) FALSE)); if (ok) break }
  system(sprintf('pkill -P %s; kill %s 2>/dev/null', pid, pid))
  errs <- grep('Error', readLines(log, warn = FALSE), value = TRUE)
  check('app', 'app starts and serves the start page', with_detail(ok && length(errs) == 0, paste(head(errs, 2), collapse = ' | ')))
}

n_fail <- sum(!results$ok)
cat(sprintf('\n%d checks, %d passed, %d failed\n', nrow(results), sum(results$ok), n_fail))
quit(status = if (n_fail > 0) 1 else 0)
