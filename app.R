if (file.exists('~/.Renviron')) readRenviron('~/.Renviron')
library(shiny)
library(shinythemes)
library(shiny.router)
library(shinycssloaders)
library(DT)
library(DBI)
library(duckdb)
library(ggplot2)
library(future)
library(promises)
library(memoise)
plan(multisession, workers = 2)
# LLM backends: ellmer handles Anthropic + ollama; mall for batch column ops
if (requireNamespace('ellmer', quietly = TRUE)) library(ellmer)
if (requireNamespace('mall',   quietly = TRUE)) library(mall)

# ── Module sources (load order matters) ───────────────────────────────────────
source('R/utils/helpers.R')
source('R/validation/input_checks.R')   # must precede import/ and db/ modules

source('R/db/connect.R')
source('R/db/schema.R')
source('R/db/queries.R')
source('R/db/migrations.R')

source('R/import/metadata.R')
source('R/import/generic_csv.R')
source('R/import/biotek.R')
source('R/import/tecan.R')
source('R/import/logphase_reader.R')
source('R/import/keio_import.R')
source('R/import/aida_import.R')
source('R/import/baker_import.R')
source('R/import/waithaka_import.R')
source('R/import/taylorcrooks_import.R')
source('R/import/tamminen_import.R')
source('R/import/eisner_import.R')

source('R/plugins/model_free.R')
source('R/plugins/gcplyr.R')
source('R/plugins/growthrates.R')
source('R/plugins/dgrothr.R')
source('R/plugins/kinbiont.R')
source('R/plugins/registry.R')

source('R/plots/growth_curves.R')
source('R/plots/comparison.R')
source('R/plots/qc.R')

source('R/validation/metadata_checks.R')
source('R/validation/plate_checks.R')

source('R/llm/schema_context.R')
source('R/llm/nl_query.R')
source('R/llm/rule_parser.R')

source('R/embargo/access_key.R')
source('R/embargo/release_check.R')
source('R/utils/email.R')

# ── Shared bottom navigation bar ─────────────────────────────────────────────
bottom_nav <- tagList(
  tags$head(tags$style(HTML("
    .bottom-fixed {
      position: fixed; bottom: 0; width: 100%;
      background-color: #f8f8f8; border-top: 1px solid #ddd;
      padding: 0; z-index: 1000; font-size: 15px;
    }
    .bottom-fixed a { text-decoration: none; color: #337ab7; }
    .bottom-fixed a:hover { color: #23527c; }
    .pictogram-caption { font-weight: bold; margin-top: 6px; }
    .pictogram-cell img:hover { opacity: 0.8; transform: scale(1.05);
      transition: all 0.2s ease; }
    .list-group-item { border-radius: 4px !important; margin-bottom: 4px; }
  "))),
  tags$div(
    class = 'bottom-fixed',
    tags$div(class = 'row', style = 'margin:0;',
      tags$div(class = 'col-xs-6', style = 'text-align:center; padding:10px 0; border-right:1px solid #ddd;',
        tags$a(href = route_link('/'), icon('home'), ' Home')),
      tags$div(class = 'col-xs-6', style = 'text-align:center; padding:10px 0;',
        tags$a(href = '#', onclick = 'history.back(); return false;',
               icon('arrow-left'), ' Return'))
    )
  )
)

# ── HOME PAGE ─────────────────────────────────────────────────────────────────
ui_home <- fluidPage(
  theme = shinytheme('cerulean'),
  tags$head(tags$style(HTML("
    body { padding-bottom: 60px; }

    /* ── Hero ── */
    .home-hero { text-align: center; padding: 36px 0 12px; }
    .home-logo-row { display: flex; align-items: center;
                     justify-content: center; gap: 24px; margin-bottom: 10px;
                     background: none; }
    .home-logo-row img { background: none; border: none; box-shadow: none; }
    .home-title { font-size: 32px; font-weight: 700;
                  color: #1a3a5c; margin: 0 0 10px; }
    .home-tagline { color: #444; font-size: 19px;
                    max-width: 640px; margin: 0 auto 6px; }
    .home-abbrev  { color: #888; font-size: 15px;
                    max-width: 500px; margin: 0 auto 28px; line-height: 1.7; }

    /* ── Primary action cards ── */
    .home-primary { display: flex; justify-content: center;
                    gap: 20px; flex-wrap: wrap; margin-bottom: 10px; }
    .home-card {
      display: flex; flex-direction: column; align-items: center;
      width: 230px; padding: 22px 16px 18px;
      background: #fff; border: 1px solid #d0dce8;
      border-radius: 10px; text-decoration: none; color: #1a3a5c;
      box-shadow: 0 2px 6px rgba(0,0,0,.07);
      transition: box-shadow .18s, transform .18s, border-color .18s;
    }
    .home-card:hover {
      box-shadow: 0 6px 18px rgba(0,0,0,.13);
      transform: translateY(-3px); border-color: #2980b9;
      text-decoration: none; color: #1a3a5c;
    }
    .home-card img { width: 150px; height: 150px; margin-bottom: 12px; }
    .home-card-label { font-weight: 700; font-size: 19px; margin-bottom: 4px; }
    .home-card-sub   { color: #888; font-size: 15px; text-align: center; }

    /* ── Secondary nav links ── */
    .home-secondary {
      display: flex; justify-content: center;
      gap: 28px; flex-wrap: wrap; margin-top: 114px; margin-bottom: 16px;
    }
    .home-sec-link {
      display: flex; flex-direction: column; align-items: center;
      text-decoration: none; color: #555;
      transition: color .15s;
    }
    .home-sec-link:hover { color: #2980b9; text-decoration: none; }
    .home-sec-link img { width: 65px; height: 65px;
                         margin-bottom: 5px; opacity: .75; }
    .home-sec-link:hover img { opacity: 1; }
    .home-sec-label { font-size: 15px; font-weight: 600;
                      text-align: center; max-width: 90px; }

    /* ── Stats bar ── */
    .home-stats-bar {
      display: flex; justify-content: center; gap: 40px; flex-wrap: wrap;
      margin: 18px auto 6px; padding: 14px 28px;
      background: #f0f6ff; border-radius: 10px; max-width: 700px;
    }
    .home-stat { display: flex; flex-direction: column; align-items: center; }
    .home-stat-n { font-size: 22px; font-weight: 700; color: #1a3a5c; }
    .home-stat-label { font-size: 12px; color: #666;
                       text-transform: uppercase; letter-spacing: .04em; }
  "))),

  tags$div(class = 'home-hero',

    # ── Logos + title ──────────────────────────────────────────────────────
    tags$div(class = 'home-logo-row',
      img(src = 'PhaMiG_logo.png',  height = '112px', alt = 'PhaMiG logo',
          style = 'display:block;'),
      img(src = 'QresDB_logo.png',  height = '112px', alt = 'Qres DB logo',
          style = 'display:block;')
    ),

    tags$h2(class = 'home-title',
      'PhaMiG Platform', tags$span(style = 'font-weight:400; color:#2980b9;',
      ' Powered by Qres DB')),

    tags$p(class = 'home-tagline',
      'Repository and benchmarking platform for microbial growth and microbial phenotyping time-series data.'),

    tags$p(class = 'home-abbrev',
      tags$strong('PhaMiG'), HTML(' &ndash; Phenotypic Analysis of Microbial Growth'),
      tags$br(),
      tags$strong('Qres DB'), HTML(' &ndash; Quantitative Dose–Response Database')
    ),

    # ── Three primary action cards ─────────────────────────────────────────
    tags$div(class = 'home-primary',
      tags$a(class = 'home-card', href = route_link('search'),
        img(src = 'search_parse2.png', alt = 'Search'),
        tags$span(class = 'home-card-label', 'Search'),
        tags$span(class = 'home-card-sub',   'Find existing datasets.')
      ),
      tags$a(class = 'home-card', href = route_link('submit'),
        img(src = 'submit2.png', alt = 'Submit'),
        tags$span(class = 'home-card-label', 'Submit'),
        tags$span(class = 'home-card-sub',   'Deposit new datasets.')
      ),
      tags$a(class = 'home-card', href = route_link('analyse'),
        img(src = 'data_analysis2.png', alt = 'Analyze'),
        tags$span(class = 'home-card-label', 'Analyze'),
        tags$span(class = 'home-card-sub',   'Analyze uploaded growth curves.')
      )
    ),

    # ── Live stats bar ─────────────────────────────────────────────────────
    uiOutput('home_stats_bar'),

    # ── Secondary nav ──────────────────────────────────────────────────────
    tags$div(class = 'home-secondary',
      tags$a(class = 'home-sec-link', href = route_link('metadata_prep'),
        img(src = 'metadata_prep2.png', alt = 'Metadata Preparation'),
        tags$span(class = 'home-sec-label', 'Metadata Preparation')
      ),
      tags$a(class = 'home-sec-link', href = route_link('ask'),
        img(src = 'contact2.png', alt = 'Natural Language Query'),
        tags$span(class = 'home-sec-label', 'Ask Qres DB')
      ),
      tags$a(class = 'home-sec-link', href = route_link('plugins'),
        img(src = 'project_development2.png', alt = 'Learn'),
        tags$span(class = 'home-sec-label', 'Learn')
      ),
      tags$a(class = 'home-sec-link', href = route_link('acknowledgement'),
        img(src = 'acknowledgement.png', alt = 'Acknowledgements'),
        tags$span(class = 'home-sec-label', 'Acknowledgements')
      )
    )

  ),

  bottom_nav
)

# ── METADATA PREPARATION ──────────────────────────────────────────────────────
ui_metadata <- fluidPage(
  theme = shinytheme('cerulean'),
  tags$head(tags$style(HTML('
    body { padding-bottom: 60px; }
    .required-label { font-weight: bold; }
    .required-star  { color: #c0392b; font-weight: bold; }
    .optional-label { color: #777; }
    .key-box { background:#f0f8ff; border:1px solid #b8d4e8;
               border-radius:4px; padding:10px; margin-bottom:8px; font-size:13px; }
  '))),
  fluidRow(
    column(4, tags$span(
  img(src = 'PhaMiG_logo.png',  height = '52px', alt = 'PhaMiG',
      style = 'margin-right:8px;'),
  img(src = 'QresDB_logo.png',  height = '52px', alt = 'Qres DB')
)),
    column(8,
      h3('Metadata Template Preparation'),
      tags$div(class = 'alert alert-info', style = 'margin-bottom:0; font-size:13px; padding:10px 14px;',
        icon('circle-info'), tags$strong(' What this page does:'), ' ',
        'Describe the ', tags$strong('per-well layout'), ' of your plate — which wells carry which concentration, which are blanks, and which are controls.',
        tags$br(),
        tags$span(style = 'color:#31708f;',
          'Species, medium, temperature, and publication details are entered once on the ',
          tags$strong('Submit'), ' page — not here.'
        ),
        tags$br(),
        tags$small(tags$span(class = 'required-star', '★'), ' = required')
      )
    )
  ),
  hr(),
  sidebarLayout(
    sidebarPanel(width = 3,

      selectInput('plateSize', tags$span(class='required-label', 'Plate format ★'),
                  choices = c('96-well (8×12)' = '8x12',
                              '384-well (16×24)' = '16x24',
                              '48-well (6×8)'  = '6x8',
                              '24-well (4×6)'  = '4x6')),

      radioButtons('meta_fill_order',
        opt_label('Pipetting direction',
          'Controls the order in which values (replicates, mutations, temperature gradient) are assigned to wells. Row-by-row (A1→A2→…→H12) is standard for horizontal multi-channel pipettes. Column-by-column (A1→B1→…→H12) suits those who pipette down columns first.'),
        inline = TRUE,
        choices = c('Row-by-row' = 'row', 'Column-by-column' = 'col'),
        selected = 'row'),

      tags$label(class='required-label', 'Perturbation type ★'),
      radioButtons('meta_perturb_type', label = NULL,
        choices = c('Chemical stressor'           = 'chemical',
                    'Genetic modification'         = 'genetic',
                    'Chemical + Genetic'           = 'chemical_genetic',
                    'Temperature gradient'         = 'temperature',
                    'None (replicates only)'       = 'none'),
        selected = 'none'),

      tags$hr(),

      conditionalPanel("input.meta_perturb_type == 'chemical'",
        textInput('meta_perturb',
          tags$span(class='required-label', 'Perturbation name ★'),
          placeholder = 'rifampicin'),
        fluidRow(
          column(7, textInput('meta_conc', 'Starting concentration', placeholder = '128')),
          column(5, textInput('meta_conc_unit', 'Unit', placeholder = 'µg/mL'))
        ),
        checkboxInput('meta_dilution', 'Serial dilution along columns'),
        conditionalPanel('input.meta_dilution == true',
          textInput('meta_ratio', 'Dilution ratio', placeholder = '1:2'))
      ),

      conditionalPanel("input.meta_perturb_type == 'genetic'",
        textInput('meta_genetic_bg',
          opt_label('Mutation / genetic background ★',
            'Comma-separated list of mutations or strain names. Assigned in order to all wells that are not blank and not control, cycling if there are more wells than mutations. Example: ΔtrpB, ΔlacZ, ΔaraB → A6 gets ΔtrpB, A7 gets ΔlacZ, A8 gets ΔaraB, A9 gets ΔtrpB again, …'),
          placeholder = 'ΔtrpB, ΔlacZ, ΔaraB'),
        tags$small(style = 'display:block; margin:8px 0 3px; color:#555;',
          icon('circle-info'), ' ',
          'The ', tags$strong('concentration'), ' column can represent inoculation volume,',
          ' initial cell density, or any numeric gradient — for example a serial dilution',
          ' of a mutant stock across columns.'
        ),
        fluidRow(
          column(7, textInput('meta_gen_conc', 'Starting concentration',
                              placeholder = '1000')),
          column(5, textInput('meta_gen_conc_unit', 'Unit',
                              placeholder = 'cells/mL'))
        ),
        checkboxInput('meta_gen_dilution', 'Serial dilution along columns'),
        conditionalPanel('input.meta_gen_dilution == true',
          textInput('meta_gen_ratio', 'Dilution ratio', placeholder = '1:2'))
      ),

      conditionalPanel("input.meta_perturb_type == 'chemical_genetic'",
        tags$small(tags$strong('— Chemical stressor —')),
        textInput('meta_cg_perturb',
          tags$span(class='required-label', 'Drug / stressor name ★'),
          placeholder = 'rifampicin'),
        fluidRow(
          column(7, textInput('meta_cg_chem_conc', 'Starting concentration',
                              placeholder = '128')),
          column(5, textInput('meta_cg_chem_unit', 'Unit', placeholder = 'µg/mL'))
        ),
        checkboxInput('meta_cg_chem_dilution', 'Serial dilution along columns'),
        conditionalPanel('input.meta_cg_chem_dilution == true',
          textInput('meta_cg_chem_ratio', 'Dilution ratio', placeholder = '1:2')),
        tags$hr(style = 'margin:6px 0;'),
        tags$small(tags$strong('— Genetic modification —')),
        textInput('meta_cg_genetic_bg',
          opt_label('Mutation / genetic background ★',
            'Comma-separated mutations cycling across non-blank, non-control wells — same logic as the Genetic modification type. Per well the perturbation column stores: drug;mutation.'),
          placeholder = 'ΔtrpB, ΔlacZ'),
        tags$small(style = 'display:block; margin:6px 0 3px; color:#555;',
          icon('circle-info'), ' ',
          'Concentration column stores two values: ',
          tags$code('chemical_conc;inoculum'),
          ' — first = drug concentration, second = inoculation volume.'
        ),
        fluidRow(
          column(7, textInput('meta_cg_inoc_conc', 'Inoculum starting value',
                              placeholder = '1000')),
          column(5, textInput('meta_cg_inoc_unit', 'Unit', placeholder = 'cells/mL'))
        ),
        checkboxInput('meta_cg_inoc_dilution', 'Serial dilution along columns'),
        conditionalPanel('input.meta_cg_inoc_dilution == true',
          textInput('meta_cg_inoc_ratio', 'Inoculum dilution ratio', placeholder = '1:2'))
      ),

      conditionalPanel("input.meta_perturb_type == 'temperature'",
        textInput('meta_temp_start',
          tags$span(class='required-label', 'Starting temperature (°C) ★'),
          placeholder = '25'),
        textInput('meta_temp_step',
          opt_label('Step per column (°C)',
            'Temperature increment between adjacent columns. If left blank but Ending temperature is set, the step is calculated automatically from start → end across the plate columns.'),
          placeholder = '1'),
        textInput('meta_temp_end',
          opt_label('Ending temperature (°C)',
            'Temperature of the last column. Used to auto-calculate the step when Step is left blank.'),
          placeholder = '45'),
        textInput('meta_temp_control',
          opt_label('Control temperature (°C)',
            'Column whose temperature matches this value will be marked as control (is_control = TRUE). Typically 37°C for E. coli experiments.'),
          placeholder = '37')
      ),

      tags$hr(),

      textInput('meta_control_wells',
        opt_label('Control wells (no perturbation)',
          'Wells that received no treatment (concentration set to 0). Comma-separated or range: A12:H12 = all 8 wells of column 12; A1:A12 = full row A across all 12 columns.'),
        placeholder = 'e.g. A12:H12'),

      textInput('meta_blank_wells',
        opt_label('Blank wells',
          'Wells containing medium only — no bacteria. Comma-separated or range: A12:E12 = A12, B12, C12, D12, E12 (same column, rows A to E); H1:H12 = full row H.'),
        placeholder = 'e.g. A1, H12 or H1:H12'),

      tags$hr(),
      actionButton('applyMetadata', 'Apply & preview',
                   class = 'btn-primary', width = '100%'),
      br(), br(),
      downloadButton('downloadTemplate', 'Download plate template CSV',
                     class = 'btn-success'),
      tags$hr(style = 'margin:10px 0;'),
      actionButton('update_meta', 'Update', width = '100%', class = 'btn-default'),
      br(), br(),
      actionButton('clear_meta',  'Clear',  width = '100%', class = 'btn-default'),
      br(), br(),
      actionButton('toggle_optional', 'Optional fields', width = '100%', class = 'btn-default'),
      uiOutput('optional_fields_ui')
    ),

    mainPanel(width = 9,
      tabsetPanel(id = 'meta_tabs',
        tabPanel('Plate preview',
          br(),
          DTOutput('plateMatrix'),
          br(),
          helpText('Only columns containing at least one value are shown.',
                   'Empty columns = not yet applied.')),

        tabPanel('Field reference',
          br(),
          tags$div(class = 'alert alert-info', style = 'font-size:13px; padding:10px 14px;',
            icon('circle-info'), ' ',
            'These are the columns in the ', tags$strong('plate template CSV'), ' you download from this page.',
            tags$br(),
            tags$span(style = 'color:#31708f;',
              'Dataset-level fields — species, medium, temperature, plate reader, authors, DOI — ',
              'are entered separately on the ', tags$strong('Submit'), ' page.'
            )
          ),
          wellPanel(
            tags$h5('Plate template columns'),
            tags$table(class = 'table table-condensed table-bordered',
              style = 'font-size:13px;',
              tags$thead(
                tags$tr(tags$th('Column'), tags$th('Set by'), tags$th('Description'), tags$th('Example'))
              ),
              tags$tbody(
                tags$tr(
                  tags$td(tags$code('well')),
                  tags$td('auto'),
                  tags$td('Well identifier'),
                  tags$td('A1, B12, H12')),
                tags$tr(
                  tags$td(tags$code('perturbation_type')),
                  tags$td('form'),
                  tags$td(tags$code('chemical'), ' | ', tags$code('genetic'), ' | ',
                          tags$code('temperature'), ' | ', tags$code('none')),
                  tags$td('chemical')),
                tags$tr(
                  tags$td(tags$code('perturbation')),
                  tags$td('form'),
                  tags$td('Drug name, mutation, or temperature value per well'),
                  tags$td('rifampicin / ΔtrpB / 37')),
                tags$tr(
                  tags$td(tags$code('concentration')),
                  tags$td('form'),
                  tags$td('Numeric concentration (chemical only); 0 = control well'),
                  tags$td('128, 64, 32…')),
                tags$tr(
                  tags$td(tags$code('concentration_unit')),
                  tags$td('form'),
                  tags$td('Unit for concentration'),
                  tags$td('µg/mL, mg/L, mM')),
                tags$tr(
                  tags$td(tags$code('is_blank')),
                  tags$td('Blank wells'),
                  tags$td('Medium only — no bacteria'),
                  tags$td('TRUE / FALSE')),
                tags$tr(
                  tags$td(tags$code('is_control')),
                  tags$td('Control wells'),
                  tags$td('No perturbation applied'),
                  tags$td('TRUE / FALSE')),
                tags$tr(
                  tags$td(tags$span(class='optional-label', 'medium_state')),
                  tags$td('Optional fields'),
                  tags$td('Physical state of medium (default: liquid)'),
                  tags$td('liquid, solid, semi-solid')),
                tags$tr(
                  tags$td(tags$span(class='optional-label', 'taxon_id')),
                  tags$td('Optional fields'),
                  tags$td('NCBI Taxonomy numeric ID'),
                  tags$td('562')),
                tags$tr(
                  tags$td(tags$span(class='optional-label', 'replicate')),
                  tags$td('Optional fields'),
                  tags$td('Replicate number per well — cycled across non-blank, non-control wells'),
                  tags$td('1, 2, 3, 4 → repeats'))
              )
            )
          ),
          wellPanel(
            tags$h5('Workflow'),
            tags$ol(
              tags$li(tags$strong('Here (Metadata Preparation):'),
                      ' describe the plate layout → Apply & preview → download plate template CSV.'),
              tags$li(tags$strong('Submit page:'),
                      ' upload your raw OD CSV + the plate template CSV, and fill in species, medium, temperature, DOI, etc.'),
              tags$li('The app links both files via ', tags$code('dataset_id'),
                      ' and stores everything in the database.')
            ),
            tags$p(tags$strong('Raw OD CSV format:'),
                   ' column 1 named ', tags$code('Time'), ' (values in hours);',
                   ' columns 2–N are OD values with well names as headers (A1, A2, … H12).')
          )
        )
      )
    )
  ),
  bottom_nav
)

# ── SEARCH & BROWSE ───────────────────────────────────────────────────────────
ui_search <- fluidPage(
  theme = shinytheme('cerulean'),
  tags$head(
    tags$style(HTML('
      body { padding-bottom: 60px; }
      .dataTables_wrapper { overflow-x: auto; }
      table.dataTable td { white-space: nowrap; }

      /* ── Google-style search page ─────────────────────────────────────── */
      .search-page-wrap {
        display: flex; flex-direction: column;
        min-height: calc(100vh - 130px);
      }

      /* Hero: vertically centred when no results are shown */
      #search-hero-section {
        display: flex; flex-direction: column;
        align-items: center; justify-content: center;
        flex: 1 0 220px;
        padding: 7vh 16px 24px;
        transition: padding .22s ease, flex .22s ease;
      }
      #search-hero-section.compact {
        flex: 0 0 auto;
        padding: 14px 16px 10px;
        border-bottom: 1px solid #dde4eb;
        margin-bottom: 18px;
      }

      /* Logos */
      .search-logo-wrap { margin-bottom: 28px; text-align: center; }
      #search-hero-section.compact .search-logo-wrap { margin-bottom: 8px; }
      #search-hero-section.compact .search-logo-wrap img { height: 36px; }

      /* Search input */
      .search-hero { width: 100%; max-width: 620px; text-align: center; }
      .search-hero .form-group { margin-bottom: 6px; }
      .search-hero .form-control {
        font-size: 16px; height: 50px;
        border-radius: 24px;
        border: 1px solid #dfe1e5;
        padding: 10px 20px;
        box-shadow: 0 1px 6px rgba(32,33,36,.13);
        transition: box-shadow .18s;
      }
      .search-hero .form-control:focus,
      .search-hero .form-control:hover {
        box-shadow: 0 1px 14px rgba(32,33,36,.22);
        border-color: transparent; outline: none;
      }
      #search-hero-section.compact .search-hero .form-control {
        height: 38px; font-size: 14px; border-radius: 18px; padding: 6px 16px;
        box-shadow: 0 1px 4px rgba(32,33,36,.10);
      }

      /* Examples */
      .search-examples { color: #888; font-size: 12px; margin: 6px 0 14px; }
      .search-examples a { cursor: pointer; color: #2980b9; text-decoration: none; }
      .search-examples a:hover { text-decoration: underline; }
      #search-hero-section.compact .search-examples { display: none; }

      /* No-results notice */
      .search-no-results {
        color: dimgray; font-size: 14px; text-align: center;
        margin-top: 18px; font-style: italic;
      }

      /* Result header */
      .result-count { font-size: 15px; font-weight: 600; color: #1a3a5c; margin: 0 0 6px; }

      /* Facets */
      .facet-panel { font-size: 13px; color: #333; }
      .facet-panel h6 {
        font-weight: 700; font-size: 11px; text-transform: uppercase;
        color: #777; margin: 14px 0 4px; letter-spacing: .05em;
      }
      .facet-row {
        display: flex; justify-content: space-between;
        padding: 2px 0; border-bottom: 1px dotted #eee;
      }
      .facet-count { color: #aaa; font-size: 11px; min-width: 24px; text-align: right; }

      /* Advanced filters toggle */
      details > summary {
        cursor: pointer; color: #2980b9; font-size: 13px;
        user-select: none; padding: 6px 0; list-style: none;
      }
      details > summary::-webkit-details-marker { display: none; }
      details > summary::before { content: "\\25B6  "; font-size: 10px; }
      details[open] > summary::before { content: "\\25BC  "; font-size: 10px; }
      .adv-section label { font-size: 12px; color: #555; margin-bottom: 2px; }
      .adv-section .form-group { margin-bottom: 5px; }
      .adv-section .form-control { height: 28px; font-size: 12px; padding: 2px 6px; }
      .adv-section select.form-control { height: 28px; }
      .adv-group-label {
        font-size: 10px; font-weight: 700; text-transform: uppercase;
        color: #aaa; letter-spacing: .06em; margin: 10px 0 4px; display: block;
      }

      /* Dataset detail panel */
      .dataset-detail { margin-top: 18px; border-top: 2px solid #e0e8f0; padding-top: 10px; }
    ')),
    tags$link(rel = 'stylesheet',
      href = 'https://cdn.datatables.net/buttons/2.4.1/css/buttons.dataTables.min.css'),
    tags$script(src = 'https://cdn.datatables.net/buttons/2.4.1/js/dataTables.buttons.min.js'),
    tags$script(src = 'https://cdn.datatables.net/buttons/2.4.1/js/buttons.colVis.min.js'),
    tags$script(HTML("
      function fillSearch(txt) {
        var el = document.getElementById('search');
        if (!el) return;
        el.value = txt;
        Shiny.setInputValue('search', txt);
        setTimeout(function() {
          var btn = document.getElementById('run_search');
          if (btn) btn.click();
        }, 80);
      }
      Shiny.addCustomMessageHandler('searchStateChange', function(msg) {
        var hero = document.getElementById('search-hero-section');
        if (!hero) return;
        hero.classList.toggle('compact', msg.hasResults);
      });
      $(document).on('keydown', '#search', function(e) {
        if (e.key === 'Enter') {
          e.preventDefault();
          var btn = document.getElementById('run_search');
          if (btn) btn.click();
        }
      });
    "))
  ),

  tags$div(class = 'search-page-wrap',

    # ── Hero: logos + search box (always visible, compacts when results appear) ──
    tags$div(id = 'search-hero-section',

      tags$div(class = 'search-logo-wrap',
        img(src = 'PhaMiG_logo.png', height = '52px', alt = 'PhaMiG',
            style = 'margin-right:8px;'),
        img(src = 'QresDB_logo.png', height = '52px', alt = 'Qres DB')
      ),

      tags$div(class = 'search-hero',
        tags$div(style = 'display:flex; align-items:flex-end; gap:6px;',
          tags$div(style = 'flex:1;',
            textInput('search', label = NULL,
                      placeholder = 'Search microbial growth datasets…',
                      width = '100%')
          ),
          actionButton('clear_search', icon('times'),
                       class = 'btn-default',
                       title = 'Clear and return to start',
                       style = 'height:50px; margin-bottom:15px; flex-shrink:0; border-radius:50%;')
        ),
        tags$div(class = 'search-examples',
          'Try: ',
          tags$a(onclick = "fillSearch('E. coli 37°C')",                    'E. coli 37°C'), '  ·  ',
          tags$a(onclick = "fillSearch('Bacillus subtilis tetracycline')",   'Bacillus tetracycline'), '  ·  ',
          tags$a(onclick = "fillSearch('yeast ethanol stress')",             'yeast ethanol')
        ),
        fluidRow(column(6, offset = 3,
          actionButton('run_search', tagList(icon('search'), ' Search'),
                       class = 'btn-primary btn-lg', width = '100%')
        ))
      ),

      # Shown after a search that returned nothing
      conditionalPanel(
        condition = 'output.search_performed_flag === true && output.search_has_results === false',
        tags$p(class = 'search-no-results',
               'No datasets found for the given criteria.')
      )
    ),

    # ── Results (only when search returned matches) ──────────────────────────
    conditionalPanel(
      condition = 'output.search_has_results === true',

      fluidRow(
        column(12, uiOutput('search_result_bar'))
      ),

      fluidRow(

        # Left: facets + advanced filters
        column(3,
          uiOutput('search_facets'),
          br(),
          tags$details(
            tags$summary('Advanced filters'),
            tags$div(class = 'adv-section',
              tags$span(class = 'adv-group-label', 'Biology'),
              textInput('adv_species',  'Species',  placeholder = 'e.g. Escherichia coli', width = '100%'),
              textInput('adv_strain',   'Strain',   placeholder = 'e.g. K-12',             width = '100%'),
              textInput('adv_medium',   'Medium',   placeholder = 'e.g. LB, M9',           width = '100%'),
              fluidRow(
                column(6, numericInput('adv_temp_min', 'Temp min °C', value = NA, step = 1)),
                column(6, numericInput('adv_temp_max', 'Temp max °C', value = NA, step = 1))
              ),
              selectInput('adv_perturb_type', 'Perturbation type',
                choices = c('(all)' = '', 'chemical', 'genetic', 'temperature', 'none'),
                width = '100%'),
              textInput('adv_perturb', 'Perturbation', placeholder = 'e.g. rifampicin', width = '100%'),
              tags$hr(style = 'margin:4px 0;'),
              tags$span(class = 'adv-group-label', 'Experimental'),
              textInput('adv_plate_reader', 'Plate reader', placeholder = 'e.g. Biotek', width = '100%'),
              selectInput('adv_signal_type', 'Measurement type',
                choices = c('(all)'                       = '',
                            'OD600 (absorbance at 600 nm)' = 'OD600',
                            'OD750 (absorbance at 750 nm)' = 'OD750',
                            'Fluorescence intensity (RFU)'  = 'fluorescence',
                            'Luminescence (RLU)'            = 'luminescence',
                            'Viable cells (CFU/mL)'         = 'CFU',
                            'Cell count (cells/mL)'         = 'cell_count'),
                width = '100%'),
              selectInput('adv_measurement_unit', 'Measurement unit',
                choices = c('(all)'    = '',
                            'OD'       = 'OD',
                            'RFU'      = 'RFU',
                            'RLU'      = 'RLU',
                            'CFU/ml'   = 'CFU/ml',
                            'cells/ml' = 'cells/ml'),
                width = '100%'),
              tags$hr(style = 'margin:4px 0;'),
              tags$span(class = 'adv-group-label', 'Publication'),
              textInput('adv_author', 'Author', placeholder = 'e.g. Bollenbach', width = '100%'),
              fluidRow(
                column(6, numericInput('adv_year_min', 'From year', value = NA, min = 1950, max = 2035, step = 1)),
                column(6, numericInput('adv_year_max', 'To year',   value = NA, min = 1950, max = 2035, step = 1))
              ),
              textInput('adv_doi', 'DOI', placeholder = 'e.g. 10.1038/…', width = '100%'),
              br(),
              actionButton('adv_clear', 'Clear all filters', class = 'btn-default btn-xs btn-block'),
              tags$hr(style = 'margin:8px 0;'),
              actionButton('init_db', tagList(icon('database'), ' Initialize / refresh DB'),
                           class = 'btn-default btn-xs btn-block',
                           title = 'Scans data/raw_csv/ and data/metaData.csv and registers them in DuckDB.')
            )
          )
        ),

        # Right: table + actions + detail panel
        column(9,
          uiOutput('signal_type_legend'),
          DTOutput('dataset_table'),
          br(),
          fluidRow(
            column(6, uiOutput('analyze_data_btn_ui')),
            column(6, align = 'right', uiOutput('download_search_ui'))
          ),
          conditionalPanel('output.has_selected_dataset',
            tags$div(class = 'dataset-detail',
              uiOutput('detail_selector_ui'),
              tabsetPanel(id = 'datasetDetailTabs',
                tabPanel('Links',
                  br(),
                  uiOutput('ncbi_link_box'),
                  uiOutput('gene_links_box'),
                  uiOutput('medium_comp_box'),
                  uiOutput('doi_link_box')
                ),
                tabPanel('QC',
                  br(),
                  uiOutput('qc_unavailable_msg'),
                  plotOutput('qc_cv_plot',      height = '260px'),
                  br(),
                  plotOutput('qc_heatmap_plot', height = '260px')
                )
              )
            )
          )
        )
      )
    )

  ), # end search-page-wrap

  bottom_nav
)

# ── ANALYSE & COMPARE ─────────────────────────────────────────────────────────
ui_analyse <- fluidPage(
  theme = shinytheme('cerulean'),
  tags$head(tags$style(HTML('
    body { padding-bottom: 60px; }
    .ac-row {
      display: flex; gap: 18px; justify-content: center;
      margin: 20px 0 28px; flex-wrap: wrap;
    }
    .ac-card {
      display: flex; flex-direction: column; align-items: center;
      width: 200px; padding: 20px 14px 16px;
      background: #fff; border: 2px solid #d0dce8; border-radius: 10px;
      cursor: pointer; text-align: center; user-select: none;
      box-shadow: 0 2px 6px rgba(0,0,0,.07);
      transition: box-shadow .18s, transform .18s, border-color .18s;
    }
    .ac-card:hover {
      box-shadow: 0 6px 18px rgba(0,0,0,.13);
      transform: translateY(-3px); border-color: #2980b9;
    }
    .ac-card.ac-active {
      border-color: #2980b9; background: #eaf4fb;
      box-shadow: 0 4px 12px rgba(41,128,185,.22);
    }
    .ac-card.ac-disabled {
      opacity: 0.4; cursor: not-allowed; pointer-events: none;
    }
    .ac-card img  { width: 80px; height: 80px; margin-bottom: 10px; }
    .ac-label     { font-weight: 700; font-size: 15px; color: #1a3a5c; margin-bottom: 3px; }
    .ac-sub       { color: #888; font-size: 12px; }
    .ac-active .ac-label { color: #1a6ba0; }
    /* ── plate map ── */
    .plate-map-wrap { overflow-x: auto; margin: 2px 0 6px; }
    .pm-table { border-collapse: collapse; }
    .pm-table th { font-size:10px; color:#888; padding:1px 3px; text-align:center; font-weight:normal; }
    .pm-table td { width:26px; height:26px; border:1px solid #ccc; cursor:pointer; text-align:center;
                   font-size:9px; vertical-align:middle; transition:opacity .12s; }
    .pm-table td:hover { opacity:.72; }
    .pm-empty  { background:#f0f0f0 !important; cursor:default !important; pointer-events:none; }
    .pm-active { background:#d5e8f5; color:#444; }
    .pm-blank  { background:#ffd700; color:#333; }
    .pm-ctrl   { background:#2c2c2c; color:#fff; }
  '))),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('setAnalyseMode', function(data) {
      $('.ac-card').removeClass('ac-active');
      if (data.mode) {
        $('.ac-card[data-mode=\"' + data.mode + '\"]').addClass('ac-active');
      }
      if (data.hasIds) {
        $('.ac-card[data-mode=\"B\"], .ac-card[data-mode=\"C\"]')
          .removeClass('ac-disabled');
      } else {
        $('.ac-card[data-mode=\"B\"], .ac-card[data-mode=\"C\"]')
          .addClass('ac-disabled');
      }
    });
  ")),
  fluidRow(
    column(4,
      img(src = 'PhaMiG_logo.png', height = '52px', alt = 'PhaMiG',
          style = 'margin-right:8px; vertical-align:middle;'),
      img(src = 'QresDB_logo.png',  height = '52px', alt = 'Qres DB',
          style = 'vertical-align:middle;')
    ),
    column(8, h3('Analyze Data'))
  ),
  hr(),

  # ── Mode selector cards ────────────────────────────────────────────────────
  tags$div(class = 'ac-row',
    tags$div(class = 'ac-card', `data-mode` = 'A',
      onclick = "Shiny.setInputValue('analyse_mode_click', 'A', {priority: 'event'})",
      img(src = 'qres_my_data.png',    alt = 'My file'),
      tags$div(class = 'ac-label', 'My file'),
      tags$div(class = 'ac-sub',   'Upload & analyse your own data')
    ),
    tags$div(class = 'ac-card ac-disabled', `data-mode` = 'B',
      onclick = "Shiny.setInputValue('analyse_mode_click', 'B', {priority: 'event'})",
      img(src = 'qres_repository.png', alt = 'Repository'),
      tags$div(class = 'ac-label', 'Repository'),
      tags$div(class = 'ac-sub',   'Analyse datasets from Search Data')
    ),
    tags$div(class = 'ac-card ac-disabled', `data-mode` = 'C',
      onclick = "Shiny.setInputValue('analyse_mode_click', 'C', {priority: 'event'})",
      img(src = 'qres_compare.png',    alt = 'My file + repository'),
      tags$div(class = 'ac-label', 'My file + repository'),
      tags$div(class = 'ac-sub',   'Combine upload with DB datasets')
    )
  ),


  uiOutput('analyse_main_panel'),
  bottom_nav
)

# ── ASK (LLM) ─────────────────────────────────────────────────────────────────
ui_ask <- fluidPage(
  theme = shinytheme('cerulean'),
  tags$head(tags$style(HTML('body { padding-bottom: 60px; }'))),
  fluidRow(
    column(4, tags$span(
  img(src = 'PhaMiG_logo.png',  height = '52px', alt = 'PhaMiG',
      style = 'margin-right:8px;'),
  img(src = 'QresDB_logo.png',  height = '52px', alt = 'Qres DB')
)),
    column(8, h3('Ask in Natural Language'))
  ),
  hr(),
  p('Ask any question about the growth curve database in plain English. The LLM translates it to SQL or a comparison query.'),
  fluidRow(
    column(10,
      textAreaInput('nl_question', NULL,
        value = '',
        placeholder = paste(
          'e.g. "Compare fitted curves using all available tools for E. coli',
          'growth at 35–39°C in MOPS medium, no perturbations"'),
        rows = 3, width = '100%')
    ),
    column(2,
      br(),
      actionButton('nl_submit', 'Ask', width = '100%', class = 'btn-primary'),
      br(), br(),
      uiOutput('llm_status_badge')
    )
  ),
  uiOutput('llm_api_warning'),
  hr(),
  uiOutput('nl_preview_box'),
  uiOutput('nl_description_box'),
  uiOutput('nl_sql_box'),
  br(),
  uiOutput('nl_blank_ctrl_panel'),
  tabsetPanel(id = 'nl_result_tabs',
    tabPanel('Results table',
      br(),
      uiOutput('nl_selection_hint'),
      DTOutput('nl_table')),
    tabPanel('Growth curves',
      br(),
      uiOutput('nl_curves_panel')),
    tabPanel('Tool benchmarking',
      br(),
      tags$div(
        tags$b('Baseline subtraction', style = 'font-size:13px;'),
        tags$small(style = 'color:dimgray; margin-left:6px;',
          '— Sprouffske & Wagner (2012), Hall et al. (2014), gcplyr / Mira et al. (2022)'),
        br(),
        radioButtons('nl_comp_baseline', NULL, inline = TRUE,
          choices  = c('None' = 'none', 'Min per well' = 'min_well',
                       'First 3 tp' = 'first_3', 'Blank wells' = 'blank_wells'),
          selected = 'none'),
        uiOutput('nl_comp_blank_note')
      ),
      hr(style = 'margin: 6px 0;'),
      fluidRow(
        column(4,
          selectInput('nl_metric_select', 'Metric',
            choices  = c('doubling_time_h', 'mumax_percapita', 'lag_time_h',
                         'auc_trapezoid', 'max_od', 'delta_od', 'max_death_rate'),
            selected = 'doubling_time_h')),
        column(8,
          checkboxGroupInput('nl_comp_tools', 'Tools',
            choices  = c('model_free', 'gcplyr', 'growthrates',
                         'DGrowthR' = 'dgrothr', 'KinBiont' = 'kinbiont'),
            selected = c('model_free', 'gcplyr', 'growthrates'),
            inline   = TRUE),
          tags$small(style = 'color: dimgray;',
            icon('hourglass-half'), ' DGrowthR (GP) and KinBiont (Julia) are slow — 30 s–3 min per dataset.'))
      ),
      withSpinner(plotOutput('nl_comp_plot', height = '420px'), type = 6, color = '#5b9bd5'),
      fluidRow(column(2, checkboxInput('nl_comp_log_y', 'Log y (plot)', FALSE))),
      br(),
      fluidRow(
        column(12,
          tags$b('Fitted parameters — all wells', style = 'font-size:13px;'),
          tags$span(style = 'margin-left:10px;',
            downloadButton('nl_comp_dl_csv', 'Download CSV', class = 'btn-sm btn-default'))
        )
      ),
      br(),
      withSpinner(DT::DTOutput('nl_comp_table'), type = 6, color = '#5b9bd5'))
  ),
  bottom_nav
)

# ── SUBMIT / NOTES ────────────────────────────────────────────────────────────
ui_submit <- fluidPage(
  theme = shinytheme('cerulean'),
  tags$head(tags$style(HTML('
    body { padding-bottom: 60px; }
    .access-key-box {
      background: #fff8e1; border: 2px solid #f9a825;
      border-radius: 6px; padding: 14px 18px; margin-top: 12px; font-family: monospace;
    }
    .access-key-box .key-value {
      font-size: 18px; font-weight: bold; letter-spacing: 2px;
      color: #b71c1c; word-break: break-all;
    }
    .embargo-panel { background: #f8f4ff; border: 1px solid #ce93d8;
                     border-radius: 6px; padding: 12px; margin-top: 8px; }
    .manage-panel  { background: #fff3e0; border: 1px solid #ffb74d;
                     border-radius: 6px; padding: 14px; }
    .step-label { font-weight: bold; color: #1565c0; margin-top: 10px; }
    .spinner-inline { display:inline-block; margin-left:8px; }
    .key-box { background:#f0f8ff; border:1px solid #b8d4e8;
               border-radius:4px; padding:10px; margin-bottom:8px; font-size:13px; }
    details.optional-section { margin: 8px 0; }
    details.optional-section > summary {
      cursor: pointer; font-weight: bold; color: #555;
      padding: 4px 0; list-style: none; user-select: none;
    }
    details.optional-section > summary::-webkit-details-marker { display: none; }
    details.optional-section > summary::before { content: "▶ "; font-size: 11px; }
    details.optional-section[open] > summary::before { content: "▼ "; }
    details.optional-section > summary span.opt-badge {
      background: #eee; border-radius: 3px; padding: 1px 5px;
      font-size: 11px; color: #777; margin-left: 6px; font-weight: normal;
    }
  '))),
  fluidRow(
    column(4, tags$span(
  img(src = 'PhaMiG_logo.png',  height = '52px', alt = 'PhaMiG',
      style = 'margin-right:8px;'),
  img(src = 'QresDB_logo.png',  height = '52px', alt = 'Qres DB')
)),
    column(8, h3('Submit Data & Manage Deposits'))
  ),
  hr(),
  tabsetPanel(
    # ── TAB 1: Upload ───────────────────────────────────────────────────────
    tabPanel('Upload raw data',
      br(),
      tags$div(class = 'key-box',
        tags$strong('Upload workflow: '),
        '① upload raw growth file  ② optionally upload plate template  ③ add dataset metadata  ④ choose release mode  ⑤ register.',
        tags$br(),
        tags$small(
          tags$span(class = 'required-star', '★'), ' required. ',
          'For multi-plate Excel files, raw data and plate template must have matching sheets.'
        )
      ),
      fluidRow(
        column(12,
          wellPanel(
            # STEP 1: Files
            tags$p(class = 'step-label', '① Files'),
            tags$div(style = 'position:relative;',
              fileInput('upload_raw_csv',
                req_label('Raw data file (.csv or .xlsx)',
                  'CSV or Excel file from your plate reader. First column must be named "Time" (HH:MM:SS or decimal hours). Remaining columns are OD values with well names A1, A2 … H12 as headers. At least 5 time points and 2 wells required. For Excel: each sheet = one plate measurement.'),
                accept = c('.csv', '.xlsx'), buttonLabel = 'Browse…', placeholder = 'No file selected'),
              tags$div(style = 'position:absolute; top:0; right:0;',
                actionButton('show_demo_raw', 'Demo file',
                             class = 'btn btn-xs btn-info', icon = icon('table')))
            ),
            tags$div(style = 'position:relative;',
              fileInput('upload_meta_csv',
                opt_label('Plate template (.csv or .xlsx)',
                  'Generated on the Metadata Preparation page. Describes per-well conditions (perturbation, concentration, blank/control flags). Without it, one summary row is created for the whole plate. For multi-sheet Excel uploads: provide a matching .xlsx with the same number of sheets in the same order.'),
                accept = c('.csv', '.xlsx'), buttonLabel = 'Browse…', placeholder = 'No file selected (optional)'),
              tags$div(style = 'position:absolute; top:0; right:0;',
                tags$a(href = route_link('metadata_prep'),
                  class = 'btn btn-xs btn-default',
                  icon('arrow-right'), ' Go to Metadata Prep'))
            ),

            # STEP 2: Dataset metadata
            tags$p(class = 'step-label', '② Dataset metadata'),
            fluidRow(
              column(6,
                textInput('upload_dataset_id',
                  req_label('Dataset ID',
                    'Unique identifier linking all tables. Allowed characters: letters, digits, underscore, hyphen, dot. Max 100 characters. Example: ecoli_MOPS_37C_2026_smith'),
                  placeholder = 'ecoli_MOPS_37C_2026_smith', value = '')
              ),
              column(6,
                textInput('upload_species',
                  req_label('Species',
                    'Scientific or common name. Max 120 characters. Examples: "Escherichia coli", "Saccharomyces cerevisiae", "Bacillus subtilis"'),
                  placeholder = 'Escherichia coli')
              )
            ),
            fluidRow(
              column(3,
                textInput('upload_temp_c',
                  req_label('Temperature (°C)',
                    'Incubation temperature in degrees Celsius. Valid range: -5 to 105. Example: 37'),
                  placeholder = '37')
              ),
              column(9,
                textInput('upload_medium',
                  req_label('Medium type',
                    'Growth medium full description. Max 200 characters. Examples: "LB", "MOPS + 3 g/L glucose", "YPD"'),
                  placeholder = 'MOPS + 3 g/L glucose')
              )
            ),

            # Publication (optional, collapsible)
            tags$details(class = 'optional-section',
              tags$summary('Publication info', tags$span(class = 'opt-badge', 'optional')),
              br(),
              textInput('upload_authors',
                opt_label('Authors', 'Semicolon-separated. Example: "Smith, J.; Doe, A."'),
                placeholder = 'Smith, J.; Doe, A.'),
              fluidRow(
                column(3, numericInput('upload_year',
                  opt_label('Year', 'Publication or collection year. Range 1950–2035.'),
                  value = as.integer(format(Sys.Date(), '%Y')), min = 1950, max = 2035)),
                column(9, textInput('upload_doi',
                  opt_label('DOI', 'Must start with "10." Example: 10.1038/nature12345.'),
                  placeholder = '10.1234/journal.xxx'))
              )
            ),

            # Notes (optional, collapsible)
            tags$details(class = 'optional-section',
              tags$summary('Notes', tags$span(class = 'opt-badge', 'optional')),
              br(),
              textAreaInput('upload_notes', NULL,
                rows = 2, placeholder = 'e.g. Outlier wells removed; temperature drifted ±0.5°C')
            ),

            # Paste plate-reader procedure output → auto-fill instrument fields
            tags$details(class = 'optional-section', id = 'procedure_paste_section',
              tags$summary(
                tagList(icon('magic'), ' Auto-fill from plate reader output'),
                tags$span(class = 'opt-badge', 'beta')),
              br(),
              tags$div(class = 'alert alert-info', style = 'font-size:12px; padding:6px 10px; margin-bottom:8px;',
                icon('circle-info'),
                ' Paste the "Procedure Summary" text from your plate reader software.',
                ' Fields are extracted automatically. You can still edit them below.'),
              fluidRow(
                column(9,
                  textAreaInput('upload_procedure_paste', NULL, rows = 3,
                    placeholder = 'Procedure Summary  Plate Type: ...  Temperature setpoint: 30 °C  Shaking frequency: 800 RPMs …')),
                column(3, br(),
                  actionButton('upload_procedure_extract', tagList(icon('wand-magic-sparkles'), ' Extract'),
                               class = 'btn-info btn-sm', style = 'margin-top:4px; width:100%;'),
                  uiOutput('upload_procedure_extract_status'))
              )
            ),

            # Instrument & plate details (optional, collapsible)
            tags$details(class = 'optional-section',
              tags$summary('Instrument & plate details', tags$span(class = 'opt-badge', 'optional')),
              br(),
              fluidRow(
                column(5,
                  textInput('upload_plate_reader',
                    opt_label('Plate reader', 'Brand and model. Example: Tecan Infinite 200 Pro.'),
                    placeholder = 'e.g. Tecan Infinite 200 Pro')),
                column(3,
                  numericInput('upload_wavelength',
                    opt_label('Wavelength (nm)', 'OD600 standard; OD750 for cyanobacteria.'),
                    value = 600, min = 200, max = 1000)),
                column(4,
                  textInput('upload_spectrophotometer',
                    opt_label('Spectrophotometer', 'If different from plate reader.'),
                    placeholder = 'optional'))
              ),
              fluidRow(
                column(6,
                  selectInput('upload_plate_colour',
                    opt_label('Plate colour', 'Bottom material. Transparent = standard absorbance.'),
                    choices = c('transparent' = 'transparent', 'white' = 'white',
                                'black' = 'black', 'other' = 'other'),
                    selected = 'transparent')),
                column(6,
                  radioButtons('upload_lid', opt_label('Lid', 'Lid used during measurement?'),
                    choices = c('Yes' = 'TRUE', 'No' = 'FALSE'), selected = 'TRUE', inline = TRUE))
              ),
              fluidRow(
                column(3,
                  numericInput('upload_shake_rpm',
                    opt_label('Shaking (rpm)', '0 = no shaking.'),
                    value = NA, min = 0, max = 2000)),
                column(3,
                  selectInput('upload_time_unit',
                    opt_label('Time unit',
                      'Only for plain-number Time columns. Colon-separated formats (HH:MM:SS, MM:SS) are auto-detected.'),
                    choices = c('hours' = 'h', 'minutes' = 'min', 'seconds' = 's'),
                    selected = 'h')),
                column(3,
                  textInput('upload_strain',
                    opt_label('Strain', 'Dataset-wide default; override per well in plate template.'),
                    placeholder = 'e.g. K12 NCM3722')),
                column(3,
                  textInput('upload_taxon_id',
                    opt_label('NCBI Taxon ID', 'Example: 562 (E. coli), 4932 (S. cerevisiae).'),
                    placeholder = '562'))
              )
            ),

            # STEP 3: Embargo / access control
            tags$p(class = 'step-label',
              '③ Data access & release',
              tags$small(
                actionLink('show_release_conditions',
                           tagList(icon('info-circle'), ' Conditions'),
                           style = 'margin-left:8px; font-size:12px; font-weight:normal; vertical-align:middle;')
              )
            ),
            tags$div(class = 'embargo-panel',
              helpText(icon('lock'), ' Unpublished data can be stored privately until you are ready to release it. You always need your email + access key + passphrase to delete your dataset.'),
              radioButtons('upload_release_mode', NULL,
                choices = c(
                  'Immediate (already published)'  = 'immediate',
                  'Set a release date'             = 'date',
                  'bioRxiv preprint — release on journal publication' = 'biorxiv'
                ),
                selected = 'immediate'),
              conditionalPanel("input.upload_release_mode == 'date'",
                dateInput('upload_release_date',
                  req_label('Release date',
                    'Your dataset will become publicly visible in PhaMiG searches on this date.'),
                  value = Sys.Date() + 365, min = Sys.Date() + 1)
              ),
              conditionalPanel("input.upload_release_mode == 'biorxiv'",
                textInput('upload_biorxiv_url',
                  req_label('bioRxiv URL or DOI',
                    'Full URL or DOI (starting with 10.1101/). PhaMiG checks the bioRxiv API monthly. Your data is released automatically when the paper is published in a peer-reviewed journal.'),
                  placeholder = 'https://www.biorxiv.org/content/10.1101/2024.01.01.000001')
              ),
              hr(style = 'margin: 8px 0;'),
              fluidRow(
                column(6,
                  textInput('upload_email',
                    req_label('Your email', 'Required for deletion. Not stored in plain text.'),
                    placeholder = 'yourname@institution.edu')
                ),
                column(6,
                  passwordInput('upload_passphrase',
                    req_label('Passphrase (≥8 chars)', 'Hashed before storage. Needed to delete your dataset.'),
                    placeholder = '≥ 8 characters'),
                  passwordInput('upload_passphrase2',
                    req_label('Confirm passphrase', 'Must match exactly.'),
                    placeholder = 'repeat your passphrase')
                )
              ),
              tags$details(class = 'optional-section', style = 'margin-top:6px;',
                tags$summary('ORCID', tags$span(class = 'opt-badge', 'optional')),
                br(),
                textInput('upload_orcid',
                  opt_label('ORCID', 'Format: 0000-0000-0000-0000. Links dataset to your researcher profile.'),
                  placeholder = '0000-0000-0000-0000')
              )
            ),

            tags$details(class = 'optional-section',
              tags$summary('Quick analysis', tags$span(class = 'opt-badge', 'optional')),
              br(),
              checkboxInput('upload_run_analysis',
                tagList('Run model_free analysis after upload',
                  tip('Estimates doubling time, mumax, and AUC for all wells. Results appear in the Analyze tab. Takes 5–30 s.')),
                value = TRUE)
            ),
            uiOutput('upload_progress_ui'),
            uiOutput('upload_key_display'),
            verbatimTextOutput('upload_status'),
            actionButton('run_upload', tags$span(icon('upload'), ' Register in database'),
                         class = 'btn-primary btn-lg', width = '100%')
          )
        )
      )
    ),

    # ── TAB 2: Manage my data ───────────────────────────────────────────────
    tabPanel(tags$span(icon('key'), ' Manage my data'),
      br(),
      fluidRow(
        column(6,
          tags$div(class = 'manage-panel',
            h4(icon('search'), ' Check release status'),
            helpText('Enter your email to see all your datasets and their current release status.'),
            textInput('status_email', 'Your email', placeholder = 'yourname@institution.edu'),
            actionButton('run_status_check', 'Show my datasets', class = 'btn-info', width = '100%'),
            br(), br(),
            DTOutput('my_datasets_table')
          ),
          br(),
          tags$div(class = 'manage-panel',
            h4(icon('trash'), ' Delete a dataset'),
            helpText('All four factors must match the values you provided at upload time. Deletion is permanent and removes raw data, metadata, and all analysis results.'),
            br(),
            textInput('del_dataset_id',
              req_label('Dataset ID to delete',
                'Exact dataset_id as used at upload. Case-sensitive.'),
              placeholder = 'ecoli_MOPS_37C_2026_smith'),
            textInput('del_email',
              req_label('Your email',
                'Same email address you provided at upload. Case-insensitive.'),
              placeholder = 'yourname@institution.edu'),
            textInput('del_access_key',
              req_label('32-character access key',
                'The access key shown once after upload. All 32 characters, exact match.'),
              placeholder = 'ABCDEFGHIJKLMNOPabcdefghijklmnop'),
            passwordInput('del_passphrase',
              req_label('Passphrase',
                'The passphrase you chose at upload. Hashed and compared — we do not store the plain text.'),
              placeholder = 'your passphrase'),
            br(),
            actionButton('run_delete', tags$span(icon('trash'), ' Delete permanently'),
                         class = 'btn-danger', width = '100%'),
            br(), br(),
            verbatimTextOutput('delete_status')
          ),
          br(),
          tags$div(class = 'manage-panel',
            h4(icon('wrench'), ' Request a correction'),
            helpText('Use this if you uploaded a mismatched or incorrect metadata–raw data pair.',
                     'Your embargo record and access credentials are preserved;',
                     'only the uploaded files are cleared so you can re-submit corrected data',
                     'under the same dataset ID in the "Upload raw data" tab.'),
            br(),
            textInput('reopen_dataset_id',
              req_label('Dataset ID',
                'Exact dataset_id as used at upload. Case-sensitive.'),
              placeholder = 'ecoli_MOPS_37C_2026_smith'),
            textInput('reopen_email',
              req_label('Your email',
                'Same email address provided at upload.'),
              placeholder = 'yourname@institution.edu'),
            textInput('reopen_access_key',
              req_label('32-character access key',
                'The access key shown once after upload.'),
              placeholder = 'ABCDEFGHIJKLMNOPabcdefghijklmnop'),
            passwordInput('reopen_passphrase',
              req_label('Passphrase',
                'The passphrase you chose at upload.'),
              placeholder = 'your passphrase'),
            br(),
            actionButton('run_reopen', tags$span(icon('wrench'), ' Clear upload & reopen'),
                         class = 'btn-warning', width = '100%'),
            br(), br(),
            verbatimTextOutput('reopen_status')
          )
        ),
        column(6,
          wellPanel(
            h5(icon('info-circle'), ' About data release modes'),
            tags$dl(
              tags$dt(icon('globe'), ' Immediate release'),
              tags$dd('Data is publicly searchable in PhaMiG as soon as it is registered. Use this for already-published datasets.'),
              br(),
              tags$dt(icon('calendar'), ' Set a release date'),
              tags$dd('Data remains private until the chosen date. PhaMiG automatically releases it at midnight on that date. Useful when you know your submission or conference date.'),
              br(),
              tags$dt(icon('file-alt'), ' bioRxiv preprint'),
              tags$dd('Data remains private while your work is a preprint. PhaMiG polls the bioRxiv API every month. When your preprint receives a journal DOI, the data is released automatically. You will need to provide the bioRxiv URL or DOI.'),
              br(),
              tags$dt(icon('trash'), ' Deleting your data'),
              tags$dd('You can delete your dataset at any time — before or after release — using your email + access key + passphrase. Save your access key immediately after upload: it is shown only once and is not recoverable.')
            ),
            hr(),
            h5(icon('shield-alt'), ' Privacy notes'),
            tags$ul(
              tags$li('Your passphrase is stored as a SHA-256 hash only — we never see the plain text.'),
              tags$li('Your email is stored in lowercase and is only visible to the database administrator.'),
              tags$li('The access key is a 32-character random token. Write it down or copy it immediately.'),
              tags$li('Embargoed datasets are completely hidden from all search results, analysis tabs, and downloads.')
            )
          )
        )
      )
    ),

    # ── TAB 3: Notes & observations ─────────────────────────────────────────
    tabPanel('Notes & observations',
      tags$head(tags$style(HTML('
        .admin-corner { position:absolute; top:8px; right:14px; z-index:20; }
        .admin-corner a { color:#ccc; font-size:11px; text-decoration:none; }
        .admin-corner a:hover { color:#888; }
        .admin-badge { background:#e74c3c; color:#fff; border-radius:3px;
                       padding:1px 5px; font-size:10px; margin-left:4px; }
      '))),
      br(),
      tags$div(style = 'position:relative;',
        # Admin icon — top-right corner
        tags$div(class = 'admin-corner',
          actionLink('admin_notes_btn',
            tagList(icon('lock', style='font-size:11px;'),
                    tags$span(' admin')),
            title = 'Admin login')
        ),
        fluidRow(
          column(12,
            wellPanel(
              h5('Submit a note, limitation, or bug observation'),
              helpText('Stored locally and logged to file. The maintainer is notified by email.'),
              fluidRow(
                column(4,
                  selectInput('note_type', 'Type',
                              choices = c('bug', 'limitation', 'observation',
                                          'data_quality', 'feature_request'),
                              width = '100%')
                ),
                column(8,
                  textInput('note_author',
                    opt_label('Your name or email', 'Optional. Used only to follow up if needed.'),
                    width = '100%')
                )
              ),
              textInput('note_dataset_id',
                opt_label('Dataset ID', 'If the note relates to a specific dataset, enter its ID here.'),
                width = '100%', placeholder = 'ecoli_MOPS_37C_2026_smith  (optional)'),
              textAreaInput('note_text', 'Description', rows = 5, width = '100%',
                            placeholder = 'Describe the issue, limitation, or observation…'),
              actionButton('submit_note', 'Submit note', class = 'btn-warning', width = '100%')
            )
          )
        ),
        br(),
        uiOutput('notes_section_ui')
      )
    )
  ),
  bottom_nav
)

# ── PLUGINS & REFERENCES ──────────────────────────────────────────────────────
ui_plugins <- fluidPage(
  theme = shinytheme('cerulean'),
  tags$head(tags$style(HTML('body { padding-bottom: 60px; }'))),
  fluidRow(
    column(4, tags$span(
  img(src = 'PhaMiG_logo.png',  height = '52px', alt = 'PhaMiG',
      style = 'margin-right:8px;'),
  img(src = 'QresDB_logo.png',  height = '52px', alt = 'Qres DB')
)),
    column(8, h3('Plugins & References'))
  ),
  hr(),
  h5('Available analysis plugins'),
  DTOutput('plugin_table'),
  br(),
  h5('References & source code'),
  tags$div(class = 'list-group',
    tags$a(
      href = 'https://github.com/tpetzoldt/growthrates', target = '_blank',
      class = 'list-group-item',
      tags$strong('growthrates (R)'), tags$br(),
      'Petzoldt T (2022). Parametric and spline-based growth model fitting.',
      tags$span(class = 'pull-right label label-default', 'github.com/tpetzoldt/growthrates')
    ),
    tags$a(
      href = 'https://bio-datascience.github.io/DGrowthR/', target = '_blank',
      class = 'list-group-item',
      tags$strong('DGrowthR (R)'), tags$br(),
      'Feldl M & Olayo-Alarcon R. Gaussian Process Regression growth analysis & differential testing.',
      tags$span(class = 'pull-right label label-default', 'bio-datascience.github.io/DGrowthR')
    ),
    tags$a(
      href = 'https://medinabajramovic.de/differential-growth-curve-analysis/',
      target = '_blank', class = 'list-group-item',
      tags$strong('DGrowthR — methods blog post'), tags$br(),
      "Medina Feldl's explanation of differential growth curve analysis with GPR.",
      tags$span(class = 'pull-right label label-default', 'medinabajramovic.de')
    ),
    tags$a(
      href = 'https://github.com/pinheiroGroup/Kinbiont.jl', target = '_blank',
      class = 'list-group-item',
      tags$strong('KinBiont.jl (Julia)'), tags$br(),
      'Pinheiro group. Log-linear and ODE-based growth modelling in Julia. Authors in contact.',
      tags$span(class = 'pull-right label label-default', 'github.com/pinheiroGroup/Kinbiont.jl')
    ),
    tags$a(
      href = 'https://github.com/melinabajramovic/gcplyr', target = '_blank',
      class = 'list-group-item',
      tags$strong('gcplyr (R)'), tags$br(),
      'Blazanin M (2023). Per-capita derivative-based growth rate estimation. doi:10.1093/g3journal/jkad261',
      tags$span(class = 'pull-right label label-default', 'github.com')
    )
  ),
  br(),
  h5('Processing runs'),
  DTOutput('processing_table'),
  br(), br(),
  bottom_nav
)

# ── ACKNOWLEDGEMENTS ──────────────────────────────────────────────────────────
ui_acknowledgement <- fluidPage(
  theme = shinytheme('cerulean'),
  tags$head(tags$style(HTML('body { padding-bottom: 60px; }'))),
  fluidRow(
    column(4, tags$span(
  img(src = 'PhaMiG_logo.png',  height = '52px', alt = 'PhaMiG',
      style = 'margin-right:8px;'),
  img(src = 'QresDB_logo.png',  height = '52px', alt = 'Qres DB')
)),
    column(8, h3('Acknowledgements'))
  ),
  hr(),
  fluidRow(
    column(8, offset = 2,
      wellPanel(
        h4('Funding'),
        p('This work is supported by the ', tags$strong('German Research Foundation (DFG).'),
          ' We are grateful for the funding that makes PhaMiG possible.'),
        hr(),
        h4('Tool authors'),
        tags$ul(
          tags$li(tags$strong('gcplyr:'), ' Michael Blazanin — per-capita derivative-based growth rate estimation'),
          tags$li(tags$strong('growthrates:'), ' Thomas Petzoldt — parametric and spline-based growth modelling in R'),
          tags$li(tags$strong('DGrowthR:'), ' Medina Feldl & Roberto Olayo-Alarcon — Gaussian Process Regression for growth analysis'),
          tags$li(tags$strong('KinBiont.jl:'), ' Pinheiro group — Julia-based ODE and log-linear growth modelling')
        ),
        p('We are in contact with the authors of all tools listed above and are grateful for their foundational work.'),
        hr(),
        h4('Data'),
        p('All ', tags$em('E. coli'), ' growth curve measurements across 27–45°C were contributed to PhaMiG for the purpose of cross-tool benchmarking. Yeast and other microbial datasets are being integrated.'),
        hr(),
        h4('Similar resources'),
        tags$ul(
          tags$li(tags$a(href = 'https://mgrowthdb.gbiomed.kuleuven.be/', target = '_blank',
                         'mGrowthDB (KU Leuven)'), ' — microbial growth curve database'),
          tags$li(tags$a(href = 'https://github.com/bio-datascience/DGrowthR/', target = '_blank',
                         'DGrowthR GitHub'))
        ),
        hr(),
        h4(tags$img(src = 'acknowledgement.png', height = '80px'), ' Thank you!'),
        p('To all dataset contributors, tool developers, and the scientific community.')
      )
    )
  ),
  bottom_nav
)

# ── Router ─────────────────────────────────────────────────────────────────────
router <- router_ui(
  route('/',               ui_home),
  route('metadata_prep',   ui_metadata),
  route('search',          ui_search),
  route('analyse',         ui_analyse),
  route('ask',             ui_ask),
  route('submit',          ui_submit),
  route('plugins',         ui_plugins),
  route('acknowledgement', ui_acknowledgement)
)

ui <- fluidPage(router)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  router_server('/')

  con <- qres_connect('data/qres.duckdb')
  onStop(function() qres_disconnect(con))

  # ── Home page live stats ────────────────────────────────────────────────────
  output$home_stats_bar <- renderUI({
    fmt <- function(n) formatC(as.integer(n), format = 'd', big.mark = ',')
    stats <- tryCatch(list(
      datasets = DBI::dbGetQuery(con,
        "SELECT COUNT(DISTINCT d.dataset_id) AS n
         FROM dataset d
         JOIN raw_measurement_registry rmr USING(dataset_id)
         LEFT JOIN upload_embargo ue USING(dataset_id)
         WHERE (ue.released = TRUE OR ue.dataset_id IS NULL)")$n,
      species  = DBI::dbGetQuery(con,
        "SELECT COUNT(DISTINCT w.species) AS n
         FROM well_metadata w
         JOIN plate p ON p.plate_id = w.plate_id
         JOIN raw_measurement_registry rmr ON rmr.dataset_id = p.dataset_id
         LEFT JOIN upload_embargo ue ON ue.dataset_id = p.dataset_id
         WHERE w.species IS NOT NULL AND trim(w.species) != ''
           AND (ue.released = TRUE OR ue.dataset_id IS NULL)")$n,
      curves   = DBI::dbGetQuery(con,
        "SELECT COALESCE(SUM(n_wells), 0) AS n
         FROM raw_measurement_registry rmr
         LEFT JOIN upload_embargo ue USING(dataset_id)
         WHERE (ue.released = TRUE OR ue.dataset_id IS NULL)")$n,
      tools    = 5L   # supported analysis plugins: model_free, gcplyr, growthrates, DGrowthR, KinBiont
    ), error = function(e) NULL)
    if (is.null(stats)) return(NULL)
    .stat <- function(n, lbl)
      tags$div(class = 'home-stat',
        tags$span(class = 'home-stat-n',     fmt(n)),
        tags$span(class = 'home-stat-label', lbl))
    tags$div(class = 'home-stats-bar',
      .stat(stats$datasets, 'Datasets'),
      .stat(stats$species,  'Species'),
      .stat(stats$curves,   'Growth curves'),
      .stat(stats$tools,    'Tools benchmarked')
    )
  })

  selected_dataset     <- reactiveVal(NULL)
  detail_dataset_id    <- reactiveVal(NULL)   # which of the selected datasets to show in Links / QC
  dataset_results      <- reactiveVal(data.frame())
  search_performed     <- reactiveVal(FALSE)  # TRUE once user hits Search; drives hero ↔ results toggle
  comp_filtered_rv     <- reactiveVal(data.frame())
  comp_upload_rv         <- reactiveVal(NULL)
  comp_upload_valid      <- reactiveVal(FALSE)
  comp_upload_tried      <- reactiveVal(FALSE)  # TRUE once user picks a file; FALSE after Clear
  comp_upload_n_plates   <- reactiveVal(1L)      # >1 for multi-sheet Excel
  comp_plate_labels_rv   <- reactiveVal(NULL)    # named char: plate_id → sheet name
  comp_well_meta_rv      <- reactiveVal(NULL)   # parsed metadata CSV; NULL = manual mode
  comp_replicates_rv     <- reactiveVal(list()) # named list: well → integer group ID
  comp_email_status    <- reactiveVal('')
  analyze_selected_ids <- reactiveVal(character(0))  # passed from Search Data
  analyse_mode_rv      <- reactiveVal(NULL)           # 'A', 'B', or 'C'
  mode_a_reset         <- reactiveVal(0L)             # increment forces Mode A re-render

  # ── Advanced-filter reactive: narrows dataset_results() without a DB round-trip ──
  display_results <- reactive({
    df <- dataset_results()
    if (nrow(df) == 0) return(df)

    .txt <- function(df, col, val) {
      v <- trimws(val %||% '')
      if (!nzchar(v) || !col %in% names(df)) return(df)
      df[grepl(v, as.character(df[[col]]), ignore.case = TRUE, fixed = FALSE), ]
    }

    df <- .txt(df, 'species',           input$adv_species)
    df <- .txt(df, 'strain',            input$adv_strain)
    df <- .txt(df, 'medium_type',       input$adv_medium)
    df <- .txt(df, 'perturbation_type', input$adv_perturb_type)
    df <- .txt(df, 'perturbation',      input$adv_perturb)
    df <- .txt(df, 'plate_reader',      input$adv_plate_reader)
    st_sel <- input$adv_signal_type %||% ''
    if (nzchar(st_sel) && 'signal_type' %in% names(df))
      df <- df[!is.na(df$signal_type) & df$signal_type == st_sel, ]
    mu_sel <- input$adv_measurement_unit %||% ''
    if (nzchar(mu_sel) && 'measurement_unit' %in% names(df))
      df <- df[!is.na(df$measurement_unit) & df$measurement_unit == mu_sel, ]
    df <- .txt(df, 'authors',           input$adv_author)
    df <- .txt(df, 'doi',               input$adv_doi)

    t_lo <- suppressWarnings(as.numeric(input$adv_temp_min))
    t_hi <- suppressWarnings(as.numeric(input$adv_temp_max))
    if (!is.na(t_lo) && 'temperature_c' %in% names(df))
      df <- df[!is.na(df$temperature_c) & df$temperature_c >= t_lo, ]
    if (!is.na(t_hi) && 'temperature_c' %in% names(df))
      df <- df[!is.na(df$temperature_c) & df$temperature_c <= t_hi, ]

    y_lo <- suppressWarnings(as.integer(input$adv_year_min))
    y_hi <- suppressWarnings(as.integer(input$adv_year_max))
    if (!is.na(y_lo) && 'year' %in% names(df))
      df <- df[!is.na(df$year) & df$year >= y_lo, ]
    if (!is.na(y_hi) && 'year' %in% names(df))
      df <- df[!is.na(df$year) & df$year <= y_hi, ]

    # Capitalize first letter of each word without lowercasing first — preserves
    # multi-char initials like "BW" that tolower()+toTitleCase() would corrupt to "Bw"
    # Guard nrow: %||% treats character(0) as NULL → returns '' (length 1) → crash on 0-row df
    if (nrow(df) > 0 && 'authors' %in% names(df))
      df$authors <- gsub("\\b([a-z])", "\\U\\1", df$authors, perl = TRUE)

    df
  })

  # ── Auto-load on startup — populate filter dropdowns and search results ──────
  # Deferred via later::later(0): lets Shiny flush the initial UI to the browser
  # before running the blocking DB queries, so the home page appears immediately.
  session$onFlushed(function() {
    later::later(function() {
      rows <- tryCatch({
        qres_migrate_schema(con)
        qres_search_datasets(con)
      }, error = function(e) data.frame())
      if (nrow(rows) > 0) dataset_results(rows)
      # Populate Analyse page filter dropdowns
      tryCatch({
        updateSelectizeInput(session, 'comp_author',
                             choices = c('(none)' = '', qres_get_distinct_authors(con)),
                             server  = TRUE)
        updateSelectizeInput(session, 'comp_species',
                             choices = c('' , qres_get_species_with_counts(con)), server = TRUE)
        updateSelectizeInput(session, 'comp_perturb_type',
                             choices = c('', qres_get_distinct_perturbation_types(con)),
                             server = TRUE)
      }, error = function(e) NULL)
    }, delay = 0)
  }, once = TRUE)

  # ── Initialize DB ──────────────────────────────────────────────────────────
  observeEvent(input$init_db, {
    withProgress(message = 'Initializing database…', {
      # Drop all tables and recreate with current schema (handles old DBs)
      qres_reset_tables(con)
      # Safety net: add any columns that couldn't be dropped/recreated
      qres_migrate_schema(con)

      setProgress(0.15, detail = 'Importing metadata…')
      qres_import_metadata(con, 'data/metaData.csv')

      md <- data.table::fread('data/metaData.csv')
      names(md) <- trimws(tolower(names(md)))   # normalise: dataset_id, raw_file
      n  <- nrow(md)
      for (i in seq_len(n)) {
        raw_file   <- md$raw_file[i]
        dataset_id <- md$dataset_id[i]
        setProgress(0.15 + 0.75 * i / n,
                    detail = paste('Registering', raw_file, '…'))
        csv_path <- file.path('data/raw_csv', raw_file)
        if (file.exists(csv_path)) {
          tryCatch(
            qres_register_raw_measurements(con,
              dataset_id = dataset_id,
              raw_csv    = csv_path),
            error = function(e) message('Skip ', dataset_id, ': ', conditionMessage(e))
          )
        }
      }

      updateSelectizeInput(session, 'comp_species',
                           choices = c('', qres_get_species_with_counts(con)), server = TRUE)
      updateSelectizeInput(session, 'comp_author',
                           choices = c('(none)' = '', qres_get_distinct_authors(con)),
                           server  = TRUE)
      updateSelectizeInput(session, 'comp_perturb_type',
                           choices = c('', qres_get_distinct_perturbation_types(con)),
                           server = TRUE)

      temp_range <- qres_get_temp_range(con)
      if (all(is.finite(temp_range))) {
        updateSliderInput(session, 'comp_temp',
                          min   = floor(temp_range[1]),
                          max   = ceiling(temp_range[2]),
                          value = c(floor(temp_range[1]), ceiling(temp_range[2])))
      }

      dataset_results(qres_search_datasets(con))
    })
  })

  # ── Search ─────────────────────────────────────────────────────────────────
  # Show which LLM backend is active (none = rule-based only, no key needed)
  # Result count bar
  output$search_result_bar <- renderUI({
    if (!search_performed()) return(NULL)
    n_all <- nrow(dataset_results())
    n_dis <- nrow(display_results())
    if (n_dis == 0) return(NULL)  # no-results message shown in the hero section

    # "Repository overview" when no active text query and all datasets are shown.
    # "X datasets found" / "X of Y" when a filter is in effect.
    q_active <- nzchar(trimws(input$search %||% ''))
    adv_active <- any(nzchar(c(
      input$adv_species %||% '', input$adv_strain %||% '', input$adv_medium %||% '',
      input$adv_perturb %||% '', input$adv_author %||% '', input$adv_doi %||% '',
      input$adv_signal_type %||% '', input$adv_measurement_unit %||% ''
    ))) || !is.na(input$adv_temp_min) || !is.na(input$adv_temp_max) ||
           nzchar(input$adv_perturb_type %||% '')

    lbl <- if (!q_active && !adv_active && n_dis == n_all)
      sprintf('Repository overview — %d dataset%s', n_all, if (n_all == 1) '' else 's')
    else if (n_dis == n_all)
      sprintf('%d dataset%s found', n_dis, if (n_dis == 1) '' else 's')
    else
      sprintf('%d of %d dataset%s match', n_dis, n_all, if (n_dis == 1) '' else 's')

    tags$p(class = 'result-count', lbl)
  })

  # Legend strip shown above the results table when multiple signal types are present
  output$signal_type_legend <- renderUI({
    if (!search_performed()) return(NULL)
    df <- display_results()
    if (nrow(df) == 0 || !'signal_type' %in% names(df)) return(NULL)
    types_present <- sort(unique(na.omit(df$signal_type)))
    if (length(types_present) == 0) return(NULL)

    clr <- c(OD600        = 'rgba(91,155,213,0.22)',
             OD750        = 'rgba(51,120,190,0.22)',
             fluorescence = 'rgba(180,100,200,0.22)',
             luminescence = 'rgba(240,160,30,0.22)',
             CFU          = 'rgba(112,173,71,0.22)',
             cell_count   = 'rgba(60,140,50,0.22)')
    lbl <- c(OD600        = 'OD600 (absorbance)',
             OD750        = 'OD750 (absorbance)',
             fluorescence = 'Fluorescence (RFU)',
             luminescence = 'Luminescence (RLU)',
             CFU          = 'Viable cells (CFU/mL)',
             cell_count   = 'Cell count (cells/mL)')

    chips <- lapply(types_present, function(st) {
      bg  <- clr[[st]] %||% 'rgba(180,180,180,0.22)'
      txt <- lbl[[st]] %||% st
      tags$span(txt, style = paste0(
        'display:inline-block; padding:1px 8px; border-radius:3px; margin-left:8px;',
        'font-size:11px; color:#444; background:', bg, ';'))
    })

    tags$div(style = 'text-align:right; margin-bottom:3px; line-height:2;',
      tags$span('Row colour:', style = 'font-size:11px; color:#999;'),
      chips)
  })

  # Facets panel — counts from the currently displayed (filtered) results
  output$search_facets <- renderUI({
    df <- display_results()
    if (nrow(df) == 0) return(NULL)

    # Generic block: title (total), top-2 visible, rest behind toggle.
    # Counts unique dataset_ids per term so JOIN-multiplied rows don't inflate counts.
    .block <- function(title, vals, ids = df$dataset_id) {
      vals <- as.character(vals)
      ok   <- !is.na(vals) & nzchar(vals) & vals != 'NA'
      vals <- vals[ok]
      ids  <- ids[ok]
      if (length(vals) == 0) return(NULL)
      counts <- sort(
        sapply(split(ids, vals), function(x) length(unique(x))),
        decreasing = TRUE)
      n     <- length(counts)   # unique terms in this facet category
      total <- n                # header shows: how many distinct values exist
      uid     <- paste0('facet_more_', tolower(gsub('[^a-z0-9]', '', title)))

      make_row <- function(i)
        tags$div(class = 'facet-row',
          tags$span(names(counts)[i]),
          tags$span(class = 'facet-count', as.integer(counts[i])))

      visible <- lapply(seq_len(min(2L, n)), make_row)

      more <- if (n > 2) {
        n_more <- n - 2L
        list(
          tags$div(id = uid, style = 'display:none;',
                   lapply(seq(3L, n), make_row)),
          tags$a(href = '#',
            style   = 'font-size:11px; color:#999; cursor:pointer; display:block; margin-top:2px;',
            onclick = sprintf(
              paste0("var e=document.getElementById('%s');",
                     "var o=e.style.display!=='none';",
                     "e.style.display=o?'none':'';",
                     "this.textContent=o?'Show %d more…':'Show less';",
                     "return false;"),
              uid, n_more),
            sprintf('Show %d more…', n_more))
        )
      } else list()

      tagList(
        tags$h6(title,
          tags$span(style = 'float:right; font-weight:normal; color:#bbb;',
                    paste0('(', total, ')'))),
        visible, more,
        tags$hr(style = 'margin:5px 0;'))
    }

    # Perturbation: 4 fixed subgroups (chemical / genetic / temperature / none)
    .block_perturbation <- function(df) {
      if (!'perturbation_type' %in% names(df)) return(NULL)
      ok_pt  <- !is.na(df$perturbation_type) & nzchar(df$perturbation_type)
      # Header shows number of distinct perturbation types present (e.g. 2 for Chemical + Temperature)
      total  <- length(unique(df$perturbation_type[ok_pt]))
      if (total == 0) return(NULL)

      subgroup_labels <- c(chemical = 'Chemical', genetic = 'Genetic',
                           temperature = 'Temperature', none = 'No perturbation')
      sub_blocks <- lapply(names(subgroup_labels), function(ptype) {
        sub_df <- df[!is.na(df$perturbation_type) & df$perturbation_type == ptype, ]
        if (nrow(sub_df) == 0) return(NULL)
        n_sub <- length(unique(sub_df$dataset_id))
        uid   <- paste0('facet_perturb_', ptype)

        pvals  <- as.character(sub_df$perturbation %||% '')
        pids   <- sub_df$dataset_id
        ok_p   <- !is.na(pvals) & nzchar(pvals) & !pvals %in% c('NA','none','None')
        pvals  <- pvals[ok_p]; pids <- pids[ok_p]
        ex     <- if (length(pvals) > 0)
          names(sort(sapply(split(pids, pvals), function(x) length(unique(x))), decreasing = TRUE))[1] else NULL
        all_rows <- if (length(pvals) > 0) {
          ct <- sort(sapply(split(pids, pvals), function(x) length(unique(x))), decreasing = TRUE)
          lapply(seq_along(ct), function(i)
            tags$div(class = 'facet-row', style = 'padding-left:14px; font-size:12px;',
              tags$span(names(ct)[i]),
              tags$span(class = 'facet-count', as.integer(ct[i]))))
        } else list()

        tagList(
          tags$div(class = 'facet-row',
            style = 'cursor:pointer; font-weight:600; font-size:12px; margin-top:3px;',
            onclick = sprintf(
              paste0("var e=document.getElementById('%s');",
                     "var o=e.style.display!=='none';",
                     "e.style.display=o?'none':'';",
                     "return false;"),
              uid),
            tags$span(subgroup_labels[ptype]),
            tags$span(class = 'facet-count', n_sub)
          ),
          if (!is.null(ex))
            tags$div(style = 'font-size:11px; color:#aaa; padding-left:10px; margin-bottom:2px;',
                     'e.g. ', tags$em(ex)),
          tags$div(id = uid, style = 'display:none;', all_rows)
        )
      })
      sub_blocks <- Filter(Negate(is.null), sub_blocks)
      if (length(sub_blocks) == 0) return(NULL)
      tagList(
        tags$h6('Perturbation',
          tags$span(style = 'float:right; font-weight:normal; color:#bbb;',
                    paste0('(', total, ')'))),
        sub_blocks,
        tags$hr(style = 'margin:5px 0;'))
    }

    .signal_label <- function(st) {
      vapply(as.character(st), function(x) switch(x,
        'OD600'        = 'OD600 (absorbance)',
        'OD750'        = 'OD750 (absorbance)',
        'fluorescence' = 'Fluorescence (RFU)',
        'luminescence' = 'Luminescence (RLU)',
        'CFU'          = 'Viable cells (CFU/mL)',
        'cell_count'   = 'Cell count (cells/mL)',
        x), character(1L))
    }
    .unit_label <- function(u) {
      vapply(as.character(u), function(x) switch(x,
        'OD'       = 'OD (absorbance)',
        'RFU'      = 'RFU — fluorescence',
        'RLU'      = 'RLU — luminescence',
        'CFU/ml'   = 'CFU/mL — viable cells',
        'cells/ml' = 'cells/mL — cell count',
        x), character(1L))
    }

    tags$div(class = 'facet-panel',
      .block('Species',          df$species),
      .block_perturbation(df),
      if ('signal_type' %in% names(df) && any(!is.na(df$signal_type)))
        .block('Measurement type', .signal_label(df$signal_type)),
      if ('measurement_unit' %in% names(df) && any(!is.na(df$measurement_unit)))
        .block('Unit',             .unit_label(df$measurement_unit)),
      .block('Temperature',      paste0(df$temperature_c, '°C')),
      .block('Medium',           df$medium_type)
    )
  })

  # Controls conditional detail panel visibility
  output$has_selected_dataset <- reactive({ !is.null(selected_dataset()) })
  outputOptions(output, 'has_selected_dataset', suspendWhenHidden = FALSE)

  # Drive search hero ↔ results layout toggle
  output$search_performed_flag <- reactive({ isTRUE(search_performed()) })
  outputOptions(output, 'search_performed_flag', suspendWhenHidden = FALSE)

  output$search_has_results <- reactive({
    isTRUE(search_performed()) && nrow(display_results()) > 0
  })
  outputOptions(output, 'search_has_results', suspendWhenHidden = FALSE)

  # Send hero compact/expand state to JS whenever results state changes
  observe({
    session$sendCustomMessage('searchStateChange',
      list(hasResults = isTRUE(search_performed()) && nrow(display_results()) > 0))
  })

  # Clear all advanced filter fields
  observeEvent(input$adv_clear, {
    updateTextInput(session,    'adv_species',      value = '')
    updateTextInput(session,    'adv_strain',       value = '')
    updateTextInput(session,    'adv_medium',       value = '')
    updateNumericInput(session, 'adv_temp_min',     value = NA)
    updateNumericInput(session, 'adv_temp_max',     value = NA)
    updateSelectInput(session,  'adv_perturb_type',  selected = '')
    updateTextInput(session,    'adv_perturb',       value = '')
    updateTextInput(session,    'adv_plate_reader',  value = '')
    updateSelectInput(session,  'adv_signal_type',      selected = '')
    updateSelectInput(session,  'adv_measurement_unit', selected = '')
    updateTextInput(session,    'adv_author',       value = '')
    updateNumericInput(session, 'adv_year_min',     value = NA)
    updateNumericInput(session, 'adv_year_max',     value = NA)
    updateTextInput(session,    'adv_doi',          value = '')
  })

  observeEvent(input$clear_search, {
    updateTextInput(session, 'search', value = '')
    search_performed(FALSE)
    selected_dataset(NULL)
    detail_dataset_id(NULL)
  })

  observeEvent(input$search, {
    if (!nzchar(trimws(input$search %||% ''))) {
      search_performed(FALSE)
      selected_dataset(NULL)
      detail_dataset_id(NULL)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$run_search, {
    qres_init_schema(con)
    q <- trimws(input$search %||% '')

    perturb_type <- if (nzchar(input$search_perturb_type %||% '') &&
                        input$search_perturb_type != 'all')
      input$search_perturb_type else NULL
    perturb_name <- if (nzchar(trimws(input$search_perturb %||% '')))
      trimws(input$search_perturb) else NULL

    result_rows  <- NULL   # single exit point — set once, written once
    notify_msg   <- NULL

    # ── Step 0: Sidebar-only search (empty q, perturbation filters set) ──────
    if (!nzchar(q) && (!is.null(perturb_type) || !is.null(perturb_name))) {
      rows <- tryCatch(
        qres_filter_datasets(con,
          perturbation_type = perturb_type,
          perturbation      = perturb_name,
          registered_only   = FALSE),
        error = function(e) NULL
      )
      if (!is.null(rows) && nrow(rows) > 0) {
        result_rows <- rows
        notify_msg  <- paste0('Matched ', nrow(rows), ' dataset(s) by perturbation filter')
      }
    }

    # ── Step 1: Rule-based NL parser (always runs, no key needed) ─────────
    if (is.null(result_rows) && nzchar(q) && !grepl('^10\\.', q)) {
      parsed <- tryCatch(qres_parse_nl_query(q), error = function(e) NULL)

      if (!is.null(parsed) && length(parsed) > 0) {
        if (!is.null(perturb_type)) parsed$perturbation_type <- perturb_type
        if (!is.null(perturb_name)) parsed$perturbation      <- perturb_name
        rows <- tryCatch(
          do.call(qres_filter_datasets, c(list(con = con, registered_only = FALSE), parsed)),
          error = function(e) NULL
        )
        if (!is.null(rows) && nrow(rows) > 0) {
          result_rows <- rows
          notify_msg  <- paste0('Matched ', nrow(rows), ' dataset(s) — filters: ',
                                paste(names(parsed), collapse = ', '))
        }
      }

      # ── Step 2: LLM fallback (only when rule parser found nothing) ────
      if (is.null(result_rows) && qres_llm_available()) {
        filters <- withProgress(
          message = paste0('Asking ', qres_llm_backend_name(), '…'),
          value   = 0.5,
          expr    = tryCatch(qres_nl_to_action(q), error = function(e) NULL)
        )
        if (!is.null(filters) && length(filters) > 0) {
          if (!is.null(perturb_type)) filters$perturbation_type <- perturb_type
          if (!is.null(perturb_name)) filters$perturbation      <- perturb_name
          rows <- tryCatch(
            do.call(qres_filter_datasets, c(list(con = con, registered_only = FALSE), filters)),
            error = function(e) NULL
          )
          if (!is.null(rows) && nrow(rows) > 0) {
            result_rows <- rows
            notify_msg  <- paste0('LLM (', qres_llm_backend_name(), ') matched ',
                                  nrow(rows), ' dataset(s)')
          }
        }
      }
    }

    # ── Step 3: Keyword fallback ───────────────────────────────────────────
    if (is.null(result_rows)) {
      result_rows <- tryCatch(qres_search_datasets(con, q), error = function(e) data.frame())
      if (nrow(result_rows) > 0) {
        if (!is.null(perturb_type) && 'perturbation_type' %in% names(result_rows))
          result_rows <- result_rows[
            is.na(result_rows$perturbation_type) |
            grepl(perturb_type, result_rows$perturbation_type, ignore.case = TRUE), ]
        if (!is.null(perturb_name) && 'perturbation' %in% names(result_rows))
          result_rows <- result_rows[
            is.na(result_rows$perturbation) |
            grepl(perturb_name, result_rows$perturbation, ignore.case = TRUE), ]
      }
    }

    dataset_results(result_rows %||% data.frame())
    search_performed(TRUE)
    if (!is.null(notify_msg)) showNotification(notify_msg, type = 'message', duration = 4)
  }, ignoreInit = TRUE)

  output$dataset_table <- renderDT(server = TRUE, {
    df <- display_results()
    if (nrow(df) == 0) return(datatable(data.frame(message = 'No datasets found. Click "Initialize / refresh DB" first.')))

    # Truncate long fields for display; full value still in download
    display <- df
    if ('authors' %in% names(display))
      display$authors <- ifelse(nchar(display$authors %||% '') > 35,
                                paste0(substr(display$authors, 1, 35), '…'),
                                display$authors)
    if ('doi' %in% names(display))
      display$doi <- ifelse(nchar(display$doi %||% '') > 22,
                            paste0(substr(display$doi, 1, 22), '…'),
                            display$doi)
    if ('medium_type' %in% names(display))
      display$medium_type <- ifelse(nchar(display$medium_type %||% '') > 25,
                                    paste0(substr(display$medium_type, 1, 25), '…'),
                                    display$medium_type)

    # Column order: most useful first, verbose last
    col_order <- c('dataset_id', 'species', 'strain', 'temperature_c',
                   'perturbation_type', 'perturbation', 'medium_type',
                   'signal_type', 'measurement_unit',
                   'year', 'authors', 'has_raw', 'doi', 'raw_file', 'plate_reader')
    display <- display[, intersect(col_order, names(display)), drop = FALSE]

    # Columns hidden by default (accessible via column visibility button)
    hidden_idx <- which(names(display) %in% c('dataset_id', 'raw_file', 'plate_reader', 'doi')) - 1L

    # Row coloring by signal_type via JS rowCallback
    st_col <- which(names(display) == 'signal_type') - 1L   # 0-indexed for JS
    row_cb <- if (length(st_col) == 1L) {
      DT::JS(sprintf(
        "function(row, data) {
           var st = data[%d];
           var bg = {
             'OD600':        'rgba(91,155,213,0.13)',
             'OD750':        'rgba(51,120,190,0.13)',
             'fluorescence': 'rgba(180,100,200,0.13)',
             'luminescence': 'rgba(240,160,30,0.13)',
             'CFU':          'rgba(112,173,71,0.13)',
             'cell_count':   'rgba(60,140,50,0.13)'
           }[st];
           if (bg) $(row).css('background-color', bg);
         }", st_col))
    } else NULL

    dt_opts <- list(
      pageLength     = 12,
      scrollX        = TRUE,
      scrollCollapse = TRUE,
      autoWidth      = FALSE,
      dom            = 'Bfrtip',
      buttons        = list(list(extend = 'colvis', text = 'Show/hide columns')),
      columnDefs     = list(
        list(visible = FALSE, targets = hidden_idx),
        list(width = '90px',  targets = which(names(display) == 'dataset_id') - 1L),
        list(width = '80px',  targets = which(names(display) == 'species') - 1L),
        list(width = '55px',  targets = which(names(display) == 'temperature_c') - 1L),
        list(width = '110px', targets = which(names(display) == 'perturbation_type') - 1L),
        list(width = '100px', targets = which(names(display) == 'signal_type') - 1L),
        list(width = '80px',  targets = which(names(display) == 'measurement_unit') - 1L),
        list(width = '130px', targets = which(names(display) %in% c('medium_type','authors')) - 1L),
        list(className = 'dt-center', targets = '_all')
      )
    )
    if (!is.null(row_cb)) dt_opts$rowCallback <- row_cb

    datatable(display,
              selection  = 'multiple',
              extensions = 'Buttons',
              options    = dt_opts,
              rownames   = FALSE)
  })

  # Download current (filtered) search results as CSV
  output$download_search_ui <- renderUI({
    if (nrow(display_results()) > 0)
      downloadButton('download_search_csv', 'Download results (CSV)', class = 'btn-default')
  })

  output$download_search_csv <- downloadHandler(
    filename = function() paste0('phamig_search_', Sys.Date(), '.csv'),
    content  = function(file) write.csv(display_results(), file, row.names = FALSE)
  )

  observeEvent(input$dataset_table_rows_selected, ignoreNULL = FALSE, {
    df  <- display_results()
    row <- input$dataset_table_rows_selected
    if (length(row) >= 1 && nrow(df) >= row[1]) {
      id <- df$dataset_id[row[1]]
      selected_dataset(id)
      detail_dataset_id(id)
    } else {
      selected_dataset(NULL)
      detail_dataset_id(NULL)
    }
  })

  # Shared picker shown above the Links / QC tabs — keeps both tabs in sync
  output$detail_selector_ui <- renderUI({
    sel <- input$dataset_table_rows_selected
    df  <- display_results()
    if (length(sel) == 0 || nrow(df) == 0) return(NULL)
    ids <- df$dataset_id[sel]
    n   <- length(ids)
    cur <- detail_dataset_id() %||% ids[1]

    if (n == 1) {
      tags$p(style = 'font-size:13px; color:#555; margin:0 0 6px;',
             tags$strong('Dataset: '), ids[1])
    } else {
      choices <- setNames(ids, sprintf('%d  —  %s', seq_along(ids), ids))
      tags$div(style = 'margin-bottom:6px;',
        radioButtons('detail_dataset_selector',
                     label  = tags$span(style = 'font-size:12px; font-weight:600; color:#555;',
                                        'Viewing:'),
                     choices  = choices,
                     selected = if (cur %in% ids) cur else ids[1],
                     inline   = TRUE)
      )
    }
  })

  observeEvent(input$detail_dataset_selector, {
    detail_dataset_id(input$detail_dataset_selector)
  })

  output$selected_info <- renderUI({ NULL })   # removed — big green button replaces this

  observeEvent(input$go_to_comparison, {
    sel <- input$dataset_table_rows_selected
    df  <- display_results()
    ids <- if (length(sel) > 0 && nrow(df) > 0)
      df$dataset_id[sel]
    else
      selected_dataset()
    if (is.null(ids) || length(ids) == 0) return()
    analyze_selected_ids(ids)
    # Auto-upgrade to C when user already has a valid file upload
    analyse_mode_rv(if (isTRUE(comp_upload_valid())) 'C' else 'B')
    change_page('analyse')
  })

  # ── Raw curves ─────────────────────────────────────────────────────────────
  raw_long <- reactive({
    req(detail_dataset_id())
    tryCatch(
      qres_get_raw_curves(con, detail_dataset_id()),
      error = function(e) {
        validate(need(FALSE, paste('No raw measurements yet for', detail_dataset_id(),
                                   '— click "Initialize / refresh DB" first.')))
      }
    )
  })

  output$growth_plot <- renderPlot({
    did  <- selected_dataset()
    df   <- dataset_results()
    row  <- if (nrow(df) > 0) df[df$dataset_id == did, ][1, ] else NULL
    raw  <- raw_long()

    # Q1 organism · Q2 condition (medium · temperature · perturbation)
    title <- if (!is.null(row) && nrow(row) > 0) {
      parts <- c(
        paste0(row$species %||% did,
               if (!is.na(row$strain %||% NA) && nzchar(row$strain %||% ''))
                 paste0(' ', row$strain) else ''),
        if (!is.na(row$medium_type %||% NA)) row$medium_type else NULL,
        if (!is.na(row$temperature_c %||% NA)) paste0(row$temperature_c, '°C') else NULL,
        if (!is.na(row$perturbation_type %||% NA) && row$perturbation_type != 'none')
          paste0(row$perturbation_type,
                 if (!is.na(row$perturbation %||% NA) && nzchar(row$perturbation %||% ''))
                   paste0(': ', row$perturbation) else '') else NULL
      )
      paste(Filter(Negate(is.null), parts), collapse = ' · ')
    } else { did }
    # Q3 measurement · Q4 preprocessing · Q5 observation unit
    sig   <- if (!is.null(raw) && 'signal_type' %in% names(raw))
               unique(na.omit(raw$signal_type))[1] else NULL
    munit <- if (!is.null(row) && !is.null(row$measurement_unit %||% NULL) &&
                 nzchar(row$measurement_unit %||% '')) row$measurement_unit else NULL
    meas_lbl  <- if (!is.null(sig) && !is.null(munit)) paste0(sig, ' [', munit, ']')
                 else sig
    sub_parts <- Filter(Negate(is.null), list(meas_lbl, 'no baseline correction', '1 line = 1 well'))
    subtitle  <- paste(sub_parts, collapse = ' · ')
    p <- qres_plot_growth(raw, log_y = input$log_y, title = title)
    if (nzchar(subtitle)) p <- p + ggplot2::labs(subtitle = subtitle)
    p
  })

  output$plate_qc_text <- renderPrint({
    req(raw_long())
    qc <- qres_check_plate_data(raw_long())
    cat(sprintf('Wells: %d  |  Rows: %d  |  Time: %.1f – %.1f h\n',
                qc$n_wells, qc$n_rows, qc$time_range[1], qc$time_range[2]))
    if (!qc$ok) cat('Issues:\n', paste('-', qc$issues, collapse = '\n'), '\n')
  })

  # ── Single plugin run ──────────────────────────────────────────────────────
  observeEvent(input$run_plugin, {
    req(selected_dataset())
    did    <- selected_dataset()
    plugin <- input$plugin
    data   <- raw_long()
    params <- list(correction_type = input$correction_type)

    if (plugin == 'kinbiont' && !isTRUE(.kinbiont_julia_ready())) {
      showNotification(
        tagList(
          tags$strong('KinBiont.jl not available.'), tags$br(),
          tags$span(style = 'font-size:12px;',
            '1. Install Julia 1.11+: ',
            tags$a('julialang.org/downloads', href='https://julialang.org/downloads/', target='_blank'), tags$br(),
            '2. In Julia: ', tags$code('using Pkg; Pkg.add(url="https://github.com/pinheiroGroup/Kinbiont.jl")'), tags$br(),
            '3. Optional: set JULIA_HOME in ~/.Renviron to the Julia bin dir.'
          )
        ),
        type = 'warning', duration = 15
      )
      return()
    }

    withProgress(message = paste0('Running ', plugin, ' on ', did, '…'), value = 0.1, {

      processing_id <- qres_register_processing_run(
        con, did, plugin,
        correction_type = input$correction_type,
        params          = params,
        status          = 'running'
      )

      setProgress(0.4, detail = 'Fitting curves…')
      result <- tryCatch(
        qres_run_plugin(data, plugin, params),
        error = function(e) list(
          derived_metrics = data.frame(),
          logs = list(message = conditionMessage(e), status = 'error')
        )
      )

      setProgress(0.8, detail = 'Saving results…')
      status_out <- if (nrow(result$derived_metrics) > 0) 'finished' else 'no_results'

      if (nrow(result$derived_metrics) > 0) {
        m               <- result$derived_metrics
        m$processing_id <- processing_id
        m$dataset_id    <- m$dataset_id %||% did
        m$metric_id     <- paste(processing_id, m$well_id, m$metric_name,
                                 seq_len(nrow(m)), sep = '__')
        cols_needed <- c('metric_id', 'processing_id', 'dataset_id', 'well_id',
                         'metric_name', 'metric_value', 'metric_unit', 'model_name')
        m <- m[, intersect(cols_needed, names(m)), drop = FALSE]
        DBI::dbWriteTable(con, 'derived_metric', m, append = TRUE)
      }

      DBI::dbExecute(con,
        "UPDATE processing_run SET status = ?, message = ? WHERE processing_id = ?",
        params = list(status_out, result$logs$message %||% '', processing_id))
    })

    # Notify and switch to Metrics tab
    log_msg <- result$logs$message %||% ''
    if (nrow(result$derived_metrics) > 0) {
      n_wells <- length(unique(result$derived_metrics$well_id))
      showNotification(
        paste0(plugin, ': fitted ', n_wells, ' well(s). Results in Metrics tab.'),
        type = 'message', duration = 6
      )
      updateTabsetPanel(session, 'mainTabset', selected = 'Metrics')
    } else {
      showNotification(
        tagList(tags$strong(paste0(plugin, ' returned no results. ')),
                if (nzchar(log_msg)) log_msg else 'Check plugin requirements.'),
        type = 'warning', duration = 10
      )
    }
  })

  output$metrics_table <- renderDT({
    qres_init_schema(con)
    datatable(
      DBI::dbGetQuery(con, 'SELECT * FROM derived_metric LIMIT 1000'),
      options = list(pageLength = 20, scrollX = TRUE))
  })

  output$processing_table <- renderDT({
    qres_init_schema(con)
    datatable(
      DBI::dbGetQuery(con, 'SELECT * FROM processing_run ORDER BY created_at DESC'),
      options = list(pageLength = 8, scrollX = TRUE))
  })

  output$plugin_table <- renderDT({
    datatable(qres_plugins(), options = list(dom = 't', scrollX = TRUE))
  })

  # ── NCBI taxonomy link ─────────────────────────────────────────────────────
  output$ncbi_link_box <- renderUI({
    req(detail_dataset_id())
    wm <- tryCatch(
      DBI::dbGetQuery(con,
        "SELECT DISTINCT species, strain, taxon_id FROM well_metadata WHERE plate_id LIKE ? LIMIT 1",
        params = list(paste0(detail_dataset_id(), '%'))),
      error = function(e) NULL
    )
    if (is.null(wm) || nrow(wm) == 0) return(NULL)
    taxon_id <- wm$taxon_id[1]
    species  <- wm$species[1]
    strain   <- wm$strain[1]
    strain_clean  <- trimws(strain %||% '')
    strain_first  <- if (nzchar(strain_clean)) strsplit(strain_clean, '\\s+')[[1]][1] else ''
    # Display label uses full strain; search term uses only first strain word (avoids NCBI misses)
    org_label     <- if (nzchar(strain_clean)) paste(species, strain_clean) else species
    search_term   <- if (nzchar(strain_first)) paste(species, strain_first) else species
    if (!is.na(taxon_id) && nzchar(taxon_id) && !taxon_id %in% c('undefined', 'NA')) {
      ncbi_url <- paste0('https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=', taxon_id)
      tags$div(class = 'alert alert-info',
        tags$strong('NCBI Taxonomy: '),
        tags$em(org_label), ' — ',
        tags$a(href = ncbi_url, target = '_blank', paste0('TaxID ', taxon_id, ' ↗'))
      )
    } else if (!is.na(species) && nzchar(species)) {
      search_url <- paste0('https://www.ncbi.nlm.nih.gov/taxonomy/?term=',
                           utils::URLencode(search_term))
      tags$div(class = 'alert alert-warning',
        tags$strong('No NCBI TaxID stored. '),
        'Search NCBI: ',
        tags$a(href = search_url, target = '_blank', paste0(org_label, ' ↗'))
      )
    }
  })

  # ── DOI / PubMed link for selected dataset ────────────────────────────────
  output$doi_link_box <- renderUI({
    req(detail_dataset_id())
    d <- tryCatch(
      DBI::dbGetQuery(con,
        "SELECT doi, source_url, authors, year, title FROM dataset WHERE dataset_id = ? LIMIT 1",
        params = list(detail_dataset_id())),
      error = function(e) NULL
    )
    if (is.null(d) || nrow(d) == 0) return(NULL)
    links <- list()
    doi <- d$doi[1]; url <- d$source_url[1]
    if (!is.na(doi) && nzchar(doi %||% '')) {
      doi_url    <- paste0('https://doi.org/', doi)
      pubmed_url <- paste0('https://pubmed.ncbi.nlm.nih.gov/?term=', utils::URLencode(doi))
      links <- c(links, list(
        tags$span(tags$strong('DOI: '),
          tags$a(href = doi_url,    target = '_blank', doi), ' | ',
          tags$a(href = pubmed_url, target = '_blank', 'Search PubMed ↗'))
      ))
    }
    if (!is.na(url) && nzchar(url %||% '')) {
      links <- c(links, list(
        tags$span(tags$strong('Source: '),
          tags$a(href = url, target = '_blank', url))
      ))
    }
    if (length(links) == 0) return(NULL)
    tags$div(class = 'alert alert-info',
      tags$strong(paste0(d$authors[1], ' (', d$year[1], '). ')),
      if (!is.na(d$title[1])) tags$em(paste0(d$title[1], '. ')),
      tags$br(),
      do.call(tagList, links)
    )
  })

  # ── Gene-level links (UniProt, CGSC, KEGG) — shown for Keio datasets ────────
  output$gene_links_box <- renderUI({
    req(detail_dataset_id())
    did <- detail_dataset_id()
    if (!grepl('^keio_', did)) return(NULL)

    # Extract JWID from dataset_id: keio_jw1950_lb → JW1950
    parts <- strsplit(did, '_')[[1]]
    jwid  <- toupper(parts[2])   # e.g. "JW1950"

    # Gene name from well_metadata perturbation column
    wm <- tryCatch(
      DBI::dbGetQuery(con,
        "SELECT DISTINCT perturbation, gene_category FROM well_metadata WHERE plate_id LIKE ? LIMIT 1",
        params = list(paste0(did, '%'))),
      error = function(e) NULL)
    if (is.null(wm) || nrow(wm) == 0) return(NULL)

    gene_name <- wm$perturbation[1]
    cat_abbr  <- wm$gene_category[1]
    cat_full  <- KEIO_CAT_LABELS[cat_abbr %||% ''] %||% cat_abbr %||% ''

    tags$div(class = 'alert alert-info',
      tags$strong('Gene resources: '),
      tags$em(paste0('Δ', gene_name)), ' — ', jwid,
      if (nzchar(cat_full %||% ''))
        tags$span(style = 'color:#666; font-size:12px;',
                  paste0(' | Category: ', cat_full, ' (', cat_abbr, ')')),
      tags$br(),
      tags$a(href = keio_uniprot_url(gene_name), target = '_blank',
             'UniProt (E. coli K-12) ↗'), ' · ',
      tags$a(href = keio_kegg_url(gene_name), target = '_blank',
             'KEGG ECO ↗')
    )
  })

  # ── Medium composition table — shown for chemically defined media datasets ───
  output$medium_comp_box <- renderUI({
    req(detail_dataset_id())
    did <- detail_dataset_id()
    if (!grepl('^aida_', did)) return(NULL)

    mc <- tryCatch(
      DBI::dbGetQuery(con,
        "SELECT compound, conc_mm, conc_base_mm, is_stressor, is_buffer_adj
         FROM medium_composition WHERE dataset_id = ?
         ORDER BY is_stressor DESC, is_buffer_adj DESC, compound",
        params = list(did)),
      error = function(e) NULL)
    if (is.null(mc) || nrow(mc) == 0) return(NULL)

    # Keep only compounds present (> 0) in base or test
    mc <- mc[!is.na(mc$conc_mm) & (mc$conc_mm > 0 | mc$conc_base_mm > 0), ]
    if (nrow(mc) == 0) return(NULL)

    mc$Role <- ifelse(mc$is_stressor,   'stressor',
               ifelse(mc$is_buffer_adj, 'buffer adj.', ''))
    mc$conc_base_mm <- round(mc$conc_base_mm, 4)
    mc$conc_mm      <- round(mc$conc_mm, 4)

    tags$div(class = 'alert alert-success', style = 'padding-bottom:6px;',
      tags$strong('Medium composition'),
      tags$span(style = 'color:#555; font-size:12px; margin-left:8px;',
                '(base pattern vs. this condition)'),
      tags$br(), tags$br(),
      DT::dataTableOutput(paste0('mc_dt_', did))
    )
  })

  observeEvent(detail_dataset_id(), {
    did <- detail_dataset_id()
    if (is.null(did) || !grepl('^aida_', did)) return()
    mc <- tryCatch(
      DBI::dbGetQuery(con,
        "SELECT compound, conc_mm, conc_base_mm, is_stressor, is_buffer_adj
         FROM medium_composition WHERE dataset_id = ?
         ORDER BY is_stressor DESC, is_buffer_adj DESC, compound",
        params = list(did)),
      error = function(e) NULL)
    if (is.null(mc) || nrow(mc) == 0) return()
    mc <- mc[!is.na(mc$conc_mm) & (mc$conc_mm > 0 | mc$conc_base_mm > 0), ]
    mc$Role <- ifelse(mc$is_stressor,   'stressor',
               ifelse(mc$is_buffer_adj, 'buffer adj.', ''))
    mc$conc_base_mm <- round(mc$conc_base_mm, 4)
    mc$conc_mm      <- round(mc$conc_mm, 4)
    output[[paste0('mc_dt_', did)]] <- DT::renderDataTable({
      DT::datatable(
        mc[, c('compound','conc_base_mm','conc_mm','Role')],
        colnames  = c('Compound', 'Base (mM)', 'This condition (mM)', 'Role'),
        rownames  = FALSE,
        options   = list(pageLength = 15, dom = 'tp', scrollX = TRUE),
        selection = 'none'
      ) |>
        DT::formatStyle('Role',
          target = 'row',
          backgroundColor = DT::styleEqual(
            c('stressor', 'buffer adj.'),
            c('#fff3cd',  '#e8f4f8')))
    })
  })

  # ── Publication search — default: show all on page load ───────────────────
  pub_results <- reactiveVal(NULL)  # NULL = not yet loaded; data.frame = results

  # Auto-load all datasets when the By Publication tab is first visited
  observeEvent(once = TRUE, ignoreNULL = TRUE, ignoreInit = FALSE,
    input$mainTabset,  # fires when tab changes
    {
      if (is.null(pub_results())) {
        tryCatch({
          pub_results(qres_search_by_publication(con))
        }, error = function(e) pub_results(data.frame()))
      }
    }
  )

  observeEvent(input$run_pub_search, {
    res <- tryCatch(
      qres_search_by_publication(con,
        author   = input$pub_author,
        year_min = input$pub_year_min,
        year_max = input$pub_year_max,
        doi      = input$pub_doi,
        device   = input$pub_device),
      error = function(e) {
        showNotification(paste('Search error:', conditionMessage(e)), type = 'error')
        data.frame()
      }
    )
    pub_results(res)
  })

  observeEvent(input$clear_pub_search, {
    tryCatch(pub_results(qres_search_by_publication(con)),
             error = function(e) pub_results(data.frame()))
  })

  output$pub_search_table <- renderDT(server = TRUE, {
    df <- pub_results()
    if (is.null(df))   return(datatable(data.frame(message = 'Loading…')))
    if (nrow(df) == 0) return(datatable(data.frame(message = 'No datasets match the current filters.')))

    # Truncate verbose columns
    if ('authors' %in% names(df))
      df$authors <- ifelse(nchar(df$authors %||% '') > 40,
                           paste0(substr(df$authors, 1, 40), '…'), df$authors)
    if ('title' %in% names(df))
      df$title <- ifelse(nchar(df$title %||% '') > 50,
                         paste0(substr(df$title, 1, 50), '…'), df$title)
    if ('medium_type' %in% names(df))
      df$medium_type <- ifelse(nchar(df$medium_type %||% '') > 22,
                               paste0(substr(df$medium_type, 1, 22), '…'), df$medium_type)

    # Put most-useful columns first, hide verbose ones
    col_order <- c('dataset_id', 'species', 'medium_type', 'temperature_c',
                   'perturbation_type', 'perturbation', 'authors', 'year',
                   'doi', 'plate_reader', 'has_raw', 'title', 'source_url')
    df <- df[, intersect(col_order, names(df)), drop = FALSE]
    hidden_idx <- which(names(df) %in% c('dataset_id', 'title', 'source_url', 'plate_reader')) - 1L

    datatable(df, selection = 'single', rownames = FALSE,
              extensions = 'Buttons',
              options = list(
                pageLength  = 15,
                scrollX     = TRUE,
                scrollCollapse = TRUE,
                dom         = 'Bfrtip',
                buttons     = list(list(extend = 'colvis', text = 'Columns')),
                columnDefs  = list(
                  list(visible = FALSE, targets = hidden_idx),
                  list(width = '130px', targets = which(names(df) == 'authors') - 1L),
                  list(width = '90px',  targets = which(names(df) == 'dataset_id') - 1L),
                  list(className = 'dt-center',
                       targets = which(names(df) %in% c('temperature_c','year','has_raw')) - 1L)
                )
              ))
  })

  output$pub_doi_links <- renderUI({
    df <- pub_results()
    if (nrow(df) == 0) return(NULL)
    rows <- input$pub_search_table_rows_selected
    if (length(rows) == 0) return(NULL)
    doi <- df$doi[rows[1]]; url <- df$source_url[rows[1]]
    links <- list()
    if (!is.na(doi) && nzchar(doi %||% '')) {
      links <- c(links, list(tags$li(
        tags$a(href = paste0('https://doi.org/', doi), target = '_blank',
               paste0('DOI: ', doi)),
        ' | ',
        tags$a(href = paste0('https://pubmed.ncbi.nlm.nih.gov/?term=',
                             utils::URLencode(doi)),
               target = '_blank', 'PubMed search ↗')
      )))
    }
    if (!is.na(url) && nzchar(url %||% '')) {
      links <- c(links, list(tags$li(
        tags$a(href = url, target = '_blank', 'Source URL ↗')
      )))
    }
    if (length(links) == 0) return(NULL)
    tagList(tags$hr(), tags$ul(links))
  })

  # ── QC plots ───────────────────────────────────────────────────────────────
  # Both plots require standard plate-format wells (rows A–P, cols 1–24, ≥4 wells).
  # Non-plate uploads (e.g. 3-replicate gene knockouts) produce meaningless CV spikes
  # and empty heatmaps, so both are suppressed for such datasets.
  .has_plate_wells <- function(dat, min_wells = 4L) {
    if (is.null(dat) || nrow(dat) == 0L) return(FALSE)
    wells <- unique(as.character(dat$well))
    sum(grepl("^[A-Pa-p]([1-9]|1[0-9]|2[0-4])$", wells)) >= min_wells
  }

  output$qc_unavailable_msg <- renderUI({
    dat <- raw_long()
    req(dat, nrow(dat) > 0)

    # Fetch plate and dataset metadata once
    meta <- tryCatch(
      DBI::dbGetQuery(con,
        "SELECT d.raw_file, d.source_url, d.notes,
                p.plate_format, p.spectrophotometer
         FROM dataset d
         LEFT JOIN plate p USING(dataset_id)
         WHERE d.dataset_id = ?",
        params = list(detail_dataset_id())),
      error = function(e) NULL)

    plate_fmt  <- tolower(meta$plate_format[1]  %||% '')
    spectro    <- tolower(meta$spectrophotometer[1] %||% '')
    src        <- meta$source_url[1] %||% ''
    raw_ext    <- tolower(tools::file_ext(meta$raw_file[1] %||% ''))

    # Is the vessel a standard microplate?
    is_microplate <- grepl('well|plate|96|384|48|24|12|6\\s*-?well', plate_fmt) ||
                     (nzchar(plate_fmt) && !grepl('flask|tube|cuvette|vial|bottle', plate_fmt))
    # Was it recorded by a plate reader (automated, consistent well addresses)?
    is_plate_reader <- grepl('epoch|synergy|tecan|biotek|spectramax|varioskan|victor|infinite|clariostar|spark',
                              plate_fmt %||% spectro)

    repo <- if (grepl('figshare', src)) 'Figshare'
            else if (grepl('zenodo', src)) 'Zenodo'
            else if (grepl('doi\\.org', src)) 'the data repository'
            else NULL
    fmt_label <- switch(raw_ext, xlsx = 'XLSX', xls = 'XLS', csv = 'CSV', 'tabular')

    if (!.has_plate_wells(dat)) {
      # ── No plate-format well names at all ──────────────────────────────────
      repo_str <- if (!is.null(repo)) paste0(' (', repo, ')') else ''
      tags$div(
        class = 'alert alert-info', role = 'alert',
        tags$b('QC plots not available. '),
        sprintf(
          'The raw data were originally deposited in %s format%s without a
           standard microplate well layout. Measurements were recorded per sample,
           not per plate position. CV and endpoint heatmap require plate-reader
           output with A1–H12 well coordinates.',
          fmt_label, repo_str)
      )
    } else if (!is_microplate || grepl('flask|tube|cuvette|vial|bottle', plate_fmt)) {
      # ── Data have plate-like well labels but were not measured in a microplate ─
      vessel <- if (nzchar(plate_fmt)) plate_fmt else 'individual vessels (flasks or tubes)'
      instr  <- if (!is.null(repo))
        paste0(' Original data: <a href="', src, '" target="_blank">', repo, '</a>.') else ''
      tags$div(
        class = 'alert alert-warning', role = 'alert',
        tags$b('⚠ QC plots may be misleading. '),
        HTML(sprintf(
          'Growth was measured in %s, not in a microplate. Well labels
           (A1, B1…) are assigned per strain group, not spatial plate
           positions. The CV plot reflects biological variation <em>between
           strains</em>, not technical replicate agreement within a single
           condition. The endpoint heatmap shows relative yield per strain
           group rather than a spatial plate map.%s',
          vessel, instr))
      )
    }
    # else: standard microplate data — no message needed
  })

  output$qc_cv_plot <- renderPlot({
    dat <- raw_long()
    req(dat, .has_plate_wells(dat))
    qres_plot_qc_cv(dat)
  })

  output$qc_heatmap_plot <- renderPlot({
    dat <- raw_long()
    req(dat, .has_plate_wells(dat))
    qres_plot_endpoint_heatmap(dat)
  })

  # ── Analyse page — mode panel ─────────────────────────────────────────────

  .ui_filter_sidebar <- function() {
    wellPanel(
      h5('Filter datasets'),
      selectizeInput('comp_author', 'Author', choices = NULL,
                     options = list(placeholder = '(none) — no filter')),
      selectizeInput('comp_species', 'Species', choices = NULL,
                     options = list(placeholder = 'all species')),
      conditionalPanel(
        'output.comp_strain_visible === true',
        selectizeInput('comp_strain', 'Strain', choices = NULL,
                       options = list(placeholder = 'all strains'))
      ),
      sliderInput('comp_temp', 'Temperature (°C)',
                  min = 0, max = 60, value = c(25, 45), step = 1),
      textInput('comp_medium', 'Medium', placeholder = 'e.g. LB, MOPS, M9'),
      selectizeInput('comp_perturb_type', 'Perturbation type', choices = NULL,
                     options = list(placeholder = 'all types')),
      conditionalPanel(
        "input.comp_perturb_type !== null && input.comp_perturb_type !== ''",
        selectizeInput('comp_perturb_val', 'Perturbation', choices = NULL,
                       options = list(placeholder = 'all'))
      ),
      br(),
      uiOutput('comp_search_btn'),
      tags$div(style = 'margin-top:6px; text-align:right;',
        actionLink('comp_clear_filters', tagList(icon('times'), ' Clear filters')))
    )
  }

  # ── Well-input help reference panel ─────────────────────────────────────
  .ui_well_help <- function() {
    tags$details(style = 'margin-bottom:6px;',
      tags$summary(style = 'cursor:pointer; font-size:12px; color:#2980b9;',
        icon('circle-question'), ' Well selection syntax'),
      tags$div(style = 'background:#f8f9fa; border:1px solid #dee2e6; border-radius:4px;
                        padding:8px 12px; margin-top:4px; font-size:12px;',
        tags$div(style = 'display:grid; grid-template-columns:1fr 1fr; gap:0 16px;',
          # Left column
          tags$div(
            tags$p(style='margin:2px 0; font-weight:600;', 'Range notation'),
            tags$table(style='border-collapse:collapse; width:100%;',
              tags$tr(tags$td(tags$code('A1:H1'),   style='padding:1px 6px 1px 0;'),
                      tags$td('A1, B1, C1, D1, E1, F1, G1, H1  (same column)')),
              tags$tr(tags$td(tags$code('A1:A12'),  style='padding:1px 6px 1px 0;'),
                      tags$td('A1, A2, …, A12  (same row)')),
              tags$tr(tags$td(tags$code('A1:12'),   style='padding:1px 6px 1px 0;'),
                      tags$td('A1, A2, …, A12  (same row, shorthand)')),
              tags$tr(tags$td(tags$code('E12:H12'), style='padding:1px 6px 1px 0;'),
                      tags$td('E12, F12, G12, H12  (same col number)')),
              tags$tr(tags$td(tags$code('A1,B1,C1'),style='padding:1px 6px 1px 0;'),
                      tags$td('Comma-separated list'))
            ),
            tags$p(style='margin:6px 0 2px; font-weight:600;', 'Multi-plate prefix (stripped)'),
            tags$table(style='border-collapse:collapse;',
              tags$tr(tags$td(tags$code('P1:A1:H1'),     style='padding:1px 6px 1px 0;'),
                      tags$td('Wells on plate 1 (applies to all plates)')),
              tags$tr(tags$td(tags$code('P2:c(A1,B1)'),  style='padding:1px 6px 1px 0;'),
                      tags$td('R-vector on plate 2'))
            )
          ),
          # Right column
          tags$div(
            tags$p(style='margin:2px 0; font-weight:600;', 'Auto-detected blank labels'),
            tags$ul(style='margin:2px 0 6px; padding-left:16px;',
              tags$li(tags$code('only medium'), ', ', tags$code('medium'),
                      ', ', tags$code('no bacteria'), ' → ', tags$b('blank')),
              tags$li(tags$code('no stressor'), ', ', tags$code('no drug'),
                      ', ', tags$code('only inoculum'), ' → ', tags$b('control'))
            ),
            tags$p(style='margin:6px 0 2px; font-weight:600;', 'Replicate colours'),
            tags$p(style='margin:2px 0;',
              'Click-mode ', tags$b('Replicate'), ' on the plate grid, pick a group ID,',
              ' then click wells. Each group gets a distinct colour in the growth curves plot.',
              ' Legend shows e.g. ', tags$code('R1 (A1, B1, C1, D1)'), '.')
          )
        )
      )
    )
  }

  .ui_upload_section <- function() {
    tagList(
      .ui_well_help(),
      fluidRow(
        column(8,
          tags$div(style = 'position:relative;',
            fileInput('comp_upload_file', NULL,
                      accept = c('.csv', '.tsv', '.xlsx', '.xls', 'text/csv',
                                 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                                 'application/vnd.ms-excel'),
                      placeholder = 'Choose OD file (CSV or Excel)'),
            tags$div(style = 'position:absolute; top:0; right:0;',
              actionButton('show_comp_demo', 'Demo file',
                           class = 'btn btn-xs btn-info', icon = icon('table')))
          ),
          tags$small(style = 'color:#777;',
            icon('circle-info'),
            ' CSV or Excel (.xlsx/.xls). Column 1 = time (h), remaining = wells (A1, B2…). Excel: first sheet used.')
        ),
        column(4, br(), uiOutput('comp_upload_status'))
      ),
      conditionalPanel('output.comp_upload_ready === true',
        hr(),
        fluidRow(
          column(12,
            fileInput('comp_well_meta_file',
              tags$span('Well annotations',
                tags$small(' (optional — CSV with well, type, replicate columns)', style = 'color:#888;')),
              accept = '.csv', width = '100%')
          )
        ),
        uiOutput('comp_well_meta_status'),
        fluidRow(
          column(10,
            # Single-plate only: visual plate map + click-mode assignment
            conditionalPanel('output.comp_has_well_meta !== true && output.comp_is_multiplate !== true',
              tags$div(style = 'display:flex; align-items:center; gap:10px; margin-bottom:4px; flex-wrap:wrap;',
                tags$small(style = 'font-weight:600; color:#555;', 'Click mode:'),
                radioButtons('plate_click_mode', NULL, inline = TRUE,
                  choices = c('Blank'='blank', 'Control'='control',
                              'Replicate'='replicate', 'Clear'='clear'),
                  selected = 'blank')
              ),
              conditionalPanel('input.plate_click_mode === "replicate"',
                tags$div(style = 'display:flex; align-items:center; gap:8px; margin-bottom:4px;',
                  tags$small(style = 'color:#555;', 'Assign to group:'),
                  numericInput('plate_rep_group_id', NULL,
                               value = 1L, min = 1L, max = 99L, step = 1L, width = '70px')
                )
              ),
              uiOutput('comp_plate_map')
            ),
            # Multi-plate info banner + per-plate view toggle
            conditionalPanel('output.comp_is_multiplate === true && output.comp_has_well_meta !== true',
              tags$div(class = 'alert alert-info', style = 'font-size:12px; padding:6px 10px; margin-bottom:4px;',
                icon('layer-group'), ' ',
                uiOutput('comp_multiplate_info')
              ),
              tags$div(style = 'display:flex; align-items:center; gap:10px; margin-bottom:6px; flex-wrap:wrap;',
                tags$small(style = 'font-weight:600; color:#555;', 'Plot plates:'),
                radioButtons('comp_plate_view', NULL, inline = TRUE,
                  choices = c(
                    'All together (colour by plate)' = 'together',
                    'Per plate (separate panels)'    = 'per_plate'
                  ),
                  selected = 'together')
              )
            ),
            # Blank / control entry — always visible once data is loaded (single or multi-plate)
            conditionalPanel('output.comp_has_well_meta !== true',
              fluidRow(
                column(6,
                  tags$label('Blank wells', class = 'control-label'),
                  tags$div(style = 'display:flex; gap:4px; margin-bottom:3px;',
                    tags$input(id = 'comp_blank_range_txt', type = 'text',
                      class = 'form-control input-sm',
                      placeholder = 'e.g. E12:H12 or A1,B1',
                      style = 'height:28px; font-size:12px;'),
                    tags$button(id = 'comp_blank_range_btn', type = 'button',
                      class = 'btn btn-sm btn-default', style = 'height:28px; white-space:nowrap;',
                      icon('plus'), ' Add')
                  ),
                  uiOutput('comp_blank_range_status'),
                  selectizeInput('comp_upload_blanks', NULL,
                                 choices = NULL, multiple = TRUE,
                                 options = list(placeholder = 'none selected')),
                  actionButton('comp_clear_blank_sel', tagList(icon('xmark'), ' Clear blanks'),
                    class = 'btn btn-xs btn-outline-secondary',
                    style = 'font-size:11px; height:22px; padding:1px 7px; margin-bottom:3px;'),
                  tags$div(style = 'margin-top:2px;',
                    checkboxInput('comp_autodetect_blanks',
                      tags$span(style = 'font-size:11px; font-weight:normal;',
                        icon('wand-magic-sparkles', style = 'font-size:10px; color:#888;'),
                        ' Auto-detect from OD signal'),
                      value = FALSE),
                    conditionalPanel('input.comp_autodetect_blanks === true',
                      uiOutput('comp_autodetect_result')
                    )
                  )
                ),
                column(6,
                  tags$label('Control wells', class = 'control-label'),
                  tags$div(style = 'display:flex; gap:4px; margin-bottom:3px;',
                    tags$input(id = 'comp_ctrl_range_txt', type = 'text',
                      class = 'form-control input-sm',
                      placeholder = 'e.g. A12:H12 or P1:A1:4',
                      style = 'height:28px; font-size:12px;'),
                    tags$button(id = 'comp_ctrl_range_btn', type = 'button',
                      class = 'btn btn-sm btn-default', style = 'height:28px; white-space:nowrap;',
                      icon('plus'), ' Add')
                  ),
                  uiOutput('comp_ctrl_range_status'),
                  selectizeInput('comp_upload_controls', NULL,
                                 choices = NULL, multiple = TRUE,
                                 options = list(placeholder = 'none selected')),
                  actionButton('comp_clear_ctrl_sel', tagList(icon('xmark'), ' Clear controls'),
                    class = 'btn btn-xs btn-outline-secondary',
                    style = 'font-size:11px; height:22px; padding:1px 7px; margin-bottom:3px;')
                )
              ),
              # JS: Enter key in range text boxes triggers the Add button
              tags$script(HTML("
                $(document).on('keyup', '#comp_blank_range_txt,#comp_ctrl_range_txt', function(e) {
                  if (e.keyCode === 13) {
                    var btn = this.id === 'comp_blank_range_txt'
                              ? 'comp_blank_range_btn' : 'comp_ctrl_range_btn';
                    Shiny.setInputValue(btn, Math.random(), {priority:'event'});
                    Shiny.setInputValue(this.id + '_val', this.value, {priority:'event'});
                    this.value = '';
                  }
                });
                $(document).on('click', '#comp_blank_range_btn,#comp_ctrl_range_btn', function() {
                  var txt_id = this.id === 'comp_blank_range_btn'
                               ? 'comp_blank_range_txt' : 'comp_ctrl_range_txt';
                  var el = document.getElementById(txt_id);
                  Shiny.setInputValue(this.id + '_val', el ? el.value : '', {priority:'event'});
                  Shiny.setInputValue(this.id, Math.random(), {priority:'event'});
                  if (el) el.value = '';
                });
              "))
            ),
            conditionalPanel('output.comp_has_well_meta === true',
              uiOutput('comp_well_meta_summary')
            ),
            # Replicate aggregation mode — shown when any replicate groups are assigned
            conditionalPanel('output.comp_has_replicates === true',
              tags$hr(style = 'margin:10px 0 6px;'),
              tags$small(style = 'font-weight:600; color:#555;', 'Replicate handling for tool benchmarking:'),
              radioButtons('comp_rep_agg_mode', NULL, width = '100%',
                choices = c(
                  'per_well'   = 'Per well — fit each well independently',
                  'agg_before' = 'Average OD across replicates → fit one curve per group',
                  'agg_after'  = 'Fit per well → average metrics per replicate group'
                ),
                selected = 'per_well')
            )
          )
        ),
        br(),
        fluidRow(column(12,
          actionButton('comp_clear_upload', tagList(icon('trash'), ' Clear data'),
                       class = 'btn-sm btn-danger',
                       title = 'Remove uploaded data and reset the form'),
          tags$small(style = 'color:#888; margin-left:10px;',
            'To permanently archive your data, use ',
            tags$a(href = route_link('submit'), 'Submit Data'), '.')
        ))
      )
    )
  }

  # ── Mode card clicks → analyse_mode_rv ────────────────────────────────────
  observeEvent(input$analyse_mode_click, {
    m <- input$analyse_mode_click
    # Clicking Repository always starts clean — user must search again to pick datasets
    if (m == 'A') analyze_selected_ids(character(0))
    if (m %in% c('B', 'C') && length(analyze_selected_ids()) == 0) return()
    analyse_mode_rv(m)
  })

  # Auto-activate Mode B when IDs arrive AND user is on the Analyse page
  observe({
    ids <- analyze_selected_ids()
    if (length(ids) > 0 && is_page('analyse') && is.null(analyse_mode_rv()))
      analyse_mode_rv('B')
  })

  # Sync card highlight + disabled state with server mode
  observe({
    mode   <- analyse_mode_rv() %||% ''
    hasIds <- length(analyze_selected_ids()) > 0
    session$sendCustomMessage('setAnalyseMode', list(mode = mode, hasIds = hasIds))
  })

  # ── "Analyze Data" button in Search Data ───────────────────────────────────
  output$analyze_data_btn_ui <- renderUI({
    sel <- input$dataset_table_rows_selected
    n   <- length(sel)
    if (n == 0)
      tags$button(icon('chart-bar'), ' Analyze selected',
                  class = 'btn btn-success disabled',
                  disabled = NA, style = 'width:100%; cursor:not-allowed; opacity:0.55;',
                  title = 'Select at least one row in the Datasets table first')
    else
      actionButton('go_to_analyze',
                   tagList(icon('chart-bar'), sprintf(' Analyze %d dataset(s)', n)),
                   class = 'btn-success', width = '100%')
  })

  observeEvent(input$go_to_analyze, {
    sel  <- input$dataset_table_rows_selected
    rows <- display_results()
    if (length(sel) == 0 || nrow(rows) == 0) return()
    analyze_selected_ids(rows$dataset_id[sel])
    analyse_mode_rv('B')
    shiny.router::change_page('analyse')
  })

  # ── Dataset summary for Mode B / C header ──────────────────────────────────
  output$comp_selected_summary <- DT::renderDT({
    meta <- tryCatch(
      qres_get_datasets_by_ids(con, analyze_selected_ids()),
      error = function(e) data.frame())
    if (nrow(meta) == 0) return(DT::datatable(data.frame(), options = list(dom = 't')))
    show <- intersect(c('dataset_id', 'species', 'strain', 'temperature_c',
                         'medium_type', 'perturbation'), names(meta))
    DT::datatable(meta[, show, drop = FALSE], rownames = FALSE,
                  options = list(dom = 't', pageLength = 50, scrollX = TRUE))
  }, server = FALSE)

  # ── Mode A/B/C main panel ──────────────────────────────────────────────────
  output$analyse_main_panel <- renderUI({
    mode <- analyse_mode_rv() %||% ''
    if (!nzchar(mode))
      return(tags$div(
        style = 'color:dimgray; text-align:center; padding:40px;',
        icon('arrow-up'),
        ' Select a mode above.',
        tags$br(), tags$br(),
        tags$small('Repository and My file + repository require datasets selected in ',
                   tags$a(href = route_link('search'), 'Search Data'), '.')))

    if (mode == 'A') {
      mode_a_reset()   # declare dependency so Clear forces re-render
      tagList(
        hr(style = 'margin-top:0;'),
        .ui_upload_section(),
        conditionalPanel('output.comp_upload_ready === true',
          hr(),
          tabsetPanel(id = 'mode_a_tabs',
            tabPanel('Growth curves',   br(), uiOutput('comp_curves_ui')),
            tabPanel('Tool benchmarking', br(), uiOutput('comp_comparison_ui'))
          )
        )
      )

    } else if (mode == 'B') {
      n   <- length(analyze_selected_ids())
      tagList(
        tags$div(
          style = paste('background:#eaf4fb; border-left:4px solid #2980b9;',
                        'padding:10px 14px; margin-bottom:14px; border-radius:4px;',
                        'display:flex; justify-content:space-between; align-items:center;'),
          tags$span(icon('database'),
            tags$b(sprintf(' %d dataset(s)', n)),
            tags$span(style = 'color:#555; margin-left:6px;', 'selected from repository')),
          tags$span(
            tags$a(href = route_link('search'), style = 'font-size:12px; color:#2980b9;',
                   icon('arrow-left'), ' Change selection'),
            tags$span(style = 'margin: 0 8px; color:#ccc;', '|'),
            actionLink('comp_upgrade_to_c', tagList(icon('upload'), ' + Add my own file'))
          )
        ),
        tags$details(style = 'margin-bottom:10px;',
          tags$summary(style = 'font-size:12px; color:#888; cursor:pointer;',
            'Show selected datasets'),
          br(),
          DT::DTOutput('comp_selected_summary')
        ),
        # Blank / control wells — same input IDs as Mode A; Mode B exclusive so no ID clash
        fluidRow(
          column(6,
            tags$label('Blank wells', class = 'control-label'),
            tags$div(style = 'display:flex; gap:4px; margin-bottom:3px;',
              tags$input(id = 'comp_blank_range_txt', type = 'text',
                class = 'form-control input-sm',
                placeholder = 'e.g. E12:H12 or A1,B1',
                style = 'height:28px; font-size:12px;'),
              tags$button(id = 'comp_blank_range_btn', type = 'button',
                class = 'btn btn-sm btn-default', style = 'height:28px; white-space:nowrap;',
                icon('plus'), ' Add')
            ),
            uiOutput('comp_blank_range_status'),
            selectizeInput('comp_upload_blanks', NULL,
                           choices = NULL, multiple = TRUE,
                           options = list(placeholder = 'none selected')),
            actionButton('comp_clear_blank_sel', tagList(icon('xmark'), ' Clear blanks'),
              class = 'btn btn-xs btn-outline-secondary',
              style = 'font-size:11px; height:22px; padding:1px 7px; margin-bottom:3px;'),
            tags$div(style = 'margin-top:2px;',
              checkboxInput('comp_autodetect_blanks',
                tags$span(style = 'font-size:11px; font-weight:normal;',
                  icon('wand-magic-sparkles', style = 'font-size:10px; color:#888;'),
                  ' Auto-detect from OD signal'),
                value = FALSE),
              conditionalPanel('input.comp_autodetect_blanks === true',
                uiOutput('comp_autodetect_result')
              )
            )
          ),
          column(6,
            tags$label('Control wells', class = 'control-label'),
            tags$div(style = 'display:flex; gap:4px; margin-bottom:3px;',
              tags$input(id = 'comp_ctrl_range_txt', type = 'text',
                class = 'form-control input-sm',
                placeholder = 'e.g. A12:H12',
                style = 'height:28px; font-size:12px;'),
              tags$button(id = 'comp_ctrl_range_btn', type = 'button',
                class = 'btn btn-sm btn-default', style = 'height:28px; white-space:nowrap;',
                icon('plus'), ' Add')
            ),
            uiOutput('comp_ctrl_range_status'),
            selectizeInput('comp_upload_controls', NULL,
                           choices = NULL, multiple = TRUE,
                           options = list(placeholder = 'none selected')),
            actionButton('comp_clear_ctrl_sel', tagList(icon('xmark'), ' Clear controls'),
              class = 'btn btn-xs btn-outline-secondary',
              style = 'font-size:11px; height:22px; padding:1px 7px; margin-bottom:3px;')
          )
        ),
        tags$script(HTML("
          $(document).on('keyup', '#comp_blank_range_txt,#comp_ctrl_range_txt', function(e) {
            if (e.keyCode === 13) {
              var btn = this.id === 'comp_blank_range_txt'
                        ? 'comp_blank_range_btn' : 'comp_ctrl_range_btn';
              Shiny.setInputValue(btn, Math.random(), {priority:'event'});
              Shiny.setInputValue(this.id + '_val', this.value, {priority:'event'});
              this.value = '';
            }
          });
          $(document).on('click', '#comp_blank_range_btn,#comp_ctrl_range_btn', function() {
            var txt_id = this.id === 'comp_blank_range_btn'
                         ? 'comp_blank_range_txt' : 'comp_ctrl_range_txt';
            var el = document.getElementById(txt_id);
            Shiny.setInputValue(this.id + '_val', el ? el.value : '', {priority:'event'});
            Shiny.setInputValue(this.id, Math.random(), {priority:'event'});
            if (el) el.value = '';
          });
        ")),
        hr(style = 'margin:10px 0;'),
        tabsetPanel(id = 'mode_b_tabs',
          tabPanel('Growth curves',   br(), uiOutput('comp_curves_ui')),
          tabPanel('Tool benchmarking', br(), uiOutput('comp_comparison_ui'))
        )
      )

    } else {  # mode == 'C'
      n <- length(analyze_selected_ids())
      tagList(
        tags$div(
          style = paste('background:#eaf4fb; border-left:4px solid #2980b9;',
                        'padding:10px 14px; margin-bottom:14px; border-radius:4px;',
                        'display:flex; justify-content:space-between; align-items:center;'),
          tags$span(icon('database'),
            tags$b(sprintf(' %d dataset(s)', n)),
            tags$span(style = 'color:#555; margin-left:6px;', 'selected from repository')),
          tags$a(href = route_link('search'), style = 'font-size:12px; color:#2980b9;',
                 icon('arrow-left'), ' Change selection')
        ),
        tags$details(style = 'margin-bottom:10px;',
          tags$summary(style = 'font-size:12px; color:#888; cursor:pointer;',
            'Show selected datasets'),
          br(),
          DT::DTOutput('comp_selected_summary')
        ),
        tags$div(
          style = 'border:1px solid #d0dce8; border-radius:6px; padding:12px; margin-bottom:14px;',
          tags$div(style = 'font-weight:bold; color:#337ab7; margin-bottom:8px;',
            icon('upload'), ' My own file',
            tags$small(style = 'font-weight:normal; color:#777; margin-left:6px;',
                       '— combined with repository datasets above')),
          .ui_upload_section()
        ),
        tabsetPanel(id = 'mode_c_tabs',
          tabPanel('Growth curves',   br(), uiOutput('comp_curves_ui')),
          tabPanel('Tool benchmarking', br(), uiOutput('comp_comparison_ui'))
        )
      )
    }
  })

  # Clear uploaded data — Mode A: re-render; Mode C: downgrade to B
  observeEvent(input$comp_clear_upload, {
    comp_upload_rv(NULL)
    comp_upload_valid(FALSE)
    comp_upload_tried(FALSE)
    comp_upload_n_plates(1L)
    comp_plate_labels_rv(NULL)
    comp_well_meta_rv(NULL)
    comp_replicates_rv(list())
    updateSelectizeInput(session, 'comp_upload_blanks',
                         choices = character(0), selected = character(0))
    updateSelectizeInput(session, 'comp_upload_controls',
                         choices = character(0), selected = character(0))
    updateCheckboxInput(session, 'comp_autodetect_blanks', value = FALSE)
    updateRadioButtons(session,   'comp_plate_view',   selected = 'together')
    updateRadioButtons(session,   'comp_rep_agg_mode', selected = 'per_well')
    if ((analyse_mode_rv() %||% '') == 'C')
      analyse_mode_rv('B')
    else
      mode_a_reset(mode_a_reset() + 1L)  # force re-render → resets file input
  })

  # "+ Add my own file" in Mode B → upgrade to C
  observeEvent(input$comp_upgrade_to_c, analyse_mode_rv('C'))

  # Mode B: populate blank/control selectize choices from repo wells, then auto-show auto-detect
  observe({
    req((analyse_mode_rv() %||% '') == 'B')
    raw <- tryCatch(comp_all_raw(), error = function(e) NULL)
    req(!is.null(raw) && nrow(raw) > 0)
    wells <- sort(unique(raw$well))
    updateSelectizeInput(session, 'comp_upload_blanks',   choices = wells, server = TRUE)
    updateSelectizeInput(session, 'comp_upload_controls', choices = wells, server = TRUE)
    updateCheckboxInput(session, 'comp_autodetect_blanks', value = TRUE)
  })

  # Clear all filter inputs (Mode B / C)
  observeEvent(input$comp_clear_filters, {
    updateSelectizeInput(session, 'comp_author',   selected = '')
    updateSelectizeInput(session, 'comp_species',  selected = '')
    updateSelectizeInput(session, 'comp_strain',   selected = '')
    updateSliderInput(session,    'comp_temp',     value    = c(25, 45))
    updateTextInput(session,      'comp_medium',   value    = '')
    updateSelectizeInput(session, 'comp_perturb_type', selected = '')
    updateSelectizeInput(session, 'comp_perturb_val',  selected = '')
    comp_filtered_rv(data.frame())
  })

  # ── Analyse page — cascade observers ─────────────────────────────────────

  # Author → narrows species list
  observeEvent(input$comp_author, {
    au <- input$comp_author %||% ''
    sp_choices <- tryCatch(
      c('', qres_get_species_with_counts(con, author = if (nzchar(au)) au else NULL)),
      error = function(e) c('')
    )
    updateSelectizeInput(session, 'comp_species', choices = sp_choices,
                         selected = '', server = TRUE)
  }, ignoreInit = TRUE)

  # Species → strain (only populate when strains exist)
  observeEvent(input$comp_species, {
    sp <- input$comp_species %||% ''
    strains <- if (nzchar(sp))
      tryCatch(qres_get_distinct_strains(con, sp), error = function(e) character(0))
    else character(0)
    updateSelectizeInput(session, 'comp_strain',
                         choices  = c('', strains),
                         selected = '',
                         server   = TRUE)
  }, ignoreInit = TRUE)

  # Show strain selector only when the selected species has strains in the DB
  output$comp_strain_visible <- reactive({
    sp <- input$comp_species %||% ''
    if (!nzchar(sp)) return(FALSE)
    strains <- tryCatch(qres_get_distinct_strains(con, sp), error = function(e) character(0))
    length(strains) > 0
  })
  outputOptions(output, 'comp_strain_visible', suspendWhenHidden = FALSE)

  # Dynamic Search button — requires at least one filter or a valid upload
  output$comp_search_btn <- renderUI({
    has_filter <- nzchar(input$comp_author      %||% '') ||
                  nzchar(input$comp_species     %||% '') ||
                  nzchar(input$comp_medium      %||% '') ||
                  nzchar(input$comp_perturb_type %||% '')
    enabled <- has_filter || isTRUE(comp_upload_valid())
    if (enabled)
      actionButton('comp_filter', 'Search', class = 'btn-primary', width = '100%')
    else
      tags$button('Search', disabled = NA, class = 'btn btn-primary',
                  style = 'width:100%; cursor:not-allowed; opacity:0.55;',
                  title = 'Select at least one filter, or upload a file above')
  })

  observeEvent(input$comp_perturb_type, {
    pt <- input$comp_perturb_type %||% ''
    choices <- if (nzchar(pt))
      c('', tryCatch(qres_get_distinct_perturbations(con, pt), error = function(e) character(0)))
    else c('')
    updateSelectizeInput(session, 'comp_perturb_val', choices = choices, server = TRUE)
  }, ignoreInit = TRUE)

  # ── Filter button → comp_filtered_rv ──────────────────────────────────────
  observeEvent(input$comp_filter, {
    result <- tryCatch(
      qres_filter_datasets(
        con,
        species          = if (nzchar(input$comp_species %||% ''))       input$comp_species       else NULL,
        strain           = if (nzchar(input$comp_strain %||% ''))         input$comp_strain        else NULL,
        temp_min         = input$comp_temp[1],
        temp_max         = input$comp_temp[2],
        medium           = if (nzchar(input$comp_medium %||% ''))         input$comp_medium        else NULL,
        perturbation_type = if (nzchar(input$comp_perturb_type %||% '')) input$comp_perturb_type  else NULL,
        perturbation      = if (nzchar(input$comp_perturb_val %||% ''))  input$comp_perturb_val   else NULL,
        author           = if (nzchar(input$comp_author %||% ''))         input$comp_author        else NULL,
        registered_only  = TRUE
      ),
      error = function(e) data.frame()
    )
    # Apply strain post-hoc if needed (already handled above, but safety net)
    comp_filtered_rv(result)
  })

  output$comp_filtered_dt <- DT::renderDT({
    df <- comp_filtered_rv()
    if (nrow(df) == 0)
      return(DT::datatable(
        data.frame(Note = 'Click Search in the sidebar to find datasets.'),
        rownames = FALSE, options = list(dom = 't')))
    show <- intersect(c('dataset_id', 'species', 'strain', 'temperature_c',
                         'medium_type', 'perturbation', 'year', 'authors'), names(df))
    DT::datatable(df[, show, drop = FALSE],
      rownames  = FALSE, selection = 'multiple', filter = 'top',
      options   = list(pageLength = 15, scrollX = TRUE))
  }, server = TRUE)

  # ── Upload pipeline ────────────────────────────────────────────────────────
  output$comp_upload_ready <- reactive({ isTRUE(comp_upload_valid()) })
  outputOptions(output, 'comp_upload_ready', suspendWhenHidden = FALSE)

  observeEvent(input$show_comp_demo, {
    demo_path <- 'data/raw_csv/raw_data_template.csv'
    df <- tryCatch(
      utils::read.csv(demo_path, check.names = FALSE, stringsAsFactors = FALSE),
      error = function(e) NULL)
    showModal(modalDialog(
      title     = tagList(icon('table'), ' Growth curves CSV — format reference'),
      size      = 'l',
      easyClose = TRUE,
      footer    = tagList(
        downloadButton('download_comp_demo_raw', 'Download template',
                       class = 'btn-success btn-sm'),
        modalButton('Close')
      ),
      tags$div(class = 'alert alert-info',
               style = 'font-size:13px; padding:8px 12px; margin-bottom:10px;',
        tags$strong('Required format:'), ' First column = ', tags$code('time_h'),
        ' (decimal hours). Remaining columns = well names ',
        tags$code('A1'), ', ', tags$code('A2'), ' … ', tags$code('H12'),
        ' containing OD values. One row per time point.',
        tags$br(),
        if (!is.null(df))
          tags$span(style = 'color:#31708f;',
            'Preview: first 5 rows × first 7 columns (',
            paste0(nrow(df), ' time points, ', ncol(df) - 1, ' wells'), ').')
        else tags$span(style = 'color:#a94442;', 'Template file not found.')
      ),
      if (!is.null(df)) {
        preview <- df[seq_len(min(5, nrow(df))), seq_len(min(7, ncol(df))), drop = FALSE]
        tableOutput('comp_demo_table')
      }
    ))
    if (!is.null(df)) {
      preview <- df[seq_len(min(5, nrow(df))), seq_len(min(7, ncol(df))), drop = FALSE]
      output$comp_demo_table <- renderTable(preview, striped = TRUE, bordered = TRUE,
                                            spacing = 'xs', align = 'c')
    }
  })
  output$download_comp_demo_raw <- downloadHandler(
    filename = 'demo_growth_curves.csv',
    content  = function(file) {
      demo_path <- 'data/raw_csv/raw_data_template.csv'
      if (file.exists(demo_path)) {
        file.copy(demo_path, file)
      } else {
        t  <- seq(0, 24, by = 0.5)
        df <- data.frame(time_h = t)
        for (w in c('A1', 'A2', 'B1', 'B2'))
          df[[w]] <- round(0.01 + 0.5 * (1 - exp(-0.2 * t)) +
                             stats::rnorm(length(t), 0, 0.005), 4)
        utils::write.csv(df, file, row.names = FALSE)
      }
    }
  )

  observeEvent(input$comp_upload_file, {
    req(input$comp_upload_file)
    # Reset everything first so a re-upload always starts from scratch
    comp_upload_rv(NULL)
    comp_upload_valid(FALSE)
    comp_upload_n_plates(1L)
    comp_plate_labels_rv(NULL)
    comp_well_meta_rv(NULL)
    comp_replicates_rv(list())
    updateSelectizeInput(session, 'comp_upload_blanks',
                         choices = character(0), selected = character(0))
    updateSelectizeInput(session, 'comp_upload_controls',
                         choices = character(0), selected = character(0))
    updateCheckboxInput(session, 'comp_autodetect_blanks', value = FALSE)
    updateRadioButtons(session,   'comp_plate_view',   selected = 'together')
    updateRadioButtons(session,   'comp_rep_agg_mode', selected = 'per_well')
    comp_upload_tried(TRUE)
    raw_path  <- input$comp_upload_file$datapath
    is_excel  <- grepl('\\.xlsx?$', input$comp_upload_file$name, ignore.case = TRUE)
    n_sheets  <- if (is_excel && requireNamespace('readxl', quietly = TRUE))
                   length(readxl::excel_sheets(raw_path)) else 1L

    if (is_excel && qres_detect_logphase_format(raw_path)) {
      # LogPhase600 format: "Plate N - Raw Data" sheets for curves;
      # "Plate N - Results" sheets for well metadata (strain, concentration, replicate)
      long <- tryCatch(
        qres_read_logphase_raw(raw_path, dataset_id = '__upload__'),
        error = function(e) NULL)
      if (is.null(long) || nrow(long) == 0) { comp_upload_valid(FALSE); comp_upload_rv(NULL); return() }
      comp_upload_rv(long)
      comp_upload_valid(TRUE)
      raw_plate_count <- length(unique(long$plate_id))
      comp_upload_n_plates(raw_plate_count)
      # Store plate_id → "Plate N" label for per-plate facet display
      pids    <- sort(unique(long$plate_id))
      p_nums  <- regmatches(pids, regexpr('\\d+$', pids))
      comp_plate_labels_rv(setNames(paste('Plate', p_nums), pids))
      # Auto-load well metadata from Results sheets
      well_meta <- tryCatch(
        qres_read_logphase_well_meta(raw_path, dataset_id = '__upload__'),
        error = function(e) NULL)
      if (!is.null(well_meta) && nrow(well_meta) > 0) {
        comp_well_meta_rv(well_meta)
        # Auto-detect blanks from is_blank column
        blank_wells  <- unique(well_meta$well[isTRUE(well_meta$is_blank) | well_meta$is_blank == TRUE])
        updateSelectizeInput(session, 'comp_upload_blanks',   selected = blank_wells)
        # Auto-detect replicates
        if ('replicate' %in% names(well_meta)) {
          rep_ids <- setNames(as.list(as.integer(well_meta$replicate)), well_meta$well)
          rep_ids <- rep_ids[!is.na(unlist(rep_ids))]
          comp_replicates_rv(rep_ids)
        }
      }
    } else if (is_excel && n_sheets > 1L) {
      # Generic multi-sheet Excel: combine all sheets; bypass single-plate CSV validator
      sheet_names <- readxl::excel_sheets(raw_path)
      long <- tryCatch(
        qres_excel_to_long(raw_path, dataset_id = '__upload__'),
        error = function(e) NULL)
      if (is.null(long) || nrow(long) == 0) { comp_upload_valid(FALSE); comp_upload_rv(NULL); return() }
      comp_upload_rv(long)
      comp_upload_valid(TRUE)
      comp_upload_n_plates(n_sheets)
      # Store plate_id → original sheet name for per-plate facet labels
      # Sort plate_ids numerically by the trailing integer so Sheet N → Plate N is correct
      # even for 10+ sheets (lexicographic sort would misplace plate_10 before plate_2).
      pids <- unique(long$plate_id)
      pids <- pids[order(as.integer(sub('.*__plate_', '', pids)))]
      comp_plate_labels_rv(setNames(sheet_names[seq_along(pids)], pids))
    } else {
      # Single plate: CSV or single-sheet Excel
      path <- tryCatch(
        qres_normalise_to_csv(raw_path),
        error = function(e) { comp_upload_valid(FALSE); comp_upload_rv(NULL); NULL })
      req(!is.null(path))
      check <- tryCatch(qres_validate_raw_csv(path),
                        error = function(e) list(ok = FALSE, issues = conditionMessage(e)))
      if (!isTRUE(check$ok)) { comp_upload_valid(FALSE); comp_upload_rv(NULL); return() }
      long <- tryCatch(qres_raw_to_long(path, dataset_id = '__upload__'), error = function(e) NULL)
      if (is.null(long) || nrow(long) == 0) { comp_upload_valid(FALSE); return() }
      comp_upload_rv(as.data.frame(long))
      comp_upload_valid(TRUE)
      comp_upload_n_plates(1L)
    }
    wells <- unique(comp_upload_rv()$well)
    updateSelectizeInput(session, 'comp_upload_blanks',   choices = wells, server = TRUE)
    updateSelectizeInput(session, 'comp_upload_controls', choices = wells, server = TRUE)
    updateCheckboxInput(session, 'comp_autodetect_blanks', value = TRUE)
  })

  output$comp_upload_status <- renderUI({
    if (!isTRUE(comp_upload_tried())) return(NULL)
    if (isTRUE(comp_upload_valid())) {
      raw      <- comp_upload_rv()
      n_plates <- comp_upload_n_plates()
      plate_lbl <- if (n_plates > 1L)
        sprintf(', %d plates', n_plates) else ''
      tags$small(style = 'color:#27ae60;', icon('check'),
        sprintf(' Valid — %d wells, %d time-points%s.',
                length(unique(raw$well)), length(unique(raw$time_h)), plate_lbl))
    } else
      tags$small(style = 'color:#e74c3c;', icon('times'), ' Invalid format — check column names.')
  })

  output$comp_has_well_meta  <- reactive({ !is.null(comp_well_meta_rv()) })
  output$comp_has_replicates <- reactive({ length(comp_replicates_rv()) > 0 })
  output$comp_is_multiplate  <- reactive({ isTRUE(comp_upload_n_plates() > 1L) })
  outputOptions(output, 'comp_has_well_meta',  suspendWhenHidden = FALSE)
  outputOptions(output, 'comp_has_replicates', suspendWhenHidden = FALSE)
  outputOptions(output, 'comp_is_multiplate',  suspendWhenHidden = FALSE)

  output$comp_multiplate_info <- renderUI({
    n <- comp_upload_n_plates()
    raw <- comp_upload_rv()
    if (is.null(raw)) return(NULL)
    n_wells <- length(unique(raw$well))
    n_tp    <- length(unique(raw$time_h[raw$plate_id == unique(raw$plate_id)[[1]]]))
    tags$span(sprintf('%d plates loaded (%d wells × %d time-points each). Blank/control selection applies to all plates by well name.',
                      n, n_wells, n_tp))
  })

  .REP_PALETTE <- c('#e74c3c','#3498db','#2ecc71','#f39c12',
                    '#9b59b6','#1abc9c','#e67e22','#16a085')

  output$comp_plate_map <- renderUI({
    req(comp_upload_valid(), is.null(comp_well_meta_rv()))
    raw      <- comp_upload_rv()
    wells_in <- unique(raw$well)
    blanks   <- input$comp_upload_blanks   %||% character(0)
    controls <- input$comp_upload_controls %||% character(0)
    reps     <- comp_replicates_rv()

    row_letters <- sort(unique(sub('[0-9]+', '', wells_in)))
    col_nums    <- sort(as.integer(unique(sub('[A-Za-z]+', '', wells_in))))
    all_rows    <- LETTERS[seq_len(max(match(row_letters, LETTERS), 8))]
    all_cols    <- seq_len(max(col_nums, 12))

    header <- tags$tr(
      tags$th('', style = 'min-width:18px;'),
      lapply(all_cols, function(cc) tags$th(cc, style = 'min-width:26px;'))
    )
    body_rows <- lapply(all_rows, function(rr) {
      cells <- lapply(all_cols, function(cc) {
        wname    <- paste0(rr, cc)
        on_click <- sprintf("Shiny.setInputValue('plate_well_click','%s',{priority:'event'})", wname)
        if (!wname %in% wells_in)
          return(tags$td(class = 'pm-empty', title = wname))
        if (wname %in% blanks)
          return(tags$td(class = 'pm-blank',  title = wname, onclick = on_click, wname))
        if (wname %in% controls)
          return(tags$td(class = 'pm-ctrl',   title = wname, onclick = on_click, wname))
        if (!is.null(reps[[wname]])) {
          gid <- reps[[wname]]
          bg  <- .REP_PALETTE[(gid - 1L) %% length(.REP_PALETTE) + 1L]
          return(tags$td(
            style   = sprintf('width:26px;height:26px;background:%s;color:#fff;border:1px solid #ccc;cursor:pointer;text-align:center;font-size:9px;vertical-align:middle;', bg),
            title   = sprintf('%s (Rep %d)', wname, gid),
            onclick = on_click, wname))
        }
        tags$td(class = 'pm-active', title = wname, onclick = on_click, wname)
      })
      tags$tr(tags$th(rr), cells)
    })

    rep_groups  <- sort(unique(as.integer(unlist(reps))))
    rep_legend  <- lapply(rep_groups, function(gid) {
      bg <- .REP_PALETTE[(gid - 1L) %% length(.REP_PALETTE) + 1L]
      tags$span(style = 'margin-right:10px;',
        tags$span(style = sprintf('display:inline-block;width:11px;height:11px;background:%s;border:1px solid #bbb;vertical-align:middle;margin-right:3px;', bg)),
        sprintf('Rep %d', gid))
    })
    legend <- tags$div(style = 'font-size:11px; color:#888; margin-top:5px;',
      lapply(list(c('#ffd700','Blank'), c('#2c2c2c','Control'), c('#d5e8f5','Active')),
             function(x)
               tags$span(style = 'margin-right:10px;',
                 tags$span(style = sprintf('display:inline-block;width:11px;height:11px;background:%s;border:1px solid #bbb;vertical-align:middle;margin-right:3px;', x[1])),
                 x[2])),
      rep_legend
    )

    tagList(
      tags$div(class = 'plate-map-wrap',
        tags$table(class = 'pm-table', header, body_rows)
      ),
      legend
    )
  })

  output$comp_well_meta_status <- renderUI({
    meta <- comp_well_meta_rv()
    if (is.null(meta)) return(NULL)
    blanks   <- input$comp_upload_blanks   %||% character(0)
    controls <- input$comp_upload_controls %||% character(0)
    tags$div(class = 'alert alert-success',
             style = 'padding:5px 10px; font-size:12px; margin:4px 0;',
      icon('check'), sprintf(' Annotations loaded — %d blank wells, %d control wells.',
                             length(blanks), length(controls))
    )
  })

  output$comp_well_meta_summary <- renderUI({
    if (is.null(comp_well_meta_rv())) return(NULL)
    blanks   <- input$comp_upload_blanks   %||% character(0)
    controls <- input$comp_upload_controls %||% character(0)
    tagList(
      tags$p(style = 'color:#27ae60; font-size:13px; margin-bottom:4px;',
             icon('circle-check'), ' Well annotations auto-detected'),
      tags$p(style = 'font-size:12px; margin-bottom:2px;',
             tags$strong('Blank wells: '),
             if (length(blanks) == 0) tags$em('none') else paste(blanks, collapse = ', ')),
      tags$p(style = 'font-size:12px;',
             tags$strong('Control wells: '),
             if (length(controls) == 0) tags$em('none') else paste(controls, collapse = ', '))
    )
  })

  observeEvent(input$plate_well_click, {
    wname    <- input$plate_well_click
    mode_sel <- input$plate_click_mode %||% 'blank'
    req(!is.null(comp_upload_rv()))
    blanks   <- input$comp_upload_blanks   %||% character(0)
    controls <- input$comp_upload_controls %||% character(0)
    reps     <- comp_replicates_rv()
    if (mode_sel == 'blank') {
      blanks      <- if (wname %in% blanks) setdiff(blanks, wname) else c(blanks, wname)
      controls    <- setdiff(controls, wname)
      reps[[wname]] <- NULL
    } else if (mode_sel == 'control') {
      controls    <- if (wname %in% controls) setdiff(controls, wname) else c(controls, wname)
      blanks      <- setdiff(blanks, wname)
      reps[[wname]] <- NULL
    } else if (mode_sel == 'replicate') {
      gid <- as.integer(input$plate_rep_group_id %||% 1L)
      blanks      <- setdiff(blanks, wname)
      controls    <- setdiff(controls, wname)
      reps[[wname]] <- if (identical(reps[[wname]], gid)) NULL else gid
    } else {   # clear
      blanks      <- setdiff(blanks, wname)
      controls    <- setdiff(controls, wname)
      reps[[wname]] <- NULL
    }
    updateSelectizeInput(session, 'comp_upload_blanks',   selected = blanks)
    updateSelectizeInput(session, 'comp_upload_controls', selected = controls)
    comp_replicates_rv(reps)
  })

  # ── Range-text Add buttons for blank / control well entry ────────────────
  .apply_range_add <- function(txt_val, target_input) {
    raw_src   <- comp_upload_rv() %||% tryCatch(comp_all_raw(), error = function(e) NULL)
    req(!is.null(raw_src))
    available <- unique(raw_src$well)
    new_wells <- .parse_wells(txt_val %||% '', available = available)
    existing  <- input[[target_input]] %||% character(0)
    merged    <- union(existing, new_wells)
    updateSelectizeInput(session, target_input, selected = merged)
    list(n_new = length(new_wells), n_bad = length(new_wells) == 0 && nzchar(trimws(txt_val %||% '')))
  }

  observeEvent(input$comp_blank_range_btn, {
    res <- .apply_range_add(input$comp_blank_range_btn_val, 'comp_upload_blanks')
    output$comp_blank_range_status <- renderUI(
      if (res$n_bad)
        tags$small(style='color:#e74c3c;', icon('times'), ' No matching wells found.')
      else if (res$n_new > 0)
        tags$small(style='color:#27ae60;', icon('check'),
                   sprintf(' +%d well%s added.', res$n_new, if(res$n_new>1) 's' else ''))
      else NULL)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  observeEvent(input$comp_ctrl_range_btn, {
    res <- .apply_range_add(input$comp_ctrl_range_btn_val, 'comp_upload_controls')
    output$comp_ctrl_range_status <- renderUI(
      if (res$n_bad)
        tags$small(style='color:#e74c3c;', icon('times'), ' No matching wells found.')
      else if (res$n_new > 0)
        tags$small(style='color:#27ae60;', icon('check'),
                   sprintf(' +%d well%s added.', res$n_new, if(res$n_new>1) 's' else ''))
      else NULL)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # Criterion: sd < 0.05 across all timepoints — flat wells are blanks regardless of mean.
  # Falls back to repo data in Mode B (no file uploaded).
  comp_blank_candidates <- reactive({
    raw <- comp_upload_rv() %||% tryCatch(comp_all_raw(), error = function(e) NULL)
    if (is.null(raw) || nrow(raw) == 0)
      return(data.frame(well = character(0), mean_od = numeric(0),
                        sd_od = numeric(0), candidate = logical(0)))
    # Compute SD per (dataset, well) first — prevents inflating SD when
    # multiple datasets with different OD baselines are combined.
    per_ds <- dplyr::summarise(
      dplyr::group_by(raw, dataset_id, well),
      mean_od = mean(value, na.rm = TRUE),
      sd_od   = dplyr::coalesce(sd(value, na.rm = TRUE), 0),
      .groups = 'drop')
    dplyr::arrange(
      dplyr::summarise(
        dplyr::group_by(per_ds, well),
        mean_od   = mean(mean_od, na.rm = TRUE),
        sd_od     = mean(sd_od,   na.rm = TRUE),
        candidate = any(sd_od < 0.1),
        .groups   = 'drop'),
      sd_od
    )
  })

  output$comp_autodetect_result <- renderUI({
    req(isTRUE(input$comp_autodetect_blanks))
    df    <- comp_blank_candidates()
    req(nrow(df) > 0)
    cands <- df$well[df$candidate]
    if (length(cands) == 0)
      return(tags$div(style = 'font-size:12px; color:#888; padding:2px 0;',
        icon('circle-info'), ' No flat wells detected (sd >= 0.1 for all wells).'))
    tagList(
      tags$div(style = 'font-size:12px; margin-bottom:3px;',
        tags$strong(length(cands)),
        tags$span(style = 'color:#555;',
          sprintf(' candidate blank%s (OD sd < 0.1):',
                  if (length(cands) > 1) 's' else ''))
      ),
      tags$div(
        title = 'Click to select all text, then copy',
        style = paste0('background:#f0f7f0; border:1px solid #c3e6cb; border-radius:3px;',
                       'padding:3px 7px; font-family:monospace; font-size:12px;',
                       'margin-bottom:4px; cursor:text; user-select:all;'),
        paste(sort(cands), collapse = ', ')
      ),
      actionButton('comp_autodetect_add_blanks', 'Add to blanks',
                   class = 'btn btn-xs btn-success', icon = icon('plus'),
                   style = 'height:24px; padding:2px 8px; font-size:11px;')
    )
  })

  observeEvent(input$comp_autodetect_add_blanks, {
    cands  <- comp_blank_candidates()$well[comp_blank_candidates()$candidate]
    merged <- union(input$comp_upload_blanks %||% character(0), cands)
    updateSelectizeInput(session, 'comp_upload_blanks', selected = merged)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  observeEvent(input$comp_clear_blank_sel, {
    updateSelectizeInput(session, 'comp_upload_blanks', selected = character(0))
    updateCheckboxInput(session, 'comp_autodetect_blanks', value = FALSE)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  observeEvent(input$comp_clear_ctrl_sel, {
    updateSelectizeInput(session, 'comp_upload_controls', selected = character(0))
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # Wipe all Analyse Data state when the user navigates away from the page.
  observeEvent(is_page('analyse'), {
    if (isTRUE(is_page('analyse'))) return()
    analyse_mode_rv(NULL)
    analyze_selected_ids(character(0))
    comp_upload_rv(NULL)
    comp_upload_valid(FALSE)
    comp_upload_tried(FALSE)
    comp_upload_n_plates(1L)
    comp_plate_labels_rv(NULL)
    comp_well_meta_rv(NULL)
    comp_replicates_rv(list())
    mode_a_reset(mode_a_reset() + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$comp_well_meta_file, {
    req(input$comp_well_meta_file)
    path <- input$comp_well_meta_file$datapath
    meta <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(meta) || !('well' %in% tolower(names(meta)))) {
      showNotification('Well metadata CSV must have a "well" column.', type = 'error')
      return()
    }
    names(meta) <- tolower(names(meta))
    raw_wells   <- unique(comp_upload_rv()$well)
    .norm <- function(x) sub('^([A-Za-z])0+', '\\1', trimws(toupper(x)))
    meta$well <- .norm(meta$well)

    # Detect blanks/controls from explicit columns or from free-text strain/type labels
    label_col <- intersect(names(meta), c('type', 'strain', 'condition', 'treatment', 'content'))[1]
    if (!is.null(label_col) && !is.na(label_col)) {
      cls      <- .classify_well_label(meta[[label_col]])
      blanks   <- meta$well[cls == 'blank']
      controls <- meta$well[cls == 'control']
    } else {
      blanks   <- if ('is_blank'   %in% names(meta)) meta$well[as.logical(meta$is_blank)   %in% TRUE] else character(0)
      controls <- if ('is_control' %in% names(meta)) meta$well[as.logical(meta$is_control) %in% TRUE] else character(0)
    }
    blanks   <- intersect(.norm(blanks),   raw_wells)
    controls <- intersect(.norm(controls), raw_wells)
    updateSelectizeInput(session, 'comp_upload_blanks',   choices = raw_wells, selected = blanks,   server = TRUE)
    updateSelectizeInput(session, 'comp_upload_controls', choices = raw_wells, selected = controls, server = TRUE)

    # Auto-detect replicate groups from metadata
    rep_col <- intersect(tolower(names(meta)),
                         c('replicate', 'rep', 'group', 'rep_group', 'replicate_id', 'replication'))
    if (length(rep_col) > 0) {
      rep_vals  <- meta[[rep_col[1]]]
      rep_wells <- meta$well
      rep_ids   <- as.integer(factor(rep_vals))   # normalise to 1, 2, 3…
      reps      <- setNames(as.list(rep_ids), rep_wells)
      reps      <- reps[names(reps) %in% raw_wells]
      comp_replicates_rv(reps)
    } else {
      comp_replicates_rv(list())
    }
    comp_well_meta_rv(meta)
  })

  # ── Active dataset IDs (DB selection) + upload flag ───────────────────────
  comp_active_ids <- reactive({
    mode <- analyse_mode_rv() %||% ''
    if (mode %in% c('B', 'C')) return(analyze_selected_ids())
    character(0)
  })

  comp_has_upload <- reactive({
    isTRUE(comp_upload_valid()) && !is.null(comp_upload_rv())
  })

  # ── Replicate aggregation helpers ─────────────────────────────────────────
  # reps: named list well → integer group ID (from comp_replicates_rv)

  .agg_replicates_before <- function(raw, reps) {
    rep_df <- data.frame(well      = names(reps),
                         rep_group = as.integer(unlist(reps)),
                         stringsAsFactors = FALSE)
    raw <- dplyr::left_join(as.data.frame(raw), rep_df, by = 'well')
    unrep <- raw[is.na(raw$rep_group), setdiff(names(raw), 'rep_group')]
    reped <- raw[!is.na(raw$rep_group), ]
    if (nrow(reped) == 0) return(unrep)
    agg <- reped |>
      dplyr::group_by(rep_group, time_h) |>
      dplyr::summarise(
        value       = mean(value, na.rm = TRUE),
        signal_type = dplyr::first(signal_type),
        dataset_id  = dplyr::first(dataset_id),
        plate_id    = dplyr::first(plate_id),
        .groups     = 'drop'
      ) |>
      dplyr::mutate(well    = paste0('Rep', rep_group),
                    well_id = paste0(dplyr::first(reped$dataset_id), '__Rep', rep_group)) |>
      dplyr::select(-rep_group)
    keep <- intersect(names(unrep), names(agg))
    dplyr::bind_rows(unrep[, keep], agg[, keep])
  }

  .agg_metrics_after <- function(cmp, raw_orig, reps) {
    well_map <- unique(as.data.frame(raw_orig)[, c('well', 'well_id')])
    rep_df   <- data.frame(well      = names(reps),
                           rep_group = paste0('Rep', as.integer(unlist(reps))),
                           stringsAsFactors = FALSE)
    well_map <- dplyr::left_join(well_map, rep_df, by = 'well')
    cmp2 <- dplyr::left_join(cmp, well_map[, c('well_id', 'rep_group')], by = 'well_id')
    cmp2$well_id <- ifelse(!is.na(cmp2$rep_group), cmp2$rep_group, cmp2$well_id)
    cmp2 |>
      dplyr::group_by(dataset_id, well_id, tool, metric_name, metric_unit, plot_label) |>
      dplyr::summarise(metric_value = mean(metric_value, na.rm = TRUE), .groups = 'drop') |>
      as.data.frame()
  }

  # ── Combined raw data (for growth curves) ──────────────────────────────────
  comp_all_raw <- reactive({
    db_ids <- comp_active_ids()
    has_up <- comp_has_upload()
    req(length(db_ids) > 0 || has_up)

    df_meta <- comp_filtered_rv()
    .label <- function(did) {
      if (nrow(df_meta) == 0 || !did %in% df_meta$dataset_id) return(did)
      row   <- df_meta[df_meta$dataset_id == did, ][1, ]
      sp    <- trimws(row$species %||% '')
      tc    <- row$temperature_c %||% NA
      ptype <- tolower(trimws(row$perturbation_type %||% ''))
      pert  <- trimws(row$perturbation %||% '')
      lab   <- paste0(if (nzchar(sp)) sp else did,
                      if (is.finite(as.numeric(tc))) paste0(' ', round(as.numeric(tc), 1), '°C') else '')
      show  <- nzchar(pert) && !tolower(pert) %in% c('none', 'na', '') &&
               !ptype %in% c('temperature', 'none', '') &&
               is.na(suppressWarnings(as.numeric(pert)))
      if (show) lab <- paste0(lab, ' + ', pert)
      lab
    }

    raw_list <- lapply(db_ids, function(did) {
      raw <- tryCatch(qres_get_raw_curves(con, did), error = function(e) NULL)
      if (!is.null(raw) && nrow(raw) > 0) raw$plot_label <- .label(did)
      raw
    })

    if (has_up) {
      raw_up   <- comp_upload_rv()
      agg_mode <- input$comp_rep_agg_mode %||% 'per_well'
      reps     <- comp_replicates_rv()
      if (agg_mode == 'agg_before' && length(reps) > 0)
        raw_up <- .agg_replicates_before(raw_up, reps)
      if (nrow(raw_up) > 0) raw_up$plot_label <- 'Uploaded data'
      raw_list <- c(raw_list, list(raw_up))
    }

    dplyr::bind_rows(Filter(Negate(is.null), raw_list))
  })

  # ── Metrics reactive (shared between boxplot and table) ─────────────────────
  comp_metrics_long <- reactive({
    db_ids <- comp_active_ids()
    has_up <- comp_has_upload()
    req(length(db_ids) > 0 || has_up)

    nl_bm    <- input$comp_baseline %||% 'none'
    nl_tools <- input$comp_tools %||% c('model_free', 'gcplyr', 'growthrates')
    all_ids  <- c(if (has_up) '__upload__' else character(0), db_ids)

    nl_blank_ids <- character(0)
    if (nl_bm == 'blank_wells') {
      if (length(db_ids) > 0)
        nl_blank_ids <- c(nl_blank_ids,
          tryCatch(qres_get_blank_wells(con, db_ids), error = function(e) character(0)))
      # Convert well names → well_ids for the upload dataset
      blank_names <- input$comp_upload_blanks %||% character(0)
      if (has_up && length(blank_names) > 0) {
        up_raw <- comp_upload_rv()
        if (!is.null(up_raw))
          nl_blank_ids <- union(nl_blank_ids,
                                unique(up_raw$well_id[up_raw$well %in% blank_names]))
      }
    }

    df_meta <- comp_filtered_rv()
    .label <- function(did) {
      if (did == '__upload__') return('Uploaded data')
      if (nrow(df_meta) == 0 || !did %in% df_meta$dataset_id) return(did)
      row   <- df_meta[df_meta$dataset_id == did, ][1, ]
      sp    <- trimws(row$species %||% '')
      tc    <- row$temperature_c %||% NA
      ptype <- tolower(trimws(row$perturbation_type %||% ''))
      pert  <- trimws(row$perturbation %||% '')
      lab   <- paste0(if (nzchar(sp)) sp else did,
                      if (is.finite(as.numeric(tc))) paste0(' ', round(as.numeric(tc), 1), '°C') else '')
      show  <- nzchar(pert) && !tolower(pert) %in% c('none', 'na', '') &&
               !ptype %in% c('temperature', 'none', '') &&
               is.na(suppressWarnings(as.numeric(pert)))
      if (show) lab <- paste0(lab, ' + ', pert)
      lab
    }

    agg_mode <- input$comp_rep_agg_mode %||% 'per_well'
    reps     <- comp_replicates_rv()

    all_cmp <- lapply(all_ids, function(did) {
      if (did == '__upload__') {
        raw <- comp_upload_rv()
        if (agg_mode == 'agg_before' && length(reps) > 0)
          raw <- .agg_replicates_before(raw, reps)
      } else {
        raw <- tryCatch(qres_get_raw_curves(con, did), error = function(e) NULL)
      }
      if (is.null(raw) || nrow(raw) == 0) return(NULL)
      raw <- .apply_baseline(raw, method = nl_bm, blank_ids = nl_blank_ids)
      # Exclude blank wells from fitting
      if (length(nl_blank_ids) > 0)
        raw <- raw[!raw$well_id %in% nl_blank_ids, , drop = FALSE]
      cmp <- tryCatch(qres_compare_plugin_outputs(raw, plugins = nl_tools),
                      error = function(e) NULL)
      if (!is.null(cmp) && nrow(cmp) > 0) {
        cmp$dataset_id <- did
        cmp$plot_label <- .label(did)
        if (did == '__upload__' && agg_mode == 'agg_after' && length(reps) > 0)
          cmp <- .agg_metrics_after(cmp, comp_upload_rv(), reps)
      }
      cmp
    })
    dplyr::bind_rows(Filter(Negate(is.null), all_cmp))
  }) |> bindCache(comp_active_ids(), comp_has_upload(),
                  input$comp_tools, input$comp_baseline,
                  input$comp_rep_agg_mode, length(comp_replicates_rv()))

  # ── Wide table for download ─────────────────────────────────────────────────
  .comp_wide_analyse <- reactive({
    long <- comp_metrics_long()
    if (is.null(long) || nrow(long) == 0) return(data.frame())
    meta <- comp_filtered_rv()
    long$well <- sub('^.*__', '', long$well_id)
    wide <- long |>
      dplyr::select(dataset_id, well_id, well, metric_name, metric_value, tool) |>
      dplyr::mutate(col_name = paste0(tool, '·', metric_name)) |>
      dplyr::select(-tool, -metric_name) |>
      tidyr::pivot_wider(names_from = col_name, values_from = metric_value, values_fn = mean) |>
      dplyr::select(-well_id)
    if (!is.null(meta) && nrow(meta) > 0 && 'dataset_id' %in% names(meta)) {
      mcols <- intersect(c('species', 'temperature_c', 'medium_type',
                            'perturbation_type', 'perturbation'), names(meta))
      if (length(mcols) > 0) {
        msub <- dplyr::distinct(meta[, c('dataset_id', mcols), drop = FALSE])
        wide <- dplyr::left_join(wide, msub, by = 'dataset_id')
      }
    }
    front <- intersect(c('dataset_id', 'species', 'temperature_c', 'medium_type',
                          'perturbation_type', 'perturbation', 'well'), names(wide))
    mcols2 <- setdiff(names(wide), front)
    dplyr::arrange(dplyr::select(wide, dplyr::all_of(c(front, mcols2))),
                   dataset_id, well)
  })

  # ── Growth curves tab UI ────────────────────────────────────────────────────
  output$comp_curves_ui <- renderUI({
    ids    <- comp_active_ids()
    has_up <- comp_has_upload()
    if (length(ids) == 0 && !has_up)
      return(tags$div(style = 'color:dimgray; padding:30px; text-align:center;',
        icon('arrow-left'), ' Select datasets in the Datasets tab to view growth curves.'))
    blank_ids_db <- if (length(ids) > 0)
      tryCatch(qres_get_blank_wells(con, ids), error = function(e) character(0))
    else character(0)
    blank_ids_up <- input$comp_upload_blanks %||% character(0)
    has_blanks   <- length(c(blank_ids_db, blank_ids_up)) > 0
    tagList(
      tags$div(
        tags$b('Baseline subtraction', style = 'font-size:13px;'),
        tags$small(style = 'color:dimgray; margin-left:6px;',
          '— Sprouffske & Wagner (2012), Hall et al. (2014), gcplyr / Mira et al. (2022)'),
        br(),
        radioButtons('comp_curves_baseline', NULL, inline = TRUE,
          choices  = c('None' = 'none', 'Min per well' = 'min_well',
                       'First 3 tp' = 'first_3', 'Blank wells' = 'blank_wells'),
          selected = 'none'),
        if (!has_blanks)
          tags$small(style = 'color:#888; font-size:11px;',
            icon('info-circle'), ' No blank wells designated in these datasets.')
      ),
      fluidRow(column(2, checkboxInput('comp_log_y', 'Log y', FALSE))),
      tags$div(style = 'position:relative;',
        withSpinner(
          plotOutput('comp_curves', height = '420px',
                     hover = hoverOpts('comp_curves_hover', delay = 80,
                                       delayType = 'debounce', nullOutside = TRUE)),
          type = 6, color = '#5b9bd5'),
        uiOutput('comp_curves_tooltip')
      )
    )
  })

  # Shared reactive: baseline-corrected data used by both the plot and the hover tooltip.
  # nearPoints() requires the same data that was plotted to compute pixel distances correctly.
  comp_curves_data <- reactive({
    raw <- comp_all_raw()
    req(!is.null(raw) && nrow(raw) > 0)

    # Guard: refuse to combine datasets with different signal types.
    # OD600 + CFU or cell_count are dimensionally incompatible — the combined
    # y-axis would be meaningless and plugin fits would be misleading.
    if ('signal_type' %in% names(raw)) {
      sig_types <- sort(unique(na.omit(raw$signal_type)))
      sig_types <- sig_types[nzchar(sig_types)]
      if (length(sig_types) > 1)
        validate(need(FALSE, paste0(
          "Incompatible measurement types selected: ",
          paste(sig_types, collapse = " + "), ". ",
          "Cannot combine ", paste(sig_types, collapse = ", "),
          " data in the same analysis — the y-axes are dimensionally incompatible. ",
          "Please select datasets that share the same signal type.")))
    }

    bm           <- input$comp_curves_baseline %||% 'none'
    blank_names  <- input$comp_upload_blanks %||% character(0)
    blank_ids_db <- if (bm == 'blank_wells')
      tryCatch(qres_get_blank_wells(con, comp_active_ids()), error = function(e) character(0))
    else character(0)
    raw  <- qres_prepare_curves(raw, method = bm,
                                blank_names = blank_names, blank_ids = blank_ids_db)
    if (is.null(raw) || nrow(raw) == 0) return(raw)
    # Attach clean plate labels (sheet names or "Plate N") so the plot can
    # colour by plate even after the reactive result is cached by bindCache.
    lbls <- comp_plate_labels_rv()
    if ('plate_id' %in% names(raw)) {
      mapped <- if (!is.null(lbls)) lbls[raw$plate_id] else rep(NA_character_, nrow(raw))
      raw$plate_label <- ifelse(is.na(mapped),
                                 sub('.*__plate_', 'Plate ', raw$plate_id),
                                 as.character(mapped))
    }
    raw
  })

  # ── Shared 5-question title (Q1 organism · Q2 condition · Q3 measurement ·
  # Q4 preprocessing · Q5 observation unit) used by both curves and boxplot. ──
  comp_plot_labels <- reactive({
    raw  <- tryCatch(comp_curves_data(), error = function(e) NULL)
    meta <- tryCatch({
      m <- comp_filtered_rv()
      if (nrow(m) > 0 && 'dataset_id' %in% names(m))
        m[m$dataset_id %in% comp_active_ids(), , drop = FALSE]
      else m
    }, error = function(e) data.frame())
    .dv <- function(col) {
      if (!col %in% names(meta)) return(character(0))
      v <- sort(unique(na.omit(as.character(meta[[col]]))))
      v[nzchar(v) & v != 'NA']
    }
    abbrev_sp <- function(s) sub("^(\\S)\\S+ (\\S+).*$", "\\1. \\2", s)
    sp_vec <- .dv('species')
    sp_lbl <- switch(as.character(length(sp_vec)), "0" = NULL, "1" = abbrev_sp(sp_vec),
                     paste(sapply(sp_vec, abbrev_sp), collapse = ', '))
    med_vec <- .dv('medium_type')
    med_lbl <- switch(as.character(length(med_vec)), "0" = NULL, "1" = med_vec,
                      paste0(length(med_vec), ' media'))
    tc_vec  <- suppressWarnings(sort(as.numeric(.dv('temperature_c'))))
    tc_vec  <- tc_vec[is.finite(tc_vec)]
    tc_lbl  <- if (length(tc_vec) == 0) NULL else if (length(tc_vec) == 1) paste0(tc_vec, '°C')
               else paste0(min(tc_vec), '–', max(tc_vec), '°C')
    pt_vec  <- .dv('perturbation_type')
    pt_vec  <- pt_vec[!pt_vec %in% c('none', 'temperature', '')]
    pt_lbl  <- if (length(pt_vec) == 0) NULL else paste(unique(pt_vec), collapse = '/')
    title_parts <- Filter(Negate(is.null), list(sp_lbl, med_lbl, tc_lbl, pt_lbl))
    plot_title  <- if (length(title_parts) > 0) paste(title_parts, collapse = ' · ') else NULL
    sig_types  <- if (!is.null(raw)) sort(unique(na.omit(raw$signal_type))) else character(0)
    meas_units <- if (!is.null(raw) && 'measurement_unit' %in% names(raw) &&
                      any(!is.na(raw$measurement_unit)))
                    sort(unique(na.omit(raw$measurement_unit)))
                  else .dv('measurement_unit')
    meas_lbl <- if (length(sig_types) > 0 && length(meas_units) > 0)
                  paste0(sig_types[1], ' [', meas_units[1], ']')
                else if (length(sig_types) > 0) sig_types[1] else NULL
    bm_used  <- input$comp_curves_baseline %||% 'none'
    prep_lbl <- switch(bm_used,
      none='no baseline correction', min_well='min-per-well subtraction',
      first3='first-3-timepoints baseline', blank_wells='blank-well subtraction', bm_used)
    one_line <- if (length(comp_replicates_rv()) > 0) '1 line = 1 replicate group'
                else '1 line = 1 well'
    sub_parts <- Filter(Negate(is.null), list(meas_lbl, prep_lbl, one_line))
    list(title    = plot_title,
         subtitle = paste(sub_parts, collapse = ' · '))
  })

  output$comp_curves <- renderPlot({
    raw           <- comp_curves_data()
    ctrl_well_ids <- raw$well_id[raw$well %in% (input$comp_upload_controls %||% character(0))]
    multiplate  <- 'plate_label' %in% names(raw) && length(unique(raw$plate_label)) > 1L
    per_plate   <- multiplate && (input$comp_plate_view %||% 'together') == 'per_plate'
    colour_col  <- if (multiplate && !per_plate) 'plate_label'
                   else if ('plot_label' %in% names(raw)) 'plot_label'
                   else 'dataset_id'
    lbl           <- comp_plot_labels()
    plot_title    <- lbl$title
    plot_subtitle <- lbl$subtitle

    p <- qres_plot_growth(raw, log_y = input$comp_log_y %||% FALSE,
                          colour_by     = colour_col,
                          control_wells = ctrl_well_ids,
                          rep_groups    = comp_replicates_rv(),
                          title         = plot_title)
    if (nzchar(plot_subtitle))
      p <- p + ggplot2::labs(subtitle = plot_subtitle)
    if (per_plate) {
      lbls <- comp_plate_labels_rv()
      p <- p + ggplot2::facet_wrap(
        ~ plate_id, ncol = 2,
        labeller = if (!is.null(lbls))
          ggplot2::labeller(plate_id = lbls)
        else ggplot2::label_value)
    }
    p
  }) |> bindCache(comp_curves_data(), input$comp_log_y, input$comp_curves_baseline,
                  input$comp_upload_controls, input$comp_plate_view,
                  comp_upload_n_plates(), length(comp_replicates_rv()))

  # Hover tooltip: show well ID. Uses comp_curves_data() so coordinates match the plot.
  output$comp_curves_tooltip <- renderUI({
    hov <- input$comp_curves_hover
    if (is.null(hov)) return(NULL)
    raw <- comp_curves_data()
    if (is.null(raw) || nrow(raw) == 0) return(NULL)
    near <- tryCatch(
      nearPoints(raw, hov, xvar = 'time_h', yvar = 'value',
                 maxpoints = 1, threshold = 15),
      error = function(e) data.frame())
    if (nrow(near) == 0) return(NULL)
    lbl <- if ('well_id' %in% names(near)) near$well_id[1] else near$well[1]
    px <- hov$coords_css$x %||% (hov$x %||% 0)
    py <- hov$coords_css$y %||% (hov$y %||% 0)
    tags$div(
      style = sprintf(
        'position:absolute; left:%fpx; top:%fpx; background:rgba(40,40,40,.82);
         color:#fff; padding:3px 7px; border-radius:4px; font-size:11px;
         pointer-events:none; z-index:100; white-space:nowrap;',
        px + 10, py - 20),
      lbl
    )
  })

  # ── Tool benchmarking tab UI ─────────────────────────────────────────────────
  output$comp_comparison_ui <- renderUI({
    ids    <- comp_active_ids()
    has_up <- comp_has_upload()
    if (length(ids) == 0 && !has_up)
      return(tags$div(style = 'color:dimgray; padding:30px; text-align:center;',
        icon('arrow-left'), ' Select datasets or upload a file to run tool benchmarking.'))
    tagList(
      tags$div(
        tags$b('Baseline subtraction', style = 'font-size:13px;'),
        tags$small(style = 'color:dimgray; margin-left:6px;',
          '— Sprouffske & Wagner (2012), Hall et al. (2014), gcplyr / Mira et al. (2022)'),
        br(),
        radioButtons('comp_baseline', NULL, inline = TRUE,
          choices  = c('None' = 'none', 'Min per well' = 'min_well',
                       'First 3 tp' = 'first_3', 'Blank wells' = 'blank_wells'),
          selected = 'none'),
        uiOutput('comp_blank_note')
      ),
      hr(style = 'margin: 6px 0;'),
      fluidRow(
        column(4,
          selectInput('comp_metric', 'Metric',
            choices  = c('doubling_time_h', 'mumax_percapita', 'lag_time_h',
                         'auc_trapezoid', 'max_od', 'delta_od', 'max_death_rate'),
            selected = 'doubling_time_h')),
        column(8,
          checkboxGroupInput('comp_tools', 'Tools',
            choices  = c('model_free', 'gcplyr', 'growthrates',
                         'DGrowthR' = 'dgrothr', 'KinBiont' = 'kinbiont'),
            selected = c('model_free', 'gcplyr', 'growthrates'),
            inline   = TRUE),
          tags$small(style = 'color:dimgray;',
            icon('hourglass-half'),
            ' DGrowthR (GP) and KinBiont (Julia) are slow — 30 s–3 min per dataset.'))
      ),
      withSpinner(plotOutput('comp_boxplot', height = '420px'), type = 6, color = '#5b9bd5'),
      fluidRow(column(2, checkboxInput('comp_boxplot_log_y', 'Log y (plot)', FALSE))),
      br(),
      fluidRow(
        column(12,
          tags$b('Fitted parameters — all wells', style = 'font-size:13px;'),
          tags$span(style = 'margin-left:10px;',
            downloadButton('comp_dl_csv', 'Download CSV', class = 'btn-sm btn-default')),
          tags$span(style = 'margin-left:10px;',
            actionButton('send_comp_email', icon('envelope'),
              class = 'btn-sm btn-default', title = 'Email results')),
          uiOutput('comp_email_ui')
        )
      ),
      br(),
      withSpinner(DT::DTOutput('comp_params_table'), type = 6, color = '#5b9bd5')
    )
  })

  output$comp_blank_note <- renderUI({
    req(input$comp_baseline == 'blank_wells')
    ids  <- comp_active_ids()
    bids <- if (length(ids) > 0)
      tryCatch(qres_get_blank_wells(con, ids), error = function(e) character(0))
    else character(0)
    bids <- c(bids, input$comp_upload_blanks %||% character(0))
    if (length(bids) > 0)
      tags$small(style = 'color:#27ae60;',
        icon('check'), sprintf(' %d blank well(s) found.', length(bids)))
    else
      tags$small(style = 'color:#e74c3c;', icon('exclamation-triangle'),
        ' No blank wells designated in these datasets. Select a different method.')
  })

  output$comp_boxplot <- renderPlot({
    combined <- comp_metrics_long()
    metric   <- input$comp_metric %||% 'doubling_time_h'
    if (is.null(combined) || nrow(combined) == 0) {
      return(ggplot2::ggplot() +
        ggplot2::annotate('text', x = .5, y = .5, size = 5, colour = 'grey50',
          label = 'No metrics computed yet.') +
        ggplot2::theme_void())
    }
    facet_col <- if ('plot_label' %in% names(combined)) 'plot_label' else 'dataset_id'
    qres_plot_metric_comparison(combined, metric = metric,
      facet_by      = if (length(unique(combined[[facet_col]])) > 1) facet_col else NULL,
      log_y         = isTRUE(input$comp_boxplot_log_y),
      context_title = comp_plot_labels()$title)
  }) |> bindCache(comp_active_ids(), comp_has_upload(),
                  input$comp_tools, input$comp_baseline,
                  input$comp_rep_agg_mode, length(comp_replicates_rv()),
                  input$comp_metric, input$comp_boxplot_log_y)

  output$comp_params_table <- DT::renderDT({
    wide <- .comp_wide_analyse()
    if (nrow(wide) == 0)
      return(DT::datatable(data.frame(Note = 'No results yet.'),
                           rownames = FALSE, options = list(dom = 't')))
    num_cols <- names(wide)[sapply(wide, is.numeric)]
    DT::datatable(wide, rownames = FALSE, filter = 'top',
      extensions = 'RowGroup',
      options = list(
        pageLength  = 50, scrollX = TRUE,
        rowGroup    = list(dataSrc = which(names(wide) == 'dataset_id') - 1L),
        columnDefs  = list(list(visible = FALSE,
                                targets = which(names(wide) == 'dataset_id') - 1L)),
        dom = 'frtip'
      )) |> DT::formatRound(num_cols, digits = 4)
  }, server = TRUE)

  output$comp_dl_csv <- downloadHandler(
    filename = function() paste0('comparison_', format(Sys.time(), '%Y%m%d_%H%M%S'), '.csv'),
    content  = function(file) utils::write.csv(.comp_wide_analyse(), file, row.names = FALSE)
  )

  # ── Email results ───────────────────────────────────────────────────────────
  output$comp_email_status <- renderText(comp_email_status())

  output$comp_email_ui <- renderUI({
    msg <- comp_email_status()
    if (!nzchar(msg)) return(NULL)
    tags$div(style = 'margin-top:8px; max-width:400px;',
      tags$p(style = 'color:#555; font-size:12px; margin:0 0 4px;', msg),
      textInput('comp_email_addr', NULL,
        value       = Sys.getenv('PHAMIG_ADMIN_EMAIL', unset = ''),
        placeholder = 'your@email.com')
    )
  })

  observeEvent(input$send_comp_email, {
    comp_email_status('Enter address above and click again to send.')
    addr <- trimws(input$comp_email_addr %||% '')
    if (!nzchar(addr)) return()
    df <- tryCatch(comp_metrics_long(), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) { comp_email_status('No results to send.'); return() }
    comp_email_status('Sending…')
    tryCatch({
      box_png  <- tempfile(fileext = '.png')
      csv_path <- tempfile(fileext = '.csv')
      facet_col <- if ('plot_label' %in% names(df)) 'plot_label' else 'dataset_id'
      ggplot2::ggsave(box_png,
        plot  = qres_plot_metric_comparison(df, metric = input$comp_metric %||% 'doubling_time_h',
          facet_by = if (length(unique(df[[facet_col]])) > 1) facet_col else NULL),
        width = 8, height = 5, dpi = 120)
      utils::write.csv(.comp_wide_analyse(), csv_path, row.names = FALSE)
      phamig_send_comparison_email(
        to = addr, metric = input$comp_metric %||% 'doubling_time_h',
        n_datasets = length(unique(df$dataset_id)),
        attachments = list(box_png, csv_path))
      comp_email_status(paste0('Sent to ', addr, '.'))
    }, error = function(e) comp_email_status(paste0('Failed: ', conditionMessage(e))))
  })

  # ── Reset tool checkboxes when navigating away from the Analyse route ────────
  observeEvent(get_page(), {
    if (!is_page('analyse')) {
      comp_email_status('')
    }
  }, ignoreInit = TRUE)

  # ── Metadata template generator ────────────────────────────────────────────
  .plate_cols <- function(size) switch(size, '8x12'=12, '16x24'=24, '6x8'=8, '4x6'=6, 12)
  .plate_rows <- function(size) switch(size, '8x12'=8,  '16x24'=16, '6x8'=6, '4x6'=4,  8)

  .extract_num <- function(txt) {
    m <- regmatches(txt, regexpr('[0-9]+(?:\\.[0-9]*)?', txt, perl = TRUE))
    if (length(m)) as.numeric(m) else NA_real_
  }

  .replace_num <- function(txt, val) sub('[0-9]+(?:\\.[0-9]*)?', val, txt, perl = TRUE)

  plateMatrix        <- reactiveVal(data.frame())
  show_optional_meta <- reactiveVal(FALSE)
  well_edit_open     <- reactiveVal(NULL)   # well name currently in detail tab

  # Shared helper: builds a fresh well list for the current plate size
  .blank_plate <- function(size) {
    nc <- .plate_cols(size); nr <- .plate_rows(size)
    row_letters <- if (nr <= 26) LETTERS[seq_len(nr)] else
      c(LETTERS, paste0('A', LETTERS))[seq_len(nr)]
    wells <- as.vector(t(outer(row_letters, seq_len(nc), paste0)))
    data.frame(
      well               = wells,
      perturbation_type  = NA_character_,
      perturbation       = NA_character_,
      concentration      = NA_real_,
      concentration_unit = NA_character_,
      taxon_id           = NA_character_,
      medium_state       = 'liquid',
      is_blank           = FALSE,
      is_control         = FALSE,
      replicate          = 1L,
      stringsAsFactors   = FALSE)
  }

  # Shared helper: apply form inputs onto a plate data.frame
  .apply_meta_inputs <- function(plate, input) {
    nc <- .plate_cols(input$plateSize); nr <- .plate_rows(input$plateSize)
    ptype      <- input$meta_perturb_type
    fill_order <- input$meta_fill_order %||% 'row'
    plate$perturbation_type <- ptype

    # Sort eligible well indices into column-major order (A1,B1,…,H1,A2,…)
    # when the user selected column-by-column pipetting direction.
    .reorder_elig <- function(elig) {
      if (fill_order != 'col' || length(elig) == 0) return(elig)
      wn  <- plate$well[elig]
      col <- as.integer(sub('[A-Za-z]+', '', wn))
      row <- match(sub('[0-9]+', '', toupper(wn)), LETTERS)
      elig[order(col, row)]
    }

    # Mark blank / control wells first so eligibility logic below can skip them
    blanks_pre <- .parse_wells(input$meta_blank_wells)
    if (length(blanks_pre) > 0)
      plate$is_blank[toupper(plate$well) %in% blanks_pre] <- TRUE

    ctrls_pre <- .parse_wells(input$meta_control_wells)
    if (length(ctrls_pre) > 0)
      plate$is_control[toupper(plate$well) %in% ctrls_pre] <- TRUE

    if (ptype == 'chemical') {
      plate$perturbation       <- input$meta_perturb
      plate$concentration_unit <- trimws(input$meta_conc_unit %||% '')
      if (isTRUE(input$meta_dilution) && nzchar(input$meta_ratio)) {
        ratio <- as.numeric(strsplit(input$meta_ratio, ':')[[1]])
        a <- ratio[1]; b <- ratio[2]; start <- .extract_num(input$meta_conc)
        for (j in seq_len(nr)) for (i in seq_len(nc))
          plate[nc*(j-1)+i, 'concentration'] <- start * (a/b)^(i-1)
      } else {
        plate$concentration <- .extract_num(input$meta_conc)
      }
    } else if (ptype == 'genetic') {
      muts <- trimws(strsplit(input$meta_genetic_bg %||% '', ',')[[1]])
      muts <- muts[nzchar(muts)]
      if (length(muts) > 0) {
        eligible <- .reorder_elig(which(!plate$is_blank & !plate$is_control))
        if (length(eligible) > 0)
          plate$perturbation[eligible] <-
            muts[((seq_along(eligible) - 1L) %% length(muts)) + 1L]
      }
      plate$concentration_unit <- trimws(input$meta_gen_conc_unit %||% '')
      if (isTRUE(input$meta_gen_dilution) && nzchar(input$meta_gen_ratio %||% '')) {
        ratio <- as.numeric(strsplit(input$meta_gen_ratio, ':')[[1]])
        a <- ratio[1]; b <- ratio[2]; start <- .extract_num(input$meta_gen_conc)
        for (j in seq_len(nr)) for (i in seq_len(nc))
          plate[nc*(j-1)+i, 'concentration'] <- start * (a/b)^(i-1)
      } else if (nzchar(trimws(input$meta_gen_conc %||% ''))) {
        plate$concentration <- .extract_num(input$meta_gen_conc)
      }

    } else if (ptype == 'chemical_genetic') {
      drug <- trimws(input$meta_cg_perturb %||% '')
      muts <- trimws(strsplit(input$meta_cg_genetic_bg %||% '', ',')[[1]])
      muts <- muts[nzchar(muts)]

      # Chemical concentration per well (as character for semicolon combining)
      chem_v <- rep(NA_character_, nrow(plate))
      if (isTRUE(input$meta_cg_chem_dilution) && nzchar(input$meta_cg_chem_ratio %||% '')) {
        ratio <- as.numeric(strsplit(input$meta_cg_chem_ratio, ':')[[1]])
        a <- ratio[1]; b <- ratio[2]; start <- .extract_num(input$meta_cg_chem_conc)
        for (j in seq_len(nr)) for (i in seq_len(nc))
          chem_v[nc*(j-1)+i] <- as.character(start * (a/b)^(i-1))
      } else if (nzchar(trimws(input$meta_cg_chem_conc %||% ''))) {
        chem_v[] <- as.character(.extract_num(input$meta_cg_chem_conc))
      }

      # Inoculum per well
      inoc_v <- rep(NA_character_, nrow(plate))
      if (isTRUE(input$meta_cg_inoc_dilution) && nzchar(input$meta_cg_inoc_ratio %||% '')) {
        ratio <- as.numeric(strsplit(input$meta_cg_inoc_ratio, ':')[[1]])
        a <- ratio[1]; b <- ratio[2]; start <- .extract_num(input$meta_cg_inoc_conc)
        for (j in seq_len(nr)) for (i in seq_len(nc))
          inoc_v[nc*(j-1)+i] <- as.character(start * (a/b)^(i-1))
      } else if (nzchar(trimws(input$meta_cg_inoc_conc %||% ''))) {
        inoc_v[] <- as.character(.extract_num(input$meta_cg_inoc_conc))
      }

      # Blank wells → "0;0", control wells → keep chem conc, zero inoculum
      blank_cg <- which(plate$is_blank)
      if (length(blank_cg) > 0) { chem_v[blank_cg] <- '0'; inoc_v[blank_cg] <- '0' }
      ctrl_cg <- which(plate$is_control)
      if (length(ctrl_cg) > 0) inoc_v[ctrl_cg] <- '0'

      # Combine: "chem;inoc" in concentration column
      plate$concentration      <- paste0(ifelse(is.na(chem_v), '', chem_v),
                                         ';',
                                         ifelse(is.na(inoc_v), '', inoc_v))
      plate$concentration_unit <- paste0(trimws(input$meta_cg_chem_unit %||% ''),
                                         ';',
                                         trimws(input$meta_cg_inoc_unit %||% ''))

      # Perturbation: "drug" for all wells; "drug;mutation" for eligible wells
      if (nzchar(drug)) plate$perturbation <- drug
      eligible <- .reorder_elig(which(!plate$is_blank & !plate$is_control))
      if (length(muts) > 0 && length(eligible) > 0) {
        well_muts <- muts[((seq_along(eligible) - 1L) %% length(muts)) + 1L]
        plate$perturbation[eligible] <- if (nzchar(drug))
          paste0(drug, ';', well_muts) else well_muts
      }

    } else if (ptype == 'temperature') {
      # Temperature IS the perturbation — no chemical concentration
      plate$concentration <- 0

      # Fill temperatures only into non-blank, non-control wells, in pipetting order
      eligible <- .reorder_elig(which(!plate$is_blank & !plate$is_control))
      n_elig   <- length(eligible)

      if (n_elig > 0) {
        start_t <- .extract_num(input$meta_temp_start)
        end_t   <- if (nzchar(trimws(input$meta_temp_end  %||% ''))) .extract_num(input$meta_temp_end)  else NA_real_
        step_t  <- if (nzchar(trimws(input$meta_temp_step %||% ''))) .extract_num(input$meta_temp_step) else NA_real_
        if (is.na(step_t))
          step_t <- if (!is.na(end_t) && n_elig > 1) (end_t - start_t) / (n_elig - 1) else 0

        temps <- start_t + step_t * (seq_len(n_elig) - 1)
        plate$perturbation[eligible] <- as.character(temps)

        # Optionally mark a specific temperature as control within the gradient
        ctrl_t <- if (nzchar(trimws(input$meta_temp_control %||% ''))) .extract_num(input$meta_temp_control) else NA_real_
        if (!is.na(ctrl_t)) {
          ctrl_in_elig <- eligible[abs(temps - ctrl_t) < 0.5]
          if (length(ctrl_in_elig) > 0) plate$is_control[ctrl_in_elig] <- TRUE
        }
      }
    }

    # Optional fields (only if panel is open and inputs exist)
    if (!is.null(input$meta_medium_state))
      plate$medium_state <- input$meta_medium_state
    # Replicate sequence: cycle across all wells in pipetting order
    rep_labels <- .parse_replicate_seq(input$meta_replicate_seq %||% '')
    if (length(rep_labels) > 0) {
      eligible <- .reorder_elig(which(!plate$is_blank & !plate$is_control))
      if (length(eligible) > 0)
        plate$replicate[eligible] <-
          rep_labels[((seq_along(eligible) - 1L) %% length(rep_labels)) + 1L]
    }
    if (!is.null(input$meta_taxon_id) && nzchar(trimws(input$meta_taxon_id)))
      plate$taxon_id <- trimws(input$meta_taxon_id)

    # Final overrides — applied after type-specific logic
    # Use the pre-parsed well lists (not is_blank/is_control) so that temperature-matched
    # control wells (is_control=TRUE but with a real temperature value) are not overwritten.
    if (ptype != 'chemical_genetic') {
      if (length(blanks_pre) > 0) {
        bi <- toupper(plate$well) %in% blanks_pre
        plate$concentration[bi] <- 0
      }
      if (length(ctrls_pre) > 0) {
        ci <- toupper(plate$well) %in% ctrls_pre
        plate$concentration[ci] <- 0
        plate$perturbation[ci]  <- 'none'
      }
    }

    plate
  }

  observeEvent(input$plateSize, {
    plateMatrix(.blank_plate(input$plateSize))
  })

  observeEvent(input$applyMetadata, {
    plateMatrix(.apply_meta_inputs(.blank_plate(input$plateSize), input))
  })

  observeEvent(input$update_meta, {
    plateMatrix(.apply_meta_inputs(plateMatrix(), input))
  })

  observeEvent(input$clear_meta, {
    plateMatrix(.blank_plate(input$plateSize))
  })

  observeEvent(input$toggle_optional, {
    show_optional_meta(!show_optional_meta())
  })

  # ── Well detail tab (double-click on plate row) ────────────────────────────
  observeEvent(input$plate_row_dblclick, {
    idx   <- input$plate_row_dblclick$row
    plate <- plateMatrix()
    if (is.null(idx) || idx < 1 || idx > nrow(plate)) return()
    row       <- plate[idx, ]
    well_name <- row$well
    well_edit_open(well_name)

    conc_str <- if (is.na(row$concentration)) '' else as.character(row$concentration)

    tab_ui <- tagList(
      br(),
      tags$h5(tags$strong(well_name), ' — manual override'),
      helpText('Values entered here replace what Apply / Update set for this specific well.'),
      tags$hr(style = 'margin:6px 0 12px;'),
      fluidRow(
        column(6,
          selectInput('wd_perturb_type', 'Perturbation type',
            choices  = c('chemical', 'genetic', 'chemical_genetic', 'temperature', 'none'),
            selected = row$perturbation_type %||% 'none'),
          textInput('wd_perturbation', 'Perturbation / name',
            value = as.character(row$perturbation %||% '')),
          textInput('wd_concentration', 'Concentration',
            placeholder = 'numeric or chem;inoc for combined', value = conc_str),
          textInput('wd_conc_unit', 'Concentration unit',
            value = as.character(row$concentration_unit %||% ''))
        ),
        column(6,
          selectInput('wd_medium_state', 'Medium state',
            choices  = c('liquid', 'solid', 'semi-solid'),
            selected = row$medium_state %||% 'liquid'),
          numericInput('wd_replicate', 'Replicate',
            value = as.integer(row$replicate %||% 1L), min = 1L, step = 1L),
          textInput('wd_taxon_id', 'Taxon ID',
            value = as.character(row$taxon_id %||% '')),
          br(),
          checkboxInput('wd_is_blank',   'Blank well',   value = isTRUE(row$is_blank)),
          checkboxInput('wd_is_control', 'Control well', value = isTRUE(row$is_control))
        )
      ),
      tags$hr(style = 'margin:8px 0;'),
      fluidRow(
        column(2, actionButton('well_detail_ok',     'OK',     class = 'btn-primary')),
        column(2, actionButton('well_detail_cancel', 'Cancel', class = 'btn-default'))
      ),
      br()
    )

    removeTab('meta_tabs', 'well_edit')
    appendTab('meta_tabs',
      tabPanel('Well detail', value = 'well_edit', tab_ui),
      select = TRUE)
  })

  observeEvent(input$well_detail_ok, {
    well_name <- well_edit_open()
    if (is.null(well_name)) return()
    plate <- plateMatrix()
    idx   <- which(plate$well == well_name)
    if (length(idx) == 0) return()

    plate$perturbation_type[idx]  <- input$wd_perturb_type  %||% 'none'
    plate$perturbation[idx]       <- trimws(input$wd_perturbation  %||% '')
    # concentration: keep as character so semicolon values (chemical_genetic) survive
    conc_raw <- trimws(input$wd_concentration %||% '')
    plate$concentration <- as.character(plate$concentration)
    plate$concentration[idx] <- if (nzchar(conc_raw)) conc_raw else NA_character_
    plate$concentration_unit[idx] <- trimws(input$wd_conc_unit %||% '')
    plate$medium_state[idx]  <- input$wd_medium_state %||% 'liquid'
    plate$replicate[idx]     <- as.integer(input$wd_replicate %||% 1L)
    plate$taxon_id[idx]      <- trimws(input$wd_taxon_id %||% '')
    plate$is_blank[idx]      <- isTRUE(input$wd_is_blank)
    plate$is_control[idx]    <- isTRUE(input$wd_is_control)

    plateMatrix(plate)
    removeTab('meta_tabs', 'well_edit')
    well_edit_open(NULL)
  })

  observeEvent(input$well_detail_cancel, {
    removeTab('meta_tabs', 'well_edit')
    well_edit_open(NULL)
  })

  output$optional_fields_ui <- renderUI({
    if (!show_optional_meta()) return(NULL)
    tagList(
      br(),
      selectInput('meta_medium_state',
        opt_label('Medium state',
          'Physical state of the growth medium. Liquid is the default for plate reader experiments.'),
        choices = c('liquid', 'solid', 'semi-solid'), selected = 'liquid'),
      textInput('meta_replicate_seq',
        opt_label('Replicate pattern',
          paste0(
            'Replicate numbers assigned to wells in reading order, skipping blanks and controls. ',
            'Three notations are accepted:\n',
            '  1:4           → cycle 1, 2, 3, 4, 1, 2, 3, 4, …\n',
            '  1, 2, 3, 4   → same as above\n',
            '  rep(1:4, 12) → R rep() expression — useful for explicit layouts.\n',
            'Examples:\n',
            '  rep(1:12, 8)         — columns as replicates (96-well, 8 rows each)\n',
            '  rep(1:4, each = 24)  — first 24 wells = rep 1, next 24 = rep 2, …\n',
            '  rep(c(1,2), 48)      — alternating 1-2 across 96 wells'
          )),
        placeholder = 'e.g. 1:4  or  rep(1:12, 8)'),
      textInput('meta_taxon_id',
        opt_label('NCBI Taxon ID',
          'Numeric identifier from NCBI Taxonomy. Example: 562 for Escherichia coli, 4932 for S. cerevisiae.'),
        placeholder = '562')
    )
  })

  output$plateMatrix <- renderDT({
    plate <- plateMatrix()
    if (nrow(plate) == 0) return(datatable(data.frame(message = 'Select plate format to start.')))

    # ── Smart column ordering and header logic ─────────────────────────────
    # Preferred display order: well → replicate → stressor → concentration →
    #   concentration_unit → genetic background → blank → control → rest
    col_order <- c('well', 'replicate', 'perturbation_type', 'perturbation',
                   'concentration', 'concentration_unit',
                   'taxon_id', 'medium_state', 'is_blank', 'is_control')
    present    <- intersect(col_order, names(plate))
    extra      <- setdiff(names(plate), present)
    plate      <- plate[, c(present, extra), drop = FALSE]

    # Drop columns that are entirely empty / default
    non_empty <- vapply(names(plate), function(cn) {
      col <- plate[[cn]]
      if (cn == 'well')     return(TRUE)
      if (cn == 'replicate') return(!all(is.na(col) | col == 1L | col == '1'))
      if (cn == 'is_blank' || cn == 'is_control') return(any(col == TRUE, na.rm = TRUE))
      if (cn == 'medium_state') return(!all(is.na(col) | col == 'liquid'))
      !all(is.na(col) | trimws(as.character(col)) %in% c('', 'NA', 'FALSE'))
    }, logical(1))
    plate <- plate[, non_empty, drop = FALSE]

    # Smart stressor-column rename:
    # If all non-blank perturbation values are the same stressor, rename the
    # concentration column header to that stressor name and drop perturbation col.
    conc_label <- 'Concentration'
    drop_perturb <- FALSE
    if ('perturbation' %in% names(plate) && 'concentration' %in% names(plate)) {
      non_blank_perturb <- plate$perturbation[is.na(plate$is_blank) | plate$is_blank != TRUE]
      non_blank_perturb <- non_blank_perturb[!is.na(non_blank_perturb) & nzchar(non_blank_perturb)]
      uniq_p <- unique(non_blank_perturb)
      if (length(uniq_p) == 1 && !grepl(';', uniq_p)) {
        conc_label   <- uniq_p   # e.g. "rifampicin"
        drop_perturb <- TRUE
      }
    }
    if (drop_perturb) {
      plate <- plate[, setdiff(names(plate), c('perturbation', 'perturbation_type')), drop = FALSE]
    }

    # Friendly column names
    col_labels <- c(
      well               = 'Well',
      replicate          = 'Replicate',
      perturbation_type  = 'Type',
      perturbation       = 'Stressor / mutation',
      concentration      = conc_label,
      concentration_unit = 'Unit',
      taxon_id           = 'Taxon ID',
      medium_state       = 'Medium state',
      is_blank           = 'Blank',
      is_control         = 'Control'
    )
    col_names <- unname(vapply(names(plate), function(n) col_labels[[n]] %||% n, character(1)))

    datatable(plate,
              colnames  = col_names,
              selection = 'none',
              options   = list(pageLength = 10, dom = 'lt', scrollX = TRUE,
                               lengthMenu = list(c(10, -1), c('10', 'All'))),
              callback  = JS("
                table.on('dblclick', 'tr', function() {
                  var idx = table.row(this).index();
                  if (idx === undefined || isNaN(idx)) return;
                  Shiny.setInputValue('plate_row_dblclick',
                    {row: idx + 1, ts: Date.now()}, {priority: 'event'});
                });
              "))
  })

  output$downloadTemplate <- downloadHandler(
    filename = function() paste0('phamig_template_', Sys.Date(), '.csv'),
    content  = function(file) write.csv(plateMatrix(), file, row.names = FALSE)
  )

  # ── Demo raw CSV modal ────────────────────────────────────────────────────
  .demo_raw_path <- 'data/raw_csv/raw_data_template.csv'

  observeEvent(input$show_release_conditions, {
    showModal(modalDialog(
      title = tagList(icon('lock'), ' Data access & release conditions'),
      size  = 'm',
      easyClose = TRUE,
      footer = modalButton('Close'),
      h5(icon('info-circle'), ' About data release modes'),
      tags$dl(
        tags$dt(icon('globe'), ' Immediate release'),
        tags$dd('Data is publicly searchable in PhaMiG as soon as it is registered. Use this for already-published datasets.'),
        br(),
        tags$dt(icon('calendar'), ' Set a release date'),
        tags$dd('Data remains private until the chosen date. PhaMiG automatically releases it at midnight on that date. Useful when you know your submission or conference date.'),
        br(),
        tags$dt(icon('file-alt'), ' bioRxiv preprint'),
        tags$dd('Data remains private while your work is a preprint. PhaMiG polls the bioRxiv API every month. When your preprint receives a journal DOI, the data is released automatically. You will need to provide the bioRxiv URL or DOI.'),
        br(),
        tags$dt(icon('trash'), ' Deleting your data'),
        tags$dd('You can delete your dataset at any time — before or after release — using your email + access key + passphrase. Save your access key immediately after upload: it is shown only once and is not recoverable.')
      ),
      hr(),
      h5(icon('shield-alt'), ' Privacy notes'),
      tags$ul(
        tags$li('Your passphrase is stored as a SHA-256 hash only — we never see the plain text.'),
        tags$li('Your email is stored in lowercase and is only visible to the database administrator.'),
        tags$li('The access key is a 32-character random token. Write it down or copy it immediately.'),
        tags$li('Embargoed datasets are completely hidden from all search results, analysis tabs, and downloads.')
      )
    ))
  })

  observeEvent(input$show_demo_raw, {
    df <- tryCatch(
      read.csv(.demo_raw_path, check.names = FALSE, stringsAsFactors = FALSE),
      error = function(e) NULL)

    showModal(modalDialog(
      title = tagList(icon('table'), ' Raw data CSV — format reference'),
      size  = 'l',
      easyClose = TRUE,
      footer = tagList(
        downloadButton('download_demo_raw', 'Download template', class = 'btn-success btn-sm'),
        modalButton('Close')
      ),

      tags$div(class = 'alert alert-info', style = 'font-size:13px; padding:8px 12px; margin-bottom:10px;',
        tags$strong('Required format:'), ' First column = ', tags$code('Time'),
        ' (HH:MM:SS or decimal hours). Remaining columns = well names ',
        tags$code('A1'), ', ', tags$code('A2'), ' … ', tags$code('H12'),
        ' containing OD values. One row per time point. At least 5 time points and 2 wells required.',
        tags$br(),
        tags$span(style = 'color:#31708f;',
          'Preview shows the first 5 rows and first 7 columns of a 96-well template (',
          if (!is.null(df)) paste0(nrow(df), ' time points, ', ncol(df) - 1, ' wells') else '?',
          ').')
      ),

      if (is.null(df)) {
        tags$p(class = 'text-danger', 'Template file not found.')
      } else {
        preview <- df[seq_len(min(5, nrow(df))), seq_len(min(7, ncol(df))), drop = FALSE]
        tableOutput('demo_raw_table')
      }
    ))

    if (!is.null(df)) {
      preview <- df[seq_len(min(5, nrow(df))), seq_len(min(7, ncol(df))), drop = FALSE]
      output$demo_raw_table <- renderTable(preview, striped = TRUE, bordered = TRUE,
                                           spacing = 'xs', align = 'c')
    }
  })

  output$download_demo_raw <- downloadHandler(
    filename = 'raw_data_template.csv',
    content  = function(file) file.copy(.demo_raw_path, file)
  )

  # ── Data upload ────────────────────────────────────────────────────────────
  # Reactive to hold the access key from last upload (shown once, not stored plain-text)
  last_access_key <- reactiveVal(NULL)

  output$upload_progress_ui <- renderUI(NULL)  # cleared until upload starts

  output$upload_key_display <- renderUI({
    key_info <- last_access_key()
    if (is.null(key_info)) return(NULL)
    tags$div(class = 'access-key-box',
      tags$strong(icon('key'), ' Save your access key — shown only once!'),
      tags$br(), tags$br(),
      tags$div(class = 'key-value', key_info$access_key),
      tags$br(),
      tags$p(style = 'color:#555; font-size:13px;',
        'Dataset: ', tags$code(key_info$dataset_id), tags$br(),
        'Email:   ', key_info$email, tags$br(),
        'You need all three (email + this key + your passphrase) to delete this dataset.'
      ),
      tags$hr(style = 'margin:8px 0;'),
      tags$p(style = 'color:#b71c1c; font-size:12px; margin:0;',
        icon('exclamation-triangle'),
        ' This key is not stored in recoverable form. Copy it now.')
    )
  })

  # ── Procedure text paste → auto-fill instrument fields ─────────────────────
  observeEvent(input$upload_procedure_extract, {
    txt <- trimws(input$upload_procedure_paste %||% '')
    if (!nzchar(txt)) {
      output$upload_procedure_extract_status <- renderUI(
        tags$small(style = 'color:#e67e22;', icon('triangle-exclamation'), ' Paste text first.'))
      return()
    }
    meta <- tryCatch(qres_parse_procedure_text(txt),
                     error = function(e) list())
    if (length(meta) == 0) {
      output$upload_procedure_extract_status <- renderUI(
        tags$small(style = 'color:#e74c3c;', icon('times'), ' No fields recognised.'))
      return()
    }
    filled <- character(0)
    if (!is.null(meta$plate_type) && nzchar(meta$plate_type)) {
      updateTextInput(session, 'upload_plate_reader', value = meta$plate_type)
      filled <- c(filled, 'Plate reader')
    }
    if (!is.null(meta$wavelength_nm) && !is.na(meta$wavelength_nm)) {
      updateNumericInput(session, 'upload_wavelength', value = meta$wavelength_nm)
      filled <- c(filled, 'Wavelength')
    }
    if (!is.null(meta$shaking_rpm) && !is.na(meta$shaking_rpm)) {
      updateNumericInput(session, 'upload_shake_rpm', value = meta$shaking_rpm)
      filled <- c(filled, 'Shaking rpm')
    }
    if (!is.null(meta$temperature_c) && !is.na(meta$temperature_c)) {
      updateTextInput(session, 'upload_temp_c', value = as.character(meta$temperature_c))
      filled <- c(filled, 'Temperature')
    }
    if (length(filled)) {
      output$upload_procedure_extract_status <- renderUI(
        tags$small(style = 'color:#27ae60;', icon('check'),
          paste(' Filled:', paste(filled, collapse = ', '))))
    } else {
      output$upload_procedure_extract_status <- renderUI(
        tags$small(style = 'color:#e67e22;', icon('triangle-exclamation'),
          ' Text parsed but no matching fields found.'))
    }
  })

  observeEvent(input$run_upload, {
    req(input$upload_raw_csv)

    dataset_id    <- trimws(input$upload_dataset_id  %||% '')
    species       <- trimws(input$upload_species      %||% '')
    medium        <- trimws(input$upload_medium       %||% '')
    temp_c        <- suppressWarnings(as.numeric(input$upload_temp_c %||% NA))
    email         <- trimws(tolower(input$upload_email        %||% ''))
    passphrase    <- input$upload_passphrase  %||% ''
    passphrase2   <- input$upload_passphrase2 %||% ''
    release_mode  <- input$upload_release_mode %||% 'immediate'
    release_date  <- if (!is.null(input$upload_release_date)) as.character(input$upload_release_date) else NA
    biorxiv_url   <- trimws(input$upload_biorxiv_url %||% '')

    # Clear previous key display when a new upload starts
    last_access_key(NULL)

    errs <- character(0)
    if (!nzchar(dataset_id))  errs <- c(errs, 'Dataset ID is required.')
    if (!nzchar(species))     errs <- c(errs, 'Species is required.')
    if (!nzchar(medium))      errs <- c(errs, 'Medium type is required.')
    if (!is.finite(temp_c))   errs <- c(errs, 'Temperature (°C) must be a number.')

    # Embargo auth fields
    if (!nzchar(email) || !grepl('@', email))
      errs <- c(errs, 'A valid email address is required (used for dataset ownership).')
    if (nchar(passphrase) < 8)
      errs <- c(errs, 'Passphrase must be at least 8 characters.')
    if (!identical(passphrase, passphrase2))
      errs <- c(errs, 'The two passphrases do not match.')
    if (release_mode == 'date' && (is.na(release_date) || !nzchar(release_date %||% '')))
      errs <- c(errs, 'A release date is required when "Set a release date" is chosen.')
    if (release_mode == 'biorxiv' && !nzchar(biorxiv_url))
      errs <- c(errs, 'A bioRxiv URL or DOI is required for the bioRxiv release mode.')

    # dataset_id format
    if (nzchar(dataset_id)) {
      id_check <- tryCatch(.check_field('dataset_id', dataset_id), error = function(e) conditionMessage(e))
      if (length(id_check) > 0) errs <- c(errs, id_check)
    }

    if (length(errs) > 0) {
      output$upload_status <- renderText(
        paste('Please fix the following before uploading:\n', paste('✗', errs, collapse = '\n')))
      return()
    }

    raw_path <- tryCatch(
      qres_normalise_to_csv(input$upload_raw_csv$datapath),
      error = function(e) {
        output$upload_progress_ui <- renderUI(NULL)
        output$upload_status <- renderText(paste('Could not read file:', conditionMessage(e)))
        NULL
      })
    req(!is.null(raw_path))

    # Pre-flight: validate raw CSV/Excel format before any DB write
    output$upload_progress_ui <- renderUI(
      helpText(icon('spinner'), ' Validating raw file format…'))

    csv_check <- qres_validate_raw_csv(raw_path)
    if (!csv_check$ok) {
      output$upload_progress_ui <- renderUI(NULL)
      output$upload_status <- renderText(paste(
        'Raw CSV validation failed — fix these issues before uploading:\n',
        paste('✗', csv_check$issues, collapse = '\n')))
      return()
    }
    warn_msg <- if (length(csv_check$warnings) > 0)
      paste0('\n\nWarnings (non-blocking):\n', paste('⚠', csv_check$warnings, collapse = '\n'))
    else ''

    output$upload_progress_ui <- renderUI(
      helpText(icon('spinner'), ' Registering in database…'))

    tryCatch({
      qres_init_schema(con)

      # Duplicate check — allow re-upload only if needs_correction flag is set
      existing <- DBI::dbGetQuery(con, 'SELECT 1 FROM dataset WHERE dataset_id=?',
                                  params = list(dataset_id))
      if (nrow(existing) > 0) {
        correction_flag <- DBI::dbGetQuery(con,
          'SELECT needs_correction FROM upload_embargo WHERE dataset_id = ?',
          params = list(dataset_id))
        if (nrow(correction_flag) == 0 || !isTRUE(correction_flag$needs_correction[1])) {
          output$upload_progress_ui <- renderUI(NULL)
          output$upload_status <- renderText(paste0(
            'Dataset ID "', dataset_id, '" already exists in the database.\n',
            'Choose a different ID, or use "Request a correction" in the "Manage my data" tab to reopen it.'))
          return()
        }
      }

      # Build metadata row and import
      tmp_csv <- tempfile(fileext = '.csv')
      write.csv(data.frame(
        dataset_id        = dataset_id,
        raw_file          = input$upload_raw_csv$name,
        species           = species,
        medium_type       = medium,
        temperature_c     = temp_c,
        perturbation_type = 'none',
        strain            = trimws(input$upload_strain    %||% ''),
        taxon_id          = trimws(input$upload_taxon_id  %||% ''),
        medium_state      = 'liquid',
        plate_colour      = input$upload_plate_colour  %||% 'transparent',
        lid               = input$upload_lid           %||% 'TRUE',
        plate_reader      = trimws(input$upload_plate_reader      %||% ''),
        spectrophotometer = trimws(input$upload_spectrophotometer %||% ''),
        wavelength_nm     = suppressWarnings(as.numeric(input$upload_wavelength %||% 600)),
        shake_freq_rpm    = suppressWarnings(as.numeric(input$upload_shake_rpm  %||% NA)),
        time_unit         = input$upload_time_unit %||% 'h',
        authors           = trimws(input$upload_authors %||% ''),
        year              = suppressWarnings(as.integer(input$upload_year %||% NA)),
        doi               = trimws(input$upload_doi    %||% ''),
        notes             = trimws(input$upload_notes  %||% ''),
        stringsAsFactors  = FALSE
      ), tmp_csv, row.names = FALSE)

      qres_import_metadata(con, tmp_csv)

      # Register raw measurements → Parquet
      qres_register_raw_measurements(con, dataset_id = dataset_id, raw_csv = raw_path)

      # Optional plate template
      if (!is.null(input$upload_meta_csv)) {
        tryCatch(
          qres_import_plate_template(con, input$upload_meta_csv$datapath, dataset_id),
          error = function(e)
            showNotification(
              paste('Plate template warning:', conditionMessage(e)), type = 'warning'))
      }

      # Register embargo record
      is_correction <- {
        cf <- DBI::dbGetQuery(con,
          'SELECT needs_correction, access_key FROM upload_embargo WHERE dataset_id = ?',
          params = list(dataset_id))
        nrow(cf) > 0 && isTRUE(cf$needs_correction[1])
      }

      if (is_correction) {
        # Embargo record already exists — clear the correction flag, keep credentials
        DBI::dbExecute(con,
          "UPDATE upload_embargo SET needs_correction = FALSE WHERE dataset_id = ?",
          params = list(dataset_id))
        existing_key <- DBI::dbGetQuery(con,
          'SELECT access_key FROM upload_embargo WHERE dataset_id = ?',
          params = list(dataset_id))$access_key[1]
        last_access_key(NULL)   # key was already shown at original upload
        correction_note <- '\n(Correction re-upload — original access key and embargo settings preserved.)'
      } else {
        key_info <- qres_register_embargo(
          con, dataset_id, email, passphrase,
          release_mode = release_mode,
          release_date = if (release_mode == 'date') release_date else NA,
          biorxiv_url  = if (release_mode == 'biorxiv') biorxiv_url else NA,
          orcid        = trimws(input$upload_orcid %||% '')
        )
        last_access_key(list(
          access_key = key_info$access_key,
          dataset_id = dataset_id,
          email      = email
        ))
        correction_note <- ''
      }

      # Optional auto-analysis with model_free
      analysis_note <- ''
      if (isTRUE(input$upload_run_analysis)) {
        output$upload_progress_ui <- renderUI(
          helpText(icon('spinner'), ' Running model_free analysis…'))
        analysis_note <- tryCatch({
          raw_long <- qres_get_raw_curves(con, dataset_id)
          run_res  <- qres_plugin_model_free(raw_long)
          if (!is.null(run_res$derived_metrics) && nrow(run_res$derived_metrics) > 0) {
            dt_vals <- run_res$derived_metrics$metric_value[
              run_res$derived_metrics$metric_name == 'doubling_time_h']
            sprintf('\nQuick analysis done: median doubling time = %.2f h across %d wells.',
                    median(dt_vals, na.rm = TRUE),
                    length(unique(run_res$derived_metrics$well_id)))
          } else ''
        }, error = function(e) paste0('\nAuto-analysis failed: ', conditionMessage(e)))
      }

      output$upload_progress_ui <- renderUI(NULL)
      output$upload_status <- renderText(paste0(
        '✓ Registered successfully: ', dataset_id, '\n',
        'Species: ', species, ' | Medium: ', medium, ' | Temp: ', temp_c, '°C\n',
        'Release mode: ', release_mode,
        if (release_mode == 'date')    paste0(' (', release_date, ')') else '',
        if (release_mode == 'biorxiv') paste0(' (', substr(biorxiv_url, 1, 50), ')') else '',
        analysis_note, warn_msg, correction_note, '\n\n',
        if (!is_correction) '⚠ Copy the access key shown above before leaving this page!' else ''
      ))

      # Auto-refresh search results
      dataset_results(qres_search_datasets(con))

    }, error = function(e) {
      output$upload_progress_ui <- renderUI(NULL)
      msg <- conditionMessage(e)
      # Make validation errors human-readable
      msg <- sub('^Metadata validation failed:\n', 'Metadata problem:\n', msg)
      msg <- sub('^Raw CSV validation failed.*?\n', 'Raw data problem:\n', msg)
      output$upload_status <- renderText(paste('Upload failed:\n', msg))
    })
  })

  # ── Shared helper: refresh the "my datasets" table for a given email ────────
  .release_dot <- function(mode, needs_correction = FALSE) {
    col <- ifelse(needs_correction,    'goldenrod',
           ifelse(mode == 'immediate', 'darkolivegreen',
           ifelse(mode == 'biorxiv',   'salmon',
           ifelse(mode == 'date',      'firebrick', '#aaaaaa'))))
    sprintf('<span style="color:%s; font-size:15px; line-height:1;">&#9679;</span>', col)
  }

  .refresh_my_datasets <- function(email) {
    if (!nzchar(trimws(email %||% ''))) return()
    tryCatch({
      my_ds <- qres_my_datasets(con, email)
      keep  <- intersect(c('dataset_id', 'species', 'medium_type', 'temperature_c',
                            'release_mode', 'release_date', 'released', 'needs_correction',
                            'created_at'),
                         names(my_ds))
      tbl <- my_ds[, keep, drop = FALSE]
      nc  <- if ('needs_correction' %in% names(tbl)) isTRUE(tbl$needs_correction) else FALSE
      tbl <- cbind(` ` = .release_dot(tbl$release_mode, nc), tbl)
      output$my_datasets_table <- renderDT({
        datatable(tbl,
                  escape   = FALSE,
                  options  = list(pageLength = 10, scrollX = TRUE,
                                  columnDefs = list(list(width = '20px', targets = 0))),
                  rownames = FALSE)
      })
    }, error = function(e) NULL)
  }

  # ── Request a correction (reopen upload) ────────────────────────────────────
  observeEvent(input$run_reopen, {
    dataset_id <- trimws(input$reopen_dataset_id  %||% '')
    email      <- trimws(input$reopen_email       %||% '')
    access_key <- trimws(input$reopen_access_key  %||% '')
    passphrase <- input$reopen_passphrase %||% ''

    errs <- character(0)
    if (!nzchar(dataset_id)) errs <- c(errs, 'Dataset ID is required.')
    if (!nzchar(email))      errs <- c(errs, 'Email is required.')
    if (nchar(access_key) != 32) errs <- c(errs, 'Access key must be exactly 32 characters.')
    if (nchar(passphrase) < 8)   errs <- c(errs, 'Passphrase must be at least 8 characters.')

    if (length(errs) > 0) {
      output$reopen_status <- renderText(paste('✗', errs, collapse = '\n'))
      return()
    }

    result <- tryCatch(
      qres_reopen_dataset(con, dataset_id, email, access_key, passphrase),
      error = function(e) list(ok = FALSE, message = conditionMessage(e))
    )
    output$reopen_status <- renderText(
      if (result$ok) paste('✓', result$message)
      else           paste('✗', result$message)
    )
    if (isTRUE(result$ok)) {
      updateTextInput(session, 'status_email', value = email)
      .refresh_my_datasets(email)
      showNotification(
        tagList(icon('wrench'),
                ' Upload cleared — go to "Upload raw data" to re-submit.'),
        type     = 'message',
        duration = 8
      )
    }
  })

  # ── Status check (my datasets) ──────────────────────────────────────────────
  observeEvent(input$run_status_check, {
    .refresh_my_datasets(trimws(input$status_email %||% ''))
  })

  # ── Delete dataset ──────────────────────────────────────────────────────────
  observeEvent(input$run_delete, {
    dataset_id  <- trimws(input$del_dataset_id  %||% '')
    email       <- trimws(input$del_email       %||% '')
    access_key  <- trimws(input$del_access_key  %||% '')
    passphrase  <- input$del_passphrase %||% ''

    errs <- character(0)
    if (!nzchar(dataset_id)) errs <- c(errs, 'Dataset ID is required.')
    if (!nzchar(email))      errs <- c(errs, 'Email is required.')
    if (nchar(access_key) != 32) errs <- c(errs, 'Access key must be exactly 32 characters.')
    if (nchar(passphrase) < 8)   errs <- c(errs, 'Passphrase must be at least 8 characters.')

    if (length(errs) > 0) {
      output$delete_status <- renderText(paste('✗', errs, collapse = '\n'))
      return()
    }

    result <- tryCatch(
      qres_delete_dataset(con, dataset_id, email, access_key, passphrase),
      error = function(e) list(ok = FALSE, message = conditionMessage(e))
    )
    output$delete_status <- renderText(
      if (result$ok) paste('✓', result$message)
      else           paste('✗', result$message)
    )
    if (isTRUE(result$ok)) {
      dataset_results(qres_search_datasets(con))
      # Mirror email into the status field and refresh the table
      updateTextInput(session, 'status_email', value = email)
      .refresh_my_datasets(email)
    }
  })

  output$recent_datasets_table <- renderDT({
    input$run_upload
    tryCatch({
      df <- DBI::dbGetQuery(con, "
        SELECT d.dataset_id, w.species, w.medium_type, p.temperature_c,
               w.perturbation_type, w.perturbation,
               d.year,
               CASE WHEN r.path IS NOT NULL THEN '✓' ELSE '–' END AS raw_data,
               CASE WHEN ue.released = TRUE OR ue.dataset_id IS NULL
                    THEN 'public' ELSE 'embargoed' END AS release_status
        FROM dataset d
        LEFT JOIN plate p USING(dataset_id)
        LEFT JOIN (SELECT plate_id, species, medium_type, perturbation_type, perturbation
                   FROM well_metadata GROUP BY 1,2,3,4,5) w USING(plate_id)
        LEFT JOIN raw_measurement_registry r USING(dataset_id)
        LEFT JOIN upload_embargo ue USING(dataset_id)
        ORDER BY d.import_date DESC LIMIT 50")

      if ('medium_type' %in% names(df))
        df$medium_type <- ifelse(nchar(df$medium_type %||% '') > 22,
                                 paste0(substr(df$medium_type, 1, 22), '…'), df$medium_type)

      datatable(df, rownames = FALSE,
                options = list(pageLength = 10, scrollX = TRUE,
                               scrollCollapse = TRUE, autoWidth = FALSE,
                               columnDefs = list(
                                 list(className = 'dt-center',
                                      targets = which(names(df) %in%
                                        c('temperature_c','raw_data','release_status','year')) - 1L)
                               )))
    }, error = function(e) datatable(data.frame(message = 'No datasets yet.')))
  })

  output$download_all_meta <- downloadHandler(
    filename = function() paste0('phamig_metadata_', Sys.Date(), '.csv'),
    content  = function(file) {
      df <- DBI::dbGetQuery(con, "
        SELECT d.dataset_id, d.raw_file, w.species, w.strain, w.medium_type,
               p.temperature_c, w.perturbation_type, w.perturbation,
               w.concentration, w.concentration_unit,
               p.plate_format, p.plate_reader, p.wavelength_nm, p.shake_freq_rpm,
               d.time_unit, d.authors, d.year, d.doi, d.notes
        FROM dataset d
        LEFT JOIN plate p USING(dataset_id)
        LEFT JOIN (SELECT plate_id, species, strain, medium_type, perturbation_type,
                          perturbation, concentration, concentration_unit
                   FROM well_metadata GROUP BY 1,2,3,4,5,6,7,8) w USING(plate_id)
        ORDER BY d.import_date")
      write.csv(df, file, row.names = FALSE)
    }
  )

  # ── Notes / submissions ────────────────────────────────────────────────────
  observeEvent(input$submit_note, {
    req(nzchar(trimws(input$note_text %||% '')))
    note_id   <- paste0('note_', format(Sys.time(), '%Y%m%d%H%M%S'),
                        '_', sample(1000:9999, 1))
    note_type <- input$note_type
    note_text <- trimws(input$note_text)
    author    <- trimws(input$note_author %||% '')
    ts        <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')

    # Store in DuckDB
    ds_id <- trimws(input$note_dataset_id %||% '')
    note <- data.frame(
      note_id    = note_id,
      dataset_id = if (nzchar(ds_id)) ds_id else NA_character_,
      note_type  = note_type,
      note_text  = note_text,
      author     = author,
      stringsAsFactors = FALSE
    )
    DBI::dbWriteTable(con, 'user_note', note, append = TRUE)

    # Write to Submittion_logs folder
    log_dir  <- 'Submittion_logs'
    dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
    log_file <- file.path(log_dir, paste0(note_id, '.txt'))
    writeLines(c(
      paste('ID:        ', note_id),
      paste('Type:      ', note_type),
      paste('Author:    ', if (nzchar(author)) author else '(anonymous)'),
      paste('Timestamp: ', ts),
      '',
      '--- Note text ---',
      note_text
    ), log_file)

    # Email notification (silent if not configured)
    phamig_notify_note(note_id, note_type, note_text,
                       author = author, timestamp = ts)

    updateTextAreaInput(session, 'note_text', value = '')
    showNotification('Note submitted — thank you!', type = 'message')
  })

  # ── Notes admin ────────────────────────────────────────────────────────────
  admin_auth      <- reactiveVal(FALSE)
  notes_refresh   <- reactiveVal(0)

  # Open login modal
  observeEvent(input$admin_notes_btn, {
    showModal(modalDialog(
      title = tagList(icon('lock'), ' Admin login'),
      textInput('admin_user', 'Username'),
      passwordInput('admin_pwd', 'Password'),
      footer = tagList(
        modalButton('Cancel'),
        actionButton('admin_login_btn', 'Login', class = 'btn-primary')
      ),
      easyClose = TRUE, size = 's'
    ))
  })

  # Verify credentials — compared against ~/.Renviron hashes, never plain text
  observeEvent(input$admin_login_btn, {
    ok_user <- identical(trimws(input$admin_user),
                         Sys.getenv('PHAMIG_ADMIN_USER', unset = ''))
    ok_pwd  <- identical(qres_hash_passphrase(trimws(input$admin_pwd)),
                         Sys.getenv('PHAMIG_ADMIN_PWD_HASH', unset = ''))
    if (ok_user && ok_pwd) {
      admin_auth(TRUE)
      removeModal()
      showNotification(tagList(icon('check'), ' Admin access granted.'),
                       type = 'message', duration = 3)
    } else {
      showNotification('Incorrect username or password.', type = 'error', duration = 4)
    }
  })

  # Delete a note (admin only)
  observeEvent(input$admin_delete_note_id, {
    req(admin_auth(), nzchar(input$admin_delete_note_id))
    DBI::dbExecute(con, 'DELETE FROM user_note WHERE note_id = ?',
                   params = list(input$admin_delete_note_id))
    notes_refresh(notes_refresh() + 1)
    showNotification('Note deleted.', type = 'message', duration = 2)
  })

  # Notes section: public or admin view
  output$notes_section_ui <- renderUI({
    if (admin_auth()) {
      tagList(
        tags$div(
          tags$span(icon('shield-alt'),
            tags$strong(' Admin view'),
            tags$span(class = 'admin-badge', 'authenticated'),
            style = 'color:#c0392b; margin-bottom:6px; display:block;'
          ),
          actionLink('admin_logout_btn',
            tagList(icon('sign-out-alt', style='font-size:11px;'), ' log out'),
            style = 'font-size:11px; color:#aaa;')
        ),
        br(),
        DTOutput('notes_table_admin')
      )
    } else {
      tagList(
        h5('Submitted notes'),
        DTOutput('notes_table')
      )
    }
  })

  observeEvent(input$admin_logout_btn, {
    admin_auth(FALSE)
    showNotification('Logged out.', type = 'message', duration = 2)
  })

  # Public notes table (truncated, no author)
  output$notes_table <- renderDT({
    input$submit_note; notes_refresh()
    qres_init_schema(con)
    if (!DBI::dbExistsTable(con, 'user_note')) return(datatable(data.frame()))
    datatable(
      DBI::dbGetQuery(con,
        'SELECT note_type, note_text, created_at FROM user_note ORDER BY created_at DESC'),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  # Admin notes table — full text, author/email, delete buttons
  output$notes_table_admin <- renderDT({
    input$submit_note; notes_refresh()
    if (!DBI::dbExistsTable(con, 'user_note')) return(datatable(data.frame()))
    df <- DBI::dbGetQuery(con,
      'SELECT note_id, note_type, author, dataset_id, note_text, created_at
       FROM user_note ORDER BY created_at DESC')
    df$delete <- sprintf(
      '<button class="btn btn-danger btn-xs"
         onclick="Shiny.setInputValue(\'admin_delete_note_id\', \'%s\', {priority:\'event\'})">
         <i class=\"fa fa-trash\"></i></button>',
      df$note_id
    )
    datatable(
      df[, c('delete', 'note_type', 'author', 'dataset_id', 'note_text', 'created_at')],
      escape   = FALSE,
      rownames = FALSE,
      colnames = c('', 'Type', 'Author / email', 'Dataset ID', 'Note', 'Submitted'),
      options  = list(
        pageLength = 25, scrollX = TRUE,
        columnDefs = list(
          list(width = '30px',  targets = 0),
          list(width = '80px',  targets = 1),
          list(width = '130px', targets = 2),
          list(width = '140px', targets = 3),
          list(width = '120px', targets = 5)
        )
      )
    )
  })

  # ── LLM / Ask tab ─────────────────────────────────────────────────────────
  nl_result   <- reactiveVal(NULL)
  nl_pending  <- reactiveVal(NULL)   # parsed but not yet confirmed

  output$llm_api_warning <- renderUI({
    backend <- tryCatch(qres_llm_backend(), error = function(e) 'none')
    if (backend == 'none') {
      tags$div(class = 'alert alert-info',
        tags$strong('Rule-based mode active. '),
        'NL queries use pattern matching. For LLM-powered queries, set ',
        tags$code('ANTHROPIC_API_KEY'), ' in ', tags$code('~/.Renviron'),
        ' and restart R — or run a local ', tags$a('Ollama', href = 'https://ollama.com', target = '_blank'), ' server.'
      )
    }
  })

  output$llm_status_badge <- renderUI({
    backend <- tryCatch(qres_llm_backend(), error = function(e) 'none')
    switch(backend,
      'anthropic' = tags$span(class = 'label label-success', 'Anthropic Claude'),
      'ollama'    = tags$span(class = 'label label-success', 'Ollama (local)'),
      tags$span(class = 'label label-default', 'Rule-based')
    )
  })

  # ── NL preview box: shown after parsing, before results ────────────────────
  .fmt_filters <- function(filters) {
    parts <- c()
    if (!is.null(filters$species))  parts <- c(parts, paste0('species: ', filters$species))
    if (!is.null(filters$temp_min) || !is.null(filters$temp_max)) {
      lo <- filters$temp_min %||% '?'; hi <- filters$temp_max %||% '?'
      parts <- c(parts, paste0('temperature: ', lo, '–', hi, '°C'))
    }
    if (!is.null(filters$medium))   parts <- c(parts, paste0('medium contains: "', filters$medium, '"'))
    if (!is.null(filters$perturbation)) parts <- c(parts, paste0('perturbation: ', filters$perturbation))
    if (length(parts) == 0) return('(all datasets)')
    paste(parts, collapse = ', ')
  }

  output$nl_preview_box <- renderUI({
    p <- nl_pending()
    if (is.null(p)) return(NULL)
    n_total <- nrow(p$rows)
    n_raw   <- if ('raw_path' %in% names(p$rows)) sum(!is.na(p$rows$raw_path)) else n_total
    tags$div(
      class = 'alert alert-warning',
      style = 'margin-bottom: 8px;',
      tags$strong(icon('search'), ' Query interpreted as: '),
      tags$span(p$human_readable),
      tags$br(),
      tags$span(style = 'font-size: 13px;',
        icon('database'), ' Found ', tags$strong(n_total), ' dataset(s)',
        if (n_raw < n_total)
          tagList(' (', tags$strong(n_raw), ' with raw measurement data)')
        else
          tagList(' — all have raw data'),
        '.'
      ),
      tags$br(), tags$br(),
      tags$span(style = 'font-size: 12px; color: #666;',
        'If this is not what you meant, edit your question and ask again.'),
      tags$br(), tags$br(),
      actionButton('nl_confirm', 'Show results for this query',
                   class = 'btn-success btn-sm',
                   icon  = icon('check'))
    )
  })

  observeEvent(input$nl_submit, {
    q <- trimws(input$nl_question %||% '')
    req(nzchar(q))
    nl_result(NULL)
    nl_pending(NULL)

    withProgress(message = 'Interpreting query…', {

      # ── Step 1: Rule-based parser ──────────────────────────────────────────
      setProgress(0.2, detail = 'Checking patterns…')
      parsed <- tryCatch(qres_parse_nl_query(q), error = function(e) NULL)

      if (!is.null(parsed) && length(parsed) > 0) {
        rows <- tryCatch(
          do.call(qres_filter_datasets, c(list(con = con, registered_only = FALSE), parsed)),
          error = function(e) NULL
        )
        if (!is.null(rows) && nrow(rows) > 0) {
          nl_pending(list(
            rows          = rows,
            human_readable = .fmt_filters(parsed),
            type          = 'table',
            description   = paste0('Rule-based: ', paste(names(parsed), collapse = ', ')),
            sql           = NULL
          ))
          return()
        }
      }

      # ── Step 2: LLM ───────────────────────────────────────────────────────
      if (qres_llm_available()) {
        setProgress(0.5, detail = paste0('Asking ', qres_llm_backend_name(), '…'))
        filters <- tryCatch(qres_nl_to_action(q), error = function(e) NULL)

        if (!is.null(filters) && length(filters) > 0) {
          setProgress(0.8, detail = 'Running query…')
          rows <- tryCatch(
            do.call(qres_filter_datasets, c(list(con = con, registered_only = FALSE), filters)),
            error = function(e) NULL
          )
          if (!is.null(rows) && nrow(rows) > 0) {
            nl_pending(list(
              rows          = rows,
              human_readable = .fmt_filters(filters),
              type          = 'table',
              description   = paste0('LLM (', qres_llm_backend_name(), '): ',
                                     paste(names(filters), collapse = ', ')),
              sql           = NULL
            ))
            return()
          }
        }
      }

      # ── No results ────────────────────────────────────────────────────────
      nl_result(list(
        type        = 'table',
        data        = data.frame(message = paste0(
          'No datasets matched "', q, '". ',
          if (!qres_llm_available())
            'Tip: set ANTHROPIC_API_KEY in ~/.Renviron and restart R for LLM-powered search.'
          else
            'Try rephrasing — e.g. "E. coli 37°C" or "rifampicin dose response".'
        )),
        description = '',
        sql         = NULL,
        plot_type   = 'table'
      ))
    })
  })

  # ── Confirm pending NL query → populate results ─────────────────────────────
  observeEvent(input$nl_confirm, {
    p <- nl_pending()
    req(!is.null(p))
    nl_result(list(
      type        = p$type,
      data        = p$rows,
      description = p$description,
      sql         = p$sql,
      plot_type   = 'table'
    ))
    nl_pending(NULL)
  })

  output$nl_description_box <- renderUI({
    req(nl_result())
    desc <- nl_result()$description
    if (nzchar(desc %||% ''))
      tags$div(class = 'alert alert-info', tags$strong('Query: '), desc)
  })

  output$nl_sql_box <- renderUI({
    req(nl_result())
    sql <- nl_result()$sql
    if (!is.null(sql) && nzchar(sql)) {
      tags$details(
        tags$summary('Generated SQL'),
        tags$pre(sql, style = 'background:#f5f5f5; padding:8px; font-size:12px;')
      )
    }
  })

  output$nl_selection_hint <- renderUI({
    req(nl_result())
    df <- nl_result()$data
    if (is.null(df) || nrow(df) == 0 || !'dataset_id' %in% names(df)) return(NULL)
    n_sel <- length(input$nl_table_rows_selected)
    if (n_sel > 0) {
      tags$p(style = 'font-size:12px; color:#2980b9; margin-bottom:4px;',
        icon('check-circle'),
        sprintf(' %d dataset(s) selected — tool benchmarking and growth curves use only these.
                 Click rows to toggle; click elsewhere to deselect all.', n_sel))
    } else {
      tags$p(style = 'font-size:12px; color:#888; margin-bottom:4px;',
        icon('info-circle'),
        ' Click one or more rows to restrict tool benchmarking and growth curves to those datasets.')
    }
  })

  output$nl_table <- renderDT({
    req(nl_result())
    df <- nl_result()$data
    if (is.null(df) || nrow(df) == 0) return(datatable(data.frame(message = 'No results.')))
    display_cols <- intersect(c('species', 'strain', 'medium_type', 'perturbation_type',
                                 'perturbation', 'temperature_c', 'year', 'authors', 'doi'),
                              names(df))
    datatable(df[, display_cols, drop = FALSE],
              selection = 'multiple',
              rownames  = FALSE,
              options   = list(pageLength = 20, scrollX = TRUE))
  })

  # Returns selected dataset_ids (user-picked rows) or all ids with raw data.
  nl_active_ids <- reactive({
    res <- nl_result()
    if (is.null(res) || is.null(res$data) || nrow(res$data) == 0) return(NULL)
    d   <- res$data
    if (!'dataset_id' %in% names(d)) return(NULL)
    sel <- input$nl_table_rows_selected
    if (length(sel) > 0) return(unique(d$dataset_id[sel]))
    ids <- unique(d$dataset_id)
    if ('raw_path' %in% names(d)) ids <- unique(d$dataset_id[!is.na(d$raw_path)])
    ids
  })

  output$nl_comp_blank_note <- renderUI({
    req(input$nl_comp_baseline == 'blank_wells')
    ids  <- nl_active_ids()
    bids <- if (!is.null(ids) && length(ids) > 0)
      tryCatch(qres_get_blank_wells(con, ids), error = function(e) character(0))
    else character(0)
    if (length(bids) > 0)
      tags$small(style = 'color:#27ae60;',
        icon('check'), sprintf(' %d blank well(s) found.', length(bids)))
    else
      tags$small(style = 'color:#e74c3c;',
        icon('exclamation-triangle'),
        ' No blank wells designated in these datasets. Select a different method.')
  })

  # Blank/control entry panel shown above both NL tabs when datasets are active.
  output$nl_blank_ctrl_panel <- renderUI({
    ids <- nl_active_ids()
    if (is.null(ids) || length(ids) == 0) return(NULL)
    tagList(
      hr(style = 'margin: 6px 0;'),
      fluidRow(
        column(6,
          opt_label('Blank wells',
            'Well names or ranges (e.g. A1:H1). Auto-detect uses SD < 0.1 per well per dataset.'),
          textInput('nl_blank_range', NULL, value = '',
            placeholder = 'e.g. A1:H1, or use auto-detect', width = '100%'),
          checkboxInput('nl_autodetect_blanks',
            tagList('Auto-detect from OD signal (SD < 0.1)',
                    tip('Flat wells — likely blank medium — are detected per dataset automatically.')),
            value = TRUE),
          uiOutput('nl_blank_autodetect_result'),
          actionButton('nl_clear_blanks', tagList(icon('xmark'), ' Clear blanks'),
            class = 'btn btn-xs btn-outline-secondary',
            style = 'font-size:11px; height:22px; padding:1px 7px; margin-top:3px;')
        ),
        column(6,
          opt_label('Control wells',
            'Plotted in black; excluded from curve fitting.'),
          textInput('nl_ctrl_range', NULL, value = '',
            placeholder = 'e.g. H12', width = '100%'),
          actionButton('nl_clear_controls', tagList(icon('xmark'), ' Clear controls'),
            class = 'btn btn-xs btn-outline-secondary',
            style = 'font-size:11px; height:22px; padding:1px 7px; margin-top:3px;')
        )
      ),
      hr(style = 'margin: 6px 0;')
    )
  })

  observeEvent(input$nl_clear_blanks, {
    updateTextInput(session, 'nl_blank_range', value = '')
    updateCheckboxInput(session, 'nl_autodetect_blanks', value = FALSE)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  observeEvent(input$nl_clear_controls, {
    updateTextInput(session, 'nl_ctrl_range', value = '')
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # Auto-detect blank candidates for NL datasets: per-dataset SD < 0.1.
  nl_blank_candidates <- reactive({
    ids <- nl_active_ids()
    req(!is.null(ids) && length(ids) > 0)
    all_raw <- lapply(ids, function(did) {
      tryCatch(qres_get_raw_curves(con, did), error = function(e) NULL)
    })
    raw <- dplyr::bind_rows(Filter(Negate(is.null), all_raw))
    req(nrow(raw) > 0)
    per_ds <- dplyr::summarise(
      dplyr::group_by(raw, dataset_id, well),
      mean_od = mean(value, na.rm = TRUE),
      sd_od   = dplyr::coalesce(sd(value, na.rm = TRUE), 0),
      .groups = 'drop')
    dplyr::arrange(
      dplyr::summarise(
        dplyr::group_by(per_ds, well),
        mean_od   = mean(mean_od, na.rm = TRUE),
        sd_od     = mean(sd_od,   na.rm = TRUE),
        candidate = any(sd_od < 0.1),
        .groups   = 'drop'),
      sd_od
    )
  })

  output$nl_blank_autodetect_result <- renderUI({
    req(isTRUE(input$nl_autodetect_blanks))
    df <- tryCatch(nl_blank_candidates(), error = function(e) NULL)
    req(!is.null(df) && nrow(df) > 0)
    hits <- df$well[df$candidate]
    if (length(hits) == 0)
      return(tags$small(style = 'color:#888;', 'No flat wells found (SD < 0.1).'))
    tagList(
      tags$small(style = 'color:#27ae60;',
        icon('check'), sprintf(' %d candidate(s): %s', length(hits),
                               paste(hits, collapse = ', ')))
    )
  })

  # ── Shared computation: run plugins on NL-selected datasets ─────────────────
  # Cached without nl_metric_select (plot filters post-hoc; table shows all).
  nl_comp_metrics_long <- reactive({
    req(nl_result())
    res  <- nl_result()
    nl_bm <- input$nl_comp_baseline %||% 'none'

    if (res$type == 'comparison' && !is.null(res$data) && nrow(res$data) > 0)
      return(res$data)

    if (res$type != 'table' || is.null(res$data) ||
        !'dataset_id' %in% names(res$data) || nrow(res$data) == 0)
      return(data.frame())

    ids <- nl_active_ids()
    user_sel <- length(input$nl_table_rows_selected) > 0
    if (!user_sel) ids <- head(ids, 5)
    if (length(ids) == 0) return(data.frame())

    nl_blank_ids <- if (nl_bm == 'blank_wells')
      tryCatch(qres_get_blank_wells(con, ids), error = function(e) character(0))
    else character(0)
    # Manual blank names: auto-detect (if enabled) takes priority over text input
    nl_blank_names <- {
      if (isTRUE(input$nl_autodetect_blanks)) {
        cands <- tryCatch(nl_blank_candidates(), error = function(e) NULL)
        if (!is.null(cands)) cands$well[cands$candidate] else character(0)
      } else {
        .parse_wells(input$nl_blank_range %||% '')
      }
    }

    .nl_label <- function(did) {
      row   <- res$data[res$data$dataset_id == did, ][1, ]
      sp    <- trimws(row$species %||% '')
      tc    <- row$temperature_c %||% NA
      ptype <- tolower(trimws(row$perturbation_type %||% ''))
      pert  <- trimws(row$perturbation %||% '')
      lab   <- paste0(if (nzchar(sp)) sp else did,
                      if (is.finite(as.numeric(tc))) paste0(' ', round(as.numeric(tc), 1), '°C') else '')
      show  <- nzchar(pert) &&
               !tolower(pert) %in% c('none', 'na', '') &&
               !ptype %in% c('temperature', 'none', '') &&
               is.na(suppressWarnings(as.numeric(pert)))
      if (show) lab <- paste0(lab, ' + ', pert)
      lab
    }

    nl_tools <- input$nl_comp_tools %||% c('model_free', 'gcplyr', 'growthrates')

    all_cmp <- lapply(ids, function(did) {
      raw <- tryCatch(qres_get_raw_curves(con, did), error = function(e) NULL)
      if (is.null(raw)) return(NULL)
      raw <- qres_prepare_curves(raw, method = nl_bm,
                                  blank_names = nl_blank_names,
                                  blank_ids   = nl_blank_ids)
      cmp <- tryCatch(
        qres_compare_plugin_outputs(raw, plugins = nl_tools),
        error = function(e) NULL)
      if (!is.null(cmp) && nrow(cmp) > 0) {
        cmp$dataset_id <- did
        cmp$plot_label <- .nl_label(did)
      }
      cmp
    })
    dplyr::bind_rows(Filter(Negate(is.null), all_cmp))
  }) |> bindCache(nl_active_ids(), input$nl_table_rows_selected,
                  input$nl_comp_tools, input$nl_comp_baseline,
                  input$nl_autodetect_blanks, input$nl_blank_range)

  output$nl_comp_plot <- renderPlot({
    combined <- nl_comp_metrics_long()
    metric   <- input$nl_metric_select %||% 'doubling_time_h'
    if (is.null(combined) || nrow(combined) == 0) {
      return(ggplot2::ggplot() +
        ggplot2::annotate('text', x = .5, y = .5, size = 5, colour = 'grey50',
          label = 'Run a query to populate the comparison plot.') +
        ggplot2::theme_void())
    }
    # Build context title from NL result metadata (same logic as nl_curves_plot)
    nl_meta <- tryCatch({
      m <- nl_result()$data
      ids <- nl_active_ids()
      if (!is.null(m) && 'dataset_id' %in% names(m)) m[m$dataset_id %in% ids, , drop=FALSE]
      else data.frame()
    }, error = function(e) data.frame())
    .dv2 <- function(col) {
      if (!col %in% names(nl_meta)) return(character(0))
      v <- sort(unique(na.omit(as.character(nl_meta[[col]]))))
      v[nzchar(v) & v != 'NA']
    }
    abbrev_sp2 <- function(s) sub("^(\\S)\\S+ (\\S+).*$", "\\1. \\2", s)
    sp_vec2  <- .dv2('species')
    sp_lbl2  <- if (length(sp_vec2) == 0) NULL else if (length(sp_vec2) == 1)
                  abbrev_sp2(sp_vec2) else paste(sapply(sp_vec2, abbrev_sp2), collapse = ', ')
    med_vec2 <- .dv2('medium_type')
    med_lbl2 <- switch(as.character(length(med_vec2)), "0"=NULL, "1"=med_vec2,
                       paste0(length(med_vec2), ' media'))
    tc_vec2  <- suppressWarnings(sort(as.numeric(.dv2('temperature_c'))))
    tc_lbl2  <- if (length(tc_vec2) == 0) NULL else if (length(tc_vec2) == 1)
                  paste0(tc_vec2, '°C') else paste0(min(tc_vec2), '–', max(tc_vec2), '°C')
    nl_ctx   <- paste(Filter(Negate(is.null), list(sp_lbl2, med_lbl2, tc_lbl2)), collapse = ' · ')
    facet_col <- if ('plot_label' %in% names(combined)) 'plot_label' else 'dataset_id'
    qres_plot_metric_comparison(combined, metric = metric,
      facet_by      = if (length(unique(combined[[facet_col]])) > 1) facet_col else NULL,
      log_y         = isTRUE(input$nl_comp_log_y),
      context_title = if (nzchar(nl_ctx)) nl_ctx else NULL)
  }) |> bindCache(nl_active_ids(), input$nl_table_rows_selected,
                  input$nl_comp_tools, input$nl_comp_baseline,
                  input$nl_metric_select, input$nl_comp_log_y)

  # ── Wide table: one row per (dataset × well), columns = tool__metric ─────────
  .nl_comp_wide <- reactive({
    long <- nl_comp_metrics_long()
    if (is.null(long) || nrow(long) == 0) return(data.frame())

    meta <- tryCatch(nl_result()$data, error = function(e) NULL)

    long$well <- sub('^.*__', '', long$well_id)

    wide <- long |>
      dplyr::select(dataset_id, well_id, well, metric_name, metric_value, tool) |>
      dplyr::mutate(col_name = paste0(tool, '·', metric_name)) |>
      dplyr::select(-tool, -metric_name) |>
      tidyr::pivot_wider(names_from  = col_name,
                         values_from = metric_value,
                         values_fn   = mean) |>
      dplyr::select(-well_id)

    if (!is.null(meta) && 'dataset_id' %in% names(meta)) {
      meta_cols <- intersect(c('species', 'temperature_c', 'medium_type',
                                'perturbation_type', 'perturbation'), names(meta))
      if (length(meta_cols) > 0) {
        meta_sub <- dplyr::distinct(meta[, c('dataset_id', meta_cols), drop = FALSE])
        wide <- dplyr::left_join(wide, meta_sub, by = 'dataset_id')
      }
    }

    front <- intersect(c('dataset_id', 'species', 'temperature_c', 'medium_type',
                          'perturbation_type', 'perturbation', 'well'), names(wide))
    metric_cols <- setdiff(names(wide), front)
    wide <- dplyr::arrange(
      dplyr::select(wide, dplyr::all_of(c(front, metric_cols))),
      dataset_id, well)
    wide
  })

  output$nl_comp_table <- DT::renderDT({
    wide <- .nl_comp_wide()
    if (nrow(wide) == 0)
      return(DT::datatable(data.frame(Note = 'No results yet — run a query first.'),
                           rownames = FALSE, options = list(dom = 't')))
    num_cols <- names(wide)[sapply(wide, is.numeric)]
    DT::datatable(
      wide,
      rownames    = FALSE,
      filter      = 'top',
      extensions  = 'RowGroup',
      options     = list(
        pageLength  = 50,
        scrollX     = TRUE,
        rowGroup    = list(dataSrc = which(names(wide) == 'dataset_id') - 1L),
        columnDefs  = list(list(visible = FALSE,
                                targets = which(names(wide) == 'dataset_id') - 1L)),
        dom         = 'frtip'
      )
    ) |> DT::formatRound(num_cols, digits = 4)
  }, server = TRUE)

  output$nl_comp_dl_csv <- downloadHandler(
    filename = function() {
      paste0('nl_comparison_', format(Sys.time(), '%Y%m%d_%H%M%S'), '.csv')
    },
    content  = function(file) {
      wide <- .nl_comp_wide()
      utils::write.csv(wide, file, row.names = FALSE)
    }
  )

  output$nl_curves_panel <- renderUI({
    ids <- nl_active_ids()
    if (is.null(ids) || length(ids) == 0) return(NULL)
    blank_ids <- tryCatch(qres_get_blank_wells(con, ids), error = function(e) character(0))
    has_blanks <- length(blank_ids) > 0
    baseline_choices <- c('None' = 'none', 'Min per well' = 'min_well',
                          'First 3 tp' = 'first_3', 'Blank wells' = 'blank_wells')
    tagList(
      tags$div(
        tags$b('Baseline subtraction', style = 'font-size:13px;'),
        tags$small(style = 'color:dimgray; margin-left:6px;',
          '— Sprouffske & Wagner (2012), Hall et al. (2014), gcplyr / Mira et al. (2022)'),
        br(),
        radioButtons('nl_baseline', NULL, choices = baseline_choices,
                     selected = 'none', inline = TRUE),
        if (!has_blanks)
          tags$small(style = 'color:#888; font-size:11px;',
            icon('info-circle'), ' No blank wells designated in these datasets.')
      ),
      fluidRow(
        column(2, checkboxInput('nl_log_y', 'Log y', FALSE))
      ),
      plotOutput('nl_curves_plot', height = '420px')
    )
  })

  output$nl_curves_plot <- renderPlot({
    req(nl_result())
    res      <- nl_result()
    user_sel <- length(input$nl_table_rows_selected) > 0
    ids      <- nl_active_ids()
    if (!user_sel) ids <- head(ids, 8)   # cap auto-mode
    if (is.null(ids) || length(ids) == 0) return(NULL)

    raw_list <- lapply(ids, function(did) {
      df <- tryCatch(qres_get_raw_curves(con, did), error = function(e) NULL)
      if (!is.null(df) && nrow(df) > 0) {
        df$dataset_id <- did
        # Attach label from NL result metadata if available
        if ('dataset_id' %in% names(res$data)) {
          row   <- res$data[res$data$dataset_id == did, ][1, ]
          sp    <- trimws(row$species %||% '')
          tc    <- row$temperature_c %||% NA
          ptype <- tolower(trimws(row$perturbation_type %||% ''))
          pert  <- trimws(row$perturbation %||% '')
          lab   <- paste0(if (nzchar(sp)) sp else did,
                          if (is.finite(as.numeric(tc))) paste0(' ', round(as.numeric(tc), 1), '°C') else '')
          show  <- nzchar(pert) && !tolower(pert) %in% c('none', 'na', '') &&
                   !ptype %in% c('temperature', 'none', '') &&
                   is.na(suppressWarnings(as.numeric(pert)))
          if (show) lab <- paste0(lab, ' + ', pert)
          df$plot_label <- lab
        }
        df
      }
    })
    combined <- dplyr::bind_rows(Filter(Negate(is.null), raw_list))
    if (nrow(combined) == 0) return(NULL)

    # Guard: refuse to combine datasets with different signal types
    if ('signal_type' %in% names(combined)) {
      sig_types <- sort(unique(na.omit(combined$signal_type)))
      sig_types <- sig_types[nzchar(sig_types)]
      if (length(sig_types) > 1)
        validate(need(FALSE, paste0(
          "Incompatible measurement types in query results: ",
          paste(sig_types, collapse = " + "),
          ". Cannot overlay ", paste(sig_types, collapse = ", "),
          " on the same y-axis. Refine your query to return one measurement type.")))
    }

    nl_bmethod     <- input$nl_baseline %||% 'none'
    nl_bids        <- if (nl_bmethod == 'blank_wells')
      tryCatch(qres_get_blank_wells(con, ids), error = function(e) character(0))
    else character(0)
    nl_blank_names <- if (isTRUE(input$nl_autodetect_blanks)) {
      cands <- tryCatch(nl_blank_candidates(), error = function(e) NULL)
      if (!is.null(cands)) cands$well[cands$candidate] else character(0)
    } else {
      .parse_wells(input$nl_blank_range %||% '')
    }
    nl_ctrl_names <- .parse_wells(input$nl_ctrl_range %||% '')
    combined      <- qres_prepare_curves(combined, method = nl_bmethod,
                                          blank_names = nl_blank_names,
                                          blank_ids   = nl_bids)
    ctrl_ids    <- combined$well_id[combined$well %in% nl_ctrl_names]
    colour_col  <- if ('plot_label' %in% names(combined)) 'plot_label' else 'dataset_id'
    n_groups    <- length(unique(combined[[colour_col]]))
    colour_by   <- if (n_groups <= 1L) 'well' else colour_col

    # 5-question title from query result metadata
    nl_meta <- tryCatch(res$data, error = function(e) NULL)
    nl_meta <- if (!is.null(nl_meta) && nrow(nl_meta) > 0 &&
                   'dataset_id' %in% names(nl_meta))
                 nl_meta[nl_meta$dataset_id %in% ids, , drop = FALSE]
               else data.frame()
    .dv <- function(col) {
      if (!col %in% names(nl_meta)) return(character(0))
      v <- sort(unique(na.omit(as.character(nl_meta[[col]]))))
      v[nzchar(v) & v != 'NA']
    }
    abbrev_sp <- function(s) sub("^(\\S)\\S+ (\\S+).*$", "\\1. \\2", s)
    sp_vec  <- .dv('species')
    sp_lbl  <- if (length(sp_vec) == 0) NULL else if (length(sp_vec) == 1)
                 abbrev_sp(sp_vec) else paste(sapply(sp_vec, abbrev_sp), collapse = ', ')
    med_lbl <- switch(as.character(length(.dv('medium_type'))),
                 "0" = NULL, "1" = .dv('medium_type')[1],
                 paste0(length(.dv('medium_type')), ' media'))
    tc_vec  <- suppressWarnings(sort(as.numeric(.dv('temperature_c'))))
    tc_vec  <- tc_vec[is.finite(tc_vec)]
    tc_lbl  <- if (length(tc_vec) == 0) NULL else if (length(tc_vec) == 1)
                 paste0(tc_vec, '°C') else paste0(min(tc_vec), '–', max(tc_vec), '°C')
    pt_vec  <- .dv('perturbation_type')
    pt_vec  <- pt_vec[!pt_vec %in% c('none', 'temperature', '')]
    pt_lbl  <- if (length(pt_vec) == 0) NULL else paste(pt_vec, collapse = '/')
    nl_title_parts <- Filter(Negate(is.null), list(sp_lbl, med_lbl, tc_lbl, pt_lbl))
    nl_title       <- if (length(nl_title_parts) > 0)
                        paste(nl_title_parts, collapse = ' · ') else NULL
    sig    <- sort(unique(na.omit(combined$signal_type)))[1]
    bm_lbl <- switch(nl_bmethod,
      none='no baseline correction', min_well='min-per-well subtraction',
      first3='first-3-timepoints baseline', blank_wells='blank-well subtraction', nl_bmethod)
    nl_subtitle <- paste(Filter(Negate(is.null),
      list(if (!is.null(sig)) sig else NULL, bm_lbl, '1 line = 1 well')), collapse = ' · ')

    p <- qres_plot_growth(combined, log_y = input$nl_log_y %||% FALSE,
                     colour_by = colour_by, control_wells = ctrl_ids, title = nl_title)
    if (nzchar(nl_subtitle)) p <- p + ggplot2::labs(subtitle = nl_subtitle)
    p
  })
}

shinyApp(ui, server)
