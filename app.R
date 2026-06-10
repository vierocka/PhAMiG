if (file.exists('~/.Renviron')) readRenviron('~/.Renviron')
library(shiny)
library(shinythemes)
library(shiny.router)
library(DT)
library(DBI)
library(duckdb)
library(ggplot2)
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

source('R/plugins/model_free.R')
source('R/plugins/gcplyr.R')
source('R/plugins/growthrates.R')
source('R/plugins/dgrothr.R')
source('R/plugins/kinbiont.R')
source('R/plugins/omniplate.R')
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
    .home-title { font-size: 26px; font-weight: 700;
                  color: #1a3a5c; margin: 0 0 10px; }
    .home-tagline { color: #444; font-size: 15px;
                    max-width: 640px; margin: 0 auto 6px; }
    .home-abbrev  { color: #888; font-size: 12px;
                    max-width: 500px; margin: 0 auto 28px; line-height: 1.7; }

    /* ── Primary action cards ── */
    .home-primary { display: flex; justify-content: center;
                    gap: 20px; flex-wrap: wrap; margin-bottom: 10px; }
    .home-card {
      display: flex; flex-direction: column; align-items: center;
      width: 190px; padding: 22px 16px 18px;
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
    .home-card img { width: 100px; height: 100px; margin-bottom: 12px; }
    .home-card-label { font-weight: 700; font-size: 15px; margin-bottom: 4px; }
    .home-card-sub   { color: #888; font-size: 12px; text-align: center; }

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
    .home-sec-link img { width: 52px; height: 52px;
                         margin-bottom: 5px; opacity: .75; }
    .home-sec-link:hover img { opacity: 1; }
    .home-sec-label { font-size: 12px; font-weight: 600;
                      text-align: center; max-width: 90px; }
  "))),

  tags$div(class = 'home-hero',

    # ── Logos + title ──────────────────────────────────────────────────────
    tags$div(class = 'home-logo-row',
      img(src = 'PhaMiG_logo.png',  height = '90px', alt = 'PhaMiG logo',
          style = 'display:block;'),
      img(src = 'QresDB_logo.png',  height = '90px', alt = 'Qres DB logo',
          style = 'display:block;')
    ),

    tags$h2(class = 'home-title',
      'PhaMiG Platform', tags$span(style = 'font-weight:400; color:#2980b9;',
      ' Powered by Qres DB')),

    tags$p(class = 'home-tagline',
      'A repository for microbial growth curves with integrated analysis and benchmarking tools.'),

    tags$p(class = 'home-abbrev',
      tags$strong('PhaMiG'), HTML(' &ndash; Phenotypic Analysis of Microbial Growth'),
      tags$br(),
      tags$strong('Qres DB'), HTML(' &ndash; Quantitative Dose–Response Database')
    ),

    # ── Three primary action cards ─────────────────────────────────────────
    tags$div(class = 'home-primary',
      tags$a(class = 'home-card', href = route_link('search'),
        img(src = 'search_parse2.png', alt = 'Search'),
        tags$span(class = 'home-card-label', 'Search Data'),
        tags$span(class = 'home-card-sub',   'Find existing datasets.')
      ),
      tags$a(class = 'home-card', href = route_link('submit'),
        img(src = 'submit2.png', alt = 'Submit'),
        tags$span(class = 'home-card-label', 'Submit Data'),
        tags$span(class = 'home-card-sub',   'Deposit new datasets.')
      ),
      tags$a(class = 'home-card', href = route_link('analyse'),
        img(src = 'data_analysis2.png', alt = 'Analyze'),
        tags$span(class = 'home-card-label', 'Analyze Data'),
        tags$span(class = 'home-card-sub',   'Analyze uploaded growth curves.')
      )
    ),

    # ── Secondary nav ──────────────────────────────────────────────────────
    tags$div(class = 'home-secondary',
      tags$a(class = 'home-sec-link', href = route_link('metadata_prep'),
        img(src = 'metadata_prep2.png', alt = 'Metadata Preparation'),
        tags$span(class = 'home-sec-label', 'Metadata Preparation')
      ),
      tags$a(class = 'home-sec-link', href = route_link('ask'),
        img(src = 'contact2.png', alt = 'Natural Language Query'),
        tags$span(class = 'home-sec-label', 'Natural Language Query')
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
    tags$style(HTML('body { padding-bottom: 60px; }
      /* Keep DT tables from overflowing their container */
      .dataTables_wrapper { overflow-x: auto; }
      table.dataTable td { white-space: nowrap; }
    ')),
    # DataTables Buttons extension (column visibility)
    tags$link(rel = 'stylesheet',
              href = 'https://cdn.datatables.net/buttons/2.4.1/css/buttons.dataTables.min.css'),
    tags$script(src = 'https://cdn.datatables.net/buttons/2.4.1/js/dataTables.buttons.min.js'),
    tags$script(src = 'https://cdn.datatables.net/buttons/2.4.1/js/buttons.colVis.min.js')
  ),
  fluidRow(
    column(4, align = 'left',
      tags$span(
  img(src = 'PhaMiG_logo.png',  height = '52px', alt = 'PhaMiG',
      style = 'margin-right:8px;'),
  img(src = 'QresDB_logo.png',  height = '52px', alt = 'Qres DB')
)),
    column(8,
      h3('Search & Browse Datasets'))
  ),
  hr(),
  sidebarLayout(
    sidebarPanel(width = 3,
      actionButton('init_db', 'Initialize / refresh DB', width = '100%',
                   class = 'btn-primary'),
      helpText(tip('Scans data/raw_csv/ and data/metaData.csv and registers them in DuckDB. Safe to click multiple times — already-registered datasets are skipped. Required before searching if you added files manually.'),
               ' First run: register all CSVs as Parquet.'),
      tags$hr(),
      tags$label(
        'Search datasets',
        tags$abbr(
          title = paste(
            'Supports both keywords and natural language (requires API key).',
            'Examples:',
            '"E. coli" — simple keyword;',
            '"E. coli measured in 2023 by Bollenbach in LB medium with temperature as perturbation" — natural language;',
            '"yeast 30 degrees synthetic medium" — mixed.',
            'NL queries are routed through Claude when ANTHROPIC_API_KEY is set; keyword search is always available.'
          ),
          style = 'cursor:help; margin-left:5px; color:#888; font-size:12px; border-bottom:1px dotted #888;',
          'ⓘ info'
        )
      ),
      textInput('search', label = NULL,
                placeholder = 'e.g. "E. coli 37°C LB" or "Bollenbach 2023 rifampicin"'),
      uiOutput('search_mode_badge'),
      tags$hr(),
      h5('Filter by perturbation'),
      selectInput('search_perturb_type',
        req_label('Perturbation type',
          'Filter datasets by experimental perturbation category. "all" shows everything. "temperature" finds growth curves at different temperatures. "chemical" finds antibiotic/drug experiments. "genetic" finds mutant strains.'),
        choices = c('all' = 'all', 'chemical', 'genetic', 'temperature', 'none'),
        selected = 'all'),
      textInput('search_perturb',
        opt_label('Perturbation name',
          'Enter a drug name (e.g. rifampicin), a temperature value (e.g. 37), or a mutation (e.g. ΔtrpB). For temperature perturbations, a numeric value also matches plate.temperature_c within ±0.5°C.'),
        placeholder = 'e.g. rifampicin  or  37  or  ΔtrpB'),
      tags$hr(),
      actionButton('run_search', 'Search', class = 'btn-primary', width = '100%'),
      br(), br(),
      uiOutput('download_search_ui'),
      tags$hr(),
      h5('Single-dataset analysis'),
      selectInput('plugin',
        tagList('Plugin', tip('model_free: fast, no model assumptions. gcplyr/growthrates: standard R packages. DGrowthR: Gaussian process. KinBiont.jl: Julia (needs Julia 1.11+). omniplate: Python via reticulate (needs conda env omniplate_env).')),
        choices = c('model_free', 'gcplyr', 'growthrates',
                    'dgrothr', 'kinbiont', 'omniplate')),
      conditionalPanel("input.plugin == 'kinbiont'",
        tags$small(
          icon('circle-info'), ' KinBiont.jl: log-linear growth modelling in Julia.',
          tags$br(),
          tags$a('Pinheiro et al. (2025) Nat. Commun.',
                 href = 'https://www.nature.com/articles/s41467-025-61592-6',
                 target = '_blank'),
          ' | ',
          tags$a('GitHub', href = 'https://github.com/pinheiroGroup/Kinbiont.jl',
                 target = '_blank'),
          ' | ',
          tags$a('bio.tools', href = 'https://bio.tools/kinbiont', target = '_blank'),
          tags$br(),
          tags$em('Requires Julia 1.11+ with Kinbiont.jl installed.')
        )
      ),
      selectInput('correction_type',
        tagList('Blank correction', tip('none: raw OD values. blank_static: subtract mean of blank wells at time 0. blank_timewise: subtract blank curve at each time point. smooth_only: Savitzky-Golay smoothing only.')),
        choices = c('none', 'blank_static', 'blank_timewise', 'smooth_only')),
      actionButton('run_plugin', 'Run plugin on selected row', width = '100%'),
      tags$hr(),
      checkboxInput('log_y', 'Log-scale y axis', FALSE)
    ),
    mainPanel(width = 9,
      tabsetPanel(id = 'mainTabset',
        tabPanel('Datasets',
          br(),
          DTOutput('dataset_table'),
          br(),
          uiOutput('selected_info'),
          uiOutput('ncbi_link_box'),
          uiOutput('doi_link_box')
        ),
        tabPanel('By publication',
          br(),
          fluidRow(
            column(3,
              wellPanel(
                h5('Filter by publication'),
                helpText('All datasets are shown by default. Add filters to narrow results.'),
                textInput('pub_author',
                  opt_label('Author name',
                    'Partial match — enter surname or first author. Example: "Bollenbach" or "Smith, J."'),
                  placeholder = 'e.g. Bollenbach, Mustonen'),
                fluidRow(
                  column(6, numericInput('pub_year_min',
                    opt_label('From year', 'Earliest publication year. Leave blank for no lower limit.'),
                    value = NULL, min = 1950, max = 2035, step = 1)),
                  column(6, numericInput('pub_year_max',
                    opt_label('To year', 'Latest publication year. Leave blank for no upper limit.'),
                    value = NULL, min = 1950, max = 2035, step = 1))
                ),
                textInput('pub_doi',
                  opt_label('DOI or URL fragment',
                    'Partial match on DOI or source URL. Example: "10.1038" finds all Nature papers. "plos" finds PLOS ONE papers.'),
                  placeholder = 'e.g. 10.1038/... or plos or biorxiv'),
                textInput('pub_device',
                  opt_label('Plate reader / device',
                    'Partial match on instrument name. Examples: "Biotek", "Synergy H1", "Tecan", "Infinite".'),
                  placeholder = 'e.g. Biotek, Tecan, Synergy'),
                actionButton('run_pub_search', 'Apply filters', width = '100%',
                             class = 'btn-primary'),
                br(),
                actionButton('clear_pub_search', 'Show all', width = '100%',
                             class = 'btn-default')
              )
            ),
            column(9,
              DTOutput('pub_search_table'),
              br(),
              uiOutput('pub_doi_links')
            )
          )
        ),
        tabPanel('Raw curves',
          br(),
          plotOutput('growth_plot', height = '460px'),
          br(),
          verbatimTextOutput('plate_qc_text')
        ),
        tabPanel('Metrics',
          br(),
          DTOutput('metrics_table')
        ),
        tabPanel('QC',
          br(),
          plotOutput('qc_cv_plot',      height = '300px'),
          br(),
          plotOutput('qc_heatmap_plot', height = '300px')
        )
      )
    )
  ),
  bottom_nav
)

# ── ANALYSE & COMPARE ─────────────────────────────────────────────────────────
ui_analyse <- fluidPage(
  theme = shinytheme('cerulean'),
  tags$head(tags$style(HTML('body { padding-bottom: 60px; }'))),
  fluidRow(
    column(4, tags$span(
  img(src = 'PhaMiG_logo.png',  height = '52px', alt = 'PhaMiG',
      style = 'margin-right:8px;'),
  img(src = 'QresDB_logo.png',  height = '52px', alt = 'Qres DB')
)),
    column(8, h3('Analyze & Compare Tools'))
  ),
  hr(),
  fluidRow(
    column(3,
      wellPanel(
        h5('Filter datasets'),
        selectizeInput('comp_species', 'Species', choices = NULL,
                       options = list(placeholder = 'all species')),
        sliderInput('comp_temp', 'Temperature range (°C)',
                    min = 0, max = 60, value = c(25, 45), step = 1),
        textInput('comp_medium', 'Medium contains',
                  placeholder = 'e.g. LB, MOPS, M9'),
        hr(),
        h5('Tools to compare'),
        checkboxGroupInput('comp_tools', NULL,
          choices  = c('model_free', 'gcplyr', 'growthrates',
                       'dgrothr', 'kinbiont', 'omniplate'),
          selected = c('model_free', 'gcplyr', 'growthrates')),
        selectInput('comp_metric', 'Primary metric',
          choices  = c('doubling_time_h', 'mumax_percapita', 'lag_time_h',
                       'auc_trapezoid', 'max_od', 'delta_od', 'asymptote_od',
                       'max_death_rate'),
          selected = 'doubling_time_h'),
        actionButton('run_comparison', 'Run comparison', width = '100%',
                     class = 'btn-success'),
        br(), br(),
        textOutput('comp_status')
      )
    ),
    column(9,
      tabsetPanel(
        tabPanel('Box plot',
          br(), plotOutput('comp_boxplot', height = '420px')),
        tabPanel('Growth curves (overlay)',
          br(),
          uiOutput('comp_curves_panel')),
        tabPanel('Metrics table',
          br(), DTOutput('comp_table')),
        tabPanel('Pairwise scatter',
          br(),
          fluidRow(
            column(6, selectInput('scatter_x', 'X tool', choices = NULL)),
            column(6, selectInput('scatter_y', 'Y tool', choices = NULL))
          ),
          plotOutput('comp_scatter', height = '380px'))
      )
    )
  ),
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
  uiOutput('nl_description_box'),
  uiOutput('nl_sql_box'),
  br(),
  tabsetPanel(id = 'nl_result_tabs',
    tabPanel('Results table',
      br(), DTOutput('nl_table')),
    tabPanel('Comparison plot',
      br(),
      selectInput('nl_metric_select', 'Metric to plot',
        choices  = c('doubling_time_h', 'mumax_percapita', 'lag_time_h',
                     'auc_trapezoid', 'max_od', 'delta_od', 'max_death_rate'),
        selected = 'doubling_time_h'),
      plotOutput('nl_comp_plot', height = '420px')),
    tabPanel('Growth curves',
      br(),
      uiOutput('nl_curves_panel'))
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

  selected_dataset  <- reactiveVal(NULL)
  dataset_results   <- reactiveVal(data.frame())
  comparison_result <- reactiveVal(data.frame())
  comp_raw_long     <- reactiveVal(NULL)

  # ── Auto-load on startup — show existing DB data without clicking Initialize
  session$onFlushed(function() {
    rows <- tryCatch({
      qres_migrate_schema(con)   # patch any missing columns silently
      qres_search_datasets(con)
    }, error = function(e) data.frame())
    if (nrow(rows) > 0) dataset_results(rows)
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

      species_list <- qres_get_distinct_species(con)
      updateSelectizeInput(session, 'comp_species',
                           choices = c('', species_list), server = TRUE)

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
  output$search_mode_badge <- renderUI({
    backend <- tryCatch(qres_llm_backend_name(), error = function(e) 'none')
    switch(backend,
      'anthropic' = tags$small(style = 'color:#5cb85c;', '● NL: Anthropic Claude'),
      'ollama'    = tags$small(style = 'color:#5cb85c;', '● NL: local Ollama'),
      tags$small(style = 'color:#888;', '● NL: rule-based (always on)')
    )
  })

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
    if (!is.null(notify_msg)) showNotification(notify_msg, type = 'message', duration = 4)
  }, ignoreInit = TRUE)

  output$dataset_table <- renderDT({
    df <- dataset_results()
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
                   'year', 'authors', 'has_raw', 'doi', 'raw_file', 'plate_reader')
    display <- display[, intersect(col_order, names(display)), drop = FALSE]

    # Columns hidden by default (accessible via column visibility button)
    hidden_idx <- which(names(display) %in% c('dataset_id', 'raw_file', 'plate_reader', 'doi')) - 1L

    datatable(display,
              selection  = 'single',
              extensions = 'Buttons',
              options    = list(
                pageLength  = 12,
                scrollX     = TRUE,
                scrollCollapse = TRUE,
                autoWidth   = FALSE,
                dom         = 'Bfrtip',
                buttons     = list(list(extend = 'colvis', text = 'Show/hide columns')),
                columnDefs  = list(
                  list(visible = FALSE, targets = hidden_idx),
                  list(width = '90px',  targets = which(names(display) == 'dataset_id') - 1L),
                  list(width = '80px',  targets = which(names(display) == 'species') - 1L),
                  list(width = '55px',  targets = which(names(display) == 'temperature_c') - 1L),
                  list(width = '110px', targets = which(names(display) == 'perturbation_type') - 1L),
                  list(width = '130px', targets = which(names(display) %in% c('medium_type','authors')) - 1L),
                  list(className = 'dt-center', targets = '_all')
                )
              ),
              rownames = FALSE)
  })

  # Download current search results as CSV
  output$download_search_ui <- renderUI({
    if (nrow(dataset_results()) > 0)
      downloadButton('download_search_csv', 'Download results (CSV)', class = 'btn-default')
  })

  output$download_search_csv <- downloadHandler(
    filename = function() paste0('phamig_search_', Sys.Date(), '.csv'),
    content  = function(file) write.csv(dataset_results(), file, row.names = FALSE)
  )

  observeEvent(input$dataset_table_rows_selected, {
    df  <- dataset_results()
    row <- input$dataset_table_rows_selected
    if (length(row) == 1 && nrow(df) >= row)
      selected_dataset(df$dataset_id[row])
  })

  output$selected_info <- renderUI({
    req(selected_dataset())
    tagList(
      tags$p(tags$strong('Selected dataset: '), selected_dataset()),
      actionButton('go_to_comparison', 'Open in Comparison',
                   class = 'btn-info btn-sm', icon = icon('chart-bar'))
    )
  })

  observeEvent(input$go_to_comparison, {
    did <- selected_dataset()
    df  <- dataset_results()
    sp  <- df$species[df$dataset_id == did][1]
    if (!is.na(sp) && nzchar(sp))
      updateSelectizeInput(session, 'comp_species', selected = sp)
    change_page('analyse')
  })

  # ── Raw curves ─────────────────────────────────────────────────────────────
  raw_long <- reactive({
    req(selected_dataset())
    tryCatch(
      qres_get_raw_curves(con, selected_dataset()),
      error = function(e) {
        validate(need(FALSE, paste('No raw measurements yet for', selected_dataset(),
                                   '— click "Initialize / refresh DB" first.')))
      }
    )
  })

  output$growth_plot <- renderPlot({
    did  <- selected_dataset()
    df   <- dataset_results()
    row  <- if (nrow(df) > 0) df[df$dataset_id == did, ][1, ] else NULL

    title <- if (!is.null(row) && nrow(row) > 0) {
      parts <- c(
        paste0(row$species %||% did,
               if (!is.na(row$strain %||% NA) && nzchar(row$strain %||% ''))
                 paste0(' (', row$strain, ')') else ''),
        if (!is.na(row$temperature_c %||% NA)) paste0(row$temperature_c, '°C') else NULL,
        if (!is.na(row$perturbation_type %||% NA) && row$perturbation_type != 'none')
          paste0(row$perturbation_type,
                 if (!is.na(row$perturbation %||% NA) && nzchar(row$perturbation %||% ''))
                   paste0(': ', row$perturbation) else '') else NULL,
        if (!is.na(row$medium_type %||% NA)) row$medium_type else NULL
      )
      paste('Raw curves —', paste(Filter(Negate(is.null), parts), collapse = ' | '))
    } else {
      paste('Raw curves —', did)
    }
    qres_plot_growth(raw_long(), log_y = input$log_y, title = title)
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
    req(selected_dataset())
    wm <- tryCatch(
      DBI::dbGetQuery(con,
        "SELECT DISTINCT species, taxon_id FROM well_metadata WHERE plate_id LIKE ? LIMIT 1",
        params = list(paste0(selected_dataset(), '%'))),
      error = function(e) NULL
    )
    if (is.null(wm) || nrow(wm) == 0) return(NULL)
    taxon_id <- wm$taxon_id[1]
    species  <- wm$species[1]
    if (!is.na(taxon_id) && nzchar(taxon_id) && !taxon_id %in% c('undefined', 'NA')) {
      ncbi_url <- paste0('https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=', taxon_id)
      tags$div(class = 'alert alert-info',
        tags$strong('NCBI Taxonomy: '),
        tags$em(species), ' — ',
        tags$a(href = ncbi_url, target = '_blank', paste0('TaxID ', taxon_id, ' ↗'))
      )
    } else if (!is.na(species) && nzchar(species)) {
      search_url <- paste0('https://www.ncbi.nlm.nih.gov/taxonomy/?term=',
                           utils::URLencode(species))
      tags$div(class = 'alert alert-warning',
        tags$strong('No NCBI TaxID stored. '),
        'Search NCBI: ',
        tags$a(href = search_url, target = '_blank', paste0(species, ' ↗'))
      )
    }
  })

  # ── DOI / PubMed link for selected dataset ────────────────────────────────
  output$doi_link_box <- renderUI({
    req(selected_dataset())
    d <- tryCatch(
      DBI::dbGetQuery(con,
        "SELECT doi, source_url, authors, year, title FROM dataset WHERE dataset_id = ? LIMIT 1",
        params = list(selected_dataset())),
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

  output$pub_search_table <- renderDT({
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
  output$qc_cv_plot <- renderPlot({
    req(raw_long())
    qres_plot_qc_cv(raw_long())
  })

  output$qc_heatmap_plot <- renderPlot({
    req(raw_long())
    qres_plot_endpoint_heatmap(raw_long())
  })

  # ── Comparison (multi-dataset, multi-tool) ─────────────────────────────────
  observeEvent(input$run_comparison, {
    req(length(input$comp_tools) >= 1)

    withProgress(message = 'Running comparison…', {
      filtered <- qres_filter_datasets(
        con,
        species         = if (nzchar(input$comp_species %||% '')) input$comp_species else NULL,
        temp_min        = input$comp_temp[1],
        temp_max        = input$comp_temp[2],
        medium          = if (nzchar(input$comp_medium %||% '')) input$comp_medium else NULL,
        registered_only = TRUE
      )

      if (nrow(filtered) == 0) {
        output$comp_status <- renderText('No registered datasets match the filter.')
        comparison_result(data.frame())
        return()
      }

      output$comp_status <- renderText(
        paste('Found', nrow(filtered), 'datasets. Running', length(input$comp_tools), 'tools…')
      )

      # Build human-readable label: "E. coli 37°C" or "E. coli 37°C + rifampicin"
      .mk_label <- function(sp, tc, pert) {
        lab  <- paste0(if (nzchar(sp %||% '')) sp else '?', ' ', round(tc, 1), '°C')
        pert <- trimws(pert %||% '')
        if (nzchar(pert) && !tolower(pert) %in% c('none', 'na')) lab <- paste0(lab, ' + ', pert)
        lab
      }
      label_map <- setNames(
        mapply(.mk_label, filtered$species, filtered$temperature_c, filtered$perturbation %||% ''),
        filtered$dataset_id
      )

      all_raw    <- list()
      all_metrics <- lapply(seq_len(nrow(filtered)), function(i) {
        did   <- filtered$dataset_id[i]
        tc    <- filtered$temperature_c[i]
        label <- label_map[[did]]
        setProgress(i / nrow(filtered), detail = paste('Processing', label, '…'))

        raw <- tryCatch(qres_get_raw_curves(con, did), error = function(e) NULL)
        if (is.null(raw)) return(NULL)

        raw$temperature_c <- tc
        raw$plot_label    <- label
        all_raw[[did]] <<- raw

        cmp <- qres_compare_plugin_outputs(raw, plugins = input$comp_tools)
        if (nrow(cmp) > 0) {
          cmp$temperature_c <- tc
          cmp$dataset_id    <- cmp$dataset_id %||% did
          cmp$plot_label    <- label
        }
        cmp
      })

      combined <- dplyr::bind_rows(Filter(Negate(is.null), all_metrics))
      comparison_result(combined)
      comp_raw_long(dplyr::bind_rows(all_raw))

      output$comp_status <- renderText(
        paste('Done.', nrow(combined), 'metric rows across',
              length(unique(combined$dataset_id)), 'datasets.')
      )

      avail_tools <- unique(combined$tool)
      updateSelectInput(session, 'scatter_x', choices = avail_tools, selected = avail_tools[1])
      updateSelectInput(session, 'scatter_y', choices = avail_tools,
                        selected = avail_tools[min(2, length(avail_tools))])
    })
  })

  output$comp_boxplot <- renderPlot({
    df <- comparison_result()
    if (is.null(df) || nrow(df) == 0) {
      return(ggplot2::ggplot() +
        ggplot2::annotate('text', x = 0.5, y = 0.5, size = 5, colour = 'grey50',
                          label = 'Click "Run comparison" to populate the plot.') +
        ggplot2::theme_void())
    }
    facet_col <- if ('plot_label' %in% names(df)) 'plot_label' else 'dataset_id'
    qres_plot_metric_comparison(df, metric = input$comp_metric,
                                 facet_by = if (length(unique(df[[facet_col]])) > 1)
                                   facet_col else NULL)
  })

  output$comp_curves_panel <- renderUI({
    raw <- comp_raw_long()
    if (is.null(raw) || nrow(raw) == 0) return(NULL)
    tagList(
      checkboxInput('comp_log_y', 'Log y', FALSE),
      plotOutput('comp_curves', height = '420px')
    )
  })

  output$comp_curves <- renderPlot({
    raw <- comp_raw_long()
    req(!is.null(raw))
    colour_col <- if ('plot_label' %in% names(raw)) 'plot_label' else 'dataset_id'
    qres_plot_growth(raw, log_y = input$comp_log_y %||% FALSE,
                     colour_by = colour_col,
                     title = NULL)
  })

  output$comp_table <- renderDT({
    req(nrow(comparison_result()) > 0)
    df  <- comparison_result()
    hide <- which(names(df) %in% c('dataset_id', 'model_name')) - 1L
    datatable(df, rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE,
                             columnDefs = list(list(visible = FALSE, targets = hide))))
  })

  output$comp_scatter <- renderPlot({
    req(nrow(comparison_result()) > 0, input$scatter_x, input$scatter_y)
    qres_plot_tool_scatter(comparison_result(),
                            tool_x = input$scatter_x,
                            tool_y = input$scatter_y,
                            metric = input$comp_metric)
  })

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
    ptype <- input$meta_perturb_type
    plate$perturbation_type <- ptype

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
        eligible <- which(!plate$is_blank & !plate$is_control)
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
      eligible <- which(!plate$is_blank & !plate$is_control)
      if (length(muts) > 0 && length(eligible) > 0) {
        well_muts <- muts[((seq_along(eligible) - 1L) %% length(muts)) + 1L]
        plate$perturbation[eligible] <- if (nzchar(drug))
          paste0(drug, ';', well_muts) else well_muts
      }

    } else if (ptype == 'temperature') {
      # Temperature IS the perturbation — no chemical concentration
      plate$concentration <- 0

      # Fill temperatures only into non-blank, non-control wells, in reading order
      eligible <- which(!plate$is_blank & !plate$is_control)
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
    # Replicate sequence: cycle across all wells that are not blank and not control
    rep_labels <- .parse_replicate_seq(input$meta_replicate_seq %||% '')
    if (length(rep_labels) > 0) {
      eligible <- which(!plate$is_blank & !plate$is_control)
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
    non_empty <- vapply(plate, function(col)
      !all(is.na(col) | col == '' | isFALSE(col)), logical(1))
    non_empty['well'] <- TRUE
    datatable(plate[, non_empty, drop = FALSE],
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

    raw_path <- input$upload_raw_csv$datapath

    # Pre-flight: validate raw CSV format before any DB write
    output$upload_progress_ui <- renderUI(
      helpText(icon('spinner'), ' Validating raw CSV format…'))

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
  observeEvent(input$init_db, {
    updateSelectInput(session, 'note_dataset',
      choices = c('general / not dataset-specific' = '',
                  setNames(dataset_results()$dataset_id, dataset_results()$dataset_id)))
  })

  observeEvent(dataset_results(), {
    df <- dataset_results()
    if (nrow(df) > 0) {
      updateSelectInput(session, 'note_dataset',
        choices = c('general / not dataset-specific' = '',
                    setNames(df$dataset_id, df$dataset_id)))
    }
  })

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
  nl_result <- reactiveVal(NULL)

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

  observeEvent(input$nl_submit, {
    q <- trimws(input$nl_question %||% '')
    req(nzchar(q))

    withProgress(message = 'Interpreting query…', {

      # ── Step 1: Rule-based parser (always works, no key needed) ──────────
      setProgress(0.2, detail = 'Checking patterns…')
      parsed <- tryCatch(qres_parse_nl_query(q), error = function(e) NULL)

      if (!is.null(parsed) && length(parsed) > 0) {
        rows <- tryCatch(
          do.call(qres_filter_datasets, c(list(con = con, registered_only = FALSE), parsed)),
          error = function(e) NULL
        )
        if (!is.null(rows) && nrow(rows) > 0) {
          nl_result(list(
            type        = 'table',
            data        = rows,
            description = paste0('Rule-based: ', paste(names(parsed), collapse = ', ')),
            sql         = NULL,
            plot_type   = 'table'
          ))
          return()
        }
      }

      # ── Step 2: LLM (ellmer — anthropic or ollama) ────────────────────────
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
            nl_result(list(
              type        = 'table',
              data        = rows,
              description = paste0('LLM (', qres_llm_backend_name(), '): ',
                                   paste(names(filters), collapse = ', ')),
              sql         = NULL,
              plot_type   = 'table'
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

  output$nl_table <- renderDT({
    req(nl_result())
    df <- nl_result()$data
    if (is.null(df) || nrow(df) == 0) return(datatable(data.frame(message = 'No results.')))
    display_cols <- intersect(c('species', 'strain', 'medium_type', 'perturbation_type',
                                 'perturbation', 'temperature_c', 'year', 'authors', 'doi'),
                              names(df))
    datatable(df[, display_cols, drop = FALSE],
              rownames = FALSE,
              options  = list(pageLength = 20, scrollX = TRUE))
  })

  output$nl_comp_plot <- renderPlot({
    req(nl_result())
    res    <- nl_result()
    metric <- input$nl_metric_select %||% 'doubling_time_h'

    # LLM-returned comparison result
    if (res$type == 'comparison' && !is.null(res$data) && nrow(res$data) > 0) {
      return(qres_plot_metric_comparison(res$data, metric = metric,
               facet_by = if (length(unique(res$data$dataset_id)) > 1) 'dataset_id' else NULL))
    }

    # Rule-based table result: auto-run comparison on returned datasets (cap 5)
    if (res$type == 'table' && !is.null(res$data) &&
        'dataset_id' %in% names(res$data) && nrow(res$data) > 0) {
      ids <- unique(res$data$dataset_id)
      if ('raw_path' %in% names(res$data))
        ids <- unique(res$data$dataset_id[!is.na(res$data$raw_path)])
      ids <- head(ids, 5)
      if (length(ids) == 0) {
        return(ggplot2::ggplot() +
          ggplot2::annotate('text', x = .5, y = .5, size = 4.5, colour = 'grey50',
            label = 'No raw data registered for these datasets.') +
          ggplot2::theme_void())
      }
      # Build label map from res$data (has species, temperature_c, perturbation)
      .nl_label <- function(did) {
        row  <- res$data[res$data$dataset_id == did, ][1, ]
        sp   <- row$species %||% ''
        tc   <- row$temperature_c %||% NA
        pert <- trimws(row$perturbation %||% '')
        lab  <- paste0(if (nzchar(sp)) sp else did,
                       if (is.finite(as.numeric(tc))) paste0(' ', round(as.numeric(tc), 1), '°C') else '')
        if (nzchar(pert) && !tolower(pert) %in% c('none', 'na')) lab <- paste0(lab, ' + ', pert)
        lab
      }
      all_cmp <- lapply(ids, function(did) {
        raw <- tryCatch(qres_get_raw_curves(con, did), error = function(e) NULL)
        if (is.null(raw)) return(NULL)
        cmp <- tryCatch(
          qres_compare_plugin_outputs(raw,
            plugins = input$comp_tools %||% c('model_free', 'gcplyr', 'growthrates')),
          error = function(e) NULL)
        if (!is.null(cmp) && nrow(cmp) > 0) {
          cmp$dataset_id <- did
          cmp$plot_label <- .nl_label(did)
        }
        cmp
      })
      combined <- dplyr::bind_rows(Filter(Negate(is.null), all_cmp))
      if (nrow(combined) == 0) {
        return(ggplot2::ggplot() +
          ggplot2::annotate('text', x = .5, y = .5, size = 4.5, colour = 'grey50',
            label = 'No metrics computed for these datasets.') +
          ggplot2::theme_void())
      }
      facet_col <- if ('plot_label' %in% names(combined)) 'plot_label' else 'dataset_id'
      return(qres_plot_metric_comparison(combined, metric = metric,
               facet_by = if (length(unique(combined[[facet_col]])) > 1) facet_col else NULL))
    }

    ggplot2::ggplot() +
      ggplot2::annotate('text', x = .5, y = .5, size = 5, colour = 'grey50',
                        label = 'Run a query to populate the comparison plot.') +
      ggplot2::theme_void()
  })

  output$nl_curves_panel <- renderUI({
    res <- nl_result()
    if (is.null(res)) return(NULL)
    has_ids <- (res$type == 'comparison' && !is.null(res$filtered)) ||
               (res$type == 'table' && !is.null(res$data) &&
                'dataset_id' %in% names(res$data) && nrow(res$data) > 0)
    if (!has_ids) return(NULL)
    tagList(
      checkboxInput('nl_log_y', 'Log y', FALSE),
      plotOutput('nl_curves_plot', height = '420px')
    )
  })

  output$nl_curves_plot <- renderPlot({
    req(nl_result())
    res <- nl_result()

    ids <- if (res$type == 'comparison' && !is.null(res$filtered)) {
      res$filtered$dataset_id
    } else if (res$type == 'table' && !is.null(res$data) &&
               'dataset_id' %in% names(res$data) && nrow(res$data) > 0) {
      head(res$data$dataset_id, 8)
    } else {
      NULL
    }
    if (is.null(ids) || length(ids) == 0) return(NULL)

    raw_list <- lapply(ids, function(did) {
      df <- tryCatch(qres_get_raw_curves(con, did), error = function(e) NULL)
      if (!is.null(df) && nrow(df) > 0) {
        df$dataset_id <- did
        # Attach label from NL result metadata if available
        if ('dataset_id' %in% names(res$data)) {
          row  <- res$data[res$data$dataset_id == did, ][1, ]
          sp   <- trimws(row$species %||% '')
          tc   <- row$temperature_c %||% NA
          pert <- trimws(row$perturbation %||% '')
          lab  <- paste0(if (nzchar(sp)) sp else did,
                         if (is.finite(as.numeric(tc))) paste0(' ', round(as.numeric(tc), 1), '°C') else '')
          if (nzchar(pert) && !tolower(pert) %in% c('none', 'na')) lab <- paste0(lab, ' + ', pert)
          df$plot_label <- lab
        }
        df
      }
    })
    combined <- dplyr::bind_rows(Filter(Negate(is.null), raw_list))
    if (nrow(combined) == 0) return(NULL)

    colour_col <- if ('plot_label' %in% names(combined)) 'plot_label' else 'dataset_id'
    qres_plot_growth(combined, log_y = input$nl_log_y %||% FALSE,
                     colour_by = colour_col, title = NULL)
  })
}

shinyApp(ui, server)
