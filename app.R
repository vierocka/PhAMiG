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

# ── Shared bottom navigation bar ─────────────────────────────────────────────
bottom_nav <- tagList(
  tags$head(tags$style(HTML("
    .bottom-fixed {
      position: fixed; bottom: 0; width: 100%;
      background-color: #f8f8f8; border-top: 1px solid #ddd;
      padding: 8px 0; z-index: 1000; text-align: center;
    }
    .pictogram-caption { font-weight: bold; margin-top: 6px; }
    .pictogram-cell img:hover { opacity: 0.8; transform: scale(1.05);
      transition: all 0.2s ease; }
    .list-group-item { border-radius: 4px !important; margin-bottom: 4px; }
  "))),
  tags$div(
    class = 'bottom-fixed',
    tags$a(href = route_link('/'), icon('home'), ' Home',
           style = 'font-size:15px; text-decoration:none; color:#337ab7;')
  )
)

# ── HOME PAGE ─────────────────────────────────────────────────────────────────
ui_home <- fluidPage(
  theme = shinytheme('cerulean'),
  tags$head(tags$style(HTML("
    .pictogram-cell img:hover { opacity: 0.8; transform: scale(1.05);
      transition: all 0.2s ease; cursor: pointer; }
    body { padding-bottom: 60px; }
  "))),
  br(), br(),
  fluidRow(
    column(12, align = 'center',
      img(src = 'Qres_DB_logo.png', height = '180px'),
      h2('PHenotypic Analysis of Microbial Growth (PhaMiG)'),
      p(style = 'color:#666; max-width:700px; margin:auto;',
        'A DuckDB-powered database for microbial growth curve analysis — bacteria, yeast, and beyond.',
        'Compare gcplyr, growthrates, DGrowthR, KinBiont.jl and more on the same raw data.'),
      br(), br()
    )
  ),

  # Row 1: Search | Metadata Preparation | Analyse
  fluidRow(
    column(4, align = 'center',
      tags$a(href = route_link('search'),
        tags$div(class = 'pictogram-cell',
          img(src = 'search_parse2.png', height = '160px', width = '160px'),
          tags$p(class = 'pictogram-caption', 'SEARCH & BROWSE')
        )
      )
    ),
    column(4, align = 'center',
      tags$a(href = route_link('metadata_prep'),
        tags$div(class = 'pictogram-cell',
          img(src = 'metadata_prep2.png', height = '160px', width = '160px'),
          tags$p(class = 'pictogram-caption', 'METADATA PREPARATION')
        )
      )
    ),
    column(4, align = 'center',
      tags$a(href = route_link('analyse'),
        tags$div(class = 'pictogram-cell',
          img(src = 'data_analysis2.png', height = '160px', width = '160px'),
          tags$p(class = 'pictogram-caption', 'ANALYSE & COMPARE')
        )
      )
    )
  ),

  br(), br(),

  # Row 2: Submit | Develop | Contact/Ask
  fluidRow(
    column(4, align = 'center',
      tags$a(href = route_link('submit'),
        tags$div(class = 'pictogram-cell',
          img(src = 'submit2.png', height = '160px', width = '160px'),
          tags$p(class = 'pictogram-caption', 'SUBMIT DATA')
        )
      )
    ),
    column(4, align = 'center',
      tags$a(href = route_link('plugins'),
        tags$div(class = 'pictogram-cell',
          img(src = 'project_development2.png', height = '160px', width = '160px'),
          tags$p(class = 'pictogram-caption', 'DEVELOP & PLUGINS')
        )
      )
    ),
    column(4, align = 'center',
      tags$a(href = route_link('ask'),
        tags$div(class = 'pictogram-cell',
          img(src = 'contact2.png', height = '160px', width = '160px'),
          tags$p(class = 'pictogram-caption', 'CONTACT & NL QUERY')
        )
      )
    )
  ),

  br(), br(),

  # Row 3: Acknowledgements
  fluidRow(
    column(12, align = 'center',
      tags$a(href = route_link('acknowledgement'),
        tags$div(class = 'pictogram-cell',
          img(src = 'acknowledgement.png', height = '140px', width = '140px'),
          tags$p(class = 'pictogram-caption', 'ACKNOWLEDGEMENTS')
        )
      )
    )
  ),

  br(), br(), br(),
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
    column(4, img(src = 'Qres_DB_logo.png', height = '80px')),
    column(8,
      h3('Metadata Template Preparation'),
      tags$div(class = 'key-box',
        tags$strong('How metadata links to raw data:'), tags$br(),
        tags$code('dataset_id'), ' (unique key) → ',
        tags$code('raw_file'), ' (CSV with time + OD columns)', tags$br(),
        'Both must be provided when submitting data. The plate template below is the ',
        tags$em('per-well'), ' breakdown of conditions within one raw CSV.',
        tags$br(),
        tags$span(class = 'required-star', '★'), ' = required field'
      )
    )
  ),
  hr(),
  sidebarLayout(
    sidebarPanel(width = 3,

      # ── REQUIRED block ───────────────────────────────────────────────────
      tags$p(class = 'required-label',
        tags$span(class = 'required-star', '★ '), 'Required fields'),

      selectInput('plateSize', tags$span(class='required-label', 'Plate format ★'),
                  choices = c('96-well (8×12)' = '8x12',
                              '384-well (16×24)' = '16x24',
                              '48-well (6×8)'  = '6x8',
                              '24-well (4×6)'  = '4x6')),

      textInput('meta_species',
        tags$span(class='required-label', 'Species ★'),
        placeholder = 'Escherichia coli'),

      textInput('meta_medium',
        tags$span(class='required-label', 'Medium type ★'),
        placeholder = 'LB, MOPS + 3g/L glucose, M9 + glucose…'),

      tags$label(class='required-label', 'Perturbation type ★'),
      radioButtons('meta_perturb_type', label = NULL,
        choices = c('Chemical stressor' = 'chemical',
                    'Genetic modification' = 'genetic',
                    'Temperature gradient' = 'temperature',
                    'None (replicates only)' = 'none'),
        selected = 'none'),

      tags$hr(),

      # ── Temperature (required under chemical/genetic/none) ────────────
      conditionalPanel("input.meta_perturb_type == 'chemical'",
        textInput('meta_temp_chem',
          tags$span(class='required-label', 'Temperature (°C) ★'), placeholder = '37'),
        textInput('meta_perturb',
          tags$span(class='required-label', 'Perturbation name ★'), placeholder = 'rifampicin'),
        textInput('meta_conc', 'Starting concentration', placeholder = '50 mg/L'),
        checkboxInput('meta_dilution', 'Serial dilution along columns'),
        conditionalPanel('input.meta_dilution == true',
          textInput('meta_ratio', 'Dilution ratio', placeholder = '1:2'))
      ),
      conditionalPanel("input.meta_perturb_type == 'genetic'",
        textInput('meta_temp_gen',
          tags$span(class='required-label', 'Temperature (°C) ★'), placeholder = '37'),
        textInput('meta_genetic_bg',
          tags$span(class='required-label', 'Genetic background ★'),
          placeholder = 'ΔtrpB  (comma-separate multiple)')
      ),
      conditionalPanel("input.meta_perturb_type == 'temperature'",
        textInput('meta_temp_start',
          tags$span(class='required-label', 'Starting temperature (°C) ★'), placeholder = '30'),
        textInput('meta_temp_step',
          'Step per column (°C)', placeholder = '1  (leave blank = same temp all wells)')
      ),
      conditionalPanel("input.meta_perturb_type == 'none'",
        textInput('meta_temp_none',
          tags$span(class='required-label', 'Temperature (°C) ★'), placeholder = '37')
      ),

      tags$hr(),

      # ── OPTIONAL block ───────────────────────────────────────────────────
      tags$p(class = 'optional-label', 'Optional fields'),

      checkboxInput('meta_use_strain', 'Specify strain'),
      conditionalPanel('input.meta_use_strain == true',
        textInput('meta_strain', tags$span(class='optional-label','Strain'),
                  placeholder = 'MG1655')),

      checkboxInput('meta_medium_mod', 'Medium modification'),
      conditionalPanel('input.meta_medium_mod == true',
        textInput('meta_medium_mod_val', tags$span(class='optional-label','Modification'),
                  placeholder = '+ 50 g/L glucose'),
        checkboxInput('meta_medium_dilution', 'Serial dilution along columns'),
        conditionalPanel('input.meta_medium_dilution == true',
          textInput('meta_medium_ratio', tags$span(class='optional-label','Dilution ratio'),
                    placeholder = '1:2'))
      ),

      checkboxInput('meta_control_col', 'Last column = no-perturbation control'),
      textInput('meta_blank_wells',
        tags$span(class='optional-label', 'Blank wells (comma-separated)'),
        placeholder = 'A1, H12'),

      tags$hr(),
      actionButton('applyMetadata', 'Apply & preview',
                   class = 'btn-primary', width = '100%'),
      br(), br(),
      downloadButton('downloadTemplate', 'Download plate template CSV',
                     class = 'btn-success')
    ),

    mainPanel(width = 9,
      tabsetPanel(
        tabPanel('Plate preview',
          br(),
          DTOutput('plateMatrix'),
          br(),
          helpText('Only columns containing at least one value are shown.',
                   'Empty columns = not yet applied.')),

        tabPanel('Field reference',
          br(),
          fluidRow(
            column(6,
              wellPanel(
                tags$h5(tags$span(class='required-star','★'), ' Required fields (6)'),
                tags$table(class='table table-condensed table-bordered',
                  tags$thead(tags$tr(tags$th('Column'), tags$th('Description'), tags$th('Example'))),
                  tags$tbody(
                    tags$tr(tags$td(tags$code('dataset_id')), tags$td('Unique key — set in Submit page'), tags$td('ecoli_LB_37C_2023')),
                    tags$tr(tags$td(tags$code('raw_file')),   tags$td('Raw CSV filename (time + OD)'), tags$td('plate01_run3.csv')),
                    tags$tr(tags$td(tags$code('species')),    tags$td('Organism name'), tags$td('Escherichia coli')),
                    tags$tr(tags$td(tags$code('medium_type')),tags$td('Growth medium'), tags$td('MOPS + 3g/L glucose')),
                    tags$tr(tags$td(tags$code('temperature_c')), tags$td('Measurement temperature °C'), tags$td('37')),
                    tags$tr(tags$td(tags$code('perturbation_type')), tags$td('chemical | genetic | temperature | none'), tags$td('chemical'))
                  )
                )
              )
            ),
            column(6,
              wellPanel(
                tags$h5(tags$span(class='optional-label', 'Optional fields (16)')),
                tags$table(class='table table-condensed',
                  tags$thead(tags$tr(tags$th('Column'), tags$th('Example'))),
                  tags$tbody(
                    tags$tr(tags$td(tags$code('perturbation')),       tags$td('rifampicin / 37 / ΔtrpB')),
                    tags$tr(tags$td(tags$code('strain')),             tags$td('K12 NCM3722')),
                    tags$tr(tags$td(tags$code('taxon_id')),           tags$td('562')),
                    tags$tr(tags$td(tags$code('medium_state')),       tags$td('liquid')),
                    tags$tr(tags$td(tags$code('concentration')),      tags$td('50')),
                    tags$tr(tags$td(tags$code('concentration_unit')), tags$td('mg/L')),
                    tags$tr(tags$td(tags$code('plate_format')),       tags$td('96')),
                    tags$tr(tags$td(tags$code('plate_reader')),       tags$td('Biotek Synergy H1')),
                    tags$tr(tags$td(tags$code('wavelength_nm')),      tags$td('600')),
                    tags$tr(tags$td(tags$code('shake_freq_rpm')),     tags$td('150')),
                    tags$tr(tags$td(tags$code('spectrophotometer')),  tags$td('OD600')),
                    tags$tr(tags$td(tags$code('time_unit')),          tags$td('h')),
                    tags$tr(tags$td(tags$code('authors')),            tags$td('Smith, J.; Doe, A.')),
                    tags$tr(tags$td(tags$code('year')),               tags$td('2024')),
                    tags$tr(tags$td(tags$code('doi')),                tags$td('10.1234/...')),
                    tags$tr(tags$td(tags$code('notes')),              tags$td('free text'))
                  )
                )
              )
            )
          ),
          wellPanel(
            tags$h5('Workflow'),
            tags$ol(
              tags$li(tags$strong('Metadata Preparation page:'), ' fill the form → Apply & preview → download the plate template CSV (per-well breakdown).'),
              tags$li(tags$strong('Submit page → Upload raw data:'), ' provide dataset_id ★, upload the raw CSV (time + OD) and the plate template CSV together.'),
              tags$li('The app links them via dataset_id → stores everything in DuckDB as Parquet.'),
              tags$li('Search, analyse, and download from the Search and Analyse pages.')
            ),
            tags$p(tags$strong('Raw data CSV format:'), ' column 1 = time (in time_unit, default hours);',
                   ' columns 2–N = OD values, header = well names (A1, A2, …, H12).')
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
      img(src = 'Qres_DB_logo.png', height = '80px')),
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
        tagList('Plugin', tip('model_free: fast, no model assumptions. gcplyr/growthrates: standard R packages. DGrowthR: Gaussian process. KinBiont.jl: Julia (needs Julia 1.11). omniplate: Python via reticulate (needs conda env omniplate_env).')),
        choices = c('model_free', 'gcplyr', 'growthrates',
                    'dgrothr', 'kinbiont', 'omniplate')),
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
    column(4, img(src = 'Qres_DB_logo.png', height = '80px')),
    column(8, h3('Analyse & Compare Tools'))
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
                       'auc_trapezoid', 'max_od', 'delta_od', 'asymptote_od'),
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
          checkboxInput('comp_log_y', 'Log y', FALSE),
          plotOutput('comp_curves', height = '420px')),
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
    column(4, img(src = 'Qres_DB_logo.png', height = '80px')),
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
        choices  = c('mumax_percapita', 'lag_time_h', 'doubling_time_h',
                     'auc_trapezoid', 'max_od', 'delta_od'),
        selected = 'mumax_percapita'),
      plotOutput('nl_comp_plot', height = '420px')),
    tabPanel('Growth curves',
      br(),
      checkboxInput('nl_log_y', 'Log y', FALSE),
      plotOutput('nl_curves_plot', height = '420px'))
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
  '))),
  fluidRow(
    column(4, img(src = 'Qres_DB_logo.png', height = '80px')),
    column(8, h3('Submit Data & Manage Deposits'))
  ),
  hr(),
  tabsetPanel(
    # ── TAB 1: Upload ───────────────────────────────────────────────────────
    tabPanel('Upload raw data',
      br(),
      tags$div(class = 'key-box',
        tags$strong('Unique key chain: '),
        tags$code('dataset_id'), ' → all metadata tables → Parquet raw data → analysis results.',
        tags$br(),
        tags$span(class = 'required-star', '★'), ' = required.  ',
        tags$span(class = 'optional-label', '○ = optional.'),
        '  Visit ', tags$strong('Metadata Preparation'), ' first to generate the plate template CSV.',
        tags$br(),
        tags$strong('Steps: '),
        '① fill required fields  ② choose release mode  ③ click Register'
      ),
      fluidRow(
        column(5,
          # STEP 1: Files
          wellPanel(
            tags$p(class = 'step-label', '① Files'),
            fileInput('upload_raw_csv',
              req_label('Raw data CSV',
                'CSV file from your plate reader. Column 1 must be named "time" (values in hours by default). Columns 2–N are OD values; headers = well names A1, A2 … H12. At least 5 time points and 2 wells required.'),
              accept = '.csv', buttonLabel = 'Browse…', placeholder = 'No file selected'),
            fileInput('upload_meta_csv',
              opt_label('Plate template CSV',
                'Generated on the Metadata Preparation page. Describes per-well conditions (species, medium, concentration). Without it, one summary row is created for the whole plate.'),
              accept = '.csv', buttonLabel = 'Browse…', placeholder = 'No file selected (optional)'),

            # STEP 2: Biological metadata
            tags$p(class = 'step-label', '② Dataset metadata'),
            textInput('upload_dataset_id',
              req_label('Dataset ID',
                'Unique identifier linking all tables. Allowed characters: letters, digits, underscore, hyphen, dot. Max 100 characters. Example: ecoli_MOPS_37C_2026_smith'),
              placeholder = 'ecoli_MOPS_37C_2026_smith', value = ''),
            fluidRow(
              column(6, selectInput('upload_perturb_type',
                req_label('Perturbation type',
                  'The main experimental variable. Use "temperature" when you grew bacteria at different temperatures. Use "chemical" for drugs, antibiotics. "genetic" for mutants. "none" for untreated controls.'),
                choices = c('none', 'chemical', 'genetic', 'temperature'))),
              column(6, textInput('upload_temp_c',
                req_label('Temperature (°C)',
                  'Measurement temperature in degrees Celsius. Valid range: -5 to 105. Example: 37'),
                placeholder = '37'))
            ),
            textInput('upload_species',
              req_label('Species',
                'Scientific or common name. Max 120 characters. Examples: "Escherichia coli", "Saccharomyces cerevisiae", "Bacillus subtilis"'),
              placeholder = 'Escherichia coli'),
            textInput('upload_medium',
              req_label('Medium type',
                'Growth medium full description. Max 200 characters. Examples: "LB", "MOPS + 3 g/L glucose", "YPD", "Modified Mueller-Hinton broth pH 7.4"'),
              placeholder = 'MOPS + 3 g/L glucose'),

            # Publication (optional)
            tags$p(class = 'step-label', '○ Publication info (optional)'),
            textInput('upload_authors',
              opt_label('Authors',
                'Author list as it appears in the paper. Semicolon-separated. Max 500 characters. Example: "Smith, J.; Doe, A.; Brown, C."'),
              placeholder = 'Smith, J.; Doe, A.'),
            fluidRow(
              column(5, numericInput('upload_year',
                opt_label('Year', 'Year of publication or data collection. Range 1950–2035.'),
                value = as.integer(format(Sys.Date(), '%Y')), min = 1950, max = 2035)),
              column(7, textInput('upload_doi',
                opt_label('DOI', 'Digital Object Identifier. Must start with "10." Example: 10.1038/nature12345. Max 200 characters.'),
                placeholder = '10.1234/journal.xxx'))
            ),
            textAreaInput('upload_notes',
              opt_label('Notes', 'Free-text notes about this dataset. Max 1000 characters.'),
              rows = 2, placeholder = 'e.g. Outlier wells removed; temperature drifted ±0.5°C'),

            # STEP 3: Embargo / access control
            tags$p(class = 'step-label', '③ Data access & release'),
            tags$div(class = 'embargo-panel',
              helpText(icon('lock'), ' Unpublished data can be stored privately until you are ready to release it. You always need your email + access key + passphrase to delete your dataset.'),
              radioButtons('upload_release_mode', NULL,
                choices = c(
                  'immediate'  = 'immediate',
                  'Set a release date'  = 'date',
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
              textInput('upload_email',
                req_label('Your email',
                  'Needed to verify your identity for deletion. Not stored in plain text beyond the embargo table. Max 200 characters.'),
                placeholder = 'yourname@institution.edu'),
              passwordInput('upload_passphrase',
                req_label('Choose a passphrase (≥8 characters)',
                  'A phrase you will remember — NOT your password. Example: the name of the organism you study. It is hashed (SHA-256) before storage; we never see the plain text. You need it later to delete your dataset.'),
                placeholder = '≥ 8 characters, e.g. Escherichia2026'),
              passwordInput('upload_passphrase2',
                req_label('Confirm passphrase',
                  'Re-enter your passphrase to confirm. It must match exactly.'),
                placeholder = 'repeat your passphrase')
            ),

            br(),
            checkboxInput('upload_run_analysis',
              tagList('Run quick analysis (model_free) after upload',
                      tip('Automatically estimates doubling time, mumax, and AUC for all wells using the model-free method. Results appear in the Analyse tab. Takes 5–30 seconds depending on plate size.')),
              value = TRUE),
            uiOutput('upload_progress_ui'),
            actionButton('run_upload', tags$span(icon('upload'), ' Register in database'),
                         class = 'btn-primary btn-lg', width = '100%')
          )
        ),
        column(7,
          uiOutput('upload_key_display'),
          br(),
          h5('Upload status'),
          verbatimTextOutput('upload_status'),
          br(),
          h5('My registered datasets'),
          helpText('Shows all your datasets. Embargoed datasets are only visible to you here, not in Search.'),
          DTOutput('recent_datasets_table'),
          br(),
          downloadButton('download_all_meta', 'Download all metadata (CSV)',
                         class = 'btn-default')
        )
      )
    ),

    # ── TAB 2: Manage my data ───────────────────────────────────────────────
    tabPanel(tags$span(icon('key'), ' Manage my data'),
      br(),
      fluidRow(
        column(6,
          tags$div(class = 'manage-panel',
            h4(icon('trash'), ' Delete a dataset'),
            helpText('All three factors must match the values you provided at upload time. Deletion is permanent and removes raw data, metadata, and all analysis results.'),
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
            h4(icon('search'), ' Check release status'),
            helpText('Enter your email to see all your datasets and their current release status.'),
            textInput('status_email', 'Your email', placeholder = 'yourname@institution.edu'),
            actionButton('run_status_check', 'Show my datasets', class = 'btn-info', width = '100%'),
            br(), br(),
            DTOutput('my_datasets_table')
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
      br(),
      fluidRow(
        column(6,
          wellPanel(
            h5('Submit a note, limitation, or bug observation'),
            helpText('Stored in the local database and reviewed by the maintainer.'),
            selectInput('note_dataset', 'Linked dataset (optional)',
                        choices = c('general / not dataset-specific' = ''),
                        width = '100%'),
            selectInput('note_type', 'Type',
                        choices = c('bug', 'limitation', 'observation',
                                    'data_quality', 'feature_request'),
                        width = '100%'),
            textInput('note_author',
              opt_label('Your name or email', 'Optional. Used only to follow up if needed.'),
              width = '100%'),
            textAreaInput('note_text', 'Description', rows = 5, width = '100%',
                          placeholder = 'Describe the issue, limitation, or observation…'),
            actionButton('submit_note', 'Submit note', class = 'btn-warning', width = '100%')
          )
        ),
        column(6,
          h5('Submitted notes'),
          DTOutput('notes_table')
        )
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
    column(4, img(src = 'Qres_DB_logo.png', height = '80px')),
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
    column(4, img(src = 'Qres_DB_logo.png', height = '80px')),
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

    # Sidebar perturbation overrides (always applied on top of any parsed result)
    perturb_type <- if (nzchar(input$search_perturb_type %||% '') &&
                        input$search_perturb_type != 'all')
      input$search_perturb_type else NULL
    perturb_name <- if (nzchar(trimws(input$search_perturb %||% '')))
      trimws(input$search_perturb) else NULL

    # ── Step 1: Rule-based NL parser (always runs, no key needed) ─────────
    # Handles: species names, temperature ranges ("35-37", "35 to 37", "37°C"),
    # medium names, drug names, author patterns, year ranges.
    if (nzchar(q) && !grepl('^10\\.', q)) {   # skip bare DOIs
      parsed <- tryCatch(qres_parse_nl_query(q), error = function(e) NULL)

      if (!is.null(parsed) && length(parsed) > 0) {
        if (!is.null(perturb_type)) parsed$perturbation_type <- perturb_type
        if (!is.null(perturb_name)) parsed$perturbation      <- perturb_name

        rows <- tryCatch(
          do.call(qres_filter_datasets, c(list(con = con, registered_only = FALSE), parsed)),
          error = function(e) NULL
        )
        if (!is.null(rows) && nrow(rows) > 0) {
          dataset_results(rows)
          showNotification(
            paste0('Matched ', nrow(rows), ' dataset(s) — filters: ',
                   paste(names(parsed), collapse = ', ')),
            type = 'message', duration = 4)
          return()
        }
      }

      # ── Step 2: LLM fallback (ellmer — anthropic or ollama) ───────────
      if (qres_llm_available()) {
        withProgress(message = paste0('Asking ', qres_llm_backend_name(), '…'), {
          filters <- tryCatch(qres_nl_to_action(q), error = function(e) NULL)

          if (!is.null(filters) && length(filters) > 0) {
            if (!is.null(perturb_type)) filters$perturbation_type <- perturb_type
            if (!is.null(perturb_name)) filters$perturbation      <- perturb_name
            rows <- tryCatch(
              do.call(qres_filter_datasets, c(list(con = con, registered_only = FALSE), filters)),
              error = function(e) NULL
            )
            if (!is.null(rows) && nrow(rows) > 0) {
              dataset_results(rows)
              showNotification(
                paste0('LLM matched ', nrow(rows), ' dataset(s)'),
                type = 'message', duration = 4)
              return()
            }
          }
        })
      }
    }

    # Keyword fallback: apply perturbation sidebar filters on top of keyword search
    base_rows <- qres_search_datasets(con, q)
    if ((!is.null(perturb_type) || !is.null(perturb_name)) && nrow(base_rows) > 0) {
      if (!is.null(perturb_type) && 'perturbation_type' %in% names(base_rows)) {
        base_rows <- base_rows[
          is.na(base_rows$perturbation_type) |
          grepl(perturb_type, base_rows$perturbation_type, ignore.case = TRUE), ]
      }
      if (!is.null(perturb_name) && 'perturbation' %in% names(base_rows)) {
        base_rows <- base_rows[
          is.na(base_rows$perturbation) |
          grepl(perturb_name, base_rows$perturbation, ignore.case = TRUE), ]
      }
    }
    dataset_results(base_rows)
  }, ignoreInit = FALSE)

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
    hidden_idx <- which(names(display) %in% c('raw_file', 'plate_reader', 'doi')) - 1L

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
    tags$p(tags$strong('Selected dataset: '), selected_dataset())
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
    qres_plot_growth(raw_long(), log_y = input$log_y,
                     title = paste('Raw curves —', selected_dataset()))
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
    data   <- raw_long()
    params <- list(correction_type = input$correction_type)

    processing_id <- qres_register_processing_run(
      con, selected_dataset(), input$plugin,
      correction_type = input$correction_type,
      params          = params,
      status          = 'running'
    )

    result <- tryCatch(
      qres_run_plugin(data, input$plugin, params),
      error = function(e) list(derived_metrics = data.frame(),
                                logs = list(message = conditionMessage(e)))
    )

    if (nrow(result$derived_metrics) > 0) {
      m               <- result$derived_metrics
      m$processing_id <- processing_id
      m$dataset_id    <- m$dataset_id %||% selected_dataset()
      m$metric_id     <- paste(processing_id, m$well_id, m$metric_name,
                               seq_len(nrow(m)), sep = '__')
      cols_needed <- c('metric_id', 'processing_id', 'dataset_id', 'well_id',
                       'metric_name', 'metric_value', 'metric_unit', 'model_name')
      m <- m[, intersect(cols_needed, names(m)), drop = FALSE]
      DBI::dbWriteTable(con, 'derived_metric', m, append = TRUE)
    }

    DBI::dbExecute(con,
      "UPDATE processing_run SET status = 'finished', message = ? WHERE processing_id = ?",
      params = list(result$logs$message %||% '', processing_id))
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
    hidden_idx <- which(names(df) %in% c('title', 'source_url', 'plate_reader')) - 1L

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

      all_raw    <- list()
      all_metrics <- lapply(seq_len(nrow(filtered)), function(i) {
        did <- filtered$dataset_id[i]
        tc  <- filtered$temperature_c[i]
        setProgress(i / nrow(filtered), detail = paste('Processing', did, '…'))

        raw <- tryCatch(qres_get_raw_curves(con, did), error = function(e) NULL)
        if (is.null(raw)) return(NULL)

        raw$temperature_c <- tc
        all_raw[[did]] <<- raw

        cmp <- qres_compare_plugin_outputs(raw, plugins = input$comp_tools)
        if (nrow(cmp) > 0) {
          cmp$temperature_c <- tc
          cmp$dataset_id    <- cmp$dataset_id %||% did
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
    req(nrow(comparison_result()) > 0)
    qres_plot_metric_comparison(comparison_result(), metric = input$comp_metric,
                                 facet_by = if (length(unique(comparison_result()$dataset_id)) > 1)
                                   'dataset_id' else NULL)
  })

  output$comp_curves <- renderPlot({
    req(!is.null(comp_raw_long()))
    qres_plot_growth(comp_raw_long(), log_y = input$comp_log_y,
                     title = 'Filtered datasets — raw growth curves')
  })

  output$comp_table <- renderDT({
    req(nrow(comparison_result()) > 0)
    datatable(comparison_result(), options = list(pageLength = 15, scrollX = TRUE))
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

  plateMatrix <- reactiveVal(data.frame())

  observeEvent(input$plateSize, {
    nc <- .plate_cols(input$plateSize); nr <- .plate_rows(input$plateSize)
    row_letters <- if (nr <= 26) LETTERS[seq_len(nr)] else
      c(LETTERS, paste0('A', LETTERS))[seq_len(nr)]
    wells <- as.vector(t(outer(row_letters, seq_len(nc), paste0)))
    plateMatrix(data.frame(
      Well=wells, Species=NA, Strain=NA, Medium=NA, Medium_Modification=NA,
      Perturbation_Type=NA, Perturbation=NA, Concentration=NA, Temperature=NA,
      Is_Blank=FALSE, stringsAsFactors=FALSE))
  })

  observeEvent(input$applyMetadata, {
    plate <- plateMatrix()
    nc <- .plate_cols(input$plateSize); nr <- .plate_rows(input$plateSize)
    plate$Species <- input$meta_species
    plate$Medium  <- input$meta_medium
    if (isTRUE(input$meta_use_strain)) plate$Strain <- input$meta_strain

    # Medium modification / serial dilution
    if (isTRUE(input$meta_medium_mod)) {
      if (isTRUE(input$meta_medium_dilution) && nzchar(input$meta_medium_ratio)) {
        ratio <- as.numeric(strsplit(input$meta_medium_ratio, ':')[[1]])
        a <- ratio[1]; b <- ratio[2]
        start <- .extract_num(input$meta_medium_mod_val)
        for (j in seq_len(nr)) for (i in seq_len(nc)) {
          plate[nc*(j-1)+i, 'Medium_Modification'] <-
            .replace_num(input$meta_medium_mod_val, start * (a/b)^(i-1))
        }
      } else {
        plate$Medium_Modification <- input$meta_medium_mod_val
      }
    }

    # Perturbation
    ptype <- input$meta_perturb_type
    if (ptype == 'chemical') {
      plate$Perturbation_Type <- 'chemical'
      plate$Perturbation      <- input$meta_perturb
      plate$Temperature       <- as.numeric(input$meta_temp_chem)
      if (isTRUE(input$meta_dilution) && nzchar(input$meta_ratio)) {
        ratio <- as.numeric(strsplit(input$meta_ratio, ':')[[1]])
        a <- ratio[1]; b <- ratio[2]; start <- .extract_num(input$meta_conc)
        for (j in seq_len(nr)) for (i in seq_len(nc))
          plate[nc*(j-1)+i, 'Concentration'] <-
            .replace_num(input$meta_conc, start * (a/b)^(i-1))
      } else {
        plate$Concentration <- input$meta_conc
      }
    } else if (ptype == 'genetic') {
      plate$Perturbation_Type <- 'genetic'
      plate$Temperature <- as.numeric(input$meta_temp_gen)
      muts <- trimws(strsplit(input$meta_genetic_bg, ',')[[1]])
      if (length(muts) > 0) plate$Perturbation <- muts
    } else if (ptype == 'temperature') {
      plate$Perturbation_Type <- 'temperature'
      start_t <- .extract_num(input$meta_temp_start)
      step_t  <- if (nzchar(input$meta_temp_step)) .extract_num(input$meta_temp_step) else 0
      temps <- start_t + step_t * (seq_len(nc) - 1)
      plate$Temperature <- rep(temps, times = nr)
    } else {
      plate$Temperature <- as.numeric(input$meta_temp_none)
    }

    # Control last column
    if (isTRUE(input$meta_control_col)) {
      last_col_idx <- seq(nc, nrow(plate), by = nc)
      plate[last_col_idx, 'Concentration']  <- 0
      plate[last_col_idx, 'Perturbation']   <- 'none'
    }

    # Blank wells
    if (nzchar(input$meta_blank_wells)) {
      blanks <- trimws(strsplit(input$meta_blank_wells, ',')[[1]])
      plate$Is_Blank[plate$Well %in% blanks] <- TRUE
    }

    plateMatrix(plate)
  })

  output$plateMatrix <- renderDT({
    plate <- plateMatrix()
    if (nrow(plate) == 0) return(datatable(data.frame(message = 'Select plate format to start.')))
    non_empty <- vapply(plate, function(col)
      !all(is.na(col) | col == '' | isFALSE(col)), logical(1))
    non_empty['Well'] <- TRUE
    datatable(plate[, non_empty, drop = FALSE],
              options = list(pageLength = 96, dom = 't', scrollX = TRUE))
  })

  output$downloadTemplate <- downloadHandler(
    filename = function() paste0('phamig_template_', Sys.Date(), '.csv'),
    content  = function(file) write.csv(plateMatrix(), file, row.names = FALSE)
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
    ptype         <- input$upload_perturb_type %||% 'none'
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
    if (!nzchar(ptype))       errs <- c(errs, 'Perturbation type is required.')

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

      # Duplicate check
      existing <- DBI::dbGetQuery(con, 'SELECT 1 FROM dataset WHERE dataset_id=?',
                                  params = list(dataset_id))
      if (nrow(existing) > 0) {
        output$upload_progress_ui <- renderUI(NULL)
        output$upload_status <- renderText(paste0(
          'Dataset ID "', dataset_id, '" already exists in the database.\n',
          'Choose a different ID, or delete the existing dataset from the "Manage my data" tab.'))
        return()
      }

      # Build metadata row and import
      tmp_csv <- tempfile(fileext = '.csv')
      write.csv(data.frame(
        dataset_id        = dataset_id,
        raw_file          = input$upload_raw_csv$name,
        species           = species,
        medium_type       = medium,
        temperature_c     = temp_c,
        perturbation_type = ptype,
        perturbation      = if (ptype == 'temperature') as.character(temp_c) else NA_character_,
        strain            = NA_character_,
        taxon_id          = NA_character_,
        medium_state      = 'liquid',
        plate_reader      = NA_character_,
        time_unit         = 'h',
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
      key_info <- qres_register_embargo(
        con, dataset_id, email, passphrase,
        release_mode = release_mode,
        release_date = if (release_mode == 'date') release_date else NA,
        biorxiv_url  = if (release_mode == 'biorxiv') biorxiv_url else NA
      )

      # Show access key prominently (only once)
      last_access_key(list(
        access_key = key_info$access_key,
        dataset_id = dataset_id,
        email      = email
      ))

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
        analysis_note, warn_msg, '\n\n',
        '⚠ Copy the access key shown above before leaving this page!'
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
    if (isTRUE(result$ok)) dataset_results(qres_search_datasets(con))
  })

  # ── Status check (my datasets) ──────────────────────────────────────────────
  observeEvent(input$run_status_check, {
    email <- trimws(input$status_email %||% '')
    if (!nzchar(email)) return()
    tryCatch({
      my_ds <- qres_my_datasets(con, email)
      output$my_datasets_table <- renderDT({
        datatable(my_ds[, c('dataset_id', 'species', 'medium_type', 'temperature_c',
                             'release_mode', 'release_date', 'released', 'created_at')],
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
      })
    }, error = function(e) NULL)
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
    note <- data.frame(
      note_id    = paste0('note_', format(Sys.time(), '%Y%m%d%H%M%S'),
                          '_', sample(1000:9999, 1)),
      dataset_id = if (nzchar(input$note_dataset %||% '')) input$note_dataset else NA_character_,
      note_type  = input$note_type,
      note_text  = trimws(input$note_text),
      author     = trimws(input$note_author %||% ''),
      stringsAsFactors = FALSE
    )
    DBI::dbWriteTable(con, 'user_note', note, append = TRUE)
    updateTextAreaInput(session, 'note_text', value = '')
    showNotification('Note submitted — thank you!', type = 'message')
  })

  output$notes_table <- renderDT({
    input$submit_note
    qres_init_schema(con)
    if (!DBI::dbExistsTable(con, 'user_note')) return(datatable(data.frame()))
    datatable(
      DBI::dbGetQuery(con,
        'SELECT note_type, note_text, author, dataset_id, created_at FROM user_note ORDER BY created_at DESC'),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  # ── LLM / Ask tab ─────────────────────────────────────────────────────────
  nl_result <- reactiveVal(NULL)

  output$llm_api_warning <- renderUI({
    if (!qres_llm_available()) {
      tags$div(class = 'alert alert-warning',
        tags$strong('API key not set. '),
        'Set ANTHROPIC_API_KEY to enable natural-language queries:',
        tags$br(),
        tags$code('Sys.setenv(ANTHROPIC_API_KEY = "sk-ant-…")')
      )
    }
  })

  output$llm_status_badge <- renderUI({
    if (qres_llm_available())
      tags$span(class = 'label label-success', 'LLM ready')
    else
      tags$span(class = 'label label-warning', 'No API key')
  })

  observeEvent(input$nl_submit, {
    req(nzchar(trimws(input$nl_question)))
    withProgress(message = 'Asking Claude…', {
      setProgress(0.3, detail = 'Translating question to query…')
      action <- qres_nl_to_action(input$nl_question)

      setProgress(0.7, detail = 'Executing…')
      result <- tryCatch(
        qres_execute_action(con, action),
        error = function(e) list(type = 'info',
                                  description = paste('Execution error:', conditionMessage(e)),
                                  data = NULL, plot_type = 'table')
      )
      nl_result(result)
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
    datatable(df, options = list(pageLength = 20, scrollX = TRUE))
  })

  output$nl_comp_plot <- renderPlot({
    req(nl_result())
    res <- nl_result()
    if (res$type != 'comparison' || is.null(res$data) || nrow(res$data) == 0) return(NULL)
    metric <- input$nl_metric_select %||% (res$metric %||% 'mumax_percapita')
    qres_plot_metric_comparison(res$data, metric = metric,
                                 facet_by = if (length(unique(res$data$dataset_id)) > 1)
                                   'dataset_id' else NULL)
  })

  output$nl_curves_plot <- renderPlot({
    req(nl_result())
    res <- nl_result()
    if (res$type != 'comparison' || is.null(res$filtered)) return(NULL)
    raw_list <- lapply(res$filtered$dataset_id, function(did) {
      tryCatch(qres_get_raw_curves(con, did), error = function(e) NULL)
    })
    combined <- dplyr::bind_rows(Filter(Negate(is.null), raw_list))
    if (nrow(combined) == 0) return(NULL)
    qres_plot_growth(combined, log_y = input$nl_log_y,
                     title = paste('Growth curves —', nrow(res$filtered), 'datasets'))
  })
}

shinyApp(ui, server)
