# PhaMiG note-submission email notifier.
#
# Requires the blastula package and a one-time credential setup:
#
#   install.packages("blastula")
#   blastula::create_smtp_creds_file(
#     file     = "~/.phamig_smtp_creds",
#     user     = Sys.getenv('PHAMIG_ADMIN_EMAIL'),   # set in ~/.Renviron
#     host     = "smtp.gmail.com",
#     port     = 587,
#     use_ssl  = TRUE
#   )
#   # → paste your Gmail App Password when prompted
#   #   (create one at myaccount.google.com → Security → App Passwords)
#
# After setup the function works silently; if blastula is absent or the
# creds file is missing it logs a warning and returns FALSE.

PHAMIG_ADMIN_EMAIL  <- Sys.getenv('PHAMIG_ADMIN_EMAIL', unset = '')
PHAMIG_SMTP_CREDS   <- path.expand(
  Sys.getenv('PHAMIG_SMTP_CREDS', unset = '~/.phamig_smtp_creds')
)

phamig_send_comparison_email <- function(to, metric, n_datasets,
                                          attachments = list(),
                                          timestamp   = format(Sys.time(), '%Y-%m-%d %H:%M:%S')) {
  if (!requireNamespace('blastula', quietly = TRUE)) {
    warning('[email] blastula not installed — skipping.', call. = FALSE)
    return(invisible(FALSE))
  }
  if (!file.exists(PHAMIG_SMTP_CREDS)) {
    warning('[email] SMTP credentials not found at ', PHAMIG_SMTP_CREDS, call. = FALSE)
    return(invisible(FALSE))
  }

  body <- paste0(
    '## PhaMiG — Comparison Results\n\n',
    '**Metric:** ', metric, '  \n',
    '**Datasets:** ', n_datasets, '  \n',
    '**Generated:** ', timestamp, '  \n\n',
    'Plots (PNG) and the metrics table (CSV) are attached.\n\n',
    '---\n*Sent by PhaMiG · Phenotypic Analysis of Microbial Growth*\n'
  )

  tryCatch({
    email <- blastula::compose_email(body = blastula::md(body))
    for (path in attachments) {
      if (!is.null(path) && file.exists(path))
        email <- blastula::add_attachment(email, file = path)
    }
    blastula::smtp_send(
      email       = email,
      to          = to,
      from        = PHAMIG_ADMIN_EMAIL,
      subject     = paste0('[PhaMiG] Comparison results — ', metric,
                           ' (', n_datasets, ' datasets, ', timestamp, ')'),
      credentials = blastula::creds_file(PHAMIG_SMTP_CREDS)
    )
    message('[email] Comparison results sent to ', to)
    invisible(TRUE)
  }, error = function(e) {
    warning('[email] Failed: ', conditionMessage(e), call. = FALSE)
    invisible(FALSE)
  })
}

phamig_notify_note <- function(note_id, note_type, note_text,
                                author    = '',
                                timestamp = format(Sys.time(), '%Y-%m-%d %H:%M:%S')) {
  if (!requireNamespace('blastula', quietly = TRUE)) {
    warning('[email] blastula not installed — skipping notification.',
            '\nRun: install.packages("blastula")', call. = FALSE)
    return(invisible(FALSE))
  }
  if (!file.exists(PHAMIG_SMTP_CREDS)) {
    warning('[email] SMTP credentials not found at ', PHAMIG_SMTP_CREDS,
            '\nSee R/utils/email.R for setup instructions.', call. = FALSE)
    return(invisible(FALSE))
  }

  body <- paste0(
    'A new note has been submitted to PhaMiG.\n\n',
    'ID:        ', note_id,    '\n',
    'Type:      ', note_type,  '\n',
    'Author:    ', if (nzchar(trimws(author))) author else '(anonymous)', '\n',
    'Timestamp: ', timestamp,  '\n\n',
    '--- Note text ---\n',
    note_text, '\n'
  )

  tryCatch({
    email <- blastula::compose_email(body = blastula::md(body))
    blastula::smtp_send(
      email       = email,
      to          = PHAMIG_ADMIN_EMAIL,
      from        = PHAMIG_ADMIN_EMAIL,
      subject     = paste0('[PhaMiG] New ', note_type, ' note — ', note_id),
      credentials = blastula::creds_file(PHAMIG_SMTP_CREDS)
    )
    message('[email] Notification sent for ', note_id)
    invisible(TRUE)
  }, error = function(e) {
    warning('[email] Failed to send notification: ', conditionMessage(e), call. = FALSE)
    invisible(FALSE)
  })
}
