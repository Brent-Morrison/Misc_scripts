# excelR example: edit a data set from a CSV file and save it to a data frame.
#
# data.csv is loaded at startup; "Data file (CSV)" loads a different file in
# the same format. Saved edits are cleared when a new file is loaded.
#
# Choose a data set, edit cells in the grid, then click "Save to data frame".
# The saved data frame is shown below the grid, can be downloaded as CSV, and
# is assigned to the global environment under the data set's measure name
# (e.g. gross_capex) for inspection after the app closes.
#
# Saved edits are kept per data set while the app runs: reselecting a data set
# shows its last saved version. "Reset to original" discards them and reloads
# the data set from the file.
#
# The saved data frame table has three filters on columns 3 to 8: for each,
# pick a column, then one or more values. Filters apply in order, so each one
# only offers values left by the filters before it, and a column used by one
# filter can't be picked in another. This only filters what is shown; the data
# frame in the global environment and the CSV download keep every row.
#
# install.packages(c("shiny", "excelR"))

library(shiny)
library(excelR)

# A data file holds several data sets. A data set is the rows sharing a
# column_1 value; its column names are column_3 of the "<name>_names" rows in
# order, column_2 repeats the file's column names for convenience. The value_*
# columns must always hold numbers.

# Data set names in a data file: the column_1 values that aren't header rows
dataset_choices_of <- function(data) {
  unique(data$column_1[!grepl("_names$", data$column_1)])
}

# Reads a data file. Returns list(data, name); name labels the file in
# messages. Stops with a reason if the file isn't in the expected format
read_source <- function(path, name) {
  data <- read.csv(path, stringsAsFactors = FALSE)
  missing <- setdiff(c("column_1", "column_3"), names(data))
  if (length(missing) > 0) stop("missing column(s) ", paste(missing, collapse = ", "))
  if (length(dataset_choices_of(data)) == 0) stop("no data sets found in column_1")
  list(data = data, name = name)
}

# Default file. RStudio's Run App uses the app folder as working directory;
# runApp() from the repo root does not. Check the subfolder first: the repo
# root has its own, unrelated data.csv
default_path <- "shiny_examples/data.csv"
if (!file.exists(default_path)) default_path <- "data.csv"
default_source <- if (file.exists(default_path)) read_source(default_path, "data.csv")

# Columns the saved data frame table can be filtered on (by position), and
# the number of filters
filter_cols <- 3:8
n_filters <- 3

# Input ids of filter i's column and values dropdowns
filter_col_id <- function(i) paste0("filter_col_", i)
filter_values_id <- function(i) paste0("filter_values_", i)

# Column values as filter choices; missing values become "(blank)"
filter_key <- function(x) {
  key <- as.character(x)
  key[is.na(key)] <- "(blank)"
  key
}

alert <- function(...) modalDialog(..., title = "Alert", easyClose = TRUE)

# Why a value that as.numeric() rejected isn't a number
why_not_numeric <- function(x) {
  chars <- unique(strsplit(gsub("[0-9.,+-]", "", x), "")[[1]])
  if (length(chars) == 0) return("not a valid number (check decimal points and signs)")
  chars[chars == " "] <- "space"
  paste("non-numeric characters:", paste(chars, collapse = " "))
}

# Converts the value columns of df (positions of the value_* columns in
# file_names) to numeric, dropping thousands separators. row_labels says where
# each row came from, for the error list. Returns list(df, errors); errors is
# NULL or one row per cell that can't be converted
convert_values <- function(df, row_labels, file_names) {
  errors <- NULL
  for (j in grep("^value_", file_names)) {
    x <- trimws(as.character(df[[j]]))
    x[x == ""] <- NA
    num <- suppressWarnings(as.numeric(gsub(",", "", x)))
    bad <- which(!is.na(x) & is.na(num))
    if (length(bad) > 0) {
      errors <- rbind(errors, data.frame(
        row = row_labels[bad],
        column = sprintf("%s (%s)", names(df)[j], file_names[j]),
        value = x[bad],
        reason = vapply(x[bad], why_not_numeric, "", USE.NAMES = FALSE),
        stringsAsFactors = FALSE
      ))
    }
    df[[j]] <- num
  }
  list(df = df, errors = errors)
}

# Alert listing the cells convert_values() couldn't convert
conversion_alert <- function(errors, intro, max_rows = 50) {
  shown_errors <- head(errors, max_rows)
  alert(
    p(intro),
    tags$table(
      class = "table table-condensed",
      tags$thead(tags$tr(lapply(c("Row", "Column", "Value", "Reason"), tags$th))),
      tags$tbody(lapply(seq_len(nrow(shown_errors)), function(i) {
        tags$tr(lapply(unlist(shown_errors[i, ]), tags$td))
      }))
    ),
    if (nrow(errors) > max_rows) p(sprintf("...and %d more.", nrow(errors) - max_rows)),
    size = "l"
  )
}

# Non-value columns: let R pick the type (read.csv typed columns over the
# whole file, so retype for this data set alone)
retype_other_cols <- function(df, file_names) {
  other <- grep("^value_", file_names, invert = TRUE)
  df[other] <- lapply(df[other], type.convert, as.is = TRUE)
  df
}

# Rows of one data set in src (from read_source()), named from its
# <name>_names rows, with the value columns converted to numeric. Returns
# list(df, alert): df is NULL and alert says why if the header rows are missing
# or don't match the column count, or a value cell can't be converted
get_dataset <- function(name, src) {
  data <- src$data
  nms <- data$column_3[data$column_1 == paste0(name, "_names")]
  if (length(nms) != ncol(data)) {
    return(list(df = NULL, alert = alert("Column header details missing")))
  }
  rows <- which(data$column_1 == name)
  df <- data[rows, , drop = FALSE]
  names(df) <- make.unique(nms)
  rownames(df) <- NULL

  # +1 for the header line
  conv <- convert_values(df, paste(src$name, "line", rows + 1), names(data))
  if (!is.null(conv$errors)) {
    intro <- sprintf("Data set '%s' not loaded: these value cells can't be converted to numbers.", name)
    return(list(df = NULL, alert = conversion_alert(conv$errors, intro)))
  }
  list(df = retype_other_cols(conv$df, names(data)), alert = NULL)
}

ui <- navbarPage(
  "excelR examples",
  tabPanel(
    "Data sets",
    br(),
    fluidRow(
      column(
        12,
        # Collapsible: <details> starts closed; clicking the heading opens it
        # Bootstrap 3 makes <summary> a plain block, hiding the browser's arrow,
        # so draw one: right-pointing when closed, down-pointing when open
        tags$style(HTML(
          "#about summary { cursor: pointer; }",
          "#about summary h4 { display: inline; }",
          "#about summary::before { content: '\\25B8'; margin-right: 6px; font-size: 18px; }",
          "#about details[open] summary::before { content: '\\25BE'; }"
        )),
        wellPanel(
          id = "about",
          tags$details(
            tags$summary(
              h4("About this app"),
              tags$small(class = "text-muted", "(click to show / hide)")
            ),
            p(
              "Load a CSV data file (data.csv by default, or choose another with",
              "\"Select data file (CSV)\"), pick a data set, edit cells in the grid and click",
              "\"Save to data frame\"."
            ),
            tags$ul(
              tags$li(HTML(
                "The saved data frame appears below the grid. It is also assigned in R",
                "under the data set's measure name (e.g. <code>gross_capex</code>)",
                "and can be downloaded as CSV."
              )),
              tags$li(
                "Saved edits are kept for each data set while the app runs.",
                "\"Reset to original\" reloads the data set from the file. Loading a new",
                "file clears saved edits."
              ),
              tags$li(
                "The saved table can be filtered with up to three filters on columns 3 to 8.",
                "Each filter only offers values left by the ones before it. Filtering only",
                "changes what is displayed."
              )
            ),
            h4("Data file rules"),
            tags$ul(
              tags$li(HTML(
                "The header row has the same columns as data.csv: <code>column_1</code>",
                "to <code>column_8</code> and <code>value_1</code> to <code>value_4</code>."
              )),
              tags$li(HTML(
                "<code>column_1</code> is either the measure name (a data row) or",
                "<code>&lt;measure&gt;_names</code> (a column-name row for that measure)."
              )),
              tags$li(HTML("<code>column_2</code> is the entity in data rows.")),
              tags$li(HTML(
                "Each measure needs one <code>&lt;measure&gt;_names</code> row per column",
                "of the file, in column order: <code>column_2</code> repeats the file's",
                "column name and <code>column_3</code> gives the name to use for it.",
                "If they're missing or the count is wrong, the app shows",
                "\"Column header details missing\"."
              )),
              tags$li(HTML(
                "<code>value_&lt;n&gt;</code> columns must be numeric. Thousands separators",
                "(e.g. <code>1,234.5</code>) are removed. Any other non-numeric value",
                "stops the data set loading, or an edit saving, and the cells at fault are",
                "listed."
              ))
            )
          )
        ),
        hr(),
        h4("Load / amend data"),
        fileInput(
          "data_file", "Select data file (CSV)", accept = c(".csv", "text/csv"),
          placeholder = if (is.null(default_source)) "No file selected" else default_source$name
        ),
        selectInput("dataset_select", "Select data set", choices = dataset_choices_of(default_source$data)),
        p("Data set rows from the data file. Edit cells, then save to update the data frame."),
        # excelR puts jexcel's "jexcel" class (right and bottom borders, meant for
        # the table) on the full-width output div too, drawing lines across the page
        tags$style("#dataset.jexcel { border: none; }"),
        excelOutput("dataset", height = "auto"),
        br(),
        br(),
        actionButton("dataset_save", "Save to data frame", class = "btn-primary"),
        actionButton("dataset_clear", "Reset to original"),
        br(),
        br()
      )
    ),
    hr(),
    fluidRow(
      column(
        12,
        h4("Inspect data"),
        fluidRow(lapply(seq_len(n_filters), function(i) {
          column(
            2,
            selectInput(filter_col_id(i), paste("Filter", i, "column"), choices = "(none)"),
            selectizeInput(
              filter_values_id(i), paste("Filter", i, "values"), choices = NULL, multiple = TRUE,
              options = list(placeholder = "All values")
            )
          )
        })),
        tableOutput("dataset_table"),
        downloadButton("dataset_download", "Download CSV")
      )
    )
  )
)

server <- function(input, output, session) {
  # The loaded data file (from read_source()), plus a version number that
  # changes each time a file is loaded
  data_source <- reactiveVal(if (!is.null(default_source)) c(default_source, version = 1))

  saved <- reactiveValues(dataset = NULL)

  # Incremented by the reset button to force the grid to re-render
  reset <- reactiveValues(dataset = 0)

  # The selected data set as loaded from the file
  orig <- reactiveVal(NULL)

  # Last saved version of each data set, keyed by name
  stored <- reactiveValues()

  # The version shown in the grid: the stored one if saved, else orig()
  shown <- reactiveVal(NULL)

  # input$dataset is only sent after an edit and isn't cleared on re-render,
  # so track whether it belongs to the data set currently shown
  edited <- reactiveVal(FALSE)

  # File version and name of the data set in the grid, so it isn't loaded twice
  # when a new file is loaded and the dropdown then reports the same selection
  loaded_key <- reactiveVal(NULL)

  load_dataset <- function(name) {
    src <- data_source()
    if (is.null(src) || !isTruthy(name)) return()
    key <- paste(src$version, name)
    if (identical(key, loaded_key())) return()
    loaded_key(key)

    res <- get_dataset(name, src)
    if (!is.null(res$alert)) showModal(res$alert)
    df <- res$df
    orig(df)
    if (!is.null(df) && !is.null(stored[[name]])) df <- stored[[name]]
    shown(df)
    saved$dataset <- df
    edited(FALSE)
  }

  observeEvent(input$dataset_select, load_dataset(input$dataset_select))

  # A new data file replaces the current one: clear saved edits (they belong
  # to the old file), refresh the data set choices, keeping the selected data
  # set if the new file has one of the same name, and load it
  observeEvent(input$data_file, {
    new_source <- tryCatch(
      read_source(input$data_file$datapath, input$data_file$name),
      error = function(e) e
    )
    if (inherits(new_source, "error")) {
      showModal(alert(sprintf("File '%s' not loaded: %s", input$data_file$name, conditionMessage(new_source))))
      return()
    }
    old_version <- if (is.null(data_source())) 0 else data_source()$version
    data_source(c(new_source, version = old_version + 1))
    for (nm in names(stored)) stored[[nm]] <- NULL

    choices <- dataset_choices_of(new_source$data)
    current <- input$dataset_select
    selected <- if (isTRUE(current %in% choices)) current else choices[1]
    updateSelectInput(session, "dataset_select", choices = choices, selected = selected)
    load_dataset(selected)
  })

  observeEvent(input$dataset, edited(TRUE))

  output$dataset <- renderExcel({
    reset$dataset
    df <- shown()
    req(df)
    excelTable(
      data = df,
      columns = data.frame(
        title = names(df),
        type = ifelse(sapply(df, is.numeric), "numeric", "text"),
        width = 110
      ),
      allowInsertRow = TRUE
    )
  })

  # When req() stops the render, htmlwidgets hides the grid, and Shiny by
  # default stops updating hidden outputs, so the grid would never come back
  outputOptions(output, "dataset", suspendWhenHidden = FALSE)

  observeEvent(input$dataset_save, {
    req(edited(), input$dataset)
    df <- excel_to_R(input$dataset)
    names(df) <- names(orig())
    file_names <- names(data_source()$data)
    conv <- convert_values(df, paste("grid row", seq_len(nrow(df))), file_names)
    if (!is.null(conv$errors)) {
      showModal(conversion_alert(conv$errors, "Not saved: these value cells can't be converted to numbers."))
      return()
    }
    df <- retype_other_cols(conv$df, file_names)
    saved$dataset <- df
    # Not written to shown(): that would redraw the grid mid-edit
    stored[[input$dataset_select]] <- df
  })

  observeEvent(input$dataset_clear, {
    stored[[input$dataset_select]] <- NULL
    shown(orig())
    # Forces a redraw even when shown() was already the original
    reset$dataset <- reset$dataset + 1
    saved$dataset <- orig()
    edited(FALSE)
  })

  # --- Saved data frame outputs --------------------------------------------
  # Named after the measure (column_1 value) of the data set it holds
  observeEvent(saved$dataset, {
    assign(isolate(input$dataset_select), saved$dataset, envir = .GlobalEnv)
  })

  # Rows of df left after applying filters 1 to k in turn
  apply_filters <- function(df, k) {
    for (i in seq_len(k)) {
      col <- input[[filter_col_id(i)]]
      vals <- input[[filter_values_id(i)]]
      if (isTruthy(col) && col %in% names(df) && length(vals) > 0) {
        df <- df[filter_key(df[[col]]) %in% vals, , drop = FALSE]
      }
    }
    df
  }

  for (i in seq_len(n_filters)) {
    local({
      i <- i
      others <- setdiff(seq_len(n_filters), i)

      # Column choices: columns 3 to 8 of this data set (names differ between
      # data sets), minus those picked in the other filters. Keeps the selected
      # column if it's still available
      observeEvent(list(names(saved$dataset), lapply(others, function(j) input[[filter_col_id(j)]])), {
        taken <- unlist(lapply(others, function(j) input[[filter_col_id(j)]]))
        available <- setdiff(names(saved$dataset)[filter_cols], taken)
        current <- input[[filter_col_id(i)]]
        selected <- if (isTRUE(current %in% available)) current else "(none)"
        # "(none)" rather than "": selectize treats an empty value as a
        # placeholder, so it couldn't be picked again once a column was chosen
        updateSelectInput(session, filter_col_id(i), choices = c("(none)", available), selected = selected)
      }, ignoreNULL = FALSE)

      # Value choices: values of the selected column in the rows left by the
      # earlier filters. Drops selected values that are no longer there
      observeEvent(list(
        saved$dataset,
        lapply(seq_len(i), function(j) input[[filter_col_id(j)]]),
        lapply(seq_len(i - 1), function(j) input[[filter_values_id(j)]])
      ), {
        df <- saved$dataset
        col <- input[[filter_col_id(i)]]
        if (is.null(df) || !isTruthy(col) || !col %in% names(df)) {
          updateSelectizeInput(session, filter_values_id(i), choices = character(0))
          return()
        }
        df <- apply_filters(df, i - 1)
        choices <- filter_key(sort(unique(df[[col]]), na.last = TRUE))
        selected <- intersect(input[[filter_values_id(i)]], choices)
        updateSelectizeInput(session, filter_values_id(i), choices = choices, selected = selected)
      }, ignoreNULL = FALSE)
    })
  }

  # Row numbers are the rows of the saved data frame (and grid), so filtered
  # rows keep their original numbers
  output$dataset_table <- renderTable({
    df <- saved$dataset
    if (!is.null(df)) df <- apply_filters(df, n_filters)
    df
  }, rownames = TRUE)

  output$dataset_download <- downloadHandler(
    filename = function() paste0(input$dataset_select, ".csv"),
    content = function(file) write.csv(saved$dataset, file, row.names = FALSE)
  )
}

shinyApp(ui, server)
