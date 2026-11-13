# Load L2 (published) data through Arrow. Unlike the L1 loader there is no
# processing script: the user picks a habitat (data_index protocol), one or
# more L2 tables, and optional partner / site / date filters, and each table
# opens in its own read-only tab with QC, sample event and visualization views.

## Helpers ####

# The column the date range filter applies to: the table's first DATE column
# in database_structure order, or NA when the table has none.
l2_date_column <- function(table_id) {
  date_cols <- marinegeo.utils::utl_mg_get_registry(
    "database_structure",
    table_id = table_id
  ) %>%
    filter(data_type == "DATE") %>%
    pull(column_name)

  if (length(date_cols) == 0) NA_character_ else date_cols[[1]]
}

# Apply each filter only when it has values and the dataset has the column,
# so one set of filters can be shared by tables with different columns.
l2_filter <- function(
  ds,
  partners = NULL,
  sites = NULL,
  date_col = NA,
  date_range = NULL
) {
  cols <- names(ds)

  if (length(partners) > 0 && "partner_code" %in% cols) {
    ds <- ds %>% filter(partner_code %in% partners)
  }
  if (length(sites) > 0 && "site_name" %in% cols) {
    ds <- ds %>% filter(site_name %in% sites)
  }
  if (length(date_range) == 2 && !is.na(date_col) && date_col %in% cols) {
    date_sym <- rlang::sym(date_col)
    ds <- ds %>%
      filter(!!date_sym >= date_range[[1]], !!date_sym <= date_range[[2]])
  }

  ds
}

# Distinct, sorted, non-missing values of one column across several datasets.
l2_distinct_values <- function(datasets, column) {
  values <- lapply(datasets, function(ds) {
    if (!column %in% names(ds)) {
      return(NULL)
    }
    ds %>% distinct(!!rlang::sym(column)) %>% collect() %>% pull(1)
  })
  sort(unique(stats::na.omit(unlist(values))))
}

## Module ####
load_l2_data_UI <- function(id) {
  ns <- NS(id)
  layout_columns(
    card(
      card_header("Load L2 Data"),
      uiOutput(ns("select_habitat")),
      uiOutput(ns("select_tables")),
      uiOutput(ns("select_partners")),
      uiOutput(ns("select_sites")),
      uiOutput(ns("select_dates")),
      actionButton(ns("extract_data"), "Extract Data"),
      uiOutput(ns("extract_status"))
    ),
    card(
      card_header("About L2 Data"),
      p(
        "L2 data are the published tables built from processed L1 files. ",
        "Extracted tables open read-only: QC results, sample event ",
        "summaries and visualizations are available, but there is no ",
        "processing script to edit or rerun."
      ),
      p(
        "Filters are optional. Leaving a filter empty extracts every ",
        "value; large tables may take a moment."
      )
    )
  )
}

load_l2_data_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      loaded_bundles <- reactiveVal(NULL)
      extract_status <- reactiveVal(NULL)

      # L2 tables the user can reach: the same "directory must exist" rule
      # the L1 loader applies to data_sources.json groupings.
      l2_tables <- marinegeo.utils::utl_mg_get_registry("data_index") %>%
        filter(level == 2, location == "github") %>%
        filter(dir.exists(paste0(Sys.getenv("repository_filepath"), directory)))

      ## Lazy Arrow datasets for the selected tables ####
      selected_datasets <- reactive({
        req(input$select_tables)
        datasets <- lapply(input$select_tables, function(table_id) {
          tryCatch(
            marinegeo.utils::db_arrow_marinegeo(table_id),
            error = function(e) NULL
          )
        })
        names(datasets) <- input$select_tables
        purrr::compact(datasets)
      })

      date_columns <- reactive({
        req(input$select_tables)
        cols <- vapply(input$select_tables, l2_date_column, character(1))
        cols[!is.na(cols)]
      })

      # Full span of the selected tables' dates. The date filter is only
      # applied when the user narrows it, so rows with a missing date are not
      # silently dropped by an untouched range.
      full_date_range <- reactive({
        datasets <- selected_datasets()
        date_cols <- date_columns()
        ranges <- lapply(names(date_cols), function(table_id) {
          ds <- datasets[[table_id]]
          col <- date_cols[[table_id]]
          if (is.null(ds) || !col %in% names(ds)) {
            return(NULL)
          }
          date_sym <- rlang::sym(col)
          ds %>%
            summarize(
              min_date = min(!!date_sym, na.rm = TRUE),
              max_date = max(!!date_sym, na.rm = TRUE)
            ) %>%
            collect()
        })
        ranges <- bind_rows(ranges)
        if (nrow(ranges) == 0) {
          return(NULL)
        }
        c(
          min(ranges$min_date, na.rm = TRUE),
          max(ranges$max_date, na.rm = TRUE)
        )
      })

      ## UI outputs ####
      output$select_habitat <- renderUI({
        habitats <- sort(unique(l2_tables$protocol))
        shiny::validate(need(
          length(habitats) > 0,
          "No L2 data directories were found under repository_filepath."
        ))
        pickerInput(
          session$ns("select_habitat"),
          "Select Habitat",
          choices = habitats,
          options = pickerOptions(container = "body"),
          width = "100%"
        )
      })

      output$select_tables <- renderUI({
        req(input$select_habitat)
        tables <- l2_tables %>% filter(protocol == input$select_habitat)
        pickerInput(
          session$ns("select_tables"),
          "Select Tables",
          choices = setNames(tables$table_id, tables$table_name),
          multiple = TRUE,
          options = pickerOptions(
            container = "body",
            actionsBox = TRUE,
            noneSelectedText = "Select one or more tables"
          ),
          width = "100%"
        )
      })

      output$select_partners <- renderUI({
        partners <- l2_distinct_values(selected_datasets(), "partner_code")
        # Some tables (e.g. Reef Life Survey) have no partner_code column
        if (length(partners) == 0) {
          return(NULL)
        }
        pickerInput(
          session$ns("select_partners"),
          "Partner Code",
          choices = partners,
          multiple = TRUE,
          options = pickerOptions(
            container = "body",
            liveSearch = TRUE,
            actionsBox = TRUE,
            noneSelectedText = "All partners"
          ),
          width = "100%"
        )
      })

      output$select_sites <- renderUI({
        # Narrow site choices to the chosen partners where that is possible
        datasets <- lapply(
          selected_datasets(),
          l2_filter,
          partners = input$select_partners
        )
        sites <- l2_distinct_values(datasets, "site_name")
        if (length(sites) == 0) {
          return(NULL)
        }
        pickerInput(
          session$ns("select_sites"),
          "Site Name",
          choices = sites,
          selected = intersect(isolate(input$select_sites), sites),
          multiple = TRUE,
          options = pickerOptions(
            container = "body",
            liveSearch = TRUE,
            actionsBox = TRUE,
            noneSelectedText = "All sites"
          ),
          width = "100%"
        )
      })

      output$select_dates <- renderUI({
        date_range <- full_date_range()
        if (is.null(date_range)) {
          return(NULL)
        }
        dateRangeInput(
          session$ns("select_dates"),
          paste0(
            "Date Range (",
            paste(unique(date_columns()), collapse = ", "),
            ")"
          ),
          start = date_range[[1]],
          end = date_range[[2]],
          min = date_range[[1]],
          max = date_range[[2]],
          width = "100%"
        )
      })

      output$extract_status <- renderUI({
        req(extract_status())
        tags$ul(
          style = "margin-top: 8px;",
          purrr::map(extract_status(), tags$li)
        )
      })

      ## Extract ####
      observeEvent(input$extract_data, {
        req(input$select_tables)

        datasets <- selected_datasets()
        date_cols <- vapply(input$select_tables, l2_date_column, character(1))

        date_range <- input$select_dates
        full_range <- full_date_range()
        if (
          length(date_range) == 2 &&
            length(full_range) == 2 &&
            all(as.numeric(as.Date(date_range)) == as.numeric(full_range))
        ) {
          date_range <- NULL
        }

        partner_label <- if (length(input$select_partners) == 0) {
          "all"
        } else {
          paste(input$select_partners, collapse = ", ")
        }

        bundles <- list()
        status <- character(0)

        withProgress(message = "Extracting L2 data", value = 0, {
          for (table_id in input$select_tables) {
            incProgress(1 / length(input$select_tables), detail = table_id)

            df <- tryCatch(
              {
                if (is.null(datasets[[table_id]])) {
                  stop("could not open dataset")
                }
                datasets[[table_id]] %>%
                  l2_filter(
                    partners = input$select_partners,
                    sites = input$select_sites,
                    date_col = date_cols[[table_id]],
                    date_range = date_range
                  ) %>%
                  collect()
              },
              error = function(e) {
                showNotification(
                  paste0(table_id, ": ", conditionMessage(e)),
                  type = "error",
                  duration = NULL
                )
                NULL
              }
            )

            if (is.null(df)) {
              status <- c(status, paste0(table_id, ": failed to load"))
              next
            }
            if (nrow(df) == 0) {
              showNotification(
                paste0(table_id, ": no rows match the selected filters"),
                type = "warning"
              )
              status <- c(status, paste0(table_id, ": 0 rows (no tab opened)"))
              next
            }

            short_name <- l2_tables %>%
              filter(table_id == !!table_id) %>%
              pull(short_name)
            if (length(short_name) == 0 || is.na(short_name[[1]])) {
              short_name <- table_id
            }

            bundles[[table_id]] <- list(
              source_level = "L2",
              in_df = df,
              out_df = df,
              data_filepath = NULL,
              data_filename = NULL,
              project_directory = NULL,
              script_filepath = NULL,
              output_table_id = table_id,
              output_req_cols = marinegeo.utils::utl_mg_column_order(table_id),
              loaded_input_filenames = if ("input_filename" %in% names(df)) {
                unique(df$input_filename)
              } else {
                character(0)
              },
              tab_label = paste0(
                "L2: ",
                short_name[[1]],
                " (",
                partner_label,
                ")"
              ),
              # Makes a repeated identical extract a new value, so the
              # reactiveVal still fires and the tabs reopen
              extracted_at = Sys.time()
            )
            status <- c(status, paste0(table_id, ": ", nrow(df), " rows"))
          }
        })

        extract_status(status)
        if (length(bundles) > 0) loaded_bundles(unname(bundles))
      })

      return(loaded_bundles)
    }
  )
}
