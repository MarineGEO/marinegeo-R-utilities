# source_level is "L1" (a file run through a processing script) or "L2"
# (a read-only extract of published data). L2 tabs have no script, so the
# script reload button and the Generate Code tab are left out.
mod_table_instance_UI <- function(id, source_level = "L1") {
  ns <- NS(id)
  is_l1 <- source_level == "L1"
  layout_sidebar(
    fill = TRUE,
    fillable = TRUE,
    style = "height: calc(100vh - 56px);",
    sidebar = sidebar(
      general_sidebar_UI(ns("general_settings")),
      if (is_l1) mod_run_processing_script_UI(ns("reload_r")),
      qc_flag_UI(ns("qc_sidebar"))
    ),
    navset_card_tab(
      nav_panel("Interactive Table",
        card(DT_table_UI(ns("rls_excel_table")), fill = TRUE, full_screen = TRUE)),
      if (is_l1) nav_panel("Generate Code", suggest_code_UI(ns("code_suggestions"))),
      nav_panel("Sample Event Summary", sample_event_UI(ns("sample_events"))),
      nav_panel("Data Structure", table_structure_UI(ns("table_structure"))),
      nav_panel("Visualizations", visualizations_UI(ns("viz"))),
    )
  )
}

mod_table_instance_server <- function(id, bundle) {
  moduleServer(id, function(input, output, session) {

    source_level <- if (is.null(bundle$source_level)) "L1" else bundle$source_level

    input_list <- reactiveValues(
      source_level = source_level,
      # input_filename values already present in out_df. Views that append
      # the rest of the L2 dataset exclude these so no row is counted twice.
      loaded_input_filenames = if (source_level == "L2") {
        bundle$loaded_input_filenames
      } else {
        bundle$data_filename
      },
      in_df = bundle$in_df,
      out_df = bundle$out_df,
      flag_df = tibble(flag = NA_character_, row_num = NA_integer_, .rows = 0),
      selected_flag = NULL,
      data_filepath = bundle$data_filepath,
      data_filename = bundle$data_filename,
      script_filepath = bundle$script_filepath,
      output_table_id = bundle$output_table_id,
      output_req_cols = bundle$output_req_cols,
      project_directory = bundle$project_directory,
      code_chain = list(),
      load_code_chain_flag = 0,
      flush_code_chain_ids = NULL,
      table_selections = list(),
      display_col_order = NULL,
      agg_failures = NULL
    )

    general_sidebar_server("general_settings", input_list)
    qc_flag_server("qc_sidebar", input_list)
    if (source_level == "L1") {
      mod_run_processing_script_server("reload_r", input_list)
      suggest_code_server("code_suggestions", input_list)
    }
    DT_table_server("rls_excel_table", input_list)
    sample_event_server("sample_events", input_list)
    table_structure_server("table_structure", input_list)
    visualizations_server("viz", input_list)

  })
}
