#' @title create a tibble with information about selected Cristin units
#' @description Create a tibble with information about (nested) units in Cristin
#'  (e.g., A University -> Faculties -> Departments -> Groups). The tibble can
#'  than be used to extract data for each unit from Cristin. Used by
#'  `CristinMonthly`
#' @param unit.id Unit to search for
#' @param subunits Add subunits, Default: TRUE
#' @param recursive Search for (nested) sub-units, Default: FALSE
#' @param ancestors Search for parent units, Default: FALSE
#' @param lang PARAM_DESCRIPTION, Default: `en`
#' @return A tibble containing information about selected units
#' @details Used with `CristinMontlhy` to create month-to-month bibliography of
#' selected units
#' @examples
#' \donttest{
#' # Find units for Inland University
#'   CristinUnits("209.0.0.0") |>
#'     dplyr::select(id, name) |>
#'     print(width = 80)
#' }
#' @seealso
#'  \code{\link[httr]{RETRY}}
#'  \code{\link[dplyr]{transmute}}, \code{\link[dplyr]{bind_rows}}
#'  \code{\link[purrr]{pmap}}
#' @rdname CristinUnits
#' @export
CristinUnits <- \(unit.id,
                  subunits = TRUE,
                  recursive = FALSE,
                  ancestors = FALSE,
                  lang = "en") {

  # Visible bindings
  cristin_unit_id <- unit_name <- parent_unit <- name <- parent_units <-
    id <- core <- NULL

  # Languages
  # Set lang as nn if no
  if (lang %in% c("no")) lang <- "nn"
  # Set lang to en if not Norwegian
  if (!lang %in% c("nb", "nn", "no")) lang <- "en"

  # Definitions
  affiliation <- Dict("affiliation", lang)

  # Find units from Cristin API
  httr.get <- Online(
    httr::RETRY(
      "GET",
      sprintf("https://api.cristin.no/v2/units/%s/", unit.id),
      query = list(lang = lang),
      quiet = TRUE
    ),
    message = "Cristin API",
    silent = TRUE
  )

  # Return NULL data if error
  if (httr.get$error) {
    return(NULL)
  }

  # Define units data
  units.data <- jsonlite::fromJSON(ParseUrl(httr.get$data))

  # Create units information
  units <- GoFish(units.data$parent_units$unit_name, NULL) |>
    c(name = units.data$unit_name) |>
    unlist() |>
    bind_rows() |>
    dplyr::mutate(
      path = dplyr::case_when(
        "subunits" %in% names(units.data) ~ affiliation,
        TRUE ~ NA_character_
      ),
      core = purrr::pmap(dplyr::across(!dplyr::last_col()), ~ unname(c(...)))
    ) |>
    dplyr::select_if(~ !all(is.na(.))) |>
    dplyr::rename_with(~ paste0("path", seq_along(.)), !dplyr::last_col()) |>
    tibble::add_column(id = units.data$cristin_unit_id, .before = 1)

  # Find and return unit with ancestors if ancestors = TRUE
  if (ancestors) {
    unit.ids <- c(
      GoFish(units.data$parent_units$cristin_unit_id, NULL),
      units.data$cristin_unit_id
    )
    if (length(unit.ids) > 1) {
      units <- CristinUnits(unit.ids[[1]], recursive = TRUE, lang = lang) |>
        dplyr::filter(id %in% unit.ids)
    }
  }

  # Check if unit has subunits
  if (subunits && "subunits" %in% names(units.data)) {

    # Else add subunits
    units <- dplyr::bind_rows(
      units,
      units.data$subunits |>
        tibble::as_tibble() |>
        dplyr::transmute(
          id = cristin_unit_id,
          !!paste0("path", ncol(units)-2) := unlist(unit_name)
        )
    ) |>
      tidyr::fill(dplyr::starts_with("path"))

    # Add nested subunits if recursive is TRUE
    if (recursive) {
      units <- dplyr::bind_rows(
        units[1, ],
        lapply(seq_len(nrow(units))[-1], \(i) {
          CristinUnits(
            units[i, ]$id,
            subunits = TRUE,
            recursive = TRUE,
            lang = lang)
        })
      )
    }
  }

  # Arrange units
  units <- units |>
    dplyr::select(c(id, core, dplyr::starts_with("path"))) |>
    dplyr::arrange(
      dplyr::across(
        dplyr::starts_with("path"),
        ~ data.frame(!is.na(.x), !.x %in% affiliation, .x)
      )
    ) |>
    dplyr::distinct()


  units <- units |>
    dplyr::mutate(
      core = purrr::pmap(
        dplyr::select(units, dplyr::starts_with("path")),
        function(...) {
          x <- c(...)
          x[!is.na(x)]
        }
      ),
      core = purrr::map(core, ~ .x[!stringr::str_detect(.x, "Affiliated")]),
      name = purrr::map_chr(core, ~ tail(.x, 1))
    )

  return (units)
}
