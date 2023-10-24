#' @title Convert Cristin monthly data to JSON format
#' @description This function converts the provided Cristin monthly data into
#' JSON format, which can be used for various purposes.
#' @param cristin.monthly A list containing the monthly data from Cristin,
#' including bibliographic information.
#' @param local.storage Path to local storage directory. Default is NULL.
#' @param user.cards Logical, indicating whether to update user information
#' from INN. Default is TRUE.
#' @param full.update Logical, indicating whether to perform a full update of
#' user information from INN. Default is FALSE.
#' @param lang Language for localization ("nb", "nn", "no", or "en"). Defaults
#' to "nn".
#' @param silent Logical, indicating whether to suppress log messages. Default
#' is FALSE.
#' @param log List to store log messages. Default is an empty list.
#' @return A JSON-formatted string representing the converted Cristin monthly
#' data.
#' @details Used with `CristinMonthly` to create month-to-month bibliography in
#' JSON format
#' @examples
#' \donttest{
#'   # Create monthlies for unit 209.5.10.0
#'   example <- CristinMonthly(
#'     c2z::Zotero(
#'       id = "4827927",
#'       api = "RqlAmlH5l1KPghfCseAq1sQ1",
#'       user = FALSE
#'     ),
#'     unit.key = "209.5.10.0",
#'     start.date = "2023-07",
#'     post = TRUE,
#'     silent = TRUE
#'   )
#'
#'   # Create json data
#'   example.json <- CristinJson(example, user.cards = FALSE)
#' }
#' @rdname CristinJson
#' @export
CristinJson <- \(cristin.monthly,
                 local.storage = NULL,
                 user.cards = TRUE,
                 full.update = FALSE,
                 lang = "nn",
                 silent = FALSE,
                 log = list()) {

  # Visible bindings
  abstract <- bib.item <- key <- level <- month <- name <- parentCollection <-
    sdg <- title <- type <- year <- inn.cards <-  cristin.id <-
    cristin.ids <- ezproxy <- unpaywall <- NULL

  # Languages
  # Set language to en if not no
  lang <- if (lang %in% c("no", "nn", "nb")) "no" else "en"

  # Definitions
  monthlies <- cristin.monthly$monthlies
  unit.paths <- cristin.monthly$unit.paths

  # Define menu and link items
  menu <- tibble::tibble(
    startYear = min(monthlies$year),
    endYear = max(monthlies$year),
    nCollections = max(unit.paths$level),
    type = list(unique(monthlies$type)),
    cardURL = Dict("inn.url", lang),
    cristinURL = "https://app.cristin.no/results/show.jsf?id=",
    zoteroURL = paste0(dirname(monthlies$zotero.url[[1]]),"/")
  )

  # Define main html elements
  html <- tibble::tibble(
    bibBody = gsub("%s", "", monthlies$bib.body[[1]])
  )

  # Define collections
  collections <- unit.paths |>
    dplyr::transmute(
      key,
      level,
      name,
      parent = parentCollection
    )

  # Update user info @ inn if user.cards = TRUE
  if (user.cards) {
    inn.cards <- GetCards(local.storage, full.update, lang, silent)
  } # End user.cards

  # Function to find contributors in Inn Cards
  CreateContributors <- \(x, inn.cards = NULL) {
    if (!any(nrow(inn.cards))) {
      return (NULL)
    }
    inn.card <- inn.cards |>
      dplyr::filter(cristin.id %in% x) |>
      dplyr::arrange(match(cristin.id, x)) |>
      dplyr::pull(cristin.id)
    if (!any(length(inn.card))) inn.card <- NULL
    return (inn.card)
  }

  # Create items
  items <- monthlies |>
    dplyr::transmute(
      key,
      cristinId = cristin.id,
      type,
      html = bib.item,
      contributors = purrr::map(
        cristin.ids, ~
          CreateContributors(.x, inn.cards),
        .progress = !silent
      ),
      abstract,
      title,
      year,
      month,
      collections,
      sdg,
      GoFish(unpaywall),
      GoFish(ezproxy)
    )

  # Create json data
  json <- jsonlite::toJSON(list(
    menu = jsonlite::unbox(menu),
    html = jsonlite::unbox(html),
    collections = collections,
    items = items
  ))

  # Remove empty elements
  json <- gsub(",\"\\w+?\":(\\{\\}|\\[\\])", "", json)

  return (json)

}
