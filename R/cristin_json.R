#' @title Convert Cristin monthly data to JSON format
#' @description This function converts the provided Cristin monthly data into
#' JSON format, which can be used for various purposes.
#' @param monthlies Data containing monthly information from Cristin.
#' @param unit.paths Data containing the Zotero paths of Cristin units.
#' @param sdg.data Data containing information about Sustainable Development
#' Goals (SDGs). Default is NULL.
#' @param local.storage Path to local storage directory. Default is NULL.
#' @param user.cards Logical, indicating whether to update user information
#' from INN. Default is TRUE.
#' @param use.multisession Logical. If \code{TRUE} (default), parallel
#' processing using multisession is employed; otherwise, processing is sequential.
#' @param min.multisession Minimum number of results for using multisession.
#'   Default: 25
#' @param n.workers Optional integer for the number of workers to be used in
#' multisession mode. If \code{NULL}, it defaults to the number of available
#' cores minus one (with a minimum of one).
#' @param handler The progress handler to be used by the \code{progressr}
#' package. If \code{NULL} and
#'   \code{silent} is \code{FALSE}, it defaults to \code{"txtprogressbar"}.
#'   When \code{silent} is \code{TRUE},
#'   the handler is set to \code{"void"}.
#' @param restore.defaults Logical. If \code{TRUE} (default), the current
#' \code{future} plan is saved and restored upon exit.
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
#'       api = "Io1bwAiOjB1jPgUNegjJhJxF",
#'       user = FALSE
#'     ),
#'     unit.id = "209.5.10.0",
#'     start.date = "2023-07",
#'     post = TRUE,
#'     silent = TRUE,
#'     use.citeproc = FALSE
#'   ) |>
#'     c2z:::GoFish()
#'
#'   # Create json data
#'   example.json <- CristinJson(
#'     example$monthlies,
#'     example$unit.paths,
#'     user.cards = FALSE
#'   ) |>
#'     jsonlite::prettify() |>
#'     c2z:::GoFish()
#'
#'   if (any(!is.na(example.json))) cat(example.json)
#' }
#' @rdname CristinJson
#' @export
CristinJson <- \(monthlies,
                 unit.paths,
                 sdg.data = NULL,
                 local.storage = NULL,
                 user.cards = TRUE,
                 use.multisession = FALSE,
                 min.multisession = 25,
                 n.workers = NULL,
                 handler = "cli",
                 restore.defaults = FALSE,
                 full.update = FALSE,
                 lang = "nn",
                 silent = FALSE,
                 log = list()) {

  # Visible bindings
  abstract <- bib.item <- key <- level <- month <- name <- parentCollection <-
    sdg <- title <- type <- year <- inn.cards <-  cristin.id <-
    cristin.ids <- ezproxy <- unpaywall <- synopsis <- keyword <-
    research.type <- research.design <- keywords <- NULL

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

  # Languages
  # Set lang as nn if no
  if (lang %in% c("no")) lang <- "nn"
  # Set lang to en if not Norwegian
  if (!lang %in% c("nb", "nn", "no")) lang <- "en"

  # Define menu and link items
  menu <- tibble::tibble(
    startYear = min(monthlies$year),
    sdgs = GoFish(list(sdg.data$sum), NULL),
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
  if (user.cards & !is.null(local.storage)) {
    inn.cards <- GetCards(local.storage, full.update, lang, silent)
  } # End user.cards


  CreateJsonItems <- \(data, silent = TRUE) {
    data |>
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
        synopsis,
        keywords = GoFish(Map(c, research.type, research.design, keywords)),
        unpaywall = GoFish(unpaywall),
        ezproxy = GoFish(ezproxy)
      )
  }

  start.message <- sprintf(
    "Converting %s to JSON",
    Numerus(
      nrow(monthlies),
      "item",
    )
  )

  monthlies.data <- c2z::ProcessData(
    data = monthlies,
    func = CreateJsonItems,
    by.rows = FALSE,
    min.multisession = min.multisession,
    n.workers = n.workers,
    limit = 50,
    use.multisession = use.multisession,
    start.message = start.message,
    handler = handler,
    silent = silent
  )

  items <- monthlies.data$results
  log <- c(log, monthlies.data$log)

  # Remove empty elements
  items <- items |>
    dplyr::mutate(dplyr::across(dplyr::where(is.list), ~ RemoveNames(.x)))
  items.list <- split(items, seq(nrow(items)))
  cleaned.items <- unname(
    lapply(items.list, \(x) {
      as.list(unlist(RemoveEmpty(x), recursive = FALSE))
    }
    )
  )

  # Create json data
  json <- jsonlite::toJSON(list(
    menu = jsonlite::unbox(menu),
    html = jsonlite::unbox(html),
    collections = collections,
    items = cleaned.items
  ), auto_unbox = TRUE)

  return (json)

}
