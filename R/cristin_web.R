#' @title Create HTML and Metadata Files from Cristin Data for Web Display
#' @description This function generates HTML and metadata files for web
#' display using data from Cristin.
#' @param monthlies Data containing monthly information from Cristin.
#' @param sdg.data Data containing information about Sustainable Development
#' Goals (SDGs). Default is NULL.
#' @param sdg.path Path to SDG images. Default is NULL.
#' @param archive.url Relative URL to archive. Default is NULL.
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
#' @return A list containing metadata and HTML content for web display.
#' @details Used with `CristinMonthly` to create month-to-month bibliography in
#' markdown format
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
#'     use.citeproc = FALSE,
#'   )
#'
#'   # Create md files
#'   example.web <- CristinWeb(
#'     example$monthlies,
#'     user.cards = FALSE
#'   )
#'
#'   # Print the three first individual pages (if any)
#'   if (!is.null(example$monthlies)) {
#'     example$monthlies |>
#'       dplyr::select(title) |>
#'       print(n = 3, width = 80)
#'   }
#' }
#' @rdname CristinWeb
#' @export
CristinWeb <- \(monthlies,
                       sdg.data = NULL,
                       sdg.path = NULL,
                       archive.url = NULL,
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

  # Return NULL if no monthlies
  if (!any(nrow(monthlies))) {
    return(NULL)
  }

  # Visible bindings
  cristin.id <- inn.cards <- research.type <- research.design <- NULL

  # Languages: if lang is "no" set to "nn"; if not Norwegian, set to "en"
  if (lang %in% c("no")) lang <- "nn"
  if (!lang %in% c("nb", "nn", "no")) lang <- "en"

  # Update user info (cards) if requested
  if (user.cards) {
    inn.cards <- GetCards(local.storage, full.update, lang, silent)
  }

  # Inner function that creates a params list for a single publication/item.
  CreateParams <- \(item) {

    # Helper function to create a vector of collection names
    CollectionNames <- \(x) {
      Trim(as.character(unlist(strsplit(unlist(x), split = "\\|\\|"))))
    }

    params <- list()

    # Title: use item$title if available, otherwise use the key.
    if (any(!is.na(GoFish(item$title)))) {
      params$title <- item$title
    } else {
      params$title <- item$key
    }

    # Frontmatter: a simple YAML block for use in the Rmd
    params$frontmatter <- sprintf(
      'title: "%s"\ntype: pub', gsub("\"", "'", params$title)
    )

    # Reference: if a bibliography entry is present, add it.
    if (any(!is.na(GoFish(item$bib)))) {
      params$reference <- item$bib
    }

    # Keywords: combine research.type, research.design, and keywords fields.
    keywords <- item |>
      mutate(
        keywords = purrr::map_chr(
          Map(c, research.type, research.design, keywords),
          ~ paste(.x, collapse = ", ")
        )
      ) |>
      dplyr::pull(keywords) |>
      GoFish(type = "")
    if (keywords != "") {
      params$keywords <- keywords
    }

    # Synopsis
    if (any(!is.na(GoFish(item$synopsis)))) {
      params$synopsis <- item$synopsis
    }

    # Abstract
    if (any(!is.na(GoFish(item$abstract)))) {
      params$abstract <- item$abstract
    }

    # Contributors: if there are Cristin IDs and if user cards are available.
    if (any(!is.na(GoFish(item$cristin.ids[[1]]))) & any(nrow(inn.cards))) {
      ids <- item$cristin.ids[[1]]
      inn.card <- inn.cards |>
        dplyr::filter(cristin.id %in% ids) |>
        dplyr::arrange(match(cristin.id, ids))

      if (any(nrow(inn.card))) {
        params$contributors <- inn.card$card
      }
    }

    # SDG: include SDG info if available.
    if (any(!is.na(GoFish(item$sdg[[1]]))) & any(length(sdg.data$sum))) {
      params$sdg <- SdgInfo(
        sdg.data$sum,
        as.numeric(item$sdg[[1]]),
        lang = lang,
        sdg.path,
        archive.url
      )
    }

    # Archive: combine collection names, publication year, and month.
    tags <- c(
      CollectionNames(item$collection.names),
      item$year,
      Month(item$month, lang)
    )
    params$archive <- tags

    # Unpaywall link
    if (any(!is.na(GoFish(item$unpaywall)))) {
      params$unpaywall <- item$unpaywall
    }

    # EZproxy link
    if (any(!is.na(GoFish(item$ezproxy)))) {
      params$ezproxy <- item$ezproxy
    }

    # Additional links for Cristin and Zotero
    params$cristin.url <- item$cristin.url
    params$zotero.url <- item$zotero.url

    # Also include key and year.month and lang for later use.
    params$key <- item$key
    params$year.month <- item$year.month
    params$lang <- lang

    # Return as a tibble row with a list column for params
    out <- tibble::tibble(
      key = item$key,
      version = item$version,
      params = list(params),
      year.month = item$year.month,
      lang = lang
    )
    return(out)
  }

  start.message <- sprintf(
    "Converting %s to Rmd params",
    Numerus(nrow(monthlies), "item")
  )

  monthlies.data <- c2z::ProcessData(
    data = monthlies,
    func = CreateParams,
    by.rows = TRUE,
    min.multisession = min.multisession,
    n.workers = n.workers,
    limit = 100,
    use.multisession = use.multisession,
    start.message = start.message,
    handler = handler,
    silent = silent
  )

  results <- monthlies.data$results
  log <- c(log, monthlies.data$log)

  return(list(results = results, log = log))
}
