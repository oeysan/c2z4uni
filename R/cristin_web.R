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
#' @param full.update Logical, indicating whether to perform a full update of
#' user information from INN. Default is FALSE.
#' @param lang Language for localization ("nb", "nn", "no", or "en"). Defaults
#' to "nn".
#' @param silent Logical, indicating whether to suppress log messages. Default
#' is FALSE.
#' @param log List to store log messages. Default is an empty list.
#' @return A tibble containing metadata and HTML content for web display.
#' @details Used with `CristinMonthly` to create month-to-month bibliography in
#' markdown format
#' @examples
#' \donttest{
#'   # Create monthlies for unit 209.5.10.0
#'   example <- CristinMonthly(
#'     c2z::Zotero(
#'       id = "4827927",
#'       api = "RqlAmlH5l1KPghfCseAq1sQ1",
#'       user = FALSE
#'     ),
#'     unit.id = "209.5.10.0",
#'     start.date = "2023-07",
#'     post = TRUE,
#'     silent = TRUE
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
CristinWeb <- function(monthlies,
                       sdg.data = NULL,
                       sdg.path = NULL,
                       archive.url = NULL,
                       local.storage = NULL,
                       user.cards = TRUE,
                       full.update = FALSE,
                       lang = "nn",
                       silent = FALSE,
                       log = list()) {

  # Visible bindings
  cristin.id <- inn.cards <- NULL

  # Languages
  # Set lang as nn if no
  if (lang %in% c("no")) lang <- "nn"
  # Set lang to en if not Norwegian
  if (!lang %in% c("nb", "nn", "no")) lang <- "en"

  # Update user info @ inn if user.cards = TRUE
  if (user.cards) {
    inn.cards <- GetCards(local.storage, full.update, lang, silent)
  } # End user.cards

  # Function to create md files
  CreateMd <- function(item) {

    # Visible bindings
    title <- reference <- abstract <- contributors <- sdg <- archive <-
      unpaywall <- ezproxy <- reference.button <- abstract.button <-
      contributors.button <- sdg.button <- cristin.button <- zotero.button <-
      archive.button <- unpaywall.button <- ezproxy.button <- keywords <-
      keywords.button <- synopsis <- synopsis.button <- NULL

    # Function to create a vector of collection names
    CollectionNames <- \(x) {
      x |>
        unlist() |>
        strsplit(split = "\\|\\|") |>
        unlist() |>
        as.character() |>
        Trim()
    }

    # Define title
    if (any(!is.na(GoFish(item$title)))) {
      title <- item$title
    } else {
      title <- item$key
    }

    # Reference
    if (any(!is.na(GoFish(item$bib)))) {
      reference <- htmltools::tags$article(
        id = paste0("csl-bib-container-", item$key),
        class = "csl-bib-container",
        htmltools::HTML(item$bib)
      )
    }

    # Keywords
    keywords <- item |>
      mutate(
        keywords = purrr::map_chr(
          Map(c, research.type, research.design, keywords),
          ~ paste(.x, collapse = ", "))
      ) |>
      dplyr::pull(keywords) |>
      GoFish(type = "")
    if (keywords != "") {
      keywords <- htmltools::tags$article(
        htmltools::h1(Dict("keywords", lang)),
        keywords,
        id = paste0("keywords-article-", item$key),
        class = "abstract-article"
      )
      keywords.button <- htmltools::a(
        Dict("keywords", lang),
        href = paste0("#", paste0("keywords-article-", item$key)),
        class = "csl-bib-button"
      )
    }

    # Synopsis
    if (any(!is.na(GoFish(item$synopsis)))) {
      synopsis <- htmltools::tags$article(
        htmltools::h1(Dict("about_pub", lang)),
        item$synopsis,
        id = paste0("about-article-", item$key),
        class = "abstract-article"
      )
      synopsis.button <- htmltools::a(
        Dict("about_pub", lang),
        href = paste0("#", paste0("about-article-", item$key)),
        class = "csl-bib-button"
      )
    }

    # Abstract
    if (any(!is.na(GoFish(item$abstract)))) {
      abstract <- htmltools::tags$article(
        htmltools::h1(Dict("abstract", lang)),
        item$abstract,
        id = paste0("abstract-article-", item$key),
        class = "abstract-article"
      )
      abstract.button <- htmltools::a(
        Dict("abstract", lang),
        href = paste0("#", paste0("abstract-article-", item$key)),
        class = "csl-bib-button"
      )
    }

    # Contributors
    if (any(!is.na(GoFish(item$cristin.ids[[1]]))) &
        any(nrow(inn.cards))) {

      ids <- item$cristin.ids[[1]]
      inn.card <- inn.cards |>
        dplyr::filter(cristin.id %in% ids) |>
        dplyr::arrange(match(cristin.id, ids))

      if (any(nrow(inn.card))) {
        contributors <- htmltools::tags$article(
          htmltools::h1(Dict("contributors", lang)),
          htmltools::HTML(inn.card$card),
          id = paste0("contributors-article-", item$key),
          class = "contributors-article"
        )
        contributors.button <- htmltools::a(
          Dict("contributors", lang),
          href = paste0("#", paste0("contributors-article-", item$key)),
          class = "csl-bib-button"
        )
      }
    }

    # SDG
    if (any(!is.na(GoFish(item$sdg[[1]]))) & any(length(sdg.data$sum))) {

      sdg <- htmltools::tagAppendChild(
        htmltools::tags$article(
          htmltools::h1(Dict("sdg", lang)),
          id = paste0("sdg-article-", item$key),
          class = "sdg-article"
        ),
        htmltools::div(
          SdgInfo(
            sdg.data$sum,
            as.numeric(item$sdg[[1]]),
            lang = lang,
            sdg.path,
            archive.url
          ) |>
            do.call(what = htmltools::HTML),
          class = "sdg-container"
        )
      )

      sdg.button <- htmltools::a(
        Dict("sdg", lang),
        href = paste0("#", paste0("sdg-article-", item$key)),
        class = "csl-bib-button"
      )
    }

    # Archive
    tags <- c(
      CollectionNames(item$collection.names),
      item$year,
      Month(item$month, lang)
    )
    archive <- htmltools::tags$article(
      htmltools::h1(Dict("archive", lang)),
      htmltools::tags$ul(
        purrr::map(tags, ~ htmltools::tags$li(.x))
      ),
      id = paste0("taxonomy-article-", item$key),
      class = "taxonomy-article"
    )
    archive.button <- htmltools::a(
      Dict("archive", lang),
      href = paste0("#", paste0("taxonomy-article-", item$key)),
      class = "csl-bib-button"
    )

    # Unpaywall
    if (any(!is.na(GoFish(item$unpaywall)))) {
      unpaywall.button <- htmltools::a(
        "Unpaywall",
        href = item$unpaywall,
        class = "csl-bib-button"
      )
    }

    # Unpaywall
    if (any(!is.na(GoFish(item$ezproxy)))) {
      ezproxy.button <- htmltools::a(
        "EZproxy",
        href = item$ezproxy,
        class = "csl-bib-button"
      )
    }

    # Add Cristin and Zotero buttons
    cristin.button <- htmltools::a(
      "Cristin",
      href = item$cristin.url,
      alt = "Cristin URL",
      class = "csl-bib-button"
    )
    zotero.button <- htmltools::a(
      "Zotero",
      href = item$zotero.url,
      alt = "Zotero URL",
      class = "csl-bib-button"
    )

    # Combine params
    md.frontmatter <- sprintf(
      'title: "%s"\ntype: pub',
      gsub("\"", "'", title)
    )

    md.html <- htmltools::tagList(
      htmltools::h1(Dict("publications", lang)),
      htmltools::tagAppendChildren(
        # Reference
        reference,
        # Buttons
        htmltools::div(
          archive.button,
          cristin.button,
          zotero.button,
          keywords.button,
          synopsis.button,
          contributors.button,
          sdg.button,
          unpaywall.button,
          ezproxy.button,
          class = "csl-bib-buttons"
        ),
        htmltools::div(
          id = paste0("csl-bib-meta-container-", item$key)
        )
      ),
      htmltools::div(
        # Synopsis
        synopsis,
        # Keywords,
        keywords,
        # Abstract
        abstract,
        # Contributors
        contributors,
        # SDG
        sdg,
        # Taxonomy
        archive,
        id = paste0("csl-bib-meta-", item$key),
        class = "csl-bib-meta"
      )
    )

    md <- tibble::tibble(
      key = item$key,
      frontmatter = md.frontmatter,
      html = as.character(md.html),
      year.month = item$year.month
    )

    return (md)
  }


  # Markdownlist
  markdowns <- list()
  # Start time for query
  query.start <- Sys.time()
  # Cycle through monthlies and create markdowns
  for (i in seq_len(nrow(monthlies))) {
    markdowns[[i]] <- CreateMd(monthlies[i, ])
    # Estimate time of arrival
    log.eta <-
      LogCat(
        Eta(query.start,
            i,
            nrow(monthlies)),
        silent = silent,
        flush = TRUE,
        log = log,
        append.log = FALSE
      )
  }

  # Create tibble
  markdowns <- dplyr::bind_rows(markdowns)

  return (markdowns)
}
