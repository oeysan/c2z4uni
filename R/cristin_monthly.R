#' @title Create a month-to-month bibliography of selected units
#' @description Create a bibliography in a newsletter format from month-to-month
#' From either a specified unit or a set of units (e.g., A University ->
#' Faculties -> Departments -> Groups).
#' @param zotero What Zotero library to use
#' @param unit.id What unit to search for
#' @param unit.recursive Find subunits of defined unit key, Default: TRUE
#' @param sdg.host host conducting SDG predictions, Default: NULL
#' @param sdg.batch The batch size for each API call to the SDG host, specifying
#'  the number of items to be processed per request., Default: 20
#' @param get.unpaywall Find Unpaywall resources, Default: FALSE
#' @param get.ezproxy Use ezproxy, Default: FALSE
#' @param ezproxy.host ezproxy host, Default: 'inn.no'
#' @param local.storage Path to local storage of collections, items and
#' bibliography, Default: 'NULL'
#' @param style Citation style to use for appended bibliography and/or
#'   citations, Default: 'apa-single-spaced'
#' @param locale Desired language format of bibliography, Default: 'nn-NO'
#' @param start.date Results created from specified date (YYYY-MM),
#' Default: Sys.Date()
#' @param end.date Results created before specified date (YYYY-MM),
#' Default: NULL
#' @param use.filter Filter out specific items (otherwise supported by
#' `CristinWrangler`), Default: TRUE
#' @param filter Filter out specific item types, will default to item types
#' usually associated with NVI, Default: NULL
#' @param nvi Filter out Cristin items not 1/2 in NVI, Default: TRUE
#' @param check.items Examine items for duplicates and multidepartemental
#' publications, Default: TRUE
#' @param use.identifiers Use if ISBN/DOI identifiers if enabled, Default: TRUE
#' @param use.multisession Logical. If \code{TRUE} (default), parallel
#' processing using multisession is employed; otherwise, processing is sequential.
#' @param min.multisession Minimum number of results for using multisession.
#'   Default: 25
#' @param n.workers Optional integer for the number of workers to be used in
#' multisession mode. If \code{NULL}, it defaults to the number of available
#' cores minus one (with a minimum of one).
#' @param n.chunks Optional integer for the number of chunks to process.
#' If \code{NULL}, it defaults to the number of workers.
#' @param handler The progress handler to be used by the \code{progressr}
#' package. If \code{NULL} and
#'   \code{silent} is \code{FALSE}, it defaults to \code{"txtprogressbar"}.
#'   When \code{silent} is \code{TRUE},
#'   the handler is set to \code{"void"}.
#' @param restore.defaults Logical. If \code{TRUE} (default), the current
#' \code{future} plan is saved and restored upon exit.
#' @param full.update Update bibliography for all items in library,
#' Default: FALSE'
#' @param use.citeproc, Use local citeproc and translator to create bibliography,
#' Default: FALSE
#' @param lang Define bibliography language (nn, nb, en), Default: 'nn'
#' @param post.lang Define language for Zotero collections (nn, nb, en),
#' Default: 'nn'
#' @param post Post new items to specified Zotero library, Default: FALSE
#' @param post.only Post new items and nothing more, Default: FALSE
#' @param silent c2z is noisy, tell it to be quiet, Default: FALSE
#' @param cristin.silent keep queries to Cristin quiet Default: TRUE
#' @param log A list for storing log elements, Default: list()
#' @return A list with bibliography and information for defined units
#' @details Used with `CristinUnits` to create month-to-month bibliography of
#' selected units
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
#'     silent = TRUE
#'   ) |>
#'   c2z:::GoFish(type = NULL)
#'
#'   # Print the three first individual pages (if any)
#'   if (!is.null(example$monthlies)) {
#'     example$monthlies |>
#'       dplyr::select(title) |>
#'       print(n = 3, width = 80)
#'   }
#' }
#' @seealso
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{arrange}},
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{bind_rows}},
#'  \code{\link[dplyr]{bind_cols}}, \code{\link[dplyr]{select}},
#'  \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{case_when}},
#'  \code{\link[dplyr]{context}}, \code{\link[dplyr]{distinct}},
#'  \code{\link[dplyr]{pull}}, \code{\link[dplyr]{mutate-joins}},
#'  \code{\link[dplyr]{join_by}}, \code{\link[dplyr]{rename}},
#'  \code{\link[dplyr]{across}}, \code{\link[dplyr]{reexports}},
#'  \code{\link[dplyr]{mutate_all}}
#'  \code{\link[purrr]{pmap}}, \code{\link[purrr]{map}}
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[stats]{setNames}}
#' @rdname CristinMonthly
#' @export
CristinMonthly <- \(zotero,
                    unit.id,
                    unit.recursive = TRUE,
                    sdg.host = NULL,
                    sdg.batch = 20,
                    get.unpaywall = FALSE,
                    get.ezproxy = FALSE,
                    ezproxy.host = "inn.no",
                    local.storage = NULL,
                    style = "apa-single-spaced",
                    locale = "nn-NO",
                    start.date = Sys.Date(),
                    end.date = NULL,
                    use.filter = TRUE,
                    filter = NULL,
                    nvi = TRUE,
                    check.items = TRUE,
                    use.identifiers = TRUE,
                    use.multisession = FALSE,
                    min.multisession = 25,
                    n.workers = NULL,
                    n.chunks = NULL,
                    handler = NULL,
                    restore.defaults = TRUE,
                    use.citeproc = FALSE,
                    full.update = FALSE,
                    lang = "nn",
                    post.lang = "nn",
                    post = FALSE,
                    post.only = FALSE,
                    silent = FALSE,
                    cristin.silent = TRUE,
                    log = list()) {

  # Visible bindings
  paths <- email <- web <- key <- cristin.url <- zotero.url <-
    bib.item <- extra <- identifier <- core.key <- parentCollection <- name <-
    year.key <- desc <- core <- month.key <- title <- short.title <-
    destination.key <- lang.month <- bib <- bib.body <- bib.web <- abstract <-
    new.keys <- examine.items <- monthlies <- version.x <-
    version.y <- log.eta <- id <- multidepartmental <-
    duplicates <- extras <- cristin.id <- items <- collections <-
    updated.keys <- col.lang <- sdg <- sdg.summary <- zotero.items <-
    new.items <- value <- llm_sdgs_final <- extra <- bibliography <-
    monthlies <- itemType <- NULL

  # Languages
  # Set lang as nn if no
  if (lang %in% c("no")) lang <- "nn"
  # Set lang to en if not Norwegian
  if (!lang %in% c("nb", "nn", "no")) lang <- "en"

  # Define units
  units <- CristinUnits(
    unit.id,
    recursive = unit.recursive,
    lang = post.lang
  )

  if (!any(nrow(units))) {
    # Log
    log <-  LogCat(
      "No units found.",
      silent = silent,
      log = log
    )
    return (log)
  }

  n.paths <- ncol(dplyr::select(units, dplyr::starts_with("path")))+2

  # Try to restore collections if local.storage is TRUE
  collections <- LocalStorage(
    "collections",
    local.storage,
    message = "Loading Collections",
    silent = silent
  )

  # Search collections if collections are empty
  if (full.update | is.null(collections)) {

    # Find collections
    zotero <- c2z::Zotero(
      user = zotero$user,
      id = zotero$id,
      api = zotero$api,
      library = TRUE,
      get.items = FALSE,
      silent = silent,
      force = TRUE,
      log = zotero$log
    )

    collections <- zotero$collections

  }

  # Post monthly data to Zotero library if post is TRUE
  if (post) {

    # Format start.date as beginning of defined month
    start.date <- FloorDate(start.date)
    # Set end.date as start.date plus a month if end.date is NULL
    if (is.null(end.date)) {
      end.date <- ChangeDate(start.date, 1)
      # Else format end.date as beginning of defined month
    } else {
      end.date <- FloorDate(end.date)
      if (start.date >= end.date) end.date <- ChangeDate(start.date, 1)
    }

    # Log number of units and periods
    log <-  LogCat(
      sprintf(
        "Cristin query based on %s from %s" ,
        Numerus(nrow(units), "unit"),
        paste(
          format(start.date, format="%m.%Y"),
          format(end.date, format="%m.%Y"),
          sep = " to "
        )
      ),
      silent = silent,
      log = log
    )

    # Create tibble with k periods equaling 1 month
    period <- seq(start.date, end.date, by = "months") |>
      (\(x) {
        tibble::tibble(
          start.date = x[seq_len(length(x)-1)],
          end.date = x[seq_len(length(x))[-1]]-1
        )
      })()

    # Add period date to units data
    post.data <- tidyr::crossing(units, period)
    # Try to restore post data if local.storage is TRUE
    old.data <- LocalStorage("post_data", local.storage)
    if (any(nrow(old.data))) {
      post.data <- dplyr::anti_join(
        post.data,
        old.data,
        join_by(id, start.date, end.date)
      )
    }

    # Log
    zotero$log <-  LogCat(
      "Creating new collections",
      silent = silent,
      log = zotero$log
    )

    # Start time for query
    query.start <- Sys.time()

    for (i in seq_len(nrow(post.data))) {

      # Current unit
      unit <- post.data[i, ] |>
        dplyr::select(dplyr::starts_with("path")) |>
        (\(x) x[!is.na(x)])()
      # Current month number
      month.i <- as.numeric(format(post.data$start.date[[i]], format="%m"))
      # Current month
      month <- Month(month.i, post.lang)
      # Current year
      year <- as.numeric(format(post.data$start.date[[i]], format="%Y"))
      # Define path name
      path.name <- c(
        unit,
        # year
        year,
        # Month (e.g., 01: Januar)
        sprintf("%02d: %s", month.i, month)
      )

      # Find path to collection
      find.path <- FindPath(collections, path.name)

      # Add to path list
      paths <- append(paths, list(find.path$path))

      # Update data
      collections <- find.path$data

      # Estimate time of arrival
      log.eta <- LogCat(
        Eta(query.start, i, nrow(post.data)),
        silent = silent,
        flush = TRUE,
        log = log,
        append.log = FALSE
      )

    } # End for loop

    # Add to log
    zotero$log <- append(zotero$log, log.eta)

    # Append path to post data
    post.data$path <- paths

    # Save post.data if local.storage is defined
    # Combine old and new data
    post.data <- dplyr::bind_rows(old.data, post.data)

    # Save post.data
    post.data <- LocalStorage("post_data", local.storage, post.data)

    # Save collections
    collections <- LocalStorage("collections", local.storage, collections)

    # use filter if use.filter = TRUE
    if (use.filter) {
      # use predined filter if filter is not defined
      if (is.null(filter)) {
        filter <- c(
          "ACADEMICREVIEW",
          "ARTICLEJOURNAL",
          "ARTICLE",
          "ANTHOLOGYACA",
          "CHAPTER",
          "CHAPTERACADEMIC",
          "CHAPTERARTICLE",
          "COMMENTARYACA",
          "MONOGRAPHACA"
        )
      }
    }

    # Try to restore zotero items if local.storage is defined
    zotero$items <- items <- LocalStorage(
      "items",
      local.storage,
      message = "Loading items",
      silent = silent
    )

    # Run full update if full.update is TRUE or duplicate check is empty
    if (full.update | is.null(zotero$items) & !is.null(collections)) {

      # Find core location (i.e., where all items in units are stored)
      core.location <- collections |>
        dplyr::filter(version > 0 & parentCollection == FALSE) |>
        dplyr::pull(key)

      # Fetch items
      if (length(core.location)) {

        check.keys <- c2z::ZoteroGet(
          ListValue(zotero, "collection.key", core.location),
          format = "versions",
          silent = TRUE,
          append.items = TRUE,
          result.type = "key",
          force = TRUE,
        )$results


        start.message <- sprintf(
          "Fetching %s from Zotero library",
          Numerus(
            nrow(check.keys),
            "item",
          )
        )

        zotero.data <- c2z::ProcessData(
          data = check.keys,
          func = \(data) {
            c2z::ZoteroGet(
              zotero,
              item.keys = data$key,
              item.type = "-attachment || note",
              silent = TRUE,
              force = TRUE,
              use.collection = FALSE
            )$results
          },
          by.rows = FALSE,
          min.multisession = min.multisession,
          n.workers = n.workers,
          n.chunks = n.chunks,
          limit = 100,
          use.multisession = use.multisession,
          start.message = start.message,
          handler = handler,
          silent = silent
        )

        zotero$items <- items <- LocalStorage(
          "items",
          local.storage,
          zotero.data$results,
          message = "Saving items",
          silent = silent
        )
        log <- c(log, zotero.data$log)
      }
    }

    # Define end.date in Cristin format
    end.date <- CeilingDate(ChangeDate(end.date, -1))
    start.message <- sprintf(
      "Searching Cristin using %s",
      Numerus(
        nrow(units),
        "unit",
      )
    )

    UnitResults <- \(data) {

      cristin <- c2z::Cristin(
        unit = data$id,
        created_since = start.date,
        created_before = end.date,
        use.identifiers = use.identifiers,
        use.multisession = use.multisession,
        handler = handler,
        restore.defaults = restore.defaults,
        filter = filter,
        force = TRUE,
        silent = cristin.silent,
        nvi = nvi,
        zotero = zotero
      )

      # Return NULL if no new/modified results
      if (!any(nrow(cristin$results))) return (NULL)

      period.data <- post.data |>
        dplyr::filter(data$id == post.data$id)

      # For each period (row) in period.data, check which elements of
      # cristin$created fall between the start.date and end.date.
      # The result is a list of logical vectors.
      between.results <- purrr::pmap(
        list(period.data$start.date, period.data$end.date),
        \(start.date, end.date) {
          # dplyr::between returns a logical vector for cristin$created
          dplyr::between(cristin$created, start.date, end.date)
        }
      ) |>
        tibble::enframe() |>
        tidyr::unnest_wider(value, names_sep = "")

      # For each column in between.results (excluding the "name" column),
      # extract the corresponding "path" values from period.data where
      # the condition is TRUE.
      cristin$results$collections <- between.results |>
        dplyr::select(-name) |>
        purrr::imap(~ unname(unlist(period.data[.x, "path"])))

      return(cristin$results)

    }

    cristin.data <- c2z::ProcessData(
      data = units,
      func = UnitResults,
      by.rows = TRUE,
      min.multisession = min.multisession,
      n.workers = n.workers,
      n.chunks = n.chunks,
      limit = 100,
      use.multisession = use.multisession,
      start.message = start.message,
      handler = handler,
      silent = silent
    )

    log <- c(log, cristin.data$log)
    new.items <- cristin.data$results

    # Make sure that new items has a unique key.
    new.items <- new.items |>
      # Filter out NA items
      AddMissing(missing.names = c("itemType", "extra")) |>
      dplyr::filter(!is.na(itemType) | !is.na(extra)) |>
      dplyr::mutate(
        key = replace(
          key,
          version == 0,
          purrr::map_chr(which(version == 0), ~ZoteroKey())
        )
      )

    # Find multidepartmental and duplicate items if any items
    if (any(nrow(new.items))) {

      # Log number of units and periods
      zotero$log <- LogCat(
        sprintf(
          "Found %s new, or modfied, %s",
          max(0,nrow(new.items)),
          Numerus(max(0,nrow(new.items)), "item", prefix = FALSE)
        ),
        silent = silent,
        log = zotero$log
      )

      # Examine items
      if (check.items) {
        examine.items <- ExamineItems(
          new.items,
          collections,
          FALSE,
          zotero$log,
          n.paths
        )
      }

      # Update items
      zotero$items <- new.items <- examine.items$items

      if (any(nrow(new.items))) {

        # Remove collections not found in Cristin query
        zotero$collections <- collections |>
          dplyr::filter(key %in% unique(unlist(new.items$collections)))

        # Post Cristin items to specified collections
        zotero <- ZoteroPost(
          zotero,
          silent = silent,
          force = TRUE,
          post.token = TRUE
        )

      }

    } # End add items

    # Define updated collections
    if (any(nrow(zotero$collections))) {

      # Update collections
      collections <- UpdateInsert(collections, zotero$collections)

      # Save collections if local.storage is defined
      collections <- LocalStorage(
        "collections",
        local.storage,
        collections,
        message = "Saving collections",
        silent = silent
      )

      # Update Zotero list
      zotero$collections <- collections |>
        dplyr::filter(version > 0) |>
        dplyr::distinct(key, .keep_all = TRUE)

    }

    # Define updated collections
    if (any(nrow(new.items))) {

      items <- UpdateInsert(items, zotero$items)
      zotero$items <- items <- LocalStorage(
        "items",
        local.storage,
        items,
        message = "Saving items",
        silent = silent
      )

      updated.keys <- LocalStorage(
        "updated_keys",
        local.storage,
        items$key,
        lang = lang,
        message = "Saving updated keys",
        silent = silent
      )


    }

    # Return Zotero list if post.only is TRUE
    if (post.only) return (zotero)

  } # End post

  # Remove collections with no items
  collections  <- collections  |>
    dplyr::filter(version > 0) |>
    dplyr::distinct(key, .keep_all = TRUE)

  # Return NULL if there are no collections, and thus no items
  if (!any(nrow(collections))) {
    return (NULL)
  }

  # Find all ancestors of defined unit key
  units <- CristinUnits(
    unit.id,
    recursive = unit.recursive,
    ancestors = TRUE,
    lang = post.lang
  )

  # Set unit paths
  unit.paths <- collections |>
    dplyr::filter(name %in% lapply(units$core, \(x) tail(x, 1)))  |>
    dplyr::arrange(match(name, lapply(units$core, \(x) tail(x, 1)))) |>
    dplyr::transmute(
      id = purrr::map_chr(name, ~ {
        x <- .x
        units |>
          dplyr::filter(dplyr::if_any(dplyr::starts_with("path"), ~ .x == x)) |>
          dplyr::slice_min(order_by = id, n = 1) |>
          dplyr::pull(id)
      }),
      name,
      key,
      parentCollection,
      paths = purrr::map(key, ~ AncestorPath(collections, .x)),
      level = purrr::map_int(paths, length),
      affiliated = purrr::map_chr(key, ~ {
        x <- collections |>
          dplyr::filter(
            parentCollection == .x &
              grepl(Dict("affiliation", post.lang), name)
          ) |>
          dplyr::pull(key)
        if (!length(x)) x <- NA

        return (x)
      })
    )

  if (lang != post.lang) {
    units <- CristinUnits(unit.paths$id[[1]], recursive = TRUE, lang = lang) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::everything(), ~
            replace(.x, .x == Dict("affiliation", lang), NA)
        ),
        name = pmap_chr(
          dplyr::across(dplyr::starts_with("path")), ~ {
            tail(na.omit(c(...)), 1)
          }
        )
      ) |>
      dplyr::select(id, name)

    unit.paths <- unit.paths |>
      dplyr::rows_update(units, by = "id", unmatched = "ignore")
  }

  # Save unit paths if local storage is defined
  unit.paths <- LocalStorage(
    "unit_paths",
    local.storage,
    unit.paths,
    lang = lang,
    message = "Saving unit paths",
    silent = silent
  )

  # Return NULL if no unit paths.
  if (!any(nrow(unit.paths))) {
    # Log
    log <-  LogCat(
      "Zotero library is empty!",
      silent = silent,
      log = log
    )
    return (NULL)
  }

  # Create Monthlies
  # Loading items, bibliography and monthlies
  if (!is.null(local.storage)) {
    items <- LocalStorage("items", local.storage)
    bibliography <- LocalStorage("bibliography", local.storage, lang = lang)
    monthlies <- LocalStorage("monthlies", local.storage, lang = lang)
  }

  # Creating monthlies
  monthlies <- CreateMonthlies(
    zotero = zotero,
    items = items,
    bibliography = bibliography,
    monthlies = monthlies,
    unit.paths = unit.paths,
    collections = collections,
    full.update = full.update,
    use.citeproc = use.citeproc,
    use.multisession = use.multisession,
    min.multisession = min.multisession,
    n.workers = n.workers,
    n.chunks = n.chunks,
    handler = handler,
    restore.defaults = restore.defaults,
    lang = lang,
    style = style,
    locale = locale,
    silent = silent,
    log = zotero$log
  )

  log <- monthlies$log
  # Updated keys
  updated.keys <- monthlies$updated.keys

  # Saving items, bibliography and monthlies
  items <- LocalStorage(
    "items",
    local.storage,
    monthlies$items,
    message = "Saving items",
    silent = silent
  )
  bibliography <- LocalStorage(
    "bibliography",
    local.storage,
    monthlies$bibliography,
    lang = lang,
    message = "Saving bibliography",
    silent = silent
  )
  monthlies <- monthlies$monthlies

  # Create extras if any items
  if (any(nrow(items))) {

    # Create Monthlies
    # Loading extras
    extras <- LocalStorage(
      "monthlies_extras",
      local.storage,
      message = "Loading monthlies")

    # Check if any missing items
    if (any(nrow(extras)) && !full.update) {
      extras.data <- items |>
        dplyr::anti_join(extras, by = c("key", "version"))
    } else {
      extras.data <- items
    }


    if (any(nrow(extras.data))) {

      # Updated keys
      updated.keys <- c(updated.keys, extras.data$key)
      # Perpare for collecting extras for new keys

      start.message <- sprintf(
        "Creating %s",
        Numerus(
          nrow(extras.data),
          "enhanced bibliography",
          "enhanced bibliographies"
        )
      )

      extras.data <- c2z::ProcessData(
        data = extras.data,
        func = \(data) {
          CreateExtras(
            extras = data,
            get.unpaywall = get.unpaywall,
            get.ezproxy = get.ezproxy,
            ezproxy.host = ezproxy.host,
            silent = TRUE
          )
        },
        by.rows = TRUE,
        min.multisession = min.multisession,
        n.workers = n.workers,
        n.chunks = n.chunks,
        limit = 100,
        use.multisession = use.multisession,
        start.message = start.message,
        handler = handler,
        silent = FALSE
      )
      log <- c(log, extras.data$log)
      extras <- LocalStorage(
        "monthlies_extras",
        local.storage,
        UpdateInsert(extras, extras.data$results),
        message = "Saving extras",
        silent = silent
      )
    }
  }

  # Create SDGs if any items
  if (any(nrow(items)) & !is.null(sdg.host)) {
    # Loading SDG Predictions
    sdg <- LocalStorage(
      "sdg_predictions",
      local.storage,
      message = "Loading SDG Predictions",
      silent = silent
    )

    # Creating SDG Predictions
    sdg <- CreateSdgs(
      items = items,
      sdg = sdg,
      sdg.host = sdg.host,
      sdg.batch = sdg.batch,
      full.update = full.update,
      lang = lang,
      silent = silent,
      log = log
    )

    # Saving log
    log <- sdg$log
    # Updated keys
    updated.keys <- c(updated.keys, sdg$updated.keys)

    # Saving SDG Predictions
    sdg <- LocalStorage(
      "sdg_predictions",
      local.storage,
      sdg$predictions,
      message = "Saving SDG Predictions",
      silent = silent
    )

    # Create SDG Summary
    if (any(nrow(sdg))) {
      sdg.summary <- LocalStorage(
        "sdg_predictions_summary",
        local.storage,
        SdgSummary(sdg),
        message = "Creating SDG Summary",
        silent = silent
      )
    }
  }

  # Find multi-departmental and duplicate items
  if (check.items & any(nrow(items))) {

    # Log examine items
    log <-  LogCat(
      "Searching for duplicates and multi-departmental publications",
      silent = silent,
      log = log
    )

    examine.items <- ExamineItems(
      items,
      collections,
      silent,
      log,
      n.paths
    )
    log <- examine.items$log
    multidepartmental <- examine.items$multidepartmental
    duplicates <- examine.items$duplicates
  }

  # Define updated keys.
  updated.keys <- LocalStorage(
    "updated_keys",
    local.storage,
    updated.keys,
    lang = lang,
    message = "Saving updated keys",
    silent = silent
  )

  # Finalize monthlies
  if (any(nrow(monthlies)) & any(nrow(extras))) {
    monthlies <- monthlies |>
      dplyr::left_join(
        extras,
        by = join_by(key, version),
        suffix = c("", ".remove")) |>
      dplyr::select(-c(dplyr::ends_with(".remove")))
  }

  if (any(nrow(monthlies)) & any(nrow(sdg))) {
    col.lang <- if (lang == "en") "" else paste0("_", lang)
    monthlies <- monthlies |>
      dplyr::left_join(
        sdg |>
          dplyr::select(
            key,
            version,
            sdg = llm_sdgs_final,
            research.field = !!sym(paste0("llm_keywords", col.lang)),
            research.type = !!sym(paste0("llm_research_type", col.lang)),
            research.design = !!sym(paste0("llm_research_design", col.lang)),
            theories = !!sym(paste0("llm_theories", col.lang)),
            synopsis = !!sym(paste0("llm_synopsis", col.lang)),
            keywords = !!sym(paste0("llm_keywords", col.lang))
          ),
        by = join_by(key, version),
        suffix = c("", ".remove")) |>
      dplyr::select(-c(dplyr::ends_with(".remove")))
  }

  monthlies <- LocalStorage(
    "monthlies",
    local.storage,
    monthlies,
    lang = lang,
    message = "Saving monthlies",
    silent = silent
  )

  # Log examine items
  log <-  LogCat(
    "Finalizing monthlies",
    silent = silent,
    log = log
  )


  unit.key <- unit.paths |>
    dplyr::filter(id == unit.id) |>
    dplyr::pull(key)

  if (any(nrow(items))) {
    items <- items |>
      dplyr::filter(purrr::map2_lgl(unit.key, collections, ~ .x %in% .y))
  }
  if (any(nrow(bibliography))) {
    bibliography <- bibliography |>
      dplyr::semi_join(items, by = c("key", "version"))
  }
  if (any(nrow(monthlies))) {
    monthlies <- monthlies |>
      dplyr::semi_join(items, by = c("key", "version"))
  }
  if (any(nrow(sdg))) {
    sdg <- sdg |>
      dplyr::semi_join(items, by = c("key", "version"))
  }

  # Create return.list
  return.list <- list(
    unit.paths = unit.paths,
    items = items,
    bibliography = bibliography,
    monthlies =  monthlies,
    sdg = sdg,
    sdg.summary = sdg.summary,
    updated.keys = updated.keys,
    multidepartmental = multidepartmental,
    duplicates = duplicates,
    log = log
  )

  return(return.list)

}
