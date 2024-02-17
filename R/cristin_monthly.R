#' @title Create a month-to-month bibliography of selected units
#' @description Create a bibliography in a newsletter format from month-to-month
#' From either a specified unit or a set of units (e.g., A University ->
#' Faculties -> Departments -> Groups).
#' @param zotero What Zotero library to use
#' @param unit.key What unit to search for
#' @param unit.recursive Find subunits of defined unit key, Default: TRUE
#' @param sdg.script script to run SDG predictions, Default: NULL
#' @param sdg.model model to use for SDG predictions, Default: NULL
#' @param sdg.cutoff cut off value for SDG predictions, Default: '0.98'
#' @param sdg.host host conducting SDG predictions, Default: NULL
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
#' @param full.update Update bibliography for all items in library,
#' Default: FALSE
#' @param lang Define bibliography language (nn, nb, en), Default: 'nn'
#' @param post.lang Define language for Zotero collections (nn, nb, en),
#' Default: 'nn'
#' @param post Post new items to specified Zotero library, Default: FALSE
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
#'       api = "RqlAmlH5l1KPghfCseAq1sQ1",
#'       user = FALSE
#'     ),
#'     unit.key = "209.5.10.0",
#'     start.date = "2023-07",
#'     post = TRUE,
#'     silent = TRUE
#'   )
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
                    unit.key,
                    unit.recursive = TRUE,
                    sdg.script = NULL,
                    sdg.model = NULL,
                    sdg.cutoff = 0.98,
                    sdg.host = NULL,
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
                    full.update = FALSE,
                    lang = "nn",
                    post.lang = "nn",
                    post = FALSE,
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
    duplicates <- extras <- cristin.id <- items <- collections <- NULL

  # Languages
  # Set language to en if not no
  lang <- if (lang %in% c("no", "nn", "nb")) "no" else "en"
  post.lang <- if (post.lang %in% c("no", "nn", "nb")) "no" else "en"

  # Define units
  units <- CristinUnits(
    unit.key,
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
  if (!is.null(local.storage)) {

    collections <- GoFish(
      readRDS(file.path(local.storage, "collections.rds")),
      NULL
    )

  }

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
    if (!is.null(local.storage)) {

      old.data <- GoFish(
        readRDS(file.path(local.storage, "post_data.rds")),
        NULL
      )

      if (any(nrow(old.data))) {
        post.data <- dplyr::anti_join(
          post.data,
          old.data,
          join_by(id, start.date, end.date)
        )
      }

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
    if (!is.null(local.storage)) {

      # Combine old and new data
      post.data <- dplyr::bind_rows(old.data, post.data)

      # Save post.data
      saveRDS(post.data, file.path(local.storage, "post_data.rds"))

      # Save collections
      saveRDS(collections, file.path(local.storage, "collections.rds"))

    }

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
    if (!is.null(local.storage)) {

      zotero$items <- GoFish(
        readRDS(file.path(local.storage, "items.rds")),
        NULL
      )

    }

    # Run full update if full.update is TRUE or duplicate check is empty
    if (full.update | is.null(zotero$items) & !is.null(collections)) {

      # Find core location (i.e., where all items in units are stored)
      core.location <- collections |>
        dplyr::filter(version > 0 & parentCollection == FALSE) |>
        dplyr::pull(key)

      # Fetch items
      if (length(core.location)) {
        zotero <- c2z::Zotero(
          zotero = zotero,
          collection.key = core.location,
          search.collections = FALSE,
          item.type = "-attachment || note",
          library = TRUE,
          silent = silent,
          force = TRUE,
          log = zotero$log
        )
      }

      # Save items if local.storage is defined
      if (!is.null(local.storage) & !is.null(zotero$items)) {

        # Log
        zotero$log <-  LogCat(
          "Saving items",
          silent = silent,
          log = zotero$log
        )

        # Save items
        saveRDS(zotero$items, file.path(local.storage, "items.rds"))

      }
    }

    # Log Cristin search
    zotero$log <-  LogCat(
      "Searching Cristin",
      silent = silent,
      log = zotero$log
    )

    # Start time for query
    query.start <- Sys.time()

    # Cycle through post.data searching Cristin for each row
    ## Should perhaps vectorize, but for loop seems more informative tbh
    for (i in seq_len(nrow(units))) {

      # Searching Cristin
      cristin <- c2z::Cristin(
        unit = units$id[[i]],
        created_since = start.date,
        created_before = CeilingDate(ChangeDate(end.date, -1)),
        use.identifiers = use.identifiers,
        filter = filter,
        force = FALSE,
        silent = cristin.silent,
        nvi = nvi,
        zotero = zotero
      )

      if (any(nrow(cristin$results))) {

        # Only use period for current units
        period.data <-  post.data |>
          dplyr::filter(units$id[[i]] == post.data$id)

        # Define the collections of items
        cristin$results$collections <- period.data |>
          # Find date period (year - month)
          dplyr::transmute(
            purrr::pmap(
              list(start.date, end.date), ~
                t(dplyr::between(cristin$created, .x, .y)) |>
                as.data.frame()
            )
          ) |>
          (\(x) {
            x <- do.call(dplyr::bind_rows, x)
            lapply(seq_len(ncol(x)), \(i) unlist(period.data[ x[, i], "path"]))
          })()

        # Accumulate results
        items <- dplyr::bind_rows(items, cristin$results)

      } # End Cristin results

      # Estimate time of arrival
      log.eta <- LogCat(
        Eta(query.start, i, nrow(units)),
        silent = silent,
        flush = TRUE,
        log = log,
        append.log = FALSE
      )

    } # End for loop

    # Add to log
    zotero$log <- append(zotero$log, log.eta)

    # Log number of units and periods
    zotero$log <- LogCat(
      sprintf(
        "Found %s new, or modfied, %s",
        max(0,nrow(items)),
        Numerus(max(0,nrow(items)), "item", prefix = FALSE)
      ),
      silent = silent,
      log = zotero$log
    )

    # Find multidepartmental and duplicate items if any items
    if (any(nrow(items))) {

      # Examine items
      if (check.items) {
        examine.items <- ExamineItems(
          items,
          collections,
          TRUE,
          zotero$log,
          n.paths
        )
      }

      # Update items
      zotero$items <- examine.items$items

      # Remove collections not found in Cristin query
      zotero$collections <- collections |>
        dplyr::filter(key %in% unique(unlist(examine.items$items$collections)))

      # Post Cristin items to specified collections
      zotero <- ZoteroPost(zotero, silent = silent, force = TRUE)

    } # End add items

    # Define updated collections
    if (!is.null(zotero$collections)) {

      # Update collections
      collections <- AddMissing(collections, "prefix", NA_character_) |>
        dplyr::rows_update(zotero$collections, by = "key", unmatched = "ignore")

      # Save collections if local.storage is defined
      if (!is.null(local.storage)) {

        # Log
        zotero$log <-  LogCat(
          "Saving collections",
          silent = silent,
          log = zotero$log
        )

        # Save items
        saveRDS(
          zotero$collections, file.path(local.storage, "collections.rds")
        )

      }

    }

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
    unit.key,
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
  if (!is.null(local.storage)) {
    saveRDS(
      unit.paths,
      file.path(local.storage, paste0("unit_paths_", lang, ".rds"))
    )
  }

  # Create Monthlies
  monthlies <- CreateMonthlies(
    zotero,
    unit.paths,
    collections,
    local.storage,
    full.update,
    lang,
    style,
    locale,
    silent,
    log = zotero$log
  )

  # Create extras if any monthlies
  if (any(nrow(monthlies$items))) {
    extras <- CreateExtras(
      monthlies,
      sdg.script,
      sdg.model,
      sdg.cutoff,
      sdg.host,
      get.unpaywall,
      get.ezproxy,
      ezproxy.host,
      local.storage,
      full.update,
      lang,
      silent,
      log = monthlies$log
    )
  }

  # Find multidepartmental and duplicate items
  if (check.items & any(nrow(monthlies$items))) {

    # Log examine items
    extras$log <-  LogCat(
      "Searching for duplicates and multidepartmental publications",
      silent = silent,
      log = extras$log
    )

    examine.items <- ExamineItems(
      monthlies$items,
      collections,
      silent,
      extras$log,
      n.paths
    )
    multidepartmental <- examine.items$multidepartmental
    duplicates <- examine.items$duplicates
  }

  # Join monthlies and extras
  if (any(nrow(monthlies$monthlies)) & any(nrow(extras$extras))) {
    monthlies$monthlies <- monthlies$monthlies |>
      dplyr::left_join(extras$extras, by = "key", suffix = c("", ".remove")) |>
      dplyr::select(-c(dplyr::ends_with(".remove")))
  }

  # Create return.list
  return.list <- list(
    unit.paths = unit.paths,
    monthlies =  monthlies$monthlies,
    sdg = extras$sdg,
    updated.keys = monthlies$updated.keys,
    multidepartmental = multidepartmental,
    duplicates = duplicates,
    log = examine.items$log
  )

  return(return.list)

}
