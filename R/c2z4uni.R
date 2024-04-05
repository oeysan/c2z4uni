################################################################################
#####################################Import#####################################
################################################################################

#' @import c2z
#' @importFrom dplyr
#'   arrange bind_rows case_when coalesce distinct filter group_by mutate
#'   na_if one_of pull row_number select slice_head transmute ungroup
#'   full_join join_by any_of
#' @importFrom httr
#'   GET RETRY add_headers content http_error
#' @importFrom jsonlite
#'   fromJSON toJSON unbox
#' @importFrom purrr
#'   map pmap pmap_chr
#' @importFrom rlang
#'   syms sym :=
#' @importFrom rvest
#'   html_attr html_attrs html_children html_name html_nodes html_text read_html
#' @importFrom stats
#'   setNames
#' @importFrom tibble
#'   add_column as_tibble remove_rownames tibble is_tibble
#' @importFrom utils
#'   adist flush.console head tail write.csv
#' @importFrom htmltools
#'   tags h1 div a span
#' @importFrom tidyr
#'   unnest
#' @importFrom readr
#'   read_rds
NULL
#> NULL

################################################################################
#################################Internal Data##################################
################################################################################

#' @title i18n
#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname) {
  DictLoad("no", pkgname)
  DictLoad("en", pkgname)
}

#' @title zotero.types
#' @keywords internal
#' @noRd
zotero.types <- c2z::zotero.types

################################################################################
###############################Internal Functions###############################
################################################################################

#' @title Ezproxy
#' @keywords internal
#' @noRd
Ezproxy <- \(doi, href = FALSE, host = "inn.no") {

  # Return NA if not found
  if (any(is.na(doi))) {
    return (NA)
  }

  # define ezproxy URL
  ezproxy <- sprintf("http://ezproxy.%s/login?url=", host)
  # define DOI base
  doi.base <- "https://doi.org/"

  # Search for DOI href if href = TRUE
  if (href) {

    # Return NA if href DOI not found
    if (!grepl(paste0("href=\"", doi.base), doi)) {
      return (doi)
    }

    # Insert ezproxy
    doi <- gsub(
      "href=\"https://doi",
      paste0("href=\"", ezproxy, doi.base),
      doi
    )
    # Else prefix ezproxy to DOI
  } else {

    # If doi.base in doi prefix ezproxy
    if (grepl(doi.base, doi)) {

      doi <- paste0(ezproxy, doi)
      # Else prefix both ezproxy and doi.base
    } else {

      doi <- paste0(ezproxy, doi.base, doi)

    }

  }

  return (doi)

}

#' @title GetDoi
#' @keywords internal
#' @noRd
GetDoi <- \(doi, extra) {

  # Try to get DOI from extra if DOI is NA
  if (any(is.na(doi))) {
    doi <- tail(ZoteroId("DOI", extra), 1)
  }

  return (doi)
}

#' @title Unpaywall
#' @keywords internal
#' @noRd
Unpaywall <- \(doi, email = NULL) {

  # Define url to Unpaywall api
  url <- paste0("https://api.unpaywall.org/v2/", doi)
  # Set email if empty
  if (is.null(email)) email <- Sys.getenv("CROSSREF_EMAIL")

  # Query Unpaywall
  httr.get <- Online(
    httr::RETRY(
      "GET",
      url = url,
      query = list(
        email = email
      ),
      quiet = TRUE
    ),
    silent = TRUE
  )

  # Return NA if not found
  if (httr.get$error) {
    return (NA)
  }

  # Create list from JSON
  unpaywall <- httr::content(httr.get$data)

  # Return NA if not open source
  if (!any(GoFish(unpaywall$is_oa, FALSE))) {
    return (NA)
  }

  # Try to get location of pdf
  open.access <- GoFish(unpaywall$best_oa_location$url_for_pdf)

  # if open.access is not defined try URL
  if (any(is.na(open.access))) {
    open.access <- GoFish(unpaywall$best_oa_location$url)
  }

  # if open.access is not defined try landing site
  if (any(is.na(open.access))) {
    open.access <- GoFish(unpaywall$best_oa_location$url_for_landing_page)
  }

  return (open.access)

}

#' @title CheckDesc
#' @keywords internal
#' @noRd
CheckDesc <- \(type, change.value = NULL) {

  desc.file <- file.path(".", "DESCRIPTION")
  desc.info <- file.info(desc.file <- file.path(".", "DESCRIPTION"))
  desc.read <- readLines(desc.file)
  type <- sprintf("^%s\\:", type)
  desc.field <- grep(type, tolower(desc.read))

  if (!length(desc.field)) {
    return (NULL)
  }

  # Get current value
  value <- Trim(gsub(".*:\\s*", "", desc.read))[[desc.field]]

  # Change value if change.value is defined
  if (!is.null(change.value)) {
    value <- change.value
    desc.read[desc.field] <- sprintf(
      "%s: %s",
      gsub(":.*", "", desc.read[desc.field]),
      value
    )
    writeLines(desc.read, con = desc.file)
  }

  return (value)

}

#' @title DictLoad
#' @keywords internal
#' @noRd
DictLoad <- \(lang = "no", pkgname) {

  # Get the path to the CSV file
  csv.file <- system.file(
    "extdata/i18n",
    paste0(lang, ".csv"),
    package = pkgname
  )
  # Read the CSV file into a data frame
  dict.data <- readr::read_csv(csv.file, col_types = readr::cols())

  # Export dict.data
  assign(paste0("i18n.", lang), dict.data, envir = asNamespace(pkgname))

}

#' @title Dict
#' @keywords internal
#' @noRd
Dict <- \(x = NULL,
          lang = "no",
          count = 1,
          to.lower = FALSE,
          prefix = FALSE,
          error = NULL,
          i18n = NULL) {



  # Visible bindings
  key <- NULL

  # Define language
  lang <- if (!lang %in% c("nb", "nn", "no")) "en" else "no"

  # Use extdata if data is not defined
  if (is.null(i18n)) {
    i18n <- if (lang == "no") i18n.no else i18n.en
  }

  # Return Dict data if x not defined
  if (is.null(x)) {
    return (i18n)
  }

  # Define singular or pluralis
  numerus <- if (count == 1) "singularis" else "pluralis"
  anti.numerus <- if (count != 1)  "singularis" else "pluralis"

  # Create dictionary
  string <- i18n |>
    # Filter by defined string
    dplyr::filter(tolower(x) == tolower(key)) |>
    # Decide numerus
    dplyr::mutate(
      numerus = dplyr::case_when(
        # Set as numerus if not NA
        !is.na(!!rlang::sym(numerus)) ~ !!rlang::sym(numerus),
        # Set as anti.numerus if NA
        TRUE ~ !!rlang::sym(anti.numerus)
      )
    ) |>
    # Pull numerus
    dplyr::pull(numerus) |>
    # Trim
    Trim()

  # Set as tolower if TRUE
  if (to.lower) string <- tolower(string)

  # Append count if prefix is TRUE
  if (prefix & any(length(string))) string <- paste(count, string)

  # set as error of x still not defined
  if (!length(string)) string <- error

  return (string)

}

#' @title Numerus
#' @keywords internal
#' @noRd
Numerus <- \(count, singularis, pluralis = NULL, prefix = TRUE) {

  if (is.null(pluralis)) pluralis <- paste0(singularis, "s")
  string <- if (count == 1) singularis else pluralis

  if (prefix) string <- paste(count, string)

  return (string)

}

#' @title FilePath
#' @keywords internal
#' @noRd
FilePath <- \(...) {
  Clean <- \(x) !is.null(x) && !is.na(x) && length(x) > 0 && x != ""
  x <- Filter(Clean, Trim(c(...)))
  do.call(file.path, as.list(x))
}


#' @title PadVector
#' @keywords internal
#' @noRd
AddPad <- function(x, pad = NULL, add = "0") {
  if (is.null(pad)) pad <- max(nchar(x))
  gsub("\\s", add, format(x, width = pad))
}

#' @title FindNvi
#' @keywords internal
#' @noRd
FindNvi <- \(data) {

  nvi <- lapply(seq_len(nrow(data)), \(i) {

    x <- data[i, ]
    nvi <- 0

    # Find NVI if result is part of another work
    if (!is.na(GoFish(x$part_of$url))) {
      book.nvi <- c2z::Cristin(
        id = basename(x$part_of$url),
        zotero.import = FALSE,
        nvi = TRUE,
        silent = TRUE
      )
      nvi <- GoFish(book.nvi$results$nvi, 0)
    }

    if (identical(nvi, 0)) {

      search <- grep("nvi", names(unlist(x)))
      if (length(search)) {
        nvis <- as.numeric(unlist(x)[search])
        nvi <- max(0, nvis[!is.na(nvis)])
      }
    }

    return (nvi)

  })

  return(unlist(nvi))

}

#' @title SdgPredictions
#' @keywords internal
#' @noRd
SdgPredictions <- \(data, sdg.host, sdg.batch, silent = FALSE) {

  # Visible bindings
  predictions <- abstractNote <- key <- title <- text <- translated_text <- NULL

  if (!any(nrow(data))) {
    return (NULL)
  }

  # Find abstracts
  abstracts <- data |>
    dplyr::filter(!is.na(abstractNote)) |>
    dplyr::transmute(
      key,
      version,
      text =  paste(title, abstractNote, sep = ". ")
    )  |>
    tidyr::drop_na()

  if (!any(nrow(abstracts))) {
    return (NULL)
  }

  SdgApi <- \(items) {

    # Define JSON
    data <- jsonlite::toJSON(
      items, dataframe = "rows", auto_unbox = TRUE
    )

    # Fetch SDG predictions
    httr.post <- Online(
      httr::RETRY(
        "POST",
        sdg.host,
        body = data,
        add_headers(.headers = c('Content-Type' = 'application/json')),
        quiet = TRUE
      ),
      silent = TRUE
    )

    if (httr.post$error) {
      return (NULL)
    }
    results <- JsonToTibble(httr.post$data)
    predictions <- results |>
      dplyr::select(order(as.numeric(gsub("\\D", "", colnames(results))))) |>
      dplyr::select(key, version, dplyr::everything(), -c(text, translated_text))

    return (predictions)

  }

  sdg.batches <- SplitData(abstracts, sdg.batch)

  # Start time for query
  query.start <- Sys.time()

  for (i in seq_along(sdg.batches)) {

    # Wrangle data
    predictions <- dplyr::bind_rows(predictions, SdgApi(sdg.batches[[i]]))

    # Estimate time of arrival
    log.eta <- LogCat(
      Eta(query.start, i, length(sdg.batches)),
      silent = silent,
      flush = TRUE,
      log = list(),
      append.log = FALSE
    )

  } # End for loop


  return (predictions)

}

#' @title SdgSummary
#' @keywords internal
#' @noRd
SdgSummary <- \(data, column = "llm_sdgs_final") {

  if (!any(nrow(data))) {
    return (NULL)
  }

  sdg.cols = paste0("sdg", 1:17)
  sdgs <- lapply(seq_len(nrow(data)), \(i) {
    tibble::tibble(
      key = data[i, ]$key,
      version = data[i, ]$version,
      !!!setNames(as.numeric(seq(17) %in% unlist(data[i, column])), sdg.cols)
    )
  }) |>
    dplyr::bind_rows()


  sum <- sdgs |>
    dplyr::summarise(
      dplyr::across(
        dplyr::starts_with("sdg") , ~ sum(.x, na.rm = TRUE)
      )
    ) |>
    unlist()

  return.list <- list(
    sdgs = sdgs,
    sum = sum
  )

  return (return.list)

}

#' @title SdgInfo
#' @keywords internal
#' @noRd
SdgInfo <- \(sdg.sum,
             range = NULL,
             lang = "no",
             sdg.path = NULL,
             archive.url = NULL,
             archive.append = NULL,
             sort = FALSE,
             delete = FALSE) {

  # Languages
  # Set language to en if not nb or nn
  lang <- if (!lang %in% c("nb", "nn", "no")) "en" else "no"

  SdgUrls <- \(lang = "no", sdgs) {

    # Find Norwegian urls
    if (lang == "no") {

      httr.get <- Online(
        httr::RETRY(
          "GET",
          "https://www.fn.no/om-fn/fns-baerekraftsmaal",
          query = list(lang = "nno-NO"),
          quiet = TRUE),
        silent = TRUE
      )

      sdg.urls <- httr.get$data |>
        rvest::read_html() |>
        rvest::html_nodes(".header_gols_content_list li a") |>
        rvest::html_attr('href') |>
        (\(x) paste0("https://www.fn.no", x)[sdgs])()

    } else {

      sdg.urls <- paste0("https://sdgs.un.org/goals/goal", sdgs)

    }

    return (sdg.urls)

  }

  # List of SDGs
  sdgs <- if (is.null(range)) 1:17 else range
  # Delete zero if delete is true
  if (delete) {
    sdgs <- sdgs[sdg.sum > 0]
    sdg.sum <- sdg.sum[sdg.sum > 0]
  }
  # Sort if sort is true
  if (sort) {
    sdgs <- sdgs[order(sdg.sum, decreasing = TRUE)]
    sdg.sum <- sdg.sum[order(sdg.sum, decreasing = TRUE)]
  }

  # Urls
  sdg.urls <- SdgUrls(lang, sdgs)

  # Initialize an empty list to store the HTML code for each SDG
  sdg.html <- lapply(seq_along(sdgs), \(i) {
    x <- sdgs[[i]]
    sdg.id <- sprintf("sdg%d", x)
    if (is.null(sdg.path)) sdg.path <- "/images/sdg"
    sdg.archive.url <- paste0(
      archive.url,
      paste0("?sdg=", x, archive.append, "#archive")
    )
    sdg.image <- file.path(sdg.path, sprintf("sdg%02d_%s.png", x, lang))
    sdg.publications <- Dict("publications", lang, sdg.sum[[i]])
    sdg.span <- sprintf("%s", sdg.sum[[i]])
    sdg.url <- sprintf("%s", sdg.urls[[i]])

    # Create the HTML code for the current SDG
    sdg.code <- sprintf(
      '<div id="%s" class="sdg">
        <img src="%s" class="image" alt="SDG %d">
        <div class="sdg-overlay">
          <a href="%s" class="sdg-publication-count"><span>%s</span> %s</a>
          <p><a href="%s" class="sdg-read-more">%s</a></p>
        </div>
      </div>',
      sdg.id,
      sdg.image,
      x,
      sdg.archive.url,
      sdg.span,
      sdg.publications,
      sdg.url,
      Dict("readmore", lang)
    )

    # Append the HTML code to the list
    Trim(sdg.code)

  })

  return( sdg.html[lengths(sdg.html) > 0])

}

#' @title CreateMonthlies
#' @keywords internal
#' @noRd
CreateMonthlies <- \(zotero,
                     unit.paths,
                     collections,
                     local.storage,
                     full.update,
                     lang,
                     style,
                     locale,
                     silent,
                     log = list()) {

  # Visible bindings
  new.zotero <- key <- missing.keys <- version.x <- version.y <- name <- id <-
    cristin.id <- year <- month <- items <- bibliography <- monthlies <-
    inn.cards <- new.keys <- where <- key <- abstractNote <- NULL

  # Function to enhance bibliography
  EnhanceBib <- \(items,
                  bibliography,
                  collections,
                  unit.paths,
                  silent) {

    CollectionNames <- \(unit.paths, collections, cristin.id) {
      # Filter out collections found in unit.paths
      x <- unit.paths |>
        dplyr::filter(key %in% collections) |>
        pull(name)
      # Create data frame if number of collections is >= levels in collecitons
      if (length(x) <= max(unit.paths$level)) {
        x <- data.frame(t(x))
        # The item is a multidepartemental publication
      } else {
        cristin.ids <- CristinId(cristin.id, unit = TRUE)
        x <- unit.paths |>
          # Find unit names of
          dplyr::filter(id %in% cristin.ids) |>
          dplyr::arrange(match(id, cristin.ids)) |>
          dplyr::pull(key) |>
          # Find unit paths of each contributor (e.g., Uni -> Faculty -> Dep)
          purrr::map(
            ~ data.frame(t(AncestorPath(unit.paths, .x, TRUE)))
          ) |>
          dplyr::bind_rows() |>
          # Replace duplicates with NA
          dplyr::mutate(
            dplyr::across(
              dplyr::everything(), ~ replace(.x, duplicated(.x), NA)
            )
          ) |>
          # Create a single row with combined units
          dplyr::summarise(
            dplyr::across(
              dplyr::everything(), ~ paste(.x[!is.na(.x)], collapse = " || ")
            )
          )
      }
      # Create column names
      names(x) <- paste0("collection.name", AddPad(seq_len(ncol(x))))

      # Set a tibble and return
      return(tibble::tibble(x))

    }

    bib <- bibliography |>
      dplyr::arrange(match(key, items$key)) |>
      dplyr::mutate(
        cristin.id = ZoteroId("Cristin", items$extra),
        collections = items$collections,
        collection.names = purrr::pmap(
          list(collections, cristin.id), ~ CollectionNames(unit.paths, .x, .y),
          .progress = if (!silent) "Finding collections" else FALSE
        ),
        year = purrr::map_int(collections, ~ {
          .env$collections |>
            dplyr::filter(
              key %in% .x, grepl("^\\d{4}$", name)
            ) |>
            dplyr::pull(name) |>
            (\(x) as.numeric(x)[[1]])()
        },
        .progress = if (!silent) "Finding year" else FALSE
        ),
        month = purrr::map_int(collections, ~ {
          .env$collections |>
            dplyr::filter(
              key %in% .x, grepl("^\\d{1,2}: [A-Za-z]+$", name)
            ) |>
            dplyr::pull(name) |>
            (\(x) as.numeric(gsub("\\D", "", x[[1]])))()
        },
        .progress = if (!silent) "Finding month" else FALSE
        ),
        year.month = paste0(year, "_" , AddPad(month, 2))
      )

    return (bib)

  }

  # Try to restore zotero items if local.storage is defined
  if (!is.null(local.storage)) {

    # Items
    items <- GoFish(
      readRDS(file.path(local.storage, "items.rds")),
      NULL
    )

    # Bibliography
    bibliography <- GoFish(
      readRDS(
        file.path(local.storage, paste0("bibliography_", lang, ".rds"))
      ),
      NULL
    )

    # Log
    log <-  LogCat(
      "Checking Zotero library for updates",
      silent = silent,
      log = log
    )

    # Find keys and versions from Zotero library
    check.items <- c2z::ZoteroGet(
      ListValue(zotero, "collection.key", unit.paths$key[[1]]),
      format = "versions",
      silent = FALSE,
      append.items = TRUE,
      result.type = "key",
      force = TRUE
    )$results

    # Remove items not found in Zotero
    if (any(nrow(items))) {
      items <- items |>
        filter(key %in% check.items$key)
    }
    if (any(nrow(bibliography))) {
      bibliography <- bibliography |>
        filter(key %in% check.items$key)
    }
    # Check if there are new/modified items
    new.keys <- check.items |>
      dplyr::anti_join(items, by = c("key", "version")) |>
      dplyr::pull(key) |>
      GoFish(type = NULL)

    # Check if there are keys in items that are not in bibliography
    if (any(nrow(items) > nrow(bibliography))) {
      missing.keys <- items |>
        dplyr::anti_join(bibliography, by = c("key", "version")) |>
        dplyr::pull(key) |>
        GoFish(type = NULL)
      new.keys <- unique(c(new.keys, missing.keys))
    }

    # Set new keys as all keys in zotero if no items
    if (!any(nrow(items)) & any(nrow(check.items))) {
      new.keys <- check.items$key
    }
    # Fetch bibliography for new items
    if (any(length(new.keys))) {

      # Save to storage
      saveRDS(
        new.keys,
        file.path(local.storage, "updated_keys.rds")
      )

      # Log
      log <-  LogCat(
        "Creating bibliographies",
        silent = silent,
        log = log
      )

      # New items
      new.zotero <- c2z::ZoteroGet(
        zotero,
        item.keys = new.keys,
        item.type = "-attachment || note",
        library.type = "data,bib,citation",
        style = style,
        locale = locale,
        silent = silent,
        force = TRUE,
        use.collection = FALSE
      )

      # Update items
      items <- UpdateInsert(items, new.zotero$results)

      # Update bibliography
      bibliography <- UpdateInsert(
        bibliography, new.zotero$bibliography
      )

      # Arrange bibliography according to items
      if (any(nrow(bibliography)) & any(nrow(items))) {
        bibliography <- bibliography |>
          dplyr:: arrange(match(key, items$key))
      }

    }

  }

  # Find all items if full upgrade or zotero items are empty
  if (full.update | is.null(items) | is.null(bibliography)) {

    zotero <- ZoteroLibrary(
      ListValue(zotero, "collection.key", unit.paths$key[[1]]),
      item.type = "-attachment || note",
      library.type = "data,bib,citation",
      style = style,
      locale = locale,
      silent = silent,
      force = TRUE
    )
    items <- zotero$items
    bibliography <- zotero$bibliography

  }

  # Return if no collections
  if (!any(nrow(items)) |
      !any(nrow(bibliography))) {
    return(zotero)
  }

  # Save zotero.items and bibliography
  if (!is.null(local.storage)) {

    # Log
    log <-  LogCat(
      "Saving items and bibliographies to database",
      silent = silent,
      log = log
    )

    # Save to storage
    saveRDS(
      items,
      file.path(local.storage, "items.rds")
    )
    saveRDS(
      bibliography,
      file.path(local.storage, paste0("bibliography_", lang, ".rds"))
    )

  }

  # Log monthly bibliographies for Cristin
  log <-  LogCat(
    "Creating monthly bibliographies",
    silent = silent,
    log = log
  )

  # Try to restore monthlies to local storage if defined
  if (!is.null(local.storage)) {

    monthlies <- GoFish(
      readRDS(
        file.path(local.storage, paste0("monthlies_", lang, ".rds"))
      ),
      NULL
    )

    # Update if new items
    if (any(nrow(new.zotero$results)) & any(nrow(monthlies))) {

      # Create monthlies for new items
      new.monthlies <- EnhanceBib(
        new.zotero$results,
        new.zotero$bibliography,
        collections,
        unit.paths,
        silent
      )

      # Update or insert items
      monthlies <- UpdateInsert(monthlies, new.monthlies)

    }

  }

  # Run full update if full.update is TRUE or monthlies are missing
  if (full.update | !any(nrow(monthlies))) {

    monthlies <- EnhanceBib(
      items,
      bibliography,
      collections,
      unit.paths,
      silent
    )

  }

  # Save data to local storage if defined
  if (!is.null(local.storage)) {

    if (any(nrow(monthlies))) {
      # Log
      log <-  LogCat(
        "Saving monthly bibliographies to database",
        silent = silent,
        log = log
      )
      saveRDS(
        monthlies,
        file.path(local.storage, paste0("monthlies_", lang, ".rds"))
      )
    }

  }

  # Define new keys
  new.keys <- if (!is.null(local.storage)) new.keys else items$key

  # Create return.list
  return.list <- list(
    items = items,
    monthlies = monthlies,
    updated.keys = new.keys,
    log = log
  )

  return (return.list)

}


#' @title CreateExtras
#' @keywords internal
#' @noRd
CreateExtras <- \(monthlies,
                  sdg.host,
                  sdg.batch,
                  get.unpaywall,
                  get.ezproxy,
                  ezproxy.host,
                  local.storage,
                  full.update,
                  lang,
                  silent,
                  log = list()) {

  # Visible bindings
  sdg <- new.items <- missing.items <- where <- key <- title <- itemType <-
    abstractNote <- extra <- cristin.id <- prefix <- ISBN <- DOI <-
    doi <- extras <- llm_sdgs_final <- NULL

  # Function to enhance bibliography
  GetExtras <- \(items,
                 sdgs,
                 get.unpaywall,
                 get.ezproxy,
                 ezproxy.host,
                 silent) {

    extras <- items |>
      dplyr::transmute(
        key,
        version,
        title,
        type = itemType,
        abstract = abstractNote,
        cristin.id = ZoteroId("Cristin", extra),
        cristin.url = paste0(
          "https://app.cristin.no/results/show.jsf?id=",
          cristin.id
        ),
        zotero.url = sprintf(
          "http://zotero.org/%s/items/%s", prefix, key
        ),
        cristin.ids = purrr::map(
          cristin.id, ~ CristinId(.x),
          .progress = if (!silent) "Finding Cristin IDs" else FALSE
        ),
        isbn = GoFish(ISBN),
        doi =  purrr::pmap_chr(
          list(GoFish(DOI), GoFish(extra)), ~
            GetDoi(...),
          .progress = if (!silent) "Finding DOI" else FALSE
        ),
        extra
      )

    # Add SDG if sdg.model is defined
    if (any(nrow(sdgs))) {

      sdgs <- sdgs |>
        dplyr::select(key, sdg = llm_sdgs_final)
      extras <- dplyr::left_join(extras, sdgs, by = "key")

    }

    # Add unpaywall if get.unpaywall is TRUE
    if (get.unpaywall) {

      extras <- extras |>
        dplyr::mutate(
          unpaywall =  purrr::map_chr(
            doi, ~ Unpaywall(.x),
            .progress = if (!silent) "Searching Unpaywall" else FALSE
          )
        )
    }

    # Add ezproxy if get.ezproxy is TRUE
    if (get.ezproxy) {
      extras <- extras |>
        dplyr::mutate(
          ezproxy =
            dplyr::case_when(
              !is.na(GoFish(unpaywall)) ~ NA_character_,
              TRUE ~ purrr::map_chr(
                doi, ~ Ezproxy(.x, host = ezproxy.host),
                .progress = if (!silent) "Defining EZproxy" else FALSE
              )
            )
        )
    }

    return (extras)

  }

  # Try to restore sdgs to local storage if defined
  if (!is.null(local.storage)) {

    # Log SDG predictions
    log <-  LogCat(
      "Creating SDG predictions (this may take awhile)",
      silent = silent,
      log = log
    )

    # Check if any sdg predictions exist
    sdg <- GoFish(
      readRDS(file.path(local.storage, "sdg_predictions.rds")),
      NULL
    )

    # Check if any missing items
    missing.items <- monthlies$items |>
      dplyr::anti_join(sdg, by = c("key", "version")) |>
      GoFish(type = NULL)

    # Update sdg if missing items
    if (any(nrow(missing.items)) & any(nrow(sdg))) {
      new.sdg <- SdgPredictions(missing.items, sdg.host, sdg.batch, silent)

      # Update or insert sdg
      if (any(nrow(new.sdg))) {
        sdg <- UpdateInsert(sdg, new.sdg)
      }
    }
  }

  # Run full sdg predictions if full update or sdg are empty
  if ((full.update | !any(nrow(sdg))) & !is.null(sdg.host)) {
    sdg <- SdgPredictions(monthlies$items, sdg.host, sdg.batch, silent)
  }

  # Save data to local storage if defined
  if (!is.null(local.storage) & !is.null(sdg.host)) {

    if (any(nrow(sdg))) {
      # Log
      log <-  LogCat(
        "Saving SDG predictions",
        silent = silent,
        log = log
      )
      saveRDS(
        sdg,
        file.path(local.storage, "sdg_predictions.rds")
      )
    }
  }

  # Try to restore monthlies to local storage if defined
  if (!is.null(local.storage)) {

    # Log Extras
    log <-  LogCat(
      "Creating extras",
      silent = silent,
      log = log
    )

    extras <- GoFish(
      readRDS(file.path(local.storage, "monthlies_extras.rds")),
      NULL
    )

    # Check if any missing items
    missing.items <- monthlies$items |>
      dplyr::anti_join(extras, by = c("key", "version")) |>
      GoFish(type = NULL)

    # Update if new items
    if (any(nrow(missing.items)) & any(nrow(extras))) {

      # Create monthlies for new items
      new.extras <- GetExtras(
        missing.items,
        sdg,
        get.unpaywall,
        get.ezproxy,
        ezproxy.host,
        silent
      )

      # Update or insert items
      extras <- UpdateInsert(extras, new.extras)

    }

  }

  # Run full update if full.update is TRUE or monthlies are missing
  if (full.update | !any(nrow(extras))) {

    extras <- GetExtras(
      monthlies$items,
      sdg,
      get.unpaywall,
      get.ezproxy,
      ezproxy.host,
      silent
    )

  }

  # Save data to local storage if defined
  if (!is.null(local.storage)) {

    if (any(nrow(extras))) {
      # Log
      log <-  LogCat(
        "Saving extras to database",
        silent = silent,
        log = log
      )
      saveRDS(
        extras,
        file.path(local.storage, "monthlies_extras.rds")
      )
    }

  }

  # Create return.list
  return.list <- list(
    extras = extras,
    sdg = sdg,
    log = log
  )

  return (return.list)

}

#' @title ExamineItems
#' @keywords internal
#' @noRd
ExamineItems <- \(items,
                  collections,
                  silent = FALSE,
                  log = list(),
                  n.paths) {

  # Visible bindings
  multidepartmental <- duplicates <- extra <- title <- identifier <-
    computer.title <- itemType <- NULL

  items <- AddMissing(
    items, c("ISBN", "DOI", "extra"), na.type = NA_character_
  ) |>
    # Group by extra and identify identical extra fields
    dplyr::group_by(extra) |>
    dplyr::mutate(
      # Merge collections for identical results
      collections = dplyr::case_when(
        dplyr::n() > 1 ~ list(unique(unlist(collections))),
        TRUE ~ collections
      ),
      # Identify number of identifical fields
      # These are most likely multidepartmental
      multidepartmental = dplyr::n() - 1
    )  |>
    # Remove duplicates
    dplyr::distinct(extra, .keep_all = TRUE) |>
    dplyr::ungroup() |>
    # Create an identifier field based on DOI and ISBN
    dplyr::mutate(
      identifier = dplyr::case_when(
        !is.na(ISBN) ~ Trim(gsub('[^[:alnum:] ]', "", ISBN)),
        !is.na(DOI) ~ DOI,
        TRUE ~ NA
      ),
      # Create short.title to check for duplicates
      computer.title = ComputerFriendly(title)
    ) |>
    # Group by identifiers and find duplicates
    dplyr::group_by(identifier, computer.title, itemType) |>
    dplyr::mutate(
      # Add to extra field the duplicated id
      extra = dplyr::case_when(
        dplyr::n() > 1 & !is.na(identifier) ~ AddAppend(
          paste("Duplicate ID:", identifier), extra, "\n"
        ),
        TRUE ~ extra
      ),
      # Find number of duplicated datapoints
      duplicates = dplyr::case_when(
        dplyr::n() > 1 & !is.na(identifier) ~ 1,
        TRUE ~ 0
      )
    ) |>
    dplyr::ungroup()

  # Define multidepartmental (collections greater than max paths)
  multidepartmental <- lengths(items$collections) > n.paths

  # Log number multidepartmental items
  if (sum(multidepartmental)) {
    log <-  LogCat(
      sprintf(
        "Merged %s multidepartmental %s (see `$multidepartmental`)",
        sum(multidepartmental),
        Numerus(
          sum(multidepartmental), "publication", prefix = FALSE
        )
      ),
      silent = silent,
      log = log
    )

    # Find Cristin ID of multidepartmental items
    multidepartmental <- items[multidepartmental, ] |>
      (\(x) ZoteroId("Cristin", x$extra))()

  }

  # Log number of duplicated items
  if (sum(items$duplicates)) {

    log <-  LogCat(
      sprintf(
        "Found %s possible duplicated %s (see `$duplicates`)" ,
        sum(items$duplicates),
        Numerus(
          sum(items$duplicates), "publication", prefix = FALSE
        )
      ),
      silent = silent,
      log = log
    )

    # Find Cristin ID of duplicated items
    duplicates <- items |>
      dplyr::filter(duplicates > 0) |>
      dplyr::pull(extra) |>
      (\(x) ZoteroId("Cristin", x))()

  }

  # Remove added columns
  items <- items |>
    dplyr::select(-c(multidepartmental, duplicates, identifier, computer.title))

  # Create return list
  return.list <- list(
    items = items,
    multidepartmental = multidepartmental,
    duplicates = duplicates,
    log = log
  )

  return (return.list)

}

#' @title Csl
#' @keywords internal
#' @noRd
Csl <- \(csl.type = "apa-single-spaced",
         csl.name = "style",
         save.data = FALSE,
         save.path = tempdir(),
         silent = FALSE) {

  # Fetch csl
  httr.get <- Online(
    httr::RETRY(
      "GET",
      sprintf(
        "%s/citation-style-language/styles/master/%s.csl",
        "https://raw.githubusercontent.com",
        csl.type
      ),
      quiet = TRUE),
    silent = TRUE
  )
  log <- httr.get$log

  # Log and return error if status code != 200
  if (httr.get$error) {
    return (NULL)
  }

  # Log CSL format
  log <-  LogCat(
    sprintf("Found CLS in `%s` format", csl.type),
    silent = silent,
    log = log
  )

  # Format csl
  csl.style <- rawToChar(httr.get$data$content)

  # Save data if save.data is TRUE
  if (save.data) {
    csl.style <- SaveData(csl.style, csl.name, "csl", save.path)

    # Add to log
    log <- LogCat(
      sprintf("CSL saved in %s", csl.style),
      silent = silent,
      log = log
    )
  }

  return (list(style = csl.style, log = log))

}

#' @title ListValue
#' @keywords internal
#' @noRd
ListValue <- \(list, name, value) {

  list[[name]] <- value

  return (list)

}

#' @title CreateHeader
#' @keywords internal
#' @noRd
CreateHeader <- \(title,
                  level,
                  md.headers = FALSE,
                  id = "",
                  class = "") {

  if (any(is.na(GoFish(title)))) {
    return (character(0))
  }

  if (md.headers) {
    header <- sprintf(
      "<div id=\"%s\" class=\"%s\">\n\n%s %s\n\n</div>",
      id,
      class,
      strrep("#", level),
      title
    )
  } else {
    header <- sprintf(
      "<h%1$s id=\"%3$s\" class=\"%4$s\">%2$s</h%1$s>",
      level,
      title,
      id,
      class
    )
  }

  return (header)
}

#' @title CristinId
#' @keywords internal
#' @noRd
CristinId <- \(id, error = NULL, unit = FALSE) {

  # Visible bindings
  cristin_person_id <- affiliations <- unit_cristin_unit_id <-  NULL

  # Find contributors to Cristin reference
  httr.get <- Online(
    httr::RETRY(
      "GET",
      sprintf(
        "https://api.cristin.no/v2/results/%s/contributors",
        id),
      quiet = TRUE
    ),
    silent = TRUE,
    message = "Cristin contributors"
  )

  # Return Null not found
  if (httr.get$error) {
    return (error)
  }

  # Find id from Cristin profile
  if (unit) {
    cristin <- httr.get$data |>
      JsonToTibble() |>
      tidyr::unnest(affiliations) |>
      tidyr::unnest(unit, names_sep = "_") |>
      dplyr::pull(unit_cristin_unit_id)
  } else {
    # Find names from Cristin profile
    cristin <- httr.get$data |>
      JsonToTibble() |>
      dplyr::pull(cristin_person_id)
  }

  return (cristin)

}

#' @title CristinName
#' @keywords internal
#' @noRd
CristinName <- \(full.name, institution = 209) {

  # Visible bindings
  id <- NA

  if (any(is.na(GoFish(full.name)))) {
    return (NA)
  }

  # Function to calculate Levenshtein distance and find the closest match
  ClosestMatch <- function(query, data, cut.off = 2) {

    names <- paste(data$first_name, data$surname)
    distances <- stringdist::stringdistmatrix(query, names, method = "lv")
    closest.index <- which.min(distances)
    min.distance <- min(distances)

    if (min.distance <= cut.off) {
      id <- GoFish(data[closest.index, ]$cristin_person_id[[1]])
      return(id)
    } else {
      return(NA)
    }
  }

  # Define names
  no.middlename <- strsplit(full.name, "\\s+")[[1]] |>
    (\(x) paste(x[c(1, length(x))], collapse = " "))()
  last.name <- tail(strsplit(no.middlename, "\\s+")[[1]], 1)
  first.name <- head(strsplit(no.middlename, "\\s+")[[1]], 1)

  # Create a list of the names
  names <- list(
    full.name = full.name,
    no.middlename = no.middlename,
    last.name = last.name,
    first.name = first.name
  )

  # Loop through the names
  for (i in seq_len(4)) {

    # Query user url
    httr.get <- Online(
      httr::RETRY(
        "GET",
        "https://api.cristin.no/v2/persons/",
        query = list(
          name = names[[i]],
          institution = institution
        ),
        quiet = TRUE
      ),
      silent = TRUE
    )


    # Return NA if not found
    if (httr.get$error) {
      next
    }

    # Get Cristin data
    data <- JsonToTibble(httr.get$data)

    # Skip if no rows are found
    if (!any(nrow(data))) {
      next
    }

    # Check if a match is found using full name
    id <- ClosestMatch(names$full.name, data)
    # Break loop if id is found
    if (any(!is.na(id))) break
    # Check if a match is found using first and last name
    id <- ClosestMatch(names$no.middlename, data)
    # Break loop if id is found
    if (any(!is.na(id))) break

  }

  return (id)

}

#' @title GetCards
#' @keywords internal
#' @noRd
GetCards <- \(local.storage, full.update, lang, silent) {


  # Try to restore inn.cards if local.storage is defined
  if (!is.null(local.storage)) {
    inn.cards <- GoFish(
      readRDS(file.path(local.storage, paste0("inn_cards_", lang, ".rds"))),
      NULL
    )
  }

  if (full.update | is.null(inn.cards)) {

    # Log
    log <-  LogCat(
      "Updating INN database, please wait...",
      silent = silent,
      log = log
    )

    # Update card
    inn.cards <- InnUsers(silent = silent, lang = lang)

    # Save inn cards to local storage if defined
    if (!is.null(local.storage)) {

      # Log
      log <-  LogCat(
        "Saving INN database",
        silent = silent,
        log = log
      )
      saveRDS(
        inn.cards,
        file.path(local.storage, paste0("inn_cards_", lang, ".rds"))
      )
    }
  }

  return (inn.cards)

}

#' @title InnCards
#' @keywords internal
#' @noRd
InnCards <- \(url, error = NA) {

  # Query user url
  httr.get <- Online(
    httr::RETRY(
      "GET",
      url,
      quiet = TRUE,
      httr::accept("text/html")
    ),
    silent = TRUE,
    message = "Cristin contributors"
  )

  # Return Null not found
  if (httr.get$error) {
    return (error)
  }

  # Find card
  card <- httr.get$data |>
    rvest::read_html() |>
    rvest::html_nodes(".vrtx-hinn-person-card") |>
    toString() |>
    Trim()

  # Add user url to adress
  card <- sub(
    "#vrtx-hinn-addresses",
    paste0(url, "#vrtx-hinn-addresses"),
    card
  )

  return (card)

}

#' @title InnUsers
#' @keywords internal
#' @noRd
InnUsers <- \(i = 1,
              lang = "no",
              get.cristin = TRUE,
              get.cards = TRUE,
              silent = FALSE,
              log = list()) {

  UpdateCard <- \(x, y) {

    # Add photo class if missing
    if (!grepl("class=\"photo\"", x)) {
      missing.person <- paste0(
        "vrtx-hinn-person-card\">\n
        <div class=\"photo\">
          <i class=\"lar la-user-circle missing-person\"></i>
        </div>"
      )
      x <- gsub("vrtx-hinn-person-card\">", missing.person, x)
    }

    card <- htmltools::div(htmltools::HTML(x), class = "personas")
    cristin.url <- htmltools::a(
      "Cristin",
      href = paste0("https://app.cristin.no/persons/show.jsf?id=", y),
      alt = "Cristin URL",
      class = "personas-cristin"
    )
    htmltools::tagAppendChild(card, cristin.url) |>
      as.character() |>
      Trim()
  }

  # Visible bindings
  first.name <- last.name <- full.name <- card <- cristin.id <- NULL

  # Languages
  # Set language to en if not nb or nn
  lang <- if (!lang %in% c("nb", "nn", "no")) "en" else "no"

  # Definitions
  card.url <- Dict("inn.url", lang)

  GetUsers <- \(i) {
    httr.get <- Online(
      httr::RETRY(
        "GET",
        card.url,
        query = list(page = i),
        quiet = TRUE,
        httr::accept("text/html")
      ),
      silent = TRUE,
      message = "Cristin contributor"
    )

    # Return Null not found
    if (httr.get$error) {
      return (NULL)
    }

    # Find urls
    urls <- httr.get$data |>
      rvest::read_html() |>
      rvest::html_nodes(".vrtx-person-listing li span:first-child a") |>
      rvest::html_attr('href')

    # Return Null not found
    if (!length(urls)) {
      return (NULL)
    }

    names <- httr.get$data |>
      rvest::read_html() |>
      rvest::html_nodes(".vrtx-person-listing li span:first-child a") |>
      rvest::html_text() |>
      Trim() |>
      (\(x) {
        dplyr::bind_rows(
          lapply(strsplit(x, ", "), \(x) {
            setNames(x, c("last.name", "first.name"))
          })
        )
      })() |>
      dplyr::mutate(
        full.name = paste(first.name, last.name)
      )

    users <- tibble::tibble(names, url = urls)

    remaining.pages <- httr.get$data |>
      rvest::read_html() |>
      rvest::html_nodes(".vrtx-paging-wrapper a") |>
      rvest::html_text() |>
      (\(x) {
        x <- suppressWarnings(as.numeric(x))
        GoFish(x[!is.na(x)][-1], NULL)
      })()

    list(users = users, remaining.pages = remaining.pages)
  }

  log <-  LogCat(
    "Finding names at INN.no",
    silent = silent,
    log = log
  )

  # Get first page of users
  users <- GetUsers(i)

  # Check if remaining pages
  if (!is.null(users$remaining.pages)) {

    # Add remaining users
    users <- list(
      users = dplyr::bind_rows(
        users$users,
        users$remaining.pages |>
          purrr::map(\(i) GetUsers(i)$users, .progress = !silent),
      )
    )
  }

  # Filter users
  users <- users$users |>
    tidyr::drop_na()

  # Add cristin id if get.cristin = TRUE
  if (get.cristin) {

    log <-  LogCat(
      "Finding Cristin id of users",
      silent = silent,
      log = log
    )

    users <- users |>
      dplyr::mutate(
        cristin.id = purrr::map_chr(
          full.name, ~ CristinName(.x), .progress = !silent
        )
      ) |>
      dplyr::filter(!is.na(cristin.id)) |>
      dplyr::distinct(cristin.id, .keep_all = TRUE)

  }

  # Add user cards at INN if get.cards is TRUE
  if (get.cristin & get.cards) {

    log <-  LogCat(
      "Finding business cards at INN.no",
      silent = silent,
      log = log
    )

    users <- users |>
      dplyr::mutate(
        card = purrr::map_chr(
          url, ~ InnCards(.x),
          .progress = !silent
        ),
        card = purrr::pmap_chr(
          list(card, cristin.id), ~
            UpdateCard(.x, .y),
          .progress = TRUE
        )
      ) |>
      dplyr::filter(!is.na(card))

  }

  return (users)

}

#' @title Interleave
#' @keywords internal
#' @noRd
Interleave <- function(a, b, rm.na = TRUE) {
  interleave <- order(c(seq_along(a), seq_along(b)))
  ab <- c(a, b)[interleave]
  if (rm.na) ab <- ab[!is.na(ab)]
  return(ab)
}

#' @title AncestorPath
#' @keywords internal
#' @noRd
AncestorPath <- \(data, path.key, name = FALSE) {

  # Return NULL if not in data
  if (!path.key %in% data$key) {
    return (NULL)
  }

  # Find parent key
  parent = data[data$key == path.key, ]$parentCollection
  # Use parent name if name is TRUE else use key
  id <- if(name) data[data$key == path.key, ]$name else path.key

  # Return id if parentCollection is FALSE
  if(parent == "FALSE")  {
    path <- id
    # Else continue searching for ancestor
  } else {
    path <- c(AncestorPath(data, parent, name), id)
  }

  return (path)

}

#' @title DescendingPath
#' @keywords internal
#' @noRd
DescendingPath <- \(data, path.key, name = FALSE) {

  # Return NULL if not in data
  if (!path.key %in% data$key) {
    return (NULL)
  }

  # Function to travel trough the lineage of the collections
  Descending <- \(data, path.key, name) {

    # Find parent key
    child <- data[data$parentCollection == path.key, ]$key
    # Use child name if name is TRUE else use key
    id <- if(name) {
      data[data$parentCollection == path.key, ]$name
    } else {
      child
    }

    # Return id if child key not defined in parentCollection
    path <- unlist(lapply(seq_along(child), \(i) {
      if (!child[i] %in% data$parentCollection) {
        id[i]
      } else {
        c(id[i], Descending(data, child[i], name))
      }
    }))

    return (path)

  }

  # Define ancestor
  ancestor <- if(name) data[data$key == path.key, ]$name else path.key

  return (c(ancestor, Descending(data, path.key, name)))

}

#' @title FindPath
#' @keywords internal
#' @noRd
FindPath <- \(data, path.name, case.insensitive = TRUE) {

  # Visible bindings
  path <- item <- name <- parentCollection <- NULL

  for (i in seq_along(path.name)) {

    # Find parent, if k=1 there are no parents, strangely enough
    parent <- if (i == 1) "FALSE" else parent

    # Search for item using name and parentCollection if data is defined
    if (!is.null(data)) {
      item <- data |>
        dplyr::filter(grepl(
          path.name[i], name, ignore.case = case.insensitive) &
            parentCollection == parent
        ) |>
        dplyr::arrange(version) |>
        dplyr::slice(1)
    }

    # Create item if not found
    if (!any(nrow(item))) {
      item <- tibble::tibble(
        key = ZoteroKey(),
        version = 0,
        name = path.name[i],
        parentCollection = parent
      )

      # Add to data
      data <- dplyr::bind_rows(data, item)
    }

    # Define parrent
    parent <- item$key
    # Add key to path
    path <- c(path, item$key)

  }

  return (list(data = data, path = path))

}

#' @title Month
#' @keywords internal
#' @noRd
Month <- \(i = NULL,
           lang = NULL,
           abbreviation = FALSE) {

  # Return Null
  if (!GoFish(as.numeric(i) %in% seq(12), FALSE)) {
    return (NULL)
  } else {
    i <- as.numeric(i)
  }

  # Define months by norwegian if lang = no
  if (any(lang %in% c("no", "nn", "nb"))) {

    # Month names in Norwegian
    month.name <- c(
      "Januar",
      "Februar",
      "Mars",
      "April",
      "Mai",
      "Juni",
      "Juli",
      "August",
      "September",
      "Oktober",
      "November",
      "Desember"
    )

    # Abbreviated month names in Norwegian
    month.abb <- c(
      "Jan",
      "Feb",
      "Mar",
      "Apr",
      "Mai",
      "Jun",
      "Jul",
      "Aug",
      "Sep",
      "Okt",
      "Nov",
      "Des"
    )

  }

  # Set month as either month or abbreviated monther by integer
  month <- if (abbreviation) month.abb[i] else month.name[i]

  return (month)

}

#' @title ChangeDate
#' @keywords internal
#' @noRd
ChangeDate <- \(date, i, type = "months") {

  # Return NULL if not a date
  if (is.na(GoFish(as.Date(date)))) {
    return (NULL)
  }

  # sequence date by +- date (e.g., months) subtract tail
  date <- seq(date, by = paste(i, type), length = 2)[2]

  return (date)
}

#' @title FloorDate
#' @keywords internal
#' @noRd
FloorDate <- \(date) {

  # Return null if no dash in date
  if (!grepl("-", date)) {
    return (NULL)
  }

  # Convert date to character and split by dash
  date <- strsplit(as.character(date), "-")[[1]]
  # Remove days if any
  if (any(length(date) == 3)) date <- head(date, -1)

  # Paste and add day 1
  date <- date |>
    paste(collapse = "-") |>
    paste0("-01") |>
    as.Date() |>
    GoFish(NULL) # Set as null if date is erronous


  return (date)

}

#' @title CeilingDate
#' @keywords internal
#' @noRd
CeilingDate <-  \(date) {

  # Return NULL if not a date
  if (is.na(GoFish(as.Date(date)))) {
    return (NULL)
  }

  # Floor date, add month, subtract one day
  date <- FloorDate(date) |>
    ChangeDate(1, "months") |>
    ChangeDate(-1, "days")

  return (date)

}

#' @title ZoteroId
#' @keywords internal
#' @noRd
ZoteroId <- \(id.type,
              data,
              sep = "\n",
              return.type = NA) {

  # Define search pattern
  search <- sprintf(".*%s: (.*)", id.type)
  id <- unlist(lapply(data, \(x) {
    # Split vector by sep (e.g., linebreak)
    x <- unlist(strsplit(x, sep))
    # Extract by id.type for elements containing id.type
    x <- gsub(search, "\\1", x[grepl(paste0(id.type,":"), x)])
    # Set missing vector elements as return.type (e.g., NA)
    if (!length(x)) x <- return.type

    return (x)
  }))

  return (id)

}

#' @title Online
#' @keywords internal
#' @noRd
Online <- \(query,
            message = NULL,
            reference = NULL,
            silent = FALSE,
            log = list()) {

  # Visible bindings
  data <- NULL
  error <- TRUE

  # Check query/url and set 404 error upon error
  query <- GoFish(query, stats::setNames(list(404), "status_code"))

  # Set data as query upon success else NULL
  if (query$status_code %in% 200:299) {
    data <- query
    error <- FALSE
  }

  # Some status codes
  status.codes <- c(
    "Invalid type/field (unparseable JSON)" = 400,
    "The target library is locked" = 409,
    "Precondition Failed (e.g.,
    the provided Zotero-Write-Token has already been submitted)" = 412,
    "Request Entity Too Large" = 413,
    "Forbidden (check API key)" = 403,
    "Resource not found" = 404,
    "Everything is awesome" = 200,
    "Doom of the Noldor" = 204
  )

  # Define message
  if (is.null(message)) {
    message <- names(status.codes[status.codes %in% query$status_code])
    # Set unknown error if error.code is undefined
    if (!length(message)) message <- "No message. Sorry."
  } else {
    append.message <- if (error) "not OK" else "OK"
    message <- sprintf("%s is %s", message, append.message)
  }

  # Create feedback
  feedback <- sprintf("Status %s: %s.", query$status_code, message)

  # Add reference if any
  if (!is.null(reference)) {
    feedback <- sprintf(
      "%s (`%s`: %s)",
      feedback,
      as.character(sys.call(-1)[[1]]),
      reference
    )
  }

  # Clean feedback
  feedback <- Trim(gsub("\r?\n|\r", " ", feedback))

  # Log message
  log <-  LogCat(
    feedback,
    silent = silent,
    log = log
  )

  return (
    list(
      error = error,
      code = query$status_code,
      data = data,
      log = log
    )
  )

}


#' @title FixCreators
#' @keywords internal
#' @noRd
FixCreators <- \(data = NULL) {

  if (all(is.na(GoFish(data)))) {
    return (NULL)
  }

  data <- AddMissing(
    data, c("firstName", "lastName", "name"), na.type = ""
  ) |>
    dplyr::mutate_if(is.character, list(~dplyr::na_if(., ""))) |>
    dplyr::mutate(
      lastName = dplyr::case_when(
        !is.na(name) ~ NA_character_,
        TRUE ~ lastName
      ),
      firstName = dplyr::case_when(
        !is.na(name) ~ NA_character_,
        TRUE ~ firstName
      ),
      name = dplyr::case_when(
        is.na(firstName) & !is.na(lastName) ~ lastName,
        !is.na(lastName) & is.na(lastName) ~ firstName,
        is.na(firstName) & is.na(lastName) ~ name,
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select(dplyr::where(~sum(!is.na(.x)) > 0)) |>
    dplyr::filter(!dplyr::if_all(dplyr::everything(), is.na))

  return (data)

}

#' @title ZoteroCreator
#' @keywords internal
#' @noRd
ZoteroCreator <- \(data = NULL) {

  # Visible bindings
  creators <- NULL

  # Run if not empty
  if (all(is.na(GoFish(data)))) {
    return (NULL)
  }

  # Check that first element of data is a list
  if (!is.list(data[[1]])) data <- list(data)

  # Define creator types
  types <- c(
    author = "author",
    editor = "editor",
    translator = "translator",
    aut = "author",
    edt = "editor",
    red = "editor",
    trl = "translator",
    editorialboard = "editor",
    programmer = "programmer",
    curator = "author",
    serieseditor = "seriesEditor"
  )

  # Create zotero-type matrix
  creators <- dplyr::bind_rows(
    lapply(data, \(x) {

      # Remove commas and dots from names
      name <- Trim(gsub("\\.|\\,", " ", x$name))

      # Set as Cher if only one name is given
      if (length(name) == 1) {
        name <- c(name = name)
      } else {
        name <- c(lastName = name[1], firstName = name[2])
      }

      # Define creator type according to match in creator types
      type <- as.character(types[names(types) %in% tolower(x$type)])
      # Set as contributor if not found
      if (!length(type)) type <- "contributor"

      # Combine creatorType and names
      lapply(type, \(type) c(creatorType = type, name))

    })
  )

  # Check if names are all capital letters
  creators <- creators |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("lastName", "firstName")), ~ {
        dplyr::case_when(
          stringr::str_detect(.x, "^[[:upper:][:space:]]+$") ~
            stringr::str_to_title(.x),
          TRUE ~ .x
        )
      })
    )

  return (creators)

}

#' @title ZoteroToJson
#' @keywords internal
#' @noRd
ZoteroToJson <- \(data = NULL) {

  # Run if not empty
  if (all(is.na(GoFish(data)))) {
    return (NULL)
  }

  # Convert data to JSON
  data <- jsonlite::toJSON(data)
  # Convert character FALSE to logical false
  data <- gsub("\"FALSE\"", "false", data)
  # Remove empty elements
  data <- gsub(",\"\\w+?\":\\{\\}", "", data)

  return (data)

}

#' @title ZoteroFormat
#' @keywords internal
#' @noRd
ZoteroFormat <- \(data = NULL,
                  format = NULL,
                  prefix = NULL) {

  # Run if not empty
  if (all(is.na(GoFish(data)))) {
    return (NULL)
  }

  # Return as tibble if format is keys/versions
  if (any(format %in% c("keys", "versions"))) {

    # If string of characters (e.g., keys only)
    if (format == "keys") {

      data <- strsplit(data, "\n") |>
        unlist() |>
        (\(x) tibble::tibble(key = x))()

      # Else if keys and versions
    } else {

      data <- jsonlite::fromJSON(data) |>
        (\(x) tibble::tibble(key = names(x), version = unlist(x)))()

    }

    return (data)

  }

  # Visible bindings
  creators <- NULL

  multiline.items <- c("tags",
                       "extra",
                       "abstractNote",
                       "note",
                       "creators",
                       "relations")
  double.items <- c("version", "mtime")
  list.items <- c("collections", "relations", "tags")

  # Check if metadata and not in a data frame
  if (!is.data.frame(data) &
      (any(format == "json", is.null(format)))) {

    # Check that first element of data is a list
    if (is.list(data[[1]])) data <-  unlist(data, recursive = FALSE)

    # Check all element in the meta list
    data.list <- lapply(seq_along(data), \(i) {

      # Define data
      x <- data[[i]]
      names <- names(data[i])

      # Make certain fields not in multiline are strings
      if (!names %in% multiline.items) x <- ToString(x)

      # Add to list if element is a data frame
      ## Make certain list.items is in a list
      if (is.data.frame(x) | names %in% list.items) {
        x <- if (all(is.na(GoFish(as.character(unlist(x)))))) NA else list(x)

        # Make certain double.items are double
      } else if (names %in% double.items) {
        x <- as.double(x)
        # Else make certain remaining items are character
      } else {
        x <- as.character(x)
      }

      return (x)

    })
    # Name elements
    names(data.list) <- names(data)
    # Keep number of columns fixed for simple conversion to tibble/JSON
    ## Replace empty elements with NA
    data.list[lengths(data.list) == 0] <- NA
    # Set key and initial version is missing
    if (!"key" %in% names(data.list)) {
      data.list <- c(key = ZoteroKey(), version = 0, data.list)
    }
    # Remove elements not in category if item
    if (!"parentCollection" %in% names(data.list)) {
      data.list <- data.list[
        names(data.list) %in%
          c("key", "version", ZoteroTypes(data.list$itemType))]
    }

    # Format as tibble and remove empty elements
    data <- tibble::as_tibble(data.list[lengths(data.list) != 0])

  }

  # Set data as tibble if data frame
  if (is.data.frame(data)) {

    data <- tibble::as_tibble(data) |>
      # Replace empty string with NA
      dplyr::mutate_if(is.character, list(~dplyr::na_if(., ""))) |>
      dplyr::mutate(
        # Make certain tags is a data.frame within a list
        dplyr::across(
          dplyr::any_of("tags"), ~ purrr::map(tags, ~ {
            if (all(!is.na(.x))) as.data.frame(.x)
          })
        ),
        # Make certain that parentCollection is a character
        dplyr::across(dplyr::any_of("parentCollection"), as.character),
        # Add prefix if defined
        prefix = GoFish(prefix),
        # Fix creators
        creators = GoFish(purrr::map(creators, FixCreators))
      ) |>
      # Remove empty columns
      dplyr::select(dplyr::where(~sum(!is.na(.x)) > 0))

    # Else convert to string
  } else {
    data <- ToString(data, "\n")
  }

  return (data)

}

#' @title ZoteroUrl
#' @keywords internal
#' @noRd
ZoteroUrl <- \(url,
               collection.key = NULL,
               use.collection = TRUE,
               item.key = NULL,
               use.item = FALSE,
               api = NULL,
               append.collections = FALSE,
               append.items = FALSE,
               append.file = FALSE,
               append.top = FALSE) {

  # Set ute.item to FALSE if item.key is NULL
  if(is.null(item.key)) use.item <- FALSE
  # Default is not key
  use.key <- FALSE

  # Add item.key if defined and use.item set to TRUE
  if (!is.null(item.key) & use.item) {
    url <- paste0(url,"items/",item.key,"/")
    use.key <- TRUE
    # Else add collection key if defined
  } else if (!is.null(collection.key) & use.collection) {
    url <- paste0(url, "collections/", collection.key, "/")
    use.key <- TRUE
    # Else set append.items to TRUE if no keys
  } else if (!append.collections) {
    append.items <- TRUE
  }

  #  Add top if append.top set to TRUE
  if (use.key & append.top) {
    url <- paste0(url,"top")
  }

  #  Add file if append.file set to TRUE
  if (use.key & append.file) {
    url <- paste0(url,"file")
  }

  # If not using specific item or top level
  if (!use.item & !append.top & !append.file) {
    #  Add items if append.items set to TRUE
    if (append.items) {
      url <- paste0(url,"items")
      # Else add collections if append.collection set to TRUE
    } else if (append.collections) {
      url <- paste0(url,"collections")
    }
  }

  # Add API if defined
  if (!is.null(api)) {
    if (grepl("Sys.getenv", api, perl = TRUE)) api <- eval(parse(text=api))
    url <- sprintf("%s?key=%s", url, api)
  }

  return (url)

}

#' @title Pad
#' @keywords internal
#' @noRd
Pad <- \(string, sep = "-", max.width = 80) {

  # Find remaining character
  remaining.char <- max(0, max.width - nchar(string)) / 2
  head.char <- paste0(rep(sep, floor(remaining.char)), collapse = "")
  tail.char <- paste0(rep(sep, ceiling(remaining.char)), collapse = "")

  # Add pad
  padded <- paste0(head.char, string, tail.char)

  return (padded)

}

#' @title Eta
#' @keywords internal
#' @noRd
Eta <- \(start.time,
         i ,
         total,
         message = NULL,
         flush = TRUE,
         sep = "\u2014",
         max.width = 80) {

  # Estimate time of arrival
  eta <- Sys.time() + ( (total - i) * ((Sys.time() - start.time) / i) )

  # Format ETA message
  eta.message <- sprintf(
    "Progress: %.02f%% (%s/%s). ETA: %s",
    (i * 100) / total,
    i,
    total,
    format(eta,"%d.%m.%Y - %H:%M:%S")
  )
  # Arrived message
  if (i == total) {

    final <- sprintf(
      "Process: %.02f%% (%s/%s). Elapsed time: %s",
      (i * 100) / total,
      i,
      total,
      format(
        as.POSIXct(
          as.numeric(difftime(Sys.time(), start.time, units = "secs")),
          origin = "1970-01-01", tz = "UTC"
        ),
        '%H:%M:%S'
      )
    )

    arrived <- sprintf(
      "Task completed: %s", format(eta, "%d.%m.%Y - %H:%M:%S")
    )
    eta.message <- c(final, arrived)
    # Else results to ETA message if requested.
  } else if (length(message)) {
    eta.message <- sprintf("%s. %s", message, eta.message)
  }

  # Pad ETA message to avoid spilover-effect
  eta.message[1] <- Pad(eta.message[1], sep, max.width)

  return (eta.message)

}

#' @title SplitData
#' @keywords internal
#' @noRd
SplitData <- \(data, limit) {

  # Split metadata into acceptable chunks (k > 50)
  if (nrow(data)>limit) {
    data <- split(
      data,
      rep(
        seq_len(ceiling(nrow(data)/limit)),
        each=limit,
        length.out=nrow(data)
      )
    )
  } else {
    data <- list(data)
  }

  return (data)

}

#' @title ComputerFriendly
#' @keywords internal
#' @noRd
ComputerFriendly <- \(x, sep = "_", remove.after = FALSE) {

  # Remove after common line identifers
  if (remove.after) {
    character.vector <- c(".",",",":",";","-","--",
                          "\u2013","\u2014","\r","\n","/","?")
    remove.after <- paste0("\\", character.vector, collapse=".*|")
    x <- gsub(remove.after, "", x)
  }

  # Try to replace accents and foreign letters
  s <- iconv(x, "utf8", "ASCII//TRANSLIT")
  # Ignore cyrillic and similiar languages
  if (any(grepl("\\?\\?\\?", s))) {
    s <- s
  } else {
    # Replace foreign and whitespace with sep
    s <- gsub("\\W+", sep, s, perl = TRUE)
    # Trim and set to lower or set as x if only ??? (e.g., russian)
    s <- Trim(tolower(s))
  }

  return (s)

}

#' @title JsonToTibble
#' @keywords internal
#' @noRd
JsonToTibble <- \(data) {

  # Parse url
  data.parsed <-ParseUrl(data, "text")

  # Return NULL if data.pased is empty
  if (is.null(data.parsed)) {
    return(NULL)
  }

  # Parse raw data as JSON
  data <- jsonlite::fromJSON(data.parsed)

  # Convert nested elements in list as data.frame
  if (!is.data.frame(data)) {
    data <- lapply(data, \(x) {
      if (is.list(x) | length(x)>1) x <- list(x)
      return (x)
    })
  }
  # Convert and return as tibble
  return(tibble::as_tibble(data))

}

#' @title GoFish
#' @keywords internal
#' @noRd
GoFish <- \(data, type = NA) {

  data <- suppressWarnings(
    tryCatch(data, silent = TRUE, error=function(err) logical(0))
  )
  if (!length(data) | all(is.na(data))) data <- type

  return (data)

}

#' @title ToString
#' @keywords internal
#' @noRd
ToString <- \(x, sep = ", ") {

  x <- paste(unlist(GoFish(x,NULL)), collapse = sep)

  if (x == "") x <- NULL

  return (x)

}

#' @title UpdateInsert
#' @keywords internal
#' @noRd
UpdateInsert <- \(x, y, key = "key") {

  if (!any(nrow(y))) return(x)
  if (!any(nrow(x))) return(y)

  x <- dplyr::bind_rows(x, y) |>
    dplyr::rows_update(
      y, by = key, unmatched = "ignore"
    ) |>
    dplyr::distinct(!!rlang::sym(key), .keep_all = TRUE)

  return(x)

}

#' @title AddMissing
#' @keywords internal
#' @noRd
AddMissing <- \(data,
                missing.names,
                na.type = NA_real_,
                location = 1) {

  missing <- stats::setNames(
    rep(na.type, length(missing.names)), missing.names
  )
  data <- tibble::add_column(data, !!!missing[
    setdiff(names(missing), names(data))], .before = location)

  return (data)

}

#' @title Trim
#' @keywords internal
#' @noRd
Trim <- function(x, multi = TRUE) {
  if (multi) x <- gsub("\\s+", " ", x)
  x <- gsub("^\\s+|\\s+$", "", x)

  return(x)

}

#' @title TrimSplit
#' @keywords internal
#' @noRd
TrimSplit <- \(x,
               sep = ",",
               fixed = FALSE,
               perl = FALSE,
               useBytes = FALSE) {

  x <- Trim(unlist(strsplit(x, sep, fixed, perl, useBytes)))

  return(x)

}

#' @title AddAppend
#' @keywords internal
#' @noRd
AddAppend <- \(data = NULL, old.data = NULL, sep = NULL) {

  data <- GoFish(data, NULL)

  old.data <- GoFish(old.data, NULL)

  if (!is.null(old.data) & !is.null(data)) {
    data <- if (is.data.frame(data)) {
      dplyr::bind_rows(old.data, data) |>
        dplyr::distinct()
    } else if (is.double(data) | (is.list(data))) {
      c(old.data, data)
    } else if(is.character(data)) {
      paste(old.data, data, sep = sep)
    }
  }

  return (data)

}

#' @title LogCat
#' @keywords internal
#' @noRd
LogCat <- \(message = NULL,
            fatal = FALSE,
            flush = FALSE,
            trim = TRUE,
            width = 80,
            log = list(),
            append.log = TRUE,
            silent = FALSE) {

  # Trim message if trim is set to TRUE
  if (trim) message <- Trim(gsub("\r?\n|\r", " ", message))

  # if fatal stop function
  if (fatal) {
    stop(message, call. = FALSE)
  }

  # Print text if silent is set to FALSE
  if (!silent) {
    # flush console after each message if flush and trim is set to TRUE
    if (flush & trim) {
      cat("\r" , message[[1]], sep="")
      # Remove padding
      utils::flush.console()
      # if arrived is in message insert new line
      if (length(message)>1) {
        cat("\n")
      }
      # else trim message and print if trim is set to TRUE
    } else if (trim) {
      cat(message,"\n")
      # else print message as is
    } else {
      print(message, width = width)
      cat("\n")
    }
  }

  # Remove padding
  message <- Trim(gsub("\u2014", "", message))

  # Append to log if append.log set to TRUE else static
  log <- if (append.log) append(log, message) else message

  return (log)

}

#' @title SaveData
#' @keywords internal
#' @noRd
SaveData <- \(data,
              save.name,
              extension,
              save.path = NULL,
              append = FALSE) {

  # Define path and file
  file <- sprintf("%s.%s", save.name, extension)
  if (!is.null(save.path)) {
    # Create folder if it does not exist
    dir.create(file.path(save.path), showWarnings = FALSE, recursive = TRUE)
    file <- file.path(save.path, file)
  }

  # save as csv
  if (extension == "csv") {
    utils::write.csv(data, file, row.names = FALSE, fileEncoding = "UTF-8")
    # Save as text file
  } else {
    write(data, file = file, append = append)
  }

  return (file)

}

#' @title CleanText
#' @keywords internal
#' @noRd
CleanText <- \(x, multiline = FALSE) {

  # Trim original vector
  x <- Trim(x)

  # List of characters to remove
  character.vector <- c(",",":",";","-","--","\u2013","\u2014",
                        "[","]","{","}","=","&","/")
  remove.characters <- paste0("\\", character.vector, collapse="|")

  # Remove first character if unwanted
  first.character <- gsub(remove.characters, "", substring(x, 1, 1))
  # Put Humpty togheter again
  if (max(0,nchar(Trim(gsub("(\\s).*", "\\1", x)))) == 1) {
    x <- paste(first.character, Trim(substring(x, 2)))
  } else {
    x <- paste0(first.character, Trim(substring(x, 2)))
  }

  # Remove last character if unwanted
  last.character <- gsub(remove.characters, "", substr(x, nchar(x), nchar(x)))
  # Put Humpty togheter again
  x <- paste0(Trim(substr(x, 1, nchar(x)-1)), last.character)

  # Remove any #. (e.g., 1. Title)
  x <- Trim(gsub("^\\s*\\d+\\s*\\.", "", x))

  # Remove \r\n if multiline is set to FALSE
  if (!multiline) x <- Trim(gsub("\r?\n|\r", " ", x))

  # Remove HTML/XML tags
  x <- Trim(gsub("<.*?>|\ufffd|&lt;|&gt", "", x))

  # Remove any abstract from beginning of string
  if (any(grepl("abstract", tolower(substr(x, 1, 8))))) {
    x <- substring(x, 9)
  }

  # Convert ALL CAPITALS to All Capitals
  if (all(stringr::str_detect(x, "^[[:upper:][:space:]]+$"))) {
    x <- stringr::str_to_title(x)
  }

  # Remove NA
  x <- x[! x %in% c("NANA")]

  return (x)

}

#' @title Mime
#' @keywords internal
#' @noRd
Mime <- \(x, mime = FALSE, charset = FALSE) {

  # Remove charset information
  if (grepl("charset",x)) {
    x <- unlist(strsplit(x,";"))
    x.charset <- x[2]
    x <- x[1]
  }

  # Define Mime types
  data <- c(bib = "application/x-bibtex",
            csv = "text/csv",
            json = "application/json",
            html = "text/html;charset",
            json = "application/vnd.citationstyles.csl+json",
            xml = "application/mods+xml",
            pdf = "application/pdf",
            ris = "application/x-research-info-systems",
            rdf = "application/rdf+xml",
            xml = "text/xml",
            txt = "text/x-wiki")

  # Set data as either Mime or extension
  data <- if (!mime) names(data[data == x]) else data[[x]]

  # If no data set as json / txt
  if (!length(data)) {
    data <- if (mime) "application/json" else "txt"
  }

  # Append charset if Mime and charset is set to TRUE
  if (!mime & charset) data <- paste0(data,x.charset)

  return (data)

}

#' @title ReadCss
#' @keywords internal
#' @noRd
ReadCss <- \(data, css, clean.text = TRUE) {

  data <- data |>
    rvest::html_nodes(css) |>
    rvest::html_text()

  if (clean.text) data <- data |> CleanText()

  return (data)

}

#' @title ReadXpath
#' @keywords internal
#' @noRd
ReadXpath <- \(data, xpath, clean.text = TRUE, first = FALSE) {

  data <- data |>
    rvest::html_nodes(xpath = xpath) |>
    rvest::html_text()

  if (first) data <- head(data, 1)
  if (clean.text) data <- data |> CleanText()

  return (data)

}

#' @title ReadAttr
#' @keywords internal
#' @noRd
ReadAttr <- \(data, xpath, attr) {

  data <- data |>
    rvest::html_nodes(xpath = xpath) |>
    rvest::html_attr(attr)

  return (data)

}

#' @title ParseUrl
#' @keywords internal
#' @noRd
ParseUrl <- \(x,
              format = "text",
              as = NULL,
              type = NULL,
              encoding = "UTF-8") {


  # Define parse method
  if (is.null(as)) {

    formats <- c(raw = "raw",
                 coins = "parsed",
                 csv = "parsed",
                 bookmarks = "parsed",
                 json = "text",
                 csljson = "text",
                 bibtex = "text",
                 biblatex = "text",
                 mods = "text",
                 refer = "text",
                 rdf_bibliontology = "text",
                 rdf_dc = "text",
                 rdf_zotero = "text",
                 ris = "text",
                 tei = "text",
                 wikipedia = "text",
                 txt = "text",
                 text = "text")

    as <- formats[format]
    if (is.na(as)) as <- "text"
  }

  # Parse data
  result <- httr::content(
    x, as, type, encoding, show_col_types = FALSE
  )

  # Remove empty json
  if (as == "text" & any(length(result))) {
    if (result == "[]" | result == "\n" | result == "") result <- NULL
  }

  return (result)

}

#' @title EditionFix
#' @keywords internal
#' @noRd
EditionFix <- \(edition) {

  # Some ISBN lists two edtions (e.g., 2nd ed. and global ed.)
  if (length(edition)>1) {
    edition <-   edition <- GoFish(
      sort(unique(unlist(lapply(edition, \(x) {
        x[grepl("\\d", EditionFix(x))]
      }))))[[1]]
    )
  }

  if (length(edition)) {
    # Convert English letters [1:20] to numeric
    English2Numeric <- \(word) {
      rank <- (c("first", "second", "third", "fourth", "fifth", "sixth",
                 "seventh", "eighth", "ninth", "tenth", "eleventh",
                 "twelfth", "thirteenth", "fourteenth", "fifteenth",
                 "sixteenth", "seventeenth", "eighteenth",
                 "nineteenth", "twentieth"))
      pos <- which(lapply(rank, \(x) grepl(x, tolower(word))) == TRUE)
      if (length(pos)) word <- pos[[1]]
      return (word)
    }

    # Convert Norwegian letters [1:20] to nureric
    Norwegian2Numeric <- \(word) {
      rank <- (c("f\u00f8rste", "andre", "tredje", "fjerde", "femte", "sjette",
                 "syvende", "\u00e5ttende", "niende", "tiende", "ellevte",
                 "tolvte", "trettende", "fjortende", "femtende",
                 "sekstende", "syttende", "attende",
                 "nittende", "tjuende"))
      pos <- which(lapply(rank, \(x) grepl(x, tolower(word))) == TRUE)
      if (length(pos)) word <- pos[[1]]
      return (word)
    }

    # Replace written edition with numeric
    if (!is.null(edition)) {
      if (!grepl("\\d", edition)) {
        edition <- English2Numeric(edition)
        edition <- Norwegian2Numeric(edition)
      }
      if (grepl("\\d", edition)) {
        # Extract numeric only from edition
        edition <-  suppressWarnings(
          as.numeric(gsub("[^0-9-]", "", edition))
        )
      }
    }

    # set edition as NA if first edition
    if (edition == "1") edition <- NA
  }

  return (as.character(edition))

}
