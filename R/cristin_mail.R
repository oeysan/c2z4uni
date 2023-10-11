#' @title Create HTML Email Content from Cristin Data
#' @description This function generates HTML content for an email using data
#' from Cristin.
#' @param unit.key The key of the unit for which the email content is generated.
#' @param cristin.monthly Data containing monthly information from Cristin.
#' @param subject The subject of the email. If not provided, it will be
#' generated based on the data.
#' @param header The header of the email. If not provided, it will be generated
#' based on the data.
#' @param footer The footer of the email. If not provided, a default footer
#' will be used.
#' @param replace.style Logical, indicating whether to replace styles in the
#' generated HTML.
#' @param width Width of the email content in pixels.
#' @param lang Language for localization ("nb", "nn", "no", or "en"). Defaults
#' to "nn".
#' @return A list containing the subject and HTML body of the email.
#' @details Used with `CristinMonthly` to create month-to-month bibliography in
#' HTML format for email of selected units
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
#'     silent = FALSE
#'   )
#'
#'   # Create HTML email
#'   if (any(nrow(example$monthlies))) {
#'     example.mail <- CristinMail("209.5.10.0", example)
#'
#'     # Show subject
#'     cat(example.mail$subject)
#'   }
#' }
#' @rdname CristinMail
#' @export
CristinMail <- \(unit.key,
                 cristin.monthly,
                 subject = NULL,
                 header = NULL,
                 footer = NULL,
                 replace.style = TRUE,
                 width = 700,
                 lang = "nn") {

  # Visible bindings
  body <- month.key <- year.month <- year <- id <- collections <-
    collection.names <- NULL

  # Languages
  # Set language to en if not no
  lang <- if (lang %in% c("no", "nn", "nb")) "no" else "en"

  # Definitions
  monthlies <- cristin.monthly$monthlies
  unit.paths <- cristin.monthly$unit.paths

  # Keep only latest entry
  bib <- monthlies |>
    dplyr::filter(
      grepl(
        dplyr::filter(unit.paths, id == unit.key)$key,
        collections)
    ) |>
    dplyr::group_split(year.month) |>
    rev() |>
    purrr::pluck(1) |>
    tidyr::unnest(collection.names) |>
    dplyr::arrange(
      dplyr::across(
        dplyr::starts_with("collection.name"),
        ~ data.frame(!is.na(.x), .x)
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        c(year, dplyr::starts_with("collection.name")), ~
          replace(.x, duplicated(.x), NA)
      ),
      # Create headers for each row
      headers = purrr::pmap_chr(
        dplyr::across(dplyr::starts_with("collection.name")), ~ {
          x <- c(...)
          lapply(seq_along(x), \(i) {
            if (!is.na(x[[i]])) {
              if (i == 1) {
                sprintf("<h1 style=\"text-align:center;\">%s</h1>", x[[i]])
              } else {
                sprintf("<h%1$s>%2$s</h%1$s>", i, x[[i]])
              }
            }
          }) |>
            ToString("\n") |>
            GoFish()
        }
      )
    )

  # Return NULL if no data
  if (!any(nrow(bib))) {
    return (NULL)
  }

  # replace style from bibliography
  ## CSS is poorly supported in many email clients
  if (replace.style) {
    bib$bib <- gsub(
      "*.style=(\"|')[^(\"|')]*(\"|')",
      "",
      bib$bib
    )
    # Add space between items
    bib$bib <- gsub(
      "class=\"csl-entry\"",
      "class=\"csl-entry\" style=\"margin: 0.5em 0;\"",
      bib$bib
    )
  }

  # Define subject
  if (is.null(subject)) {
    subject <- sprintf(
      Dict("email.subject", lang),
      tolower(Month(bib$month[[1]], lang = lang)),
      bib$year[[1]]
    )
  }

  # Define header
  if (is.null(header)) {
    header <- sprintf(
      Dict("email.header", lang),
      ToString(AncestorPath(
        unit.paths,
        dplyr::filter(unit.paths, id == unit.key)$key,
        name = TRUE
      )),
      tolower(Month(bib$month[[1]], lang = lang)),
      bib$year[[1]]
    )
  }

  # Define footer
  if (is.null(footer)) {
    footer <- sprintf(
      "<div style=\"font-size:80%%;margin: 2em auto 0 auto; text-align: center;\">
    %s <i><a href=\"https://oeysan.github.io/c2z4uni/\">c2z4uni</a></i></div>",
      Dict("c2z", lang)
    )
  }

  # Create HTML body with references from Cristin
  body <- Trim(sprintf(
    "<table width=\"%1$s\" cellpadding=\"0\" cellspacing=\"0\">
    <tr><td>
    <div style=\"max-width:%1$spx;\">
    %2$s%3$s%4$s
    </div>
    </td></tr>
    </table>",
    width,
    header,
    ToString(Interleave(bib$headers, bib$bib), "\n"),
    footer
  ))

  return (list(subject = subject, body = body))

}
