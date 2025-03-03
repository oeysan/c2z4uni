% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cristin_json.R
\name{CristinJson}
\alias{CristinJson}
\title{Convert Cristin monthly data to JSON format}
\usage{
CristinJson(
  cristin.monthly,
  local.storage = NULL,
  user.cards = TRUE,
  full.update = FALSE,
  lang = "nn",
  silent = FALSE,
  log = list()
)
}
\arguments{
\item{cristin.monthly}{A list containing the monthly data from Cristin,
including bibliographic information.}

\item{local.storage}{Path to local storage directory. Default is NULL.}

\item{user.cards}{Logical, indicating whether to update user information
from INN. Default is TRUE.}

\item{full.update}{Logical, indicating whether to perform a full update of
user information from INN. Default is FALSE.}

\item{lang}{Language for localization ("nb", "nn", "no", or "en"). Defaults
to "nn".}

\item{silent}{Logical, indicating whether to suppress log messages. Default
is FALSE.}

\item{log}{List to store log messages. Default is an empty list.}
}
\value{
A JSON-formatted string representing the converted Cristin monthly
data.
}
\description{
This function converts the provided Cristin monthly data into
JSON format, which can be used for various purposes.
}
\details{
Used with `CristinMonthly` to create month-to-month bibliography in
JSON format
}
\examples{
\donttest{
  # Create monthlies for unit 209.5.10.0
  example <- CristinMonthly(
    c2z::Zotero(
      id = "4827927",
      api = "RqlAmlH5l1KPghfCseAq1sQ1",
      user = FALSE
    ),
    unit.id = "209.5.10.0",
    start.date = "2023-07",
    post = TRUE,
    silent = TRUE
  ) |>
    c2z:::GoFish()

  # Create json data
  example.json <- CristinJson(example, user.cards = FALSE) |>
    jsonlite::prettify() |>
    c2z:::GoFish()

  if (any(!is.na(example.json))) cat(example.json)
}
}
