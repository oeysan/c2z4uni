# c2z4uni (development version)

#### Features

* `CristinMonthly` can now conduct SDG predictions derived from 
Vanderfeesten et al. (2022). Either using a self-hosted python script and models
or a user-defined host with proper using proper BERT-models

* `CristinMonthly` can now check for open source publications using `Unpaywall`
(set `get.unpaywall` to TRUE) and/or use `get.ezproxy` to append a EZproxy as
defined by `ezproxy.host`

#### Minor 

* Separated `CristinMonthly` into a main function an two internal helper 
functions: `CreateMonthlies` and `CreateExtras`. `CreateMonthlies` will create
the main monthly bibliographies and store (if `local.storage` is defined) at 
`monthlies_LANG.rds`, similary `CreateExtras` will store extra metadata (e.g.,
SDG predictions, Unpaywall links, EZproxy links etc.) in `monthlies.extras.rds`

* Some minor adjustment to the internal function `InnCristin`, that now only 
searches for first and last name
* Added a missing profile image icon to user cards

#### Bug fixes

* Path to SDG images
* Workflow for windows

# c2z4uni 0.1.0.9000

* Initial launch

#### Major

* Greatly improved `CristinMonthly` (that is, making it less cumbersome) by 
speeding up the various steps. The process could be further sped up by using 
the argument `local.storage` thus pointing to a storage path of the various
outputs in rds format (possibly using sqlite in the future). The logical 
argument `full.update` (default FALSE) enables, wait for it, a full update of
collections, items, and paths.

* Moved `CristinMonthly` and `CristinUnits` from `c2z` to this package to keep
maintenance simpler.
