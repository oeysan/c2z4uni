# c2z4uni (development version)

#### Minor 

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
