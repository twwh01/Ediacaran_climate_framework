## dataframe of currently considered duration of Ediacaran glaciations
## Author: Thomas W. Wong Hearing
## Updated: 2024-04-24

ecesd_glaciations <- data.frame(
  glaciation_name = c(
    "Mid-Ediacaran icehouse",
    "Late Ediacaran icehouse"
  ),
  glaciation_abbr = c(
    "MEIH",
    "LEIH"
  ),
  Compilation = c(
    "ECESD",
    "ECESD"
  ),
  Age_min = c(
    579,
    550
  ),
  Age_max = c(
    593,
    565
  ),
  Age_min_U = c(
    579,
    550
  ),
  Age_max_U = c(
    595,
    560
  )
)

ecesd_glaciations$Age_mid <- (ecesd_glaciations$Age_max + ecesd_glaciations$Age_min)/2
