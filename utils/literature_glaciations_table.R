## comparison dataframes of possible Ediacaran glaciation timings
## Author: Thomas W. Wong Hearing
## Updated: 2025-05-30

literature_glaciations <- data.frame(
  glaciation_name = ordered(
    c(
      "GEG (uncertainty)",
      "Great Ediacaran Glaciation",
      "Gaskiers",
      "Upper Ediacaran Glacial Period",
      "Gaskiers",
      "Fauquier",
      "Bou-Azzer",
      "Hankalchough",
      "Gaskiers",
      "Fauquier",
      "Bou-Azzer",
      "Hankalchough",
      "Mid-Ediacaran icehouse",
      "Late Ediacaran icehouse"
    ),
    levels = c(
      "GEG (uncertainty)",
      "Great Ediacaran Glaciation",
      "Gaskiers",
      "Fauquier",
      "Bou-Azzer",
      "Hankalchough",
      "Upper Ediacaran Glacial Period",
      "Mid-Ediacaran icehouse",
      "Late Ediacaran icehouse"
    )
  ),
  glaciation_abbr = c(
    "GEG:U",
    "GEG",
    "Gas",
    "UEGP",
    "Gas",
    "Fau",
    "BAz",
    "Han",
    "Gas",
    "Fau",
    "BAz",
    "Han",
    "MEIH", 
    "LEIH"
  ),
  Compilation = ordered(
    c(
      "Wang et al 2023a,b",
      "Wang et al 2023a,b",
      "Linnemann et al 2022",
      "Linnemann et al 2022",
      "Retallack 2022",
      "Retallack 2022",
      "Retallack 2022",
      "Retallack 2022",
      "Niu et al 2024",
      "Niu et al 2024",
      "Niu et al 2024",
      "Niu et al 2024",
      "this study",
      "this study"
    ),
    levels = c(
      "Linnemann et al 2022",
      "Retallack 2022",
      "Wang et al 2023a,b",
      "Niu et al 2024",
      "this study"
    )
  ),
  Age_min = c(546,
              560,
              579,
              561,
              579,
              570,
              564,
              549,
              579,
              570.5,
              560,
              551,
              579,
              550
              ),
  Age_max = c(597,
              580,
              581,
              568,
              581,
              572,
              566,
              555,
              581,
              571.5,
              565,
              562,
              593,
              565
              ),
  Age_min_U = c(546,
                546,
                579,
                561,
                579,
                570,
                564,
                549,
                579,
                571.5,
                560,
                551,
                579,
                550
                ),
  Age_max_U = c(597,
                597,
                581,
                568,
                581,
                572,
                566,
                555,
                581,
                570.5,
                565,
                562,
                593,
                560
                )
)

literature_glaciations$Age_mid <-
  (literature_glaciations$Age_max + literature_glaciations$Age_min) / 2
