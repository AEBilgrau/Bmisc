#' Installs brainarray CDF and probe information packages
#'
#' @export
install.brainarray <- function(array,
                               version,
                               type = "ensg",
                               force.reinstall = FALSE,
                               force.download = FALSE,
                               use.temp.dir = TRUE,
                               path) {
  #array <- "hgu133a"; version <- "18.0.0"; type <- "ensg"
  #force.reinstall <- TRUE; force.download <- TRUE; use.temp.dir <- TRUE

  # Construct the file names and path used
  files <- paste0(array, "hs", type, c("cdf_", "probe_"), version, ".tar.gz")

  # Load or install if not installed
  package.name <- paste0(array, "hs", type, c("cdf", "probe"))

  for (i in seq_along(package.name)) {

    pkg <- package.name[i]

    # Is pkg installed... ?
    if (!(force.download | force.reinstall) &
          require(pkg, character.only = TRUE)) {

      # ... and is it the correct version?
      if (packageVersion(pkg) == version) {
        next                                    # if yes, then do the next pkg
      }

    } else {
   
      # Prepare to download
      # Base URL to the brainarray site
      base.url <-
        paste0("http://brainarray.mbni.med.umich.edu/Brainarray/Database/",
               "CustomCDF/", version, "/" , type, ".download")

      # Path to download files to
      if (missing(path)) {
        path <- ifelse(use.temp.dir,
                       tempdir(),
                       file.path(array, "Brainarray"))
      }

      # Create the dir
      dir.create(path = path, recursive = TRUE, showWarnings = FALSE)

      # Download the files if they aren't already
      if (!file.exists(file.path(path, files[i])) | force.download) {
        download.file(url = file.path(base.url, files[i]),
                      destfile = file.path(path, files[i]))
      }

      # Install package
      install.packages(pkgs = file.path(path, files[i]),
                       repos = NULL, type = "source")

      # Load package
      require(pkg, character.only = TRUE)

    }

  }

}
