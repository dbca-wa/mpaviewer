#' Build and push Docker images
#'
#' @export
make_docker <- function() {
  message("Deleting old package build...")
  fs::dir_ls(".", glob = "*.tar.gz") %>% fs::file_delete()

  # Why does this build the whole package? Does this ever get used for anything???
  message("Building current package...")
  devtools::build(path = ".")

  dn <- "claudehspencer/mpaviewer"
  # dn <- "dddamonnn/mpaviewer"
  dv <- utils::packageVersion("mpaviewer")
  gp <- Sys.getenv("GITHUB_PAT")
  message(glue::glue("Building and pushing {dn}:{dv}..."))
  status <- system(glue::glue(
    "docker build . -t {dn} -t {dn}:{dv} ",
    "--build-arg GITHUB_PAT={gp} && docker push {dn}"
  ))

  if (status == 1) {
    stop("Build failed (check Dockerfile)")
  }

  if (status == 125) {
    stop("Docker failed to run")
  }

  if (status == 126) {
    stop("Permission issue")
  }

  if (status == 127) {
    stop("Docker not installed")
  }

  if (status == 137) {
    stop("Container killed (memory issue)")
  }

  if (status == 0) {
    message(glue::glue("Success, pushed {dn}:{dv}"))
  }
}


# To test (starting the container)
# docker run -d --rm -p 3838:3838 my-shinyapp-image
