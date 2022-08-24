#' Build and push Docker images
#'
#' @export
make_docker <- function() {
  message("Deleting old package build...")
  fs::dir_ls(".", glob = "*.tar.gz") %>% fs::file_delete()

  message("Building current package...")
  devtools::build(path = ".")

  # dn <- "dbcawa/mpaviewer"
  dn <- "florianmayer/mpaviewer"
  dv <- utils::packageVersion("mpaviewer")
  gp <- Sys.getenv("GITHUB_PAT")
  message(glue::glue("Building and pushing {dn}:{dv}..."))
  system(glue::glue(
    "docker build . -t {dn} -t {dn}:{dv} ",
    "--build-arg GITHUB_PAT={gp} && docker push {dn}"
  ))
  message(glue::glue("Success, pushed {dn}:{dv}"))
}
