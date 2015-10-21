# Set up some useful options
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.growthcurver <- list(
    growthcurver.path = "~/growthcurver",
    growthcurver.install.args = "",
    growthcurver.name = "Kathleen Sprouffske",
    growthcurver.desc.author = '"Kathleen Sprouffske <sprouffske@gmail.com> [aut, cre]"',
    growthcurver.desc.license = "GPL (>=2)",
    growthcurver.desc.suggests = NULL,
    growthcurver.des = list()
  )
  toset <- !(names(op.growthcurver) %in% names(op))
  if(any(toset)) options(op.growthcurver[toset])

  invisible()
}
