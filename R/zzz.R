# nocov start
.onLoad <- function(...) {
  register_s3_method("pillar", "type_sum", "mdl_ts")
  register_s3_method("pillar", "type_sum", "hilo")
  register_s3_method("pillar", "type_sum", "fcdist")
  register_s3_method("pillar", "type_sum", "lst_mdl")
  register_s3_method("pillar", "type_sum", "fbl_ts")
  
  register_s3_method("pillar", "obj_sum", "hilo")
  register_s3_method("pillar", "obj_sum", "fcdist")
  
  register_s3_method("pillar", "pillar_shaft", "hilo")
  register_s3_method("pillar", "pillar_shaft", "fcdist")
  register_s3_method("pillar", "pillar_shaft", "agg_vec")
  
  register_s3_method("pillar", "is_vector_s3", "hilo")
  register_s3_method("pillar", "is_vector_s3", "fcdist")
  
  register_s3_method("tibble", "tbl_sum", "dcmp_ts")
  register_s3_method("tibble", "tbl_sum", "mdl_df")
  register_s3_method("tibble", "tbl_sum", "fbl_ts")
  
  register_s3_method("dplyr", "filter", "fbl_ts")
  register_s3_method("dplyr", "filter", "grouped_fbl")
  register_s3_method("dplyr", "filter", "mdl_df")
  
  register_s3_method("ggplot2", "scale_type", "agg_vec")
  
  op <- options()
  op.fable <- list(
    fable.show_progress = TRUE
  )
  toset <- !(names(op.fable) %in% names(op))
  if (any(toset)) options(op.fable[toset])
  
  invisible()
}

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)
  
  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }
  
  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }
  
  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
# nocov end