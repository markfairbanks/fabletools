#' @export
components.mdl_df <- function(object, ...){
  object <- gather(object, ".model", ".fit", !!!syms(object%@%"models"))
  kv <- key_vars(object)
  object <- transmute(as_tibble(object),
                      !!!syms(kv), !!sym(".model"),
                      cmp = map(!!sym(".fit"), components))
  attrs <- combine_dcmp_attr(object[["cmp"]])
  idx <- index(object[["cmp"]][[1L]])
  kv <- c(kv, key_vars(object[["cmp"]][[1L]]))
  object <- unnest(object)
  as_dable(object, index = !!idx, key = kv,
           method = attrs[["method"]], resp = !!attrs[["response"]],
           seasons = attrs[["seasons"]], aliases = attrs[["aliases"]])
}

#' @export
components.mdl_ts <- function(object, ...){
  components(object$fit, ...)
}