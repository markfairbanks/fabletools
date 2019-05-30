#' Extract the response data from a model
#' 
#' @param object The object containing response data
#' @param ... Additional parameters passed on to other methods
#' 
#' @export
response <- function(object, ...){
  UseMethod("response")
}

#' @export
response.mdl_df <- function(object, ...){
  object <- gather(object, ".model", ".fit", !!!syms(object%@%"models"))
  kv <- key_vars(object)
  object <- transmute(as_tibble(object),
                   !!!syms(kv),
                   !!sym(".model"),
                   response = map(!!sym(".fit"), response)
  )
  
  idx <- index(object[["response"]][[1L]])
  kv <- c(kv, key_vars(object[["response"]][[1L]]))
  as_tsibble(unnest(object, !!sym("response")), index = !!idx, key = kv)
}

#' @export
response.mdl_ts <- function(object, ...){
  bt <- map(object$transformation, invert_transformation)
  
  resp <- as.list(object$data)[measured_vars(object$data)]
  resp <- map2(bt, resp, function(bt, fit) bt(fit))
  
  nm <- if(length(resp) == 1) ".response" else map_chr(object$response, expr_text)
  
  transmute(object$data, !!!set_names(resp, nm))
}