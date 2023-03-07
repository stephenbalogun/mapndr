validate_lga_maps <- function(
    label_lga,
    label_fill,
    size_lga,
    size_fill,
    # gradient,
    # grad_direction,
    interactive) {
  if (!is.logical(label_lga)) {
    rlang::abort("The value supplied to the `label_lga` argument is not a logical vector. Logical vectors in R are written in capital letters and unquoted. Did you forget to write the word in capital letters or did you add quotes?")
  }

  if (!label_lga && !is.null(size_lga)) {
    rlang::inform("The `label_lga` value is currently set to `FALSE`. Therefore, you cannot have a font-size for the lga labels and the argument is ignored. Do you want to set the `label_lga` value to `TRUE`?")
  }

  if (!is.logical(label_fill)) {
    rlang::abort("The value supplied to the `label_fill` argument is not a logical vector. Logical vectors in R are written in capital letters and unquoted. Did you forget to write the word in capital letters or did you add quotes?")
  }

  if (!label_fill && !is.null(size_fill)) {
    rlang::inform("The `label_fill` value is currently set to `FALSE`. Therefore, you cannot have a font-size for the fill labels and the argument is ignored. Do you want to set the `label_fill` value to `TRUE`?")
  }

  if (!is.null(size_lga) && !is.numeric(size_lga)) {
    rlang::abort("`size_lga` value must be in numbers")
  }

  if (!is.null(size_fill) && !is.numeric(size_fill)) {
    rlang::abort("`size_fill` value must be in numbers")
  }

  # if (is.null(gradient) && !is.null(grad_direction)) {
  #   rlang::inform("Ignoring `grad_direction` argument as the `gradient` argument is currently set to NULL")
  # }

  if (!is.logical(interactive)) {
    rlang::abort("The interactive value supplied is not a logical vector. Did you forget to write the word in capital letters?")
  }
}



validate_state_maps <- function(
    label_state,
    label_fill,
    size_state,
    size_fill,
    interactive) {
  if (!is.logical(label_state)) {
    rlang::abort("The value supplied to the `label_state` argument is not a logical vector. Logical vectors in R are written in capital letters and unquoted. Did you forget to write the word in capital letters or did you add quotes?")
  }

  if (!label_state && !is.null(size_state)) {
    rlang::inform("The `label_state` value is currently set to `FALSE`. Therefore, you cannot have a font-size for the state labels and the argument is ignored. Do you want to set the `label_state` value to `TRUE`?")
  }

  if (!is.logical(label_fill)) {
    rlang::abort("The value supplied to the `label_fill` argument is not a logical vector. Logical vectors in R are written in capital letters and unquoted. Did you forget to write the word in capital letters or did you add quotes?")
  }

  if (!label_fill && !is.null(size_fill)) {
    rlang::inform("The `label_fill` value is currently set to `FALSE`. Therefore, you cannot have a font-size for the fill labels and the argument is ignored. Do you want to set the `label_fill` value to `TRUE`?")
  }

  if (!is.null(size_state) && !is.numeric(size_state)) {
    rlang::abort("`size_state` value must be in numbers")
  }

  if (!is.null(size_fill) && !is.numeric(size_fill)) {
    rlang::abort("`size_fill` value must be in numbers")
  }

  if (!is.logical(interactive)) {
    rlang::abort("The interactive value supplied is not a logical vector. Did you forget to write the word in capital letters?")
  }
}

validate_pyramid <- function(label, interactive, size, border, border_color, inverse) {
  if (!is.logical(label)) {
    rlang::abort("The label value is not a logical vector. Logical vectors in R are written in capital letters and unquoted. Did you forget to write the word in capital letters or did you add quotes?")
  }

  if (!is.logical(interactive)) {
    rlang::abort("The interactive value supplied is not a logical vector. Did you forget to write the word in capital letters?")
  }

  if (!is.null(size) && !is.numeric(size)) {
    rlang::abort("`size` value must be in numbers")
  }

  if (!is.logical(border)) {
    rlang::abort("The value supplied to the `border` argument must be either `TRUE` or `FALSE`")
  }

  if (!border && !is.null(border_color)) {
    rlang::inform("This plot border argument has been set to `FALSE` and the border color supplied will be ignored")
  }

  if (!is.logical(inverse)) {
    rlang::abort("The value supplied to the `inverse` argument is not a logical vector. Did you forget to write the word in capital letters?")
  }
}
