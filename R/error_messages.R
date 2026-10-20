# Helper used by plot_sunburst() to signal problems.
# `type` is accepted for backwards compatibility with the old pop-up
# behaviour; all conditions are now signalled as plain R warnings/errors.
error_messages <- function(type = "text", error_message, is_error = FALSE) {
  if (is_error) {
    rlang::abort(error_message)
  } else {
    rlang::warn(error_message)
  }
}
