####################################################################
# Helper function
# Display errors depending on the option selected by the user
####################################################################
error_messages <- function(type="all", error_message,
                           is_error = FALSE){
  if(type == "all"){
    if (is_error == TRUE){
      tcltk::tk_messageBox(type=c("ok"), message=error_message,
                           icon="error", caption="Critical Error!!!")
    } else{
      tcltk::tk_messageBox(type=c("ok"), message=error_message,
                           icon="warning", caption="Warning!")
    }

  }
  # Stop executing program, or just warn the user
  if (is_error == TRUE){
    rlang::abort(error_message)
  } else{
    rlang::warn(error_message)
  }

}
