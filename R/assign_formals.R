#' Assign the formals of a function
#' 
#' Assign the 'formals' of a function to the global enviroment. Helpful when
#' debugging or prototyping. 
#'
#' @param fun A function.
#'
#' @return Assigns the default values of the arguments of a function to
#'   variables of the argument name.
#'   Invisibly returns the assigned variable names.
#' @examples
#' assigned_vars <- assign_formals(stats::binom.test)
#' sort(assigned_vars)
#' ls()
#' alternative
#' eval(alternative)
#' @export
assign_formals <- function(fun) {
  the_formals <- formals(fun)
  
  arg_names <- names(the_formals)
  the_formals <- the_formals[arg_names != "..."]
  
  for (i in seq_along(the_formals))  {
    assign(x = arg_names[i], value = the_formals[[i]],
           envir = .GlobalEnv)
  }
  return(invisible(arg_names))
}
