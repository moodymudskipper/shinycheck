# check that all module scripts contain exactly 2 functions named appropriately

check_file_and_function_names <- function(
  path = here::here("R")) {
  files <- list.files(path, pattern = "mod[_a-z]+\\.R", full.names = TRUE)
  for(file in files) {
    mod_name <- basename(file)
    mod_name <- substr(mod_name,5, nchar(mod_name)-2)
    env <- new.env()
    source(file, env)
    our_names <- sort(names(env))
    if(length(our_names) != 2) {
      msg <- paste(sprintf(
        "* In 'mod_%s.R', we don't find exactly two functions.`",
        mod_name))
     message(msg)
      next
    }

    good_names <- paste0("mod_", mod_name, c("_server", "_ui"))
    mismatches <- our_names != good_names
    if(any(mismatches)) {
      msg <- paste(sprintf(
        "* In 'mod_%s.R', ui and server function names should be `mod_%s_ui` and `mod_%s_server.`",
        mod_name, mod_name, mod_name))
     message(msg)
    }
  }
}

# check that all module ui functions use ns() or NS() on argument named id/inputId/outputId
check_ns_used_for_ids <- function(
  path = here::here("R")) {
  files <- list.files(path, pattern = "mod[_a-z]+\\.R", full.names = TRUE)
  for(file in files) {
    mod_name <- basename(file)
    mod_name <- substr(mod_name,5, nchar(mod_name)-2)
    env <- new.env()
    source(file, env)
    ui_fun_nm <- grep("_ui$", names(env), value = TRUE)
    ui_fun <- env[[ui_fun_nm]]

    recurse <- function(expr) {
      ## are we exploring a call
      if(!is.call(expr)) return(invisible(NULL))
      ## is the calling function a symbol that exists (i.e. defined or attached)?
      if(is.symbol(expr[[1]]) && exists(fun_nm <- as.character(expr[[1]]))) {
        fun <- eval(expr[[1]])
        ## is it a closure ?
        if(!is.primitive(fun)) {
          named_args <- as.list(match.call(fun, expr))[-1]
          # assuming we don't have both id and inputId
          id_arg <- c(named_args[["id"]], named_args[["inputId"]], named_args[["outputId"]])[[1]]
          if(!is.null(id_arg)) {
            if(!is.call(id_arg) || ! list(id_arg[[1]]) %in% c(quote(ns), quote(NS))) {
              msg <- paste(sprintf(
                "* In 'mod_%s_ui', a call to `%s` is not using `NS` or `ns` around its `id`, `inputId` or `outputId` argument",
                mod_name, fun_nm))
             message(msg)
            }
          }
        }
      }
      invisible(lapply(expr, recurse))
    }
    recurse(body(ui_fun))
  }
}

# check that in ui module ui functions refer to modules which exist and ids fed to them are formatted properly
check_ui_funs_use_well_formatted_ids <- function(
  path = here::here("R")) {
  files <- list.files(path, pattern = "mod[_a-z0-9]+\\.R", full.names = TRUE)
  for(file in files) {
    mod_name <- basename(file)
    mod_name <- substr(mod_name,5, nchar(mod_name)-2)
    env <- new.env()
    source(file, env)
    ui_fun_nm <- grep("_ui$", names(env), value = TRUE)
    ui_fun <- env[[ui_fun_nm]]

    recurse <- function(expr) {
      ## are we exploring a call
      if(!is.call(expr)) return(invisible(NULL))
      ## is the calling function a module ui fun?
      if(is.symbol(expr[[1]]) && grepl("^mod_.*_ui$", fun_nm <- as.character(expr[[1]]))) {

        mod <- gsub("^mod_(.*)_ui$", "\\1", fun_nm)

        # check that module exists
        if(!paste0("mod_",mod, ".R") %in% basename(files)) {
          msg <- sprintf("* In 'mod_%s_ui', we find a call to `%s` but '%s' doesn't exist",
                         mod_name, fun_nm, paste0("mod_",mod, ".R"))
          message(msg)
        }

        id_arg <- expr[[2]]

        # check that ids fed to module functions are compatible
        if (as.character(id_arg[[1]]) == "ns") {
          id <- id_arg[[2]]
        } else if (as.character(id_arg[[1]]) == "NS") {
          id <- id_arg[[3]]
        } else {
          # possibly redundant with check from previous function
          msg <- sprintf("* In 'mod_%s_ui', a call to `%s` doesn't use `ns` or `NS` around its `id` argument",
                         mod_name, fun_nm)
          message(msg)
        }

        # should not happen, but to be safe
        if(!is.character(id)) id <- deparse1(id)


        if(!startsWith(id, paste0(mod,"_"))) {
          msg <- paste(sprintf(
            "* In 'mod_%s_ui', a call to `%s` is using '%s' as an `id`, which is not prefixed by '%s'",
            mod_name, fun_nm, id, paste0(mod,"_")))
          message(msg)
        }

      }
      invisible(lapply(expr, recurse))
    }
    recurse(body(ui_fun))
  }
}

# check that ns() is never called in any other arg
check_ns_not_used_for_not_ids <- function(
  path = here::here("R")) {
  files <- list.files(path, pattern = "mod[_a-z]+\\.R", full.names = TRUE)
  for(file in files) {
    mod_name <- basename(file)
    mod_name <- substr(mod_name,5, nchar(mod_name)-2)
    env <- new.env()
    source(file, env)
    ui_fun_nm <- grep("_ui$", names(env), value = TRUE)
    ui_fun <- env[[ui_fun_nm]]

    recurse <- function(expr) {
      ## are we exploring a call
      if(!is.call(expr)) return(invisible(NULL))
      ## is the calling function a symbol that exists (i.e. defined or attached)?
      if(is.symbol(expr[[1]]) && exists(as.character(expr[[1]]))) {
        fun <- eval(expr[[1]])
        ## is it a closure ?
        if(!is.primitive(fun)) {
          named_args <- as.list(match.call(fun, expr))[-1]
          # assuming we don't have both id and inputId
          used_ns_lgl <- sapply(named_args, function(x)
            is.call(x) && list(x[[1]]) %in% c(quote(ns), quote(NS)))

          if(length(setdiff(names(named_args)[used_ns_lgl], c("id", "inputId", "outputId")))) {
            msg <- paste(sprintf(
              "* In 'mod_%s_ui', a call to `%s` is using `NS` or `ns` in arguments other than `id`, `inputId` or `outputId`",
              mod_name, as.character(expr[[1]])))
           message(msg)
          }
        }

      }
      invisible(lapply(expr, recurse))
    }

    recurse(body(ui_fun))
  }
}

# check that the module args of callModule are of the form "mod_MODULENAME_server",
# that there is an R file properly named for "MODULENAME", and that the id argument
# is prefixed by "MODULENAME_"
check_callmodule_args <- function(
  path = here::here("R")) {
  files <- list.files(path, pattern = "mod[_a-z]+\\.R", full.names = TRUE)
  for(file in files) {
    mod_name <- basename(file)
    mod_name <- substr(mod_name,5, nchar(mod_name)-2)
    env <- new.env()
    source(file, env)
    server_fun_nm <- grep("_server$", names(env), value = TRUE)
    server_fun <- env[[server_fun_nm]]

    recurse <- function(expr) {
      ## are we exploring a call
      if(!is.call(expr)) return(invisible(NULL))
      ## is the calling function a callModule?
      if(is.symbol(expr[[1]]) && as.character(expr[[1]]) %in% c("callModule", "moduleServer")) {
        ## fetch its id argument
        fun <- eval(expr[[1]])
        id_arg <- as.list(match.call(fun, expr))[["id"]]
        module_arg <- as.list(match.call(fun, expr))[["module"]]
        called_mod_fun_name <- deparse1(module_arg)
        # check that module argument has a proper name
        if(!startsWith(called_mod_fun_name, "mod_") || !endsWith(called_mod_fun_name, "_server")) {
          msg <- paste(sprintf(
            "* In 'mod_%s_server', a call to `%s` provides a module argument '%s' which doesn't not respect the 'mod_MODULENAME_server' convention",
            mod_name, as.character(expr[[1]]), called_mod_fun_name))
         message(msg)
          return(invisible(NULL))
        }
        called_module_name <- gsub("^mod_(.*)_server$", "\\1", called_mod_fun_name)

        # check if file exists
        if(!file.exists(file.path(path, paste0("mod_", called_module_name, ".R")))) {
          msg <- paste(sprintf(
            "* In 'mod_%s_server', a call to `%s` has a module argument `%s` but there is no file named '%s'",
            mod_name, as.character(expr[[1]]), called_mod_fun_name, paste0("mod_", called_module_name, ".R")))
         message(msg)
          return(invisible(NULL))
        }

        # check that the id and module argument are compatible
        if(is.character(id_arg) && !startsWith(id_arg, paste0(called_module_name, "_"))) {
          msg <- paste(sprintf(
            "* In 'mod_%s_server', a call to `%s` has a module argument `%s` and an `id` argument '%s' that is not prefixed by the module name '%s'",
            mod_name, as.character(expr[[1]]), called_mod_fun_name, id_arg, called_module_name))
         message(msg)
          return(invisible(NULL))
        }
      }
      invisible(lapply(expr, recurse))
    }

    recurse(body(server_fun))
  }
}

# check that modules and module ids mentionned on both ui and server side are consistent
check_modules_called_on_both_sides <- function(
  path = here::here("R")) {
  files <- list.files(path, pattern = "mod[_a-z]+\\.R", full.names = TRUE)
  for(file in files) {
    mod_name <- basename(file)
    mod_name <- substr(mod_name,5, nchar(mod_name)-2)
    env <- new.env()
    source(file, env)
    server_fun_nm <- grep("_server$", names(env), value = TRUE)
    server_fun <- env[[server_fun_nm]]

    ui_fun_nm <- grep("_ui$", names(env), value = TRUE)
    ui_fun <- env[[ui_fun_nm]]

    recurse_server <- function(expr) {
      ## are we exploring a call
      if(!is.call(expr)) return(NULL)
      ## is the calling function a callModule?
      if(is.symbol(expr[[1]]) && as.character(expr[[1]]) %in% c("callModule", "moduleServer")) {
        fun <- eval(expr[[1]])
        module_arg <- deparse1(as.list(match.call(fun, expr))[["module"]])
        return(module_arg)
      }
      unlist(lapply(expr, recurse_server))
    }

    server_modules <- recurse_server(body(server_fun))
    server_modules <- substr(server_modules, 5, nchar(server_modules) - 7)

    recurse_ui <- function(expr) {
      ## are we exploring a call
      if(!is.call(expr)) return(NULL)
      ## is the calling function a callModule?
      if(is.symbol(expr[[1]]) && grepl("^mod_.*_ui$", as.character(expr[[1]]))) {
        return(as.character(expr[[1]]))
      }
      unlist(lapply(expr, recurse_ui))
    }

    ui_modules <- recurse_ui(body(ui_fun))
    ui_modules <- substr(ui_modules, 5, nchar(ui_modules) - 3)

    for(mod in setdiff(server_modules, ui_modules)) {
      msg <- paste(sprintf(
        "* In 'mod_%s_server' we find calls to `mod_%s_server` but we don't find `mod_%s_ui` in `mod_%s_ui`'",
        mod_name, mod, mod, mod_name))
      message(msg)
    }

    for(mod in setdiff(ui_modules, server_modules)) {
      msg <- paste(sprintf(
        "* In 'mod_%s_ui' we find calls to `mod_%s_ui` but we don't find `mod_%s_server` in `mod_%s_server`'",
        mod_name, mod, mod, mod_name))
      message(msg)
    }

    recurse_server <- function(expr) {
      ## are we exploring a call
      if(!is.call(expr)) return(NULL)
      ## is the calling function a callModule?
      if(is.symbol(expr[[1]]) && as.character(expr[[1]]) %in% c("callModule", "moduleServer")) {
        ## fetch its id argument
        fun <- eval(expr[[1]])
        id_arg <- as.list(match.call(fun, expr))[["id"]]
        return(id_arg)
      }
      unlist(lapply(expr, recurse_server))
    }

    server_ids <- recurse_server(body(server_fun))

    recurse_ui <- function(expr) {
      ## are we exploring a call
      if(!is.call(expr)) return(NULL)
      ## is the calling function a callModule?
      if(is.symbol(expr[[1]]) && grepl("^mod_.*_ui$", as.character(expr[[1]]))) {
        id_arg <- expr[[2]]
        if(!is.call(id_arg) ||
           !is.symbol(id_arg[[1]]) ||
           !as.character(id_arg[[1]]) %in% c("ns","NS")) return(NULL)
        ns_fun_nm <- as.character(id_arg[[1]])
        if(ns_fun_nm == "ns") {
          return(as.character(id_arg[[2]]))
        } else {
          return(as.character(id_arg[[3]]))
        }
      }
      unlist(lapply(expr, recurse_ui))
    }
    ui_ids <- recurse_ui(body(ui_fun))

    for(mod in setdiff(server_ids, ui_ids)) {
      msg <- paste(sprintf(
        "* In 'mod_%s_server' we find the module id '%s' but we don't find it in 'mod_%s_ui'",
        mod_name, mod, mod_name))
      message(msg)
    }

    for(mod in setdiff(ui_ids, server_ids)) {
      msg <- paste(sprintf(
        "* In 'mod_%s_ui' we find the module id '%s' but we don't find it in 'mod_%s_server'",
        mod_name, mod, mod_name))
      message(msg)
    }
  }
}

#' check shiny
#'
#' This is designed with the  {golem} framework in mind, in that case the function
#' can be run without arguments in the relevant project. For optimal results attach
#' all packages used by the app and load all defined functions, as shinycheck doesn't
#' comment on functions that don't exist in the workspace.
#'
#' @param path path which stores the scripts
#'
#' @export
#' @examples
check_shiny <- function(path = here::here("R")) {
  message(
    "-----------------------------------------------------------------------\n",
    "Check that all module scripts contain exactly 2 functions named ",
    "appropriately")
  check_file_and_function_names(path)

  message(
    "-----------------------------------------------------------------------\n",
    "Check that all module ui functions use ns() or NS() on argument named ",
    "id/inputId/outputId")
  check_ns_used_for_ids(path)

  message(
    "-----------------------------------------------------------------------\n",
    "Check that in ui, module ui functions, named `mod_MODULE_ui` refer to ",
    "modules which exist and ids fed to them are prefixed with \"MODULE_\"")
  check_ui_funs_use_well_formatted_ids(path)

  message(
    "-----------------------------------------------------------------------\n",
    "Check that ns() and NS() are never called in an argument that isn't ",
    "id/inputId/outputId")
  check_ns_not_used_for_not_ids(path)

  message(
    "-----------------------------------------------------------------------\n",
    "Check that the module args of callModule are of the form ",
    '"mod_MODULENAME_server", that there is an R file properly named for ',
    '"MODULENAME", and that the id argument is prefixed by "MODULENAME_"')
  check_callmodule_args(path)

  message(
    "-----------------------------------------------------------------------\n",
    "Check that modules and module ids mentionned on both ui and server side are consistent")
  check_modules_called_on_both_sides(path)
}


# to do

# test and refine, current implementation is very rough
# check that all inputs are used
# check that all outputs are used
# check mymoduleServer functions to comply with RStudio's new recommendations
