#' Run the Shiny Application
#' @rdname flash_cards
#' @param .data a data.frame containing columns `question` and `answer`
#' @param path the path to a `.xlsx`, `.csv`, `.rds` file containing a 
#' data.frame with columns `question` and `answer`
#' @param type the type of flash_cards to run. Can be either "shiny" or "local"
#' @param width the width of the rstudio modal dialog box (in pixels)
#' @param height the height of the rstudio modal dialog box (in pixels)
#' @param envir the environment to look for flash card decks when using 
#' type = "local"; Defaults to .GlobalEnv
#' @param question the column name for the questions in the dataset
#' @param answer the column name for the answers in the dataset
#' @param clean logical; whether or not to clean the column names
#' 
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
flash_cards <- function(.data = NULL, path = NULL, type = "shiny", 
                        width = 1000, height = 800, 
                        question = question, answer = answer, 
                        clean = TRUE) {
  type <- match.arg(type, c("shiny", "local"))
  
  question <- dplyr::enquo(question)
  answer <- dplyr::enquo(answer)
  
  if (type == "shiny"){
    flash_shiny(.data = .data, path = path, question = !!question, 
                answer = !!answer, clean = clean)
  } else if (type == "local"){
    flash_local(.data = .data, path = path, width = width, height = height, 
                question = !!question, answer = !!answer, clean = clean)
  }
}

#' @rdname flash_cards
#' @export
flash_shiny <- function(.data = NULL, path = NULL, 
                        question = question, answer = answer, 
                        clean = TRUE){
  question <- dplyr::enquo(question)
  answer <- dplyr::enquo(answer)
  
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server
    ), 
    golem_opts = list(.data = .data, path = path, question = question, 
                      answer = answer, clean = clean)
  )
}

#' @rdname flash_cards
#' @export
flash_local <- function(.data = NULL, path = NULL, width = 1000, height = 800,
                        question = question, answer = answer, clean = TRUE){
  viewer = shiny::dialogViewer("flashcaRd", width = width, height = height)
  
  question <- dplyr::enquo(question)
  answer <- dplyr::enquo(answer)
  
  app <- shinyApp(
    ui = addin_ui, 
    server = addin_server
  )
  
  app$appOptions$golem_options <- list(.data = .data, path = path,
                                       question = question, 
                                       answer = answer, 
                                       clean = clean)
  
  shiny::runGadget(app = app, viewer = viewer)
}

#' @rdname flash_cards
#' @export
flash_addin_envir <- function(envir = .GlobalEnv){
  user_dat <- get_valid_decks(envir = envir)
  
  q <- dplyr::sym("question")
  a <- dplyr::sym("answer")
  
  flash_local(.data = user_dat, question = !!q, answer = !!a)
}

flash_addin_envir_custom <- function(envir = .GlobalEnv){
  question <- readline("Enter question column name: ")
  answer <- readline("Enter answer column name: ")
  clean <- readline("Should column names be cleaned (Y/N): ")
  question <- dplyr::sym(question)
  answer <- dplyr::sym(answer)
  clean <- ifelse(clean == "Y", TRUE, FALSE)
  
  user_dat <- get_valid_decks(envir = envir, question = question, 
                              answer = answer)
  
  flash_local(.data = user_dat, question = !!question, answer = !!answer, 
              clean = clean)
}

#' @rdname flash_cards
#' @export
flash_addin_file <- function(){
  usr_file <- rstudioapi::selectFile()
  
  flash_local(path = usr_file)
}

#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts. 
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options 
run_app <- function(
    onStart = NULL,
    options = list(), 
    enableBookmarking = NULL,
    uiPattern = "/",
    .data = NULL, path = NULL, 
    question = question, answer = answer, 
    clean = TRUE,
    ...
) {
  
  question <- dplyr::enquo(question)
  answer <- dplyr::enquo(answer)
  
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options, 
      enableBookmarking = enableBookmarking, 
      uiPattern = uiPattern
    ), 
    golem_opts = list(.data = .data, path = path, question = question, 
                      answer = answer, clean = clean)
  )
}