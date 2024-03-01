#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  # options(shiny.useragg = TRUE)

  # thematic_shiny(font = "auto")

  shiny::tagList(
    golem_add_external_resources(),
    bslib::page_navbar(
      ### Theme ----
      theme = bslib::bs_theme(
        # bg = "#FFFFFF", fg = "#46494f",
        primary = "#04abeb",
        secondary = "#024762",
        version = 5,
        row_sizes = 5,
        base_font = bslib::font_google("Open Sans"),
        heading_font = bslib::font_google("Fira Sans")
      ),
      window_title = "QuizzyDeck",
      title = tags$img(src = "img/quizzy_deck.webp", height = "110", width = "120"),
      underline = TRUE,
      bg = "#a9d3e6",
      id = "inNavPage",
      ### Sidebar ----
      sidebar = bslib::sidebar(
        style = "display:block; overflow:scroll;",
        open = "desktop",
        title = tags$h3(tags$b("Set Flashcards")),
        bslib::card_body(
          min_height = "1000px",
          shiny::selectInput(
            inputId = "question1",
            label = h5("Is this a question?"),
            selected = "",
            choices = c("", "Yes", "No"),
            width = "100%"
          ),
          fileInput("fileUpload", "Choose a file to upload your flashcards", accept = ".csv"),
          textInput(label = "Provide a link that you would like summarised as flashcards",
                    inputId = "descriptionFlashcard"),
          actionButton(inputId = "actionFlashcard", label = "Make Cards!")
        )
      ),
      ### Main Page ###
      bslib::nav_panel(
        title = "Main Page",
        icon = icon("r-project"),
        align = "left",
        bslib::card(
          mod_gen_card_ui("gen_card_ui_1"),
          full_screen = TRUE
        )
      ),
      bslib::nav_menu(
        title = "More",
        align = "right",
        bslib::nav_item(
          tags$a(
            shiny::icon("database"), "My Website",
            href = "https://duncangates.me",
            target = "_blank"
          )
        ),
        bslib::nav_item(
          tags$a(
            shiny::icon("info-circle"), "Tutorial",
            href = "https://www.youtube.com/watch?v=o0CEkx0r5yY",
            target = "_blank"
          )
        ),
        bslib::nav_item(
          tags$a(
            shiny::icon("commenting"), "Give Duncan Feedback",
            href = "https://teachinglab.iad1.qualtrics.com/jfe/form/SV_0Bqu3SUziXrmvlA?Source=template_dashboard",
            target = "_blank"
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(addin = FALSE) {
  golem::add_resource_path(
    "www", app_sys("app/www")
  )

  if (addin) {
    style <- tags$link(rel = "stylesheet", type = "text/css", href = "www/styles-addin.css")
  } else {
    style <- tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css")
  }

  tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "flashcaRd"
    ),
    shinyjs::useShinyjs(),
    shiny::tags$link(href = "http://fonts.googleapis.com/css?family=Fira+Sans", rel = "stylesheet", type = "text/css"),
    shiny::tags$link(href = "https://fonts.googleapis.com/css?family=Open+Sans&display=swap", rel = "stylesheet"),
    style,
    tags$script(src = "www/button-click.js")
  )
}


addin_ui <- function() {
  shiny::tagList(
    golem_add_external_resources(addin = TRUE),
    miniUI::miniPage(
      miniUI::gadgetTitleBar("flashcaRd"),
      mod_gen_card_ui("gen_card_ui_1", addin = TRUE)
    )
  )
}
