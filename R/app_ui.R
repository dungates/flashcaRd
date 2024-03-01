#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  options(shiny.useragg = TRUE)

  thematic_shiny(font = "auto")

  shiny::tagList(
    ### Import Google Sign In JavaScript Requirements ###
    ### Enable to add authentication ###
    tags$head(
      # tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
      # tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      disconnectMessage(
        text = "Something went wrong! Try refreshing the page.",
        refresh = "Refresh",
        background = "#000000",
        colour = "#FFFFFF",
        refreshColour = "#2C97F5",
        overlayColour = "#000000",
        overlayOpacity = 0.5,
        width = 450,
        top = "center",
        size = 24,
        css = ""
      )
    ),
    page_navbar(
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
      title = tags$img(src = "quizzy_deck-removebg.png", height = "110", width = "120"),
      underline = TRUE,
      bg = "#a9d3e6",
      id = "inNavPage",
      ### Sidebar ----
      sidebar = bslib::sidebar(
        style = "display:block; overflow:scroll;",
        open = "desktop",
        title = tags$h3(tags$b("Set Flashcards")),
        card_body(
          min_height = "1000px",
          selectInput(
            inputId = "question1",
            label = h5("Is this a question?"),
            selected = "",
            choices = c("", "Yes", "No"),
            width = "100%"
          ),
          fileInput("fileUpload", "Choose a file to upload the data you want to see", accept = ".csv")
        )
      ),
      ### Main Page ###
      nav_panel(
        title = "Main Page",
        icon = icon("r-project"),
        align = "left",
        card(
          mod_gen_card_ui("gen_card_ui_1"),
          plotOutput("plot1") |>
            shinycssloaders::withSpinner(color = "#04abeb", type = 7)
        ),
        tableOutput("contents") |>
          shinycssloaders::withSpinner(color = "#04abeb", type = 7)
      ),
      nav_menu(
        title = "More",
        align = "right",
        nav_item(
          tags$a(
            shiny::icon("database"), "My Website",
            href = "https://duncangates.me",
            target = "_blank"
          )
        ),
        nav_item(
          tags$a(
            shiny::icon("info-circle"), "Tutorial",
            href = "https://www.youtube.com/watch?v=o0CEkx0r5yY",
            target = "_blank"
          )
        ),
        nav_item(
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
    shinyalert::useShinyalert(),
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
