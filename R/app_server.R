# Define server logic
server <- function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  .data <- golem::get_golem_options(".data")
  path <- golem::get_golem_options("path")
  question <- golem::get_golem_options("question")
  answer <- golem::get_golem_options("answer")
  clean <- golem::get_golem_options("clean")
  
  question <- dplyr::enquo(question)
  answer <- dplyr::enquo(answer)
  
  rv <- shiny::reactiveValues(
    answer_visible = FALSE,
    question_visible = TRUE,
    card_keep = numeric(0),
    card_know = numeric(0),
    dat = NULL
  )
  
  observe({
    req(is.null(rv$dat))
    if (!is.null(.data)){
      rv$dat <- valid_flash_cards(.data, question = !!question, 
                                  answer = !!answer, clean = clean)
    } else if (!is.null(path)){
      rv$dat <- read_flash_cards(path, question = !!question, 
                                 answer = !!answer, clean = clean)
    } else {
      isolate(shiny::callModule(mod_load_data_server, "load_data_ui_1", 
                                rv = rv))
    }
  })
  
  
  
  shiny::observe({
    rv$dat
    req(rv$dat)
    shinyjs::show("main-content")
  })
  
  
  observeEvent(input$change_dataset, {
    shinyjs::hide("main-content")
    rv$dat <- NULL
    isolate(shiny::callModule(mod_load_data_server, "load_data_ui_1", 
                              rv = rv))
    # shinyjs::show("main-content")
  })
  
  shiny::observe({
    req(rv$dat)
    
    n_cards <- rv$dat |>
      dplyr::distinct(!!question) |>
      nrow()
    
    rv$n_cards <- n_cards
    rv$card_idx <- sample(1:rv$n_cards, rv$n_cards)
    rv$n <- 1
  })
  
  callModule(mod_gen_card_server, "gen_card_ui_1", 
             rv = rv, question = !!question, 
             answer = !!answer)
  
  output$plot1 <- renderPlot({
    
    ggplot(data = data.frame(x = 0, y = 0),
           aes(x = x, y = y)) +
      geom_text(aes(label = input$question1), size = 20) +
      labs(title = "Is this a question?") +
      theme_minimal()
    
  })
  
  output$contents <- renderTable({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    read.csv(file$datapath, header = input$header)
  })
  
}
