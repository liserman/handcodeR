server <- function(input, output, session) {

  # Function for output generation
  gen_output <- function(data, values){

    # Generate final data.frame to be displayed as output
    final <- data.frame(id = data$id, texts = data$texts, kat1 = values$code1, kat2 = values$code2, kat3 = values$code3)

    # Reduce to size of original dataframe
    final <- final[,1:(ncol(data)-2)]

    # Take names from original dataframe
    names(final) <- names(data)[-c(2,3)]

    # Make sure all NA is saved as ""
    final[is.na(final)] <- ""

    # Return
    return(final)
  }

  # Initialize reactiveValues
  values <- shiny::reactiveValues(
    # Set counter to start
    counter = start_app,

    # Initialize coding as existing classification
    code1 = container[,1],
    code2 = container[,2],
    code3 = container[,3]
  )

  # Initialize radioButtons
  observe({
    shiny::updateRadioButtons(session, "code1", selected = values$code1[values$counter])
    shiny::updateRadioButtons(session, "code2", selected = values$code2[values$counter])
    shiny::updateRadioButtons(session, "code3", selected = values$code3[values$counter])
  })


  # Behaviour when previous is clicked
  shiny::observeEvent(input$previouspage, {

    # Save current coding
    values$code1[values$counter] <- input$code1
    try(values$code2[values$counter] <- input$code2, silent = TRUE)
    try(values$code3[values$counter] <- input$code3, silent = TRUE)

    # Update the counter value by substracting 1 but only if page is bigger 1
    if (values$counter > 1) {
      values$counter <- values$counter - 1
    }

    # Set the selected value of radioButtons to "" or already coded category
    shiny::updateRadioButtons(session, "code1", selected = ifelse(is.na(values$code1[values$counter]), "", values$code1[values$counter]))
    shiny::updateRadioButtons(session, "code2", selected = ifelse(is.na(values$code2[values$counter]), "", values$code2[values$counter]))
    shiny::updateRadioButtons(session, "code3", selected = ifelse(is.na(values$code3[values$counter]), "", values$code3[values$counter]))
  })

  # Behaviour when next is clicked
  shiny::observeEvent(input$nextpage, {

    # Save current coding
    values$code1[values$counter] <- input$code1
    try(values$code2[values$counter] <- input$code2, silent = TRUE)
    try(values$code3[values$counter] <- input$code3, silent = TRUE)

    # If max pages reached, save and exit
    if (values$counter == length(data_app$texts)) {

      # Print the coding for all text samples when the app is closed
      shiny::observeEvent(session$onSessionEnded, {

        final <- gen_output(data_app, values)

        # Write final to e
        e$output <- final

      }, once = TRUE)


      # Stop the shiny app
      shiny::stopApp()
    }

    # Update the counter value by adding 1
    values$counter <- values$counter + 1

    # Set the selected value of radioButtons to "" or already coded category
    shiny::updateRadioButtons(session, "code1", selected = ifelse(is.na(values$code1[values$counter]), "", values$code1[values$counter]))
    shiny::updateRadioButtons(session, "code2", selected = ifelse(is.na(values$code2[values$counter]), "", values$code2[values$counter]))
    shiny::updateRadioButtons(session, "code3", selected = ifelse(is.na(values$code3[values$counter]), "", values$code3[values$counter]))
  })

  # Update text displayed
  if(context_app){
    current_text <- reactive({
      paste0("<font color =\"#D3D3D3\">", data_app$before[values$counter], "</font> <b>", data_app$texts[values$counter], "</b> <font color =\"#D3D3D3\">", data_app$after[values$counter], "</font>")
  })} else {
    current_text <- reactive({
      data_app$texts[values$counter]
    })
  }

  output$statement <- renderText({
    current_text()
  })


  # Behaviour when save is clicked
  shiny::observeEvent(input$save, {

    # Save current coding
    values$code1[values$counter] <- input$code1
    try(values$code2[values$counter] <- input$code2, silent = TRUE)
    try(values$code3[values$counter] <- input$code3, silent = TRUE)

    # Print the coding for all text samples when the app is closed
    shiny::observeEvent(session$onSessionEnded, {

      final <- gen_output(data_app, values)

      # Write final to e
      e$output <- final
    }, once = TRUE)

    # Stop the shiny app
    shiny::stopApp()
  })

}
