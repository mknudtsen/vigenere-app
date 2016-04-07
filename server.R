library(plyr)
library("highcharter")
library(shiny)
source("helpers.R")
source("data/refs.R")


shinyServer(function(input, output, session) {
  
  v <- reactiveValues(key = "", encrypted = c(), decrypted = "")
  
  ############################################ ui outputs
  
  # slider input for letter position selection
  output$letter_slider <- renderUI({
    if (input$KL > 1) {
      sliderInput("LET", "Letter Position", min = 1, max = input$KL, value = 1, step = 1)
    }
  })

  # chart output built with highcharts (js)
  output$chart <- renderHighchart({
    if(!is.null(input$LET)) {
      reference_dist <- switch(input$ref, 
                               "English literature classics" = classics,
                               "Oxford dictionary" = oxford)

      txt <- input$enter
      position_shift <- input$POS
      key_length <- input$KL
      letter_position <- input$LET
      build_highchart(txt, reference_dist, key_length, position_shift, letter_position)
    }
  })
  
  # text output to display current key
  output$key <- renderText({
    v$key
  })
  
  # text output to display current sample
  output$sample <- renderText({
    update_sample()
  })

  
  ############################################ reactive functions
  
  # reactive to update the keyword based on current inputs
  keyword <- reactive({
    if(!is.null(input$LET) && input$KL >= input$LET) {
      
      key_length <- input$KL
      letter_position <- input$LET
      position_shift <- input$POS
      
      length(v$key) <- key_length
      v$key[is.na(v$key)] <- "-"
      v$key[[letter_position]] <- LETTERS[position_shift + 1]
      return(v$key)
      
    }
  })
  
  # reactive to update the sample text based on current inputs
  update_sample <- reactive({
    if(!is.null(input$LET) && input$KL >= input$LET) {
      
      key <- paste(keyword(), collapse = "")
      key_length <- input$KL
      letter_position <- input$LET
      # sample_text used as reference to update copy stored in reactiveValues
      sample_text <- substr(input$enter, 1, 20)
      sample_ints <- str2ints(sample_text)
      n <- length(sample_ints)
      # get sequence of indices that need to be updated/decrypted according to current key
      seq_idx <- seq(letter_position, n, key_length)
      updates <- sample_ints[seq_idx]
      
      key_ints <- str2ints(key)
      current_key <- rep(key_ints[letter_position], len = length(updates)) - 1
      edits <- mod1(updates + (-1)*current_key, length(LETTERS))
      v$encrypted[seq_idx] <- edits
      
      return(paste(LETTERS[v$encrypted], collapse = ""))
      
    }
  })
  
  ############################################ observers to run reactives and change state
  
  observeEvent(input$enter, {
    first_20 <- substr(input$enter, 1, 20)
    v$encrypted <- str2ints(first_20)
  })
  
  # control issues where keyword length slider input value < letter position slider input value
  observe({
    new_val <- input$KL
    updateSliderInput(session, "LET", min = 1, max = new_val, value = 1)
  })
  
  observeEvent(input$LET, {
    new_letter_pos <- input$LET
    key_letter <- v$key[[new_letter_pos]]
    val <- str2ints(key_letter)
    updateSliderInput(session, "POS", value = val - 1)
    keyword()
  })
  
  # output to show decrypted text up to 2000 characters -- ('decrypt text' panel)
  output$decrypted <- renderText({
    txt_sub <- substr(v$decrypted, 1, 2000)
    txt_sub
  })
  
  # observe action button to decrypt text -- ('decrypt text' panel)
  observeEvent(input$decrypt, {
    keyword <- isolate(input$key_input)
    text <- isolate(input$enter)
    decrypt <- vigen(input = text, key = keyword, decrypt = TRUE)
    v$decrypted <- decrypt
  })
  
})

