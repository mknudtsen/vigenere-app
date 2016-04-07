#ui.R
library(highcharter)

shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  
  titlePanel("De Vigenère Cipher"),
  
  fluidRow(
    
    column(3, 
      wellPanel(

        textInput(inputId = "enter", label = "Text Input", placeholder = "Enter cyphertext here...", value = initial_txt),
        
        radioButtons("ref", 
                     label = "Reference Distribution",
                     choices = list("Oxford dictionary", "English literature classics"),
                     selected = "Oxford dictionary"),
        
        sliderInput("KL",
                    "Keyword Length",
                    min = 1,
                    max = 10,
                    value = 3),
        
        uiOutput("letter_slider"),
        
        hr(),
        
        helpText(HTML("
                      <em><ol><h5>Building a Keyword</h5>
                      <li>Enter encrypted text above</li>
                      <li>Select a 'Keyword Length'</li>
                      <li>Adjust the 'Shift' slider to match bars (frequencies)</li>
                      <li>Repeat process for each 'Letter Position' (1:length(keyword))</li>
                      <li>Go to 'Decrypt Text' panel to test the created Keyword</li>
                      </ol></em>"))
 
      ) # end well panel
    ), # end column
    
  
    column(8,
      tabsetPanel(
        tabPanel("Keyword Analysis",
          fluidRow(
            column(6,
              # reactive keyword output
              h3("Keyword", textOutput("key"), align = "center")
            ),
            column(6,
              # decrypted text output
              h3("Sample Text", textOutput("sample"), align = "center")
            )
          ), # end info/text row
          
          fluidRow(
            highchartOutput("chart"),
            sliderInput("POS",
                        label = "Shift",
                        min = 0,
                        max = 25,
                        value = 0,
                        ticks = FALSE,
                        width = '100%')
          ) # end main plot/slider row      
        ), # end tabpanel
        
        tabPanel("Decrypt Text",
          fluidRow(
            # enter keyword text
            br(),
            column(4, 
              textInput(inputId = "key_input", label = NULL, placeholder = "Enter keyword here...")
            ),
            column(2,
              actionButton(inputId = "decrypt", label = "Decrypt", align = "left")
            )
          ), # end fluidRow
        
          fluidRow(
            column(12,
              verbatimTextOutput("decrypted")
            )
          )
        ) # end tabPanel

      ) # end tabsetpanel
    ) # end column        
  ) # end fluidRow
)) # end shinyUI(fluidPage(