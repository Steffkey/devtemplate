library(dplyr)
library(knitr)
library(pandoc)
library(readxl)
library(rmarkdown)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyRadioMatrix)
library(shinyWidgets)
library(tinytex)
library(rsconnect)

#### PREPARATIONS '#############################################################

rm(list = ls()) # clear environment
load("dev_d1.RData") # Load the processed data
path = "deviation_template_1.xlsx" # set path to template excel
source("functionlibrary.R", local = TRUE) # get functions

#### UI ########################################################################

ui <- fluidPage(
  
  #### link external scripts (css for styling, js for functionalities)  #####
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "shinystyles.css"), # contains styling, e.g. for header
    tags$script(type="text/javascript", src = "shiny_js_functions.js")
    #tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
  ),
  #### END link external scripts (css for styling, js for functionalities)
  
  #### header ####
  fluidRow(
    column(width = 3),
    column(width = 9, class = "logobar",
           div(img(src = "ZPID_Logo_Redesign_2023_RZ_english.svg", style = "height: 5em; width: auto; padding:1em; padding-left:0;", class = "logo"))
    )
  ),
  
  fluidRow(
    column(width = 3, class = "sidecolumn"), 
    column(width = 9,
           fluidRow(
             div(class = "headerbar", p(HTML("<strong>Pre</strong>Reg: Deviation Template")))
           ),
           fluidRow(
             div(class = "whitebar")
           ))
  ),
  
  #### END header
  #### select Instruction/Template ####
  fluidRow(
    column(width = 3, class = "column",
           selectInput("selecttemplate", "",
                       choices = list('Instructions'= 0, 'Template'='dev'), multiple = FALSE, selected = '0')
    )),
  #### END select Instruction/Template 
  
  #### Instruction page / landing page ####
  fluidRow(
    br(),
    br(),
    conditionalPanel(
      condition = "input.selecttemplate != 'dev'",
      column("", width = 3),
      column(width = 9,
             p("This template is designed to assist you in documenting and reporting deviations from your preregistration."),
             br(),
             p("Please note that deviations from preregistered plans are quite normal. For example, they may occur due to discovered mistakes, newly acquired knowledge, or unforeseen circumstances. In such cases, deviating from the preregistration can help improve the quality of the study. However, it is crucial to report all deviations transparently so that the study results can be accurately interpreted."),
             p("Please use the listed items to disclose and justify all deviations transparently."),
             p("Step 1: Complete the template in this App."),
             p("Step 2: Export your study protocol as PDF."),
             p("Step 3: Submit your PDF to PsychArchives (https://pasa.psycharchives.org/). Select the collection \"other\" and link it to your preregistration."),
             br(),
             p(strong("IMPORTANT:"), "Save your progress once in a while by clicking the ", strong("download button"), "that you will see on the left."),
             p("In addition to the file format you selected, a ", strong(".rds file"), " will be downloaded. This is the most important file because it can be uploaded again later."),
             br(),
             p("To begin, select “Template” in the drop-down menu on the upper left."),
             br()
      )
    ),
  ),
  #### END Instruction page / landing page
  
  #### Action & Button panel ####
  
  fluidRow(
    class = "conditional-panel",
      conditionalPanel(
        condition = "input.selecttemplate == 'dev'",
          fluidRow(
            #  class = "row",
            column(width = 3, class = "column",
                   materialSwitch("descript", label = "Show descriptions", value = TRUE, status = "primary", right = FALSE),
                   #tags$p(actionLink("browse", "Browse examples")),
                   #tags$br(),
                   
                   div(class = "flex-container",
                       radioButtons('format', 'Export as:', c('PDF', 'XML', 'Word'), inline = TRUE),
                       downloadButton("report", label = "", class = "download-btn", onclick = "document.getElementById('state').click()"),
                       downloadButton("state", label = "",  style = "opacity: 0; position: fixed; pointer-events:none;")
                   ),
                   
                   br(),
                   
                   div(class = "flex-container",
                       fileInput("uploadFile", "Upload Previous State")
                   )
            ),
            
            column(width = 9, class = "column",
                   uiOutput("dev_panel")
            )
          )
        )
  )
  #### END Action & Button panel
)

#### SERVER ####################################################################

server <- function(input, output, session) {
  # #### browse link ####  
  # # The following code causes that the a PsychArchives Search with prespecified criteria is opened in a new browser window
  # # when the user clicks on the button "browse"; search criteria and resulting PsychArchives link: 
  # # dc.type:preregistration AND zpid.tags.visible:PRP-QUANT AND dc.rights: openAccess
  # # "https://www.psycharchives.org/en/browse/?fq=dcType_keyword%3A%28%22preregistration%22%29+AND+dcRights_keyword%3A%28%22openAccess%22%29&q=dc.type%3Apreregistration+AND+zpid.tags.visible%3APRP-QUANT+AND+dc.rights%3A+openAccess"
  # observeEvent(input$browse, {
  #   session$sendCustomMessage("openNewWindow", list(url = "https://www.psycharchives.org/en/browse/?fq=dcType_keyword%3A%28%22preregistration%22%29+AND+dcRights_keyword%3A%28%22openAccess%22%29&q=dc.type%3Apreregistration+AND+zpid.tags.visible%3APRP-QUANT+AND+dc.rights%3A+openAccess"))
  # })
  # ##### END browse link 
  
  #### add/remove contributors ###########################    
  # Track the number of input boxes to render
  counter <- reactiveVal(1) # Counter for contributors
  
  # Track the previous data and user input
  prevData <- reactiveVal(list(contributor = character(0), orcid = character(0)))
  
  # Function to generate new text input fields; usage: adding/removing contributors
  generate_text_inputs <- function() {
    n <- counter()
    textInputs <- lapply(seq_len(n), function(i) {
      inputPanel(
        textInput(inputId = paste0("contributor", i),
                  label = paste0("Contributor ", i),
                  value = isolate(ifelse(i <= length(prevData()$contributor), prevData()$contributor[i], "Surname, given name(s)"))),
        textInput(inputId = paste0("orcid", i),
                  label = paste0("ORCID ", i),
                  value = isolate(ifelse(i <= length(prevData()$orcid), prevData()$orcid[i], "e.g., 0000-1234-5678-9101")))
      )
    })
    return(textInputs)
  }
  
  # Render the UI for text input fields
  output$contributor_ui <- renderUI({
    generate_text_inputs()
  })
  
  # Render the initial mainPanel
  output$dev_panel <- renderUI({
    generate_dev_panel(dev_sheets, dev_items)
  })
  
  # Function to handle uploaded file
  observeEvent(input$uploadFile, {
    inFile <- input$uploadFile
    
    if (!is.null(inFile)) { #Datei darf nicht leer sein
      data <- readRDS(inFile$datapath) # hier dann über code googlesheet einlesen
      
      # Update the previous data with uploaded data
      prevData(list(contributor = data[[1]], orcid = data[[2]])) # die Daten stehen in sheet 1, spalte 1 und spalte 2
      
      # Determine the number of input fields based on uploaded data
      n <- length(data[[1]]) # das wäre dann die Anzahl der Zeilen
      
      # Update the counter reactive value
      counter(n)
      
      ########################################## fill textinputs
      sheets <- excel_sheets(path = path) #contains list of sheet names
      
      # get user data (stored in params)
      
      # update excel
      modified_sheets <- update_sheets_with_user_data(path, data) # data[[3]] = params[[3]]
      #save(modified_sheets, file = "show_modified_sheets3.RData")
      
      # set up list to store items of all sheets
      all_items <- list()
      
      # loop through modified sheets
      for (m in seq_along(sheets))
      {
        mylist <- items_sheet(modified_sheets[[m]])
        all_items <- append(all_items, list(mylist))          # list that contains items of one section
      }
      
      # overwrite or create dev_items with new data
      itemsname <- "modified_items"
      assign(itemsname, all_items)
      #save(modified_items, file = "new_modified_items.RData")
      ##########################################
      
      # Update the mainPanel with new data
      output$dev_panel <- renderUI({
        generate_dev_panel(dev_sheets, modified_items)
      })
      
    }
  })
  
  # Handle adding contributors
  observeEvent(input$add_btn, {
    counter(counter() + 1)
    update_prev_data()
  })
  
  # Handle removing contributors
  observeEvent(input$rm_btn, {
    if (counter() > 1) {
      counter(counter() - 1)
      update_prev_data()
    }
  })
  
  # Observe changes in text inputs
  observe({
    debounce_input <- reactive({
      input_names <- names(input)
      contributors <- grep("^contributor", input_names, value = TRUE)
      orcids <- grep("^orcid", input_names, value = TRUE)
      list(contributors = contributors, orcids = orcids)
    }) %>% debounce(500)  # Adjust debounce time as necessary
    
    observeEvent(debounce_input(), {
      input_names <- names(input)
      contributors <- grep("^contributor", input_names, value = TRUE)
      orcids <- grep("^orcid", input_names, value = TRUE)
      if (length(contributors) > 0 || length(orcids) > 0) {
        update_prev_data()
      }
    })
  })
  
  ## Function to update previous data; usage: adding/removing contributors, uploading 
  ## saved state, and keeping previous inputs
  update_prev_data <- function() {
    input_names <- names(input)
    contributors <- grep("^contributor", input_names, value = TRUE)
    orcids <- grep("^orcid", input_names, value = TRUE)
    prevData(list(contributor = sapply(contributors, function(x) input[[x]]),
                  orcid = sapply(orcids, function(x) input[[x]])))
  }  
  
  ##################### END add/remove contributors  
  
  #### navigate to next tabPanel ###########################
  # Define a reactive expression to track the current tab
  current_tab <- reactiveVal(1)
  
  # Observe the selected tab in the navlistPanel and update current_tab accordingly
  observeEvent(input$dev_panel, { # observe which tab is active
    #  print(input$dev_panel) # for debugging
    current_tab(match(input$dev_panel, dev_sheets)) # find out the index of the current tab
    #  print(current_tab()) # for debugging
  })
  
  # Set up observer for next button using lapply
  lapply(1:length(dev_sheets), function(i) {
    observeEvent(input[[paste0("next", i)]], {
      current_tab(current_tab() + 1) # increase index if button is clicked
      #  print(current_tab()) # for debugging
    })
  })
  
  # Set up observer for previous button using lapply
  lapply(1:length(dev_sheets), function(i) {
    observeEvent(input[[paste0("previous", i)]], {
      current_tab(current_tab() - 1) # decrease index if button is clicked
      #  print(current_tab()) # for debugging
    })
  })
  
  # Update the selected tab based on the current_tab value
  observe({
    updateTabsetPanel(session, "dev_panel", selected = dev_sheets[current_tab()]) # generalize for all templates and sheets
    session$sendCustomMessage(type = "scrollTop", message = list())
  })
  
  ##################### END navigate to next tabPanel 
  #### print report and save params ###########################
  output$report <- downloadHandler(
    filename = function() {
      paste('report', sep = '', switch(
        input$format, PDF = '.pdf', XML = 'metadata.xml', Word = '.docx'
      ))
    },
    content = function(file) {
      # Ensure counter is passed to generate_params
      params <- generate_params(input, counter()) # Pass counter() to the function
      
      if (input$format == 'XML') {
        xmldoc <- generate_xml(params)
        # Save the XML content to the file
        xml2::write_xml(xmldoc, file)
      } else {
        src <- normalizePath('report.Rmd')
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd)) # once the path is created in the previous function, setwd to this path
        
        file.copy(src, 'report.Rmd', overwrite = TRUE)
        
        out <- rmarkdown::render('report.Rmd',
                                 output_format = switch(
                                   input$format,
                                   PDF = pdf_document(
                                     latex_engine = "pdflatex",
                                     pandoc_args = c(
                                       "--pdf-engine-opt=-output-profile=pdfa-2",
                                       "--pdf-engine-opt=-dPDFSETTINGS=/prepress"
                                     )
                                   ), 
                                   HTML = html_document(), 
                                   Word = word_document()
                                 ),
                                 params = params,
                                 envir = new.env(parent = globalenv())
        )    
        file.rename(out, file)
      }
    }
  )
  
  # Second downloadHandler
  output$state <- downloadHandler(
    filename = function() {
      paste0(create_statename(), ".rds")
    },
    content = function(file) {
      params <- generate_params(input, counter())
      saveRDS(params, file = file)
    }
  )
  ##################### END print report and save params 
}
#### RUN APP ###################################################################

shinyApp(ui = ui, server = server)