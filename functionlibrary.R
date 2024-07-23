# FUNCTIONS 

# add the following code line to the scripts that use one of these functions: 
# source("C:/Users/mueller_admin.ZPIDNB21/Documents/Desktop/Rprojects/scripts/functionlibrary.R", local = TRUE)


#### check item type
check_item_type <- function(tabpanel, i, bool_ex) {
  item <- NULL # Define item as NULL before assigning any value
  fieldwidth <- "100%" # used for width of items
  if (bool_ex == FALSE){
    example <- ""
  } else {
    example <- tabpanel$example[i]
  }
 # example <- "" #tabpanel$Example[i] 
  # 1. do different things for different item types 
  #------------------------------------------------
  switch(tabpanel$type[i],
         textareainput = {
           item <- textAreaInput(tabpanel$id[i], label = NULL, value = example, width = fieldwidth)
         }, #close textareainput
         textinput = {
           item <- textInput(tabpanel$id[i], label = NULL, value = example, width = fieldwidth)
         }, #close textinput
         checkbox = {
           initial_value <- ifelse(is.na(example) || example == "", FALSE, as.logical(example))
           item <- checkboxInput(tabpanel$id[i], label = tabpanel$options[i], value = initial_value, width = fieldwidth)
         },#close checkbox
         checkboxgroup = {
           c_options <- as.list(unlist(strsplit(as.character(tabpanel$options[i]), " tab ")))
           
           # Split the Example column by comma and trim whitespace
           initial_selection <- strsplit(as.character(example), ",")[[1]]
           initial_selection <- trimws(initial_selection)
           # Check if there are any invalid options
           valid_selection <- all(initial_selection %in% c_options)
           if (!valid_selection) {
             initial_selection <- NULL
           }
           
           item <- prettyCheckboxGroup(tabpanel$id[i], label = NULL, 
                                       selected = initial_selection,
                                       choices = c_options,
                                       icon = icon("square-check"),
                                       status = "primary",
                                       outline = TRUE,
                                       animation = "jelly")
         }, #close checkboxgroup
         radiobutton = {
           options <- as.list(unlist(strsplit(as.character(tabpanel$options[i]), " tab "))) # Split text string into a list of options
           initial_selection <- ifelse(example %in% options, example, character(0)) # Check if example, i.e. tabpanel$Example[i], is a valid option
           item <- prettyRadioButtons(tabpanel$id[i], label = NULL,
                                      choices = options,
                                      selected = initial_selection, 
                                      icon = icon("check"),
                                      bigger = TRUE,
                                      status = "primary",
                                      animation = "jelly")
         }, #close radiobutton
         none = {
           ps <- unlist(strsplit(as.character(tabpanel$descript[i]), "tab"))
           item <- lapply(ps, p)
           
           # items of type "none" don't get the option to display a description  
           templist <- 
             list(
               fluidRow(
                 box(
                   div(
                     class = "icon-paragraph",
                     tags$i(id = tabpanel$icon_ids[i], class = "fas fa-info-circle info_icon", style = "cursor: pointer;", `data-tooltip` = tabpanel$resources[i]),
                     #p(
                     strong(
                       tabpanel$head[i]
                     )#)
                   ),
                   width = 8 #set this to e.g. 3 to display the headings on the left of the items/input fields
                 ),
                 box(
                   item,
                   width = 8
                 ),
               )
             )
         }, #close none
         matrix2 = {
           m_options <- as.list(unlist(strsplit(as.character(tabpanel$options[i]), " tab ")))
           #item <- radioMatrixInput(inputId = tabpanel$id[i], rowIDs = c("analysis code", "experimental code",  "raw data", "processed data"),
           item <- radioMatrixInput(inputId = tabpanel$id[i], rowIDs = c("1", "2",  "3", "4"),
                                    rowLLabels = as.matrix(data.frame(object = c("analysis code", "experimental code", "raw data", "processed data"))),
                                    #rowLLabels = as.matrix(data.frame(l = c(":", ":", ":", ":"))),
                                    choices = m_options)
         }, #close matrix2
         stop("Unknown type found in row ", i) # If type is undefined, stop and print an error message
  ) #END: do different things for different item types
  
  # 2. add tooltip icon + title, optional description + item
  #---------------------------------------------------------
  if (tabpanel$type[i] != "none") { # do this except for items of type "none"  
    split_helptext <- unlist(strsplit(as.character(tabpanel$descript[i]), " tab ")) #use literally " tab " to split lines in excel sheet
    paragraphed_description <- lapply(split_helptext, p) # applies p() to the split units
    
    templist <- 
      list(
        fluidRow(
          box(
            div(
              class = "icon-paragraph",
              # if you don't want the info icons in your app, simply make the next line a comment
             # tags$i(id = tabpanel$icon_ids[i], class = "fas fa-info-circle info_icon", style = "cursor: pointer;", `data-tooltip` = tabpanel$resources[i]),
              strong(tabpanel$head[i])
            ),
            #style = "border: 1px solid black; padding-top: 1em;",
            style = "padding-top: 1em; padding-bottom: 1em;",
            width = 8 #set this to e.g. 3 to display the headings on the left of the items/input fields
          ),
          box(
            conditionalPanel(
              condition = "input.descript",
              helpText(paragraphed_description)),#helpText(tabpanel$descript[i])),
            item,
            #style = "border: 1px solid red;",
            width = 8 #this width determines the width of the items! 
          )
        )#, br()
      )
  } #END: add tooltip icon + title, optional description + item 
  #---- 
  
  # 3. check, if the item should be displayed depending on another item
  #--------------------------------------------------------------------
  if (tabpanel$dependancy[i] == TRUE) {  # yes, dependent item -> conditionalPanel!

    evalID <- tabpanel$dep_item[i] # id of item on which this item depends, z.B. Q1
    depValue <- tabpanel$dep_value[i] # if this option is selected, current item should be shown
    if (tabpanel$choice[i] == "mc"){ # use .include for multiple choice items 
      mylistitem <-
        list(
          conditionalPanel(
            #condition = (paste("input.", evalID, " == ", "'", depValue, "'", sep = "")), #paste("input.", evalID, sep = ""),
            condition = (paste("input.", evalID, ".includes(", "'", depValue, "')", sep = "")), #input.evalID.includes('depValue')
            templist))
    } else {
      mylistitem <-
        list(
          conditionalPanel(
            condition = (paste("input.", evalID, " == ", "'", depValue, "'", sep = "")), #paste("input.", evalID, sep = ""), 
            #condition = (paste("input.", evalID, ".includes(", "'", depValue, "')", sep = "")), #input.evalID.includes('depValue') 
            templist))
      } 
  }
  else {  # always display item
    mylistitem <- templist
  } #END: check, if the item should be displayed depending on another item
  #---- 
  
  return(mylistitem)
}
# item <- check_item_type(tabpanel, i)

#### create item list
items_sheet <- function(tabpanel){
  
  tabpanel$icon_ids <- with(tabpanel, paste("icon_", id, sep="")) # merge "icon_" with ID, e.g. "icon_T1"
  mylist <- list() # list that will store one complete item
  
  for (i in 1:nrow(tabpanel)) #loop through all items of a single sheet, i = number of items
  {
    item <- check_item_type(tabpanel, i, TRUE) # TRUE if you want examples to be included, FALSE if examples should be "" 
    mylist <- append(mylist, item) # list that contains one complete item  	
  } # close item-loop
  return(mylist)
}
# mylist <- items_sheet(tabpanel) # call function

#### check dependency to create cond column (for ques_ans)
# The actual condition is evaluated in the app as it depends on user input. 
check_cond <- function(row) {
  if (row["dependancy"]) {
    # evalID <- tabpanel$dep_item[i] # id of item on which this item depends, z.B. Q1
    # depValue <- tabpanel$dep_value[i] # if this option is selected, current item should be shown  
    return(paste(row["id"], row["dep_item"], row["dep_value"], sep = ", "))
  } else {
    return("static")
  }
}
#cond <- apply(tabpanel, 1, check_cond) # call function and apply to each row

#### copy template excel and update copy with user data
update_sheets_with_user_data <- function(sheets, params){
  #sheets <- excel_sheets(path = path) # contains list of sheet names
  modified_sheets <- vector("list", length = length(sheets)) # initialize blank vector
  
  for (s in seq_along(sheets)) {
    sheet <- read_excel(path, sheets[s])    # read single sheet
    
    for (i in seq_len(nrow(sheet))) {
      id <- sheet$id[i] # id of the item in row i
      if (length(params) >= 3 && id %in% names(params[[3]])) {
        val <- params[[3]][[id]]
      } else {
        val <- NULL
      }
      
      if (!is.null(val) && is.vector(val)) {
        # If the value is a vector, join the elements into a single string
        val <- paste(val, collapse = ", ")
      }
      
      sheet$example[i] <- ifelse(!is.null(val), val, NA)
    }
    
    modified_sheets[[s]] <- sheet # add user data
  }
  
  names(modified_sheets) <- sheets # set names for the updated sheets to the original names
  return(modified_sheets)
}
# modified_sheets <- update_sheets_with_user_data(temp_sheets, params) # usage

#### generate_prp_panel 
generate_prp_panel <- function(temp_sheets, temp_items){ 
  tabsetPanel(id = "prp",#paste0(chosen_template), 
              # "prp", #ACHTUNG! Anzahl der submenus wird hier manuell festgelegt
              tabPanel(temp_sheets[1],
                       uiOutput("contributor_ui"), 
                       actionButton("add_btn", "add contributor"),
                       actionButton("rm_btn", "remove contributor"),
                       br(),
                       temp_items[1],
                       #actionButton("previous1", "Previous page"),
                       actionButton("next1", "Next page"),
                       
              ),
              tabPanel(temp_sheets[2],
                       temp_items[2],
                       actionButton("previous2", "Previous page"),
                       actionButton("next2", "Next page"),
                       
              ),
              tabPanel(temp_sheets[3],
                       temp_items[3],
                       actionButton("previous3", "Previous page"),
                       actionButton("next3", "Next page"),
                       
              ),
              tabPanel(temp_sheets[4],
                       temp_items[4],
                       actionButton("previous4", "Previous page"),
                       actionButton("next4", "Next page"),
                       
              ),
              tabPanel(temp_sheets[5],
                       temp_items[5],
                       actionButton("previous5", "Previous page"),
                       actionButton("next5", "Next page"),
                       
              ),
              tabPanel(temp_sheets[6],
                       temp_items[6],
                       actionButton("previous6", "Previous page"),
                       actionButton("next6", "Next page"),
                       
              ),
              tabPanel(temp_sheets[7],
                       temp_items[7],
                       actionButton("previous7", "Previous page"),
                       actionButton("next7", "Next page"),
                       
              ),
              tabPanel(temp_sheets[8],
                       temp_items[8],
                       actionButton("previous8", "Previous page"),
                       #actionButton("next8", "Next"), #die letzte Seite kriegt keinen Next-Button,
                       
              )
  ) # end PRP QUANT panel
}
# output$prp_panel <- renderUI({generate_prp_panel(prp_sheets, prp_items)}) # usage

#### generate_scope_panel 
generate_scope_panel <- function(temp_sheets, temp_items){ 
  tabsetPanel(id = "scope_panel",#paste0(chosen_template), 
              # "prp", #ACHTUNG! Anzahl der submenus wird hier manuell festgelegt
              tabPanel(temp_sheets[1],
                       uiOutput("contributor_ui"), 
                       actionButton("add_btn", "add contributor"),
                       actionButton("rm_btn", "remove contributor"),
                       br(),
                       temp_items[1],
                       #actionButton("previous1", "Previous page"), #die erste Seite kriegt keinen Previous-Button,
                       actionButton("next1", "Next page"),
                       
              ),
              tabPanel(temp_sheets[2],
                       temp_items[2],
                       actionButton("previous2", "Previous page"),
                       actionButton("next2", "Next page"),
                       
              ),
              tabPanel(temp_sheets[3],
                       temp_items[3],
                       actionButton("previous3", "Previous page"),
                       actionButton("next3", "Next page"),
                       
              ),
              tabPanel(temp_sheets[4],
                       temp_items[4],
                       actionButton("previous4", "Previous page"),
                       actionButton("next4", "Next page"),
                       
              ),
              tabPanel(temp_sheets[5],
                       temp_items[5],
                       actionButton("previous5", "Previous page"),
                       actionButton("next5", "Next page"),
                       
              ),
              tabPanel(temp_sheets[6],
                       temp_items[6],
                       actionButton("previous6", "Previous page"),
                       #actionButton("next6", "Next page"), #die letzte Seite kriegt keinen Next-Button,
                       
              )
  ) # end SCOPE QUANT panel
}
# output$scope_panel <- renderUI({generate_scope_panel(temp_sheets, temp_items)}) # usage

#### generate_dev_panel 
generate_dev_panel <- function(temp_sheets, temp_items){ 
  tabsetPanel(id = "dev_panel",#paste0(chosen_template), 
              # "prp", #ACHTUNG! Anzahl der submenus wird hier manuell festgelegt
              tabPanel(temp_sheets[1],
                       uiOutput("contributor_ui"),
                       actionButton("add_btn", "add contributor"),
                       actionButton("rm_btn", "remove contributor"),
                       br(),
                       temp_items[1],
                       #actionButton("previous1", "Previous page"), #die erste Seite kriegt keinen Previous-Button,
                       actionButton("next1", "Next page"),
                       
              ),
              tabPanel(temp_sheets[2],
                       temp_items[2],
                       actionButton("previous2", "Previous page"),
                       actionButton("next2", "Next page"),
                       
              ),
              tabPanel(temp_sheets[3],
                       temp_items[3],
                       actionButton("previous3", "Previous page"),
                       actionButton("next3", "Next page"),
                       
              ),
              tabPanel(temp_sheets[4],
                       temp_items[4],
                       actionButton("previous4", "Previous page"),
                       actionButton("next4", "Next page"),
                       
              ),
              tabPanel(temp_sheets[5],
                       temp_items[5],
                       actionButton("previous5", "Previous page"),
                       actionButton("next5", "Next page"),
                       
              ),
              tabPanel(temp_sheets[6],
                       temp_items[6],
                       actionButton("previous6", "Previous page"),
                       #actionButton("next6", "Next page"), #die letzte Seite kriegt keinen Next-Button,
                       
              )
  ) # end DEV panel
}
# output$dev_panel <- renderUI({generate_dev_panel(temp_sheets, temp_items)}) # usage

#### generate params for report 
generate_params <- function(input, counter) {
  input_names <- names(input)
  
  ## 1. contributors and orcids
  # Use grep to get IDs for contributors and orcids
  contributor_ids <- grep("^contributor\\d+$", input_names, value = TRUE)
  orcid_ids <- grep("^orcid\\d+$", input_names, value = TRUE)
  
  # Filter the contributor and orcid IDs based on the current counter value
  max_contributors <- counter
  
  filtered_contributor_ids <- contributor_ids[1:max_contributors]
  filtered_orcid_ids <- orcid_ids[1:max_contributors]
  
  # Collect only the filtered contributors and orcids
  respauth <- sapply(filtered_contributor_ids, function(id) input[[id]])
  resporc <- sapply(filtered_orcid_ids, function(id) input[[id]])
  
  ## 2. find conditional items that are TRUE
  # Assuming 'temp_ques_ans' is available in the environment
  condition <- temp_ques_ans$cond
  print_items <- c()
  
  for (i in seq_along(condition)) {
    if (condition[i] != "static") {
      a <- as.list(unlist(strsplit(as.character(condition[i]), ", ")))
      dep_item <- input[[a[[2]]]]
      
      if (!is.null(dep_item) && all(dep_item == a[[3]])) {
        print_items <- c(print_items, i)
      }
    } else {
      print_items <- c(print_items, i)
    }
  }
  
  ## 3a. short version includes only conditional items that are TRUE
  ID <- temp_ques_ans$ID[print_items]
  ques <- temp_ques_ans$ques[print_items]
  metadata <- temp_ques_ans$meta[print_items]
  
  ## 3b. long version includes all items
  ID_l <- temp_ques_ans$ID
  ques_l <- temp_ques_ans$ques
  metadata_l <- temp_ques_ans$meta
  
  ans <- setNames(lapply(ID, function(ID) input[[ID]]), ID)
  ans_l <- setNames(lapply(ID_l, function(ID_l) input[[ID_l]]), ID_l)
  
  params_s <- list(authors = respauth, orcid = resporc, answers = ans, questions = ques, metadata = metadata)
  params_long <- list(authors = respauth, orcid = resporc, answers = ans_l, questions = ques_l, metadata = metadata_l)
  
  params <- list(params_s = params_s, params_long = params_long)
 # saveRDS(params_s, "params_s.rds")
  return(params)
}

#### generate xml document 
generate_xml <- function(params){
  # Create an XML document
  xmldoc <- xml_new_root("metadata")
  
  # Loop through each row of your data
  for (i in seq_along(params$answers)) {
    
    # Get the metadata category for this row
    metadata_category <- params$metadata[i]
    
    # Skip the row if the metadata category is "skip" or NA
    if (is.na(metadata_category) || metadata_category == "skip") {
      next
    }
    
    
    # Get the metadata categories for this row
    metadata_components <- strsplit(params$metadata[i], ".", fixed = TRUE)
    
    
    # Create a new response node for this row
    response_node <- xml_add_child(xmldoc, "response")
    
    # Add the response value as a child node of the response node
    xml_add_child(response_node, "value", params$answers[i])
    
    # Loop through each metadata component for this row
    for (component in metadata_components) {
      # Add metadata elements to the XML document
      parent_node <- response_node  # Set the starting point as the response node for each metadata entry
      for (sub_component in component) {
        node <- xml_add_child(parent_node, "metadata")  # Add a new child node with the name "metadata"
        xml_set_attr(node, "name", sub_component)  # Set an attribute "name" with the value of the current metadata component
        parent_node <- node  # Move to the newly added child node for the next iteration
      }
    }
  }
  return(xmldoc)
}
#generate_xml(params)

#### create fixed name with date and save locally on user's computer
# This is an alternative version to creating an identifiable spreadsheetname and usercode to access file on drive
create_statename <- function(){ # first contributor can be found in params[[1]][[1]]
  
  # date and time of submission
  current_time <- Sys.time() # get the current date and time
  date_time <- format(current_time, "%m-%d-%Y-%H:%M:%S") # add formatting
  only_date <- format(current_time, "%m-%d-%Y") # add formatting
  
  # create spreadsheetname and code
  statename <- date_time #paste0("state-on-", date_time) # e.g. "intermediate-state-on-05-16-2024-10:12:40"
  
  return(statename)
}

###################### create an identifiable spreadsheetname and usercode to retrieve it
# This is an alternative version to creating a fixed filename with date and saving it locally on user's computer
create_spreadsheetname <- function(first_contributor){ # first contributor can be found in params[[1]][[1]]
  # name of first contributor
  codename <- unlist(strsplit(first_contributor, split = ", ")) # split "Surname, given names(s)" into two strings at ", "
  surname <- codename[[1]]
  givenname <- codename[[2]]
  # use only the first two letters of the given name (thereby ignore middlenames and such)
  only_2_letters <- unlist(strsplit(givenname, "")) # split string into single characters
  givenname_short <- paste(only_2_letters[1:2], collapse="") # merge the two characters back into string
  
  # date and time of submission
  current_time <- Sys.time() # get the current date and time
  date_time <- format(current_time, "%m-%d-%Y-%H:%M:%S") # add formatting
  only_date <- format(current_time, "%m-%d-%Y") # add formatting
  
  # create spreadsheetname and code
  spreadsheetname <- paste0(givenname_short, "-", surname, "-", date_time) # e.g. "SteMueller_05-16-2024-10:12:40"
  #usercode <- paste0(givenname_short, surname, "_", only_date) # code that the user has to enter to retrieve their file, e.g. "SteMueller_05-16-2024"
  
  return(spreadsheetname)
}
# spreadsheetname <- create_spreadsheetname(params[[1]][[1]]) # usage
