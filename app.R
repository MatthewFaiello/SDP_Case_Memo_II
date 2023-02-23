library(shiny)
library(shinyWidgets)
library(shinymanager)
library(shinyalert)
library(tidyverse)
library(plotly)
library(shinythemes)
library(knitr)
library(kableExtra)
library(htmltools)
library(DT)
library(viridis)
library(scales)

###############################################################################
#-------------------------------- load files ---------------------------------#
viz =
  read_csv("viz.csv")

###############################################################################
#--------------------- factor order functions --------------------------------#
reorder_within <- 
  function(x, by, within, fun = median, sep = "___", ...) {
    new_x <- paste(x, within, sep = sep)
    stats::reorder(new_x, by, FUN = fun)
  }

scale_x_reordered <- 
  function(..., sep = "___") {
    reg <- paste0(sep, ".+$")
    ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
  }

scale_y_reordered <- 
  function(..., sep = "___") {
    reg <- paste0(sep, ".+$")
    ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
  }

#------------------ string wrapping function ---------------------------------#
wrap.it <- 
  function(x, len) { 
    sapply(x, function(y) paste(strwrap(y, len), collapse = "\n"),
           USE.NAMES = FALSE)}

wrap.labels <- 
  function(x, len) {
    if (is.list(x)) {
      lapply(x, wrap.it, len) } 
    else {wrap.it(x, len)}}

#--------- function factory for getting integer y-axis values ----------------#
integer_breaks <- 
  function(n = 5, ...) {
    fxn <- 
      function(x) {
        breaks <- 
          floor(pretty(x, n, ...))
        names(breaks) <- 
          attr(breaks, "labels")
        breaks}
    return(fxn)}

#------------------ idle time and login creds --------------------------------#
inactivity <- 
  "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


# data.frame with credentials info
credentials <- 
  data.frame(user = "app", password = "SDP", stringsAsFactors = F)


############################### shiny app #####################################
#--------------------------- ui ----------------------------------------------#
ui <- 
  secure_app(head_auth = tags$script(inactivity), theme = shinytheme("yeti"),
             fab_position = "top-right",
             background = "url('https://sdp.cepr.harvard.edu/sites/hwpi.harvard.edu/files/styles/os_files_xxlarge/public/sdp/files/sdp_logo.png?m=1515605022&itok=svSqwQf_')  no-repeat bottom fixed;",
             fluidPage(tags$style(type = "text/css",
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: visible; content: 'Loading...the data is coming :) or the chosen filters produced a dataset with no applicable student data'; }"),
                       tags$head(
                         tags$script(
                           HTML("
            $(document).ready(function(){
              // Mark columns we want to toggle
              $('body').find('div [class=col-sm-4]').addClass('sidebarPanel');
              $('body').find('div [class=col-sm-8]').addClass('mainPanel');
            })


            Shiny.addCustomMessageHandler ('resize',function (message) {
              $('.sidebarPanel').toggle();
              $('.mainPanel').toggleClass('col-sm-8 col-sm-12');
              $(window).trigger('resize')
            });"))),
            titlePanel("SDP Case Memo II: Prevalence of Dual Enrollment (DE)", "SDP Case Memo II: Prevalence of Dual Enrollment (DE)"),
            sidebarLayout(position = "left",
                          sidebarPanel(width = 3,
                                       helpText(HTML("<b>", "Full Screen ↓", "</b>", '<br/>',
                                                     "- Click **Move Main Panel** to have tables and charts expand to full screen at the bottom of the page")),
                                       actionButton("showpanel", "Move Main Panel", icon = icon("redo")),
                                       helpText(HTML('<br/>', "<b>", "Filter Info ↓↓↓↓↓", "</b>", '<br/>',
                                                     "- Available filter options are updated based on the previous filter's input/s.", '<br/>', 
                                                     "- Input/s filter data on all pages.")),
                                       uiOutput("cohort"),
                                       uiOutput("district"),
                                       uiOutput("location"),
                                       uiOutput("size"),
                                       uiOutput("school")),
                          mainPanel(
                            tabsetPanel(type = "tabs",
                                        tabPanel("Highlights", 
                                                 helpText(HTML("")),
                                                 fluidRow(
                                                   column(width = 3, helpText(HTML("")),
                                                          actionButton("info1", "Click Here for Chart Info", icon = icon("list-alt")),
                                                          helpText(HTML("")),
                                                          uiOutput("facet"))),
                                                 fluidRow(
                                                   column(width = 6, plotlyOutput("plot_enroll", height = 900)),
                                                   column(width = 6, tableOutput("demos"))),
                                                 plotlyOutput("plot_hours", height = 600)),
                                        tabPanel("Dataset", 
                                                 helpText(HTML("")),
                                                 actionButton("info2", "Click Here for Dataset Download Info", icon = icon("list-alt")),
                                                 helpText(HTML("")),
                                                 dataTableOutput("table_all")))))))


#--------------------------- server ------------------------------------------#
server = 
  shinyServer(
    function(input, output, session) {
      
      result_auth <- 
        secure_server(check_credentials = check_credentials(credentials))
      
      output$res_auth <- 
        renderPrint({
          reactiveValuesToList(result_auth)})
      
      observeEvent(input$showpanel,{
        session$sendCustomMessage(type = 'resize', message = 1)})
      
      observeEvent(input$info1, {
        shinyalert("Plot Info", 
                   "Hover over charts to view stats and functional buttons:\n- Click the **camera** functional button to download a chart based on your current filter settings: ", 
                   type = "info")})
      
      observeEvent(input$info2, {
        shinyalert("Download Info", 
                   "To download a dataset based on your current filter settings:\n(1) Make sure 'Show **All** entries' is selected\n(2) Click the **CSV** button.", 
                   type = "info")})
      
      #--------------------------- data --------------------------------------------#           
      data <-
        viz
      
      #--------------------------- filters -----------------------------------------#            
      #render filters
      output$cohort <- 
        renderUI({
          pickerInput(inputId = "Cohort", "Cohort", choices = var_cohort(), 
                      multiple = T, options = pickerOptions(`actions-box` = T))})
      
      output$district <- 
        renderUI({
          pickerInput(inputId = "District", "District", choices = var_district(), 
                      multiple = T, options = pickerOptions(`actions-box` = T))})
      
      output$location <- 
        renderUI({
          pickerInput(inputId = "Location", "Location", choices = var_location(), 
                      multiple = T, options = pickerOptions(`actions-box` = T))})
      
      output$size <- 
        renderUI({
          pickerInput(inputId = "Size", "Size", choices = var_size(), 
                      multiple = T, options = pickerOptions(`actions-box` = T))})
      
      output$school <- 
        renderUI({
          pickerInput(inputId = "School", "School", choices = var_school(), 
                      multiple = T, options = pickerOptions(`actions-box` = T, 
                                                            liveSearch = T,
                                                            liveSearchNormalize = T,
                                                            liveSearchPlaceholder = "Search School",
                                                            liveSearchStyle = "contains"))})
      
      output$facet <- 
        renderUI({
          selectInput("Facet", "Aggregation", 
                      selected = "Overall",
                      multiple = F, choices = c("Overall", "District", "Size", "Location", "School"))})
      
      #filtered data
      data_filtered <- 
        reactive({
          viz %>% 
            filter(Cohort %in% cohort(), 
                   District %in% district(),
                   Location %in% location(), 
                   Size %in% size(),
                   School %in% school())})
      
      #get filters from inputs
      cohort <- 
        reactive({
          if (is.null(input$Cohort)) unique(viz$Cohort) else input$Cohort})
      
      district <- 
        reactive({
          if (is.null(input$District)) unique(viz$District) else input$District})
      
      location <- 
        reactive({
          if (is.null(input$Location)) unique(viz$Location) else input$Location})
      
      size <- 
        reactive({
          if (is.null(input$Size)) unique(viz$Size) else input$Size})
      
      school <- 
        reactive({
          if (is.null(input$School)) unique(viz$School) else input$School})
      
      #get available categories
      var_cohort <- 
        reactive({
          file1 <- 
            data
          if (is.null(data)) {return()}
          as.list(unique(file1$Cohort))})
      
      var_district <- 
        reactive({
          data %>% 
            filter(Cohort %in% cohort()) %>% 
            pull(District) %>% 
            unique()})
      
      var_location <- 
        reactive({
          data %>% 
            filter(Cohort %in% cohort(),
                   District %in% district()) %>% 
            pull(Location) %>% 
            unique()})
      
      var_size <- 
        reactive({
          data %>% 
            filter(Cohort %in% cohort(),
                   District %in% district(),
                   Location %in% location()) %>% 
            pull(Size) %>% 
            unique()})
      
      var_school <- 
        reactive({
          data %>% 
            filter(Cohort %in% cohort(),
                   District %in% district(),
                   Location %in% location(),
                   Size %in% size()) %>% 
            pull(School) %>% 
            unique()})
      
      
      #--------------------------- content -----------------------------------------#
      #tab 1 ##################################################################
      
      #left upper
      output$plot_enroll <- 
        renderPlotly({
          
          if (input$Facet == "Overall") {
            
            vizBY <-
              data_filtered() %>% 
              group_by(facet = Cohort) %>% 
              summarise(Percent = round(mean(dual_enrl_taken) * 100, 1)) %>% 
              mutate(facet = fct_rev(as.factor(facet)),
                     all = "Overall")
            
            ggplotly(
              ggplot(vizBY, aes(x = Percent, y = facet, fill = facet)) + 
                geom_col(position = "dodge", alpha = 0.65) +
                scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
                labs(title = paste0("Dual Enrollment | ", input$Facet), 
                     x = "", y = "") +
                theme(legend.position = "none") +
                facet_wrap(~ all) +
                scale_fill_viridis(discrete = T),
              tooltip = c("Percent")) %>% 
              layout(title = list(y = .95, xref = "plot"), margin = list(t = 100))
            
          } else {
            
            vizBY <-
              data_filtered() %>% 
              group_by(Cohort, facet = !!sym(input$Facet)) %>% 
              summarise(Percent = round(mean(dual_enrl_taken) * 100, 1)) %>% 
              arrange(desc(Percent)) %>% 
              slice_head(n = 8)
            
            ggplotly(
              ggplot(vizBY, aes(x = Percent, y = reorder_within(facet, Percent, Cohort), 
                                fill = reorder_within(facet, Percent, Cohort))) + 
                geom_col(position = "dodge", alpha = 0.65) +
                scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
                labs(title = ifelse(input$Facet == "School", paste0("Dual Enrollment | ", input$Facet, " (Top 8)"),
                                    paste0("Dual Enrollment | ", input$Facet)), 
                     x = "", y = "") +
                scale_y_reordered() +
                theme(legend.position = "none") +
                facet_grid(rows = vars(Cohort), scales = "free_y") +
                scale_fill_viridis(discrete = T),
              tooltip = c("Percent")) %>% 
              layout(title = list(y = .925, xref = "plot"), margin = list(t = 100))
          }
        })
      
      #right upper
      output$demos <- 
        function() {
          
          if (length(input$Cohort) == 2 | length(input$Cohort) == 0) {
            
            if (input$Facet == "School") {
              
              vizBY <-
                data_filtered() %>% 
                group_by(Cohort, School) %>% 
                summarise(Percent = round(mean(dual_enrl_taken) * 100, 1)) %>% 
                arrange(desc(Percent)) %>% 
                slice_head(n = 8) %>% 
                ungroup()
              
              d <-
                data_filtered() %>% 
                filter(School %in% vizBY$School)
              
              title <-
                HTML("<b>", "Demographic Comparison (Top 8 by DE % Schools)", "</b>")
              
            } else {
              
              d <-
                data_filtered()
              
              title <-
                HTML("<b>", "Demographic Comparison", "</b>")
              
            }
            
            demos <-
              d %>% 
              group_by(Cohort, `Dual Enrollment`) %>% 
              summarise(across(`Math 8th`:`ELA 8th`, ~ round(mean(.), 1)),
                        across(`IEP Ever`:`FRPL Ever`, ~ round(sum(.) / n() * 100, 1))) %>% 
              pivot_longer(cols = `Math 8th`:`FRPL Ever`, names_to = "Category") %>% 
              pivot_wider(id_cols = Cohort:Category, names_from = `Dual Enrollment`, values_from = value) %>% 
              mutate(Difference = round(Participants - `Non-Participants`, 1)) %>% 
              pivot_wider(id_cols = Category, names_from = Cohort, values_from = `Non-Participants`:Difference) %>% 
              mutate(Measure = if_else(Category %in% c("Math 8th", "ELA 8th"), "Mean", "Percent"),
                     Variable = case_when(Category %in% c("Female", "African American", "Asian American",
                                                          "Hispanic", "White, Not Hispanic", "Other Race/Ethnicity") ~ "Race/Ethnicity",
                                          Category %in% c("Math 8th", "ELA 8th", "IEP Ever",
                                                          "LEP Ever", "Graduated", "On-Time Graduation",
                                                          "College Enrollment") ~ "Academic",
                                          Category == "FRPL Ever" ~ "Socioeconomic")) %>% 
              ungroup() %>% 
              select(Variable, Measure, Category, 
                     Participants_2007, `Non-Participants_2007`, Difference_2007,
                     Participants_2008, `Non-Participants_2008`, Difference_2008)
            
            demos %>% 
              select(-c(Variable, Measure)) %>% 
              mutate(across(c(Difference_2007, Difference_2008), ~ cell_spec(., color = ifelse(. < 0, "red", "black"), bold = T))) %>% 
              kable("html", caption = title,
                    escape = F, col.names = c("Category", 
                                              rep(c("Any DE", "no DE", "Diff."), 2)),
                    align = c("l", rep("c", 6))) %>%
              kable_styling("striped", full_width = F) %>% 
              pack_rows(index = table(demos$Variable), bold = T) %>%
              group_rows("Mean", 1, 2, bold = F, italic = T) %>% 
              group_rows("Percent", 3, 7, bold = F, italic = T) %>% 
              group_rows("Percent", 8, 13, bold = F, italic = T) %>% 
              group_rows("Percent", 14, 14, bold = F, italic = T) %>% 
              column_spec(1, bold = T) %>% 
              column_spec(4, border_right = T) %>% 
              add_header_above(c(" " = 1, "2007" = 3, "2008" = 3))
            
          } else {
            
            if (input$Facet == "School") {
              
              vizBY <-
                data_filtered() %>% 
                group_by(Cohort, School) %>% 
                summarise(Percent = round(mean(dual_enrl_taken) * 100, 1)) %>% 
                arrange(desc(Percent)) %>% 
                slice_head(n = 8) %>% 
                ungroup()
              
              d <-
                data_filtered() %>% 
                filter(School %in% vizBY$School)
              
              title <-
                HTML("<b>", "Demographic Comparison (Top 8 by DE % Schools)", "</b>")
              
            } else {
              
              d <-
                data_filtered()
              
              title <-
                HTML("<b>", "Demographic Comparison", "</b>")
              
            }
            
            demos <-
              d %>% 
              group_by(`Dual Enrollment`) %>% 
              summarise(across(`Math 8th`:`ELA 8th`, ~ round(mean(.), 1)),
                        across(`IEP Ever`:`FRPL Ever`, ~ round(sum(.) / n() * 100, 1))) %>% 
              pivot_longer(cols = `Math 8th`:`FRPL Ever`, names_to = "Category") %>% 
              pivot_wider(id_cols = Category, names_from = `Dual Enrollment`, values_from = value) %>% 
              mutate(Difference = round(Participants - `Non-Participants`, 1)) %>% 
              mutate(Measure = if_else(Category %in% c("Math 8th", "ELA 8th"), "Mean", "Percent"),
                     Variable = case_when(Category %in% c("Female", "African American", "Asian American",
                                                          "Hispanic", "White, Not Hispanic", "Other Race/Ethnicity") ~ "Race/Ethnicity",
                                          Category %in% c("Math 8th", "ELA 8th", "IEP Ever",
                                                          "LEP Ever", "Graduated", "On-Time Graduation",
                                                          "College Enrollment") ~ "Academic",
                                          Category == "FRPL Ever" ~ "Socioeconomic")) %>% 
              ungroup() %>% 
              select(Variable, Measure, Category, 
                     Participants, `Non-Participants`, Difference)
            
            lab <-
              setNames(c(1, 3), c(" ", input$Cohort[1]))
            
            demos %>% 
              select(-c(Variable, Measure)) %>% 
              mutate(across(c(Difference), ~ cell_spec(., color = ifelse(. < 0, "red", "black"), bold = T))) %>% 
              kable("html", caption = title,
                    escape = F, col.names = c("Category", 
                                              "Any DE", "no DE", "Diff."),
                    align = c("l", rep("c", 3))) %>%
              kable_styling("striped", full_width = F) %>% 
              pack_rows(index = table(demos$Variable), bold = T) %>%
              group_rows("Mean", 1, 2, bold = F, italic = T) %>% 
              group_rows("Percent", 3, 7, bold = F, italic = T) %>% 
              group_rows("Percent", 8, 13, bold = F, italic = T) %>% 
              group_rows("Percent", 14, 14, bold = F, italic = T) %>% 
              column_spec(1, bold = T) %>% 
              column_spec(4) %>% 
              add_header_above(lab)
            
          }
          
          
        }

      #bottom
      output$plot_hours <- 
        renderPlotly({
          
          if (input$Facet == "Overall") {
            
            hrs <-
              data_filtered() %>% 
              filter(dual_enrl_taken == 1) %>% 
              mutate(Cohort = as.factor(Cohort),
                     all = "Overall")
            
            if (nrow(hrs) == 0) {
              
              ggplot() + 
                labs(title = "No students participated in DE",
                     caption = "none") +
                theme(panel.background = element_blank())
              
            } else {
              
              ggplotly(
                ggplot(hrs, aes(y = dual_enrl_hours, x = Cohort, fill = Cohort)) + 
                  geom_boxplot(alpha = 0.65) +
                  labs(title = paste0("Dual Enrollment Hours* | ", input$Facet, "\n*Among students who took DE courses"), 
                       y = "Hours", x = "") +
                  theme(legend.position = "none") +
                  scale_y_continuous(breaks = c(seq(0, 100, 20), 200, 400, max(hrs$dual_enrl_hours)),
                                     limits = c(min(hrs$dual_enrl_hours), max(hrs$dual_enrl_hours))) +
                  facet_wrap(~ all) +
                  scale_fill_viridis(discrete = T)) %>% 
                layout(title = list(y = .925, xref = "plot"), margin = list(t = 100))
              
            }
            
          } else {
            
            vizBY <-
              data_filtered() %>% 
              group_by(Cohort, facet = !!sym(input$Facet)) %>% 
              summarise(Percent = round(mean(dual_enrl_taken) * 100, 1)) %>% 
              arrange(desc(Percent)) %>% 
              slice_head(n = 8) %>% 
              ungroup()
            
            hrs <-
              data_filtered() %>% 
              filter(dual_enrl_taken == 1) %>% 
              mutate(Cohort = as.factor(Cohort),
                     facet = !!sym(input$Facet)) %>% 
              filter(facet %in% vizBY$facet)
            
            if (nrow(hrs) == 0) {
              
              ggplot() + 
                labs(title = "No students participated in DE",
                     caption = "none") +
                theme(panel.background = element_blank())
              
            } else {
              
              ggplotly(
                ggplot(hrs, aes(y = dual_enrl_hours, 
                                x = fct_rev(reorder_within(facet, dual_enrl_hours, Cohort)), 
                                fill = fct_rev(reorder_within(facet, dual_enrl_hours, Cohort)))) + 
                  geom_boxplot(alpha = 0.65) +
                  labs(title = ifelse(input$Facet == "School", paste0("Dual Enrollment Hours* | ", input$Facet, " (Top 8 by DE %)",
                                                                      "\n*Among students who took DE courses"),
                                      paste0("Dual Enrollment Hours* | ", input$Facet, "\n*Among students who took DE courses")), 
                       y = "Hours", x = "") +
                  theme(legend.position = "none", axis.text.x = element_text(angle = 35)) +
                  scale_y_continuous(breaks = c(seq(0, 100, 20), 200, 400, max(hrs$dual_enrl_hours)),
                                     limits = c(min(hrs$dual_enrl_hours), max(hrs$dual_enrl_hours))) +
                  scale_x_reordered() +
                  facet_wrap(~ Cohort, scales = "free_x") +
                  scale_fill_viridis(discrete = T), 
                tooltip = c("tooltip")) %>% 
                layout(title = list(y = .925, xref = "plot"), margin = list(t = 100))
              
            }
          
          }})
      
      #tab 2 ##################################################################
      output$table_all <- 
        renderDataTable({
          d <-
            data_filtered()
          
          g <-
            d %>% 
            select(sdpsid, `Dual Enrollment`, dual_enrl_hours:`LEP Ever`,
                   `FRPL Ever`, Female:`Other Race/Ethnicity`, Graduated:`College Enrollment`) %>% 
            mutate(across(c(`Dual Enrollment`, District:Cohort, 
                            `IEP Ever`:`College Enrollment`), ~ as.factor(.)),
                   sdpsid = as.character(sdpsid))
          
          datatable(g,
                    filter = "top",
                    extensions = c('FixedColumns', 'Buttons'), 
                    rownames = F, 
                    options = list(dom = 'Blfrtip',
                                   buttons = c('csv'),
                                   fixedColumns = list(leftColumns = 1),
                                   lengthMenu = list(c(10, 50, 100, -1),
                                                     c(10, 50, 100, "All"))))})
      
    })

#--------------------------- launch app ---------------------------------------#
shinyApp(ui, server)







