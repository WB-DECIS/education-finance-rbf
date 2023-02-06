# library(shiny)
# library(shinyBS) 
# 
# title_html <- "<em>Tooltip</em> <u>with</u> <b>HTML</b>"
# 
# shinyApp(
#   ui = navbarPage(
#     # bsTooltip(id = "someInput", title = "This is an input", 
#     #           placement = "left", trigger = "hover"),
#     tags$script(HTML('
#            $( document ).on("shiny:sessioninitialized", function(event) {
#                 $(\'span[data-toggle="tooltip"]\').tooltip({
#                     html: true
#                 });
#            });'
#     )),
# 
#     tabsetPanel(
#       tabPanel(span("Tab 1", title = title_html,`data-toggle`="tooltip", `data-placement` ="bottom")),
#       tabPanel(span("Tab 2",title="bbb",`data-toggle`="tooltip")),
#       tabPanel(span("Tab 3",title="ccc",`data-toggle`="tooltip"))
#     )
#   ),
# 
#   server = function(input, output) {
#     # addTooltip(id = "someInput", title = "This is an input.",
#     #            placement = "left", trigger = "hover")
#   }
# )


library(shiny)
library(shinydashboard)
library(shinyWidgets)

ui <- dashboardPage(
  dashboardHeader( title = "app",
                   tags$li(class = "dropdown",
                           dropMenu(
                             dropdownButton("Info", status = 'success', icon = icon('info')),
                             h3(strong('Information')),
                             br(),
                             h5('This is really helpful'),
                             textInput('text', 'You can also put UI elements here'),
                             placement = "bottom",
                             arrow = TRUE)
                           
                   )
                   
  )
  ,
  dashboardSidebar(),
  dashboardBody(actionButton('help', 'Help'))
)

server <- function(input, output) { 
  
  observeEvent(input$help,{
    showModal(modalDialog(
      title = "Help!",
      "Information",
      textInput('text2', 'You can also put UI elements here')
    ))
  })
}

shinyApp(ui, server)