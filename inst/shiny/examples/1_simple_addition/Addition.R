
Addition <- R6::R6Class(
  "Addition", 
   inherit = tidymodules::TidyModule,
   public = list(
     initialize = function(...){
       # mandatory
       super$initialize(...)
       
       self$definePort({
         self$addInputPort(
           name = "left",
           description = "input value to add to the user selected number",
           sample = 5)
         
       self$addOutputPort(
         name = "total",
         description = "Sum of the two numbers",
         sample = 6)
       })
     },
     ui = function() {
       div(style="width:30%;background:lightgrey;border: solid;border-color: grey;padding: 20px;",
         "Module input : ",textOutput(self$ns("left")),
         " + ",sliderInput(self$ns("right"),label = "Number to add",min = 1,max = 100,value = 1),
         " = ",textOutput(self$ns("total"))
       )
     },
     server = function(input, output, session){
       # Mandatory
       super$server(input, output, session)
       
       sum_numbers <- reactive({
         req(input$right)
         req(self$getInput(1))
         as.numeric(self$getInput(1)())+as.numeric(input$right)
       })
       
       output$left <- renderText({
         req(self$getInput(1))
         self$getInput(1)()
       })
       
       output$total <- renderText({
         sum_numbers()
       })
       
       self$assignPort({
         self$updateOutputPort(
           id = "total",
           output = sum_numbers)
       })
     }
   )
)


