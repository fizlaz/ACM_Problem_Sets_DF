
shinyUI(
    
    ###########
    #my code below
    pageWithSidebar(
        headerPanel("Loan Data"),

        sidebarPanel(
            wellPanel(
                numericInput("muxa","x mean approved",4)
            ),
            wellPanel(
                numericInput("muya","y mean approved",150)
            ),
            wellPanel(
                numericInput("sdxa","x sd approved",1)
            ),
            wellPanel(
                numericInput("sdya","y sd approved",20)
            ),

            wellPanel(
                numericInput("muxd","x mean denied",10)
            ),
            wellPanel(
                numericInput("muyd","y mean denied",100)
            ),
            wellPanel(
                numericInput("sdxd","x sd denied",2)
            ),
            wellPanel(
                numericInput("sdyd","y sd denied",30)
            )

        ),

        mainPanel(
            plotOutput("plot1")
        )
    )
    #my code above
    ###########

) # END of shinyUI     
