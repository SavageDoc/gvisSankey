# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Demo of Sankey Plots")
  , tabsetPanel( id='baseTab'
                 , tabPanel( 'Base', htmlOutput( 'sankeyBase' ) )
                 , tabPanel( 'Colour', htmlOutput( 'sankeyColour' ) )
                 , tabPanel( 'Fancy', htmlOutput( 'sankeyFancy' ) ) )
  )
)
