# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$sankeyBase <- renderGvis({
    sankeyData1 <- sankeyLevel( sankeyData, Total, xyz_campaign_id )
    sankeyData2 <- sankeyLevel( sankeyData, xyz_campaign_id, gender )
    sankeyData3 <- sankeyLevel( sankeyData, gender, age )
    
    plotData <- bind_rows( sankeyData1, sankeyData2, sankeyData3 )
    
    sankeyPlot <- gvisSankey( plotData
                              , from='From'
                              , to='To'
                              , weight='nClicks'
                              , options=list( tooltip="{isHtml: 'TRUE'}"))
    sankeyPlot
  })
  
  output$sankeyColour <- renderGvis({
    sankeyData1 <- sankeyLevel( sankeyData, Total, xyz_campaign_id )
    sankeyData2 <- sankeyLevel( sankeyData, xyz_campaign_id, gender )
    sankeyData3 <- sankeyLevel( sankeyData, gender, age )
    
    plotData <- bind_rows( sankeyData1, sankeyData2, sankeyData3 )
    
    sankeyPlot <- gvisSankey( plotData
                              , from='From'
                              , to='To'
                              , weight='nClicks'
                              , options=list( tooltip="{isHtml: 'TRUE'}"
                                              , sankey="{link: {colorMode:'gradient'}
                                              , label:{fontsize: 14, bold: 'TRUE'}}"))
    sankeyPlot
  })
  
  output$sankeyFancy <- renderGvis({
    sankeyData1 <- sankeyLevel( sankeyData, Total, xyz_campaign_id ) %>%
      mutate( revPerClick=totalSpend/nClicks )
    sankeyData2 <- sankeyLevel( sankeyData, xyz_campaign_id, gender ) %>%
      mutate( revPerClick=totalSpend/nClicks )
    sankeyData3 <- sankeyLevel( sankeyData, gender, age ) %>%
      mutate( revPerClick=totalSpend/nClicks )
    
    plotData <- bind_rows( sankeyData1, sankeyData2, sankeyData3 ) %>%
      ungroup() %>%      
      mutate( rowID=row_number()
              , gaugeLabel='Revenue Per Click' ) 

    gaugeData <- plotData %>%
      select( rowID, gaugeLabel, revPerClick ) %>%
      group_by( rowID ) %>%
      do( gauge.tooltip=HTML( gvisGauge( data=as.data.frame(.)[,c('gaugeLabel', 'revPerClick' )]
                                   , labelvar='gaugeLabel'
                                   , numvar = 'revPerClick' 
                                   , options=list( min=0
                                                   , max=2
                                                   , redFrom=0
                                                   , redTo=1
                                                   , yellowFrom=1
                                                   , yellowTo=1.25
                                                   , greenFrom=1.25
                                                   , greenTo=2 )
                                   )$html$chart )
          ) %>%
      inner_join( plotData, by='rowID' ) %>%
      ungroup() %>%
      select( From, To, nClicks, gauge.tooltip )
    
    sankeyPlot <- gvisSankey( gaugeData %>% select( From, To, nClicks )
                              , from='From'
                              , to='To'
                              , weight='nClicks'
                              , options=list( tooltip="{isHtml: 'TRUE'}" ) )
    sankeyPlot
  })
})
