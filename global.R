# Global file for Sankey demo app

## Package load ----
library( shiny )
library( tidyverse )
suppressPackageStartupMessages( library( googleVis ) )
library( scales )

## Data load ----
rawSankeyData <- readr::read_csv( '~/Kaggle/Conversion/KAG_conversion_data.csv' )
sankeyData <- rawSankeyData %>%
  # Convert the campaign id to a character
  mutate( xyz_campaign_id=as.character( xyz_campaign_id )
          , Total='Total' )

## Functions ----
sankeyLevel <- function( sankeyData, fromVar, toVar ){
  # See vignette( 'programming' ) for more information as to how this works
  fromVar <- enquo( fromVar )
  toVar <- enquo( toVar )
  
  levelData <- sankeyData %>%
    group_by( From = !! fromVar, To=!! toVar ) %>%
    summarise( nClicks=sum( Clicks )
               , nImpressions=sum( Total_Conversion )
               , nSold=sum( Approved_Conversion )
               , totalSpend=sum( Spent ) ) %>% 
    mutate( sankey.tooltip=paste( 'Clicks:', nClicks, '<br>'
                                  , 'Impressions:', nImpressions, '<br>'
                                  , 'Sales:', nSold, '<br>'
                                  , 'Value:', scales::dollar( totalSpend ) ) )
  return( levelData )
}