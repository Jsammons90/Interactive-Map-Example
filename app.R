# load required libraries
library(rgeos)
library(maptools)
library(rgdal)
library(sf)
library(dplyr)
library(leaflet)
library(shiny)
library(shinydashboard)
require(sp)

##I used a local directory for the initial setup and testing for the bulk of the project
##and switched to relative file paths when I made the project live.
#setwd("C://Users//Sammons Family//Desktop//College - Final Year//Software Project")

##Loading the initial data
spdf = st_read("./Census2011_Admin_Counties_generalised20m.shp")
median_earned_income_by_education = read.csv("./Median earned income per person by highest level of education achieved and county of work 2016_cleaned.csv")
genderDF = read.csv("./genderData.csv")
popDF = read.csv("./Pop_Data.csv")

##Cleaning it up by selecting just want I want from the spdf data frame
county_data = select(spdf,"GEOGID", "NUTS1", "NUTS1NAME", "NUTS2", "NUTS2NAME", "NUTS3", "NUTS3NAME", "COUNTY", "COUNTYNAME", "geometry")
#Updating the county names to match to the median_earned_income csv
county_data[2, 9] = "Limerick"
county_data[6, 9] = "Waterford"
county_data[8, 9] = "Galway"
county_data[9, 9] = "Leitrim"
county_data[10, 9] = "Mayo"
county_data[11, 9] = "Roscommon"
county_data[12, 9] = "Sligo"
county_data[13, 9] = "Cavan"
county_data[14, 9] = "Donegal"
county_data[15, 9] = "Monaghan"
county_data[16, 9] = "Carlow"
county_data[20, 9] = "Dun-Laoghaire Rathdown"
county_data[21, 9] = "Kildare"
county_data[22, 9] = "Kilkenny"
county_data[23, 9] = "Laois"
county_data[24, 9] = "Longford"
county_data[25, 9] = "Louth"
county_data[26, 9] = "Meath"
county_data[27, 9] = "Offaly"
county_data[28, 9] = "Westmeath"
county_data[29, 9] = "Wexford"
county_data[30, 9] = "Wicklow"
county_data[31, 9] = "Clare"
county_data[34, 9] = "Kerry"
colnames(county_data) = c("GEOGID", "NUTS1", "NUTS1NAME", "NUTS2", "NUTS2NAME", "NUTS3", "NUTS3NAME", "COUNTYNUMBER", "County", "geometry")

#Grabbing only the columns I need
median_earned_income_by_education = select(median_earned_income_by_education, "County", "Technical", "Advanced.certficate", "Higher.certificate", "Ordinary.degree", "Honours.degree", "Postgraduate", "Ph.D")
#Removing one un-needed row (State)
median_earned_income_by_education = median_earned_income_by_education[-c(1),]

#Fixing county header in popDF
colnames(popDF) = c("County", "Males", "Females")

##Merging the data
all_data = merge(data.frame(genderDF, row.names = NULL), data.frame(median_earned_income_by_education, row.names = NULL), by = "County", all = TRUE)
all_data = merge(data.frame(all_data, row.names = NULL), data.frame(popDF, row.names = NULL), by = "County", all = TRUE)

# Cleaning up and converting salary data to numeric
#Technical
all_data$Technical = gsub(" ","",all_data$Technical)
all_data$Technical = gsub(",","",all_data$Technical)
technical = as.numeric(all_data$Technical)
#Advanced certficate
all_data$Advanced.certficate = gsub(" ","",all_data$Advanced.certficate)
all_data$Advanced.certficate = gsub(",","",all_data$Advanced.certficate)
advanced_certficate = as.numeric(all_data$Advanced.certficate)
#Higher certificate
all_data$Higher.certificate = gsub(" ","",all_data$Higher.certificate)
all_data$Higher.certificate = gsub(",","",all_data$Higher.certificate)
higher_certificate = as.numeric(all_data$Higher.certificate)
#Ordinary degree
all_data$Ordinary.degree = gsub(" ","",all_data$Ordinary.degree)
all_data$Ordinary.degree = gsub(",","",all_data$Ordinary.degree)
ordinary_degree = as.numeric(all_data$Ordinary.degree)
#Honours degree
all_data$Honours.degree = gsub(" ","",all_data$Honours.degree)
all_data$Honours.degree = gsub(",","",all_data$Honours.degree)
honours_degree = as.numeric(all_data$Honours.degree)
#Postgraduate
all_data$Postgraduate = gsub(" ","",all_data$Postgraduate)
all_data$Postgraduate = gsub(",","",all_data$Postgraduate)
postgraduate = as.numeric(all_data$Postgraduate)
#Ph.D
all_data$Ph.D = gsub(" ","",all_data$Ph.D)
all_data$Ph.D = gsub(",","",all_data$Ph.D)
ph_d = as.numeric(all_data$Ph.D)


str(all_data)
str(county_data)
convertedData = as_Spatial(county_data)
convertedData$id = rownames(convertedData)

# Must convert the datum so that it's readable by leaflet
convertedData = spTransform(convertedData, CRS("+proj=longlat +datum=WGS84 +no_defs"))
oo = merge(convertedData,all_data, by="County")
oo$Technical = as.numeric(oo$Technical)
oo$Advanced.certficate = as.numeric(oo$Advanced.certficate)
oo$Higher.certificate = as.numeric(oo$Higher.certificate)
oo$Ordinary.degree = as.numeric(oo$Ordinary.degree)
oo$Honours.degree = as.numeric(oo$Honours.degree)
oo$Postgraduate = as.numeric(oo$Postgraduate)
oo$Ph.D = as.numeric(oo$Ph.D)

# Setting up the color scheme based off of the chosen education level
#Technical
pal_technical = colorNumeric(
  palette = "Blues",
  domain = oo$technical)
#Advanced certficate
pal_advanced_certificate = colorNumeric(
  palette = "Blues",
  domain = oo$advanced_certficate)
#Higher certificate
pal_higher_certificate = colorNumeric(
  palette = "Blues",
  domain = oo$higher_certificate)
#Ordinary degree
pal_ordinary_degree = colorNumeric(
  palette = "Blues",
  domain = oo$ordinary_degree)
#Honours degree
pal_honours_degree = colorNumeric(
  palette = "Blues",
  domain = oo$honours_degree)
#Postgraduate
pal_postgraduate = colorNumeric(
  palette = "Blues",
  domain = oo$postgraduate)
#Ph.D
pal_phd = colorNumeric(
  palette = "Blues",
  domain = oo$ph_d)

##Shiny UI
css = HTML("
  .leaflet-top, .leaflet-bottom {
    z-index: unset !important;
  }

  .leaflet-touch .leaflet-control-layers, .leaflet-touch .leaflet-bar {
    z-index: 10000000000 !important;
  }
")
ui = fluidPage(
  tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
  absolutePanel(top = 10, left = 500, fixed = TRUE,
                tags$div(style = "opacity: 0.90; background: #FFFEE; padding: 4px; ",
                         htmlOutput("header")
                )),
  selectInput("education", "Level of education:",
              list("Technical", "Advanced Certificate", "Higher Certificate", "Ordinary Degree", "Honours Degree", "Postgraduate", "Ph.D")
  ),
  textOutput("result"),
  leafletOutput("mymap"),
  absolutePanel(bottom = 100, left = 10, fixed = FALSE,
                tags$div(style = "opacity: 0.90; background: #FFFEE; padding: 4px; ",
                         helpText("Please click on a county for more information."),
                         htmlOutput("text")
                )
  ),
  plotOutput("genderPie"),
  plotOutput("popPie")
)

##Shiny server
server = function(input, output, session){
  output$header = renderUI({
    header_str = paste("Salary Scales by County in the Republic of Ireland")
    HTML(paste("<h2>",header_str,"</h2>"))})
  
  output$mymap = renderLeaflet({
    #################
    ##Technical Map##
    #################
    if(input$education == "Technical"){
      # Using leaflet to plot the shape file on a map for interactivity
      leaflet(data=oo,
              options = leafletOptions(zoomControl = TRUE,
                                       minZoom = 6, maxZoom = 10,
                                       dragging = TRUE)) %>%
        addTiles() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        # Setting the view to Ireland
        setView(lat=53, lng=-8, zoom=7) %>%
        #Adding the polygon information for the counties
        addPolygons(fillColor = ~pal_technical(oo$Technical),
                    color = NA,
                    fillOpacity = 1,
                    highlight = highlightOptions(weight = 2,
                                                 color = "black",
                                                 fillOpacity = .1,
                                                 bringToFront = FALSE),
                    label = ~County,
                    layerId = ~County) %>%
        # Adding a legend to the map
        addLegend(data = convertedData, 
                  colors = c("#000099", "#3399ff", "#ccffff"),
                  labels = c("More", " ", "Less"),
                  opacity = 1.0, 
                  title = "Salary",
                  position = "bottomright")
      
    }
    
    
    ############################
    ##Advanced Certificate Map##
    ############################
    else if(input$education == "Advanced Certificate"){
      # Using leaflet to plot the shape file on a map for interactivity
      leaflet(data=oo,
              options = leafletOptions(zoomControl = TRUE,
                                       minZoom = 6, maxZoom = 10,
                                       dragging = TRUE)) %>%
        addTiles() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        # Setting the view to Ireland
        setView(lat=53, lng=-8, zoom=7) %>%
        #Adding the polygon information for the counties
        addPolygons(fillColor = ~pal_advanced_certificate(oo$Advanced.certficate),
                    color = NA,
                    fillOpacity = 1,
                    highlight = highlightOptions(weight = 2,
                                                 color = "black",
                                                 fillOpacity = .1,
                                                 bringToFront = FALSE),
                    label = ~County,
                    layerId = ~County) %>%
        # Adding a legend to the map
        addLegend(data = convertedData, 
                  colors = c("#000099", "#3399ff", "#ccffff"),
                  labels = c("More", " ", "Less"),
                  opacity = 1.0, 
                  title = "Salary",
                  position = "bottomright")
    }
    
    
    ##########################
    ##Higher Certificate Map##
    ##########################
    else if(input$education == "Higher Certificate"){
      # Using leaflet to plot the shape file on a map for interactivity
      leaflet(data=oo,
              options = leafletOptions(zoomControl = TRUE,
                                       minZoom = 6, maxZoom = 10,
                                       dragging = TRUE)) %>%
        addTiles() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        # Setting the view to Ireland
        setView(lat=53, lng=-8, zoom=7) %>%
        #Adding the polygon information for the counties
        addPolygons(fillColor = ~pal_higher_certificate(oo$Higher.certificate),
                    color = NA,
                    fillOpacity = 1,
                    highlight = highlightOptions(weight = 2,
                                                 color = "black",
                                                 fillOpacity = .1,
                                                 bringToFront = FALSE),
                    label = ~County,
                    layerId = ~County) %>%
        # Adding a legend to the map
        addLegend(data = convertedData, 
                  colors = c("#000099", "#3399ff", "#ccffff"),
                  labels = c("More", " ", "Less"),
                  opacity = 1.0, 
                  title = "Salary",
                  position = "bottomright")
    }
    
    
    #######################
    ##Ordinary Degree Map##
    #######################
    else if(input$education == "Ordinary Degree"){
      # Using leaflet to plot the shape file on a map for interactivity
      leaflet(data=oo,
              options = leafletOptions(zoomControl = TRUE,
                                       minZoom = 6, maxZoom = 10,
                                       dragging = TRUE)) %>%
        addTiles() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        # Setting the view to Ireland
        setView(lat=53, lng=-8, zoom=7) %>%
        #Adding the polygon information for the counties
        addPolygons(fillColor = ~pal_ordinary_degree(oo$Ordinary.degree),
                    color = NA,
                    fillOpacity = 1,
                    highlight = highlightOptions(weight = 2,
                                                 color = "black",
                                                 fillOpacity = .1,
                                                 bringToFront = FALSE),
                    label = ~County,
                    layerId = ~County) %>%
        # Adding a legend to the map
        addLegend(data = convertedData, 
                  colors = c("#000099", "#3399ff", "#ccffff"),
                  labels = c("More", " ", "Less"),
                  opacity = 1.0, 
                  title = "Salary",
                  position = "bottomright")
    }
    
    
    ######################
    ##Honours Degree Map##
    ######################
    else if(input$education == "Honours Degree"){
      # Using leaflet to plot the shape file on a map for interactivity
      leaflet(data=oo,
              options = leafletOptions(zoomControl = TRUE,
                                       minZoom = 6, maxZoom = 10,
                                       dragging = TRUE)) %>%
        addTiles() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        # Setting the view to Ireland
        setView(lat=53, lng=-8, zoom=7) %>%
        #Adding the polygon information for the counties
        addPolygons(fillColor = ~pal_honours_degree(oo$Honours.degree),
                    color = NA,
                    fillOpacity = 1,
                    highlight = highlightOptions(weight = 2,
                                                 color = "black",
                                                 fillOpacity = .1,
                                                 bringToFront = FALSE),
                    label = ~County,
                    layerId = ~County) %>%
        # Adding a legend to the map
        addLegend(data = convertedData, 
                  colors = c("#000099", "#3399ff", "#ccffff"),
                  labels = c("More", " ", "Less"),
                  opacity = 1.0, 
                  title = "Salary",
                  position = "bottomright")
    }
    
    
    ####################
    ##Postgraduate Map##
    ####################
    else if(input$education == "Postgraduate"){
      # Using leaflet to plot the shape file on a map for interactivity
      leaflet(data=oo,
              options = leafletOptions(zoomControl = TRUE,
                                       minZoom = 6, maxZoom = 10,
                                       dragging = TRUE)) %>%
        addTiles() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        # Setting the view to Ireland
        setView(lat=53, lng=-8, zoom=7) %>%
        #Adding the polygon information for the counties
        addPolygons(fillColor = ~pal_postgraduate(oo$Postgraduate),
                    color = NA,
                    fillOpacity = 1,
                    highlight = highlightOptions(weight = 2,
                                                 color = "black",
                                                 fillOpacity = .1,
                                                 bringToFront = FALSE),
                    label = ~County,
                    layerId = ~County) %>%
        # Adding a legend to the map
        addLegend(data = convertedData, 
                  colors = c("#000099", "#3399ff", "#ccffff"),
                  labels = c("More", " ", "Less"),
                  opacity = 1.0, 
                  title = "Salary",
                  position = "bottomright")
    }
    
    
    ############
    ##Ph.D Map##
    ############
    else if(input$education == "Ph.D"){
      # Using leaflet to plot the shape file on a map for interactivity
      leaflet(data=oo,
              options = leafletOptions(zoomControl = TRUE,
                                       minZoom = 6, maxZoom = 10,
                                       dragging = TRUE)) %>%
        addTiles() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        # Setting the view to Ireland
        setView(lat=53, lng=-8, zoom=7) %>%
        #Adding the polygon information for the counties
        addPolygons(fillColor = ~pal_phd(oo$Ph.D),
                    color = NA,
                    fillOpacity = 1,
                    highlight = highlightOptions(weight = 2,
                                                 color = "black",
                                                 fillOpacity = .1,
                                                 bringToFront = FALSE),
                    label = ~County,
                    layerId = ~County) %>%
        # Adding a legend to the map
        addLegend(data = convertedData, 
                  colors = c("#000099", "#3399ff", "#ccffff"),
                  labels = c("More", " ", "Less"),
                  opacity = 1.0, 
                  title = "Salary",
                  position = "bottomright")
    }
    
  })
  observe({
    if(input$education == "Technical"){
      click = input$mymap_shape_click
      sub = oo[oo$County==input$mymap_shape_click$id, c("County", "Technical","Advanced.certficate", "Higher.certificate", "Ordinary.degree", "Honours.degree", "Postgraduate", "Ph.D", "Technical.Male", "Technical.Female", "Males", "Females")]
      #County
      ob_county = sub$County
      #Median Salary
      ob_salary = sub$Technical
      #Number of males with this degree
      ob_male = sub$Technical.Male
      #Number of females with this degree
      ob_female = sub$Technical.Female
      #Number of males in the county
      ob_male_pop = sub$Males
      #Number of females in the county
      ob_female_pop = sub$Females
      #Total population of the county
      ob_total_pop = ob_male_pop + ob_female_pop
      #Total number of people with this degree in the county
      ob_total_edu = ob_male + ob_female
      #Percentage of the county with this degree in the county
      ob_total_pc_pop = round((ob_total_edu/ob_total_pop)*100)
      #Percentage of males in the population with this degree in the county
      ob_male_pc_pop = round((ob_male/ob_total_pop)*100)
      #Percentage of females in the population with this degree in the county
      ob_female_pc_pop = round((ob_female/ob_total_pop)*100)
      
      ##Salary change data
      ob_adv_cert = sub$Advanced.certficate
      ob_high_cert = sub$Higher.certificate
      ob_ord_deg = sub$Ordinary.degree
      ob_hon_deg = sub$Honours.degree
      ob_post = sub$Postgraduate
      ob_ph_d = sub$Ph.D
      
      ob_tech_to_adv_cert = round(((ob_adv_cert-ob_salary)/ob_salary)*100)
      ob_adv_cert_to_high_cert = round(((ob_high_cert-ob_adv_cert)/ob_adv_cert)*100)
      ob_high_cert_to_ord_deg = round(((ob_ord_deg-ob_high_cert)/ob_high_cert)*100)
      ob_ord_deg_to_hon_deg = round(((ob_hon_deg-ob_ord_deg)/ob_ord_deg)*100)
      ob_hon_deg_to_post = round(((ob_post-ob_hon_deg)/ob_hon_deg)*100)
      ob_post_to_ph_d = round(((ob_ph_d-ob_post)/ob_post)*100)
      
      ##Plot data
      ob_pie = c(ob_male, ob_female)
      ob_pop_pie = c(ob_total_pop, ob_total_edu)
      if(is.null(click))
        return()
      else 
        output$text = renderUI({
          county_str = paste("<b>Selected county: </b>", ob_county)
          county_pop_str = paste("<b>County population: </b>", ob_total_pop)
          salary_str = paste("<b>Median salary for current education level: </b>", ob_salary)
          male_str = paste("<b>Males with this level of degree in 2016: </b>", ob_male)
          female_str = paste("<b>Females with this level of degree in 2016: </b>", ob_female)
          change_str = paste("<b>Salary % change with each education level: </b>", "<br/>",
                             "Technical to Advanced Certificate: ", ob_tech_to_adv_cert,"%", "<br/>",
                             "Advanced Cert to Higher Cert: ", ob_adv_cert_to_high_cert, "%", "<br/>",
                             "Higher Cert to Ordinary Degree: ", ob_high_cert_to_ord_deg, "%", "<br/>",
                             "Ordinary Degree to Honours Degree: ", ob_ord_deg_to_hon_deg, "%", "<br/>",
                             "Honours Degree to Postgraduate: ", ob_hon_deg_to_post, "%", "<br/>",
                             "Postgraduate to Ph.D: ", ob_post_to_ph_d, "%")
          HTML(paste(county_str, county_pop_str, salary_str, male_str, female_str, change_str, sep = '<br/>'))})
      
      output$genderPie = renderPlot({
        total = ob_male + ob_female
        female_pct = round((ob_female)/sum(total)*100)
        male_pct = round((ob_male)/sum(total)*100)
        female_lbl = paste(female_pct) # add percents to label
        female_lbl = paste(female_pct, "% Female", sep="") # add % to label
        male_lbl = paste(male_pct) # add percents to label
        male_lbl = paste(male_pct, "% Male", sep="") # add % to label
        pie(ob_pie, labels = c(male_lbl, female_lbl), main = "Males/Females with this degree in the county") 
      })
      output$popPie = renderPlot({
        lbl = paste(ob_total_pc_pop) # add percents to label
        lbl = paste(ob_total_pc_pop, "% with this degree", sep="") # add % to label
        pie(ob_pop_pie, labels = lbl, main = "Percentage of people with this degree in the county") 
      })
    }
    else if(input$education == "Advanced Certificate"){
      click = input$mymap_shape_click
      sub = oo[oo$County==input$mymap_shape_click$id, c("County", "Technical","Advanced.certficate", "Higher.certificate", "Ordinary.degree", "Honours.degree", "Postgraduate", "Ph.D", "Advanced.certificate.Male", "Advanced.certificate.Female", "Males", "Females")]
      #County
      ob_county = sub$County
      #Median Salary
      ob_salary = sub$Advanced.certficate
      #Number of males with this degree
      ob_male = sub$Advanced.certificate.Male
      #Number of females with this degree
      ob_female = sub$Advanced.certificate.Female
      #Number of males in the county
      ob_male_pop = sub$Males
      #Number of females in the county
      ob_female_pop = sub$Females
      #Total population of the county
      ob_total_pop = ob_male_pop + ob_female_pop
      #Total number of people with this degree in the county
      ob_total_edu = ob_male + ob_female
      #Percentage of the county with this degree in the county
      ob_total_pc_pop = round((ob_total_edu/ob_total_pop)*100)
      #Percentage of males in the population with this degree in the county
      ob_male_pc_pop = round((ob_male/ob_total_pop)*100)
      #Percentage of females in the population with this degree in the county
      ob_female_pc_pop = round((ob_female/ob_total_pop)*100)
      
      ##Salary change data
      ob_tech = sub$Technical
      ob_adv_cert = sub$Advanced.certficate
      ob_high_cert = sub$Higher.certificate
      ob_ord_deg = sub$Ordinary.degree
      ob_hon_deg = sub$Honours.degree
      ob_post = sub$Postgraduate
      ob_ph_d = sub$Ph.D
      
      ob_tech_to_adv_cert = round(((ob_salary-ob_tech)/ob_tech)*100)
      ob_adv_cert_to_high_cert = round(((ob_high_cert-ob_salary)/ob_salary)*100)
      ob_high_cert_to_ord_deg = round(((ob_ord_deg-ob_high_cert)/ob_high_cert)*100)
      ob_ord_deg_to_hon_deg = round(((ob_hon_deg-ob_ord_deg)/ob_ord_deg)*100)
      ob_hon_deg_to_post = round(((ob_post-ob_hon_deg)/ob_hon_deg)*100)
      ob_post_to_ph_d = round(((ob_ph_d-ob_post)/ob_post)*100)
      
      #Plot data
      ob_pie = c(ob_male, ob_female)
      ob_pop_pie = c(ob_total_pop, ob_total_edu)
      if(is.null(click))
        return()
      else 
        output$text = renderUI({
          county_str = paste("<b>Selected county: </b>", ob_county)
          county_pop_str = paste("<b>County population: </b>", ob_male_pop + ob_female_pop)
          salary_str = paste("<b>Median salary for current education level: </b>", ob_salary)
          male_str = paste("<b>Males with this level of degree in 2016: </b>", ob_male)
          female_str = paste("<b>Females with this level of degree in 2016: </b>", ob_female)
          change_str = paste("<b>Salary % change with each education level: </b>", "<br/>",
                             "Technical to Advanced Certificate: ", ob_tech_to_adv_cert,"%", "<br/>",
                             "Advanced Cert to Higher Cert: ", ob_adv_cert_to_high_cert, "%", "<br/>",
                             "Higher Cert to Ordinary Degree: ", ob_high_cert_to_ord_deg, "%", "<br/>",
                             "Ordinary Degree to Honours Degree: ", ob_ord_deg_to_hon_deg, "%", "<br/>",
                             "Honours Degree to Postgraduate: ", ob_hon_deg_to_post, "%", "<br/>",
                             "Postgraduate to Ph.D: ", ob_post_to_ph_d, "%")
          HTML(paste(county_str, county_pop_str, salary_str, male_str, female_str, change_str, sep = '<br/>'))})
      
      output$genderPie = renderPlot({
        total = ob_male + ob_female
        female_pct = round((ob_female)/sum(total)*100)
        male_pct = round((ob_male)/sum(total)*100)
        female_lbl = paste(female_pct) # add percents to label
        female_lbl = paste(female_pct, "% Female", sep="") # add % to label
        male_lbl = paste(male_pct) # add percents to label
        male_lbl = paste(male_pct, "% Male", sep="") # add % to label
        pie(ob_pie, labels = c(male_lbl, female_lbl), main = "Males/Females with this degree in the county") 
      })
      
      output$popPie = renderPlot({
        lbl = paste(ob_total_pc_pop) # add percents to label
        lbl = paste(ob_total_pc_pop, "% with this degree", sep="") # add % to label
        pie(ob_pop_pie, labels = lbl, main = "Percentage of people with this degree in the county") 
      })
    }
    else if(input$education == "Higher Certificate"){
      click = input$mymap_shape_click
      sub = oo[oo$County==input$mymap_shape_click$id, c("County", "Technical","Advanced.certficate", "Higher.certificate", "Ordinary.degree", "Honours.degree", "Postgraduate", "Ph.D", "Higher.certificate.Male", "Higher.certificate.Female", "Males", "Females")]
      #County
      ob_county = sub$County
      #Median Salary
      ob_salary = sub$Higher.certificate
      #Number of males with this degree
      ob_male = sub$Higher.certificate.Male
      #Number of females with this degree
      ob_female = sub$Higher.certificate.Female
      #Number of males in the county
      ob_male_pop = sub$Males
      #Number of females in the county
      ob_female_pop = sub$Females
      #Total population of the county
      ob_total_pop = ob_male_pop + ob_female_pop
      #Total number of people with this degree in the county
      ob_total_edu = ob_male + ob_female
      #Percentage of the county with this degree in the county
      ob_total_pc_pop = round((ob_total_edu/ob_total_pop)*100)
      #Percentage of males in the population with this degree in the county
      ob_male_pc_pop = round((ob_male/ob_total_pop)*100)
      #Percentage of females in the population with this degree in the county
      ob_female_pc_pop = round((ob_female/ob_total_pop)*100)
      
      ##Salary change data
      ob_tech = sub$Technical
      ob_adv_cert = sub$Advanced.certficate
      ob_high_cert = sub$Higher.certificate
      ob_ord_deg = sub$Ordinary.degree
      ob_hon_deg = sub$Honours.degree
      ob_post = sub$Postgraduate
      ob_ph_d = sub$Ph.D
      
      ob_tech_to_adv_cert = round(((ob_adv_cert-ob_tech)/ob_tech)*100)
      ob_adv_cert_to_high_cert = round(((ob_high_cert-ob_adv_cert)/ob_adv_cert)*100)
      ob_high_cert_to_ord_deg = round(((ob_ord_deg-ob_high_cert)/ob_high_cert)*100)
      ob_ord_deg_to_hon_deg = round(((ob_hon_deg-ob_ord_deg)/ob_ord_deg)*100)
      ob_hon_deg_to_post = round(((ob_post-ob_hon_deg)/ob_hon_deg)*100)
      ob_post_to_ph_d = round(((ob_ph_d-ob_post)/ob_post)*100)
      
      #Plot data
      ob_pie = c(ob_male, ob_female)
      ob_pop_pie = c(ob_total_pop, ob_total_edu)
      if(is.null(click))
        return()
      else 
        output$text = renderUI({
          county_str = paste("<b>Selected county: </b>", ob_county)
          county_pop_str = paste("<b>County population: </b>", ob_male_pop + ob_female_pop)
          salary_str = paste("<b>Median salary for current education level: </b>", ob_salary)
          male_str = paste("<b>Males with this level of degree in 2016: </b>", ob_male)
          female_str = paste("<b>Females with this level of degree in 2016: </b>", ob_female)
          change_str = paste("<b>Salary % change with each education level: </b>", "<br/>",
                             "Technical to Advanced Certificate: ", ob_tech_to_adv_cert,"%", "<br/>",
                             "Advanced Cert to Higher Cert: ", ob_adv_cert_to_high_cert, "%", "<br/>",
                             "Higher Cert to Ordinary Degree: ", ob_high_cert_to_ord_deg, "%", "<br/>",
                             "Ordinary Degree to Honours Degree: ", ob_ord_deg_to_hon_deg, "%", "<br/>",
                             "Honours Degree to Postgraduate: ", ob_hon_deg_to_post, "%", "<br/>",
                             "Postgraduate to Ph.D: ", ob_post_to_ph_d, "%")
          HTML(paste(county_str, county_pop_str, salary_str, male_str, female_str, change_str, sep = '<br/>'))})
      
      output$genderPie = renderPlot({
        total = ob_male + ob_female
        female_pct = round((ob_female)/sum(total)*100)
        male_pct = round((ob_male)/sum(total)*100)
        female_lbl = paste(female_pct) # add percents to label
        female_lbl = paste(female_pct, "% Female", sep="") # add % to label
        male_lbl = paste(male_pct) # add percents to label
        male_lbl = paste(male_pct, "% Male", sep="") # add % to label
        pie(ob_pie, labels = c(male_lbl, female_lbl), main = "Males/Females with this degree in the county") 
      })
      
      output$popPie = renderPlot({
        lbl = paste(ob_total_pc_pop) # add percents to label
        lbl = paste(ob_total_pc_pop, "% with this degree", sep="") # add % to label
        pie(ob_pop_pie, labels = lbl, main = "Percentage of people with this degree in the county") 
      })
    }
    else if(input$education == "Ordinary Degree"){
      click = input$mymap_shape_click
      sub = oo[oo$County==input$mymap_shape_click$id, c("County", "Technical","Advanced.certficate", "Higher.certificate", "Ordinary.degree", "Honours.degree", "Postgraduate", "Ph.D", "Ordinary.degree.Male", "Ordinary.degree.Female", "Males", "Females")]
      #County
      ob_county = sub$County
      #Median Salary
      ob_salary = sub$Ordinary.degree
      #Number of males with this degree
      ob_male = sub$Ordinary.degree.Male
      #Number of females with this degree
      ob_female = sub$Ordinary.degree.Female
      #Number of males in the county
      ob_male_pop = sub$Males
      #Number of females in the county
      ob_female_pop = sub$Females
      #Total population of the county
      ob_total_pop = ob_male_pop + ob_female_pop
      #Total number of people with this degree in the county
      ob_total_edu = ob_male + ob_female
      #Percentage of the county with this degree in the county
      ob_total_pc_pop = round((ob_total_edu/ob_total_pop)*100)
      #Percentage of males in the population with this degree in the county
      ob_male_pc_pop = round((ob_male/ob_total_pop)*100)
      #Percentage of females in the population with this degree in the county
      ob_female_pc_pop = round((ob_female/ob_total_pop)*100)
      
      ##Salary change data
      ob_tech = sub$Technical
      ob_adv_cert = sub$Advanced.certficate
      ob_high_cert = sub$Higher.certificate
      ob_ord_deg = sub$Ordinary.degree
      ob_hon_deg = sub$Honours.degree
      ob_post = sub$Postgraduate
      ob_ph_d = sub$Ph.D
      
      ob_tech_to_adv_cert = round(((ob_adv_cert-ob_tech)/ob_tech)*100)
      ob_adv_cert_to_high_cert = round(((ob_high_cert-ob_adv_cert)/ob_adv_cert)*100)
      ob_high_cert_to_ord_deg = round(((ob_ord_deg-ob_high_cert)/ob_high_cert)*100)
      ob_ord_deg_to_hon_deg = round(((ob_hon_deg-ob_ord_deg)/ob_ord_deg)*100)
      ob_hon_deg_to_post = round(((ob_post-ob_hon_deg)/ob_hon_deg)*100)
      ob_post_to_ph_d = round(((ob_ph_d-ob_post)/ob_post)*100)
      
      #Plot data
      ob_pie = c(ob_male, ob_female)
      ob_pop_pie = c(ob_total_pop, ob_total_edu)
      if(is.null(click))
        return()
      else 
        output$text = renderUI({
          county_str = paste("<b>Selected county: </b>", ob_county)
          county_pop_str = paste("<b>County population: </b>", ob_male_pop + ob_female_pop)
          salary_str = paste("<b>Median salary for current education level: </b>", ob_salary)
          male_str = paste("<b>Males with this level of degree in 2016: </b>", ob_male)
          female_str = paste("<b>Females with this level of degree in 2016: </b>", ob_female)
          change_str = paste("<b>Salary % change with each education level: </b>", "<br/>",
                             "Technical to Advanced Certificate: ", ob_tech_to_adv_cert,"%", "<br/>",
                             "Advanced Cert to Higher Cert: ", ob_adv_cert_to_high_cert, "%", "<br/>",
                             "Higher Cert to Ordinary Degree: ", ob_high_cert_to_ord_deg, "%", "<br/>",
                             "Ordinary Degree to Honours Degree: ", ob_ord_deg_to_hon_deg, "%", "<br/>",
                             "Honours Degree to Postgraduate: ", ob_hon_deg_to_post, "%", "<br/>",
                             "Postgraduate to Ph.D: ", ob_post_to_ph_d, "%")
          HTML(paste(county_str, county_pop_str, salary_str, male_str, female_str, change_str, sep = '<br/>'))})
      
      output$genderPie = renderPlot({
        total = ob_male + ob_female
        female_pct = round((ob_female)/sum(total)*100)
        male_pct = round((ob_male)/sum(total)*100)
        female_lbl = paste(female_pct) # add percents to label
        female_lbl = paste(female_pct, "% Female", sep="") # add % to label
        male_lbl = paste(male_pct) # add percents to label
        male_lbl = paste(male_pct, "% Male", sep="") # add % to label
        pie(ob_pie, labels = c(male_lbl, female_lbl), main = "Males/Females with this degree in the county") 
      })
      
      output$popPie = renderPlot({
        lbl = paste(ob_total_pc_pop) # add percents to label
        lbl = paste(ob_total_pc_pop, "% with this degree", sep="") # add % to label
        pie(ob_pop_pie, labels = lbl, main = "Percentage of people with this degree in the county") 
      })
    }
    else if(input$education == "Honours Degree"){
      click = input$mymap_shape_click
      sub = oo[oo$County==input$mymap_shape_click$id, c("County", "Technical","Advanced.certficate", "Higher.certificate", "Ordinary.degree", "Honours.degree", "Postgraduate", "Ph.D", "Honours.degree.Male", "Honours.degree.Female", "Males", "Females")]
      #County
      ob_county = sub$County
      #Median Salary
      ob_salary = sub$Honours.degree
      #Number of males with this degree
      ob_male = sub$Honours.degree.Male
      #Number of females with this degree
      ob_female = sub$Honours.degree.Female
      #Number of males in the county
      ob_male_pop = sub$Males
      #Number of females in the county
      ob_female_pop = sub$Females
      #Total population of the county
      ob_total_pop = ob_male_pop + ob_female_pop
      #Total number of people with this degree in the county
      ob_total_edu = ob_male + ob_female
      #Percentage of the county with this degree in the county
      ob_total_pc_pop = round((ob_total_edu/ob_total_pop)*100)
      #Percentage of males in the population with this degree in the county
      ob_male_pc_pop = round((ob_male/ob_total_pop)*100)
      #Percentage of females in the population with this degree in the county
      ob_female_pc_pop = round((ob_female/ob_total_pop)*100)
      
      ##Salary change data
      ob_tech = sub$Technical
      ob_adv_cert = sub$Advanced.certficate
      ob_high_cert = sub$Higher.certificate
      ob_ord_deg = sub$Ordinary.degree
      ob_hon_deg = sub$Honours.degree
      ob_post = sub$Postgraduate
      ob_ph_d = sub$Ph.D
      
      ob_tech_to_adv_cert = round(((ob_adv_cert-ob_tech)/ob_tech)*100)
      ob_adv_cert_to_high_cert = round(((ob_high_cert-ob_adv_cert)/ob_adv_cert)*100)
      ob_high_cert_to_ord_deg = round(((ob_ord_deg-ob_high_cert)/ob_high_cert)*100)
      ob_ord_deg_to_hon_deg = round(((ob_hon_deg-ob_ord_deg)/ob_ord_deg)*100)
      ob_hon_deg_to_post = round(((ob_post-ob_hon_deg)/ob_hon_deg)*100)
      ob_post_to_ph_d = round(((ob_ph_d-ob_post)/ob_post)*100)
      
      #Plot data
      ob_pie = c(ob_male, ob_female)
      ob_pop_pie = c(ob_total_pop, ob_total_edu)
      if(is.null(click))
        return()
      else 
        output$text = renderUI({
          county_str = paste("<b>Selected county: </b>", ob_county)
          county_pop_str = paste("<b>County population: </b>", ob_male_pop + ob_female_pop)
          salary_str = paste("<b>Median salary for current education level: </b>", ob_salary)
          male_str = paste("<b>Males with this level of degree in 2016: </b>", ob_male)
          female_str = paste("<b>Females with this level of degree in 2016: </b>", ob_female)
          change_str = paste("<b>Salary % change with each education level: </b>", "<br/>",
                             "Technical to Advanced Certificate: ", ob_tech_to_adv_cert,"%", "<br/>",
                             "Advanced Cert to Higher Cert: ", ob_adv_cert_to_high_cert, "%", "<br/>",
                             "Higher Cert to Ordinary Degree: ", ob_high_cert_to_ord_deg, "%", "<br/>",
                             "Ordinary Degree to Honours Degree: ", ob_ord_deg_to_hon_deg, "%", "<br/>",
                             "Honours Degree to Postgraduate: ", ob_hon_deg_to_post, "%", "<br/>",
                             "Postgraduate to Ph.D: ", ob_post_to_ph_d, "%")
          HTML(paste(county_str, county_pop_str, salary_str, male_str, female_str, change_str, sep = '<br/>'))})
      
      output$genderPie = renderPlot({
        total = ob_male + ob_female
        female_pct = round((ob_female)/sum(total)*100)
        male_pct = round((ob_male)/sum(total)*100)
        female_lbl = paste(female_pct) # add percents to label
        female_lbl = paste(female_pct, "% Female", sep="") # add % to label
        male_lbl = paste(male_pct) # add percents to label
        male_lbl = paste(male_pct, "% Male", sep="") # add % to label
        pie(ob_pie, labels = c(male_lbl, female_lbl), main = "Males/Females with this degree in the county") 
      })
      
      output$popPie = renderPlot({
        lbl = paste(ob_total_pc_pop) # add percents to label
        lbl = paste(ob_total_pc_pop, "% with this degree", sep="") # add % to label
        pie(ob_pop_pie, labels = lbl, main = "Percentage of people with this degree in the county") 
      })
    }
    else if(input$education == "Postgraduate"){
      click = input$mymap_shape_click
      sub = oo[oo$County==input$mymap_shape_click$id, c("County", "Technical","Advanced.certficate", "Higher.certificate", "Ordinary.degree", "Honours.degree", "Postgraduate", "Ph.D", "Postgraduate.Male", "Postgraduate.Female", "Males", "Females")]
      #County
      ob_county = sub$County
      #Median Salary
      ob_salary = sub$Postgraduate
      #Number of males with this degree
      ob_male = sub$Postgraduate.Male
      #Number of females with this degree
      ob_female = sub$Postgraduate.Female
      #Number of males in the county
      ob_male_pop = sub$Males
      #Number of females in the county
      ob_female_pop = sub$Females
      #Total population of the county
      ob_total_pop = ob_male_pop + ob_female_pop
      #Total number of people with this degree in the county
      ob_total_edu = ob_male + ob_female
      #Percentage of the county with this degree in the county
      ob_total_pc_pop = round((ob_total_edu/ob_total_pop)*100)
      #Percentage of males in the population with this degree in the county
      ob_male_pc_pop = round((ob_male/ob_total_pop)*100)
      #Percentage of females in the population with this degree in the county
      ob_female_pc_pop = round((ob_female/ob_total_pop)*100)
      
      ##Salary change data
      ob_tech = sub$Technical
      ob_adv_cert = sub$Advanced.certficate
      ob_high_cert = sub$Higher.certificate
      ob_ord_deg = sub$Ordinary.degree
      ob_hon_deg = sub$Honours.degree
      ob_post = sub$Postgraduate
      ob_ph_d = sub$Ph.D
      
      ob_tech_to_adv_cert = round(((ob_adv_cert-ob_tech)/ob_tech)*100)
      ob_adv_cert_to_high_cert = round(((ob_high_cert-ob_adv_cert)/ob_adv_cert)*100)
      ob_high_cert_to_ord_deg = round(((ob_ord_deg-ob_high_cert)/ob_high_cert)*100)
      ob_ord_deg_to_hon_deg = round(((ob_hon_deg-ob_ord_deg)/ob_ord_deg)*100)
      ob_hon_deg_to_post = round(((ob_post-ob_hon_deg)/ob_hon_deg)*100)
      ob_post_to_ph_d = round(((ob_ph_d-ob_post)/ob_post)*100)
      
      #Plot data
      ob_pie = c(ob_male, ob_female)
      ob_pop_pie = c(ob_total_pop, ob_total_edu)
      if(is.null(click))
        return()
      else 
        output$text = renderUI({
          county_str = paste("<b>Selected county: </b>", ob_county)
          county_pop_str = paste("<b>County population: </b>", ob_male_pop + ob_female_pop)
          salary_str = paste("<b>Median salary for current education level: </b>", ob_salary)
          male_str = paste("<b>Males with this level of degree in 2016: </b>", ob_male)
          female_str = paste("<b>Females with this level of degree in 2016: </b>", ob_female)
          change_str = paste("<b>Salary % change with each education level: </b>", "<br/>",
                             "Technical to Advanced Certificate: ", ob_tech_to_adv_cert,"%", "<br/>",
                             "Advanced Cert to Higher Cert: ", ob_adv_cert_to_high_cert, "%", "<br/>",
                             "Higher Cert to Ordinary Degree: ", ob_high_cert_to_ord_deg, "%", "<br/>",
                             "Ordinary Degree to Honours Degree: ", ob_ord_deg_to_hon_deg, "%", "<br/>",
                             "Honours Degree to Postgraduate: ", ob_hon_deg_to_post, "%", "<br/>",
                             "Postgraduate to Ph.D: ", ob_post_to_ph_d, "%")
          HTML(paste(county_str, county_pop_str, salary_str, male_str, female_str, change_str, sep = '<br/>'))})
      
      output$genderPie = renderPlot({
        total = ob_male + ob_female
        female_pct = round((ob_female)/sum(total)*100)
        male_pct = round((ob_male)/sum(total)*100)
        female_lbl = paste(female_pct) # add percents to label
        female_lbl = paste(female_pct, "% Female", sep="") # add % to label
        male_lbl = paste(male_pct) # add percents to label
        male_lbl = paste(male_pct, "% Male", sep="") # add % to label
        pie(ob_pie, labels = c(male_lbl, female_lbl), main = "Males/Females with this degree in the county") 
      })
      
      output$popPie = renderPlot({
        lbl = paste(ob_total_pc_pop) # add percents to label
        lbl = paste(ob_total_pc_pop, "% with this degree", sep="") # add % to label
        pie(ob_pop_pie, labels = lbl, main = "Percentage of people with this degree in the county") 
      })
    }
    else if(input$education == "Ph.D"){
      click = input$mymap_shape_click
      sub = oo[oo$County==input$mymap_shape_click$id, c("County", "Technical","Advanced.certficate", "Higher.certificate", "Ordinary.degree", "Honours.degree", "Postgraduate", "Ph.D", "Ph.D.Male", "Ph.D.Female", "Males", "Females")]
      #County
      ob_county = sub$County
      #Median Salary
      ob_salary = sub$Ph.D
      #Number of males with this degree
      ob_male = sub$Ph.D.Male
      #Number of females with this degree
      ob_female = sub$Ph.D.Female
      #Number of males in the county
      ob_male_pop = sub$Males
      #Number of females in the county
      ob_female_pop = sub$Females
      #Total population of the county
      ob_total_pop = ob_male_pop + ob_female_pop
      #Total number of people with this degree in the county
      ob_total_edu = ob_male + ob_female
      #Percentage of the county with this degree in the county
      ob_total_pc_pop = ((ob_total_edu/ob_total_pop)*100)
      #Percentage of males in the population with this degree in the county
      ob_male_pc_pop = round((ob_male/ob_total_pop)*100)
      #Percentage of females in the population with this degree in the county
      ob_female_pc_pop = round((ob_female/ob_total_pop)*100)
      
      ##Salary change data
      ob_tech = sub$Technical
      ob_adv_cert = sub$Advanced.certficate
      ob_high_cert = sub$Higher.certificate
      ob_ord_deg = sub$Ordinary.degree
      ob_hon_deg = sub$Honours.degree
      ob_post = sub$Postgraduate
      ob_ph_d = sub$Ph.D
      
      ob_tech_to_adv_cert = round(((ob_adv_cert-ob_tech)/ob_tech)*100)
      ob_adv_cert_to_high_cert = round(((ob_high_cert-ob_adv_cert)/ob_adv_cert)*100)
      ob_high_cert_to_ord_deg = round(((ob_ord_deg-ob_high_cert)/ob_high_cert)*100)
      ob_ord_deg_to_hon_deg = round(((ob_hon_deg-ob_ord_deg)/ob_ord_deg)*100)
      ob_hon_deg_to_post = round(((ob_post-ob_hon_deg)/ob_hon_deg)*100)
      ob_post_to_ph_d = round(((ob_ph_d-ob_post)/ob_post)*100)
      
      #Plot data
      ob_pie = c(ob_male, ob_female)
      ob_pop_pie = c(ob_total_pop, ob_total_edu)
      if(is.null(click))
        return()
      else 
        output$text = renderUI({
          county_str = paste("<b>Selected county: </b>", ob_county)
          county_pop_str = paste("<b>County population: </b>", ob_male_pop + ob_female_pop)
          salary_str = paste("<b>Median salary for current education level: </b>", ob_salary)
          male_str = paste("<b>Males with this level of degree in 2016: </b>", ob_male)
          female_str = paste("<b>Females with this level of degree in 2016: </b>", ob_female)
          change_str = paste("<b>Salary % change with each education level: </b>", "<br/>",
                             "Technical to Advanced Certificate: ", ob_tech_to_adv_cert,"%", "<br/>",
                             "Advanced Cert to Higher Cert: ", ob_adv_cert_to_high_cert, "%", "<br/>",
                             "Higher Cert to Ordinary Degree: ", ob_high_cert_to_ord_deg, "%", "<br/>",
                             "Ordinary Degree to Honours Degree: ", ob_ord_deg_to_hon_deg, "%", "<br/>",
                             "Honours Degree to Postgraduate: ", ob_hon_deg_to_post, "%", "<br/>",
                             "Postgraduate to Ph.D: ", ob_post_to_ph_d, "%")
          HTML(paste(county_str, county_pop_str, salary_str, male_str, female_str, change_str, sep = '<br/>'))})
      
      output$genderPie = renderPlot({
        total = ob_male + ob_female
        female_pct = round((ob_female)/sum(total)*100)
        male_pct = round((ob_male)/sum(total)*100)
        female_lbl = paste(female_pct) # add percents to label
        female_lbl = paste(female_pct, "% Female", sep="") # add % to label
        male_lbl = paste(male_pct) # add percents to label
        male_lbl = paste(male_pct, "% Male", sep="") # add % to label
        pie(ob_pie, labels = c(male_lbl, female_lbl), main = "Males/Females with this degree in the county") 
      })
      
      output$popPie = renderPlot({
        lbl = paste(ob_total_pc_pop) # add percents to label
        lbl = paste(ob_total_pc_pop, "% with this degree", sep="") # add % to label
        pie(ob_pop_pie, labels = lbl, main = "Percentage of people with this degree in the county") 
      })
    }
  })
}
shinyApp(ui, server)