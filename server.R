library(rsconnect)
library(sf)
library(spatstat)
library(sp)
library(rgdal)
library(maptools)
library(ggmap)
library(RColorBrewer)
library(dplyr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(rgeoda)

source("02_R Functions/KDE_India.R")
s.sfIndia <- readShapeSpatial("03_Additional Files/India_Country_shape_file/india.shp")
s.owinIndia <- as(s.sfIndia, "owin")

s.sfKar <- readShapeSpatial("03_Additional Files/Karnataka_shape_file/Karnataka_SUWOvOb.shp")
s.owinKarnataka <- as(s.sfKar, "owin")

India_Malnutrition <-readShapeSpatial("03_Additional Files/India_District_shape_file/India_SUWOvOb.shp")


function(session, input, output) {
  
  # Reactive element for Radio Input data
  MalCond <- reactive({
    input$Indiaradio
  }) %>%
    bindCache(input$Indiaradio)
  
  
  # Render the KDE of India across malnutrition conditions
  output$KDEIndia <- renderPlot({
    print("Hi")
    
    breakpoints <- c(-50,-40,-30,-20,-10,0,100,200,300,400,600,800,1000,1200, 1400)
    colors<- c("#08519c","#3182bd","#9ecae1","#c6dbef","#f7f7f7","#fff5f0","#fee0d2","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#67000d")
    
    print(input$Indiaradio)
    data.ppp = MalnutrtionKDE(MalCond(), s.owinIndia, "India")
    plot(density(data.ppp, sigma = 1), col=colors, breaks = breakpoints, pch=".", main = paste0(MalCond()," in children under five in India." ))
    contour(density(data.ppp, 1), axes = F, add=TRUE, pch=".")
    plot(s.owinIndia, add=TRUE, las=1, pch=".")
    
  })%>%
    bindCache(input$Indiaradio)
  
  
  # Render the KDE of Karnataka across malnutrition conditions
  output$KDEKarnataka <- renderPlot({
    
    breakpoints<- c(-25,-20,-15,-10,-5,0,50,100,150,200,250,300,350,400,475)
    colors<- c("#08519c","#3182bd","#9ecae1","#c6dbef","#f7f7f7","#fff5f0","#fee0d2","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#67000d")
    
    print(input$Indiaradio)
    MalCond = input$Indiaradio
    # MalCond = "stunt"
    data.ppp = MalnutrtionKDE(MalCond(), s.owinKarnataka, "Karnataka")
    plot(density(data.ppp, sigma = 0.35), col=colors, breaks = breakpoints, pch=".", main = paste0(MalCond()," in children under five in Karnataka."))
    contour(density(data.ppp, 0.35), axes = F, add=TRUE, pch=".")
    plot(s.owinKarnataka, add=TRUE, las=1, pch=".")
    
  })%>%
    bindCache(input$Indiaradio)
  
  QgisHtmlgetPageIndia<-function() {
    return(includeHTML("qgisindexindia.html"))
  }
  
  output$QgisOverlayIndia <- 
    renderUI({QgisHtmlgetPageIndia()})
  
  QgisHtmlgetPageKarnataka<-function() {
    return(includeHTML("qgisindexkarnataka.html"))
  }
  
  output$QgisOverlayKarnataka <- 
    renderUI({QgisHtmlgetPageKarnataka()})
  
  output$LISAStunt <- renderPlot({
    # load data
    ore_sf <- st_as_sf(India_Malnutrition) %>%
      tibble::rownames_to_column("id")
    
    to_clust <- ore_sf %>%
      filter(!is.na(stunt_Coun))
    queen_wts <- queen_weights(to_clust)
    moran <- local_moran(queen_wts, st_drop_geometry(to_clust["stunt_Coun"]))
    moran_lbls <- lisa_labels(moran)
    moran_colors <- setNames(lisa_colors(moran), moran_lbls)
    
    ore_clustered <- to_clust %>%
      st_drop_geometry() %>%
      select(id) %>%
      mutate(cluster_num = lisa_clusters(moran) + 1, # add 1 bc clusters are zero-indexed
             cluster = factor(moran_lbls[cluster_num], levels = moran_lbls)) %>%
      right_join(ore_sf, by = "id") %>%
      st_as_sf()
    
    ggplot(ore_clustered, aes(fill = cluster)) +
      geom_sf(color = "white", size = 0) +
      scale_fill_manual(values = moran_colors, na.value = "green") +
      theme_dark()
  })
  
  output$LISAUnder <- renderPlot({
    # load data
    ore_sf <- st_as_sf(India_Malnutrition) %>%
      tibble::rownames_to_column("id")
    
    to_clust <- ore_sf %>%
      filter(!is.na(under_Coun))
    queen_wts <- queen_weights(to_clust)
    moran <- local_moran(queen_wts, st_drop_geometry(to_clust["under_Coun"]))
    moran_lbls <- lisa_labels(moran)
    moran_colors <- setNames(lisa_colors(moran), moran_lbls)
    
    ore_clustered <- to_clust %>%
      st_drop_geometry() %>%
      select(id) %>%
      mutate(cluster_num = lisa_clusters(moran) + 1, # add 1 bc clusters are zero-indexed
             cluster = factor(moran_lbls[cluster_num], levels = moran_lbls)) %>%
      right_join(ore_sf, by = "id") %>%
      st_as_sf()
    
    ggplot(ore_clustered, aes(fill = cluster)) +
      geom_sf(color = "white", size = 0) +
      scale_fill_manual(values = moran_colors, na.value = "green") +
      theme_dark()
  })
  
  output$LISAWast <- renderPlot({
    # load data
    ore_sf <- st_as_sf(India_Malnutrition) %>%
      tibble::rownames_to_column("id")
    
    to_clust <- ore_sf %>%
      filter(!is.na(wast_Count))
    queen_wts <- queen_weights(to_clust)
    moran <- local_moran(queen_wts, st_drop_geometry(to_clust["wast_Count"]))
    moran_lbls <- lisa_labels(moran)
    moran_colors <- setNames(lisa_colors(moran), moran_lbls)
    
    ore_clustered <- to_clust %>%
      st_drop_geometry() %>%
      select(id) %>%
      mutate(cluster_num = lisa_clusters(moran) + 1, # add 1 bc clusters are zero-indexed
             cluster = factor(moran_lbls[cluster_num], levels = moran_lbls)) %>%
      right_join(ore_sf, by = "id") %>%
      st_as_sf()
    
    ggplot(ore_clustered, aes(fill = cluster)) +
      geom_sf(color = "white", size = 0) +
      scale_fill_manual(values = moran_colors, na.value = "green") +
      theme_dark()
  })
  
  output$LISAOver <- renderPlot({
    # load data
    ore_sf <- st_as_sf(India_Malnutrition) %>%
      tibble::rownames_to_column("id")
    
    to_clust <- ore_sf %>%
      filter(!is.na(overweight))
    queen_wts <- queen_weights(to_clust)
    moran <- local_moran(queen_wts, st_drop_geometry(to_clust["overweight"]))
    moran_lbls <- lisa_labels(moran)
    moran_colors <- setNames(lisa_colors(moran), moran_lbls)
    
    ore_clustered <- to_clust %>%
      st_drop_geometry() %>%
      select(id) %>%
      mutate(cluster_num = lisa_clusters(moran) + 1, # add 1 bc clusters are zero-indexed
             cluster = factor(moran_lbls[cluster_num], levels = moran_lbls)) %>%
      right_join(ore_sf, by = "id") %>%
      st_as_sf()
    
    ggplot(ore_clustered, aes(fill = cluster)) +
      geom_sf(color = "white", size = 0) +
      scale_fill_manual(values = moran_colors, na.value = "green") +
      theme_dark()
  })
  
  output$LISAObese <- renderPlot({
    # load data
    ore_sf <- st_as_sf(India_Malnutrition) %>%
      tibble::rownames_to_column("id")
    
    to_clust <- ore_sf %>%
      filter(!is.na(obese_Coun))
    queen_wts <- queen_weights(to_clust)
    moran <- local_moran(queen_wts, st_drop_geometry(to_clust["obese_Coun"]))
    moran_lbls <- lisa_labels(moran)
    moran_colors <- setNames(lisa_colors(moran), moran_lbls)
    
    ore_clustered <- to_clust %>%
      st_drop_geometry() %>%
      select(id) %>%
      mutate(cluster_num = lisa_clusters(moran) + 1, # add 1 bc clusters are zero-indexed
             cluster = factor(moran_lbls[cluster_num], levels = moran_lbls)) %>%
      right_join(ore_sf, by = "id") %>%
      st_as_sf()
    
    ggplot(ore_clustered, aes(fill = cluster)) +
      geom_sf(color = "white", size = 0) +
      scale_fill_manual(values = moran_colors, na.value = "green") +
      theme_dark()
  })
  
  output$BiLISAStuntUnder <- renderPlot({
    # load data
    ore_sf <- st_as_sf(India_Malnutrition) %>%
      tibble::rownames_to_column("id")
    
    to_clust <- ore_sf %>%
      filter(!is.na(stunt_Coun))%>%
      filter(!is.na(under_Coun))
    queen_wts <- queen_weights(to_clust)
    moran <- local_bimoran(queen_wts, st_drop_geometry(to_clust[c("stunt_Coun","under_Coun")]))
    moran_lbls <- lisa_labels(moran)
    moran_colors <- setNames(lisa_colors(moran), moran_lbls)
    
    ore_clustered <- to_clust %>%
      st_drop_geometry() %>%
      select(id) %>%
      mutate(cluster_num = lisa_clusters(moran) + 1, # add 1 bc clusters are zero-indexed
             cluster = factor(moran_lbls[cluster_num], levels = moran_lbls)) %>%
      right_join(ore_sf, by = "id") %>%
      st_as_sf()
    
    ggplot(ore_clustered, aes(fill = cluster)) +
      geom_sf(color = "white", size = 0) +
      scale_fill_manual(values = moran_colors, na.value = "green") +
      theme_dark()
  })
  
  output$BiLISAStuntWast <- renderPlot({
    # load data
    ore_sf <- st_as_sf(India_Malnutrition) %>%
      tibble::rownames_to_column("id")
    
    to_clust <- ore_sf %>%
      filter(!is.na(stunt_Coun))%>%
      filter(!is.na(wast_Count))
    queen_wts <- queen_weights(to_clust)
    moran <- local_bimoran(queen_wts, st_drop_geometry(to_clust[c("stunt_Coun","wast_Count")]))
    moran_lbls <- lisa_labels(moran)
    moran_colors <- setNames(lisa_colors(moran), moran_lbls)
    
    ore_clustered <- to_clust %>%
      st_drop_geometry() %>%
      select(id) %>%
      mutate(cluster_num = lisa_clusters(moran) + 1, # add 1 bc clusters are zero-indexed
             cluster = factor(moran_lbls[cluster_num], levels = moran_lbls)) %>%
      right_join(ore_sf, by = "id") %>%
      st_as_sf()
    
    ggplot(ore_clustered, aes(fill = cluster)) +
      geom_sf(color = "white", size = 0) +
      scale_fill_manual(values = moran_colors, na.value = "green") +
      theme_dark()
  })
  
  output$BiLISAStuntOver <- renderPlot({
    # load data
    ore_sf <- st_as_sf(India_Malnutrition) %>%
      tibble::rownames_to_column("id")
    
    to_clust <- ore_sf %>%
      filter(!is.na(stunt_Coun))%>%
      filter(!is.na(overweight))
    queen_wts <- queen_weights(to_clust)
    moran <- local_bimoran(queen_wts, st_drop_geometry(to_clust[c("stunt_Coun","overweight")]))
    moran_lbls <- lisa_labels(moran)
    moran_colors <- setNames(lisa_colors(moran), moran_lbls)
    
    ore_clustered <- to_clust %>%
      st_drop_geometry() %>%
      select(id) %>%
      mutate(cluster_num = lisa_clusters(moran) + 1, # add 1 bc clusters are zero-indexed
             cluster = factor(moran_lbls[cluster_num], levels = moran_lbls)) %>%
      right_join(ore_sf, by = "id") %>%
      st_as_sf()
    
    ggplot(ore_clustered, aes(fill = cluster)) +
      geom_sf(color = "white", size = 0) +
      scale_fill_manual(values = moran_colors, na.value = "green") +
      theme_dark()
  })
  
  output$BiLISAStuntObese <- renderPlot({
    # load data
    ore_sf <- st_as_sf(India_Malnutrition) %>%
      tibble::rownames_to_column("id")
    
    to_clust <- ore_sf %>%
      filter(!is.na(stunt_Coun))%>%
      filter(!is.na(obese_Coun))
    queen_wts <- queen_weights(to_clust)
    moran <- local_bimoran(queen_wts, st_drop_geometry(to_clust[c("stunt_Coun","obese_Coun")]))
    moran_lbls <- lisa_labels(moran)
    moran_colors <- setNames(lisa_colors(moran), moran_lbls)
    
    ore_clustered <- to_clust %>%
      st_drop_geometry() %>%
      select(id) %>%
      mutate(cluster_num = lisa_clusters(moran) + 1, # add 1 bc clusters are zero-indexed
             cluster = factor(moran_lbls[cluster_num], levels = moran_lbls)) %>%
      right_join(ore_sf, by = "id") %>%
      st_as_sf()
    
    ggplot(ore_clustered, aes(fill = cluster)) +
      geom_sf(color = "white", size = 0) +
      scale_fill_manual(values = moran_colors, na.value = "green") +
      theme_dark()
  })
  
  output$BiLISAUnderWast <- renderPlot({
    # load data
    ore_sf <- st_as_sf(India_Malnutrition) %>%
      tibble::rownames_to_column("id")
    
    to_clust <- ore_sf %>%
      filter(!is.na(under_Coun))%>%
      filter(!is.na(wast_Count))
    queen_wts <- queen_weights(to_clust)
    moran <- local_bimoran(queen_wts, st_drop_geometry(to_clust[c("under_Coun","wast_Count")]))
    moran_lbls <- lisa_labels(moran)
    moran_colors <- setNames(lisa_colors(moran), moran_lbls)
    
    ore_clustered <- to_clust %>%
      st_drop_geometry() %>%
      select(id) %>%
      mutate(cluster_num = lisa_clusters(moran) + 1, # add 1 bc clusters are zero-indexed
             cluster = factor(moran_lbls[cluster_num], levels = moran_lbls)) %>%
      right_join(ore_sf, by = "id") %>%
      st_as_sf()
    
    ggplot(ore_clustered, aes(fill = cluster)) +
      geom_sf(color = "white", size = 0) +
      scale_fill_manual(values = moran_colors, na.value = "green") +
      theme_dark()
  })
  
  output$BiLISAUnderOver <- renderPlot({
    # load data
    ore_sf <- st_as_sf(India_Malnutrition) %>%
      tibble::rownames_to_column("id")
    
    to_clust <- ore_sf %>%
      filter(!is.na(under_Coun))%>%
      filter(!is.na(overweight))
    queen_wts <- queen_weights(to_clust)
    moran <- local_bimoran(queen_wts, st_drop_geometry(to_clust[c("under_Coun","overweight")]))
    moran_lbls <- lisa_labels(moran)
    moran_colors <- setNames(lisa_colors(moran), moran_lbls)
    
    ore_clustered <- to_clust %>%
      st_drop_geometry() %>%
      select(id) %>%
      mutate(cluster_num = lisa_clusters(moran) + 1, # add 1 bc clusters are zero-indexed
             cluster = factor(moran_lbls[cluster_num], levels = moran_lbls)) %>%
      right_join(ore_sf, by = "id") %>%
      st_as_sf()
    
    ggplot(ore_clustered, aes(fill = cluster)) +
      geom_sf(color = "white", size = 0) +
      scale_fill_manual(values = moran_colors, na.value = "green") +
      theme_dark()
  })
  
  output$BiLISAUnderObese <- renderPlot({
    # load data
    ore_sf <- st_as_sf(India_Malnutrition) %>%
      tibble::rownames_to_column("id")
    
    to_clust <- ore_sf %>%
      filter(!is.na(under_Coun))%>%
      filter(!is.na(obese_Coun))
    queen_wts <- queen_weights(to_clust)
    moran <- local_bimoran(queen_wts, st_drop_geometry(to_clust[c("under_Coun","obese_Coun")]))
    moran_lbls <- lisa_labels(moran)
    moran_colors <- setNames(lisa_colors(moran), moran_lbls)
    
    ore_clustered <- to_clust %>%
      st_drop_geometry() %>%
      select(id) %>%
      mutate(cluster_num = lisa_clusters(moran) + 1, # add 1 bc clusters are zero-indexed
             cluster = factor(moran_lbls[cluster_num], levels = moran_lbls)) %>%
      right_join(ore_sf, by = "id") %>%
      st_as_sf()
    
    ggplot(ore_clustered, aes(fill = cluster)) +
      geom_sf(color = "white", size = 0) +
      scale_fill_manual(values = moran_colors, na.value = "green") +
      theme_dark()
  })
  
  output$BiLISAWastOver <- renderPlot({
    # load data
    ore_sf <- st_as_sf(India_Malnutrition) %>%
      tibble::rownames_to_column("id")
    
    to_clust <- ore_sf %>%
      filter(!is.na(wast_Count))%>%
      filter(!is.na(overweight))
    queen_wts <- queen_weights(to_clust)
    moran <- local_bimoran(queen_wts, st_drop_geometry(to_clust[c("wast_Count","overweight")]))
    moran_lbls <- lisa_labels(moran)
    moran_colors <- setNames(lisa_colors(moran), moran_lbls)
    
    ore_clustered <- to_clust %>%
      st_drop_geometry() %>%
      select(id) %>%
      mutate(cluster_num = lisa_clusters(moran) + 1, # add 1 bc clusters are zero-indexed
             cluster = factor(moran_lbls[cluster_num], levels = moran_lbls)) %>%
      right_join(ore_sf, by = "id") %>%
      st_as_sf()
    
    ggplot(ore_clustered, aes(fill = cluster)) +
      geom_sf(color = "white", size = 0) +
      scale_fill_manual(values = moran_colors, na.value = "green") +
      theme_dark()
  })
  
  output$BiLISAWastObese <- renderPlot({
    # load data
    ore_sf <- st_as_sf(India_Malnutrition) %>%
      tibble::rownames_to_column("id")
    
    to_clust <- ore_sf %>%
      filter(!is.na(wast_Count))%>%
      filter(!is.na(obese_Coun))
    queen_wts <- queen_weights(to_clust)
    moran <- local_bimoran(queen_wts, st_drop_geometry(to_clust[c("wast_Count","obese_Coun")]))
    moran_lbls <- lisa_labels(moran)
    moran_colors <- setNames(lisa_colors(moran), moran_lbls)
    
    ore_clustered <- to_clust %>%
      st_drop_geometry() %>%
      select(id) %>%
      mutate(cluster_num = lisa_clusters(moran) + 1, # add 1 bc clusters are zero-indexed
             cluster = factor(moran_lbls[cluster_num], levels = moran_lbls)) %>%
      right_join(ore_sf, by = "id") %>%
      st_as_sf()
    
    ggplot(ore_clustered, aes(fill = cluster)) +
      geom_sf(color = "white", size = 0) +
      scale_fill_manual(values = moran_colors, na.value = "green") +
      theme_dark()
  })
  
  output$BiLISAOverObese <- renderPlot({
    # load data
    ore_sf <- st_as_sf(India_Malnutrition) %>%
      tibble::rownames_to_column("id")
    
    to_clust <- ore_sf %>%
      filter(!is.na(overweight))%>%
      filter(!is.na(obese_Coun))
    queen_wts <- queen_weights(to_clust)
    moran <- local_bimoran(queen_wts, st_drop_geometry(to_clust[c("overweight","obese_Coun")]))
    moran_lbls <- lisa_labels(moran)
    moran_colors <- setNames(lisa_colors(moran), moran_lbls)
    
    ore_clustered <- to_clust %>%
      st_drop_geometry() %>%
      select(id) %>%
      mutate(cluster_num = lisa_clusters(moran) + 1, # add 1 bc clusters are zero-indexed
             cluster = factor(moran_lbls[cluster_num], levels = moran_lbls)) %>%
      right_join(ore_sf, by = "id") %>%
      st_as_sf()
    
    ggplot(ore_clustered, aes(fill = cluster)) +
      geom_sf(color = "white", size = 0) +
      scale_fill_manual(values = moran_colors, na.value = "green") +
      theme_dark()
  })
  
}
