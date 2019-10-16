library(ggplot2)
library(shiny) 
library(plotly) 
library(dplyr)
library(DT)
library(rsconnect)

shinyServer(function(input, output) {

#########################  DATA FOR PROTEIN  ######################### 
  
  tbl_1 <- reactive({
    
    hvm_protein = read.csv('hvm_protein.csv', header = T)
    tbl_1 = hvm_protein %>% filter(food_type %in% input$food_type_pro)
    wght = input$weight_pro
    
    tbl_1 = tbl_1 %>%
      select(food_type, land_use, freshwater, GHG_emissions) %>%
      mutate(water.rate = wght * round((freshwater/2204.62) * 264.172 * 0.001, 2)) %>% # 264.172 gallon = 1 cubic meter 
      mutate(CO2.emission = wght * round((GHG_emissions/2204.62) * 2.20462, 2)) %>% # 1 tonne = 2204.62 lbs = 2.20462 thousand lbs
      mutate(land.use = wght * round((land_use/2204.62) * 2.47105, 2)) %>% # 1 ha = 2.47105 acres
      select(-c(land_use, freshwater, GHG_emissions))
    
    tbl_1
  })

#########################  DATA FOR KCAL  ######################### 
  
  tbl_2 <- reactive({
    
    hvm_calories = read.csv('hvm_calories.csv', header = T)
  
    tbl_2 =  hvm_calories %>% filter(food_type %in% input$food_type_kcal)
    wght = input$weight_cal
    
    tbl_2 = tbl_2 %>%
      select(food_type, land_use, freshwater, GHG_emissions) %>%
      mutate(water.rate = wght * round(freshwater * 0.001 * 264.172, 2)) %>%
      mutate(CO2.emission = wght * round(GHG_emissions * 0.001 * 2204.62, 2)) %>%
      mutate(land.use = wght * round(land_use * 0.001 * 2.47105, 2)) %>%
      select(-c(land_use, freshwater, GHG_emissions))
    
    tbl_2
  })

#########################  DATA FOR BEEF AND CUT  #########################  

  rad_all <- reactive({
    
    hvm_carcass = read.csv('carcass_calculator_data.csv', header = T)
    
    wght = input$weight_beef
    water.rate <- 6.355078 # (million gallons)
    co2.emision <- 102.959 # (thousand lbs/animal)
    land.use <- 77 # (acres)
    
    rad_all = hvm_carcass %>%
      mutate(number.cows = ceiling(wght/total_weight)) %>% 
      mutate(water.rate = water.rate * number.cows) %>% 
      mutate(CO2.emission = co2.emision * number.cows) %>% 
      mutate(land.use = land.use * number.cows) %>%
      select(-c(total_weight))
    
    rad_all
  })
  
  
#########################  PROTEIN  #########################
  
  output$tbl_protein <- renderDT({
    
    tbl = tbl_1()
    colnames(tbl) = c('Food', 'Water Consumption (kgal)', 'CO2 Emission (thousand lbs)', 'Land Use (acres)')
    tbl
    
  })
  
  output$pro.all <- renderPlotly({
    
    pro.tbl = rbind(cbind(tbl_1()[1], Value = round(unname(tbl_1()[2]), 2), Type = 'Water (mgal)'),
                    cbind(tbl_1()[1], Value = round(unname(tbl_1()[3]), 2), Type = 'CO2 (klbs)'),
                    cbind(tbl_1()[1], Value = round(unname(tbl_1()[4]), 2), Type = 'Land (acres)'))
    
    pro.plot = ggplot(data = pro.tbl, aes(x = reorder(food_type, desc(Value)), y = Value)) + 
      geom_bar(stat = 'identity', aes(fill = Type)) + 
      scale_fill_manual(values = c('dodgerblue', 'dodgerblue2', 'dodgerblue4')) +
      facet_grid(Type ~ ., scales = "free_y") + 
      labs(x = 'Food', y = '') + 
      theme_bw() + 
      theme(axis.text.x = element_text(size = 9, angle = 45), 
            axis.title.x = element_blank(), axis.title.y = element_blank(),
            legend.position = 'none')
    
    ggplotly(pro.plot, height = 550, width = 600) %>%
      style(hoverlabel = list(bgcolor = "white"))
    
  })

#########################  KCAL  #########################
  
  output$tbl_kcal <- renderDT({

    tbl <- tbl_2()
    colnames(tbl) = c('Food', 'Water Consumption (gallon)', 'CO2 Emission (lbs)', 'Land Use (acres)')
    tbl
    
  })
  
  output$kcal.all <- renderPlotly({
    
    kcal.tbl = rbind(cbind(tbl_2()[1], Value = round(unname(tbl_2()[2]), 2), Type = 'Water (mgal)'),
                     cbind(tbl_2()[1], Value = round(unname(tbl_2()[3]), 2), Type = 'CO2 (klbs)'),
                     cbind(tbl_2()[1], Value = round(unname(tbl_2()[4]), 2), Type = 'Land (acres)'))
    
    kcal.plot = ggplot(data = kcal.tbl, aes(x = reorder(food_type, desc(Value)), y = Value)) + 
      geom_bar(stat = 'identity', aes(fill = Type)) + 
      scale_fill_manual(values = c('dodgerblue', 'dodgerblue2', 'dodgerblue4')) +
      facet_grid(Type ~ ., scales = "free_y") + 
      theme_bw() + 
      theme(axis.text.x = element_text(size = 9, angle = 45), 
            axis.title.x = element_blank(), axis.title.y = element_blank(),
            legend.position = 'none')
    
    ggplotly(kcal.plot, height = 550, width = 600) %>%
      style(hoverlabel = list(bgcolor = "white"))
    
  })

#########################  BEEF CUT --- TABLE  #########################
  
  output$tbl_cut <- renderDT({
    
    wght = input$weight_beef
    
    input_water = rad_all()$water.rate[rad_all()$cut == input$cut]
    input_co2 = rad_all()$CO2.emission[rad_all()$cut == input$cut]
    input_land = rad_all()$land.use[rad_all()$cut == input$cut]
    
    rad_all_comp = rad_all() %>% 
      mutate(meat = (input_water/water.rate) * wght)
    
    colnames(rad_all_comp) = c('Cut', 'Number of Animal', 'Water Consumption (Mgal)', 'CO2 Emission (thousand lbs)', 
                               'Land Use (acres)', 'Weight of meat with same water consumption (lbs)')
    rad_all_comp[2:6] <- format(round(rad_all_comp[2:6], 2), big.mark = ',')
    rad_all_comp

  })
  
#########################  BEEF CUT --- PLOT  #########################
  
  output$tbl_cut_point <- renderPlotly({
    
    input_water = rad_all()$water.rate[rad_all()$cut == input$cut]
    wght = input$weight_beef
    rad_all_comp = rad_all() %>% 
      mutate(meat = (input_water/water.rate) * wght)
    
    plot_all <- plot_ly(rad_all_comp, x = ~ water.rate, y = ~ CO2.emission, z = ~ land.use, 
                        hoverinfo = 'text',
                        text = ~ paste('</br> Cut: ', cut, 
                                       '</br> Number of Animal: ', number.cows,
                                       '</br> Water: ', format(round(water.rate, 2), big.mark = ','), 'Mgal',
                                       '</br> CO2: ', format(round(CO2.emission, 2), big.mark = ','), 'Kips',
                                       '</br> Land: ', format(round(land.use, 2), big.mark = ','), 'Acres',
                                       '</br> Alternative weight: ', format(round(meat, 2), big.mark = ','), 'lbs'),
                        marker = list(color = ~ meat, colorscale = c('RdBu'), showscale = T)) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = 'Water Consumption in Mgal',
                                       showgrid = T, zeroline = F, tickcolor = 'markers'),
                          yaxis = list(title = 'CO2 Emission in thousand lbs',
                                       showgrid = T, zeroline = F),
                          zaxis = list(title = 'Land Use in acres',
                                       showgrid = T, zeroline = F)),
             annotations = list(
               x = 1.15, y = 1.05,
               text = 'Alternative cut in lbs',
               xref = 'paper', yref = 'paper',
               showarrow = F
             ))
    plot_all
  })
  
  
  output$tbl_cut_bar <- renderPlotly({
    
    rad_all_1 = rbind(cbind(rad_all()[1], Value = round(unname(rad_all()[2]),2), Type = 'Number of animal'),
                      cbind(rad_all()[1], Value = round(unname(rad_all()[3]),2), Type = 'Water (mgal)'),
                      cbind(rad_all()[1], Value = round(unname(rad_all()[4]),2), Type = 'CO2 (klbs)'),
                      cbind(rad_all()[1], Value = round(unname(rad_all()[5]),2), Type = 'Land (acres)'))
    
    plot = ggplot(data = rad_all_1, aes(x = reorder(cut, desc(Value)), y = Value)) + 
      geom_bar(stat = 'identity', aes(fill = Type)) + 
      facet_grid(Type ~ ., scales = "free_y") + 
      scale_fill_manual(values = c('dodgerblue', 'dodgerblue1', 'dodgerblue2', 'dodgerblue3')) +
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 60), axis.title.x = element_blank(), axis.title.y = element_blank(),
            legend.position = 'none')
    
    ggplotly(plot, height = 600)%>%
      style(hoverlabel = list(bgcolor = "white"))
  })
})