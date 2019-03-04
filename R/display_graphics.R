# Display Figures for Fire Effects Project
#
# Contains scripts for producing display figures

source("R/0.1_utilities.R")

theme_set(theme_classic(base_size = 18))

# ---------------------------------------------------------------------
# Consumption Output Figure
# Relation between percent canopy mortality and percent consumption


consumption_figure = function(consumption_parameter, title_name){
  
  percent_canopy_mortality <- seq(from=0,to=1,length.out=101)
  
  tmp_func = function (percent_canopy_mortality, consumption_parameter){
    if(consumption_parameter == 1){
      percent_mortality <- percent_canopy_mortality*100
    }
    else {
      percent_mortality <-  (((consumption_parameter^percent_canopy_mortality) - 1)/(consumption_parameter-1)*100)
    }
  }
  
  percent_canopy_mortality_rep <- rep(percent_canopy_mortality, length(consumption_parameter))
  consumption_parameter_rep <- rep(consumption_parameter, each=length(percent_canopy_mortality))
  
  happy <- percent_canopy_mortality_rep %>%
    cbind(percent_canopy_mortality_rep = ., consumption_parameter_rep) %>%
    as.data.frame()
  
  percent_mortality <- mapply(tmp_func,happy$percent_canopy_mortality_rep, happy$consumption_parameter_rep)
  happy <- cbind(happy, percent_mortality)
  happy$consumption_parameter_rep <- as.factor(happy$consumption_parameter_rep)
  
  
  x <- ggplot(data = happy) +
    geom_line(aes(x=percent_canopy_mortality_rep, y=percent_mortality, linetype = consumption_parameter_rep, color = consumption_parameter_rep), size=1.2) +
    #theme_bw(base_size=21) +
    labs(title = title_name, x = "Proportion of Carbon Mortality", y = "Proportion of Mortality Consumed") +
    #scale_linetype(name = "Consumption\nParameter\n(k_consumption)") +
    #scale_color_brewer(palette = "Dark2", name = "Consumption\nParameter\n(k_consumption)") +
    scale_linetype(name = "Consumption\nParameter") +
    scale_color_brewer(palette = "Dark2", name = "Consumption\nParameter") +
    theme(axis.text = element_text(size=18)) +
    NULL
  #plot(x)
  return(x)
}

fig_consumption = consumption_figure(consumption_parameter = c(0.01, 1, 100), title_name = "Consumption")
ggsave("display_consumption.pdf", plot = fig_consumption, path = DISPLAY_FIGURES_DIR,
       width = 7.5, height = 4.5)


# ---------------------------------------------------------------------
# Consumption Output Figures (These figures are no longer used)


consumption_output_figure = function(consumption_parameter, title_name){

  percent_mortality <- seq(from=0,to=1,length.out=101)
  percent_consumed <- vector()
  
  for(aa in seq_along(percent_mortality)){
    if(consumption_parameter == 1){
      percent_consumed[aa] <- percent_mortality[aa] * percent_mortality[aa]
    }
    else {
      percent_consumed[aa] <- percent_mortality[aa] * ((consumption_parameter^percent_mortality[aa]) - 1)/(consumption_parameter-1)
    }
  }
  
  percent_litter <- percent_mortality - percent_consumed

  happy = percent_mortality %>%
    cbind(percent_consumed, percent_litter) %>%
    `*`(100) %>%
    as.data.frame() %>%
    gather("carbon_pool", "Percent", 2:3)
  
  x <- ggplot(data = happy) +
    geom_line(aes(x=., y=Percent, linetype = carbon_pool, color = carbon_pool), size=1.2) +
    #theme_bw(base_size=16) +
    labs(title = title_name, x = "Canopy Mortality (%)", y = "Percent of Canopy Carbon") +
    scale_linetype(name = "Carbon Pool", labels=c("Litter","Consumed")) +
    scale_color_brewer(palette = "Dark2", name = "Carbon Pool", labels=c("Litter","Consumed"))
  #plot(x)
  return(x)
}

# x = consumption_output_figure(consumption_parameter = 0.01, title_name = "High Consumption")
# ggsave("consumption_high.pdf", plot = x, path = DISPLAY_FIGURES_DIR)
# x = consumption_output_figure(consumption_parameter = 1, title_name = "Medium Consumption")
# ggsave("consumption_medium.pdf", plot = x, path = DISPLAY_FIGURES_DIR)
# x = consumption_output_figure(consumption_parameter = 100, title_name = "Low Consumption")
# ggsave("consumption_low.pdf", plot = x, path = DISPLAY_FIGURES_DIR)



# ---------------------------------------------------------------------
# Understory Mortality Figure
# Relation between pspread and percent mortality


understory_figure = function(pspread_mortality_parameter, title_name){
  
  pspread <- seq(from=0,to=1,length.out=101)
  
  tmp_func = function (pspread, pspread_mortality_parameter){
    if(pspread_mortality_parameter == 1){
      percent_mortality <- pspread*100
    }
    else {
      percent_mortality <-  (((pspread_mortality_parameter^pspread) - 1)/(pspread_mortality_parameter-1)*100)
    }
  }
  
  pspread_rep <- rep(pspread, length(pspread_mortality_parameter))
  pspread_mortality_parameter_rep <- rep(pspread_mortality_parameter, each=length(pspread))

  happy <- pspread_rep %>%
    cbind(pspread_rep = ., pspread_mortality_parameter_rep) %>%
    as.data.frame()
  
  percent_mortality <- mapply(tmp_func,happy$pspread_rep, happy$pspread_mortality_parameter_rep)
  happy <- cbind(happy, percent_mortality)
  happy$pspread_mortality_parameter_rep <- as.factor(happy$pspread_mortality_parameter_rep)
  
  
  x <- ggplot(data = happy) +
    geom_line(aes(x=pspread_rep, y=percent_mortality, linetype = pspread_mortality_parameter_rep, color = pspread_mortality_parameter_rep), size=1.2) +
    #theme_bw(base_size=16) +
    # labs(title = title_name, x = "Pspread", y = "Understory Carbon Mortality (%)") +
    labs(title = title_name, x = expression(Intensity~(`I'`[u])), y = "Proportion of Carbon Mortality") +
    #scale_linetype(name = "Understory\nMortality\nParameter\n(k_mort_u)") +
    #scale_linetype(name = expression(Understory~\nMortality~\nParameter~\n(k[consumption]))) +
    #scale_linetype(name = bquote(atop("Understory Mortality", Parameter~(k[consumption])))) +  # Note that adding expression plus multiple lines is not really supported in plotmath. https://stackoverflow.com/questions/13317428/
    #scale_color_brewer(palette = "Dark2", name = "Understory\nMortality\nParameter\n(k_mort_u)") +
    scale_linetype(name = "Understory\nMortality\nParameter") +
    scale_color_brewer(palette = "Dark2", name = "Understory\nMortality\nParameter") +
    theme(axis.text = element_text(size=18)) +
    NULL
  #plot(x)
  return(x)
}


fig_under = understory_figure(pspread_mortality_parameter = c(0.01, 1, 100), title_name = "Understory Mortality")
ggsave("display_understory.pdf", plot = fig_under, path = DISPLAY_FIGURES_DIR,
       width = 7.5, height = 4.5)



# ---------------------------------------------------------------------
# Overstory Mortality Figure
# Relation between understory biomass mortality and overstory mortality


overstory_figure = function(k1, k2, title_name){
  
  understory_biomass_mortality <- seq(from=0,to=2000, length.out = 201)   # gC/m2
  
  tmp_func = function (understory_biomass_mortality, k1, k2){
    overstory_percent_mortality <- (1-1/(1+exp(-(k1*(understory_biomass_mortality-k2))))*100)+100
    return(overstory_percent_mortality)
  }
  
  happy <- expand.grid(understory_biomass_mortality,k1,k2)
  happy <- dplyr::rename(happy, understory_biomass_mortality_rep = Var1, k1_rep = Var2, k2_rep = Var3)

  overstory_percent_mortality <- mapply(tmp_func, happy$understory_biomass_mortality_rep, happy$k1_rep, happy$k2_rep)

  happy <- cbind(happy, overstory_percent_mortality)
  happy$k1_rep <- as.factor(happy$k1_rep)
  happy$k2_rep <- as.factor(happy$k2_rep)

  x <- ggplot(data = happy) +
    geom_line(aes(x=understory_biomass_mortality_rep, y=overstory_percent_mortality, linetype = k1_rep, color = k2_rep), size=1.2) +
    #theme_bw(base_size=16) +
    labs(title = title_name, x = expression(Understory~"&"~Litter~Consumption~(gC/m^2)), y = "Proportion of Carbon Mortality") +
    #scale_linetype(name = "Slope\nParameter\n(k_1_mort_o)") +
    #scale_color_brewer(palette = "Dark2", name = "Centerpoint\nParameter\n(k_2_mort_o)") +
    scale_linetype(name = "Slope\nParameter") +
    scale_color_brewer(palette = "Dark2", name = "Centerpoint\nParameter") +
    theme(axis.text = element_text(size=18)) +
    NULL
  #plot(x)
  return(x)
}

fig_over <- overstory_figure(k1 = c(-0.005, -0.01, -0.05), k2 = c(500, 1500), title_name = "Overstory Mortality")
ggsave("display_overstory.pdf", plot = fig_over, path = DISPLAY_FIGURES_DIR,
       width = 7.5, height = 4.5)



# ---------------------------------------------------------------------
# Carbon balance Figure
# 


carbon_balance_figure = function(prop_c_mort, title_name){
  
  variable <- c("0.1","0.1","0.1","0.5","0.5","0.5","0.9","0.9","0.9")
  category <- c("Residual", "Consumed", "Alive",
                "Residual", "Consumed", "Alive",
                "Residual", "Consumed", "Alive")
  value <- c(prop_c_mort*100*(.9), prop_c_mort*100*(.1), 100 - prop_c_mort*100,
             prop_c_mort*100*(.5), prop_c_mort*100*(.5), 100 - prop_c_mort*100,
             prop_c_mort*100*(.1), prop_c_mort*100*(.9), 100 - prop_c_mort*100)
  
  c_tib <- tibble::tibble(variable = variable,
                          category = category,
                          value = value)

  x <- ggplot(data = c_tib) +
    geom_col(aes(x = variable, y = value, fill = category), width = .7) +
    geom_text(aes(x = variable, y = value),label=category, position = position_stack(vjust = 0.5), size = 6) +
    labs(title = title_name, x = "Proportion of mortality consumed (prop_mort_consumed)", y = "Proportion of Pre-Fire Canopy Carbon (%)") +
    scale_fill_brewer(palette = "Dark2", name = "Category") +
    theme_classic(base_size=18) +
    theme(legend.position="none", axis.text = element_text(size=18)) +
    NULL
  #plot(x)
  
  return(x)
}

fig_carb_bal <- carbon_balance_figure(prop_c_mort = 0.8, title_name = "Carbon Balance")
ggsave("carbon_balance.pdf", plot = fig_carb_bal, path = DISPLAY_FIGURES_DIR,
       width = 7.5, height = 7)







# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# The following code was designed to stitch all the figures together using
# cowplot. It will actually be easier to use powerpoint




# # ---------------------------------------------------------------------
# # Big Leaf diagram
# 
# library(magick)
# 
# img_path <- file.path(DISPLAY_FIGURES_DIR, "fig_2_photo_tmp_example.tif")
# 
# fig_big_leaf <- ggdraw() + draw_image(img_path, scale = 0.9)
# 
# 
# 
# # ---------------------------------------------------------------------
# # ---------------------------------------------------------------------
# # ---------------------------------------------------------------------
# # Combine into single figure
# 
# theme_set(theme_bw(base_size = 12))
# 
# fig_consumption = consumption_figure(consumption_parameter = c(0.01, 1, 100), title_name = "Consumption")
# fig_under = understory_figure(pspread_mortality_parameter = c(0.01, 1, 100), title_name = "Understory Mortality")
# fig_over <- overstory_figure(k1 = c(-0.005, -0.01, -0.05), k2 = c(500, 1500), title_name = "Overstory Mortality")
# fig_carb_bal <- carbon_balance_figure(prop_c_mort = 0.8, title_name = "Carbon Balance")
# 
# 
# 
# 
# left_col <- cowplot::plot_grid(fig_big_leaf,
#                                fig_carb_bal,
#                                labels=c("A","B"),
#                                nrow=2)
# 
# 
# right_col <- cowplot::plot_grid(fig_under,
#                                 fig_over,
#                                 fig_consumption,
#                                 labels=c("C","D","E"),
#                                 nrow=3)
# 
# # Combine figures for Cowplot
# fe_concep_fig <- cowplot::plot_grid(left_col,
#                                     right_col,
#                                     nrow=1,
#                                     rel_widths = c(1.3, 1))
# 
# 
# 
# # Output conceptual figure
# cowplot::save_plot(file.path(DISPLAY_FIGURES_DIR,"fe_concep_fig.pdf"),
#                    fe_concep_fig,
#                    ncol=1,
#                    nrow=2,
#                    base_aspect_ratio=2.5)
