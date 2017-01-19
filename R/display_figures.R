# Display Figures for Fire Effects Project
#
# Contains scripts for producing display figures

library(ggplot2)
library(dplyr)
library(tidyr)


# ---------------------------------------------------------------------
# Input and output paths 

OUTPUT_DIR <- "outputs"
DISPLAY_FIGURES_DIR <- file.path(OUTPUT_DIR, "display_figures")


# ---------------------------------------------------------------------
# Volatilization Figure


vaporization_figure = function(vapor_loss_parameter, title_name){

  percent_loss <- seq(from=0,to=1,length.out=101)
  percent_vap <- vector()
  
  for(aa in seq_along(percent_loss)){
    if(vapor_loss_parameter == 1){
      percent_vap[aa] <- percent_loss[aa] * percent_loss[aa]
    }
    else {
      percent_vap[aa] <- percent_loss[aa] * ((vapor_loss_parameter^percent_loss[aa]) - 1)/(vapor_loss_parameter-1)
    }
  }
  
  percent_litter <- percent_loss - percent_vap

  happy = percent_loss %>%
    cbind(percent_vap, percent_litter) %>%
    `*`(100) %>%
    as.data.frame() %>%
    gather("carbon_pool", "Percent", 2:3)
  
  x <- ggplot(data = happy) +
    geom_line(aes(x=., y=Percent, linetype = carbon_pool, color = carbon_pool), size=1.2) +
    theme_bw(base_size=16) +
    labs(title = title_name, x = "Percent Canopy Loss") +
    scale_linetype(name = "Carbon Pool", labels=c("Litter","Volatilized")) +
    scale_color_brewer(palette = "Dark2", name = "Carbon Pool", labels=c("Litter","Volatilized"))
  plot(x)
  return(x)
}

x = vaporization_figure(vapor_loss_parameter = 0.01, title_name = "High Volatilization")
ggsave("volatilization_high.pdf", plot = x, path = DISPLAY_FIGURES_DIR)
x = vaporization_figure(vapor_loss_parameter = 1, title_name = "Medium Volatilization")
ggsave("volatilization_medium.pdf", plot = x, path = DISPLAY_FIGURES_DIR)
x = vaporization_figure(vapor_loss_parameter = 100, title_name = "Low Volatilization")
ggsave("volatilization_low.pdf", plot = x, path = DISPLAY_FIGURES_DIR)



# ---------------------------------------------------------------------
# Understory Loss Figure
# Relation between pspread and percent loss


understory_figure = function(pspread_loss_parameter, title_name){
  
  pspread <- seq(from=0,to=1,length.out=101)
  
  tmp_func = function (pspread, pspread_loss_parameter){
    if(pspread_loss_parameter == 1){
      percent_loss <- pspread*100
    }
    else {
      percent_loss <-  (((pspread_loss_parameter^pspread) - 1)/(pspread_loss_parameter-1)*100)
    }
  }
  
  pspread_rep <- rep(pspread, length(pspread_loss_parameter))
  pspread_loss_parameter_rep <- rep(pspread_loss_parameter, each=length(pspread))

  happy <- pspread_rep %>%
    cbind(pspread_rep = ., pspread_loss_parameter_rep) %>%
    as.data.frame()
  
  percent_loss <- mapply(tmp_func,happy$pspread_rep, happy$pspread_loss_parameter_rep)
  happy <- cbind(happy, percent_loss)
  happy$pspread_loss_parameter_rep <- as.factor(happy$pspread_loss_parameter_rep)
  
  
  x <- ggplot(data = happy) +
    geom_line(aes(x=pspread_rep, y=percent_loss, linetype = pspread_loss_parameter_rep, color = pspread_loss_parameter_rep), size=1.2) +
    theme_bw(base_size=16) +
    labs(title = title_name, x = "Pspread", y = "Percent Understory Loss") +
    scale_linetype(name = "Pspread\nParameter") +
    scale_color_brewer(palette = "Greens", name = "Pspread\nParameter")
  plot(x)
  return(x)
}


x = understory_figure(pspread_loss_parameter = c(0.01, 1, 100), title_name = "Understory Loss")
ggsave("display_understory.pdf", plot = x, path = DISPLAY_FIGURES_DIR)


# ---------------------------------------------------------------------
# Overstory Loss Figure
# Relation between understory biomass loss and overstory loss






