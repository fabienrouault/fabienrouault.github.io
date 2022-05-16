---
title: "Co2 emission of car homologated in Chile"
output: html_notebook

---
The "French Citizen convention for the Climate" (Convention citoyenne pour le climat) proposed a tax 10€/kg over 1400 kg in the acquisition (proposition SD-C1.2) to foster the acquisition of lightweight vehicles. This project is carried out for a post in Instagram on Chilean account to the most and the least contaminating cars in the market. To do so, the dataset of homologated cars in Chile is used. Web scrapping of the webpage carfolio.com is made to get the the relationship between weight and CO2 emissions

```{r echo=FALSE}
library(readxl)
library(tidyverse)
library(rvest)
library(svMisc)
library(ggplot2)
library(svglite)

```

# Importing Data

## Dataset of homologated cars in Chile

```{r}
# importing CSV from the Chilean Department of Transport 
df<- read.csv("RendimientoUrbanoyEmision_2508451623629182737.csv", encoding = "UTF-8", header= TRUE)


print(head(df %>% select(Marca, Modelo, Combustible, Carrocería , Cilindrada, CO2.g.km)))

```

```{r, warning=FALSE}

# Selecting variables and converting CO2 column to numeric
df <- df %>% group_by(Marca,Modelo, Combustible, Carrocería, Cilindrada) %>% 
  mutate(CO2.g.km = as.numeric(gsub(",",".", CO2.g.km)))%>%
  summarise(co2 = mean(CO2.g.km, na.rm= TRUE))




```

## Webscraping of cars weight

```{r message=FALSE}

df_w_weight <- df

# Adding 2 columns to import data from carfolio webpage
df_w_weight$url_carfolio <- NA
df_w_weight$kerb_weight <- NA

# Web scraping of kerb weight from carfolio
for (k in 4:nrow(df)) {

# Browse in carfolio the car based on its brand, model, fuel type, body and capacity    
brand <- gsub(" ", "+",df_w_weight$Marca[k])
model <- gsub(" ", "+",df_w_weight$Modelo[k])
fuel <- ifelse(df_w_weight$Combustible[k] == "Gasolina", "petrol",ifelse(df_w_weight$Combustible[1] == "Diesel", "diesel", ""))
body <- gsub(" ", "+",df_w_weight$Carrocería[k])
capacity <- df_w_weight$Cilindrada[k]


search_url = paste("https://www.carfolio.com/search/results/?s=y&o=d&terms=", paste(brand,model, fuel, body,capacity ,sep = "+"),sep ="")
 
search_results <- read_html(search_url) %>% html_nodes('a') %>% html_attr('href')
  

# get the page of the first result
df_w_weight$url_carfolio[k]<-paste("https://www.carfolio.com", search_results[16],sep ="")

# Pause to avoid ip ban
Sys.sleep(10+runif(1, min=-5, max=5))

# extract the car weight only in the case that a car was found, i-e, length greater then 40 characters
if (nchar(df_w_weight$url_carfolio[k]) > 40) {
df_w_weight$kerb_weight[k]<- (read_html(df_co2$url_carfolio[k]) %>% html_table())[[2]][16, 2]
}
# Pause to avoid ip ban
Sys.sleep(20+runif(1, min=-5, max=5))

progress(k, max.value = nrow(df))
}


```

```{r}

not_found <- sum(df_w_weight$kerb_weight=="")
wo_weight <- sum(is.na(df_w_weight$kerb_weight))
Model_w_weight <- nrow(df_w_weight)-not_found-wo_weight

print(paste("Models not found:" ,not_found))
print(paste("Models without weight:" ,wo_weight))
print(paste("Models with weight:" ,Model_w_weight))
```



```{r}
df_w_weight <- df_w_weight %>% filter(!is.na(kerb_weight)) %>%
  separate(kerb_weight,c("weight", "unite"), sep = " ")%>%
  mutate_at(vars(weight), as.numeric) %>%
  filter(!is.na(weight))

df_w_weight <- df_w_weight %>% 
  mutate(Combustible=factor(Combustible,levels=c("Diesel","Eléctrico/Híbrido", "Gasolina"))) %>% 
  na.omit(Combustible)

```

# Data visualization

```{r echo=FALSE}
theme_custom <- function(base_size,fill.color) {
  theme(
    # Specify axis options
    axis.line = element_blank(),  
    axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
    axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
    axis.ticks = element_line(color = "white", size  =  0.2),  
    axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
    axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
    axis.ticks.length = unit(0.3, "lines"),   
    # Specify legend options
    legend.background = element_rect(color = NA, fill = fill.color),  
    legend.key = element_rect(color = fill.color,  fill = fill.color),  
    legend.key.size = unit(1.2, "lines"),  
    legend.key.height = NULL,  
    legend.key.width = NULL,      
    legend.text = element_text(size = base_size*0.8, color = "white"),  
    legend.title = element_blank(),  
    legend.position = "bottom",  
    legend.text.align = NULL,  
    legend.title.align = NULL,  
    legend.direction = "horizontal",  
    legend.box = NULL, 
    # Specify panel options
    panel.background = element_rect(fill = fill.color, color  =  NA),  
    panel.border = element_rect(fill = NA, color = "white"),  
    panel.grid.major = element_line(color = "grey35"),  
    panel.grid.minor = element_line(color = "grey20"),  
    #panel.margin = unit(0.5, "lines"),   
    # Specify facetting options
    strip.background = element_rect(fill = "grey30", color = "grey10"),  
    strip.text.x = element_text(size = base_size*0.8, color = "white"),  
    strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
    # Specify plot options
    plot.background = element_rect(color = fill.color, fill =fill.color),  
    plot.title = element_text(size = base_size*1.4, color = "white",hjust = 0, face="bold"),
    plot.subtitle = element_text(hjust = 0, colour = "white"),            # Centrer le sous-titre
    plot.caption = element_text(hjust = 1, face = "italic", color = "white"),
    plot.margin = unit(rep(1, 4), "lines"))
  
}

N_models <- df %>% ungroup()%>%group_by(Combustible) %>% 
           summarise(Nmodels = length(Combustible))

```

## Comparing CO2 emissions by fuel type

The dataset is composed of 1066 observations with 199 diesel, 17 Electric or hybrid and 848 gasoline. The following figures shows the distribution of CO2 emissions by fuel type.

```{r echo=TRUE, fig.height=5, fig.width=5, warning=FALSE}

df <- df %>% 
  mutate(Combustible=factor(Combustible,levels=c("Diesel","Eléctrico/Híbrido", "Gasolina"))) %>% 
  na.omit(Combustible)


ggplot(data= df, aes(x=Combustible , y=co2, fill=Combustible))+ geom_boxplot(color="white")+
  geom_violin(alpha=0.5)+
    labs(x="", y="Emisiones de CO2 (gCO2/km)",
       title = expression("Emisiones"~"de"~"CO"[2]~"de los autos"),
       subtitle = "Distribución por tipo de combustible",
       caption= "Fuente : energiaabierta.cl")+
  theme_custom(base_size = 10, fill.color = "gray10")+
    scale_x_discrete(levels(df$Combustible), drop=FALSE)+
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y=element_blank(),
        legend.title=element_blank())

ggsave("Boxplot.svg", width=5, height= 5) 




```
```{r}

Top10_Co2 <- df %>%
                arrange((co2)) %>% head(10) %>% 
                mutate(name = paste(Marca,Modelo, sep = " ")) 
                
                
Top10_Co2 <-  Top10_Co2    %>%
                mutate(name = factor(name, levels=Top10_Co2$name))


Top10_Co2_inv <- df %>%
                arrange(desc(co2)) %>% head(10) %>% 
                mutate(name = paste(Marca,Modelo,Carrocería, sep = " ")) 
                
                
Top10_Co2_inv <-  Top10_Co2_inv    %>%
                mutate(name = factor(name, levels=Top10_Co2_inv$name))
```



```{r}
ggplot(data= Top10_Co2, aes(x=name , y=co2, fill=Combustible))+ geom_bar(stat= "identity", width=.6)+
  #geom_text(aes(y=co2, label= Combustible), hjust=1.1, color = "white")+ 
  labs(x="", y="Emisiones de CO2 (gCO2/km)",
       title = expression("Emisiones"~"de"~"CO"[2]~"de los autos"),
       subtitle = "Top 10 de los autos menos emisores",
       caption= "Fuente : energiaabierta.cl" )+
  geom_text(aes(y=co2, label= co2), hjust=-0.1, color = "white")+
  coord_flip(ylim=c(20,115))+
  theme_custom(base_size = 10, fill.color = "gray10")+
  theme(axis.title.y=element_blank(), 
        axis.ticks.y=element_blank(),
        legend.title=element_blank())


ggsave("Top10_Co2.svg", width=5, height= 5)
```


```{r}
ggplot(data= Top10_Co2_inv, aes(x=name , y=co2, fill=Combustible))+ geom_bar(stat= "identity", width=.6)+
  #geom_text(aes(y=co2, label= Combustible), hjust=1.1, color = "white")+ 
  labs(x="", y="Emisiones de CO2 (gCO2/km)",
       title = expression("Emisiones"~"de"~"CO"[2]~"de los autos"),
       subtitle = "Top 10 de los autos menos emisores",
       caption= "Fuente : energiaabierta.cl" )+
  geom_text(aes(y=co2, label= co2), hjust=-0.1, color = "white")+
  coord_flip(ylim=c(300,400))+
  theme_custom(base_size = 10, fill.color = "gray10")+
  theme(axis.title.y=element_blank(), 
        axis.ticks.y=element_blank(),
        legend.title=element_blank())


ggsave("Top10_Co2.svg", width=5, height= 5)
```

```{r message=FALSE, warning=FALSE}
ggplot(data=df_w_weight , aes(x=weight, y=co2, color= as.factor(Combustible)))+
  geom_point()+
  geom_smooth(method ="lm", se= FALSE)+
  geom_text(data = df_w_weight[df_w_weight$co2 == min(df_w_weight$co2), ],aes(x=weight, y=co2, label = paste(Marca,Modelo)),color= "white", size=3 ,hjust=-0.05 )+
  geom_text(data = df_w_weight[df_w_weight$co2 == max(df_w_weight$co2), ],aes(x=weight, y=co2, label = paste(Marca,Modelo)),color= "white", size=3 ,hjust=-0.05 )+
  geom_text(data = df_w_weight[df_w_weight$weight == min(df_w_weight$weight), ],aes(x=weight, y=co2, label = paste(Marca,Modelo)),color= "white", size=3 ,hjust=-0.05 )+
  geom_text(data = df_w_weight[df_w_weight$weight == max(df_w_weight$weight), ],aes(x=weight, y=co2, label = paste(Marca,Modelo)),color= "white", size=3 ,hjust=-0.05 )+
  labs(x="peso en vacío (kg)", y= "Emisiones de CO2 (gCO2/km)",
       title = expression("Emisiones"~"de"~"CO"[2]~"de los autos"),
       subtitle = "Emisiones vs. Peso en vacío",
       caption= "Fuente : energiaabierta.cl- carfolio.com" )+
  theme_custom(base_size = 10, fill.color = "gray10")


ggsave("CO2_vs_Peso.svg", width=5, height= 5)
```

