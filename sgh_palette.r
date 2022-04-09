SR 07.04.2022 v1
This script defines a new ggthemr palette 'sgh_palette'

if(!require(devtools)){
  install.packages("devtools")
  library(devtools)
}
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}
if(!require(ggthemr)){
  devtools::install_github('cttobin/ggthemr')
  library(ggthemr)
}

sgh_palette = define_palette(
  swatch = c("#007481", "#00b5e2", "#cedc00", "#000000", # SGH official colours
             "#233B43", "#E84646", "#65ADC2"), # "fresh" theme colours
  gradient = c("#007481", "#00b5e2"),
  background = "#ffffff",
  text = c("#000000", "#000000"),
  line = c("#000000", "#000000")
)

ggthemr(sgh_palette,
        layout = "clear",
        spacing = 1,
        type = "inner"
        )

##### Exemplary plots #####

# if(!require(data.table)){
#   install.packages("data.table")
#   library(data.table)
# }

State = rownames(USArrests)
dt_wide = data.table(State, USArrests)
rm(State)
dt_long = melt.data.table(dt_wide,
                          id.vars = c("State", "UrbanPop"),
                          variable.name = "CrimeType",
                          value.name = "Frequency")

ggplot(dt_long,
       aes(x = UrbanPop,
           y = Frequency,
           color = factor(CrimeType, levels = c("Assault", "Rape", "Murder")))) +
  geom_point() +
  scale_colour_ggthemr_d() +
  labs(title = "US Crime Frequency by Crime Type and Urbanisation Level",
       subtitle = "Data from the US States",
       caption = "Source: R built-in data",
       x = "Urban population (percent)",
       y = "Frequency (per 100,000 individuals)",
       color = "Type of crime") +
  theme(legend.position = "bottom")

ggplot(Panel,
       aes(x = time,
           y = valence,
           group=country,
           colour=country)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2, size = 2, linetype="dotted", color = "black") +
  geom_vline(xintercept = 14, size = 2, linetype="dotted", color = "black") + 
  # scale_colour_ggthemr_d() +
  labs(title = "Title",
       subtitle = "subtitle",
       caption = "Source: R built-in data",
       x = "x title",
       y = "y title",
       color = "Country") +
  theme(legend.position = "bottom")


ggplot(dt_long[CrimeType %in% c("Rape", "Murder"), ],
       aes(x = reorder(State, -Frequency))) +
  geom_bar(aes(y = Frequency,
               fill = CrimeType),
           stat = "identity",
           position = position_dodge()) +
  geom_line(aes(y = UrbanPop/2,
                group = 1)) +
  labs(title = "US Crime Frequency by Crime Type and State",
       subtitle = "Data from the US States",
       caption = "Source: R built-in data",
       x = "State",
       fill = "Type of crime") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(
    name = "Frequency (per 100,000 individuals)",
    sec.axis = sec_axis(~.*2, name="Urban population (percent)")
  )

ggplot(dt_long[State %in% c("California", "Illinois", "Massachusetts"), ],
       aes(x = reorder(State, -Frequency),
           y = Frequency,
           fill = factor(CrimeType, levels = c("Assault", "Rape", "Murder")))) +
  geom_bar(stat = "identity",
           position = position_dodge()) +
  geom_text(aes(label = Frequency),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "US Crime Frequency by Crime Type and State",
       subtitle = "Data from the selected US States",
       caption = "Source: R built-in data",
       x = "State",
       fill = "Type of crime") +
  theme(legend.position = "bottom")

