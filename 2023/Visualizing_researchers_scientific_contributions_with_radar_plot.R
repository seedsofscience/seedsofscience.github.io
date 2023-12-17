##Figure 1##
library(worldfootballR)  #for scraping
library(tidyverse)       #for ggplot, dplyr and several other stuff
library(forcats)         #for sorting within ggplot
library(glue)            #easier than paste()
library(ggplot2)

df_selected<- data.frame(player_name = "Takefusa Kubo",
                         Statistic = c("Non-Penalty Goals",
                                       "npxG",
                                       "Shots Total",
                                       "Assists",
                                       "xAG",
                                       "npxG+xA",
                                       "Shot-Creating Actions",
                                       "Passes Attempted",
                                       "Pass Completion %",
                                       "Progressive Passes",
                                       "Progressive Carries",
                                       "Successful Take-Ons",
                                       "Touches (Att Pen)",
                                       "Progressive Passes Received",
                                       "Tackles",
                                       "Interceptions",
                                       "Blocks",
                                       "Clearances",
                                       "Arieals Won"
                         ))
Per90 = c(0.38,
          0.24,
          2.63,
          0.16,
          0.24,
          0.45,
          3.94,
          33.14,
          71.8,
          2.98,
          4.41,
          2.00,
          4.76,
          8.60,
          1.40,
          0.41,
          1.14,
          0.16,
          0.48)
Percentile = c(99,
               94,
               99,
               79,
               98,
               98,
               92,
               12,
               7,
               10,
               99,
               98,
               99,
               99,
               14,
               7,
               42,
               1,
               23
)
stat=c("Attacking",
       "Attacking",
       "Attacking",
       "Attacking",
       "Attacking",
       "Attacking",
       "Attacking",
       "Possession",
       "Possession",
       "Possession",
       "Possession",
       "Possession",
       "Possession",
       "Possession",
       "Defending",
       "Defending",
       "Defending",
       "Defending",
       "Defending")

temp <- (360/(nrow(df_selected))/2)                             #find the difference in angle between to labels and divide by two.
myAng <- seq(-temp, -360+temp, length.out = nrow(df_selected))  #get the angle for every label
ang<-ifelse(myAng < -90, myAng+180, myAng)                                    #rotate label by 180 in some places for readability
ang<-ifelse(ang < -90, ang+180, ang)                                          #rotate some lables back for readability...  
df_selected$Statistic <- gsub(" ","\n",df_selected$Statistic)

ggplot(df_selected,aes(fct_reorder(Statistic,stat),Percentile)) +                       #select the columns to plot and sort it so the types of metric are grouped
  geom_bar(aes(y=100,fill=stat),stat="identity",width=1,colour="white",                 #make the whole pizza first
           alpha=0.5) +                                                                          #change alphe to make it more or less visible
  geom_bar(stat="identity",width=1,aes(fill=stat),colour="white") +                     #insert the values 
  coord_polar() +                                                                       #make it round
  geom_label(aes(label=Per90,fill=stat),size=3,color="white",show.legend = FALSE)+     #add a label for the value. Change 'label=Per.90' to 'label=Percentile' to show the percentiles
  scale_fill_manual(values=c("Possession" = "#D70232",                                   #choose colors to fill the pizza parts
                             "Attacking" = "#1A78CF",
                             "Defending" = "#FF9300")) +                                                              
  scale_y_continuous(limits = c(-10,100))+                                              #create the white part in the middle.   
  labs(fill="",                                                                         #remove legend title
       caption = "Data from StatsBomb via FBref",                                       #credit FBref/StatsBomb
       title="Takefusa Kubo | Based on 2835 minutes played",
       subtitle = "Comparison positional peers in Men Big 5 Leagues, UCL, UEL over the last 365 days")+                                                   #let the title be te name of the player
  
  theme_minimal() +                                                                     #from here it's only themeing. 
  theme(legend.position = "top",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, angle = ang),
        text = element_text(family="Spartan-Light"),                                    #I downloaded this font from Google Fonts. You can use your own font of course
        plot.title = element_text(hjust=0.5),
        plot.caption = element_text(hjust=0.5,size=8),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 


##Researcher A##
library(worldfootballR)  #for scraping
library(tidyverse)       #for ggplot, dplyr and several other stuff
library(forcats)         #for sorting within ggplot
library(glue)            #easier than paste()
library(ggplot2)

df_selected<- data.frame(
  Statistic = c("Concept", 
                "Data Curation",
                "Formal analysis",
                "Investigation",
                "Methodology",
                "Software",
                "Funding Acquisition",
                "Project Administration",
                "Resources",
                "Supervision",
                "Validation",
                "Visualization",
                "Writing",
                "Revising"
  ),
  Per90 = c(4,
            9,
            7,
            9,
            6,
            1,
            1,
            11,
            3,
            2,
            11,
            4,
            13,
            24),
  Percentile = c(0.3,
                 0.6,
                 0.5,
                 0.6,
                 0.4,
                 0.1,
                 0.1,
                 0.7,
                 0.2,
                 0.1,
                 0.7,
                 0.3,
                 0.9,
                 1.6),
  stat=c("Conceptualizaion",
         "Analysis",
         "Analysis",
         "Analysis",
         "Analysis",
         "Analysis",
         "Administration",
         "Administration",
         "Administration",
         "Administration",
         "Administration",
         "Manuscript",
         "Manuscript",
         "Manuscript"
  ))

temp <- (360/(nrow(df_selected))/2)                             #find the difference in angle between to labels and divide by two.
myAng <- seq(-temp, -360+temp, length.out = nrow(df_selected))  #get the angle for every label
ang<-ifelse(myAng < -90, myAng+180, myAng)                                    #rotate label by 180 in some places for readability
ang<-ifelse(ang < -90, ang+180, ang)                                          #rotate some lables back for readability...  
df_selected$Statistic <- gsub(" ","\n",df_selected$Statistic)

ggplot(df_selected,aes(fct_reorder(Statistic,stat),Per90)) +                       
  geom_bar(aes(y=30,fill=stat),stat="identity",width=1,colour="white",                 
           alpha=0.5) +                                                                         
  geom_bar(stat="identity",width=1,aes(fill=stat),colour="white") +                      
  coord_polar() +                                                                      
  geom_label(aes(label=Per90,fill=stat),size=2,color="white",show.legend = FALSE)+     
  scale_fill_manual(values=c("Conceptualizaion" = "#5A9675",
                             "Analysis" = "#D70232", 
                             "Manuscript" = "#1A78CF",
                             "Administration" = "#FF9300")) +                                                              
  scale_y_continuous(limits = c(-5,30))+                                                 
  labs(fill="",                                                                     )+                                                    
  
  theme_minimal() +                                                                      
  theme(legend.position = "top",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, angle = ang),
        text = element_text(family="Spartan-Light"),                                    
        plot.title = element_text(hjust=0.5),
        plot.caption = element_text(hjust=0.5,size=8),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 


##Researcher B##
library(worldfootballR)  #for scraping
library(tidyverse)       #for ggplot, dplyr and several other stuff
library(forcats)         #for sorting within ggplot
library(glue)            #easier than paste()
library(ggplot2)

df_selected<- data.frame(
  Statistic = c("Concept", 
                "Data Curation",
                "Formal analysis",
                "Investigation",
                "Methodology",
                "Software",
                "Funding Acquisition",
                "Project Administration",
                "Resources",
                "Supervision",
                "Validation",
                "Visualization",
                "Writing",
                "Revising"
  ),
  Per90 = c(35,
            5,
            6,
            9,
            29,
            17,
            7,
            24,
            8,
            42,
            31,
            3,
            8,
            23),
  Percentile = c(2.3,
                 0.3,
                 0.4,
                 0.6,
                 1.9,
                 1.1,
                 0.5,
                 1.6,
                 0.5,
                 2.8,
                 2.1,
                 0.2,
                 0.5,
                 1.5),
  stat=c("Conceptualizaion",
         "Analysis",
         "Analysis",
         "Analysis",
         "Analysis",
         "Analysis",
         "Administration",
         "Administration",
         "Administration",
         "Administration",
         "Administration",
         "Manuscript",
         "Manuscript",
         "Manuscript"
  ))

ggplot(df_selected,aes(fct_reorder(Statistic,stat),Per90)) +                       #select the columns to plot and sort it so the types of metric are grouped
  geom_bar(aes(y=45,fill=stat),stat="identity",width=1,colour="white",                 #make the whole pizza first
           alpha=0.5) +                                                                          #change alphe to make it more or less visible
  geom_bar(stat="identity",width=1,aes(fill=stat),colour="white") +                     #insert the values 
  coord_polar() +                                                                       #make it round
  geom_label(aes(label=Per90,fill=stat),size=2,color="white",show.legend = FALSE)+     #add a label for the value. Change 'label=Per.90' to 'label=Percentile' to show the percentiles
  scale_fill_manual(values=c("Conceptualizaion" = "#5A9675",
                             "Analysis" = "#D70232", #choose colors to fill the pizza parts
                             "Manuscript" = "#1A78CF",
                             "Administration" = "#FF9300")) +                                                              
  scale_y_continuous(limits = c(-5,45))+                                              #create the white part in the middle.   
  labs(fill="",                                                                         #remove legend title
       caption = "Researcher B",                                       #credit FBref/StatsBomb
       title=df_selected$Player[1])+                                                    #let the title be te name of the player
  
  theme_minimal() +                                                                     #from here it's only themeing. 
  theme(legend.position = "top",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(family="Spartan-Light"),                                    #I downloaded this font from Google Fonts. You can use your own font of course
        plot.title = element_text(hjust=0.5),
        plot.caption = element_text(hjust=0.5,size=6),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 


##Compare##
###
# Load necessary packages
library(worldfootballR)  #for scraping
library(tidyverse)       #for ggplot, dplyr and several other stuff
library(forcats)         #for sorting within ggplot
library(glue)            #easier than paste()
library(ggplot2)

# Create data frames
df_selected_1 <- data.frame(
  Statistic = c("Concept", 
                "Data Curation",
                "Formal analysis",
                "Investigation",
                "Methodology",
                "Software",
                "Funding",
                "Admin",
                "Resources",
                "Supervision",
                "Validation",
                "Visualization",
                "Writing",
                "Revising"
  ),
  Per90 = c(4, 9, 7, 9, 6, 1, 1, 11, 3, 2, 11, 4, 13, 24),
  Percentile = c(0.3, 0.6, 0.5, 0.6, 0.4, 0.1, 0.1, 0.7, 0.2, 0.1, 0.7, 0.3, 0.9, 1.6),
  stat = rep("Researcher A", 14)
)

df_selected_2 <- data.frame(
  Statistic = c("Concept", 
                "Data Curation",
                "Formal analysis",
                "Investigation",
                "Methodology",
                "Software",
                "Funding",
                "Admin",
                "Resources",
                "Supervision",
                "Validation",
                "Visualization",
                "Writing",
                "Revising"
  ),
  Per90 = c(35, 5, 6, 9, 29, 17, 7, 24, 8, 42, 31, 3, 8, 23),
  Percentile = c(2.3, 0.3, 0.4, 0.6, 1.9, 1.1, 0.5, 1.6, 0.5, 2.8, 2.1, 0.2, 0.5, 1.5),
  stat = rep("Researcher B", 14)
)

# Combine the selected data frames
combined_data <- bind_rows(
  mutate(df_selected_1, Researcher = "Researcher A"),
  mutate(df_selected_2, Researcher = "Researcher B")
)

#temp <- (360/(nrow(combined_data))/2)                             #find the difference in angle between to labels and divide by two.
#myAng <- seq(-temp, -360+temp, length.out = nrow(combined_data))  #get the angle for every label
#ang<-ifelse(myAng < -90, myAng+180, myAng)                                    #rotate label by 180 in some places for readability
#ang<-ifelse(ang < -90, ang+180, ang)                                          #rotate some lables back for readability...  
#combined_data$Statistic <- gsub(" ","\n",combined_data$Statistic)

###
# Create a polar bar chart using ggplot2 with specified colors
comparison_plot <- ggplot(combined_data, aes(x = reorder(Statistic,Per90), y = Per90, fill = stat)) +
  #geom_bar(stat = "identity", position = "identity", width = 1, color = "black", alpha = 0.45) +
  
  geom_bar(data = subset(combined_data, stat == "Researcher A"), 
           stat = "identity", position = "identity", width = 1, color = "black", alpha = 0.9) +
  geom_bar(data = subset(combined_data, stat == "Researcher B"), 
           stat = "identity", position = "identity", width = 1, color = "black", alpha = 0.4) +
  
  
  # Display labels only for values greater than a threshold (e.g., 10)
  geom_label(data = subset(combined_data, Per90 > 5), aes(label = Per90), size = 2, color = "white", show.legend = FALSE) +
  coord_polar() +
  
  scale_fill_manual(values = c("Researcher A" = "#FC766AFF",
                               "Researcher B" = "#5B84B1FF")) +
  scale_y_continuous(limits = c(0, max(combined_data$Per90) + 5), 
                     breaks = seq(0, max(combined_data$Per90) + 5, by = 5)) +
  labs(fill = "",
       title = "Comparison between Researcher A and Researcher B") +
  
  theme_minimal() +
  theme(legend.position = "top",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(family = "Spartan-Light"),                                    
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 6),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "lightgray"),
        axis.text = element_text(color = "black"))  # Adjusted axis text color to black

# Print the comparison plot with a larger size
print(comparison_plot)