#setwd("L:\\school\\cuny\\608\\HW1")
library(stringr)
library(dplyr)
library(ggplot2)
dfComp <- read.table("inc5000_data.csv", stringsAsFactors = F, sep = ',', header = T, quote = '"')

#get counts per state
stateCounts <- dfComp %>% 
                    group_by(State) %>% 
                    summarise(n()) 

colnames(stateCounts)<- c("State", "Count")
stateCounts <- stateCounts[order(-stateCounts$Count),]


plot1 <- ggplot(stateCounts,
       #aes(x= State, y = Count))+
       aes(x= reorder(State, Count), State , y = Count))+  #interesting ggplot doesn't pay attn to df order?
    geom_bar(stat = "identity", fill = "blue", colour = "black", width = 0.75) +
    coord_flip() + 
    labs(x= "US State", y= "Company Count Per State") +
    theme_light() + 
    ggtitle("Count of Fastest Growing Companies Per State") +
    theme(plot.title = element_text(hjust = 0.5))

plot1    
ggsave("plot1.png")


#find state with 3rd most companies
stateCounts[3,]
#(and it is NY)

#filter by NY (3rd most company's state)                
filteredIndGrpNY <-  (dfComp[complete.cases(dfComp),] %>% 
                        group_by(Industry) %>%
                        filter(State == "NY"))[,5:6]  #just get industry and employee count columns

#we are going to remove the bottom and top 10% from the box plot to filter outliers

y10 <- distinct(filter(filteredIndGrpNY, Employees <= quantile(Employees, probs = .10)) %>%
                    filter(Employees == max(Employees)),.keep_all = T)
y25 <- distinct(filter(filteredIndGrpNY, Employees <= quantile(Employees, probs = .25)) %>%
                              filter(Employees == max(Employees)),.keep_all = T)
y50 <- distinct(filter(filteredIndGrpNY, Employees >= quantile(Employees, probs = .50)) %>%
                            filter(Employees == min(Employees)),.keep_all = T)
y75 <- distinct(filter(filteredIndGrpNY, Employees >= quantile(Employees, probs = .75)) %>%
                              filter(Employees == min(Employees)),.keep_all = T)
y90 <- distinct(filter(filteredIndGrpNY, Employees >= quantile(Employees, probs = .90)) %>%
                    filter(Employees == min(Employees)),.keep_all = T)

empGroups <- y10 %>% inner_join(y25, by = 'Industry') %>%
                     inner_join(y50, by  = 'Industry') %>%
                     inner_join(y75, by = 'Industry') %>%
                     inner_join(y90, by = 'Industry') 
colnames(empGroups) <- c('Industry', 'y10', 'y25', 'y50', 'y75', 'y90')

plot2 <- ggplot(empGroups, aes(x = Industry, ymin = y10, lower = y25, middle = y50, upper = y75, ymax = y90)) +
                        geom_boxplot(stat = "identity", fill = "lightblue", colour = "black", show.legend = T) +
                        theme_light() +
                        theme(axis.text.x = element_text(color = "blue", angle = 90, hjust = 1),
                              axis.text.y = element_text(color = "blue")) +
                        ggtitle("Boxplot of Employees in Companies Per Industry Within NY State", "Graph of Min of 10%, Max of 90% to Exclude Outliers") +
                        theme(plot.title = element_text(hjust = 0.5), plot.subtitle =  element_text(hjust = 0.5)) 
         
plot2 
ggsave("plot2.png")

#find avg revenue per employee per industry
revPerEmp <- summarise(industryGroup, sum(Revenue)/sum(Employees)) 
colnames(revPerEmp) <- c('Industry', 'perEmp')

plot3 <- ggplot(revPerEmp) +
       aes(x= Industry , y = perEmp)+
    geom_bar(stat = "identity", fill = "red", colour = "black", width = 0.75, linetype = 0) +
    labs(x= "Industry", y= "Revenue Per Employee", font.lab = 2) +
    theme_light() + 
    theme(axis.text.x = element_text(color = "blue", angle = 90, hjust = 1, face = "bold"),
          axis.text.y = element_text(color = "blue", face = "bold"),
          axis.title = element_text(face = "bold")) +
    ggtitle("Revenue Per Employee By Industry") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

plot3
ggsave("plot3.png")                        


