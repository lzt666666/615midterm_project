library(shiny)
library(tidyverse)
library(ggplot2)
library(maps)
#read data
df<-read.csv("df.csv")

total_map=read.csv("total_map.csv")
county = map_data("county")
damage_hurricane<-read.csv("damage_hurricane.csv")
total_declaration<-df%>%group_by(state,year)%>%summarise(total_declaration
                                                    =n())
total_map$project_legend<-total_map$total_project%>%cut(breaks=c(1.356e+03,1.781e+05,1.012e+06,5.603e+06,9.801e+09),
                                                        include.lowest=T)
#define ui
ui <- fluidPage(
    mainPanel(
        headerPanel("Damage by hurricane data"),
        tabsetPanel(
            tabPanel("Declaration by each state",
                     br(),
                     sidebarPanel(
                         selectInput('stateinput','State',
                                     choices=sort(unique(df$state))),
                         selectInput("yearinput","Year",c("ALL",unique(df$year)))
                     ),
                     mainPanel(
                         h5('number of declaration for each state'),
                         plotOutput('plot_state'),
                         h5('Table for damage for each state in particular year'),
                         dataTableOutput('table_state')
                     )),
            tabPanel("Map for hurricane damage",
                     br(),
                     sidebarPanel(
                         selectInput("region","State",choices=sort(unique(total_map$region)))
                     ),
                     mainPanel(
                         h5('Map for each state'),
                         plotOutput('mapstate'),
                     )),
            tabPanel("map of total project amount in each county by year",
                     br(),
                     sidebarLayout(sidebarPanel(
                         selectInput(
                             "years",
                             label = "select a year",
                             choices = list("2009", "2010", "2011", "2012", "2013", "2016", "2017", "2018")
                         )
                     ),
                     mainPanel(plotOutput("mapping"))
                     ))
        )
    )
)
            
                     

# Define server logic required to draw plot and table
server <- function(input, output) {
    output$plot_state<-renderPlot({
        hurricane_state<-total_declaration%>%filter(state==input$stateinput)
        total<-ggplot(data=hurricane_state)+geom_bar(aes(x=year,weight=total_declaration,fill=year))+coord_flip()
        total

})
    output$table_state<-renderDataTable({
        hurricane_filtered<-df%>%filter(state==input$stateinput&year==input$yearinput)
        print(hurricane_filtered)
        
})
    output$mapstate<-renderPlot({
        map<-total_map%>%filter(region==input$region)
        map_state<-ggplot()+
            geom_polygon(data=county,aes(long,lat,group=group),colour="black",fill="white")+
            geom_polygon(data=map,aes(long,lat,group=group,fill=project_legend))+
            scale_fill_brewer(palette="Reds")+
            ggtitle("Total project amount by county")+
            theme(plot.title=element_text(hjust=0.5))
        map_state
    })
    
    
    output$mapping=renderPlot({
        part_map=total_map%>%filter(year==input$years)
        part_map$project_legend_2 =part_map$total_project  %>% cut(
            breaks =unique(quantile(part_map$total_project, probs = seq(0, 1, 0.2))),
            include.lowest = T
        )
        
        print(ggplot()+
                  geom_polygon(data=county,aes(long,lat,group=group),color="black",fill="white")+
                  geom_polygon(data=part_map,aes(long,lat,group=group,fill=project_legend_2))+
                  geom_path(county, mapping=aes(x=long, y=lat, group=group),color="grey")+
                  scale_fill_brewer(palette="Blues")+
                  ggtitle("total project amount by county")+
                  theme(plot.title=element_text(hjust=0.5)))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
