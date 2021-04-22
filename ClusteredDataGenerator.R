#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(lme4)
library(lmerTest)

#Declaring variables for loops
x<-NULL
y<-NULL
groups<-NULL
weightsFac<-NULL

getDat <- function(nGroups,groupN,popInt,popSlopeL1,popSlopeL2,popInter,intVar,slopeVar,noise){
    #Generating (# of groups * size of groups) x values
    x <- rnorm(nGroups*groupN,0,1)
    
    #Generating group values
    rSlopes <- rnorm(nGroups,popSlopeL1,slopeVar) #Generating a slope for each group, centered around the level 1 population level slope
    rInts <- rnorm(nGroups,popInt,intVar) #Generating an intercept for each group, centered around the population intercept
    xOffset<-rnorm(nGroups,5,10) #Creating a value to offset each group's x variable - needed to create distinct patches of data
    weights<-round(rnorm(nGroups,5,5),0) #our Level 2 variable for each group
    
    
    for (i in 1:nGroups) {
        #Figuring out the boundaries for the group
        iMax = groupN*i 
        iMin = iMax-(groupN)+1
        
        #Applying our x offset
        x[iMin:iMax]<-x[iMin:iMax]+xOffset[i]
        
        #Creating y values from input values
        y[iMin:iMax]<-((x[iMin:iMax]*rSlopes[i])+(rInts[i])+(weights[i]*popSlopeL2)+(noise*rnorm(groupN,0,1)+(weights[i]*x[iMin:iMax]*popInter)))
        
        #Associating a group and weight with each data point
        groups[iMin:iMax]<-rep(i,groupN)
        weightsFac[iMin:iMax]<-rep(weights[i],groupN)
    }
    
    #Adding generated values to a data frame and returning it
    df <- data.frame("Level1"=x,y=y,"groups"=as.factor(groups),"Level2"=weightsFac)
    return(df)
}
ui <- fluidPage(
    titlePanel("Random Coefficients Modeling"),
    sidebarLayout(
        sidebarPanel(
            actionButton("save", 
                         "Save Data as .CSV",
                         width="100%"),
            sliderInput("nGroups",
                        "Number of groups:",
                        min = 1,
                        max = 50,
                        value = 25),
            sliderInput("groupN",
                        "Size of groups:",
                        min = 1,
                        max = 1000,
                        value = 200),
            sliderInput("popInt",
                        "Population Intercept:",
                        min = 1,
                        max = 100,
                        value = 50),
            sliderInput("popSlopeL1",
                        "Population Slope (L1):",
                        min = -5,
                        max = 5,
                        value = 0.05,
                        step = 0.05),
            sliderInput("popSlopeL2",
                        "Population Slope (L2):",
                        min = -5,
                        max = 5,
                        value = 0.05,
                        step = 0.05),
            sliderInput("popInter",
                        "L1xL2 Interaction",
                        min = -5,
                        max = 5,
                        value = 0.05,
                        step = 0.05),
            sliderInput("intVar",
                        "Intercept SD:",
                        min = 0,
                        max = 100,
                        value = 5),
            sliderInput("slopeVar",
                        "Slope SD:",
                        min = 0,
                        max = 5,
                        value = 0.05,
                        step = 0.05),
            sliderInput("noise",
                        "L1 Error:",
                        min = 0,
                        max = 50,
                        value = 5,
                        step = 1)
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("OLS Model",
                         plotOutput("olsPlot"),
                         verbatimTextOutput("olsOut")),
                tabPanel("LME Model",
                         plotOutput("rcmPlot"),
                         verbatimTextOutput("LMEout"))
                )
            )
    )
)
server <- function(input, output, session) {
    observeEvent(input$save, {
        write.csv(makeDF(),"data.csv")
        session$sendCustomMessage(type = 'Message',
                                  message = paste("File written to",getwd()))
    })
    makeDF <- reactive({
        getDat(input$nGroups,input$groupN,input$popInt,input$popSlopeL1,input$popSlopeL2, input$popInter, input$intVar,input$slopeVar,input$noise)
    })
    output$rcmPlot <- renderPlot({
        plot <- ggplot(data=makeDF(),aes(x=Level1,y=y))+
            geom_point(aes(color=as.factor(groups)))+
            geom_smooth(method="lm",color="black")+
            geom_smooth(method="loess",color="blue")+            
            geom_hline(yintercept=0,lty=2)+
            geom_vline(xintercept=0,lty=2)+
            theme_classic()
        plot
    })
    output$olsPlot <- renderPlot({
        plot <- ggplot(data=makeDF(),aes(x=Level1,y=y))+
            geom_point()+
            geom_smooth(method="lm")+            
            geom_hline(yintercept=0,lty=2)+
            geom_vline(xintercept=0,lty=2)+
            theme_classic()
        plot
    })
    output$olsOut <- renderPrint({
      summary(lm(y~Level1*Level2,data=makeDF()))
    })
    output$LMEout <- renderPrint({
      summary(lmer(y~Level1*Level2+(Level1|groups),data=makeDF()))
    })
}
shinyApp(ui = ui, server = server)
