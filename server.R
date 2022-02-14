
library(shiny)
library(ggplot2)
library(GGally)
library(hrbrthemes)
library(viridis)
library(plotly)
library(reshape2)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {


    archtypes.scores<-function()
    {
        dnum<-6
        rnum<-4
        scores<-data.frame(matrix(c(rep(0,dnum+rnum),
                                    c(rep(0,dnum),rep(3,rnum)),
                                    c(rep(3,dnum),rep(0,rnum)),
                                    rep(3,dnum+rnum)),
      #                              c(runif(dnum,0,1),runif(rnum,2,3)),
      #                              c(runif(dnum,0,1),runif(rnum,2,3)),
      #                              c(runif(dnum,0,1),runif(rnum,2,3)),
      #                              c(runif(dnum,0,1),runif(rnum,2,3)),
      #                              c(runif(dnum,0,1),runif(rnum,2,3)),
      #                              c(runif(dnum,2,3),runif(rnum,0,1)),
      #                              c(runif(dnum,2,3),runif(rnum,0,1)),
      #                              c(runif(dnum,2,3),runif(rnum,0,1)),
      #                              c(runif(dnum,2,3),runif(rnum,0,1)),
      #                              c(runif(dnum,2,3),runif(rnum,0,1))),
                                  4,dnum+rnum,byrow = TRUE))
        colnames(scores)<-c("Data: Type","Data: Precision","Data: Bias","Data: Species ID","Data: Temporal","Data: Coverage","Res: Time","Res: Funding","Res: Capacity","Res: analysts/stocks")
        DL_parcoor<-data.frame(Scenario=c("No constraints","Resources constraints","Data constraints","Data & Resource constraints"),Shapes=21,scores)
        return(DL_parcoor)
    }
    ###############
    observeEvent(input$Spp_lab,{
        updateSelectizeInput(session, "fishery_choice", choices = strsplit(input$Spp_lab,split=",")[[1]], server = TRUE)
    })    
    
    
    output$LolliPlot <- renderPlot({
        #Lollipop
        DL_parcoor<-archtypes.scores()
        DL_parcoor<-rbind(DL_parcoor,DL_parcoor[1,])
        DL_parcoor<-DL_parcoor[,-2]
        DL_parcoor[nrow(DL_parcoor),]<-data.frame(input$Spp_lab,input$D_type,input$D_prez,input$D_bias,input$D_spp,input$D_spatial,input$D_temp,input$R_time,input$R_funds,input$R_cap,input$R_an2stocks)
        DR_plot_lollipop<-melt(DL_parcoor)
        DR_plot_lollipop$Type<-"A"
        DR_plot_lollipop$Type[grep("Data",DR_plot_lollipop$variable)]<-"Data"
        DR_plot_lollipop$Type[grep("Res",DR_plot_lollipop$variable)]<-"Resource"
        DR_plot_lollipop_sub<-subset(DR_plot_lollipop,Scenario==Scenario[nrow(DR_plot_lollipop)])
        lolliggplot<-ggplot(DR_plot_lollipop_sub, aes(x=variable, y=value,color=Type)) +
            geom_point(size=6) + 
            scale_y_continuous(limits=c(0,3))+
            ggtitle(input$fishery_choice)+
            coord_flip() +
            geom_segment( aes(x=variable, xend=variable, y=0, yend=value),lwd=2)+
            theme(legend.position = "none")+
            xlab("Attributes")+
            ylab("Score")
        print(lolliggplot)
        output$downloadlollipopplots <- downloadHandler(
            filename = function() { paste0('Lollipop',timestamp, '.png')},
            content = function(file) {
                png(file, type='cairo',width=800,height=720)
                print(lolliggplot)
                dev.off()},contentType = 'image/png') 
    })
    
    output$QuadPlot <- renderPlotly({
        ### Run quadplot ####
            DL_parcoor<-archtypes.scores()
            DL_parcoor<-rbind(DL_parcoor,DL_parcoor[1,])
            DL_parcoor[nrow(DL_parcoor),]<-data.frame(input$fishery_choice,8,input$D_type,input$D_prez,input$D_bias,input$D_spp,input$D_spatial,input$D_temp,input$R_time,input$R_funds,input$R_cap,input$R_an2stocks)
            DR_plot<-data.frame(Scenario=DL_parcoor$Scenario,Shapes=DL_parcoor$Shapes,Data=rowMeans(DL_parcoor[,3:8]),Resources=rowMeans(DL_parcoor[,9:12]))
        res<-ggplotly(ggplot(DR_plot,aes(Data,Resources,fill=Scenario))+
            geom_point(size=4,shape=DR_plot$Shapes)+
            theme(legend.position = "none")+
            geom_vline(xintercept=1.5,color="red",lty=2)+
            geom_hline(yintercept=1.5,color="red",lty=2))
        res
    })

    output$ParCoorPlot <- renderPlotly({
        DL_parcoor<-archtypes.scores()
        DL_parcoor<-rbind(DL_parcoor,DL_parcoor[1,])
        DL_parcoor<-DL_parcoor[,-2]
        DL_parcoor[nrow(DL_parcoor),]<-data.frame(input$Spp_lab,input$D_type,input$D_prez,input$D_bias,input$D_spp,input$D_spatial,input$D_temp,input$R_time,input$R_funds,input$R_cap,input$R_an2stocks)
        DL_parcoor$colorsin<-1:nrow(DL_parcoor)
        plot_ly(DL_parcoor,type = 'parcoords', 
                line = list(color = ~colorsin,
                            colorscale = list(c(0,'red'),c(1,'green'),c(2,'blue'))),
                       dimensions = list(
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = colnames(DL_parcoor)[2], values = as.formula(paste("~",colnames(DL_parcoor[2])))),
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = colnames(DL_parcoor)[3], values = as.formula(paste("~",colnames(DL_parcoor[3])))),
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = colnames(DL_parcoor)[4], values = as.formula(paste("~",colnames(DL_parcoor[4])))),
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = colnames(DL_parcoor)[5], values = as.formula(paste("~",colnames(DL_parcoor[5])))),
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = colnames(DL_parcoor)[6], values = as.formula(paste("~",colnames(DL_parcoor[6])))),
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = colnames(DL_parcoor)[7], values = as.formula(paste("~",colnames(DL_parcoor[7])))),
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = colnames(DL_parcoor)[8], values = as.formula(paste("~",colnames(DL_parcoor[8])))),
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = colnames(DL_parcoor)[9], values = as.formula(paste("~",colnames(DL_parcoor[9])))),
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = colnames(DL_parcoor)[10], values = as.formula(paste("~",colnames(DL_parcoor[10])))),
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = colnames(DL_parcoor)[11], values = as.formula(paste("~",colnames(DL_parcoor[11]))))
                       )
        )
    })
    
})
