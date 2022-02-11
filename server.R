
library(shiny)
library(ggplot2)
library(GGally)
library(hrbrthemes)
library(viridis)
library(plotly)
library(reshape2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {


    archtypes.scores<-function()
    {
        dnum<-6
        rnum<-4
        scores<-data.frame(matrix(c(rep(0,dnum+rnum),
                                    c(rep(0,dnum),rep(3,rnum)),
                                    c(rep(3,dnum),rep(0,rnum)),
                                    rep(3,dnum+rnum),
                                    c(runif(dnum,0,1),runif(rnum,2,3)),
                                    c(runif(dnum,0,1),runif(rnum,2,3)),
                                    c(runif(dnum,0,1),runif(rnum,2,3)),
                                    c(runif(dnum,0,1),runif(rnum,2,3)),
                                    c(runif(dnum,0,1),runif(rnum,2,3)),
                                    c(runif(dnum,2,3),runif(rnum,0,1)),
                                    c(runif(dnum,2,3),runif(rnum,0,1)),
                                    c(runif(dnum,2,3),runif(rnum,0,1)),
                                    c(runif(dnum,2,3),runif(rnum,0,1)),
                                    c(runif(dnum,2,3),runif(rnum,0,1))),
                                  14,dnum+rnum,byrow = TRUE))
        colnames(scores)<-c(paste0("Data_",1:6),paste0("Resr_",1:4))
        DL_parcoor<-data.frame(Scenario=c("No_DR","No_D","No_R","DR",paste0("S_",1:10)),scores)
        return(DL_parcoor)
    }
    ###############
    
        output$QuadPlot <- renderPlot({
        ### Run quadplot ####
            DL_parcoor<-archtypes.scores()
            DL_parcoor<-rbind(DL_parcoor,DL_parcoor[1,])
            DL_parcoor[nrow(DL_parcoor),]<-data.frame(input$Spp_lab,input$D_type,input$D_prez,input$D_bias,input$D_spp,input$D_spatial,input$D_temp,input$R_time,input$R_funds,input$R_cap,input$R_an2stocks)
            DR_plot<-data.frame(Scenario=DL_parcoor$Scenario,Data=rowMeans(DL_parcoor[,2:7]),Resources=rowMeans(DL_parcoor[,9:11]))
        ggplot(DR_plot,aes(Data,Resources,color=Scenario))+
            geom_point(size=4)+
            geom_vline(xintercept=1.5,color="red",lty=2)+
            geom_hline(yintercept=1.5,color="red",lty=2)
    })

    output$ParCoorPlot <- renderPlotly({
        DL_parcoor<-archtypes.scores()
        DL_parcoor<-rbind(DL_parcoor,DL_parcoor[1,])
        DL_parcoor[nrow(DL_parcoor),]<-data.frame(input$Spp_lab,input$D_type,input$D_prez,input$D_bias,input$D_spp,input$D_spatial,input$D_temp,input$R_time,input$R_funds,input$R_cap,input$R_an2stocks)
        plot_ly(DL_parcoor,type = 'parcoords', 
                       line = list(list(color = ~Scenario,
                                        colorscale = list(c(0, 'red'), c(0.5, 'green'), c(1, 'blue')))),
                       dimensions = list(
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = colnames(DL_parcoor)[2], values = ~Data_1),
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = colnames(DL_parcoor)[3], values = ~Data_2),
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = colnames(DL_parcoor)[4], values = ~Data_3),
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = colnames(DL_parcoor)[5], values = ~Data_4),
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = colnames(DL_parcoor)[6], values = ~Data_5),
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = colnames(DL_parcoor)[7], values = ~Data_6),
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = colnames(DL_parcoor)[8], values = ~Resr_1),
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = colnames(DL_parcoor)[9], values = ~Resr_2),
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = colnames(DL_parcoor)[10], values = ~Resr_3),
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = colnames(DL_parcoor)[11], values = ~Resr_4)
                       )
        )
    })
    
    
    output$LolliPlot <- renderPlot({
        #Lollipop
        DL_parcoor<-archtypes.scores()
        DL_parcoor<-rbind(DL_parcoor,DL_parcoor[1,])
        DL_parcoor[nrow(DL_parcoor),]<-data.frame(input$Spp_lab,input$D_type,input$D_prez,input$D_bias,input$D_spp,input$D_spatial,input$D_temp,input$R_time,input$R_funds,input$R_cap,input$R_an2stocks)
        DR_plot_lollipop<-melt(DL_parcoor)
    DR_plot_lollipop$Type<-c(rep("Data",nrow(DR_plot_lollipop)/2),rep("Resources",nrow(DR_plot_lollipop)/2))
    DR_plot_lollipop_sub<-subset(DR_plot_lollipop,Scenario==Scenario[nrow(DR_plot_lollipop)])
    lolliggplot<-ggplot(DR_plot_lollipop_sub, aes(x=variable, y=value,color=Type)) +
        geom_point(size=6) + 
        scale_y_continuous(limits=c(0,3))+
        ggtitle(input$Spp_lab)+
        coord_flip() +
        geom_segment( aes(x=variable, xend=variable, y=0, yend=value),lwd=2)+
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
})
