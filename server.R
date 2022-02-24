
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
    
    
    archtypes.scores.comp<-function(fishery.labs)
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
      scores.fish<-data.frame(matrix(rep(0,(dnum+rnum)*length(fishery.labs)),
                                       length(fishery.labs),dnum+rnum,byrow = TRUE))
      colnames(scores)<-colnames(scores.fish)<-c("Data: Type","Data: Precision","Data: Bias","Data: Species ID","Data: Temporal","Data: Coverage","Res: Time","Res: Funding","Res: Capacity","Res: analysts/stocks")
      DL_parcoor_comp<-data.frame(Scenario=c("No constraints","Resources constraints","Data constraints","Data & Resource constraints"),Shapes=21,scores)
      DL_parcoor_comp.fish<-data.frame(Scenario=fishery.labs,Shapes=8,scores.fish)
      
      #      for(i in 1:length(fishery.labs)){
 #       DL_parcoor_comp_temp<-data.frame(fishery.labs[i],Shapes=8,colnames(scores)[1]=0,colnames(scores)[2]=0,colnames(scores)[3]=0,colnames(scores)[4]=0,colnames(scores)[5]=0,colnames(scores)[6]=0,colnames(scores)[7]=0,colnames(scores)[8]=0,colnames(scores)[9]=0,colnames(scores)[10]=0))
  #    }
      DL_parcoor_comp<-rbind(DL_parcoor_comp,DL_parcoor_comp.fish)
      return(DL_parcoor_comp)
    }
    ###############
  
################################      
### Create objects for plots ###
################################
    
    observeEvent(input$Spp_lab,{
        updateSelectizeInput(session, "fishery_choice", choices = strsplit(input$Spp_lab,split=",")[[1]], server = TRUE)
    })    
    observeEvent(input$Spp_lab,{
      updateSelectizeInput(session, "fishery_compare", choices = strsplit(input$Spp_lab,split=",")[[1]], server = TRUE)
    })    
    
    DL_comps<-reactiveValues()
    DL_comps$scores<-DL_parcoor_comp
    
    DL_parcoor_scores<-eventReactive(input$D_type|input$D_prez|input$D_bias|input$D_spp|input$D_spatial|input$D_temp|input$R_time|input$R_funds|input$R_cap|input$R_an2stocks,{
      if(length(unlist(strsplit(input$Spp_lab,split=",")))>0)
      {
        if(any(DL_comps$scores$Scenario==input$fishery_choice))
        {
          it.scenario<-DL_comps$scores$Scenario==input$fishery_choice
          DL_comps$scores[it.scenario,3:12]<-c(input$D_type,input$D_prez,input$D_bias,input$D_spp,input$D_spatial,input$D_temp,input$R_time,input$R_funds,input$R_cap,input$R_an2stocks)
          #mapply(function(x) rbind(DL_comps[[x]],))
          #          DL_comps$scores<-rbind(isolate(DL_comps$scores),isolate(DL_comps[[input$fishery_choice]]))
          DL_parcoor_scores<-DL_comps$scores
          # DL_parcoor_comp[it.scenario,3:12]<-c(input$D_type,input$D_prez,input$D_bias,input$D_spp,input$D_spatial,input$D_temp,input$R_time,input$R_funds,input$R_cap,input$R_an2stocks)
          # DL_parcoor_scores<-DL_parcoor_comp
        }
        if(all(DL_comps$scores$Scenario!=input$fishery_choice))
        {
          #df.scores<-data.frame(Scenario=input$fishery_choice,Shapes=8,matrix(data=c(input$D_type,input$D_prez,input$D_bias,input$D_spp,input$D_spatial,input$D_temp,input$R_time,input$R_funds,input$R_cap,input$R_an2stocks),nrow=1))
          #names(df.scores)<-names(DL_parcoor_comp)
          DL_comps_temp<-data.frame(Scenario=input$fishery_choice,Shapes=8,matrix(data=c(input$D_type,input$D_prez,input$D_bias,input$D_spp,input$D_spatial,input$D_temp,input$R_time,input$R_funds,input$R_cap,input$R_an2stocks),nrow=1))
          colnames(DL_comps_temp)<-colnames(DL_comps$scores)
          DL_comps$scores<-rbind(DL_comps$scores,DL_comps_temp)
          DL_parcoor_scores<-DL_comps$scores
          #names(DL_parcoor_scores)<-names(DL_parcoor_comp)
          #DL_parcoor_comp<-rbind(DL_parcoor_comp,df.scores)
          #DL_parcoor_scores<-DL_parcoor_comp
        }
      }
      else(DL_parcoor_scores<-DL_parcoor_comp)
      DL_parcoor_scores
    })
    
    
#############
### PLOTS ###
#############

    output$LolliPlot <- renderPlot({
        #Lollipop
        #DL_parcoor<-archtypes.scores()
        #DL_parcoor<-rbind(DL_parcoor,DL_parcoor[1,])
        #DL_parcoor<-DL_parcoor[,-2]
        #DL_parcoor[nrow(DL_parcoor),]<-data.frame(input$Spp_lab,input$D_type,input$D_prez,input$D_bias,input$D_spp,input$D_spatial,input$D_temp,input$R_time,input$R_funds,input$R_cap,input$R_an2stocks)
      if(length(unlist(strsplit(input$Spp_lab,split=",")))==0|input$fishery_choice=="")
        {
          DL_parcoor<-archtypes.scores()[1,-c(1,2)]
#          DL_parcoor<-rbind(DL_parcoor,DL_parcoor[1,])
#          DL_parcoor<-DL_parcoor[,-c(2,3)]
#          DL_parcoor[nrow(DL_parcoor),]<-data.frame(input$Spp_lab,input$D_type,input$D_prez,input$D_bias,input$D_spp,input$D_spatial,input$D_temp,input$R_time,input$R_funds,input$R_cap,input$R_an2stocks)
        }
      if(length(unlist(strsplit(input$Spp_lab,split=",")))>0&input$fishery_choice!="")
        {
        if(any(DL_parcoor_scores()$Scenario==input$fishery_choice))
        {
          DL_parcoor<-DL_parcoor_scores()[DL_parcoor_scores()$Scenario==input$fishery_choice,3:12]
        }
        if(any(DL_parcoor_scores()$Scenario==input$fishery_choice))
        {
          DL_parcoor<-data.frame(matrix(data=c(input$D_type,input$D_prez,input$D_bias,input$D_spp,input$D_spatial,input$D_temp,input$R_time,input$R_funds,input$R_cap,input$R_an2stocks),nrow=1))
          colnames(DL_parcoor)<-colnames(DL_parcoor_scores()[-c(1,2)])
        }
        else(DL_parcoor<-archtypes.scores()[1,-c(1,2)])
      }
        DR_plot_lollipop<-melt(DL_parcoor)
        DR_plot_lollipop$Type<-"A"
        DR_plot_lollipop$Type[grep("Data",DR_plot_lollipop$variable)]<-"Data"
        DR_plot_lollipop$Type[grep("Res",DR_plot_lollipop$variable)]<-"Resource"
        #DR_plot_lollipop_sub<-subset(DR_plot_lollipop,Scenario==Scenario[nrow(DR_plot_lollipop)])
        lolliggplot<-ggplot(DR_plot_lollipop, aes(x=variable, y=value,color=Type)) +
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
    
         
         ### Run quadplot ####
    output$QuadPlot <- renderPlotly({
#            print(DL_parcoor_scores())
#            DL_parcoor<-archtypes.scores()
#            DL_parcoor<-rbind(DL_parcoor,DL_parcoor[1,])
#            DL_parcoor[nrow(DL_parcoor),]<-data.frame(input$fishery_choice,8,input$D_type,input$D_prez,input$D_bias,input$D_spp,input$D_spatial,input$D_temp,input$R_time,input$R_funds,input$R_cap,input$R_an2stocks)
             #DL_parcoor<-DL_parcoor_scores()
      if(length(unlist(strsplit(input$Spp_lab,split=",")))==0|input$fishery_choice=="")
        {DL_parcoor_in<-DL_parcoor_comp}
      if(length(unlist(strsplit(input$Spp_lab,split=",")))>0&input$fishery_choice!="")
      {DL_parcoor_in<-DL_parcoor_scores()}
      if(is.null(input$fishery_compare)){DR_plot<-data.frame(Scenario=DL_parcoor_comp$Scenario,Shapes=DL_parcoor_comp$Shapes,Data=rowMeans(DL_parcoor_comp[,3:8]),Resources=rowMeans(DL_parcoor_comp[,9:12]))}
      if(is.null(input$fishery_compare)==FALSE)
        {
          Scenario_comp<-DL_parcoor_in$Scenario%in%c(DL_parcoor_in$Scenario[1:4],input$fishery_compare)
          DR_plot<-data.frame(Scenario=DL_parcoor_in$Scenario[Scenario_comp],Shapes=DL_parcoor_in$Shapes[Scenario_comp],Data=rowMeans(DL_parcoor_in[Scenario_comp,3:8]),Resources=rowMeans(DL_parcoor_in[Scenario_comp,9:12]))
        }
      res<-ggplotly(ggplot(DR_plot,aes(Data,Resources,fill=Scenario))+
            geom_point(size=4,shape=DR_plot$Shapes)+
            theme(legend.position = "none")+
            geom_vline(xintercept=1.5,color="red",lty=2)+
            geom_hline(yintercept=1.5,color="red",lty=2))
        res
    })

    output$ParCoorPlot <- renderPlotly({
        # DL_parcoor<-archtypes.scores()
        # DL_parcoor<-rbind(DL_parcoor,DL_parcoor[1,])
        # DL_parcoor<-DL_parcoor[,-2]
        # DL_parcoor[nrow(DL_parcoor),]<-data.frame(input$Spp_lab,input$D_type,input$D_prez,input$D_bias,input$D_spp,input$D_spatial,input$D_temp,input$R_time,input$R_funds,input$R_cap,input$R_an2stocks)
      if(is.null(input$fishery_compare)==TRUE)
      {
        DL_parcoor<-archtypes.scores()
        #DL_parcoor<-DL_parcoor_scores()[,-2]  
      }
      if(is.null(input$fishery_compare)==FALSE)
      {
        Scenario_comp<-DL_parcoor_scores()$Scenario%in%c(DL_parcoor_scores()$Scenario[1:4],input$fishery_compare)
        DL_parcoor<-DL_parcoor_scores()[Scenario_comp,-2]
      }
      
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






#         DL_parcoor_comps<-eventReactive({length(unlist(strsplit(input$Spp_lab,split=",")))>0},{
#           if(input$Spp_lab==""){DL_parcoor_comps<-archtypes.scores.comp(input$Spp_lab)}
#           else{DL_parcoor_comps<-archtypes.scores.comp(unlist(strsplit(input$Spp_lab,split=",")))}
#           print(DL_parcoor_comps[(nrow(DL_parcoor_comps)-1):nrow(DL_parcoor_comps),])
#           DL_parcoor_comps   
#        })
