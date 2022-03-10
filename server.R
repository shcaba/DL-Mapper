
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
        colnames(scores)<-c("Data: Type","Data: Precision","Data: Bias","Data: Species ID","Data: Spatial","Data: Temporal","Res: Time","Res: Funding","Res: Capacity","Res: analysts/stocks")
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
      scores.fish<-data.frame(matrix(rep(3,(dnum+rnum)*length(fishery.labs)),
                                       length(fishery.labs),dnum+rnum,byrow = TRUE))
      colnames(scores)<-colnames(scores.fish)<-c("Data: Type","Data: Precision","Data: Bias","Data: Species ID","Data: Spatial","Data: Temporal","Res: Time","Res: Funding","Res: Capacity","Res: analysts/stocks")
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
    
    observeEvent(input$fishery_choice,{
      if(any(DL_comps$scores$Scenario==input$fishery_choice)){dtype.in<-DL_comps$scores[DL_comps$scores$Scenario==input$fishery_choice,3]}
      else (dtype.in<-3)
      updateSliderInput(session,"D_type",
                        "Types",
                        min = 0,
                        max = 3,
                        step=0.1,
                        #value = DL_comps$scores[DL_comps$scores$Scenario==input$fishery_choice,3])
                        value = dtype.in)
    }
    )
    
    observeEvent(input$fishery_choice,{
      if(any(DL_comps$scores$Scenario==input$fishery_choice)){dprez.in<-DL_comps$scores[DL_comps$scores$Scenario==input$fishery_choice,4]}
      else (dprez.in<-3)
      updateSliderInput(session,"D_prez",
                        "Imprecision",
                        min = 0,
                        max = 3,
                        step=0.1,
                        #value = DL_comps$scores[DL_comps$scores$Scenario==input$fishery_choice,4])
                        value = dprez.in)
    }
    )
    
    observeEvent(input$fishery_choice,{
      if(any(DL_comps$scores$Scenario==input$fishery_choice)){dbias.in<-DL_comps$scores[DL_comps$scores$Scenario==input$fishery_choice,5]}
      else (dbias.in<-3)
      updateSliderInput(session,"D_bias",
                        "Bias",
                        min = 0,
                        max = 3,
                        step=0.1,
#                        value = DL_comps$scores[DL_comps$scores$Scenario==input$fishery_choice,5])
                        value = dbias.in)
    }
    )
    
    observeEvent(input$fishery_choice,{
      if(any(DL_comps$scores$Scenario==input$fishery_choice)){dspp.in<-DL_comps$scores[DL_comps$scores$Scenario==input$fishery_choice,6]}
      else (dspp.in<-3)
      updateSliderInput(session,"D_spp",
                        "Species-specific",
                        min = 0,
                        max = 3,
                        step=0.1,
                        value = dspp.in)
      #value = DL_comps$scores[DL_comps$scores$Scenario==input$fishery_choice,6])
      }
    )
    
    observeEvent(input$fishery_choice,{
      if(any(DL_comps$scores$Scenario==input$fishery_choice)){dspat.in<-DL_comps$scores[DL_comps$scores$Scenario==input$fishery_choice,7]}
      else (dspat.in<-3)
      updateSliderInput(session,"D_spatial",
                        "Spatial limitations",
                        min = 0,
                        max = 3,
                        step=0.1,
                        value = dspat.in)
      #value = DL_comps$scores[DL_comps$scores$Scenario==input$fishery_choice,7])
      }
    )

    observeEvent(input$fishery_choice,{
      if(any(DL_comps$scores$Scenario==input$fishery_choice)){dtemp.in<-DL_comps$scores[DL_comps$scores$Scenario==input$fishery_choice,8]}
      else (dtemp.in<-3)
      updateSliderInput(session,"D_temp",
                        "Temporal limitations",
                        min = 0,
                        max = 3,
                        step=0.1,
                        value = dtemp.in)
      #value = DL_comps$scores[DL_comps$scores$Scenario==input$fishery_choice,8])
      }
    )
    
    observeEvent(input$fishery_choice,{
      if(any(DL_comps$scores$Scenario==input$fishery_choice)){rtime.in<-DL_comps$scores[DL_comps$scores$Scenario==input$fishery_choice,9]}
      else (rtime.in<-3)
      updateSliderInput(session,"R_time",
                        "Time",
                        min = 0,
                        max = 3,
                        step=0.1,
                        value = rtime.in)
      #value = DL_comps$scores[DL_comps$scores$Scenario==input$fishery_choice,9])
      }
    )
    
    observeEvent(input$fishery_choice,{
      if(any(DL_comps$scores$Scenario==input$fishery_choice)){rfunds.in<-DL_comps$scores[DL_comps$scores$Scenario==input$fishery_choice,10]}
      else (rfunds.in<-3)
      updateSliderInput(session,"R_funds",
                        "Funding",
                        min = 0,
                        max = 3,
                        step=0.1,
                        value = rfunds.in)
      #value = DL_comps$scores[DL_comps$scores$Scenario==input$fishery_choice,10])
      }
    )
    
    observeEvent(input$fishery_choice,{
      if(any(DL_comps$scores$Scenario==input$fishery_choice)){rcap.in<-DL_comps$scores[DL_comps$scores$Scenario==input$fishery_choice,11]}
      else (rcap.in<-3)
      updateSliderInput(session,"R_cap",
                        "Technical capacity",
                        min = 0,
                        max = 3,
                        step=0.1,
                        value = rcap.in)
      #value = DL_comps$scores[DL_comps$scores$Scenario==input$fishery_choice,11])
      }
    )
    
    observeEvent(input$fishery_choice,{
      if(any(DL_comps$scores$Scenario==input$fishery_choice)){ranst.in<-DL_comps$scores[DL_comps$scores$Scenario==input$fishery_choice,12]}
      else (ranst.in<-3)
      updateSliderInput(session,"R_an2stocks",
                        "Analysts:Stocks",
                        min = 0,
                        max = 3,
                        step=0.1,
                        value = ranst.in)
      #value = DL_comps$scores[DL_comps$scores$Scenario==input$fishery_choice,12])
      }
    )
    
#Create object that accumulates fisheries scores    
    DL_comps<-reactiveValues()
    DL_comps$scores<-DL_parcoor_comp
    
#Reactive updating of fisheries scores
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
          DL_parcoor<-archtypes.scores()[4,-c(1,2)]
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
#        DR_plot_lollipop<-melt(DL_parcoor)
        DR_labels<-c("Data: #Types","Data: Precision","Data: Bias","Data: Spp ID","Data: Spatial","Data: Temporal", "Res: Time","Res: Funding","Res: Capacity","Res: Analysts:Stocks")
        DR_plot_lollipop<-data.frame(Attribute=DR_labels,Score=as.numeric(DL_parcoor),Type="A")
        DR_plot_lollipop$Attribute<-factor(DR_plot_lollipop$Attribute,levels=rev(unique(DR_plot_lollipop$Attribute)))
#        DR_plot_lollipop$Type<-"A"
        DR_plot_lollipop$Type[grep("Data",DR_plot_lollipop$Attribute)]<-"Data"
        DR_plot_lollipop$Type[grep("Res",DR_plot_lollipop$Attribute)]<-"Resource"
        #DR_plot_lollipop_sub<-subset(DR_plot_lollipop,Scenario==Scenario[nrow(DR_plot_lollipop)])
        lolliggplot<-ggplot(DR_plot_lollipop, aes(x=Attribute, y=Score,color=Type)) +
            geom_point(size=6) + 
            scale_y_continuous(limits=c(0,3))+
            ggtitle(input$fishery_choice)+
            coord_flip() +
            geom_segment( aes(x=Attribute, xend=Attribute, y=0, yend=Score),lwd=2)+
            theme(legend.position = "none")+
            xlab("Attributes")+
            ylab("Constraint score")
        print(lolliggplot)
        output$downloadlollipopplots <- downloadHandler(
            filename = function() { paste0('Lollipop',timestamp, '.png')},
            content = function(file) {
                png(file, type='cairo',width=800,height=720)
                print(lolliggplot)
                dev.off()},contentType = 'image/png') 
        
        output$downloadAttScores <- downloadHandler(
          filename = function() {paste0("Attribute_Scores.csv") },
          content = function(file) {write.csv(DL_parcoor_scores(), file=file)}
        )
        
            })
    

    output$LolliPlot.principles <- renderPlot({
      if(length(unlist(strsplit(input$Spp_lab,split=",")))==0|input$fishery_choice=="")
      {
        DL_parcoor<-archtypes.scores()[4,-c(1,2)]
      }
      if(length(unlist(strsplit(input$Spp_lab,split=",")))>0&input$fishery_choice!="")
      {
        if(any(DL_parcoor_scores()$Scenario==input$fishery_choice))
        {
          DL_parcoor<-DL_parcoor_scores()[DL_parcoor_scores()$Scenario==input$fishery_choice,3:12]
        }
        if(any(DL_parcoor_scores()$Scenario==input$fishery_choice))
        {
          DL_parcoor<-data.frame(matrix(data=c(input$D_type,
                                               input$D_prez,
                                               input$D_bias,
                                               input$D_spp,
                                               input$D_spatial,
                                               input$D_temp,
                                               input$R_time,
                                               input$R_funds,
                                               input$R_cap,
                                               input$R_an2stocks),
                                               nrow=1))
          colnames(DL_parcoor)<-colnames(DL_parcoor_scores()[-c(1,2)])
        }
        else(DL_parcoor<-archtypes.scores()[1,-c(1,2)])
      }
      
      #Guidance scoring
      DR_labels<-c("Data: #Types","Data: Precision","Data: Bias","Data: Spp ID","Data: Spatial","Data: Temporal", "Res: Time","Res: Funding","Res: Capacity","Res: Analysts:Stocks")
      DR_plot_lollipop<-data.frame(Attribute=DR_labels,Score=as.numeric(DL_parcoor),Type="A")
      DR_plot_lollipop$Attribute<-factor(DR_plot_lollipop$Attribute,levels=rev(unique(DR_plot_lollipop$Attribute)))
      DR_plot_lollipop$Type[grep("Data",DR_plot_lollipop$Attribute)]<-"Data"
      DR_plot_lollipop$Type[grep("Res",DR_plot_lollipop$Attribute)]<-"Resource"
      Principle.names<-c("Data training", "Improve data","Local input","Analytical training","Simple methods","Complex models","Static MMs","Dynamic CRs","Improve Mod. Specs.","Improve governance")
      Principle.scores<-rep(NA,length(Principle.names))
      
      ###
      # #Train on data
      # if(DR_plot_lollipop$Score[1]==3){Principle.scores[1]<-3}
      # if(DR_plot_lollipop$Score[1]<3){Principle.scores[1]<-mean(DR_plot_lollipop$Score[c(1:6,9)])}
      # #Improve data
      # Principle.scores[2]<-mean(DR_plot_lollipop$Score[c(1:6,10)])
      # #Local Knowledge
      # if(DR_plot_lollipop$Score[1]==3){Principle.scores[3]<-3}
      # if(DR_plot_lollipop$Score[1]<3){Principle.scores[3]<-mean(mean(DR_plot_lollipop$Score[1:6]),mean(DR_plot_lollipop$Score[7:10]))}
      # #Train on assessments
      # Principle.scores[4]<-mean(DR_plot_lollipop$Score[9],mean(DR_plot_lollipop$Score[1:6]),mean(DR_plot_lollipop$Score[c(7,8,10)]))
      # #Do DL assessments
      # if(mean(DR_plot_lollipop$Score[c(1,7:10)])==3){Principle.scores[5]<-0}
      # if(mean(DR_plot_lollipop$Score[c(1,7:10)])<3){Principle.scores[5]<-mean(DR_plot_lollipop$Score[c(1,7:10)])}
      # #Do more complex methods
      # if(DR_plot_lollipop$Score[1]==3){Principle.scores[6]<-0}
      # if(DR_plot_lollipop$Score[1]<3){Principle.scores[6]<-mean(3-mean(DR_plot_lollipop$Score[1:6]),3-mean(DR_plot_lollipop$Score[7:10]))}
      # #Static MMs
      # Principle.scores[7]<-max(mean(DR_plot_lollipop$Score[c(1:6)]),mean(DR_plot_lollipop$Score[c(7:10)]))
      # #Dynamics MMs
      # Principle.scores[8]<-mean(3-DR_plot_lollipop$Score[1],3-mean(DR_plot_lollipop$Score[c(5,7:10)]))
      # #Improve Model specifications
      # if(DR_plot_lollipop$Score[1]==3|mean(DR_plot_lollipop$Score[7:10]==3)){Principle.scores[9]<-0}
      # else {Principle.scores[9]<-mean(3-mean(DR_plot_lollipop$Score[1:6]),3-mean(DR_plot_lollipop$Score[7:10]))}
      # #Improve governance
      # if(mean(DR_plot_lollipop$Score[7:9])>=2.5){Principle.scores[10]<-3}
      # if(mean(DR_plot_lollipop$Score[7:9])<2.5){Principle.scores[10]<-mean(DR_plot_lollipop$Score[c(1,4,7:9)])}
      #####

      #Train on data
      if(mean(DR_plot_lollipop$Score[1:6])>=input$traindata){Principle.scores[1]<-3}
      if(mean(DR_plot_lollipop$Score[1:6])<input$traindata){Principle.scores[1]<-mean(DR_plot_lollipop$Score[c(1:6,9)])}
      #Improve data
      Principle.scores[2]<-mean(DR_plot_lollipop$Score[c(1:6,10)])
      #Local Knowledge
      if(mean(DR_plot_lollipop$Score[1:6])>=input$locknow){Principle.scores[3]<-3}
      if(mean(DR_plot_lollipop$Score[1:6])<input$locknow){Principle.scores[3]<-mean(c(mean(DR_plot_lollipop$Score[1:6]),mean(DR_plot_lollipop$Score[7:10])))}
      #Train on assessments
      Principle.scores[4]<-mean(c(DR_plot_lollipop$Score[9],mean(DR_plot_lollipop$Score[1:6]),mean(DR_plot_lollipop$Score[c(7,8,10)])))
      #Do DL assessments
      if(mean(DR_plot_lollipop$Score[c(1,7:10)])>=input$DoDL){Principle.scores[5]<-0}
      if(mean(DR_plot_lollipop$Score[c(1,7:10)])<input$DoDL){Principle.scores[5]<-mean(DR_plot_lollipop$Score[c(1,7:10)])}
      #Do more complex methods
      if(mean(DR_plot_lollipop$Score[1:6])>=input$Docomplex|mean(DR_plot_lollipop$Score[7:10])>=input$Docomplex){Principle.scores[6]<-0}
      else {Principle.scores[6]<-mean(c(3-mean(DR_plot_lollipop$Score[1:6]),3-mean(DR_plot_lollipop$Score[7:10])))}
      #Static MMs
      Principle.scores[7]<-max(mean(DR_plot_lollipop$Score[c(1:6)]),mean(DR_plot_lollipop$Score[c(7:10)]))
      #Dynamics MMs
      Principle.scores[8]<-mean(c(3-DR_plot_lollipop$Score[1],3-mean(DR_plot_lollipop$Score[c(5,7:10)])))
      #Improve Model specifications
      if(mean(DR_plot_lollipop$Score[1:6])>=input$Modspecs|mean(DR_plot_lollipop$Score[7:10])>=input$Modspecs){Principle.scores[9]<-0}
      else {Principle.scores[9]<-mean(c(3-mean(DR_plot_lollipop$Score[1:6]),3-mean(DR_plot_lollipop$Score[7:10])))}
      #Improve governance
      if(mean(DR_plot_lollipop$Score[7:9])>=input$Gov){Principle.scores[10]<-3}
      if(mean(DR_plot_lollipop$Score[7:9])<input$Gov){Principle.scores[10]<-mean(DR_plot_lollipop$Score[c(1,4,7:9)])}
      #####
      
      Guidance_plot_lollipop<-data.frame(Names=Principle.names,Scores=Principle.scores,Type=1)
      Guidance_plot_lollipop$Names<-factor(Guidance_plot_lollipop$Names,levels=rev(unique(Guidance_plot_lollipop$Names)))
      Lolliggplot.principles<-ggplot(Guidance_plot_lollipop, aes(x=Names, y=Scores,color=Type)) +
        geom_point(size=6) + 
        scale_y_continuous(limits=c(0,3))+
        ggtitle(input$fishery_choice)+
        coord_flip() +
        geom_segment( aes(x=Names, xend=Names, y=0, yend=Scores),lwd=2)+
        theme(legend.position = "none")+
        xlab("Guidance options")+
        ylab("Recommendation score")
      print(Lolliggplot.principles)
      output$downloadlollipop.principles <- downloadHandler(
        filename = function() { paste0('Lolliggplot.principles',timestamp, '.png')},
        content = function(file) {
          png(file, type='cairo',width=800,height=720)
          print(Lolliggplot.principles)
          dev.off()},contentType = 'image/png')
      
      output$downloadGuideScores <- downloadHandler(
        filename = function() {paste0("Guidance_Scores.csv") },
        content = function(file) {write.csv(Guidance_plot_lollipop, file=file)}
      )
      
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
            geom_point(size=3,shape=DR_plot$Shapes)+
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
        DL_parcoor<-archtypes.scores()[,-2]
        #DL_parcoor<-DL_parcoor_scores()[,-2]  
      }
      if(is.null(input$fishery_compare)==FALSE)
      {
        Scenario_comp<-DL_parcoor_scores()$Scenario%in%c(DL_parcoor_scores()$Scenario[1:4],input$fishery_compare)
        DL_parcoor<-DL_parcoor_scores()[Scenario_comp,-2]
      }
#      width = 800, height = 400,              #layout(margin=list(t=0))

        DL_parcoor$colorsin<-1:nrow(DL_parcoor)
        plot_ly(DL_parcoor,type = 'parcoords',labelside="bottom",labelfont=list(size=10),labelangle=-15,
                line = list(color = ~colorsin,
                            colorscale = list(c(0,'red'),c(1,'green'),c(2,'blue'))),
                      
                 dimensions = list(
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = "# Types", values = as.formula(paste("~",colnames(DL_parcoor[2])))),
 #                               label = colnames(DL_parcoor)[2], values = as.formula(paste("~",colnames(DL_parcoor[2])))),
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = "Precision", values = as.formula(paste("~",colnames(DL_parcoor[3])))),
#                                label = colnames(DL_parcoor)[3], values = as.formula(paste("~",colnames(DL_parcoor[3])))),
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = "Bias", values = as.formula(paste("~",colnames(DL_parcoor[4])))),                                
#                                label = colnames(DL_parcoor)[4], values = as.formula(paste("~",colnames(DL_parcoor[4])))),
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = "Species ID", values = as.formula(paste("~",colnames(DL_parcoor[5])))),
                                #label = colnames(DL_parcoor)[5], values = as.formula(paste("~",colnames(DL_parcoor[5])))),
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = "Temporal", values = as.formula(paste("~",colnames(DL_parcoor[6])))),
                                #label = colnames(DL_parcoor)[6], values = as.formula(paste("~",colnames(DL_parcoor[6])))),
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = "Spatial", values = as.formula(paste("~",colnames(DL_parcoor[7])))),
                                #label = colnames(DL_parcoor)[7], values = as.formula(paste("~",colnames(DL_parcoor[7])))),
                           list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = "Time", values = as.formula(paste("~",colnames(DL_parcoor[8])))),
                                #label = colnames(DL_parcoor)[8], values = as.formula(paste("~",colnames(DL_parcoor[8])))),
                            list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = "Funding", values = as.formula(paste("~",colnames(DL_parcoor[9])))),
                                #label = colnames(DL_parcoor)[9], values = as.formula(paste("~",colnames(DL_parcoor[9])))),
                            list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = "Capacity", values = as.formula(paste("~",colnames(DL_parcoor[10])))),
                                #label = colnames(DL_parcoor)[10], values = as.formula(paste("~",colnames(DL_parcoor[10])))),
                            list(range = c(0,3),
                                #constraintrange = c(0,3),
                                tickvals = c(0,1,2,3),
                                label = "analysts:stocks", values = as.formula(paste("~",colnames(DL_parcoor[11]))))
                                #label = colnames(DL_parcoor)[11], values = as.formula(paste("~",colnames(DL_parcoor[11]))))
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
