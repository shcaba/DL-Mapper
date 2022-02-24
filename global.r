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
#scores.fish<-data.frame(matrix(rep(0,(dnum+rnum)*length(fishery.labs)),
#                               length(fishery.labs),dnum+rnum,byrow = TRUE))
colnames(scores)<-c("Data: Type","Data: Precision","Data: Bias","Data: Species ID","Data: Temporal","Data: Coverage","Res: Time","Res: Funding","Res: Capacity","Res: analysts/stocks")
DL_parcoor_comp<-data.frame(Scenario=c("No constraints","Resources constraints","Data constraints","Data & Resource constraints"),Shapes=21,scores)
#DL_parcoor_comp.fish<-data.frame(Scenario=fishery.labs,Shapes=8,scores.fish)

#DL_parcoor_comp<-rbind(DL_parcoor_comp,DL_parcoor_comp.fish)
