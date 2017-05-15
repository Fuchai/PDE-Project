a<-array(rsectional[1],dim=c(timestepprecision,N,N))

for (r in rsectional){
    abind(a,aray(r,dim=c(timestepprecision,N,N)),along=4)
}

ggplot()+
    geom_point(aes(x=1:100,y=quarteraveragemoisture,color='quarter'))+
    geom_point(aes(x=1:50,y=halfaveragemoisture,color='half'))