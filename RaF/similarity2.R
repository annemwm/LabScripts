### Similarity Approach2.0 ###
### aka similarity part 2: the electric boogaloo
## cos sim of characters ## 
setwd("~/Desktop/ANNE/scripts")
source("similarity.R")


a_score<-adjustment_score(asian, 100)
b_score<-adjustment_score(black, 100)
i_score<-adjustment_score(indigenous, 100)
l_score<-adjustment_score(latinx, 100)
n_score<-adjustment_score(neast, 100)
s_score<-adjustment_score(south, 100)
w_score<-adjustment_score(white, 100)

af_score<-adjustment_score(asian_f, 100)
bf_score<-adjustment_score(black_f, 100)
if_score<-adjustment_score(indigenous_f, 100)
lf_score<-adjustment_score(latinx_f, 100)
sf_score<-adjustment_score(south_f, 100)
wf_score<-adjustment_score(white_f, 100)

am_score<-adjustment_score(asian_m, 100)
bm_score<-adjustment_score(black_m, 100)
im_score<-adjustment_score(indigenous_m, 100)
lm_score<-adjustment_score(latinx_m, 100)
nm_score<-adjustment_score(neast_m, 100)
sm_score<-adjustment_score(south_m, 100)
wm_score<-adjustment_score(white_m, 100)

results<-cbind(c(a_score, af_score, am_score),c(b_score,bf_score,bm_score),c(i_score,if_score,im_score),
               c(l_score,lf_score,lm_score),c(n_score,0,nm_score),c(s_score,sf_score,sm_score),
               c(w_score,wf_score,wm_score))
row.names(results)<-c("all","women","men")
colnames(results)<-c("asian","black", "indig","latinx", "neast","south", "white")


adj_jacf<-jacf/results[2, -which(colnames(results) %in% "neast")]
adj_jacm<-jacm/results[3,]
adj_jacall<-jacall/results[1,]

box<-NULL
box<-kl_div(asian,box)
box<-kl_div(black,box)
box<-kl_div(indigenous,box)
box<-kl_div(latinx,box)
box<-kl_div(neast,box)
box<-kl_div(south,box)
box<-kl_div(white,box)

colnames(box)<-c("asian","black", "indig","latinx", "neast","south", "white")
boxa<-build_anova(data.frame(box))

boxf<-NULL
boxf<-kl_div(asian_f,boxf)
boxf<-kl_div(black_f,boxf)
boxf<-kl_div(indigenous_f,boxf)
boxf<-kl_div(latinx_f,boxf)
boxf<-kl_div(south_f,boxf)
boxf<-kl_div(white_f,boxf)

colnames(boxf)<-c("asian f","black f", "indig f","latinx f", "south f", "white f")
boxfa<-build_anova(data.frame(boxf))

boxm<-NULL
boxm<-kl_div(asian_m,boxm)
boxm<-kl_div(black_m,boxm)
boxm<-kl_div(indigenous_m,boxm)
boxm<-kl_div(latinx_m,boxm)
boxm<-kl_div(neast_m,boxm)
boxm<-kl_div(south_m,boxm)
boxm<-kl_div(white_m,boxm)

colnames(boxm)<-c("asian m","black m", "indig m","latinx m", "neast m","south m", "white m")
boxma<-build_anova(data.frame(boxm))


jacall<-NULL
jacall<-mult_jac(asian,500)
jacall<-cbind(jacall,mult_jac(black,500))
jacall<-cbind(jacall,mult_jac(indigenous,500))
jacall<-cbind(jacall,mult_jac(latinx,500))
jacall<-cbind(jacall,mult_jac(neast,500))
jacall<-cbind(jacall,mult_jac(south,500))
jacall<-cbind(jacall,mult_jac(white,500))

colnames(jacall)<-c("asian","black", "indig","latinx", "neast","south", "white")
summary(jacall)
anovajac<-build_anova(data.frame(jacall))
summary(anovajac)

jacf<-NULL
jacf<-mult_jac(asian_f,500)
jacf<-cbind(jacf,mult_jac(black_f,500))
jacf<-cbind(jacf,mult_jac(indigenous_f,500))
jacf<-cbind(jacf,mult_jac(latinx_f,500))
jacf<-cbind(jacf,mult_jac(south_f,500))
jacf<-cbind(jacf,mult_jac(white_f,500))

colnames(jacf)<-c("asian f","black f", "indig f","latinx f", "south f", "white f")
summary(jacf)
anovajacf<-build_anova(data.frame(jacf))
summary(anovajacf)

jacm<-NULL
jacm<-mult_jac(asian_m,500)
jacm<-cbind(jacm,mult_jac(black_m,500))
jacm<-cbind(jacm,mult_jac(indigenous_m,500))
jacm<-cbind(jacm,mult_jac(latinx_m,500))
jacm<-cbind(jacm,mult_jac(neast_m,500))
jacm<-cbind(jacm,mult_jac(south_m,500))
jacm<-cbind(jacm,mult_jac(white_m,500))

colnames(jacm)<-c("asian m","black m", "indig m","latinx m", "neast m","south m", "white m")
summary(jacm)
anovajacm<-build_anova(data.frame(jacm))
summary(anovajacm)


### Similarity part 3: the sampling jubilee ### 
# Choose 20 characters and take a 100 word sample from each one to create our word vector 
# Then randomly sample 50 jac. from each one and take the mean
# repeat 1000 times ad nauseum 

output<-NULL
for (i in 1:1000){
  output<-rbind(output,build_many_samples())
}

