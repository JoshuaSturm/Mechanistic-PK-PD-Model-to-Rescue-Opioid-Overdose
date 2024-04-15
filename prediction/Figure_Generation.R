
#---- Script to Combine original/mechanistic predictions to produce Figure 3 and 4
#---- John Mann


times<-c(seq(0,300,1),seq(305,3600,5))


# Read in necessary data
All_real<-read.csv("data/real_data.csv") #Time course outputs from mechanistic simulations
All_orig<-read.csv("data/original_pred_data_2.csv") # Time course outputs from standard deep learning model
All_mech<-read.csv("data/mechanistic_pred_data_a.csv") # Time course outputs from mechanistically-inspired deep learning model



mech_loss<-read.table("../training/save_j1/lossmech.txt") #Error values mech deep learning

orig_loss<-read.table("../training/save_j1/lossorig.txt") # Error values standard deep learning

times_1<-seq(1,88,1)/88*48 # Mech model completes 88 runs in 48hrs
times_2<-seq(1,192,1)/192*48 # Standard model completes 192 runs in 48 hrs  Assuming similar run time for each epoch
times_11<-seq(1,88,1)


#----- Generate Publication Figures: 
mech_g<-data.frame(cbind(times_1,mech_loss[1:length(times_1),1]))
orig_g<-data.frame(cbind(times_2,orig_loss[1:length(times_2),1]))
names(mech_g)<-names(orig_g)<-c("times","Training_error")
mech_g$model<-"Mechanistic"
orig_g$model<-"Traditional"
library(ggplot2)
p1<-ggplot(data=mech_g,aes(x=times,y=Training_error,color=model))+geom_point()+
		geom_point(data=orig_g,aes(x=times,y=Training_error,color=model))+
		theme_bw()+
		ggtitle("A  Equal Time")+
		xlab("Time (hrs)")+
		ylab("")+
		scale_x_continuous(breaks=seq(0,48,12),limits=c(0,50))+
		scale_y_continuous(breaks=seq(0,20,10),limits=c(0,25))+
		labs(color = "Model Type")+
		theme(legend.position=c(.75,.75))

ggsave("Results/Manuscript_training_time.pdf",p1)
times_11<-seq(1,88,1)
mech_g<-data.frame(cbind(times_11,mech_loss[1:length(times_11),1]))
orig_g<-data.frame(cbind(times_11,orig_loss[1:length(times_11),1]))
names(mech_g)<-names(orig_g)<-c("times","Training_error")
mech_g$model<-"Mechanistic"
orig_g$model<-"Traditional"
library(ggplot2)
p2<-ggplot(data=mech_g,aes(x=times,y=Training_error,color=model))+geom_point()+
		geom_point(data=orig_g,aes(x=times,y=Training_error,color=model))+
		theme_bw()+
		ggtitle("B   Equal Epochs")+
		xlab("Epochs")+
		ylab("")+
		scale_x_continuous(breaks=seq(0,90,30),limits=c(0,95))+
		scale_y_continuous(breaks=seq(0,20,10),limits=c(0,25))+
		#labs(color = "Model Type")+
		theme(legend.position="none")

ggsave("Results/Manuscript_training_epochs.pdf",p2)

library(gridExtra)
library(grid)
GA<-grid.arrange(p1,p2, ncol=1,nrow=2, 
		top="Figure 3 Mechanistic Vs Traditional AI Error Comparison ",
		left="Training Error")


ggsave("Results/Combine_grid.pdf",GA,height=6,width=10)


