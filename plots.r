library(ggplot2)
library(data.table)
library(ggsci)
library(ftplottools)
library(here)

pillar2 = fread(here("data", "pillar2.csv"))
cog =  fread(here("data", "cog.csv"))
ons =  fread(here("data", "ons.csv"))
samples =  fread(here("data", "samples.csv"))
ct =  fread(here("data", "ct_data.csv"))

## Figure 1A
g = ggplot(pillar2[date >= as.Date("2021-01-31") & Region_name == "London"])
g = g + geom_line(data=ons[area == "LONDON"], aes(x=date,y=pos_me * population,colour="S+ Infections (ONS)"))
g = g + geom_ribbon(data=ons[area == "LONDON"], aes(x=date,y=pos_me*population, ymin = pos_li*population, ymax = pos_ui*population), alpha = 0.1,fill="blue")
g = g + geom_line(aes(y=spos,x=date,colour="S+ Cases (Pillar 2)"))
g = g + geom_line(data=cog[Region_name == "London"],aes(y=pos,x=date,colour="Non-B.1.1.7 Sequences (COG-UK)"))
g = g + geom_ribbon(data=samples[date >= as.Date("2021-01-31") & Region_name == "London"],aes(x=date,y=spos, ymin = spos_li, ymax = spos_ui), alpha = 0.1,fill="red") 
g = g + scale_y_log10("Number of people", breaks=c(10,20,50,100,200,500,1000,2000,5000,10000)) 
g = g + xlab("")
g = g + scale_x_date(date_labels = "%-d %b", date_breaks = "2 week") 
g = g + scale_color_lancet(limits=c("S+ Infections (ONS)","S+ Cases (Pillar 2)","Non-B.1.1.7 Sequences (COG-UK)"))
g = g +  scale_fill_lancet() + ft_theme()
g = g + theme(legend.title = element_blank()) 
g

## Figure 1B
g = ggplot(pillar2[date >= as.Date("2021-01-31")])
g = g + geom_line(data=ons[date >= as.Date("2021-01-31")], aes(x=date,y=pos_me * population,colour="S+ Infections (ONS)"))
g = g + geom_ribbon(data=ons[date >= as.Date("2021-01-31")], aes(x=date,y=pos_me*population, ymin = pos_li*population, 
                                                                 ymax = pos_ui*population), alpha = 0.1,fill="blue") 
g = g + geom_line(aes(y=spos,x=date,colour="S+ Cases (Pillar 2)"))
g = g + geom_line(data=cog,aes(y=pos,x=date,colour="Non-B.1.1.7 Sequences (COG-UK)"))
g = g + geom_ribbon(data=samples[date >= as.Date("2021-01-31")],aes(x=date,y=spos, ymin = spos_li, ymax = spos_ui), alpha = 0.1,
                    fill="red") 
g = g + scale_y_log10("Number of people") #, breaks=c(10,20,50,100,200,500,1000,2000,5000,10000)) 
g = g + xlab("")
g = g + scale_color_lancet(limits=c("S+ Infections (ONS)","S+ Cases (Pillar 2)","Non-B.1.1.7 Sequences (COG-UK)")) +  scale_fill_lancet() + ft_theme()
g = g + theme(legend.title = element_blank()) 
g = g + facet_wrap(~ Region_name) #, scales="free")
g = g + scale_x_date(date_labels = "%b", date_breaks = "1 month") 
g

## Appendix Figure A
g = ggplot(pillar2[date >= as.Date("2021-01-31") & Region_name == "London"])
g = g + geom_line(aes(y=frac,x=date,colour="Pillar 2"))
g = g + geom_ribbon(data=samples[date >= as.Date("2021-01-31") & Region_name == "London"],aes(x=date,y=frac, ymin = 
                                                                                                lower_ci, ymax = upper_ci), alpha = 0.1,fill="red") 
g = g + scale_y_continuous("Fraction S+\n",labels = scales::label_percent(accuracy = 1L),expand=c(0,0)) + expand_limits(y=0)
g = g + xlab("")
g = g + geom_line(data=ons[area == "LONDON"],aes(x=date,y=pos_me / (pos_me + neg_me),colour="ONS"))
g = g + geom_line(data=cog[Region_name == "London"],aes(x=date,y=frac,colour="COG-UK"))
g = g + geom_ribbon(data=cog[Region_name == "London"],aes(x=date,y=frac, ymin = lower_ci, ymax = upper_ci), alpha = 0.25,fill="green") 
g = g + scale_color_lancet(limits=c("ONS","Pillar 2","COG-UK"))
g = g + scale_fill_lancet() + ft_theme()
g = g + theme(legend.title = element_blank()) 
g = g + scale_x_date(date_labels = "%-d %b", date_breaks = "2 week")
g = g + geom_ribbon(data=ons[area == "LONDON"],aes(x=date,y=pos_me / (pos_me + neg_me), ymin = fraction.lo, ymax = fraction.hi), alpha = 0.1,fill="blue")
g

## Appendix Figure B
g = ggplot(pillar2[date >= as.Date("2021-01-31")])
g = g + geom_line(aes(y=frac,x=date,colour="Pillar 2"))
g = g + geom_ribbon(data=samples[date >= as.Date("2021-01-31") ],aes(x=date,y=frac, ymin = lower_ci, ymax = upper_ci), alpha = 0.2,fill="red") 
g = g + scale_y_continuous("Fraction S+\n",labels = scales::label_percent(accuracy = 1L),expand=c(0,0)) + expand_limits(y=0)
g = g + xlab("")
g = g + geom_line(data=ons[date >= as.Date("2021-01-31")],aes(x=date,y=pos_me / (pos_me + neg_me),colour="ONS"))
g = g + geom_line(data=cog,aes(x=date,y=frac,colour="COG-UK"))
g = g + geom_ribbon(data=cog,aes(x=date,y=frac, ymin = lower_ci, ymax = upper_ci), alpha = 0.2,fill="green") 
g = g + scale_color_lancet(limits=c("ONS","Pillar 2","COG-UK"))
g = g +scale_fill_lancet() + ft_theme()
g = g + theme(legend.title = element_blank()) 
g = g + geom_ribbon(data=ons,aes(x=date,y=pos_me / (pos_me + neg_me), ymin = fraction.lo, ymax = fraction.hi), alpha = 0.1,fill="blue")
g = g + scale_x_date(date_labels = "%b", date_breaks = "1 month") 
g = g + facet_wrap(~ Region_name) #, scales="free")
g

## Figure 2 and Appendix Figure 3
ct <- ct[,gene := factor(gene, levels = c("ORF1ab  (Ct<=30)","N  (Ct<=30)","MS2 control" ))]
for(i in unique(ct$phec_name)){
    p <- ggplot(ct[ct$phec_name==i,],aes(x=Date,y=ct_mean,colour=sneg_g)) +
      # labs(colour="SGTF") +
      geom_ribbon(aes(ymin=ct_mean-1.96*se,ymax=ct_mean+1.96*se, linetype=NA,fill=sneg_g),alpha=0.3,show.legend = FALSE) +
      geom_line(aes(y=ct_mean),size=1) +
      facet_wrap(~gene) +
      theme_classic() +
      scale_x_date(date_labels = "%d %b", date_breaks = "3 week") +
      ylab("Mean Ct value") +
      theme(panel.grid.major = element_line(colour="lightgrey",linetype="dashed", size = 0.1),
            strip.background = element_blank(),strip.text.x = element_text(size = 12)) +
      scale_fill_lancet() + 
      scale_color_lancet() +
      ft_theme() +
      theme(axis.text.x = element_text( angle = 45,hjust = 1)) +
      theme(legend.title=element_blank()) +
      ggtitle(i) 
    # if (i=="London"){
    #   p <- p + scale_x_date(date_labels = "%b %d", date_breaks = "3 week")
    # }
    ggsave(paste0(here(),"/figures/",i,"_ct.pdf"),p, width=8, height = 5)
}
