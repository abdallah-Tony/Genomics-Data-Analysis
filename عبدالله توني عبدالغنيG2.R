
anthro<-read.csv("G2_anthropometry.csv")
anthro
View(anthro)
str(anthro)
summary(anthro)

#Data cleaning

#1) Re-code the gender feature to Male and Female

anthro$Sex[anthro$gender=="F" ]="Female"
anthro$Sex[anthro$gender=="cm"]="Male"
anthro

#2) show all the rows that have NA

anthro[!complete.cases(anthro) , ]

#3)Replace each NA in foot_length according to the mean of
#foot_length to all males for males and foot_length to all females for females

m_mean<-mean(anthro[anthro$Sex=="Male" , 'foot_length' ], na.rm=T)
anthro[is.na(anthro$foot_length) & anthro$Sex=="Male"  , 'foot_length']=m_mean
anthro

f_mean<-mean(anthro[anthro$Sex=="Female" , 'foot_length' ], na.rm=T)
anthro[is.na(anthro$foot_length) & anthro$Sex=="Female"  , 'foot_length']=f_mean

#4)Re_code age variable 
anthro$ageRange[anthro$age <= 5]="0 _ 5"
anthro$ageRange[anthro$age > 5 & anthro$age <= 10]="6 _ 10"
anthro$ageRange[anthro$age >10 ]="11 _ .."
anthro

#5)remove the text(cm)in height feature to convert it to numeric to use in the analysis

anthro$height<-gsub(" cm" ,"" , anthro$height)
anthro$height<-as.numeric(anthro$height)
anthro


#6)Re_code the height feature using if else

x<-mean(anthro$height)

anthro$heightCat<-as.factor(ifelse(anthro$height< x & anthro$age >10,"Abnormal kid" 
                                   ,"Normal kid"))
anthro
#7)Re_code of code

anthro$heightCat2[anthro$heightCat=="Normal kid"]='0'
anthro$heightCat2[anthro$heightCat=="Abnormal kid"]='1'
anthro$heightCat2<-as.factor(anthro$heightCat2)
anthro

#8)Display The ratio of normal and abnormal child 

a<-mean(anthro$heightCat2==0)
b<-mean(anthro$heightCat2==1)
a
b

#9)Subset only males who have foot_length greater than  200

sub1<-anthro[anthro$Sex=="Male" & anthro$foot_length >200 , ]
sub1

#10)Subset abnormal childs

sub2<-anthro[anthro$heightCat2== 1 , ]
#or
sub2<-anthro[anthro$heightCat=="Abnormal kid" ,]
sub2

#11)Subset childs who have foot_length greater than the median and have height 
#greater than or equal to 135  for specific features(age , sex)

sub3<-anthro[anthro$foot_length > median(anthro$foot_length) &
               anthro$height >= 135 ,c(1,5) ]
sub3

#12)sort the data set ascending according to 2 variables

sorted<-anthro[order(anthro$age ,anthro$height) , ]
anthro

#13)Get only the first 30 rows
h<-head(anthro ,30)
h

#14)Get only the last 50 rows
t<-tail(anthro ,50)
t
#______________________________________________ now the data is ready to visualization

library(ggplot2)

#15)display the effect of the height on foot_length (co_relation) using scatter plot,name the figure

fig1<-ggplot(anthro , aes(x=foot_length  , y= height))
fig1 + geom_point() + ggtitle("The co_relation between the Height and Foot_length")

#16)display the effect of height on foot_length colored by the groups of age range using scatterplot
fig2<-ggplot(anthro , aes(foot_length , height))
fig2 + geom_point(aes(color=ageRange)) +stat_smooth(se=FALSE)  


#17)Show the distribution of footlength using histogram,name the figure and rename the x,y
fig3<-ggplot(anthro , aes(foot_length))
fig3 + geom_histogram(binwidth = 8)
fig3 + geom_histogram(fill = "orange")+ ggtitle("Child's foot length distribution")+labs(x="Foot Length" , y="Frequency")

#18)Show the distribution of Height using histogram ,name the figure
fig4<-ggplot(anthro , aes(height))
fig4 + geom_histogram(binwidth = 8)
fig4 + geom_histogram(fill = "red")+ ggtitle("Child's Height distribution") 

#19)summarize the heightcat2 0,1 to Sex and ageRange groups using Bar chart

fig5<-ggplot(anthro , aes(x=heightCat2  ,fill= Sex))
fig5 +geom_bar()+labs(y=" Heightcat count" ,title="Height category rate")
fig5 +geom_bar() +theme_light()+facet_wrap(~ageRange)






