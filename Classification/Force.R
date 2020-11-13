#Force the outputs for Amazon: For every Name we count how often he has to occur in the Test set, so we can get a total occurancy of 30 per author.

names <-c("McKee","Auken","Harp","Robert","Janson","Sherwin","Nigam","Comdet","Morrison","Brown",
 "Lovitt","Vision","Brody","Peterson","Lee","Mark", "Dent", "Wilson","Calvinnme","Walters",
 "Taylor","Mitchell","Shea", "Vernon","Goonan","Power","Bukowsky","Agresti", "Ashbacher","Kolln",
 "Neal", "Cutey","Koenig","Messick", "Hayes","Lawyeraau","Chachra", "Mahlers2nd","Grove","Engineer",   
 "Cholette","CFH","Chandler","Chell","Johnson", "Merritt", "Corn", "Blankenship","Davisson","Riley")

numbers <- c(11,12,12,12,12,12,12,12,13,13,13,13,13,13,13,13,13,13,14,14,
14,14,14,14,15,15,15,15,15,16,16,16,16,16,16,16,16,17,17,17,
17,18,18,18,18,19,19,20,20,20)


#Starting with our best file from RandomTree. 
best_file <- "//Users//johannesdorsch//Desktop//RTB_amazon_result.csv"
Best <- read.csv(best_file)
classes <- Best$Class

#counting how often in fact a the authors occur
now_numbers<-numeric(50)
for (i in 1:50){
  now_numbers[i] <- sum(classes==names[i])
}


#Getting some compare files

#SVC
SVC <- "//Users//johannesdorsch//Desktop//linearSVC_amazon_result.csv"
svc <- read.csv(SVC)
#LOG
LOG <- "//Users//johannesdorsch//Desktop//LOG_amazon_result.csv"
log <- read.csv(LOG)
#Second best Random Tree
RT <- "//Users//johannesdorsch//Desktop//RT_amazon_result.csv"
rt <- read.csv(RT)

c1<-svc$Class

c2<-log$Class

c3<-rt$Class

#We do our procedure more than once
best_classes <- classes
#Taking now the in total (possible) second best classes
for (i in 1:50){
  if (c2[i]==c3[i]){
    c1[i]<-c2[i]
  }
}

#We aren't changing anything with the already right number of authors
less_names<-c()
less_numbers<-c()
much_names<-c()
much_numbers<-c()

for (i in 1:50){
  if (now_numbers[i]<numbers[i]){
    less_names<-c(less_names,names[i])
    less_numbers<-c(less_numbers,numbers[i]-now_numbers[i])
  }
  
  if (now_numbers[i]>numbers[i]){
    much_names<-c(much_names,names[i])
    much_numbers<-c(much_numbers,now_numbers[i]-numbers[i])
  }
}

if (length(much_names) != 0){
for (i in 1:length(much_names)){
for (k in 1:length(c1))
{if (classes[k] == much_names[i]){
  if (c1[k] %in% less_names){
    if(1<=much_numbers[i]){
      little_index<-(less_names==c1[k])
      classes[k]<-c1[k]
      much_numbers[i]<-much_numbers[i]-1
      less_numbers[little_index]<-less_numbers[little_index]-1
    }
  }
}}
}
}




df <- data.frame(ID = c(750:1499),
                 Class = classes)
write.csv(df,"AMAZON.csv", row.names = FALSE)
