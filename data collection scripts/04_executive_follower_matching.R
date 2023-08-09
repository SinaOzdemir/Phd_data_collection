
packs = c("rtweet","tidyverse")

lapply(packs,library,character.only =T)

#create dir

replierDIR = paste0(getwd(),"/daily_repliers/")

tweetSaveDIR = paste0(getwd(),"/replier_profiles/replier_tweets/")

followerSaveDIR =paste0(getwd(),"/replier_profiles/replier_followers/")

friendsSaveDIR = paste0(getwd(),"/replier_profiles/replier_friends/")

eu_tweets<- readRDS(file = file.choose())

eu_twt_ids <- eu_tweets$status_id

repliers<- readRDS(file = file.choose())

repliers<- unique(repliers$user_id)

rtwt_files<- list.files(path = "C:/Users/sinaf/OneDrive - NTNU/Work/Trondheim -/PHD/phd data/Phd_data_collection/Scripts/extended_list_collection/Retweeter_ids",pattern = "*.RDS",full.names = T)

rtwt_files<-rtwt_files[1:230]

rtwt_prof<- lapply(rtwt_files, FUN = function(x){
  print(x)
  a<- readRDS(file = x)
  b<- a[which(names(a)%in%eu_twt_ids)]
  c<- unlist(b)
})



rtwt_prof_list<-unlist(rtwt_prof)

rtwt_prof_list_un<-unique(rtwt_prof_list)

saveRDS(rtwt_prof_list_un,file = paste0(getwd(),"retweeter_user_ids.RDS"))
