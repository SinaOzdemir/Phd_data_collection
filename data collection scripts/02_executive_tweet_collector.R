#list tester

#response tweet streaming(?)
#setting up the necessaries
#working directory-----
start_time = Sys.time()
collect.date = as.Date(format(Sys.Date()-1, "%Y-%m-%d"))

#necessary packages for scraping-----
packs = c("rtweet","tidyverse","readxl")
lapply(packs, library,character.only = T)
#creating twitter app token----

daily.collector= create_token(app = "",
                              consumer_key = "",
                              consumer_secret = "",
                              access_token = "",
                              access_secret = "",set_renv = F)


#reading in the actor data(seeding)----
eu.act = readRDS(file = "C:/Users/sinaf/OneDrive - NTNU/Work/Trondheim -/PHD/phd data/Phd_data_collection/meta/eu_actor_twitterhandles_updated.RDS")



to_act = paste0("to:",eu.act)
#get actor tweets for the day-----
#use search tweet or get_timeline?


act.twt.m = vector(mode = "list")

for (h in 1:length(eu.act)) {

   rl = rate_limits(token = daily.collector)

   rl = filter(rl, query %in% c("application/rate_limit_status","statuses/user_timeline"))

   if (isTRUE(rl[1,3] <= 2 | rl[2,3] <= 1)) {
     print(x = paste0(h," is out of limit and waiting for 15 minutes at ", Sys.time()))

     Sys.sleep(time = (15*60))

   }

  # if(isTRUE(h%%56 == 0)){
  #   print(x = paste0(h," is out of limit and waiting for 15 minutes at ", Sys.time()))  
  #      
  #      Sys.sleep(time = (15*60))
  #     
  # }
   
 try(expr = {act.twt.m[[h]]= get_timeline(user = eu.act[h],
                                           token = daily.collector,
                                           check = F)},silent = T)

   
  
  
}

catch


act.twt.m = do_call_rbind(act.twt.m)
act.twt.m$date = as.Date(act.twt.m$created_at, format = "%Y-%m-%d")
act.twt.m = filter(act.twt.m, date %in% collect.date)




a = ifelse(eu.act%in%as.character(act.twt.m$screen_name),T,F)
b = as.data.frame(cbind(eu.act,a))
c = filter(b, a ==F)
d = as.character(c$eu.act)

remainder = vector("list",length = length(d))


for (g in 1:length(d)) {
  
  rl = rate_limits(token = daily.collector)
  
  rl = filter(rl, query %in% c("application/rate_limit_status","statuses/user_timeline"))
  
  if (isTRUE(rl[1,3] <= 2 | rl[2,3] <= 1)) {
    print(x = paste0(g," is out of limit and waiting for 15 minutes at ", Sys.time()))
    
    Sys.sleep(time = (15*60))
    
  }
  
  
  
  try(expr = {remainder[[g]]= get_timeline(user = d[g],
                                           token = daily.collector,
                                           check = F,parse = T)},silent = T)
  
  
  
  
}

remainder = do_call_rbind(remainder)
remainder$date = as.Date(remainder$created_at, format = "%Y-%m-%d")
remainder.a = filter(remainder, date %in% collect.date)




#write tweets from todays actors

act.twt = rbind(act.twt.m,remainder.a)

write_as_csv(x = act.twt,
             file_name = paste0(getwd(),"/Actor_tweets/","actor_twt_",collect.date))


saveRDS(act.twt,file = paste0(getwd(),"/Actor_tweets/","actor_twt_",collect.date,".RDS"))

saveRDS(act.twt,file = paste0(getwd(),"/Actor_tweets/","actor_twt_ascii",collect.date,".RDS"),ascii = T)



#scrape the replies-----

daily.replies = vector(mode = "list")

for (i in 1:length(to_act)) {

   rl = rate_limits(token = daily.collector)

   rl = filter(rl, query %in% c("application/rate_limit_status","search/tweets"))

   if (isTRUE(rl[1,3] <= 2 | rl[2,3] <= 1)) {
     print(x = paste0(i," is out of limit and waiting for 15 minutes at ", Sys.time()))

     Sys.sleep(time = (15*60))

   }

  # if(isTRUE(i%%56 == 0)){
  #   print(x = paste0(h," is out of limit and waiting for 15 minutes at ", Sys.time()))  
  #   
  #   Sys.sleep(time = (15*60))
  #   
  # }
   
  try(expr = {daily.replies[[i]] = search_tweets(q = to_act[i], n = 5000000,
                                     retryonratelimit = F,token = daily.collector,
                                     include_rts = F,verbose = F)},silent = T)
  
  
}

#store the replies----
daily.replies = do_call_rbind(daily.replies)

daily.replies$date = as.Date(daily.replies$created_at, format = "%Y-%m-%d")

daily.replies.tday = filter(daily.replies, date %in% collect.date)


replies.missing = ifelse(eu.act%in%as.character(daily.replies.tday$reply_to_screen_name),T,F)
replies.missing.a = as.data.frame(cbind(eu.act,replies.missing))
replies.missing.b = filter(replies.missing.a, replies.missing ==F)
replies.missing.c = as.character(replies.missing.b$eu.act)
replies.missing.d = paste0("to:",replies.missing.c)
remainder.replies = vector("list",length = length(replies.missing.d))



for (k in 1:length(replies.missing.d)) {
  
  rl = rate_limits(token = daily.collector)
  
  rl = filter(rl, query %in% c("application/rate_limit_status","search/tweets"))
  
  if (isTRUE(rl[1,3] <= 2 | rl[2,3] <= 1)) {
    print(x = paste0(k," is out of limit and waiting for 15 minutes at ", Sys.time()))
    
    Sys.sleep(time = (15*60))
    
  }
  
  # if(isTRUE(i%%56 == 0)){
  #   print(x = paste0(h," is out of limit and waiting for 15 minutes at ", Sys.time()))  
  #   
  #   Sys.sleep(time = (15*60))
  #   
  # }
  
  try(expr = {remainder.replies[[k]] = search_tweets(q = replies.missing.d[k], n = 5000000,
                                                 retryonratelimit = F,token = daily.collector,
                                                 include_rts = F,verbose = F)},silent = T)
  
  
}

remainder.replies.b = do_call_rbind(remainder.replies)
remainder.replies.b$date = as.Date(remainder.replies.b$created_at, format = "%Y-%m-%d")
remainder.replies.c = filter(remainder.replies.b, date %in% collect.date)

daily.replies.b = rbind(daily.replies.tday,remainder.replies.c)
#If the script is going to be run daily - if it will be run every 9 days or so, this part is unnecessary


write_as_csv(daily.replies.b,
             file_name = paste0(getwd(),"/replies_to_actors","/replies_",collect.date))

saveRDS(daily.replies.b,file = paste0(getwd(),"/replies_to_actors/","replies_raw_",collect.date,".RDS"))
saveRDS(daily.replies.b,file = paste0(getwd(),"/replies_to_actors/","replies_raw_ascii",collect.date,".RDS"),ascii = T)
#lookup repliers----


daily.repliers = unique(daily.replies.b$user_id)
saveRDS(daily.repliers,file = paste0(getwd(),"/daily_repliers/","repliers_raw_",collect.date,".RDS"))

#retweeter ids
daily.e.retweeted = filter(act.twt.m, retweet_count >2)

retweeter.id = vector(mode = "list",length = nrow(daily.e.retweeted))
retweeter.count = round(((daily.e.retweeted$retweet_count*20)/100))



for (j in 1:nrow(daily.e.retweeted)) {
 
   print(paste0("scraping the retweeters of ", j,"th tweet", "out of ",nrow(daily.e.retweeted), " at ",Sys.time()))
   rl = rate_limit(token = daily.collector)
   rl = filter(rl, query %in% c("application/rate_limit_status","statuses/retweeters/ids"))
   
   if (isTRUE(rl[1,3] <= 2 | rl[2,3] <= 1)) {
     print(x = paste0(j," is out of limit and waiting for 15 minutes at ", Sys.time()))  
     
     Sys.sleep(time = (15*60))
     
   }
   
  
  # if(isTRUE(j%%50==0)){
  #   print(x = paste0(j," is out of limit and waiting for 15 minutes at ", Sys.time()))  
  #      
  #      Sys.sleep(time = (15*60))
  #      
  # }
  # 
    
  
  
  try(expr = {retweeter.id[[j]] = get_retweeters(status_id = daily.e.retweeted$status_id[j],
                                     n = retweeter.count[j],
                                     parse = T,token = daily.collector)},silent = T)
  
}

status.ids = daily.e.retweeted$status_id
names(retweeter.id) = status.ids


save(retweeter.id,file = paste0(getwd(),"/Retweeter_ids/","retweeter_id_",collect.date,".RDS"))
save(retweeter.id,file = paste0(getwd(),"/Retweeter_ids/","retweeter_id_ascii",collect.date,".RDS"),ascii = T)


finish_time = Sys.time()

print(paste0("started at ", start_time, "finished at ", finish_time))
