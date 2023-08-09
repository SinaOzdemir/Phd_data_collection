packs = c("tidyverse","rtweet","xlsx")

lapply(packs, library, character.only =T)

idvars = read.xlsx(file = file.choose(),sheetIndex = 1,encoding = "UTF-8")


idvars_twt = idvars[!is.na(idvars$twitter_link),]

idvars_twt$Screen_name = gsub("https://twitter.com/|http://twitter.com/|http://www.twitter.com/|https://mobile.twitter.com/|?lang=en|",
                              "",idvars_twt$twitter_link)

idvars_twt$Screen_name = gsub("?|/","",idvars_twt$Screen_name)

#actor roles:
# supranational legislative - institution,
# supranational legislative - individual,
# supranational executive - individual,
# supranational executive - institution,
# supranational advisory - institution,


unique(idvars_twt$Actor_type)

idvars_twt$Actor_type %<>% car::recode(recodes = "'President of the commission' = 'President'")
idvars_twt$Actor_type_inst = paste(idvars_twt$Actor_type, idvars_twt$Actor_institution, sep = " - ")
instName = idvars_twt[idvars_twt$Actor_type_inst == "Institution - Not applicable",]

idvars_twt$Actor_role = ifelse(idvars_twt$Actor_type_inst %in% c("Agency - Not applicable",
                                                                          "Directorate general - European Commission"),
                                        "Supranational Executive - Institution",
                                        ifelse(idvars_twt$Actor_type_inst%in%c("Deputy director general - European Commission","Director general - European Commission",
                                                                               "Vice president - European Commission", "President - European Commission",
                                                                               "Executive vice-president - European Commission",
                                                                               "Comissioner - European Commission",
                                                                               "Head of State - the European Council",
                                                                               "High representative and vice president - European Commission",
                                                                                "President - the European Council",
                                                                               "President - the Eurogroup"),
                                               "Supranational Executive - Individual",
                                               ifelse(idvars_twt$Actor_type_inst%in%c("Political party - European Parliament",
                                                                                      "Party Group - European Parliament"),
                                                      "Supranational Legislative - Institution",
                                                      ifelse(idvars_twt$Actor_type_inst%in%c("MEP - Euroepan parliament","MEP - European Parliament"),
                                                             "Supranational Legislative - Individual",
                                                             ifelse(idvars_twt$Actor_name %in%c("the European Council",
                                                                                                "European External Action Service (EEAS)",
                                                                                                "European Central bank","European Commission"),"Supranational Executive - Institution",
                                                                    ifelse(idvars_twt$Actor_name%in%c("European Committee of the Regions (CoR)","European Parliament",
                                                                                                      "European Economic and Social Committee (EESC)","the Council of the European Union"),
                                                                           "Supranational Legislative - Institution",
                                                                           ifelse(idvars_twt$Actor_name%in%c("EU Court of Justice","European Ombudsman","The court of Auditors","European Data Protection Supervisor (EDPS)"),
                                                                                  "Supranational Advisory - Institution",NA)))))))



#twitter handle information

handle_collector = create_token(app = "",
                                consumer_key = "",
                                consumer_secret = "",
                                access_token = "",
                                access_secret = "",
                                set_renv = F)

twitter_info = lookup_users(users = idvars_twt$Screen_name,parse = T,token = handle_collector)

twitter_info_trimmed = select(twitter_info, user_id,screen_name, name,location,description,url,protected,followers_count,friends_count,statuses_count,favourites_count,account_created_at,verified,profile_url,account_lang,profile_banner_url,profile_background_url,profile_image_url)

names(idvars_twt)[names(idvars_twt) == "Screen_name"] = "screen_name"

idvars_twt$screen_name_low = tolower(idvars_twt$screen_name)

twitter_info_trimmed$screen_name_low = tolower(twitter_info_trimmed$screen_name)

idvars_major = left_join(idvars_twt,twitter_info_trimmed, by = "screen_name_low")

idvars_major_na = idvars_major[!is.na(idvars_major$user_id),]

write.xlsx(x = idvars_major_na,file = "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim -/PHD/phd data/Phd_data_collection/meta/ID_variables_v2.xlsx")
