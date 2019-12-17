source("core.R")

# This loads the map information for Canada
ca <- readOGR("province.shp")
ca_map <- fortify(ca, region="NAME")
ca_map[ca_map[,6]=="Newfoundland and Labrado",6]<-"Newfoundland and Labrador"
centroids.df <- as.data.frame(coordinates(ca))
names(centroids.df) <- c("Longitude", "Latitude")  #more sensible column names
centroids.df<-centroids.df %>%
  mutate(Latitude=ifelse(Latitude>60,62,Latitude))

# The core map data that you merge other data into
mapdata<-data.frame(id=ca@data[,1],centroids.df) %>%
  mutate(id=ifelse(id=="Newfoundland and Labrado","Newfoundland and Labrador",as.character(id))) %>%
  mutate(Latitude=ifelse(id=="Prince Edward Island",47,Latitude)) %>%
  mutate(Latitude=ifelse(id=="New Brunswick",46,Latitude)) %>%
  mutate(Longitude=ifelse(id=="Nova Scotia",-61,Longitude)) %>%
  mutate(Latitude=ifelse(id=="Nova Scotia",43.5,Latitude)) %>%
  mutate(Longitude=ifelse(id=="Newfoundland and Labrador",-60.5,Longitude)) %>%
  mutate(Latitude=ifelse(id=="Newfoundland and Labrador",54,Latitude)) %>%
  filter(!(id=="Ontario" & Latitude<50))

# Merge in Data from the Spreadsheet
supp_data<-read.csv("supp_data.csv")
mapdata<-mapdata %>%
  left_join(supp_data,by="id")

############################
### Plot provincial data ###
############################

# Share of population over 65
popdata<-getTABLE("17100005")
subdata<-popdata %>%
  filter(Age.group %in% c("All ages","65 years and over"),Sex=="Both sexes",
         Ref_Date==max(Ref_Date)) %>%
  group_by(GEO) %>%
  mutate(share=Value[2]/Value[1]) %>%
  filter(Age.group=="All ages",GEO!="Canada") %>%
  rename(id=GEO)
plotdata<-mapdata %>%
  left_join(subdata,by="id")%>%
  #filter(!(id %in% c("Northwest Territories","Yukon","Nunavut"))) %>%
  filter(!is.na(share)) 
ggplot(plotdata,aes(map_id=id,fill=share)) + 
  geom_map(map=ca_map,color="white",show.legend = F) +
  expand_limits(x=c(-119,-59),y=c(44,60))+
  coord_map("lambert",0,85)+
  scale_fill_gradient2(low = "#deebf7",mid="#deebf7",high = "#3182bd",
                      midpoint=0.11) +
  #scale_fill_manual(name="",values=c("#008FD5","#FF2700"))+
  mythememap+
  geom_text_repel(aes(label = paste(round(100*share,1),"%",sep=""), x = Longitude, y = Latitude),
                  point.padding = unit(0,"cm"), box.padding = unit(0,"cm"),fontface="bold",size=3) + 
  labs(x="",y="",title="Share of Population Aged 65+ (July 2018)",
       subtitle="Source: Own calculations from Statistics Canada data table 17-10-0005.
Graph by @trevortombe.")
ggsave("plot.png",width=7,height=4,dpi=300)

##################################
# Just Provinces, No Territories #
##################################
plotdata<-mapdata %>%
  left_join(subdata,by="id")%>%
  filter(!(id %in% c("Northwest Territories","Yukon","Nunavut"))) %>%
  filter(!is.na(share)) 
ggplot(plotdata,aes(map_id=id,fill=transfers2020)) + 
  geom_map(map=ca_map,color="white",show.legend = F) +
  expand_limits(x=c(-119,-59),y=c(44,60))+
  coord_map("lambert",0,85)+
  #scale_fill_gradient2(low = "#c3d2e7",mid="#deebf7",high = "#396ab1",
  #                     midpoint=0.11) +
  scale_fill_gradient(low = "#c3d2e7",high = "#396ab1")+
  #scale_fill_manual(name="",values=c("#008FD5","#FF2700"))+
  mythememap+
  geom_text_repel(aes(label = dollar(transfers2020),
                      x = Longitude, y = Latitude),
                  point.padding = unit(0,"cm"), box.padding = unit(0,"cm"),fontface="bold",size=3) + 
  labs(x="",y="",title="Federal Transfers to Provinces (Per Capita, 2020/21)",
       subtitle="Source: Finance Canada, https://www.fin.gc.ca/fedprov/mtp-eng.asp.
Graph by @trevortombe.")
ggsave("plot.png",width=7,height=4,dpi=300)
