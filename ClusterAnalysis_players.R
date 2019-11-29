library(dplyr)
library(raster)
library(cluster)
library(stringr)
library(factoextra)
library(corrgram)
library(gridExtra)
library(clusterSim)

## Seeing data

players <- read.csv2("C:/Users/User/Desktop/R/cluster_analysis/cluster/players_50.csv", row.names=1)
tail(players,7)
str(players)
summary(players)

ggplot(players, aes(x = Goals, y = Passes, color = Clear, size = Dribbles))+ geom_point()

ggplot(players, aes(x = Tackels, y = Assists, color= Fouls))+ geom_point()

## Variable selection, outliers

sapply(players, cv)


corrgram(players, lower.panel=panel.cor, upper.panel= NULL)

players_stand<- as.data.frame(scale(players))
summary(players_stand)


for (i in 1:50){
  if  (rowSums(abs(players_stand[i,]<3) ) != dim(players_stand)[2]){
    print(players_stand[i,])
  }
}

players_stand<- players_stand[rowSums(abs(players_stand<3))==dim(players_stand)[2],]

## K-means


k2<- kmeans(players_stand, centers =2, nstart = 100)
rownames(players_stand)<- word(rownames(players_stand), -1)

fviz_cluster(k2, data = players_stand)

k3<- kmeans(players_stand, centers =3, nstart = 100)
k4<- kmeans(players_stand, centers =4, nstart = 100)
k5<- kmeans(players_stand, centers =5, nstart = 100)
k9<- kmeans(players_stand, centers =9, nstart = 100)

plot3<- fviz_cluster(k3, data = players_stand)+ ggtitle("k = 3")
plot4<- fviz_cluster(k4, data = players_stand)+ ggtitle("k = 4")
plot5<- fviz_cluster(k5, data = players_stand)+ ggtitle("k = 5")

plot3
plot4
plot5

## Elbow method


total_withinss<- c()
for (i in 1:20){
  km<- kmeans(players_stand, centers = i, nstart = 100 )
  total_withinss[i]<- km$tot.withinss
}

names(total_withinss)<-c("1-klaster","2-klastry","3-klastry","4-klastry","5-klastrów","6-klastrów"
                         ,"7-klastrów","8-klastrów","9-klastrów","10-klastrów",
                         "11-klastrów","12-klastrów","13-klastrów","14-klastrów","15-klastrów",
                         "16-klastrów","16-klastrów","18-klastrów","19-klastrów","20-klastrów")

total_withinss

fviz_nbclust(players_stand, kmeans, method = "wss",k.max = 20, nstart=100) +
  labs(subtitle = "Elbow method")

## Silhouette


sylwetka <- c()
for(i in 2:20){
  podzial <- kmeans(players_stand, centers = i, nstart = 100)
  sylwetka[i] = index.S(dist(players_stand),podzial$cluster)
  
}

names(sylwetka)<-c("1-klaster","2-klastry","3-klastry","4-klastry","5-klastrów","6-klastrów"
                   ,"7-klastrów","8-klastrów","9-klastrów","10-klastrów",
                   "11-klastrów","12-klastrów","13-klastrów","14-klastrów","15-klastrów",
                   "16-klastrów","16-klastrów","18-klastrów","19-klastrów","20-klastrów")


sylwetka
fviz_nbclust(players_stand, kmeans, method = "silhouette", k.max = 20, nstart = 100)+
  labs(subtitle = "Silhouette method")


fviz_cluster(k9, data = players_stand)+ ggtitle("k = 9")


## Ward

distance <- dist(players_stand, method = "euclidean")
ward <- hclust(distance, method = "ward.D2")

plot(ward, hang = -1, cex =0.7, main = "Ward method")



sylwetka <- c()
for(i in 2:20){
  podzial <- cutree(ward, k = i)
  sylwetka[i] = index.S(distance, podzial)
}
names(sylwetka)<-c("1-klaster","2-klastry","3-klastry","4-klastry","5-klastrów","6-klastrów"
                   ,"7-klastrów","8-klastrów","9-klastrów","10-klastrów",
                   "11-klastrów","12-klastrów","13-klastrów","14-klastrów","15-klastrów",
                   "16-klastrów","16-klastrów","18-klastrów","19-klastrów","20-klastrów")
sylwetka
plot(sylwetka, type = "b")



plot(ward, cex = 0.6)
rect.hclust(ward, k = 9, border = 2:10)


## Kmeans vs WARD


group9<- cutree(ward,9)
fviz_cluster(list(data = players_stand, cluster = group9),geom = "point")+ ggtitle("Ward method")

fviz_cluster(k9,geom = "point", data = players_stand)+ggtitle("Kmeans method")




positions<- c("Napastnik","Skrzydłowy","Środkowy obrońca",
              "Środkowy obrońca2","Skrzydłowy2","Środkowy obrońca3",
              "Napastnik2","Środkowy obrońca4","Boczny obrońca",
              "Boczny obrońca2","Środkowy obrońca5","Napastnik3",
              "Środkowy pomocnik","Napastnik4","Środkowy pomocnik2",
              "Boczny obrońca3","Skrzydłowy3","Środkowy pomocnik3","Boczny obrońca4",
              "Środkowy obrońca6","Środkowy pomocnik4","Boczny obrońca5",
              "Środkowy pomocnik5", "Środkowy pomocnik6","Środkowy pomocnik7",
              "Skrzydłowy4","Środkowy pomocnik8","Środkowy pomocnik9","Napastnik5",
              "Skrzydłowy5","Boczny obrońca6","Środkowy pomocnik10","Napastnik6",
              "Skrzydłowy6","Środkowy pomocnik11","Napastnik7","Środkowy obrońca7",
              "Napastnik8", "Skrzydłowy7", "Napastnik9","Boczny obrońca7",
              "Boczny obrońca8", "Napastnik10","Boczny obrońca9","Skrzydłowy8",
              "Napastnik11", "Środkowy obrońca8")

players_pos<- players_stand
rownames(players_pos)<- positions

fviz_cluster(k9, data = players_pos )



players_km<- players
players_km<- players[c(-4,-14,-31),]
players_km$group9<- k9$cluster

mean_groups<- group_by(players_km, group9) %>%
  summarise_all(mean)%>%
  arrange(desc(Goals))

sd_groups<- group_by(players_km, group9) %>%
  summarise_all(sd) 

mean_groups
sd_groups




