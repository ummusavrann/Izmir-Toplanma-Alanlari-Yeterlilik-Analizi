# Veri görselleþtirme proje ödevi.
# Konu: Ýzmir ilçeler bazýnda acil toplanma alanlarý yeterliliði araþtýrýlýp analiz edilmiþtir.
# Analizleri görselleþtirmek için kullanýlan grafikler:


# 1. ilçeler bazýnda verilerin koroplet haritasý çizdirilmiþtir.
#
# KOROPLET HARÝTASI
#

#Gerekli Paketler yüklenmiþtir ve kütüphaneden çaðýrýlmýþtýr.
library(ggplot2)
library(scales)
library(readr)
library(tigris)
library(dplyr)
library(leaflet)
library(sp)
library(ggmap)
library(maptools)
library(broom)
library(httr)
library(rgdal)
library(tidyverse)

# Ýlçelerin koordinatlarý çizdirilmek için R sp level 2 Yüklenmiþtir.
tur <- readRDS("C:/Users/ummu/Downloads/gadm36_TUR_2_sp.rds")

# Ýlçelerin koordinatlarý çizdirilmiþtir.
plot(tur)

#Fortify fonksiyonunu kullanarak her ilçenin enlem ve boylam deðerlerini alýnmýþtýr.
tur_for <- fortify(tur) 
head(tur@data)
head(tur_for)
View(tur_for)

#Ggplot ile temel harita çizdirilmiþtir.

ggplot(tur_for) + geom_polygon(aes(x = long, y = lat,group = group),color = "white",fill = "red") +
  theme_void() + coord_fixed()

# tur_for dosyasýndaki enlem ve boylam deðerleriyle tur@data dosyasýndaki ilçe isimlerini eþleþtirilmiþtir.
# Bunu tur_data bulunan ”id” numarasýyla ve tur@data dosyasýndaki sýra adlarýyla (row names) yapýlmýþtýr.
# tur_for ve tur@data dosyalarýndaki id numaralarýnda fark olduðu için  tur@data dosyasýndaki sýra adlarýný yeni bir sütuna alýp bu fark düzeltilmiþtir.

tur@data$id<-rownames(tur@data)
tur@data$id<-as.numeric(tur@data$id)
tur_for$id<-as.numeric(tur_for$id)
tur_for$id<-tur_for$id-1

#“left_join” kodunu kullanarak iki dosya birleþtirilmiþtir.
match <- left_join(tur_for, tur@data, by = "id")

#iþlem yapýlacak ili seçmek için ”HASC_2” sütunundan seçme yapýlmýþtýr.
unique(match$HASC_2)

# Ýzmir verilerini dataset'ten çekilmiþtir.
izmir_for<-filter(match, grepl("TR.IZ", HASC_2))
head(izmir_for$NAME_2)

#ilçe isimleri küçük harf olarak deðiþtirilmiþtir.
izmir_for<- izmir_for %>% mutate(NAME_2 = str_to_lower(NAME_2))

--------------------------------------------------------------------------------------------
#
#Ýzmir büyükþehir belediyesi açýk veri portalýndan  "bina sayýlarý mahalle bazlý" veri seti kullanýlmýþtýr.
#ilk olarak ilçelerde toplam bina sayýlarýný görmek için gerekli iþlemler uygulanmýþtýr.
#  
#bina sayýlarý veri seti yüklenmiþtir.  
  
View(bina_sayilari_mahalle_bazli)

# veri setinin ismi ilce_izmir olarak deðiþtirilmiþtir.

ilce_izmir<- bina_sayilari_mahalle_bazli

#veri setindeki kayýp veriler silinmiþtir.

ilce_izmir<- na.omit(ilce_izmir)

#veri setindeki gerekli sütünlar çekilmiþtir.

ilce_izmir<-ilce_izmir[c(1,3,4)]
View(ilce_izmir)

#mutate fonksiyonu ile yapý ve diðer yapý deðerleri toplanmýþtýr ve yapý nesnesine atanmýþtýr.

ilce_izmir<- mutate(ilce_izmir, Yapý= YAPI+DIGER_YAPI)

#kullanýlacak sütünlar çekilmiþtir.

ilce_izmir<-ilce_izmir[c(1,4)]

# group_by ve summarise fonksiyonlarý ile bir ilçede bulunan toplam bina sayýsý düzenlenmiþtir.

ilce_izmir<- group_by(ilce_izmir, ILCE)
summarise(ilce_izmir, sum(Yapý))
ilce_izmir<- summarise(ilce_izmir, sum(Yapý))

#
#koroplet harýtaýný çizdirirken yukarýda kullanýlacak hale getirilmiþ veri setinde kayýp verilerin oluþumunun önüne  geçmek için veri seti manuel olarak tekrardan 
#ele alýnmýþtýr.
#
# veri setini manuel olarak oluþturulmuþtur. 
#ilçe isimleri NAME_2 nesnesine atanmýþtýr.
NAME_2 <- c("aliaða", "balçova", "bayýndýr", "bayrakli", "bergama", "beydað", "bornova", "buca", "çeþme", "çiðli", "dikili", "foça",
            "gaziemir", "güzelbahçe", "karabaðlar","karaburun", "karþýyaka", "kemalpaþa", "kiraz", "kýnýk", "konak", "menderes", "menemen", "narlýdere", "ödemiþ", "seferihisar",
            "selçuk", "tire", "torbalý", "urla")
# Ýlçelere karþýlýk gelen toplam bina sayýlarý Yapý nesnesine atanmýþtýr.

Yapý<- c(19334, 6973, 25285, 33111, 34831, 6664, 62515, 56388, 38957, 21653, 39963, 
         21900, 15816, 11407, 53444, 14588, 22444, 31128, 24139, 10383, 26854, 38786, 27778, 7262, 46058, 31290,
         10447, 31802, 33790, 38336)
# data.frame ile ilce_izmir veri seti oluþtulmuþtur.
ilce_izmir <- data.frame(NAME_2, Yapý)
View(ilce_izmir)  

# left join fonksiyonu ile izmir_for ve ilce_izmir veri setlerini ilçe isimlerine göre birleþtirilmiþtir ve final_map nesnesine atanmýþtýr.
final_map<- left_join(izmir_for, ilce_izmir, by="NAME_2")
head(final_map)
View(final_map)
# ggplot ile grafiði oluþturuyoruz.

library(ggplot2)
ggplot(final_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Yapý),color = "black")+
  labs(x="",y="")+ theme_bw()+
  coord_fixed()

# lejant okunur hale getirilmek için gerekli deðerler ile deðiþtirilmiþtir ve ggplot ile koroplet haritasý çizdirmek için
# p2 nesnesine diðer gerekli iþlemler de atanarak koroplet haritasý oluþturulmuþtur. 

p2<-ggplot(final_map, aes(x=long, y=lat, group=group)) + 
  geom_polygon(aes(fill=Yapý),color = "grey")+
  labs(x="",y="")+ theme_bw()+
  theme(legend.key.size = unit(2, "cm"), legend.title = element_text(size = 18), legend.text = element_text(size = 14) )+
  coord_fixed()

p2<-p2+scale_fill_gradientn(colours=c("white","grey","green"), 
                            values=rescale(c(0,20000,70000)))
p2<-p2+theme(axis.line=element_blank(),
             axis.text.x=element_blank(),
             axis.text.y=element_blank(),
             axis.ticks=element_blank(),
             axis.title.x=element_blank(),
             axis.title.y=element_blank(),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.border = element_blank(),
             panel.background = element_blank())
p2<-p2+ theme(legend.position="left")
p2<-p2+labs(fill = "Bina Sayýlarý",title = "Ýzmir Bina Sayýlarý")
p2<-p2+ theme(plot.title = element_text(hjust = 0.5))
p2 

--------------------------------------------------------------------------------------

#
#Ýzmir büyükþehir belediyesi açýk veri portalýndan  "afet_ve_acil_durum_toplanma_alanlari" veri seti kullanýlmýþtýr.
#ilk olarak ilçelerde acil toplanma alanlarýnýn toplam m^2 bazýnda görmek için gerekli iþlemler uygulanmýþtýr.
#
#afet ve acil toplanma alanlarý veri seti yüklenmiþtir.
afet_ve_acil_durum_toplanma_alanlari <- read_delim("C:/Users/ummu/Downloads/afet_ve_acil_durum_toplanma_alanlari.csv", 
                                                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)


View(afet_ve_acil_durum_toplanma_alanlari)

#veri seti ismi adta olarak atanmýþtýr.

adta<- afet_ve_acil_durum_toplanma_alanlari

#Veri setinde gereksiz sütunlar çýkarýlmýþtýr.

adta<-adta[c(1,3,9)]
View(adta)

#mutate fonksiyonu ile enlem boylam çarpýmý yapýlmýþtýr.
library(tidyverse)
adta<- mutate(adta, EnBoy= ENLEM*BOYLAM)

#group_by ve summarise fonksiyonlarý ile 1 ilçede toplam kaç metre kare toplanma alanýný görmekþ için hesaplama yapýlýmýþtýr.

adta<- group_by(adta, ILCE)
summarise(adta, sum(EnBoy))
adta<- summarise(adta, sum(EnBoy))
  
#
#koroplet harýtaýný çizdirirken yukarýda kullanýlacak hale getirilmiþ veri setinde kayýp verilerin oluþumunun önüne  geçmek için veri seti manuel olarak tekrardan 
#ele alýnmýþtýr.
#
# veri setini manuel olarak oluþturulmuþtur. 
#ilçe isimleri NAME_2 nesnesine atanmýþtýr.

NAME_2<- c("aliaða", "balçova", "bayýndýr", "bayrakli", "bergama", "beydað", "bornova", "buca", "çeþme", "çiðli", "dikili", "foça",
       "gaziemir", "güzelbahçe", "karabaðlar","karaburun", "karþýyaka", "kemalpaþa", "kiraz", "kýnýk", "konak", "menderes", "menemen", "narlýdere", "ödemiþ", "seferihisar",
       "selçuk", "tire", "torbalý", "urla")

# Ýlçelere karþýlýk gelen toplam acil toplanma alanlarý m^2 olarak Toplanma_alaný nesnesine atanmýþtýr.

Toplanma_alaný <- c(40857.91, 15577.35, 41190.10, 35531.96, 136424.60, 37644.22, 54455.35, 61564.63,
                    67605.76, 53118.24, 38893.39, 12460.93, 39521.48, 64970.84, 95726.80, 17381.54, 
                    118856.47, 156352.32, 54553.16, 69179.55, 37532.66, 16593.54, 72112.08, 68408.32, 
                    108054.89, 14357.20, 32197.33, 104510.90, 25064.32, 32771.52)

# data.frame ile izmir_adta veri seti oluþtulmuþtur.

izmir_adta <- data.frame(NAME_2, Toplanma_alaný)
view(izmir_adta)

# left join fonksiyonu ile izmir_for ve izmir_adta veri setlerini ilçe isimlerine göre birleþtirilmiþtir ve final_map nesnesine atanmýþtýr.

final_map<- left_join(izmir_for, izmir_adta, by="NAME_2")
head(final_map)  
view(final_map)

# ggplot ile grafiði oluþturuyoruz.

library(ggplot2)
ggplot(final_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Toplanma_alaný),color = "grey")+
  labs(x="",y="")+ theme_bw()+
  coord_fixed()

# lejant okunur hale getirilmek için gerekli deðerler ile deðiþtirilmiþtir ve ggplot ile koroplet haritasý çizdirmek için
# p2 nesnesine diðer gerekli iþlemler de atanarak koroplet haritasý oluþturulmuþtur. 

p2<-ggplot(final_map, aes(x=long, y=lat, group=group)) + 
  geom_polygon(aes(fill=Toplanma_alaný),color = "grey")+
  labs(x="",y="")+ theme_bw()+
  theme(legend.key.size = unit(2, "cm"), legend.title = element_text(size = 18), legend.text = element_text(size = 14) )+
  coord_fixed()

p2<-p2+scale_fill_gradientn(colours=c("white","grey","green"), 
                            values=rescale(c(0,10000,60000,100000,140000)))
p2<-p2+theme(axis.line=element_blank(),
             axis.text.x=element_blank(),
             axis.text.y=element_blank(),
             axis.ticks=element_blank(),
             axis.title.x=element_blank(),
             axis.title.y=element_blank(),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.border = element_blank(),
             panel.background = element_blank())
p2<-p2+ theme(legend.position="left")
p2<-p2+labs(fill = "Alan",title = "Ýzmir Acil Toplanma Alanlarý")
p2<-p2+ theme(plot.title = element_text(hjust = 0.5))
p2  
-------------------------------------------------------------------------------------------------

#
# Proje ödevinde yön vermek için izmir halkýndan 30 ekim 2020 yýlýnda hissettikleri depremi hangi ilçe hissettiklerini ve  o ilçedeki
# acil toplanma alanlarý nerelerde olduklarýný bilip bilmediklerini analizi yapýlmýþtýr.
#
# 2. Toplam sonuçlarý waffle grafiði ile görselleþtirilmiþtir.
#WAFFLE GRAFÝÐÝ
  
#Gerekli Paketler yüklenmiþtir ve kütüphaneden çaðýrýlmýþtýr.

library(waffle)
library(magrittr)
library(hrbrthemes) 
library(ggplot2)

waffle(c('Evet=53%' = 53, 'Hayýr=47%' = 47), rows = 10, colors = c("#FD6F6F", "#93FB98"),
         title = 'Sonuçlar', legend_pos="bottom")
geom_waffle(color = "white", size = 0.33, n_rows= 4, flip = TRUE) +
  facet_wrap(~fct) +
  theme(legend.position = "bottom") +
  theme(legend.key.size = unit(5, "cm"), legend.title = element_text(size = 14), legend.text = element_text(size = 10) )+
  theme(strip.text.x = element_text(hjust = 0.5)) 
  
#
# Anket sonuçlarýnýn cevaplarý analizi sonucunda 4 ilçede daha çok cevap alýndýðýný belirlenmiþtir.
# Bu sonuca baðlý 4 ilçede bulunan halkýn yüzde kaç oranda evet hayýr cevaplarý aldýðý görselleþtirilmiþtir.
#
# 3. ilçelerden gelen sonuçlar çubuk grafiði ile görselleþtirilmiþtir.
# ÇUBUK GRAFÝÐÝ

# Ýlk olarak analizi yapýlmýþ deðerleri manuel olarak veri seti haline getirilmiþtir.
# Ýlçe isimleri, cevaplar, deðerler olarak nesnelere atamalar yapýlmýþtýr.
ilçeler<- c(rep("Bornova",2), rep("Buca",2),  rep("Konak",2), rep("Gaziemir",2))
cevaplar<- rep(c("Evet","Hayýr"),4)
deðerler<- c(53,47,50, 50, 58, 42,50, 50)

#data frame ile veri seti oluþturulmuþtur ve data11 nesnesine atanmýþtýr.

data11<- data.frame(ilçeler, cevaplar, deðerler)  
view(data11)

# Çubuk grafiði oluþturulmuþtur.

ggplot(data11, aes(fill=cevaplar, y=deðerler, x=ilçeler)) + 
  geom_bar(position="dodge",  stat="identity")+
  labs(x= "Ýlçeler", y="Yüzdelik Deðerler",
       title = "Anket Analizi Sonucu Ýlçe Bazlý Cevaplar ",
       fill= "Cevaplar")+
  lims(y=c(0,100))

#
# 4 ilçenin toplam bina sayýsý ve toplam acil toplanma alanlarýný daha rahat görebilmek için 
# dairesel paketleme grafiði ile 4 ilçe görselleþtirilmiþtir.
#
# 4. 4 ilçe dairesel paketleme grafiði ile görselleþtirilmiþtir.
# DAÝRESEL PAKETLEME GRAFÝÐÝ

# gerekli paketler yüklenmiþtir ve kütüphaneden çaðýrýlmýþtýr.

library(packcircles)
library(reshape2)
library(dplyr)
library(ggplot2)
library(readr)

# Bina sayýlarýnýn veri seti tekrardan yüklenmiþtir ve izmir1 nesnesine atanmýþtýr.

library(readr)
bina_sayilari_mahalle_bazli <- read_delim("C:/Users/ummu/Downloads/bina-sayilari-mahalle-bazli.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(bina_sayilari_mahalle_bazli)

izmir1<- bina_sayilari_mahalle_bazli

view(izmir1)

#veri setindeki kayýp veriler silinmiþtir.

izmir1<- na.omit(izmir1)

#veri setindeki gerekli sütünlar çekilmiþtir.

izmir1<-izmir1[c(1,3,4)]
View(izmir1)

#mutate fonksiyonu ile yapý ve diðer yapý deðerleri toplanmýþtýr ve yapý nesnesine atanmýþtýr.

izmir1<- mutate(izmir1, Yapý= YAPI+DIGER_YAPI)

#kullanýlacak sütünlar çekilmiþtir.

izmir1<-izmir1[c(1,4)]

# group_by ve summarise fonksiyonlarý ile bir ilçede bulunan toplam izmir1 sayýsý düzenlenmiþtir.

izmir1<- group_by(izmir1, ILCE)
summarise(izmir1, sum(Yapý))
izmir1<- summarise(izmir1, sum(Yapý))
names(izmir1)[2]<- "n"
izmir1<- izmir1 %>% filter(ILCE %in% c("KONAK", "GAZÝEMÝR", "BORNOVA", "BUCA"))

# izmir veri setindeki n nesnesine atanan verileri kullanarak circleProgressiveLayout komutu çalýþtýrarak konumlarýn x y koordinatlarýný ve 
# sonra yarýçapý verilmiþtir ve abc nesnesine atanmýþtýr.

abc<- circleProgressiveLayout(izmir1$n, sizetype = 'area')
head(abc)

# circleLayoutVertices fonksiyonu ile merkez x-y koordinatlarý ve boyutlarý için sütunlar içeren bir daire düzeni için veri çerçevesi verildiðinde bu 
# iþlev ggplot iþlevleriye kullanýlabilecek bir veri kümesi oluþturulmuþtur ve myPlotcord nesnesine atanmýþtýr.
myPlotcord<- circleLayoutVertices(abc)
head(myPlotcord)
View(myPlotcord)
# izmir veri seti ve abc nesnesi birleþtirilmiþtir ve mydata nesnesine atanmýþtýr.

mydata<- cbind(izmir1, abc)
View(mydata)
head(mydata)


# ggplot yardýmýyla dairesel paket graiði oluþturulmuþtur. 

ggplot()+
  geom_polygon(data= myPlotcord, aes(x, y, group= id, fill= as.factor(id)))+
  geom_text(data = mydata, aes(x, y, label= paste0(ILCE, '\n', n)))+
  theme_void()+
  theme(legend.position ="none")+#1. satýr.
  coord_equal()+
  labs(title = "Ýzmir Ýlçelerinde Toplam Bina Sayýlarý")

# 1. satýr için yorum: lejantlar alfabetik sýraya göre renklendirilmiþtir ama bunu görselde görmemize gerek olmadýðý için kaldýrýlmýþtýr.

#
# Acil toplanma alanlarý veri seti tekrar yüklenmiþtir ve izmir nesnesine atanmýþtýr.
#


afet_ve_acil_durum_toplanma_alanlari <- read_delim("C:/Users/ummu/Downloads/afet_ve_acil_durum_toplanma_alanlari.csv", 
                                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

View(afet_ve_acil_durum_toplanma_alanlari)

izmir<-afet_ve_acil_durum_toplanma_alanlari
View(izmir)

# Kullanýlacak sütünlar çekilmiþtir.

izmir<-izmir[c(1,3,9)]
View(izmir)

#mutate fonksiyonu ile enlem boylam çarpýmý yapýlmýþtýr.
library(tidyverse)
izmir<- mutate(izmir, EnBoy= ENLEM*BOYLAM)

#group_by ve summarise fonksiyonlarý ile 1 ilçede toplam kaç metre kare toplanma alanýný görmek için hesaplama yapýlýmýþtýr.

izmir<- group_by(izmir, ILCE)
summarise(izmir, sum(EnBoy))
izmir<- summarise(izmir, sum(EnBoy))


#filter ile gerekli ilçeler filtrelendi.
izmir<- izmir %>% filter(ILCE %in% c("KONAK", "GAZÝEMÝR", "BORNOVA", "BUCA") )
names(izmir)[2]<- "n"

# izmir veri setindeki n nesnesine atanan verileri kullanarak circleProgressiveLayout komutu çalýþtýrarak konumlarýn x y koordinatlarýný ve
# sonra yarýçapý verilmiþtir ve abc nesnesine atanmýþtýr.

abc<- circleProgressiveLayout(izmir$n, sizetype = 'area')
head(abc)

# circleLayoutVertices fonksiyonu ile merkez x-y koordinatlarý ve boyutlarý için sütunlar içeren bir daire düzeni için veri çerçevesi verildiðinde
# bu iþlev ggplot iþlevleriye kullanýlabilecek bir veri kümesi oluþturulmuþtur ve myPlotcord nesnesine atanmýþtýr.

myPlotcord<- circleLayoutVertices(abc)
head(myPlotcord)
View(myPlotcord)

# izmir veri seti ve abc nesnesi birleþtirilmiþtir ve mydata nesnesine atanmýþtýr.

mydata<- cbind(izmir, abc)
View(mydata)
head(mydata)


# ggplot yardýmýyla dairesel paket graiði oluþturulmuþtur. 

ggplot()+
  geom_polygon(data= myPlotcord, aes(x, y, group= id, fill= as.factor(id)))+
  geom_text(data = mydata, aes(x, y, label= paste0(ILCE, '\n', n)))+
  theme_void()+
  theme(legend.position ="none")+#1. satýr.
  coord_equal()+
  labs(title = "Ýzmir Ýlçelerinde Bulunan Acil Toplanma Alanlarý")

# 1. satýr için yorum: lejantlar alfabetik sýraya göre renklendirilmiþtir ama bunu görselde görmemize gerek olmadýðý için kaldýrýlmýþtýr.

#
# 4. Ýlçenin toplam bina sayýsý ile toplanma alanalrýný saçýlým grafiði üzerinde görselleþtirilmiþtir.
# SAÇILIM GRAFÝÐÝ
#

# Gerekli paketler yüklenmiþtir ve kütüphaneden çaðýrýlmýþtýr.
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggExtra)
library(ggrepel)

# veri setlerindeki isimlendirmeler düzenlenmiþtir.

names(izmir1)[2]<- "Toplam Bina Sayýsý"
names(izmir)[2]<- "Toplanma Alaný"

names(izmir1)
names(izmir)
view(izmir1)
view(izmir)

#merge fonksiyonu ile dairesel paketleme grafiðinde oluþturulan izmir1 ve izmir veri setleri  birleþtirilmiþtir ve bina_alan nesnesine atanmýþtýr.

bina_alan<- merge(izmir1, izmir, by='ILCE')
view(bina_alan)

# ggplot ile saçýlým grafiði oluþturulmuþtur.
 ggplot(bina_alan, aes(y = `Toplam Bina Sayýsý`, x = `Toplanma Alaný`, colour= factor(ILCE))) + 
  geom_point(size=3) + geom_label_repel(aes(label=ILCE), size=4)+
  theme(legend.position="none")+
  labs(title = "Ýzmir Ýlçelerinde Bulunan Bina Ve Toplanma Alaný")


