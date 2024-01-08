transform<-function(Contacts,X,Y,x,y,Chemin,obs){
#transforamtion .csv issu de tadarida ver .xlsx pour analyse des sons
#version 1 du 202310331
#Vincent Parmain


Contacts$ID<-seq(1,nrow(Contacts))

#Creer et ajouter les colonnes de verif des sequences + autres colonnes pour calculs ci-apres
nblig<-nrow(Contacts)
verif<-setNames(data.frame(matrix(ncol = 10, nrow = nblig)), 
                c("Lien", "Verif", "Comment", "GrpeTax", "Taxon1", "Taxon2", "Taxon3", "Taxon4","X_RGF93","Y_RGF93"))
Contacts<-cbind(Contacts,verif)
Contacts$Lien<-as.character(Contacts$Lien)
Contacts$Verif<-as.character(Contacts$Verif)
Contacts$Comment<-as.character(Contacts$Comment)
Contacts$GrpeTax<-as.character(Contacts$GrpeTax)
Contacts$Taxon1<-as.character(Contacts$Taxon1)
Contacts$Taxon2<-as.character(Contacts$Taxon2)
Contacts$Taxon3<-as.character(Contacts$Taxon3)
Contacts$Taxon4<-as.character(Contacts$Taxon4)
Contacts$X_RGF93<-as.numeric(Contacts$X_RGF93)
Contacts$Y_RGF93<-as.numeric(Contacts$Y_RGF93)

rm(verif)

#Creation colonne Nb_contact 
concatname<-table(Contacts$nom.du.fichier) #cree une table d'occurrence avec une ligne par nom de fichier et son nb d'occurrence
concatname<-as.data.frame(concatname) #convertit la table en data.frame
colnames(concatname) <- c("nom.du.fichier","Nb_contact") #change le nom des colonnes
Contacts<-merge(Contacts,concatname,by="nom.du.fichier")  #fait la jointure avec donn?es Contacts
rm(concatname) #nettoie

#Remplissage colonne Contact sans la boucle (bcp plus rapide)
Contacts1<-subset(Contacts, Nb_contact =="1",ID) #cree une sous-table avec seulement les lignes avec 1 contact, et seulement la colonne ID
Contacts1$Contact<-"Premier"

if(any((Contacts$Nb_contact=="2")==1)){
  Contacts2<-subset(Contacts, Nb_contact =="2",ID) #Idem avec seulement les lignes avec 2 contacts
  Contacts2$Contact<-c("Premier", "Deuxieme")  
}

if(any((Contacts$Nb_contact=="3")==1)){
  Contacts3<-subset(Contacts, Nb_contact =="3",ID)
  Contacts3$Contact<-c("Premier", "Deuxieme", "Troisieme")
}

if(any((Contacts$Nb_contact=="4")==1)){
  Contacts4<-subset(Contacts, Nb_contact =="4",ID)
  Contacts4$Contact<-c("Premier", "Deuxieme", "Troisieme","Quatrieme")
}
if(any((Contacts$Nb_contact=="5")==1)){
  Contacts5<-subset(Contacts, Nb_contact =="5",ID)
  Contacts5$Contact<-c("Premier", "Deuxieme", "Troisieme","Quatrieme","Cinquième")
}
if(any((Contacts$Nb_contact=="6")==1)){
  Contacts6<-subset(Contacts, Nb_contact =="6",ID)
  Contacts6$Contact<-c("Premier", "Deuxieme", "Troisieme","Quatrieme","Cinquième", "Sixième")
}
#Agrege toutes ces nouvelles tables en testant leur existence!
Contacts7<-Contacts1
if(exists("Contacts2")){
  Contacts7<-rbind(Contacts7,Contacts2)
}
if(exists("Contacts3")){
  Contacts7<-rbind(Contacts7,Contacts3)
}
if(exists("Contacts4")){
  Contacts7<-rbind(Contacts7,Contacts4)
}
if(exists("Contacts5")){
  Contacts7<-rbind(Contacts7,Contacts5)
}
if(exists("Contacts6")){
  Contacts7<-rbind(Contacts7,Contacts6)
}
Contacts<-merge(Contacts,Contacts7,by="ID",all.x=TRUE)

rm(Contacts1)
if(exists("Contacts2")){
  rm(Contacts2)
}
if(exists("Contacts3")){
  rm(Contacts3)
}
if(exists("Contacts4")){
  rm(Contacts4)
}
if(exists("Contacts5")){
  rm(Contacts5)
}
if(exists("Contacts6")){
  rm(Contacts6)
}
rm(Contacts7)

#Ajouter colonne Duree
Contacts$Duree<-Contacts$temps_fin - Contacts$temps_debut   #cree la colone avec formule en fin de tableau

#reecrire date et heure lisible
Contacts$DAT_HEUR<-str_sub(Contacts$nom.du.fichier,-19,-5)#-15-1 si pas kaleidoscope
#Contacts$DAT_HEUR<-str_extract(Contacts$nom.du.fichier,"_.{8}_.{6}")
Contacts$DAT_HEUR<-chartr(Contacts$DAT_HEUR, old="_", new=" ")
Contacts$DAT_HEUR<-str_c(str_sub(Contacts$DAT_HEUR, 1,4),"/",str_sub(Contacts$DAT_HEUR, 5,6),"/",str_sub(Contacts$DAT_HEUR,7,8)," "
                         ,str_sub(Contacts$DAT_HEUR,10,11), ":",str_sub(Contacts$DAT_HEUR, 12,13),":"
                         ,str_sub(Contacts$DAT_HEUR, 14,15))

Contacts$Date<-str_sub(Contacts$DAT_HEUR,1,10)
Contacts$Heure<-str_sub(Contacts$DAT_HEUR,12,19)
Contacts$Date<-strptime(Contacts$Date,"%Y/%m/%d") #conversion colonne en format date lisible par R

#Idem pour generer Date_Nuit, mais sans boucle, bcp plus rapide
Datnuit<-subset(Contacts,Contacts$Heure > "12:00",c(ID,Date))
Datnuit$Date_Nuit<-as.Date(Datnuit$Date)
Datnuit2<-subset(Contacts,Contacts$Heure <= "12:00",c(ID,Date))
Datnuit2$Date_Nuit<-as.Date(Datnuit2$Date) - 1

Datnuit3<-rbind(Datnuit,Datnuit2)
Datnuit3<-subset(Datnuit3, select = -Date )
Contacts<-merge(Contacts,Datnuit3,by="ID",all.x=TRUE)
rm(Datnuit,Datnuit2,Datnuit3)


#suite
Contacts$Date<-as.character(Contacts$Date) #repasser la colonne en format caract?re, pour ?viter mise en forme auto par R ? cause des manips sur T/H
Contacts$Heure<-as.character(Contacts$Heure) #repasser la colonne en format caract?re, pour ?viter mise en forme auto par R ? cause des manips sur T/H
Contacts$Date_Nuit<-as.character(Contacts$Date_Nuit) #repasser la colonne en format caract?re, pour ?viter mise en forme auto par R ? cause des manips sur T/H
Contacts$Date<-gsub("-","/",Contacts$Date) #remplacer le caract?re "-" par "/"
Contacts$Date_Nuit<-gsub("-","/",Contacts$Date_Nuit) #remplacer le caract?re "-" par "/"

Contacts$DAT_HEUR<-as.POSIXct(Contacts$DAT_HEUR, format= "%Y/%m/%d %H:%M:%S", tz="UTC")#definir comme date heure pour int?gration des donn?es HOBO

#Ajouter Lien hypertexte de la colonne Lien

Contacts$Lien<-paste("=LIEN_HYPERTEXTE(", '"',
                     gsub("/","\\\\",Chemin),
                     '\\',
                     gsub("/","-",Contacts$Date_Nuit),  ###modif arcachon!!!
                     '\\',
                     Contacts$nom.du.fichier,'.wav")'
                     ,sep="")

for (i in 1:nrow(Contacts)){
  Contacts$tri[i]<-sample(x=10000000, size=1,replace=FALSE)/10000000
}

############################################################################



######################################################################

Contacts$Duree<-Contacts$temps_fin - Contacts$temps_debut   #R?tablir colonne dur?e si besoin
Contacts<-subset(Contacts, select = -DAT_HEUR ) #Supprimer la colonne DAT_HEUR qui ne sert que pour importer donn?es T/H
Contacts<-subset(Contacts, select = -ID ) #Idem colonne ID
Contacts<-Contacts[order(Contacts$nom.du.fichier), ] #remettre le tableau dans l'ordre (tri sur colonne ID)
Contacts$Verif<-"NON"#remplir colonne verif
Contacts$X_RGF93<-as.numeric(X)#remplir coordonnees
Contacts$Y_RGF93<-as.numeric(Y)
Contacts$X_WGS84<-as.numeric(x)
Contacts$Y_WGS84<-as.numeric(y)

Contacts$validateur_ONF<-obs

Contacts$Comment<-""
Contacts$GrpeTax<-""
Contacts$Taxon1<-""
Contacts$Taxon2<-""
Contacts$Taxon3<-""
Contacts$observateur_taxon <- ""
Contacts$observateur_probabilite <- ""
Contacts$validateur_taxon <- ""
Contacts$validateur_probabilite<- ""





  Contacts<-Contacts[,c(
                        "nom.du.fichier",
                        "Lien",
                        "Contact",
                        "Nb_contact",
                        "temps_debut",
                        "temps_fin",
                        "Duree",
                        "frequence_mediane",
                        "tadarida_taxon",
                        "tadarida_probabilite",
                        "tadarida_taxon_autre",
                        "Verif",
                        "Comment",
                        "GrpeTax",
                        "Taxon1",
                        "Taxon2",
                        "Taxon3",
                        "validateur_ONF",
                        "observateur_taxon",
                        "observateur_probabilite",
                        "validateur_taxon",
                        "validateur_probabilite",
                        "Date_Nuit",
                        "Heure",
                        "X_RGF93",
                        "Y_RGF93",
                        "X_WGS84",
                        "Y_WGS84",
                        "Date"
                        )]


#rm(list=(ls()[ls()!="Contacts"]))

return(Contacts)

}

##################################################################################

#avec sp
#rgf2wgs<-function(X,Y){
#  loc<-matrix(c(X, Y), nrow=1) #coordonn?es en RGF93
#  loc<-suppressWarnings(SpatialPoints(loc, proj4string=CRS("+init=epsg:2154")))#en SpatialPoints
#  #loc<-SpatialPoints(loc, proj4string=CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "))
#  loc<-spTransform(loc,CRSobj=CRS("+init=epsg:4326")) #reprojeter en WGS84 pour calcul
#  locT<-loc@coords
#  return(locT)
#}

#wgs2rgf<-function(X,Y){
#  loc<-matrix(c(X, Y), nrow=1) #coordonn?es en RGF93
#  loc<-SpatialPoints(loc, proj4string=CRS("+init=epsg:4326"))#en SpatialPoints
#  loc<-suppressWarnings(spTransform(loc,CRSobj=CRS("+init=epsg:2154"))) #reprojeter en WGS84 pour calcul
#  locT<-loc@coords
#  return(locT)
#}

#avec sf
rgf2wgs<-function(X,Y){
  loc<-st_sfc(st_point(c(X,Y)))%>% st_set_crs(2154)%>% st_transform(4326)
  return(loc[[1]])
}

wgs2rgf<-function(X,Y){
  loc<-st_sfc(st_point(c(X,Y)))%>% st_set_crs(4326)%>% st_transform(2154)
  return(loc[[1]])
}
###############################################################################
# fonctionne dans un script mais pas en shiny, pourquoi? => code complet dans server.R
formatExcel <- function(){
  dimA <- paste0("A2:A", nrow(data))
  dims <- paste0("J2:J", nrow(data))
  dims1 <- paste0("G2:G", nrow(data))


  wb <- wb_workbook()
  wb$add_dxfs_style(name = "Style0", font_color = wb_color(hex = "black"), bg_fill = wb_color(hex = "red"))
  wb$add_dxfs_style(name = "Style1", font_color = wb_color(hex = "black"), bg_fill = wb_color(hex = "orange"))
  wb$add_dxfs_style(name = "Style2", font_color = wb_color(hex = "black"), bg_fill = wb_color(hex = "yellow"))
  wb$add_dxfs_style(name = "Style3", font_color = wb_color(hex = "black"), bg_fill = wb_color(hex = "yellowgreen"))
  wb$add_dxfs_style(name = "Style4", font_color = wb_color(hex = "black"), bg_fill = wb_color(hex = "green"))
  wb$add_dxfs_style(name = "Style5", font_color = wb_color(hex = "black"), bg_fill = wb_color(hex = "mediumseagreen"))

  wb$add_worksheet("Tadarida")
  wb$add_data("Tadarida", data)
  wb$add_conditional_formatting(
    "Tadarida",
    dims = dims,
    rule = ">0",
    style = "Style0"
  )
  wb$add_conditional_formatting(
    "Tadarida",
    dims = dims,
    rule =">=0.25",
    style = "Style1"
  )
  wb$add_conditional_formatting(
    "Tadarida",
    dims = dims,
    rule = ">=0.5",
    style = "Style2"
  )
  wb$add_conditional_formatting(
    "Tadarida",
    dims = dims,
    rule = ">=0.75",
    style = "Style3"
  )
  wb$add_conditional_formatting(
    "Tadarida",
    dims = dims,
    rule = ">=0.9",
    style = "Style4"
  )
  wb$add_conditional_formatting(
    "Tadarida",
    dims = dims,
    rule = ">=0.99",
    style = "Style5"
  )
  wb$add_conditional_formatting(
    "Tadarida",
    dims = dims1,
    type = "dataBar",
    rule = c(0,5),
    style = c("green","green"),
    params = list(
      showValue = TRUE,
      gradient = TRUE)
  )

  wb$add_cell_style(dims = dimA, horizontal="right")
  wb$set_col_widths(cols=c(1,4:6,18,19:23),widths=c(19,rep(5,3),15,rep(10,5)))


  wb$save(filename)
 
}

############################################################################################
plotG<- function(df1=data1$table, x1=proba,x2=size){
  #vecteur espèces
  sp<-c("Barbar","Eptnil","Eptser","Hypsav","Minsch","Myoalc","Myobec","Myobly","Myobra","Myocap","Myocry","Myodas","Myodau",
        "Myoema","Myoesc","Myomyo","Myomys","Myonat","Myopun","Nyclas","Nyclei","Nycnoc","Pipkuh","Pipnat","Pippip","Pippyg",
        "Pleaur","Pleaus","Plemac","Rhieur","Rhifer","Rhihip","Rhimeh","Tadten","Vesmur")
  #vecteurs groupes
  autres<- c("Barbar","Hypsav","Pleaur","Pleaus","Plemac")
  PipMi<-c("Pippip","Pipkuh","Pipnat","Pippyg","Minsch")
  Myosp<-c("Myoalc","Myobec","Myobly","Myobra","Myocap","Myocry","Myodas","Myodau",
           "Myoema","Myoesc","Myomyo","Myomys","Myonat","Myopun")
  Serot<- c("Eptnil","Eptser","Nyclas","Nyclei","Nycnoc","Tadten","Vesmur")
  Rhi<- c("Rhieur","Rhifer","Rhihip","Rhimeh")
  
  #filtrer chiros
  df1<-df1[df1$tadarida_taxon %in% sp,]
  
  #affecter groupes aux données
  df1$group <-"autres"
  df1$group[df1$tadarida_taxon %in% PipMi]<-"PipMi"
  df1$group[df1$tadarida_taxon %in% Myosp]<-"Myosp"
  df1$group[df1$tadarida_taxon %in% Serot]<-"Serot"
  df1$group[df1$tadarida_taxon %in% Rhi]<-"Rhi"
  df1$group <- as.factor(df1$group)
  
  #df1$interact <- paste(df1$group,":", df1$tadarida_taxon)
  
  #df1 <- df1%>%group_by(group)%>%mutate(cool = as.integer(factor(tadarida_taxon)))
  
  df1$DAT_HEUR<-str_sub(df1$nom.du.fichier,-19,-5)
  df1$DAT_HEUR<-chartr(df1$DAT_HEUR, old="_", new=" ")
  df1$DAT_HEUR<-str_c(str_sub(df1$DAT_HEUR, 1,4),"/",str_sub(df1$DAT_HEUR, 5,6),"/",str_sub(df1$DAT_HEUR,7,8)," "
                      ,str_sub(df1$DAT_HEUR,10,11), ":",str_sub(df1$DAT_HEUR, 12,13),":"
                      ,str_sub(df1$DAT_HEUR, 14,15))
  df1$DAT_HEUR <- as.POSIXct(df1$DAT_HEUR)
  
  df1$Date<-str_sub(df1$DAT_HEUR,1,10)
  df1$Heure<-str_sub(df1$DAT_HEUR,12,19)
  #generer Date_Nuit
  df1$nuit <-as.Date(df1$Date)
  df1$nuit[df1$Heure<"12:00"] <-as.Date(df1$Date[df1$Heure<"12:00"]) - 1
  
  df1$label<-paste(df1$group,":",df1$tadarida_taxon)
  df<-df1[df1$tadarida_probabilite>x1,]
  
  legend <- df %>% distinct(group, tadarida_taxon)%>%
                  mutate(shape= case_when(group=="autres" ~ 15,
                                          group=="PipMi" ~ 17,
                                          group=="Myosp" ~ 11,
                                          group=="Serot" ~ 19,
                                          group=="Rhi" ~ 9),
                         
                         colour= case_when(tadarida_taxon=="Barbar" ~"firebrick3",
                                           tadarida_taxon=="Pippip" ~"firebrick3",
                                           tadarida_taxon=="Myoalc" ~"firebrick3",
                                           tadarida_taxon== "Eptnil" ~"firebrick3",
                                           tadarida_taxon=="Rhieur" ~"firebrick3",
                                           
                                           tadarida_taxon=="Hypsav" ~"darkgreen",
                                           tadarida_taxon=="Pipkuh" ~"darkgreen",
                                           tadarida_taxon== "Myobec" ~"darkgreen",
                                           tadarida_taxon=="Eptser" ~"darkgreen",
                                           tadarida_taxon=="Rhifer" ~"darkgreen",
                                           
                                           tadarida_taxon=="Pleaur" ~"blue",
                                           tadarida_taxon=="Pleaus" ~"blue",
                                           tadarida_taxon=="Plemac" ~"blue",
                                           tadarida_taxon=="Pipnat" ~"blue",
                                           tadarida_taxon=="Myobly" ~"blue",
                                           tadarida_taxon=="Myomyo" ~"blue",
                                           tadarida_taxon=="Nyclas" ~"blue",
                                           tadarida_taxon=="Rhihip" ~"blue",
                                           
                                           tadarida_taxon=="Pippyg" ~"orange",
                                           tadarida_taxon=="Myobra" ~"orange",
                                           tadarida_taxon=="Nyclei" ~"orange",
                                           tadarida_taxon=="Rhimel" ~"orange",
                                           
                                           tadarida_taxon=="Minsch" ~"black",
                                           tadarida_taxon=="Myocap" ~"black",
                                           tadarida_taxon=="Nycnoc" ~"black",
                                           
                                           tadarida_taxon=="Myocry" ~"darksalmon",
                                           tadarida_taxon=="Myonat" ~"darksalmon",
                                           tadarida_taxon=="Myoesc" ~"darksalmon",
                                           tadarida_taxon=="Tadten" ~"darksalmon",
                                           
                                           tadarida_taxon=="Myodau" ~"gold2",
                                           tadarida_taxon=="Vesmur" ~"gold2",
                                           
                                           tadarida_taxon=="Myoema" ~"grey47",
                                           
                                           tadarida_taxon=="Myomys" ~"purple"
                                           ),
                         label=paste(group,":",tadarida_taxon))



legend <- legend[,-c(1:2)]
nshape<-legend%>%group_by(shape)%>%table()%>%as.data.frame()
nshape<-nshape[nshape$Freq!=0,]


vshape <- as.numeric(as.character(nshape[,1]))
cshape<- as.character(nshape[,2])



ggplot()+
  geom_point(data=df,aes(x=DAT_HEUR,y=frequence_mediane,shape=label,color=label), size=x2,)+
  scale_color_manual(values=cshape)+
  scale_shape_manual(values=vshape)+
  scale_x_datetime(date_breaks  ="hour", date_labels="%H" )+
  facet_wrap(~nuit,scales="free_x")+
  xlab("Heure")+
  #ylim(0,120)
  scale_y_continuous(limits = c(0, 120),breaks = seq(0, 120, 10))+
  theme_gray()+
  theme(legend.text=element_text(size=17), legend.key.size=unit(1,'cm'))
 
  
}  
