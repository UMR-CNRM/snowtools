#! /usr/bin/env python
# -*- coding: utf-8 -*-

import os
import sys
import re
import xml.dom.minidom


# M Lafaysse 04/08/2017
# Give metadata about massifs used for avalanche hazard forecasting
# Most routines are extracted from SCMtools in snowtools 1 (JM Willemet, M Lafaysse)


class infomassifs() :

    def __init__(self) :

        self.dico_units={'lat':"degrees_north",'LAT':"degrees_north",'lon':"degrees_east",'LON':"degrees_east",
                         'altitude':'m','ZS':'m','aspect':'degrees from north',
                         'slope':'degrees from horizontal','ZREF':'m','UREF':'m' ,'massif_number':'','station':''}        
        
        self.dicexpo={'1':0,'2':45,'3':90,'4':135,'5':180,'6':225,'7':270,'8':315,'0':-1}

        # ancien vers nouveau massif
        self.dicoldnewmassif={    1:1,
                    2:2,
                    4:3,
                    5:4,
                    3:5,
                    9:6,
                    6:7,
                    7:8,
                    25:9,
                    8:10,
                    10:11,
                    29:12,
                    11:13,
                    12:14,
                    13:15,
                    14:16,
                    16:17,
                    15:18,
                    30:19,
                    31:20,
                    17:21,
                    19:22,
                    18:23,
                    40:40,
                    41:41 }     

        # nouveau vers ancien massif
        self.dicnewoldmassif={}
        for oldmas in self.dicoldnewmassif.keys() :
            self.dicnewoldmassif[self.dicoldnewmassif[oldmas]]=oldmas

        self.dicoldnewmassifPyr={1:64,
                                 2:65,
                                 3:66,
                                 4:67,
                                 5:68,
                                 6:69,
                                 7:70,
                                 11:71,
                                 8:72,
                                 9:73,
                                 10:74,
                                 61:80,
                                 62:81,
                                 63:82,
                                 64:83,
                                 65:84,
                                 66:85,
                                 67:86,
                                 68:87,
                                 69:88,
                                 70:89,
                                 71:90,
                                 72:91}
        self.dicnewoldmassifPyr={}
        for oldmas in self.dicoldnewmassifPyr.keys() :
            self.dicnewoldmassifPyr[self.dicoldnewmassifPyr[oldmas]]=oldmas        

        # Exposition (rose de 360) vers numero d expositions
        self.dicExpo2Numexpo={}
        for numexpo in self.dicexpo.keys() :
            self.dicExpo2Numexpo[self.dicexpo[numexpo]]=numexpo

        # dictionnaire referencant une liste de massif par une chaine de caractere
        self.dicArea={
                "isere":[7,8,12,14,15], # les 5 massifs de l'Isère
                "oisans":[15],
                "lautaret":[12,13,15,16],
                "fissures":[2,6,10,15]
                }
        self.dicArea["alpes"]=range(1,24) # les 23 massifs des Alpes
        
        self.dicArea["pyrenees_F"]=range(64,75)
        self.dicArea["pyrenees_E"]=range(80,92)        
        self.dicArea["pyrenees"]=range(64,75)+range(80,92)
        self.dicArea["corse"]=[40,41]
        self.dicArea["all"]=range(1,24)+range(40,42)+range(64,75)+range(80,92)

        self.dicAltArea={}

        self.dicPostesArea={}

        self.dicPostesArea["alpes"]=[]
        self.dicPostesArea["pyrenees"]=[]
        self.dicPostesArea["corse"]=[]
        self.dicPostesArea["all"]=self.dicPostesArea["alpes"]+self.dicPostesArea["pyrenees"]+self.dicPostesArea["corse"]
        
        self.missval=-99

        # reading of the meta data file
        try :
            #Massiffile=open('/manto/willemet/METADATA.xml','r')
            
            if os.path.isfile('METADATA.xml') or os.path.islink('METADATA.xml'):
                metadata='METADATA.xml'
            elif 'WHERE' in os.environ.keys():
                if os.environ['WHERE']=="SOPRANO":
                    metadata=os.environ['HOME_RO']+'/METADATA.xml'
                else:
                    metadata=os.environ['SNOWTOOLS_CEN']+'/DATA/METADATA.xml'
            else:
                metadata=os.environ['SNOWTOOLS_CEN']+'/DATA/METADATA.xml'

            self.caracLoc=xml.dom.minidom.parse(metadata)
            
            # initialisation of lat and lon
            self.latCenter = {}
            self.lonCenter = {}
            for massif in self.caracLoc.documentElement.getElementsByTagName("Massif") :
                numMassif = int(massif.getElementsByTagName("number")[0].childNodes[0].data)
                self.latCenter[numMassif] = float(massif.getElementsByTagName("latCenter")[0].childNodes[0].data)
                self.lonCenter[numMassif] = float(massif.getElementsByTagName("lonCenter")[0].childNodes[0].data)         
            
        except :
            print "attention !!!!!! spatial information in xml file not found "


    def getListMassif_of_region(self,area) :

        try :
            listMassif=self.dicArea[area]
        except :
            listMassif=[]
            raise BaseException("""le domaine " + area + " n est pas connu de l application"
            Vérifiez le domaine ou mettre a jour SCMtools.""")


        return listMassif
    
    def getListPostes_of_region(self,area) :

        try :
            listPostes=self.dicPostesArea[area]
        except :
            listPostes=[]
            raise BaseException("""le domaine " + area + " n est pas connu de l application"
            Vérifiez le domaine ou mettre a jour SCMtools.""")

        return listPostes    

    def getStringOldMassif(self,numMassif) :

        if 'NUMMASSIF' in os.environ.keys():
            oldnumero = os.environ['NUMMASSIF']=="OLD"
        else:
            oldnumero=False
            
        if oldnumero:
            if numMassif in self.dicArea["pyrenees"]:
                return '%0*d' % (2,self.dicnewoldmassifPyr[numMassif])            
            else:
                return '%0*d' % (2,self.dicnewoldmassif[numMassif])
            
        else:
            return self.getStringNewMassif(numMassif)

    def getStringNewMassif(self,numMassif) :

        return '%0*d' % (2,numMassif)

    def infoposte(self,num_poste):
        
        for poste in self.caracLoc.documentElement.getElementsByTagName("Site") :           
            if str(poste.getElementsByTagName("number")[0].childNodes[0].data).strip() == num_poste :
                lati=float(poste.getElementsByTagName("lat")[0].childNodes[0].data)
                longi=float(poste.getElementsByTagName("lon")[0].childNodes[0].data)
                alti=float(poste.getElementsByTagName("altitude")[0].childNodes[0].data)
                break
        return lati,longi,alti
    
    def exposlopeposte(self,num_poste):    
        for poste in self.caracLoc.documentElement.getElementsByTagName("Site") :           
            if str(poste.getElementsByTagName("number")[0].childNodes[0].data).strip() == num_poste :
                aspect=float(poste.getElementsByTagName("aspect")[0].childNodes[0].data)
                slope=float(poste.getElementsByTagName("slope")[0].childNodes[0].data)
                break
        return aspect,slope   

    def massifposte(self,num_poste):
        for poste in self.caracLoc.documentElement.getElementsByTagName("Site") :
            if str(poste.getElementsByTagName("number")[0].childNodes[0].data).strip() == num_poste :
                try:
                    massif=int(poste.getElementsByTagName("massif")[0].childNodes[0].data)
                except:
                    massif=-1
                break
        return massif        

    def nameposte(self,num_poste):
        
        for poste in self.caracLoc.documentElement.getElementsByTagName("Site") :
            if str(poste.getElementsByTagName("number")[0].childNodes[0].data).strip() == num_poste :
                name=poste.getElementsByTagName("name")[0].childNodes[0].data
                break
        return name.encode("utf-8")

    def maskposte(self,num_poste):
        for poste in self.caracLoc.documentElement.getElementsByTagName("Site") :
            if str(poste.getElementsByTagName("number")[0].childNodes[0].data).strip() == num_poste :
                try:
                    listazi=list(eval(poste.getElementsByTagName("azimut")[0].childNodes[0].data))
                    listmask=list(eval(poste.getElementsByTagName("mask")[0].childNodes[0].data))
                    if not 360 in listazi:
                        listazi.append(360)
                        if 0 in listazi:
                            listmask.append(listazi[0])
                        else:
                            listmask.append(0)
                    if not 0 in listazi:
                        listazi.insert(0,0)                        
                        if 360 in listazi:
                            listmask.insert(0,listmask[-1]) 
                        else:                                                      
                            listmask.insert(0,0)                        
                except:
                    listazi=[0,360]
                    listmask=[0,0]
                break
            else:
                listazi=[0,360]
                listmask=[0,0]                
        return listazi,listmask



    def getListAlt(self,num_massif,tagname="Massif"):

        (altMin, altMax) = self.getAltMinMax(num_massif,tagname=tagname)

        if altMin!=0:                
            alt=(int((altMin-1.)/300.)+1) * 300
        else :
            alt=0
        listAlt=[]

        while alt <= round(altMax) :
            listAlt = listAlt + [ self.alti2code(alt) ]
            alt=alt+300

        return listAlt

    def getListAltMeter(self,num_massif,tagname="Massif"):

        listAltcode = self.getListAlt(num_massif, tagname=tagname)
        listAltMeter = []
        for code in listAltcode:
            listAltMeter.append(int(self.code2alti(code)))
            
        return listAltMeter

    def getAltMinMax(self,num_massif,tagname="Massif"):
        
        for massif in self.caracLoc.documentElement.getElementsByTagName(tagname) :           
            if int(massif.getElementsByTagName("number")[0].childNodes[0].data) == num_massif :
                altMin=float(massif.getElementsByTagName("altMin")[0].childNodes[0].data)
                altMax=float(massif.getElementsByTagName("altMax")[0].childNodes[0].data)
                break

        return (altMin, altMax)

    def indAltiArea(self,area,altitude,tagname="Massif"):
        # Matthieu L 13/05/2015
        # Pour tracer des cartes à des niveaux d'altitudes où tous les massifs de la zone ne sont pas définis
        listindices=[]
        for i,num_massif in enumerate(self.dicArea[area]):
            (altMin, altMax)=self.getAltMinMax(num_massif,tagname=tagname)
            if altitude>=altMin and altitude<=altMax:
                listindices.append(i)
        return listindices

    def getMassifNameFromLatLon(self,lat,lon,tagname="Massif"):
        return self.getMassifName(self.getMassifFromLatLon(lat,lon,tagname),tagname).strip()

    def getMassifName(self,num_massif,tagname="Massif"):

        for massif in self.caracLoc.documentElement.getElementsByTagName(tagname) :
            if int(massif.getElementsByTagName("number")[0].childNodes[0].data) == num_massif :
                massifName = massif.getElementsByTagName("name")[0].childNodes[0].data
                break

        return  massifName.encode("utf-8")


    def getAllMassifLatLon(self,tagname="Massif"):
        self.dicMassifLatLon={}
        dicLonLatMassif={}
        for massif in self.caracLoc.documentElement.getElementsByTagName(tagname) :
            lat=float(massif.getElementsByTagName("latCenter")[0].childNodes[0].data)
            lon=float(massif.getElementsByTagName("lonCenter")[0].childNodes[0].data)
            massifNumber = int(massif.getElementsByTagName("number")[0].childNodes[0].data)
            self.dicMassifLatLon[(lat,lon)]=massifNumber
            dicLonLatMassif[massifNumber]=(lon,lat)
        return dicLonLatMassif
    
    def getMassifFromLatLon(self,lat,lon,tagname="Massif"):
        
        if hasattr(self,"dicMassifLatLon"):
            return self.dicMassifLatLon[(lat,lon)]
        else:
            for massif in self.caracLoc.documentElement.getElementsByTagName(tagname) :
                if abs(float(massif.getElementsByTagName("latCenter")[0].childNodes[0].data) -lat)<0.001 and abs(float(massif.getElementsByTagName("lonCenter")[0].childNodes[0].data) -lon)<0.001 :
                    massifNumber = massif.getElementsByTagName("number")[0].childNodes[0].data
                    break

        return  int(massifNumber)      
        

    def getListMassif(self,tagname="Massif"):
        dicMassif={}
        for massif in self.caracLoc.documentElement.getElementsByTagName(tagname) :
            massifName = massif.getElementsByTagName("name")[0].childNodes[0].data
            numMassif = massif.getElementsByTagName("number")[0].childNodes[0].data
            latCenter = float(massif.getElementsByTagName("latCenter")[0].childNodes[0].data)
            lonCenter = float(massif.getElementsByTagName("lonCenter")[0].childNodes[0].data)
            dicMassif[(latCenter,lonCenter)] = (numMassif,massifName.encode("utf-8"))

        return dicMassif

    def getListSites(self):
        listSites=[]
        for site in self.caracLoc.documentElement.getElementsByTagName("Site"):
            listSites.append(str(site.getElementsByTagName("number")[0].childNodes[0].data).strip())
        return listSites
    
    def display(self,tagname="Massif"):
        # Print all available massifs
        dicMassif=self.getListMassif(tagname=tagname)
        dicNumMassif={}
        for massif in dicMassif:
            dicNumMassif[int(dicMassif[massif][0])]=dicMassif[massif][1]

        self.__ligneplus()            
        print("Massifs connus :")
        self.__ligneplus()                       
        for massif in sorted(dicNumMassif):
            print (str(massif)+": "+dicNumMassif[massif])
            
        self.__ligneplus()               
        print("Régions connues :")
        self.__ligneplus()           
        for area in sorted(self.dicArea):
            print area+": "+str(self.dicArea[area])
    
    def __ligneplus(self):
        print "+"*50            

