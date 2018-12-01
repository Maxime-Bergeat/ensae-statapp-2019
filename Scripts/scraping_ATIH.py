#!/home/samuel/anaconda3/bin/python
import requests
import urllib
import bs4
import pandas as pd
import re
from urllib import request
import collections
import sys

def adapter_url(ghm,typ, annee) :
	url = 'https://www.scansante.fr/applications/statistiques-activite-MCO-par-GHM/submit?snatnav=&mbout=undefined&annee='+ str(annee)+'&tgeo=fe&codegeo=99&base='+str(typ)+'&ghm='+ghm
	return url

def typ(type) :
	if type == "pri" :
		return(9)
	elif type == "pub" :
		return(100)
	else :
		print("Pb sur le type")
		sys.exit(1)

def renvoie_une_ligne(ghm, hopital, annee):
	""" Ajouter une ligne à la liste items"""
	url = adapter_url(ghm, typ(hopital), annee)
	page = request.urlopen(url).read()
	page = bs4.BeautifulSoup(page, "lxml")
	#data = collections.defaultdict()
	donnees={}
	if len(page.findAll('table', { 'class' : "table"})) < 1 :
		return(donnees)
	else :
		table=page.findAll('table', { 'class' : "table"})[0]
		table_body = table.find('tbody')
		for row in table_body.findChildren(['tr']) : 
			if len(row) > 1 : # attention aux tr qui ne contiennent rien
				#print(row)
				if len(row.findAll('td', { 'class' : 'l data'})) < 1 :
					nom = "ERREUR"
				else :
					nom = row.findAll('td', { 'class' : 'l data'})[0].getText()
				if len(row.findAll('td', { 'class' : 'r data'})) <1 :
					info=""
				else :
					info = row.findAll('td', { 'class' : 'r data'})[0].getText()
					info = info.replace(" ","")
				donnees[nom] = info
	donnees["ghm.nro"] = ghm
	donnees["annee"] = annee
	donnees["type"] = hopital
	donnees["url"] = url
	return(donnees)

#############################################################
liste_ghm = pd.read_csv("GHM.NRO.csv")
items = []

#while i < len(liste_ghm) :
def scrap(a1,a2,i1=0,i2=len(liste_ghm),hopitaux=["pri", "pub"]):
	for hopital in hopitaux :
		print(hopital)
		for annee in range(a1,a2+1):
			i = i1
			while i < i2 :
				ghm = liste_ghm.iloc[i,1]
				donnees = renvoie_une_ligne(ghm, hopital, annee)
				print(annee, ghm, hopital, i)
				items.append(donnees)
				i+=1
	df = pd.DataFrame(items)
	cols = df.columns.tolist()
	cols = cols[-4:] + cols[:-4]
	df = df[cols]
	return(df)

df= scrap(a1=2017, a2=2017, i1=1000, i2=1001, hopitaux=["pri", "pub"])

import datetime
now = datetime.datetime.now()
now = now.strftime("%Y-%m-%d-%Hh")
nom = "ghm_atih_"+now+".csv"
print(nom)
df.to_csv(nom)

