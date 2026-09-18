#!/usr/bin/python3

import csv
from snowtools.utils.infomassifs import infomassifs

def analyser_stations(fichier_altcheck, fichier_sortie):
    valid = [] # Postes valides
    sondage_ouverts = []  # Postes de sondage ouverts (3ème colonne = 2, 4ème colonne = b'2199-12-31')
    autres_ouverts = []    # Autres postes ouverts (3ème colonne ≠ 2, 4ème colonne = b'2199-12-31')
    sondage_fermes = []    # Postes de sondage fermés (3ème colonne = 2, 4ème colonne ≠ b'2199-12-31')
    autres_fermes = []     # Autres postes fermés (3ème colonne ≠ 2, 4ème colonne ≠ b'2199-12-31')
    lignes_filtrees = []  # Pour stocker les lignes à écrire dans le fichier filtré
   
    # Lecture du fichier carpost
    listpostes = []
    name = {}
    lat = {}
    lon = {}
    alti = {}
    slope = {}
    aspect = {}
    massif = {}
    massif_bdclim = {}
    datferm = {}
   
    with open(fichier_altcheck, mode='r', encoding='utf-8') as fichier_in, \
         open(fichier_sortie, mode='w', encoding='utf-8', newline='') as fichier_out:
        lecteur_csv = csv.reader(fichier_in, delimiter=' ')
        ecrivain_csv = csv.writer(fichier_out, delimiter=' ')

        for ligne in lecteur_csv:
            if len(ligne) >= 7:
                try:
                    ecart = int(ligne[6])
                    type_station = ligne[2]  # 3ème colonne (index 2)
                    date_fermeture = ligne[3]  # 4ème colonne (index 3)
                    numero_station = ligne[0]

                    if abs(ecart) > 100:
                        if type_station == '2':
                            if date_fermeture == "b'2199-12-31'":
                                sondage_ouverts.append(numero_station)
                            else:
                                sondage_fermes.append(numero_station)
                        else:
                            if date_fermeture == "b'2199-12-31'":
                                autres_ouverts.append(numero_station)
                            else:
                                autres_fermes.append(numero_station)
                                
                        # Stocker la ligne pour le fichier filtré (7 premières colonnes)
                        lignes_filtrees.append(ligne[:7])
                    else:
                        valid.append(numero_station)
                except (ValueError, IndexError):
                    continue
               
        # Écrire toutes les lignes filtrées dans le fichier de sortie
        ecrivain_csv.writerows(lignes_filtrees)

    # Jonction des 4 listes
    jonction = sondage_ouverts + autres_ouverts + sondage_fermes + autres_fermes

    return sondage_ouverts, autres_ouverts, sondage_fermes, autres_fermes, jonction

# Exemple d'utilisation
fichier_carpost = "carposts_info.csv"
fichier_altcheck = "altitude_check.csv"
fichier_sortie = "blacklist.csv"
sondage_ouverts, autres_ouverts, sondage_fermes, autres_fermes, resultat_final = analyser_stations(fichier_altcheck, fichier_sortie)

print(f"Postes de sondage ouverts (3ème colonne = 2, 4ème colonne = b'2199-12-31') ({len(sondage_ouverts)} postes) :")
print(sondage_ouverts)
print(f"\nAutres postes ouverts (3ème colonne ≠ 2, 4ème colonne = b'2199-12-31') ({len(autres_ouverts)} postes) :")
print(autres_ouverts)
print(f"\nPostes de sondage fermés (3ème colonne = 2, 4ème colonne ≠ b'2199-12-31') ({len(sondage_fermes)} postes) :")
print(sondage_fermes)
print(f"\nAutres postes fermés (3ème colonne ≠ 2, 4ème colonne ≠ b'2199-12-31') ({len(autres_fermes)} postes) :")
print(autres_fermes)

print(f"\nJonction des 4 listes ({len(resultat_final)} postes, doit être identique au résultat précédent) :")
print(resultat_final)

