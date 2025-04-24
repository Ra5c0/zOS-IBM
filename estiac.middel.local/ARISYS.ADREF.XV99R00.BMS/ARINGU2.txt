ARINGU2  DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,TIOAPFX=YES
* *********************************************************************
* MAP CONSULTATION EN ACCES DIRECT (ARTICLE)
* *********************************************************************
* ARIMGU2 MAP DE TAILLE 24X80 (NB LIGNES X NB COLONNES)
ARIMGU2  DFHMDI SIZE=(24,80),                                          *
               LINE=1,                                                 *
               COLUMN=1,                                               *
               CTRL=(FREEKB,FRSET)
* TEXTE EN LIGNE 1 COLONNE 1, DE LONGUEUR 45 CARACTERES
         DFHMDF POS=(01,01),                                           *
               LENGTH=45,                                              *
               ATTRB=(ASKIP,BRT),                                      *
               INITIAL='FORMATION CICS -            ESTIAC INSTITUT  '
* TEXTE EN LIGNE 1 COLONNE 62, 6 CARACTERES
         DFHMDF POS=(01,62),                                           *
               LENGTH=06,                                              *
               ATTRB=(ASKIP),                                          *
               INITIAL='DATE :'
* CHAMP CONTENANT UNE VARIABLE AFFICHANT LA DATE DU JOUR
MDATE    DFHMDF POS=(01,69),                                           *
               LENGTH=10,                                              *
               ATTRB=(ASKIP,BRT)
* TEXTE EN LIGNE 2 COLONNE 20,36 CARACTERES
         DFHMDF POS=(02,20),                                           *
               LENGTH=36,                                              *
               ATTRB=(ASKIP),                                          *
               INITIAL='===================================='
* TEXTE EN LIGNE 2 COLONNE 58, 10 CARACTERES
         DFHMDF POS=(02,58),                                           *
               LENGTH=10,                                              *
               ATTRB=(ASKIP),                                          *
               INITIAL='TERMINAL :'
* VARIABLE AFFICHANT LE TERMINAL DE L'UTILISATEUR
MTERM    DFHMDF POS=(02,69),                                           *
               LENGTH=04,                                              *
               ATTRB=(ASKIP,BRT)
* TEXTE EN LIGNE 3 COLONNE 61, 7 CARACTERES
         DFHMDF POS=(03,61),                                           *
               LENGTH=07,                                              *
               ATTRB=(ASKIP),                                          *
               INITIAL='TACHE :'
* VARIABLE AFFICHANT LE NUMERO DE TACHE DE L'APPLICATION GESTART
MTASK    DFHMDF POS=(03,69),                                           *
               LENGTH=07,                                              *
               ATTRB=(ASKIP,BRT)
* TEXTE EN LIGNE 4 COLONNE 59, 9 CARACTERES
         DFHMDF POS=(04,59),                                           *
               LENGTH=09,                                              *
               ATTRB=(ASKIP),                                          *
               INITIAL='TRANSID :'
* VARIABLE AFFICHANT LE NUMERO DE TRANSACTION
MTRAN    DFHMDF POS=(04,69),                                           *
               LENGTH=04,                                              *
               ATTRB=(ASKIP,BRT)
* TEXTE EN LIGNE 5 COLONNE 18, 42 CARACTERES
         DFHMDF POS=(05,18),                                           *
               LENGTH=42,                                              *
               ATTRB=(ASKIP,BRT),                                      *
               INITIAL='APPLICATION GESTART - CONSULTATION DIRECTE'
* TEXTE EN LIGNE 6 COLONNE 18, 42 CARACTERES
         DFHMDF POS=(06,18),                                           *
               LENGTH=42,                                              *
               ATTRB=(ASKIP),                                          *
               INITIAL='------------------------------------------'
* TEXTE EN LIGNE 9 COLONNE 5, 6 CARACTERES
         DFHMDF POS=(09,05),                                           *
               LENGTH=06,                                              *
               ATTRB=(ASKIP),                                          *
               INITIAL='CODE :'
* CHAMP PERMETTANT à L'UTILISATEUR D'INDIQUER UN CODE D'ARTICLE
MCODE    DFHMDF POS=(09,12),                                           *
               LENGTH=05,                                              *
               ATTRB=(BRT,FSET,IC)
* TEXTE EN LIGNE 9 COLONNE 18, 13 CARACTERES
         DFHMDF POS=(09,18),                                           *
               LENGTH=13,                                              *
               ATTRB=(ASKIP),                                          *
               INITIAL='    ARTICLE :'
* VARIABLE AFFICHANT LE NOM DE L'ARTICLE CORRESPONDANT AU CODE
MLIBEL   DFHMDF POS=(09,32),                                           *
               LENGTH=20,                                              *
               ATTRB=(ASKIP,BRT)
* TEXTE EN LIGNE 9 COLONNE 18, 11 CARACTERES
         DFHMDF POS=(09,58),                                           *
               LENGTH=11,                                              *
               ATTRB=(ASKIP),                                          *
               INITIAL='CATEGORIE :'
* VARIABLE AFFICHANT LA CATEGORIE DE L'ARTICLE CORRESPONDANT AU CODE
MCATEG   DFHMDF POS=(09,70),                                           *
               LENGTH=05,                                              *
               ATTRB=(ASKIP,BRT)
* TEXTE EN LIGNE 10 COLONNE 13, 13 CARACTERES
         DFHMDF POS=(10,13),                                           *
               LENGTH=13,                                              *
               ATTRB=(ASKIP),                                          *
               INITIAL='FOURNISSEUR :'
* VARIABLE AFFICHANT LE NOM DU FOURNISSEUR DE L'ARTICLE
MFOUR    DFHMDF POS=(10,27),                                           *
               LENGTH=05,                                              *
               ATTRB=(ASKIP,BRT)
* TEXTE EN LIGNE 10 COLONNE 40, 14 CARACTERES
         DFHMDF POS=(10,40),                                           *
               LENGTH=14,                                              *
               ATTRB=(ASKIP),                                          *
               INITIAL='DELAI APPRO. :'
* VARIABLE AFFICHANT LE DELAI APPROXIMATIF D'OBTENTION DE L'ARTICLE
MAPPRO   DFHMDF POS=(10,55),                                           *
               LENGTH=02,                                              *
               ATTRB=(ASKIP,BRT)
* TEXTE EN LIGNE 10 COLONNE 58, 7 CARACTERES
         DFHMDF POS=(10,58),                                           *
               LENGTH=07,                                              *
               ATTRB=(ASKIP),                                          *
               INITIAL='JOUR(S)'
* TEXTE EN LIGNE 10 COLONNE 40, 14 CARACTERES
         DFHMDF POS=(11,07),                                           *
               LENGTH=11,                                              *
               ATTRB=(ASKIP),                                          *
               INITIAL='NBRE LOTS :'
* VARIABLE AFFICHANT LE NOMBRE DE LOTS DE L'ARTICLE
MNLOT    DFHMDF POS=(11,19),                                           *
               LENGTH=01,                                              *
               ATTRB=(ASKIP,BRT)
* TEXTE EN LIGNE 11 COLONNE 30, 11 CARACTERES
         DFHMDF POS=(11,30),                                           *
               LENGTH=11,                                              *
               ATTRB=(ASKIP),                                          *
               INITIAL='QTE STOCK :'
* VARIABLE AFFICHANT LA QUANTITE EN STOCK DE L'ARTICLE
MQTSTK   DFHMDF POS=(11,42),                                           *
               LENGTH=06,                                              *
               ATTRB=(ASKIP,BRT)
* TEXTE EN LIGNE 11 COLONNE 60, 12 CARACTERES
         DFHMDF POS=(11,60),                                           *
               LENGTH=12,                                              *
               ATTRB=(ASKIP),                                          *
               INITIAL='QTE ALERTE :'
* VARIABLE AFFICHANT LA QUANTITE EN STOCK MINIMALE POUR ETRE INFORME
* QUE LE STOCK COMMENCE A ETRE INSUFFISANT
MQTALE   DFHMDF POS=(11,73),                                           *
               LENGTH=05,                                              *
               ATTRB=(ASKIP,BRT)
* TEXTE EN LIGNE 14 COLONNE 2, 78 CARACTERES
         DFHMDF POS=(14,02),                                           *
               LENGTH=78,                                              *
               ATTRB=(ASKIP),                                          *
               INITIAL='         LIBELLE LOT         QUANTITE EN STOCK *
                       PRIX UNITAIRE'
* VARIABLE AFFICHANT LE LIBELLE DES LOTS DE L'ARTICLE
MLOT     DFHMDF POS=(15,01),                                           *
               LENGTH=79,                                              *
               ATTRB=(ASKIP,BRT),                                      *
               OCCURS=5
* TEXTE EN LIGNE 21 COLONNE 1, 9 CARACTERES
         DFHMDF POS=(21,01),                                           *
               LENGTH=09,                                              *
               ATTRB=(ASKIP),                                          *
               INITIAL='MESSAGE :'
* VARIABLE AFFICHANT LE MESSAGE D'ERREUR SI ANOMALIE
MMSG     DFHMDF POS=(22,01),                                           *
               LENGTH=79,                                              *
               ATTRB=(ASKIP,BRT)
* TEXTE EN LIGNE 24 COLONNE 14, 50 CARACTERES
         DFHMDF POS=(24,14),                                           *
               LENGTH=50,                                              *
               ATTRB=(ASKIP,BRT),                                      *
               INITIAL='ENTREE => DEMANDE INFO - PF1 => AIDE - PF3 => M*
               ENU'
* MAP AIDE
* ARIMHP2 MAP DE TAILLE 24X80 (NB LIGNES X NB COLONNES)
ARIMHP2  DFHMDI SIZE=(24,80),                                          *
               LINE=1,                                                 *
               COLUMN=1,                                               *
               CTRL=(FREEKB,FRSET)
* TEXTE EN LIGNE 1 COLONNE 1, 45 CARACTERES
         DFHMDF POS=(01,01),                                           *
               LENGTH=45,                                              *
               ATTRB=(ASKIP,BRT),                                      *
               INITIAL='FORMATION CICS -            ESTIAC INSTITUT'
* TEXTE EN LIGNE 2 COLONNE 20,36 CARACTERES
         DFHMDF POS=(02,20),                                           *
               LENGTH=36,                                              *
               ATTRB=(ASKIP,BRT),                                      *
               INITIAL='===================================='
* TEXTE EN LIGNE 5 COLONNE 9, 63 CARACTERES
         DFHMDF POS=(05,09),                                           *
               LENGTH=63,                                              *
               ATTRB=(ASKIP,BRT),                                      *
               INITIAL='APPLICATION GESTART - CONSULTATION DIRECTE - AI*
               DE EN LIGNE'
* TEXTE EN LIGNE 6 COLONNE 9, 63 CARACTERES
         DFHMDF POS=(06,09),                                           *
               LENGTH=63,                                              *
               ATTRB=(ASKIP),                                          *
               INITIAL='-----------------------------------------------*
               -----------'
* TEXTE EN LIGNE 9 COLONNE 1, 60 CARACTERES
         DFHMDF POS=(09,02),                                           *
               LENGTH=70,                                              *
               ATTRB=(ASKIP),                                          *
               INITIAL='INITIALISER LE CHAMPS CODE AVANT CHAQUE DEMANDE*
                D''INFORMATON (ENTREE).'
* TEXTE EN LIGNE 10 COLONNE 10, 54 CARACTERES
         DFHMDF POS=(10,02),                                           *
               LENGTH=73,                                              *
               ATTRB=(ASKIP),                                          *
               INITIAL='L''APPLICATION AFFICHE L''ENSEMBLE DES INFORMAT*
               IONS RELATIVE A UN ARTICLE.'
* TEXTE EN LIGNE 11 COLONNE 10, 56 CARACTERES
         DFHMDF POS=(11,02),                                           *
               LENGTH=70,                                              *
               ATTRB=(ASKIP),                                          *
               INITIAL='DANS LA TABLE DES LOTS, CHAQUE POSTE DECRIT UN *
               LOT DIFFERENT.'
* TEXTE EN LIGNE 15 COLONNE 2, 70 CARACTERES
         DFHMDF POS=(15,02),                                           *
               LENGTH=70,                                              *
               ATTRB=(ASKIP,BRT),                                      *
               INITIAL='LES TOUCHES FONCTIONS SUIVANTES SONT ACTIVES LO*
               RS DE LA CONSULTATION :'
* TEXTE EN LIGNE 16 COLONNE 2, 76 CARACTERES
         DFHMDF POS=(16,10),                                           *
               LENGTH=66,                                              *
               ATTRB=(ASKIP),                                          *
               INITIAL='ENTREE ==> ENVOI DU CODE ARTICLE SAISI (DEMANDE*
                D''INFORMATION)'
* TEXTE EN LIGNE 17 COLONNE 2, 52 CARACTERES
         DFHMDF POS=(17,10),                                           *
               LENGTH=66,                                              *
               ATTRB=(ASKIP),                                          *
               INITIAL='PF1    ==> AFFICHAGE DE L''AIDE CONTEXTUELLE EN*
                LIGNE'
* TEXTE EN LIGNE 18 COLONNE 2, 67 CARACTERES
         DFHMDF POS=(18,10),                                           *
               LENGTH=66,                                              *
               ATTRB=(ASKIP),                                          *
               INITIAL='PF3    ==> RETOUR AU MENU PRINCIPAL'
* TEXTE EN LIGNE 24 COLONNE 4, 71 CARACTERES
         DFHMDF POS=(24,04),                                           *
               LENGTH=71,                                              *
               ATTRB=(ASKIP,BRT),                                      *
               INITIAL='APPUYER SUR N''IMPORTE QUELLE TOUCHE FONCTION P*
               OUR REVENIR AU TRAITEMENT'
* FIN DU BMS
         DFHMSD TYPE=FINAL
         END
