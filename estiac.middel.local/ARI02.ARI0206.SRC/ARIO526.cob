      *CBL,TEST,LIST,NOOFFSET
      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIO526                                   *
      *  NOM DU REDACTEUR : GIGON OSCAR                               *
      *  SOCIETE          : ESTIAC INSTITUT                           *
      *  DATE DE CREATION : 14/03/2025                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      * CE PROGRAMME A POUR OBJECTIF DE LIRE DES ENREGISTREMENTS      *
      * DEPUIS UN FICHIER SYSIN (DEMANDES). PUIS DE RECUPERER DES     *
      * INFORMATIONS DU FICHIER DES COMPTES CLIENTS (F-CPTE-E) POUR   *
      * REMPLIR LES FICHIERS ETAT DES DEMANDES DE LISTES DE COMPTES   *
      * (ETATCLI) ET ETAT DES ANOMALIES (ETATANO). AINSI QU'UN        *
      * COMPTE RENDU D'EXECUTION EN SYSOUT.                           *
      *---------------------------------------------------------------*
      *--               HISTORIQUE DES MODIFICATIONS                --*
      *---------------------------------------------------------------*
      * DATE  MODIF   §          NATURE DE LA MODIFICATION           *
      *---------------------------------------------------------------*
      * 14/03/2025    §  CREATION DU FICHIER / DEBUT CODE            *
      *               §                                              *
      *===============================================================*
      *
      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID.      ARIO526
      *
      *                  ==============================               *
      *=================<   ENVIRONMENT     DIVISION   >==============*
      *                  ==============================               *
      *                                                               *
      *===============================================================*
      *
      **********************
       ENVIRONMENT DIVISION.
      **********************
      *
      *======================
       CONFIGURATION SECTION.
      *======================
      *
      *--------------
       SPECIAL-NAMES.
      *--------------
           DECIMAL-POINT IS COMMA.
      *
      *=====================
       INPUT-OUTPUT SECTION.
      *=====================
      *
      *-------------
       FILE-CONTROL.
      *-------------
      *                     -------------------------------------------
      *                     F-CPTE-E: FICHIER DES COMPTES CLIENTS
      *                     -------------------------------------------
           SELECT F-CPTE-E                  ASSIGN TO INP001
                  ORGANIZATION              IS INDEXED
                  ACCESS MODE               IS DYNAMIC
                  RECORD KEY                IS FS-CPTE-CPTE
                  ALTERNATE RECORD          IS FS-CPTE-NOM
                                            WITH DUPLICATES
                  FILE STATUS               IS WS-FS-CPTE-E.
      *                     -------------------------------------------
      *                     F-ETATCLI-S : FICHIER ETAT DES DEMANDES
      *                     -------------------------------------------
           SELECT F-ETATCLI-S               ASSIGN TO ETATCLI
                  FILE STATUS               IS WS-FS-ETATCLI-S.
      *                     -------------------------------------------
      *                     F-ETATANO-S : FICHIER ETAT DES ANOMALIES
      *                     -------------------------------------------
           SELECT F-ETATANO-S               ASSIGN TO ETATANO
                  FILE STATUS               IS WS-FS-ETATANO-S.
      *
      *                  ==============================               *
      *=================<   DATA            DIVISION   >==============*
      *                  ==============================               *
      *                                                               *
      *===============================================================*
      *
      ***************
       DATA DIVISION.
      ***************
      *
      *=============
       FILE SECTION.
      *=============
      *
       FD  F-CPTE-E
           RECORD CONTAINS 50 CHARACTERS.
       01  FS-ENRG-CPTE-E.
           05  FS-CPTE-CPTE                 PIC X(10).
           05  FS-CPTE-NOM                  PIC X(14).
           05  FILLER                       PIC X(26).
      *
       FD  F-ETATCLI-S
           RECORDING MODE IS F.
       01  FS-ENRG-ETATCLI-S                PIC X(80).
      *
       FD  F-ETATANO-S
           RECORDING MODE IS F.
       01  FS-ENRG-ETATANO-S                PIC X(80).
      *
      *========================
       WORKING-STORAGE SECTION.
      *========================
      *
      *------------------- LIGNES D'EDITION --------------------------*
      *
       COPY TP5LEDIT.
      *
      *------------------- ENREGISTREMENT DEMANDE EN SYSIN -----------*
      *
       COPY TP5DEMAN.
      *
      *------------------- ENREGISTREMENT CPTE EN ENTREE -------------*
      *
       COPY TP5CPTE.
      *
      *------------------- VARIABLES DE TRAITEMENT -------------------*
      *
       01  WS-BUFFER                        PIC X(80).
      *
       01  WS-FS-CPTE-E                     PIC XX.
           88  CPTE-OK                      VALUE '00'.
           88  DUPLICATE-KEY                VALUE '02'.
           88  EOF-CPTE                     VALUE '10'.
           88  NOT-ENRG-CPTE                VALUE '23'.
      *
       01  WS-FS-ETATCLI-S                  PIC XX.
           88  ETATCLI-OK                   VALUE '00'.
      *
       01  WS-FS-ETATANO-S                  PIC XX.
           88  ETATANO-OK                   VALUE '00'.
      *
       01  WS-FIRST-CPTE                    PIC 9(10).
      *
       01  WS-FIRST-NAME                    PIC X(14).
      *
       01  WS-CPT-DEM                       PIC S9(4) COMP.
      *
       01  WS-CPT-ERR                       PIC S9(4) COMP.
           88  CPT-ERR-NULL                 VALUE 0.
      *
       01  WS-CPT-ENRG-VIDE                 PIC S9(4) COMP.
           88  CPT-ENRG-VIDE-NULL           VALUE 0.
      *
       01  WS-CODE-ERROR                    PIC 9(2)  VALUE ZERO.
           88  CODE-ERR-NULL                VALUE 0.
      *
       01  WS-MSG-ERROR                     PIC X(49) VALUE SPACE.
      *
      *                  ==============================               *
      *=================<    PROCEDURE      DIVISION   >==============*
      *                  ==============================               *
      *
      *===============================================================*
      *
      *********************
       PROCEDURE  DIVISION.
      *********************
      *
      *---------------------------------------------------------------*
      *               TRAITEMENT PRINCIPAL                            *
      *               ====================                            *
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      * DEBUT DU PROGRAMME                                            *
      *---------------------------------------------------------------*
       0000-TRT-PRINCIPAL-DEB.
      *
      *----------- PREPARATION DU TRAITEMENT
      *
           PERFORM 6000-OPEN-FCPTE-DEB
              THRU 6000-OPEN-FCPTE-FIN.
      *
           PERFORM 6010-OPEN-FETATCLI-DEB
              THRU 6010-OPEN-FETATCLI-FIN.
      *
           PERFORM 6020-OPEN-FETATANO-DEB
              THRU 6020-OPEN-FETATANO-FIN.
      *
           PERFORM 6030-POINTER-PRI-DEB
              THRU 6030-POINTER-PRI-FIN.
      *
           PERFORM 6040-READ-FCPTE-PRI-DEB
              THRU 6040-READ-FCPTE-PRI-FIN.
      *
           PERFORM 6050-POINTER-SEC-DEB
              THRU 6050-POINTER-SEC-FIN.
      *
           PERFORM 6060-READ-FCPTE-SEC-DEB
              THRU 6060-READ-FCPTE-SEC-FIN.
      *
           PERFORM 7020-FIRST-NAME-DEB
              THRU 7020-FIRST-NAME-FIN.
      *
           PERFORM 6070-READ-SYSIN-DEB
              THRU 6070-READ-SYSIN-FIN.
      *
      *----------- APPEL DU COMPOSANT SUIVANT
      *
           PERFORM 1000-TRT-DEMANDE-DEB
              THRU 1000-TRT-DEMANDE-FIN
             UNTIL EOF-DEMANDE.
      *
      *----------- FIN DU TRAITEMENT
      *
           IF NOT CPT-ERR-NULL
              PERFORM 8070-EDIT-PP-FETATANO-DEB
                 THRU 8070-EDIT-PP-FETATANO-FIN
           END-IF.
      *
           PERFORM 8999-STATISTIQUES-DEB
              THRU 8999-STATISTIQUES-FIN.
      *
           PERFORM 6120-CLOSE-FCPTE-DEB
              THRU 6120-CLOSE-FCPTE-FIN.
      *
           PERFORM 6130-CLOSE-FETATCLI-DEB
              THRU 6130-CLOSE-FETATCLI-FIN.
      *
           PERFORM 6140-CLOSE-FETATANO-DEB
              THRU 6140-CLOSE-FETATANO-FIN.
      *
           PERFORM 9999-FIN-PROGRAMME-DEB
              THRU 9999-FIN-PROGRAMME-FIN.
      *
      *---------------------------------------------------------------*
      * FIN DU PROGRAMME                                              *
      *---------------------------------------------------------------*
       0000-TRT-PRINCIPAL-FIN.
           STOP RUN.
      *
      *---------------------------------------------------------------*
      *               TRAITEMENT DES DEMANDES                         *
      *               =======================                         *
      *---------------------------------------------------------------*
      *
       1000-TRT-DEMANDE-DEB.
      *
      *----------- PREPARATION DU TRAITEMENT
      *
           PERFORM 7030-IN-TRT-DEM-DEB
              THRU 7030-IN-TRT-DEM-FIN.
      *
      *----------- APPEL DU COMPOSANT SUIVANT
      *
           EVALUATE TRUE
              WHEN TYPE-A
                 PERFORM 2000-TRT-A-DEB
                    THRU 2000-TRT-A-FIN
              WHEN TYPE-B
                 PERFORM 2010-TRT-B-DEB
                    THRU 2010-TRT-B-FIN
              WHEN OTHER
                 PERFORM 2020-TRT-ANO-DEB
                    THRU 2020-TRT-ANO-FIN
           END-EVALUATE.
      *
      *----------- FIN DE TRAITEMENT
      *
           PERFORM 6070-READ-SYSIN-DEB
              THRU 6070-READ-SYSIN-FIN.
      *
       1000-TRT-DEMANDE-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *               TRAITEMENT DES DEMANDES A                       *
      *               =========================                       *
      *---------------------------------------------------------------*
      *
       2000-TRT-A-DEB.
      *
      *----------- PREPARATION DU TRAITEMENT
      *
           PERFORM 6150-POINTER-PRI-CPTE-DEB
              THRU 6150-POINTER-PRI-CPTE-FIN.
      *
           EVALUATE TRUE
              WHEN (WS-DEM-NOM = SPACE OR
                    WS-DEM-CPT-DEB = SPACE OR
                    WS-DEM-CPT-FIN = SPACE)
                 PERFORM 7050-ERR-02-DEB
                    THRU 7050-ERR-02-FIN
              WHEN (WS-DEM-CPT-DEB IS NOT NUMERIC OR
                    WS-DEM-CPT-FIN IS NOT NUMERIC)
                 PERFORM 7060-ERR-03-DEB
                    THRU 7060-ERR-03-FIN
              WHEN (WS-DEM-CPT-DEB > WS-DEM-CPT-FIN)
                 PERFORM 7070-ERR-04-DEB
                    THRU 7070-ERR-04-FIN
              WHEN (WS-DEM-CPT-FIN < WS-FIRST-CPTE)
                 PERFORM 7080-ERR-05-DEB
                    THRU 7080-ERR-05-FIN
              WHEN NOT-ENRG-CPTE
                 PERFORM 7080-ERR-05-DEB
                    THRU 7080-ERR-05-FIN
              WHEN OTHER
                 PERFORM 7160-NO-ERR-DEB
                    THRU 7160-NO-ERR-FIN
           END-EVALUATE.
      *
           PERFORM 7090-INIT-CPT-ENRG-VIDE-DEB
              THRU 7090-INIT-CPT-ENRG-VIDE-FIN.
      *
      *----------- APPEL DU COMPOSANT SUIVANT
      *
           IF CODE-ERR-NULL
              PERFORM 3000-TRT-COMPTES-DEB
                 THRU 3000-TRT-COMPTES-FIN
           ELSE
              PERFORM 3010-TRT-ERR-DEB
                 THRU 3010-TRT-ERR-FIN
           END-IF.
      *
       2000-TRT-A-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *               TRAITEMENT DES DEMANDES B                       *
      *               =========================                       *
      *---------------------------------------------------------------*
      *
       2010-TRT-B-DEB.
      *
      *----------- PREPARATION DU TRAITEMENT
      *
           PERFORM 6160-POINTER-SEC-NOM-DEB
              THRU 6160-POINTER-SEC-NOM-FIN.
      *
           EVALUATE TRUE
              WHEN (WS-DEM-NOM = SPACE OR
                    WS-DEM-CLI-DEB = SPACE OR
                    WS-DEM-CLI-FIN = SPACE)
                 PERFORM 7050-ERR-02-DEB
                    THRU 7050-ERR-02-FIN
              WHEN (WS-DEM-CLI-DEB IS NUMERIC OR
                    WS-DEM-CLI-FIN IS NUMERIC)
                 PERFORM 7060-ERR-03-DEB
                    THRU 7060-ERR-03-FIN
              WHEN (WS-DEM-CLI-DEB > WS-DEM-CLI-FIN)
                 PERFORM 7070-ERR-04-DEB
                    THRU 7070-ERR-04-FIN
              WHEN (WS-DEM-CLI-FIN < WS-FIRST-NAME)
                 PERFORM 7080-ERR-05-DEB
                    THRU 7080-ERR-05-FIN
              WHEN NOT-ENRG-CPTE
                 PERFORM 7080-ERR-05-DEB
                    THRU 7080-ERR-05-FIN
              WHEN OTHER
                 PERFORM 7160-NO-ERR-DEB
                    THRU 7160-NO-ERR-FIN
           END-EVALUATE.
      *
           PERFORM 7090-INIT-CPT-ENRG-VIDE-DEB
              THRU 7090-INIT-CPT-ENRG-VIDE-FIN.
      *
      *----------- APPEL DU COMPOSANT SUIVANT
      *
           IF CODE-ERR-NULL
              PERFORM 3020-TRT-NOMS-DEB
                 THRU 3020-TRT-NOMS-FIN
           ELSE
              PERFORM 3010-TRT-ERR-DEB
                 THRU 3010-TRT-ERR-FIN
           END-IF.
      *
       2010-TRT-B-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *               TRAITEMENT DES ANOMALIES                        *
      *               ========================                        *
      *---------------------------------------------------------------*
      *
       2020-TRT-ANO-DEB.
      *
           PERFORM 7140-GEST-ANO-DEB
              THRU 7140-GEST-ANO-FIN.
      *
           IF CPT-ERR-NULL
              PERFORM 8050-EDIT-ENT-FETATANO-DEB
                 THRU 8050-EDIT-ENT-FETATANO-FIN
           END-IF.
      *
           PERFORM 8060-EDIT-LG-FETATANO-DEB
              THRU 8060-EDIT-LG-FETATANO-FIN.
      *
       2020-TRT-ANO-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *               TRAITEMENT DES COMPTES                          *
      *               ======================                          *
      *---------------------------------------------------------------*
      *
       3000-TRT-COMPTES-DEB.
      *
      *----------- PREPARATION DU TRAITEMENT
      *
           PERFORM 7110-GEST-ENT-CPTE-DEB
              THRU 7110-GEST-ENT-CPTE-FIN.
      *
           PERFORM 8000-EDIT-ENT-FETATCLI-DEB
              THRU 8000-EDIT-ENT-FETATCLI-FIN.
      *
           PERFORM 6040-READ-FCPTE-PRI-DEB
              THRU 6040-READ-FCPTE-PRI-FIN.
      *
      *----------- APPEL DU COMPOSANT SUIVANT
      *
           PERFORM 4000-TRT-COMPTE-DEB
              THRU 4000-TRT-COMPTE-FIN
             UNTIL (WS-CPTE-CPTE > WS-DEM-CPT-FIN) OR EOF-CPTE.
      *
      *----------- FIN DU TRAITEMENT
      *
           IF CPT-ENRG-VIDE-NULL
              PERFORM 8020-EDIT-0-CPTE-DEB
                 THRU 8020-EDIT-0-CPTE-FIN
           END-IF.
      *
           PERFORM 8040-EDIT-PP-FETATCLI-DEB
              THRU 8040-EDIT-PP-FETATCLI-FIN.
      *
       3000-TRT-COMPTES-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *               TRAITEMENT DES ERREURS                          *
      *               ======================                          *
      *---------------------------------------------------------------*
      *
       3010-TRT-ERR-DEB.
      *
           IF CPT-ERR-NULL
              PERFORM 8050-EDIT-ENT-FETATANO-DEB
                 THRU 8050-EDIT-ENT-FETATANO-FIN
           END-IF.
      *
           PERFORM 7150-GEST-ERR-DEB
              THRU 7150-GEST-ERR-FIN.
      *
           PERFORM 8060-EDIT-LG-FETATANO-DEB
              THRU 8060-EDIT-LG-FETATANO-FIN.
      *
       3010-TRT-ERR-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *               TRAITEMENT DES NOMS                             *
      *               ===================                             *
      *---------------------------------------------------------------*
      *
       3020-TRT-NOMS-DEB.
      *
      *----------- PREPARATION DU TRAITEMENT
      *
           PERFORM 7120-GEST-ENT-NOM-DEB
              THRU 7120-GEST-ENT-NOM-FIN.
      *
           PERFORM 8000-EDIT-ENT-FETATCLI-DEB
              THRU 8000-EDIT-ENT-FETATCLI-FIN.
      *
           PERFORM 6060-READ-FCPTE-SEC-DEB
              THRU 6060-READ-FCPTE-SEC-FIN.
      *
      *----------- APPEL DU COMPOSANT SUIVANT
      *
           PERFORM 4010-TRT-NOM-DEB
              THRU 4010-TRT-NOM-FIN
             UNTIL (WS-CPTE-NOM > WS-DEM-CLI-FIN) OR EOF-CPTE.
      *
      *----------- FIN DU TRAITEMENT
      *
           IF CPT-ENRG-VIDE-NULL
              PERFORM 8030-EDIT-0-NOM-DEB
                 THRU 8030-EDIT-0-NOM-FIN
           END-IF.
      *
           PERFORM 8040-EDIT-PP-FETATCLI-DEB
              THRU 8040-EDIT-PP-FETATCLI-FIN.
      *
       3020-TRT-NOMS-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *               TRAITEMENT DE COMPTE                            *
      *               ====================                            *
      *---------------------------------------------------------------*
      *
       4000-TRT-COMPTE-DEB.
      *
           PERFORM 7130-GEST-LG-ETATCLI-DEB
              THRU 7130-GEST-LG-ETATCLI-FIN.
      *
           PERFORM 8010-EDIT-LG-FETATCLI-DEB
              THRU 8010-EDIT-LG-FETATCLI-FIN.
      *
           PERFORM 6040-READ-FCPTE-PRI-DEB
              THRU 6040-READ-FCPTE-PRI-FIN.
      *
       4000-TRT-COMPTE-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *               TRAITEMENT DE CLIENT                            *
      *               ====================                            *
      *---------------------------------------------------------------*
      *
       4010-TRT-NOM-DEB.
      *
           PERFORM 7130-GEST-LG-ETATCLI-DEB
              THRU 7130-GEST-LG-ETATCLI-FIN.
      *
           PERFORM 8010-EDIT-LG-FETATCLI-DEB
              THRU 8010-EDIT-LG-FETATCLI-FIN.
      *
           PERFORM 6060-READ-FCPTE-SEC-DEB
              THRU 6060-READ-FCPTE-SEC-FIN.
      *
       4010-TRT-NOM-FIN.
           EXIT.
      *
      *===============================================================*
      *===============================================================*
      *    STRUCTURATION DE LA PARTIE INDEPENDANTE DU PROGRAMME       *
      *---------------------------------------------------------------*
      *                                                               *
      *   6XXX-  : ORDRES DE MANIPULATION DES FICHIERS                *
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *   9XXX-  : ORDRES DE MANIPULATION DES SOUS-PROGRAMMES         *
      *   9999-  : PROTECTION FIN DE PROGRAMME                        *
      *                                                               *
      *===============================================================*
      *===============================================================*
      *
      *---------------------------------------------------------------*
      *   6XXX-  : ORDRES DE MANIPULATION DES FICHIERS                *
      *---------------------------------------------------------------*
      *
       6000-OPEN-FCPTE-DEB.
           OPEN INPUT F-CPTE-E.
           IF NOT CPTE-OK
              DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER F-CPTE-E'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-CPTE-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6000-OPEN-FCPTE-FIN.
            EXIT.
      *---------------------------------------------------------------*
      *
       6010-OPEN-FETATCLI-DEB.
           OPEN OUTPUT F-ETATCLI-S.
           IF NOT ETATCLI-OK
              DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER F-ETATCLI-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATCLI-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6010-OPEN-FETATCLI-FIN.
            EXIT.
      *---------------------------------------------------------------*
      *
       6020-OPEN-FETATANO-DEB.
           OPEN OUTPUT F-ETATANO-S.
           IF NOT ETATANO-OK
              DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER F-ETATANO-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6020-OPEN-FETATANO-FIN.
            EXIT.
      *---------------------------------------------------------------*
      *
       6030-POINTER-PRI-DEB.
           MOVE LOW-VALUE                   TO FS-CPTE-CPTE.
           START F-CPTE-E
              KEY >= FS-CPTE-CPTE
           END-START.
           IF NOT CPTE-OK AND NOT NOT-ENRG-CPTE
              DISPLAY 'PROBLEME DE POSITIONNEMENT SUR F-CPTE-E'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-CPTE-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6030-POINTER-PRI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6040-READ-FCPTE-PRI-DEB.
           READ F-CPTE-E NEXT
              INTO WS-ENRG-F-CPTE
           END-READ.
           IF NOT CPTE-OK AND NOT EOF-CPTE
              DISPLAY 'PROBLEME DE LECTURE DU FICHIER F-CPTE-E'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-CPTE-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6040-READ-FCPTE-PRI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6050-POINTER-SEC-DEB.
           MOVE WS-CPTE-CPTE                TO WS-FIRST-CPTE.
           MOVE LOW-VALUE                   TO FS-CPTE-NOM.
           START F-CPTE-E
              KEY >= FS-CPTE-NOM
           END-START.
           IF NOT CPTE-OK AND NOT NOT-ENRG-CPTE
              DISPLAY 'PROBLEME DE POSITIONNEMENT SUR F-CPTE-E'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-CPTE-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6050-POINTER-SEC-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6060-READ-FCPTE-SEC-DEB.
           READ F-CPTE-E NEXT
              INTO WS-ENRG-F-CPTE
           END-READ.
           IF NOT CPTE-OK AND NOT EOF-CPTE AND NOT DUPLICATE-KEY
              DISPLAY 'PROBLEME DE LECTURE DU FICHIER F-CPTE-E'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-CPTE-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6060-READ-FCPTE-SEC-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6070-READ-SYSIN-DEB.
           ACCEPT WS-ENRG-DEMANDE.
       6070-READ-SYSIN-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6080-WRITE-NEW-FETATCLI-DEB.
           WRITE FS-ENRG-ETATCLI-S          FROM WS-BUFFER AFTER PAGE.
           IF NOT ETATCLI-OK
              DISPLAY 'PROBLEME DE SAUT DE PAGE DU FICHIER F-ETATCLI-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATCLI-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6080-WRITE-NEW-FETATCLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6090-WRITE-FETATCLI-DEB.
           WRITE FS-ENRG-ETATCLI-S          FROM WS-BUFFER.
           IF NOT ETATCLI-OK
              DISPLAY 'PROBLEME D''ECRITURE DU FICHIER F-ETATCLI-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATCLI-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6090-WRITE-FETATCLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6100-WRITE-NEW-FETATANO-DEB.
           WRITE FS-ENRG-ETATANO-S          FROM WS-BUFFER AFTER PAGE.
           IF NOT ETATANO-OK
              DISPLAY 'PROBLEME SAUT DE PAGE DU FICHIER F-ETATANO-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6100-WRITE-NEW-FETATANO-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6110-WRITE-FETATANO-DEB.
           WRITE FS-ENRG-ETATANO-S          FROM WS-BUFFER.
           IF NOT ETATANO-OK
              DISPLAY 'PROBLEME D''ECRITURE DU FICHIER F-ETATANO-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6110-WRITE-FETATANO-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6120-CLOSE-FCPTE-DEB.
           CLOSE F-CPTE-E.
           IF NOT CPTE-OK
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER F-CPTE-E'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-CPTE-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6120-CLOSE-FCPTE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6130-CLOSE-FETATCLI-DEB.
           CLOSE F-ETATCLI-S.
           IF NOT ETATCLI-OK
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER F-ETATCLI-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATCLI-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6130-CLOSE-FETATCLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6140-CLOSE-FETATANO-DEB.
           CLOSE F-ETATANO-S.
           IF NOT ETATANO-OK
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER F-ETATANO-S'
              DISPLAY 'VEROUILLE TA SESSION 13032025' WS-FS-ETATANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6140-CLOSE-FETATANO-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6150-POINTER-PRI-CPTE-DEB.
           MOVE WS-DEM-CPT-DEB              TO FS-CPTE-CPTE.
           START F-CPTE-E
              KEY >= FS-CPTE-CPTE
           END-START.
           IF NOT CPTE-OK AND NOT NOT-ENRG-CPTE
              DISPLAY 'PROBLEME DE POSITIONNEMENT SUR F-CPTE-E'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-CPTE-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6150-POINTER-PRI-CPTE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6160-POINTER-SEC-NOM-DEB.
           MOVE WS-DEM-CLI-DEB              TO FS-CPTE-NOM.
           START F-CPTE-E
              KEY >= FS-CPTE-NOM
           END-START.
           IF NOT CPTE-OK AND NOT NOT-ENRG-CPTE
              DISPLAY 'PROBLEME DE POSITIONNEMENT SUR F-CPTE-E'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-CPTE-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6160-POINTER-SEC-NOM-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *
       7020-FIRST-NAME-DEB.
           MOVE WS-CPTE-NOM                 TO WS-FIRST-NAME.
       7020-FIRST-NAME-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7030-IN-TRT-DEM-DEB.
           ADD 1                            TO WS-CPT-DEM.
           MOVE SPACE                       TO WS-MSG-ERROR.
       7030-IN-TRT-DEM-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7040-CLE-PRI-TRT-A-DEB.
           MOVE WS-DEM-CPT-DEB              TO FS-CPTE-CPTE.
       7040-CLE-PRI-TRT-A-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7050-ERR-02-DEB.
           MOVE 2                           TO WS-CODE-ERROR.
           MOVE 'INFORMATIONS MANQUANTES'   TO WS-MSG-ERROR.
       7050-ERR-02-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7060-ERR-03-DEB.
           MOVE 3                           TO WS-CODE-ERROR.
           MOVE 'MAUVAIS FORMAT POUR LES BORNES'
                                            TO WS-MSG-ERROR.
       7060-ERR-03-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7070-ERR-04-DEB.
           MOVE 4                           TO WS-CODE-ERROR.
           MOVE 'BORNE INF SUPERIEUR A LA BORNE SUP'
                                            TO WS-MSG-ERROR.
       7070-ERR-04-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7080-ERR-05-DEB.
           MOVE 5                           TO WS-CODE-ERROR.
           MOVE 'CLE DE RECHERCHE HORS DES LIMITES DU FICHIER'
                                            TO WS-MSG-ERROR.
       7080-ERR-05-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7090-INIT-CPT-ENRG-VIDE-DEB.
           MOVE 0                           TO WS-CPT-ENRG-VIDE.
       7090-INIT-CPT-ENRG-VIDE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7100-CLE-SEC-TRT-B-DEB.
           MOVE WS-DEM-CLI-DEB              TO FS-CPTE-NOM.
       7100-CLE-SEC-TRT-B-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7110-GEST-ENT-CPTE-DEB.
           MOVE WS-DEM-NOM                  TO WS-LETAT-NOMD-ED.
           MOVE WS-CPT-DEM                  TO WS-LETAT-NUM-ED.
           MOVE 1                           TO WS-LETAT-PAGE-ED.
           MOVE 'NUMERO DE COMPTE'          TO WS-LETAT-TYPE-ED.
           MOVE WS-DEM-CPT-DEB              TO WS-LETAT-REFDEB-ED.
           MOVE WS-DEM-CPT-FIN              TO WS-LETAT-REFFIN-ED.
       7110-GEST-ENT-CPTE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7120-GEST-ENT-NOM-DEB.
           MOVE WS-DEM-NOM                  TO WS-LETAT-NOMD-ED.
           MOVE WS-CPT-DEM                  TO WS-LETAT-NUM-ED.
           MOVE 1                           TO WS-LETAT-PAGE-ED.
           MOVE 'NOM DU CLIENT'             TO WS-LETAT-TYPE-ED.
           MOVE WS-DEM-CLI-DEB              TO WS-LETAT-REFDEB-ED.
           MOVE WS-DEM-CLI-FIN              TO WS-LETAT-REFFIN-ED.
       7120-GEST-ENT-NOM-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7130-GEST-LG-ETATCLI-DEB.
           MOVE WS-CPTE-CPTE                TO WS-LETAT-NUMCPT-ED.
           MOVE WS-CPTE-DCREA-SS            TO WS-LETAT-DCREA-SS-ED.
           MOVE WS-CPTE-DCREA-AA            TO WS-LETAT-DCREA-AA-ED.
           MOVE WS-CPTE-DCREA-MM            TO WS-LETAT-DCREA-MM-ED.
           MOVE WS-CPTE-DCREA-JJ            TO WS-LETAT-DCREA-JJ-ED.
           MOVE WS-CPTE-DMAJ-SS             TO WS-LETAT-DMAJ-SS-ED.
           MOVE WS-CPTE-DMAJ-AA             TO WS-LETAT-DMAJ-AA-ED.
           MOVE WS-CPTE-DMAJ-MM             TO WS-LETAT-DMAJ-MM-ED.
           MOVE WS-CPTE-DMAJ-JJ             TO WS-LETAT-DMAJ-JJ-ED.
           MOVE WS-CPTE-SOLDE               TO WS-LETAT-SOLDE-ED.
           MOVE WS-CPTE-NOM                 TO WS-LETAT-NOMC-ED.
           ADD 1                            TO WS-CPT-ENRG-VIDE.
       7130-GEST-LG-ETATCLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7140-GEST-ANO-DEB.
           MOVE 1                           TO WS-LANO-NUM-ED.
           MOVE 'TYPE DE DEMANDE INCORRECT' TO WS-LANO-TYP-ED.
           MOVE WS-ENRG-DEMANDE                  TO WS-LANO-ENR-ED.
           ADD 1                            TO WS-CPT-ERR.
       7140-GEST-ANO-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7150-GEST-ERR-DEB.
           MOVE WS-ENRG-DEMANDE                  TO WS-LANO-ENR-ED.
           MOVE WS-CODE-ERROR               TO WS-LANO-NUM-ED.
           MOVE WS-MSG-ERROR                TO WS-LANO-TYP-ED.
           ADD 1                            TO WS-CPT-ERR.
       7150-GEST-ERR-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7160-NO-ERR-DEB.
           MOVE 0                         TO WS-CODE-ERROR.
       7160-NO-ERR-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *---------------------------------------------------------------*
      *
       8000-EDIT-ENT-FETATCLI-DEB.
           MOVE WS-LETAT-TIRET              TO WS-BUFFER.
           PERFORM 6080-WRITE-NEW-FETATCLI-DEB
              THRU 6080-WRITE-NEW-FETATCLI-FIN.
      *
           MOVE WS-LETAT-ENTETE             TO WS-BUFFER.
           PERFORM 6090-WRITE-FETATCLI-DEB
              THRU 6090-WRITE-FETATCLI-FIN.
      *
           MOVE WS-LETAT-BLANC              TO WS-BUFFER.
           PERFORM 6090-WRITE-FETATCLI-DEB
              THRU 6090-WRITE-FETATCLI-FIN.
      *
           MOVE WS-LETAT-TITRE              TO WS-BUFFER.
           PERFORM 6090-WRITE-FETATCLI-DEB
              THRU 6090-WRITE-FETATCLI-FIN.
      *
           MOVE WS-LETAT-BLANC              TO WS-BUFFER.
           PERFORM 6090-WRITE-FETATCLI-DEB
              THRU 6090-WRITE-FETATCLI-FIN.
      *
           MOVE WS-LETAT-REFDEB             TO WS-BUFFER.
           PERFORM 6090-WRITE-FETATCLI-DEB
              THRU 6090-WRITE-FETATCLI-FIN.
      *
           MOVE WS-LETAT-REFFIN             TO WS-BUFFER.
           PERFORM 6090-WRITE-FETATCLI-DEB
              THRU 6090-WRITE-FETATCLI-FIN.
      *
           MOVE WS-LETAT-BLANC              TO WS-BUFFER.
           PERFORM 6090-WRITE-FETATCLI-DEB
              THRU 6090-WRITE-FETATCLI-FIN.
      *
           MOVE WS-LETAT-INTITULE           TO WS-BUFFER.
           PERFORM 6090-WRITE-FETATCLI-DEB
              THRU 6090-WRITE-FETATCLI-FIN.
      *
           MOVE WS-LETAT-BLANC              TO WS-BUFFER.
           PERFORM 6090-WRITE-FETATCLI-DEB
              THRU 6090-WRITE-FETATCLI-FIN.
       8000-EDIT-ENT-FETATCLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8010-EDIT-LG-FETATCLI-DEB.
           MOVE WS-LETAT-DETAIL             TO WS-BUFFER.
           PERFORM 6090-WRITE-FETATCLI-DEB
              THRU 6090-WRITE-FETATCLI-FIN.
       8010-EDIT-LG-FETATCLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8020-EDIT-0-CPTE-DEB.
           MOVE '| AUCUN COMPTE TROUVE
      -     '                      |'
                                            TO WS-BUFFER.
           PERFORM 6090-WRITE-FETATCLI-DEB
              THRU 6090-WRITE-FETATCLI-FIN.
       8020-EDIT-0-CPTE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8030-EDIT-0-NOM-DEB.
           MOVE '| AUCUN CLIENT TROUVE
      -     '                      |'
                                            TO WS-BUFFER.
           PERFORM 6090-WRITE-FETATCLI-DEB
              THRU 6090-WRITE-FETATCLI-FIN.
       8030-EDIT-0-NOM-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8040-EDIT-PP-FETATCLI-DEB.
           MOVE WS-LETAT-TIRET              TO WS-BUFFER.
           PERFORM 6090-WRITE-FETATCLI-DEB
              THRU 6090-WRITE-FETATCLI-FIN.
       8040-EDIT-PP-FETATCLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8050-EDIT-ENT-FETATANO-DEB.
           MOVE WS-LANO-ASTER               TO WS-BUFFER.
           PERFORM 6100-WRITE-NEW-FETATANO-DEB
              THRU 6100-WRITE-NEW-FETATANO-FIN.
      *
           MOVE WS-LANO-TITRE               TO WS-BUFFER.
           PERFORM 6110-WRITE-FETATANO-DEB
              THRU 6110-WRITE-FETATANO-FIN.
      *
           MOVE WS-LANO-ASTER               TO WS-BUFFER.
           PERFORM 6110-WRITE-FETATANO-DEB
              THRU 6110-WRITE-FETATANO-FIN.
       8050-EDIT-ENT-FETATANO-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8060-EDIT-LG-FETATANO-DEB.
           IF NOT CPT-ERR-NULL
              MOVE WS-LANO-INTERL           TO WS-BUFFER
              PERFORM 6110-WRITE-FETATANO-DEB
                 THRU 6110-WRITE-FETATANO-FIN
           END-IF.
      *
           MOVE WS-LANO-ERREUR              TO WS-BUFFER.
           PERFORM 6110-WRITE-FETATANO-DEB
              THRU 6110-WRITE-FETATANO-FIN.
      *
           MOVE WS-LANO-ENR1                TO WS-BUFFER.
           PERFORM 6110-WRITE-FETATANO-DEB
              THRU 6110-WRITE-FETATANO-FIN.
      *
           MOVE WS-LANO-ENR2                TO WS-BUFFER.
           PERFORM 6110-WRITE-FETATANO-DEB
              THRU 6110-WRITE-FETATANO-FIN.
       8060-EDIT-LG-FETATANO-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8070-EDIT-PP-FETATANO-DEB.
           MOVE WS-LANO-ASTER               TO WS-BUFFER.
           PERFORM 6110-WRITE-FETATANO-DEB
              THRU 6110-WRITE-FETATANO-FIN.
       8070-EDIT-PP-FETATANO-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8999-STATISTIQUES-DEB.
           DISPLAY WS-LCRE-ASTER.
           DISPLAY WS-LCRE-TITRE.
           DISPLAY WS-LCRE-ASTER.
           MOVE 'NOMBRE TOTAL DE DEMANDES'  TO WS-LCRE-DET-LIB-ED.
           MOVE WS-CPT-DEM                  TO WS-LCRE-DET-TOT-ED.
           DISPLAY WS-LCRE-DETAIL.
           MOVE 'NOMBRE DE DEMANDES ERRONEES'
                                            TO WS-LCRE-DET-LIB-ED.
           MOVE WS-CPT-ERR                  TO WS-LCRE-DET-TOT-ED.
           DISPLAY WS-LCRE-DETAIL.
           DISPLAY WS-LCRE-ASTER.
       8999-STATISTIQUES-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   9XXX-  : ORDRES DE MANIPULATION DES SOUS-PROGRAMMES         *
      *---------------------------------------------------------------*
      *
      *9000-APPEL-SP-DEB.
      *
      *9000-APPEL-SP-FIN.
      *     EXIT.
      *
      *---------------------------------------------------------------*
      *   9999-  : PROTECTION FIN DE PROGRAMME                        *
      *---------------------------------------------------------------*
      *
       9999-FIN-PROGRAMME-DEB.
           DISPLAY '*============================================*'.
           DISPLAY '*     FIN NORMALE DU PROGRAMME ARIO526       *'.
           DISPLAY '*============================================*'.
       9999-FIN-PROGRAMME-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       9999-ERREUR-PROGRAMME-DEB.
           DISPLAY '*============================================*'.
           DISPLAY '*        UNE ANOMALIE A ETE DETECTEE         *'.
           DISPLAY '*     FIN ANORMALE DU PROGRAMME ARIO526      *'.
           DISPLAY '*============================================*'.
           MOVE 12 TO RETURN-CODE.
       9999-ERREUR-PROGRAMME-FIN.
           STOP RUN.
