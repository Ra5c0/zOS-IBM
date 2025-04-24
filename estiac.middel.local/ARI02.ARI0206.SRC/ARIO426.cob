      * CBL    MAP,SSRANGE,LIST,NOOFFSET
      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIO426                                   *
      *  NOM DU REDACTEUR : GIGON                                     *
      *  SOCIETE          : ESTIAC INSTITUT                           *
      *  DATE DE CREATION : 10/03/2025                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      *  METTRE A JOUR LE FICHIER DES COMPTES CLIENTS (F-CPTE-ES) A   *
      *  PARTIR DES MOUVEMENTS BANCAIRES (F-MVTS-E).                  *
      *  EDITER DES FICHIERS D'IMPRESSIONS : ETAT DES CLIENTS ET      *
      *  ETAT DES ANOMALIES.                                          *
      *  ECRIRE UN COMPTE RENDU D'EXECUTION DANS LA SYSOUT.           *
      *---------------------------------------------------------------*
      *--               HISTORIQUE DES MODIFICATIONS                --*
      *---------------------------------------------------------------*
      * DATE  MODIF   §          NATURE DE LA MODIFICATION           *
      *---------------------------------------------------------------*
      * 10/03/2025    §  CREATION DU FICHIER                         *
      *               §                                              *
      *===============================================================*
      *
      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID.      ARIO426
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
      *                     F-MVTS-E: FICHIER DES MOUVEMENTS
      *                     -------------------------------------------
           SELECT F-MVTS-E                  ASSIGN TO INP001
                  FILE STATUS               IS WS-FS-MVTS-E.
      *                     -------------------------------------------
      *                     F-MVTS-E: FICHIER DES COMPTES CLIENTS
      *                     -------------------------------------------
           SELECT F-CPTE-ES                 ASSIGN TO IO001
                  ORGANIZATION              IS INDEXED
                  ACCESS MODE               IS RANDOM
                  RECORD KEY                IS KEY-CPTES
                  FILE STATUS               IS WS-FS-CPTE-ES.
      *                     -------------------------------------------
      *                     F-MVTS-E: FICHIER ETAT DES CLIENTS
      *                     -------------------------------------------
           SELECT F-ETATCLI-S               ASSIGN TO ETATCLI
                  FILE STATUS               IS WS-FS-ETATCLI-S.
      *                     -------------------------------------------
      *                     F-MVTS-E: FICHIER ETAT DES ANOMALIES
      *                     -------------------------------------------
           SELECT F-ETATANO-S               ASSIGN TO ETATANO
                  FILE STATUS               IS WS-FS-ETATANO-S.
      *                     -------------------------------------------
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
       FD  F-MVTS-E
           RECORDING MODE IS F.
       01  FS-ENRG-MVTS-E                   PIC X(50).
      *
       FD  F-CPTE-ES
           RECORD CONTAINS 50 CHARACTERS.
       01  FS-ENRG-CPTE-ES.
           05  KEY-CPTES                    PIC X(10).
           05  FILLER                       PIC X(40).
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
       01  WS-FS-MVTS-E                     PIC XX.
           88  MVTS-OK                      VALUE '00'.
           88  EOF-MVTS                     VALUE '10'.
      *
       01  WS-FS-CPTE-ES                    PIC XX.
           88  CPTES-OK                     VALUE '00'.
           88  NOT-ENRG-CPTES               VALUE '23'.
      *
       01  WS-FS-ETATCLI-S                  PIC XX.
           88  ETATCLI-OK                   VALUE '00'.
      *
       01  WS-FS-ETATANO-S                  PIC XX.
           88  ETATANO-OK                   VALUE '00'.
      *
      *------------------- LIGNES D'EDITION --------------------------*
      *
       COPY TP4LEDIT.
      *
      *------------------- ENREGISTREMENT MVTS EN ENTREE -------------*
      *
       COPY TP4MVTS.
      *
      *------------------- ENREGISTREMENT CPTE EN ENTREE -------------*
      *
       COPY TP4CPTES.
      *
      *------------------- VARIABLES DE TRAITEMENT -------------------*
      *
       01  WS-DATE-TMP.
           05  WS-SS-TMP                    PIC 9(2).
           05  WS-AA-TMP                    PIC 9(2).
           05  WS-MM-TMP                    PIC 9(2).
           05  WS-JJ-TMP                    PIC 9(2).
      *
       01  WS-CPAGE                         PIC S9(4) COMP.
      *
       01  WS-LETAT-CLOSE                   PIC X(18).
      *
       01  WS-CMVT-VALID                    PIC S9(4) COMP.
           88  CMVT-VALID-NULL              VALUE 0.
           88  CMVT-VALID-FIVE              VALUE 5.
      *
       01  WS-CRET                          PIC S9(4) COMP VALUE 0.
      *
       01  WS-CCB                           PIC S9(4) COMP VALUE 0.
      *
       01  WS-CDEP                          PIC S9(4) COMP VALUE 0.
      *
       01  WS-CERR                          PIC S9(4) COMP VALUE 0.
           88  FIRST-ANO                    VALUE 1.
      *
       01  WS-CK                            PIC S9(4) COMP VALUE 0.
      *
       01  WS-LANO-TOT                      PIC 9(11)V99 COMP-3
                                            VALUE 0.
           88  TOT-ANO-NULL                 VALUE 0.
      *
       01  WS-TOT-CPT                       PIC S9(4) COMP VALUE 0.
      *
       01  WS-NEW-CPT                       PIC S9(4) COMP VALUE 0.
      *
       01  WS-STD-CPT                       PIC S9(4) COMP VALUE 0.
      *
       01  WS-K-CPT                         PIC S9(4) COMP VALUE 0.
      *
       01  WS-CDB                           PIC 9(11)V99 COMP-3.
           88  DB-NULL                      VALUE 0.
      *
       01  WS-CCR                           PIC 9(11)V99 COMP-3.
           88  CR-NULL                      VALUE 0.
      *
       01  WS-BUFFER                        PIC X(80).
      *
       01  WS-LETAT-SOLD-TMP                PIC S9(11)V99 COMP-3.
      *
       01  WS-CLOTURE                       PIC 9.
           88  CLOTURE-TRUE                 VALUE 1.
           88  CLOTURE-FALSE                VALUE 0.
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
      *
      *---------------------------------------------------------------*
      *                      TRAITEMENT PRINCIPAL                     *
      *                      ====================                     *
      *---------------------------------------------------------------*
      *
       0000-TRT-PRINCIPAL-DEB.
      *
      *---------------------------------------------------------------*
      * PREPARATION DU TRAITEMENT                                     *
      *---------------------------------------------------------------*
      *
           PERFORM 6000-OPEN-FMVTS-DEB
              THRU 6000-OPEN-FMVTS-FIN.
      *
           PERFORM 6010-OPEN-FCPTES-DEB
              THRU 6010-OPEN-FCPTES-FIN.
      *
           PERFORM 6020-OPEN-FETATCLI-DEB
              THRU 6020-OPEN-FETATCLI-FIN.
      *
           PERFORM 6030-OPEN-FETATANO-DEB
              THRU 6030-OPEN-FETATANO-FIN.
      *
           PERFORM 6040-READ-FMVTS-DEB
              THRU 6040-READ-FMVTS-FIN.
      *
           IF EOF-MVTS
              PERFORM 8000-MVTS-EMPTY-DEB
                 THRU 8000-MVTS-EMPTY-FIN
           END-IF.
      *
           PERFORM 7000-INIT-DATE-DEB
              THRU 7000-INIT-DATE-FIN.
      *
           PERFORM 8010-EDIT-PG-ETATCLI-DEB
              THRU 8010-EDIT-PG-ETATCLI-FIN.
      *
           PERFORM 8020-EDIT-PG-ETATANO-DEB
              THRU 8020-EDIT-PG-ETATANO-FIN.
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT                                    *
      *---------------------------------------------------------------*
      *
           PERFORM 1000-TRT-COMPTE-DEB
              THRU 1000-TRT-COMPTE-FIN
             UNTIL EOF-MVTS.
      *
      *---------------------------------------------------------------*
      * FIN DU TRAITEMENT                                             *
      *---------------------------------------------------------------*
      *
           PERFORM 7180-GST-CRE-DEB
              THRU 7180-GST-CRE-FIN.
      *
           IF NOT TOT-ANO-NULL
              PERFORM 8080-EDIT-PP-ETATANO-DEB
                 THRU 8080-EDIT-PP-ETATANO-FIN
           END-IF.
      *
           PERFORM 8999-STATISTIQUES-CRE-DEB
              THRU 8999-STATISTIQUES-CRE-FIN.
      *
           PERFORM 6130-CLOSE-FMVTS-DEB
              THRU 6130-CLOSE-FMVTS-FIN.
      *
           PERFORM 6140-CLOSE-FCPTES-DEB
              THRU 6140-CLOSE-FCPTES-FIN.
      *
           PERFORM 6150-CLOSE-FETATCLI-DEB
              THRU 6150-CLOSE-FETATCLI-FIN.
      *
           PERFORM 6160-CLOSE-FETATANO-DEB
              THRU 6160-CLOSE-FETATANO-FIN.
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
      *                      TRAITEMENT COMPTE                        *
      *                      =================                        *
      *---------------------------------------------------------------*
      *
       1000-TRT-COMPTE-DEB.
      *
      *---------------------------------------------------------------*
      * PREPARATION DU TRAITEMENT                                     *
      *---------------------------------------------------------------*
      *
           PERFORM 6050-READ-FCPTES-DEB
              THRU 6050-READ-FCPTES-FIN.
      *
           PERFORM 7010-GST-IN-TRT-CPTE-DEB
              THRU 7010-GST-IN-TRT-CPTE-FIN.
      *
      *---------------------------------------------------------------*
      * APPEL DU COMPOSANT SUIVANT                                    *
      *---------------------------------------------------------------*
      *
           IF NOT NOT-ENRG-CPTES
              PERFORM 2000-TRT-CPTE-EXIST-DEB
                 THRU 2000-TRT-CPTE-EXIST-FIN
           ELSE
              PERFORM 2010-TRT-CPTE-INEXIST-DEB
                 THRU 2010-TRT-CPTE-INEXIST-FIN
           END-IF.
      *
      *---------------------------------------------------------------*
      * FIN DU TRAITEMENT                                             *
      *---------------------------------------------------------------*
      *
           IF NOT CMVT-VALID-NULL
              PERFORM 7170-GST-PP-ETATCLI-DEB
                 THRU 7170-GST-PP-ETATCLI-FIN
      *
              PERFORM 8070-EDIT-PP-ETATCLI-DEB
                 THRU 8070-EDIT-PP-ETATCLI-FIN
           END-IF.
      *
       1000-TRT-COMPTE-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *                 TRAITEMENT COMPTE EXISTANT                    *
      *                 ==========================                    *
      *---------------------------------------------------------------*
      *
       2000-TRT-CPTE-EXIST-DEB.
      *
      *------------ ENTREE
      *
           PERFORM 7020-GST-IN-CPTE-YES-DEB
              THRU 7020-GST-IN-CPTE-YES-FIN.
      *
      *------------ APPEL
      *
           PERFORM 3000-TRT-MVT-CPTE-EXIST-DEB
              THRU 3000-TRT-MVT-CPTE-EXIST-FIN
             UNTIL (WS-MVTS-CPTE NOT = WS-CPTES-CPTE) OR
                   EOF-MVTS.
      *
      *------------ SORTIE
      *
           IF CLOTURE-FALSE
              PERFORM 7140-GST-OUT-CPTE-STD-DEB
                 THRU 7140-GST-OUT-CPTE-STD-FIN
      *
              PERFORM 6110-REWRITE-FCPTES-DEB
                 THRU 6110-REWRITE-FCPTES-FIN
           ELSE
              PERFORM 7150-GST-OUT-CPTE-K-DEB
                 THRU 7150-GST-OUT-CPTE-K-FIN
      *
              PERFORM 6120-DELETE-FCPTES-DEB
                 THRU 6120-DELETE-FCPTES-FIN
           END-IF.
      *
       2000-TRT-CPTE-EXIST-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *                 TRAITEMENT COMPTE INEXISTANT                  *
      *                 ============================                  *
      *---------------------------------------------------------------*
      *
       2010-TRT-CPTE-INEXIST-DEB.
      *
      *------------ ENTREE
      *
           PERFORM 7050-GST-IN-CPTE-NO-DEB
              THRU 7050-GST-IN-CPTE-NO-FIN.
      *
      *------------ APPEL
      *
           PERFORM 3010-TRT-MVT-CPTE-INEXIST-DEB
              THRU 3010-TRT-MVT-CPTE-INEXIST-FIN
             UNTIL (WS-MVTS-CPTE NOT = WS-CPTES-CPTE) OR
                   EOF-MVTS.
      *
      *------------ SORTIE
      *
           IF NOT (DB-NULL AND CR-NULL) AND CLOTURE-FALSE
              PERFORM 7160-GST-OUT-CPTE-NEW-DEB
                 THRU 7160-GST-OUT-CPTE-NEW-FIN
      *
              PERFORM 6060-WRITE-FCPTES-DEB
                 THRU 6060-WRITE-FCPTES-FIN
           ELSE
              PERFORM 7190-CLR-F-DEB
                 THRU 7190-CLR-F-FIN
           END-IF.
      *
       2010-TRT-CPTE-INEXIST-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *            TRAITEMENT MOUVEMENT COMPTE EXISTANT               *
      *            ====================================               *
      *---------------------------------------------------------------*
      *
       3000-TRT-MVT-CPTE-EXIST-DEB.
      *
      *------------ ENTREE
      *
           PERFORM 7030-INIT-DB-CR-DEB
              THRU 7030-INIT-DB-CR-FIN.
      *
           IF (RETRAIT OR CB OR DEPOT)
              IF CMVT-VALID-FIVE
                 PERFORM 7130-GST-PP-TMP-ETATCLI-DEB
                    THRU 7130-GST-PP-TMP-ETATCLI-FIN
      *
                 PERFORM 8070-EDIT-PP-ETATCLI-DEB
                    THRU 8070-EDIT-PP-ETATCLI-FIN
              END-IF
              IF CMVT-VALID-NULL
                 PERFORM 7040-GST-ENT-ETATCLI-DEB
                    THRU 7040-GST-ENT-ETATCLI-FIN
      *
                 PERFORM 8030-EDIT-ENT-ETATCLI-DEB
                    THRU 8030-EDIT-ENT-ETATCLI-FIN
              END-IF
           END-IF.
      *
      *------------ APPEL
      *
           EVALUATE TRUE
              WHEN RETRAIT
                   PERFORM 4000-TRT-RETRAIT-DEB
                      THRU 4000-TRT-RETRAIT-FIN
              WHEN CB
                   PERFORM 4010-TRT-CB-DEB
                      THRU 4010-TRT-CB-FIN
              WHEN DEPOT
                   PERFORM 4020-TRT-DEPOT-DEB
                      THRU 4020-TRT-DEPOT-FIN
              WHEN CLOTURE
                   PERFORM 4030-TRT-CLOTURE-DEB
                      THRU 4030-TRT-CLOTURE-FIN
              WHEN OTHER
                   PERFORM 4040-TRT-ANO-DEB
                      THRU 4040-TRT-ANO-FIN
           END-EVALUATE.
      *
      *------------ SORTIE
      *
           PERFORM 6040-READ-FMVTS-DEB
              THRU 6040-READ-FMVTS-FIN.
      *
       3000-TRT-MVT-CPTE-EXIST-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *            TRAITEMENT MOUVEMENT COMPTE INEXISTANT             *
      *            ======================================             *
      *---------------------------------------------------------------*
      *
       3010-TRT-MVT-CPTE-INEXIST-DEB.
      *
      *------------ ENTREE
      *
           PERFORM 7030-INIT-DB-CR-DEB
              THRU 7030-INIT-DB-CR-FIN.
      *
           IF (RETRAIT OR CB OR DEPOT)
              IF DB-NULL AND CR-NULL
                 PERFORM 7060-DATE-1ST-MVT-DEB
                    THRU 7060-DATE-1ST-MVT-FIN
              END-IF
      *
              IF CMVT-VALID-FIVE
                 PERFORM 7130-GST-PP-TMP-ETATCLI-DEB
                    THRU 7130-GST-PP-TMP-ETATCLI-FIN
      *
                 PERFORM 8070-EDIT-PP-ETATCLI-DEB
                    THRU 8070-EDIT-PP-ETATCLI-FIN
              END-IF
      *
              IF CMVT-VALID-NULL
                 PERFORM 7070-GST-ENT-ETATCLI-NEW-DEB
                    THRU 7070-GST-ENT-ETATCLI-NEW-FIN
      *
                 PERFORM 8030-EDIT-ENT-ETATCLI-DEB
                    THRU 8030-EDIT-ENT-ETATCLI-FIN
              END-IF
           END-IF.
      *
      *------------ APPEL
      *
           EVALUATE TRUE
              WHEN RETRAIT
                   PERFORM 4000-TRT-RETRAIT-DEB
                      THRU 4000-TRT-RETRAIT-FIN
              WHEN CB
                   PERFORM 4010-TRT-CB-DEB
                      THRU 4010-TRT-CB-FIN
              WHEN DEPOT
                   PERFORM 4020-TRT-DEPOT-DEB
                      THRU 4020-TRT-DEPOT-FIN
              WHEN CLOTURE
                   PERFORM 4030-TRT-CLOTURE-DEB
                      THRU 4030-TRT-CLOTURE-FIN
              WHEN OTHER
                   PERFORM 4040-TRT-ANO-DEB
                      THRU 4040-TRT-ANO-FIN
           END-EVALUATE.
      *
      *------------ SORTIE
      *
           PERFORM 6040-READ-FMVTS-DEB
              THRU 6040-READ-FMVTS-FIN.
      *
       3010-TRT-MVT-CPTE-INEXIST-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *         TRAITEMENT RETRAIT/CB/DEPOT/CLOTURE/ANOMALIE          *
      *         ============================================          *
      *---------------------------------------------------------------*
      *
       4000-TRT-RETRAIT-DEB.
           PERFORM 7080-GST-RETRAIT-DEB
              THRU 7080-GST-RETRAIT-FIN.
      *
           PERFORM 8050-EDIT-LG-ETATCLI-DEB
              THRU 8050-EDIT-LG-ETATCLI-FIN.
       4000-TRT-RETRAIT-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       4010-TRT-CB-DEB.
           PERFORM 7090-GST-CARTE-DEB
              THRU 7090-GST-CARTE-FIN.
      *
           PERFORM 8050-EDIT-LG-ETATCLI-DEB
              THRU 8050-EDIT-LG-ETATCLI-FIN.
       4010-TRT-CB-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       4020-TRT-DEPOT-DEB.
           PERFORM 7100-GST-DEPOT-DEB
              THRU 7100-GST-DEPOT-FIN.
      *
           PERFORM 8050-EDIT-LG-ETATCLI-DEB
              THRU 8050-EDIT-LG-ETATCLI-FIN.
       4020-TRT-DEPOT-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       4030-TRT-CLOTURE-DEB.
           PERFORM 7120-GST-CLOTURE-DEB
              THRU 7120-GST-CLOTURE-FIN.
       4030-TRT-CLOTURE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       4040-TRT-ANO-DEB.
           PERFORM 7110-GST-ANO-DEB
              THRU 7110-GST-ANO-FIN.
      *
           IF FIRST-ANO
              PERFORM 8040-EDIT-ENT-ETATANO-DEB
                 THRU 8040-EDIT-ENT-ETATANO-FIN
           END-IF.
      *
           PERFORM 8060-EDIT-LG-ETATANO-DEB
              THRU 8060-EDIT-LG-ETATANO-FIN.
       4040-TRT-ANO-FIN.
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
       6000-OPEN-FMVTS-DEB.
           OPEN INPUT F-MVTS-E.
           IF NOT MVTS-OK
              DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER F-MVTS-E'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-MVTS-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6000-OPEN-FMVTS-FIN.
            EXIT.
      *---------------------------------------------------------------*
      *
       6010-OPEN-FCPTES-DEB.
           OPEN I-O F-CPTE-ES.
           IF NOT CPTES-OK
              DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER F-CPTE-ES'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-CPTE-ES
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6010-OPEN-FCPTES-FIN.
            EXIT.
      *---------------------------------------------------------------*
      *
       6020-OPEN-FETATCLI-DEB.
           OPEN OUTPUT F-ETATCLI-S.
           IF NOT ETATCLI-OK
              DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER F-ETATCLI-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATCLI-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6020-OPEN-FETATCLI-FIN.
            EXIT.
      *---------------------------------------------------------------*
      *
       6030-OPEN-FETATANO-DEB.
           OPEN OUTPUT F-ETATANO-S.
           IF NOT ETATANO-OK
              DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER F-ETATANO-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6030-OPEN-FETATANO-FIN.
            EXIT.
      *---------------------------------------------------------------*
      *
       6040-READ-FMVTS-DEB.
           READ F-MVTS-E                    INTO WS-ENRG-F-MVTS.
           IF NOT (MVTS-OK OR EOF-MVTS)
              DISPLAY 'PROBLEME DE LECTURE DU FICHIER F-MVTS-E'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-MVTS-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6040-READ-FMVTS-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6050-READ-FCPTES-DEB.
           MOVE WS-MVTS-CPTE                TO KEY-CPTES.
           READ F-CPTE-ES INTO WS-ENRG-F-CPTES.
           IF NOT (CPTES-OK OR NOT-ENRG-CPTES)
              DISPLAY 'PROBLEME DE LECTURE DU FICHIER F-CPTE-ES'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-CPTE-ES
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6050-READ-FCPTES-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6060-WRITE-FCPTES-DEB.
           WRITE FS-ENRG-CPTE-ES            FROM WS-ENRG-F-CPTES.
           IF NOT CPTES-OK
              DISPLAY 'PROBLEME D''ECRITURE DU FICHIER F-CPTE-ES'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-CPTE-ES
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6060-WRITE-FCPTES-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6070-WRITE-NEW-FETATCLI-DEB.
           WRITE FS-ENRG-ETATCLI-S          FROM WS-BUFFER AFTER PAGE.
           IF NOT ETATCLI-OK
              DISPLAY 'PROBLEME DE SAUT DE PAGE DU FICHIER F-ETATCLI-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATCLI-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6070-WRITE-NEW-FETATCLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6080-WRITE-FETATCLI-DEB.
           WRITE FS-ENRG-ETATCLI-S          FROM WS-BUFFER.
           IF NOT ETATCLI-OK
              DISPLAY 'PROBLEME D''ECRITURE DU FICHIER F-ETATCLI-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATCLI-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6080-WRITE-FETATCLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6090-WRITE-NEW-FETATANO-DEB.
           WRITE FS-ENRG-ETATANO-S          FROM WS-BUFFER AFTER PAGE.
           IF NOT ETATANO-OK
              DISPLAY 'PROBLEME SAUT DE PAGE DU FICHIER F-ETATANO-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6090-WRITE-NEW-FETATANO-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6100-WRITE-FETATANO-DEB.
           WRITE FS-ENRG-ETATANO-S          FROM WS-BUFFER.
           IF NOT ETATANO-OK
              DISPLAY 'PROBLEME D''ECRITURE DU FICHIER F-ETATANO-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6100-WRITE-FETATANO-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6110-REWRITE-FCPTES-DEB.
           REWRITE FS-ENRG-CPTE-ES          FROM WS-ENRG-F-CPTES.
           IF NOT CPTES-OK
              DISPLAY 'PROBLEME DE RE-ECRITURE DU FICHIER F-CPTE-ES'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-CPTE-ES
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6110-REWRITE-FCPTES-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6120-DELETE-FCPTES-DEB.
           MOVE WS-CPTES-CPTE                TO KEY-CPTES.
           DELETE F-CPTE-ES.
           IF NOT CPTES-OK
              DISPLAY 'PROBLEME DE SUPPRESSION DU FICHIER F-CPTE-ES'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-CPTE-ES
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6120-DELETE-FCPTES-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6130-CLOSE-FMVTS-DEB.
           CLOSE F-MVTS-E.
           IF NOT MVTS-OK
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER F-MVTS-E'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-MVTS-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6130-CLOSE-FMVTS-FIN.
            EXIT.
      *---------------------------------------------------------------*
      *
       6140-CLOSE-FCPTES-DEB.
           CLOSE F-CPTE-ES.
           IF NOT CPTES-OK
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER F-CPTE-ES'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-CPTE-ES
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6140-CLOSE-FCPTES-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6150-CLOSE-FETATCLI-DEB.
           CLOSE F-ETATCLI-S.
           IF NOT ETATCLI-OK
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER F-ETATCLI-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATCLI-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6150-CLOSE-FETATCLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6160-CLOSE-FETATANO-DEB.
           CLOSE F-ETATANO-S.
           IF ETATANO-OK
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER F-ETATANO-S'
              DISPLAY 'VEROUILLE TA SESSION 13032025' WS-FS-ETATANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6160-CLOSE-FETATANO-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *
       7000-INIT-DATE-DEB.
           ACCEPT WS-DATE-TMP               FROM DATE YYYYMMDD.
           MOVE WS-SS-TMP                   TO WS-LETAT-SS-ED
                                               WS-L7-SS-ED.
           MOVE WS-AA-TMP                   TO WS-LETAT-AA-ED
                                               WS-L7-AA-ED.
           MOVE WS-MM-TMP                   TO WS-LETAT-MM-ED
                                               WS-L7-MM-ED.
           MOVE WS-JJ-TMP                   TO WS-LETAT-JJ-ED
                                               WS-L7-JJ-ED.
       7000-INIT-DATE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7010-GST-IN-TRT-CPTE-DEB.
           MOVE WS-DATE-TMP                 TO WS-CPTES-DMAJ.
           MOVE 0                           TO WS-CDB
                                               WS-CCR
                                               WS-CMVT-VALID
                                               WS-CPAGE.
       7010-GST-IN-TRT-CPTE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7020-GST-IN-CPTE-YES-DEB.
           ADD 1                            TO WS-TOT-CPT.
           ADD 1                            TO WS-STD-CPT.
           MOVE SPACE                       TO WS-LETAT-CLOSE.
           MOVE WS-LETAT-CLOSE              TO WS-LETAT-CLOSE-ED.
           MOVE WS-CPTES-CPTE               TO WS-LETAT-NUMCPT-ED.
           MOVE WS-CPTES-SOLDE              TO WS-LETAT-SOLD-TMP.
       7020-GST-IN-CPTE-YES-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7030-INIT-DB-CR-DEB.
           MOVE 0                           TO WS-LETAT-OP-DEBIT-ED
                                               WS-LETAT-OP-CREDIT-ED.
       7030-INIT-DB-CR-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7040-GST-ENT-ETATCLI-DEB.
           MOVE 'ANCIEN SOLDE'              TO WS-LETAT-LIB-ED.
           ADD 1                            TO WS-CPAGE.
           MOVE WS-CPAGE                    TO WS-LETAT-PAGE-ED.
           MOVE WS-CPTES-SOLDE              TO WS-LETAT-SOLD-ED.
       7040-GST-ENT-ETATCLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7050-GST-IN-CPTE-NO-DEB.
           ADD 1                            TO WS-TOT-CPT.
           MOVE SPACE                       TO WS-LETAT-CLOSE.
           MOVE WS-LETAT-CLOSE              TO WS-LETAT-CLOSE-ED.
           MOVE WS-MVTS-CPTE                TO WS-CPTES-CPTE
                                               WS-LETAT-NUMCPT-ED.
           MOVE 0                           TO WS-CPTES-SOLDE
                                               WS-LETAT-SOLD-TMP.
       7050-GST-IN-CPTE-NO-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7060-DATE-1ST-MVT-DEB.
           MOVE WS-MVTS-DATE                TO WS-CPTES-DCREA.
       7060-DATE-1ST-MVT-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7070-GST-ENT-ETATCLI-NEW-DEB.
           MOVE 'ANCIEN SOLDE'              TO WS-LETAT-LIB-ED.
           MOVE 'CREATION DE COMPTE'        TO WS-LETAT-OPEN-ED.
           ADD 1                            TO WS-CPAGE.
           MOVE WS-CPAGE                    TO WS-LETAT-PAGE-ED.
           MOVE 0                           TO WS-LETAT-SOLD-ED.
       7070-GST-ENT-ETATCLI-NEW-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7080-GST-RETRAIT-DEB.
           ADD WS-MVTS-MT                   TO WS-CDB.
           ADD 1                            TO WS-CRET.
           MOVE 'RETRAIT DAB'               TO WS-LETAT-OP-LIB-ED.
           MOVE WS-MVTS-SS                  TO WS-LETAT-OP-SS-ED.
           MOVE WS-MVTS-AA                  TO WS-LETAT-OP-AA-ED.
           MOVE WS-MVTS-MM                  TO WS-LETAT-OP-MM-ED.
           MOVE WS-MVTS-JJ                  TO WS-LETAT-OP-JJ-ED.
           MOVE WS-MVTS-MT                  TO WS-LETAT-OP-DEBIT-ED.
           ADD 1                            TO WS-CMVT-VALID.
       7080-GST-RETRAIT-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7090-GST-CARTE-DEB.
           ADD WS-MVTS-MT                   TO WS-CDB.
           ADD 1                            TO WS-CCB.
           MOVE 'CARTE BLEUE'               TO WS-LETAT-OP-LIB-ED.
           MOVE WS-MVTS-SS                  TO WS-LETAT-OP-SS-ED.
           MOVE WS-MVTS-AA                  TO WS-LETAT-OP-AA-ED.
           MOVE WS-MVTS-MM                  TO WS-LETAT-OP-MM-ED.
           MOVE WS-MVTS-JJ                  TO WS-LETAT-OP-JJ-ED.
           MOVE WS-MVTS-MT                  TO WS-LETAT-OP-DEBIT-ED.
           ADD 1                            TO WS-CMVT-VALID.
       7090-GST-CARTE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7100-GST-DEPOT-DEB.
           ADD WS-MVTS-MT                   TO WS-CCR.
           ADD 1                            TO WS-CDEP.
           MOVE 'DEPOT GUICHET'             TO WS-LETAT-OP-LIB-ED.
           MOVE WS-MVTS-SS                  TO WS-LETAT-OP-SS-ED.
           MOVE WS-MVTS-AA                  TO WS-LETAT-OP-AA-ED.
           MOVE WS-MVTS-MM                  TO WS-LETAT-OP-MM-ED.
           MOVE WS-MVTS-JJ                  TO WS-LETAT-OP-JJ-ED.
           MOVE WS-MVTS-MT                  TO WS-LETAT-OP-CREDIT-ED.
           ADD 1                            TO WS-CMVT-VALID.
       7100-GST-DEPOT-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7110-GST-ANO-DEB.
           ADD 1                            TO WS-CERR.
           MOVE WS-MVTS-CPTE                TO WS-LANO-NUMCPT-ED.
           MOVE WS-MVTS-CODE                TO WS-LANO-CODEMVT-ED.
           ADD WS-MVTS-MT                   TO WS-LANO-TOT.
           MOVE WS-MVTS-MT                  TO WS-LANO-MONTANT-ED.
       7110-GST-ANO-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7120-GST-CLOTURE-DEB.
           MOVE 'CLOTURE DE COMPTE'         TO WS-LETAT-CLOSE.
           ADD 1                            TO WS-CK.
           SET CLOTURE-TRUE                 TO TRUE.
       7120-GST-CLOTURE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7130-GST-PP-TMP-ETATCLI-DEB.
           MOVE 'SOUS TOTAL DES OPERATIONS' TO WS-LETAT-TOT-LIB-ED.
           MOVE 'SOLDE INTERMEDIAIRE'       TO WS-LETAT-LIB-ED.
           COMPUTE WS-LETAT-SOLD-TMP =
                   WS-CPTES-SOLDE + WS-CCR - WS-CDB.
           MOVE WS-CDB                      TO WS-LETAT-TOTDB-ED.
           MOVE WS-CCR                      TO WS-LETAT-TOTCR-ED.
           MOVE WS-LETAT-SOLD-TMP           TO WS-LETAT-SOLD-ED.
           MOVE 0                           TO WS-CMVT-VALID.
       7130-GST-PP-TMP-ETATCLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7140-GST-OUT-CPTE-STD-DEB.
           COMPUTE WS-LETAT-SOLD-ED =
                   WS-CPTES-SOLDE + WS-CCR - WS-CDB.
           MOVE WS-LETAT-SOLD-ED            TO WS-CPTES-SOLDE.
       7140-GST-OUT-CPTE-STD-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7150-GST-OUT-CPTE-K-DEB.
           COMPUTE WS-LETAT-SOLD-ED =
                   WS-CPTES-SOLDE + WS-CCR - WS-CDB.
           MOVE WS-LETAT-SOLD-ED            TO WS-CPTES-SOLDE.
           ADD 1                            TO WS-K-CPT.
           SET CLOTURE-FALSE                TO TRUE.
       7150-GST-OUT-CPTE-K-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7160-GST-OUT-CPTE-NEW-DEB.
           COMPUTE WS-LETAT-SOLD-ED =
                   WS-CPTES-SOLDE + WS-CCR - WS-CDB.
           MOVE WS-LETAT-SOLD-ED            TO WS-CPTES-SOLDE.
           ADD 1                            TO WS-NEW-CPT.
       7160-GST-OUT-CPTE-NEW-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7170-GST-PP-ETATCLI-DEB.
           MOVE 'TOTAL DES OPERATIONS'      TO WS-LETAT-TOT-LIB-ED.
           MOVE 'NOUVEAU SOLDE'             TO WS-LETAT-LIB-ED.
           MOVE WS-CDB                      TO WS-LETAT-TOTDB-ED.
           MOVE WS-CCR                      TO WS-LETAT-TOTCR-ED.
           MOVE WS-LETAT-CLOSE              TO WS-LETAT-CLOSE-ED.
       7170-GST-PP-ETATCLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7180-GST-CRE-DEB.
           COMPUTE WS-LCRE-MVTS-TOT-ED =
                   WS-CRET + WS-CCB + WS-CDEP + WS-CERR + WS-CK.
           MOVE WS-LANO-TOT                 TO WS-LANO-TOTAL-ED.
       7180-GST-CRE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7190-CLR-F-DEB.
           COMPUTE WS-LETAT-SOLD-ED =
                   WS-CPTES-SOLDE + WS-CCR - WS-CDB.
           SET CLOTURE-FALSE                TO TRUE.
       7190-CLR-F-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *---------------------------------------------------------------*
      *
       8000-MVTS-EMPTY-DEB.
           DISPLAY 'F-MVTS-E VIDE'.
       8000-MVTS-EMPTY-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8010-EDIT-PG-ETATCLI-DEB.
           MOVE WS-ENTETE-L1                TO WS-BUFFER.
           PERFORM 6070-WRITE-NEW-FETATCLI-DEB
              THRU 6070-WRITE-NEW-FETATCLI-FIN.
      *
           MOVE WS-ENTETE-L2                TO WS-BUFFER.
           PERFORM 6080-WRITE-FETATCLI-DEB
              THRU 6080-WRITE-FETATCLI-FIN.
      *
           MOVE WS-ENTETE-L3                TO WS-BUFFER.
           PERFORM 6080-WRITE-FETATCLI-DEB
              THRU 6080-WRITE-FETATCLI-FIN.
      *
           MOVE WS-ENTETE-L4                TO WS-BUFFER.
           PERFORM 6080-WRITE-FETATCLI-DEB
              THRU 6080-WRITE-FETATCLI-FIN.
      *
           MOVE WS-ENTETE-L2                TO WS-BUFFER.
           PERFORM 6080-WRITE-FETATCLI-DEB
              THRU 6080-WRITE-FETATCLI-FIN.
      *
           MOVE WS-ENTETE-L2                TO WS-BUFFER.
           PERFORM 6080-WRITE-FETATCLI-DEB
              THRU 6080-WRITE-FETATCLI-FIN.
      *
           MOVE WS-ENTETE-L5                TO WS-BUFFER.
           PERFORM 6080-WRITE-FETATCLI-DEB
              THRU 6080-WRITE-FETATCLI-FIN.
      *
           MOVE WS-ENTETE-L6                TO WS-BUFFER.
           PERFORM 6080-WRITE-FETATCLI-DEB
              THRU 6080-WRITE-FETATCLI-FIN.
      *
           MOVE WS-ENTETE-L2                TO WS-BUFFER.
           PERFORM 6080-WRITE-FETATCLI-DEB
              THRU 6080-WRITE-FETATCLI-FIN.
      *
           MOVE WS-ENTETE-L7                TO WS-BUFFER.
           PERFORM 6080-WRITE-FETATCLI-DEB
              THRU 6080-WRITE-FETATCLI-FIN.
      *
           MOVE WS-ENTETE-L8                TO WS-BUFFER.
           PERFORM 6080-WRITE-FETATCLI-DEB
              THRU 6080-WRITE-FETATCLI-FIN.
      *
           MOVE WS-ENTETE-L2                TO WS-BUFFER.
           PERFORM 6080-WRITE-FETATCLI-DEB
              THRU 6080-WRITE-FETATCLI-FIN.
      *
           MOVE WS-ENTETE-L1                TO WS-BUFFER.
           PERFORM 6080-WRITE-FETATCLI-DEB
              THRU 6080-WRITE-FETATCLI-FIN.
       8010-EDIT-PG-ETATCLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8020-EDIT-PG-ETATANO-DEB.
           MOVE WS-ENTETE-L1                TO WS-BUFFER.
           PERFORM 6090-WRITE-NEW-FETATANO-DEB
              THRU 6090-WRITE-NEW-FETATANO-FIN.
      *
           MOVE WS-ENTETE-L2                TO WS-BUFFER.
           PERFORM 6100-WRITE-FETATANO-DEB
              THRU 6100-WRITE-FETATANO-FIN.
      *
           MOVE WS-LANO-ENTETE-L3           TO WS-BUFFER.
           PERFORM 6100-WRITE-FETATANO-DEB
              THRU 6100-WRITE-FETATANO-FIN.
      *
           MOVE WS-LANO-ENTETE-L4           TO WS-BUFFER.
           PERFORM 6100-WRITE-FETATANO-DEB
              THRU 6100-WRITE-FETATANO-FIN.
      *
           MOVE WS-ENTETE-L2                TO WS-BUFFER.
           PERFORM 6100-WRITE-FETATANO-DEB
              THRU 6100-WRITE-FETATANO-FIN.
      *
           MOVE WS-LANO-ENTETE-L5           TO WS-BUFFER.
           PERFORM 6100-WRITE-FETATANO-DEB
              THRU 6100-WRITE-FETATANO-FIN.
      *
           MOVE WS-LANO-ENTETE-L6           TO WS-BUFFER.
           PERFORM 6100-WRITE-FETATANO-DEB
              THRU 6100-WRITE-FETATANO-FIN.
      *
           MOVE WS-ENTETE-L2                TO WS-BUFFER.
           PERFORM 6100-WRITE-FETATANO-DEB
              THRU 6100-WRITE-FETATANO-FIN.
      *
           MOVE WS-ENTETE-L7                TO WS-BUFFER.
           PERFORM 6100-WRITE-FETATANO-DEB
              THRU 6100-WRITE-FETATANO-FIN.
      *
           MOVE WS-ENTETE-L8                TO WS-BUFFER.
           PERFORM 6100-WRITE-FETATANO-DEB
              THRU 6100-WRITE-FETATANO-FIN.
      *
           MOVE WS-ENTETE-L2                TO WS-BUFFER.
           PERFORM 6100-WRITE-FETATANO-DEB
              THRU 6100-WRITE-FETATANO-FIN.
      *
           MOVE WS-ENTETE-L1                TO WS-BUFFER.
           PERFORM 6100-WRITE-FETATANO-DEB
              THRU 6100-WRITE-FETATANO-FIN.
       8020-EDIT-PG-ETATANO-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8030-EDIT-ENT-ETATCLI-DEB.
           MOVE WS-ENTETE-L1                TO WS-BUFFER.
           PERFORM 6070-WRITE-NEW-FETATCLI-DEB
              THRU 6070-WRITE-NEW-FETATCLI-FIN.
      *
           MOVE WS-LETAT-DATE-PAGE          TO WS-BUFFER.
           PERFORM 6080-WRITE-FETATCLI-DEB
              THRU 6080-WRITE-FETATCLI-FIN.
      *
           MOVE WS-LETAT-NUMCPT             TO WS-BUFFER.
           PERFORM 6080-WRITE-FETATCLI-DEB
              THRU 6080-WRITE-FETATCLI-FIN.
      *
           MOVE WS-LETAT-TIRETS             TO WS-BUFFER.
           PERFORM 6080-WRITE-FETATCLI-DEB
              THRU 6080-WRITE-FETATCLI-FIN.
      *
           MOVE WS-LETAT-SOLD-OP            TO WS-BUFFER.
           PERFORM 6080-WRITE-FETATCLI-DEB
              THRU 6080-WRITE-FETATCLI-FIN.
      *
           MOVE WS-LETAT-TIRETS             TO WS-BUFFER.
           PERFORM 6080-WRITE-FETATCLI-DEB
              THRU 6080-WRITE-FETATCLI-FIN.
      *
           MOVE WS-LETAT-TITRES             TO WS-BUFFER.
           PERFORM 6080-WRITE-FETATCLI-DEB
              THRU 6080-WRITE-FETATCLI-FIN.
      *
           MOVE WS-LETAT-TIRETS             TO WS-BUFFER.
           PERFORM 6080-WRITE-FETATCLI-DEB
              THRU 6080-WRITE-FETATCLI-FIN.
       8030-EDIT-ENT-ETATCLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8040-EDIT-ENT-ETATANO-DEB.
           MOVE WS-LANO-L1                  TO WS-BUFFER.
           PERFORM 6090-WRITE-NEW-FETATANO-DEB
              THRU 6090-WRITE-NEW-FETATANO-FIN.
      *
           MOVE WS-LANO-TITRES              TO WS-BUFFER.
           PERFORM 6100-WRITE-FETATANO-DEB
              THRU 6100-WRITE-FETATANO-FIN.
      *
           MOVE WS-LANO-L3                  TO WS-BUFFER.
           PERFORM 6100-WRITE-FETATANO-DEB
              THRU 6100-WRITE-FETATANO-FIN.
       8040-EDIT-ENT-ETATANO-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8050-EDIT-LG-ETATCLI-DEB.
           MOVE WS-LETAT-DETAIL-OP          TO WS-BUFFER.
           PERFORM 6080-WRITE-FETATCLI-DEB
              THRU 6080-WRITE-FETATCLI-FIN.
       8050-EDIT-LG-ETATCLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8060-EDIT-LG-ETATANO-DEB.
           MOVE WS-LANO-DETAIL              TO WS-BUFFER.
           PERFORM 6100-WRITE-FETATANO-DEB
              THRU 6100-WRITE-FETATANO-FIN.
       8060-EDIT-LG-ETATANO-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8070-EDIT-PP-ETATCLI-DEB.
           MOVE WS-LETAT-TIRETS             TO WS-BUFFER.
           PERFORM 6080-WRITE-FETATCLI-DEB
              THRU 6080-WRITE-FETATCLI-FIN.
      *
           MOVE WS-LETAT-TOT-OP             TO WS-BUFFER.
           PERFORM 6080-WRITE-FETATCLI-DEB
              THRU 6080-WRITE-FETATCLI-FIN.
      *
           MOVE WS-LETAT-TIRETS             TO WS-BUFFER.
           PERFORM 6080-WRITE-FETATCLI-DEB
              THRU 6080-WRITE-FETATCLI-FIN.
      *
           MOVE WS-LETAT-SOLD-OP            TO WS-BUFFER.
           PERFORM 6080-WRITE-FETATCLI-DEB
              THRU 6080-WRITE-FETATCLI-FIN.
      *
           MOVE WS-ENTETE-L1                TO WS-BUFFER.
           PERFORM 6080-WRITE-FETATCLI-DEB
              THRU 6080-WRITE-FETATCLI-FIN.
       8070-EDIT-PP-ETATCLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8080-EDIT-PP-ETATANO-DEB.
           MOVE WS-LANO-L3                  TO WS-BUFFER.
           PERFORM 6100-WRITE-FETATANO-DEB
              THRU 6100-WRITE-FETATANO-FIN.
      *
           MOVE WS-LANO-TOTAL               TO WS-BUFFER.
           PERFORM 6100-WRITE-FETATANO-DEB
              THRU 6100-WRITE-FETATANO-FIN.
      *
           MOVE WS-LANO-L1                  TO WS-BUFFER.
           PERFORM 6100-WRITE-FETATANO-DEB
              THRU 6100-WRITE-FETATANO-FIN.
       8080-EDIT-PP-ETATANO-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8090-EDIT-ETATANO-VIDE-DEB.
      *
           MOVE WS-LANO-OK                  TO WS-BUFFER.
           PERFORM 6090-WRITE-NEW-FETATANO-DEB
              THRU 6090-WRITE-NEW-FETATANO-FIN.
      *
       8090-EDIT-ETATANO-VIDE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8999-STATISTIQUES-CRE-DEB.
      *
            DISPLAY '*********************************************'
            DISPLAY '*     STATISTIQUES DU PROGRAMME ARIO426     *'
            DISPLAY '*     =================================     *'
            DISPLAY '*********************************************'.
      *
            DISPLAY WS-LCRE-ASTER.
            DISPLAY WS-LCRE-TITRE.
            DISPLAY WS-LCRE-ASTER.
            MOVE WS-TOT-CPT                 TO WS-LCRE-CPT-TRT-TOT-ED.
            DISPLAY WS-LCRE-CPT-TRT-ED.
            MOVE WS-NEW-CPT                 TO WS-LCRE-CPT-CRE-TOT-ED.
            DISPLAY WS-LCRE-CPT-CRE-ED.
            MOVE WS-STD-CPT                 TO WS-LCRE-CPT-STD-TOT-ED.
            DISPLAY WS-LCRE-CPT-STD-ED.
            MOVE WS-K-CPT                   TO WS-LCRE-CPT-CLR-TOT-ED.
            DISPLAY WS-LCRE-CPT-CLR-ED.
            DISPLAY WS-LCRE-MVTS-ED.
            MOVE WS-CERR                    TO WS-LCRE-ANOM-TOT-ED.
            DISPLAY WS-LCRE-ANOM-ED.
            MOVE WS-CRET                    TO WS-LCRE-RET-TOT-ED.
            DISPLAY WS-LCRE-RET-ED.
            MOVE WS-CCB                     TO WS-LCRE-CBS-TOT-ED.
            DISPLAY WS-LCRE-CBS-ED.
            MOVE WS-CDEP                    TO WS-LCRE-DEP-TOT-ED.
            DISPLAY WS-LCRE-DEP-ED.
            DISPLAY WS-LCRE-ASTER.
      *
       8999-STATISTIQUES-CRE-FIN.
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
           DISPLAY '*     FIN NORMALE DU PROGRAMME ARIO426       *'.
           DISPLAY '*============================================*'.
       9999-FIN-PROGRAMME-FIN.
           EXIT.
      *
       9999-ERREUR-PROGRAMME-DEB.
           DISPLAY '*============================================*'.
           DISPLAY '*        UNE ANOMALIE A ETE DETECTEE         *'.
           DISPLAY '*     FIN ANORMALE DU PROGRAMME ARIO426      *'.
           DISPLAY '*============================================*'.
           MOVE 12 TO RETURN-CODE.
       9999-ERREUR-PROGRAMME-FIN.
           STOP RUN.
