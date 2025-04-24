      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIO326                                   *
      *  NOM DU REDACTEUR : GIGON                                     *
      *  SOCIETE          : ESTIAC                                    *
      *  DATE DE CREATION : 28/02/2025                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      *  METTRE A JOUR LE FICHIER DES COMPTES CLIENTS (F-CPTE-E) A    *
      *  PARTIR DES MOUVEMENTS BANCAIRES (F-MVTS-E) DANS UN FICHIER   *
      *  DE MISE à JOUR DES COMPTES CLIENTS (F-CPTE-S).               *
      *  EDITER DES FICHIERS D'IMPRESSIONS : ETAT DES CLIENTS ET      *
      *  ETAT DES ANOMALIES.                                          *
      *  ECRIRE UN COMPTE RENDU D'EXECUTION DANS LA SYSOUT.           *
      *---------------------------------------------------------------*
      *--               HISTORIQUE DES MODIFICATIONS                --*
      *---------------------------------------------------------------*
      * DATE  MODIF   !          NATURE DE LA MODIFICATION            *
      *---------------------------------------------------------------*
      * 28/02/2025    !  CREATION DU FICHIER COBOL                    *
      *               !                                               *
      *===============================================================*
      *
      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID.      ARIO326.
      *
      *                  ==============================               *
      *=================<  ENVIRONMENT      DIVISION   >==============*
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
      *
      *                      ------------------------------------------
      *                      F-MVTS-E : FICHIER DES MOUVEMENTS
      *                      ------------------------------------------
           SELECT  F-MVTS-E            ASSIGN TO INP001
                   FILE STATUS         IS WS-FS-MVTS-E.
      *                      ------------------------------------------
      *                      F-CPTE-E : FICHIER DES COMPTES CLIENTS
      *                      ------------------------------------------
           SELECT  F-CPTE-E            ASSIGN TO INP002
                   FILE STATUS         IS WS-FS-CPTE-E.
      *                      ------------------------------------------
      *                      F-CPTE-S : FICHIER DES COMPTES CLIENTS MAJ
      *                      ------------------------------------------
           SELECT  F-CPTE-S            ASSIGN TO OUT001
                   FILE STATUS         IS WS-FS-CPTE-S.
      *                      ------------------------------------------
      *                      F-ETATCLI-S : FICHIER ETAT DES CLIENTS
      *                      ------------------------------------------
           SELECT  F-ETATCLI-S         ASSIGN TO ETATCLI
                   FILE STATUS         IS WS-FS-ETATCLI-S.
      *                      ------------------------------------------
      *                      F-ETATANO-S : FICHIER ETAT DES ANOMALIES
      *                      ------------------------------------------
           SELECT  F-ETATANO-S         ASSIGN TO ETATANO
                   FILE STATUS         IS WS-FS-ETATANO-S.
      *                      ------------------------------------------
      *                                                               *
      *                  ==============================               *
      *=================<       DATA        DIVISION   >==============*
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
           DATA RECORD IS F.
       01  FS-ENRG-MVTS-E          PIC X(50).
      *
       FD  F-CPTE-E
           DATA RECORD IS F.
       01  FS-ENRG-CPTE-E          PIC X(50).
      *
       FD  F-CPTE-S
           DATA RECORD IS F.
       01  FS-ENRG-CPTE-S          PIC X(50).
      *
       FD  F-ETATCLI-S
           DATA RECORD IS F.
       01  FS-ENRG-ETATCLI-S       PIC X(80).
      *
       FD  F-ETATANO-S
           DATA RECORD IS F.
       01  FS-ENRG-ETATANO-S       PIC X(80).
      *
      *========================
       WORKING-STORAGE SECTION.
      *========================
      *
      *------------------- VARIABLES FILE STATUS ---------------------*
      *
       01  WS-FS-MVTS-E            PIC X(2).
           88  OK-MVTS             VALUE '00'.
           88  EMPTY-FILE-MVTS     VALUE '10'.
      *
       01  WS-FS-CPTE-E            PIC X(2).
           88  OK-CPTE             VALUE '00'.
           88  EMPTY-FILE-CPTE     VALUE '10'.
      *
       01  WS-FS-CPTE-S            PIC X(2).
           88  OK-CPTS             VALUE '00'.
      *
       01  WS-FS-ETATCLI-S         PIC X(2).
           88  OK-ETATCLI          VALUE '00'.
      *
       01  WS-FS-ETATANO-S         PIC X(2).
           88  OK-ETATANO          VALUE '00'.
      *
      *------------------- LIGNES D'EDITION --------------------------*
      *
       COPY TP3LEDIT.
      *
      *------------------- ENREGISTREMENT MVTS EN ENTREE -------------*
      *
       COPY TP3MVTS.
      *
      *------------------- ENREGISTREMENT CPTE EN ENTREE -------------*
      *
       COPY TP3CPTE.
      *
      *------------------- ENREGISTREMENT CPTS EN SORTIE -------------*
      *
       COPY TP3CPTS.
      *
      *------------------- VARIABLES DE TRAITEMENT -------------------*
      *
       01  WS-LETAT-CPT-PAGE       PIC S9(4) COMP VALUE 1.
      *
       01  WS-CRET                 PIC S9(4) COMP VALUE 0.
      *
       01  WS-CCB                  PIC S9(4) COMP VALUE 0.
      *
       01  WS-CDEP                 PIC S9(4) COMP VALUE 0.
      *
       01  WS-CERR                 PIC S9(4) COMP VALUE 0.
           88  FIRST-ANO           VALUE 1.
      *
       01  WS-LCRE-CLINEW-TOT      PIC S9(4) COMP VALUE 0.
      *
       01  WS-LCRE-CLISOP-TOT      PIC S9(4) COMP VALUE 0.
      *
       01  WS-LCRE-CLISTD-TOT      PIC S9(4) COMP VALUE 0.
      *
       01  WS-CDEBIT               PIC 9(11)V99 COMP-3.
           88  PB-DBT-NULL         VALUE 0.
      *
       01  WS-CCREDIT              PIC 9(11)V99 COMP-3.
           88  PB-CRT-NULL         VALUE 0.
      *
       01  WS-DATE-US-TMP.
           05  WS-SS-US            PIC 9(2).
           05  WS-AA-US            PIC 9(2).
           05  WS-MM-US            PIC 9(2).
           05  WS-JJ-US            PIC 9(2).
      *
       01  WS-BUFFER               PIC X(80).
      *
       01  WS-ANO-TOT              PIC 9(9)V99 COMP-3.
           88  TOT-ANO-NULL        VALUE 0.
      *
      *                  ==============================               *
      *=================<   PROCEDURE       DIVISION   >==============*
      *                  ==============================               *
      *                                                               *
      *===============================================================*
      *
       PROCEDURE           DIVISION.
      *
      *===============================================================*
      *    STRUCTURATION DE LA PARTIE ALGORITHMIQUE DU PROGRAMME      *
      *---------------------------------------------------------------*
      *                                                               *
      *    1 : LES COMPOSANTS DU DIAGRAMME SONT CODES A L'AIDE DE     *
      *        DEUX PARAGRAPHES  XXXX-COMPOSANT-DEB                   *
      *                          XXYY-COMPOSANR-FIN                   *
      *                                                               *
      *    2 : XX REPRESENTE LE NIVEAU HIERARCHIQUE                   *
      *        YY DIFFERENCIE LES COMPOSANTS DE MEME NIVEAU           *
      *                                                               *
      *    3 : TOUT COMPOSANT EST PRECEDE D'UN CARTOUCHE DE           *
      *        COMMENTAIRE QUI EXPLICITE LE ROLE DU COMPOSANT         *
      *                                                               *
      *                                                               *
      *===============================================================*
      *===============================================================*
      *
      *
      *---------------------------------------------------------------*
      *                     TRAITEMENT PRINCIPAL                      *
      *                     ====================                      *
      *---------------------------------------------------------------*
      *
       0000-PROGRAMME-DEB.
      *
      *----------------- ENTREE --------------------------------------*
      *
           PERFORM 6000-OPEN-FMVTS-DEB
              THRU 6000-OPEN-FMVTS-FIN.
      *
           PERFORM 6010-OPEN-FCPTE-DEB
              THRU 6010-OPEN-FCPTE-FIN.
      *
           PERFORM 6020-OPEN-FCPTS-DEB
              THRU 6020-OPEN-FCPTS-FIN.
      *
           PERFORM 6030-OPEN-FETATCLI-DEB
              THRU 6030-OPEN-FETATCLI-FIN.
      *
           PERFORM 6040-OPEN-FETATANO-DEB
              THRU 6040-OPEN-FETATANO-FIN.
      *
           PERFORM 6050-READ-FMVTS-DEB
              THRU 6050-READ-FMVTS-FIN.
      *
           IF EMPTY-FILE-MVTS
              PERFORM 8000-MVTS-EMPTY-DEB
                 THRU 8000-MVTS-EMPTY-FIN
           END-IF.
      *
           PERFORM 6060-READ-FCPTE-DEB
              THRU 6060-READ-FCPTE-FIN.
      *
           IF EMPTY-FILE-CPTE
              PERFORM 8010-CPTE-EMPTY-DEB
                 THRU 8010-CPTE-EMPTY-FIN
           END-IF.
      *
           PERFORM 7000-INIT-DATE-DEB
              THRU 7000-INIT-DATE-FIN.
      *
           PERFORM 8020-EDIT-PG-ETATCLI-DEB
              THRU 8020-EDIT-PG-ETATCLI-FIN.
      *
           PERFORM 8030-EDIT-PG-ETATANO-DEB
              THRU 8030-EDIT-PG-ETATANO-FIN.
      *
      *----------------- ITERATIVE -----------------------------------*
      *
           PERFORM 1000-ASSORTIMENT-DEB
              THRU 1000-ASSORTIMENT-FIN
             UNTIL (MVTS-CPTE-MAX AND CPTE-CPTE-MAX).
      *
      *----------------- SORTIE --------------------------------------*
      *
           PERFORM 7130-GST-ANO-CRE-DEB
              THRU 7130-GST-ANO-CRE-FIN.
      *
           IF NOT TOT-ANO-NULL
              PERFORM 8090-EDIT-PP-ETATANO-DEB
                 THRU 8090-EDIT-PP-ETATANO-FIN
           ELSE
              PERFORM 8100-EDIT-ETATANO-VIDE-DEB
                 THRU 8100-EDIT-ETATANO-VIDE-FIN
           END-IF.
      *
           PERFORM 8999-STATISTIQUES-CRE-DEB
              THRU 8999-STATISTIQUES-CRE-FIN.
      *
           PERFORM 6120-CLOSE-FMVTS-DEB
              THRU 6120-CLOSE-FMVTS-FIN.
      *
           PERFORM 6130-CLOSE-FCPTE-DEB
              THRU 6130-CLOSE-FCPTE-FIN.
      *
           PERFORM 6140-CLOSE-FCPTS-DEB
              THRU 6140-CLOSE-FCPTS-FIN.
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
       0000-PROGRAMME-FIN.
            STOP RUN.
      *
      *---------------------------------------------------------------*
      *                     ASSORTIMENT                               *
      *                     ===========                               *
      *---------------------------------------------------------------*
      *
       1000-ASSORTIMENT-DEB.
      *
      *----------------- ALTERNATIVE MULTIPLE ------------------------*
      *
           EVALUATE TRUE
              WHEN WS-CPTE-CPTE < WS-MVTS-CPTE
                   PERFORM 2000-CPTE-SANS-MVT-DEB
                      THRU 2000-CPTE-SANS-MVT-FIN
              WHEN WS-CPTE-CPTE = WS-MVTS-CPTE
                   PERFORM 2010-CPTE-AVEC-MVT-DEB
                      THRU 2010-CPTE-AVEC-MVT-FIN
              WHEN WS-CPTE-CPTE > WS-MVTS-CPTE
                   PERFORM 2020-MVT-SANS-CPTE-DEB
                      THRU 2020-MVT-SANS-CPTE-FIN
           END-EVALUATE.
      *
       1000-ASSORTIMENT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *              TRAITEMENT COMPTE SANS MOUVEMENT                 *
      *              ================================                 *
      *---------------------------------------------------------------*
      *
       2000-CPTE-SANS-MVT-DEB.
      *
           PERFORM 7010-GST-CPTE-SANS-MVT-DEB
              THRU 7010-GST-CPTE-SANS-MVT-FIN.
      *
           PERFORM 6070-WRITE-FCPTS-DEB
              THRU 6070-WRITE-FCPTS-FIN.
      *
           PERFORM 6060-READ-FCPTE-DEB
              THRU 6060-READ-FCPTE-FIN.
      *
       2000-CPTE-SANS-MVT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *              TRAITEMENT COMPTE AVEC MOUVEMENT                 *
      *              ================================                 *
      *---------------------------------------------------------------*
      *
       2010-CPTE-AVEC-MVT-DEB.
      *
      *----------------- ENTREE --------------------------------------*
      *
           PERFORM 7020-GST-IN-CPTE-AVEC-MVT-DEB
              THRU 7020-GST-IN-CPTE-AVEC-MVT-FIN.
      *
      *----------------- ITERATIVE -----------------------------------*
      *
           PERFORM 3000-TRT-MVT-DEB
              THRU 3000-TRT-MVT-FIN
             UNTIL (WS-MVTS-CPTE NOT = WS-CPTS-CPTE) OR
                   EMPTY-FILE-MVTS.
      *
      *----------------- SORTIE --------------------------------------*
      *
           IF NOT (PB-DBT-NULL AND PB-CRT-NULL)
              PERFORM 7110-GST-OUT-CPTE-AVEC-MVT-DEB
                 THRU 7110-GST-OUT-CPTE-AVEC-MVT-FIN
      *
              PERFORM 8080-EDIT-PP-ETATCLI-DEB
                 THRU 8080-EDIT-PP-ETATCLI-FIN
           END-IF.
      *
           PERFORM 6070-WRITE-FCPTS-DEB
              THRU 6070-WRITE-FCPTS-FIN.
      *
           PERFORM 6060-READ-FCPTE-DEB
              THRU 6060-READ-FCPTE-FIN.
      *
       2010-CPTE-AVEC-MVT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *              TRAITEMENT MOUVEMENT SANS COMPTE                 *
      *              ================================                 *
      *---------------------------------------------------------------*
      *
       2020-MVT-SANS-CPTE-DEB.
      *
      *----------------- ENTREE --------------------------------------*
      *
           PERFORM 7050-GST-IN-MVT-SANS-CPTE-DEB
              THRU 7050-GST-IN-MVT-SANS-CPTE-FIN.
      *
      *----------------- ITERATIVE -----------------------------------*
      *
           PERFORM 3010-TRT-MVT-DEB
              THRU 3010-TRT-MVT-FIN
             UNTIL (WS-MVTS-CPTE NOT = WS-CPTS-CPTE) OR
                   EMPTY-FILE-MVTS.
      *
      *----------------- SORTIE --------------------------------------*
      *
           IF NOT (PB-DBT-NULL AND PB-CRT-NULL)
              PERFORM 7120-GST-OUT-MVT-SANS-CPTE-DEB
                 THRU 7120-GST-OUT-MVT-SANS-CPTE-FIN
      *
              PERFORM 8080-EDIT-PP-ETATCLI-DEB
                 THRU 8080-EDIT-PP-ETATCLI-FIN
      *
              PERFORM 6070-WRITE-FCPTS-DEB
                 THRU 6070-WRITE-FCPTS-FIN
           END-IF.
      *
       2020-MVT-SANS-CPTE-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *                     TRAITEMENT MOUVEMENT                      *
      *                     ====================                      *
      *---------------------------------------------------------------*
      *
       3000-TRT-MVT-DEB.
      *
      *----------------- ENTREE --------------------------------------*
      *
           PERFORM 7030-INIT-DB-CR-ETATCLI-DEB
              THRU 7030-INIT-DB-CR-ETATCLI-FIN.
      *
           IF ((RETRAIT OR CB OR DEPOT)
              AND (PB-DBT-NULL AND PB-CRT-NULL))
              PERFORM 7040-INIT-ENT-ETATCLI-DEB
                 THRU 7040-INIT-ENT-ETATCLI-FIN
      *
              PERFORM 8040-EDIT-ENT-ETATCLI-DEB
                 THRU 8040-EDIT-ENT-ETATCLI-FIN
           END-IF.
      *
      *----------------- ALTERNATIVE MULTIPLE -----------------------*
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
              WHEN OTHER
                   PERFORM 4030-TRT-ANO-DEB
                      THRU 4030-TRT-ANO-FIN
           END-EVALUATE.
      *
      *----------------- SORTIE --------------------------------------*
      *
           PERFORM 6050-READ-FMVTS-DEB
              THRU 6050-READ-FMVTS-FIN.
      *
       3000-TRT-MVT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *
       3010-TRT-MVT-DEB.
      *
      *----------------- ENTREE --------------------------------------*
      *
           PERFORM 7030-INIT-DB-CR-ETATCLI-DEB
              THRU 7030-INIT-DB-CR-ETATCLI-FIN.
      *
           IF ((RETRAIT OR CB OR DEPOT)
              AND (PB-DBT-NULL AND PB-CRT-NULL))
              PERFORM 7060-INIT-ENT-NEW-ETATCLI-DEB
                 THRU 7060-INIT-ENT-NEW-ETATCLI-FIN
      *
              PERFORM 8040-EDIT-ENT-ETATCLI-DEB
                 THRU 8040-EDIT-ENT-ETATCLI-FIN
           END-IF.
      *
      *----------------- ALTERNATIVE MULTIPLE -----------------------*
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
              WHEN OTHER
                   PERFORM 4030-TRT-ANO-DEB
                      THRU 4030-TRT-ANO-FIN
           END-EVALUATE.
      *
      *----------------- SORTIE --------------------------------------*
      *
           PERFORM 6050-READ-FMVTS-DEB
              THRU 6050-READ-FMVTS-FIN.
      *
       3010-TRT-MVT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *                     TRAITEMENT RETRAIT                        *
      *                     ==================                        *
      *---------------------------------------------------------------*
      *
       4000-TRT-RETRAIT-DEB.
      *
           PERFORM 7070-GST-RETRAIT-DEB
              THRU 7070-GST-RETRAIT-FIN.
      *
           PERFORM 8060-EDIT-LG-ETATCLI-DEB
              THRU 8060-EDIT-LG-ETATCLI-FIN.
      *
       4000-TRT-RETRAIT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *                     TRAITEMENT CB                             *
      *                     =============                             *
      *---------------------------------------------------------------*
      *
       4010-TRT-CB-DEB.
      *
           PERFORM 7080-GST-CB-DEB
              THRU 7080-GST-CB-FIN.
      *
           PERFORM 8060-EDIT-LG-ETATCLI-DEB
              THRU 8060-EDIT-LG-ETATCLI-FIN.
      *
       4010-TRT-CB-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *                     TRAITEMENT DEPOT                          *
      *                     ================                          *
      *---------------------------------------------------------------*
      *
       4020-TRT-DEPOT-DEB.
      *
           PERFORM 7090-GST-DEPOT-DEB
              THRU 7090-GST-DEPOT-FIN.
      *
           PERFORM 8060-EDIT-LG-ETATCLI-DEB
              THRU 8060-EDIT-LG-ETATCLI-FIN.
      *
       4020-TRT-DEPOT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *                     TRAITEMENT ANO                            *
      *                     ==============                            *
      *---------------------------------------------------------------*
      *
       4030-TRT-ANO-DEB.
      *
           PERFORM 7100-GST-ANO-DEB
              THRU 7100-GST-ANO-FIN.
      *
           IF FIRST-ANO
              PERFORM 8050-EDIT-ENT-ETATANO-DEB
                 THRU 8050-EDIT-ENT-ETATANO-FIN
           END-IF.
      *
           PERFORM 8070-EDIT-LG-ETATANO-DEB
              THRU 8070-EDIT-LG-ETATANO-FIN.
      *
       4030-TRT-ANO-FIN.
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
      *                                                               *
       6000-OPEN-FMVTS-DEB.
           OPEN INPUT F-MVTS-E.
           IF NOT OK-MVTS
              DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER F-MVTS-E'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-MVTS-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6000-OPEN-FMVTS-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6010-OPEN-FCPTE-DEB.
           OPEN INPUT F-CPTE-E.
           IF NOT OK-CPTE
              DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER F-CPTE-E'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-CPTE-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6010-OPEN-FCPTE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6020-OPEN-FCPTS-DEB.
           OPEN OUTPUT F-CPTE-S.
           IF NOT OK-CPTS
              DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER F-CPTE-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-CPTE-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6020-OPEN-FCPTS-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6030-OPEN-FETATCLI-DEB.
           OPEN OUTPUT F-ETATCLI-S.
           IF NOT OK-ETATCLI
              DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER F-ETATCLI-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATCLI-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6030-OPEN-FETATCLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6040-OPEN-FETATANO-DEB.
           OPEN OUTPUT F-ETATANO-S.
           IF NOT OK-ETATANO
              DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER F-ETATANO-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6040-OPEN-FETATANO-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6050-READ-FMVTS-DEB.
           READ F-MVTS-E INTO WS-ENRG-F-MVTS.
           IF NOT (OK-MVTS OR EMPTY-FILE-MVTS)
              DISPLAY 'PROBLEME DE LECTURE DU FICHIER F-MVTS-E'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-MVTS-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *
           IF EMPTY-FILE-MVTS
              SET MVTS-CPTE-MAX TO TRUE
           END-IF.
       6050-READ-FMVTS-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6060-READ-FCPTE-DEB.
           READ F-CPTE-E INTO WS-ENRG-F-CPTE.
           IF NOT (OK-CPTE OR EMPTY-FILE-CPTE)
              DISPLAY 'PROBLEME DE LECTURE DU FICHIER F-CPTE-E'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-CPTE-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
      *
           IF EMPTY-FILE-CPTE
              SET CPTE-CPTE-MAX TO TRUE
           END-IF.
       6060-READ-FCPTE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6070-WRITE-FCPTS-DEB.
           WRITE FS-ENRG-CPTE-S FROM WS-ENRG-F-CPTS.
           IF NOT OK-CPTS
              DISPLAY 'PROBLEME D''ECRITURE DU FICHIER F-CPTE-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-CPTE-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6070-WRITE-FCPTS-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6080-WRITE-ETATCLI-NEW-DEB.
           WRITE FS-ENRG-ETATCLI-S FROM WS-BUFFER AFTER PAGE.
           IF NOT OK-ETATCLI
              DISPLAY 'PROBLEME DE SAUT DE PAGE DU FICHIER F-ETATCLI-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATCLI-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6080-WRITE-ETATCLI-NEW-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6090-WRITE-ETATCLI-DEB.
           WRITE FS-ENRG-ETATCLI-S FROM WS-BUFFER.
           IF NOT OK-ETATCLI
              DISPLAY 'PROBLEME D''ECRITURE DU FICHIER F-ETATCLI-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATCLI-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6090-WRITE-ETATCLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6100-WRITE-ETATANO-NEW-DEB.
           WRITE FS-ENRG-ETATANO-S FROM WS-BUFFER AFTER PAGE.
           IF NOT OK-ETATANO
              DISPLAY 'PROBLEME SAUT DE PAGE DU FICHIER F-ETATANO-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6100-WRITE-ETATANO-NEW-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6110-WRITE-ETATANO-DEB.
           WRITE FS-ENRG-ETATANO-S FROM WS-BUFFER.
           IF NOT OK-ETATANO
              DISPLAY 'PROBLEME D''ECRITURE DU FICHIER F-ETATANO-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6110-WRITE-ETATANO-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6120-CLOSE-FMVTS-DEB.
           CLOSE F-MVTS-E.
           IF NOT OK-MVTS
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER F-MVTS-E'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-MVTS-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6120-CLOSE-FMVTS-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6130-CLOSE-FCPTE-DEB.
           CLOSE F-CPTE-E.
           IF NOT OK-CPTE
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER F-CPTE-E'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-CPTE-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6130-CLOSE-FCPTE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6140-CLOSE-FCPTS-DEB.
           CLOSE F-CPTE-S.
           IF NOT OK-CPTS
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER F-CPTE-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-CPTE-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6140-CLOSE-FCPTS-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6150-CLOSE-FETATCLI-DEB.
           CLOSE F-ETATCLI-S.
           IF NOT OK-ETATCLI
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
           IF NOT OK-ETATANO
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER F-ETATANO-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATANO-S
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
      *
           ACCEPT WS-CPTS-DMAJ         FROM DATE YYYYMMDD.
           ACCEPT WS-DATE-US-TMP       FROM DATE YYYYMMDD.
           MOVE WS-JJ-US               TO WS-L7-JJ-ED.
           MOVE WS-MM-US               TO WS-L7-MM-ED.
           MOVE WS-SS-US               TO WS-L7-SS-ED.
           MOVE WS-AA-US               TO WS-L7-AA-ED.
      *
       7000-INIT-DATE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7010-GST-CPTE-SANS-MVT-DEB.
      *
           MOVE WS-ENRG-F-CPTE         TO WS-ENRG-F-CPTS.
           ACCEPT WS-CPTS-DMAJ         FROM DATE YYYYMMDD.
           ADD 1                       TO WS-LCRE-CLISOP-TOT.
      *
       7010-GST-CPTE-SANS-MVT-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7020-GST-IN-CPTE-AVEC-MVT-DEB.
      *
           MOVE 0                      TO WS-CDEBIT
                                          WS-CCREDIT.
           MOVE WS-CPTE-CPTE           TO WS-CPTS-CPTE.
           MOVE WS-CPTE-DCREA          TO WS-CPTS-DCREA.
           ADD 1                       TO WS-LCRE-CLISTD-TOT.
           MOVE WS-CPTE-SOLDE          TO WS-CPTS-SOLDE.
      *
       7020-GST-IN-CPTE-AVEC-MVT-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7030-INIT-DB-CR-ETATCLI-DEB.
      *
           MOVE 0                      TO WS-LETAT-OP-DEBIT-ED
                                          WS-LETAT-OP-CREDIT-ED.
      *
       7030-INIT-DB-CR-ETATCLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7040-INIT-ENT-ETATCLI-DEB.
      *
           MOVE WS-LETAT-CPT-PAGE      TO WS-LETAT-PAGE-ED.
           MOVE WS-CPTE-CPTE           TO WS-LETAT-NUMCPT-ED.
           MOVE 'ANCIEN SOLDE'         TO WS-LETAT-LIB-ED.
           MOVE WS-CPTE-SOLDE          TO WS-CPTS-SOLDE.
           MOVE WS-CPTE-SOLDE          TO WS-LETAT-SOLD-ED.
           MOVE WS-JJ-US               TO WS-LETAT-JJ-ED.
           MOVE WS-MM-US               TO WS-LETAT-MM-ED.
           MOVE WS-SS-US               TO WS-LETAT-SS-ED.
           MOVE WS-AA-US               TO WS-LETAT-AA-ED.
      *
       7040-INIT-ENT-ETATCLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7050-GST-IN-MVT-SANS-CPTE-DEB.
      *
           MOVE 0                      TO WS-CDEBIT
                                          WS-CCREDIT.
           MOVE WS-MVTS-CPTE           TO WS-CPTS-CPTE.
           MOVE WS-MVTS-DATE           TO WS-CPTS-DCREA.
           ADD 1                       TO WS-LCRE-CLINEW-TOT.
      *
       7050-GST-IN-MVT-SANS-CPTE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7060-INIT-ENT-NEW-ETATCLI-DEB.
      *
           MOVE WS-JJ-US               TO WS-LETAT-JJ-ED.
           MOVE WS-MM-US               TO WS-LETAT-MM-ED.
           MOVE WS-SS-US               TO WS-LETAT-SS-ED.
           MOVE WS-AA-US               TO WS-LETAT-AA-ED.
           MOVE WS-LETAT-CPT-PAGE      TO WS-LETAT-PAGE-ED.
           MOVE WS-MVTS-CPTE           TO WS-LETAT-NUMCPT-ED.
           MOVE 'CREATION DE COMPTE'   TO WS-LETAT-OPEN-ED.
           MOVE 'ANCIEN SOLDE'         TO WS-LETAT-LIB-ED.
           MOVE 0                      TO WS-LETAT-SOLD-ED.
           MOVE WS-MVTS-DATE           TO WS-CPTS-DCREA.
      *
       7060-INIT-ENT-NEW-ETATCLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7070-GST-RETRAIT-DEB.
      *
           ADD WS-MVTS-MT              TO WS-CDEBIT.
           ADD 1                       TO WS-CRET.
           MOVE 'RETRAIT DAB'          TO WS-LETAT-OP-LIB-ED.
           MOVE WS-MVTS-SS             TO WS-LETAT-OP-SS-ED.
           MOVE WS-MVTS-AA             TO WS-LETAT-OP-AA-ED.
           MOVE WS-MVTS-MM             TO WS-LETAT-OP-MM-ED.
           MOVE WS-MVTS-JJ             TO WS-LETAT-OP-JJ-ED.
           MOVE WS-MVTS-MT             TO WS-LETAT-OP-DEBIT-ED.
      *
       7070-GST-RETRAIT-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7080-GST-CB-DEB.
      *
           ADD WS-MVTS-MT              TO WS-CDEBIT.
           ADD 1                       TO WS-CCB.
           MOVE 'CARTE BLEUE'          TO WS-LETAT-OP-LIB-ED.
           MOVE WS-MVTS-SS             TO WS-LETAT-OP-SS-ED.
           MOVE WS-MVTS-AA             TO WS-LETAT-OP-AA-ED.
           MOVE WS-MVTS-MM             TO WS-LETAT-OP-MM-ED.
           MOVE WS-MVTS-JJ             TO WS-LETAT-OP-JJ-ED.
           MOVE WS-MVTS-MT             TO WS-LETAT-OP-DEBIT-ED.
      *
       7080-GST-CB-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7090-GST-DEPOT-DEB.
      *
           ADD WS-MVTS-MT              TO WS-CCREDIT.
           ADD 1                       TO WS-CDEP.
           MOVE 'DEPOT GUICHET'        TO WS-LETAT-OP-LIB-ED.
           MOVE WS-MVTS-SS             TO WS-LETAT-OP-SS-ED.
           MOVE WS-MVTS-AA             TO WS-LETAT-OP-AA-ED.
           MOVE WS-MVTS-MM             TO WS-LETAT-OP-MM-ED.
           MOVE WS-MVTS-JJ             TO WS-LETAT-OP-JJ-ED.
           MOVE WS-MVTS-MT             TO WS-LETAT-OP-CREDIT-ED.
      *
       7090-GST-DEPOT-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7100-GST-ANO-DEB.
      *
           ADD WS-MVTS-MT              TO WS-ANO-TOT.
           ADD 1                       TO WS-CERR.
           MOVE WS-MVTS-CPTE           TO WS-LANO-NUMCPT-ED.
           MOVE WS-MVTS-CODE           TO WS-LANO-CODEMVT-ED.
           MOVE WS-MVTS-MT             TO WS-LANO-MONTANT-ED.
      *
       7100-GST-ANO-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7110-GST-OUT-CPTE-AVEC-MVT-DEB.
      *
           COMPUTE WS-LETAT-SOLD-ED =
                   WS-CPTE-SOLDE + WS-CCREDIT - WS-CDEBIT.
           MOVE WS-CDEBIT              TO WS-LETAT-TOTDB-ED.
           MOVE WS-CCREDIT             TO WS-LETAT-TOTCR-ED.
           MOVE 'NOUVEAU SOLDE'        TO WS-LETAT-LIB-ED.
           MOVE WS-LETAT-SOLD-ED       TO WS-CPTS-SOLDE.
      *
       7110-GST-OUT-CPTE-AVEC-MVT-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7120-GST-OUT-MVT-SANS-CPTE-DEB.
      *
           SUBTRACT WS-CDEBIT FROM WS-CCREDIT GIVING WS-LETAT-SOLD-ED.
           MOVE WS-CDEBIT              TO WS-LETAT-TOTDB-ED.
           MOVE WS-CCREDIT             TO WS-LETAT-TOTCR-ED.
           MOVE 'NOUVEAU SOLDE'        TO WS-LETAT-LIB-ED.
           MOVE WS-LETAT-SOLD-ED       TO WS-CPTS-SOLDE.
      *
       7120-GST-OUT-MVT-SANS-CPTE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7130-GST-ANO-CRE-DEB.
      *
           MOVE WS-ANO-TOT             TO WS-LANO-TOTAL-ED.
           COMPUTE WS-LCRE-CLI-TOT-ED =
                   WS-LCRE-CLINEW-TOT + WS-LCRE-CLISOP-TOT +
                   WS-LCRE-CLISTD-TOT.
           COMPUTE WS-LCRE-MVTS-TOT-ED =
                   WS-CRET + WS-CCB + WS-CDEP + WS-CERR.
      *
       7130-GST-ANO-CRE-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *---------------------------------------------------------------*
      *
       8000-MVTS-EMPTY-DEB.
      *
           DISPLAY 'F-MVTS-E VIDE'.
      *
       8000-MVTS-EMPTY-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8010-CPTE-EMPTY-DEB.
      *
           DISPLAY 'F-CPTE-E VIDE'.
      *
       8010-CPTE-EMPTY-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8020-EDIT-PG-ETATCLI-DEB.
      *
           MOVE WS-ENTETE-L1           TO WS-BUFFER.
           PERFORM 6080-WRITE-ETATCLI-NEW-DEB
              THRU 6080-WRITE-ETATCLI-NEW-FIN.
      *
           MOVE WS-ENTETE-L2           TO WS-BUFFER.
           PERFORM 6090-WRITE-ETATCLI-DEB
              THRU 6090-WRITE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L3           TO WS-BUFFER.
           PERFORM 6090-WRITE-ETATCLI-DEB
              THRU 6090-WRITE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L4           TO WS-BUFFER.
           PERFORM 6090-WRITE-ETATCLI-DEB
              THRU 6090-WRITE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L2           TO WS-BUFFER.
           PERFORM 6090-WRITE-ETATCLI-DEB
              THRU 6090-WRITE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L2           TO WS-BUFFER.
           PERFORM 6090-WRITE-ETATCLI-DEB
              THRU 6090-WRITE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L5           TO WS-BUFFER.
           PERFORM 6090-WRITE-ETATCLI-DEB
              THRU 6090-WRITE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L6           TO WS-BUFFER.
           PERFORM 6090-WRITE-ETATCLI-DEB
              THRU 6090-WRITE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L2           TO WS-BUFFER.
           PERFORM 6090-WRITE-ETATCLI-DEB
              THRU 6090-WRITE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L7           TO WS-BUFFER.
           PERFORM 6090-WRITE-ETATCLI-DEB
              THRU 6090-WRITE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L8           TO WS-BUFFER.
           PERFORM 6090-WRITE-ETATCLI-DEB
              THRU 6090-WRITE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L2           TO WS-BUFFER.
           PERFORM 6090-WRITE-ETATCLI-DEB
              THRU 6090-WRITE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L1           TO WS-BUFFER.
           PERFORM 6090-WRITE-ETATCLI-DEB
              THRU 6090-WRITE-ETATCLI-FIN.
      *
       8020-EDIT-PG-ETATCLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8030-EDIT-PG-ETATANO-DEB.
      *
           MOVE WS-ENTETE-L1           TO WS-BUFFER.
           PERFORM 6100-WRITE-ETATANO-NEW-DEB
              THRU 6100-WRITE-ETATANO-NEW-FIN.
      *
           MOVE WS-ENTETE-L2           TO WS-BUFFER.
           PERFORM 6110-WRITE-ETATANO-DEB
              THRU 6110-WRITE-ETATANO-FIN.
      *
           MOVE WS-LANO-ENTETE-L3      TO WS-BUFFER.
           PERFORM 6110-WRITE-ETATANO-DEB
              THRU 6110-WRITE-ETATANO-FIN.
      *
           MOVE WS-LANO-ENTETE-L4      TO WS-BUFFER.
           PERFORM 6110-WRITE-ETATANO-DEB
              THRU 6110-WRITE-ETATANO-FIN.
      *
           MOVE WS-ENTETE-L2           TO WS-BUFFER.
           PERFORM 6110-WRITE-ETATANO-DEB
              THRU 6110-WRITE-ETATANO-FIN.
      *
           MOVE WS-LANO-ENTETE-L5      TO WS-BUFFER.
           PERFORM 6110-WRITE-ETATANO-DEB
              THRU 6110-WRITE-ETATANO-FIN.
      *
           MOVE WS-LANO-ENTETE-L6      TO WS-BUFFER.
           PERFORM 6110-WRITE-ETATANO-DEB
              THRU 6110-WRITE-ETATANO-FIN.
      *
           MOVE WS-ENTETE-L2           TO WS-BUFFER.
           PERFORM 6110-WRITE-ETATANO-DEB
              THRU 6110-WRITE-ETATANO-FIN.
      *
           MOVE WS-ENTETE-L7           TO WS-BUFFER.
           PERFORM 6110-WRITE-ETATANO-DEB
              THRU 6110-WRITE-ETATANO-FIN.
      *
           MOVE WS-ENTETE-L8           TO WS-BUFFER.
           PERFORM 6110-WRITE-ETATANO-DEB
              THRU 6110-WRITE-ETATANO-FIN.
      *
           MOVE WS-ENTETE-L2           TO WS-BUFFER.
           PERFORM 6110-WRITE-ETATANO-DEB
              THRU 6110-WRITE-ETATANO-FIN.
      *
           MOVE WS-ENTETE-L1           TO WS-BUFFER.
           PERFORM 6110-WRITE-ETATANO-DEB
              THRU 6110-WRITE-ETATANO-FIN.
      *
       8030-EDIT-PG-ETATANO-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8040-EDIT-ENT-ETATCLI-DEB.
      *
           MOVE WS-ENTETE-L1           TO WS-BUFFER.
           PERFORM 6080-WRITE-ETATCLI-NEW-DEB
              THRU 6080-WRITE-ETATCLI-NEW-FIN.
      *
           MOVE WS-LETAT-DATE-PAGE     TO WS-BUFFER.
           PERFORM 6090-WRITE-ETATCLI-DEB
              THRU 6090-WRITE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-NUMCPT        TO WS-BUFFER.
           PERFORM 6090-WRITE-ETATCLI-DEB
              THRU 6090-WRITE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-TIRETS        TO WS-BUFFER.
           PERFORM 6090-WRITE-ETATCLI-DEB
              THRU 6090-WRITE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-SOLD-OP       TO WS-BUFFER.
           PERFORM 6090-WRITE-ETATCLI-DEB
              THRU 6090-WRITE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-TIRETS        TO WS-BUFFER.
           PERFORM 6090-WRITE-ETATCLI-DEB
              THRU 6090-WRITE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-TITRES        TO WS-BUFFER.
           PERFORM 6090-WRITE-ETATCLI-DEB
              THRU 6090-WRITE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-TIRETS        TO WS-BUFFER.
           PERFORM 6090-WRITE-ETATCLI-DEB
              THRU 6090-WRITE-ETATCLI-FIN.
      *
       8040-EDIT-ENT-ETATCLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8050-EDIT-ENT-ETATANO-DEB.
      *
           MOVE WS-LANO-L1             TO WS-BUFFER.
           PERFORM 6100-WRITE-ETATANO-NEW-DEB
              THRU 6100-WRITE-ETATANO-NEW-FIN.
      *
           MOVE WS-LANO-TITRES         TO WS-BUFFER.
           PERFORM 6110-WRITE-ETATANO-DEB
              THRU 6110-WRITE-ETATANO-FIN.
      *
           MOVE WS-LANO-L3             TO WS-BUFFER.
           PERFORM 6110-WRITE-ETATANO-DEB
              THRU 6110-WRITE-ETATANO-FIN.
      *
       8050-EDIT-ENT-ETATANO-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8060-EDIT-LG-ETATCLI-DEB.
      *
           MOVE WS-LETAT-DETAIL-OP     TO WS-BUFFER.
           PERFORM 6090-WRITE-ETATCLI-DEB
              THRU 6090-WRITE-ETATCLI-FIN.
      *
       8060-EDIT-LG-ETATCLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8070-EDIT-LG-ETATANO-DEB.
      *
           MOVE WS-LANO-DETAIL         TO WS-BUFFER.
           PERFORM 6110-WRITE-ETATANO-DEB
              THRU 6110-WRITE-ETATANO-FIN.
      *
       8070-EDIT-LG-ETATANO-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8080-EDIT-PP-ETATCLI-DEB.
      *
           MOVE WS-LETAT-TIRETS        TO WS-BUFFER.
           PERFORM 6090-WRITE-ETATCLI-DEB
              THRU 6090-WRITE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-TOT-OP        TO WS-BUFFER.
           PERFORM 6090-WRITE-ETATCLI-DEB
              THRU 6090-WRITE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-TIRETS        TO WS-BUFFER.
           PERFORM 6090-WRITE-ETATCLI-DEB
              THRU 6090-WRITE-ETATCLI-FIN.
      *
           MOVE WS-LETAT-SOLD-OP       TO WS-BUFFER.
           PERFORM 6090-WRITE-ETATCLI-DEB
              THRU 6090-WRITE-ETATCLI-FIN.
      *
           MOVE WS-ENTETE-L1           TO WS-BUFFER.
           PERFORM 6090-WRITE-ETATCLI-DEB
              THRU 6090-WRITE-ETATCLI-FIN.
      *
       8080-EDIT-PP-ETATCLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8090-EDIT-PP-ETATANO-DEB.
      *
           MOVE WS-LANO-L3             TO WS-BUFFER.
           PERFORM 6110-WRITE-ETATANO-DEB
              THRU 6110-WRITE-ETATANO-FIN.
      *
           MOVE WS-LANO-TOTAL          TO WS-BUFFER.
           PERFORM 6110-WRITE-ETATANO-DEB
              THRU 6110-WRITE-ETATANO-FIN.
      *
           MOVE WS-LANO-L1             TO WS-BUFFER.
           PERFORM 6110-WRITE-ETATANO-DEB
              THRU 6110-WRITE-ETATANO-FIN.
      *
       8090-EDIT-PP-ETATANO-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8100-EDIT-ETATANO-VIDE-DEB.
      *
           MOVE WS-LANO-OK             TO WS-BUFFER.
           PERFORM 6100-WRITE-ETATANO-NEW-DEB
              THRU 6100-WRITE-ETATANO-NEW-FIN.
      *
       8100-EDIT-ETATANO-VIDE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8999-STATISTIQUES-CRE-DEB.
      *
            DISPLAY '*********************************************'
            DISPLAY '*     STATISTIQUES DU PROGRAMME ARIO326     *'
            DISPLAY '*     =================================     *'
            DISPLAY '*********************************************'.
      *
            DISPLAY WS-LCRE-ASTER.
            DISPLAY WS-LCRE-TITRE.
            DISPLAY WS-LCRE-ASTER.
            DISPLAY WS-LCRE-CLIENT-ED.
            MOVE WS-LCRE-CLINEW-TOT    TO WS-LCRE-CLINEW-TOT-ED.
            DISPLAY WS-LCRE-CLINEW-ED.
            MOVE WS-LCRE-CLISOP-TOT    TO WS-LCRE-CLISOP-TOT-ED.
            DISPLAY WS-LCRE-CLISOP-ED.
            MOVE WS-LCRE-CLISTD-TOT    TO WS-LCRE-CLISTD-TOT-ED.
            DISPLAY WS-LCRE-CLISTD-ED.
            DISPLAY WS-LCRE-MVTS-ED.
            MOVE WS-CERR               TO WS-LCRE-ANOM-TOT-ED.
            DISPLAY WS-LCRE-ANOM-ED.
            MOVE WS-CRET               TO WS-LCRE-RET-TOT-ED.
            DISPLAY WS-LCRE-RET-ED.
            MOVE WS-CCB                TO WS-LCRE-CBS-TOT-ED.
            DISPLAY WS-LCRE-CBS-ED.
            MOVE WS-CDEP               TO WS-LCRE-DEP-TOT-ED.
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
      *    EXIT.
      *
      *---------------------------------------------------------------*
      *   9999-  : PROTECTION FIN DE PROGRAMME                        *
      *---------------------------------------------------------------*
      *
       9999-FIN-PROGRAMME-DEB.
      *
            DISPLAY '*===========================================*'.
            DISPLAY '*     FIN NORMALE DU PROGRAMME ARIO326      *'.
            DISPLAY '*===========================================*'.
      *
       9999-FIN-PROGRAMME-FIN.
            EXIT.
      *
       9999-ERREUR-PROGRAMME-DEB.
      *
            DISPLAY '*===========================================*'.
            DISPLAY '*        UNE ANOMALIE A ETE DETECTEE        *'.
            DISPLAY '*     FIN ANORMALE DU PROGRAMME ARIO326     *'.
            DISPLAY '*===========================================*'.
            MOVE 12 TO RETURN-CODE.
      *
       9999-ERREUR-PROGRAMME-FIN.
            STOP RUN.
