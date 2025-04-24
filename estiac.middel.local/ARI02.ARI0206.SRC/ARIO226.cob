      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIO226                                   *
      *  NOM DU REDACTEUR : GIGON                                     *
      *  SOCIETE          : ESTIAC                                    *
      *  DATE DE CREATION : 24/02/2025                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      *  A PARTIR D'UN FICHIER DE MOUVEMENTS BANCAIRE, ON VEUT        *
      *  AFFICHER LES MOUVEMENTS EN ERREURS ET LES MOUVEMENTS PAR     *
      *  CLIENT DANS DES FICHIERS D'IMPRESSION ET UN RECAPITULATIF    *
      *  DES MOUVEMENTS                                               *
      *---------------------------------------------------------------*
      *--               HISTORIQUE DES MODIFICATIONS                --*
      *---------------------------------------------------------------*
      * DATE  MODIF   !          NATURE DE LA MODIFICATION            *
      *---------------------------------------------------------------*
      * 24/02/2025    !  CREATION PROGRAMME                           *
      *               !                                               *
      *===============================================================*
      *
      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID.      ARIO226.
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
      *                      -------------------------------------------
      *                      F-MVTS-E : FICHIER DES MOUVEMENTS
      *                      -------------------------------------------
           SELECT  F-MVTS-E            ASSIGN TO INP001
                   FILE STATUS         IS WS-FS-MVTS-E.
      *                      -------------------------------------------
      *                      F-ETATCLI-S : FICHIER ETAT CLIENT
      *                      -------------------------------------------
           SELECT  F-ETATCLI-S         ASSIGN TO ETATCLI
                   FILE STATUS         IS WS-FS-ETATCLI-S.
      *                      -------------------------------------------
      *                      F-ETATANO-S : FICHIER ETAT DES ANOMALIES
      *                      -------------------------------------------
           SELECT  F-ETATANO-S         ASSIGN TO ETATANO
                   FILE STATUS         IS WS-FS-ETATANO-S.
      *
      *
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
           RECORDING MODE IS F.
       01  FS-ENRG-MVTS-E          PIC X(50).
      *
       FD  F-ETATCLI-S
           RECORDING MODE IS F.
       01  FS-ENRG-ETATCLI-S       PIC X(80).
      *
       FD  F-ETATANO-S
           RECORDING MODE IS F.
       01  FS-ENRG-ETATANO-S       PIC X(80).
      *
      *========================
       WORKING-STORAGE SECTION.
      *========================
      *
      *---------------- ENREGISTREMENT F-MVTS-E ----------------------*
      *
       01  WS-FS-MVTS-E            PIC XX.
       01  WS-ENRG-F-MVTS.
           05  WS-MVTS-CPTE        PIC 9(10).
           05  WS-MVTS-DATE.
               10  WS-MVTS-ANNEE.
                   15  WS-MVTS-SS  PIC 99.
                   15  WS-MVTS-AA  PIC 99.
               10  WS-MVTS-MM      PIC 99.
               10  WS-MVTS-JJ      PIC 99.
           05  WS-MVTS-CODE        PIC X.
           05  WS-MVTS-MT          PIC 9(8)V99.
           05  FILLER              PIC X(21).
      *
      *---------------- FICHIER ETAT CLIENT --------------------------*
      *
       01  WS-FS-ETATCLI-S          PIC XX.
       01  WS-LETAT-ASTER           PIC X(78)         VALUE ALL '*'.
       01  WS-LETAT-ENT.
           05  FILLER               PIC X(21)
               VALUE '* NUMERO DE COMPTE : '.
           05  WS-LETAT-AST-CPTE-ED PIC 9(10).
           05  FILLER               PIC X(32)         VALUE ALL SPACE.
           05  FILLER               PIC X(3)          VALUE 'LE '.
           05  WS-LETAT-AST-DATE-ED PIC X(10).
           05  FILLER               PIC XX            VALUE ' *'.
      *
       01  WS-LETAT-TITRE.
           05  FILLER               PIC XX            VALUE '* '.
           05  FILLER               PIC X(7)          VALUE 'LIBELLE'.
           05  FILLER               PIC X(34)         VALUE ALL SPACE.
           05  FILLER               PIC X(7)          VALUE '*      '.
           05  FILLER               PIC X(5)          VALUE 'DEBIT'.
           05  FILLER               PIC X(6)          VALUE '     *'.
           05  FILLER               PIC X(5)          VALUE ALL SPACE.
           05  FILLER               PIC X(6)          VALUE 'CREDIT'.
           05  FILLER               PIC X(6)          VALUE '     *'.
      *
       01  WS-LETAT-DETAIL.
           05  FILLER               PIC XX            VALUE '* '.
           05  WS-LETAT-DET-MVT-ED  PIC X(13).
           05  FILLER               PIC X(28)         VALUE ALL SPACE.
           05  FILLER               PIC X(5)          VALUE '*    '.
           05  WS-LETAT-DET-MTDB-ED PIC ZZZZZZZ9,99   VALUE ZERO
               BLANK WHEN ZERO.
           05  FILLER               PIC X(6)          VALUE ' *    '.
           05  WS-LETAT-DET-MTCR-ED PIC ZZZZZZZ9,99   VALUE ZERO
               BLANK WHEN ZERO.
           05  FILLER               PIC XX            VALUE ' *'.
      *
       01  WS-LETAT-TOTAL.
           05  FILLER               PIC X(7)          VALUE '* TOTAL'.
           05  FILLER               PIC X(36)         VALUE ALL SPACE.
           05  FILLER               PIC X(3)          VALUE '*  '.
           05  WS-LETAT-TOT-MTDB-ED PIC ZZZZZZZZZ9,99 VALUE ZERO
               BLANK WHEN ZERO.
           05  FILLER               PIC X(4)          VALUE ' *  '.
           05  WS-LETAT-TOT-MTCR-ED PIC ZZZZZZZZZ9,99 VALUE ZERO
               BLANK WHEN ZERO.
           05  FILLER               PIC XX            VALUE ' *'.
      *----------- VARIABLES CUMULES DEBIT / CREDIT ------------------*
       01  WS-CDBT                  PIC 9(10)V99.
       01  WS-CCDT                  PIC 9(10)V99.
      *
      *---------------- FICHIER ETAT DES ANOMALIES -------------------*
      *
       01  WS-FS-ETATANO-S          PIC XX.
       01  WS-LANO-L1.
           05  FILLER               PIC X             VALUE ALL '*'.
           05  FILLER               PIC X(53)         VALUE ALL '-'.
           05  FILLER               PIC X             VALUE ALL '*'.
      *
       01  WS-LANO-TITRE.
           05  FILLER               PIC X(3)          VALUE '|  '.
           05  FILLER               PIC X(9)          VALUE 'NR COMPTE'.
           05  FILLER               PIC X(5)          VALUE '  |  '.
           05  FILLER               PIC X(14)
               VALUE 'CODE MOUVEMENT'.
           05  FILLER               PIC X(7)          VALUE '  |    '.
           05  FILLER               PIC X(7)          VALUE 'MONTANT'.
           05  FILLER               PIC X(10)
               VALUE '         |'.
      *
       01  WS-LANO-L3.
           05  FILLER               PIC X             VALUE ALL '|'.
           05  FILLER               PIC X(53)         VALUE ALL '-'.
           05  FILLER               PIC X             VALUE ALL '|'.
      *
       01  WS-LANO-DETAIL.
           05  FILLER               PIC XX            VALUE '| '.
           05  WS-LANO-DET-CPT-ED   PIC 9(10).
           05  FILLER               PIC X(3)          VALUE ALL '  |'.
           05  FILLER               PIC X(8)          VALUE ALL SPACE.
           05  WS-LANO-DET-MVT-ED   PIC X.
           05  FILLER               PIC X(9)          VALUE ALL SPACE.
           05  FILLER               PIC X(7)
               VALUE ALL '|      '.
           05  WS-LANO-DET-MT-ED    PIC ZZZZZZZ9,99.
           05  FILLER               PIC X(4)          VALUE ALL '   |'.
      *
       01  WS-LANO-TOTAL.
           05  FILLER               PIC XX            VALUE '| '.
           05  FILLER               PIC X(31)
               VALUE 'MONTANT TOTAL DES ANOMALIES    '.
           05  FILLER               PIC X(5)
               VALUE ALL '|    '.
           05  WS-LANO-TOT-MT-ED    PIC ZZZZZZZZZ9,99
               BLANK WHEN ZERO.
           05  FILLER               PIC X(4)          VALUE ALL '   |'.
      *
      *---------------- COMPTE RENDU D'EXECUTION ---------------------*
      *
       01  WS-LCRE-ASTER            PIC X(45)         VALUE ALL '*'.
      *
       01  WS-LCRE-TITRE.
           05  FILLER               PIC X(5)          VALUE '*    '.
           05  FILLER               PIC X(34)
               VALUE 'COMPTE RENDU D''EXECUTION (ARIO226)'.
           05  FILLER               PIC X(6)          VALUE '     *'.
      *
       01  WS-LCRE-DETAIL.
           05  FILLER               PIC X(3)          VALUE '*  '.
           05  WS-LCRE-DET-LIB-ED   PIC X(28).
           05  FILLER               PIC X(6)          VALUE '  :   '.
           05  WS-LCRE-DET-TOT-ED   PIC ZZ9.
           05  FILLER               PIC X(5)          VALUE '    *'.
      *
      *---------------- VARIABLES DE TRAITEMENT ----------------------*
      *
       01  WS-CCLI                  PIC 9(3)          VALUE ZERO.
       01  WS-CMVT                  PIC 9(3)          VALUE ZERO.
       01  WS-CERR                  PIC 9(3)          VALUE ZERO.
       01  WS-CRET                  PIC 9(3)          VALUE ZERO.
       01  WS-CCB                   PIC 9(3)          VALUE ZERO.
       01  WS-CDEP                  PIC 9(3).
       01  WS-MVT-VALID             PIC 9             VALUE ZERO.
       01  WS-ANO-MVT-TOT           PIC 9(10)V99      VALUE ZERO.
       01  WS-BUFFER                PIC X(80).
       01  WS-R-MVT-CPTE            PIC 9(10).
       01  WS-DATE-US.
           05  WS-DATE-AAAA         PIC 9999.
           05  WS-DATE-MM           PIC 99.
           05  WS-DATE-JJ           PIC 99.
       01  WS-DATE-FR.
           05  WS-DATE-JJ           PIC 99.
           05  FILLER               PIC X             VALUE '/'.
           05  WS-DATE-MM           PIC 99.
           05  FILLER               PIC X             VALUE '/'.
           05  WS-DATE-AAAA         PIC 9999.
      *
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
      *   TRAITEMENT PRINCIPAL                                        *
      *---------------------------------------------------------------*
      *
       0000-TRT-PRINCIPAL-DEB.
      *
      *--------------- PREPARATION DU TRAITEMENT ---------------------*
      *
           PERFORM 6000-OPEN-FMVTS-DEB
              THRU 6000-OPEN-FMVTS-FIN.
      *
           PERFORM 6010-OPEN-FETATCLI-DEB
              THRU 6010-OPEN-FETATCLI-FIN.
      *
           PERFORM 6020-OPEN-FETATANO-DEB
              THRU 6020-OPEN-FETATANO-FIN.
      *
           PERFORM 6030-READ-FMVTS-DEB
              THRU 6030-READ-FMVTS-FIN.
      *
           IF WS-FS-MVTS-E = '10'
              PERFORM 8000-FICHIER-VIDE-DEB
                 THRU 8000-FICHIER-VIDE-FIN
           END-IF.
      *
      *--------------- APPEL DU COMPOSANT SUIVANT --------------------*
      *
           PERFORM 1000-TRT-COMPTE-DEB
              THRU 1000-TRT-COMPTE-FIN
             UNTIL WS-FS-MVTS-E = '10'.
      *
      *--------------- FIN DE TRAITEMENT -----------------------------*
      *
           IF WS-CERR NOT = 0
              PERFORM 7110-GEST-BOT-ETATANO-DEB
                 THRU 7110-GEST-BOT-ETATANO-FIN
              PERFORM 8060-EDIT-BOT-ETATANO-DEB
                 THRU 8060-EDIT-BOT-ETATANO-FIN
           END-IF.
      *
           PERFORM 7120-CALCUL-AP-PRINCIPAL-DEB
              THRU 7120-CALCUL-AP-PRINCIPAL-FIN.
      *
           PERFORM 8999-COMPTE-RENDU-EXEC-DEB
              THRU 8999-COMPTE-RENDU-EXEC-FIN.
      *
           PERFORM 6080-CLOSE-FMVTS-DEB
              THRU 6080-CLOSE-FMVTS-FIN.
      *
           PERFORM 6090-CLOSE-FETATCLI-DEB
              THRU 6090-CLOSE-FETATCLI-FIN.
      *
           PERFORM 6100-CLOSE-FETATANO-DEB
              THRU 6100-CLOSE-FETATANO-FIN.
      *
           PERFORM 9999-FIN-PROGRAMME-DEB
              THRU 9999-FIN-PROGRAMME-FIN.
      *
       0000-TRT-PRINCIPAL-FIN.
           STOP RUN.
      *
      *---------------------------------------------------------------*
      *   TRAITEMENT COMPTE                                           *
      *---------------------------------------------------------------*
      *
       1000-TRT-COMPTE-DEB.
      *
      *--------------- PREPARATION DU TRAITEMENT ---------------------*
      *
           PERFORM 7000-CALCUL-AV-COMPTE-DEB
              THRU 7000-CALCUL-AV-COMPTE-FIN.
      *
      *--------------- APPEL DU COMPOSANT SUIVANT --------------------*
      *
           PERFORM 2000-TRT-MOUVEMENT-DEB
              THRU 2000-TRT-MOUVEMENT-FIN
             UNTIL (WS-MVTS-CPTE NOT = WS-R-MVT-CPTE
                   OR WS-FS-MVTS-E = '10').
      *
      *--------------- FIN DE TRAITEMENT -----------------------------*
      *
           IF WS-MVT-VALID = 1
              PERFORM 7100-GEST-BOT-ETATCLI-DEB
                 THRU 7100-GEST-BOT-ETATCLI-FIN
              PERFORM 8030-EDIT-BOT-ETATCLI-DEB
                 THRU 8030-EDIT-BOT-ETATCLI-FIN
           END-IF.
      *
       1000-TRT-COMPTE-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   TRAITEMENT MOUVEMENT                                        *
      *---------------------------------------------------------------*
      *
       2000-TRT-MOUVEMENT-DEB.
      *
      *--------------- PREPARATION DU TRAITEMENT ---------------------*
      *
      *
      *--------------- APPEL DU COMPOSANT SUIVANT --------------------*
      *
           EVALUATE WS-MVTS-CODE
               WHEN 'R'   PERFORM 3000-TRT-RTRAIT-DEB
                             THRU 3000-TRT-RTRAIT-FIN
               WHEN 'C'   PERFORM 3010-TRT-CARTE-BLEUE-DEB
                             THRU 3010-TRT-CARTE-BLEUE-FIN
               WHEN 'D'   PERFORM 3020-TRT-DPOT-DEB
                             THRU 3020-TRT-DPOT-FIN
               WHEN OTHER PERFORM 3030-TRT-AUTRE-DEB
                             THRU 3030-TRT-AUTRE-FIN
           END-EVALUATE.
      *
      *--------------- FIN DE TRAITEMENT -----------------------------*
      *
           PERFORM 6030-READ-FMVTS-DEB
              THRU 6030-READ-FMVTS-FIN.
      *
       2000-TRT-MOUVEMENT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   TRAITEMENT RETRAIT                                           *
      *---------------------------------------------------------------*
      *
       3000-TRT-RTRAIT-DEB.
           PERFORM 7010-CALCUL-RTRAIT-DEB
              THRU 7010-CALCUL-RTRAIT-FIN.
      *
           IF WS-MVT-VALID NOT = 1
              PERFORM 7020-GEST-TOP-ETATCLI-DEB
                 THRU 7020-GEST-TOP-ETATCLI-FIN
              PERFORM 8010-EDIT-TOP-ETATCLI-DEB
                 THRU 8010-EDIT-TOP-ETATCLI-FIN
           END-IF.
      *
           PERFORM 7030-GEST-LIGNE-ETATCLI-RT-DEB
              THRU 7030-GEST-LIGNE-ETATCLI-RT-FIN.
           PERFORM 8020-EDIT-LIGNE-ETATCLI-DEB
              THRU 8020-EDIT-LIGNE-ETATCLI-FIN.
      *
       3000-TRT-RTRAIT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   TRAITEMENT CARTE BLEUE                                      *
      *---------------------------------------------------------------*
      *
       3010-TRT-CARTE-BLEUE-DEB.
           PERFORM 7040-CALCUL-CARTE-BLEUE-DEB
              THRU 7040-CALCUL-CARTE-BLEUE-FIN.
      *
           IF WS-MVT-VALID NOT = 1
              PERFORM 7020-GEST-TOP-ETATCLI-DEB
                 THRU 7020-GEST-TOP-ETATCLI-FIN
              PERFORM 8010-EDIT-TOP-ETATCLI-DEB
                 THRU 8010-EDIT-TOP-ETATCLI-FIN
           END-IF.
      *
           PERFORM 7050-GEST-LIGNE-ETATCLI-CB-DEB
              THRU 7050-GEST-LIGNE-ETATCLI-CB-FIN.
           PERFORM 8020-EDIT-LIGNE-ETATCLI-DEB
              THRU 8020-EDIT-LIGNE-ETATCLI-FIN.
      *
       3010-TRT-CARTE-BLEUE-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   TRAITEMENT DEPOT                                             *
      *---------------------------------------------------------------*
      *
       3020-TRT-DPOT-DEB.
           PERFORM 7060-CALCUL-DPOT-DEB
              THRU 7060-CALCUL-DPOT-FIN.
      *
           IF WS-MVT-VALID NOT = 1
              PERFORM 7020-GEST-TOP-ETATCLI-DEB
                 THRU 7020-GEST-TOP-ETATCLI-FIN
              PERFORM 8010-EDIT-TOP-ETATCLI-DEB
                 THRU 8010-EDIT-TOP-ETATCLI-FIN
           END-IF.
      *
           PERFORM 7070-GEST-LIGNE-ETATCLI-DP-DEB
              THRU 7070-GEST-LIGNE-ETATCLI-DP-FIN.
           PERFORM 8020-EDIT-LIGNE-ETATCLI-DEB
              THRU 8020-EDIT-LIGNE-ETATCLI-FIN.
      *
       3020-TRT-DPOT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   TRAITEMENT AUTRE                                            *
      *---------------------------------------------------------------*
      *
       3030-TRT-AUTRE-DEB.
           IF WS-CERR = 0
              PERFORM 8040-EDIT-TOP-ETATANO-DEB
                 THRU 8040-EDIT-TOP-ETATANO-FIN
           END-IF.
      *
           PERFORM 7080-CALCUL-AUTRE-DEB
              THRU 7080-CALCUL-AUTRE-FIN.
      *
           PERFORM 7090-GEST-LIGNE-ETATANO-DEB
              THRU 7090-GEST-LIGNE-ETATANO-FIN.
      *
           PERFORM 8050-EDIT-LIGNE-ETATANO-DEB
              THRU 8050-EDIT-LIGNE-ETATANO-FIN.
      *
       3030-TRT-AUTRE-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   6XXX-  : ORDRES DE MANIPULATION DES FICHIERS                *
      *---------------------------------------------------------------*
      *                                                               *
       6000-OPEN-FMVTS-DEB.
           OPEN INPUT F-MVTS-E.
           IF WS-FS-MVTS-E NOT = '00'
              DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER F-MVTS-E'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-MVTS-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6000-OPEN-FMVTS-FIN.
           EXIT.
      *
       6010-OPEN-FETATCLI-DEB.
           OPEN OUTPUT F-ETATCLI-S.
           IF WS-FS-ETATCLI-S NOT = '00'
              DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER F-ETATCLI-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATCLI-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6010-OPEN-FETATCLI-FIN.
           EXIT.
      *
       6020-OPEN-FETATANO-DEB.
           OPEN OUTPUT F-ETATANO-S.
           IF WS-FS-ETATANO-S NOT = '00'
              DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER F-ETATANO-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6020-OPEN-FETATANO-FIN.
           EXIT.
      *
       6030-READ-FMVTS-DEB.
           READ F-MVTS-E INTO WS-ENRG-F-MVTS.
           IF NOT (WS-FS-MVTS-E = '00' OR '10')
              DISPLAY 'PROBLEME DE LECTURE DU FICHIER F-MVTS-E'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-MVTS-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6030-READ-FMVTS-FIN.
           EXIT.
      *
       6040-WRITE-ETATCLI-NEWPAGE-DEB.
           WRITE FS-ENRG-ETATCLI-S FROM WS-BUFFER AFTER PAGE.
           IF WS-FS-ETATCLI-S NOT = '00'
              DISPLAY 'PROBLEME SAUT DE PAGE DU FICHIER F-ETATCLI-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATCLI-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6040-WRITE-ETATCLI-NEWPAGE-FIN.
           EXIT.
      *
       6050-WRITE-ETATCLI-DEB.
           WRITE FS-ENRG-ETATCLI-S FROM WS-BUFFER.
           IF WS-FS-ETATCLI-S NOT = '00'
              DISPLAY 'PROBLEME D''ECRITURE DU FICHIER F-ETATCLI-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATCLI-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6050-WRITE-ETATCLI-FIN.
           EXIT.
      *
       6060-WRITE-ETATANO-NEWPAGE-DEB.
           WRITE FS-ENRG-ETATANO-S FROM WS-BUFFER AFTER PAGE.
           IF WS-FS-ETATANO-S NOT = '00'
              DISPLAY 'PROBLEME SAUT DE PAGE DU FICHIER F-ETATANO-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6060-WRITE-ETATANO-NEWPAGE-FIN.
           EXIT.
      *
       6070-WRITE-ETATANO-DEB.
           WRITE FS-ENRG-ETATANO-S FROM WS-BUFFER.
           IF WS-FS-ETATANO-S NOT = '00'
              DISPLAY 'PROBLEME D''ECRITURE DU FICHIER F-ETATANO-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6070-WRITE-ETATANO-FIN.
           EXIT.
      *
       6080-CLOSE-FMVTS-DEB.
           CLOSE F-MVTS-E.
           IF WS-FS-MVTS-E NOT = '00'
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER F-MVTS-E'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-MVTS-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6080-CLOSE-FMVTS-FIN.
           EXIT.
      *
       6090-CLOSE-FETATCLI-DEB.
           CLOSE F-ETATCLI-S.
           IF WS-FS-ETATCLI-S NOT = '00'
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER F-ETATCLI-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATCLI-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6090-CLOSE-FETATCLI-FIN.
           EXIT.
      *
       6100-CLOSE-FETATANO-DEB.
           CLOSE F-ETATANO-S.
           IF WS-FS-ETATANO-S NOT = '00'
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER F-ETATANO-S'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-ETATANO-S
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6100-CLOSE-FETATANO-FIN.
           EXIT.

      *
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *
       7000-CALCUL-AV-COMPTE-DEB.
           MOVE WS-MVTS-CPTE                   TO WS-R-MVT-CPTE.
           MOVE ZERO                           TO WS-CDBT
                                                  WS-CCDT
                                                  WS-MVT-VALID
           ADD 1                               TO WS-CCLI.
       7000-CALCUL-AV-COMPTE-FIN.
           EXIT.
      *
       7010-CALCUL-RTRAIT-DEB.
           ADD WS-MVTS-MT                      TO WS-CDBT.
           ADD 1                               TO WS-CRET.
       7010-CALCUL-RTRAIT-FIN.
           EXIT.
      *
       7020-GEST-TOP-ETATCLI-DEB.
           MOVE 1                              TO WS-MVT-VALID.
           MOVE WS-MVTS-CPTE                   TO WS-LETAT-AST-CPTE-ED.
           ACCEPT WS-DATE-US                   FROM DATE YYYYMMDD.
           MOVE CORR WS-DATE-US                TO WS-DATE-FR.
           MOVE WS-DATE-FR                     TO WS-LETAT-AST-DATE-ED.
       7020-GEST-TOP-ETATCLI-FIN.
           EXIT.
      *
       7030-GEST-LIGNE-ETATCLI-RT-DEB.
           MOVE 'RETRAIT DAB'                  TO WS-LETAT-DET-MVT-ED.
           MOVE WS-MVTS-MT                     TO WS-LETAT-DET-MTDB-ED.
           MOVE 0                              TO WS-LETAT-DET-MTCR-ED.
       7030-GEST-LIGNE-ETATCLI-RT-FIN.
           EXIT.
      *
       7040-CALCUL-CARTE-BLEUE-DEB.
           ADD WS-MVTS-MT                      TO WS-CDBT.
           ADD 1                               TO WS-CCB.
       7040-CALCUL-CARTE-BLEUE-FIN.
           EXIT.
      *
       7050-GEST-LIGNE-ETATCLI-CB-DEB.
           MOVE 'CARTE BLEUE'                  TO WS-LETAT-DET-MVT-ED.
           MOVE WS-MVTS-MT                     TO WS-LETAT-DET-MTDB-ED.
           MOVE 0                              TO WS-LETAT-DET-MTCR-ED.
       7050-GEST-LIGNE-ETATCLI-CB-FIN.
           EXIT.
      *
       7060-CALCUL-DPOT-DEB.
           ADD WS-MVTS-MT                      TO WS-CCDT.
           ADD 1                               TO WS-CDEP.
       7060-CALCUL-DPOT-FIN.
           EXIT.
      *
       7070-GEST-LIGNE-ETATCLI-DP-DEB.
           MOVE 'DEPOT GUICHET'                TO WS-LETAT-DET-MVT-ED.
           MOVE WS-MVTS-MT                     TO WS-LETAT-DET-MTCR-ED.
           MOVE 0                              TO WS-LETAT-DET-MTDB-ED.
       7070-GEST-LIGNE-ETATCLI-DP-FIN.
           EXIT.
      *
       7080-CALCUL-AUTRE-DEB.
           ADD 1                               TO WS-CERR.
           ADD WS-MVTS-MT                      TO WS-ANO-MVT-TOT.
       7080-CALCUL-AUTRE-FIN.
           EXIT.
      *
       7090-GEST-LIGNE-ETATANO-DEB.
           MOVE WS-MVTS-CPTE                   TO WS-LANO-DET-CPT-ED.
           MOVE WS-MVTS-CODE                   TO WS-LANO-DET-MVT-ED.
           MOVE WS-MVTS-MT                     TO WS-LANO-DET-MT-ED.
       7090-GEST-LIGNE-ETATANO-FIN.
           EXIT.
      *
       7100-GEST-BOT-ETATCLI-DEB.
           MOVE WS-CDBT                        TO WS-LETAT-TOT-MTDB-ED.
           MOVE WS-CCDT                        TO WS-LETAT-TOT-MTCR-ED.
       7100-GEST-BOT-ETATCLI-FIN.
           EXIT.
      *
       7110-GEST-BOT-ETATANO-DEB.
           MOVE WS-ANO-MVT-TOT                 TO WS-LANO-TOT-MT-ED.
       7110-GEST-BOT-ETATANO-FIN.
           EXIT.
      *
       7120-CALCUL-AP-PRINCIPAL-DEB.
           COMPUTE WS-CMVT = (WS-CRET + WS-CCB + WS-CDEP + WS-CERR).
       7120-CALCUL-AP-PRINCIPAL-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITS                   *
      *---------------------------------------------------------------*
      *
       8000-FICHIER-VIDE-DEB.
           DISPLAY 'FICHIER F-MVTS-E VIDE'.
       8000-FICHIER-VIDE-FIN.
           EXIT.
      *
       8010-EDIT-TOP-ETATCLI-DEB.
           MOVE WS-LETAT-ASTER                  TO WS-BUFFER.
           PERFORM 6040-WRITE-ETATCLI-NEWPAGE-DEB
              THRU 6040-WRITE-ETATCLI-NEWPAGE-FIN.
           MOVE WS-LETAT-ENT                    TO WS-BUFFER.
           PERFORM 6050-WRITE-ETATCLI-DEB
              THRU 6050-WRITE-ETATCLI-FIN.
           MOVE WS-LETAT-ASTER                  TO WS-BUFFER.
           PERFORM 6050-WRITE-ETATCLI-DEB
              THRU 6050-WRITE-ETATCLI-FIN.
           MOVE WS-LETAT-TITRE                  TO WS-BUFFER.
           PERFORM 6050-WRITE-ETATCLI-DEB
              THRU 6050-WRITE-ETATCLI-FIN.
           MOVE WS-LETAT-ASTER                  TO WS-BUFFER.
           PERFORM 6050-WRITE-ETATCLI-DEB
              THRU 6050-WRITE-ETATCLI-FIN.
       8010-EDIT-TOP-ETATCLI-FIN.
           EXIT.
      *
       8020-EDIT-LIGNE-ETATCLI-DEB.
           MOVE WS-LETAT-DETAIL                 TO WS-BUFFER.
           PERFORM 6050-WRITE-ETATCLI-DEB
              THRU 6050-WRITE-ETATCLI-FIN.
       8020-EDIT-LIGNE-ETATCLI-FIN.
           EXIT.
      *
       8030-EDIT-BOT-ETATCLI-DEB.
           MOVE WS-LETAT-ASTER                  TO WS-BUFFER.
           PERFORM 6050-WRITE-ETATCLI-DEB
              THRU 6050-WRITE-ETATCLI-FIN.
           MOVE WS-LETAT-TOTAL                  TO WS-BUFFER.
           PERFORM 6050-WRITE-ETATCLI-DEB
              THRU 6050-WRITE-ETATCLI-FIN.
           MOVE WS-LETAT-ASTER                  TO WS-BUFFER.
           PERFORM 6050-WRITE-ETATCLI-DEB
              THRU 6050-WRITE-ETATCLI-FIN.
       8030-EDIT-BOT-ETATCLI-FIN.
           EXIT.
      *
       8040-EDIT-TOP-ETATANO-DEB.
           MOVE WS-LANO-L1                      TO WS-BUFFER.
           PERFORM 6060-WRITE-ETATANO-NEWPAGE-DEB
              THRU 6060-WRITE-ETATANO-NEWPAGE-FIN.
           MOVE WS-LANO-TITRE                   TO WS-BUFFER.
           PERFORM 6070-WRITE-ETATANO-DEB
              THRU 6070-WRITE-ETATANO-FIN.
           MOVE WS-LANO-L3                      TO WS-BUFFER.
           PERFORM 6070-WRITE-ETATANO-DEB
              THRU 6070-WRITE-ETATANO-FIN.
       8040-EDIT-TOP-ETATANO-FIN.
           EXIT.
      *
       8050-EDIT-LIGNE-ETATANO-DEB.
           MOVE WS-LANO-DETAIL                  TO WS-BUFFER.
           PERFORM 6070-WRITE-ETATANO-DEB
              THRU 6070-WRITE-ETATANO-FIN.
       8050-EDIT-LIGNE-ETATANO-FIN.
           EXIT.
      *
       8060-EDIT-BOT-ETATANO-DEB.
           MOVE WS-LANO-L3                      TO WS-BUFFER.
           PERFORM 6070-WRITE-ETATANO-DEB
              THRU 6070-WRITE-ETATANO-FIN.
           MOVE WS-LANO-TOTAL                   TO WS-BUFFER.
           PERFORM 6070-WRITE-ETATANO-DEB
              THRU 6070-WRITE-ETATANO-FIN.
           MOVE WS-LANO-L1                      TO WS-BUFFER.
           PERFORM 6070-WRITE-ETATANO-DEB
              THRU 6070-WRITE-ETATANO-FIN.
       8060-EDIT-BOT-ETATANO-FIN.
           EXIT.
      *
       8999-COMPTE-RENDU-EXEC-DEB.
           DISPLAY WS-LCRE-ASTER.
           DISPLAY WS-LCRE-TITRE.
           DISPLAY WS-LCRE-ASTER.
           MOVE 'NOMBRE DE CLIENTS'            TO WS-LCRE-DET-LIB-ED.
           MOVE WS-CCLI                        TO WS-LCRE-DET-TOT-ED.
           DISPLAY WS-LCRE-DETAIL.
           MOVE 'NOMBRE DE MOUVEMENTS'         TO WS-LCRE-DET-LIB-ED.
           MOVE WS-CMVT                        TO WS-LCRE-DET-TOT-ED.
           DISPLAY WS-LCRE-DETAIL.
           MOVE 'NOMBRE DE MOUVEMENTS ERRONES' TO WS-LCRE-DET-LIB-ED.
           MOVE WS-CERR                        TO WS-LCRE-DET-TOT-ED.
           DISPLAY WS-LCRE-DETAIL.
           MOVE 'NOMBRE DE RETRAITS'            TO WS-LCRE-DET-LIB-ED.
           MOVE WS-CRET                         TO WS-LCRE-DET-TOT-ED.
           DISPLAY WS-LCRE-DETAIL.
           MOVE 'NOMBRE DE CARTES BLEUES'       TO WS-LCRE-DET-LIB-ED.
           MOVE WS-CCB                          TO WS-LCRE-DET-TOT-ED.
           DISPLAY WS-LCRE-DETAIL.
           MOVE 'NOMBRE DE DEPOTS'              TO WS-LCRE-DET-LIB-ED.
           MOVE WS-CDEP                         TO WS-LCRE-DET-TOT-ED.
           DISPLAY WS-LCRE-DETAIL.
           DISPLAY WS-LCRE-ASTER.
       8999-COMPTE-RENDU-EXEC-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   9XXX-  : ORDRES DE MANIPULATION DES SOUS-PROGRAMMES         *
      *---------------------------------------------------------------*
      *
      *
      *
      *---------------------------------------------------------------*
      *   9999-  : PROTECTION FIN DE PROGRAMME                        *
      *---------------------------------------------------------------*
      *
       9999-FIN-PROGRAMME-DEB.
      *
           DISPLAY '*===========================================*'.
           DISPLAY '*     FIN NORMALE DU PROGRAMME ARIO226      *'.
           DISPLAY '*===========================================*'.
      *
       9999-FIN-PROGRAMME-FIN.
           EXIT.
      *
       9999-ERREUR-PROGRAMME-DEB.
      *
           DISPLAY '*===========================================*'.
           DISPLAY '*        UNE ANOMALIE A ETE DETECTEE        *'.
           DISPLAY '*     FIN ANORMALE DU PROGRAMME ARIO226     *'.
           DISPLAY '*===========================================*'.
           MOVE 12 TO RETURN-CODE.
      *
       9999-ERREUR-PROGRAMME-FIN.
           STOP RUN.
