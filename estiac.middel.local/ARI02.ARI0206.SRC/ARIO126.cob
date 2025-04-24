      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIO126                                   *
      *  NOM DU REDACTEUR : GIGON                                     *
      *  SOCIETE          : ESTIAC                                    *
      *  DATE DE CREATION : 18/02/2025                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      *  A PARTIR D'UN FICHIER DE MOUVEMENTS BANCAIRE, ON VEUT        *
      *  AFFICHER LES MOUVEMENTS EN ERREURS, UN RECAPITULATIF         *
      *  DES MOUVEMENTS PAR CLIENT ET RECAPITULATIF DES MOUVEMENTS    *
      *---------------------------------------------------------------*
      *--               HISTORIQUE DES MODIFICATIONS                --*
      *---------------------------------------------------------------*
      * DATE  MODIF   !          NATURE DE LA MODIFICATION            *
      *---------------------------------------------------------------*
      * 18/02/2025    !  CREATION PROGRAMME                           *
      *               !                                               *
      *===============================================================*
      *
      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID.      ARIO126.
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
      *---------------- FICHIER NOMBRE EN ENTREE----------------------*
      * LONGUEUR ENREGISTREMENT = 50                                  *
      *---------------------------------------------------------------*
       FD  F-MVTS-E
           RECORDING MODE IS F.
      *
      *---------------- DESCRIPTION DE L'ENREGISTREMENT --------------*
       01  FS-ENRG-MVTS-E    PIC X(50).
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
      *---------------- VARIABLES ETAT D'OPERATIONS ------------------*
      *
       01  WS-LASTER               PIC X(45)      VALUE ALL '*'.
       01  WS-LTIRET               PIC X(45)      VALUE ALL '-'.
       01  WS-LCPTE.
           05  FILLER              PIC X(31)
               VALUE 'NUMERO DE COMPTE            :  '.
           05  WS-OCPT             PIC 9(10).
       01  WS-LCB.
           05  FILLER              PIC X(31)
               VALUE 'CUMUL CARTE-BLEUE           :  '.
           05  WS-OCB              PIC 9(10)V99    VALUE ZERO.
       01  WS-LRDAB.
           05  FILLER              PIC X(31)
               VALUE 'CUMUL RETRAIT DAB           :  '.
           05  WS-ORDAB            PIC 9(10)V99    VALUE ZERO.
       01  WS-LDGUI.
           05  FILLER              PIC X(31)
               VALUE 'CUMUL DEPOT GUICHET         :  '.
           05  WS-ODGUI            PIC 9(10)V99    VALUE ZERO.
       01  WS-LBAL.
           05  FILLER              PIC X(31)
               VALUE 'BALANCE DES OPERATIONS      :  '.
           05  WS-OBAL             PIC S9(10)V99.
      *
      *---------------- VARIABLES ETAT ERREURS -----------------------*
      *
       01  WS-LECPT.
           05  FILLER              PIC X(28)
               VALUE 'ERREUR POUR LE COMPTE    :  '.
           05  WS-ECPT             PIC 9(10).
       01  WS-LEMVT.
           05  FILLER              PIC X(28)
               VALUE 'CODE MOUVEMENT           :  '.
           05  WS-EMVT             PIC X.
       01  WS-LEMT.
           05  FILLER              PIC X(28)
               VALUE 'MONTANT                  :  '.
           05  WS-EMT              PIC 9(8)V99.
      *
      *---------------- VARIABLES COMPTE RENDU D'EXECUTION -----------*
      *
       01  WS-CCLI                 PIC 9(3)       VALUE ZERO.
       01  WS-CMVT                 PIC 9(3)       VALUE ZERO.
       01  WS-CERR                 PIC 9(3)       VALUE ZERO.
       01  WS-CRET                 PIC 9(3)       VALUE ZERO.
       01  WS-CCB                  PIC 9(3)       VALUE ZERO.
       01  WS-CDEP                 PIC 9(3).
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
           PERFORM 6010-READ-FMVTS-DEB
              THRU 6010-READ-FMVTS-FIN.
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
           PERFORM 7060-CALCUL-AP-PRINCIPAL-DEB
              THRU 7060-CALCUL-AP-PRINCIPAL-FIN.
      *
           PERFORM 8999-COMPTE-RENDU-EXEC-DEB
              THRU 8999-COMPTE-RENDU-EXEC-FIN.
      *
           PERFORM 6020-CLOSE-FMVTS-DEB
              THRU 6020-CLOSE-FMVTS-FIN.
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
             UNTIL (WS-MVTS-CPTE NOT = WS-OCPT
                   OR WS-FS-MVTS-E = '10').
      *
      *--------------- FIN DE TRAITEMENT -----------------------------*
      *
           IF NOT (WS-ORDAB = 0 AND WS-OCB = 0 AND WS-ODGUI = 0)
              PERFORM 7050-CALCUL-AP-COMPTE-DEB
                 THRU 7050-CALCUL-AP-COMPTE-FIN
              PERFORM 8020-ETAT-OPERATION-DEB
                 THRU 8020-ETAT-OPERATION-FIN
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
               WHEN 'R'   PERFORM 3000-TRT-RETRAIT-DEB
                             THRU 3000-TRT-RETRAIT-FIN
               WHEN 'C'   PERFORM 3010-TRT-CARTE-BLEUE-DEB
                             THRU 3010-TRT-CARTE-BLEUE-FIN
               WHEN 'D'   PERFORM 3020-TRT-DEPOT-DEB
                             THRU 3020-TRT-DEPOT-FIN
               WHEN OTHER PERFORM 3030-TRT-AUTRE-DEB
                             THRU 3030-TRT-AUTRE-FIN
           END-EVALUATE.
      *
      *--------------- FIN DE TRAITEMENT -----------------------------*
      *
           PERFORM 6010-READ-FMVTS-DEB
              THRU 6010-READ-FMVTS-FIN.
      *
       2000-TRT-MOUVEMENT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   TRAITEMENT RETRAIT                                          *
      *---------------------------------------------------------------*
      *
       3000-TRT-RETRAIT-DEB.
           PERFORM 7010-CALCUL-RETRAIT-DEB
              THRU 7010-CALCUL-RETRAIT-FIN.
       3000-TRT-RETRAIT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   TRAITEMENT CARTE BLEUE                                      *
      *---------------------------------------------------------------*
      *
       3010-TRT-CARTE-BLEUE-DEB.
           PERFORM 7020-CALCUL-CARTE-BLEUE-DEB
              THRU 7020-CALCUL-CARTE-BLEUE-FIN.
       3010-TRT-CARTE-BLEUE-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   TRAITEMENT DEPOT                                            *
      *---------------------------------------------------------------*
      *
       3020-TRT-DEPOT-DEB.
           PERFORM 7030-CALCUL-DEPOT-DEB
              THRU 7030-CALCUL-DEPOT-FIN.
       3020-TRT-DEPOT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   TRAITEMENT AUTRE                                            *
      *---------------------------------------------------------------*
      *
       3030-TRT-AUTRE-DEB.
           PERFORM 7040-GESTION-AUTRE-DEB
              THRU 7040-GESTION-AUTRE-FIN.
           PERFORM 8010-ETAT-ERREUR-DEB
              THRU 8010-ETAT-ERREUR-FIN.
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
       6010-READ-FMVTS-DEB.
           READ F-MVTS-E INTO WS-ENRG-F-MVTS.
           IF NOT (WS-FS-MVTS-E = '00' OR '10')
              DISPLAY 'PROBLEME DE LECTURE DU FICHIER F-MVTS-E'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-MVTS-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6010-READ-FMVTS-FIN.
           EXIT.
      *
       6020-CLOSE-FMVTS-DEB.
           CLOSE F-MVTS-E.
           IF WS-FS-MVTS-E NOT = '00'
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER F-MVTS-E'
              DISPLAY 'VALEUR DU FILE STATUS = ' WS-FS-MVTS-E
              PERFORM 9999-ERREUR-PROGRAMME-DEB
                 THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6020-CLOSE-FMVTS-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *
       7000-CALCUL-AV-COMPTE-DEB.
           MOVE WS-MVTS-CPTE TO WS-OCPT.
           MOVE ZERO         TO WS-OCB WS-ORDAB WS-ODGUI.
           ADD 1             TO WS-CCLI.
       7000-CALCUL-AV-COMPTE-FIN.
           EXIT.
      *
       7010-CALCUL-RETRAIT-DEB.
           ADD WS-MVTS-MT    TO WS-ORDAB.
           ADD 1             TO WS-CRET.
       7010-CALCUL-RETRAIT-FIN.
           EXIT.
      *
       7020-CALCUL-CARTE-BLEUE-DEB.
           ADD WS-MVTS-MT    TO WS-OCB.
           ADD 1             TO WS-CCB.
       7020-CALCUL-CARTE-BLEUE-FIN.
           EXIT.
      *
       7030-CALCUL-DEPOT-DEB.
           ADD WS-MVTS-MT    TO WS-ODGUI.
           ADD 1             TO WS-CDEP.
       7030-CALCUL-DEPOT-FIN.
           EXIT.
      *
       7040-GESTION-AUTRE-DEB.
           MOVE WS-MVTS-CPTE TO WS-ECPT.
           MOVE WS-MVTS-CODE TO WS-EMVT.
           MOVE WS-MVTS-MT   TO WS-EMT.
           ADD 1             TO WS-CERR.
       7040-GESTION-AUTRE-FIN.
           EXIT.
      *
       7050-CALCUL-AP-COMPTE-DEB.
           COMPUTE WS-OBAL = (WS-ODGUI - WS-ORDAB - WS-OCB).
       7050-CALCUL-AP-COMPTE-FIN.
           EXIT.
      *
       7060-CALCUL-AP-PRINCIPAL-DEB.
           COMPUTE WS-CMVT = (WS-CRET + WS-CCB + WS-CDEP + WS-CERR).
       7060-CALCUL-AP-PRINCIPAL-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *---------------------------------------------------------------*
      *
       8000-FICHIER-VIDE-DEB.
           DISPLAY 'FICHIER F-MVTS-E VIDE'.
       8000-FICHIER-VIDE-FIN.
           EXIT.
      *
       8010-ETAT-ERREUR-DEB.
           DISPLAY WS-LASTER.
           DISPLAY WS-LECPT.
           DISPLAY WS-LEMVT.
           DISPLAY WS-LEMT.
           DISPLAY WS-LASTER.
       8010-ETAT-ERREUR-FIN.
           EXIT.
      *
       8020-ETAT-OPERATION-DEB.
           DISPLAY WS-LASTER.
           DISPLAY WS-LCPTE.
           DISPLAY WS-LTIRET.
           DISPLAY WS-LCB.
           DISPLAY WS-LRDAB.
           DISPLAY WS-LDGUI.
           DISPLAY WS-LTIRET.
           DISPLAY WS-LBAL.
           DISPLAY WS-LASTER.
       8020-ETAT-OPERATION-FIN.
           EXIT.
      *
       8999-COMPTE-RENDU-EXEC-DEB.
           DISPLAY WS-LASTER.
           DISPLAY 'NOMBRE DE CLIENTS             :  ' WS-CCLI.
           DISPLAY 'NOMBRE DE MOUVEMENTS          :  ' WS-CMVT.
           DISPLAY 'NOMBRE DE MOUVEMENTS ERRONES  :  ' WS-CERR.
           DISPLAY 'NOMBRE DE RETRAITS            :  ' WS-CRET.
           DISPLAY 'NOMBRE DE CARTES BLEUES       :  ' WS-CCB.
           DISPLAY 'NOMBRE DE DEPOTS              :  ' WS-CDEP.
           DISPLAY WS-LASTER.
       8999-COMPTE-RENDU-EXEC-FIN.
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
           DISPLAY '*==============================================*'.
           DISPLAY '*     FIN NORMALE DU PROGRAMME ARIO126         *'.
           DISPLAY '*==============================================*'.
      *
       9999-FIN-PROGRAMME-FIN.
           EXIT.
      *
       9999-ERREUR-PROGRAMME-DEB.
      *
           DISPLAY '*==============================================*'.
           DISPLAY '*        UNE ANOMALIE A ETE DETECTEE           *'.
           DISPLAY '*     FIN ANORMALE DU PROGRAMME ARIO126        *'.
           DISPLAY '*==============================================*'.
           MOVE 12 TO RETURN-CODE.
      *
       9999-ERREUR-PROGRAMME-FIN.
           STOP RUN.
