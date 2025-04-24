      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : XXXXXXXX                                  *
      *  NOM DU REDACTEUR : GIGON                             *
      *  SOCIETE          : XXXXXXXX                                  *
      *  DATE DE CREATION : JJ/MM/SSAA                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      *                                                               *
      *                                                               *
      *---------------------------------------------------------------*
      *--               HISTORIQUE DES MODIFICATIONS                --*
      *---------------------------------------------------------------*
      * DATE  MODIF   !          NATURE DE LA MODIFICATION            *
      *---------------------------------------------------------------*
      * JJ/MM/SSAA    !                                               *
      *               !                                               *
      *===============================================================*
      *
      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID.      SQUELETTE.
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
      *                      XXXXXXX : FICHIER DES XXXXX
      *                      -------------------------------------------
           SELECT  XXXXXXXXXXXX        ASSIGN TO XXXXXXX
                   FILE STATUS         IS WS-FS-XXXXXXX.
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
       FD  XXXXXXXXXXXX
           DATA RECORD IS XXXXXXXX.
       01  XXXXXXXXXXXX.
      *
      *========================
       WORKING-STORAGE SECTION.
      *========================
       77  WS-FS-XXXXXXX    PIC X(2).
      *
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
      *               DESCRIPTION DU COMPOSANT PROGRAMME              *
      *               ==================================              *
      *---------------------------------------------------------------*
      *
       0000-PROGRAMME-DEB.
      *
      *
      *
           PERFORM  8999-STATISTIQUES-DEB
              THRU  8999-STATISTIQUES-FIN
      *
           PERFORM  9999-FIN-PROGRAMME-DEB
              THRU  9999-FIN-PROGRAMME-FIN.
      *
       0000-PROGRAMME-FIN.
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
      *6000-ORDRE-FICHIER-DEB.
      *
      *6000-ORDRE-FICHIER-FIN.
      *    EXIT.
      *
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *
      *7000-ORDRE-CALCUL-DEB.
      *
      *7000-ORDRE-CALCUL-FIN.
      *    EXIT.
      *
      *---------------------------------------------------------------*
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *---------------------------------------------------------------*
      *
      *8000-ORDRE-EDITION-DEB.
      *
      *8000-ORDRE-EDITION-FIN.
      *    EXIT.
      *
       8999-STATISTIQUES-DEB.
      *
            DISPLAY '************************************************'
            DISPLAY '*     STATISTIQUES DU PROGRAMME XXXXXXXX       *'
            DISPLAY '*     ==================================       *'
            DISPLAY '************************************************'.
      *
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
      *    EXIT.
      *
      *---------------------------------------------------------------*
      *   9999-  : PROTECTION FIN DE PROGRAMME                        *
      *---------------------------------------------------------------*
      *
       9999-FIN-PROGRAMME-DEB.
      *
            DISPLAY '*==============================================*'.
            DISPLAY '*     FIN NORMALE DU PROGRAMME XXXXXXXX        *'.
            DISPLAY '*==============================================*'.
      *
       9999-FIN-PROGRAMME-FIN.
            STOP RUN.
      *
       9999-ERREUR-PROGRAMME-DEB.
      *
            DISPLAY '*==============================================*'.
            DISPLAY '*        UNE ANOMALIE A ETE DETECTEE           *'.
            DISPLAY '*     FIN ANORMALE DU PROGRAMME XXXXXXXX       *'.
            DISPLAY '*==============================================*'.
            MOVE 12 TO RETURN-CODE.
      *
       9999-ERREUR-PROGRAMME-FIN.
            STOP RUN.
