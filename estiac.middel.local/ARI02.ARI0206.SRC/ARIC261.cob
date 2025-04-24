      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIC261                                   *
      *  NOM DU REDACTEUR : GIGON OSCAR                               *
      *  SOCIETE          : ESTIAC INSTITUT                           *
      *  DATE DE CREATION : 09/04/2025                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      *  L'ECRITURE DE CE PROGRAMME PERMETTRA : DE CONCEVOIR UN       *
      *  PROGRAMME DE GESTION D'UN MENU, DE METTRE EN OEUVRE LES      *
      *  ORDRES D'AFFICHAGE ET DE LECTURE DES MAPS, DE METTRE EN      *
      *  OEUVRE L'ORDRE D'AFFICHAGE DES DONNEES NON FORMATEES ET DE   *
      *  METTRE EN OEUVRE LES ORDRES DE GESTION DES PROGRAMMES.       *
      *---------------------------------------------------------------*
      *--               HISTORIQUE DES MODIFICATIONS                --*
      *---------------------------------------------------------------*
      * DATE  MODIF   !          NATURE DE LA MODIFICATION            *
      *---------------------------------------------------------------*
      * 09/04/2025    !  CREATION DU FICHIER ET DEBUT ECRITURE DU     *
      *               !  PROGRAMME                                    *
      *===============================================================*
      *
      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID.      ARIC261.
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
      *
      *=====================
       INPUT-OUTPUT SECTION.
      *=====================
      *
      *-------------
       FILE-CONTROL.
      *-------------
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
      *========================
       WORKING-STORAGE SECTION.
      *========================
      *
      * TEST DES TOUCHES FONCTION
           COPY DFHAID.
      * MODIFICATION DYNAMIQUE DES ATTRIBUTS DE MAP
           COPY DFHBMSCA.
      * TABLES DES MESSAGES
           COPY TABMSG.
      * WS ARIN261
           COPY ARIN261.
      * COMMAREA
           COPY COMMAREA.
      *
       01  WS-RC                            PIC S9(4) COMP.
       01  WS-RC-ED                         PIC X(10).
      *
       01  WS-ABSTIME                       PIC S9(15) COMP-3.
      *
       01  WS-CHOIX                         PIC X.
           88  CHOIX-VALIDE
               VALUE '1' '2' '3' '4' '5' '6'.
           88  CHOIX-1                      VALUE '1'.
           88  CHOIX-6                      VALUE '6'.
      *
       01  WS-MSG-VALIDE                    PIC X(80).
       01  WS-MSG-FIN                       PIC X(80).
      *
       01  WS-TABSPG.
           05  FILLER                       PIC X(8) VALUE 'ARIC262'.
           05  FILLER                       PIC X(8) VALUE 'ARIC263'.
           05  FILLER                       PIC X(8) VALUE 'ARIC264'.
           05  FILLER                       PIC X(8) VALUE 'ARIC265'.
           05  FILLER                       PIC X(8) VALUE 'ARIC266'.
           05  FILLER                       PIC X(8) VALUE 'ARIC267'.
       01  FILLER REDEFINES WS-TABSPG.
           05  WS-PROG                      PIC X(8) OCCURS 6.
      *
      *
      *================
       LINKAGE SECTION.
      *================
      *
       01  DFHCOMMAREA                  PIC X(4096).
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
      *               DESCRIPTION DU COMPOSANT PRINCIPAL              *
      *               ==================================              *
      *---------------------------------------------------------------*
      *
       0000-PRINCIPAL-DEB.
      *
      *-------------- DEBUT DU TRAITEMENT ----------------------------*
      *
      * INITIALISE ARIM261O A LOW VALUE
           PERFORM 7000-INIT-LV-DEB
              THRU 7000-INIT-LV-FIN.
      *
      *-------------- APPEL COMPOSANT --------------------------------*
      *
           EVALUATE TRUE
               WHEN EIBCALEN = 0
                 PERFORM 1000-1ERE-FOIS-DEB
                    THRU 1000-1ERE-FOIS-FIN
               WHEN LOOP-MENU
                 PERFORM 1010-MAP-MENU-DEB
                    THRU 1010-MAP-MENU-FIN
               WHEN LOOP-SPG
                 PERFORM 1020-APPEL-SOUS-PGM-DEB
                    THRU 1020-APPEL-SOUS-PGM-FIN
               WHEN OTHER
                 PERFORM 1030-ABEND-DEB
                    THRU 1030-ABEND-FIN
           END-EVALUATE.
      *
      *-------------- FIN DU TRAITEMENT ------------------------------*
      *
      * RETURN TRANSID
           PERFORM  9999-FIN-RTRANSID-DEB
              THRU  9999-FIN-RTRANSID-FIN.
      *
       0000-PRINCIPAL-FIN.
            STOP RUN.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT 1ERE FOIS              *
      *               ==================================              *
      *---------------------------------------------------------------*
      *
       1000-1ERE-FOIS-DEB.
      *
      *-------------- TRAITEMENT -------------------------------------*
      *
      * INITIALISE LA DATE ET LES NUMEROS : TERM, TASK TRANS
           PERFORM 7100-LV-0-DEB
              THRU 7100-LV-0-FIN.
           PERFORM 7010-INIT-DATE-NUM-DEB
              THRU 7010-INIT-DATE-NUM-FIN.
      *
           PERFORM 6000-SEND-ERASE-DEB
              THRU 6000-SEND-ERASE-FIN.
      *
       1000-1ERE-FOIS-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT MAP MENU               *
      *               =================================               *
      *---------------------------------------------------------------*
      *
       1010-MAP-MENU-DEB.
      *
      *-------------- TRAITEMENT -------------------------------------*
      *
           EVALUATE EIBAID
              WHEN DFHENTER
                 PERFORM 2000-ENTER-DEB
                    THRU 2000-ENTER-FIN
              WHEN DFHPF3
                 PERFORM 2010-F3-DEB
                    THRU 2010-F3-FIN
              WHEN DFHCLEAR
                 PERFORM 2020-ALT-C-DEB
                    THRU 2020-ALT-C-FIN
              WHEN OTHER
                 PERFORM 2030-AUTRE-DEB
                    THRU 2030-AUTRE-FIN
           END-EVALUATE.
      *
       1010-MAP-MENU-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT SOUS PGM               *
      *               =================================               *
      *---------------------------------------------------------------*
      *
       1020-APPEL-SOUS-PGM-DEB.
      *
      *-------------- TRAITEMENT -------------------------------------*
      *
           PERFORM 9000-APPEL-SPG-DEB
              THRU 9000-APPEL-SPG-FIN.
      *
       1020-APPEL-SOUS-PGM-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT ABEND                  *
      *               ==============================                  *
      *---------------------------------------------------------------*
      *
       1030-ABEND-DEB.
      *
      *-------------- TRAITEMENT -------------------------------------*
      *
           PERFORM 7080-MSG-ERR-AIG-DEB
              THRU 7080-MSG-ERR-AIG-FIN.
      *
           PERFORM 9999-ABEND-PRG-DEB
              THRU 9999-ABEND-PRG-FIN.
      *
       1030-ABEND-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT ENTER                  *
      *               ==============================                  *
      *---------------------------------------------------------------*
      *
       2000-ENTER-DEB.
      *
      *-------------- DEBUT DU TRAITEMENT ----------------------------*
      *
           PERFORM 6010-RECEIVE-DEB
              THRU 6010-RECEIVE-FIN.
      *
      *-------------- APPEL COMPOSANT --------------------------------*
      *
      * CHECK SI CHOIX EST VIDE
           IF WS-RC = DFHRESP(MAPFAIL)
              PERFORM 3000-CHOIX-VIDE-DEB
                 THRU 3000-CHOIX-VIDE-FIN
           ELSE
              PERFORM 3010-CHOIX-REMPLI-DEB
                 THRU 3010-CHOIX-REMPLI-FIN
           END-IF.
      *
      *-------------- FIN DU TRAITEMENT ------------------------------*
      *

      *
       2000-ENTER-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT F3                     *
      *               ===========================                     *
      *---------------------------------------------------------------*
      *
       2010-F3-DEB.
      *
      *-------------- TRAITEMENT -------------------------------------*
      *
           PERFORM 9999-FIN-PROGRAMME-DEB
              THRU 9999-FIN-PROGRAMME-FIN.
      *
       2010-F3-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT ALT C                  *
      *               ==============================                  *
      *---------------------------------------------------------------*
      *
       2020-ALT-C-DEB.
      *
      *-------------- TRAITEMENT -------------------------------------*
      *
           PERFORM 7010-INIT-DATE-NUM-DEB
              THRU 7010-INIT-DATE-NUM-FIN.
      *
           PERFORM 7020-MSG-ALT-C-DEB
              THRU 7020-MSG-ALT-C-FIN.
      *
           PERFORM 6020-SEND-DEB
              THRU 6020-SEND-FIN.
      *
       2020-ALT-C-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT AUTRE                  *
      *               ==============================                  *
      *---------------------------------------------------------------*
      *
       2030-AUTRE-DEB.
      *
      *-------------- TRAITEMENT -------------------------------------*
      *
            PERFORM 7030-MSG-AUTRE-DEB
               THRU 7030-MSG-AUTRE-FIN.
      *
            PERFORM 6040-SEND-ERASEAUP-DEB
               THRU 6040-SEND-ERASEAUP-FIN.
      *
       2030-AUTRE-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *            DESCRIPTION DU COMPOSANT CHOIX VIDE                *
      *            ===================================                *
      *---------------------------------------------------------------*
      *
       3000-CHOIX-VIDE-DEB.
      *
      *-------------- TRAITEMENT -------------------------------------*
      *
           PERFORM 7040-MSG-NON-REMPLI-DEB
              THRU 7040-MSG-NON-REMPLI-FIN.
      *
           PERFORM 6030-SEND-DATAONLY-DEB
              THRU 6030-SEND-DATAONLY-FIN.
      *
       3000-CHOIX-VIDE-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *            DESCRIPTION DU COMPOSANT CHOIX REMPLI              *
      *            =====================================              *
      *---------------------------------------------------------------*
      *
       3010-CHOIX-REMPLI-DEB.
      *
      *-------------- DEBUT DU TRAITEMENT ----------------------------*
      *
           PERFORM 7050-CHECK-MCHOIXI-DEB
              THRU 7050-CHECK-MCHOIXI-FIN.
      *
      *-------------- APPEL COMPOSANT --------------------------------*
      *
           IF CHOIX-VALIDE
              PERFORM 4000-CHOIX-VALIDE-DEB
                 THRU 4000-CHOIX-VALIDE-FIN
           ELSE
              PERFORM 4010-CHOIX-INVALIDE-DEB
                 THRU 4010-CHOIX-INVALIDE-FIN
           END-IF.
      *
       3010-CHOIX-REMPLI-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *            DESCRIPTION DU COMPOSANT CHOIX VALIDE              *
      *            =====================================              *
      *---------------------------------------------------------------*
      *
       4000-CHOIX-VALIDE-DEB.
      *
      *-------------- TRAITEMENT -------------------------------------*
      *

           PERFORM 7090-GEST-CHOIX-VALIDE-DEB
              THRU 7090-GEST-CHOIX-VALIDE-FIN.

           EVALUATE TRUE
              WHEN (CHOIX-1 OR CHOIX-6)
                 PERFORM 9000-APPEL-SPG-DEB
                    THRU 9000-APPEL-SPG-FIN
              WHEN OTHER
                 PERFORM 7060-MSG-VALIDE-DEB
                    THRU 7060-MSG-VALIDE-FIN
                 PERFORM 6040-SEND-ERASEAUP-DEB
                    THRU 6040-SEND-ERASEAUP-FIN
           END-EVALUATE.
      *
       4000-CHOIX-VALIDE-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *            DESCRIPTION DU COMPOSANT CHOIX INVALIDE            *
      *            =======================================            *
      *---------------------------------------------------------------*
      *
       4010-CHOIX-INVALIDE-DEB.
      *
      *-------------- TRAITEMENT -------------------------------------*
      *
           PERFORM 7070-MSG-INVALIDE-DEB
              THRU 7070-MSG-INVALIDE-FIN.
      *
           PERFORM 6040-SEND-ERASEAUP-DEB
              THRU 6040-SEND-ERASEAUP-FIN.
      *
       4010-CHOIX-INVALIDE-FIN.
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
       6000-SEND-ERASE-DEB.
           EXEC CICS
              SEND MAP            ('ARIM261')
                   MAPSET         ('ARIN261')
                   FROM           (ARIM261O)
                   ERASE
                   RESP           (WS-RC)
           END-EXEC.
           IF NOT WS-RC = DFHRESP(NORMAL)
              MOVE WS-RC                    TO WS-RC-ED
              STRING 'ERREUR SEND MAP ERASE : ' WS-RC-ED
                     DELIMITED BY SIZE
                INTO WS-MSG-FIN
              PERFORM 9999-ABEND-PRG-DEB
                 THRU 9999-ABEND-PRG-FIN
           END-IF.
       6000-SEND-ERASE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6010-RECEIVE-DEB.
           EXEC CICS
              RECEIVE MAP         ('ARIM261')
                      MAPSET      ('ARIN261')
                      INTO        (ARIM261I)
                      RESP        (WS-RC)
           END-EXEC.
           IF NOT WS-RC = DFHRESP(NORMAL)
              AND NOT WS-RC = DFHRESP(MAPFAIL)
              MOVE WS-RC                    TO WS-RC-ED
              STRING 'ERREUR RECEIVE MAP : ' WS-RC-ED
                     DELIMITED BY SIZE
                INTO WS-MSG-FIN
              PERFORM 9999-ABEND-PRG-DEB
                 THRU 9999-ABEND-PRG-FIN
           END-IF.
       6010-RECEIVE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6020-SEND-DEB.
           EXEC CICS
              SEND MAP            ('ARIM261')
                   MAPSET         ('ARIN261')
                   FROM           (ARIM261O)
                   RESP           (WS-RC)
           END-EXEC.
           IF NOT WS-RC = DFHRESP(NORMAL)
              MOVE WS-RC                    TO WS-RC-ED
              STRING 'ERREUR SEND MAP : ' WS-RC-ED
                     DELIMITED BY SIZE
                INTO WS-MSG-FIN
              PERFORM 9999-ABEND-PRG-DEB
                 THRU 9999-ABEND-PRG-FIN
           END-IF.
       6020-SEND-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6030-SEND-DATAONLY-DEB.
           EXEC CICS
              SEND MAP            ('ARIM261')
                   MAPSET         ('ARIN261')
                   FROM           (ARIM261O)
                   DATAONLY
                   RESP           (WS-RC)
           END-EXEC.
           IF NOT WS-RC = DFHRESP(NORMAL)
              MOVE WS-RC                    TO WS-RC-ED
              STRING 'ERREUR SEND MAP DATAONLY : ' WS-RC-ED
                     DELIMITED BY SIZE
                INTO WS-MSG-FIN
              PERFORM 9999-ABEND-PRG-DEB
                 THRU 9999-ABEND-PRG-FIN
           END-IF.
       6030-SEND-DATAONLY-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       6040-SEND-ERASEAUP-DEB.
           EXEC CICS
              SEND MAP            ('ARIM261')
                   MAPSET         ('ARIN261')
                   FROM           (ARIM261O)
                   ERASEAUP
                   RESP           (WS-RC)
           END-EXEC.
           IF NOT WS-RC = DFHRESP(NORMAL)
              MOVE WS-RC                    TO WS-RC-ED
              STRING 'ERREUR SEND MAP ERASEAUP : ' WS-RC-ED
                     DELIMITED BY SIZE
                INTO WS-MSG-FIN
              PERFORM 9999-ABEND-PRG-DEB
                 THRU 9999-ABEND-PRG-FIN
           END-IF.
       6040-SEND-ERASEAUP-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *
       7000-INIT-LV-DEB.
      *
           MOVE DFHCOMMAREA                 TO WS-COMMAREA.
           MOVE LOW-VALUE                   TO ARIM261O.
      *
       7000-INIT-LV-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7010-INIT-DATE-NUM-DEB.
      *
           EXEC CICS
              ASKTIME
                 ABSTIME (WS-ABSTIME)
           END-EXEC.
      *
           EXEC CICS FORMATTIME
              ABSTIME             (WS-ABSTIME)
              YYYYMMDD            (WS-DATE)
              DATESEP('/')
           END-EXEC.
      *
           MOVE WS-DATE                     TO MDATEO.
           MOVE EIBTRMID                    TO MTERMO.
           MOVE EIBTASKN                    TO MTASKO.
           MOVE EIBTRNID                    TO MTRANO.
      *
       7010-INIT-DATE-NUM-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7020-MSG-ALT-C-DEB.
           MOVE WS-MSG(2)                   TO MMSGO.
       7020-MSG-ALT-C-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7030-MSG-AUTRE-DEB.
           MOVE EIBTASKN                    TO MTASKO.
           MOVE WS-MSG(1)                   TO MMSGO.
           MOVE LOW-VALUE                   TO MCHOIXO.
       7030-MSG-AUTRE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7040-MSG-NON-REMPLI-DEB.
           MOVE EIBTASKN                    TO MTASKO.
           MOVE WS-MSG(24)                  TO MMSGO.
       7040-MSG-NON-REMPLI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7050-CHECK-MCHOIXI-DEB.
           MOVE MCHOIXI                     TO WS-CHOIX.
       7050-CHECK-MCHOIXI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7060-MSG-VALIDE-DEB.
           MOVE EIBTASKN                    TO MTASKO.
           MOVE SPACE                       TO WS-MSG-VALIDE.
           STRING 'LE CHOIX NUMERO '
                  WS-CHOIX
                  ' N''EST PAS ENCORE IMPLEMENTE'
                  DELIMITED BY SIZE
             INTO WS-MSG-VALIDE.
           MOVE WS-MSG-VALIDE               TO MMSGO.
           MOVE LOW-VALUE                   TO MCHOIXO.
           MOVE ZERO                        TO WS-AIG.
       7060-MSG-VALIDE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7070-MSG-INVALIDE-DEB.
           MOVE EIBTASKN                    TO MTASKO.
           MOVE WS-MSG(25)                  TO MMSGO.
           MOVE LOW-VALUE                   TO MCHOIXO.
       7070-MSG-INVALIDE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7080-MSG-ERR-AIG-DEB.
           STRING 'ERREUR AIG : ' WS-AIG
                  DELIMITED BY SIZE
             INTO WS-MSG-FIN.
       7080-MSG-ERR-AIG-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7090-GEST-CHOIX-VALIDE-DEB.
           MOVE WS-CHOIX                    TO WS-AIG.
       7090-GEST-CHOIX-VALIDE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7100-LV-0-DEB.
           MOVE LOW-VALUE                   TO WS-COMMAREA.
           MOVE 0                           TO WS-AIG.
       7100-LV-0-FIN.
           EXIT.
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
      *               DESCRIPTION DU COMPOSANT APPEL-SPG              *
      *---------------------------------------------------------------*
      * COMPOSANT EXECUTE LORS DE CHAQUE APPEL D'UN SOUS PROGRAMME    *
      * (APRES SAISIE D'UN CHOIX OU A CHAQUE BOUCLE SUR UN DES        *
      * TRAITEMENTS DEPENDANTS)                                       *
      * IL PERMET :                                                   *
      * ==> D'INITIALISER LE NOM DU SOUS PROGRAMME A APPELER          *
      * ==> DE DONNER DYNAMIQUEMENT LE CONTROLE PROGRAMME             *
      *     CORRESPONDANT EN LUI TRANSMETTANT UNE COMMAREA QUI PERMET *
      *     DE SAUVEGARDER LES DONNEES NECCESSAIRES A LA POURSUITE    *
      *     DU TRAITEMENT (PROGRAMMATION PSEUDO CONVERSATIONNELLE)    *
      *---------------------------------------------------------------*
      *
       9000-APPEL-SPG-DEB.
           EXEC CICS XCTL PROGRAM(WS-PROG(WS-AIG))
                          COMMAREA(WS-COMMAREA)
                          RESP(WS-RC)
           END-EXEC.
           IF NOT WS-RC = DFHRESP(NORMAL)
              MOVE WS-RC                    TO WS-RC-ED
              STRING 'ERREUR APPEL SOUS PROGRAMME : ' WS-RC-ED
                     DELIMITED BY SIZE
                INTO WS-MSG-FIN
              PERFORM 9999-ABEND-PRG-DEB
                 THRU 9999-ABEND-PRG-FIN
           END-IF.
       9000-APPEL-SPG-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *   9999-  : FIN DE PROGRAMME                                   *
      *---------------------------------------------------------------*
      *
       9999-FIN-PROGRAMME-DEB.
           MOVE WS-MSG(26) TO WS-MSG-FIN.
           EXEC CICS SEND
                     FROM (WS-MSG-FIN)
                     ERASE
           END-EXEC.
           EXEC CICS RETURN
           END-EXEC.
       9999-FIN-PROGRAMME-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT FIN-RTRANSID           *
      *---------------------------------------------------------------*
      * COMPOSANT EXECUTE APRES CHAQUE AFFICHAGE POUR TERMINER LA     *
      * TRANSACTION DE FACON TEMPORAIRE.                              *
      * L'OPTION TRANSID INDIQUE LE CODE TRANSACTION QUI SERA UTILISE *
      * PAR CICS POUR REINITIALISER LA TRANSACTION (EIBTRNID CONTIENT *
      * LE DERNIER CODE UTILISE).                                     *
      * L'OPTION COMMAREA PERMET DE TRANSMETTRE UNE ZONE QUI PERMET   *
      * SAUVEGARDER DES DONNEES QUI SERONT RECUPEREES PAR LE PROGRAMME*
      * POUR LE COMPTE DE LA TRANSACTION QUI SERA REACTIVEE.          *
      *---------------------------------------------------------------*
      *
       9999-FIN-RTRANSID-DEB.
           EXEC CICS RETURN
                     TRANSID(EIBTRNID)
                     COMMAREA(WS-COMMAREA)
           END-EXEC.
       9999-FIN-RTRANSID-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT ABEND-PRG              *
      *---------------------------------------------------------------*
      * COMPOSANT EXECUTE QUAND UNE ERREUR EST DETECTEE LORS DU       *
      * TEST SUR LE CONTEXTE D'EXECUTION (CODE DIFFERENT D'UNE BOUCLE *
      * SUR LA GESTION DU MENU OU SUR UN DES TRAITEMENTS DEPENDANTS). *
      *                                                               *
      *                           ATTENTION !                         *
      * AUCUN CODE (ABCODE) N'EST UTILISE POUR IDENTIFIE L'ABEND      *
      * (UNE SEULE CONDITION D'ABEND) ET L'OPTION NODUMP PERMET DE    *
      * SUPPRIMER L'IMPRESSION PAR DEFAUT D'UN DUMP.                  *
      *---------------------------------------------------------------*
      *
       9999-ABEND-PRG-DEB.
            EXEC CICS SEND
                      FROM (WS-MSG-FIN)
                      ERASE
            END-EXEC.
            EXEC CICS RETURN
            END-EXEC.
       9999-ABEND-PRG-FIN.
            EXIT.
