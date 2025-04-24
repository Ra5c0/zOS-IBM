      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIC262                                   *
      *  NOM DU REDACTEUR : GIGON OSCAR                               *
      *  SOCIETE          : ESTIAC INSTITUT                           *
      *  DATE DE CREATION : 11/04/2025                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      *  L'ECRITURE CE PORGRAMME PERMETTRA : DE CONCEVOIR UN          *
      *  PROGRAMME DE CONSULTATION EN ACCÈS DIRECT, DE METTRE EN      *
      *  OEUVRE L'UTILISATION D'UNE DEUXIÈME MAP D'UN MAPSET,         *
      *  L'INTERACTION ENTRE LES PROGRAMMES DANS CICS ET D'ACCEDER    *
      *  À UN CLUSTER KSDS.                                           *
      *---------------------------------------------------------------*
      *--               HISTORIQUE DES MODIFICATIONS                --*
      *---------------------------------------------------------------*
      * DATE  MODIF   §          NATURE DE LA MODIFICATION            *
      *---------------------------------------------------------------*
      * 11/04/2025    §  CREATION DU FICHIER ET DEBUT CODE            *
      *               §                                               *
      *===============================================================*
      *
      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID. ARIC262.
      *
      *===============================================================*
      *           NE PAS MODIFIER LA PARTIE ENCADREE DU CODE          *
      *===============================================================*
      *
      *                  ==============================               *
      *=================<    ENVIRONMENT    DIVISION   >==============*
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
      *=================<         DATA      DIVISION   >==============*
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
      *
      *========================
       WORKING-STORAGE SECTION.
      *========================
      *
      *===============================================================*
      *             COPY - INSERTION DE SEQUENCES DE SOURCE           *
      *===============================================================*
      * TEST DES TOUCHES FONCTION
           COPY DFHAID.
      * MODIFICATION DYNAMIQUE DES ATTRIBUTS DE MAP
           COPY DFHBMSCA.
      * TABLES DES MESSAGES
           COPY TABMSG.
      * WS ARIN261
           COPY ARIN262.
      * FICHIER ARTICLE
           COPY ARTICLE.
      * COMMAREA
           COPY COMMAREA.
      *
       01  WS-RC                            PIC S9(4) COMP.
       01  WS-RC-ED                         PIC X(10).
      *
       01  WS-QTE-ED                        PIC ZZZZZ9
           BLANK WHEN ZERO.
       01  WS-ALERT-ED                      PIC ZZZZ9
           BLANK WHEN ZERO.
       01  WS-LOT-QTE-ED                    PIC ZZZZ9.
       01  WS-LOT-PXU-ED                    PIC ZZZZ9V,99.
      *
       01  WS-MAP                           PIC X(8)
                                            VALUE 'ARIM262'.
       01  WS-MAPSET                        PIC X(8)
                                            VALUE 'ARIN262'.
      *
       01  WS-MSG-FIN                       PIC X(80).
      *
      *
      *================
       LINKAGE SECTION.
      *================
      *
       01  DFHCOMMAREA                  PIC X(4096).
      *
      *                  ==============================               *
      *=================<    PROCEDURE      DIVISION   >==============*
      *                  ==============================               *
      *                                                               *
      *===============================================================*
      *
      ********************
       PROCEDURE DIVISION.
      ********************
      *
      *===============================================================*
      *    STRUCTURATION DE LA PARTIE ALGORITHMIQUE DU PROGRAMME      *
      *---------------------------------------------------------------*
      *    1 : LES COMPOSANTS DU DIAGRAMME SONT CODES A L'AIDE DE     *
      *        DEUX PARAGRAPHES  XXYY-COMPOSANT-DEB                   *
      *                          XXYY-COMPOSANT-FIN                   *
      *    2 : XX REPRESENTE LE NIVEAU HIERARCHIQUE                   *
      *        YY DIFFERENCIE LES COMPOSANTS DE MEME NIVEAU           *
      *    3 : TOUT COMPOSANT EST PRECEDE D'UN CARTOUCHE DE           *
      *        COMMENTAIRE QUI EXPLICITE LE ROLE DU COMPOSANT         *
      *===============================================================*
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT PRINCIPAL              *
      *---------------------------------------------------------------*
      *
       0000-PRINCIPAL-DEB.

           PERFORM 7000-INIT-PRINCIPAL-DEB
              THRU 7000-INIT-PRINCIPAL-FIN.

           EVALUATE TRUE
               WHEN INIT-TRT
                 PERFORM 1000-1ERE-FOIS-DEB
                    THRU 1000-1ERE-FOIS-FIN
               WHEN AFF-MAP
                 PERFORM 1010-N-FOIS-DEB
                    THRU 1010-N-FOIS-FIN
               WHEN AFF-AIDE
                 PERFORM 1020-AIDE-DEB
                    THRU 1020-AIDE-FIN
               WHEN OTHER
                 PERFORM 1030-ABEND-DEB
                    THRU 1030-ABEND-FIN
           END-EVALUATE.

           PERFORM  9999-FIN-RTRANSID-DEB
              THRU  9999-FIN-RTRANSID-FIN.

       0000-PRINCIPAL-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT 1ERE FOIS              *
      *---------------------------------------------------------------*
      *
       1000-1ERE-FOIS-DEB.

           PERFORM 7010-MOVE-INFOS-GEN-DEB
              THRU 7010-MOVE-INFOS-GEN-FIN.

           PERFORM 7050-MOVE-M-TO-TAFF-DEB
              THRU 7050-MOVE-M-TO-TAFF-FIN.

           PERFORM 7060-MOVE-MSG-SAISI-DEB
              THRU 7060-MOVE-MSG-SAISI-FIN.

           PERFORM 6000-SEND-ERASE-DEB
              THRU 6000-SEND-ERASE-FIN.

       1000-1ERE-FOIS-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT N FOIS                 *
      *---------------------------------------------------------------*
      *
       1010-N-FOIS-DEB.

           EVALUATE EIBAID
              WHEN DFHENTER
                 PERFORM 2000-ENTER-DEB
                    THRU 2000-ENTER-FIN
              WHEN DFHPF1
                 PERFORM 2010-F1-DEB
                    THRU 2010-F1-FIN
              WHEN DFHPF3
                 PERFORM 2020-F3-DEB
                    THRU 2020-F3-FIN
              WHEN DFHCLEAR
                 PERFORM 2030-ALT-C-DEB
                    THRU 2030-ALT-C-FIN
              WHEN OTHER
                 PERFORM 2040-AUTRE-DEB
                    THRU 2040-AUTRE-FIN
           END-EVALUATE.

       1010-N-FOIS-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT AIDE                   *
      *---------------------------------------------------------------*
      *
       1020-AIDE-DEB.

           PERFORM 7050-MOVE-M-TO-TAFF-DEB
              THRU 7050-MOVE-M-TO-TAFF-FIN.

           PERFORM 7070-MOVE-ECRAN2-TO-MAP-DEB
              THRU 7070-MOVE-ECRAN2-TO-MAP-FIN.

           PERFORM 7040-MOVE-OUT-COMMAREA-DEB
              THRU 7040-MOVE-OUT-COMMAREA-FIN.

           IF WS-COMMAREA-ENR NOT = LOW-VALUES

              PERFORM 7010-MOVE-INFOS-GEN-DEB
                 THRU 7010-MOVE-INFOS-GEN-FIN

              PERFORM 7030-MOVE-INFOS-ARTICLE-DEB
                 THRU 7030-MOVE-INFOS-ARTICLE-FIN

              PERFORM 7090-GEST-DISP-LOT-DEB
                 THRU 7090-GEST-DISP-LOT-FIN
              VARYING WS-IND FROM 1 BY 1
                UNTIL WS-IND > WS-ART-NB-LOT

           END-IF.

           PERFORM 7170-GEST-MDT-DEB
              THRU 7170-GEST-MDT-FIN.

           PERFORM 6000-SEND-ERASE-DEB
              THRU 6000-SEND-ERASE-FIN.

       1020-AIDE-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT ABEND                  *
      *---------------------------------------------------------------*
      *
       1030-ABEND-DEB.

           PERFORM 7110-MSG-ERR-TAFF-DEB
              THRU 7110-MSG-ERR-TAFF-FIN.

           PERFORM 9999-ABEND-PRG-DEB
              THRU 9999-ABEND-PRG-FIN.

       1030-ABEND-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT ENTER                  *
      *---------------------------------------------------------------*
      *
       2000-ENTER-DEB.

           PERFORM 6010-RECEIVE-DEB
              THRU 6010-RECEIVE-FIN.

           IF WS-RC = DFHRESP(MAPFAIL)
              PERFORM 3000-CODE-VIDE-DEB
                 THRU 3000-CODE-VIDE-FIN
           ELSE
              PERFORM 3010-CODE-REMPLI-DEB
                 THRU 3010-CODE-REMPLI-FIN
           END-IF.

       2000-ENTER-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT F1                     *
      *---------------------------------------------------------------*
      *
       2010-F1-DEB.

           PERFORM 7100-MOVE-A-TO-TAFF-DEB
              THRU 7100-MOVE-A-TO-TAFF-FIN.

           PERFORM 7080-MOVE-ECRANHP-TO-MAP-DEB
              THRU 7080-MOVE-ECRANHP-TO-MAP-FIN.

           PERFORM 6000-SEND-ERASE-DEB
              THRU 6000-SEND-ERASE-FIN.

       2010-F1-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT F3                     *
      *---------------------------------------------------------------*
      *
       2020-F3-DEB.

           PERFORM 9000-APPEL-SPG-DEB
              THRU 9000-APPEL-SPG-FIN.

       2020-F3-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT ALT+C                  *
      *---------------------------------------------------------------*
      *
       2030-ALT-C-DEB.

           PERFORM 7010-MOVE-INFOS-GEN-DEB
              THRU 7010-MOVE-INFOS-GEN-FIN.

           PERFORM 7140-MSG-ALT-C-DEB
              THRU 7140-MSG-ALT-C-FIN.

           PERFORM 7040-MOVE-OUT-COMMAREA-DEB
              THRU 7040-MOVE-OUT-COMMAREA-FIN.

           IF WS-COMMAREA-ENR NOT = LOW-VALUES

              PERFORM 7030-MOVE-INFOS-ARTICLE-DEB
                 THRU 7030-MOVE-INFOS-ARTICLE-FIN

              PERFORM 7090-GEST-DISP-LOT-DEB
                 THRU 7090-GEST-DISP-LOT-FIN
              VARYING WS-IND FROM 1 BY 1
                UNTIL WS-IND > WS-ART-NB-LOT

           END-IF.

           PERFORM 7170-GEST-MDT-DEB
              THRU 7170-GEST-MDT-FIN.

           PERFORM 6020-SEND-DEB
              THRU 6020-SEND-FIN.

       2030-ALT-C-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT AUTRE                  *
      *---------------------------------------------------------------*
      *
       2040-AUTRE-DEB.

           PERFORM 7120-GEST-AUTRE-DEB
              THRU 7120-GEST-AUTRE-FIN.

            PERFORM 6030-SEND-DATAONLY-DEB
               THRU 6030-SEND-DATAONLY-FIN.

       2040-AUTRE-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT CODE VIDE              *
      *---------------------------------------------------------------*
      *
       3000-CODE-VIDE-DEB.

           PERFORM 7130-GEST-CODE-VIDE-DEB
              THRU 7130-GEST-CODE-VIDE-FIN.

           PERFORM 6030-SEND-DATAONLY-DEB
              THRU 6030-SEND-DATAONLY-FIN.

       3000-CODE-VIDE-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT CODE REMPLI            *
      *---------------------------------------------------------------*
      *
       3010-CODE-REMPLI-DEB.

           PERFORM 6050-READ-ARTICLE-DEB
              THRU 6050-READ-ARTICLE-FIN.

           IF WS-RC = DFHRESP(NORMAL)
              PERFORM 4000-CODE-EXISTANT-DEB
                 THRU 4000-CODE-EXISTANT-FIN
           ELSE
              PERFORM 4010-CODE-INEXISTANT-DEB
                 THRU 4010-CODE-INEXISTANT-FIN
           END-IF.

       3010-CODE-REMPLI-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT CODE EXISTANT          *
      *---------------------------------------------------------------*
      *
       4000-CODE-EXISTANT-DEB.

           PERFORM 7150-GEST-CODE-EXISTANT-DEB
              THRU 7150-GEST-CODE-EXISTANT-FIN.

           PERFORM 7010-MOVE-INFOS-GEN-DEB
              THRU 7010-MOVE-INFOS-GEN-FIN.

           PERFORM 7030-MOVE-INFOS-ARTICLE-DEB
              THRU 7030-MOVE-INFOS-ARTICLE-FIN.

           PERFORM 7020-MOVE-IN-COMMAREA-DEB
              THRU 7020-MOVE-IN-COMMAREA-FIN.

           PERFORM 7090-GEST-DISP-LOT-DEB
              THRU 7090-GEST-DISP-LOT-FIN
           VARYING WS-IND FROM 1 BY 1
             UNTIL WS-IND > WS-ART-NB-LOT.

           PERFORM 7170-GEST-MDT-DEB
              THRU 7170-GEST-MDT-FIN.

           PERFORM 6000-SEND-ERASE-DEB
              THRU 6000-SEND-ERASE-FIN.

       4000-CODE-EXISTANT-FIN.
            EXIT.
      *
      *---------------------------------------------------------------*
      *               DESCRIPTION DU COMPOSANT CODE INEXISTANT        *
      *---------------------------------------------------------------*
      *
       4010-CODE-INEXISTANT-DEB.

           PERFORM 7160-GEST-CODE-INEXISTANT-DEB
              THRU 7160-GEST-CODE-INEXISTANT-FIN.

           PERFORM 7010-MOVE-INFOS-GEN-DEB
              THRU 7010-MOVE-INFOS-GEN-FIN.

           PERFORM 7030-MOVE-INFOS-ARTICLE-DEB
              THRU 7030-MOVE-INFOS-ARTICLE-FIN.

           PERFORM 7090-GEST-DISP-LOT-DEB
              THRU 7090-GEST-DISP-LOT-FIN
           VARYING WS-IND FROM 1 BY 1
             UNTIL WS-IND > WS-ART-NB-LOT.

           PERFORM 6000-SEND-ERASE-DEB
              THRU 6000-SEND-ERASE-FIN.

       4010-CODE-INEXISTANT-FIN.
            EXIT.
      *
      *===============================================================*
      *    STRUCTURATION DE LA PARTIE INDEPENDANTE DU PROGRAMME       *
      *---------------------------------------------------------------*
      *   6XXX-  : ORDRES DE MANIPULATION DES FICHIERS                *
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *   9XXX-  : ORDRES DE MANIPULATION DES SOUS PROGRAMMES         *
      *   9999-  : FIN DE PROGRAMME                                   *
      *===============================================================*
      *
      *---------------------------------------------------------------*
      *   6XXX-  : ORDRES DE MANIPULATION DES FICHIERS                *
      *---------------------------------------------------------------*
      *                                                              *
       6000-SEND-ERASE-DEB.
           EXEC CICS
              SEND MAP            (WS-MAP)
                   MAPSET         (WS-MAPSET)
                   FROM           (ARIM262O)
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
              RECEIVE MAP         (WS-MAP)
                      MAPSET      (WS-MAPSET)
                      INTO        (ARIM262I)
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
              SEND MAP            (WS-MAP)
                   MAPSET         (WS-MAPSET)
                   FROM           (ARIM262O)
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
              SEND MAP            (WS-MAP)
                   MAPSET         (WS-MAPSET)
                   FROM           (ARIM262O)
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
       6050-READ-ARTICLE-DEB.
           MOVE MCODEI                    TO WS-ART-CODE.
           EXEC CICS
              READ FILE           ('ART0206')
                   RIDFLD         (WS-ART-CODE)
                   INTO           (WS-ART-ENR)
                   RESP           (WS-RC)
           END-EXEC.
           MOVE WS-RC                       TO WS-RC-ED.
           IF WS-RC NOT = DFHRESP(NORMAL) AND
              WS-RC NOT = DFHRESP(NOTFND)
              STRING 'ERREUR READ : ' WS-RC-ED
                     DELIMITED BY SIZE
                INTO WS-MSG-FIN
              PERFORM 9999-ABEND-PRG-DEB
                 THRU 9999-ABEND-PRG-FIN
           END-IF.
       6050-READ-ARTICLE-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *
       7000-INIT-PRINCIPAL-DEB.
           MOVE DFHCOMMAREA                 TO WS-COMMAREA.
           MOVE LOW-VALUE                   TO ARIM262O.
       7000-INIT-PRINCIPAL-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7010-MOVE-INFOS-GEN-DEB.
           MOVE WS-DATE                     TO MDATEO.
           MOVE EIBTRMID                    TO MTERMO.
           MOVE EIBTASKN                    TO MTASKO.
           MOVE EIBTRNID                    TO MTRANO.
       7010-MOVE-INFOS-GEN-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7020-MOVE-IN-COMMAREA-DEB.
           MOVE WS-ART-ENR                  TO WS-COMMAREA-ENR.
       7020-MOVE-IN-COMMAREA-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7030-MOVE-INFOS-ARTICLE-DEB.
           MOVE WS-ART-QTE                  TO WS-QTE-ED.
           MOVE WS-ART-ALERT                TO WS-ALERT-ED.

           MOVE WS-ART-CODE                 TO MCODEO.
           MOVE WS-ART-LIBEL                TO MLIBELO.
           MOVE WS-ART-CATEG                TO MCATEGO.
           MOVE WS-ART-FOU                  TO MFOURO.
           MOVE WS-ART-DELAI                TO MAPPROO.
           MOVE WS-QTE-ED                   TO MQTSTKO.
           MOVE WS-ALERT-ED                 TO MQTALEO.
           MOVE WS-ART-NB-LOT               TO MNLOTO.
       7030-MOVE-INFOS-ARTICLE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7040-MOVE-OUT-COMMAREA-DEB.
           MOVE WS-COMMAREA-ENR             TO WS-ART-ENR.
       7040-MOVE-OUT-COMMAREA-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7050-MOVE-M-TO-TAFF-DEB.
           MOVE 'M'                         TO WS-TAFF.
       7050-MOVE-M-TO-TAFF-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7060-MOVE-MSG-SAISI-DEB.
           MOVE WS-MSG(3)                   TO MMSGO.
       7060-MOVE-MSG-SAISI-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7070-MOVE-ECRAN2-TO-MAP-DEB.
              MOVE 'ARIM262'                TO WS-MAP.
       7070-MOVE-ECRAN2-TO-MAP-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7080-MOVE-ECRANHP-TO-MAP-DEB.
           MOVE 'ARIMHP2'                   TO WS-MAP.
       7080-MOVE-ECRANHP-TO-MAP-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7090-GEST-DISP-LOT-DEB.
           IF WS-ART-LOT-NUM(WS-IND) NOT = LOW-VALUES
              MOVE WS-ART-LOT-QTE(WS-IND)   TO WS-LOT-QTE-ED
              MOVE WS-ART-LOT-PXU(WS-IND)   TO WS-LOT-PXU-ED
              STRING '            '
                     WS-ART-LOT-NUM(WS-IND)
                     '                  '
                     WS-LOT-QTE-ED
                     '                  '
                     WS-LOT-PXU-ED
                     DELIMITED BY SIZE
                INTO MLOTO(WS-IND)
           END-IF.
       7090-GEST-DISP-LOT-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7100-MOVE-A-TO-TAFF-DEB.
           MOVE 'A'                         TO WS-TAFF.
       7100-MOVE-A-TO-TAFF-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7110-MSG-ERR-TAFF-DEB.
           STRING 'ERREUR TAFF : ' WS-TAFF
                  DELIMITED BY SIZE
             INTO WS-MSG-FIN.
       7110-MSG-ERR-TAFF-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7120-GEST-AUTRE-DEB.
           MOVE EIBTASKN                    TO MTASKO.
           MOVE WS-MSG(1)                   TO MMSGO.
           MOVE LOW-VALUE                   TO MCODEO.
           MOVE DFHUNIMD                    TO MCODEA.
       7120-GEST-AUTRE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7130-GEST-CODE-VIDE-DEB.
           MOVE EIBTASKN                    TO MTASKO.
           MOVE WS-MSG(6)                   TO MMSGO.
       7130-GEST-CODE-VIDE-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7140-MSG-ALT-C-DEB.
           MOVE WS-MSG(2)                   TO MMSGO.
       7140-MSG-ALT-C-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7150-GEST-CODE-EXISTANT-DEB.
           MOVE LOW-VALUE                   TO ARIM262O.
           MOVE SPACES                      TO MMSGO.
       7150-GEST-CODE-EXISTANT-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7160-GEST-CODE-INEXISTANT-DEB.
           MOVE LOW-VALUE                   TO ARIM262O.
           MOVE WS-MSG(27)                  TO MMSGO.
           MOVE LOW-VALUES                  TO WS-ART-ENR
                                               WS-COMMAREA-ENR.
       7160-GEST-CODE-INEXISTANT-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7170-GEST-MDT-DEB.
           MOVE DFHUNIMD                    TO MCODEA.
       7170-GEST-MDT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *---------------------------------------------------------------*
      *
      *8999-STATISTIQUES-DEB.
      *
      *8999-STATISTIQUES-FIN.
      *     EXIT.
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
           EXEC CICS XCTL PROGRAM('ARIC261')
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

