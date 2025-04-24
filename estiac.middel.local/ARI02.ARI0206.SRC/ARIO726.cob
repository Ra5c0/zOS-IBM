
      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIO726                                   *
      *  NOM DU REDACTEUR : GIGON                                     *
      *  SOCIETE          : ESTIAC                                    *
      *  DATE DE CREATION : 20/03/2025                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      *  CE PROGRAMME CONSISTE A MODIFIER LE TP6 PAR L'ECLATEMENT     *
      *  DU TRAITEMENT ENTRE UN PROGRAMME PRINCIPAL ET UN SOUS-       *
      *  PROGRAMME EXTERNE. CE PROGRAMME DOIT PERMETTRE DE METTRE     *
      *  EN OEUVRE LA COMMUNICATION INTER PROGRAMMES AVEC TRANSMISS-  *
      *  ION D'ARGUMENTS DANS LES DEUX SENS.                          *
      *                                                               *
      *  CE FICHIER EST LE PROGRAMME PRINCIPAL.                       *
      *---------------------------------------------------------------*
      *--               HISTORIQUE DES MODIFICATIONS                --*
      *---------------------------------------------------------------*
      * DATE  MODIF   !          NATURE DE LA MODIFICATION            *
      *---------------------------------------------------------------*
      * 20/03/2025    !  CREATION DU FICHIER                          *
      *               !                                               *
      *===============================================================*
      *
      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID.      ARIO726.
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
      *
      *========================
       WORKING-STORAGE SECTION.
      *========================
       01  WS-TABLEAU-1.
           05  FILLER                       OCCURS 10.
               10  FILLER                   OCCURS 10.
                   15  FILLER               OCCURS 10.
                       20  WS-ZELEM-1       PIC 9(2).
      *
       01  WS-I                             PIC S9(4) COMP.
       01  WS-I-ED                          PIC Z9.
       01  WS-J                             PIC S9(4) COMP.
       01  WS-J-ED                          PIC Z9.
       01  WS-K                             PIC S9(4) COMP.
       01  WS-K-ED                          PIC Z9.
      *
       01  WS-TABLEAU-3.
           05  FILLER                       OCCURS 100.
               10  WS-I3                    PIC Z9.
               10  WS-J3                    PIC Z9.
               10  WS-K3                    PIC Z9.
      *
       01  WS-L                             PIC S9(4) COMP.
      *
       01  WS-SYSIN                         PIC XX.
           88  END-SYSIN                    VALUE '$$'.
       01  WS-VAL-E REDEFINES WS-SYSIN      PIC Z9.
      *
       01  WS-VAL-S                         PIC S9(4) COMP
                                            VALUE ZERO.
       01  WS-VAL-S-ED                      PIC Z9.
      *
       01  WS-CPT-VAL-SEARCH                PIC S9(4) COMP
                                            VALUE ZERO.
       01  WS-CPT-ED                        PIC Z9.
      *
       01  WS-BUFFER                        PIC X(80).
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
      *---------------------------------------------------------------*
      *                     COMPOSANT PROGRAMME                       *
      *                     ===================                       *
      *---------------------------------------------------------------*
      *
       0000-PROGRAMME-DEB.
      *
      * INIT TAB 1
           PERFORM  7000-INIT-TAB1-DEB
              THRU  7000-INIT-TAB1-FIN
           VARYING  WS-I FROM 1 BY 1
             UNTIL  WS-I > 10
             AFTER  WS-J FROM 1 BY 1
             UNTIL  WS-J > 10
             AFTER  WS-K FROM 1 BY 1
             UNTIL  WS-K > 10.
      *
      * DISPLAY TAB 1
           PERFORM  8000-DISPLAY-TAB1-DEB
              THRU  8000-DISPLAY-TAB1-FIN.
      *
      * LECTURE SYSIN
           PERFORM  6000-READ-SYSIN-DEB
              THRU  6000-READ-SYSIN-FIN.
      *
      * CALL SOUS PROG EXTERNE
           PERFORM  1000-TRAITEMENT-DEB
              THRU  1000-TRAITEMENT-FIN
             UNTIL  END-SYSIN.
      *
      * DISPLAY FIN PGM + STOP RUN
           PERFORM  9999-FIN-PROGRAMME-DEB
              THRU  9999-FIN-PROGRAMME-FIN.
      *
       0000-PROGRAMME-FIN.
            STOP RUN.
      *
      *---------------------------------------------------------------*
      *                     COMPOSANT TRAITEMENT                      *
      *                     ====================                      *
      *---------------------------------------------------------------*
      *
       1000-TRAITEMENT-DEB.
      *
      * CALL PGM EXTERNE
           PERFORM  9000-CALL-PGM-EXT-DEB
              THRU  9000-CALL-PGM-EXT-FIN.
      *
      * DISPLAY POUR VAL RECHERCHEE
           PERFORM  8010-DISPLAY-VAL-SEARCH-DEB
              THRU  8010-DISPLAY-VAL-SEARCH-FIN.
      *
      * LECTURE SYSIN
           PERFORM  6000-READ-SYSIN-DEB
              THRU  6000-READ-SYSIN-FIN.
      *
       1000-TRAITEMENT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   6XXX-  : ORDRES DE MANIPULATION DES FICHIERS                *
      *---------------------------------------------------------------*
      *
       6000-READ-SYSIN-DEB.
      *
           ACCEPT WS-SYSIN.
      *
       6000-READ-SYSIN-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *
       7000-INIT-TAB1-DEB.
      *
           COMPUTE WS-L = WS-I + WS-J + WS-K.
           MOVE WS-L
                TO WS-ZELEM-1(WS-I, WS-J, WS-K).
      *
       7000-INIT-TAB1-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *---------------------------------------------------------------*
      *
       8000-DISPLAY-TAB1-DEB.
      *
           DISPLAY 'ETAPE 2 - TABLEAU-1 GLOBAL : '.
           DISPLAY WS-TABLEAU-1.
           DISPLAY SPACE.
      *
       8000-DISPLAY-TAB1-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8010-DISPLAY-VAL-SEARCH-DEB.
      *
           ADD 1                            TO WS-CPT-VAL-SEARCH.
           MOVE WS-CPT-VAL-SEARCH           TO WS-CPT-ED.
           MOVE WS-VAL-S                    TO WS-VAL-S-ED.
      *
           MOVE SPACE                       TO WS-BUFFER.
           STRING 'ETAPE 2 - VALEUR RECHERCHEE ' WS-CPT-ED ' : '
                   WS-VAL-E
                   DELIMITED BY SIZE
              INTO WS-BUFFER.
           DISPLAY WS-BUFFER.
      *
           MOVE SPACE                       TO WS-BUFFER.
           STRING 'TROUVEE ' WS-VAL-S-ED ' FOIS DANS LES POSTES :'
                   DELIMITED BY SIZE
              INTO WS-BUFFER.
           DISPLAY WS-BUFFER.
      *
           PERFORM  8020-DISPLAY-IJK-DEB
              THRU  8020-DISPLAY-IJK-FIN
           VARYING  WS-L FROM 1 BY 1
             UNTIL  WS-L > WS-VAL-S.
      *
           DISPLAY SPACE.
      *
           MOVE 0                           TO WS-VAL-S.
      *
       8010-DISPLAY-VAL-SEARCH-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8020-DISPLAY-IJK-DEB.
      *
           MOVE SPACE                       TO WS-BUFFER.
           STRING WS-I3(WS-L) ' , ' WS-J3(WS-L) ' , ' WS-K3(WS-L)
                  DELIMITED BY SIZE
              INTO WS-BUFFER.
           DISPLAY WS-BUFFER.
      *
       8020-DISPLAY-IJK-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   9XXX-  : ORDRES DE MANIPULATION DES SOUS-PROGRAMMES         *
      *---------------------------------------------------------------*
      *
       9000-CALL-PGM-EXT-DEB.
      *
      * APPEL PGM EXTERNE
           CALL 'ARIO826'
                 USING WS-TABLEAU-3 WS-VAL-S
                 BY CONTENT WS-TABLEAU-1 WS-VAL-E.
      *
       9000-CALL-PGM-EXT-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   9999-  : PROTECTION FIN DE PROGRAMME                        *
      *---------------------------------------------------------------*
      *
       9999-FIN-PROGRAMME-DEB.
      *
            DISPLAY '*==============================================*'.
            DISPLAY '*     FIN NORMALE DU PROGRAMME ARIO726         *'.
            DISPLAY '*==============================================*'.
      *
       9999-FIN-PROGRAMME-FIN.
            EXIT.

