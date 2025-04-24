      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIO626                                   *
      *  NOM DU REDACTEUR : GIGON                                     *
      *  SOCIETE          : ESTIAC                                    *
      *  DATE DE CREATION : 19/03/2025                                *
      *---------------------------------------------------------------*
      *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
      *---------------------------------------------------------------*
      *  CE PROGRAMME CONSISTE A GERER DEUX TABLEAUX : UN PREMIR A 3  *
      *  DIMENSIONS ET UN DEUXIEME A 1 DIMENSION. CE PROGRAMME VA     *
      *  METTRE EN OEUVRE LES TABLES INDICEES VIA LA GESTION DES      *
      *  POSTES ET DES INDICES.                                       *
      *---------------------------------------------------------------*
      *--               HISTORIQUE DES MODIFICATIONS                --*
      *---------------------------------------------------------------*
      * DATE  MODIF   !          NATURE DE LA MODIFICATION            *
      *---------------------------------------------------------------*
      * 17/03/2025    !  CREATION DU FICHIER                          *
      *               !                                               *
      *===============================================================*
      *
      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID.      ARIO626.
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
       01  WS-TABLEAU-2.
           05  FILLER                       OCCURS 30.
               10  WS-ZELEM-2               PIC 9(5)  COMP-3.
      *
       01  WS-TABLEAU-2-ED.
           05  FILLER                       OCCURS 30.
               10  WS-ZELEM-2-ED            PIC ZZ9.
      *
       01  WS-I                             PIC S9(4) COMP.
       01  WS-I-ED                          PIC Z9.
      *
       01  WS-J                             PIC S9(4) COMP.
       01  WS-J-ED                          PIC Z9.
      *
       01  WS-K                             PIC S9(4) COMP.
       01  WS-K-ED                          PIC Z9.
      *
       01  WS-L                             PIC S9(4) COMP.
      *
       01  WS-D                             PIC S9(4) COMP.
      *
       01  WS-I-MAX                         PIC S9(4) COMP.
       01  WS-I-MAX-ED                      PIC Z9.
      *
       01  WS-MAX                           PIC 9(5)  COMP-3
                                            VALUE ZERO.
       01  WS-MAX-ED                        PIC Z9.
      *
       01  WS-CPT-FREQ                      PIC S9(4) COMP
                                            VALUE ZERO.
       01  WS-CPT-FREQ-ED                   PIC Z9.
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
      * INITIALISE TAB 2 A 0
           INITIALIZE WS-TABLEAU-2 REPLACING NUMERIC BY ZERO.
      *
      * INIT TAB 1 ET 2 + TROUVE OCCURENCE MAX
           PERFORM  7000-INIT-TABS-DEB
              THRU  7000-INIT-TABS-FIN
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
      * MOVE TAB 2 DANS TAB 2 EDITION
           PERFORM  7010-INIT-TAB2-ED-DEB
              THRU  7010-INIT-TAB2-ED-FIN
           VARYING  WS-L FROM 1 BY 1
             UNTIL  WS-L > 30.
      *
      * DISPLAY TAB 2 EDITION
           PERFORM  8010-DISPLAY-TAB2-DEB
              THRU  8010-DISPLAY-TAB2-FIN.
      *
      * DISPLAY VAL FREQ
           PERFORM  8020-DISPLAY-VAL-FREQ-DEB
              THRU  8020-DISPLAY-VAL-FREQ-FIN
           VARYING  WS-L FROM 1 BY 1
             UNTIL  WS-L > 30.
      *
           PERFORM  9999-FIN-PROGRAMME-DEB
              THRU  9999-FIN-PROGRAMME-FIN.
      *
       0000-PROGRAMME-FIN.
            STOP RUN.
      *
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *
       7000-INIT-TABS-DEB.
      *
           COMPUTE WS-L = WS-I + WS-J + WS-K.
           MOVE WS-L
                TO WS-ZELEM-1(WS-I, WS-J, WS-K).
           ADD 1                            TO WS-ZELEM-2(WS-L).
      *
       7000-INIT-TABS-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       7010-INIT-TAB2-ED-DEB.
      *
           MOVE WS-ZELEM-2(WS-L)            TO WS-ZELEM-2-ED(WS-L).
           IF (WS-ZELEM-2(WS-L) > WS-MAX)
              MOVE WS-ZELEM-2(WS-L)         TO WS-MAX
           END-IF.
      *
       7010-INIT-TAB2-ED-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
      *---------------------------------------------------------------*
      *
       8000-DISPLAY-TAB1-DEB.
      *
           DISPLAY 'ETAPE 1 - TABLEAU-1 GLOBAL : '.
           DISPLAY WS-TABLEAU-1.
           DISPLAY SPACE.
      *
       8000-DISPLAY-TAB1-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8010-DISPLAY-TAB2-DEB.
      *
           DISPLAY 'ETAPE 1 - TABLEAU-2 GLOBAL : '.
           DISPLAY WS-TABLEAU-2-ED.
           DISPLAY SPACE.
      *
       8010-DISPLAY-TAB2-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8020-DISPLAY-VAL-FREQ-DEB.
      *
           IF (WS-ZELEM-2(WS-L) = WS-MAX)
              ADD 1                         TO WS-CPT-FREQ
              MOVE WS-CPT-FREQ              TO WS-CPT-FREQ-ED
              MOVE WS-L                     TO WS-I-MAX
              MOVE WS-I-MAX                 TO WS-I-MAX-ED
              MOVE SPACE                    TO WS-BUFFER
              STRING 'ETAPE 1 - VALEUR LA PLUS FREQUENTE '
                      WS-CPT-FREQ-ED ' : ' WS-I-MAX-ED
                      DELIMITED BY SIZE
                 INTO WS-BUFFER
              DISPLAY WS-BUFFER
      *
              MOVE WS-MAX                   TO WS-MAX-ED
              MOVE SPACE                    TO WS-BUFFER
              STRING 'TROUVEE ' WS-MAX-ED ' FOIS DANS LES POSTES : '
                      DELIMITED BY SIZE
                 INTO WS-BUFFER
              DISPLAY WS-BUFFER
      *
              PERFORM  8030-DISPLAY-IJK-DEB
                 THRU  8030-DISPLAY-IJK-FIN
              VARYING  WS-I FROM 1 BY 1
                UNTIL  WS-I > 10
                AFTER  WS-J FROM 1 BY 1
                UNTIL  WS-J > 10
                AFTER  WS-K FROM 1 BY 1
                UNTIL  WS-K > 10
      *
              DISPLAY SPACE
           END-IF.
      *
       8020-DISPLAY-VAL-FREQ-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *
       8030-DISPLAY-IJK-DEB.
      *
           IF (WS-I-MAX = WS-ZELEM-1(WS-I, WS-J, WS-K))
              MOVE WS-I                     TO WS-I-ED
              MOVE WS-J                     TO WS-J-ED
              MOVE WS-K                     TO WS-K-ED
              MOVE SPACE                    TO WS-BUFFER
              STRING WS-I-ED ' , ' WS-J-ED ' , ' WS-K-ED
                     DELIMITED BY SIZE
                 INTO WS-BUFFER
              DISPLAY WS-BUFFER
           END-IF.
      *
       8030-DISPLAY-IJK-FIN.
           EXIT.
      *
      *---------------------------------------------------------------*
      *   9999-  : PROTECTION FIN DE PROGRAMME                        *
      *---------------------------------------------------------------*
      *
       9999-FIN-PROGRAMME-DEB.
      *
            DISPLAY '*==============================================*'.
            DISPLAY '*     FIN NORMALE DU PROGRAMME ARIO626         *'.
            DISPLAY '*==============================================*'.
      *
       9999-FIN-PROGRAMME-FIN.
            EXIT.
      *---------------------------------------------------------------*
      *
       9999-ERREUR-PROGRAMME-DEB.
      *
            DISPLAY '*==============================================*'.
            DISPLAY '*        UNE ANOMALIE A ETE DETECTEE           *'.
            DISPLAY '*     FIN ANORMALE DU PROGRAMME ARIO626        *'.
            DISPLAY '*==============================================*'.
            MOVE 12 TO RETURN-CODE.
      *
       9999-ERREUR-PROGRAMME-FIN.
            STOP RUN.

