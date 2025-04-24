

      *===============================================================*
      *--                INFORMATIONS GENERALES                     --*
      *---------------------------------------------------------------*
      *  NOM DU PROGRAMME : ARIO826                                   *
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
      *  CE FICHIER EST LE SOUS-PROGRAMME EXTERNE.                    *
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
       PROGRAM-ID.      ARIO826.
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
      *
       01  WS-I                             PIC S9(4) COMP.
      *
       01  WS-J                             PIC S9(4) COMP.
      *
       01  WS-K                             PIC S9(4) COMP.
      *
       01  WS-L                             PIC S9(4) COMP.
      *
      *================
       LINKAGE SECTION.
      *================
      *
       01  LS-TABLEAU-1.
           05  FILLER                       OCCURS 10.
               10  FILLER                   OCCURS 10.
                   15  FILLER               OCCURS 10.
                       20  LS-ZELEM-1       PIC 9(2).
      *
       01  LS-TABLEAU-3.
           05  FILLER                       OCCURS 100.
               10  LS-I3                    PIC Z9.
               10  LS-J3                    PIC Z9.
               10  LS-K3                    PIC Z9.
      *
       01  LS-VAL-E                         PIC 99.
       01  LS-VAL-S                         PIC S9(4) COMP.
      *
      *
      *
      *                  ==============================               *
      *=================<   PROCEDURE       DIVISION   >==============*
      *                  ==============================               *
      *                                                               *
      *===============================================================*
      *
      * ARGUMENT TRANSMIS
       PROCEDURE           DIVISION
           USING LS-TABLEAU-3 LS-VAL-S LS-TABLEAU-1 LS-VAL-E.
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
      *                    COMPOSANT PROGRAMME                        *
      *                    ===================                        *
      *---------------------------------------------------------------*
      *
       0000-PROGRAMME-DEB.
      *
      * SEARCH COCCURENCE VAL-E DANS TAB1
           PERFORM  7000-SEARCH-VAL-E-DEB
              THRU  7000-SEARCH-VAL-E-FIN
           VARYING  WS-I FROM 1 BY 1
             UNTIL  WS-I > 10
             AFTER  WS-J FROM 1 BY 1
             UNTIL  WS-J > 10
             AFTER  WS-K FROM 1 BY 1
             UNTIL  WS-K > 10.
      *
           EXIT PROGRAM.
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
      *
      *---------------------------------------------------------------*
      *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
      *---------------------------------------------------------------*
      *
       7000-SEARCH-VAL-E-DEB.
      *
      * REMPLI VAL-S ET TAB3
           IF (LS-VAL-E = LS-ZELEM-1(WS-I, WS-J, WS-K))
              ADD 1                         TO LS-VAL-S
              MOVE WS-I                     TO LS-I3(LS-VAL-S)
              MOVE WS-J                     TO LS-J3(LS-VAL-S)
              MOVE WS-K                     TO LS-K3(LS-VAL-S)
           END-IF.
      *
       7000-SEARCH-VAL-E-FIN.
           EXIT.
