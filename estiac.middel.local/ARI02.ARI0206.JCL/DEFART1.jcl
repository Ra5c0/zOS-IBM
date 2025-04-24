//ARI0206C JOB (ADACC),'CREATION ARTICLE',MSGCLASS=X,REGION=4M,         JOB04771
//    CLASS=A,MSGLEVEL=(1,1),NOTIFY=&SYSUID,COND=(4,LT),TIME=(0,30)
//*
//* *=================================================================*
//* *                                                                 *
//* *                    ESTIAC INSTITUT                              *
//* *                                                                 *
//* *            UNITE DE FORMATION CICS PROGRAMMATION                *
//* *                                                                 *
//* * CREATION DU FICHIER KSDS ARTICLE AVEC LA CLE PRIMAIRE ET LA CLE *
//* * SECONDAIRE                                                      *
//* *=================================================================*
//* * REMPLACER LES 02SAVEUU PAR VOTRE GROUPE ET VOTRE USER           *
//* *=================================================================*
//* * PREMIERE ETAPE:                                                 *
//* * - SUPPRESSION DU KSDS ARTICLE PAR UN DELETE                     *
//* *   SI LE FICHIER N'EXISTE PAS IL FAUT METTRE RC A 0 POUR QUE LE  *
//* *   L'EXECUTION NE S'ARRETE PAS                                   *
//* *                                                                 *
//* * - DEFINITION DU KSDS ET DE LA CLE PRIMAIRE PAR UN DEFINE        *
//* *                                                                 *
//* * - ALIMENTATION DU KSDS A PARTIR D'UN FICHIER SEQUENTIEL PAR UN  *
//* *   REPRO                                                         *
//* *                                                                 *
//* * - AFFICHAGE DU CONTENU D'UN FICHIER PAR UN PRINT (AUSSI VALABLE *
//* *   POUR UN FICHIER KSDS)                                         *
//* *=================================================================*
//STEP1   EXEC PGM=IDCAMS
//SYSIN   DD *
 DELETE  (ARI02.ARI0206.CICS.ARTKSDS) CLUSTER
 SET MAXCC = 0
 DEFINE CLUSTER (NAME(ARI02.ARI0206.CICS.ARTKSDS)        -
                 TRACK(1 1)                              -
                 VOLUME(WRK001)                          -
                 INDEXED                                 -
                 KEY(5 0)                                -
                 RECORDSIZE(140 140)                     -
                 FREESPACE(40 40) )                      -
         DATA   (NAME(ARI02.ARI0206.CICS.ARTKSDS.D)) -
         INDEX  (NAME(ARI02.ARI0206.CICS.ARTKSDS.I))
   REPRO INDATASET(ARISYS.ADREF.XV99R00.CICS.ARTTRIE)       -
           OUTDATASET(ARI02.ARI0206.CICS.ARTKSDS)
 PRINT INDATASET(ARISYS.ADREF.XV99R00.CICS.ARTTRIE)
/*
//SYSPRINT DD SYSOUT=*
//*
//* *=================================================================*
//* * DEUXIEME ETAPE:                                                 *
//* * - DEFINITION DE LA CLE SECONDAIRE                               *
//* *                                                                 *
//* *=================================================================*
//STEP3   EXEC PGM=IDCAMS
//SYSIN   DD *
 DEFINE AIX     (NAME(ARI02.ARI0206.CICS.ARTACAT)   -
                 RELATE(ARI02.ARI0206.CICS.ARTKSDS) -
                 TRACK(1 1)                                -
                 VOLUME(WRK001)                            -
                 KEY(5 25)                                 -
                 RECORDSIZE(140 140)                       -
                 FREESPACE(40 40)                          -
                 NONUNIQUEKEY                              -
                 UPGRADE)                                  -
         DATA   (NAME(ARI02.ARI0206.CICS.ARTACAT.D)) -
         INDEX  (NAME(ARI02.ARI0206.CICS.ARTACAT.I))
 DEFINE PATH (NAME(ARI02.ARI0206.CICS.ARTPCAT)      -
              PATHENTRY(ARI02.ARI0206.CICS.ARTACAT) -
              UPDATE)
 BLDINDEX  INDATASET(ARI02.ARI0206.CICS.ARTKSDS)    -
          OUTDATASET(ARI02.ARI0206.CICS.ARTACAT)

 PRINT  INDATASET(ARI02.ARI0206.CICS.ARTKSDS)       -
        CHAR
 PRINT  INDATASET(ARI02.ARI0206.CICS.ARTACAT)       -
        CHAR
 PRINT  INDATASET(ARI02.ARI0206.CICS.ARTPCAT)       -
        CHAR
/*
//SYSPRINT DD SYSOUT=*
