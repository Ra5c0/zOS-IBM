//ARI0206C JOB (ACCT#),'ARID226',MSGCLASS=H,REGION=4M,
//    CLASS=A,MSGLEVEL=(1,1),NOTIFY=&SYSUID,
//    COND=(4,LT)
//*===================================================================*
//*                        ESTIAC   INSTITUT                          *
//*                                                                   *
//*                      ETAPE DE COMPILATION COBOL DB2               *
//*                                                                   *
//* POUR COMPILER VOTRE JOB COBOL VOUS DEVEZ REMPLACER                *
//* LES PARAMETRES PAR :                                              *
//*                                                                   *
//*  MBR : NOM DU SOURCE                                              *
//*  SRC : CHEMIN D'ACCES DU FICHIER SOURCE                           *
//*  LMOD : CHEMIN D'ACCES DU LOAD                                    *
//*  LIB  : CHEMIN D'ACCES DES COPY COBOL                             *
//*                                                                   *
//*===================================================================*
//*
//        JCLLIB ORDER=(ARISYS.ADREF.XV99R00.DB2.ISPSLIB)
//*
//* ETAPE DE COMPILATION DU PROGRAMME COBOL
//*
//STEP1   EXEC PCOMPDB2,
//        MBR=ARID226,
//        SRC=ARI02.ARI0206.SRC,
//        LMOD=ARI02.ARI0206.LOAD,
//        LIB=ARI02.ARI0206.CPY,
//        DBRML=ARI02.ARI0206.DBRM
//
