//ARI0206C JOB (ACCT#),'ARIT126',MSGCLASS=H,REGION=4M,
//    CLASS=A,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*===================================================================*
//*                         ESTIAC   INSTITUT                         *
//*                                                                   *
//*                      ETAPE DE COMPILATION COBOL                   *
//*                                                                   *
//* POUR COMPILER VOTRE JOB COBOL VOUS DEVEZ REMPLACER                *
//* LES PARAMETRES PAR :                                              *
//*                                                                   *
//*  MBR : NOM DU SOURCE                                              *
//*  SRC : CHEMIN D'ACCES DU FICHIER SOURCE                           *
//*  LMOD : CHEMIN D'ACCES DU LOAD                                    *
//*  COB.SYSLIB : CHEMIN D'ACCES DES COPY                             *
//*                                                                   *
//*===================================================================*
//*
//        JCLLIB ORDER=(ARISYS.ADREF.XV99R00.COBOL.ISPSLIB)
//*
//* ETAPE DE COMPILATION DU PROGRAMME COBOL
//*
//STEP1   EXEC PCOMPCOB,
//         MBR=ARIT126,
//         SRC=ARI02.ARI0206.SRC,
//         LMOD=ARI02.ARI0206.LOAD
//COB.SYSLIB DD DSN=ARI02.ARI0206.CPY,DISP=SHR
//
