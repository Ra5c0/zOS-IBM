//ARI0206A JOB (ACCT#),CLASS=A,MSGCLASS=X,
//    MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*
//* ********************************************************************
//*                                                                    *
//*  EXEMPLE DE TRI                                                    *
//*                                                                    *
//* ********************************************************************
//*
//STEPSORT EXEC PGM=SORT
//SORTIN   DD  DSN=ARI02.ARI0206.DB2TP2.FCPTE,DISP=OLD
//SORTOUT  DD  DSN=ARI02.ARI0206.DB2TP2.CPTTRI,DISP=(NEW,CATLG,DELETE),
//             VOL=SER=WRK001,UNIT=3390,
//             SPACE=(TRK,(10,5)),
//             DCB=(RECFM=FB,BLKSIZE=26000,LRECL=80,DSORG=PS)
//SORTWK01 DD   UNIT=SYSALLDA,SPACE=(CYL,(1,1))
//*SORTWK02 DD   UNIT=SYSALLDA,SPACE=(CYL,(1,1))
//SYSOUT   DD   SYSOUT=*
//SYSIN    DD   *
 SORT FIELDS=COPY
 OUTREC FIELDS=(1,28,C'-',
                29,2,C'-',
                31,2,
                33,10,ZD,SIGNS=(,-),EDIT=(SIIIIIIIT.TT),
                43,4,C'-',
                47,2,C'-',
                49,2)
/*
//
