
SUBROUTINE algpo(Scr1)
   IMPLICIT NONE
   INTEGER Apress , Atemp , Ifail , Iprtk , Iz(1) , Norew , Nout , Pgeom , Rd , Rdrew , Rew , Strml , Sysbuf , Two(32) , Wrt ,      &
         & Wrtrew
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / Apress , Atemp , Strml , Pgeom , Iprtk , Ifail
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew
   COMMON /system/ Sysbuf , Nout
   COMMON /two   / Two
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Iz
   INTEGER Scr1
   INTEGER caseca , casecc , geom3a , i1 , i2 , ibit , ibuf1 , ibuf2 , idp , idt , ifile , itpd , itrl(7) , izx , kaperr , katerr , &
         & labp , labt , last , left , lend(3) , lrec(5) , name(2) , nwar , nwds , nz , pload2(3) , temp(3) , tempd(3)
   INTEGER corwds , korsz , orf
   REAL dtemp , rrec(5)
   EXTERNAL orf
!
   !>>>>EQUIVALENCE (lrec(1),rrec(1))
   DATA lend/3*2147483647/
   DATA name/4HALG  , 4H    /
   DATA labp/4HPLOA/ , labt/4HTEMP/
   DATA pload2/6809 , 68 , 199/ , temp/5701 , 57 , 27/ , tempd/5641 , 65 , 98/
   DATA casecc , caseca , geom3a/101 , 201 , 202/
!
!     ALG WILL USE OPEN CORE AT IZ
!     ALLOCATE OPEN CORE
!
   nz = korsz(Iz)
   ibuf1 = nz - Sysbuf
   ibuf2 = ibuf1 - Sysbuf - 1
   last = ibuf2 - 1
!
!     CHECK FOR SUFFICIENT CORE
!
   IF ( last<=0 ) CALL mesage(-8,0,name)
   left = corwds(Iz(1),Iz(last))
   kaperr = 0
   katerr = 0
   Ifail = 1
!
!     OPEN GEOM3A FOR OUTPUT OF PLOAD2 AND TEMP DATA
!
   CALL gopen(geom3a,Iz(ibuf1),Wrtrew)
!
!     AERODYNAMIC PRESSURE SECTION
!
   IF ( Apress<0 ) GOTO 600
   ifile = Scr1
   CALL open(*1500,Scr1,Iz(ibuf2),Rdrew)
   CALL read(*200,*100,Scr1,lrec,5,1,nwar)
 100  DO WHILE ( lrec(1)/=labp )
      CALL read(*200,*100,Scr1,lrec,5,1,nwar)
   ENDDO
!
!     CREATE PLOAD2 RECORD
!
   CALL write(geom3a,pload2,3,0)
   idp = lrec(3)
   GOTO 400
!
!     NO PLOAD2 CARDS ON SCR1 FILE
!
 200  kaperr = 1
   idp = 0
   CALL rewind(Scr1)
   WRITE (Nout,99001) Uwm
!
99001 FORMAT (A25,' - ALG MODULE - AERODYNAMIC PRESSURES REQUESTED VIA',' PARAM APRESS, BUT NOUT3=0 IN AERODYNAMIC INPUT',/41X,     &
             &'OR AERODYNAMIC CALCULATION FAILED. REQUEST IGNORED.')
   GOTO 600
 300  IF ( lrec(1)/=labp ) GOTO 500
 400  CALL write(geom3a,lrec(3),3,0)
   CALL read(*500,*300,Scr1,lrec,5,1,nwar)
   GOTO 300
 500  CALL write(geom3a,Iz,0,1)
   CALL rewind(Scr1)
!
!     AERODYNAMIC TEMPERATURE SECTION
!
 600  IF ( Atemp<0 ) GOTO 1200
   IF ( Apress<0 ) CALL open(*1500,Scr1,Iz(ibuf2),Rdrew)
   CALL read(*800,*700,Scr1,lrec,5,1,nwar)
 700  DO WHILE ( lrec(1)/=labt )
      CALL read(*800,*700,Scr1,lrec,5,1,nwar)
   ENDDO
!
!     CREATE TEMP RECORD
!
   CALL write(geom3a,temp,3,0)
   idt = lrec(3)
   dtemp = rrec(5)
   itpd = 1
   GOTO 1000
!
!     NO TEMP CARDS ON SCR1 FILE
!
 800  katerr = 1
   idt = 0
   WRITE (Nout,99002) Uwm
99002 FORMAT (A25,' - ALG MODULE - AERODYNAMIC TEMPERATURES REQUESTED ','VIA PARAM ATEMP, BUT NOUT3=0 IN AERODYNAMIC INPUT',/41X,   &
             &'OR AERODYNAMIC CALCULATION FAILED. REQUEST IGNORED.')
   GOTO 1200
 900  IF ( lrec(1)/=labt ) GOTO 1100
 1000 CALL write(geom3a,lrec(3),3,0)
   itpd = itpd + 1
   IF ( itpd<=3 ) dtemp = dtemp + rrec(5)
   CALL read(*1100,*900,Scr1,lrec,5,1,nwar)
   GOTO 900
 1100 CALL write(geom3a,Iz,0,1)
!
!     CREATE TEMPD RECORD. AVERAGE FIRST THREE TEMPS. ON BLADE ROOT.
!
   CALL write(geom3a,tempd,3,0)
   CALL write(geom3a,idt,1,0)
   dtemp = dtemp/3.0
   CALL write(geom3a,dtemp,1,1)
!
!     CLOSE GEOM3A
!
 1200 CALL write(geom3a,lend,3,1)
   CALL close(geom3a,1)
   itrl(1) = geom3a
   itrl(2) = 0
   itrl(3) = 0
   itrl(4) = 0
   itrl(5) = 0
   itrl(6) = 0
   itrl(7) = 0
   IF ( Apress>=0 .AND. kaperr/=1 ) THEN
      ibit = 68
      i1 = (ibit-1)/16 + 2
      i2 = ibit - (i1-2)*16 + 16
      itrl(i1) = orf(itrl(i1),Two(i2))
   ENDIF
   IF ( Atemp>=0 .AND. katerr/=1 ) THEN
      ibit = 57
      i1 = (ibit-1)/16 + 2
      i2 = ibit - (i1-2)*16 + 16
      itrl(i1) = orf(itrl(i1),Two(i2))
      ibit = 65
      i1 = (ibit-1)/16 + 2
      i2 = ibit - (i1-2)*16 + 16
      itrl(i1) = orf(itrl(i1),Two(i2))
   ENDIF
   CALL wrttrl(itrl)
!
!     CLOSE SCR1
!
   IF ( Apress>=0 .OR. Atemp>=0 ) CALL close(Scr1,1)
   IF ( kaperr==1 ) Apress = -1
   IF ( katerr==1 ) Atemp = -1
!
!     SET IFAIL TO INDICATE ALG MODULE FAILED. CONDITIONAL JUMP BASED
!     ON VALUE OF IFAIL IS PERFORMED AFTER EXITING FROM ALG MODULE.
!
   IF ( Apress==-1 .AND. Atemp==-1 ) Ifail = -1
!
!     NEW CASE CONTROL DATA BLOCK
!     OPEN CASECC AND COPY ALL SUBCASES WITH CHANGES MADE TO
!     STATIC AND THERMAL LOAD ID-S
!
   ifile = casecc
   CALL open(*1500,casecc,Iz(ibuf1),Rdrew)
   CALL fwdrec(*1600,casecc)
   CALL gopen(caseca,Iz(ibuf2),Wrtrew)
   CALL read(*1400,*1300,casecc,Iz,left,1,nwds)
 1300 DO
      izx = 4
      Iz(izx) = idp
      izx = 7
      Iz(izx) = idt
      CALL write(caseca,Iz,nwds,1)
      CALL read(*1400,*1300,casecc,Iz,left,1,nwds)
   ENDDO
 1400 CALL close(casecc,1)
   CALL close(caseca,1)
   itrl(1) = casecc
   CALL rdtrl(itrl)
   itrl(1) = caseca
   CALL wrttrl(itrl)
   GOTO 1700
 1500 CALL mesage(-1,ifile,name)
   GOTO 1700
 1600 CALL mesage(-2,ifile,name)
 1700 RETURN
END SUBROUTINE algpo