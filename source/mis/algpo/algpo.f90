!*==algpo.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE algpo(Scr1)
   IMPLICIT NONE
   USE C_BLANK
   USE C_NAMES
   USE C_SYSTEM
   USE C_TWO
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Scr1
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: caseca , casecc , geom3a , labp , labt
   REAL :: dtemp
   INTEGER :: i1 , i2 , ibit , ibuf1 , ibuf2 , idp , idt , ifile , itpd , izx , kaperr , katerr , last , left , nwar , nwds , nz
   INTEGER , DIMENSION(7) :: itrl
   INTEGER , DIMENSION(3) , SAVE :: lend , pload2 , temp , tempd
   INTEGER , DIMENSION(5) :: lrec
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(5) :: rrec
   EXTERNAL close , corwds , fwdrec , gopen , korsz , mesage , open , orf , rdtrl , read , rewind , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
   !>>>>EQUIVALENCE (lrec(1),rrec(1))
   DATA lend/3*2147483647/
   DATA name/4HALG  , 4H    /
   DATA labp/4HPLOA/ , labt/4HTEMP/
   DATA pload2/6809 , 68 , 199/ , temp/5701 , 57 , 27/ , tempd/5641 , 65 , 98/
   DATA casecc , caseca , geom3a/101 , 201 , 202/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         IF ( Apress<0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ifile = Scr1
         CALL open(*220,Scr1,Iz(ibuf2),Rdrew)
         CALL read(*40,*20,Scr1,lrec,5,1,nwar)
 20      DO WHILE ( lrec(1)/=labp )
            CALL read(*40,*20,Scr1,lrec,5,1,nwar)
         ENDDO
!
!     CREATE PLOAD2 RECORD
!
         CALL write(geom3a,pload2,3,0)
         idp = lrec(3)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     NO PLOAD2 CARDS ON SCR1 FILE
!
 40      kaperr = 1
         idp = 0
         CALL rewind(Scr1)
         WRITE (Nout,99001) Uwm
!
99001    FORMAT (A25,' - ALG MODULE - AERODYNAMIC PRESSURES REQUESTED VIA',' PARAM APRESS, BUT NOUT3=0 IN AERODYNAMIC INPUT',/41X,  &
                &'OR AERODYNAMIC CALCULATION FAILED. REQUEST IGNORED.')
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 60      IF ( lrec(1)/=labp ) GOTO 80
         spag_nextblock_1 = 2
      CASE (2)
         CALL write(geom3a,lrec(3),3,0)
         CALL read(*80,*60,Scr1,lrec,5,1,nwar)
         GOTO 60
 80      CALL write(geom3a,Iz,0,1)
         CALL rewind(Scr1)
         spag_nextblock_1 = 3
      CASE (3)
!
!     AERODYNAMIC TEMPERATURE SECTION
!
         IF ( Atemp<0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Apress<0 ) CALL open(*220,Scr1,Iz(ibuf2),Rdrew)
         CALL read(*120,*100,Scr1,lrec,5,1,nwar)
 100     DO WHILE ( lrec(1)/=labt )
            CALL read(*120,*100,Scr1,lrec,5,1,nwar)
         ENDDO
!
!     CREATE TEMP RECORD
!
         CALL write(geom3a,temp,3,0)
         idt = lrec(3)
         dtemp = rrec(5)
         itpd = 1
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
!
!     NO TEMP CARDS ON SCR1 FILE
!
 120     katerr = 1
         idt = 0
         WRITE (Nout,99002) Uwm
99002    FORMAT (A25,' - ALG MODULE - AERODYNAMIC TEMPERATURES REQUESTED ','VIA PARAM ATEMP, BUT NOUT3=0 IN AERODYNAMIC INPUT',/41X,&
                &'OR AERODYNAMIC CALCULATION FAILED. REQUEST IGNORED.')
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 140     IF ( lrec(1)/=labt ) GOTO 160
         spag_nextblock_1 = 4
      CASE (4)
         CALL write(geom3a,lrec(3),3,0)
         itpd = itpd + 1
         IF ( itpd<=3 ) dtemp = dtemp + rrec(5)
         CALL read(*160,*140,Scr1,lrec,5,1,nwar)
         GOTO 140
 160     CALL write(geom3a,Iz,0,1)
!
!     CREATE TEMPD RECORD. AVERAGE FIRST THREE TEMPS. ON BLADE ROOT.
!
         CALL write(geom3a,tempd,3,0)
         CALL write(geom3a,idt,1,0)
         dtemp = dtemp/3.0
         CALL write(geom3a,dtemp,1,1)
         spag_nextblock_1 = 5
      CASE (5)
!
!     CLOSE GEOM3A
!
         CALL write(geom3a,lend,3,1)
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
         CALL open(*220,casecc,Iz(ibuf1),Rdrew)
         CALL fwdrec(*240,casecc)
         CALL gopen(caseca,Iz(ibuf2),Wrtrew)
         CALL read(*200,*180,casecc,Iz,left,1,nwds)
 180     DO
            izx = 4
            Iz(izx) = idp
            izx = 7
            Iz(izx) = idt
            CALL write(caseca,Iz,nwds,1)
            CALL read(*200,*180,casecc,Iz,left,1,nwds)
         ENDDO
 200     CALL close(casecc,1)
         CALL close(caseca,1)
         itrl(1) = casecc
         CALL rdtrl(itrl)
         itrl(1) = caseca
         CALL wrttrl(itrl)
         RETURN
 220     CALL mesage(-1,ifile,name)
         RETURN
 240     CALL mesage(-2,ifile,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE algpo
