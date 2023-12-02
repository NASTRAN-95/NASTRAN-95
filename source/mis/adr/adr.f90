!*==adr.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE adr
   IMPLICIT NONE
   USE C_BLANK
   USE C_CONDAS
   USE C_PACKX
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: casecc , disp , flut , freq , iaero , load , pkf , qkhl , scr1 , scr2 , scr3 , scr4 , sila , spline , useta
   INTEGER :: i , ibuf1 , ibuf2 , ibuf3 , ipa , ipd , ipq , j , k , l , m , nam , ncol , ncore , next , nfreq , nload , nns1 ,      &
            & nogo , nrow , nterma , ntermd , nterms , nw
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(7) :: mcb
   EXTERNAL adri , adrprt , close , dmpfil , fwdrec , gmmatc , gopen , korsz , mesage , open , pack , rdtrl , read , rewind ,       &
          & skprec , unpack , wrttrl , zeroc
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     AERODYNAMIC DATA RECOVERY   -  FORCE OUTPUT BY SET SELECTION
!
!     DMAP
!     FLUTTER
!     ADR  CPHIH1,CASEZZ,QKHL,CLAMAL1,SPLINE,SILA,USETA/PKF/C,N,BOV/C,
!          N,MACH=0.0/C,N,APP $
!     DYNAMICS
!     ADR  UHVT1,CASECC,QKHL,TOL1,SPLINE,SILA,USETA/PKF/V,N,BOV/C,Y,
!          MACH=0.0/C,N,APP $
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
   DATA iaero/176/
   DATA flut/4HFLUT/ , freq/4HFREQ/
   DATA disp/101/ , casecc/102/ , qkhl/103/ , load/104/
   DATA spline/105/ , sila/106/ , useta/107/ , pkf/201/
   DATA scr1/301/ , scr2/302/ , scr3/303/ , scr4/304/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
!     BUILD    P    =  Q    *  U
!               KF      KH      H
!     WHERE  QKH INTERPOLATED FOR A EIGENVALUE OR FREQUENCY - MACH DEP.
!            UH  - EIGENVALUE OR FREQUENCY
!
!
!     INITIALIZE  - LOOK FOR A REQUEST
!
         IF ( App/=flut .AND. App/=freq ) GOTO 80
         ncore = korsz(Z)
         ibuf1 = ncore - Sysbuf
         CALL open(*80,casecc,iz(ibuf1),0)
         CALL fwdrec(*80,casecc)
         CALL read(*80,*20,casecc,Z,ibuf1,0,nw)
 20      IF ( iz(iaero)/=0 ) THEN
            CALL close(casecc,1)
!
!     BUILD INTERPOLATED MATRIX FROM QHKL ON SCR1
!     DEPENDENT LIST
!     IF CLAMAL1 PICK UP FREQUENCY FROM OFP TABLE
!     IF TOL1    PICK UP FREQUENCY FROM HEADER
!     INDEPENDENT LIST ON QKHL
!
            CALL open(*80,load,iz(ibuf1),0)
            IF ( App==flut ) THEN
!
!     CLAMAL1 = LOAD
!
               CALL fwdrec(*80,load)
               CALL fwdrec(*80,load)
               CALL read(*80,*60,load,iz,ibuf1,0,nfreq)
            ELSE
!
!     TOL1 = LOAD
!
               mcb(1) = casecc
               CALL rdtrl(mcb)
               CALL read(*80,*80,load,iz,-2,0,nfreq)
               CALL read(*80,*40,load,iz,ibuf1,0,nfreq)
            ENDIF
!
!     ERROR MESSAGES
!
            CALL mesage(8,0,nam)
         ENDIF
         GOTO 80
 40      nload = mcb(2)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 60      nfreq = nfreq/6
         IF ( Bov==0.0 ) THEN
            WRITE (Out,99001) Uim
99001       FORMAT (A29,' 2272, NO FLUTTER CALCULATIONS CAN BE MADE IN ','MODULE ADR SINCE BOV = 0.0.')
            GOTO 80
         ELSE
            DO i = 1 , nfreq
               k = i*6 - 1
               Z(i) = Z(k)/(Twopi*Bov)
            ENDDO
            nload = 1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     CALL ADRI TO BUILD  (AFTER ADRI FREQUENCY*2PI*BOV IS IN Z AT EVERY
!     OTHER SLOT 0.0 ,W FOR NFREQ*2
!
         CALL close(load,1)
         CALL adri(Z,nfreq,ncore,qkhl,scr1,scr2,scr3,scr4,nrow,ncol,nogo)
         IF ( nogo==0 ) THEN
!
!     SCR1 NOW HAS QKH INTERPOLATED    NROW*NCOL(ROW5)  NFREQ(COLUMNS)
!
            ipq = nfreq*2 + 1
!
!     BUILD PKF
!
            Iout = 3
            Iti = 3
            Ito = 3
            Incr = 1
            Incr1 = 1
            mcb(1) = disp
            CALL rdtrl(mcb)
            IF ( mcb(1)>=0 ) THEN
               IF ( mcb(3)/=ncol ) THEN
                  CALL mesage(7,0,nam)
               ELSE
                  nns1 = nrow*ncol
                  Ii = 1
                  Nn = nrow
                  Inn = 1
                  ibuf2 = ibuf1 - Sysbuf
                  CALL gopen(pkf,Z(ibuf2),1)
                  ibuf3 = ibuf2 - Sysbuf
                  CALL gopen(disp,Z(ibuf3),0)
                  CALL gopen(scr1,Z(ibuf1),0)
                  mcb(1) = pkf
                  mcb(2) = 0
                  mcb(3) = Nn
                  mcb(6) = 0
                  mcb(7) = 0
                  nterms = nns1*2
                  ntermd = ncol*2
                  nterma = nrow*2
                  ipd = ipq + nterms
                  ipa = ipd + ntermd
                  next = ipa + nterma
                  IF ( next>ibuf3 ) THEN
                     CALL mesage(8,0,nam)
                  ELSE
                     DO i = 1 , nload
                        DO j = 1 , nfreq
                           spag_nextblock_2 = 1
                           SPAG_DispatchLoop_2: DO
                              SELECT CASE (spag_nextblock_2)
                              CASE (1)
!
!     UNPACK INTERPOLATED MATRIX COLUMN THEN DISP VECTOR  MULTIPLY AND
!     PACK OUT
!
                                 Nnn = nns1
                                 CALL unpack(*62,scr1,Z(ipq))
!
!     MULTIPLY BACK BY FREQUENCY (K)
!
                                 DO l = 1 , nterms , 2
                                    m = j*2
                                    Z(ipq+l) = Z(ipq+l)*Z(m)
                                 ENDDO
                                 spag_nextblock_2 = 2
                                 CYCLE SPAG_DispatchLoop_2
 62                              CALL zeroc(Z(ipq),nterms)
                                 spag_nextblock_2 = 2
                              CASE (2)
                                 Nnn = ncol
                                 CALL unpack(*64,disp,Z(ipd))
                                 spag_nextblock_2 = 3
                                 CYCLE SPAG_DispatchLoop_2
 64                              CALL zeroc(Z(ipd),ntermd)
                                 spag_nextblock_2 = 3
                              CASE (3)
                                 CALL gmmatc(Z(ipd),1,ncol,0,Z(ipq),ncol,nrow,0,Z(ipa))
                                 CALL pack(Z(ipa),pkf,mcb)
                                 EXIT SPAG_DispatchLoop_2
                              END SELECT
                           ENDDO SPAG_DispatchLoop_2
                        ENDDO
                        IF ( i/=nload ) THEN
                           CALL rewind(scr1)
                           CALL skprec(scr1,1)
                        ENDIF
                     ENDDO
                     CALL close(scr1,1)
                     CALL close(disp,1)
                     CALL close(pkf,1)
                     CALL wrttrl(mcb)
                     CALL dmpfil(-pkf,Z(ipq),ibuf3-ipq)
!
!     PUT FREQUENCY BACK TO ORIGINAL VALUE
!
                     DO i = 1 , nfreq
                        Z(i) = Z(i*2)/(Twopi*Bov)
                     ENDDO
!
!     PRINT RESULTS
!
                     CALL adrprt(casecc,pkf,spline,sila,useta,Z,nfreq,ncore,nload)
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
!
!     STOP  CLOSE ALL POSSIBLE OPENS
!
 80      CALL close(casecc,1)
         CALL close(load,1)
         CALL close(pkf,1)
         CALL close(disp,1)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE adr
