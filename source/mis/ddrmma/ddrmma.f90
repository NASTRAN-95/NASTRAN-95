!*==ddrmma.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ddrmma(Setup)
   IMPLICIT NONE
   USE C_CLSTRS
   USE C_DDRMC1
   USE C_STDATA
   USE C_ZNTPKX
!
! Dummy argument declarations rewritten by SPAG
!
   LOGICAL :: Setup
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(150) :: buf
   INTEGER , DIMENSION(75) :: bufa , bufb
   INTEGER , DIMENSION(300) :: elwork
   INTEGER :: i , icomp , iout , j , k , npt , typout
   REAL , DIMENSION(75) :: rbufa , rbufb
   EXTERNAL intpk , magpha , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!*****
!  UNPACKS DATA FROM A TRANSIENT OR FREQUENCY RESPONSE SOLUTION
!  COLUMN AS REQUIRED TO FORM ONE OFP OUTPUT LINE ENTRY.
!
!  BEFORE CALLING FOR ENTRY CONSTRUCTION ONE SETUP CALL IS REQUIRED
!  FOR EACH COLUMN. (SETUP = .TRUE.)
!*****
!
!
!
!
   !>>>>EQUIVALENCE (Buf(1),Rbuf(1),Bufa(1),Rbufa(1)) , (Rbufb(1),Bufb(1),Buf(76))
!*****
!  PERFORM SOLUTION COLUMN SETUP WHEN SETUP = .TRUE.
!*****
         IF ( .NOT.Setup ) THEN
!*****
!  FILL BUFFER WITH REAL AND OR COMPLEX VALUES.
!*****
            k = I1 - 1
            DO i = 1 , k
               bufb(i) = bufa(i)
            ENDDO
            DO i = I1 , I2
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     IF ( icomp==Irow ) THEN
!
!     NON-ZERO COMPONENT AVAILABLE.
!
                        IF ( .NOT.Trnsnt ) THEN
                           IF ( Ipass==1 ) THEN
!
!     FREQUENCY RESPONSE FOR DISPLACEMENTS OR SPCFS PASS
!
                              rbufa(i) = A(1)
                              rbufb(i) = A(2)
                              IF ( Ieol<=0 ) THEN
                                 CALL zntpki
                              ELSE
!
                                 Irow = 0
                              ENDIF
                           ELSEIF ( Ipass==2 ) THEN
!
!     FREQUENCY RESPONSE VELOCITYS PASS
!
                              rbufa(i) = -Omega*A(2)
                              rbufb(i) = Omega*A(1)
                              IF ( Ieol<=0 ) THEN
                                 CALL zntpki
                              ELSE
                                 Irow = 0
                              ENDIF
                           ELSEIF ( Ipass==3 ) THEN
!
!     FREQUENCY RESPONSE ACCELERATIONS PASS
!
                              rbufa(i) = Omega*A(1)
                              rbufb(i) = Omega*A(2)
                              IF ( Ieol<=0 ) THEN
                                 CALL zntpki
                              ELSE
                                 Irow = 0
                              ENDIF
                           ELSE
                              GOTO 2
                           ENDIF
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
!
!     TRANSIENT RESPONSE
!
 2                      rbufa(i) = A(1)
                        IF ( Ieol<=0 ) THEN
                           CALL zntpki
                        ELSE
                           Irow = 0
                        ENDIF
                     ELSE
                        rbufa(i) = 0.0
                        rbufb(i) = 0.0
                     ENDIF
                     spag_nextblock_2 = 2
                  CASE (2)
!
                     icomp = icomp + 1
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
!
            ENDDO
!*****
!  IF TRANSIENT (REAL) THEN RETURN. FOR FREQUENCY (COMPLEX) COMBINE DATA
!  FOR OUTPUT AND CONVERT TO MAGNITUDE PHASE IF NECESSARY.
!
!  BUFA CONTAINS THE REAL PART
!  BUFB CONTAINS THE IMAGINARY PART
!*****
            IF ( Trnsnt ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Itype1==4 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Itype1==5 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     POINT DATA
!
            DO k = 1 , 6
               IF ( Form==3 ) CALL magpha(bufa(k+2),bufb(k+2))
               bufa(k+8) = bufb(k+2)
            ENDDO
            Nwdsf = 14
            RETURN
         ELSE
            typout = 3
            IF ( Trnsnt ) typout = 1
            icomp = 1
            CALL intpk(*20,Scrt(6),0,typout,0)
            CALL zntpki
            RETURN
         ENDIF
 20      Irow = 0
         RETURN
      CASE (2)
!
!     ELEMENT STRESS OR FORCE DATA
!
         IF ( .NOT.(Lminor) ) THEN
            DO k = 1 , Nstxtr
               j = Savpos(Npos+k-1)
               buf(j) = Bufsav(k)
            ENDDO
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         IF ( Trnsnt ) RETURN
         iout = 0
         i = Nptsf
         spag_nextblock_1 = 4
      CASE (4)
         npt = Complx(i)
         IF ( npt<0 ) THEN
            npt = -npt
            IF ( Form/=3 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     COMPUTE MAGNITUDE PHASE
!
            CALL magpha(bufa(npt),bufb(npt))
         ELSEIF ( npt==0 ) THEN
!
!     MOVE OUTPUT DATA
!
            DO i = 1 , iout
               buf(i) = elwork(i)
            ENDDO
            Nwdsf = iout
            RETURN
         ELSE
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         iout = iout + 1
         elwork(iout) = bufa(npt)
         i = i + 1
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (6)
         IF ( npt<=Lsf ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         npt = npt - Lsf
         iout = iout + 1
         elwork(iout) = bufb(npt)
         i = i + 1
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ddrmma
