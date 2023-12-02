!*==mpy3c.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mpy3c(Z,Iz,Dz)
   IMPLICIT NONE
   USE C_MPY3CP
   USE C_MPY3TL
   USE C_UNPAKX
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: Z
   INTEGER , DIMENSION(1) :: Iz
   REAL :: Dz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: file , i , ia , iantu , ib , ibcid , ibcols , ibntu , ik , iktbp , kb , kk , l , lta , ltb , precn
   EXTERNAL filpos , mpy3nu , mpy3p , unpack
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!*****
!    PERFORMS MULTIPLICATION AND SUMMATION FOR REMAINING TERMS OF COLUMN
!    OF A.
!*****
!
!
!
!
!
!
!     DIMENSION NAME(2)
!
!
! FILES
! SUBROUTINE CALL PARAMETERS
! UNPACK
!
!
!
   !>>>>EQUIVALENCE (Ibcols,Zpntrs(11)) , (Ibcid,Zpntrs(13)) , (Ibntu,Zpntrs(15)) , (Iktbp,Zpntrs(17)) , (Iantu,Zpntrs(19))
!
!
!
!     DATA NAME / 4HMPY3,4HC    /
!*****
!    INITIALIZATION.
!*****
         Utyp = Prec
         Urow1 = 1
         Urown = N
         Uincr = 1
         precn = Prec*N
         file = Scr1
!*****
!    TEST TO SEE IF LESS THAN NK COLUMNS OF B IN CORE.
!*****
         IF ( First2 ) THEN
!*****
!    LESS THAN NK COLUMNS OF B IN CORE.
!*****
            K2 = K2 + 1
            Ltbc = K2
            kk = iktbp - 1
            SPAG_Loop_1_1: DO Ka = 1 , K
               kk = kk + 1
               IF ( Iz(kk)/=0 ) THEN
                  Ltac = Iz(kk)
                  EXIT SPAG_Loop_1_1
               ENDIF
            ENDDO SPAG_Loop_1_1
         ELSE
!*****
!    DETERMINE WHICH COLUMN OF B TO BE PUT INTO CORE.
!*****
            lta = 0
            ia = iantu - 1
            DO i = 1 , K
               ia = ia + 1
               IF ( lta<Iz(ia) ) THEN
                  lta = Iz(ia)
                  ik = iktbp + i - 1
                  Ltac = Iz(ik)
                  Ka = i
               ENDIF
            ENDDO
!*****
!    DETERMINE WHICH COLUMN OF B TO BE REPLACED.
!*****
            ltb = 0
            ib = ibntu - 1
            DO i = 1 , Nk
               ib = ib + 1
               IF ( ltb<Iz(ib) ) THEN
                  ltb = Iz(ib)
                  Ltbc = i
               ENDIF
            ENDDO
         ENDIF
!*****
!    ADD OR REPLACE COLUMN OF B INTO CORE.
!*****
         CALL filpos(Scr1,Iz(Ltac))
         kk = ibcols + precn*(Ltbc-1)
         CALL unpack(*20,Scr1,Z(kk))
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 20      ik = kk - 1
         DO l = 1 , precn
            ik = ik + 1
            Z(ik) = 0.
         ENDDO
         spag_nextblock_1 = 2
      CASE (2)
         IF ( .NOT.(First2) ) THEN
            IF ( Icore/=1 ) THEN
               CALL mpy3nu(Z)
               kk = ibntu + Ltbc - 1
               Iz(kk) = Ntbu
            ENDIF
            kk = iantu + Ka - 1
            Iz(kk) = 0
         ENDIF
         kk = ibcid + Ltbc - 1
         Iz(kk) = Ltac
         kb = Ltbc
!*****
!    PERFORM COMPUTATION.
!*****
         CALL mpy3p(Z,Z,Z)
         Ltbc = kb
         kk = iktbp + Ka - 1
         Iz(kk) = 0
         Kcount = Kcount + 1
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE mpy3c
