!*==mpy3c.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mpy3c(Z,Iz,Dz)
   USE c_mpy3cp
   USE c_mpy3tl
   USE c_unpakx
   IMPLICIT NONE
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
         utyp = prec
         urow1 = 1
         urown = n
         uincr = 1
         precn = prec*n
         file = scr1
!*****
!    TEST TO SEE IF LESS THAN NK COLUMNS OF B IN CORE.
!*****
         IF ( first2 ) THEN
!*****
!    LESS THAN NK COLUMNS OF B IN CORE.
!*****
            k2 = k2 + 1
            ltbc = k2
            kk = iktbp - 1
            SPAG_Loop_1_1: DO ka = 1 , k
               kk = kk + 1
               IF ( Iz(kk)/=0 ) THEN
                  ltac = Iz(kk)
                  EXIT SPAG_Loop_1_1
               ENDIF
            ENDDO SPAG_Loop_1_1
         ELSE
!*****
!    DETERMINE WHICH COLUMN OF B TO BE PUT INTO CORE.
!*****
            lta = 0
            ia = iantu - 1
            DO i = 1 , k
               ia = ia + 1
               IF ( lta<Iz(ia) ) THEN
                  lta = Iz(ia)
                  ik = iktbp + i - 1
                  ltac = Iz(ik)
                  ka = i
               ENDIF
            ENDDO
!*****
!    DETERMINE WHICH COLUMN OF B TO BE REPLACED.
!*****
            ltb = 0
            ib = ibntu - 1
            DO i = 1 , nk
               ib = ib + 1
               IF ( ltb<Iz(ib) ) THEN
                  ltb = Iz(ib)
                  ltbc = i
               ENDIF
            ENDDO
         ENDIF
!*****
!    ADD OR REPLACE COLUMN OF B INTO CORE.
!*****
         CALL filpos(scr1,Iz(ltac))
         kk = ibcols + precn*(ltbc-1)
         CALL unpack(*20,scr1,Z(kk))
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 20      ik = kk - 1
         DO l = 1 , precn
            ik = ik + 1
            Z(ik) = 0.
         ENDDO
         spag_nextblock_1 = 2
      CASE (2)
         IF ( .NOT.(first2) ) THEN
            IF ( icore/=1 ) THEN
               CALL mpy3nu(Z)
               kk = ibntu + ltbc - 1
               Iz(kk) = ntbu
            ENDIF
            kk = iantu + ka - 1
            Iz(kk) = 0
         ENDIF
         kk = ibcid + ltbc - 1
         Iz(kk) = ltac
         kb = ltbc
!*****
!    PERFORM COMPUTATION.
!*****
         CALL mpy3p(Z,Z,Z)
         ltbc = kb
         kk = iktbp + ka - 1
         Iz(kk) = 0
         kcount = kcount + 1
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE mpy3c
