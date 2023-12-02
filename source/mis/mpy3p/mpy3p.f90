!*==mpy3p.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mpy3p(Z,Iz,Dz)
   USE c_mpy3cp
   USE c_mpy3tl
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: Z
   INTEGER , DIMENSION(1) :: Iz
   REAL(REAL64) , DIMENSION(1) :: Dz
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: dfact
   REAL :: fact
   INTEGER :: i , i1 , i2 , iac , iacols , iakj , iat , ibcols , ic , ii , iii , ipoint , itrans , kb2 , kj , kj2 , l , l1 , ll ,   &
            & llp , lp
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!*****
!    PERFORMS MULTIPLICATION AND SUMMATION.
!*****
!
!
!
!
!
!
!
!
! SUBROUTINE CALL PARAMETERS
! FILES
!
!
!
   !>>>>EQUIVALENCE (fact,dfact)
! OPEN CORE POINTERS
   !>>>>EQUIVALENCE (Ipoint,Zpntrs(3)) , (Iacols,Zpntrs(5)) , (Itrans,Zpntrs(7)) , (Ic,Zpntrs(9)) , (Ibcols,Zpntrs(11)) ,                &
!>>>>    & (Iakj,Zpntrs(21))
!*****
!    LOOP FOR ACCUMULATING SUMS.
!*****
   kj = iakj + ka - 1
   kj2 = (iakj-1)/2 + ka
   kb = ibcols + prec*((kb-1)*n-1)
   IF ( code/=2 .AND. icore/=1 ) THEN
!*****
!    A(T)BA CASE.
!*****
      lp = ipoint - 1
      DO l = 1 , n
         spag_nextblock_1 = 1
         SPAG_DispatchLoop_1: DO
            SELECT CASE (spag_nextblock_1)
            CASE (1)
! CALCULATE FACTOR = B(LK)*A(KJ) TO BE MULTIPLIED TO NON-ZERO TERMS IN
! LTH COLUMN OF A(T)
               kb = kb + prec
               lp = lp + 1
               IF ( Iz(lp)==0 ) CYCLE
               IF ( prec==2 ) THEN
                  kb2 = (kb+1)/2
                  IF ( Dz(kb2)==0.0D0 ) CYCLE
                  dfact = Dz(kb2)*Dz(kj2)
               ELSE
                  IF ( Z(kb)==0.0 ) CYCLE
                  fact = Z(kb)*Z(kj)
               ENDIF
               i1 = Iz(lp)
               IF ( l/=n ) THEN
! ACCUMULATE SUMS FOR NON-ZERO TERMS IN COLUMN L OF A(T)
                  l1 = l + 1
                  llp = lp
                  DO ll = l1 , n
                     llp = llp + 1
                     IF ( Iz(llp)/=0 ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
               ENDIF
               i2 = laend
               spag_nextblock_1 = 3
            CASE (2)
               i2 = Iz(llp) - 1
               spag_nextblock_1 = 3
            CASE (3)
               iac = iacols + i1 - 2
               IF ( prec==2 ) THEN
! DOUBLE PRECISION CASE
                  iat = (itrans-3)/2 + i1
                  DO i = i1 , i2
                     iac = iac + 1
                     iat = iat + 1
                     ii = (ic-1)/2 + Iz(iac)
                     Dz(ii) = Dz(ii) + Dz(iat)*dfact
                  ENDDO
                  iii = (ic-1)/2 + 1
               ELSE
! SINGLE PRECISION CASE
                  iat = itrans + i1 - 2
                  DO i = i1 , i2
                     iac = iac + 1
                     iat = iat + 1
                     ii = ic + Iz(iac) - 1
                     Z(ii) = Z(ii) + Z(iat)*fact
                  ENDDO
               ENDIF
               EXIT SPAG_DispatchLoop_1
            END SELECT
         ENDDO SPAG_DispatchLoop_1
      ENDDO
!*****
!    BA CASE.
!*****
   ELSEIF ( prec==2 ) THEN
! DOUBLE PRECISION CASE
      ii = (ic-1)/2
      kb = (kb+1)/2
      DO i = 1 , n
         ii = ii + 1
         kb = kb + 1
         IF ( Dz(kb)/=0.0D0 ) Dz(ii) = Dz(ii) + Dz(kb)*Dz(kj2)
      ENDDO
   ELSE
! SINGLE PRECISION CASE
      ii = ic - 1
      DO i = 1 , n
         ii = ii + 1
         kb = kb + 1
         IF ( Z(kb)/=0.0 ) Z(ii) = Z(ii) + Z(kb)*Z(kj)
      ENDDO
   ENDIF
!
END SUBROUTINE mpy3p
