!*==mpy3p.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mpy3p(Z,Iz,Dz)
USE C_MPY3CP
USE C_MPY3TL
USE ISO_FORTRAN_ENV                 
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
   kj = iakj + Ka - 1
   kj2 = (iakj-1)/2 + Ka
   Kb = ibcols + Prec*((Kb-1)*N-1)
   IF ( Code/=2 .AND. Icore/=1 ) THEN
!*****
!    A(T)BA CASE.
!*****
      lp = ipoint - 1
      DO l = 1 , N
         spag_nextblock_1 = 1
         SPAG_DispatchLoop_1: DO
            SELECT CASE (spag_nextblock_1)
            CASE (1)
! CALCULATE FACTOR = B(LK)*A(KJ) TO BE MULTIPLIED TO NON-ZERO TERMS IN
! LTH COLUMN OF A(T)
               Kb = Kb + Prec
               lp = lp + 1
               IF ( Iz(lp)==0 ) CYCLE
               IF ( Prec==2 ) THEN
                  kb2 = (Kb+1)/2
                  IF ( Dz(kb2)==0.0D0 ) CYCLE
                  dfact = Dz(kb2)*Dz(kj2)
               ELSE
                  IF ( Z(Kb)==0.0 ) CYCLE
                  fact = Z(Kb)*Z(kj)
               ENDIF
               i1 = Iz(lp)
               IF ( l/=N ) THEN
! ACCUMULATE SUMS FOR NON-ZERO TERMS IN COLUMN L OF A(T)
                  l1 = l + 1
                  llp = lp
                  DO ll = l1 , N
                     llp = llp + 1
                     IF ( Iz(llp)/=0 ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                  ENDDO
               ENDIF
               i2 = Laend
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            CASE (2)
               i2 = Iz(llp) - 1
               spag_nextblock_1 = 3
            CASE (3)
               iac = iacols + i1 - 2
               IF ( Prec==2 ) THEN
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
   ELSEIF ( Prec==2 ) THEN
! DOUBLE PRECISION CASE
      ii = (ic-1)/2
      Kb = (Kb+1)/2
      DO i = 1 , N
         ii = ii + 1
         Kb = Kb + 1
         IF ( Dz(Kb)/=0.0D0 ) Dz(ii) = Dz(ii) + Dz(Kb)*Dz(kj2)
      ENDDO
   ELSE
! SINGLE PRECISION CASE
      ii = ic - 1
      DO i = 1 , N
         ii = ii + 1
         Kb = Kb + 1
         IF ( Z(Kb)/=0.0 ) Z(ii) = Z(ii) + Z(Kb)*Z(kj)
      ENDDO
   ENDIF
!
END SUBROUTINE mpy3p
