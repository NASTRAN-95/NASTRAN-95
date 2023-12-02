!*==dpdaa.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dpdaa
   IMPLICIT NONE
   USE C_DPDCOM
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: k , khi , klo , ngrid
   EXTERNAL close , fread , gopen , mesage
!
! End of declarations rewritten by SPAG
!
!*****
! DPDAA PERFORMS A BINARY SEARCH IN EQDYN AND CONVERTS THE GRID NO
! AND COMPONENT CODE TO AN SIL VALUE.
!*****
!
!
!
!
!
   !>>>>EQUIVALENCE (Z(1),Zz(1)) , (Buf(1),Bufr(1)) , (Msg(2),Ngrid)
!
!*****
! IF EQDYN IS NOT IN CORE, READ IT IN AND SET FLAG.
!*****
   IF ( Ineq==0 ) THEN
      CALL gopen(Eqdyn,Z(Buf3),0)
      CALL fread(Eqdyn,Z,Neqdyn+1,1)
      CALL close(Eqdyn,1)
      Ineq = 1
   ENDIF
!*****
! PERFORM SEARCH.
!*****
   klo = 1
   khi = Kn
   ngrid = Buf(L)
   k = (klo+khi+1)/2
   SPAG_Loop_1_1: DO
      IF ( ngrid<Z(2*k-1) ) THEN
         khi = k
      ELSEIF ( ngrid==Z(2*k-1) ) THEN
         EXIT SPAG_Loop_1_1
      ELSE
         klo = k
      ENDIF
      IF ( khi-klo<1 ) THEN
         CALL mesage(30,Msg,Msg(2))
         Nogo = 1
         EXIT SPAG_Loop_1_1
      ELSEIF ( khi-klo==1 ) THEN
         IF ( k==klo ) THEN
            k = khi
         ELSE
            k = klo
         ENDIF
         klo = khi
      ELSE
         k = (klo+khi+1)/2
      ENDIF
   ENDDO SPAG_Loop_1_1
   Buf(L) = Z(2*k)
   IF ( Buf(L+1)/=0 ) Buf(L) = Buf(L) + Buf(L+1) - 1
END SUBROUTINE dpdaa
