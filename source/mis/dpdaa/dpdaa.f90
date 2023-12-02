!*==dpdaa.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dpdaa
   USE c_dpdcom
   USE c_zzzzzz
   IMPLICIT NONE
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
   IF ( ineq==0 ) THEN
      CALL gopen(eqdyn,z(buf3),0)
      CALL fread(eqdyn,z,neqdyn+1,1)
      CALL close(eqdyn,1)
      ineq = 1
   ENDIF
!*****
! PERFORM SEARCH.
!*****
   klo = 1
   khi = kn
   ngrid = buf(l)
   k = (klo+khi+1)/2
   SPAG_Loop_1_1: DO
      IF ( ngrid<z(2*k-1) ) THEN
         khi = k
      ELSEIF ( ngrid==z(2*k-1) ) THEN
         EXIT SPAG_Loop_1_1
      ELSE
         klo = k
      ENDIF
      IF ( khi-klo<1 ) THEN
         CALL mesage(30,msg,msg(2))
         nogo = 1
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
   buf(l) = z(2*k)
   IF ( buf(l+1)/=0 ) buf(l) = buf(l) + buf(l+1) - 1
END SUBROUTINE dpdaa
