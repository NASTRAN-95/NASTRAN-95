!*==fndpar.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fndpar(Np2,Index)
   USE c_oscent
   USE c_sem
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Np2
   INTEGER :: Index
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , itype , k , m , nip , nop , np , np1
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL andf , mesage
!
! End of declarations rewritten by SPAG
!
!
!     FNDPAR FINDS THE INDEX INTO THE  VPS FOR PARAMETER NUMBER NP
!     IN THE CURRENT OSCAR (THIS PARAMETER MUST BE VARIABLE)
!
   DATA name/4HFNDP , 4HAR  /
!
   nip = oscar(7)
   itype = andf(oscar(3),7)
   i = 8 + 3*nip
   IF ( itype/=2 ) THEN
      nop = oscar(i)
      i = i + 3*nop + 1
   ENDIF
   i = i + 1
   np1 = oscar(i)
   np = iabs(Np2)
   IF ( np>np1 ) THEN
      IF ( Np2<=0 ) THEN
!
!     PARAMETER SPORT NOT SUPPLIES
!
         Index = -1
         RETURN
      ELSE
         WRITE (nout,99001) ufm , np
99001    FORMAT (A23,' 3123, PARAMETER NUMBER',I6,' NOT IN DMAP CALL.')
         CALL mesage(-61,0,name)
      ENDIF
   ENDIF
   np1 = np - 1
   k = i + 1
   IF ( np1/=0 ) THEN
      DO i = 1 , np1
         m = oscar(k)
         IF ( m<0 ) THEN
!
!     VARTABLE
!
            k = k + 1
         ELSE
!
!     CONSTANT
!
            k = k + 1 + m
         ENDIF
      ENDDO
   ENDIF
!
!     K POINTS  TO WANTED OSCAR WORD
!
   IF ( oscar(k)>=0 ) THEN
      IF ( Np2<=0 ) THEN
         Index = -1
         RETURN
      ELSE
         WRITE (nout,99002) ufm , np
99002    FORMAT (A23,' 3124, PARAMETER NUMBER',I6,' IS NOT A VARIABLE.')
         CALL mesage(-61,0,name)
      ENDIF
   ENDIF
   Index = andf(oscar(k),mask3)
END SUBROUTINE fndpar
