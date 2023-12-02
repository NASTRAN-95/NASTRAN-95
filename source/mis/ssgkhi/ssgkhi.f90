!*==ssgkhi.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ssgkhi(Treal,Tint,Fn)
   USE c_matout
   USE c_ssgtri
   USE c_system
   USE c_trimex
   USE c_xmssg
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(6) :: Treal
   INTEGER , DIMENSION(6) :: Tint
   REAL :: Fn
!
! Local variable declarations rewritten by SPAG
!
   REAL :: determ
   INTEGER , DIMENSION(9) :: index
   INTEGER :: ising
   EXTERNAL gmmats , invers , mesage
!
! End of declarations rewritten by SPAG
!
!
!     THIS SUBROUTINE COMPUTES THE (5X1) KHI  VECTOR FOR USE BY TRBSC,
!                                           E
!     TRPLT, AND QDPLT.
!
!     WHEN PROCESSING THE TRPLT OR QDPLT THIS ROUTINE SHOULD BE CALLED
!     AFTER THE FIRST SUBTRIANGLE ONLY DUE TO THE D MATRIX ORIENTATION.
!
!
!     DETERMINE TYPE OF TEMPERATURE DATA
!
   IF ( Tint(6)/=1 ) THEN
!
!     TEMPERATURE DATA IS TEMPP2 TYPE.
!
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
      ising = -1
      CALL invers(3,d(1),3,0,0,determ,ising,index)
      IF ( ising==2 ) THEN
         WRITE (iout,99001) ufm , eid
99001    FORMAT (A23,' 4018, A SINGULAR MATERIAL MATRIX -D- FOR ELEMENT',I9,' HAS BEEN DETECTED BY ROUTINE SSGKHI',/26X,'WHILE ',   &
                &'TRYING TO COMPUTE THERMAL LOADS WITH TEMPP2 CARD DATA.')
         CALL mesage(-61,0,0)
      ENDIF
      CALL gmmats(d(1),3,3,0,Treal(2),3,1,0,khi(1))
      khi(1) = khi(1)*Fn
      khi(2) = khi(2)*Fn
      khi(3) = khi(3)*Fn
   ELSE
!
!     TEMPERATURE DATA IS TEMPP1 OR TEMPP3 TYPE.
!
      khi(1) = -alpha1*Treal(2)*Fn
      khi(2) = -alpha2*Treal(2)*Fn
      khi(3) = -alph12*Treal(2)*Fn
   ENDIF
   khi(4) = 0.0
   khi(5) = 0.0
END SUBROUTINE ssgkhi
