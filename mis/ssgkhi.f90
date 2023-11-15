
SUBROUTINE ssgkhi(Treal,Tint,Fn)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Alph12 , Alpha1 , Alpha2 , D(9) , Dum(7) , Eid , Khi(5) , P(6) , Sysbuf
   INTEGER Iout , Ks(30)
   CHARACTER*23 Ufm
   COMMON /matout/ Dum , Alpha1 , Alpha2 , Alph12
   COMMON /ssgtri/ D , Khi , Ks , P
   COMMON /system/ Sysbuf , Iout
   COMMON /trimex/ Eid
   COMMON /xmssg / Ufm
!
! Dummy argument declarations
!
   REAL Fn
   INTEGER Tint(6)
   REAL Treal(6)
!
! Local variable declarations
!
   REAL determ
   INTEGER index(9) , ising
!
! End of declarations
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
      CALL invers(3,D(1),3,0,0,determ,ising,index)
      IF ( ising==2 ) THEN
         WRITE (Iout,99001) Ufm , Eid
99001    FORMAT (A23,' 4018, A SINGULAR MATERIAL MATRIX -D- FOR ELEMENT',I9,' HAS BEEN DETECTED BY ROUTINE SSGKHI',/26X,'WHILE ',   &
                &'TRYING TO COMPUTE THERMAL LOADS WITH TEMPP2 CARD DATA.')
         CALL mesage(-61,0,0)
      ENDIF
      CALL gmmats(D(1),3,3,0,Treal(2),3,1,0,Khi(1))
      Khi(1) = Khi(1)*Fn
      Khi(2) = Khi(2)*Fn
      Khi(3) = Khi(3)*Fn
   ELSE
!
!     TEMPERATURE DATA IS TEMPP1 OR TEMPP3 TYPE.
!
      Khi(1) = -Alpha1*Treal(2)*Fn
      Khi(2) = -Alpha2*Treal(2)*Fn
      Khi(3) = -Alph12*Treal(2)*Fn
   ENDIF
   Khi(4) = 0.0
   Khi(5) = 0.0
END SUBROUTINE ssgkhi
