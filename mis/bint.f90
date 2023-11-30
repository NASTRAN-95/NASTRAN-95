
FUNCTION bint(I,J,A,B,Iv,Iw,R,Z)
   IMPLICIT NONE
   REAL A , B
   INTEGER I , Iv , Iw , J
   REAL bint
   REAL R(1) , Z(1)
   REAL aj , aw , c1 , c1p , c2 , c2p , coef , sp1
   INTEGER ic , id , in , is1 , it , iw1 , k
   bint = 0.0
   iw1 = Iw + 1
   c1p = B
   c2p = A
   c1 = c1p
   c2 = c2p
   aw = 0.0
   IF ( R(I)/=0.0E0 .AND. R(J)/=0.0E0 ) aw = alog(R(J)/R(I))
   DO it = 1 , iw1
      ic = Iw - it + 1
      IF ( ic==0 ) c1 = 1.0
      IF ( it==1 ) c2 = 1.0
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! THE FOLLOWING CODE REPLACES REAL FUNCTION COEF
!
      IF ( it==1 ) THEN
         coef = 1.0
      ELSE
         in = 1
         id = 1
         DO k = 2 , it
            in = in*(Iw-k+2)
            id = id*(k-1)
         ENDDO
         coef = in/id
      ENDIF
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! THE FOLLOWING CODE REPLACES REAL FUNCTION AJ
!
      is1 = ic + Iv + 1
      IF ( is1==0 ) THEN
         aj = aw
      ELSE
         sp1 = is1
         aj = (R(J)**is1-R(I)**is1)/sp1
      ENDIF
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      bint = bint + c1**ic*aj*c2**(it-1)*coef
      c1 = c1p
      c2 = c2p
   ENDDO
   aw = Iw
   bint = bint/aw
END FUNCTION bint