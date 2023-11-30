
DOUBLE PRECISION FUNCTION dkint(I,J,A,B,Iv,Iw,R,Z)
   IMPLICIT NONE
   DOUBLE PRECISION A , B
   INTEGER I , Iv , Iw , J
   DOUBLE PRECISION R(1) , Z(1)
   DOUBLE PRECISION aw , bint , c1 , c1p , c2 , c2p , dkef , dkj , sp1
   INTEGER ic , id , in , is1 , it , iw1 , k
   bint = 0.0D0
   iw1 = Iw + 1
   c1p = B
   c2p = A
   c1 = c1p
   c2 = c2p
   aw = 0.0D0
   IF ( R(I)/=0.0D0 .AND. R(J)/=0.0D0 ) aw = dlog(R(J)/R(I))
   DO it = 1 , iw1
      ic = Iw - it + 1
      IF ( ic==0 ) c1 = 1.0D0
      IF ( it==1 ) c2 = 1.0D0
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! THE FOLLOWING CODE REPLACES DOUBLE PRECISION FUNCTION DKEF
!
      IF ( it==1 ) THEN
         dkef = 1.0D0
      ELSE
         in = 1
         id = 1
         DO k = 2 , it
            in = in*(Iw-k+2)
            id = id*(k-1)
         ENDDO
         dkef = in/id
      ENDIF
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! THE FOLLOWING CODE REPLACES DOUBLE PRECISION FUNCTION DKJ
!
      is1 = ic + Iv + 1
      IF ( is1==0 ) THEN
         dkj = aw
      ELSE
         sp1 = is1
         dkj = (R(J)**is1-R(I)**is1)/sp1
      ENDIF
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      bint = bint + c1**ic*dkj*c2**(it-1)*dkef
      c1 = c1p
      c2 = c2p
   ENDDO
   aw = Iw
   bint = bint/aw
   dkint = bint
END FUNCTION dkint