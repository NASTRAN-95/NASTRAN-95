!*==dlkapm.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dlkapm(Arg,Blkapm)
   IMPLICIT NONE
   USE C_BLK1
   USE C_SYSTEM
   USE C_XMSSG
!
! Dummy argument declarations rewritten by SPAG
!
   COMPLEX :: Arg
   COMPLEX :: Blkapm
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a1 , a2 , b , b1 , c2n , c2p , c2q , c3q , csec , gam0 , gamn , gamp , pi2 , r , s1 , s2 , t1 , t2
   COMPLEX :: aln , alp , alp0 , c1 , c1test , d1 , d2 , e1
   INTEGER :: i , nn
   EXTERNAL mesage
!
! End of declarations rewritten by SPAG
!
!
!     SUBROUTINE FOR COMPUTING LOGARITHMIC DERIVATIVE OF KAPPA MINUS
!
!
   c1 = -Ai/2.0*(Sps-Sns)
   pi2 = 2.0*Pi
   s1 = Sps/(Dstr**2)
   s2 = Sns/Dstr
   gam0 = Sps*Del - Sigma
   c2q = gam0/Dstr - Scrk
   c3q = gam0/Dstr + Scrk
   nn = 0
   csec = c2q*c3q
   IF ( csec<0.0 ) nn = 1
   t1 = gam0*s1
   t2 = s2*sqrt(abs(csec))
   IF ( c2q<0.0 .AND. c3q<0.0 ) t2 = -t2
   IF ( nn==0 ) alp0 = t1 + t2
   IF ( nn==1 ) alp0 = cmplx(t1,t2)
   c1 = c1 + 1.0/(Arg-alp0)
   a1 = pi2/(Sps-Sns)
   a2 = -a1
   b1 = gam0/(Sps-Sns)
   c1test = 0.0
   DO i = 1 , 200
      r = i
      gamp = pi2*r + gam0
      gamn = -pi2*r + gam0
      c2p = gamp/Dstr - Scrk
      c2q = gamp/Dstr + Scrk
      c2n = gamn/Dstr - Scrk
      c3q = gamn/Dstr + Scrk
      nn = 0
      csec = c2p*c2q
      IF ( csec<0.0 ) nn = 1
      t1 = gamp*s1
      t2 = s2*sqrt(abs(csec))
      IF ( c2p<0.0 .AND. c2q<0.0 ) t2 = -t2
      IF ( nn==0 ) alp = t1 + t2
      IF ( nn==1 ) alp = cmplx(t1,t2)
      nn = 0
      csec = c2n*c3q
      IF ( csec<0.0 ) nn = 1
      t1 = gamn*s1
      t2 = s2*sqrt(abs(csec))
      IF ( c2n<0.0 .AND. c3q<0.0 ) t2 = -t2
      IF ( nn==0 ) aln = t1 + t2
      IF ( nn==1 ) aln = cmplx(t1,t2)
      e1 = a1*r + b1 - Arg
      d1 = (alp-a1*r-b1)/e1
      d2 = d1/e1
      c1 = c1 + 1.0/(1.0+d1)*d2
      e1 = a2*r + b1 - Arg
      d1 = (aln-a2*r-b1)/e1
      d2 = d1/e1
      c1 = c1 + 1.0/(1.0+d1)*d2
      IF ( cabs((c1-c1test)/c1)<0.0006 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
      c1test = c1
   ENDDO
!
   WRITE (Ibbout,99001) Ufm
99001 FORMAT (A23,' - AMG MODULE -SUBROUTINE DLKAPM')
   CALL mesage(-61,0,0)
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
      e1 = Arg - b1
      b = Pi/a1
      c1 = c1 - 1.0/e1 + b*ccos(b*e1)/(csin(b*e1))
      Blkapm = c1
      RETURN
   END SUBROUTINE spag_block_1
END SUBROUTINE dlkapm
