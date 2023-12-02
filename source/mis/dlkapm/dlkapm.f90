!*==dlkapm.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dlkapm(Arg,Blkapm)
   USE c_blk1
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
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
   c1 = -ai/2.0*(sps-sns)
   pi2 = 2.0*pi
   s1 = sps/(dstr**2)
   s2 = sns/dstr
   gam0 = sps*del - sigma
   c2q = gam0/dstr - scrk
   c3q = gam0/dstr + scrk
   nn = 0
   csec = c2q*c3q
   IF ( csec<0.0 ) nn = 1
   t1 = gam0*s1
   t2 = s2*sqrt(abs(csec))
   IF ( c2q<0.0 .AND. c3q<0.0 ) t2 = -t2
   IF ( nn==0 ) alp0 = t1 + t2
   IF ( nn==1 ) alp0 = cmplx(t1,t2)
   c1 = c1 + 1.0/(Arg-alp0)
   a1 = pi2/(sps-sns)
   a2 = -a1
   b1 = gam0/(sps-sns)
   c1test = 0.0
   DO i = 1 , 200
      r = i
      gamp = pi2*r + gam0
      gamn = -pi2*r + gam0
      c2p = gamp/dstr - scrk
      c2q = gamp/dstr + scrk
      c2n = gamn/dstr - scrk
      c3q = gamn/dstr + scrk
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
   WRITE (ibbout,99001) ufm
99001 FORMAT (A23,' - AMG MODULE -SUBROUTINE DLKAPM')
   CALL mesage(-61,0,0)
   RETURN
CONTAINS
   SUBROUTINE spag_block_1
      E1 = Arg - B1
      B = Pi/A1
      C1 = C1 - 1.0/E1 + B*ccos(B*E1)/(csin(B*E1))
      Blkapm = C1
   END SUBROUTINE spag_block_1
END SUBROUTINE dlkapm
