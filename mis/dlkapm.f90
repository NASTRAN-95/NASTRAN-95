
SUBROUTINE dlkapm(Arg,Blkapm)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   COMPLEX Ai
   REAL Beta , Del , Dstr , Pi , Res , Scrk , Sigma , Sns , Sps , Sysbuf
   INTEGER Ibbout
   CHARACTER*23 Ufm
   COMMON /blk1  / Scrk , Sps , Sns , Dstr , Ai , Pi , Del , Sigma , Beta , Res
   COMMON /system/ Sysbuf , Ibbout
   COMMON /xmssg / Ufm
!
! Dummy argument declarations
!
   COMPLEX Arg , Blkapm
!
! Local variable declarations
!
   REAL a1 , a2 , b , b1 , c2n , c2p , c2q , c3q , csec , gam0 , gamn , gamp , pi2 , r , s1 , s2 , t1 , t2
   COMPLEX aln , alp , alp0 , c1 , c1test , d1 , d2 , e1
   INTEGER i , nn
!
! End of declarations
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
      IF ( cabs((c1-c1test)/c1)<0.0006 ) GOTO 100
      c1test = c1
   ENDDO
!
   WRITE (Ibbout,99001) Ufm
99001 FORMAT (A23,' - AMG MODULE -SUBROUTINE DLKAPM')
   CALL mesage(-61,0,0)
   GOTO 99999
 100  e1 = Arg - b1
   b = Pi/a1
   c1 = c1 - 1.0/e1 + b*ccos(b*e1)/(csin(b*e1))
   Blkapm = c1
   RETURN
99999 RETURN
END SUBROUTINE dlkapm