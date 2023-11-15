
SUBROUTINE akapm(Arg,Bkpm)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   COMPLEX Ai , Bsycon
   REAL Beta , Del , Dstr , Pi , Res , Scrk , Sigma , Sns , Sps , Sysbuf
   INTEGER Ibbout
   CHARACTER*23 Ufm
   COMMON /blk1  / Scrk , Sps , Sns , Dstr , Ai , Pi , Del , Sigma , Beta , Res
   COMMON /blk2  / Bsycon
   COMMON /system/ Sysbuf , Ibbout
   COMMON /xmssg / Ufm
!
! Dummy argument declarations
!
   COMPLEX Arg , Bkpm
!
! Local variable declarations
!
   REAL a1 , a2 , b1 , c2n , c2p , c2q , c3q , csec , gam0 , gamn , gamp , pi2 , r , s1 , s2 , t1 , t2
   COMPLEX aln , alp , alp0 , at2 , at3 , c1 , c1test
   INTEGER i , nn
!
! End of declarations
!
!
!     SUBROUTINE FOR COMPUTING KAPPA MINUS
!
!
   c1 = cexp(-Ai*Arg/2.0*(Sps-Sns))
   gam0 = Sps*Del - Sigma
   pi2 = 2.0*Pi
   s1 = Sps/(Dstr**2)
   s2 = Sns/Dstr
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
   c1 = c1*(1.0-Arg/alp0)
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
      at2 = (alp-a1*r-b1)/(a1*r+b1-Arg)
      at3 = (aln-a2*r-b1)/(a2*r+b1-Arg)
      c1 = c1*(1.0+at2)*(1.0+at3)
      IF ( cabs((c1-c1test)/c1)<0.0009 ) GOTO 100
      c1test = c1
   ENDDO
!
   WRITE (Ibbout,99001) Ufm
99001 FORMAT (A23,' - AMG MODULE -SUBROUTINE AKAPM')
   CALL mesage(-61,0,0)
   GOTO 99999
 100  c1 = c1*b1/(Arg-b1)*csin(Pi/a1*(Arg-b1))/(sin(Pi*b1/a1))
   c1 = c1*Bsycon
   Bkpm = c1
   RETURN
99999 END SUBROUTINE akapm
