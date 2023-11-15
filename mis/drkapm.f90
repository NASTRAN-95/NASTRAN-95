
SUBROUTINE drkapm(Arg,Indx,Reslt)
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
   COMPLEX Arg , Reslt
   INTEGER Indx
!
! Local variable declarations
!
   REAL a1 , a2 , b1 , c2n , c2p , c2q , c3q , csec , gam0 , gamn , gamp , pi2 , r , rindx , s1 , s2 , t1 , t2
   COMPLEX aln , alp , alp0 , at2 , at3 , c1 , c2 , c2test
   INTEGER i , nn
!
! End of declarations
!
!
!     THIS SUBROUTINE COMPUTES THE DERVIATIVE OF KAPPA MINUS
!
!
   pi2 = 2.0*Pi
   a1 = pi2/(Sps-Sns)
   a2 = -a1
   gam0 = Sps*Del - Sigma
   b1 = gam0/(Sps-Sns)
   c1 = cexp(-Ai*Arg/2.0*(Sps-Sns))
   c2q = gam0/Dstr - Scrk
   c3q = gam0/Dstr + Scrk
   s1 = Sps/(Dstr**2)
   s2 = Sns/Dstr
   nn = 0
   csec = c2q*c3q
   IF ( csec<0.0 ) nn = 1
   t1 = gam0*s1
   t2 = s2*sqrt(abs(csec))
   IF ( c2q<0.0 .AND. c3q<0.0 ) t2 = -t2
   IF ( nn==0 ) alp0 = t1 + t2
   IF ( nn==1 ) alp0 = cmplx(t1,t2)
   rindx = Indx
   IF ( Indx==0 ) THEN
      c2 = c1*b1/alp0*csin(Pi/a1*(Arg-b1))/((b1-alp0)*sin(Pi*b1/a1))*Bsycon
   ELSE
      c2 = c1*b1/alp0*csin(Pi/a1*(Arg-b1))/(a1*rindx+b1-Arg)*(1.0+(alp0-b1)/(b1-Arg))/(sin(Pi*b1/a1))*Bsycon
   ENDIF
   c2test = 0.0
   DO i = 1 , 200
      r = i
      IF ( Indx>=0 .OR. abs(rindx)/=r ) THEN
         IF ( Indx<=0 .OR. rindx/=r ) THEN
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
            c2 = c2*(1.0+at2)*(1.0+at3)
            IF ( cabs((c2-c2test)/c2)<0.0009 ) GOTO 100
            c2test = c2
         ENDIF
      ENDIF
   ENDDO
!
   WRITE (Ibbout,99001) Ufm
99001 FORMAT (A23,' - AMG MODULE -SUBROUTINE DRKAPM')
   CALL mesage(-61,0,0)
   GOTO 99999
 100  Reslt = c2
   RETURN
99999 END SUBROUTINE drkapm
