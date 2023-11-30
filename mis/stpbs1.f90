
SUBROUTINE stpbs1(X,Ncode,Bj1,By1)
   IMPLICIT NONE
   REAL Bj1 , By1 , X
   INTEGER Ncode
   REAL a , e , t , u , uw , w , z
   INTEGER name(2)
!     SUBROUTINE BES1        J AND Y BESSEL FUNCTIONS OF FIRST ORDER
!     E. ALBANO, ORGN 3721, EXT 1022, OCT 1967
!     COMPUTES J1(X) IF X IS GREATER THAN -3.
!     COMPUTES Y1(X) IF (X IS GREATER THAN E AND NCODE = 1 ),
!           WHERE
   DATA name/4HSTPB , 4HS1  /
   e = 0.00001
!                 REF. US DEPT OF COMMERCE HANBOOK (AMS 58)  PG. 370
   a = abs(X)
   IF ( a<=3. ) THEN
      z = X*X/9.
      Bj1 = X*(0.5+z*(-0.56249985+z*(0.21093573+z*(-0.03954289+z*(0.00443319+z*(-0.00031761+0.00001109*z))))))
      IF ( Ncode==1 ) THEN
         IF ( X<e ) THEN
            CALL mesage(-7,0,name)
            GOTO 100
         ELSE
            By1 = 0.63661977*Bj1*(alog(X)-.69314718)                                                                                &
                & + (-0.6366198+z*(0.2212091+z*(2.1682709+z*(-1.3164827+z*(0.3123951+z*(-0.0400976+0.0027873*z))))))/X
            RETURN
         ENDIF
      ENDIF
   ELSEIF ( X<=0 ) THEN
      CALL mesage(-7,0,name)
      GOTO 100
   ELSE
      u = 1./sqrt(X)
      z = 3./X
      w = 0.79788456 + z*(0.00000156+z*(0.01659667+z*(0.00017105+z*(-0.00249511+z*(0.00113653-0.00020033*z)))))
      t = X - 2.35619449 + z*(0.12499612+z*(0.00005650+z*(-0.00637879+z*(0.00074348+z*(0.00079824-0.00029166*z)))))
      uw = u*w
      Bj1 = uw*cos(t)
      IF ( Ncode==1 ) THEN
         By1 = uw*sin(t)
         GOTO 100
      ENDIF
   ENDIF
   RETURN
 100  RETURN
END SUBROUTINE stpbs1
