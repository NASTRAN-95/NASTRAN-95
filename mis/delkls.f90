
SUBROUTINE delkls(Del,R,Z,Kode)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER Kode
   REAL Del(15) , R(4) , Z(4)
!
! Local variable declarations
!
   REAL a , a2 , a3 , aab , ab , abb , b , b2 , b3 , ln , r1 , r2 , r3 , r4 , r5 , r6 , r7 , ri , rm , si , sm , zi , zm
   INTEGER goback , i , l , m , n
!
! End of declarations
!
!
!     SINGLE PRECISION VERSION USE DELKLS (DEL,R,Z,KODE)
!
!     PURPOSE
!        EVAULATE THE FOLLOWING FUNCTION
!          DELT(K,L) = SURFACE-INTEGRAL((R**K)*(Z**L)) DR*DZ
!        WHERE  DR*DZ  IS EITHER A TRIANGLE OR A TRAPEZOID.
!
!     USAGE
!        WHERE  DEL   =  DOUBLE PRECISION ARRAY OF 15 LOCATIONS.
!                        CONTAINING THE RESULTS.
!        WHERE  R     =  DOUBLE PRECISION ARRAY OF  4 LOCATIONS.
!                        CONTAINING THE R-COORDINATES OF THE ELEM.
!        WHERE  Z     =  DOUBLE PRECISION ARRAY OF  4 LOCATIONS.
!                        CONTAINING THE Z-COORDINATES OF THE ELEM.
!               KODE  =  0  FOR TRIANGULAR  ELEMENT
!               KODE  =  1  FOR TRAPEZOIDAL ELEMENT
!
!     PROCEDURE
!        INFORMATION IS COMPUTED AND STORED AS FOLLOWS.
!           COMPUTED  FOR        ELEMENT         STORED
!        TRIANGLE   TRAPEZOID   DELT(K,L)       DEL(LOC)
!        ************************************************
!           X          X              0,0            01
!           X          X              1,0            02
!           X          X              0,1            03
!           X          X             -1,0            04
!           X          X             -1,1            05
!           X          X             -1,2            06
!                      X              1,1            07
!                      X              1,2            08
!                      X              2,1            09
!                      X              2,0            10
!                      X              0,2            11
!                      X              3,0            12
!                      X              3,1            13
!                      X              3,2            14
!                      X              2,2            15
!
!
!     ZERO ARRAY (ONLY THAT PORTION USING)
!
   n = 15
   DO l = 1 , n
      Del(l) = 0.
   ENDDO
!
!     HERE FOR  LINE 1-2
!
   i = 1
   m = 2
   ASSIGN 100 TO goback
   GOTO 400
!
!     HERE  FOR  LINE 2-3
!
 100  i = 2
   m = 3
   ASSIGN 200 TO goback
   GOTO 400
!
!     HERE  FOR  LINE 31 (TRIANGLE),  LINE 3-4 (TRAP)
!
 200  i = 3
   IF ( Kode>0 ) THEN
      m = 4
      ASSIGN 300 TO goback
   ELSE
      m = 1
      ASSIGN 500 TO goback
   ENDIF
   GOTO 400
 300  i = 4
   m = 1
   ASSIGN 500 TO goback
!
!     BEGIN LOCAL SUBROUTINE  (DEL-KL-I,M)
!
 400  rm = R(m)
   ri = R(i)
   r1 = rm - ri
   IF ( abs(r1)>=1.E-5 ) THEN
!
!     THIS LINE IS NOT PARALLEL TO Z-AXIS
!
      zm = Z(m)
      zi = Z(i)
      IF ( zi/=0. .OR. zm/=0. ) THEN
!
!     SPECIAL CASE   ZM=ZI=0   THUS ALL  A,B = 0  AND
!     ALL DEL TERMS  = 0 .     THUS SKIP AND SAVE CPU.
!
         a = (rm*zi-ri*zm)/r1
         b = (zm-zi)/r1
         ln = alog(rm/ri)
         si = ri*ri
         sm = rm*rm
         r2 = sm - si
         si = si*ri
         sm = sm*rm
         r3 = sm - si
         si = si*ri
         sm = sm*rm
         r4 = sm - si
         si = si*ri
         sm = sm*rm
         r5 = sm - si
         a2 = a*a
         a3 = a*a2
         b2 = b*b
         b3 = b*b2
         ab = a*b
         aab = a*ab
         abb = b*ab
         Del(1) = a*r1 + b*r2/2. + Del(1)
         Del(2) = a*r2/2. + b*r3/3. + Del(2)
         Del(3) = a2*r1/2. + ab*r2/2. + b2*r3/6. + Del(3)
         Del(4) = a*ln + b*r1 + Del(04)
         Del(5) = a2*ln/2. + ab*r1 + b2*r2/4. + Del(5)
         Del(6) = a3*ln/3. + aab*r1 + abb*r2/2. + b3*r3/9. + Del(6)
         Del(7) = a2*r2/4. + ab*r3/3. + b2*r4/8. + Del(7)
         Del(8) = a3*r2/6. + aab*r3/3. + abb*r4/4. + b3*r5/15. + Del(8)
         Del(9) = a2*r3/6. + ab*r4/4. + b2*r5/10. + Del(9)
         Del(10) = a*r3/3. + b*r4/4. + Del(10)
         Del(12) = a*r4/4. + b*r5/5. + Del(12)
         IF ( Kode>=1 ) THEN
            si = si*ri
            sm = sm*rm
            r6 = sm - si
            r7 = (sm*rm-si*ri)
            Del(11) = a3*r1/3. + aab*r2/2. + abb*r3/3. + b3*r4/12. + Del(11)
            Del(13) = a2*r4/8. + ab*r5/5. + b2*r6/12. + Del(13)
            Del(14) = a3*r4/12. + aab*r5/5. + abb*r6/6. + b3*r7/21. + Del(14)
            Del(15) = a3*r3/9. + aab*r4/4. + abb*r5/5. + b3*r6/18. + Del(15)
         ENDIF
      ENDIF
   ENDIF
   GOTO goback
!
!     THE ABSOLUTE VALUE IS CHOSEN SO THAT NODES INPUT MAY BE ORDERED
!     CW OR CCW. RESULTS ARE SAME FOR A GIVEN ELEMENT.
!
 500  DO l = 1 , n
      Del(l) = abs(Del(l))
   ENDDO
END SUBROUTINE delkls
