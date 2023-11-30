
SUBROUTINE trif(Xc,Yc,Zc,Ivect,Jvect,Kvect,A,B,C,Id,Elem)
   IMPLICIT NONE
   INTEGER Ibuf , Nout
   LOGICAL Nogo
   CHARACTER*23 Ufm
   COMMON /system/ Ibuf , Nout , Nogo
   COMMON /xmssg / Ufm
   REAL A , B , C
   INTEGER Id
   REAL Elem(2) , Ivect(3) , Jvect(3) , Kvect(3) , Xc(6) , Yc(6) , Zc(6)
   REAL save , temp , x1 , x2 , y1 , y2 , z1 , z2
!
!     CALCULATEIONS FOR THE TRIANGLE USED IN TRIM6,TRPLT1,TRSHL - THE HI
!     LEVEL PLATE ELEMENTS.  COMPUTATIONS IN SINGLE PRECISION ONLY
!
!     IVECT, JVECT, AND KVECT ARE UNIT VECTORS OF THE TRIANGLE
!     B IS THE DISTANCE OF THE GRID POINT 1
!     A IS THE DISTANCE OF THE GRID POINT 3
!     C IS THE DISTANCE OF THE GRID POINT 5
!
!
!     EVALUATE DIRECTIONAL COSINES
!
   x1 = Xc(3) - Xc(1)
   y1 = Yc(3) - Yc(1)
   z1 = Zc(3) - Zc(1)
   x2 = Xc(5) - Xc(1)
   y2 = Yc(5) - Yc(1)
   z2 = Zc(5) - Zc(1)
   temp = x1*x1 + y1*y1 + z1*z1
   IF ( temp<=1.0E-10 ) THEN
!
!     GEOMETRY ERRORS
!
      WRITE (Nout,99001) Ufm , Elem , Id
!
99001 FORMAT (A23,' 2404, GRID POINTS 1 AND 3 OF ',A4,A2,' WITH ELEMENT ID =',I9,' HAVE SAME COORDINATES.')
   ELSE
      temp = sqrt(temp)
!
!     I-VECTOR
!
      Ivect(1) = x1/temp
      Ivect(2) = y1/temp
      Ivect(3) = z1/temp
      save = temp
!
!     NON-NORMALIZED K-VECTOR
!
      Kvect(1) = Ivect(2)*z2 - y2*Ivect(3)
      Kvect(2) = Ivect(3)*x2 - z2*Ivect(1)
      Kvect(3) = Ivect(1)*y2 - x2*Ivect(2)
      temp = sqrt(Kvect(1)**2+Kvect(2)**2+Kvect(3)**2)
      IF ( temp<=1.0E-10 ) THEN
         WRITE (Nout,99002) Ufm , Elem , Id
99002    FORMAT (A23,' 2405, GRID POINTS 1, 3, AND 5 OF ',A4,A2,' WITH ','ELEMENT ID =',I9,' APPEAR TO BE ON A STRAIGHT LINE.')
      ELSE
!
!     NORMALIZE K-VECTOR
!     DISTANCE C OF THE TRAINGLE IS TEMP
!
         Kvect(1) = Kvect(1)/temp
         Kvect(2) = Kvect(2)/temp
         Kvect(3) = Kvect(3)/temp
         C = temp
!
!     J-VECTOR = K X I VECTORS
!
         Jvect(1) = Kvect(2)*Ivect(3) - Ivect(2)*Kvect(3)
         Jvect(2) = Kvect(3)*Ivect(1) - Ivect(3)*Kvect(1)
         Jvect(3) = Kvect(1)*Ivect(2) - Ivect(1)*Kvect(2)
         temp = sqrt(Jvect(1)**2+Jvect(2)**2+Jvect(3)**2)
         IF ( temp<=1.0E-10 ) THEN
            WRITE (Nout,99003) Ufm , Elem , Id
99003       FORMAT (A23,' 2406, GRID POINTS 1 AND 5 OF ',A4,A2,' WITH ELEMENT ID =',I9,' HAVE SAME COORDINATES.')
         ELSE
!
!     NORMALIZE J-VECTOR TO MAKE SURE
!
            Jvect(1) = Jvect(1)/temp
            Jvect(2) = Jvect(2)/temp
            Jvect(3) = Jvect(3)/temp
!
!     DISTANCE B OF THE TRIANGLE IS OBTAINED BY DOTTING (X2,Y2,Z2) WITH
!     THE IVECT UNIT VECTOR
!
            B = x2*Ivect(1) + y2*Ivect(2) + z2*Ivect(3)
!
!     THE LOCAL X AND Y COORINATES OF THE SIX GRID PTS. ARE AS FOLLOWS
!
            Yc(1) = 0.0
            Yc(2) = 0.0
            Yc(3) = 0.0
            Yc(4) = C*0.5
            Yc(5) = C
            Yc(6) = Yc(4)
!
!     THE TRIANGLE SHOULD BELONG TO
!
!     KASE1 (ACUTE ANGLES AT GRID POINTS 1 AND 3),
!     KASE2 (OBTUSE ANGLE AT GRID POINT 3), OR
!     KASE3 (OBTUSE ANGLE AT GRID POINT 1)
!
!     KASE  = 1
!     IF (B .GT. SAVE) KASE = 2
!     IF (B .LT.  0.0) KASE = 3
            temp = -B
!     IF (KASE .EQ. 3) TEMP = ABS(B)
!     IF (B .LT.  0.0) TEMP = ABS(B)
            Xc(1) = temp
            Xc(2) = temp + save*0.5
            Xc(3) = temp + save
            Xc(4) = Xc(3)*0.5
            Xc(5) = 0.0
            Xc(6) = Xc(1)*0.5
!
!     RE-SET DISTANCE A AND B
!
            B = abs(B)
            A = abs(Xc(3))
            RETURN
         ENDIF
      ENDIF
   ENDIF
   Nogo = .TRUE.
END SUBROUTINE trif