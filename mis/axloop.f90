
SUBROUTINE axloop(Buf,Ibuf,Xx,Yy,Zz,Hc1,Hc2,Hc3)
   IMPLICIT NONE
   REAL Epse , Sysbuf
   INTEGER Idum(3) , Otpe
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / Idum , Epse
   COMMON /system/ Sysbuf , Otpe
   COMMON /xmssg / Ufm , Uwm
   REAL Hc1 , Hc2 , Hc3 , Xx , Yy , Zz
   REAL Buf(50)
   INTEGER Ibuf(50)
   REAL anx , any , anz , at1 , at2 , at3 , br , bthe , bx , by , bz , c , costhe , cx , cy , cz , dele , delf , delte1 , delte2 ,  &
      & deltf1 , deltf2 , e , f , fpi , hcx , hcy , hcz , pi , piby2 , r , r2 , rad2 , radius , realk , realk2 , rpx , rpy , rpz ,  &
      & rx , ry , rz , sinthe , sqar2s , trpx , trpy , trpz , tx , ty , tz , x1 , x2 , xc , xiacpi , xiacr , xj , xn2 , xn21 , y1 , &
      & y2 , yc , z1 , z2 , zc
   INTEGER iaxi , icid , n
!
!
   pi = 3.1415926536
   piby2 = 1.5707963268
   fpi = 12.56637062
   c = 1.
!
   xj = Buf(1)
   iaxi = Ibuf(2)
   x1 = Buf(3)
   y1 = Buf(4)
   z1 = Buf(5)
   x2 = Buf(6)
   y2 = Buf(7)
   z2 = Buf(8)
   xc = Buf(9)
   yc = Buf(10)
   zc = Buf(11)
!
!     FOR NOW, ICID = 0
!
   icid = Ibuf(12)
!
!     CHECK FOR AXISYMMETRIC PROBLEM
!
   IF ( iaxi==1 ) THEN
      xc = 0.
      yc = 0.
      zc = z1
      x2 = 0.
      y2 = x1
      z2 = z1
   ENDIF
!
!     DETERMINE THE DIRECTION OF THE CURRENT LOOP AXIS
!
   cx = x1 - xc
   cy = y1 - yc
   cz = z1 - zc
   bx = x2 - xc
   by = y2 - yc
   bz = z2 - zc
!
!     THE VECTOR AN IS NORMAL TO THE PLANE OF THE LOOP
!
   anx = cy*bz - cz*by
   any = cz*bx - cx*bz
   anz = cx*by - cy*bx
   at1 = sqrt(anx*anx+any*any+anz*anz)
   at2 = bx*bx + by*by + bz*bz
   rad2 = cx*cx + cy*cy + cz*cz
   radius = sqrt(rad2)
   xiacpi = (xj*rad2*pi)/c
!
   anx = anx/at1
   any = any/at1
   anz = anz/at1
!
!     THE VECTOR R IS FROM THE CENTER OF LOOP TO THE FIELD POINT
!
   rx = Xx - xc
   ry = Yy - yc
   rz = Zz - zc
!
   r2 = rx*rx + ry*ry + rz*rz
   r = sqrt(r2)
!
!     AT (OR NEAR) CENTER OF LOOP TEST
!
   IF ( r>=.001 ) THEN
!
      rx = rx/r
      ry = ry/r
      rz = rz/r
      costhe = anx*rx + any*ry + anz*rz
      sinthe = sqrt(1.-costhe*costhe)
!
!     ON (OR VERY NEAR) AXIS OF LOOP TEST
!
      IF ( sinthe>=.000001 ) THEN
!
         sqar2s = sqrt(rad2+r2+(2.*radius*r*sinthe))
         realk2 = (4.*radius*r*sinthe)/(rad2+r2+(2.*radius*r*sinthe))
         realk = sqrt(realk2)
         xiacr = (xj*radius)/(c*r)
!
!     A CROSS R, NORMAL TO THE PLANE OF A AND R
!
         tx = any*rz - anz*ry
         ty = anz*rx - anx*rz
         tz = anx*ry - any*rx
!
!     (A CROSS R) CROSS R, NORMAL TO THE PLANE OF R AND (A AND R)
!
         trpx = ty*rz - tz*ry
         trpy = tz*rx - tx*rz
         trpz = tx*ry - ty*rx
         at3 = sqrt(trpx*trpx+trpy*trpy+trpz*trpz)
!
!     RPERP, PERPENDICULAR TO THE VECTOR FROM THE CENTER TO THE FIELD PT
!
         rpx = trpx/at3
         rpy = trpy/at3
         rpz = trpz/at3
!
!     FOR SMALL POLAR ANGLE OR SMALL RADIUS USE ALTERNATIVE APPROX.
!
         IF ( realk2>=.0001 ) THEN
!
!     COMPUTE ELLIPTIC INTEGRAL OF FIRST KIND
!
            f = 1.
            deltf1 = 1.
            DO n = 1 , 15000
               xn2 = 2.*float(n)
               xn21 = xn2 - 1.
               deltf1 = deltf1*(xn21/xn2)*realk
               deltf2 = deltf1*deltf1
               f = f + deltf2
               IF ( abs(deltf2/f)<=Epse ) GOTO 10
            ENDDO
            delf = abs(deltf2/f)
            WRITE (Otpe,99001) Uwm , Xx , Yy , Zz , xc , yc , zc , x1 , y1 , z1 , x2 , y2 , z2 , delf , Epse
 10         f = piby2*f
!
!     COMPUTE ELLIPTIC INTEGRAL OF SECOND KIND
!
            e = 1.
            delte1 = 1.
            DO n = 1 , 15000
               xn2 = 2.*float(n)
               xn21 = xn2 - 1.
               delte1 = delte1*(xn21/xn2)*realk
               delte2 = (delte1*delte1)/xn21
               e = e - delte2
               IF ( abs(delte2/e)<=.000001 ) GOTO 20
            ENDDO
            dele = abs(delte2/e)
            WRITE (Otpe,99001) Uwm , Xx , Yy , Zz , xc , yc , zc , x1 , y1 , z1 , x2 , y2 , z2 , dele
 20         e = piby2*e
!
!     COMPUTE THE RADIAL COMPONENT OF THE MAGNETIC FIELD
!
            br = xiacr*(costhe/sinthe)*(e/sqar2s)*(realk2/(1.-realk2))
!
!     COMPUTE THE POLAR COMPONENT OF THE MAGNETIC FIELD
!
            bthe = xiacr*(1./(sqar2s*radius*r*sinthe))*(((((2.*r2)-((r2+(radius*r*sinthe))*realk2))/(1.-realk2))*e)-(2.*r2*f))
!
!     GO TO THE RESOLUTION OF FIELD COMPONENTS
!
            GOTO 100
         ENDIF
      ELSE
         costhe = 1.
         sinthe = 0.
         sqar2s = sqrt(rad2+r2)
         rx = anx
         ry = any
         rz = anz
         rpx = 0.
         rpy = 0.
         rpz = 0.
      ENDIF
   ELSE
      costhe = 1.
      sinthe = 0.
      sqar2s = sqrt(rad2+r2)
      rx = anx
      ry = any
      rz = anz
      rpx = 0.
      rpy = 0.
      rpz = 0.
   ENDIF
!
!     ALTERNATIVE APPROXIMATION FOR SMALL K**2
!
!     COMPUTE THE RADIAL COMPONENT OF THE MAGNETIC FIELD
!
   br = xiacpi*costhe*(((2.*rad2)+(2.*r2)+(radius*r*sinthe))/((sqar2s)**5))
!
!     COMPUTE THE POLAR COMPONENT OF THE MAGNETIC FIELD
!
   bthe = -xiacpi*sinthe*(((2.*rad2)-r2+(radius*r*sinthe))/((sqar2s)**5))
!
!     RESOLVE MAGNETIC FIELD COMPONENTS INTO RECTANGULAR COMPONENTS
!
 100  hcx = rx*br + rpx*bthe
   hcy = ry*br + rpy*bthe
   hcz = rz*br + rpz*bthe
   Hc1 = hcx/fpi
   Hc2 = hcy/fpi
   Hc3 = hcz/fpi
99001 FORMAT (A25,', CONVERGENCE OF ELLIPTIC INTEGRAL IS UNCERTAIN. ','GRID OR INTEGRATION POINT AT COORDINATES',/5X,1P,3E15.6,     &
             &'  IS TOO CLOSE TO CURRENT LOOP WITH CENTER AT',/5X,1P,3E15.6,' AND 2 POINTS AT ',1P,3E15.6,/5X,4HAND ,1P,3E15.6,     &
             &' COMPUTATIONS WILL CONTINUE WITH LAST VALUES',/5X,'CONVERGENCE VALUE WAS ',1P,E15.6,' CONVERGENCE CRITERION IS ',1P, &
            & E15.6)
END SUBROUTINE axloop
