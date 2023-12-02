!*==pload1.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pload1(Opt,Islt,V,Sa,Sb,Ba,Bb,Pa,Pb,Ta,Tb,Slt,Ept)
USE C_MATOUT
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Opt
   INTEGER , DIMENSION(7) :: Islt
   REAL , DIMENSION(3) :: V
   REAL , DIMENSION(3) :: Sa
   REAL , DIMENSION(3) :: Sb
   REAL , DIMENSION(3) :: Ba
   REAL , DIMENSION(3) :: Bb
   REAL , DIMENSION(6) :: Pa
   REAL , DIMENSION(6) :: Pb
   REAL , DIMENSION(9) :: Ta
   REAL , DIMENSION(9) :: Tb
   REAL , DIMENSION(7) :: Slt
   REAL , DIMENSION(32) :: Ept
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(3) :: a , b , c , tp
   REAL :: aly , alz , f1 , f2 , fscale , fx1 , fx2 , fy1 , fy2 , fz1 , fz2 , len , omaly , omalz , temp , tmp , x1 , x2
   REAL(REAL64) :: ax , ay , az , bx , by , bz , dfx1 , dfx2 , dfy1 , dfy2 , dfz1 , dfz2 , dl , dt , dx1 , dx2 , i01 , i02 , i11 ,  &
                 & i12 , i21 , i22 , i31 , i32 , i41 , i42 , s1 , s2 , s3 , s4 , s5
   REAL , DIMENSION(9) , SAVE :: d
   REAL , DIMENSION(9) :: e
   INTEGER :: i , j , scale , type
   INTEGER , SAVE :: oldid
   EXTERNAL gmmats , mesage , ploapf , sadotb , saxb
!
! End of declarations rewritten by SPAG
!
!
!     PLOAD1 CALCULATES THE END LOADS ON A BAR ELEMENT FOR PLOAD1 LOADS
!     IT IS CALLED ONLY BY PLBAR1
!
!     OPT   = 1, CALLED FROM PLBAR1/EXTERN,  2, CALLED FROM SDRX
!     SLT   = PLOAD1 CARD
!     V     = REFERENCE VECTOR IN BASIC
!     SA    = OFFSET VECTOR IN BASIC POINT A
!     SB    = OFFSET VECTOR IN BASIC POINT B
!     BA    = BASIC  COORD  FOR POINT A
!     BB    = BASIC  COORD  FOR POINT B
!     PA    = LOAD   VECOTR FOR POINT A
!     PB    = LOAD   VECOTR FOR POINT B
!     TA,TB = TRANSFORMATION MATRICES FOR A AND B ONLY USED WITH OPT 1
!     EPT   = POINTER TO EST
!
   !>>>>EQUIVALENCE (a(1),e(1)) , (b(1),e(7)) , (c(1),e(4))
   DATA oldid , d/10*0/
!
   IF ( oldid/=Islt(1) ) THEN
      oldid = Islt(1)
!
!     CALCULATE AXIS AND LENGTH, AND THE E MATRIX
!
      a(1) = Bb(1) - Ba(1) + Sb(1) - Sa(1)
      a(2) = Bb(2) - Ba(2) + Sb(2) - Sa(2)
      a(3) = Bb(3) - Ba(3) + Sb(3) - Sa(3)
      len = sqrt(sadotb(a,a))
      IF ( len==0.0 ) THEN
!
!     ERROR
!
         CALL mesage(-30,31,oldid)
         RETURN
      ELSE
         a(1) = a(1)/len
         a(2) = a(2)/len
         a(3) = a(3)/len
         CALL saxb(a,V,b)
!
         temp = sqrt(sadotb(b,b))
         IF ( temp==0.0 ) THEN
            CALL mesage(-30,31,oldid)
            RETURN
         ELSE
            b(1) = b(1)/temp
            b(2) = b(2)/temp
            b(3) = b(3)/temp
            CALL saxb(b,a,c)
!
!     TRANSVERSE SHEAR
!
            temp = Ept(31)*Ept(17)*G*len**2
            tmp = 12.0*F*Ept(18)
            aly = 0.0
            IF ( abs(temp+tmp)>1.0E-14 ) aly = tmp/(tmp+temp)
            omaly = 1.0 - aly
!
            temp = (temp/Ept(31))*Ept(32)
            tmp = 12.0*F*Ept(19)
            alz = 0.0
            IF ( abs(temp+tmp)>1.0E-14 ) alz = tmp/(tmp+temp)
            omalz = 1.0 - alz
         ENDIF
      ENDIF
   ENDIF
!
!     START BUILDING THE FORCES AND MOMENTS
!
   type = Islt(2)
   scale = Islt(3)
   x1 = Slt(4)
   f1 = Slt(5)
   x2 = Slt(6)
   f2 = Slt(7)
   i = (type-1)/6 + 1
   j = mod(type,6)
   IF ( j/=0 ) THEN
      IF ( j==4 ) THEN
         fx1 = f1
         fy1 = 0.0
         fz1 = 0.0
         fx2 = f2
         fy2 = 0.0
         fz2 = 0.0
      ELSEIF ( j==5 ) THEN
         fx1 = 0.0
         fy1 = f1
         fz1 = 0.0
         fx2 = 0.0
         fy2 = f2
         fz2 = 0.0
         CALL spag_block_2
         RETURN
      ELSEIF ( j==6 ) THEN
         CALL spag_block_1
         RETURN
      ELSE
         fx1 = a(j)*f1
         fy1 = c(j)*f1
         fz1 = b(j)*f1
         fx2 = a(j)*f2
         fy2 = c(j)*f2
         fz2 = b(j)*f2
      ENDIF
      CALL spag_block_3
      RETURN
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      fx1 = 0.0
      fy1 = 0.0
      fz1 = f1
      fx2 = 0.0
      fy2 = 0.0
      fz2 = f2
      CALL spag_block_2
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
      j = 4
      CALL spag_block_3
   END SUBROUTINE spag_block_2
   SUBROUTINE spag_block_3
!
!     SCALED
!
      IF ( scale/=2 .AND. scale/=4 ) THEN
         x1 = x1/len
         x2 = x2/len
      ENDIF
!
!     DISTRIBUTED SCALED LOADS
!
      IF ( x1==x2 ) THEN
!
!     CONCENTRATED LOADS
!
         tmp = 1.0 - x1
         IF ( i==2 ) THEN
!
!     MOMENTS
!
            temp = -(6.0/len*x1)*tmp
            Pa(1) = 0.0
            Pa(2) = temp*fz1*omaly
            Pa(3) = -temp*fy1*omalz
            Pa(4) = tmp*fx1
            temp = 1.0 - 4.0*x1 + 3.0*x1**2
            Pa(5) = temp*fy1*omalz + fy1*tmp*alz
            Pa(6) = temp*fz1*omaly + fz1*tmp*aly
            Pb(1) = 0.0
            Pb(2) = -Pa(2)
            Pb(3) = -Pa(3)
            Pb(4) = x1*fx1
            temp = 3.0*x1**2 - 2.0*x1
            Pb(5) = temp*fy1*omalz + fy1*x1*alz
            Pb(6) = temp*fz1*omaly + fz1*x1*aly
         ELSE
!
!     FORCES
!
            temp = 1.0 - 3.0*x1**2 + 2.0*x1**3
            Pa(1) = tmp*fx1
            Pa(2) = temp*fy1*omaly + fy1*tmp*aly
            Pa(3) = temp*fz1*omalz + fz1*tmp*alz
            Pa(4) = 0.0
            temp = -len*x1*tmp**2
            tmp = tmp*len*x1*.50
            Pa(5) = temp*fz1*omalz - fz1*tmp*alz
            Pa(6) = -temp*fy1*omaly + fy1*tmp*aly
            temp = 3.0*x1**2 - 2.0*x1**3
            Pb(1) = x1*fx1
            Pb(2) = temp*fy1*omaly + fy1*x1*aly
            Pb(3) = temp*fz1*omalz + fz1*x1*alz
            Pb(4) = 0.0
            temp = (1.0-x1)*len*x1**2
            Pb(5) = temp*fz1*omalz + fz1*tmp*alz
            Pb(6) = -temp*fy1*omaly - fy1*tmp*aly
         ENDIF
         CALL spag_block_5
         RETURN
      ELSE
         IF ( scale>2 .AND. j/=4 ) THEN
            fscale = sqrt(1.0-a(j)**2)
            fx1 = fscale*fx1
            fy1 = fscale*fy1
            fz1 = fscale*fz1
            fx2 = fscale*fx2
            fy2 = fscale*fy2
            fz2 = fscale*fz2
         ENDIF
!
!     DISTRIBUTED LOADS
!
         dx1 = x1
         dx2 = x2
         dl = len
         dfx1 = fx1
         dfy1 = fy1
         dfz1 = fz1
         dfx2 = fx2
         dfy2 = fy2
         dfz2 = fz2
         s1 = dx2 - dx1
         s2 = .5000000D0*(dx2**2-dx1**2)
         s3 = .3333333D0*(dx2**3-dx1**3)
         s4 = .2500000D0*(dx2**4-dx1**4)
         s5 = .2000000D0*(dx2**5-dx1**5)
         IF ( i==2 ) THEN
!
!     MOMENTS
!
            i01 = dl*(s1-s2)
            i11 = -6.0D0*(s2-s3)
            i21 = -i11
            i31 = s1 - 4.0D0*s2 + 3.0D0*s3
            i41 = -2.0D0*s2 + 3.0D0*s3
            IF ( f1==f2 ) THEN
               ax = dfx1
               ay = dfz1
               az = -dfy1
            ELSE
               i02 = (s2-s3)*dl**2
               i12 = -6.0D0*dl*(s3-s4)
               i22 = -i12
               i32 = dl*(s2-4.0D0*s3+3.0D0*s4)
               i42 = -dl*(2.0D0*s3-3.0D0*s4)
               dt = (dx2-dx1)*dl
               bx = (dfx2-dfx1)/dt
               by = (dfz2-dfz1)/dt
               bz = -(dfy2-dfy1)/dt
               ax = dfx1 + dx1*bx*dl
               ay = dfz1 + dx1*by*dl
               az = -dfy1 + dx1*bz*dl
               CALL spag_block_4
               RETURN
            ENDIF
         ELSE
!
!     FORCES
!
            i01 = dl*(s1-s2)
            i11 = dl*(s1-3.0D0*s3+2.0D0*s4)
            i21 = dl*(3.0D0*s3-2.0D0*s4)
            i31 = dl*(s2-2.0D0*s3+s4)
            i41 = dl*(s4-s3)
            dt = dl*dl
            i02 = dt*(s2-s3)
            IF ( f1==f2 ) THEN
               ax = dfx1
               ay = dfy1
               az = dfz1
            ELSE
               i12 = dt*(s2-3.0D0*s4+2.0D0*s5)
               i22 = dt*(3.0D0*s4-2.0D0*s5)
               i32 = dt*(s3-2.0D0*s4+s5)
               i42 = dt*(s5-s4)
               dt = dl*(dx2-dx1)
               bx = (dfx2-dfx1)/dt
               by = (dfy2-dfy1)/dt
               bz = (dfz2-dfz1)/dt
               ax = dfx1 - dx1*bx*dl
               ay = dfy1 - dx1*by*dl
               az = dfz1 - dx1*bz*dl
               CALL spag_block_4
               RETURN
            ENDIF
         ENDIF
         bx = 0.0D0
         by = 0.0D0
         bz = 0.0D0
         i12 = 0.0D0
         i22 = 0.0D0
         i32 = 0.0D0
         i42 = 0.0D0
      ENDIF
      CALL spag_block_4
   END SUBROUTINE spag_block_3
   SUBROUTINE spag_block_4
!
!     LOADS
!
      Pa(1) = i01*ax + i02*bx
      Pa(2) = i11*ay + i12*by
      Pa(3) = i11*az + i22*bz
      Pa(4) = 0.0
      Pa(5) = -dl*(i31*az+i32*bz)
      Pa(6) = dl*(i31*ay+i32*by)
      dt = dl*dl
      Pb(1) = dl*s2*ax + dt*s3*bx
      Pb(2) = i21*ay + i22*by
      Pb(3) = i21*az + i22*bz
      Pb(4) = 0.0
      Pb(5) = -dl*(i41*az+i42*bz)
      Pb(6) = dl*(i41*ay+i42*by)
      IF ( i==2 ) THEN
         temp = Pa(1)
         Pa(1) = Pa(4)
         Pa(4) = temp
         temp = Pb(1)
         Pb(1) = Pb(4)
         Pb(4) = temp
         IF ( aly/=0.0 ) THEN
            Pa(2) = omaly*Pa(2)
            Pa(6) = omaly*Pa(6) + aly*(i01*ay+i02*by)
            Pb(2) = omaly*Pb(2)
            Pb(6) = omaly*Pb(6) + aly*(dl*s2*ay+s3*by*dt)
         ENDIF
         IF ( alz/=0.0 ) THEN
            Pa(3) = omalz*Pa(3)
            Pa(5) = omalz*Pa(5) + alz*(i01*az+i02*bz)
            Pb(3) = omalz*Pb(3)
            Pb(5) = omalz*Pb(5) + alz*(dl*s2*az+s3*bz*dt)
         ENDIF
      ELSE
         IF ( aly/=0.0 ) THEN
            Pa(2) = omaly*Pa(2) + aly*(i01*ay+i02*by)
            Pa(6) = omaly*Pa(6) + aly*(i02*ay-i41*by*dt)*.50
            Pb(2) = omaly*Pb(2) + aly*(dl*s2*ay+s3*by*dt)
            Pb(6) = omaly*Pb(6) - aly*(i02*ay-i41*by*dt)*.50
         ENDIF
         IF ( alz/=0.0 ) THEN
            Pa(3) = omalz*Pa(3) + alz*(i01*az+i02*bz)
            Pa(5) = omalz*Pa(5) - alz*(i02*az-i41*bz*dt)*.50
            Pb(3) = omalz*Pb(3) + alz*(dl*s2*az+s3*bz*dt)
            Pb(5) = omalz*Pb(5) + alz*(i02*az-i41*bz*dt)*.50
         ENDIF
      ENDIF
      CALL spag_block_5
   END SUBROUTINE spag_block_4
   SUBROUTINE spag_block_5
!
!     PIN FLAGS
!
      CALL ploapf(Ept,Ept,len,Pa,Pb)
!
!     LOAD VECTORS DONE FOR SDRX
!
      IF ( Opt==2 ) RETURN
!
!     TRANSFORM LOAD VECTOR TO GLOBAL
!
      CALL gmmats(e,3,3,1,Pa(1),3,1,0,tp)
      CALL gmmats(Ta,3,3,1,tp,3,1,0,Pa(1))
      CALL gmmats(e,3,3,1,Pb(1),3,1,0,tp)
      CALL gmmats(Tb,3,3,1,tp,3,1,0,Pb(1))
      CALL gmmats(e,3,3,1,Pa(4),3,1,0,tp)
      CALL gmmats(Ta,3,3,1,tp,3,1,0,Pa(4))
      CALL gmmats(e,3,3,1,Pb(4),3,1,0,tp)
      CALL gmmats(Tb,3,3,1,tp,3,1,0,Pb(4))
!
      DO i = 1 , 3
         IF ( Sa(i)/=0.0 ) THEN
            CALL spag_block_6
            RETURN
         ENDIF
      ENDDO
      CALL spag_block_7
      RETURN
   END SUBROUTINE spag_block_5
   SUBROUTINE spag_block_6
      d(2) = -Sa(3)
      d(3) = Sa(2)
      d(4) = Sa(3)
      d(6) = -Sa(1)
      d(7) = -Sa(2)
      d(8) = Sa(1)
      CALL gmmats(d,3,3,0,Pa(1),3,1,0,tp)
      Pa(4) = Pa(4) + tp(1)
      Pa(5) = Pa(5) + tp(2)
      Pa(6) = Pa(6) + tp(3)
      CALL spag_block_7
   END SUBROUTINE spag_block_6
   SUBROUTINE spag_block_7
!
      DO i = 1 , 3
         IF ( Sb(i)/=0.0 ) THEN
            CALL spag_block_8
            RETURN
         ENDIF
      ENDDO
      RETURN
   END SUBROUTINE spag_block_7
   SUBROUTINE spag_block_8
      d(2) = -Sb(3)
      d(3) = Sb(2)
      d(4) = Sb(3)
      d(6) = -Sb(1)
      d(7) = -Sb(2)
      d(8) = Sb(1)
      CALL gmmats(d,3,3,0,Pb(1),3,1,0,tp)
      Pb(4) = Pb(4) + tp(1)
      Pb(5) = Pb(5) + tp(2)
      Pb(6) = Pb(6) + tp(3)
   END SUBROUTINE spag_block_8
!
END SUBROUTINE pload1
