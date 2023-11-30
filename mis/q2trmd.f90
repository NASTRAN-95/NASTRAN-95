
SUBROUTINE q2trmd(Ra,Rb,Rc,Alpha,Isinth,Icosth,Gsube,It,Ierror,Iopt,Kmat,Pmat,Smat,Zmat)
   IMPLICIT NONE
   DOUBLE PRECISION Icosth , Isinth , It
   INTEGER Ierror , Iopt
   DOUBLE PRECISION Alpha(3) , Gsube(9) , Kmat(1) , Pmat(1) , Ra(3) , Rb(3) , Rc(3) , Smat(1) , Zmat(1)
   DOUBLE PRECISION alp(3) , c(3,6) , ca(6) , cb(6) , cc(6) , e(9) , hi(27) , hitge(9) , i33 , iarea , iareat , ic , is , ivec(3) , &
                  & ixsubb , ixsubc , iysubc , jvec(3) , kvec(3) , mag , temp9(9) , tm(9) , tm3(3) , v12(3) , v13(3)
   DOUBLE PRECISION dadotb
   INTEGER i , ipart(3,3) , ipartn , irow1 , j , jpartn , k , kk
!*****
!  SUB-TRIANGLE COMPUTATION ROUTINE FOR THE QDMEM2 ELEMENT
!
!  ON INPUT
!  ========
!            RA,RB,RC = 3 (3X1) COORDINATE VECTORS FOR TRIANGLE
!            IOPT     = 1  CALL FROM STIFFNESS GENERATION MODULE
!                     = 2  CALL FROM STATIC LOAD MODULE
!                     = 3  CALL FROM STRESS RECOVERY MODULE
!            ALPHA    = 3X1 VECTOR APPROPRIATE FOR CALL
!            ISINTH   = SIN OF MATERIAL ANGLE(WHOLE - ELEMENT)
!            ICOSTH   = COS OF MATERIAL ANGLE(WHOLE - ELEMENT)
!            GSUBE    = MATERIAL MATRIX (3X3)
!            IT       = THICKNESS OF ELEMENT
!
!  ON OUTPUT
!  =========
!            IERROR = 0  IF NO ERROR
!                   = 1  IF BAD ELEMENT GEOMETRY
!
!            KMAT,PMAT,SMAT,ZMAT = FOLLOWING PER IOPT VALUE SENT
!
!
!            IOPT=1
!            ------
!            KMAT = 7 (3X3)-S = KCA,KCB,KCC,KAA,KAB,KBA,KBB
!            PMAT = UNCHANGED
!            SMAT = UNCHANGED
!            ZMAT = UNCHANGED
!
!            IOPT=2
!            ------
!            KMAT = 3 (3X3)-S = KCA,KCB,KCC
!            PMAT = 3 (3X1)-S = PA,PB,PC
!            SMAT = UNCHANGED
!            ZMAT = UNCHANGED
!
!            IOPT=3
!            ------
!            KMAT = 7 (3X3)-S = KCA,KCB,KCC,KAA,KAB,KBA,KBB
!            PMAT = 3 (3X1)-S = PTA,PTB,PTC
!            SMAT = 3 (3X3)-S = SA,SB,SC
!            ZMAT = 3 (3X1)-S = ZA,ZB,ZC
!
!*****
!
!
!
   !>>>>EQUIVALENCE (c(1,1),ca(1)) , (c(1,3),cb(1)) , (c(1,5),cc(1))
   !>>>>EQUIVALENCE (e(1),ivec(1)) , (e(4),jvec(1)) , (e(7),kvec(1))
!
   DATA ipart/28 , 46 , 1 , 37 , 55 , 10 , 0 , 0 , 19/
!
!     V    = R   - R   ,      V    = R   - R
!      12     B     A          13     C     B
!
   DO i = 1 , 3
      v12(i) = Rb(i) - Ra(i)
      v13(i) = Rc(i) - Ra(i)
   ENDDO
!
!     KVEC(UN-NORMALIZED)  =  V     X  V
!                              12       13
!
   CALL daxb(v12,v13,kvec)
   mag = dsqrt(dadotb(kvec,kvec))
   IF ( mag<=0 ) THEN
!*****
!  ERROR CONDITION, BAD GEOMETRY.
!*****
      Ierror = 1
   ELSE
!
!     NORMALIZE  K-VECTOR, AND AREA
!
      kvec(1) = kvec(1)/mag
      kvec(2) = kvec(2)/mag
      kvec(3) = kvec(3)/mag
      iarea = 0.50D0*mag
!
!     I-VECTOR = V   (NORMALIZED) THUS
!                 12
!
      mag = dsqrt(dadotb(v12,v12))
      IF ( mag<=0 ) THEN
         Ierror = 1
      ELSE
         ivec(1) = v12(1)/mag
         ivec(2) = v12(2)/mag
         ivec(3) = v12(3)/mag
         ixsubb = mag
!
!     J-VECTOR = K-VECTOR CROSS I-VECTOR THUS
!
         CALL daxb(kvec,ivec,jvec)
!
!     MATERIAL COEFFICIENTS C AND S    U,V,W = I-VECTOR
!
         mag = dsqrt(ivec(1)**2+ivec(2)**2)
         IF ( mag<=0.D0 ) THEN
            Ierror = 1
         ELSE
            ic = (ivec(1)*Icosth+ivec(2)*Isinth)/mag
            is = (ivec(1)*Isinth-ivec(2)*Icosth)/mag
!
!     X = MAGNITUDE OF V  , X = I-VEC DOT V  , Y = J-VEC DOT V
!      B                12   C             13   C             13
!
            ixsubc = dadotb(ivec,v13)
            iysubc = dadotb(jvec,v13)
            IF ( ixsubb==0 ) THEN
               Ierror = 1
            ELSEIF ( iysubc/=0 ) THEN
!
               ca(1) = -1.0D0/ixsubb
               ca(2) = 0.0D0
               i33 = 1.0D0/iysubc
               ca(3) = i33*(ixsubc/ixsubb-1.0D0)
               ca(4) = 0.0D0
               ca(5) = ca(3)
               ca(6) = ca(1)
!
               cb(1) = -ca(1)
               cb(2) = 0.0D0
               cb(3) = -i33*(ixsubc/ixsubb)
               cb(4) = 0.0D0
               cb(5) = cb(3)
               cb(6) = cb(1)
!
               cc(1) = 0.0D0
               cc(2) = 0.0D0
               cc(3) = i33
               cc(4) = 0.0D0
               cc(5) = i33
               cc(6) = 0.0D0
!
!     FORM MATERIAL-ORIENTATION-TRANSFORMATION-MATRIX  (BY-ROWS)
!
               tm(1) = ic*ic
               tm(2) = is*is
               tm(3) = ic*is
               tm(4) = tm(2)
               tm(5) = tm(1)
               tm(6) = -tm(3)
               tm(7) = 2.0D0*tm(6)
               tm(8) = -tm(7)
               tm(9) = tm(1) - tm(2)
               iareat = iarea*It
!
!     IF SSG CALL MULTIPLY ALPHA(T-TO) VECTOR BY IAREAT
!
               IF ( Iopt==2 ) THEN
                  alp(1) = Alpha(1)*iareat
                  alp(2) = Alpha(2)*iareat
                  alp(3) = Alpha(3)*iareat
               ENDIF
!
!     IF SDR CALL COMPUTE AREA   = X  * T
!                                   B
               IF ( Iopt==3 ) THEN
                  tm3(1) = tm(3)*It
                  tm3(2) = tm(6)*It
                  tm3(3) = tm(9)*It
               ENDIF
!
!     SET FIRST PARTITION ROW TO COMPUTE FOR STIFFNESS MATRICES.
!
               irow1 = 1
               IF ( Iopt==2 ) irow1 = 3
!*****
!           M
!     H  = T  C  E
!      I       I
!
!*****
               DO i = 1 , 3
                  CALL gmmatd(tm,3,3,0,c(1,2*i-1),2,3,1,temp9)
                  CALL gmmatd(temp9,3,2,0,e,2,3,0,hi(9*i-8))
               ENDDO
!*****
!     FORM OUTPUTS FOR POINTS I = A,B,C
!*****
               DO i = 1 , 3
!
!              T
!     HITGE= H  G
!             I  E
!
                  CALL gmmatd(hi(9*i-8),3,3,1,Gsube,3,3,0,hitge)
!
!     STIFFNESS MATRIX CALCULATIONS
!
!     ONLY KAA,KAB     ARE FORMED.  OUTPUT ORDER WITH EACH 3X3 STORED
!          KBA,KBB                  BY ROWS =
!          KCA,KCB,KCC              KCA,KCB,KCC,KAA,KAB,KBA,KBB
!
                  IF ( i>=irow1 ) THEN
                     kk = 0
                     DO j = 1 , 3
                        ipartn = ipart(i,j)
                        IF ( ipartn>0 ) THEN
                           DO k = 1 , 9
                              kk = kk + 1
                              temp9(k) = hi(kk)*iareat
                           ENDDO
                           CALL gmmatd(hitge,3,3,0,temp9,3,3,0,Kmat(ipartn))
                        ENDIF
                     ENDDO
                  ENDIF
                  IF ( Iopt==1 ) THEN
                  ELSEIF ( Iopt==3 ) THEN
!*****
!  SDR ADDITIONAL PHASE-1 STRESS OUTPUTS
!*****
                     jpartn = 9*i - 8
                     CALL gmmatd(Gsube,3,3,0,hi(jpartn),3,3,0,Smat(jpartn))
                     ipartn = 3*i - 2
                     CALL gmmatd(hitge,3,3,0,Alpha,3,1,0,Pmat(ipartn))
                     CALL gmmatd(tm3,3,1,1,Smat(jpartn),3,3,0,Zmat(ipartn))
                     DO j = 1 , 3
                        k = ipartn + j - 1
                        Pmat(k) = Pmat(k)*iareat
                     ENDDO
                  ELSE
!****
!  SSG LOAD GENERATION CALL ADDITIONAL DATA TO OUTPUT.
!
!  ONLY PA,PB,PC ARE FORMED.
!*****
                     CALL gmmatd(hitge,3,3,0,alp,3,1,0,Pmat(3*i-2))
                  ENDIF
!
               ENDDO
               Ierror = 0
               RETURN
            ELSE
               Ierror = 1
            ENDIF
         ENDIF
      ENDIF
   ENDIF
END SUBROUTINE q2trmd