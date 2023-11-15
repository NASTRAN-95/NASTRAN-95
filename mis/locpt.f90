
SUBROUTINE locpt(N,P,M,S,K,Ks,Eps,Loc)
   IMPLICIT NONE
!
! Dummy argument declarations
!
   INTEGER M , N
   DOUBLE PRECISION Eps(2) , Ks(3) , P(3,4) , S(3,4)
   INTEGER K(2,1) , Loc(1)
!
! Local variable declarations
!
   DOUBLE PRECISION dadotb , dvmag
   DOUBLE PRECISION edotp , v(3) , vdotk , ve(3,4) , vemag(4) , vmag , vp(3) , vpmag
   INTEGER i , k1 , k2 , ne , np
!
! End of declarations
!
!
!     DETERMINES POSITION OF EACH OF N POINTS (P) RELATIVE TO SURFACE
!        BOUNDED BY M POINTS (S)
!        ALL POINTS IN THE SAME COORDINATE SYSTEM
!     KS IS THE (UNIT) VECTOR NORMAL TO THE SURFACE
!     LOC(I) IS FLAG INDICATING POSITION OF POINT I RELATIVE TO SURFACE:
!        LOC= 1    WHEN POINT WITHIN SURFACE BOUNDRY
!        LOC= 0    WHEN POINT IS ON SURFACE BOUNDRY
!        LOC= -1   WHEN POINT OUTSIDE SURFACE BOUNDRY
!
!     MAXIMUM OF 4 POINTS MAY BE LOCATED RELATIVE
!        TO SURFACE WITH MAXIMUM OF 4 SIDES
!        WHOSE ENDPOINTS ARE IN K(2,M)
!
!
!
!
!     EPS ARRAY FOR SIGNIFICANCE TESTING
!        EPS(1) IS AREA, ANGLE LIMIT
!        EPS(2) IS LENGTH LIMIT
!
!
!
!     SET UP VECTORS ALONG EACH SURFACE EDGE
!
   DO ne = 1 , M
      k1 = K(1,ne)
      k2 = K(2,ne)
!
      DO i = 1 , 2
!     VE IS VECTOR ALONG SURFACE EDGE
         ve(i,ne) = S(i,k2) - S(i,k1)
      ENDDO
      ve(3,ne) = 0.D0
      vemag(ne) = dvmag(ve(1,ne),Eps(2))
   ENDDO
!
!      DETERMINE LOCATION OF POINT RELATIVE TO SURFACE
!
   DO np = 1 , N
!        (PRESET POINT FLAG TO INTERIOR CODE)
      Loc(np) = 1
!
      DO ne = 1 , M
         k1 = K(1,ne)
!
         DO i = 1 , 2
!     VP IS VECTOR FROM FIRST END OF EDGE VECTOR TO POINT
            vp(i) = P(i,np) - S(i,k1)
         ENDDO
         vp(3) = 0.D0
         vpmag = dvmag(vp,Eps(2))
!
!     V= VE CROSS VP
         CALL daxb(ve(1,ne),vp,v)
         vmag = dvmag(v,Eps(1))
!     VDOTK= (VE CROSS VP) DOT K,  K NORMAL TO PLANE OF SURFACE
         vdotk = dadotb(v,Ks)
!     EDOTP IS VE DOT VP
         edotp = dadotb(ve(1,ne),vp)
         IF ( vpmag<=Eps(2) ) GOTO 100
         IF ( vdotk<=Eps(1) ) THEN
!                                      INSIDE THIS EDGE
!                                                          OUTSIDE
            IF ( (vdotk>=-Eps(1)) .AND. (edotp>Eps(1)) .AND. (vemag(ne)+Eps(2)>=vpmag) ) GOTO 100
            GOTO 50
         ENDIF
!                  ON THIS EDGE
      ENDDO
!     POINT IS WITHIN SURFACE BOUNDRY IF NOT OUTSIDE ANY EDGE
!        AND NOT ON SURFACE BOUNDRY
      CYCLE
!
!     POINT IS OUTSIDE SURFACE BOUNDRY IF OUTSIDE ANY EDGE
 50   Loc(np) = -1
      CYCLE
!
!
!     POINT IS ON BOUNDRY WHEN ANGLE IS EFFECTIVELY ZERO
!        OR (EFFECTIVELY) COINCIDENT WITH EDGE POINT
 100  Loc(np) = 0
!
   ENDDO
END SUBROUTINE locpt
