!*==ssold2.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ssold2(Itype,Ftemp)
   IMPLICIT NONE
   USE C_SDR2X4
   USE C_SDR2X7
   USE C_SDR2X8
   USE C_SDR2X9
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Itype
   REAL , DIMENSION(8) :: Ftemp
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(2) , SAVE :: frlast
   INTEGER :: i , j , k , n
   INTEGER , DIMENSION(7) :: ishd
   INTEGER , DIMENSION(2) :: istyp
   INTEGER , SAVE :: lld , lsub
   INTEGER , DIMENSION(1) :: nphi
   INTEGER , DIMENSION(8) , SAVE :: typ
   EXTERNAL eject , page1 , sd2rhd , sdrchk , smmats
!
! End of declarations rewritten by SPAG
!
!
!     PHASE TWO STRESS DATA RECOVERY FOR THE SOLID ELEMENTS
!
!     ITYPE = 1,2,3,OR4 CORRESPONDING TO THE TETRA,WEDGE,HEXA1,ORHEXA2
!             ELEMENTS
!
!     PHIOUT CONTAINS THE FOLLOWING WHERE N IS THE NUMBER OF CORNERS
!
!             ELEMENT ID
!             N SILS
!             T SUB 0
!             6 THERMAL STRESS COEFFICIENTS
!             N VOLUME RATIO COEFFICIENTS
!             N 6 BY 3 MATRICES RELATING STRESS TO DISPLACEMENTS
!
!  $MIXED_FORMATS
!
   !>>>>EQUIVALENCE (Nphi(1),Phiout(1)) , (ishd(1),lsub) , (ishd(2),lld) , (ishd(6),frlast(1))
   DATA typ/4HTETR , 1HA , 4HWEDG , 1HE , 4HHEXA , 1H1 , 4HHEXA , 1H2/
   DATA lld , lsub , frlast/2* - 1 , -1.0E30 , -1.0E30/
!
   IF ( Itype==2 ) THEN
      Npts = 6
   ELSEIF ( Itype==3 .OR. Itype==4 ) THEN
      Npts = 8
   ELSE
      Npts = 4
   ENDIF
!
!
   DO i = 1 , 9
      Csig(i) = 0.0
      Sigma(i) = 0.0
   ENDDO
!
!     LOOP ON GRID POINTS, DISPLACEMENT EFFECTS
!
   DO n = 1 , Npts
      Npoint = Ivec + nphi(n+1) - 1
      Ks = 18*n + 2*Npts - 9
      CALL smmats(Phiout(Ks),6,3,0,Z(Npoint),3,1,0,Temp,Ctmp)
      DO i = 1 , 6
         Csig(i+1) = Csig(i+1) + Ctmp(i)
         Sigma(i+1) = Sigma(i+1) + Temp(i)
      ENDDO
!
!     TEMPERATURE EFFECTS
!
      IF ( Ldtemp/=-1 ) THEN
         Kbeta = Npts + n + 8
         Factor = (Ftemp(n)-Phiout(Npts+2))*Phiout(Kbeta)
!
         DO i = 1 , 6
            Kbeta = Npts + i + 2
            Sigma(i+1) = Sigma(i+1) - Phiout(Kbeta)*Factor
         ENDDO
      ENDIF
   ENDDO
   Sigma(1) = Phiout(1)
   DO i = 1 , 7
      Stres(i) = Sigma(i)
   ENDDO
!
!     OCTAHEDRAL STRESS AND HYDROSTATIC PRESSURE
!
   Stres(8) = sqrt(Sigma(2)*(Sigma(2)-Sigma(3)-Sigma(4))*2.0+2.0*Sigma(3)*(Sigma(3)-Sigma(4))+2.0*Sigma(4)                          &
            & **2+6.0*(Sigma(5)**2+Sigma(6)**2+Sigma(7)**2))/3.0
   Stres(9) = -(Sigma(2)+Sigma(3)+Sigma(4))/3.0
   IF ( Nchk>0 ) THEN
!
!   . CHECK PRECISION
!
      Csig(1) = Phiout(1)
      k = 0
!
!   . STRESSES
!
      CALL sdrchk(Sigma(2),Csig(2),6,k)
      IF ( k==0 ) RETURN
!
!   . LIMITS EXCEEDED
!
      j = 2*Itype
      istyp(1) = typ(j-1)
      istyp(2) = typ(j)
      j = 0
!
      IF ( lsub/=Isub .OR. frlast(1)/=Frtmei(1) .OR. lld/=Ild .OR. frlast(2)/=Frtmei(2) ) THEN
         lsub = Isub
         lld = Ild
         frlast(1) = Frtmei(1)
         frlast(2) = Frtmei(2)
         j = 2
         CALL page1
      ELSEIF ( eject(2)==0 ) THEN
         GOTO 50
      ENDIF
      CALL sd2rhd(ishd,j)
      Line = Line + 1
      WRITE (Nout,99001)
99001 FORMAT (7X,4HTYPE,5X,3HEID,5X,2HSX,5X,2HSY,5X,2HSZ,4X,3HTYZ,4X,3HTXZ,4X,3HTXY)
 50   WRITE (Nout,99002,ERR=99999) istyp , Csig
99002 FORMAT (1H0,6X,A4,A1,I7,6F7.1)
   ENDIF
!
99999 END SUBROUTINE ssold2
