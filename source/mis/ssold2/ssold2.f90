!*==ssold2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ssold2(Itype,Ftemp)
   USE c_sdr2x4
   USE c_sdr2x7
   USE c_sdr2x8
   USE c_sdr2x9
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
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
      npts = 6
   ELSEIF ( Itype==3 .OR. Itype==4 ) THEN
      npts = 8
   ELSE
      npts = 4
   ENDIF
!
!
   DO i = 1 , 9
      csig(i) = 0.0
      sigma(i) = 0.0
   ENDDO
!
!     LOOP ON GRID POINTS, DISPLACEMENT EFFECTS
!
   DO n = 1 , npts
      npoint = ivec + nphi(n+1) - 1
      ks = 18*n + 2*npts - 9
      CALL smmats(phiout(ks),6,3,0,z(npoint),3,1,0,temp,ctmp)
      DO i = 1 , 6
         csig(i+1) = csig(i+1) + ctmp(i)
         sigma(i+1) = sigma(i+1) + temp(i)
      ENDDO
!
!     TEMPERATURE EFFECTS
!
      IF ( ldtemp/=-1 ) THEN
         kbeta = npts + n + 8
         factor = (Ftemp(n)-phiout(npts+2))*phiout(kbeta)
!
         DO i = 1 , 6
            kbeta = npts + i + 2
            sigma(i+1) = sigma(i+1) - phiout(kbeta)*factor
         ENDDO
      ENDIF
   ENDDO
   sigma(1) = phiout(1)
   DO i = 1 , 7
      stres(i) = sigma(i)
   ENDDO
!
!     OCTAHEDRAL STRESS AND HYDROSTATIC PRESSURE
!
   stres(8) = sqrt(sigma(2)*(sigma(2)-sigma(3)-sigma(4))*2.0+2.0*sigma(3)*(sigma(3)-sigma(4))+2.0*sigma(4)                          &
            & **2+6.0*(sigma(5)**2+sigma(6)**2+sigma(7)**2))/3.0
   stres(9) = -(sigma(2)+sigma(3)+sigma(4))/3.0
   IF ( nchk>0 ) THEN
!
!   . CHECK PRECISION
!
      csig(1) = phiout(1)
      k = 0
!
!   . STRESSES
!
      CALL sdrchk(sigma(2),csig(2),6,k)
      IF ( k==0 ) RETURN
!
!   . LIMITS EXCEEDED
!
      j = 2*Itype
      istyp(1) = typ(j-1)
      istyp(2) = typ(j)
      j = 0
!
      IF ( lsub/=isub .OR. frlast(1)/=frtmei(1) .OR. lld/=ild .OR. frlast(2)/=frtmei(2) ) THEN
         lsub = isub
         lld = ild
         frlast(1) = frtmei(1)
         frlast(2) = frtmei(2)
         j = 2
         CALL page1
      ELSEIF ( eject(2)==0 ) THEN
         GOTO 50
      ENDIF
      CALL sd2rhd(ishd,j)
      line = line + 1
      WRITE (nout,99001)
99001 FORMAT (7X,4HTYPE,5X,3HEID,5X,2HSX,5X,2HSY,5X,2HSZ,4X,3HTYZ,4X,3HTXZ,4X,3HTXY)
 50   WRITE (nout,99002,ERR=99999) istyp , csig
99002 FORMAT (1H0,6X,A4,A1,I7,6F7.1)
   ENDIF
!
99999 END SUBROUTINE ssold2
