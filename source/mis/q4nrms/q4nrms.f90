!*==q4nrms.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE q4nrms(Bgpdt,Gpnorm,Iorder,Iflag)
   IMPLICIT NONE
   INTEGER Iflag
   REAL Bgpdt(4,4) , Gpnorm(4,4)
   INTEGER Iorder(4)
   DOUBLE PRECISION adi(4) , aetd(4) , di , dmag , dshp(4) , dsshp(4,2) , dv(3,3) , etd , tdshp(4) , tdsshp(4,2)
   REAL aeta(4) , axi(4) , eta , shp(4) , sshp(4,2) , tshp(4) , tsshp(4,2) , v(3,3) , vmag , xi
   INTEGER i , ii , ik , io , j , j1 , k
!     &    ENTRY Q4NRMD (BGPDT,GPNORM,IORDER,IFLAG)
!
!*****
!     COMPUTES UNIT NORMAL VECTORS FOR QUAD4 GRID POINTS.
!*****
   DATA axi/ - 1.0 , 1.0 , 1.0 , -1.0/
   DATA aeta/ - 1.0 , -1.0 , 1.0 , 1.0/
   DATA adi/ - 1.0D0 , 1.0D0 , 1.0D0 , -1.0D0/
   DATA aetd/ - 1.0D0 , -1.0D0 , 1.0D0 , 1.0D0/
!
!     SINGLE PRECISION SECTION -
!*****
!     COMPUTE SHAPE FUNCTION DERIVATIVES
!*****
   Iflag = 0
   SPAG_Loop_1_1: DO ii = 1 , 4
      io = Iorder(ii)
      xi = axi(io)
      eta = aeta(io)
      CALL q4shps(xi,eta,shp,sshp)
!*****
!     SORT THE SHAPE FUNCTIONS
!*****
      DO i = 1 , 4
         tshp(i) = shp(i)
         DO j = 1 , 2
            tsshp(i,j) = sshp(i,j)
         ENDDO
      ENDDO
!
      DO ik = 1 , 4
         i = Iorder(ik)
         shp(ik) = tshp(i)
         DO j = 1 , 2
            sshp(ik,j) = tsshp(i,j)
         ENDDO
      ENDDO
!*****
!     COMPUTE VECTOR
!*****
      DO i = 1 , 2
         DO j = 1 , 3
            v(i,j) = 0.0
            j1 = j + 1
            DO k = 1 , 4
               v(i,j) = v(i,j) + sshp(k,i)*Bgpdt(j1,k)
            ENDDO
         ENDDO
      ENDDO
!
      v(3,1) = v(1,2)*v(2,3) - v(2,2)*v(1,3)
      v(3,2) = v(1,3)*v(2,1) - v(2,3)*v(1,1)
      v(3,3) = v(1,1)*v(2,2) - v(2,1)*v(1,2)
      vmag = v(3,1)**2 + v(3,2)**2 + v(3,3)**2
!
      IF ( vmag>1.0E-11 ) THEN
!
         vmag = sqrt(vmag)
         Gpnorm(2,ii) = v(3,1)/vmag
         Gpnorm(3,ii) = v(3,2)/vmag
         Gpnorm(4,ii) = v(3,3)/vmag
      ELSE
         Iflag = 1
         EXIT SPAG_Loop_1_1
      ENDIF
   ENDDO SPAG_Loop_1_1
   RETURN
!
   ENTRY q4nrmd(Bgpdt,Gpnorm,Iorder,Iflag)
!     =======================================
!
!     DOUBLE PRECISION SECTION -
!
!*****
!     COMPUTE SHAPE FUNCTION DERIVATIVES
!*****
   Iflag = 0
   SPAG_Loop_1_2: DO ii = 1 , 4
      io = Iorder(ii)
      di = adi(io)
      etd = aetd(io)
      CALL q4shpd(di,etd,dshp,dsshp)
!
!     SORT THE SHAPE FUNCTIONS
!
      DO i = 1 , 4
         tdshp(i) = dshp(i)
         DO j = 1 , 2
            tdsshp(i,j) = dsshp(i,j)
         ENDDO
      ENDDO
!
      DO ik = 1 , 4
         i = Iorder(ik)
         dshp(ik) = tdshp(i)
         DO j = 1 , 2
            dsshp(ik,j) = tdsshp(i,j)
         ENDDO
      ENDDO
!*****
!     COMPUTE VECTOR
!*****
      DO i = 1 , 2
         DO j = 1 , 3
            dv(i,j) = 0.0D0
            j1 = j + 1
            DO k = 1 , 4
               dv(i,j) = dv(i,j) + dsshp(k,i)*Bgpdt(j1,k)
            ENDDO
         ENDDO
      ENDDO
!
      dv(3,1) = dv(1,2)*dv(2,3) - dv(2,2)*dv(1,3)
      dv(3,2) = dv(1,3)*dv(2,1) - dv(2,3)*dv(1,1)
      dv(3,3) = dv(1,1)*dv(2,2) - dv(2,1)*dv(1,2)
      dmag = dv(3,1)**2 + dv(3,2)**2 + dv(3,3)**2
!
      IF ( dmag>1.0D-11 ) THEN
!
         dmag = dsqrt(dmag)
         Gpnorm(2,ii) = dv(3,1)/dmag
         Gpnorm(3,ii) = dv(3,2)/dmag
         Gpnorm(4,ii) = dv(3,3)/dmag
      ELSE
         Iflag = 1
         EXIT SPAG_Loop_1_2
      ENDIF
   ENDDO SPAG_Loop_1_2
!
END SUBROUTINE q4nrms
