
SUBROUTINE pla41
   IMPLICIT NONE
   INTEGER Incupk , Itypeb , Iunpk , Junpk , Sysbuf
   REAL Z(1)
   COMMON /system/ Sysbuf
   COMMON /unpakx/ Itypeb , Iunpk , Junpk , Incupk
   COMMON /zzzzzz/ Z
   INTEGER buffr1 , buffr2 , clsrw , ecptnl , ecpts , eltype , eor , file , i , iecpt(100) , iflag , index , izmax , j , k , left , &
         & mcbugv(7) , name(2) , neor , ngpts(40) , nogpts , npvt , nwds , nwords(40) , ugv
   INTEGER korsz
   REAL xecpt(100)
!
!     THIS ROUTINE APPENDS DISPLACEMENT VECTOR INFORMATION TO THE
!     ECPTNL DATA BLOCK AND CREATES A SCRATCH DATA BLOCK, ECPTS, OF
!     THIS MERGED INFORMATION.  ECPTS IS PROCESSED BY SUBROUTINE PLA41.
!
   EQUIVALENCE (xecpt(1),iecpt(1))
   DATA ugv , ecptnl , ecpts/106 , 103 , 301/
   DATA name/4HPLA4 , 4H1   /
   DATA eor , neor , clsrw/1 , 0 , 1/
!
!    1        ROD       BEAM      TUBE      SHEAR     TWIST
!    2        TRIA1     TRBSC     TRPLT     TRMEM     CONROD
!    3        ELAS1     ELAS2     ELAS3     ELAS4     QDPLT
!    4        QDMEM     TRIA2     QUAD2     QUAD1     DAMP1
!    5        DAMP2     DAMP3     DAMP4     VISC      MASS1
!    6        MASS2     MASS3     MASS4     CONM1     CONM2
!    7        PLOTEL    REACT     QUAD3     BAR       CONE
!    8          X         X         X         X         X
!
   DATA nwords/20 , 0 , 19 , 0 , 0 , 33 , 0 , 0 , 27 , 20 , 0 , 0 , 0 , 0 , 0 , 32 , 27 , 32 , 38 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , &
      & 0 , 0 , 0 , 0 , 0 , 0 , 45 , 0 , 0 , 0 , 0 , 0 , 0/
   DATA ngpts/2 , 2 , 2 , 4 , 4 , 3 , 3 , 3 , 3 , 2 , 2 , 2 , 2 , 2 , 4 , 4 , 3 , 4 , 4 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 ,   &
      & 2 , 2 , 0 , 0 , 2 , 2 , 0 , 0 , 0 , 0 , 0/
!
!     INITIALIZE
!
   izmax = korsz(Z)
   buffr1 = izmax - Sysbuf
   buffr2 = buffr1 - Sysbuf
   left = buffr2 - 1
!
!     READ THE DISPLACEMENT VECTOR INTO OPEN CORE.
!
   file = ugv
   CALL gopen(ugv,Z(buffr1),0)
   mcbugv(1) = ugv
   CALL rdtrl(mcbugv)
   IF ( left<mcbugv(3) ) CALL mesage(-8,0,name)
   Itypeb = 1
   Iunpk = 1
   Junpk = mcbugv(3)
   Incupk = 1
   CALL unpack(*600,ugv,Z(1))
   CALL close(ugv,clsrw)
!
!     OPEN THE ECPTNL AND ECPTS FILES.
!
   CALL gopen(ecpts,Z(buffr1),1)
   CALL gopen(ecptnl,Z(buffr2),0)
!
!     READ AND WRITE THE PIVOT POINT
!
 100  CALL read(*300,*500,ecptnl,npvt,1,neor,iflag)
   CALL write(ecpts,npvt,1,neor)
   DO
!
!     READ ELEMENT TYPE
!
      CALL read(*400,*200,ecptnl,eltype,1,neor,iflag)
      j = nwords(eltype)
      IF ( j<=0 ) CALL mesage(-30,114,iecpt(1))
!
!     READ THE ECPT ENTRY FOR THIS ELEMENT.
!
      CALL fread(ecptnl,xecpt,j,0)
!
!     APPEND DISPLACEMENT VECTOR TO THE ECPT ENTRY
!
      j = j + 1
      nwds = 3
      IF ( eltype==34 ) nwds = 6
      nogpts = ngpts(eltype)
      DO i = 1 , nogpts
         index = iecpt(i+1)
         DO k = 1 , nwds
            xecpt(j) = Z(index)
            index = index + 1
            j = j + 1
         ENDDO
      ENDDO
!
!     THE ECPT ENTRY IS NOW COMPLETE.  WRITE IT OUT.
!
      CALL write(ecpts,eltype,1,neor)
      CALL write(ecpts,xecpt,j-1,neor)
   ENDDO
!
!     AN EOR HAS BEEN READ ON ECPTNL.  WRITE EOR ON ECPTS.
!
 200  CALL write(ecpts,0,0,eor)
   GOTO 100
!
!     PROCESSING IS COMPLETE.  CLOSE FILES.
!
 300  CALL close(ecptnl,clsrw)
   CALL close(ecpts,clsrw)
   RETURN
!
!     FATAL ERRORS
!
 400  CALL mesage(-2,file,name)
 500  CALL mesage(-3,file,name)
 600  CALL mesage(-30,83,name)
END SUBROUTINE pla41
