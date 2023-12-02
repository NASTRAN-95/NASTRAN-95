!*==pla41.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pla41
   IMPLICIT NONE
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buffr1 , buffr2 , eltype , file , i , iflag , index , izmax , j , k , left , nogpts , npvt , nwds
   INTEGER , SAVE :: clsrw , ecptnl , ecpts , eor , neor , ugv
   INTEGER , DIMENSION(100) :: iecpt
   INTEGER , DIMENSION(7) :: mcbugv
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(40) , SAVE :: ngpts , nwords
   REAL , DIMENSION(100) :: xecpt
   EXTERNAL close , fread , gopen , korsz , mesage , rdtrl , read , unpack , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE APPENDS DISPLACEMENT VECTOR INFORMATION TO THE
!     ECPTNL DATA BLOCK AND CREATES A SCRATCH DATA BLOCK, ECPTS, OF
!     THIS MERGED INFORMATION.  ECPTS IS PROCESSED BY SUBROUTINE PLA41.
!
   !>>>>EQUIVALENCE (xecpt(1),iecpt(1))
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         CALL unpack(*100,ugv,Z(1))
         CALL close(ugv,clsrw)
!
!     OPEN THE ECPTNL AND ECPTS FILES.
!
         CALL gopen(ecpts,Z(buffr1),1)
         CALL gopen(ecptnl,Z(buffr2),0)
         spag_nextblock_1 = 2
      CASE (2)
!
!     READ AND WRITE THE PIVOT POINT
!
         CALL read(*40,*80,ecptnl,npvt,1,neor,iflag)
         CALL write(ecpts,npvt,1,neor)
         DO
!
!     READ ELEMENT TYPE
!
            CALL read(*60,*20,ecptnl,eltype,1,neor,iflag)
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
 20      CALL write(ecpts,0,0,eor)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     PROCESSING IS COMPLETE.  CLOSE FILES.
!
 40      CALL close(ecptnl,clsrw)
         CALL close(ecpts,clsrw)
         RETURN
!
!     FATAL ERRORS
!
 60      CALL mesage(-2,file,name)
 80      CALL mesage(-3,file,name)
 100     CALL mesage(-30,83,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE pla41
