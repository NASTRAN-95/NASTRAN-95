!*==pla31.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pla31
   USE c_blank
   USE c_system
   USE c_unpakx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: bufr1 , bufr2 , eltype , file , i , idisp , iflag , index , izmax , j , k , left , nogpts , nwds , nwdsrd
   INTEGER , SAVE :: clsrw , delugv , eor , estnl , estnls , neor
   REAL , DIMENSION(100) :: estbk
   INTEGER , DIMENSION(40) , SAVE :: estwds , ngpts
   INTEGER , DIMENSION(100) :: iestbk
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(7) :: mcbugv
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL close , gopen , korsz , mesage , rdtrl , read , unpack , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE READS THE INCREMENTAL DISPLACEMENT VECTOR INTO CORE
!     AND APPENDS THE PROPER DISPLACEMENT VALUES TO THE ESTNL ENTRY FOR
!     EACH ELEMENT, THEREBY CREATING THE ESTNLS, THE ESTNL SCRATCH FILE,
!     WHICH IS PROCESSED BY SUBROUTINE PLA32.
!
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (estbk(1),iestbk(1))
   DATA name/4HPLA3 , 4H1   /
   DATA delugv , estnl , estnls/104 , 105 , 301/
   DATA eor , neor , clsrw/1 , 0 , 1/
!
!    1        ROD       BEAM      TUBE      SHEAR     TWIST
!    2        TRIA1     TRBSC     TRPLT     TRMEM     CONROD
!    3        ELAS1     ELAS2     ELAS3     ELAS4     QDPLT
!    4        QDMEM     TRIA2     QUAD2     QUAD1     DAMP1
!    5        DAMP2     DAMP3     DAMP4     VISC      MASS1
!    6        MASS2     MASS3     MASS4     CONM1     CONM2
!    7        PLOTEL    REACT     QUAD3     BAR       CONE
!    8        TRIARG    TRAPRG    TORDRG    CORE      CAP
!
   DATA estwds/21 , 0 , 20 , 0 , 0 , 38 , 0 , 0 , 27 , 21 , 0 , 0 , 0 , 0 , 0 , 32 , 32 , 37 , 43 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , &
      & 0 , 0 , 0 , 0 , 0 , 0 , 50 , 0 , 0 , 0 , 0 , 0 , 0/
   DATA ngpts/2 , 2 , 2 , 4 , 4 , 3 , 3 , 3 , 3 , 2 , 2 , 2 , 2 , 2 , 4 , 4 , 3 , 4 , 4 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 , 2 ,   &
      & 2 , 2 , 0 , 0 , 2 , 2 , 3 , 4 , 2 , 4 , 2/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     DETERMINE SIZE OF CORE, DEFINE BUFFERS AND INITIALIZE CORE
!     POINTERS AND COUNTERS
!
         izmax = korsz(z)
         bufr1 = izmax - bufsz
         bufr2 = bufr1 - bufsz
         left = bufr2 - 1
         idisp = 0
!
!     OPEN THE DISPLACEMENT VECTOR FILE AND READ THE DISPLACEMENT VECTOR
!     INTO OPEN CORE.
!
         file = delugv
         CALL gopen(delugv,z(bufr1),0)
         mcbugv(1) = delugv
         CALL rdtrl(mcbugv)
         IF ( left<mcbugv(3) ) CALL mesage(-8,0,name)
         itypeb = 1
         iunpk = 1
         junpk = mcbugv(3)
         incupk = 1
         CALL unpack(*100,delugv,z(idisp+1))
         CALL close(delugv,clsrw)
!
!     BUILD THE SCRATCH FILE ESTNLS
!
         CALL gopen(estnl,z(bufr1),0)
         CALL gopen(estnls,z(bufr2),1)
         spag_nextblock_1 = 2
      CASE (2)
!
!     READ AN ELEMENT TYPE FROM ESTNL AND WRITE IT ON ESTNLS.
!
         CALL read(*40,*80,estnl,eltype,1,neor,iflag)
         nwdsrd = estwds(eltype)
         IF ( nwdsrd<=0 ) CALL mesage(-30,91,eltype)
         CALL write(estnls,eltype,1,neor)
         DO
!
!     READ AN ESTNL ENTRY
!
            j = nwdsrd
            CALL read(*60,*20,estnl,estbk,j,neor,iflag)
            nogpts = ngpts(eltype)
            IF ( nogpts<=0 ) CALL mesage(-30,92,eltype)
!
!     APPEND THE DISPLACEMENT VECTORS ONTO THE ESTBK.
!
            nwds = 3
            j = j + 1
            IF ( eltype==1 .OR. eltype==3 .OR. eltype==10 .OR. eltype==6 .OR. eltype==17 .OR. eltype==18 .OR. eltype==19 .OR.       &
               & eltype==34 ) nwds = 6
            DO i = 1 , nogpts
               index = idisp + iestbk(i+1)
               DO k = 1 , nwds
                  estbk(j) = z(index)
                  index = index + 1
                  j = j + 1
               ENDDO
            ENDDO
!
!     THE APPENDED ESTNL ENTRY, WHICH IS AT ESTBK IS NOW COMPLETE.
!
            CALL write(estnls,estbk,j-1,neor)
         ENDDO
!
!     WRITE AN EOR ON THE ESTNLS FILE.
!
 20      CALL write(estnls,0,0,eor)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     PROCESSING IS NOW COMPLETE
!
 40      CALL close(estnl,clsrw)
         CALL close(estnls,clsrw)
         RETURN
!
!     FATAL ERRORS
!
 60      CALL mesage(-2,file,name)
 80      CALL mesage(-3,file,name)
 100     CALL mesage(-5,delugv,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE pla31
