
SUBROUTINE pla31
   IMPLICIT NONE
   INTEGER Bufsz , Icom , Incupk , Itypeb , Iunpk , Iz(1) , Junpk
   REAL Z(1)
   COMMON /blank / Icom
   COMMON /system/ Bufsz
   COMMON /unpakx/ Itypeb , Iunpk , Junpk , Incupk
   COMMON /zzzzzz/ Z
   INTEGER bufr1 , bufr2 , clsrw , delugv , eltype , eor , estnl , estnls , estwds(40) , file , i , idisp , iestbk(100) , iflag ,   &
         & index , izmax , j , k , left , mcbugv(7) , name(2) , neor , ngpts(40) , nogpts , nwds , nwdsrd
   REAL estbk(100)
   INTEGER korsz
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
!
!     DETERMINE SIZE OF CORE, DEFINE BUFFERS AND INITIALIZE CORE
!     POINTERS AND COUNTERS
!
   izmax = korsz(Z)
   bufr1 = izmax - Bufsz
   bufr2 = bufr1 - Bufsz
   left = bufr2 - 1
   idisp = 0
!
!     OPEN THE DISPLACEMENT VECTOR FILE AND READ THE DISPLACEMENT VECTOR
!     INTO OPEN CORE.
!
   file = delugv
   CALL gopen(delugv,Z(bufr1),0)
   mcbugv(1) = delugv
   CALL rdtrl(mcbugv)
   IF ( left<mcbugv(3) ) CALL mesage(-8,0,name)
   Itypeb = 1
   Iunpk = 1
   Junpk = mcbugv(3)
   Incupk = 1
   CALL unpack(*600,delugv,Z(idisp+1))
   CALL close(delugv,clsrw)
!
!     BUILD THE SCRATCH FILE ESTNLS
!
   CALL gopen(estnl,Z(bufr1),0)
   CALL gopen(estnls,Z(bufr2),1)
!
!     READ AN ELEMENT TYPE FROM ESTNL AND WRITE IT ON ESTNLS.
!
 100  CALL read(*300,*500,estnl,eltype,1,neor,iflag)
   nwdsrd = estwds(eltype)
   IF ( nwdsrd<=0 ) CALL mesage(-30,91,eltype)
   CALL write(estnls,eltype,1,neor)
   DO
!
!     READ AN ESTNL ENTRY
!
      j = nwdsrd
      CALL read(*400,*200,estnl,estbk,j,neor,iflag)
      nogpts = ngpts(eltype)
      IF ( nogpts<=0 ) CALL mesage(-30,92,eltype)
!
!     APPEND THE DISPLACEMENT VECTORS ONTO THE ESTBK.
!
      nwds = 3
      j = j + 1
      IF ( eltype==1 .OR. eltype==3 .OR. eltype==10 .OR. eltype==6 .OR. eltype==17 .OR. eltype==18 .OR. eltype==19 .OR. eltype==34 )&
         & nwds = 6
      DO i = 1 , nogpts
         index = idisp + iestbk(i+1)
         DO k = 1 , nwds
            estbk(j) = Z(index)
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
 200  CALL write(estnls,0,0,eor)
   GOTO 100
!
!     PROCESSING IS NOW COMPLETE
!
 300  CALL close(estnl,clsrw)
   CALL close(estnls,clsrw)
   RETURN
!
!     FATAL ERRORS
!
 400  CALL mesage(-2,file,name)
 500  CALL mesage(-3,file,name)
 600  CALL mesage(-5,delugv,name)
END SUBROUTINE pla31