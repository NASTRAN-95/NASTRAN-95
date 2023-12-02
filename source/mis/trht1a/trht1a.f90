!*==trht1a.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE trht1a(Casexx,Usetd,Gptt,Trl,Ngroup)
   IMPLICIT NONE
   USE C_BITPOS
   USE C_BLANK
   USE C_PACKX
   USE C_SYSTEM
   USE C_TRDD1
   USE C_TRHTX
   USE C_TWO
   USE C_ZBLPKX
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Casexx
   INTEGER :: Usetd
   INTEGER :: Gptt
   INTEGER :: Trl
   INTEGER :: Ngroup
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: file , i , ibuf1 , ibuf2 , iflag , iflg , igroup , inext , inltmp , intmp , ip1 , ipos , its , itstep , ival , j , k ,&
            & l , list , lusetd , m , mskud , mskue , ns , nsk , nx , nz
   INTEGER , DIMENSION(1) :: ia
   INTEGER , DIMENSION(160) :: iz
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL :: tdflt
   EXTERNAL andf , bckrec , close , fread , fwdrec , gopen , korsz , makmcb , mesage , open , read , skprec , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     TRHT1A INITIALIZES FOR TRHT MODULE
!
!     ITS TASK IS TO EXTRACT INITIAL CONDITION POINTS FROM CASEXX
!     AND TO PUT INITIAL STUFF ON ICR5
!
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (A(1),Ia(1))
   DATA name/4HTRHT , 4H1A  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
         nz = korsz(Z)
         nx = nz
         ibuf1 = nz - Sysbuf + 1
         nz = nz - Sysbuf
         CALL gopen(Casexx,iz(ibuf1),0)
         CALL fread(Casexx,iz(1),166,1)
         CALL close(Casexx,1)
         itstep = iz(38)
         Nlftp1 = iz(160)
         intmp = iz(9)
         inltmp = iz(8)
!
!     FIND STUFF ON TRL
!
         file = Trl
         CALL open(*120,Trl,iz(ibuf1),0)
         CALL read(*140,*20,Trl,iz(1),nz,0,iflag)
         ip1 = -8
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 20      ns = iz(3)
         CALL skprec(Trl,ns)
         spag_nextblock_1 = 2
      CASE (2)
         CALL read(*160,*40,Trl,iz(1),nz,0,iflag)
         ip1 = -8
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 40      IF ( iz(1)/=itstep ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     TSTEP STUFF FOUND
!
         CALL close(Trl,1)
         Ngroup = (iflag-1)/3
!
!     MOVE TSETP STUFF TO BOTTOM OF CURE
!
         nz = nx - iflag + 1
         igroup = nz + 1
         DO i = 2 , iflag
            k = igroup + i - 2
            iz(k) = iz(i)
         ENDDO
         ibuf1 = nz - Sysbuf + 1
         ibuf2 = ibuf1 - Sysbuf
         nz = ibuf2
         CALL gopen(Iscr5,iz(ibuf1),1)
         CALL write(Iscr5,iz(igroup),iflag-1,1)
         file = Usetd
!
!     BRING IN USETD
!
         CALL gopen(Usetd,iz(ibuf2),0)
         CALL read(*140,*60,Usetd,iz(1),nz,1,lusetd)
         ip1 = -8
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 60      CALL close(Usetd,1)
!
!     BUILD SIL TO SILD CONVERTER TABLE
!
         mskue = Two1(Iue)
         mskud = Two1(Iud)
         m = 1
         l = 0
         DO i = 1 , lusetd
            IF ( andf(iz(i),mskue)==0 ) THEN
               l = l + 1
               IF ( andf(iz(i),mskud)==0 ) THEN
                  iz(l) = 0
                  CYCLE
               ELSE
                  iz(l) = m
               ENDIF
            ENDIF
            m = m + 1
         ENDDO
!
!     FIND STUFF IN GPTT
!
         its = intmp
         CALL makmcb(mcb,Iscr5,m-1,2,1)
         ns = 0
         file = Gptt
         CALL open(*120,Gptt,iz(ibuf2),0)
!
!     POSITION TO HEADER RECORD
!
         ival = nz - 2*l
         CALL read(*140,*80,Gptt,iz(l+1),ival,0,iflag)
         ip1 = -8
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
!
!     PUT OUT TEMPS
!
!
!     DETERMINE NUMBER OF ELEMENT TEMP RECORDS TO SKIP.
!
 80      list = l + 3
         k = l + iflag
         SPAG_Loop_1_1: DO
            nsk = iz(k)
            IF ( nsk>0 ) EXIT SPAG_Loop_1_1
            k = k - 3
            IF ( k<=list ) EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
!
!     SET IPOS TO SKIP ELEMENT TEMP RECORDS AND DUPLICATE HEADER.
!
         ipos = -nsk
         mcb(2) = 0
         spag_nextblock_1 = 3
      CASE (3)
         IF ( its==0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         k = list
         DO WHILE ( iz(k)/=its )
            k = k + 3
            IF ( k>l+iflag ) CALL mesage(-31,its,name)
         ENDDO
!
!     FOUND TEMP SET
!
         tdflt = 0.0
         IF ( iz(k+1)/=-1 ) tdflt = Z(k+1)
         m = l + iflag
         DO i = 1 , l
            j = m + i
            Z(j) = tdflt
         ENDDO
!
!     RECORD NUMBER OF TEMP SET FOUND
!
         ns = iz(k+2)
         IF ( ns==0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO
!
!     SKIP TO DESIRED RECORD
!
            IF ( ns<ipos ) THEN
               CALL bckrec(Gptt)
               ipos = ipos - 1
            ELSEIF ( ns==ipos ) THEN
               DO
                  CALL read(*140,*100,Gptt,A,2,0,iflg)
                  IF ( ia(1)>0 ) THEN
                     j = ia(1) + m
                     Z(j) = A(2)
                  ENDIF
               ENDDO
            ELSE
               CALL fwdrec(*140,Gptt)
               ipos = ipos + 1
            ENDIF
         ENDDO
 100     ipos = ipos + 1
         spag_nextblock_1 = 4
      CASE (4)
!
!     ALL SET UP OUTPUT
!
         inext = m + 1
         DO i = 1 , l
            j = m + i
            Ii = iz(i) + m
            IF ( Ii/=m ) THEN
               IF ( Ii/=inext ) THEN
                  DO k = inext , Ii
                     Z(k) = 0.0
                  ENDDO
               ENDIF
               Z(Ii) = Z(j)
               inext = Ii + 1
            ENDIF
         ENDDO
         j = inext - (m+1)
         CALL write(Iscr5,Z(m+1),j,0)
         spag_nextblock_1 = 5
      CASE (5)
         CALL write(Iscr5,Z(1),0,1)
         mcb(2) = mcb(2) + 1
         IF ( mcb(2)==2 ) THEN
!
!     ALL DONE
!
            CALL close(Iscr5,1)
            CALL close(Gptt,1)
            CALL wrttrl(mcb)
            RETURN
         ELSE
            its = inltmp
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     ERROR MESAGES
!
 120     ip1 = -1
         spag_nextblock_1 = 6
      CASE (6)
         CALL mesage(ip1,file,name)
         RETURN
 140     ip1 = -2
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 160     CALL mesage(-31,itstep,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE trht1a
