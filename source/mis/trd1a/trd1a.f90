!*==trd1a.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE trd1a(Casexx,Trl,Ic,Nlftp,Ngroup,Moda1)
   IMPLICIT NONE
   USE C_PACKX
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Casexx
   INTEGER :: Trl
   INTEGER :: Ic
   INTEGER :: Nlftp
   INTEGER :: Ngroup
   INTEGER :: Moda1
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: file , i , i2 , ibuf1 , ibuf2 , icp , icrq , idisp , iflag , igroup , ip1 , itrl , itstep , ivel , k , l , lud , nx , &
            & nz
   INTEGER , DIMENSION(2) , SAVE :: intrl , name
   INTEGER , DIMENSION(160) :: iz
   INTEGER , DIMENSION(7) :: mcb
   EXTERNAL close , fread , gopen , korsz , makmcb , mesage , open , pack , read , skprec , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE BUILDS THE INITIAL CONDITIONS TABLE, PUTS TSTEP STUFF
!     IN CORE AND EXTRACTS THE NLFTP POINTER
!
!     THIS ROUTINE IS SUITABLE FOR SINGLE PRECISION OPERATION
!
!
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
!
   DATA name , intrl/4HTRD1 , 4HA    , 4HTRL  , 4HTRD /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     IDENTIFICATION VARIABLES
!
!     NGROUP        NUMBER OF CHANGES OF TIME STEP
!
!     ITSTEP        SELECTED TSTEP ID
!
!     NLFTP         SELECTED NON-LINEAR LOAD ID
!
!     ICP           SELECTED INITIAL CONDITION ID
!
!     LUD           LENGTH OF INITIAL CONDITION--D SET
!
!     IGROUP        POINTER TO TSTEP STUFF
!
!
!
!     INITIALIZE
!
         It1 = 1
         It2 = 1
         Ii = 1
         Incr = 1
         nz = korsz(Z)
         nx = nz
!
!     PICK UP AND STORE CASECC POINTERS
!
         ibuf1 = nz - Sysbuf + 1
         CALL gopen(Casexx,iz(ibuf1),0)
         CALL fread(Casexx,iz,166,1)
         CALL close(Casexx,1)
         itstep = iz(38)
         icp = iz(9)
         Nlftp = iz(160)
         IF ( icp/=0 .AND. Moda1==1 ) THEN
            ip1 = -51
            file = icp
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     BUILD INITIAL CONDITION FILE
!
            CALL gopen(Ic,iz(ibuf1),1)
            ibuf2 = ibuf1 - Sysbuf
            nz = nz - 2*Sysbuf
            icrq = -nz
            IF ( icrq<=0 ) THEN
               file = Trl
               CALL open(*80,Trl,iz(ibuf2),0)
               CALL read(*100,*20,Trl,iz(1),nz,0,iflag)
               icrq = nz
            ENDIF
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 20      lud = iz(iflag)
         Jj = lud
         icrq = 2*lud - nz
         IF ( icrq>0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         l = iz(3)
         itrl = l
!
!     ZERO I. C.
!
         ivel = ibuf2 - lud - 1
         idisp = ivel - lud
         DO i = 1 , lud
            k = ivel + i
            Z(k) = 0.0
            k = idisp + i
            Z(k) = 0.0
         ENDDO
         CALL makmcb(mcb,Ic,lud,2,1)
         IF ( icp==0 ) GOTO 40
         IF ( iz(3)/=0 ) THEN
            iflag = iflag - 1
            DO i = 4 , iflag
               IF ( iz(i)==icp ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
         itstep = icp
         GOTO 120
      CASE (2)
         k = i - 4
         l = iflag - i
         CALL skprec(Trl,k)
         DO
            CALL read(*100,*40,Trl,iz(1),3,0,iflag)
            k = iz(1) + idisp
            i2 = 2
            Z(k) = Z(k) + Z(i2)
            k = iz(1) + ivel
            Z(k) = Z(k) + Z(i2+1)
         ENDDO
 40      CALL pack(Z(idisp+1),Ic,mcb)
         CALL pack(Z(ivel+1),Ic,mcb)
         CALL close(Ic,1)
         CALL wrttrl(mcb)
         CALL skprec(Trl,l)
         spag_nextblock_1 = 3
      CASE (3)
!
!     BRING TSTEP STUFF INTO CORE
!
         itrl = itrl + 1
         CALL read(*120,*60,Trl,iz(1),nz,0,iflag)
         icrq = nz
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 60      IF ( iz(1)/=itstep ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     TSTEP CARD FOUND
!
         CALL close(Trl,1)
         Ngroup = (iflag-1)/3
!
!     MOVE TSTEP STUFF TO BOTTOM OF CORE
!
         nz = nx - iflag + 1
         igroup = nz + 1
         DO i = 2 , iflag
            k = igroup + i - 2
            iz(k) = iz(i)
         ENDDO
         RETURN
!
!     ERROR MESSAGES
!
 80      ip1 = -1
         spag_nextblock_1 = 4
      CASE (4)
         CALL mesage(ip1,file,name)
         RETURN
 100     ip1 = -2
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 120     CALL mesage(-31,itstep,intrl)
         RETURN
      CASE (5)
         ip1 = -8
         file = icrq
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE trd1a
