!*==gravl1.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gravl1(Nvect,Gvect,Sr1,Iharm)
   IMPLICIT NONE
   USE C_BLANK
   USE C_LOADX
   USE C_SYSTEM
   USE C_ZBLPKX
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nvect
   REAL , DIMENSION(1) :: Gvect
   INTEGER :: Sr1
   INTEGER :: Iharm
!
! Local variable declarations rewritten by SPAG
!
   REAL :: flag
   INTEGER , DIMENSION(7) :: gravt
   INTEGER :: i , icm , il , iloop , in , iout , ipm , ipont , isil , isil1 , isil2 , lcore , nz
   INTEGER , DIMENSION(4) :: igpco
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(3) :: vect
   EXTERNAL basglb , bldpk , bldpkn , close , fndpnt , gopen , gravl3 , korsz , makmcb , mesage , open , read , rewind , skprec ,   &
          & wrttrl , zblpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!
!
!
   DATA name/4HGRAV , 4HL1  /
!
! ----------------------------------------------------------------------
!
   IF ( Iharm==0 ) THEN
      lcore = korsz(Core)
      icm = 1
      nz = lcore
      lcore = lcore - Sysbuf
      CALL gopen(Sr1,Core(lcore+1),1)
      lcore = lcore - Sysbuf
      CALL gopen(Bgpdt,Core(lcore+1),0)
      Old = 0
      lcore = lcore - Sysbuf
      CALL open(*100,Cstm,Core(lcore+1),0)
      icm = 0
      CALL skprec(Cstm,1)
      lcore = lcore - Sysbuf
   ELSE
      CALL gravl3(Nvect,Gvect,Sr1,Iharm)
      RETURN
   ENDIF
 100  CALL gopen(Sil,Core(lcore+1),0)
   isil = 0
   CALL makmcb(gravt,Sr1,Nrowsp,2,1)
   DO iloop = 1 , Nvect
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
            SPAG_Loop_2_1: DO
               CALL read(*200,*120,Sil,isil1,1,0,flag)
               IF ( isil1>=0 ) THEN
                  il = (iloop-1)*3
                  ASSIGN 110 TO iout
                  ipont = 1
                  CALL bldpk(1,1,gravt(1),0,0)
                  EXIT SPAG_Loop_2_1
               ENDIF
            ENDDO SPAG_Loop_2_1
            spag_nextblock_1 = 2
         CASE (2)
            SPAG_Loop_2_2: DO
               CALL read(*200,*120,Sil,isil2,1,0,flag)
               IF ( isil2>=0 ) THEN
                  IF ( isil2-isil1==1 ) EXIT SPAG_Loop_2_2
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO SPAG_Loop_2_2
 110        isil1 = isil2
            ipont = ipont + 1
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         CASE (3)
            CALL fndpnt(igpco(1),ipont)
            DO i = 1 , 3
               in = i + il
               vect(i) = Gvect(in)
            ENDDO
            IF ( igpco(1)/=0 ) CALL basglb(vect(1),vect(1),igpco(2),igpco(1))
            DO i = 1 , 3
               B(1) = vect(i)
               Ii = isil1 - 1 + i
               CALL zblpki
            ENDDO
            GOTO iout
!
!     END SIL
!
 120        ASSIGN 130 TO iout
            IF ( Nrowsp/=isil1 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
 130        CALL rewind(Bgpdt)
            CALL rewind(Sil)
            CALL bldpkn(gravt(1),0,gravt)
            CALL skprec(Sil,1)
            isil = 0
            CALL skprec(Bgpdt,1)
            Old = 0
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
   ENDDO
   CALL close(Bgpdt,1)
   IF ( icm==0 ) CALL close(Cstm,1)
   CALL close(Sil,1)
   CALL close(gravt(1),1)
   CALL wrttrl(gravt)
   RETURN
!
 200  CALL mesage(-3,ipm,name)
!
END SUBROUTINE gravl1
