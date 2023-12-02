!*==gravl1.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gravl1(Nvect,Gvect,Sr1,Iharm)
   USE c_blank
   USE c_loadx
   USE c_system
   USE c_zblpkx
   USE c_zzzzzz
   IMPLICIT NONE
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
      lcore = korsz(core)
      icm = 1
      nz = lcore
      lcore = lcore - sysbuf
      CALL gopen(Sr1,core(lcore+1),1)
      lcore = lcore - sysbuf
      CALL gopen(bgpdt,core(lcore+1),0)
      old = 0
      lcore = lcore - sysbuf
      CALL open(*100,cstm,core(lcore+1),0)
      icm = 0
      CALL skprec(cstm,1)
      lcore = lcore - sysbuf
   ELSE
      CALL gravl3(Nvect,Gvect,Sr1,Iharm)
      RETURN
   ENDIF
 100  CALL gopen(sil,core(lcore+1),0)
   isil = 0
   CALL makmcb(gravt,Sr1,nrowsp,2,1)
   DO iloop = 1 , Nvect
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
            SPAG_Loop_2_1: DO
               CALL read(*200,*120,sil,isil1,1,0,flag)
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
               CALL read(*200,*120,sil,isil2,1,0,flag)
               IF ( isil2>=0 ) THEN
                  IF ( isil2-isil1==1 ) EXIT SPAG_Loop_2_2
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO SPAG_Loop_2_2
 110        isil1 = isil2
            ipont = ipont + 1
            spag_nextblock_1 = 2
         CASE (3)
            CALL fndpnt(igpco(1),ipont)
            DO i = 1 , 3
               in = i + il
               vect(i) = Gvect(in)
            ENDDO
            IF ( igpco(1)/=0 ) CALL basglb(vect(1),vect(1),igpco(2),igpco(1))
            DO i = 1 , 3
               b(1) = vect(i)
               ii = isil1 - 1 + i
               CALL zblpki
            ENDDO
            GOTO iout
!
!     END SIL
!
 120        ASSIGN 130 TO iout
            IF ( nrowsp/=isil1 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
 130        CALL rewind(bgpdt)
            CALL rewind(sil)
            CALL bldpkn(gravt(1),0,gravt)
            CALL skprec(sil,1)
            isil = 0
            CALL skprec(bgpdt,1)
            old = 0
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
   ENDDO
   CALL close(bgpdt,1)
   IF ( icm==0 ) CALL close(cstm,1)
   CALL close(sil,1)
   CALL close(gravt(1),1)
   CALL wrttrl(gravt)
   RETURN
!
 200  CALL mesage(-3,ipm,name)
!
END SUBROUTINE gravl1
