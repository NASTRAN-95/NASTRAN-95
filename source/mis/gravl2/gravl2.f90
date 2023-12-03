!*==gravl2.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gravl2(Nvect,Fild,Pg)
   USE c_blank
   USE c_loadx
   USE c_system
   USE c_zblpkx
   USE c_zntpkx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nvect
   INTEGER :: Fild
   INTEGER , DIMENSION(7) :: Pg
!
! Local variable declarations rewritten by SPAG
!
   REAL :: flag
   INTEGER :: ibuf , iloop , iout , ipm , isil , isil1 , isil2 , lcore , nz
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL bldpk , bldpkn , close , gopen , intpk , korsz , mesage , open , read , rewind , skpfil , skprec , wrttrl , zblpki ,    &
          & zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!
!
   DATA name/4HGRAV , 4HL2  /
!
! ----------------------------------------------------------------------
!
   lcore = korsz(core)
   nz = lcore
   lcore = lcore - sysbuf
   CALL open(*100,Pg(1),core(lcore+1),0)
   CALL skpfil(Pg,1)
   CALL skpfil(Pg,-1)
   CALL close(Pg,2)
   CALL open(*100,Pg(1),core(lcore+1),3)
   lcore = lcore - sysbuf
   CALL gopen(Fild,core(lcore+1),0)
   lcore = lcore - sysbuf
   CALL gopen(sil,core(lcore+1),0)
   ibuf = lcore
   isil = 0
   DO iloop = 1 , Nvect
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
            SPAG_Loop_2_1: DO
               CALL read(*200,*20,sil,isil1,1,0,flag)
               IF ( isil1>=0 ) THEN
                  ASSIGN 10 TO iout
                  CALL bldpk(1,1,Pg(1),0,0)
                  CALL intpk(*30,Fild,0,1,0)
                  EXIT SPAG_Loop_2_1
               ENDIF
            ENDDO SPAG_Loop_2_1
            spag_nextblock_1 = 2
         CASE (2)
            SPAG_Loop_2_2: DO
               CALL read(*200,*20,sil,isil2,1,0,flag)
               IF ( isil2>=0 ) THEN
                  IF ( isil2-isil1/=1 ) isil1 = 999999
                  EXIT SPAG_Loop_2_2
               ENDIF
            ENDDO SPAG_Loop_2_2
            spag_nextblock_1 = 3
         CASE (3)
            GOTO iout
 10         IF ( ieol/=0 ) GOTO 30
            CALL zntpki
            IF ( ll<isil1 ) THEN
               b(1) = a(1)
               ii = ll
               CALL zblpki
            ELSEIF ( ll/=isil1 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
 20         ASSIGN 30 TO iout
            IF ( nrowsp/=isil1 ) THEN
               isil1 = 999999
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
 30         CALL rewind(sil)
            CALL bldpkn(Pg(1),0,Pg)
            CALL skprec(sil,1)
            isil = 0
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
   ENDDO
   CALL close(sil,1)
   CALL close(Fild,1)
   CALL wrttrl(Pg)
   CALL close(Pg,1)
   RETURN
!
 100  ipm = Pg(1)
   CALL mesage(-1,ipm,name)
!
 200  CALL mesage(-3,sil,name)
!
END SUBROUTINE gravl2