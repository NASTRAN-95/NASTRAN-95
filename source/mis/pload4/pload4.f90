!*==pload4.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE pload4(Ibuf5,Ido,Jopen)
   USE c_gpta1
   USE c_loadx
   USE c_pindex
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ibuf5
   INTEGER :: Ido
   INTEGER :: Jopen
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: allin
   LOGICAL , SAVE :: debug
   INTEGER :: file , i , ib , ido11 , ieltyp , imhere , j , jsave , lcore , nwords , q4 , t3
   REAL :: flag
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , SAVE :: quad4 , tria3
   EXTERNAL bckrec , close , fwdrec , mesage , open , plod4d , plod4s , read , t3pl4d , t3pl4s
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     TO GENERATE PLOAD4 PRESSURE LOAD FOR QUAD4 AND TRIA3 ELEMENTS.
!
!     BOTH ELEMENT TYPES MAY BE PRESENT, OR ONLY ONE OF THE TWO IS
!     PRESENT.
!
!     THIS ROUTINE IS CALLED ONLY BY EXTERN IN SSG1 MODULE, LINK5
!
!     THIS ROUTINE  CALLS PLOD4D OR PLOD4S TO COMPUTE LOAD FOR QUAD4
!     ELEMENTS, AND CALLS T3PL4D OR T3PL4S TO COMPUTE LOAD FOR TRIA3
!
!     IN OVERLAY TREE, THIS ROUTINE SHOULD BE IN PARALLELED WITH FPONT
!     ROUTINE, AND FOLLOWED BY PLOD4D/S AND T3PL4D/S. I.E.
!
!                   ( FPONT
!            EXTERN (        ( PLOD4D  (/ZZSSA1/
!                   ( PLOAD4 ( PLOD4S
!                            ( T3PL4D
!                            ( T3PL4S
!
   !>>>>EQUIVALENCE (Core(1),Iz(1))
   DATA quad4 , tria3 , name/64 , 83 , 4HPLOA , 4HD4  /
   DATA debug/.FALSE./
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
!     T3 AND Q4 KEEP TRACK OF THE PRESENCE OF THE CTRIA3 AND CQUAD4
!     ELEMENTS
!
         t3 = 0
         q4 = 0
         lcore = Ibuf5 - ibuf
         ido11 = Ido*11
         allin = .FALSE.
         IF ( ido11<=lcore ) THEN
            IF ( debug ) WRITE (nout,99001)
99001       FORMAT (/,' * PLOAD4 IS CALLED FOR ONE LOAD CASE')
!
!     OPEN CORE IS BIG ENOUGH TO HOLD ALL PLOAD4 DATA.
!     READ THEM ALL INTO CORE
!     (BAD NEWS - OPEN CORE AT THIS TIME IS NOT AVAILABLE)
!
            IF ( allin ) THEN
!
               allin = .TRUE.
               file = slt
               imhere = 350
               CALL read(*80,*100,slt,core,ido11,0,flag)
            ENDIF
         ENDIF
!
!     OPEN CORE NOT LARGE ENOUGH TO HOLD ALL PLOAD4 DATA
!
         IF ( Jopen/=1 ) THEN
            Jopen = 1
            file = est
            CALL open(*60,est,core(Ibuf5),0)
            CALL fwdrec(*80,est)
            file = est
            CALL read(*20,*40,est,ieltyp,1,0,flag)
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         DO
            IF ( ieltyp==quad4 ) THEN
!
               IF ( q4<1 ) THEN
                  q4 = 1
                  IF ( debug ) WRITE (nout,99002) t3
99002             FORMAT (/,'   QUAD4 ELEM FOUND. SETTING Q4 TO 1.  T3 =',I3)
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( ieltyp==tria3 ) THEN
               IF ( t3/=1 ) THEN
                  t3 = 1
                  IF ( debug ) WRITE (nout,99003) q4
99003             FORMAT (/,'   TRIA3 ELEM FOUND. SETTING T3 TO 1.  Q4 =',I3)
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
            CALL fwdrec(*20,est)
            CALL read(*20,*40,est,ieltyp,1,0,flag)
         ENDDO
 20      IF ( t3+q4/=0 ) GOTO 40
         WRITE (nout,99004) ufm
99004    FORMAT (A23,', PLOAD4 PRESSURE LOAD IS USED WITHOUT THE PRESENCE',' OF QUAD4 OR TRIA3 ELEMENT')
         imhere = 435
         GOTO 80
      CASE (3)
         j = incr*(ieltyp-1)
         nwords = ielem(j+12)
         iest(1) = 0
!
         file = slt
         ib = 0
         imhere = 550
         DO j = 1 , Ido
            IF ( allin ) THEN
               DO i = 1 , 11
                  islt(i) = iz(i+ib)
               ENDDO
               ib = ib + 11
            ELSE
               jsave = j
               IF ( j/=1 .OR. t3+q4<2 ) CALL read(*80,*100,slt,islt,11,0,flag)
            ENDIF
            SPAG_Loop_2_1: DO
               IF ( islt(1)<iest(1) ) THEN
               ELSEIF ( islt(1)==iest(1) ) THEN
!
                  IF ( ieltyp==tria3 ) THEN
!
!     PLOAD4 FOR TRIA3 ELEMENT
!     SET ISLT(1) TO NEGATIVE FOR PLOAD4/TRIA3 COMPUTATION
!
                     IF ( debug ) WRITE (nout,99005) iest(1)
99005                FORMAT (' ==> PROCESS PLOAD4 FOR TRIA3 ELEM',I8)
                     islt(1) = -iabs(islt(1))
                     IF ( iprec==2 ) THEN
                        CALL t3pl4d
                     ELSE
                        CALL t3pl4s
                     ENDIF
                  ELSE
!
!     PLOAD4 FOR QUAD4 ELEMENT
!
                     IF ( debug ) WRITE (nout,99006) iest(1)
99006                FORMAT (' ==> PROCESS PLOAD4 FOR QUAD ELEM',I8)
                     IF ( iprec==2 ) THEN
                        CALL plod4d
                     ELSE
                        CALL plod4s
                     ENDIF
                  ENDIF
               ELSE
                  CALL read(*40,*40,est,iest,nwords,0,flag)
                  CYCLE
               ENDIF
               EXIT SPAG_Loop_2_1
            ENDDO SPAG_Loop_2_1
!
         ENDDO
!
 40      IF ( t3+q4>=2 ) THEN
!
            IF ( Jopen==1 ) CALL close(est,1)
            Jopen = 0
            IF ( .NOT.(allin .OR. jsave>=Ido) ) THEN
               imhere = 590
               j = (Ido-jsave)*11
               CALL read(*120,*120,slt,0,-j,0,flag)
            ENDIF
         ELSE
!
!     JUST FINISHED EITHER QUAD4 OR TRIA3 ELEMENT. BACKSPACE EST FILE,
!     AND BACKSPACE SLT FILE IF SLT DATA ARE NOT ALREADY IN CORE.
!     REPEAT PLOAD4 (LOAD TYPE 25) COMPUTAION FOR THE OTHER ELEMENT
!     (TRIA3 OR QUAD4) WHICH WE HAVE NOT YET PROCESSED IN THE FIRST
!     PASS. MUST STEP OVER OTHER LOADS THAT MIGHT BE PRESENT
!
            CALL bckrec(est)
            q4 = q4 + 1
            jsave = 0
            IF ( allin ) THEN
               CALL read(*20,*40,est,ieltyp,1,0,flag)
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ELSE
!
               CALL bckrec(slt)
               imhere = 570
               DO
                  CALL read(*80,*100,slt,i,1,0,flag)
                  IF ( i==25 ) THEN
                     imhere = 573
                     CALL read(*80,*100,slt,i,1,0,flag)
                     IF ( i==Ido ) THEN
                        imhere = 575
                        CALL read(*80,*100,slt,islt,6,0,flag)
                        IF ( islt(6)==-1 ) THEN
                           imhere = 577
                           CALL read(*80,*100,slt,islt(7),5,0,flag)
                           IF ( islt(7)==0 ) THEN
                              jsave = 1
                              CALL read(*20,*40,est,ieltyp,1,0,flag)
                              spag_nextblock_1 = 2
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         RETURN
!
 60      j = -1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 80      j = -2
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 100     j = -3
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 120     j = 1
         spag_nextblock_1 = 5
      CASE (5)
         WRITE (nout,99007) imhere , t3 , q4 , Ido , jsave
99007    FORMAT ('   IMHERE =',I5,'   T3,Q4 =',2I3,'   IDO,JSAVE =',2I5)
         CALL mesage(j,file,name(1))
         spag_nextblock_1 = 4
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE pload4
