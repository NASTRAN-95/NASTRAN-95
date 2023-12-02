!*==totape.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE totape(Caller,Z)
!
!     THIS ROUTINE IS CALLED ONLY BY DPLTST (CALLER=1), DPLOT (CALLER=2)
!     AND/OR OFP (CALLER=3) TO COPY NUMBERS OF GINO INPUT FILES TO A
!     SAVE FILE, INP9, FOR NEXT INTERACTIVE NASTRAN RUN (INTRA.LT.0)
!     THE SAVE FILE CAN BE A TAPE OR DISC.
!
!     WRITTEN BY G.CHAN/SPERRY     NOV. 1985
!
!     FILE STRUCTURE IN SAVE TAPE
!
!     RECORD NO.   CONTENT
!     ----------   -----------------------------------------------
!        1           6-WORDS (3 CALLER ID WORDS AND 3 DATE WORDS)
!                   96-WORD HEADING
!                  100-SYSTEM WORDS
!        2         MARK
!        3         CALLER ID, NO. OF FILES, NO. OF PARAMETERS
!        4         7-WORD TRAILER OF FIRST GINO INPUT FILE
!      5 TO N      FIRST GINO INPUT FILE (IF FILE IS NOT PURGED)
!       N+1        MARK
!       N+2        7-WORD TRAILER OF SECOND GINO INPUT FILE
!    N+2 TO M      SECOND GINO INPUT FILE (IF FILE IS NOT PURGED)
!       M+1        MARK
!    M+2 TO ..R    REPEAT FOR ADDITION FILES, TRAILER, AND MARK
!       R+1        PARAMETERS IN /BLANK/ OF CURRENT CALLER
!       R+2        MARK
!       R+3        NASTRAN EOF MARK
!    R+4 TO LAST   REPEAT 3 TO R+3 AS MANY TIMES AS NEEDED FROM THE
!                  SAME OR A DIFFERENT CALLER AT DIFFERENT TIME
!     LAST+1       SYSTEM EOF MARK
!
!     THE INTERACTIVE FLAG, INTRA, IN /SYSTEM/ WAS SET BY XCSA TO
!         1 FOR PLOT ONLY,
!         2 FOR OUTPUT PRINT ONLY
!      OR 3 FOR BOTH
!
   IMPLICIT NONE
   USE C_BLANK
   USE C_NAMES
   USE C_OUTPUT
   USE C_SYSTEM
   USE C_XMSSG
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Caller
   INTEGER , DIMENSION(3) :: Z
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: blank , i , ibuf , ibuf1 , ibuf2 , infil , intra , kore , m , nout , nparam
   INTEGER , DIMENSION(3) :: date
   LOGICAL :: disc
   INTEGER , SAVE :: file , nfile
   INTEGER , DIMENSION(2) :: fn , who
   INTEGER , DIMENSION(3) , SAVE :: mark
   INTEGER , DIMENSION(2) , SAVE :: sub
   INTEGER , DIMENSION(3,3) , SAVE :: tab
   EXTERNAL close , fname , korsz , mesage , open , rdtrl , read , skpfil , tapbit , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   !>>>>EQUIVALENCE (Ksystm(1),Ibuf) , (Ksystm(15),Date(1)) , (Ksystm(2),Nout) , (Ksystm(86),Intra) , (tab(2,3),blank)
   DATA tab/4HPLTS , 4HET   , 2 , 4HPLOT , 4H     , 5 , 4HOFP  , 4H     , 3/
   DATA file , nfile , mark/4HINP9 , 23 , 2*65536 , 11111/
   DATA sub/4HTOTA , 4HPE  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         IF ( intra>=0 .OR. Caller<1 .OR. Caller>3 ) RETURN
         IF ( Caller<=2 .AND. intra==-2 ) RETURN
         IF ( Caller==3 .AND. intra==-1 ) RETURN
         who(1) = tab(1,Caller)
         who(2) = tab(2,Caller)
         nparam = tab(3,Caller)
         kore = korsz(Z(1))
         ibuf1 = kore - ibuf
         ibuf2 = ibuf1 - ibuf
         kore = ibuf2 - 1
         fn(1) = file
         fn(2) = blank
         disc = .TRUE.
         IF ( tapbit(fn(1)) ) disc = .FALSE.
         IF ( .NOT.disc .OR. intra>0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
         CALL open(*40,file,Z(ibuf2),Rdrew)
         DO
            CALL read(*20,*20,file,Z(1),2,0,m)
            CALL skpfil(file,1)
         ENDDO
 20      CALL close(file,Norew)
         spag_nextblock_1 = 2
      CASE (2)
         CALL open(*40,file,Z(ibuf2),Wrt)
         IF ( intra>=0 ) THEN
            DO i = 1 , 2
               IF ( intra==i .OR. intra==3 ) THEN
                  file = tab(3,i+1)
                  CALL write(file,tab(1,Caller),3,0)
                  CALL write(file,date(1),3,0)
                  CALL write(file,Head(1),96,0)
                  CALL write(file,Ksystm(1),100,1)
                  CALL write(file,mark(1),3,1)
               ENDIF
            ENDDO
            intra = -intra
            file = tab(3,Caller)
         ENDIF
         Z(1) = Caller
         Z(2) = nfile
         Z(3) = nparam
         CALL write(file,Z(1),3,1)
         WRITE (nout,99001) Uim , who , file
99001    FORMAT (A29,', THE FOLLOWING FILES WERE COPIED FROM DMAP ',A4,A2,4H TO ,A4,5H FILE,/)
         DO i = 1 , nfile
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  infil = 100 + i
                  CALL open(*26,infil,Z(ibuf1),Rdrew)
                  Z(1) = infil
                  CALL rdtrl(Z(1))
                  CALL write(file,Z(1),7,1)
                  IF ( Z(1)<=0 ) GOTO 24
                  spag_nextblock_2 = 2
               CASE (2)
                  CALL read(*24,*22,infil,Z(1),kore,1,m)
                  CALL mesage(-8,0,sub)
 22               CALL write(file,Z(1),m,1)
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
 24               CALL close(infil,Rew)
                  CALL fname(infil,fn)
                  WRITE (nout,99002) fn
99002             FORMAT (5X,2A4)
 26               CALL write(file,mark(1),3,1)
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         CALL write(file,Param(1),nparam,1)
         CALL write(file,mark(1),3,1)
         IF ( .NOT.disc ) CALL close(file,Norew)
         IF ( disc ) CALL close(file,Rew)
         RETURN
!
 40      CALL mesage(-1,file,sub)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE totape
