
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
   INTEGER Date(3) , Head(96) , Ibuf , Intra , Ksystm(100) , Norew , Nout , Param(1) , Rd , Rdrew , Rew , Wrt , Wrtrew
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /blank / Param
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew
   COMMON /output/ Head
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm , Uim
   INTEGER Caller
   INTEGER Z(3)
   INTEGER blank , file , fn(2) , i , ibuf1 , ibuf2 , infil , kore , m , mark(3) , nfile , nparam , sub(2) , tab(3,3) , who(2)
   LOGICAL disc
   INTEGER korsz
   LOGICAL tapbit
   !>>>>EQUIVALENCE (Ksystm(1),Ibuf) , (Ksystm(15),Date(1)) , (Ksystm(2),Nout) , (Ksystm(86),Intra) , (tab(2,3),blank)
   DATA tab/4HPLTS , 4HET   , 2 , 4HPLOT , 4H     , 5 , 4HOFP  , 4H     , 3/
   DATA file , nfile , mark/4HINP9 , 23 , 2*65536 , 11111/
   DATA sub/4HTOTA , 4HPE  /
!
   IF ( Intra>=0 .OR. Caller<1 .OR. Caller>3 ) RETURN
   IF ( Caller<=2 .AND. Intra==-2 ) RETURN
   IF ( Caller==3 .AND. Intra==-1 ) RETURN
   who(1) = tab(1,Caller)
   who(2) = tab(2,Caller)
   nparam = tab(3,Caller)
   kore = korsz(Z(1))
   ibuf1 = kore - Ibuf
   ibuf2 = ibuf1 - Ibuf
   kore = ibuf2 - 1
   fn(1) = file
   fn(2) = blank
   disc = .TRUE.
   IF ( tapbit(fn(1)) ) disc = .FALSE.
   IF ( .NOT.disc .OR. Intra>0 ) GOTO 200
!
   CALL open(*500,file,Z(ibuf2),Rdrew)
   DO
      CALL read(*100,*100,file,Z(1),2,0,m)
      CALL skpfil(file,1)
   ENDDO
 100  CALL close(file,Norew)
 200  CALL open(*500,file,Z(ibuf2),Wrt)
   IF ( Intra>=0 ) THEN
      DO i = 1 , 2
         IF ( Intra==i .OR. Intra==3 ) THEN
            file = tab(3,i+1)
            CALL write(file,tab(1,Caller),3,0)
            CALL write(file,Date(1),3,0)
            CALL write(file,Head(1),96,0)
            CALL write(file,Ksystm(1),100,1)
            CALL write(file,mark(1),3,1)
         ENDIF
      ENDDO
      Intra = -Intra
      file = tab(3,Caller)
   ENDIF
   Z(1) = Caller
   Z(2) = nfile
   Z(3) = nparam
   CALL write(file,Z(1),3,1)
   WRITE (Nout,99001) Uim , who , file
99001 FORMAT (A29,', THE FOLLOWING FILES WERE COPIED FROM DMAP ',A4,A2,4H TO ,A4,5H FILE,/)
   DO i = 1 , nfile
      infil = 100 + i
      CALL open(*400,infil,Z(ibuf1),Rdrew)
      Z(1) = infil
      CALL rdtrl(Z(1))
      CALL write(file,Z(1),7,1)
      IF ( Z(1)<=0 ) GOTO 350
 250  CALL read(*350,*300,infil,Z(1),kore,1,m)
      CALL mesage(-8,0,sub)
 300  CALL write(file,Z(1),m,1)
      GOTO 250
 350  CALL close(infil,Rew)
      CALL fname(infil,fn)
      WRITE (Nout,99002) fn
99002 FORMAT (5X,2A4)
 400  CALL write(file,mark(1),3,1)
   ENDDO
   CALL write(file,Param(1),nparam,1)
   CALL write(file,mark(1),3,1)
   IF ( .NOT.disc ) CALL close(file,Norew)
   IF ( disc ) CALL close(file,Rew)
   RETURN
!
 500  CALL mesage(-1,file,sub)
END SUBROUTINE totape