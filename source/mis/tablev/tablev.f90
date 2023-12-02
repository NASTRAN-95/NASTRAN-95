!*==tablev.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE tablev(In,Ll,Trl,Name,P4,Ibuf,Z5) !HIDESTARS (*,In,Ll,Trl,Name,P4,Ibuf,Z5)
   USE c_system
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: In
   INTEGER :: Ll
   INTEGER , DIMENSION(7) :: Trl
   INTEGER , DIMENSION(3) :: Name
   INTEGER :: P4
   INTEGER :: Ibuf
   CHARACTER(5) , DIMENSION(1) :: Z5
!
! Local variable declarations rewritten by SPAG
!
   CHARACTER(1) , SAVE :: b1 , d1 , f1 , i1 , r1
   LOGICAL , SAVE :: debug
   REAL(REAL64) :: dz
   INTEGER , SAVE :: end , tble
   CHARACTER(5) , SAVE :: end5
   INTEGER , DIMENSION(2) , SAVE :: fu , sub
   INTEGER :: fuf , j , k , kore , kore9 , kount , l , lb , ln , out
   INTEGER , DIMENSION(2) :: namex
   REAL , DIMENSION(1) :: rz
   CHARACTER(1) :: z1
   CHARACTER(10) :: z10
   CHARACTER(15) :: z15
   REAL , DIMENSION(2) :: z4
   CHARACTER(5) :: z5l
   EXTERNAL close , fname , mesage , open , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     TABLE-V IS CALLED ONLY BY INPUT5 TO GENERATE A GINO TABLE
!     DATA BLOCK IN 'OUT' FROM AN INPUT FILE 'IN' - A REVERSE PROCESS
!     OF TABLE-5.
!     THE INPUT FILE WAS FORTRAN WRITTEN, FORMATTED OR UNFORMATTED
!
!     IN     = INPUT FILE, INTEGERS
!     LL     = (200+LL) IS THE OUTPUT FILE, INTEGER
!     TRL    = AN ARRAY OF 7 WORDS FOR TRAILER
!     NAME   = ORIGINAL FILE NAME FROM INPUT FILE, 2 BCD WORDS, PLUS 1
!     P4     = 0, INPUT FILE WAS WRITTEN UNFORMATTED, BINARY, INTEGER
!            = 1, INPUT FILE WAS WRITTEN FORMATTED, ASCII, INTEGER
!     IBUF   = OPEN CORE AND GINO BUFFER POINTER, INTEGER
!
   !>>>>EQUIVALENCE (z1,z5l) , (Z(1),Rz(1)) , (dz,z4(1))
   DATA i1 , r1 , b1 , d1 , f1/'I' , 'R' , '/' , 'D' , 'X'/
   DATA fu , end , end5/2H   , 2HUN , 4H*END , ' *END'/
   DATA sub , tble/4HTABL , 4HEV   , 4HTBLE/
   DATA debug/.FALSE./
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         IF ( debug ) WRITE (nout,99001)
99001    FORMAT (///,' *** IN TABLE-V, DEBUG ***')
         kore = Ibuf - 1
         kore9 = (kore/9)*9
         out = 200 + Ll
         Ll = Ll + 1
         kount = 0
!
!     OPEN GINO OUTPUT FILE AND WRITE A FILE HEADER
!
         CALL open(*60,out,z(Ibuf),1)
         CALL fname(out,namex)
         CALL write(out,namex,2,1)
         IF ( debug ) WRITE (nout,99002) namex
99002    FORMAT (/5X,'GENERATING...',2A4,/)
         Name(3) = tble
         IF ( P4==1 ) THEN
            SPAG_Loop_1_1: DO
!
!     FORMATTED READ
!
               READ (In,99003,ERR=40,END=20) ln , (Z5(j),j=1,ln)
99003          FORMAT (I10,24A5,/,(26A5))
               IF ( ln>kore ) THEN
                  CALL mesage(8,0,sub)
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  IF ( ln==1 .AND. Z5(1)==end5 ) EXIT SPAG_Loop_1_1
                  IF ( ln<=-1 ) EXIT SPAG_Loop_1_1
                  lb = (ln*5)/4 + 1
                  k = 0
                  l = 1
                  DO WHILE ( l<=ln )
                     k = k + 1
                     z5l = Z5(l)
                     IF ( z1==i1 ) THEN
                        WRITE (z10,99012) Z5(l) , Z5(l+1)
                        READ (z10,99004) z(lb+k)
99004                   FORMAT (1X,I9)
                        l = l + 2
                     ELSEIF ( z1==r1 ) THEN
!
!     REAL, SINGLE PRECISION
!
                        WRITE (z15,99012) Z5(l) , Z5(l+1) , Z5(l+2)
                        READ (z15,99005) rz(lb+k)
99005                   FORMAT (1X,E14.7)
                        l = l + 3
                     ELSE
                        IF ( z1==b1 ) THEN
!
!     BCD
!
                           READ (z5l,99006) z(lb+k)
99006                      FORMAT (1X,A4)
                        ELSEIF ( z1/=f1 ) THEN
                           IF ( z1==d1 ) THEN
!
!     REAL, DOUBLE PRECISION
!
                              WRITE (z15,99012) Z5(l) , Z5(l+1) , Z5(l+2)
                              READ (z15,99007) dz
99007                         FORMAT (1X,D14.7)
                              rz(lb+k) = z4(1)
                              rz(lb+k+1) = z4(2)
                              k = k + 1
                              l = l + 3
                              CYCLE
                           ELSE
                              WRITE (nout,99008) z5l
99008                         FORMAT (/,' SYSTEM ERROR/TABLEV @65  Z5L=',A5)
                              GOTO 40
                           ENDIF
                        ENDIF
!
!     FILLER
!
                        l = l + 1
                     ENDIF
                  ENDDO
!
                  IF ( k>0 ) THEN
                     CALL write(out,z(lb+1),k,1)
                     kount = kount + 1
                  ENDIF
               ENDIF
            ENDDO SPAG_Loop_1_1
         ELSE
            SPAG_Loop_1_2: DO
!
!     UNFORMATED READ
!
               READ (In,ERR=40,END=20) ln , (z(j),j=1,ln)
               IF ( ln>kore ) THEN
                  CALL mesage(8,0,sub)
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  IF ( ln==1 .AND. z(1)==end ) EXIT SPAG_Loop_1_2
                  CALL write(out,z(1),ln,1)
                  kount = kount + 1
               ENDIF
            ENDDO SPAG_Loop_1_2
         ENDIF
!
!     ALL DONE.
!     CLOSE OUTPUT GINO FILE AND WRITE TRAILER
!
 20      CALL close(out,1)
         IF ( debug ) WRITE (nout,99009) Trl(2) , kount
99009    FORMAT (/,' DEBUG ECHO - OLD AND NEW COLUMN COUNTS =',2I5)
         Trl(1) = out
         Trl(2) = kount
         CALL wrttrl(Trl)
         fuf = fu(1)
         IF ( P4==0 ) fuf = fu(2)
         WRITE (nout,99010) fuf , namex
99010    FORMAT (/5X,'DATA TRANSFERED SUCCESSFULLY FROM ',A2,'FORMATTED ','TAPE TO GINO OUTPUT FILE ',2A4)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     ERROR
!
 40      CALL close(out,1)
         WRITE (nout,99011) namex
99011    FORMAT (//5X,'ERROR IN READING INPUT TAPE IN TABLEV. NO ',2A4,/5X,'FILE GENERATED')
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 60      CALL mesage(1,out,sub)
         spag_nextblock_1 = 2
      CASE (2)
!
         RETURN 1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
!     INTEGER
!
99012 FORMAT (3A5)
END SUBROUTINE tablev
