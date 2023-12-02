!*==plttra.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE plttra
   IMPLICIT NONE
   USE C_BLANK
   USE C_NAMES
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(4) :: a
   INTEGER , DIMENSION(2) :: b , name
   INTEGER , SAVE :: bgpdp , bgpdt , sil , sip
   INTEGER :: buf1 , buf2 , buf3 , buf4 , delta , file , flag , nadd , ndx , ns , nz , s1 , s2
   LOGICAL , SAVE :: leof
   INTEGER , DIMENSION(7) , SAVE :: mcb
   INTEGER , DIMENSION(2) , SAVE :: plt
   EXTERNAL close , fname , fwdrec , korsz , mesage , open , rdtrl , read , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     PLTTRA MODIFIES THE SIL AND BGPDT TABLES FOR THE PURPOSE OF
!     PLOTTING SPECIAL SCALAR GRID POINTS
!
!     INPUT  SIL  BGPDT  LUSET
!     OUTPUT SIP  BGPDP  LUSEP
!
!     SPECIAL SCALAR GRID POINTS
!     BGPDT(I,1)= 1  SIL(I+1)-SIL(I)=1
!     BGPDP(I,1)=-2  SIP(I+1)-SIP(I)=6
!
!     LUSET IS THE VALUE OF SIL(LAST+1) IF IT EXISTED
!     LUSEP IS THE VALUE OF SIP(LAST+1) IF IT EXISTED
!
   !>>>>EQUIVALENCE (a(3),b(1)) , (file,mcb(1))
   DATA bgpdt , sil , bgpdp , sip/101 , 102 , 201 , 202/
   DATA plt/4HPLTT , 4HRA  / , mcb/7*0/
   DATA leof/.FALSE./
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         nadd = 0
         ns = 0
!
!     LOCATE STORAGE AREA FOR FILE BUFFERS
!
         nz = korsz(Z)
         buf1 = nz - Sysbuf + 1
         buf2 = buf1 - Sysbuf
         buf3 = buf2 - Sysbuf
         buf4 = buf3 - Sysbuf
         IF ( buf4<=0 ) CALL mesage(-8,nz,plt)
!
!     READ TRAILER RECORDS OF INPUT FILES AND CHECK COMPATABILITY
!     OPEN AND FOREWARD SPACE LABEL RECORD OF INPUT FILES
!     OPEN AND WRITE LABEL RECORD OF OUTPUT FILES
!
         file = bgpdt
         CALL rdtrl(mcb)
         CALL fname(file,name)
         IF ( file<=0 ) GOTO 40
         CALL open(*40,bgpdt,Z(buf2),Rdrew)
         CALL fwdrec(*80,bgpdt)
!
         file = sil
         CALL rdtrl(mcb)
         CALL fname(file,name)
         IF ( file<=0 ) GOTO 40
         IF ( mcb(3)/=Luset ) THEN
            WRITE (Not,99001) Ufm , Luset , mcb(3)
99001       FORMAT (A23,' 5011, FIRST PARAMETER',I6,' NE TRAILER RECORD ','PARAMETER',I6)
            CALL mesage(-61,0,0)
            RETURN
         ELSE
            CALL open(*40,sil,Z(buf1),Rdrew)
            CALL fwdrec(*80,sil)
!
            file = sip
            CALL fname(sip,a)
            CALL open(*60,sip,Z(buf3),Wrtrew)
            CALL write(sip,a,2,1)
!
            file = bgpdp
            CALL open(*60,bgpdp,Z(buf4),Wrtrew)
            CALL fname(bgpdp,b)
            CALL write(bgpdp,b,2,1)
!
!     READ SIL(I)
!
            file = sil
            CALL read(*80,*100,sil,s1,1,0,flag)
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     READ SIL(I+1)
!
         file = sil
         CALL read(*80,*20,sil,s2,1,0,flag)
         spag_nextblock_1 = 3
      CASE (3)
!
!     READ BGPDT(I,J)
!
         file = bgpdt
         CALL read(*80,*100,bgpdt,a,4,0,flag)
         delta = 0
         ns = ns + 1
!
!     CHECK IF SPECIAL SCALAR GRID POINT
!
         IF ( a(1)>=0 .AND. s2-s1/=6 ) THEN
            IF ( s2-s1/=1 ) THEN
               WRITE (Not,99002) Ufm , ns
99002          FORMAT (A23,' 5012, ENTRY',I6,' OF SIL TABLE INCOMPATIBLE WITH ','NEXT ENTRY')
               CALL mesage(-61,0,0)
               RETURN
            ELSE
!
!     SPECIAL SCALAR GRID POINT
!
               delta = 5
               a(1) = -2
            ENDIF
         ENDIF
         s1 = s1 + nadd
!
!     WRITE SIP AND BGPDP TABLE ENTRIES
!
         CALL write(sip,s1,1,0)
         CALL write(bgpdp,a,4,0)
         nadd = nadd + delta
         IF ( leof ) THEN
            Lusep = Luset + nadd
!
!     CLOSE OUTPUT FILES AND WRITE TRAILER RECORDS
!
            CALL close(sil,Clsrew)
            CALL close(bgpdt,Clsrew)
            CALL close(sip,Clsrew)
            CALL close(bgpdp,Clsrew)
            mcb(1) = bgpdp
            mcb(3) = 0
            CALL wrttrl(mcb)
            mcb(1) = sip
            mcb(3) = Lusep
            CALL wrttrl(mcb)
            RETURN
         ELSE
            s1 = s2
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     SIL(I) IS SIL(LAST)
!
 20      leof = .TRUE.
         s2 = Luset + 1
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
 40      Lusep = Luset
         RETURN
!
!     ERROR DIAGNOSTICS
!
 60      ndx = -1
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 80      ndx = -2
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 100     ndx = -3
         spag_nextblock_1 = 4
      CASE (4)
         CALL mesage(ndx,file,plt)
         CALL mesage(-61,0,0)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE plttra
