!*==emgcor.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE emgcor(Buf)
   USE c_blank
   USE c_emgfil
   USE c_emgprm
   USE c_names
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(8) :: Buf
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buf1 , buf2 , bufs , i , ibuf , n , outpt , sysbuf
   INTEGER , SAVE :: eor , scr4
   INTEGER , DIMENSION(2) :: name
   INTEGER , DIMENSION(2) , SAVE :: subr
   INTEGER , DIMENSION(3) , SAVE :: type
   EXTERNAL fname , mesage , open , page2 , skprec , write
!
! End of declarations rewritten by SPAG
!
!
!     CORE ALLOCATION AND PARAMETER INITIALIZATION FOR MAIN -EMG-
!     PROCESSOR -EMGPRO-.
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf)
   !>>>>EQUIVALENCE (Ksystm(2),Outpt)
   DATA type/4HSTIF , 4HMASS , 4HDAMP/
   DATA scr4/304/
   DATA subr/4HEMGC , 4HOR  / , eor/1/
!
   IF ( l38==0 ) WRITE (outpt,99001) uim
99001 FORMAT (A29,' 238, TURN DIAG 38 ON FOR ADDITIONAL ELEMENT ','PROCESSING INFORMATION',/)
!
!     DETERMINATION OF FUNCTIONS TO BE PERFORMED AND RESULTANT NUMBER
!     OF BUFFERS NEEDED.
!
   bufs = 1
   DO i = 1 , 3
      flags(i) = 0
      kflags(i) = 0
      IF ( nokmb(i)/=-1 ) THEN
         flags(i) = -1
         bufs = bufs + 2
      ENDIF
   ENDDO
   IF ( volume>0.0 .OR. surfac>0.0 ) bufs = bufs + 1
!
!     ALLOCATE BUFFERS
!
   n = ncore
   DO i = 1 , bufs
      Buf(i) = n - sysbuf - 2
      n = Buf(i)
   ENDDO
   ncore = n - 1
   IF ( ncore<jcore ) CALL mesage(-8,jcore-ncore,subr)
!
!  OPEN REQUIRED DATA BLOCKS.
!
   buf1 = Buf(1)
   CALL open(*100,est,z(buf1),rdrew)
   CALL skprec(est,1)
   ibuf = 1
!
!     K, M, OR B MATRIX DATA BLOCKS
!
   DO i = 1 , 3
      IF ( flags(i)/=0 ) THEN
         buf1 = Buf(ibuf+1)
         buf2 = Buf(ibuf+2)
         CALL open(*50,kmbmat(i),z(buf1),wrtrew)
         CALL open(*50,kmbdic(i),z(buf2),wrtrew)
         CALL fname(kmbmat(i),name)
         CALL write(kmbmat(i),name,2,eor)
         CALL fname(kmbdic(i),name)
         CALL write(kmbdic(i),name,2,eor)
         ibuf = ibuf + 2
         kflags(i) = 1
      ENDIF
      CYCLE
!
!     FILE REQUIRED IS MISSING
!
 50   flags(i) = 0
      CALL page2(2)
      WRITE (outpt,99002) uwm , kmbmat(i) , kmbdic(i) , type(i)
99002 FORMAT (A25,' 3103, EMGCOR OF EMG MODULE FINDS EITHER OF DATA ','BLOCKS ',I4,4H OR ,I4,' ABSENT AND THUS,',/5X,A4,            &
             &' MATRIX WILL NOT BE FORMED.')
   ENDDO
!
!     IF VOLUME OR SURFACE COMPUTATION IS REQUESTED BY USER FOR THE 2-D
!     AND 3-D ELEMENTS, OPEN SCR4 FILE. (ONLY TO BE CLOSED BY EMGFIN)
!
   IF ( volume>0.0 .OR. surfac>0.0 ) THEN
      ibuf = ibuf + 1
      buf1 = Buf(ibuf)
      CALL open(*200,scr4,z(buf1),wrtrew)
   ENDIF
!
!     ALL FILES READY TO GO.
!
   ncore = Buf(ibuf) - 1
   RETURN
!
!     EST MISSING
!
 100  CALL page2(2)
   WRITE (outpt,99003) swm , est
99003 FORMAT (A27,' 3104, EMGCOR FINDS EST (ASSUMED DATA BLOCK',I5,') MISSING.  EMG MODULE COMPUTATIONS LIMITED.')
   flags(1) = 0
   flags(2) = 0
   flags(3) = 0
   RETURN
!
 200  CALL mesage(-1,scr4,subr)
END SUBROUTINE emgcor
