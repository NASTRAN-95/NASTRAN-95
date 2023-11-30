
SUBROUTINE emgcor(Buf)
   IMPLICIT NONE
   LOGICAL Anycon , Error , Heat
   INTEGER Cls , Clsrew , Cstm , Dit , Est , Flags(3) , Geom2 , Icmbar , Icong , Icore , Icstm , Idit , Ihmat , Imat , Jcore ,      &
         & Kflags(3) , Kmbdic(3) , Kmbmat(3) , Ksystm(65) , L38 , Lcong , Lcstm , Lhmat , Lmat , Mpt , Ncong , Ncore , Ncstm ,      &
         & Ndit , Nhmat , Nmat , Nokmb(3) , Outpt , Precis , Rd , Rdrew , Sysbuf , Wrt , Wrtrew , Z(1)
   REAL Dummy(13) , Surfac , Volume
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Nokmb , Dummy , Volume , Surfac
   COMMON /emgfil/ Est , Cstm , Mpt , Dit , Geom2 , Kmbmat , Kmbdic
   COMMON /emgprm/ Icore , Jcore , Ncore , Icstm , Ncstm , Imat , Nmat , Ihmat , Nhmat , Idit , Ndit , Icong , Ncong , Lcong ,      &
                 & Anycon , Flags , Precis , Error , Heat , Icmbar , Lcstm , Lmat , Lhmat , Kflags , L38
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zzzzzz/ Z
   INTEGER Buf(8)
   INTEGER buf1 , buf2 , bufs , eor , i , ibuf , n , name(2) , scr4 , subr(2) , type(3)
!
!     CORE ALLOCATION AND PARAMETER INITIALIZATION FOR MAIN -EMG-
!     PROCESSOR -EMGPRO-.
!
   EQUIVALENCE (Ksystm(1),Sysbuf)
   EQUIVALENCE (Ksystm(2),Outpt)
   DATA type/4HSTIF , 4HMASS , 4HDAMP/
   DATA scr4/304/
   DATA subr/4HEMGC , 4HOR  / , eor/1/
!
   IF ( L38==0 ) WRITE (Outpt,99001) Uim
99001 FORMAT (A29,' 238, TURN DIAG 38 ON FOR ADDITIONAL ELEMENT ','PROCESSING INFORMATION',/)
!
!     DETERMINATION OF FUNCTIONS TO BE PERFORMED AND RESULTANT NUMBER
!     OF BUFFERS NEEDED.
!
   bufs = 1
   DO i = 1 , 3
      Flags(i) = 0
      Kflags(i) = 0
      IF ( Nokmb(i)/=-1 ) THEN
         Flags(i) = -1
         bufs = bufs + 2
      ENDIF
   ENDDO
   IF ( Volume>0.0 .OR. Surfac>0.0 ) bufs = bufs + 1
!
!     ALLOCATE BUFFERS
!
   n = Ncore
   DO i = 1 , bufs
      Buf(i) = n - Sysbuf - 2
      n = Buf(i)
   ENDDO
   Ncore = n - 1
   IF ( Ncore<Jcore ) CALL mesage(-8,Jcore-Ncore,subr)
!
!  OPEN REQUIRED DATA BLOCKS.
!
   buf1 = Buf(1)
   CALL open(*100,Est,Z(buf1),Rdrew)
   CALL skprec(Est,1)
   ibuf = 1
!
!     K, M, OR B MATRIX DATA BLOCKS
!
   DO i = 1 , 3
      IF ( Flags(i)/=0 ) THEN
         buf1 = Buf(ibuf+1)
         buf2 = Buf(ibuf+2)
         CALL open(*50,Kmbmat(i),Z(buf1),Wrtrew)
         CALL open(*50,Kmbdic(i),Z(buf2),Wrtrew)
         CALL fname(Kmbmat(i),name)
         CALL write(Kmbmat(i),name,2,eor)
         CALL fname(Kmbdic(i),name)
         CALL write(Kmbdic(i),name,2,eor)
         ibuf = ibuf + 2
         Kflags(i) = 1
      ENDIF
      CYCLE
!
!     FILE REQUIRED IS MISSING
!
 50   Flags(i) = 0
      CALL page2(2)
      WRITE (Outpt,99002) Uwm , Kmbmat(i) , Kmbdic(i) , type(i)
99002 FORMAT (A25,' 3103, EMGCOR OF EMG MODULE FINDS EITHER OF DATA ','BLOCKS ',I4,4H OR ,I4,' ABSENT AND THUS,',/5X,A4,            &
             &' MATRIX WILL NOT BE FORMED.')
   ENDDO
!
!     IF VOLUME OR SURFACE COMPUTATION IS REQUESTED BY USER FOR THE 2-D
!     AND 3-D ELEMENTS, OPEN SCR4 FILE. (ONLY TO BE CLOSED BY EMGFIN)
!
   IF ( Volume>0.0 .OR. Surfac>0.0 ) THEN
      ibuf = ibuf + 1
      buf1 = Buf(ibuf)
      CALL open(*200,scr4,Z(buf1),Wrtrew)
   ENDIF
!
!     ALL FILES READY TO GO.
!
   Ncore = Buf(ibuf) - 1
   RETURN
!
!     EST MISSING
!
 100  CALL page2(2)
   WRITE (Outpt,99003) Swm , Est
99003 FORMAT (A27,' 3104, EMGCOR FINDS EST (ASSUMED DATA BLOCK',I5,') MISSING.  EMG MODULE COMPUTATIONS LIMITED.')
   Flags(1) = 0
   Flags(2) = 0
   Flags(3) = 0
   RETURN
!
 200  CALL mesage(-1,scr4,subr)
END SUBROUTINE emgcor
