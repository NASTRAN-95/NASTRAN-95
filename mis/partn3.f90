
SUBROUTINE partn3(File,Size,Ones,Iz,Nz,Here,Buf,Core)
!DIR$ INTEGER=64
!
!     CDIR$ IS CRAY COMPILER DIRECTIVE. 64-BIT INTEGER IS USED LOCALLY
!     DO LOOP 10 MAY NOT WORK PROPERLY WITH 48 BIT INTEGER
!
!     PARTN3 CALLED BY PARTN1 AND MERGE1 (VIA PARTN2) BUILDS A BIT
!     STRING AT Z(IZ) THROUGH Z(NZ) AND CONTAINING ONE-BITS ONLY IN
!     THE RESPECTIVE POSITIONS OCCUPIED BY NON-ZERO ELEMENTS IN THE
!     COLUMN VECTOR WHICH IS STORED ON FILE.
!
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Cls , Clsrew , Cpcol , Elem(4) , Eol , Form(4) , Ireqcl , Nbpw , Outpt , Rd , Rdrew , Row , Rpcol , Sym , Sysbuf , Type ,&
         & Wrt , Wrtrew , Xxx(37) , Z(1)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Sym , Type , Form , Cpcol , Rpcol , Ireqcl
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Cls
   COMMON /system/ Sysbuf , Outpt , Xxx , Nbpw
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zntpkx/ Elem , Row , Eol
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Core , File , Iz , Nz , Ones , Size
   LOGICAL Here
   INTEGER Buf(4)
!
! Local variable declarations
!
   INTEGER bit(64) , i , j , k , mcb(7) , subr(2) , trl(6) , zbit , zword
   INTEGER lshift , orf , rshift
   LOGICAL pass
   EXTERNAL lshift , orf , rshift
!
! End of declarations
!
   EQUIVALENCE (trl(1),mcb(2))
   DATA subr/4HPART , 4HN3  /
   DATA pass/.FALSE./
!
!     SET UP TABLE OF BITS ON FIRST PASS THROUGH THIS ROUTINE.
!
   IF ( .NOT.(pass) ) THEN
      pass = .TRUE.
      j = Nbpw - 1
      k = lshift(1,j)
      DO i = 1 , Nbpw
         bit(i) = k
         k = rshift(k,1)
      ENDDO
   ENDIF
!
   CALL open(*200,File,Buf,Rdrew)
   Here = .TRUE.
   mcb(1) = File
   CALL rdtrl(mcb)
!
!     NUMBER OF WORDS IN COLUMN INCLUDING ZEROS
!
   Size = trl(2)
   IF ( Ireqcl==0 ) THEN
      Ireqcl = 1
   ELSEIF ( Ireqcl<=0 .OR. Ireqcl>trl(1) ) THEN
      IF ( trl(1)>0 ) THEN
         WRITE (Outpt,99001) Swm , File , trl(1) , Ireqcl
99001    FORMAT (A27,' 2173, PARTITIONING VECTOR FILE',I5,' CONTAINS',I10,' COLUMNS.',/5X,' THE FIRST COLUMN WILL BE USED, NOT THE',&
                &' REQUESTED COLUMN',I10)
      ENDIF
      Ireqcl = 1
   ENDIF
   CALL skprec(File,Ireqcl)
   IF ( trl(4)/=1 .AND. trl(4)/=2 ) THEN
      WRITE (Outpt,99002) Swm , File
99002 FORMAT (A27,' 2174, PARTITIONING VECTOR ON FILE',I5,' IS NOT REAL-SINGLE OR REAL-DOUBLE PRECISION.')
   ENDIF
!
!     ZERO THE BIT STRING
!
   Nz = Iz + (Size-1)/Nbpw
   IF ( Nz>Core ) CALL mesage(-8,0,subr)
   DO i = Iz , Nz
      Z(i) = 0
   ENDDO
!
!     SET UP TO UNPACK THE COLUMN
!
   Ones = 0
   Eol = 0
   CALL intpk(*100,File,0,1,0)
   DO
      CALL zntpki
      IF ( Row>Size ) THEN
!
!     ELEMENT OF COLUMN LIES OUT OF RANGE INDICATED BY TRAILER
!
         WRITE (Outpt,99003) Sfm , File
99003    FORMAT (A25,' 2175, THE ROW POSITION OF AN ELEMENT OF A COLUMN ','ON FILE',I5,/5X,'IS GREATER THAN NUMBER OF ROWS ',       &
                &'SPECIFIED BY TRAILER.')
!
!     FATAL ERROR
!
         CALL close(File,Clsrew)
         CALL mesage(-61,0,subr)
         GOTO 99999
      ELSE
         k = Row - 1
         zword = k/Nbpw + Iz
         zbit = mod(k,Nbpw) + 1
         Z(zword) = orf(Z(zword),bit(zbit))
         Ones = Ones + 1
!
!     UNPACK THE ELEMENTS AND TURN ON BITS IN THE BIT STRING.  MAINTAIN
!     COUNT OF BITS IN -ONES-.
!
         IF ( Eol>0 ) EXIT
      ENDIF
   ENDDO
!
!     BIT STRING IS COMPLETE.
!
 100  CALL close(File,Clsrew)
   RETURN
!
!     FILE IS PURGED
!
 200  Size = 0
   Ones = 0
   Here = .FALSE.
   RETURN
99999 END SUBROUTINE partn3
