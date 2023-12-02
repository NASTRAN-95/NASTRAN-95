!*==partn3.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
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
   USE c_blank
   USE c_names
   USE c_system
   USE c_xmssg
   USE c_zntpkx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: File
   INTEGER :: Size
   INTEGER :: Ones
   INTEGER :: Iz
   INTEGER :: Nz
   LOGICAL :: Here
   INTEGER , DIMENSION(4) :: Buf
   INTEGER :: Core
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(64) :: bit
   INTEGER :: i , j , k , zbit , zword
   INTEGER , DIMENSION(7) :: mcb
   LOGICAL , SAVE :: pass
   INTEGER , DIMENSION(2) , SAVE :: subr
   INTEGER , DIMENSION(6) :: trl
   EXTERNAL close , intpk , lshift , mesage , open , orf , rdtrl , rshift , skprec , zntpki
!
! End of declarations rewritten by SPAG
!
   !>>>>EQUIVALENCE (trl(1),mcb(2))
   DATA subr/4HPART , 4HN3  /
   DATA pass/.FALSE./
!
!     SET UP TABLE OF BITS ON FIRST PASS THROUGH THIS ROUTINE.
!
   IF ( .NOT.(pass) ) THEN
      pass = .TRUE.
      j = nbpw - 1
      k = lshift(1,j)
      DO i = 1 , nbpw
         bit(i) = k
         k = rshift(k,1)
      ENDDO
   ENDIF
!
   CALL open(*200,File,Buf,rdrew)
   Here = .TRUE.
   mcb(1) = File
   CALL rdtrl(mcb)
!
!     NUMBER OF WORDS IN COLUMN INCLUDING ZEROS
!
   Size = trl(2)
   IF ( ireqcl==0 ) THEN
      ireqcl = 1
   ELSEIF ( ireqcl<=0 .OR. ireqcl>trl(1) ) THEN
      IF ( trl(1)>0 ) THEN
         WRITE (outpt,99001) swm , File , trl(1) , ireqcl
99001    FORMAT (A27,' 2173, PARTITIONING VECTOR FILE',I5,' CONTAINS',I10,' COLUMNS.',/5X,' THE FIRST COLUMN WILL BE USED, NOT THE',&
                &' REQUESTED COLUMN',I10)
      ENDIF
      ireqcl = 1
   ENDIF
   CALL skprec(File,ireqcl)
   IF ( trl(4)/=1 .AND. trl(4)/=2 ) THEN
      WRITE (outpt,99002) swm , File
99002 FORMAT (A27,' 2174, PARTITIONING VECTOR ON FILE',I5,' IS NOT REAL-SINGLE OR REAL-DOUBLE PRECISION.')
   ENDIF
!
!     ZERO THE BIT STRING
!
   Nz = Iz + (Size-1)/nbpw
   IF ( Nz>Core ) CALL mesage(-8,0,subr)
   DO i = Iz , Nz
      z(i) = 0
   ENDDO
!
!     SET UP TO UNPACK THE COLUMN
!
   Ones = 0
   eol = 0
   CALL intpk(*100,File,0,1,0)
   SPAG_Loop_1_1: DO
      CALL zntpki
      IF ( row>Size ) THEN
!
!     ELEMENT OF COLUMN LIES OUT OF RANGE INDICATED BY TRAILER
!
         WRITE (outpt,99003) sfm , File
99003    FORMAT (A25,' 2175, THE ROW POSITION OF AN ELEMENT OF A COLUMN ','ON FILE',I5,/5X,'IS GREATER THAN NUMBER OF ROWS ',       &
                &'SPECIFIED BY TRAILER.')
!
!     FATAL ERROR
!
         CALL close(File,clsrew)
         CALL mesage(-61,0,subr)
         RETURN
      ELSE
         k = row - 1
         zword = k/nbpw + Iz
         zbit = mod(k,nbpw) + 1
         z(zword) = orf(z(zword),bit(zbit))
         Ones = Ones + 1
!
!     UNPACK THE ELEMENTS AND TURN ON BITS IN THE BIT STRING.  MAINTAIN
!     COUNT OF BITS IN -ONES-.
!
         IF ( eol>0 ) EXIT SPAG_Loop_1_1
      ENDIF
   ENDDO SPAG_Loop_1_1
!
!     BIT STRING IS COMPLETE.
!
 100  CALL close(File,clsrew)
   RETURN
!
!     FILE IS PURGED
!
 200  Size = 0
   Ones = 0
   Here = .FALSE.
END SUBROUTINE partn3
