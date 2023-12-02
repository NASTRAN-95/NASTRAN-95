!*==partn2.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE partn2(Cp,Rp,Core,Buf)
   USE c_blank
   USE c_prtmrg
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Cp
   INTEGER :: Rp
   INTEGER :: Core
   INTEGER , DIMENSION(4) :: Buf
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , j
   INTEGER , DIMENSION(2) , SAVE :: subr
   EXTERNAL mesage , partn3
!
! End of declarations rewritten by SPAG
!
!
!     THIS IS AN INITIALIZATION ROUTINE FOR PARTN1 AND MERGE1.
!     IT CALLS PARTN3 TO BUILD THE BIT STRINGS FROM THE PARTITIONING
!     VECTORS -CP- AND -RP- AND SETS DEFAULT OPTIONS FOR -CP- AND  -RP-
!     BASED ON -SYM-.
!
!
   DATA subr/4HPART , 4HN2  /
!
!
!              I             I             I                I
!       SYM    I  RP-PURGED  I  CP-PURGED  I NEITHER-PURGED I
!     ---------+-------------+-------------+----------------+--------
!              I             I             I                I
!       .LT.0  I  RP IS SET  I  CP IS SET  I  RP MUST HAVE  I
!              I  = TO CP    I  = TO RP    I  SAME ONES-S   I
!              I             I             I  COUNT AS CP   I
!     ---------+-------------+-------------+----------------+--------
!              I             I             I                I
!       .GE.0  I  RP IS SET  I  CP IS SET  I  USE CP AND RP I
!              I  TO ALL 0   I  TO ALL 0   I                I
!              I             I             I                I
!
!     IN ALL CASES, RESULTANT -CP- AND -RP- DIMENSIONS MUST BE
!     COMPATIBLE TO THOSE OF  -A-
!
!
!     CONVERT COLUMN PARTITIONING VECTOR TO BIT STRING.
!
   icp = 1
   ireqcl = cpcol
   CALL partn3(Cp,cpsize,cpones,icp,ncp,cphere,Buf,Core)
   cpcol = ireqcl
   IF ( cphere ) THEN
      irp = ncp + 1
   ELSE
      irp = 1
   ENDIF
!
!     CONVERT ROW PARTITIONING VECTOR TO BIT STRING.
!
   ireqcl = rpcol
   CALL partn3(Rp,rpsize,rpones,irp,nrp,rphere,Buf,Core)
   rpcol = ireqcl
!
!     BRANCH ON SYMMETRIC OR  NON-SYMMETRIC DMAP VARIABLE SYM
!
   cpnull = .FALSE.
   rpnull = .FALSE.
   IF ( sym<0 ) THEN
!
!     DMAP USER CLAIMS SYMMETRIC INPUT AND OUTPUT
!
      IF ( cphere ) THEN
!
!     -CP- IS NOT PURGED.  IF -RP- IS PURGED IT IS SET EQUAL TO -CP-.
!
         IF ( rphere ) THEN
!
!     BOTH -RP- AND -CP- ARE PRESENT AND SINCE USER HAS SPECIFIED A
!     SYMMETRIC OUTPUT PARTITION IS DESIRED THE NUMBER OF
!     NON-ZEROS IN-CP- MUST EQUAL THE NUMBER OF NON-ZEROS IN -RP- FOR NO
!     ERROR HERE.
!
            IF ( cpones/=rpones .OR. cpsize/=rpsize ) THEN
               WRITE (outpt,99001) swm , Cp , Rp
99001          FORMAT (A27,' 2171, SYM FLAG INDICATES TO THE PARTITION OR MERGE',' MODULE THAT A SYMMETRIC MATRIX IS TO BE',/5X,    &
                      &' OUTPUT.  THE PARTITIONING VECTORS',2I4,' HOWEVER DO NOT',                                                  &
                      &' CONTAIN AN IDENTICAL NUMBER OF ZEROS AND NON-ZEROS.')
            ENDIF
!
!     CHECK FOR ORDER OF ONES IN ROW AND COLUMN PARTITIONING VECTOR.
!
            IF ( cpsize==rpsize ) THEN
               j = irp
               DO i = icp , ncp
                  IF ( z(i)/=z(j) ) THEN
                     CALL spag_block_2
                     RETURN
                  ENDIF
                  j = j + 1
               ENDDO
            ENDIF
         ELSE
            irp = icp
            nrp = ncp
            rpones = cpones
            rpsize = cpsize
         ENDIF
         RETURN
!
!     -CP- IS PURGED.  CHECK FOR -RP- PURGED (ERROR), AND IF NOT SET
!     -CP- BITS EQUAL TO -RP- BITS
!
      ELSEIF ( rphere ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
!
!     DMAP USER DOES NOT REQUIRE SYMMETRY
!
   ELSEIF ( cphere ) THEN
!
!     -CP- NOT PURGED.  IF -RP- IS PURGED SET IT NULL.
!
      IF ( .NOT.(rphere) ) THEN
         rpnull = .TRUE.
         nrp = irp - 1
         rpsize = 0
         rpones = 0
      ENDIF
      RETURN
!
!     -CP- IS PURGED.  THUS -RP- MUST BE PRESENT FOR NO ERROR.
!
   ELSEIF ( rphere ) THEN
!
!     SET CP-ONES EQUAL TO 0 AND CPSIZE = 0
!
      cpnull = .TRUE.
      cpsize = 0
      cpones = 0
      RETURN
   ENDIF
!
!     BOTH -RP- AND -CP- PURGED AND -A- IS NOT PURGED (ERROR).
!
   WRITE (outpt,99002) sfm
99002 FORMAT (A25,' 2170, BOTH THE ROW AND COLUMN PARTITIONING VECTORS',' ARE PURGED AND ONLY ONE MAY BE.')
   CALL mesage(-61,0,subr)
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
!
!     SET CP-ONES = RP-ONES BY SIMPLE EQUIVALENCE OF CORE SPACE
!
      Icp = Irp
      Ncp = Nrp
      Cpones = Rpones
      Cpsize = Rpsize
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
!
!     ROW AND COLUMN PARTITIONING VECTORS DO NOT HAVE SAME ORDER.
!
      WRITE (Outpt,99001) Swm
99001 FORMAT (A27,' 2172, ROW AND COLUMN PARTITIONING VECTORS DO NOT ','HAVE IDENTICAL ORDERING OF ZERO',/5X,' AND NON-ZERO ',      &
             &'ELEMENTS, AND SYM FLAG INDICATES THAT A SYMMETRIC ','PARTITION OR MERGE IS TO BE PERFORMED.')
   END SUBROUTINE spag_block_2
END SUBROUTINE partn2
