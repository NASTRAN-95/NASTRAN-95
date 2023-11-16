
SUBROUTINE partn2(Cp,Rp,Core,Buf)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Cpcol , Cpones , Cpsize , Form(4) , Icp , Ireqcl , Irp , Ncp , Nrp , Outpt , Rpcol , Rpones , Rpsize , Sym , Sysbuf ,    &
         & Type , Z(1)
   LOGICAL Cphere , Cpnull , Rphere , Rpnull
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Sym , Type , Form , Cpcol , Rpcol , Ireqcl
   COMMON /prtmrg/ Cpsize , Rpsize , Cpones , Rpones , Cpnull , Rpnull , Cphere , Rphere , Icp , Ncp , Irp , Nrp
   COMMON /system/ Sysbuf , Outpt
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Core , Cp , Rp
   INTEGER Buf(4)
!
! Local variable declarations
!
   INTEGER i , j , subr(2)
!
! End of declarations
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
   Icp = 1
   Ireqcl = Cpcol
   CALL partn3(Cp,Cpsize,Cpones,Icp,Ncp,Cphere,Buf,Core)
   Cpcol = Ireqcl
   IF ( Cphere ) THEN
      Irp = Ncp + 1
   ELSE
      Irp = 1
   ENDIF
!
!     CONVERT ROW PARTITIONING VECTOR TO BIT STRING.
!
   Ireqcl = Rpcol
   CALL partn3(Rp,Rpsize,Rpones,Irp,Nrp,Rphere,Buf,Core)
   Rpcol = Ireqcl
!
!     BRANCH ON SYMMETRIC OR  NON-SYMMETRIC DMAP VARIABLE SYM
!
   Cpnull = .FALSE.
   Rpnull = .FALSE.
   IF ( Sym<0 ) THEN
!
!     DMAP USER CLAIMS SYMMETRIC INPUT AND OUTPUT
!
      IF ( Cphere ) THEN
!
!     -CP- IS NOT PURGED.  IF -RP- IS PURGED IT IS SET EQUAL TO -CP-.
!
         IF ( Rphere ) THEN
!
!     BOTH -RP- AND -CP- ARE PRESENT AND SINCE USER HAS SPECIFIED A
!     SYMMETRIC OUTPUT PARTITION IS DESIRED THE NUMBER OF
!     NON-ZEROS IN-CP- MUST EQUAL THE NUMBER OF NON-ZEROS IN -RP- FOR NO
!     ERROR HERE.
!
            IF ( Cpones/=Rpones .OR. Cpsize/=Rpsize ) THEN
               WRITE (Outpt,99001) Swm , Cp , Rp
99001          FORMAT (A27,' 2171, SYM FLAG INDICATES TO THE PARTITION OR MERGE',' MODULE THAT A SYMMETRIC MATRIX IS TO BE',/5X,    &
                      &' OUTPUT.  THE PARTITIONING VECTORS',2I4,' HOWEVER DO NOT',                                                  &
                      &' CONTAIN AN IDENTICAL NUMBER OF ZEROS AND NON-ZEROS.')
            ENDIF
!
!     CHECK FOR ORDER OF ONES IN ROW AND COLUMN PARTITIONING VECTOR.
!
            IF ( Cpsize==Rpsize ) THEN
               j = Irp
               DO i = Icp , Ncp
                  IF ( Z(i)/=Z(j) ) GOTO 200
                  j = j + 1
               ENDDO
            ENDIF
         ELSE
            Irp = Icp
            Nrp = Ncp
            Rpones = Cpones
            Rpsize = Cpsize
         ENDIF
         GOTO 99999
!
!     -CP- IS PURGED.  CHECK FOR -RP- PURGED (ERROR), AND IF NOT SET
!     -CP- BITS EQUAL TO -RP- BITS
!
      ELSEIF ( Rphere ) THEN
         GOTO 100
      ENDIF
!
!     DMAP USER DOES NOT REQUIRE SYMMETRY
!
   ELSEIF ( Cphere ) THEN
!
!     -CP- NOT PURGED.  IF -RP- IS PURGED SET IT NULL.
!
      IF ( .NOT.(Rphere) ) THEN
         Rpnull = .TRUE.
         Nrp = Irp - 1
         Rpsize = 0
         Rpones = 0
      ENDIF
      GOTO 99999
!
!     -CP- IS PURGED.  THUS -RP- MUST BE PRESENT FOR NO ERROR.
!
   ELSEIF ( Rphere ) THEN
!
!     SET CP-ONES EQUAL TO 0 AND CPSIZE = 0
!
      Cpnull = .TRUE.
      Cpsize = 0
      Cpones = 0
      GOTO 99999
   ENDIF
!
!     BOTH -RP- AND -CP- PURGED AND -A- IS NOT PURGED (ERROR).
!
   WRITE (Outpt,99002) Sfm
99002 FORMAT (A25,' 2170, BOTH THE ROW AND COLUMN PARTITIONING VECTORS',' ARE PURGED AND ONLY ONE MAY BE.')
   CALL mesage(-61,0,subr)
!
!     SET CP-ONES = RP-ONES BY SIMPLE EQUIVALENCE OF CORE SPACE
!
 100  Icp = Irp
   Ncp = Nrp
   Cpones = Rpones
   Cpsize = Rpsize
   GOTO 99999
!
!     ROW AND COLUMN PARTITIONING VECTORS DO NOT HAVE SAME ORDER.
!
 200  WRITE (Outpt,99003) Swm
99003 FORMAT (A27,' 2172, ROW AND COLUMN PARTITIONING VECTORS DO NOT ','HAVE IDENTICAL ORDERING OF ZERO',/5X,' AND NON-ZERO ',      &
             &'ELEMENTS, AND SYM FLAG INDICATES THAT A SYMMETRIC ','PARTITION OR MERGE IS TO BE PERFORMED.')
99999 RETURN
END SUBROUTINE partn2
