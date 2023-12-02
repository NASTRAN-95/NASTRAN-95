!*==xdcode.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xdcode
   USE c_system
   USE c_xrgdxx
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: iblank
   INTEGER :: k , notuse
   CHARACTER(80) :: temp
   CHARACTER(8) :: temp8
!
! End of declarations rewritten by SPAG
!
!
!     (MACHINE INDEPENDENT FORTRAN 77 ROUTINE)
!
!     XDCODE DECODES A 20A4 ARRAY IN RECORD INTO A 80A1 ARRAY IN ICHAR
!
!     XDCODE IS CALLED ONLY BY XRGDCF, XRGDTB, XRGSST, AND XRGSUB
!
   DATA iblank/4H    /
!
   WRITE (temp,99001) record
99001 FORMAT (20A4)
   READ (temp,99002) ichar
99002 FORMAT (80A1)
   RETURN
!
   ENTRY xecode
!     ============
!
!     XECODE ENCODES A 8A1 BCD ARRAY IN ICHAR INTO A 2A4 BCD ARRAY
!     IN NAME
!     (THIS ENTRY REPLACES THE OLD MACHINE DEPENDENT ROUTINE OF THE
!     SAME NAME)
!
!     THE INCOMING WORD IN CDC MACHINE WOULD BE ZERO FILLED, SUCH AS
!     THE CARD TABLE AND THE MED TABLE IN XGPI RESTART PROCESSING.
!     MAKE SURE THAT THE INCOMING WORD FROM A 60- OR 64- BIT MACHINE
!     IS BLANK FILLED IF IT IS LESS THAN 8 BYTE LONG
!
!     XECODE IS CALL ONLY BY XRGDTB
!
   IF ( nbpw>=60 .AND. icount/=8 ) THEN
      DO k = icount , 7
         ichar(icol+k) = iblank
      ENDDO
   ENDIF
   CALL na12a8(*100,ichar(icol),8,name,notuse)
   IF ( nbpw/=60 ) RETURN
!
!     BLANK OUT 2ND WORD (CDC ONLY)
!
   WRITE (temp8,99003) name(1)
99003 FORMAT (A8)
   name(1) = iblank
   name(2) = iblank
   READ (temp8,99004) name
99004 FORMAT (2A4)
   RETURN
!
 100  WRITE (nout,99005)
99005 FORMAT ('0BAD DATA/XECODE')
END SUBROUTINE xdcode
