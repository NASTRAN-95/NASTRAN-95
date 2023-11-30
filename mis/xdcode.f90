
SUBROUTINE xdcode
   IMPLICIT NONE
   REAL Dm37(37) , Record(20) , Skip1(3) , Skip2(8) , Skip3(2) , Skip4(2)
   INTEGER Ibuf , Ichar(80) , Icol , Icount , Name(2) , Nbpw , Nout
   COMMON /system/ Ibuf , Nout , Dm37 , Nbpw
   COMMON /xrgdxx/ Skip1 , Icol , Skip2 , Record , Ichar , Skip3 , Icount , Skip4 , Name
   INTEGER iblank , k , notuse
   CHARACTER*80 temp
   CHARACTER*8 temp8
!
!     (MACHINE INDEPENDENT FORTRAN 77 ROUTINE)
!
!     XDCODE DECODES A 20A4 ARRAY IN RECORD INTO A 80A1 ARRAY IN ICHAR
!
!     XDCODE IS CALLED ONLY BY XRGDCF, XRGDTB, XRGSST, AND XRGSUB
!
   DATA iblank/4H    /
!
   WRITE (temp,99001) Record
99001 FORMAT (20A4)
   READ (temp,99002) Ichar
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
   IF ( Nbpw>=60 .AND. Icount/=8 ) THEN
      DO k = Icount , 7
         Ichar(Icol+k) = iblank
      ENDDO
   ENDIF
   CALL na12a8(*100,Ichar(Icol),8,Name,notuse)
   IF ( Nbpw/=60 ) RETURN
!
!     BLANK OUT 2ND WORD (CDC ONLY)
!
   WRITE (temp8,99003) Name(1)
99003 FORMAT (A8)
   Name(1) = iblank
   Name(2) = iblank
   READ (temp8,99004) Name
99004 FORMAT (2A4)
   RETURN
!
 100  WRITE (Nout,99005)
99005 FORMAT ('0BAD DATA/XECODE')
END SUBROUTINE xdcode
