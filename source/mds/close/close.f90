!*==close.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE close(File,Iop)
   IMPLICIT NONE
   USE I_DSIOF
   USE I_XNSTRN
   USE C_DSUNIT
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: File
   INTEGER :: Iop
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: isave
!
! End of declarations rewritten by SPAG
!
!***************************************************************
!                          NOTICE
!
!     THIS PROGRAM BELONGS TO RPK CORPORATION.  IT IS CONSIDERED
! A TRADE SECRET AND IS NOT TO BE DIVULGED OR USED BY PARTIES
! WHO HAVE NOT RECEIVED WRITTEN AUTHORIZATION FROM RPK.
!***************************************************************
   name = File
   iocode = Iop
   iretrn = 77
   CALL dsgefl
   IF ( ifilex/=0 ) THEN
      iretrn = 0
      IF ( iand(Idiag,2**14)/=0 ) CALL dsmsg(2)
      IF ( iocode==1 ) THEN
         IF ( iprvop/=0 ) THEN
            CALL dsefwr
            IF ( (indclr-indbas)/=5 ) THEN
               ibase(indbas+4) = indclr - indbas + 1
               CALL dbmmgr(4)
            ENDIF
            CALL dsxfsz
         ENDIF
         CALL dbmmgr(2)
         nblock = 1
         indclr = indbas + 5
         indcbp = indclr
      ELSEIF ( iprvop==0 ) THEN
         IF ( indcbp/=indclr ) CALL dsskrc
         CALL dbmmgr(2)
      ELSE
         CALL dsefwr
         ibase(indbas+4) = indclr - indbas + 1
! SAVE INDBAS TO ALLOW DSBRC1 TO CORRECTLY BACKSPACE FILE OPENNED FOR WRITE
         isave = indbas
         CALL dbmmgr(4)
         CALL dsxfsz
         indbas = isave
         IF ( iocode/=-2 ) CALL dsbrc1
!      CALL DSGNCL
         CALL dbmmgr(2)
      ENDIF
      CALL dssdcb
      fcb(2,ifilex) = 0
      fcb(12,ifilex) = 0
      IF ( name>=101 .AND. name<=320 ) Iunit(name-100) = 0
   ENDIF
!***************************************************************
!                          NOTICE
!
!     THIS PROGRAM BELONGS TO RPK CORPORATION.  IT IS CONSIDERED
! A TRADE SECRET AND IS NOT TO BE DIVULGED OR USED BY PARTIES
! WHO HAVE NOT RECEIVED WRITTEN AUTHORIZATION FROM RPK.
!***************************************************************
END SUBROUTINE close
