
SUBROUTINE close(File,Iop)
   IMPLICIT NONE
   INCLUDE 'DSIOF.COM'
   INCLUDE 'XNSTRN.COM'
   REAL Dum1(77) , Dum2(21)
   INTEGER Idiag , Isysbf
   INTEGER*2 Iunit(220)
   COMMON /dsunit/ Iunit
   COMMON /system/ Isysbf , Dum1 , Idiag , Dum2
   INTEGER File , Iop
   INTEGER isave
!***************************************************************
!                          NOTICE
!
!     THIS PROGRAM BELONGS TO RPK CORPORATION.  IT IS CONSIDERED
! A TRADE SECRET AND IS NOT TO BE DIVULGED OR USED BY PARTIES
! WHO HAVE NOT RECEIVED WRITTEN AUTHORIZATION FROM RPK.
!***************************************************************
   Name = File
   Iocode = Iop
   Iretrn = 77
   CALL dsgefl
   IF ( Ifilex/=0 ) THEN
      Iretrn = 0
      IF ( iand(Idiag,2**14)/=0 ) CALL dsmsg(2)
      IF ( Iocode==1 ) THEN
         IF ( Iprvop/=0 ) THEN
            CALL dsefwr
            IF ( (Indclr-Indbas)/=5 ) THEN
               Ibase(Indbas+4) = Indclr - Indbas + 1
               CALL dbmmgr(4)
            ENDIF
            CALL dsxfsz
         ENDIF
         CALL dbmmgr(2)
         Nblock = 1
         Indclr = Indbas + 5
         Indcbp = Indclr
      ELSEIF ( Iprvop==0 ) THEN
         IF ( Indcbp/=Indclr ) CALL dsskrc
         CALL dbmmgr(2)
      ELSE
         CALL dsefwr
         Ibase(Indbas+4) = Indclr - Indbas + 1
! SAVE INDBAS TO ALLOW DSBRC1 TO CORRECTLY BACKSPACE FILE OPENNED FOR WRITE
         isave = Indbas
         CALL dbmmgr(4)
         CALL dsxfsz
         Indbas = isave
         IF ( Iocode/=-2 ) CALL dsbrc1
!      CALL DSGNCL
         CALL dbmmgr(2)
      ENDIF
      CALL dssdcb
      Fcb(2,Ifilex) = 0
      Fcb(12,Ifilex) = 0
      IF ( Name>=101 .AND. Name<=320 ) Iunit(Name-100) = 0
   ENDIF
!***************************************************************
!                          NOTICE
!
!     THIS PROGRAM BELONGS TO RPK CORPORATION.  IT IS CONSIDERED
! A TRADE SECRET AND IS NOT TO BE DIVULGED OR USED BY PARTIES
! WHO HAVE NOT RECEIVED WRITTEN AUTHORIZATION FROM RPK.
!***************************************************************
END SUBROUTINE close
