!*==vdr.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE vdr
   IMPLICIT NONE
   USE C_BLANK
   USE C_SYSTEM
   USE C_VDRCOM
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i
   INTEGER , DIMENSION(7) :: mcb
   EXTERNAL rdtrl , vdra , vdrb
!
! End of declarations rewritten by SPAG
!
!
!     VDR IS THE CONTROL PROGRAM FOR THE VECTOR DATA RECOVERY MODULE
!
!                                                          OPHID
!                                  PHID                    OUDVC1
!                                  UDVF CLAMA              OUDV1
!             CASECC  EQDYN  USETD UDVT PPF          PHLD  OPHIH  OPNL1
!     VDR     CASEXX,HEQDYN,HUSETD,PHIH,TOL   ,XYCBD,PNLH /OUHVC1,HOPNL1
!                                  UHVT HTOL         HPNLD OUHV1
!                                  HUDVT                   HOUVD1
!
!                   TRANRESP     DIRECT
!              /C,N,FREQRESP/C,N,MODAL /V,N,SORT2/V,N,OUTPUT/V,N,SDR2
!                   CEIGN
!
!              /V,N,FMODE  $      PROGRAMMER'S MANUAL PP. 4.60-1 TRHU -7
!
!
!
!     EXECUTE THE PHASES OF VDR.
!
   DO i = 1 , 50
      Buf(i) = 0
   ENDDO
   Casecc = 101
   Output = -1
   Sort2 = -1
   CALL vdra
   IF ( Sscell/=0 ) Sdr2 = 1
   IF ( Vdrreq==0 ) RETURN
   mcb(1) = Infile
   CALL rdtrl(mcb)
   IF ( mcb(1)==Infile ) CALL vdrb(Infile,Outfle,Iadisp)
   IF ( App(1)/=Trn(1) ) RETURN
   mcb(1) = Pnl
   CALL rdtrl(mcb)
   IF ( mcb(1)/=Pnl ) RETURN
   CALL vdrb(Pnl,Opnl1,Ipnl)
END SUBROUTINE vdr
