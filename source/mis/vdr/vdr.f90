!*==vdr.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE vdr
   USE c_blank
   USE c_system
   USE c_vdrcom
   IMPLICIT NONE
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
      buf(i) = 0
   ENDDO
   casecc = 101
   output = -1
   sort2 = -1
   CALL vdra
   IF ( sscell/=0 ) sdr2 = 1
   IF ( vdrreq==0 ) RETURN
   mcb(1) = infile
   CALL rdtrl(mcb)
   IF ( mcb(1)==infile ) CALL vdrb(infile,outfle,iadisp)
   IF ( app(1)/=trn(1) ) RETURN
   mcb(1) = pnl
   CALL rdtrl(mcb)
   IF ( mcb(1)/=pnl ) RETURN
   CALL vdrb(pnl,opnl1,ipnl)
END SUBROUTINE vdr
