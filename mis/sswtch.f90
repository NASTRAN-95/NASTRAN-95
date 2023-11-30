
SUBROUTINE sswtch(Nbit,L)
   IMPLICIT NONE
   REAL Dummy(3) , Xsys(78)
   INTEGER Lxlink , Maxlnk , Ns(1) , Switch(3)
   COMMON /sem   / Dummy , Ns
   COMMON /system/ Xsys , Switch
   COMMON /xlink / Lxlink , Maxlnk
   INTEGER L , Nbit
   INTEGER andf , lshift , rshift
   INTEGER i , iret , nbit2 , renter
   EXTERNAL andf , lshift , rshift
!
!     PURPOSE OF THIS ROUTINE IS TO SET L = 1 IF SENSE SWITCH BIT IS
!     ON, OTHERWISE L = 0.
!
!     SENSE SWITCH DEFINITION
!      1 = DUMP CORE WHEN SUBROUTINE DUMP OR PDUMP(NO ARGUMENTS) IS
!          CALLED
!      2 = DUMP FIAT TABLE AFTER ALLOCATION
!      3 = DUMP DATA POOL DICTIONARY AFTER ALLOCATION
!      4 = DUMP OSCAR FILE AT END OF XGPI
!      5 = CONSOLE MESSAGE DESIRED (BEGIN)
!      6 = CONSOLE MESSAGE DESIRED (END)
!      7 = EIGENVALUE EXTRACTION DIAGNOSTICS
!          (DETERMINANT AND INVERSE POWER)
!      8 = TRACES NPTP ON 1108
!      9 = TURNS ON PRINTER PLOTTER FOR ANY XYPLOT REQUESTS
!     10 = USES ALTERNATE ALGORITHUM FOR NON LINEAR LOADS SEE SPR 153
!     11 = ACTIVE ROW AND COLUMN TIME PRINTS
!     12 = CONPLEX EIGENVALUE EXTRACTION DIAGNOSTICS
!          (INVERSE POWER)
!     28 = PUNCHES OUT LINK SPECIFICATION TABLE - DECK XBSBD
!     29 = PROCESS LINK SPECIFICATION UPDATE DECK
!     30 = PUNCHES OUT ALTERS TO XSEM-S FOR SWITCHES 1-15
!     31 = PRINT LINK SPECIFICATION TABLE
!
   DATA renter/4HREEN/
!
   L = 0
   IF ( iret==1 ) RETURN
   IF ( Nbit>31 ) THEN
!
      nbit2 = Nbit - 31
      IF ( andf(lshift(1,nbit2-1),Switch(2))/=0 ) L = 1
      RETURN
   ELSE
      IF ( andf(lshift(1,Nbit-1),Switch(1))/=0 ) L = 1
      RETURN
   ENDIF
!
!
   ENTRY pressw(Nbit,L)
!     =====================
!
!     PRESSW IS CALLED ONLY BY BGNSYS AND XCSA TO SETUP DIAGNOSTIC BITS
!     FOR A PARTICULAR LINK.
!     BITS  0 THRU 47 ARE USED FOR 48 DIAGNOSTICS
!     BITS 49 THRU 63 ARE RESERVED FOR 15 LINK NOS.
!     NBIT HERE (INPUT) CONTAINS BCD WORD NSXX WHERE XX IS LINK NO.
!
   iret = 0
   IF ( Nbit==renter ) RETURN
   IF ( Switch(3)+Switch(2)==0 ) iret = 1
   i = 32 - Maxlnk
   IF ( rshift(Switch(2),i)/=0 ) THEN
      DO i = 1 , Maxlnk
         IF ( Nbit==Ns(i) ) GOTO 50
      ENDDO
      i = 0
      IF ( Nbit==Ns(16) ) i = 5
      IF ( Nbit==Ns(17) ) i = 8
      IF ( Nbit==Ns(18) ) i = 13
      IF ( Nbit==Ns(19) ) i = 6
      IF ( Nbit==Ns(20) ) i = 2
      IF ( Nbit==Ns(21) ) i = 9
      IF ( Nbit==Ns(22) ) i = 11
      IF ( Nbit==Ns(23) ) i = 15
      IF ( i==0 ) GOTO 100
 50   nbit2 = i + 31 - Maxlnk
      IF ( andf(lshift(1,nbit2),Switch(2))==0 ) iret = 1
   ENDIF
 100  IF ( iret==0 ) Switch(1) = Switch(3)
END SUBROUTINE sswtch
