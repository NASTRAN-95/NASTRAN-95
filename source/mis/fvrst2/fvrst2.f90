!*==fvrst2.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fvrst2
USE C_BLANK
USE C_CONDAD
USE C_SYSTEM
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: dum , fnam
   REAL(REAL64) :: fact , freq , period
   INTEGER :: file , ibuf1 , ifol , ifreq , ifreq1 , ifreq2 , ip1 , itol , k1 , k2 , k3 , kk , next , nfreq , ntimes , nz
   INTEGER , SAVE :: fol , frl , reord1 , reord2 , tol
   INTEGER , DIMENSION(2) , SAVE :: modnam
   INTEGER , DIMENSION(7) :: trl
   EXTERNAL close , fname , fread , fvrs2a , korsz , mesage , open , read , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!    1. ENTRY POINT - FVRST2
!
!    2. PURPOSE -  THIS MODULE IS USED DURING A FORCED VIBRATION
!                  RESPONSE ANALYSIS OF ROTATING CYCLIC STRUCTURES
!                  TO GENERATE TABLE DATA BLOCKS FRL AND FOL AND TO
!                  GENERATE MATRIX DATA BLOCKS REORDER1 AND REORDER2.
!                  FVRSTR2 ALSO COMPUTES PARAMETERS LMAX, NTSTEPS,
!                  FLMAX, NORO1 AND NORO2.
!
!    3. DMAP CALLING SEQUENCE -
!
!         FVRSTR2  TOL,,,,,,, / FRL,FOL,REORDER1,REORDER2,,,, /
!                  V,Y,NSEGS/ V,Y,CYCIO/ V,Y,LMAX=-1/ V,N,FKMAX/
!                  V,N,FLMAX/ V,N,NTSTEPS/ V,N,NORO1/ V,N,NORO2  $
!
!    4. INPUT DATA BLOCKS -
!
!         TOL    - TIME OUTPUT LIST.
!
!         NOTE   - (1) TOL MUST BE PRESENT.
!
!    5. OUTPUT DATA BLOCKS -
!
!         FRL      - FREQUENCY RESPONSE LIST.
!         FOL      - FREQUENCY OUTPUT LIST.
!         REORDER1 - LOAD REORDERING MATRIX FO TIME-DEPENDENT PROBLEMS.
!         REORDER2 - LOAD REORDERING MATRIX FO TIME-DEPENDENT PROBLEMS.
!
!         NOTE     - (1) FRL AND FOL CANNOT BE PURGED.
!                    (2) REORDER1 AND REORDER2 SHOULD NOT BE PURGED.
!
!    6. PARAMETERS -
!
!        (A) NSEGS   - INPUT-INTEGER-NO DEFAULT.  THE NUMBER OF
!                      IDENTICAL SEGMENTS IN THE STRUCTURAL MODEL.
!        (B) CYCIO   - INPUT-INTEGER-NO DEFAULT.  THE INTEGER VALUE
!                      OF THIS PARAMETER SPECIFIES THE FORM OF THE INPUT
!                      AND OUTPUT DATA FOR CYCLIC STRUCTURES. A VALUE
!                      OF +1 IS USED TO SPECIFY PHYSICAL SEGMENT REPRE-
!                      SENTATION AND A VALUE OF -1 FOR CYCLIC TRANSFOR-
!                      MATION REPRESENTATION.
!        (C) LMAX    - INPUT/OUTPUT-INTEGER.  THE INTEGER VALUE OF THIS
!                      PARAMETER SPECIFIES THE MAXIMUM TIME HARMONIC
!                      INDEX FOR CYCLIC STRUCTURES. THE DEFAULT VALUE
!                      IS NTSTEPS/2, WHERE NTSTEPS IS THE NUMBER OF
!                      TIME STEPS DEFINED BELOW.
!        (D) FKMAX   - INPUT-INTEGER-NO DEFAULT.  FUNCTION OF KMAX.
!        (E) FLMAX   - OUTPUT-INTEGER-NO DEFAULT.  FUNCTION OF LMAX.
!        (F) NTSTEPS - OUTPUT-INTEGER-NO DEFAULT.  THE NUMBER OF
!                      TIME STEPS FOUND IN DATA BLOCK TOL.
!        (G) NORO1   - OUTPUT-INTEGER-NO DEFAULT.  NORO1 =-1 IF DATA
!                      BLOCK REORDER1 IS NOT GENERATED.
!        (H) NORO2   - OUTPUT-INTEGER-NO DEFAULT.  NORO2 =-1 IF DATA
!                      BLOCK REORDER2 IS NOT GENERATED.
!
!    7. METHOD -
!
!         DATA BLOCK TOL IS READ AND THE LIST OF SOLUTION TIMES IS
!         STORED. SET NTSTEPS TO THE NUMBER OF SOLUTION TIMES READ.
!         IF NECESSARY COMPUTE THE DEFAULT VALUE OF LMAX AND THEN
!         COMPUTE FLMAX.
!         GENERATE TABLE DATA BLOCKS FOL AND FRL.
!         GENERATE MATRIX DATA BLOCKS REORDER1 AND REORDER2 AND
!         PARAMETERS NORO1 AND NORO2.
!
!    8. SUBROUTINES - FVRST2 CALLS SUBROUTINE FVRS2A AND OTHER
!                     STANDARD NASTRAN UTILITY ROUTINES.
!
!    9. DESIGN REQUIREMENTS -
!
!         (1) OPEN CORE IS DEFINED AT /ZZFVR2/.
!         (2) NO SCRATCH FILES ARE USED.
!         (3) FVRST2 RESIDES IN LINKNS07.
!         (4) OPEN CORE FOR ONE BUFFER+1 IS REQUIRED.
!
!   10. DIAGNOSTIC MESSAGES -
!
!         THE FOLLOWING MESSAGES MAY BE ISSUED - 3001,3002,3003,3008.
!
!
   DATA modnam/4HFVRS , 4HTR2 /
   DATA tol , frl , fol , reord1 , reord2/101 , 201 , 202 , 203 , 204/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
!     DETERMINE LENGTH OF OPEN CORE AND ALLOCATE BUFFERS.
!
         nz = korsz(Z)
         ibuf1 = nz - Sysbuf
         nz = ibuf1 - 1
         IF ( nz>0 ) THEN
!
!     READ DATA BLOCK TOL (TIME OUTPUT LIST).
!     LIST OF OUTPUT TIME VALUES ARE STORED IN TOL HEADER.
!
            file = tol
            itol = 1
            CALL fname(file,fnam)
            CALL open(*40,file,Z(ibuf1),0)
            CALL fread(file,dum,2,0)
!
!     INSUFFICIENT CORE TO HOLD ALL TIMES.
!
            CALL read(*60,*20,file,Z(itol),nz,1,ntimes)
         ENDIF
!
!     E-O-L ENCOUNTERED
!
         ip1 = -8
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
 20      CALL close(file,1)
!
         nz = nz - ntimes
         next = ntimes + 1
         IF ( nz<=0 ) THEN
            ip1 = -8
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     DEFINE PARAMETER NTSTEPS.
!
!     IF (CYCIO .EQ. -1) NTSTEPS = (NTIMES*FKMAX)/FKMAX
!     IF (CYCIO .EQ. +1) NTSTEPS = (NTIMES*NSEGS)/NSEGS
!
            Ntstps = ntimes
!
!     SET DEFAULT VALUE OF PARAMETER LMAX.
!
            IF ( Lmax<0 ) Lmax = Ntstps/2
!
!     DEFINE PARAMETER FLMAX
!
            kk = (Ntstps/2)*2
            IF ( kk/=Ntstps ) THEN
!
!     NTSTPS IS ODD.
!
               Flmax = 2*Lmax + 1
!
!     NTSTPS IS EVEN.
!
            ELSEIF ( Lmax/=Ntstps/2 ) THEN
               Flmax = 2*Lmax + 1
            ELSE
               Flmax = Ntstps
            ENDIF
!
!
!     GENERATE DATA BLOCKS FRL AND FOL BY CONVERTING TOL TIMES
!     TO THE FREQUENCY DOMAIN.
!
            nfreq = Flmax
            ifol = next
            next = ifol + nfreq
            nz = nz - nfreq
            IF ( nz<=0 ) THEN
               ip1 = -8
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ELSE
!
!     GENERATE FREQUENCY LIST FROM TOL TIME LIST.
!
               Z(ifol) = 0.0
               IF ( nfreq>1 ) THEN
!
                  period = dble(Z(itol+1)) + dble(Z(itol+ntimes-1))
                  freq = 1.0D0/period
                  fact = 1.0D0
!
                  ifreq1 = ifol + 1
                  ifreq2 = ifol + nfreq - 1
!
                  DO ifreq = ifreq1 , ifreq2 , 2
                     Z(ifreq) = fact*freq
                     Z(ifreq+1) = Z(ifreq)
                     fact = fact + 1.0D0
                  ENDDO
!
                  kk = (nfreq/2)*2
                  IF ( kk==nfreq ) Z(ifreq2) = fact*freq
               ENDIF
!
!
!     OUTPUT FOL TABLE (FREQUENCY OUTPUT RESPONSE LIST).
!
               file = fol
               CALL fname(file,fnam)
               CALL open(*40,file,Z(ibuf1),1)
               CALL write(file,fnam,2,0)
               CALL write(file,Z(ifol),nfreq,1)
               CALL close(file,1)
!
               trl(1) = file
               trl(2) = nfreq
               trl(3) = 1
               trl(4) = 0
               trl(5) = 0
               trl(6) = 0
               trl(7) = 0
               CALL wrttrl(trl)
!
!     GENERATE DATA BLOCK FRL FROM FOL (W = F*2*PI).
!     USE SAME CORE WHERE FOL IS STORED.
!
               DO ifreq = ifreq1 , ifreq2
                  Z(ifreq) = Z(ifreq)*Dtwopi
               ENDDO
!
!     OUTPUT FRL TABLE (FREQUENCY RESPONSE LIST).
!
               file = frl
               CALL fname(file,fnam)
               CALL open(*40,file,Z(ibuf1),1)
               CALL write(file,fnam,2,0)
               CALL write(file,1,1,1)
               CALL write(file,Z(ifol),nfreq,1)
               CALL close(file,1)
!
               trl(1) = file
               trl(2) = 1
               trl(3) = 0
               trl(4) = 0
               trl(5) = 0
               trl(6) = 0
               trl(7) = 0
               CALL wrttrl(trl)
!
!     GENERATE MATRIX DATA BLOCKS REORDER1 AND REORDER2 USED FOR
!     REORDERING COLUMNS OF A MATRIX BY POST-MULTIPLYING THE MATRIX
!     WHOSE COLUMNS ARE TO BE REORDERED.
!
               k1 = Ntstps
               k3 = Flmax
               IF ( Cycio==-1 ) k2 = Fkmax
               IF ( Cycio==+1 ) k2 = Nsegs
!
!     GENERATE MATRIX REORDER1
!
               CALL fvrs2a(reord1,k1,k2,Noro1,Z(ibuf1))
!
!     GENERATE MATRIX REORDER2
!
               CALL fvrs2a(reord2,k2,k3,Noro2,Z(ibuf1))
!
               RETURN
            ENDIF
         ENDIF
!
!     ERROR PROCESSING
!
!     DATA SET NOT IN FIST
!
 40      ip1 = -1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     E-O-F ENCOUNTERED
!
 60      ip1 = -2
         spag_nextblock_1 = 2
      CASE (2)
         CALL mesage(ip1,file,modnam)
         CALL mesage(-37,0,modnam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE fvrst2
