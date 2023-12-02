!*==gnfiat.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gnfiat
   USE i_nasnames
   USE i_dsiof
   USE i_ginox
   USE c_dsunit
   USE c_system
   USE c_xfiat
   USE c_xpfist
   USE c_xxfiat
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ind , isize , istat , k , nout , numblk
!
! End of declarations rewritten by SPAG
!
!
!
!    FORMAT OF THE MEMBER DATASET FILE CONTROL BLOCK (MDSFCB)
!          (ONE ENTRY FOR EVERY FILE)
!   0             8            16             24                 31
!   ***************************************************************
! 1 *                      OPEN FLAG                              *
!   ***************************************************************
! 2 *                     CURRENT  DSN                            *
!   ***************************************************************
! 3 *        PREVIOUS DSN       *          NEXT DSN               *
!   ***************************************************************
!
!   FORMAT OF THE FCB
!   ***************************************************************
! 1 *                    OPEN FLAG (0 - READ, 1 - WRITE )         *
!   ***************************************************************
! 2 *                   BUFFER ADDRESS                            *
!   ***************************************************************
! 3 *               CURRENT LOGICAL RECORD (CLR)                  *
!   ***************************************************************
! 4 *                 CURRENT BLOCK NUMBER                        *
!   ***************************************************************
! 5 *              FIRST BLOCK NUMBER ON EXTERNAL FILE            *
!   ***************************************************************
! 6 *               LAST BLOCK NUMBER ON EXTERNAL FILE            *
!   ***************************************************************
! 7 *          NUMBER OF BLOCKS ALLOCATED TO THIS FILE            *
!   ***************************************************************
! 8 *     FLAG FOR WRITING THE FIRST COLUMN ON FILE (0-NO, 1=YES) *
!   ***************************************************************
! 9 *              INDEX TO FIRST IN-MEMORY BLOCK                 *
!   ***************************************************************
!10 *               INDEX TO LAST IN-MEMORY BLOCK                 *
!   ***************************************************************
!11 *             INDEX TO CURRENT IN-MEMORY BLOCK                *
!   ***************************************************************
!12 *            ORIGINAL BUFFER ADDRESS (ON OPEN)                *
!   ***************************************************************
!13 *                    DMAP FILE NAME                           *
!14 *                                                             *
!   ***************************************************************
!15 *               OPEN FLAG FOR EXTERNAL FILE                   *
!   ***************************************************************
!16 *           TOTAL NUMBER OF STRINGS IN THIS MATRIX            *
!   ***************************************************************
!17 *             TOTAL NUMBER OF TERMS IN THIS MATRIX            *
!   ***************************************************************
!
!
!
!
!                          I/O BUFFER FORMAT
!   ***************************************************************
! 1 *                       DMAP FILE NAME                        *
!   ***************************************************************
! 2 *                             CBP                             *
!   ***************************************************************
! 3 *                             CLR                             *
!   ***************************************************************
! 4 *                        BLOCK NUMBER                         *
!   ***************************************************************
! 5 *                             LCW                             *
!   ***************************************************************
! 6 *          I/O BUFFER (4 THRU NBUFF+3 ARE WRITTEN)            *
!   ***************************************************************
!   *                                                             *
!   ***************************************************************
!
!
!
!
!                        I/O BUFFER CONTROL WORDS
!  DEFINITION WORD        0         8        16         24       31
!                         *****************************************
!   RECORD HEADER         *  '11'   *  FLAG   *  NUMBER OF WORDS  *
!                         *****************************************
!   RECORD TRAILER        *  '77'   *  FLAG   *        CLR        *
!                         *****************************************
!   STRING DATA           *  '22'   *  FLAG   *  NUMBER OF WORDS  *
!                         *****************************************
!   EOB STRING            *  '7F'   *  FLAG   *                   *
!                         *****************************************
!   COLUMN HEADER         *  '3B'   *         *  FORMAT  *  TYPE  *
!                         *****************************************
!                         *            COLUMN NUMBER              *
!                         *****************************************
!   COLUMN TRAILER        *  '3F'   *         *  FORMAT  *  TYPE  *
!                         *****************************************
!                         *            COLUMN NUMBER              *
!                         *****************************************
!   STRING HEADER         *  '4B'   *         *  NUMBER OF TERMS  *
!                         *****************************************
!                         *             ROW NUMBER                *
!                         *****************************************
!   STRING TRAILER        *  '4E'   *         *  NUMBER OF TERMS  *
!                         *****************************************
!                         *             ROW NUMBER                *
!                         *****************************************
!   DUMMY STRING          *  'DD'   *                             *
!                         *****************************************
!   END OF BLOCK          *  'EB'   *                             *
!                         *****************************************
!                         *  'EF'   *                             *
!                         *****************************************
!
!          FLAG   =  C-COMPLETE, E-EXTENDED, F-FURTHER EXTENDED
!          TYPE   =  1-RSP, 2-RDP, 3-CSP, 4-CDP
!          FORMAT =  1-TRAILERS, 0-NO TRAILERS
!
!    IPERM OF /SYSTEM/ HAS BITS DESIGNATED FOR THE FOLLOWING FILES
!
!               BIT                     FILE
!               7                       INPT
!               8-16                    INP1-INP9
!
! //////////////////////////////////////////////////////////////////
!
!     PERMANENT FILES IN /XXFIAT/ ARE ALLOCATED ACCORDING TO THE
!     FOLLOWING:
!
!       XFIAT(1) = UNIT FOR POOL = 22
!       XFIAT(2) = UNIT FOR OPTP = 7
!       XFIAT(3) = UNIT FOR NPTP = 8
!       XFIAT(8) = UNIT FOR INPT = 16
!       XFIAT(9) = UNIT FOR INP1 = 17
!       XFIAT(10)= UNIT FOR INP2 = 18
!       XFIAT(11)= UNIT FOR INP3 = 19
!       XFIAT(12)= UNIT FOR INP4 = 20
!       XFIAT(13)= UNIT FOR INP5 = 21
!       XFIAT(18)= UNIT FOR XPTD = 9
!
!              FORTRAN UNITS ARE ASSIGNED AS FOLLOWS:
!
!                 PUNCH = 1
!                 LINK  = 2
!                 LOG   = 3
!                 RDICT = 4
!                 INPUT = 5
!                 OUTPUT= 6
!                 PLOT  = 10
!                 UT1   = 11
!                 UT2   = 12
!                 UT3   = 13
!                 UT4   = 14
!                 UT5   = 15
!                 SOF   = 90
! /////////////////////////////////////////////////////////////////
!
!
   !>>>>EQUIVALENCE (Dum1(1),Nout)
!
   CALL dsiodd
   ifufa = 0
   idslim = inmblk
   numblk = 1
   IF ( lenwpb/=0 ) numblk = isysbf/lenwpb
   DO i = 1 , numsof
      lensof(i) = 0
   ENDDO
   DO i = 1 , maxfcb
      mdsfcb(1,i) = 0
      mdsfcb(2,i) = 0
      mdsfcb(3,i) = 0
   ENDDO
   DO i = 1 , maxfcb
      DO k = 1 , 17
         fcb(k,i) = 0
      ENDDO
      fcb(7,i) = 20000000
   ENDDO
   DO i = 1 , 220
      iunit(i) = 0
   ENDDO
   IF ( andf(4,iperm)/=0 ) mdsnam(8) = nptp
   mdsnam(7) = optp
   DO i = 1 , npfist
      xfiat(i) = 4095
   ENDDO
   DO i = 7 , 22
      IF ( dsnames(i)/='none' ) THEN
         IF ( dsnames(i)/='NONE' ) THEN
            CALL dsinqr(dsnames(i),istat,isize)
            IF ( istat/=0 ) THEN
               fcb(3,i) = 6
               fcb(4,i) = 1
               fcb(5,i) = 1
               fcb(6,i) = fcb(7,i)
               IF ( i==7 ) xfiat(2) = 7
            ENDIF
         ENDIF
      ENDIF
   ENDDO
   DO i = 23 , maxpri
      ifufa = ifufa + 1
      ind = ifufa*11 - 10
      fiat(ind) = i
   ENDDO
   xfiat(1) = 22
   xfiat(3) = 8
   xfiat(8) = 16
   xfiat(9) = 17
   xfiat(10) = 18
   xfiat(11) = 19
   xfiat(12) = 20
   xfiat(13) = 21
   xfiat(18) = 9
   ifcae = ifufa
END SUBROUTINE gnfiat
