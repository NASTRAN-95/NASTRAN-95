!*==ta1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ta1
   USE c_blank
   USE c_system
   USE c_ta1com
   USE c_two
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: genel
   INTEGER :: j , k
   INTEGER , DIMENSION(7) :: mcb
   EXTERNAL andf , delset , rdtrl , ta1a , ta1b , ta1c , ta1cpd , ta1cps , ta1h
!
! End of declarations rewritten by SPAG
!
!
!     TA1 CONTROLS THE EXECUTION OF THE TABLE ASSEMBLER.
!
!     DMAP CALL IS
!
!     TA1   ECT,EPT,BGPDT,SIL,GPTT,CSTM,MPT,EQEXIN/EST,GEI,GPECT,
!           ECPT,GPCT,MPTX,PCOMPS,EPTX/V,N,LUSET/V,N,NOSIMP=-1/
!           V,N,NOSUP=-1,1,2/V,N,NOGENEL=-1/V,N,GENEL/V,N,COMPS=1 $
!
!
!     EITHER THE GPECT OR BOTH GPECT AND ECPT, GPCT MAY BE GENERATED.
!     IF NOSUP .EQ. 1, GENERATE GPECT. IF NOSUP .EQ. 2 , GENERATE ALL.
!     IF NOSUP .LT. 0, GENERATE NONE.
!
!   1. TA1 EXECUTES TA1A WHICH BUILDS THE ELEMENT SUMMARY TABLE (EST)
!   2. TA1 EXECUTES TA1B WHICH BUILDS THE ELEMENT CONNECTION AND
!      PROPERTIES TABLE (ECPT) AND THE GRID POINT CONNECTION TABLE(GPCT)
!   3. IF GENERAL ELEMENTS ARE PRESENT, TA1 EXECUTES TA1C WHICH BUILDS
!      THE GENERAL ELEMENT INPUT (GEI).
!   4. IF LAMINATED COMPOSITE ELEMENTS ARE PRESENT, TA1 EXECUTES
!      TA1CPS/D WHICH -
!      (1) CREATES PCOMPS DATA, WHICH INCLUDES THE ECHOING OF
!          INTRINSIC LAYER PROPERTIES, AND
!      (2) CALCULATES OVERALL MATERIAL PROPERTIES.
!
!
   DATA genel/4301 , 43/
!
!     INITIALIZE
!
   CALL delset
   ect = 101
   ept = 102
   bgpdt = 103
   sil = 104
   gptt = 105
   cstm = 106
   mpt = 107
   eqexin = 108
!
   est = 201
   gei = 202
   gpect = 203
   ecpt = 204
   gpct = 205
   mptx = 206
   pcomps = 207
   eptx = 208
!
   scr1 = 301
   scr2 = 302
   scr3 = 303
   scr4 = 304
!
!     TEST FOR PRESENCE OF GENERAL ELEMENTS
!
   nogenl = -1
   mcb(1) = ect
   CALL rdtrl(mcb)
   IF ( mcb(1)>=0 ) THEN
      j = (genel(2)-1)/16
      k = genel(2) - 16*j
      IF ( andf(mcb(j+2),two(k+16))/=0 ) nogenl = 1
   ENDIF
!
!     EXECUTE TA1A FOR ALL PROBLEMS
!
   CALL ta1a
!
!     EXECUTE TA1CPD/S TO BUILD PCOMPS DATA
!
   IF ( nosup/=0 ) THEN
      IF ( comps==-1 ) THEN
         IF ( iprec==1 ) CALL ta1cps
         IF ( iprec==2 ) CALL ta1cpd
      ENDIF
      IF ( nosup==1 ) THEN
!
!     CALL TA1H TO GENERATE GPECT
!
         IF ( nosimp>0 ) CALL ta1h
         CALL spag_block_1
         RETURN
      ENDIF
   ENDIF
!
!     EXECUTE TA1B IF SIMPLE ELEMENTS ARE PRESENT
!
   IF ( nosimp>0 ) CALL ta1b
   IF ( nosup/=0 ) THEN
      IF ( nosimp>0 ) CALL ta1h
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
!
!     EXECUTE TA1C IF GENERAL ELEMENTS ARE PRESENT
!
      IF ( Nogenl>0 ) CALL ta1c
      genl = -Nogenl
   END SUBROUTINE spag_block_1
!
END SUBROUTINE ta1
