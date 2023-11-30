
SUBROUTINE ta1
   IMPLICIT NONE
   INTEGER Bgpdt , Comps , Cstm , Ecpt , Ect , Ept , Eptx , Eqexin , Est , Gei , Genl , Gpct , Gpect , Gptt , Iprec , Isystm(54) ,  &
         & Luset , Mpt , Mptx , Nogenl , Nosimp , Nosup , Nsil , Pcomps , Scr1 , Scr2 , Scr3 , Scr4 , Sil , Two(32)
   COMMON /blank / Luset , Nosimp , Nosup , Nogenl , Genl , Comps
   COMMON /system/ Isystm , Iprec
   COMMON /ta1com/ Nsil , Ect , Ept , Bgpdt , Sil , Gptt , Cstm , Mpt , Est , Gei , Gpect , Ecpt , Gpct , Mptx , Pcomps , Eptx ,    &
                 & Scr1 , Scr2 , Scr3 , Scr4 , Eqexin
   COMMON /two   / Two
   INTEGER andf
   INTEGER genel(2) , j , k , mcb(7)
   EXTERNAL andf
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
   Ect = 101
   Ept = 102
   Bgpdt = 103
   Sil = 104
   Gptt = 105
   Cstm = 106
   Mpt = 107
   Eqexin = 108
!
   Est = 201
   Gei = 202
   Gpect = 203
   Ecpt = 204
   Gpct = 205
   Mptx = 206
   Pcomps = 207
   Eptx = 208
!
   Scr1 = 301
   Scr2 = 302
   Scr3 = 303
   Scr4 = 304
!
!     TEST FOR PRESENCE OF GENERAL ELEMENTS
!
   Nogenl = -1
   mcb(1) = Ect
   CALL rdtrl(mcb)
   IF ( mcb(1)>=0 ) THEN
      j = (genel(2)-1)/16
      k = genel(2) - 16*j
      IF ( andf(mcb(j+2),Two(k+16))/=0 ) Nogenl = 1
   ENDIF
!
!     EXECUTE TA1A FOR ALL PROBLEMS
!
   CALL ta1a
!
!     EXECUTE TA1CPD/S TO BUILD PCOMPS DATA
!
   IF ( Nosup/=0 ) THEN
      IF ( Comps==-1 ) THEN
         IF ( Iprec==1 ) CALL ta1cps
         IF ( Iprec==2 ) CALL ta1cpd
      ENDIF
      IF ( Nosup==1 ) THEN
!
!     CALL TA1H TO GENERATE GPECT
!
         IF ( Nosimp>0 ) CALL ta1h
         GOTO 100
      ENDIF
   ENDIF
!
!     EXECUTE TA1B IF SIMPLE ELEMENTS ARE PRESENT
!
   IF ( Nosimp>0 ) CALL ta1b
   IF ( Nosup/=0 ) THEN
      IF ( Nosimp>0 ) CALL ta1h
   ENDIF
!
!     EXECUTE TA1C IF GENERAL ELEMENTS ARE PRESENT
!
 100  IF ( Nogenl>0 ) CALL ta1c
   Genl = -Nogenl
!
END SUBROUTINE ta1
