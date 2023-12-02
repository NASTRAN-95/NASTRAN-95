!*==emg.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE emg
   USE c_blank
   USE c_emgfil
   USE c_emgprm
   USE c_hmatdd
   USE c_machin
   USE c_system
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ipreci , j , noheat , nout
   INTEGER , DIMENSION(7) :: ibuf , mcb
   INTEGER , DIMENSION(2) , SAVE :: name
   LOGICAL :: nogo
   EXTERNAL emgcng , emgcor , emgfin , emgpro , emgsoc , emgtab , klock , korsz , mesage , rdtrl , sswtch
!
! End of declarations rewritten by SPAG
!
!
!     ELEMENT-MATRIX-GENERATOR MAIN DRIVING ROUTINE.
!
!     DMAP SEQUENCE
!
!     EMG, EST,CSTM,MPT,DIT,GEOM2, /KMAT,KDICT, MMAT,MDICT, BMAT,BDICT/
!          V,N,NOKGG/V,N,NOMGG/V,N,NOBGG/V,N,NOK4GG/V,N,NOKDGG/
!          C,Y,COUPMASS/C,Y,CPBAR/C,Y,CPROD/C,Y,CPQUAD1/C,Y,CPQUAD2/
!          C,Y,CPTRIA1/C,Y,CPTRIA2/C,Y,CPTUBE/C,Y,CQDPLT/C,Y,CPTRPLT/
!          C,Y,CPTRBSC/V,Y,VOLUME/V,Y,SURFACE $
!
   !>>>>EQUIVALENCE (Ksystm(3),Nogo) , (Ksystm(55),Ipreci) , (Ksystm(2),Nout) , (Ksystm(56),Noheat)
   DATA name/4HEMG  , 4H    /
!
!     SET EMG PRECISION FLAG TO SYSTEM PRECISION FLAG
!
   precis = ipreci
!
!     IF .NOT.1 .AND. .NOT.2 DEFAULT EMG PRECISION TO SINGLE
!
   IF ( precis<1 .OR. precis>2 ) precis = 1
!
!     HEAT  FORMULATION
!
   heat = .FALSE.
   IF ( noheat>0 ) THEN
      heat = .TRUE.
      linear = .TRUE.
      nokdgg = -1
   ENDIF
!
!     TEST FOR NO SIMPLE ELEMENTS
!
   nogo = .FALSE.
   mcb(1) = 101
   CALL rdtrl(mcb)
   IF ( mcb(1)>=0 ) THEN
      IF ( mcb(2)/=0 .OR. mcb(5)/=0 .OR. mcb(6)/=0 .OR. mcb(7)/=0 ) THEN
!
!     SET OPEN CORE
!
         ncore = korsz(z(1))
         icore = 3
         IF ( mach==3 .OR. mach==4 ) CALL emgsoc(icore,ncore,heat)
         ncore = ncore - 1
         jcore = icore
!
!     SET WORKING CORE TO ALL ZEROS
!
         DO i = icore , ncore
            z(i) = 0
         ENDDO
!
!     THIS MODULE WILL SET NOK4GG = -1 . IF DURING EXECUTION A NON-ZERO
!     DAMPING CONSTANT IS DETECTED IN A DICTIONARY BY EMGOUT, NOK4GG
!     WILL BE SET TO 1
!
!     A DMAP DETERMINATION CAN THEN BE MADE WHETHER OR NOT TO HAVE EMA
!     FORM THE K4GG MATRIX
!
         nok4gg = -1
!
!     SET GINO FILE NUMBERS
!
         est = 101
         cstm = 102
         mpt = 103
         dit = 104
         geom2 = 105
         DO i = 1 , 3
            mats(i) = 199 + 2*i
            dictn(i) = mats(i) + 1
         ENDDO
         error = .FALSE.
!
!     IF DIAG 38 IS ON, PRINT TOTAL TIME (IN SECONDS) USED BY EMGPRO
!     AND MESSAGES 3113 AND 3107  WHILE PRPCESSING ELEMENTS
!
         CALL sswtch(38,l38)
!
!     READ AND SETUP INTO CORE MISC. TABLES.
!     E.G. MPT, CSTM, DIT, ETC.
!
         CALL emgtab
!
!     PROCESS ANY CONGRUENT DATA CARDS AND BUILD TABLE IN OPEN CORE.
!
         CALL emgcng
!
!     SETUP BALANCE OF CORE WITH REQUIRED BUFFERS AND OPEN
!     REQUIRED DATA BLOCKS.
!
         CALL emgcor(ibuf)
!
!     PASS THE EST AND WRITE THE OUTPUT DATA BLOCKS.
!
         IF ( l38==1 ) CALL klock(i)
         CALL emgpro(ibuf)
         IF ( l38/=0 ) THEN
            CALL klock(j)
            j = j - i
            WRITE (nout,99001) j
99001       FORMAT (///,34H *** EMG ELEMENT PROCESSING TIME =,I10,8H SECONDS)
         ENDIF
!
!     WRAP-UP OPERATIONS.
!
         CALL emgfin
         IF ( nogo .OR. error ) CALL mesage(-37,0,name)
         RETURN
      ENDIF
   ENDIF
   nok = -1
   nom = -1
   nob = -1
   nok4gg = -1
END SUBROUTINE emg
