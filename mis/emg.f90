
SUBROUTINE emg
   IMPLICIT NONE
   LOGICAL Anycon , Error , Heat , Linear , Nogo
   INTEGER Cmass , Cstm , Dictn(3) , Dit , Est , Flags(3) , Geom2 , Icmbar , Icong , Icore , Icstm , Idit , Ihmat , Imat , Ipreci , &
         & Jcore , Kflags(3) , Ksystm(65) , L38 , Lcong , Lcstm , Lhmat , Lmat , Mach , Mats(3) , Mpt , Ncong , Ncore , Ncstm ,     &
         & Ndit , Nhmat , Nmat , Nob , Noheat , Nok , Nok4gg , Nokdgg , Nom , Nout , Precis , Z(1)
   REAL Skp(4)
   COMMON /blank / Nok , Nom , Nob , Nok4gg , Nokdgg , Cmass
   COMMON /emgfil/ Est , Cstm , Mpt , Dit , Geom2 , Mats , Dictn
   COMMON /emgprm/ Icore , Jcore , Ncore , Icstm , Ncstm , Imat , Nmat , Ihmat , Nhmat , Idit , Ndit , Icong , Ncong , Lcong ,      &
                 & Anycon , Flags , Precis , Error , Heat , Icmbar , Lcstm , Lmat , Lhmat , Kflags , L38
   COMMON /hmatdd/ Skp , Linear
   COMMON /machin/ Mach
   COMMON /system/ Ksystm
   COMMON /zzzzzz/ Z
   INTEGER i , ibuf(7) , j , mcb(7) , name(2)
   INTEGER korsz
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
   Precis = Ipreci
!
!     IF .NOT.1 .AND. .NOT.2 DEFAULT EMG PRECISION TO SINGLE
!
   IF ( Precis<1 .OR. Precis>2 ) Precis = 1
!
!     HEAT  FORMULATION
!
   Heat = .FALSE.
   IF ( Noheat>0 ) THEN
      Heat = .TRUE.
      Linear = .TRUE.
      Nokdgg = -1
   ENDIF
!
!     TEST FOR NO SIMPLE ELEMENTS
!
   Nogo = .FALSE.
   mcb(1) = 101
   CALL rdtrl(mcb)
   IF ( mcb(1)>=0 ) THEN
      IF ( mcb(2)/=0 .OR. mcb(5)/=0 .OR. mcb(6)/=0 .OR. mcb(7)/=0 ) THEN
!
!     SET OPEN CORE
!
         Ncore = korsz(Z(1))
         Icore = 3
         IF ( Mach==3 .OR. Mach==4 ) CALL emgsoc(Icore,Ncore,Heat)
         Ncore = Ncore - 1
         Jcore = Icore
!
!     SET WORKING CORE TO ALL ZEROS
!
         DO i = Icore , Ncore
            Z(i) = 0
         ENDDO
!
!     THIS MODULE WILL SET NOK4GG = -1 . IF DURING EXECUTION A NON-ZERO
!     DAMPING CONSTANT IS DETECTED IN A DICTIONARY BY EMGOUT, NOK4GG
!     WILL BE SET TO 1
!
!     A DMAP DETERMINATION CAN THEN BE MADE WHETHER OR NOT TO HAVE EMA
!     FORM THE K4GG MATRIX
!
         Nok4gg = -1
!
!     SET GINO FILE NUMBERS
!
         Est = 101
         Cstm = 102
         Mpt = 103
         Dit = 104
         Geom2 = 105
         DO i = 1 , 3
            Mats(i) = 199 + 2*i
            Dictn(i) = Mats(i) + 1
         ENDDO
         Error = .FALSE.
!
!     IF DIAG 38 IS ON, PRINT TOTAL TIME (IN SECONDS) USED BY EMGPRO
!     AND MESSAGES 3113 AND 3107  WHILE PRPCESSING ELEMENTS
!
         CALL sswtch(38,L38)
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
         IF ( L38==1 ) CALL klock(i)
         CALL emgpro(ibuf)
         IF ( L38/=0 ) THEN
            CALL klock(j)
            j = j - i
            WRITE (Nout,99001) j
99001       FORMAT (///,34H *** EMG ELEMENT PROCESSING TIME =,I10,8H SECONDS)
         ENDIF
!
!     WRAP-UP OPERATIONS.
!
         CALL emgfin
         IF ( Nogo .OR. Error ) CALL mesage(-37,0,name)
         GOTO 99999
      ENDIF
   ENDIF
   Nok = -1
   Nom = -1
   Nob = -1
   Nok4gg = -1
   RETURN
99999 RETURN
END SUBROUTINE emg