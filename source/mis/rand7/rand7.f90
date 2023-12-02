!*==rand7.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE rand7(Ifile,Nfile,Psdl,Dit,Icoup,Nfreq,Npsdl,Ntau,Ltab,Casecc,Xycdb)
   IMPLICIT NONE
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Ifile
   INTEGER :: Nfile
   INTEGER :: Psdl
   INTEGER :: Dit
   INTEGER :: Icoup
   INTEGER :: Nfreq
   INTEGER :: Npsdl
   INTEGER :: Ntau
   INTEGER :: Ltab
   INTEGER :: Casecc
   INTEGER :: Xycdb
!
! Local variable declarations rewritten by SPAG
!
   REAL :: f
   INTEGER :: file , i , i10 , i163 , ibuf1 , ip1 , irand , itabl , itau , j , jj , k , l , lcore , len , ntabl
   INTEGER , DIMENSION(6) :: ipsdl
   INTEGER , DIMENSION(7) , SAVE :: itlist
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL , DIMENSION(1) :: z
   EXTERNAL close , fread , gopen , korsz , mesage , open , pretab , read , skprec
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     STORES STUFF IN CORE FOR LATER RANDOM ANALSIS
!
!
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
!
   DATA name/4HRAND , 1H7/
   DATA itlist/2 , 55 , 25 , 1 , 56 , 26 , 5/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
! *****
!     IDENTIFICATION OF VARIABLES
! *****
!     IFILE    ARRAY OF INPUT FILES
!     NFILE    LENGTH OF IFILE ARRAY
!     PSDL     POWER SPECTRAL DENSITY LISTS FROM DPD
!     DIT      DIRECT INPUT TABLES
!     ICOUP    COUPLED,UNCOUPLED, OR NOGO FLAG
!     NFREQ    NUMBER OF FRENQUICIES
!     NPSDL    NUMBER OF PSDL  SETS
!     NTAU     NUMBER OF TAUS
!     LTAB     LENGTH OF DATA FOR TAB ROUTINE
!     CASECC   CASECONTROL FILE
!     SYSBUF   LENGTH OF ONE GINO BUFFER
!     NTABL    NUMBER OF UNIQUE TABLE ID-S
!     ITABL    POINTER TO LIST OF TABLE ID-S
!
!
!     BUILD FREQUENCY LIST
!
         Icoup = 0
         lcore = korsz(Iz)
         ibuf1 = lcore - Sysbuf
!
!     XYCDB MUST BE PRESENT
!
         file = Xycdb
         CALL open(*20,Xycdb,Iz(ibuf1),0)
         CALL close(Xycdb,1)
         lcore = ibuf1 - 1
!
!     EXTRACT  SET NO FROM CASECC
!
         CALL gopen(Casecc,Iz(ibuf1),0)
         CALL fread(Casecc,Iz,166,1)
         i163 = 163
         irand = Iz(i163)
         CALL close(Casecc,1)
         IF ( irand/=0 ) THEN
!
!     FIND DATA FILE
!
            DO i = 1 , Nfile
               file = Ifile(i)
               CALL open(*10,file,Iz(ibuf1),0)
               CALL skprec(file,1)
               CALL fread(file,Iz,10,1)
               i10 = 10
               len = Iz(i10) - 1
               Nfreq = 0
               DO
!
!     EXTRACT FREQUENCIES
!
                  CALL read(*100,*5,file,f,1,0,j)
                  CALL fread(file,Iz,-len,0)
                  Nfreq = Nfreq + 1
                  z(Nfreq) = f
               ENDDO
 5             CALL close(file,1)
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
 10         ENDDO
         ENDIF
!
!     NO DATA FILES--EXIT
!
 20      Icoup = -1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
!
!     BRING IN PSDL CARDS
!
         lcore = lcore - Nfreq
         file = Psdl
         CALL open(*20,Psdl,z(ibuf1),0)
         l = Nfreq + 1
         Npsdl = 0
         itau = -1
         CALL read(*100,*40,Psdl,Iz(Nfreq+1),lcore,0,j)
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 40      k = Nfreq + 3
         IF ( j/=2 ) THEN
            j = k + j - 1
!
!     DETERMINE RECORD THAT RANDOM TAU-S ARE IN
!
            DO i = k , j
               IF ( Iz(i)==irand ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            itau = -1
         ENDIF
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
      CASE (3)
!
!     FOUND RANDT CARDS
!
         itau = i - k
         spag_nextblock_1 = 4
      CASE (4)
         DO
!
!     FIND SELECTED PSDL CARDS
!
            CALL read(*100,*60,Psdl,ipsdl(1),6,0,j)
            IF ( ipsdl(1)==irand ) THEN
               Npsdl = Npsdl + 1
               Iz(l) = ipsdl(2)
               Iz(l+1) = ipsdl(3)
               Iz(l+2) = ipsdl(4)
               Iz(l+3) = ipsdl(5)
               Iz(l+4) = ipsdl(6)
               l = l + 5
            ENDIF
         ENDDO
 60      IF ( Npsdl/=0 ) THEN
!
!     POSITION TAPE FOR TAUS
!
            IF ( itau>0 ) CALL skprec(Psdl,itau)
            lcore = lcore - Npsdl*5
!
!     EXTRACT LIST OF TABLES  AND CHECK FOR COUPLED SYSTEM
!
            jj = Nfreq + 1
            k = Nfreq + 5*Npsdl
            ntabl = 0
            itabl = ibuf1 - 1
            SPAG_Loop_1_1: DO i = jj , k , 5
!
!     COUPLED
!
               IF ( Iz(i)/=Iz(i+1) ) Icoup = 1
               IF ( ntabl/=0 ) THEN
                  DO j = 1 , ntabl
                     l = itabl + j
                     IF ( Iz(l)==Iz(i+4) ) CYCLE SPAG_Loop_1_1
                  ENDDO
               ENDIF
!
!     STORE TABLE ID
!
               ntabl = ntabl + 1
               Iz(itabl) = Iz(i+4)
               itabl = itabl - 1
            ENDDO SPAG_Loop_1_1
            Iz(itabl) = ntabl
!
!     BRING IN  TAU-S
!
            Ntau = 0
            lcore = lcore - ntabl - 1
            IF ( itau/=-1 ) THEN
               CALL read(*80,*80,Psdl,z(k+1),lcore,0,Ntau)
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ELSE
!
!     UNABLE TO FIND SELECTED PSDL CARDS
!
            CALL close(Psdl,1)
            GOTO 20
         ENDIF
 80      CALL close(Psdl,1)
!
!     SETUP FOR TABLES
!
         lcore = lcore - Ntau
         Ltab = 0
         IF ( ntabl/=0 ) THEN
            l = k + Ntau + 1
            CALL pretab(Dit,Iz(l),z(l),Iz(ibuf1),lcore,Ltab,Iz(itabl),itlist(1))
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         RETURN
 100     DO
            ip1 = -2
!
!     FILE ERRORS
!
            CALL mesage(ip1,file,name)
         ENDDO
         spag_nextblock_1 = 6
      CASE (6)
         ip1 = -8
         CALL mesage(ip1,file,name)
         GOTO 100
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE rand7
