
SUBROUTINE rand7(Ifile,Nfile,Psdl,Dit,Icoup,Nfreq,Npsdl,Ntau,Ltab,Casecc,Xycdb)
   IMPLICIT NONE
   INTEGER Iz(1) , Sysbuf
   REAL Z(1)
   COMMON /system/ Sysbuf
   COMMON /zzzzzz/ Iz
   INTEGER Casecc , Dit , Icoup , Ltab , Nfile , Nfreq , Npsdl , Ntau , Psdl , Xycdb
   INTEGER Ifile(1)
   REAL f
   INTEGER file , i , i10 , i163 , ibuf1 , ip1 , ipsdl(6) , irand , itabl , itau , itlist(7) , j , jj , k , l , lcore , len ,       &
         & name(2) , ntabl
   INTEGER korsz
!
!     STORES STUFF IN CORE FOR LATER RANDOM ANALSIS
!
!
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
!
   DATA name/4HRAND , 1H7/
   DATA itlist/2 , 55 , 25 , 1 , 56 , 26 , 5/
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
   CALL open(*100,Xycdb,Iz(ibuf1),0)
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
         CALL open(*50,file,Iz(ibuf1),0)
         CALL skprec(file,1)
         CALL fread(file,Iz,10,1)
         i10 = 10
         len = Iz(i10) - 1
         Nfreq = 0
         DO
!
!     EXTRACT FREQUENCIES
!
            CALL read(*900,*20,file,f,1,0,j)
            CALL fread(file,Iz,-len,0)
            Nfreq = Nfreq + 1
            Z(Nfreq) = f
         ENDDO
 20      CALL close(file,1)
         GOTO 200
 50   ENDDO
   ENDIF
!
!     NO DATA FILES--EXIT
!
 100  Icoup = -1
   GOTO 800
!
!     BRING IN PSDL CARDS
!
 200  lcore = lcore - Nfreq
   file = Psdl
   CALL open(*100,Psdl,Z(ibuf1),0)
   l = Nfreq + 1
   Npsdl = 0
   itau = -1
   CALL read(*900,*300,Psdl,Iz(Nfreq+1),lcore,0,j)
   GOTO 1000
 300  k = Nfreq + 3
   IF ( j/=2 ) THEN
      j = k + j - 1
!
!     DETERMINE RECORD THAT RANDOM TAU-S ARE IN
!
      DO i = k , j
         IF ( Iz(i)==irand ) GOTO 400
      ENDDO
      itau = -1
   ENDIF
   GOTO 500
!
!     FOUND RANDT CARDS
!
 400  itau = i - k
 500  DO
!
!     FIND SELECTED PSDL CARDS
!
      CALL read(*900,*600,Psdl,ipsdl(1),6,0,j)
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
 600  IF ( Npsdl/=0 ) THEN
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
      DO i = jj , k , 5
!
!     COUPLED
!
         IF ( Iz(i)/=Iz(i+1) ) Icoup = 1
         IF ( ntabl/=0 ) THEN
            DO j = 1 , ntabl
               l = itabl + j
               IF ( Iz(l)==Iz(i+4) ) GOTO 650
            ENDDO
         ENDIF
!
!     STORE TABLE ID
!
         ntabl = ntabl + 1
         Iz(itabl) = Iz(i+4)
         itabl = itabl - 1
 650  ENDDO
      Iz(itabl) = ntabl
!
!     BRING IN  TAU-S
!
      Ntau = 0
      lcore = lcore - ntabl - 1
      IF ( itau/=-1 ) THEN
         CALL read(*700,*700,Psdl,Z(k+1),lcore,0,Ntau)
         GOTO 1000
      ENDIF
   ELSE
!
!     UNABLE TO FIND SELECTED PSDL CARDS
!
      CALL close(Psdl,1)
      GOTO 100
   ENDIF
 700  CALL close(Psdl,1)
!
!     SETUP FOR TABLES
!
   lcore = lcore - Ntau
   Ltab = 0
   IF ( ntabl/=0 ) THEN
      l = k + Ntau + 1
      CALL pretab(Dit,Iz(l),Z(l),Iz(ibuf1),lcore,Ltab,Iz(itabl),itlist(1))
   ENDIF
 800  RETURN
 900  DO
      ip1 = -2
!
!     FILE ERRORS
!
      CALL mesage(ip1,file,name)
   ENDDO
 1000 ip1 = -8
   CALL mesage(ip1,file,name)
   GOTO 900
END SUBROUTINE rand7