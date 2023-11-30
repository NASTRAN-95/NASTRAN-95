
SUBROUTINE bread(Ig,Inv,Ii3,Norig,Kg)
!
!      THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!      IT READS THE CONNECTING ELEMENTS AND GENEL ELEM. FROM GEOM2 FILE
!      AND PREPROCESS THE MPC CARDS AND THE RIGID ELEMENTS FROM GEOM4
!
!      REVISED BY G.CHAN/UNISYS
!      12/89, TO INCLUDE NEW RIGID ELEMENTS CRROD, CRBAR, CRTRPLT,
!      CRBE1, CREB2, CRBE3 AND CRSPLINE
!      03/92, TO INCLUDE DUMMY ELEMENTS, CDUM1,...,CDUM9
!
   IMPLICIT NONE
   INTEGER Dum43(43) , Dum6(6) , Geom1 , Geom2 , Geom4 , Ibuf , Ibuf1 , Ifl , Incr , Ipnw(2) , Kdim , Kdum(9) , Ke(1) , Kore ,      &
         & Last , Nbitin , Ne , Nel , Neq , Neqr , Ngrid , Nn(10) , Nompc , Nout , Rd , Rdrew , Rew , Scr1 , Wrt , Wrtrew , Z(1)
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /banda / Ibuf1 , Nompc
   COMMON /bandb / Nbitin , Kore , Ifl , Ngrid , Ipnw , Kdim
   COMMON /bandd / Dum6 , Nel , Neq , Neqr
   COMMON /bands / Nn
   COMMON /geomx / Geom1 , Geom2 , Geom4 , Scr1
   COMMON /gpta1 / Ne , Last , Incr , Ke
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew
   COMMON /system/ Ibuf , Nout , Dum43 , Kdum
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Z
   INTEGER Ii3
   INTEGER Ig(1) , Inv(Ii3,1) , Kg(7) , Norig(1)
   INTEGER chbdy , crbar , crbe1 , crbe2 , crbe3 , crigd1 , crigd2 , crigd3 , crigdr , crrod , crspln , crtrpt , genel , i , ibuf2 ,&
         & ielem , ifile , irtn , iz(3) , iz2 , j , k , kdim4 , kgpv , l , m , maxmpc , mpc , mset , ncon , ngpt1 , ngpt2 , ngpts , &
         & ntot , nwds , plotel , scalar , sub(2) , xxx(3)
   LOGICAL debug
   DATA crigdr , crigd1 , crigd2 , crigd3 , genel/8210 , 5310 , 5410 , 8310 , 4301/
   DATA chbdy , plotel , crrod , crbar , crtrpt/4208 , 5201 , 6510 , 6610 , 6710/
   DATA crbe1 , crbe2 , crbe3 , crspln , mset/6810 , 6910 , 7010 , 7110 , 4HMSET/
   DATA sub , mpc , maxmpc , debug/4HBREA , 4HD    , 4901 , 150 , .FALSE./
!
!
!     CHECK THE PRESENCE OF GEOM2 FILE
!
   Kg(1) = Geom2
   CALL rdtrl(Kg(1))
   j = Kg(2) + Kg(3) + Kg(4) + Kg(5) + Kg(6) + Kg(7)
   IF ( Kg(1)<0 .OR. j==0 ) GOTO 3700
   DO i = 1 , 7
      Kg(i) = 0
   ENDDO
!
!     UPDATE /GPTA1/ IF DUMMY ELEMENTS ARE PRESENT
!
   DO i = 1 , 9
      IF ( Kdum(i)/=0 ) THEN
         k = Kdum(i)/10000000
         l = (Kdum(i)-k*10000000)/10000
         j = (i+51)*Incr
         Ke(j+6) = 2 + k + l
         Ke(j+10) = k
      ENDIF
   ENDDO
!
!     CHECK THE PRESENCE OF MPC CARDS AND RIGID ELEMENTS.  SAVE THEIR
!     GRID DATA IN SCR1 FILE FOR TIGER AND UPDATE NEQ AND NEQR COUNTERS
!
   IF ( Nompc==0 ) GOTO 2500
   Z(1) = Geom4
   CALL rdtrl(Z(1))
   j = 0
   DO i = 2 , 7
      j = j + Z(i)
   ENDDO
   IF ( Z(1)<0 .OR. j==0 ) GOTO 2500
!
   ibuf2 = Ibuf1 - Ibuf
   CALL open(*3200,Scr1,Z(ibuf2),Wrtrew)
   ifile = Geom4
   CALL preloc(*2400,Z(Ibuf1),Geom4)
!
   IF ( Nompc==1 ) GOTO 200
!
   xxx(1) = mpc
   xxx(2) = xxx(1)/100
   CALL locate(*200,Z(Ibuf1),xxx,j)
 100  j = 1
   CALL read(*3300,*200,Geom4,iz,1,0,m)
   DO
      j = j + 1
      CALL read(*3300,*200,Geom4,Kg(j),3,0,m)
      IF ( Kg(j)==-1 ) THEN
         j = j - 1
         Kg(1) = j - 1
         CALL write(Scr1,Kg,j,1)
         Neq = Neq + 1
         GOTO 100
      ELSEIF ( j+3>maxmpc ) THEN
         GOTO 3400
      ENDIF
   ENDDO
!
!     LOCATE ANY CRIGDR AND CRROD ELEMENTS, AND SAVE THE GRID DATA IN
!     SCR1. (DEPENDENT GRID FIRST, AND ONE INDEPENDENT GRID LAST)
!
!     FOR ALL RIGID ELEMENTS, THE FIRST WORD OF KG ARRAY CONTAINS
!     (NO. OF DEPENDENT + INDEP. GRIDS)*1000 + (NO. OF INDEP. GRIDS)
!     THE DATA IN SCR1 WILL BE PROCESSED BY TIGER
!
 200  IF ( Nompc==3 ) GOTO 2300
   xxx(1) = crigdr
 300  xxx(2) = xxx(1)/100
   CALL locate(*400,Z(Ibuf1),xxx,j)
   DO
      CALL read(*3300,*400,Geom4,iz,1,0,m)
      CALL read(*3300,*400,Geom4,Kg(3),3,0,m)
      Kg(1) = 2*1000 + 1
      Kg(2) = Kg(4)
      CALL write(Scr1,Kg,3,1)
      Neqr = Neqr + 1
   ENDDO
!
 400  IF ( xxx(1)==crrod ) THEN
!
!     LOCATE ANY CRIGD1, CRIGD2  AND CRBE2  ELEMENTS, AND SAVE GRID
!     DATA IN SCR1. PUT THE ONE INDEPENDENT GRID LAST
!
      xxx(1) = crigd1
   ELSE
      xxx(1) = crrod
      GOTO 300
   ENDIF
 500  xxx(2) = xxx(1)/100
   CALL locate(*700,Z(Ibuf1),xxx,j)
 600  j = 1
   CALL read(*3300,*700,Geom4,iz,2,0,m)
   iz2 = iz(2)
   DO
      j = j + 1
      CALL read(*3300,*700,Geom4,Kg(j),1,0,m)
      CALL read(*3300,*700,Geom4,0,-6,0,m)
      IF ( Kg(j)==-1 ) THEN
         Kg(j) = iz2
         Kg(1) = (j-1)*1000 + 1
         CALL write(Scr1,Kg,j,1)
         Neqr = Neqr + 1
         GOTO 600
      ELSEIF ( j>maxmpc ) THEN
         GOTO 3400
      ENDIF
   ENDDO
 700  IF ( xxx(1)==crbe2 ) THEN
!
!     LOCATE ANY CRIGD3, CRBE1, CRBAR AND CRTRPLT ELEMENTS, AND SAVE
!     GRID DATA IN SCR1 FILE. PUT THE INDEPENDENT GRID LAST
!
      xxx(1) = crbar
      ASSIGN 800 TO irtn
      GOTO 1100
   ELSE
!
!     LOCATE ANY CRIGD2 ELEMENT
!
      IF ( xxx(1)==crigd2 ) THEN
!
!     LOCATE ANY CRBE2 ELEMENT
!
         xxx(1) = crbe2
      ELSE
         xxx(1) = crigd2
      ENDIF
      GOTO 500
   ENDIF
 800  xxx(1) = crtrpt
   ASSIGN 900 TO irtn
   GOTO 1100
 900  xxx(1) = crbe1
   ASSIGN 1000 TO irtn
   GOTO 1100
 1000 xxx(1) = crigd3
   ASSIGN 1400 TO irtn
 1100 xxx(2) = xxx(1)/100
   CALL locate(*1300,Z(Ibuf1),xxx,j)
 1200 j = 2
   k = 1
   CALL read(*3300,*1300,Geom4,iz,1,0,m)
   DO
      CALL read(*3300,*1300,Geom4,iz(k),1,0,m)
      IF ( iz(k)==mset ) THEN
         DO
            CALL read(*3300,*1300,Geom4,Kg(j),1,0,m)
            CALL read(*3300,*1300,Geom4,0,-6,0,m)
            IF ( Kg(j)==-1 ) THEN
               k = k - 1
               DO i = 1 , k
                  Kg(j) = iz(i)
                  j = j + 1
               ENDDO
               j = j - 1
               Kg(1) = (j-1)*1000 + k
               CALL write(Scr1,Kg,j,1)
               Neqr = Neqr + 1
               GOTO 1200
            ELSE
               j = j + 1
               IF ( j>maxmpc ) GOTO 3400
            ENDIF
         ENDDO
      ELSE
         CALL read(*3300,*1300,Geom4,0,-6,0,m)
         k = k + 1
         IF ( k>999 ) GOTO 3500
      ENDIF
   ENDDO
!
!     LOCATE ANY CRSPLINE ELEMENTS, AND SAVE GRID DATA IN SCR1 FILE.
!     PUT THE INDEPENDENT GRIDS LAST
 1300 GOTO irtn
!
!     LOCATE ANY CRBE3 ELEMENTS, AND SAVE GRID DATA IN SCR1 FILE. PUT
!     THE INDEPENDENT GRID LAST
!
 1400 xxx(1) = crbe3
   xxx(2) = xxx(1)/100
   CALL locate(*1900,Z(Ibuf1),xxx,j)
 1500 CALL read(*3300,*1900,Geom4,iz,3,0,m)
   iz2 = iz(2)
   j = 2
   CALL read(*3300,*1900,Geom4,0,-2,0,m)
 1600 DO
      CALL read(*3300,*1900,Geom4,Kg(j),1,0,m)
      k = -Kg(j)
      IF ( k>0 ) THEN
         IF ( k==1 ) THEN
            CALL read(*3300,*1900,Geom4,i,1,0,m)
            IF ( i==-2 ) GOTO 1700
            CALL read(*3300,*1900,Geom4,0,-1,0,m)
            CYCLE
         ELSEIF ( k==2 ) THEN
            GOTO 1700
         ELSEIF ( k==3 ) THEN
            GOTO 1800
         ENDIF
      ENDIF
      EXIT
   ENDDO
   j = j + 1
   IF ( j>maxmpc ) GOTO 3400
   GOTO 1600
 1700 DO
      CALL read(*3300,*1900,Geom4,Kg(j),1,0,m)
      IF ( Kg(j)<0 ) EXIT
      CALL read(*3300,*1900,Geom4,0,-1,0,m)
      j = j + 1
   ENDDO
 1800 Kg(j) = iz2
   Kg(1) = (j-1)*1000 + 1
   CALL write(Scr1,Kg,j,1)
   Neqr = Neqr + 1
   GOTO 1500
!
!     LOCATE ANY CRSPLINE ELEMENTS, AND SAVE GRID DATA IN SCR1 FILE.
!     PUT THE INDEPENDENT GRIDS LAST
!
 1900 xxx(1) = crspln
   xxx(2) = xxx(1)/100
   CALL locate(*2300,Z(Ibuf1),xxx,j)
 2000 CALL read(*3300,*2300,Geom4,iz,3,0,m)
   k = 1
   iz(k) = iz(3)
   j = 1
 2100 j = j + 1
   DO
      CALL read(*3300,*2200,Geom4,Kg(j),2,0,m)
      IF ( Kg(j)==-1 ) EXIT
      IF ( j+2>maxmpc ) GOTO 3400
      IF ( Kg(j+1)/=0 ) GOTO 2100
      k = k + 1
      IF ( k>999 ) GOTO 3500
      iz(k) = Kg(j)
   ENDDO
 2200 DO i = 1 , k
      Kg(j) = iz(i)
      j = j + 1
   ENDDO
   j = j - 1
   Kg(1) = (j-1)*1000 + k
   CALL write(Scr1,Kg,j,1)
   Neqr = Neqr + 1
   GOTO 2000
!
 2300 DO k = 1 , maxmpc
      Kg(k) = 0
   ENDDO
 2400 CALL close(Geom4,Rew)
   CALL close(Scr1,Rew)
!
!     PROCESS ELEMENT CARDS AND FILL UP CONNECTION TABLE IG
!
 2500 ifile = Geom2
   CALL preloc(*3300,Z(Ibuf1),Geom2)
   ielem = 1 - Incr
 2600 DO
      ielem = ielem + Incr
      IF ( ielem>Last ) THEN
!
!     SPECIAL TREATMENT FOR GENERAL ELEM.
!     (LIMITED TO KDIM*4 GRID POINTS PER GENEL)
!
         xxx(1) = genel
         xxx(2) = xxx(1)/100
         CALL locate(*3100,Z(Ibuf1),xxx,j)
         kdim4 = Kdim*4
         GOTO 2900
      ELSEIF ( Ke(ielem+3)/=chbdy ) THEN
         IF ( Ke(ielem+3)/=plotel ) THEN
            scalar = Ke(ielem+10)
            IF ( scalar/=-1 ) THEN
               CALL locate(*2600,Z(Ibuf1),Ke(ielem+3),j)
               nwds = Ke(ielem+5)
               ngpts = Ke(ielem+9)
               ngpt1 = Ke(ielem+12)
               ncon = ngpts
               EXIT
            ENDIF
         ENDIF
      ENDIF
   ENDDO
 2700 DO
      CALL read(*3300,*2600,Geom2,Kg(1),nwds,0,m)
      IF ( scalar==0 ) EXIT
      IF ( Kg(5)/=0 .AND. Kg(6)/=0 ) EXIT
   ENDDO
!     THE ABOVE CONDITIONS HOLD TRUE FOR CDAMPI, CELASI, AND CMASSI
!     WHERE I = 1,2
 2800 Nel = Nel + 1
   CALL scat(Kg(ngpt1),ncon,Inv,Ii3,Norig)
   IF ( Ngrid==-1 ) GOTO 3100
   IF ( ncon>1 ) THEN
      ngpt2 = ngpt1 + ncon - 1
      k = ngpt2 - 1
      DO i = ngpt1 , k
         l = i + 1
         DO j = l , ngpt2
            CALL setig(Kg(i),Kg(j),Ig,Norig)
         ENDDO
      ENDDO
   ENDIF
   IF ( ielem<=Last ) GOTO 2700
 2900 ntot = 0
   CALL read(*3300,*3100,Geom2,k,1,0,m)
   k = 0
   kgpv = 0
 3000 ntot = ntot + 1
   IF ( ntot<kdim4 ) ncon = ntot
   DO
      CALL read(*3300,*3100,Geom2,Kg(ncon),2,0,m)
      IF ( Kg(ncon)==-1 ) THEN
!                                           GRD  SCALAR GRD
!                                           PT.   PT.   PT.
         k = k + 1
         xxx(k) = Kg(ncon+1)
         IF ( k>=2 ) THEN
            ncon = ncon - 1
            m = xxx(1)
            nwds = 1 + (m*m-m)/2 + m
            CALL read(*3300,*3100,Geom2,k,-nwds,0,m)
            CALL read(*3300,*3100,Geom2,k,1,0,m)
            ngpt1 = 1
            IF ( k/=0 ) THEN
               nwds = m*xxx(2)
               CALL read(*3300,*3100,Geom2,k,-nwds,0,m)
            ENDIF
            GOTO 2800
         ENDIF
      ELSEIF ( Kg(ncon+1)/=0 ) THEN
         IF ( Kg(ncon)/=kgpv ) THEN
            kgpv = Kg(ncon)
            GOTO 3000
         ENDIF
      ENDIF
   ENDDO
 3100 CALL close(Geom2,Rew)
   IF ( ntot>kdim4 ) THEN
      WRITE (Nout,99001) Ufm , ntot
99001 FORMAT (A23,', GENEL ELEMENT HAS TOO MANY GRID POINTS,',I7)
      j = ntot/400 + 1
      IF ( j<=9 ) WRITE (Nout,99002) j
99002 FORMAT (5X,'USER NEEDS TO ADD A ''NASTRAN BANDTDIM=',I1,''' CARD AND RERUN JOB')
      GOTO 3600
   ELSE
      IF ( .NOT.debug ) RETURN
!
      m = Nn(1)
      WRITE (Nout,99003) Nn
99003 FORMAT (//21H /BANDS/ FROM BREAD =,10I8)
      WRITE (Nout,99004) ((Inv(i,j),j=1,2),i=1,m)
99004 FORMAT (/12H TABLE INV =,(/10X,2I8))
      RETURN
   ENDIF
!
 3200 ifile = Scr1
 3300 CALL mesage(-1,ifile,sub)
 3400 WRITE (Nout,99005) Uwm , iz(1) , maxmpc
99005 FORMAT (A25,', MPC SET (OR CRIGID ID)',I9,' IS TOO LONG,  ONLY THE FIRST',I4,/5X,                                             &
             &' GRID POINTS ARE USED IN THE BANDIT COMPUTATION')
   GOTO 2300
 3500 WRITE (Nout,99006)
99006 FORMAT ('0*** MORE THAN 1000 INDEPENDENT GRID POINTS USED IN A ','RIGID ELEMENT')
 3600 CALL mesage(-61,0,0)
!
 3700 Ngrid = 0
END SUBROUTINE bread
