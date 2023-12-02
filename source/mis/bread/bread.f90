!*==bread.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
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
   USE C_BANDA
   USE C_BANDB
   USE C_BANDD
   USE C_BANDS
   USE C_GEOMX
   USE C_GPTA1
   USE C_NAMES
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ii3
   INTEGER , DIMENSION(1) :: Ig
   INTEGER , DIMENSION(Ii3,1) :: Inv
   INTEGER , DIMENSION(1) :: Norig
   INTEGER , DIMENSION(7) :: Kg
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: chbdy , crbar , crbe1 , crbe2 , crbe3 , crigd1 , crigd2 , crigd3 , crigdr , crrod , crspln , crtrpt , genel ,  &
                   & maxmpc , mpc , mset , plotel
   LOGICAL , SAVE :: debug
   INTEGER :: i , ibuf2 , ielem , ifile , irtn , iz2 , j , k , kdim4 , kgpv , l , m , ncon , ngpt1 , ngpt2 , ngpts , ntot , nwds ,  &
            & scalar
   INTEGER , DIMENSION(3) :: iz , xxx
   INTEGER , DIMENSION(2) , SAVE :: sub
   EXTERNAL close , locate , mesage , open , preloc , rdtrl , read , scat , setig , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   DATA crigdr , crigd1 , crigd2 , crigd3 , genel/8210 , 5310 , 5410 , 8310 , 4301/
   DATA chbdy , plotel , crrod , crbar , crtrpt/4208 , 5201 , 6510 , 6610 , 6710/
   DATA crbe1 , crbe2 , crbe3 , crspln , mset/6810 , 6910 , 7010 , 7110 , 4HMSET/
   DATA sub , mpc , maxmpc , debug/4HBREA , 4HD    , 4901 , 150 , .FALSE./
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
!     CHECK THE PRESENCE OF GEOM2 FILE
!
         Kg(1) = Geom2
         CALL rdtrl(Kg(1))
         j = Kg(2) + Kg(3) + Kg(4) + Kg(5) + Kg(6) + Kg(7)
         IF ( Kg(1)<0 .OR. j==0 ) THEN
            spag_nextblock_1 = 17
            CYCLE SPAG_DispatchLoop_1
         ENDIF
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
         IF ( Nompc==0 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Z(1) = Geom4
         CALL rdtrl(Z(1))
         j = 0
         DO i = 2 , 7
            j = j + Z(i)
         ENDDO
         IF ( Z(1)<0 .OR. j==0 ) THEN
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
         ibuf2 = Ibuf1 - Ibuf
         CALL open(*300,Scr1,Z(ibuf2),Wrtrew)
         ifile = Geom4
         CALL preloc(*240,Z(Ibuf1),Geom4)
!
         IF ( Nompc/=1 ) THEN
!
            xxx(1) = mpc
            xxx(2) = xxx(1)/100
            CALL locate(*20,Z(Ibuf1),xxx,j)
            SPAG_Loop_1_1: DO
               j = 1
               CALL read(*320,*20,Geom4,iz,1,0,m)
               DO
                  j = j + 1
                  CALL read(*320,*20,Geom4,Kg(j),3,0,m)
                  IF ( Kg(j)==-1 ) THEN
                     j = j - 1
                     Kg(1) = j - 1
                     CALL write(Scr1,Kg,j,1)
                     Neq = Neq + 1
                     CYCLE SPAG_Loop_1_1
                  ELSEIF ( j+3>maxmpc ) THEN
                     spag_nextblock_1 = 14
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
               EXIT SPAG_Loop_1_1
            ENDDO SPAG_Loop_1_1
         ENDIF
!
!     LOCATE ANY CRIGDR AND CRROD ELEMENTS, AND SAVE THE GRID DATA IN
!     SCR1. (DEPENDENT GRID FIRST, AND ONE INDEPENDENT GRID LAST)
!
!     FOR ALL RIGID ELEMENTS, THE FIRST WORD OF KG ARRAY CONTAINS
!     (NO. OF DEPENDENT + INDEP. GRIDS)*1000 + (NO. OF INDEP. GRIDS)
!     THE DATA IN SCR1 WILL BE PROCESSED BY TIGER
!
 20      IF ( Nompc==3 ) GOTO 220
         xxx(1) = crigdr
         spag_nextblock_1 = 2
      CASE (2)
         xxx(2) = xxx(1)/100
         CALL locate(*40,Z(Ibuf1),xxx,j)
         DO
            CALL read(*320,*40,Geom4,iz,1,0,m)
            CALL read(*320,*40,Geom4,Kg(3),3,0,m)
            Kg(1) = 2*1000 + 1
            Kg(2) = Kg(4)
            CALL write(Scr1,Kg,3,1)
            Neqr = Neqr + 1
         ENDDO
!
 40      IF ( xxx(1)==crrod ) THEN
!
!     LOCATE ANY CRIGD1, CRIGD2  AND CRBE2  ELEMENTS, AND SAVE GRID
!     DATA IN SCR1. PUT THE ONE INDEPENDENT GRID LAST
!
            xxx(1) = crigd1
         ELSE
            xxx(1) = crrod
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         xxx(2) = xxx(1)/100
         CALL locate(*60,Z(Ibuf1),xxx,j)
         SPAG_Loop_1_2: DO
            j = 1
            CALL read(*320,*60,Geom4,iz,2,0,m)
            iz2 = iz(2)
            DO
               j = j + 1
               CALL read(*320,*60,Geom4,Kg(j),1,0,m)
               CALL read(*320,*60,Geom4,0,-6,0,m)
               IF ( Kg(j)==-1 ) THEN
                  Kg(j) = iz2
                  Kg(1) = (j-1)*1000 + 1
                  CALL write(Scr1,Kg,j,1)
                  Neqr = Neqr + 1
                  CYCLE SPAG_Loop_1_2
               ELSEIF ( j>maxmpc ) THEN
                  spag_nextblock_1 = 14
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            EXIT SPAG_Loop_1_2
         ENDDO SPAG_Loop_1_2
 60      IF ( xxx(1)==crbe2 ) THEN
!
!     LOCATE ANY CRIGD3, CRBE1, CRBAR AND CRTRPLT ELEMENTS, AND SAVE
!     GRID DATA IN SCR1 FILE. PUT THE INDEPENDENT GRID LAST
!
            xxx(1) = crbar
            ASSIGN 80 TO irtn
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
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
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 80      xxx(1) = crtrpt
         ASSIGN 100 TO irtn
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 100     xxx(1) = crbe1
         ASSIGN 120 TO irtn
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 120     xxx(1) = crigd3
         ASSIGN 160 TO irtn
         spag_nextblock_1 = 4
      CASE (4)
         xxx(2) = xxx(1)/100
         CALL locate(*140,Z(Ibuf1),xxx,j)
         SPAG_Loop_1_3: DO
            j = 2
            k = 1
            CALL read(*320,*140,Geom4,iz,1,0,m)
            DO
               CALL read(*320,*140,Geom4,iz(k),1,0,m)
               IF ( iz(k)==mset ) THEN
                  DO
                     CALL read(*320,*140,Geom4,Kg(j),1,0,m)
                     CALL read(*320,*140,Geom4,0,-6,0,m)
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
                        CYCLE SPAG_Loop_1_3
                     ELSE
                        j = j + 1
                        IF ( j>maxmpc ) THEN
                           spag_nextblock_1 = 14
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                     ENDIF
                  ENDDO
               ELSE
                  CALL read(*320,*140,Geom4,0,-6,0,m)
                  k = k + 1
                  IF ( k>999 ) THEN
                     spag_nextblock_1 = 15
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
            ENDDO
            EXIT SPAG_Loop_1_3
         ENDDO SPAG_Loop_1_3
!
!     LOCATE ANY CRSPLINE ELEMENTS, AND SAVE GRID DATA IN SCR1 FILE.
!     PUT THE INDEPENDENT GRIDS LAST
 140     GOTO irtn
!
!     LOCATE ANY CRBE3 ELEMENTS, AND SAVE GRID DATA IN SCR1 FILE. PUT
!     THE INDEPENDENT GRID LAST
!
 160     xxx(1) = crbe3
         xxx(2) = xxx(1)/100
         CALL locate(*180,Z(Ibuf1),xxx,j)
         spag_nextblock_1 = 5
      CASE (5)
         CALL read(*320,*180,Geom4,iz,3,0,m)
         iz2 = iz(2)
         j = 2
         CALL read(*320,*180,Geom4,0,-2,0,m)
         spag_nextblock_1 = 6
      CASE (6)
         SPAG_Loop_1_4: DO
            CALL read(*320,*180,Geom4,Kg(j),1,0,m)
            k = -Kg(j)
            IF ( k>0 ) THEN
               IF ( k==1 ) THEN
                  CALL read(*320,*180,Geom4,i,1,0,m)
                  IF ( i==-2 ) THEN
                     spag_nextblock_1 = 7
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  CALL read(*320,*180,Geom4,0,-1,0,m)
                  CYCLE
               ELSEIF ( k==2 ) THEN
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( k==3 ) THEN
                  spag_nextblock_1 = 8
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
            EXIT SPAG_Loop_1_4
         ENDDO SPAG_Loop_1_4
         j = j + 1
         IF ( j<=maxmpc ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 14
         CYCLE SPAG_DispatchLoop_1
      CASE (7)
         SPAG_Loop_1_5: DO
            CALL read(*320,*180,Geom4,Kg(j),1,0,m)
            IF ( Kg(j)<0 ) EXIT SPAG_Loop_1_5
            CALL read(*320,*180,Geom4,0,-1,0,m)
            j = j + 1
         ENDDO SPAG_Loop_1_5
         spag_nextblock_1 = 8
      CASE (8)
         Kg(j) = iz2
         Kg(1) = (j-1)*1000 + 1
         CALL write(Scr1,Kg,j,1)
         Neqr = Neqr + 1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
!
!     LOCATE ANY CRSPLINE ELEMENTS, AND SAVE GRID DATA IN SCR1 FILE.
!     PUT THE INDEPENDENT GRIDS LAST
!
 180     xxx(1) = crspln
         xxx(2) = xxx(1)/100
         CALL locate(*220,Z(Ibuf1),xxx,j)
         spag_nextblock_1 = 9
      CASE (9)
         CALL read(*320,*220,Geom4,iz,3,0,m)
         k = 1
         iz(k) = iz(3)
         j = 1
         SPAG_Loop_1_7: DO
            j = j + 1
            SPAG_Loop_2_6: DO
               CALL read(*320,*200,Geom4,Kg(j),2,0,m)
               IF ( Kg(j)==-1 ) EXIT SPAG_Loop_2_6
               IF ( j+2>maxmpc ) THEN
                  spag_nextblock_1 = 14
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( Kg(j+1)/=0 ) CYCLE SPAG_Loop_1_7
               k = k + 1
               IF ( k>999 ) THEN
                  spag_nextblock_1 = 15
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               iz(k) = Kg(j)
            ENDDO SPAG_Loop_2_6
            EXIT SPAG_Loop_1_7
         ENDDO SPAG_Loop_1_7
 200     DO i = 1 , k
            Kg(j) = iz(i)
            j = j + 1
         ENDDO
         j = j - 1
         Kg(1) = (j-1)*1000 + k
         CALL write(Scr1,Kg,j,1)
         Neqr = Neqr + 1
         spag_nextblock_1 = 9
         CYCLE SPAG_DispatchLoop_1
!
 220     DO k = 1 , maxmpc
            Kg(k) = 0
         ENDDO
 240     CALL close(Geom4,Rew)
         CALL close(Scr1,Rew)
         spag_nextblock_1 = 10
      CASE (10)
!
!     PROCESS ELEMENT CARDS AND FILL UP CONNECTION TABLE IG
!
         ifile = Geom2
         CALL preloc(*320,Z(Ibuf1),Geom2)
         ielem = 1 - Incr
 260     SPAG_Loop_1_8: DO
            ielem = ielem + Incr
            IF ( ielem>Last ) THEN
!
!     SPECIAL TREATMENT FOR GENERAL ELEM.
!     (LIMITED TO KDIM*4 GRID POINTS PER GENEL)
!
               xxx(1) = genel
               xxx(2) = xxx(1)/100
               CALL locate(*280,Z(Ibuf1),xxx,j)
               kdim4 = Kdim*4
               spag_nextblock_1 = 13
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Ke(ielem+3)/=chbdy ) THEN
               IF ( Ke(ielem+3)/=plotel ) THEN
                  scalar = Ke(ielem+10)
                  IF ( scalar/=-1 ) THEN
                     CALL locate(*260,Z(Ibuf1),Ke(ielem+3),j)
                     nwds = Ke(ielem+5)
                     ngpts = Ke(ielem+9)
                     ngpt1 = Ke(ielem+12)
                     ncon = ngpts
                     EXIT SPAG_Loop_1_8
                  ENDIF
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_8
         spag_nextblock_1 = 11
      CASE (11)
         SPAG_Loop_1_9: DO
            CALL read(*320,*260,Geom2,Kg(1),nwds,0,m)
            IF ( scalar==0 ) EXIT SPAG_Loop_1_9
            IF ( Kg(5)/=0 .AND. Kg(6)/=0 ) EXIT SPAG_Loop_1_9
         ENDDO SPAG_Loop_1_9
         spag_nextblock_1 = 12
      CASE (12)
!     THE ABOVE CONDITIONS HOLD TRUE FOR CDAMPI, CELASI, AND CMASSI
!     WHERE I = 1,2
         Nel = Nel + 1
         CALL scat(Kg(ngpt1),ncon,Inv,Ii3,Norig)
         IF ( Ngrid==-1 ) GOTO 280
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
         IF ( ielem<=Last ) THEN
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 13
      CASE (13)
         ntot = 0
         CALL read(*320,*280,Geom2,k,1,0,m)
         k = 0
         kgpv = 0
         SPAG_Loop_1_10: DO
            ntot = ntot + 1
            IF ( ntot<kdim4 ) ncon = ntot
            DO
               CALL read(*320,*280,Geom2,Kg(ncon),2,0,m)
               IF ( Kg(ncon)==-1 ) THEN
!                                           GRD  SCALAR GRD
!                                           PT.   PT.   PT.
                  k = k + 1
                  xxx(k) = Kg(ncon+1)
                  IF ( k>=2 ) THEN
                     ncon = ncon - 1
                     m = xxx(1)
                     nwds = 1 + (m*m-m)/2 + m
                     CALL read(*320,*280,Geom2,k,-nwds,0,m)
                     CALL read(*320,*280,Geom2,k,1,0,m)
                     ngpt1 = 1
                     IF ( k/=0 ) THEN
                        nwds = m*xxx(2)
                        CALL read(*320,*280,Geom2,k,-nwds,0,m)
                     ENDIF
                     spag_nextblock_1 = 12
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ELSEIF ( Kg(ncon+1)/=0 ) THEN
                  IF ( Kg(ncon)/=kgpv ) THEN
                     kgpv = Kg(ncon)
                     CYCLE SPAG_Loop_1_10
                  ENDIF
               ENDIF
            ENDDO
            EXIT SPAG_Loop_1_10
         ENDDO SPAG_Loop_1_10
 280     CALL close(Geom2,Rew)
         IF ( ntot>kdim4 ) THEN
            WRITE (Nout,99001) Ufm , ntot
99001       FORMAT (A23,', GENEL ELEMENT HAS TOO MANY GRID POINTS,',I7)
            j = ntot/400 + 1
            IF ( j<=9 ) WRITE (Nout,99002) j
99002       FORMAT (5X,'USER NEEDS TO ADD A ''NASTRAN BANDTDIM=',I1,''' CARD AND RERUN JOB')
            spag_nextblock_1 = 16
            CYCLE SPAG_DispatchLoop_1
         ELSE
            IF ( .NOT.debug ) RETURN
!
            m = Nn(1)
            WRITE (Nout,99003) Nn
99003       FORMAT (//21H /BANDS/ FROM BREAD =,10I8)
            WRITE (Nout,99004) ((Inv(i,j),j=1,2),i=1,m)
99004       FORMAT (/12H TABLE INV =,(/10X,2I8))
            RETURN
         ENDIF
!
 300     ifile = Scr1
 320     CALL mesage(-1,ifile,sub)
         spag_nextblock_1 = 14
      CASE (14)
         WRITE (Nout,99005) Uwm , iz(1) , maxmpc
99005    FORMAT (A25,', MPC SET (OR CRIGID ID)',I9,' IS TOO LONG,  ONLY THE FIRST',I4,/5X,                                          &
                &' GRID POINTS ARE USED IN THE BANDIT COMPUTATION')
         GOTO 220
      CASE (15)
         WRITE (Nout,99006)
99006    FORMAT ('0*** MORE THAN 1000 INDEPENDENT GRID POINTS USED IN A ','RIGID ELEMENT')
         spag_nextblock_1 = 16
      CASE (16)
         CALL mesage(-61,0,0)
         spag_nextblock_1 = 17
      CASE (17)
!
         Ngrid = 0
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE bread
