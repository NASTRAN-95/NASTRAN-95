
SUBROUTINE sptchk
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Dum2(2) , Dum3(3) , Dum4(4) , Dum6(6) , Skip(4)
   INTEGER Geom1 , Geom2 , Ibuf , Ibuf1 , Incr , Irept , Ke(1) , Last , Maxgrd , Ndd(9) , Ne , Ngrid , Nout , Npt(2) , Rd , Rdrew , &
         & Rew , Z(1)
   COMMON /banda / Ibuf1 , Dum6 , Npt
   COMMON /bandb / Dum3 , Ngrid , Dum4 , Irept
   COMMON /bandd / Ndd
   COMMON /bands / Skip , Maxgrd
   COMMON /geomx / Geom1 , Geom2
   COMMON /gpta1 / Ne , Last , Incr , Ke
   COMMON /names / Rd , Rdrew , Dum2 , Rew
   COMMON /system/ Ibuf , Nout
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER i , ielem , j , k , kg(200) , name(2) , ngpt1 , ngpts , ns , ns1 , nss , nwds , spoint(2)
!
! End of declarations
!
!
!     THIS ROUTINE IS CALLED ONLY BY BANDIT TO CHECK THE PRESENCE OF ANY
!     UNDEFINED SPOINT. RESET NGRID AND RETURN FOR ONE MORE COMPUTATION
!     IF THAT IS THE CASE
!
   DATA spoint , name/5551 , 49 , 4HBMIS , 4HS   /
!
!     LIST ALL SPOINTS IN Z(1) THRU Z(NS)
!
   IF ( Irept==3 ) GOTO 400
   ns = 1
   CALL preloc(*400,Z(Ibuf1),Geom2)
   CALL locate(*100,Z(Ibuf1),spoint,k)
   DO
      CALL read(*500,*100,Geom2,Z(ns),1,0,k)
      ns = ns + 1
   ENDDO
 100  ns = ns - 1
   CALL rewind(Geom2)
!
!     CHECK THE PRESENCE OF ELAST, DAMP AND MASS CARDS (ELEMENT TYPES
!     201 THRU 1301).  THEY MAY SPECIFY SCALAR POINTS WITHOUT USING
!     SPOINT CARDS.
!
   nss = ns
   DO ielem = 26 , 350 , Incr
      CALL locate(*200,Z(Ibuf1),Ke(ielem+3),j)
      nwds = Ke(ielem+5)
      ngpt1 = Ke(ielem+12)
      ngpts = Ke(ielem+9) + ngpt1 - 1
      DO
         CALL read(*500,*200,Geom2,kg(1),nwds,0,j)
         DO i = ngpt1 , ngpts
            IF ( ns/=0 ) THEN
               CALL bisloc(*110,kg(i),Z(1),1,ns,k)
               CYCLE
            ENDIF
 110        nss = nss + 1
            IF ( nss>=Ibuf1 ) GOTO 300
            Z(nss) = kg(i)
         ENDDO
      ENDDO
 200  ENDDO
 300  CALL close(Geom2,Rew)
   k = nss - ns - 1
   IF ( k<0 ) GOTO 400
   IF ( k/=0 ) THEN
!
!     SOME SCALAR POINTS ARE USED, BUT NOT SPECIFIED BY SPOINT CARDS.
!     SORT THEM, AND THROW OUT DUPLICATES
!
      ns1 = ns + 1
      CALL sort(0,0,1,1,Z(ns1),nss-ns)
      k = nss
      nss = ns1
      j = ns + 2
      DO i = j , k
         IF ( Z(i)/=Z(i-1) ) THEN
            nss = nss + 1
            Z(nss) = Z(i)
         ENDIF
      ENDDO
   ENDIF
!
!     RE-COMPUTE THE TOTAL NO. OF GRID POINTS, NGRID, AND RETURN FOR
!     ONE MORE BANDIT COMPUTATION
!
   Npt(2) = nss - ns
   Ngrid = Npt(1) + Npt(2)
   DO i = 1 , 9
      Ndd(i) = 0
   ENDDO
   Irept = 2
   RETURN
!
 400  WRITE (Nout,99001) Maxgrd
99001 FORMAT (                                                                                                                      &
       &120H1*** USER FATAL ERROR 2007,  THIS STRUCTURE MODEL USES MORE GRID POINTS THAN THE TOTAL NO. OF GRID CARDS IN BULK DATA (=&
      & ,I6,1H),/)
   Ngrid = 0
   GOTO 99999
!
 500  CALL mesage(-3,Geom2,name)
99999 RETURN
END SUBROUTINE sptchk
