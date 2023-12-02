!*==sptchk.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sptchk
   IMPLICIT NONE
   USE C_BANDA
   USE C_BANDB
   USE C_BANDD
   USE C_BANDS
   USE C_GEOMX
   USE C_GPTA1
   USE C_NAMES
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ielem , j , k , ngpt1 , ngpts , ns , ns1 , nss , nwds
   INTEGER , DIMENSION(200) :: kg
   INTEGER , DIMENSION(2) , SAVE :: name , spoint
   EXTERNAL bisloc , close , locate , mesage , preloc , read , rewind , sort
!
! End of declarations rewritten by SPAG
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
   IF ( Irept==3 ) GOTO 300
   ns = 1
   CALL preloc(*300,Z(Ibuf1),Geom2)
   CALL locate(*100,Z(Ibuf1),spoint,k)
   DO
      CALL read(*400,*100,Geom2,Z(ns),1,0,k)
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
   SPAG_Loop_1_1: DO ielem = 26 , 350 , Incr
      CALL locate(*200,Z(Ibuf1),Ke(ielem+3),j)
      nwds = Ke(ielem+5)
      ngpt1 = Ke(ielem+12)
      ngpts = Ke(ielem+9) + ngpt1 - 1
      DO
         CALL read(*400,*200,Geom2,kg(1),nwds,0,j)
         DO i = ngpt1 , ngpts
            IF ( ns/=0 ) THEN
               CALL bisloc(*110,kg(i),Z(1),1,ns,k)
               CYCLE
            ENDIF
 110        nss = nss + 1
            IF ( nss>=Ibuf1 ) EXIT SPAG_Loop_1_1
            Z(nss) = kg(i)
         ENDDO
      ENDDO
 200  ENDDO SPAG_Loop_1_1
   CALL close(Geom2,Rew)
   k = nss - ns - 1
   IF ( k>=0 ) THEN
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
   ENDIF
!
 300  WRITE (Nout,99001) Maxgrd
99001 FORMAT (                                                                                                                      &
       &120H1*** USER FATAL ERROR 2007,  THIS STRUCTURE MODEL USES MORE GRID POINTS THAN THE TOTAL NO. OF GRID CARDS IN BULK DATA (=&
      & ,I6,1H),/)
   Ngrid = 0
   RETURN
!
 400  CALL mesage(-3,Geom2,name)
END SUBROUTINE sptchk
