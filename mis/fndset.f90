
SUBROUTINE fndset(Gpid,X,Ibuf,N)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Bgpdt , Inprew , Ngp , Ngpset , Nirew , Norew , Oes1 , Rew , Subc
   REAL Data , Flag , Skp11(4) , Skp12(4) , Skp21(4) , Skp22(8) , Skpn1(2) , Skpp(211)
   COMMON /blank / Ngp , Skp11 , Ngpset , Skp12 , Skp21 , Bgpdt , Skp22 , Oes1
   COMMON /names / Nirew , Inprew , Skpn1 , Rew , Norew
   COMMON /xxparm/ Skpp , Subc , Flag , Data
!
! Dummy argument declarations
!
   INTEGER Ibuf , N
   INTEGER Gpid(1)
   REAL X(3,1)
!
! Local variable declarations
!
   INTEGER i , insub , j
   REAL twopi , u(3)
!
! End of declarations
!
!
!     GPID = GRID POINT TABLE FOR THIS SET
!
!     N  = 0 INPUT
!     FNDSET READS THE COORDINATES OF THE GRID POINTS IN THIS SET.
!     IF THE GRID POINT TABLE VALUE IS ZERO THE CORRESPONDING GRID
!     POINT IS NOT USED IN THIS SET AND ITS VALUES SKIPPED, OTHERWISE
!     THE XYZ COORDINDATE VALUES ARE READ FROM BGPDT AND PACKED INTO
!     X SPACE. TOTALLY THERE ARE NGPSET GRID DATA SAVED IN X.
!     CORE NEEDED FOR X = 3*NGPSET (PROVIDED BY CALLING ROUTINE)
!
!     N  = 1 INPUT/OUTPUT
!     FNDSET POSITIONS THE STRESS FILE TO THE SUBCASE/VALUE LAST
!     PROCESSED
!
   EQUIVALENCE (u(1),insub)
   DATA twopi/0.0/
!
   IF ( N/=0 ) THEN
!
!     POSITION OES1
!
      IF ( twopi<6.2 ) twopi = 8.0*atan(1.0)
      CALL gopen(Oes1,Gpid(Ibuf),Inprew)
   ELSE
      CALL gopen(Bgpdt,Gpid(Ibuf),Inprew)
      j = 1
      DO i = 1 , Ngp
         IF ( Gpid(i)/=0 ) THEN
            CALL fread(Bgpdt,0,-1,0)
            CALL fread(Bgpdt,X(1,j),3,0)
            j = j + 1
         ELSE
            CALL fread(Bgpdt,0,-4,0)
         ENDIF
      ENDDO
      CALL close(Bgpdt,Rew)
      GOTO 99999
   ENDIF
 100  CALL read(*200,*200,Oes1,j,1,0,i)
   CALL fread(Oes1,0,-2,0)
   CALL fread(Oes1,u,3,0)
   IF ( Subc==insub ) THEN
      IF ( Flag<1.0 ) GOTO 300
      IF ( Flag/=1.0 ) THEN
         j = j/10
!
!     REAL EIGENVALUE ANALYSIS - CONVERT TO FREQUENCY
!
         IF ( j==2 ) u(3) = sqrt(abs(u(3)))/twopi
         IF ( Data-u(3)<=1.0E-6 ) GOTO 300
      ELSEIF ( Data==u(2) ) THEN
         GOTO 300
      ENDIF
   ENDIF
!
!     WRONG CASE
!
   CALL fwdrec(*200,Oes1)
   CALL fwdrec(*200,Oes1)
   GOTO 100
 200  N = N + 1
 300  CALL bckrec(Oes1)
   CALL close(Oes1,Norew)
!
99999 RETURN
END SUBROUTINE fndset
