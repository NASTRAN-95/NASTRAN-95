
SUBROUTINE optp1d(Elop,Pr,Pl)
   IMPLICIT NONE
   INTEGER B1p1 , Count , Klwds , Nkl(2) , Nklw , Noeor , Npow , Nprw , Nrd , Nwdsp , Nweor , Nwrt , Outtap , Scrth1 , Sysbuf , Ycor
   REAL Kl(4) , Skp1(2) , Skp2(2) , Skp3(2) , Skp4(6)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Skp1 , Count , Skp2 , Ycor , B1p1 , Npow , Skp3 , Nprw , Nwdsp , Nklw , Skp4 , Scrth1
   COMMON /names / Nrd , Noeor , Nwrt , Nweor
   COMMON /optpw1/ Klwds , Kl
   COMMON /system/ Sysbuf , Outtap
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   INTEGER Elop(2,1) , Pr(1)
   REAL Pl(1)
   INTEGER i , ill , ip1 , ip2 , ipr , itp , l , loc , lpl , name(2) , nogo , npl , npr , pid , plp
!
!     PROPERTY OPTIMIZER   SET POINTERS TO PLIMIT
!
   EQUIVALENCE (Nkl(1),Kl(1))
   DATA name/4H OPT , 4HPID /
!
   nogo = 0
   plp = 1
 100  DO
      l = 0
      npl = 0
      CALL read(*300,*500,Scrth1,itp,1,Noeor,i)
      IF ( itp>Npow ) EXIT
!
      ip1 = Elop(2,itp)
      ip2 = Elop(2,itp+1) - 1
      npr = ip2 - ip1
      IF ( npr<=0 ) THEN
!
!     READ A NEW ELEMENT TYPE
!
         CALL fread(Scrth1,0,0,Nweor)
      ELSE
         CALL fread(Scrth1,l,1,Noeor)
         IF ( l<=0 ) EXIT
!
         CALL fread(Scrth1,Nkl(1),4,Noeor)
         l = l - 1
!
!     SEQUENTIAL SEARCH ON PLIMIT AND PROPERTY DATA
!     LPL -- LAST PLIMIT POINTED TO (BY ILL).
!     NPL -- NUMBER OF PLIMIT FOR THIS ELEMENT TYPE IN CORE.
!     PLP -- POINTER FIRST PLIMIT  --    --     -- .
!
         lpl = -9877
!
         DO ipr = ip1 , ip2 , Nwdsp
            pid = Pr(ipr)
!
 110        IF ( pid<Nkl(1) ) THEN
            ELSEIF ( pid==Nkl(1) ) THEN
               GOTO 120
!
!     CHECK UPPER RANGE PLIMIT
!
            ELSEIF ( pid<=Nkl(2) ) THEN
               GOTO 120
            ENDIF
!
!     READ NEXT PLIMIT INTO CORE
!
            IF ( l<=0 ) EXIT
            CALL fread(Scrth1,Nkl(1),4,Noeor)
            l = l - 1
            GOTO 110
!
!     PLIMIT EXISTS - SEE IF MATCHES LAST
!
 120        IF ( lpl/=l ) THEN
!
!     DOESNOT - CHECK IF PREVIOUS ENTRY
!
               IF ( npl/=0 ) THEN
                  DO lpl = plp , loc , 2
                     IF ( Pl(lpl)==Kl(3) ) THEN
                        IF ( Pl(lpl+1)==Kl(4) ) GOTO 125
                     ENDIF
                  ENDDO
               ENDIF
!
!     NEW PLIMIT
!
               IF ( npl+plp+1>Ycor ) GOTO 600
               npl = npl + 2
               loc = npl + plp - 2
               Pl(loc) = Kl(3)
               Pl(loc+1) = Kl(4)
               lpl = l
               ill = loc
               GOTO 130
!
!     PREVIOUS MATCH
!
 125           ill = lpl
               lpl = l
            ENDIF
!
!     LOAD POINTER
!
 130        Pr(ipr+5) = ill
!
         ENDDO
         GOTO 200
      ENDIF
   ENDDO
!
   CALL page2(-2)
   WRITE (Outtap,99001) Sfm , name , itp , l
99001 FORMAT (A25,' 2301,',2A4,' FILE OPTIMIZATION PARAMETER INCORRECT',' AS',2I8)
   nogo = nogo + 1
!
 200  plp = plp + npl
   CALL fread(Scrth1,0,0,Nweor)
   GOTO 100
!
!     END-OF-FILE
!
 300  Nklw = plp + npl - 1
 400  IF ( nogo>0 ) Count = -1
   RETURN
!
!     ILLEGAL EOR
!
 500  CALL mesage(-3,Scrth1,name)
!
!     INSUFFICIENT COREINTERNAL ELEMENT NUMBER PRINTED
!
 600  CALL page2(-2)
   WRITE (Outtap,99002) Ufm , name , B1p1 , itp
99002 FORMAT (A23,' 2298, INSUFFICIENT CORE ',2A4,1H(,I10,' ), PROPERTY',I9)
   Nklw = -plp
   GOTO 400
END SUBROUTINE optp1d
