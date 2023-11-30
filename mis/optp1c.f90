
SUBROUTINE optp1c(Elt,Elop,Pr)
   IMPLICIT NONE
   INTEGER B1p1 , Count , Entry(21) , Ept , Incr , Last , Ne(1) , Noeor , Npow , Nprw , Nrd , Ntypes , Nwdsp , Nweor , Nwrt ,       &
         & Outtap , Prc(1) , Prcor , Sysbuf , Ycor
   CHARACTER*25 Sfm , Uwm
   REAL Skp1(2) , Skp2(2) , Skp3(2) , Skp4 , Skp5 , Skp6(6) , X(1)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Skp1 , Count , Skp2 , Ycor , B1p1 , Npow , Skp3 , Nprw , Nwdsp , Skp4 , Skp5 , Ept , Skp6 , Entry
   COMMON /gpta1 / Ntypes , Last , Incr , Ne
   COMMON /names / Nrd , Noeor , Nwrt , Nweor
   COMMON /optpw1/ Prcor , Prc
   COMMON /system/ Sysbuf , Outtap
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ X
   INTEGER Elop(2,2) , Elt(1) , Pr(1)
   INTEGER card(2) , dtyp(21) , i , icpr , icpt , idp , idpe , idps , idx , ietyp , j1 , j2 , jetyp , m1 , name(2) , npr
   REAL rm1
!
   !>>>>EQUIVALENCE (m1,rm1)
   DATA name/4H OPT , 4HP1C / , rm1/ - 1.0/
!
!      PROPERTY CORRELATOR TO EST DESIGN VARIABLE (100*EST LOCATION).
!      THIS VALUE ADDS/SUBTRACTS FROM EST ENTRY TO GET EPT LOCATION.
!      ENTRY IS MADE BY THE ELT ARRAY (SEQUENTIAL LIST OF NUMBERS WITH
!      ZEROS FOR ELEMENTS NOT USED).
!
!              BR  EB   IS  QM  M1  M2  QP  Q1  Q2  RD
!              SH  TB   T1  T2  T6  TM  TP  TU  Q4  T3
   DATA dtyp/ - 14 , -6 , -10 , -5 , -5 , -5 , -5 , -5 , -5 , -2 , -4 , -4 , -4 , -4 , -7 , -4 , -4 , -2 , -5 , -5 , 0/
!
   jetyp = 1
   idps = Elop(2,1)
   idpe = Elop(2,2) - 1
!
   DO ietyp = 1 , Ntypes
      IF ( Elt(ietyp)<=0 ) CYCLE
      npr = (idpe+1-idps)/Nwdsp
      IF ( npr<0 ) THEN
      ELSEIF ( npr==0 ) THEN
         GOTO 50
      ELSE
!
         idx = Entry(jetyp)
         idx = Incr*(idx-1)
         idp = idx + 7
         card(1) = Ne(idp)
         card(2) = Ne(idp+1)
         IF ( Ne(idp+2)>Prcor ) GOTO 400
!
         CALL locate(*300,X(B1p1),card,i)
         icpr = Pr(idps)
         icpt = idps
         DO
!
            CALL read(*500,*600,Ept,Prc,Ne(idp+2),Noeor,i)
!
!     SEQUENTIAL PROPERTY SEARCH.  PROPERTIES THAT ARE UNSORTED ON EPT
!     WILL FAIL.  THIS MAY OCCUR FOR 2 PID/CARD (E.G., QDMEM, QUAD2,
!     SHEAR, TRIA2, TRMEM).
!
            IF ( Prc(1)<icpr ) THEN
            ELSEIF ( Prc(1)==icpr ) THEN
!
!     PROPERTY IN CORE LOCATED.
!
               npr = npr - 1
               Pr(icpt+5) = 0
               Pr(icpt+4) = m1
!
!     LOCATE VARIABLE AS SET BY OPTP1A
!
               j1 = Pr(icpt+1)/100
               j2 = j1 + dtyp(jetyp)
               Pr(icpt+3) = Prc(j2)
               Pr(icpt+2) = Prc(j2)
!
!     ICPT+0, +1 SET BY OPTP1A
!
               icpt = icpt + Nwdsp
               IF ( icpt>idpe ) THEN
!
!     NEW ELEMENT TYPE COMING
!
                  IF ( npr>0 ) EXIT
                  CALL fread(Ept,0,0,Nweor)
                  GOTO 50
               ELSE
                  icpr = Pr(icpt)
               ENDIF
            ELSE
               EXIT
            ENDIF
         ENDDO
      ENDIF
!
!     LOGIC OR UNSORTED FILE ERROR
!
      CALL page2(-2)
      WRITE (Outtap,99001) Sfm , ietyp , Prc(1) , name
99001 FORMAT (A25,' 2299, INCORRECT LOGIC FOR ELEMENT TYPE',I4,', PROPERTY',I9,2H (,2A4,2H).)
      GOTO 200
 50   idps = idpe + 1
      jetyp = jetyp + 1
      IF ( jetyp>Npow ) EXIT
      idpe = Elop(2,jetyp+1) - 1
   ENDDO
!
!
 100  RETURN
!
!     ERRORS
!
 200  Count = -1
   GOTO 100
!
!     UNABLE TO LOCATE SORTED PID
!
 300  WRITE (Outtap,99002) Sfm , name , Prc(1)
99002 FORMAT (A25,' 2300, ',2A4,'UNABLE TO LOCATE PROPERTY',I10,' ON EPT OR IN CORE.')
   GOTO 200
!
!     INSUFFICIENT CORE /OPTPW1/
!
 400  CALL page2(-2)
   WRITE (Outtap,99003) Ufm , name , Prcor , ietyp
99003 FORMAT (A23,' 2296. INSUFFICIENT CORE ',2A4,1H(,I10,' ), ELEMENT',I9)
   GOTO 200
!
!     ILLEGAL EOF
!
 500  CALL mesage(-2,Ept,name)
!
!     ILLEGAL EOR
!
 600  CALL mesage(-3,Ept,name)
   GOTO 200
END SUBROUTINE optp1c