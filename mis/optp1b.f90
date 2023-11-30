
SUBROUTINE optp1b(Elt,Elop,Ele,Pr)
   IMPLICIT NONE
   INTEGER B1p1 , Count , Ect , Incr , Itype(21) , Last , Ne(1) , Nelw , Noeor , Npow , Nprw , Nrd , Ntypes , Numelm , Nwdse ,      &
         & Nwdsp , Nweor , Nwrt , Outtap , Prc(2) , Prcor , Sysbuf , Ycor
   CHARACTER*25 Sfm , Uwm
   REAL Skp1(2) , Skp2(2) , Skp3 , Skp4(2) , Skp5(4) , X(1)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Skp1 , Count , Skp2 , Ycor , B1p1 , Npow , Nelw , Nwdse , Nprw , Nwdsp , Skp3 , Skp4 , Ect , Skp5 , Numelm ,     &
                 & Itype
   COMMON /gpta1 / Ntypes , Last , Incr , Ne
   COMMON /names / Nrd , Noeor , Nwrt , Nweor
   COMMON /optpw1/ Prcor , Prc
   COMMON /system/ Sysbuf , Outtap
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ X
   INTEGER Ele(1) , Elop(2,1) , Elt(1) , Pr(1)
   INTEGER card(2) , elcr , elpt , i , idee , ides , idp , idx , ieop , j , k , kid , l , m , name(2) , nele , npr , nx , pid ,     &
         & prpt , prpt1
!
   DATA name/4H OPT , 4HP1B /
!
!
   ieop = 1
   ides = Elop(1,ieop)
   idee = Elop(1,ieop+1)
   prpt = 1
   prpt1 = 1
   Elop(2,1) = 1
!
!     IN CASE OF ERROR SET PRC(1)
!
   Prc(1) = -1
!
   DO k = 1 , Numelm
      nele = (idee-ides)/Nwdse
      IF ( nele<0 ) GOTO 100
      IF ( nele==0 ) GOTO 200
!
      idx = Incr*(Itype(k)-1)
      idp = idx + 4
      card(1) = Ne(idp)
      card(2) = Ne(idp+1)
      IF ( Ne(idp+2)>Prcor ) GOTO 400
      CALL locate(*500,X(B1p1),card(1),i)
!
!     SEQUENTIAL ELEMENT SEARCH
!
      npr = 0
      elpt = ides
      elcr = Ele(elpt)
 50   DO
!
         CALL read(*500,*500,Ect,Prc,Ne(idp+2),Noeor,i)
         IF ( Prc(1)<elcr ) THEN
         ELSEIF ( Prc(1)==elcr ) THEN
!
!     ELEMENT ID IN CORE .EQ. ECT ID - ELEMENT TO BE OPTIMIZED
!
            pid = Prc(2)
            card(1) = pid
            card(2) = Ele(elpt+4)
!
!     TEST FOR CORE NEEDED AFTER EXPANDING TO NWDSP WORDS
!
            IF ( prpt1+Nwdsp*(npr/2+1)>Ycor ) GOTO 700
            CALL bishel(*150,card,npr,2,Pr(prpt1))
            GOTO 150
         ELSE
            EXIT
         ENDIF
      ENDDO
!
!     LOGIC OR FILE FAILURE
!
 100  CALL page2(-2)
      WRITE (Outtap,99001) Sfm , Itype(k) , Prc(1) , name
99001 FORMAT (A25,' 2297, INCORRECT LOGIC FOR ELEMENT TYPE',I4,', ELEMENT',I8,2H (,2A4,2H).)
      GOTO 600
 150  Ele(elpt+4) = pid
      elpt = elpt + Nwdse
      IF ( elpt>=idee ) THEN
!
!     NEW ELEMENT TYPE COMING
!
         CALL fread(Ect,0,0,Nweor)
!
!     EXPAND PROPERTIES TO NWDSP WORDS/PROPERTY
!
         nx = npr/2
         IF ( nx<1 ) GOTO 100
         IF ( nx/=1 ) THEN
            DO i = 1 , nx
               j = nx - i
               l = prpt1 + j*Nwdsp
               m = prpt1 + j*2
               Pr(l) = Pr(m)
               Pr(l+1) = Pr(m+1)
            ENDDO
         ENDIF
!
         prpt = prpt1 + nx*Nwdsp
!
!     PLACE POINTERS IN ELEMENT ARRAY
!
         l = idee - 1
         DO i = ides , l , Nwdse
            kid = Ele(i+4)
            CALL bisloc(*100,kid,Pr(prpt1),Nwdsp,nx,j)
            Ele(i+4) = j
         ENDDO
      ELSE
         elcr = Ele(elpt)
         GOTO 50
      ENDIF
!
!     SETUP FOR NEXT ELEMENT
!
 200  ieop = ieop + 1
      Elop(2,ieop) = prpt
      prpt1 = prpt
      ides = idee
      IF ( ieop>Npow ) EXIT
      idee = Elop(1,ieop+1)
   ENDDO
!
!
 300  Nprw = prpt - 1
   RETURN
!
!     ERRORS
!
!     INSUFFICIENT CORE IN /OPTPW1/ OR /XXOPT1/
!
 400  Count = -1
   GOTO 300
!
!     FILE ERRORS
!
 500  CALL mesage(-7,Ect,name)
 600  prpt = 1
   GOTO 300
!
!     INSUFFICIENT CORE
!
 700  CALL page2(-2)
   WRITE (Outtap,99002) Ufm , name , B1p1 , pid
99002 FORMAT (A23,' 2298, INSUFFICIENT CORE ',2A4,1H(,I10,'), PROPERTY',I9)
   GOTO 400
END SUBROUTINE optp1b
