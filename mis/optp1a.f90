
SUBROUTINE optp1a(Elt,Elop,Ele,Dtyp)
   IMPLICIT NONE
   INTEGER B1p1 , Count , Ecor , Est , Ie(1) , Imat(1) , Incr , Inflag , Itype(21) , Last , Matid , Mpt , Ne(1) , Neltyp , Nelw ,   &
         & Noeor , Npow , Nprw , Nrd , Ntypes , Nwdse , Nwdsp , Nweor , Nwrt , Outtap , Sysbuf , Ycor
   REAL Costh , E(1) , Omat(20) , Pla , Sinth , Skp1(2) , Skp2(2) , Skp3 , Skp4(3) , Skp5(2) , Temp
   CHARACTER*23 Ufm
   COMMON /blank / Skp1 , Count , Skp2 , Ycor , B1p1 , Npow , Nelw , Nwdse , Nprw , Nwdsp , Skp3 , Mpt , Skp4 , Est , Skp5 ,        &
                 & Neltyp , Itype
   COMMON /gpta1 / Ntypes , Last , Incr , Ne
   COMMON /matin / Matid , Inflag , Temp , Pla , Sinth , Costh
   COMMON /matout/ Omat
   COMMON /names / Nrd , Noeor , Nwrt , Nweor
   COMMON /optpw1/ Ecor , E
   COMMON /system/ Sysbuf , Outtap
   COMMON /xmssg / Ufm
   INTEGER Dtyp(1) , Elop(2,1) , Elt(1)
   REAL Ele(1)
   REAL c1
   INTEGER i , i1 , i2 , ietyp , intyp , ipt(21) , iwd(28) , j1 , j2 , k1 , k2 , name(2) , nest
!
   EQUIVALENCE (E(1),Ie(1)) , (Omat(1),Imat(1)) , (k1,c1)
   DATA name/4H OPT , 4HP1A /
!
!     POINTER TO IPT ARRAY - ZERO MEANS ELEMENT NOT USED.
!     UPDATE IPT DIMENSIONS AS NEW ELEMENTS ARE ADDED
!
   DATA ipt/15 , 17 , 21 , 11 , 23 , 25 , 11 , 13 , 11 , 1 , 5 , 9 , 7 , 9 , 19 , 9 , 9 , 3 , 27 , 27 , 0/
!
!     WORD POINTER TO EST AND MATERIAL STRESS LIMITS
!     WORD 1 = 100*WORD TO OPTIMIZE (EST - IF.NE.0) + ALTERNATE
!     WORD 2 = 100*WORD FOR STRESS LIMIT + ALTERNATE
!              WHERE 1 = SHEAR
!                    2 = TENSION/COMPRESSION
!                    3 = ANY/ALL NONZERO
!
!                      11        13        15        17        19
!                      21        23        25       27
   DATA iwd/506 , 201 , 500 , 200 , 700 , 100 , 709 , 303 , 700 , 300 , 800 , 300 , 810 , 303 , 1718 , 202 , 910 , 202 , 1011 ,     &
      & 303 , 1300 , 303 , 800 , 303 , 800 , 303 , 1400 , 300/
!
   Nelw = 0
   Sinth = 0.0
   Costh = 1.0
   Pla = 0.0
   Inflag = 2
!
!     COPY POINTER ARRAY INTO CORE
!
   DO i = 1 , Ntypes
      Elt(i) = Dtyp(i)
   ENDDO
!
!     ZERO OUT POINTER ARRAY
!
   i1 = 2*(Npow+1)
   DO i = 2 , i1
      Elop(i,1) = 0
   ENDDO
   Elop(1,1) = 1
!
!     READ IN ELEMENT TYPE
!
 100  CALL read(*600,*900,Est,ietyp,1,Noeor,i)
   IF ( ietyp>Ntypes ) GOTO 400
   intyp = Dtyp(ietyp)
   IF ( intyp<=0 ) GOTO 400
!
!     DECODE LIMITS NEEDED
!
   i = ipt(intyp)
   j2 = iwd(i)
   j1 = j2/100
   j2 = j2 - j1*100
   i2 = iwd(i+1)
   i1 = i2/100
   i2 = i2 - i1*100
   nest = (ietyp-1)*Incr + 12
   nest = Ne(nest)
   IF ( nest>Ecor ) THEN
      CALL page2(-2)
      WRITE (Outtap,99002) name , Ecor , ietyp
      Nelw = 0
      GOTO 700
   ENDIF
 200  CALL read(*800,*500,Est,E,nest,Noeor,k1)
   Matid = Ie(j1-1)
   IF ( Matid==0 ) Matid = Ie(j2-1)
   Temp = E(nest)
   CALL mat(Ie(1))
!
!     TEST IF PERTINENT STRESS LIMITS ARE ZERO
!
   k1 = 0
   k2 = 0
   IF ( i1/=2 .OR. i2/=2 ) THEN
!
!     SHEAR
!
      IF ( Omat(15)==0.0 ) THEN
         IF ( i1/=2 ) k1 = 1
         IF ( i2==1 .OR. i2==3 ) k2 = 1
      ENDIF
   ENDIF
   IF ( i1/=1 .OR. i2>1 ) THEN
!
!     TENSION
!
      IF ( Omat(13)==0.0 ) THEN
         IF ( i1>1 ) k1 = k1 + 1
         IF ( i2>1 ) k2 = k2 + 1
      ENDIF
!
!     COMPRESSION
!
      IF ( Omat(14)==0.0 ) THEN
         IF ( i1>1 ) k1 = k1 + 1
         IF ( i2>1 ) k2 = k2 + 1
      ENDIF
   ENDIF
!
   IF ( k1>=i1 .AND. k2>=i2 ) GOTO 200
!
!     CHECK IF PROPERTY IS NONZERO AND STORE INFO IN PID POINTER
!
   IF ( E(j1)/=0.0 ) THEN
!
      IF ( k1<i1 ) THEN
!
!     PRIMARY PROPERTY USED
!
         k1 = j1*100 + i1
         GOTO 300
      ENDIF
   ENDIF
   IF ( E(j2)==0.0 ) GOTO 200
!
   IF ( k2>=i2 ) GOTO 200
!
!     ALTERNATE PROPERTY USED
!
   k1 = j2*100 + i2
 300  IF ( Nelw+5>Ycor ) GOTO 1000
   Ele(Nelw+1) = E(1)
   Ele(Nelw+2) = Omat(13)
   Ele(Nelw+3) = Omat(14)
   Ele(Nelw+4) = Omat(15)
!
!     NOTE, K1 = C1
!
   Ele(Nelw+5) = c1
   Nelw = Nelw + Nwdse
   GOTO 200
!
!     NEW ELEMENT TYPE
!
 400  CALL fread(Est,0,0,Nweor)
   IF ( ietyp>Ntypes ) GOTO 600
   IF ( intyp<=0 ) GOTO 100
 500  Elop(1,intyp+1) = Nelw + 1
   GOTO 100
!
!     EOF
!
 600  i1 = Npow + 1
   DO i = 2 , i1
      IF ( Elop(1,i)<=0 ) Elop(1,i) = Elop(1,i-1)
   ENDDO
   IF ( Nelw==0 ) THEN
      CALL page2(-2)
      WRITE (Outtap,99001) Ufm
99001 FORMAT (A23,' 2295, NO ELEMENTS EXIST FOR OPTIMIZATION.')
      Count = -1
   ENDIF
 700  RETURN
!
!     ILLEGAL EOF
!
 800  CALL mesage(-2,Est,name)
!
!     ILLEGAL EOR
!
 900  CALL mesage(-3,Est,name)
!
!     INSUFFICIENT CORE
!
 1000 CALL page2(-2)
   WRITE (Outtap,99002) Ufm , name , B1p1 , Ie(1)
   Nelw = 0
   GOTO 700
99002 FORMAT (A23,' 2296, INSUFFICIENT CORE ',2A4,1H(,I10,' ), ELEMENT',I9)
END SUBROUTINE optp1a
