
SUBROUTINE pload4(Ibuf5,Ido,Jopen)
   IMPLICIT NONE
   REAL Core(1)
   INTEGER Est , Ibuf , Idum(5) , Ielem(1) , Iest(45) , Incr , Iprec , Islt(11) , Iz(1) , Jdum(52) , Last , Lcare , Nelem , Nout ,  &
         & Slt
   CHARACTER*23 Ufm
   COMMON /gpta1 / Nelem , Last , Incr , Ielem
   COMMON /loadx / Lcare , Slt , Idum , Est
   COMMON /pindex/ Iest , Islt
   COMMON /system/ Ibuf , Nout , Jdum , Iprec
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Core
   INTEGER Ibuf5 , Ido , Jopen
   LOGICAL allin , debug
   INTEGER file , i , ib , ido11 , ieltyp , imhere , j , jsave , lcore , name(2) , nwords , q4 , quad4 , t3 , tria3
   REAL flag
!
!     TO GENERATE PLOAD4 PRESSURE LOAD FOR QUAD4 AND TRIA3 ELEMENTS.
!
!     BOTH ELEMENT TYPES MAY BE PRESENT, OR ONLY ONE OF THE TWO IS
!     PRESENT.
!
!     THIS ROUTINE IS CALLED ONLY BY EXTERN IN SSG1 MODULE, LINK5
!
!     THIS ROUTINE  CALLS PLOD4D OR PLOD4S TO COMPUTE LOAD FOR QUAD4
!     ELEMENTS, AND CALLS T3PL4D OR T3PL4S TO COMPUTE LOAD FOR TRIA3
!
!     IN OVERLAY TREE, THIS ROUTINE SHOULD BE IN PARALLELED WITH FPONT
!     ROUTINE, AND FOLLOWED BY PLOD4D/S AND T3PL4D/S. I.E.
!
!                   ( FPONT
!            EXTERN (        ( PLOD4D  (/ZZSSA1/
!                   ( PLOAD4 ( PLOD4S
!                            ( T3PL4D
!                            ( T3PL4S
!
   EQUIVALENCE (Core(1),Iz(1))
   DATA quad4 , tria3 , name/64 , 83 , 4HPLOA , 4HD4  /
   DATA debug/.FALSE./
!
!
!     T3 AND Q4 KEEP TRACK OF THE PRESENCE OF THE CTRIA3 AND CQUAD4
!     ELEMENTS
!
   t3 = 0
   q4 = 0
   lcore = Ibuf5 - Ibuf
   ido11 = Ido*11
   allin = .FALSE.
   IF ( ido11<=lcore ) THEN
      IF ( debug ) WRITE (Nout,99001)
99001 FORMAT (/,' * PLOAD4 IS CALLED FOR ONE LOAD CASE')
!
!     OPEN CORE IS BIG ENOUGH TO HOLD ALL PLOAD4 DATA.
!     READ THEM ALL INTO CORE
!     (BAD NEWS - OPEN CORE AT THIS TIME IS NOT AVAILABLE)
!
      IF ( allin ) THEN
!
         allin = .TRUE.
         file = Slt
         imhere = 350
         CALL read(*700,*800,Slt,Core,ido11,0,flag)
      ENDIF
   ENDIF
!
!     OPEN CORE NOT LARGE ENOUGH TO HOLD ALL PLOAD4 DATA
!
   IF ( Jopen/=1 ) THEN
      Jopen = 1
      file = Est
      CALL open(*600,Est,Core(Ibuf5),0)
      CALL fwdrec(*700,Est)
      file = Est
      CALL read(*200,*400,Est,ieltyp,1,0,flag)
   ENDIF
 100  IF ( ieltyp==quad4 ) THEN
!
      IF ( q4<1 ) THEN
         q4 = 1
         IF ( debug ) WRITE (Nout,99002) t3
99002    FORMAT (/,'   QUAD4 ELEM FOUND. SETTING Q4 TO 1.  T3 =',I3)
         GOTO 300
      ENDIF
   ELSEIF ( ieltyp==tria3 ) THEN
      IF ( t3/=1 ) THEN
         t3 = 1
         IF ( debug ) WRITE (Nout,99003) q4
99003    FORMAT (/,'   TRIA3 ELEM FOUND. SETTING T3 TO 1.  Q4 =',I3)
         GOTO 300
      ENDIF
   ENDIF
   CALL fwdrec(*200,Est)
   CALL read(*200,*400,Est,ieltyp,1,0,flag)
   GOTO 100
 200  IF ( t3+q4/=0 ) GOTO 400
   WRITE (Nout,99004) Ufm
99004 FORMAT (A23,', PLOAD4 PRESSURE LOAD IS USED WITHOUT THE PRESENCE',' OF QUAD4 OR TRIA3 ELEMENT')
   imhere = 435
   GOTO 700
 300  j = Incr*(ieltyp-1)
   nwords = Ielem(j+12)
   Iest(1) = 0
!
   file = Slt
   ib = 0
   imhere = 550
   DO j = 1 , Ido
      IF ( allin ) THEN
         DO i = 1 , 11
            Islt(i) = Iz(i+ib)
         ENDDO
         ib = ib + 11
      ELSE
         jsave = j
         IF ( j/=1 .OR. t3+q4<2 ) CALL read(*700,*800,Slt,Islt,11,0,flag)
      ENDIF
      DO
         IF ( Islt(1)<Iest(1) ) THEN
         ELSEIF ( Islt(1)==Iest(1) ) THEN
!
            IF ( ieltyp==tria3 ) THEN
!
!     PLOAD4 FOR TRIA3 ELEMENT
!     SET ISLT(1) TO NEGATIVE FOR PLOAD4/TRIA3 COMPUTATION
!
               IF ( debug ) WRITE (Nout,99005) Iest(1)
99005          FORMAT (' ==> PROCESS PLOAD4 FOR TRIA3 ELEM',I8)
               Islt(1) = -iabs(Islt(1))
               IF ( Iprec==2 ) THEN
                  CALL t3pl4d
               ELSE
                  CALL t3pl4s
               ENDIF
            ELSE
!
!     PLOAD4 FOR QUAD4 ELEMENT
!
               IF ( debug ) WRITE (Nout,99006) Iest(1)
99006          FORMAT (' ==> PROCESS PLOAD4 FOR QUAD ELEM',I8)
               IF ( Iprec==2 ) THEN
                  CALL plod4d
               ELSE
                  CALL plod4s
               ENDIF
            ENDIF
         ELSE
            CALL read(*400,*400,Est,Iest,nwords,0,flag)
            CYCLE
         ENDIF
         EXIT
      ENDDO
!
   ENDDO
!
 400  IF ( t3+q4>=2 ) THEN
!
      IF ( Jopen==1 ) CALL close(Est,1)
      Jopen = 0
      IF ( .NOT.(allin .OR. jsave>=Ido) ) THEN
         imhere = 590
         j = (Ido-jsave)*11
         CALL read(*900,*900,Slt,0,-j,0,flag)
      ENDIF
   ELSE
!
!     JUST FINISHED EITHER QUAD4 OR TRIA3 ELEMENT. BACKSPACE EST FILE,
!     AND BACKSPACE SLT FILE IF SLT DATA ARE NOT ALREADY IN CORE.
!     REPEAT PLOAD4 (LOAD TYPE 25) COMPUTAION FOR THE OTHER ELEMENT
!     (TRIA3 OR QUAD4) WHICH WE HAVE NOT YET PROCESSED IN THE FIRST
!     PASS. MUST STEP OVER OTHER LOADS THAT MIGHT BE PRESENT
!
      CALL bckrec(Est)
      q4 = q4 + 1
      jsave = 0
      IF ( allin ) THEN
         CALL read(*200,*400,Est,ieltyp,1,0,flag)
         GOTO 100
      ELSE
!
         CALL bckrec(Slt)
         imhere = 570
         DO
            CALL read(*700,*800,Slt,i,1,0,flag)
            IF ( i==25 ) THEN
               imhere = 573
               CALL read(*700,*800,Slt,i,1,0,flag)
               IF ( i==Ido ) THEN
                  imhere = 575
                  CALL read(*700,*800,Slt,Islt,6,0,flag)
                  IF ( Islt(6)==-1 ) THEN
                     imhere = 577
                     CALL read(*700,*800,Slt,Islt(7),5,0,flag)
                     IF ( Islt(7)==0 ) THEN
                        jsave = 1
                        CALL read(*200,*400,Est,ieltyp,1,0,flag)
                        GOTO 100
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      ENDIF
   ENDIF
 500  RETURN
!
 600  j = -1
   GOTO 1000
 700  j = -2
   GOTO 1000
 800  j = -3
   GOTO 1000
 900  j = 1
 1000 WRITE (Nout,99007) imhere , t3 , q4 , Ido , jsave
99007 FORMAT ('   IMHERE =',I5,'   T3,Q4 =',2I3,'   IDO,JSAVE =',2I5)
   CALL mesage(j,file,name(1))
   GOTO 500
END SUBROUTINE pload4
