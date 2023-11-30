
SUBROUTINE bgrid
   IMPLICIT NONE
   REAL Dum , Dum2s(2) , Dum4w(4) , Rdum(4) , Z(1)
   INTEGER Geom1 , Geom2 , Geom4 , I77 , Ibuf1 , Icrit , Idum5d(5) , Ipass , Irept , Isys(100) , Kdim , Kmod , Kor , Mach , Machx , &
         & Mask , Maxdeg , Maxgrd , Method , Mindeg , Mm , Nbitin , Nbpw , Nedge , Nel , Neq , Neqr , Ngpts(2) , Ngrid , Nn ,       &
         & Nodep , Nompc , Nopch , Norew , Norun , Nout , Nw , Nzero , Rew , Scr1 , Two(1)
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /banda / Ibuf1 , Nompc , Nodep , Nopch , Norun , Method , Icrit , Ngpts
   COMMON /bandb / Nbitin , Kor , Dum , Ngrid , Ipass , Nw , Kdim , Nbpw , Irept
   COMMON /bandd / Idum5d , Nzero , Nel , Neq , Neqr
   COMMON /bands / Nn , Mm , Dum2s , Maxgrd , Maxdeg , Kmod , Mach , Mindeg , Nedge , Mask
   COMMON /bandw / Dum4w , I77
   COMMON /geomx / Geom1 , Geom2 , Geom4 , Scr1
   COMMON /machin/ Machx
   COMMON /names / Rdum , Rew , Norew
   COMMON /system/ Isys
   COMMON /two   / Two
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ Z
   INTEGER andf
   INTEGER grid(2) , i , igeom1 , igeom2 , igeom4 , iscr1 , itrl(8) , j , k , k1 , k2 , kdimx , kore , max , n , nelx , neqrx ,     &
         & neqx , seqgp , sub(2)
   EXTERNAL andf
!
!     THIS ROUTINE COMPUTES PROBLEM SIZE, INTEGER PACKING FACTOR, AND
!     MAXGRD AND MAXDEG CONSTANTS.
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
   EQUIVALENCE (Nout,Isys(2))
   DATA igeom1 , igeom2 , igeom4 , iscr1/201 , 208 , 210 , 301/
   DATA kdimx , nelx , neqx , neqrx/150 , 0 , 0 , 0/
   DATA grid , seqgp , sub/4501 , 45 , 53 , 4HBGRI , 4HD   /
!
   IF ( Irept==2 ) GOTO 500
   Geom1 = igeom1
   Geom2 = igeom2
   Geom4 = igeom4
   Scr1 = iscr1
   Nel = nelx
   Neq = neqx
   Neqr = neqrx
   Ngrid = 0
!
!     BANDIT QUITS IF DMI CARDS ARE PRESENT. (CHK WAS DONE IN IFS2P)
!     RE-SET PROGRAM PARAMETERS IF USER REQUESTED VIA NASTRAN CARD.
!
   k = Isys(I77)
   IF ( k<0 ) GOTO 900
   IF ( k/=0 ) THEN
      IF ( k==+9 ) THEN
         WRITE (Nout,99006) Uim
         WRITE (Nout,99001)
99001    FORMAT (5X,25HTHE PRESENCE OF DMI CARDS)
         GOTO 900
      ELSE
         DO i = 1 , 7
            itrl(i) = mod(k,10)
            k = k/10
         ENDDO
         IF ( itrl(1)>0 .AND. itrl(1)<=4 ) Icrit = itrl(1)
         IF ( itrl(2)>0 .AND. itrl(2)<=3 ) Method = itrl(2) - 2
         Nompc = itrl(3)
         IF ( itrl(4)==1 ) Nodep = -Nodep
         IF ( itrl(5)==1 ) Nopch = -Nopch
         IF ( itrl(5)==9 ) Nopch = +9
         IF ( itrl(6)==1 ) Norun = -Norun
         IF ( itrl(7)>=2 .AND. itrl(7)<=9 ) Kdim = itrl(7)
      ENDIF
   ENDIF
!
   IF ( Norun/=+1 ) THEN
!
!     OPEN GEOM1 FILE AND CHECK THE PRESENCE OF ANY SEQGP CARD.  IF
!     ONE OR MORE IS PRESENT, ABORT BANDIT JOB.  OTHERWISE CONTINUE TO
!     COUNT HOW MANY GRID POINTS IN THE PROBLEM.
!     RESET GEOM1 TO THE BEGINNING OF GRID DATA FOR BSEQGP, AND CLOSE
!     GEOM1 WITHOUT REWINDING THE FILE
!
!     COMMENT FROM G.CHAN/SPERRY
!     IF TIME AND $ ALLOW, WE SHOULD MAKE USE OF THE SORTED GRID DATA
!     FROM GEOM1 FILE AND GET RID OF INV, INT, NORIG, ILD ARRAYS LATER.
!     THE SCATTERING TECHNEQUE (REALLY A HASHING METHOD) CAN BE REPLACED
!     BY A SIMPLE BINARY SEARCH. ROUTINES SCAT, BRIGIT, AND INTERN
!     COULD BE ELIMINATED.
!
      itrl(1) = Geom1
      CALL rdtrl(itrl)
      j = itrl(2) + itrl(3) + itrl(4) + itrl(5) + itrl(6) + itrl(7)
      IF ( itrl(1)<0 .OR. j==0 ) GOTO 900
      k = seqgp
      k1 = (k-1)/16
      k2 = k - 16*k1
      k = andf(itrl(k1+2),Two(k2+16))
      IF ( k/=0 ) GOTO 800
   ENDIF
!
!     WE ASSUME THAT THE GRID POINT DATA IN GEOM1 AT THIS TIME IS NOT
!     SORTED. IF IT IS, WE CAN BLAST READ THE GRID POINT RECORD AND
!     TAKE THE LAST GRID POINT TO BE THE LARGEST GRID EXTERNAL NUMBER.
!
   CALL preloc(*600,Z(Ibuf1),Geom1)
   CALL locate(*200,Z(Ibuf1),grid,k)
   max = 0
   DO
      CALL read(*100,*100,Geom1,itrl,8,0,k)
      Ngrid = Ngrid + 1
      IF ( itrl(1)>max ) max = itrl(1)
   ENDDO
 100  CALL bckrec(Geom1)
 200  CALL close(Geom1,Norew)
!
!     IF SPOINTS ARE PRESENT, ADD THEM TO THE GRID COUNT
!
   n = 0
   CALL preloc(*400,Z(Ibuf1),Geom2)
   Ngpts(1) = 5551
   Ngpts(2) = 49
   CALL locate(*300,Z(Ibuf1),Ngpts,k)
   CALL read(*300,*300,Geom2,Z(1),Ibuf1,1,n)
 300  CALL close(Geom2,Rew)
 400  Ngpts(1) = Ngrid
   Ngpts(2) = n
   Ngrid = Ngrid + n
!
   IF ( Nopch==9 .AND. Ngrid==1 ) Ngrid = max
 500  IF ( Ngrid<=0 ) GOTO 700
   IF ( Ngrid<15 ) THEN
!
!     ERROR OR QUIT
!
      WRITE (Nout,99006) Uim
      WRITE (Nout,99002)
99002 FORMAT (5X,'SMALL PROBLEM SIZE')
      GOTO 900
   ELSE
!
!     SET WORD PACKING CONSTANT, NW - (NUMBER OF INTEGERS PER WORD)
!     MACHX =  1 DUMMY,   =  2 IBM 360/370, =  3 UNIVAC 1100, =  4 CDC,
!           =  5 VAX 780, =  6 DEC ULTRIX,  =  7 SUN,         =  8 AIX,
!           =  9 HP,      = 10 SILIC.GRAPH  = 11 MAC,         = 12 CRAY,
!           = 13 CONVEX,  = 14 NEC          = 15 FUJITSU,     = 16 DG,
!           = 17 AMDAHL   = 18 PRIME        = 19 486,         = 20 DUMMY
!           = 21 ALPHA    = 22 RESERVED
!
      IF ( Machx==1 .OR. Machx==3 ) THEN
         Nw = 4
         IF ( Ngrid>508 ) Nw = 3
         IF ( Ngrid>4095 ) Nw = 2
      ELSEIF ( Machx==2 .OR. Machx==5 .OR. Machx==6 .OR. Machx==7 .OR. Machx==8 .OR. Machx==9 .OR. Machx==10 .OR. Machx==11 .OR.    &
             & Machx==13 .OR. Machx==16 .OR. Machx==17 .OR. Machx==18 .OR. Machx==19 .OR. Machx==20 .OR. Machx==21 .OR. Machx==22 ) &
             & THEN
         Nw = 2
      ELSEIF ( Machx==12 ) THEN
         Nw = 8
         IF ( Ngrid>255 ) Nw = 4
      ELSE
         Nw = 6
         IF ( Ngrid>510 ) Nw = 5
         IF ( Ngrid>2045 ) Nw = 4
         IF ( Ngrid>16380 ) Nw = 3
         IF ( Ngrid>524288 ) Nw = 2
      ENDIF
!
      Nbitin = Nbpw/Nw
      Mask = 2**Nbitin - 1
!
!     KDIM IS THE ARRAY DIMENSNION OF A SCRATCH ARRAY USED ONLY BY GPS
!     METHOD. IT IS 150 WORDS OR 10% OF TOTAL GRID POINT NUMBER. IF
!     USER SPECIFIED BANDTDIM = N, (WHERE N IS FROM 1 THRU 9), THE ARRAY
!     DIMENSION WILL BE N*10 PERCENT INSTEAD OF THE DEFAULT OF 10%.
!
      Kdim = Ngrid*Kdim/10
      IF ( Method/=-1 ) Kdim = max0(Kdim,kdimx,Ngrid/10)
      IF ( Method==-1 ) Kdim = min0(Kdim,kdimx,Ngrid/10)
      n = Ngrid
      IF ( n<10 ) n = 10
!
!     CALCULATE WIDTH MAXDEG AND EFFECTIVE LENGTH MAXGRD OF IG MATRIX.
!
      Maxgrd = n
      kore = Kor
      DO
         Maxdeg = ((((kore-4*Kdim-8*Maxgrd-5)*Nw)/(Maxgrd+Nw))/Nw)*Nw
         Maxdeg = min0(Maxdeg,Maxgrd-1)
         IF ( Maxdeg<=0 ) THEN
            CALL mesage(-8,0,sub)
            GOTO 800
         ELSE
            j = Maxdeg*2.2
            kore = kore - j
            IF ( Kor-j/=kore ) THEN
!
!     INITIALIZE VARIABLES
!
               Nn = 0
               Mm = 0
               Nedge = 0
               Ipass = 0
               Kmod = 2*Maxgrd - ifix(2.3715*sqrt(float(Maxgrd)))
               Mindeg = 500000
               RETURN
            ENDIF
         ENDIF
      ENDDO
   ENDIF
 600  CALL mesage(-1,Geom1,sub)
 700  WRITE (Nout,99006) Uim
   WRITE (Nout,99003)
99003 FORMAT (5X,25HTHE ABSENCE OF GRID CARDS)
   CALL close(Geom1,Rew)
   GOTO 900
 800  WRITE (Nout,99006) Uim
   WRITE (Nout,99004)
99004 FORMAT (5X,27HTHE PRESENCE OF SEQGP CARDS)
 900  Isys(I77) = 0
   IF ( Nopch>0 ) Isys(I77) = -2
   IF ( Isys(I77)/=-2 ) WRITE (Nout,99005)
99005 FORMAT (1H0,10X,'**NO ERRORS FOUND - EXECUTE NASTRAN PROGRAM**')
99006 FORMAT (A29,' -  GRID-POINT RESEQUENCING PROCESSOR BANDIT IS ','NOT USED DUE TO')
END SUBROUTINE bgrid
