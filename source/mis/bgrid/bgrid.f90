!*==bgrid.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE bgrid
   USE c_banda
   USE c_bandb
   USE c_bandd
   USE c_bands
   USE c_bandw
   USE c_geomx
   USE c_machin
   USE c_names
   USE c_system
   USE c_two
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: grid , sub
   INTEGER :: i , j , k , k1 , k2 , kore , max , n , nout
   INTEGER , SAVE :: igeom1 , igeom2 , igeom4 , iscr1 , kdimx , nelx , neqrx , neqx , seqgp
   INTEGER , DIMENSION(8) :: itrl
   EXTERNAL andf , bckrec , close , locate , mesage , preloc , rdtrl , read
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE COMPUTES PROBLEM SIZE, INTEGER PACKING FACTOR, AND
!     MAXGRD AND MAXDEG CONSTANTS.
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
   !>>>>EQUIVALENCE (Nout,Isys(2))
   DATA igeom1 , igeom2 , igeom4 , iscr1/201 , 208 , 210 , 301/
   DATA kdimx , nelx , neqx , neqrx/150 , 0 , 0 , 0/
   DATA grid , seqgp , sub/4501 , 45 , 53 , 4HBGRI , 4HD   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         IF ( irept==2 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         geom1 = igeom1
         geom2 = igeom2
         geom4 = igeom4
         scr1 = iscr1
         nel = nelx
         neq = neqx
         neqr = neqrx
         ngrid = 0
!
!     BANDIT QUITS IF DMI CARDS ARE PRESENT. (CHK WAS DONE IN IFS2P)
!     RE-SET PROGRAM PARAMETERS IF USER REQUESTED VIA NASTRAN CARD.
!
         k = isys(i77)
         IF ( k<0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( k/=0 ) THEN
            IF ( k==+9 ) THEN
               WRITE (nout,99006) uim
               WRITE (nout,99001)
99001          FORMAT (5X,25HTHE PRESENCE OF DMI CARDS)
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ELSE
               DO i = 1 , 7
                  itrl(i) = mod(k,10)
                  k = k/10
               ENDDO
               IF ( itrl(1)>0 .AND. itrl(1)<=4 ) icrit = itrl(1)
               IF ( itrl(2)>0 .AND. itrl(2)<=3 ) method = itrl(2) - 2
               nompc = itrl(3)
               IF ( itrl(4)==1 ) nodep = -nodep
               IF ( itrl(5)==1 ) nopch = -nopch
               IF ( itrl(5)==9 ) nopch = +9
               IF ( itrl(6)==1 ) norun = -norun
               IF ( itrl(7)>=2 .AND. itrl(7)<=9 ) kdim = itrl(7)
            ENDIF
         ENDIF
!
         IF ( norun/=+1 ) THEN
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
            itrl(1) = geom1
            CALL rdtrl(itrl)
            j = itrl(2) + itrl(3) + itrl(4) + itrl(5) + itrl(6) + itrl(7)
            IF ( itrl(1)<0 .OR. j==0 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            k = seqgp
            k1 = (k-1)/16
            k2 = k - 16*k1
            k = andf(itrl(k1+2),two(k2+16))
            IF ( k/=0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     WE ASSUME THAT THE GRID POINT DATA IN GEOM1 AT THIS TIME IS NOT
!     SORTED. IF IT IS, WE CAN BLAST READ THE GRID POINT RECORD AND
!     TAKE THE LAST GRID POINT TO BE THE LARGEST GRID EXTERNAL NUMBER.
!
         CALL preloc(*100,z(ibuf1),geom1)
         CALL locate(*40,z(ibuf1),grid,k)
         max = 0
         DO
            CALL read(*20,*20,geom1,itrl,8,0,k)
            ngrid = ngrid + 1
            IF ( itrl(1)>max ) max = itrl(1)
         ENDDO
 20      CALL bckrec(geom1)
 40      CALL close(geom1,norew)
!
!     IF SPOINTS ARE PRESENT, ADD THEM TO THE GRID COUNT
!
         n = 0
         CALL preloc(*80,z(ibuf1),geom2)
         ngpts(1) = 5551
         ngpts(2) = 49
         CALL locate(*60,z(ibuf1),ngpts,k)
         CALL read(*60,*60,geom2,z(1),ibuf1,1,n)
 60      CALL close(geom2,rew)
 80      ngpts(1) = ngrid
         ngpts(2) = n
         ngrid = ngrid + n
!
         IF ( nopch==9 .AND. ngrid==1 ) ngrid = max
         spag_nextblock_1 = 2
      CASE (2)
         IF ( ngrid<=0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ngrid<15 ) THEN
!
!     ERROR OR QUIT
!
            WRITE (nout,99006) uim
            WRITE (nout,99002)
99002       FORMAT (5X,'SMALL PROBLEM SIZE')
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
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
            IF ( machx==1 .OR. machx==3 ) THEN
               nw = 4
               IF ( ngrid>508 ) nw = 3
               IF ( ngrid>4095 ) nw = 2
            ELSEIF ( machx==2 .OR. machx==5 .OR. machx==6 .OR. machx==7 .OR. machx==8 .OR. machx==9 .OR. machx==10 .OR.             &
                   & machx==11 .OR. machx==13 .OR. machx==16 .OR. machx==17 .OR. machx==18 .OR. machx==19 .OR. machx==20 .OR.       &
                   & machx==21 .OR. machx==22 ) THEN
               nw = 2
            ELSEIF ( machx==12 ) THEN
               nw = 8
               IF ( ngrid>255 ) nw = 4
            ELSE
               nw = 6
               IF ( ngrid>510 ) nw = 5
               IF ( ngrid>2045 ) nw = 4
               IF ( ngrid>16380 ) nw = 3
               IF ( ngrid>524288 ) nw = 2
            ENDIF
!
            nbitin = nbpw/nw
            mask = 2**nbitin - 1
!
!     KDIM IS THE ARRAY DIMENSNION OF A SCRATCH ARRAY USED ONLY BY GPS
!     METHOD. IT IS 150 WORDS OR 10% OF TOTAL GRID POINT NUMBER. IF
!     USER SPECIFIED BANDTDIM = N, (WHERE N IS FROM 1 THRU 9), THE ARRAY
!     DIMENSION WILL BE N*10 PERCENT INSTEAD OF THE DEFAULT OF 10%.
!
            kdim = ngrid*kdim/10
            IF ( method/=-1 ) kdim = max0(kdim,kdimx,ngrid/10)
            IF ( method==-1 ) kdim = min0(kdim,kdimx,ngrid/10)
            n = ngrid
            IF ( n<10 ) n = 10
!
!     CALCULATE WIDTH MAXDEG AND EFFECTIVE LENGTH MAXGRD OF IG MATRIX.
!
            maxgrd = n
            kore = kor
            DO
               maxdeg = ((((kore-4*kdim-8*maxgrd-5)*nw)/(maxgrd+nw))/nw)*nw
               maxdeg = min0(maxdeg,maxgrd-1)
               IF ( maxdeg<=0 ) THEN
                  CALL mesage(-8,0,sub)
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  j = maxdeg*2.2
                  kore = kore - j
                  IF ( kor-j/=kore ) THEN
!
!     INITIALIZE VARIABLES
!
                     nn = 0
                     mm = 0
                     nedge = 0
                     ipass = 0
                     kmod = 2*maxgrd - ifix(2.3715*sqrt(float(maxgrd)))
                     mindeg = 500000
                     RETURN
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
 100     CALL mesage(-1,geom1,sub)
         spag_nextblock_1 = 3
      CASE (3)
         WRITE (nout,99006) uim
         WRITE (nout,99003)
99003    FORMAT (5X,25HTHE ABSENCE OF GRID CARDS)
         CALL close(geom1,rew)
         spag_nextblock_1 = 5
      CASE (4)
         WRITE (nout,99006) uim
         WRITE (nout,99004)
99004    FORMAT (5X,27HTHE PRESENCE OF SEQGP CARDS)
         spag_nextblock_1 = 5
      CASE (5)
         isys(i77) = 0
         IF ( nopch>0 ) isys(i77) = -2
         IF ( isys(i77)/=-2 ) WRITE (nout,99005)
99005    FORMAT (1H0,10X,'**NO ERRORS FOUND - EXECUTE NASTRAN PROGRAM**')
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99006 FORMAT (A29,' -  GRID-POINT RESEQUENCING PROCESSOR BANDIT IS ','NOT USED DUE TO')
END SUBROUTINE bgrid
