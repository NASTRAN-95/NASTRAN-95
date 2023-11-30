
SUBROUTINE ddr1a(Pd,K2dd,B2dd,Mdd,Vud,Pad,Frl,Frqset,Scr1,Scr2,Scr3,Scr4,Itype,Scr5)
   IMPLICIT NONE
   REAL A(4) , Consts(5) , Core(1) , Twopi
   INTEGER Ieol , Ieor , Ii , Sysbuf
   COMMON /condas/ Consts
   COMMON /system/ Sysbuf
   COMMON /zntpkx/ A , Ii , Ieol , Ieor
   COMMON /zzzzzz/ Core
   INTEGER B2dd , Frl , Frqset , Itype , K2dd , Mdd , Pad , Pd , Scr1 , Scr2 , Scr3 , Scr4 , Scr5 , Vud
   REAL b(2) , w , w2
   INTEGER file , freq , i , iblk(60) , ibuf , ifile(3) , imcb(21) , ip1 , it , j , k , kk , l , mcb(7) , mcb1(7) , mcb2(7) ,       &
         & name(2) , nfreq , nload , nob2dd , nok2dd , nz , sr1 , sr3
   INTEGER korsz
!
!     ROUTINE TO COMPUTE PAD FROM MODAL APPROXIMATION TO SYSTEM
!
   !>>>>EQUIVALENCE (mcb(1),imcb(1)) , (mcb2(1),imcb(8)) , (mcb1(1),imcb(15)) , (Consts(2),Twopi)
   DATA name/4HDDR1 , 4HA   /
   DATA freq/4HFREQ/
!
!     INITIALIZE + FIND OUT WHAT EXISTS
!
   sr1 = Scr1
   sr3 = Scr3
   ibuf = korsz(Core) - Sysbuf + 1
   nok2dd = 1
   mcb(1) = K2dd
   CALL rdtrl(mcb)
   IF ( mcb(1)<=0 ) nok2dd = -1
   nob2dd = 1
   mcb(1) = B2dd
   CALL rdtrl(mcb)
   IF ( mcb(1)<=0 ) nob2dd = -1
   mcb(1) = Pd
   CALL rdtrl(mcb)
!
!     IS THIS FREQRES OR TRANSIENT
!
   IF ( Itype/=freq ) THEN
!
!     TRANSIENT ANALYSIS
!
      nload = mcb(2)
!
!     PUT DISPLACEMENT ON SCR5,VELOCITY ON SCR2,ACCELERATION SCR1
!
      it = 1
!
!     PUT HEADERS ON FILES
!
      GOTO 200
   ELSE
!
!     BRING IN FRL
!
      file = Frl
      CALL open(*300,Frl,Core(ibuf),0)
      CALL fread(Frl,0,-2,0)
      CALL read(*500,*100,Frl,Core(1),ibuf,0,nfreq)
      ip1 = -8
      GOTO 400
   ENDIF
 100  CALL close(Frl,1)
   nload = mcb(2)/nfreq
   it = 3
!
!     BUILD  ACCELERATION AND VELOCITY IF NEEDED
!
 200  CALL gopen(Vud,Core(ibuf),0)
!
!     PUT  ACCELERATION VECTOR ON SCR1
!
   nz = ibuf - Sysbuf
   CALL gopen(Scr1,Core(nz),1)
   CALL makmcb(mcb1,Scr1,mcb(3),2,it)
   IF ( nob2dd>=0 ) THEN
!
!     PUT VELOCITY VECTOR ON SCR2
!
      nz = nz - Sysbuf
      CALL gopen(Scr2,Core(nz),1)
      CALL makmcb(mcb2,Scr2,mcb(3),2,it)
   ENDIF
   IF ( Itype/=freq ) THEN
!
!     PUT DISPLACEMENT ON SCR5
!
      file = Scr5
      nz = nz - Sysbuf
      CALL gopen(Scr5,Core(nz),1)
      mcb(1) = Scr5
      mcb(2) = 0
      mcb(4) = 2
      mcb(5) = 1
      ifile(1) = Scr5
      ifile(2) = Scr2
      ifile(3) = Scr1
      mcb(6) = 0
      DO kk = 1 , nload
         CALL bldpk(1,1,Scr5,iblk,1)
         IF ( nob2dd>=0 ) CALL bldpk(1,1,Scr2,iblk(21),1)
         CALL bldpk(1,1,Scr1,iblk(41),1)
         DO i = 1 , 3
            l = i*7 - 6
            k = 20*i - 19
            file = ifile(i)
!
!     FWDREC OVER  UNNEEDED STUFF
!
            IF ( i==2 .AND. nob2dd<0 ) THEN
               CALL skprec(Vud,1)
               CYCLE
            ELSE
               CALL intpk(*210,Vud,0,1,0)
               DO WHILE ( Ieol==0 )
                  CALL zntpki
                  CALL bldpki(A,Ii,file,iblk(k))
               ENDDO
            ENDIF
!
!     END COLUMN
!
 210        CALL bldpkn(file,iblk(k),imcb(l))
         ENDDO
      ENDDO
!
!     FINISH OFF
!
      CALL close(Scr5,1)
      CALL wrttrl(mcb)
   ELSE
!
!     COMPUTE  VECTORS
!
      DO i = 1 , nfreq
         Core(i) = Core(i)*Twopi
      ENDDO
      DO j = 1 , nload
         DO i = 1 , nfreq
            w = Core(i)
            w2 = -w*w
            CALL bldpk(3,3,Scr1,iblk(1),1)
            IF ( nob2dd>=0 ) CALL bldpk(3,3,Scr2,iblk(21),1)
            CALL intpk(*220,Vud,0,3,0)
            DO WHILE ( Ieol==0 )
               CALL zntpki
               b(1) = w2*A(1)
               b(2) = w2*A(2)
               CALL bldpki(b(1),Ii,Scr1,iblk(1))
               IF ( nob2dd>=0 ) THEN
                  b(1) = -w*A(2)
                  b(2) = w*A(1)
                  CALL bldpki(b(1),Ii,Scr2,iblk(21))
               ENDIF
            ENDDO
!
!     END OF COLUMN
!
 220        CALL bldpkn(Scr1,iblk(1),mcb1(1))
            IF ( nob2dd>=0 ) CALL bldpkn(Scr2,iblk(21),mcb2(1))
         ENDDO
      ENDDO
   ENDIF
   CALL close(Scr1,1)
   CALL close(Vud,1)
   CALL wrttrl(mcb1(1))
   IF ( nob2dd>=0 ) THEN
      CALL close(Scr2,1)
      CALL wrttrl(mcb2(1))
   ENDIF
!
!     MULTIPLY OUT
!
   IF ( nob2dd<0 .AND. nok2dd<0 ) sr3 = Pad
   CALL ssg2b(Mdd,Scr1,Pd,sr3,0,1,0,Scr4)
   IF ( nok2dd<0 ) THEN
!
!     NO  K2DD
!
      sr1 = sr3
   ELSE
!
!     MULTIPLY  IN K2DD
!
      IF ( nob2dd<0 ) sr1 = Pad
      CALL ssg2b(K2dd,Scr5,sr3,sr1,0,1,0,Scr4)
   ENDIF
!
!     MULTIPLY IN B2DD
!
   IF ( nob2dd>=0 ) CALL ssg2b(B2dd,Scr2,sr1,Pad,0,1,0,Scr4)
   RETURN
!
!     ERROR MESAGES
!
 300  ip1 = -1
 400  CALL mesage(ip1,file,name)
 500  ip1 = -2
   GOTO 400
END SUBROUTINE ddr1a