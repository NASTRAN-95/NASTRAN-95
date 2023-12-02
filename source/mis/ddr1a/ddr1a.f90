!*==ddr1a.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ddr1a(Pd,K2dd,B2dd,Mdd,Vud,Pad,Frl,Frqset,Scr1,Scr2,Scr3,Scr4,Itype,Scr5)
   USE c_condas
   USE c_system
   USE c_zntpkx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Pd
   INTEGER :: K2dd
   INTEGER :: B2dd
   INTEGER :: Mdd
   INTEGER :: Vud
   INTEGER :: Pad
   INTEGER :: Frl
   INTEGER :: Frqset
   INTEGER :: Scr1
   INTEGER :: Scr2
   INTEGER :: Scr3
   INTEGER :: Scr4
   INTEGER :: Itype
   INTEGER :: Scr5
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(2) :: b
   INTEGER :: file , i , ibuf , ip1 , it , j , k , kk , l , nfreq , nload , nob2dd , nok2dd , nz , sr1 , sr3
   INTEGER , SAVE :: freq
   INTEGER , DIMENSION(60) :: iblk
   INTEGER , DIMENSION(3) :: ifile
   INTEGER , DIMENSION(21) :: imcb
   INTEGER , DIMENSION(7) :: mcb , mcb1 , mcb2
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL :: twopi , w , w2
   EXTERNAL bldpk , bldpki , bldpkn , close , fread , gopen , intpk , korsz , makmcb , mesage , open , rdtrl , read , skprec ,      &
          & ssg2b , wrttrl , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     ROUTINE TO COMPUTE PAD FROM MODAL APPROXIMATION TO SYSTEM
!
   !>>>>EQUIVALENCE (mcb(1),imcb(1)) , (mcb2(1),imcb(8)) , (mcb1(1),imcb(15)) , (Consts(2),Twopi)
   DATA name/4HDDR1 , 4HA   /
   DATA freq/4HFREQ/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     INITIALIZE + FIND OUT WHAT EXISTS
!
         sr1 = Scr1
         sr3 = Scr3
         ibuf = korsz(core) - sysbuf + 1
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
!
!     PUT HEADERS ON FILES
!
            it = 1
            spag_nextblock_1 = 2
         ELSE
!
!     BRING IN FRL
!
            file = Frl
            CALL open(*40,Frl,core(ibuf),0)
            CALL fread(Frl,0,-2,0)
            CALL read(*60,*20,Frl,core(1),ibuf,0,nfreq)
            ip1 = -8
            spag_nextblock_1 = 3
         ENDIF
         CYCLE
 20      CALL close(Frl,1)
         nload = mcb(2)/nfreq
         it = 3
         spag_nextblock_1 = 2
      CASE (2)
!
!     BUILD  ACCELERATION AND VELOCITY IF NEEDED
!
         CALL gopen(Vud,core(ibuf),0)
!
!     PUT  ACCELERATION VECTOR ON SCR1
!
         nz = ibuf - sysbuf
         CALL gopen(Scr1,core(nz),1)
         CALL makmcb(mcb1,Scr1,mcb(3),2,it)
         IF ( nob2dd>=0 ) THEN
!
!     PUT VELOCITY VECTOR ON SCR2
!
            nz = nz - sysbuf
            CALL gopen(Scr2,core(nz),1)
            CALL makmcb(mcb2,Scr2,mcb(3),2,it)
         ENDIF
         IF ( Itype/=freq ) THEN
!
!     PUT DISPLACEMENT ON SCR5
!
            file = Scr5
            nz = nz - sysbuf
            CALL gopen(Scr5,core(nz),1)
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
                     CALL intpk(*22,Vud,0,1,0)
                     DO WHILE ( ieol==0 )
                        CALL zntpki
                        CALL bldpki(a,ii,file,iblk(k))
                     ENDDO
                  ENDIF
!
!     END COLUMN
!
 22               CALL bldpkn(file,iblk(k),imcb(l))
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
               core(i) = core(i)*twopi
            ENDDO
            DO j = 1 , nload
               DO i = 1 , nfreq
                  w = core(i)
                  w2 = -w*w
                  CALL bldpk(3,3,Scr1,iblk(1),1)
                  IF ( nob2dd>=0 ) CALL bldpk(3,3,Scr2,iblk(21),1)
                  CALL intpk(*24,Vud,0,3,0)
                  DO WHILE ( ieol==0 )
                     CALL zntpki
                     b(1) = w2*a(1)
                     b(2) = w2*a(2)
                     CALL bldpki(b(1),ii,Scr1,iblk(1))
                     IF ( nob2dd>=0 ) THEN
                        b(1) = -w*a(2)
                        b(2) = w*a(1)
                        CALL bldpki(b(1),ii,Scr2,iblk(21))
                     ENDIF
                  ENDDO
!
!     END OF COLUMN
!
 24               CALL bldpkn(Scr1,iblk(1),mcb1(1))
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
 40      ip1 = -1
         spag_nextblock_1 = 3
      CASE (3)
         CALL mesage(ip1,file,name)
 60      ip1 = -2
         spag_nextblock_1 = 3
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ddr1a
