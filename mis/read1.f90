
SUBROUTINE read1(Dm,Mr,Scr1,Scr2,Scr3,Phia,Uset,Nr1,Lama,Scr4)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Core(1) , Ua , Uf , Ug , Ul , Um , Un , Uo , Ur , Us , Usb , Usg
   DOUBLE PRECISION Dcore(1)
   INTEGER Ii , Ii1 , Incur , Incur1 , Ita1 , Itb , Itb1 , Jj , Jj1 , Ksystm(63) , Nout , Sysbuf
   CHARACTER*23 Ufm
   COMMON /bitpos/ Um , Uo , Ur , Usg , Usb , Ul , Ua , Uf , Us , Un , Ug
   COMMON /packx / Ita1 , Itb1 , Ii1 , Jj1 , Incur1
   COMMON /system/ Sysbuf , Nout , Ksystm
   COMMON /unpakx/ Itb , Ii , Jj , Incur
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Core
!
! Dummy argument declarations
!
   INTEGER Dm , Lama , Mr , Nr1 , Phia , Scr1 , Scr2 , Scr3 , Scr4
   REAL Uset
!
! Local variable declarations
!
   INTEGER i , i3 , ialph , iloop , imr(7) , iphi , iscr1(7) , ivi , ivi2 , j , k , kk , kkk , l , lc , nam(2) , nprob , nr , nr2
   INTEGER korsz
   DOUBLE PRECISION si , term
   REAL ssi , sterm
!
! End of declarations
!
!
   EQUIVALENCE (Dcore(1),Core(1))
   DATA nam/4HREAD , 4H1   /
!
!     BRING MR INTO CORE
!
   lc = korsz(Core) - Sysbuf
   CALL gopen(Mr,Core(lc+1),0)
   imr(1) = Mr
   CALL rdtrl(imr)
   nr = imr(2)
   Nr1 = nr
   Ii = 1
   Jj = nr
   Incur = 1
   Itb = imr(5)
   nr2 = Itb*nr
   ivi = nr*nr
   iphi = ivi
   ivi2 = Itb*ivi
   ialph = 2*ivi
   iloop = 0
   k = 0
   DO i = 1 , nr
      CALL unpack(*50,Mr,Core(k+1))
      GOTO 100
!
!     NULL COLUMN
!
 50   DO j = 1 , nr2
         Core(j+k) = 0.0
      ENDDO
 100  kkk = k + ivi2
      DO j = 1 , nr2
         Core(j+kkk) = 0.0
      ENDDO
      IF ( Itb==1 ) THEN
         Core(kkk+i) = 1.0
      ELSE
         kkk = kkk/2
         Dcore(kkk+i) = 1.0D0
      ENDIF
      k = k + nr2
   ENDDO
   CALL close(Mr,1)
!
!     COMPUTE SI
!
   IF ( Itb/=2 ) THEN
      DO
         ssi = 0.0
         DO i = 1 , nr
            sterm = 0.0
            DO j = 1 , nr
               k = (j-1)*nr + i
               kk = ivi + j
               sterm = sterm + Core(k)*Core(kk)
            ENDDO
            k = ivi + i
            ssi = ssi + sterm*Core(k)
         ENDDO
         IF ( ssi<=0.0 ) GOTO 300
         ssi = 1.0/sqrt(ssi)
!
!     CONVERT VI INTO PHI
!
         DO i = 1 , nr
            k = ivi + i
            Core(k) = Core(k)*ssi
         ENDDO
         iloop = iloop + 1
         IF ( iloop==nr ) GOTO 500
!
!     CALCULATE ALPHAJ
!
         DO j = 1 , iloop
            k = ialph + j
            Core(k) = 0.0
            DO i = 1 , nr
               sterm = 0.0
               DO l = 1 , nr
                  kk = (l-1)*nr + i
                  kkk = ivi + nr + l
                  sterm = sterm + Core(kk)*Core(kkk)
               ENDDO
               kk = iphi + (j-1)*nr + i
               Core(k) = Core(k) + sterm*Core(kk)
            ENDDO
         ENDDO
!
!     COMPUTE NEXT V VECTOR
!
         DO i = 1 , nr
            sterm = 0.0
            DO j = 1 , iloop
               kk = ialph + j
               k = iphi + (j-1)*nr + i
               sterm = sterm + Core(kk)*Core(k)
            ENDDO
            k = ivi + nr + i
            Core(k) = Core(k) - sterm
         ENDDO
         ivi = ivi + nr
      ENDDO
   ENDIF
 200  si = 0.0D0
   DO i = 1 , nr
      term = 0.0D0
      DO j = 1 , nr
         k = (j-1)*nr + i
         kk = ivi + j
         term = term + Dcore(k)*Dcore(kk)
      ENDDO
      k = ivi + i
      si = si + term*Dcore(k)
   ENDDO
   IF ( si>0.0D0 ) GOTO 400
 300  WRITE (Nout,99001) Ufm
99001 FORMAT (A23,' 2200, INCONSISTENT RIGID BODY SYSTEM.')
   CALL mesage(-61,0,nam)
 400  si = 1.0D0/dsqrt(si)
!
!     CONVERT VI INTO PHI
!
   DO i = 1 , nr
      k = ivi + i
      Dcore(k) = Dcore(k)*si
   ENDDO
   iloop = iloop + 1
   IF ( iloop/=nr ) THEN
!
!     CALCULATE ALPHAJ
!
      DO j = 1 , iloop
         k = ialph + j
         Dcore(k) = 0.0D0
         DO i = 1 , nr
            term = 0.0D0
            DO l = 1 , nr
               kk = (l-1)*nr + i
               kkk = ivi + nr + l
               term = term + Dcore(kk)*Dcore(kkk)
            ENDDO
            kk = iphi + (j-1)*nr + i
            Dcore(k) = Dcore(k) + term*Dcore(kk)
         ENDDO
      ENDDO
!
!     COMPUTE NEXT V VECTOR
!
      DO i = 1 , nr
         term = 0.0D0
         DO j = 1 , iloop
            kk = ialph + j
            k = iphi + (j-1)*nr + i
            term = term + Dcore(kk)*Dcore(k)
         ENDDO
         k = ivi + nr + i
         Dcore(k) = Dcore(k) - term
      ENDDO
      ivi = ivi + nr
      GOTO 200
   ENDIF
!
!     PACK PHIRO
!
 500  Ita1 = Itb
   Itb1 = Itb
   Ii1 = 1
   Jj1 = nr
   Incur1 = 1
   CALL gopen(Scr1,Core(lc+1),1)
   CALL makmcb(iscr1,Scr1,nr,1,Itb)
   DO i = 1 , nr
      k = ivi2 + (i-1)*nr2
      CALL pack(Core(k+1),Scr1,iscr1)
   ENDDO
   CALL close(Scr1,1)
   CALL wrttrl(iscr1(1))
!
!     COMPUTE PHILO = DM*PHIRO
!
   CALL ssg2b(Dm,Scr1,0,Scr2,0,Itb,1,Scr4)
!
!     MERGE PHIRP AND PHILO TO FORM PHIA
!
   CALL sdr1b(Scr3,Scr2,Scr1,Scr4,Ua,Ul,Ur,Uset,0,0)
   CALL gopen(Scr4,Core(lc+1),0)
   lc = lc - Sysbuf
   CALL gopen(Phia,Core(lc+1),1)
   imr(1) = Scr4
   CALL rdtrl(imr(1))
   nprob = imr(3)
   Dcore(1) = 0.D0
   Jj = nprob
   Incur = 1
   i3 = 3
   DO j = 1 , nr
      Ii = 0
      CALL unpack(*550,Scr4,Core(i3))
      Ii1 = Ii
      Jj1 = Jj
      CALL pack(Core(i3),Phia,iscr1)
      CYCLE
!
!     NULL COLUMN
!
 550  Ii1 = 1
      Jj1 = 1
      CALL pack(Core,Phia,iscr1)
   ENDDO
   CALL close(Scr4,1)
   CALL close(Phia,1)
   lc = lc + Sysbuf
!
!     PUT NR ZEROS ON LAMA
!
   CALL gopen(Lama,Core(lc+1),1)
   Dcore(1) = 0.D0
   DO i = 1 , nr
      CALL write(Lama,Core,Itb,1)
   ENDDO
   CALL close(Lama,2)
END SUBROUTINE read1
