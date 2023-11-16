
SUBROUTINE gpstgn
   IMPLICIT NONE
!
! COMMON variable declarations
!
   DOUBLE PRECISION B(18)
   REAL Bs(18) , Dum(52)
   INTEGER Gpst , Ibuf2 , Igpst , Ii , Incr , Iprec , Isil , Isysbf , Itypot , Iz(1) , Jj , Nout , Nsing
   COMMON /gpstgx/ Gpst , Igpst , Isil , Nsing , Ibuf2
   COMMON /gpstgy/ B
   COMMON /system/ Isysbf , Nout , Dum , Iprec
   COMMON /unpakx/ Itypot , Ii , Jj , Incr
   COMMON /zzzzzz/ Iz
!
! Local variable declarations
!
   INTEGER i , ibuf1 , icore , ifile , iii , iloop , isilnx , ist , istx , isubnm(2) , ityp , j , k(3) , kgg , logic , luset ,      &
         & mcb(7) , n , npts , sil
   INTEGER korsz
!
! End of declarations
!
!
!     THIS MODULE GENERATES THE GRID POINT SINGULARITY TABLE
!     BY EXAMINING THE SUBMATRICES ALONG THE LEADING DIAGONAL
!     OF THE INPUT STIFFNESS MATRIX
!
!     MODULE DMAP SEQUENCE
!
!     GPSTGEN  KGG,SIL/GPST $
!
!
!
!WKBI 8/94 SPR93026
!
!WKBI 8/94 SPR93026   COMMON /SYSTEM/  ISYSBF
!WKBI 8/94 SPR93026
   EQUIVALENCE (Bs,B)
!
   DATA kgg , sil/101 , 102/
   DATA isubnm/4HGPST , 4HGN  /
!
   Gpst = 201
   Igpst = 0
   Nsing = 0
!WKBR 8/94 SPR93026      ITYPOT= 2
   Itypot = Iprec
   Incr = 1
   k(1) = 1
   k(2) = 1
   ibuf1 = korsz(Iz) - Isysbf - 2
   Ibuf2 = ibuf1 - Isysbf
   ifile = sil
   CALL open(*200,sil,Iz(ibuf1),0)
   CALL skprec(sil,1)
   mcb(1) = sil
   CALL rdtrl(mcb)
   luset = mcb(3)
   icore = luset + 1 - ibuf1
   IF ( icore<0 ) CALL read(*400,*100,sil,Iz,ibuf1,0,npts)
   n = -8
   ifile = icore
   GOTO 300
 100  CALL close(sil,1)
   logic = 110
   IF ( npts/=mcb(2) ) THEN
      n = -7
      GOTO 300
   ELSE
      Iz(npts+1) = luset + 1
!
      ifile = kgg
      CALL open(*200,kgg,Iz(ibuf1),0)
      CALL skprec(kgg,1)
      mcb(1) = kgg
      CALL rdtrl(mcb)
      logic = 120
      IF ( mcb(2)/=luset .OR. mcb(3)/=luset ) THEN
         n = -7
         GOTO 300
      ELSE
!
         DO i = 1 , npts
            ityp = 1
            Isil = Iz(i)
            isilnx = Iz(i+1)
            IF ( isilnx-Isil==1 ) ityp = 2
            iloop = 1
            ist = 1
            Ii = Isil
            DO
               Jj = Ii + 2*(2-ityp)
               DO j = Ii , Jj
!WKBD 8/94 SPR93026      CALL UNPACK (*30,KGG,B(IST))
!WKBNB 8/94 SPR93026
                  IF ( Iprec==1 ) CALL unpack(*102,kgg,Bs(ist))
                  IF ( Iprec==2 ) CALL unpack(*102,kgg,B(ist))
!WKBNE 8/94 SPR93026
                  GOTO 104
 102              istx = ist + 2
!WKBI 8/94 SPR93026
                  IF ( Iprec==1 ) THEN
                     DO iii = ist , istx
                        Bs(iii) = 0.0
                     ENDDO
                  ELSE
                     DO iii = ist , istx
                        B(iii) = 0.0D0
!WKBNB 8/94 SPR93026
                     ENDDO
                  ENDIF
 104              ist = ist + 3
               ENDDO
               IF ( ityp==2 ) THEN
!WKBD 8/94 SPR93026   70 IF (B(1).GT.0.0D0) GO TO 100
!WKBNB 8/94 SPR93026
                  IF ( Iprec/=2 .OR. B(1)<=0.0D0 ) THEN
                     IF ( Iprec/=1 .OR. Bs(1)<=0.0 ) THEN
!WKBNE 8/94 SPR93026
                        k(3) = Isil
                        IF ( Igpst/=1 ) THEN
                           Igpst = 1
                           CALL gopen(Gpst,Iz(Ibuf2),1)
                        ENDIF
                        Nsing = Nsing + 1
                        CALL write(Gpst,k,3,0)
                     ENDIF
                  ENDIF
                  EXIT
               ELSEIF ( iloop==2 ) THEN
!WKBD 8/94 SPR93026   90 CALL GPSTG
!WKBNB 8/94 SPR93026
                  IF ( Iprec==1 ) CALL gpstgs
                  IF ( Iprec==2 ) CALL gpstg
                  EXIT
               ELSE
                  iloop = 2
                  Ii = Ii + 3
               ENDIF
            ENDDO
!WKBNE 8/94 SPR93026
         ENDDO
         IF ( Igpst/=0 ) THEN
            CALL write(Gpst,0,0,1)
            CALL close(Gpst,1)
            CALL makmcb(mcb,Gpst,npts,luset,0)
            mcb(2) = Nsing
            CALL wrttrl(mcb)
         ENDIF
         CALL close(kgg,1)
         GOTO 99999
      ENDIF
   ENDIF
!
!     ERROR MESSAGES
!
 200  n = -1
 300  CALL mesage(n,ifile,isubnm)
 400  n = -2
   GOTO 300
!
99999 RETURN
END SUBROUTINE gpstgn
