!*==gpstgn.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE gpstgn
   IMPLICIT NONE
   USE c_gpstgx
   USE c_gpstgy
   USE c_system
   USE c_unpakx
   USE c_zzzzzz
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(18) :: bs
   INTEGER :: i , ibuf1 , icore , ifile , iii , iloop , isilnx , ist , istx , ityp , j , logic , luset , n , npts
   INTEGER , DIMENSION(2) , SAVE :: isubnm
   INTEGER , DIMENSION(3) :: k
   INTEGER , SAVE :: kgg , sil
   INTEGER , DIMENSION(7) :: mcb
!
! End of declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
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
   !>>>>EQUIVALENCE (Bs,B)
!
   DATA kgg , sil/101 , 102/
   DATA isubnm/4HGPST , 4HGN  /
!
   gpst = 201
   igpst = 0
   nsing = 0
!WKBR 8/94 SPR93026      ITYPOT= 2
   itypot = iprec
   incr = 1
   k(1) = 1
   k(2) = 1
   ibuf1 = korsz(iz) - isysbf - 2
   ibuf2 = ibuf1 - isysbf
   ifile = sil
   CALL open(*200,sil,iz(ibuf1),0)
   CALL skprec(sil,1)
   mcb(1) = sil
   CALL rdtrl(mcb)
   luset = mcb(3)
   icore = luset + 1 - ibuf1
   IF ( icore<0 ) CALL read(*400,*100,sil,iz,ibuf1,0,npts)
   n = -8
   ifile = icore
   GOTO 300
 100  CALL close(sil,1)
   logic = 110
   IF ( npts/=mcb(2) ) THEN
      n = -7
      GOTO 300
   ELSE
      iz(npts+1) = luset + 1
!
      ifile = kgg
      CALL open(*200,kgg,iz(ibuf1),0)
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
            isil = iz(i)
            isilnx = iz(i+1)
            IF ( isilnx-isil==1 ) ityp = 2
            iloop = 1
            ist = 1
            ii = isil
            DO
               jj = ii + 2*(2-ityp)
               DO j = ii , jj
!WKBD 8/94 SPR93026      CALL UNPACK (*30,KGG,B(IST))
!WKBNB 8/94 SPR93026
                  IF ( iprec==1 ) CALL unpack(*102,kgg,bs(ist))
                  IF ( iprec==2 ) CALL unpack(*102,kgg,b(ist))
!WKBNE 8/94 SPR93026
                  GOTO 104
 102              istx = ist + 2
!WKBI 8/94 SPR93026
                  IF ( iprec==1 ) THEN
                     DO iii = ist , istx
                        bs(iii) = 0.0
                     ENDDO
                  ELSE
                     DO iii = ist , istx
                        b(iii) = 0.0D0
!WKBNB 8/94 SPR93026
                     ENDDO
                  ENDIF
 104              ist = ist + 3
               ENDDO
               IF ( ityp==2 ) THEN
!WKBD 8/94 SPR93026   70 IF (B(1).GT.0.0D0) GO TO 100
!WKBNB 8/94 SPR93026
                  IF ( iprec/=2 .OR. b(1)<=0.0D0 ) THEN
                     IF ( iprec/=1 .OR. bs(1)<=0.0 ) THEN
!WKBNE 8/94 SPR93026
                        k(3) = isil
                        IF ( igpst/=1 ) THEN
                           igpst = 1
                           CALL gopen(gpst,iz(ibuf2),1)
                        ENDIF
                        nsing = nsing + 1
                        CALL write(gpst,k,3,0)
                     ENDIF
                  ENDIF
                  EXIT
               ELSEIF ( iloop==2 ) THEN
!WKBD 8/94 SPR93026   90 CALL GPSTG
!WKBNB 8/94 SPR93026
                  IF ( iprec==1 ) CALL gpstgs
                  IF ( iprec==2 ) CALL gpstg
                  EXIT
               ELSE
                  iloop = 2
                  ii = ii + 3
               ENDIF
            ENDDO
!WKBNE 8/94 SPR93026
         ENDDO
         IF ( igpst/=0 ) THEN
            CALL write(gpst,0,0,1)
            CALL close(gpst,1)
            CALL makmcb(mcb,gpst,npts,luset,0)
            mcb(2) = nsing
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
99999 END SUBROUTINE gpstgn
