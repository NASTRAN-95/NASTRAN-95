
SUBROUTINE gp4sp(Ibuf1,Ibuf2,Ibuf3)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Dd(78) , Dum(3) , Head(1) , React , Repeat , Single
   INTEGER Iautsp , Idsub , Iiii , Incr , Iogpst , Ioutpt , Ipunch , Irgt , Isysbf , Itypot , Iz(1) , Jdum(6) , Jjjj , Kdum(2) ,    &
         & Line , Luset , Mpcf1 , Mpcf2 , Mpcset , Msksng , Mskul , Mskum , Mskuo , Mskur , Mskus , Nauto , Nlpp , Noa , Nol ,      &
         & Nosets , Nskip , Omit1 , Spcset
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Luset , Mpcf1 , Mpcf2 , Single , Omit1 , React , Nskip , Repeat , Nosets , Nol , Noa , Idsub , Iautsp
   COMMON /gp4fil/ Dum , Irgt
   COMMON /gp4spx/ Mskum , Mskuo , Mskur , Mskus , Mskul , Msksng , Spcset , Mpcset , Nauto , Iogpst
   COMMON /output/ Head
   COMMON /system/ Isysbf , Ioutpt , Jdum , Nlpp , Kdum , Line , Dd , Ipunch
   COMMON /unpakx/ Itypot , Iiii , Jjjj , Incr
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Iz
!
! Dummy argument declarations
!
   INTEGER Ibuf1 , Ibuf2 , Ibuf3
!
! Local variable declarations
!
   INTEGER andf , complf , lshift , orf
   INTEGER eqexin , gpst , i , ibase , icore , idum , ieqexn , ierror , iexcld(9) , ifile , iflag , igpid , ii , iii , iloop , ims ,&
         & index , indxms(9) , initl , iok , iold , iordr , iponts(9) , iscr2 , isil , ispc , ist , istart , isubnm(2) , ityp ,     &
         & iword(8) , j , jj , jponts(9) , k , kk , logic , mcb(7) , mskin , mskms , mskxx , multi , ncard , neqexn , npts , num ,  &
         & ogpst , ogpst1(10) , scr2
   EXTERNAL andf , complf , lshift , orf
!
! End of declarations
!
!
!     ROUTINE TO LOOK AT GPST TO ELIMINATE SINGULARITIES
!
!
   DATA eqexin , gpst , ogpst , scr2/103 , 107 , 205 , 302/
   DATA isubnm/4HGP4S , 4HP   /
   DATA ncard , ierror/2*0/
   DATA iscr2 , ieqexn/2* - 1/
!
!
   index = iabs(Iautsp)
   IF ( index>2 ) THEN
      ierror = 1
      WRITE (Ioutpt,99001) Uwm
99001 FORMAT (A25,' 2439, ILLEGAL VALUE INPUT FOR PARAMETER AUTOSPC - ','SINGULARITY PROCESSING SKIPPED IN MODULE GP4')
      GOTO 1000
   ELSE
      IF ( index==2 .AND. Omit1>0 ) index = 3
      mskms = orf(Mskum,Mskus)
      IF ( Iautsp/=0 ) THEN
         multi = 0
         IF ( Mpcf1/=-1 ) THEN
            mskin = lshift(1,12)
            mskxx = complf(mskin)
            CALL gopen(Irgt,Iz(Ibuf1),0)
            Itypot = 1
            Iiii = 1
            Jjjj = 1
            Incr = 1
            DO i = 1 , Luset
               CALL unpack(*10,Irgt,idum)
               IF ( andf(Iz(i),mskms)==0 ) THEN
                  multi = 1
                  Iz(i) = orf(Iz(i),mskin)
               ENDIF
 10         ENDDO
            CALL close(Irgt,1)
         ENDIF
      ENDIF
      CALL open(*1400,gpst,Iz(Ibuf1),0)
      ifile = gpst
      CALL fwdrec(*1500,gpst)
   ENDIF
 100  DO
!
      CALL read(*1000,*1000,gpst,iordr,1,0,iflag)
      initl = iordr
      ims = 0
      ispc = 0
      DO i = 1 , 9
         indxms(i) = 0
         iexcld(i) = 0
         iponts(i) = 0
         jponts(i) = 0
      ENDDO
      CALL fread(gpst,npts,1,0)
      CALL fread(gpst,iponts,npts,0)
      ibase = iponts(1)
!
!     SET VARIOUS FLAGS FOR THE SINGULARITIES
!
      DO i = 1 , npts
         ii = iponts(i)
         j = Iz(ii)
         IF ( andf(j,mskms)/=0 ) indxms(i) = 1
         IF ( Iautsp/=0 ) THEN
            IF ( andf(j,Mskuo)/=0 .AND. andf(j,Mskul)/=0 ) GOTO 1800
            IF ( andf(j,Mskuo)/=0 .AND. andf(j,Mskur)/=0 ) GOTO 1800
            IF ( andf(j,Mskur)/=0 .AND. andf(j,Mskul)/=0 ) GOTO 1800
            IF ( andf(j,Mskur)/=0 ) iexcld(i) = 1
            IF ( multi/=0 .AND. indxms(i)==0 ) THEN
               IF ( andf(j,mskin)/=0 ) iexcld(i) = 1
            ENDIF
            IF ( index>=3 ) THEN
               IF ( andf(j,Mskul)/=0 ) iexcld(i) = 1
            ENDIF
         ENDIF
      ENDDO
!
!     DETERMINE THE ORDER OF SINGULARITY
!
      IF ( iordr<2 ) THEN
!
!     FIRST ORDER SINGULARITY
!
         DO i = 1 , npts
            IF ( indxms(i)/=0 ) GOTO 200
         ENDDO
         IF ( Iautsp/=0 ) THEN
            DO i = 1 , npts
               IF ( iexcld(i)==0 ) THEN
                  ii = iponts(i)
                  Iz(ii) = Msksng
                  Nauto = Nauto + 1
                  ispc = ispc + 1
                  jponts(ispc) = ii
                  EXIT
               ENDIF
            ENDDO
         ENDIF
         EXIT
      ELSEIF ( iordr==2 ) THEN
!
!     SECOND ORDER SINGULARITY
!
         iloop = 1
         DO
            DO i = 1 , npts , 2
               ii = iponts(i)
               IF ( ii/=0 ) THEN
                  IF ( indxms(i)/=0 ) THEN
                     ims = ims + 1
                  ELSE
                     IF ( iloop==1 ) GOTO 105
                     IF ( iexcld(i)/=0 ) GOTO 105
                     Iz(ii) = Msksng
                     Nauto = Nauto + 1
                     ispc = ispc + 1
                     jponts(ispc) = ii
                  ENDIF
                  iordr = 1
                  DO iii = 1 , npts
                     IF ( iponts(iii)==ii ) iponts(iii) = 0
                  ENDDO
                  ii = 0
               ENDIF
 105           jj = iponts(i+1)
               IF ( jj/=0 ) THEN
                  IF ( indxms(i+1)/=0 ) THEN
                     ims = ims + 1
                  ELSE
                     IF ( iloop==1 ) CYCLE
                     IF ( iexcld(i+1)/=0 ) CYCLE
                     Iz(jj) = Msksng
                     Nauto = Nauto + 1
                     ispc = ispc + 1
                     jponts(ispc) = jj
                  ENDIF
               ENDIF
               IF ( ii/=0 ) THEN
                  iordr = 1
                  DO iii = 1 , npts
                     IF ( iponts(iii)==jj ) iponts(iii) = 0
                  ENDDO
               ELSE
                  logic = 160
                  IF ( ispc+ims<2 ) GOTO 1900
                  IF ( ispc/=0 ) GOTO 900
                  GOTO 200
               ENDIF
            ENDDO
            IF ( Iautsp==0 ) GOTO 800
            IF ( iloop==2 ) GOTO 800
            iloop = 2
         ENDDO
      ELSE
!
!     THIRD ORDER SINGULARITY
!
         iok = 0
         DO i = 1 , npts
            IF ( indxms(i)/=0 ) THEN
               ims = ims + 1
            ELSEIF ( Iautsp==0 ) THEN
               iok = 1
               CYCLE
            ELSEIF ( iexcld(i)/=0 ) THEN
               iok = 1
               CYCLE
            ELSE
               ii = iponts(i)
               Iz(ii) = Msksng
               Nauto = Nauto + 1
               ispc = ispc + 1
               jponts(ispc) = ii
            ENDIF
            iordr = iordr - 1
            iponts(i) = 0
         ENDDO
         IF ( iok==1 ) THEN
            iok = 0
            DO i = 1 , npts
               IF ( iponts(i)/=0 ) THEN
                  iok = iok + 1
                  iponts(iok) = iponts(i)
                  IF ( iok/=i ) iponts(i) = 0
               ENDIF
            ENDDO
            npts = iok
            EXIT
         ELSE
            logic = 200
            IF ( ispc+ims/=3 ) GOTO 1900
            IF ( ispc/=0 ) EXIT
         ENDIF
      ENDIF
 200  ENDDO
!
!
 300  logic = 100
   IF ( ispc>initl ) GOTO 1900
   IF ( ieqexn==1 ) GOTO 500
   ieqexn = 1
!
!     BRING IN EQEXIN
!
   CALL gopen(eqexin,Iz(Ibuf2),0)
   CALL skprec(eqexin,1)
   mcb(1) = eqexin
   CALL rdtrl(mcb)
   icore = Luset + 2*(mcb(2)+1) - Ibuf2
   IF ( icore<0 ) THEN
      ifile = eqexin
      CALL read(*1500,*400,eqexin,Iz(Luset+1),Ibuf2-Luset,0,neqexn)
   ENDIF
   GOTO 1700
 400  CALL close(eqexin,1)
   CALL sort(0,0,2,2,Iz(Luset+1),neqexn)
   Iz(Luset+neqexn+1) = 0
   Iz(Luset+neqexn+2) = 10*(Luset+1)
   neqexn = neqexn + 2
!
!     LOOK UP SIL IN EQEXIN
!
   istart = 2
 500  kk = ibase
   DO i = istart , neqexn , 2
      k = Luset + i
      isil = Iz(k)/10
      IF ( kk<isil ) GOTO 600
   ENDDO
   logic = 110
   GOTO 1900
!
!     PICK UP POINT ID AND TYPE (GRID OR SCALAR) FROM EQEXIN
!
 600  igpid = Iz(k-3)
   isil = Iz(k-2)/10
   ityp = Iz(k-2) - 10*isil
   istart = i - 2
   IF ( ityp/=1 ) THEN
!
!     SCALAR POINT
!
      iordr = 0
      npts = 0
   ENDIF
!
!
   IF ( ispc/=0 ) THEN
      logic = 120
      IF ( ityp==2 .AND. ispc>1 ) GOTO 1900
      IF ( ityp==2 ) jponts(1) = 0
      IF ( iscr2/=1 ) THEN
         iscr2 = 1
         iword(1) = Spcset
         IF ( iword(1)<=0 ) iword(1) = 1
!
!     INITIALIZE SCR2
!
         CALL gopen(scr2,Iz(Ibuf3),1)
      ENDIF
!
!     WRITE AUTOMATICALLY GENERATED SPC1 DATA ON SCR2
!
      DO i = 1 , ispc
         IF ( ityp/=2 ) THEN
            IF ( jponts(i)>0 ) THEN
               jponts(i) = jponts(i) - isil + 1
            ELSE
               logic = 130
               GOTO 1900
            ENDIF
         ENDIF
         CALL write(scr2,jponts(i),1,0)
         CALL write(scr2,igpid,1,0)
      ENDDO
      IF ( ispc+ims>=initl ) GOTO 100
   ENDIF
!
!
   IF ( Iogpst/=1 ) THEN
      Iogpst = 1
!
!     INITIALIZE OGPST
!
      CALL gopen(ogpst,Iz(Ibuf2),1)
      ogpst1(1) = 0
      ogpst1(2) = 8
      ogpst1(3) = Spcset
      ogpst1(4) = Mpcset
      ogpst1(10) = 12
      CALL write(ogpst,ogpst1,10,0)
      CALL write(ogpst,Iz,40,0)
      CALL write(ogpst,Head(1),96,1)
   ENDIF
!
!     PUT OUT ERROR RECORDS ON OGPST
!
   CALL write(ogpst,igpid,1,0)
   CALL write(ogpst,ityp,1,0)
   CALL write(ogpst,iordr,1,0)
   iordr = iordr + 1
   IF ( iordr/=1 ) THEN
      DO i = 1 , npts
         IF ( iponts(i)>0 ) THEN
            iponts(i) = iponts(i) - isil + 1
         ELSE
            logic = 140
            GOTO 1900
         ENDIF
      ENDDO
      logic = 150
      IF ( iordr==1 ) GOTO 1900
      IF ( iordr==2 ) THEN
!
!     FIRST ORDER OUTPUT
!
         iponts(4) = iponts(2)
         iponts(7) = iponts(3)
         iponts(2) = 0
         iponts(3) = 0
         iponts(5) = 0
         iponts(6) = 0
         iponts(8) = 0
         iponts(9) = 0
      ELSEIF ( iordr==3 ) THEN
!
!     SECOND ORDER OUTPUT
!
         iponts(8) = iponts(6)
         iponts(7) = iponts(5)
         iponts(5) = iponts(4)
         iponts(4) = iponts(3)
         iponts(3) = 0
         iponts(6) = 0
         iponts(9) = 0
      ELSEIF ( iordr/=4 ) THEN
         GOTO 700
      ENDIF
!
!     THIRD ORDER OUTPUT
!
      CALL write(ogpst,iponts,9,0)
      GOTO 100
   ENDIF
!
!     SCALAR
!
 700  DO i = 1 , 9
      iponts(i) = 0
   ENDDO
   CALL write(ogpst,iponts,9,0)
   GOTO 100
 800  IF ( iordr/=1 ) THEN
      IF ( iordr==2 ) GOTO 300
      logic = 170
      GOTO 1900
   ENDIF
 900  iok = 0
   DO i = 1 , npts
      IF ( iponts(i)/=0 ) THEN
         iok = iok + 1
         iponts(iok) = iponts(i)
         IF ( iok/=i ) iponts(i) = 0
         IF ( i/=npts ) THEN
            ii = i + 1
            DO j = ii , npts
               IF ( iponts(j)/=0 ) THEN
                  IF ( iponts(j)==iponts(iok) ) iponts(j) = 0
               ENDIF
            ENDDO
         ENDIF
      ENDIF
   ENDDO
   npts = iok
   IF ( npts==0 ) GOTO 300
!
   logic = 180
   IF ( npts>2 ) GOTO 1900
   logic = 190
   IF ( iponts(1)/=iponts(2) ) GOTO 300
   GOTO 1900
!
 1000 CALL close(gpst,1)
   IF ( Iogpst==1 ) THEN
      CALL close(ogpst,1)
      IF ( ierror==0 ) THEN
         CALL makmcb(ogpst1,ogpst,0,0,0)
         ogpst1(2) = 8
         CALL wrttrl(ogpst1)
      ENDIF
   ENDIF
   IF ( Iautsp==0 ) GOTO 1400
   IF ( Nauto>0 ) THEN
      logic = 220
      IF ( iscr2/=1 ) GOTO 1900
      CALL write(scr2,0,0,1)
      CALL close(scr2,1)
      IF ( ierror/=0 ) GOTO 1400
      IF ( Iogpst/=1 ) WRITE (Ioutpt,99002) Uim
!
99002 FORMAT (A29,' 2435, AT USER''S REQUEST, ALL POTENTIAL ','SINGULARITIES HAVE BEEN REMOVED BY THE',/5X,                         &
             &'APPLICATION OF SINGLE POINT CONSTRAINTS.  REFER TO PRINT','OUT OF AUTOMATICALLY GENERATED SPC1 CARDS FOR DETAILS.')
      IF ( Iogpst==1 ) WRITE (Ioutpt,99003) Uim
99003 FORMAT (A29,' 2436, AT USER''S REQUEST, ONE OR MORE POTENTIAL ','SINGULARITIES HAVE BEEN REMOVED BY THE',/5X,                 &
             &'APPLICATION OF SINGLE POINT CONSTRAINTS.  REFER TO PRINT','OUT OF AUTOMATICALLY GENERATED SPC1 CARDS FOR DETAILS.')
      IF ( Iogpst==1 .AND. index<3 ) WRITE (Ioutpt,99004) Uwm
99004 FORMAT (A25,' 2437, ONE OR MORE POTENTIAL SINGULARITIES HAVE NOT',' BEEN REMOVED',/5X,'BECAUSE OF THG PRESENCE OF SUPORT ',   &
             &'CARDS AND/OR MULTIPOINT CONSTRAINTS OR RIGID ELEMENTS.',/5X,'REFER TO THE GRID POINT SINGULARITY TABLE FOR DETAILS.')
      IF ( Iogpst==1 .AND. index==3 ) WRITE (Ioutpt,99005) Uwm
99005 FORMAT (A25,' 2437, ONE OR MORE POTENTIAL SINGULARITIES HAVE NOT',' BEEN REMOVED',/5X,'BECAUSE OF THG PRESENCE OF SUPORT ',   &
             &'CARDS AND/OR MULTIPOINT CONSTRAINTS OR RIGID ELEMENTS',/5X,'OR BECAUSE THE SINGULARITIES ARE NOT PART OF THE ',      &
             &'OMIT SET (O-SET) DEGREES OF FREEDOM.',/5X,'REFER TO THE GRID POINT SINGULARITY TABLE FOR DETAILS.')
!
!     PRINT OUT AND, IF REQUESTED, PUNCH OUT
!     AUTOMATICALLY GENERATED SPC DATA CARDS
!
      CALL gopen(scr2,Iz(Ibuf3),0)
      ifile = scr2
      CALL read(*1500,*1100,scr2,Iz(Luset+1),Ibuf3-Luset,0,iflag)
      icore = Luset + 2*Nauto - Ibuf3
      GOTO 1700
   ELSE
      logic = 210
      IF ( iscr2==1 ) GOTO 1900
      IF ( Iogpst==1 .AND. index<3 ) WRITE (Ioutpt,99006) Uwm
99006 FORMAT (A25,' 2437A, IN SPITE OF THE USER''S REQUEST, NONE OF ','THE POTENTIAL SINGULARITIES HAS BEEN REMOVED',/5X,           &
             &'BECAUSE OF THG PRESENCE OF SUPORT CARDS AND/OR MULTI','POINT CONSTRAINTS OR RIGID ELEMENTS.',/5X,                    &
             &'REFER TO THE GRID POINT SINGULARITY TABLE FOR DETAILS.')
      IF ( Iogpst==1 .AND. index==3 ) WRITE (Ioutpt,99007) Uwm
99007 FORMAT (A25,' 2437A, IN SPITE OF THE USER''S REQUEST, NONE OF ','THE POTENTIAL SINGULARITIES HAS BEEN REMOVED',/5X,           &
             &'BECAUSE OF THG PRESENCE OF SUPORT CARDS AND/OR MULTI','POINT CONSTRAINTS OR RIGID ELEMENTS',/5X,'OR BECAUSE ',       &
             &'THE SINGULARITIES ARE NOT PART OF THE OMIT SET (O-SET) ','DEGREES OF FREEDOM.',/5X,                                  &
             &'REFER TO THE GRID POINT SINGULARITY TABLE FOR DETAILS.')
      GOTO 1400
   ENDIF
 1100 logic = 230
   IF ( iflag/=2*Nauto ) GOTO 1900
   CALL sort(0,0,2,1,Iz(Luset+1),iflag)
   i = Luset + 1
   iold = -1
   ist = i
   j = 0
 1200 DO WHILE ( i<=Luset+iflag )
      IF ( iold>=0 .AND. Iz(i)/=iold ) EXIT
      iold = Iz(i)
      j = j + 2
      i = i + 2
   ENDDO
   CALL sort(0,0,2,-2,Iz(ist),j)
   IF ( i>Luset+iflag ) THEN
!
      i = Luset + 1
      iold = -1
      CALL page1
      WRITE (Ioutpt,99012)
      Line = Line + 6
   ELSE
      iold = Iz(i)
      ist = i
      j = 0
      GOTO 1200
   ENDIF
 1300 ii = 2
   DO j = 1 , 6
      IF ( i>Luset+iflag ) EXIT
      IF ( iold>=0 .AND. Iz(i)/=iold ) EXIT
      iold = Iz(i)
      iword(ii+1) = Iz(i+1)
      ii = ii + 1
      i = i + 2
   ENDDO
   iword(2) = iold
   IF ( Line>Nlpp ) THEN
      CALL page1
      WRITE (Ioutpt,99012)
      Line = Line + 6
   ENDIF
   ncard = ncard + 1
   WRITE (Ioutpt,99008) ncard , (iword(j),j=1,ii)
99008 FORMAT (15X,I5,'-',8X,'SPC1    ',8I8)
   Line = Line + 1
   IF ( Iautsp<0 ) WRITE (Ipunch,99009) (iword(j),j=1,ii)
99009 FORMAT ('SPC1    ',8I8)
   IF ( i>Luset+iflag ) THEN
      CALL close(scr2,1)
   ELSE
      iold = Iz(i)
      GOTO 1300
   ENDIF
 1400 IF ( Iautsp==0 .OR. multi==0 ) RETURN
   DO i = 1 , Luset
      Iz(i) = andf(Iz(i),mskxx)
   ENDDO
   RETURN
!
!     ERROR MESSAGES
!
 1500 num = -2
 1600 CALL mesage(num,ifile,isubnm)
 1700 num = -8
   ifile = icore
   GOTO 1600
 1800 ierror = 2
   WRITE (Ioutpt,99010) Uwm
99010 FORMAT (A25,' 2440, SINGULARITY PROCESSING SKIPPED IN MODULE GP4',' BECAUSE OF INCONSISTENT SET DEFINITION')
   GOTO 1000
 1900 WRITE (Ioutpt,99011) Sfm , logic
99011 FORMAT (A25,' 2438, LOGIC ERROR NO.',I4,' IN SUBROUTINE GP4SP IN MODULE GP4')
   CALL mesage(-61,0,0)
99012 FORMAT (//32X,'A U T O M A T I C A L L Y   ','G E N E R A T E D   ','S P C 1   C A R D S',/,16X,'CARD ',8X,/,16X,'COUNT',8X,  &
             &'---1--- +++2+++ ---3--- +++4+++ ---5--- ','+++6+++ ---7--- +++8+++ ---9--- +++10+++',/)
!
END SUBROUTINE gp4sp
