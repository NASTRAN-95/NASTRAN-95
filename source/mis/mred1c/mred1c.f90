!*==mred1c.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred1c
   USE c_bitpos
   USE c_blank
   USE c_two
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(32) :: bitpat
   INTEGER , SAVE :: eqst , nhbgss
   INTEGER , DIMENSION(7) :: eqstrl
   INTEGER :: estdta , estwrt , i , icode , ifile , imsg , itest , j , k , kompnt , korbgn , loindx , nbgss , ndof , newips ,       &
            & newsil , nwds , nwdsd , nwdsrd , sildof , silind
   INTEGER , DIMENSION(2) , SAVE :: modnam
   EXTERNAL andf , close , decode , gopen , mesage , orf , rdtrl , sfetch , smsg , sofcls , suread , write , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!     THIS SUBROUTINE CONVERTS THE EQSS DATA AND BGSS DATA TO CORRESPOND
!     TO THE BOUNDARY DEGREES OF FREEDOM (UB) FOR THE MRED1 MODULE
!
!     INPUT DATA
!     SOF - BGSS - BASIC GRID POINT IDENTIFICATION TABLE
!
!     OUTPUT DATA
!     GINO - EQST - TEMPORARY EQSS DATA FILE
!
!     PARAMETERS
!     INPUT - GBUF1  - GINO BUFFER
!             KORLEN - LENGTH OF OPEN CORE
!             NEWNAM - NAME OF NEW SUBSTRUCTURE
!             RGRID  - FREEBODY MODE IDENTIFICATION NUMBERS (SET IN
!                      MRED1)
!                      RGRID(1) .EQ. GRID POINT IDENTIFICATION NUMBER
!                      RGRID(2) .EQ. NUMBER OF CONTRIBUTING SUBSTRUCTURE
!             NCSUBS - NUMBER OF CONTRIBUTING SUBSTRUCTURES
!             NAMEBS - BEGINNING ADDRESS OF BASIC SUBSTRUCTURE NAMES
!             EQSIND - BEGINNING ADDRESS OF EQSS GROUP ADDRESSES
!             NSLBGN - BEGINNING ADDRESS OF SIL DATA
!             NSIL   - NUMBER OF SIL GROUPS
!             LOCUST - BEGINNING ADDRESS OF USET ARRAY
!
   DATA eqst , nhbgss , modnam/203 , 4HBGSS , 4HMRED , 4H1C  /
!
!     IF OLDBOUNDS OPTION, GET EQST TRAILER
!
   IF ( dry==-2 ) RETURN
   eqstrl(1) = eqst
   IF ( bounds ) CALL rdtrl(eqstrl)
!
!     GET SIL DOF AND DECODE
!
   newips = 0
   DO i = 1 , nsil
      sildof = nslbgn + ((2*i)-1)
      icode = z(sildof)
      CALL decode(icode,bitpat,nwdsd)
!
!     TEST FOR DOF REMAINING IN BOUNDARY SET
!
      ndof = 0
      kompnt = 0
      DO j = 1 , nwdsd
         k = locust + (z(sildof-1)-1) + (j-1)
         IF ( andf(z(k),itwo(ub))/=0 ) THEN
            k = 32 - bitpat(j)
            kompnt = orf(kompnt,itwo(k))
            ndof = ndof + 1
         ENDIF
      ENDDO
!
!     SAVE NEW SIL DATA
!
      IF ( ndof==0 ) THEN
!
!     SIL DATA NOT NEEDED
!
         z(sildof-1) = -1
      ELSE
         newips = newips + 1
         z(sildof-1) = (8*newips) + ndof
         z(sildof) = kompnt
      ENDIF
   ENDDO
!
!     WRITE EQSS GROUP 0 DATA ONTO TEMPORARY EQST TABLE
!
   CALL gopen(eqst,z(gbuf1),1)
   CALL write(eqst,newnam,2,0)
   CALL write(eqst,ncsubs,1,0)
   CALL write(eqst,newips,1,0)
   nwds = eqsind - namebs
   CALL write(eqst,z(namebs),nwds,1)
   eqstrl(2) = nwds + 4
!
!     WRITE REMAINING EQSS GROUP DATA ONTO TEMPORARY EQST TABLE
!
   eqstrl(3) = ncsubs
   DO i = 1 , ncsubs
      j = 2*(i-1)
      estdta = z(eqsind+j)
      nwds = z(eqsind+j+1)
!
!     TEST SUBSTRUCTURE COMPONENTS
!
      IF ( nwds>0 ) THEN
         DO j = 1 , nwds , 3
            silind = nslbgn + (2*(z(estdta+j)-1))
            IF ( rgrid(1)>0 ) THEN
               IF ( i==rgrid(2) ) THEN
                  IF ( rgrid(1)==z(estdta+j-1) ) rgrid(1) = z(estdta+j)
               ENDIF
            ENDIF
            IF ( z(silind)/=-1 ) THEN
!
!     REPLACE IP, SIL NUMBERS AND WRITE DATA
!
               estwrt = estdta + j
               z(estwrt) = z(silind)/8
               z(estwrt+1) = z(silind+1)
               CALL write(eqst,z(estwrt-1),3,0)
            ENDIF
         ENDDO
      ENDIF
      CALL write(eqst,0,0,1)
   ENDDO
!
!     REDUCE SIL ENTRIES AND STORE NEW SIL DATA AT Z(2*NSIL)
!
   ndof = 1
   loindx = 0
   newsil = nslbgn + (2*nsil)
   IF ( (newsil+(2*nsil))>=korlen ) THEN
!
!     PROCESS SYSTEM FATAL ERRORS
!
      imsg = -8
      ifile = 0
      CALL sofcls
      CALL mesage(imsg,ifile,modnam)
      RETURN
   ELSE
      DO i = 1 , nsil
         j = 2*(i-1)
         IF ( z(nslbgn+j)/=-1 ) THEN
            z(newsil+loindx) = ndof
            z(newsil+loindx+1) = z(nslbgn+j+1)
            ndof = ndof + andf(z(nslbgn+j),7)
            loindx = loindx + 2
         ENDIF
      ENDDO
!
!     WRITE SIL DATA ONTO TEMPORARY EQST TABLE
!
      korbgn = namebs
      IF ( loindx<=0 ) CALL write(eqst,0,0,1)
      IF ( loindx>0 ) CALL write(eqst,z(newsil),loindx,1)
      eqstrl(4) = loindx
!
!     READ AND WRITE BGSS GROUP 0 DATA
!
      CALL sfetch(oldnam,nhbgss,1,itest)
      IF ( itest==3 ) THEN
!
!     PROCESS MODULE FATAL ERRORS
!
         imsg = -1
      ELSEIF ( itest==4 ) THEN
         imsg = -2
      ELSEIF ( itest==5 ) THEN
         imsg = -3
      ELSE
         CALL suread(z(korbgn),-1,nwdsrd,itest)
         z(korbgn) = oldnam(1)
         z(korbgn+1) = oldnam(2)
         nbgss = z(korbgn+2)
         z(korbgn+2) = loindx/2
         CALL write(eqst,z(korbgn),3,1)
!
!     ELIMINATE BGSS DATA NOT REQUIRED
!
         i = 0
         eqstrl(5) = 0
         DO j = 1 , nbgss
            CALL suread(z(korbgn),4,nwdsrd,itest)
            IF ( i<=(2*nsil) ) THEN
               IF ( z(nslbgn+i)/=-1 ) THEN
                  CALL write(eqst,z(korbgn),4,0)
                  eqstrl(5) = eqstrl(5) + 4
               ENDIF
            ENDIF
            i = i + 2
         ENDDO
         CALL write(eqst,0,0,1)
         CALL wrttrl(eqstrl)
!
!     CLOSE EQST FILE
!
         CALL close(eqst,1)
         RETURN
      ENDIF
   ENDIF
   CALL smsg(imsg,nhbgss,oldnam)
!
END SUBROUTINE mred1c
