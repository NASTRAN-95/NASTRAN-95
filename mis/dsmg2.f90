
SUBROUTINE dsmg2
   IMPLICIT NONE
   INTEGER Dscset , Ireptd , Ndskip , Sysbuf
   REAL Z(1)
   COMMON /blank / Ndskip , Ireptd , Dscset
   COMMON /system/ Sysbuf
   COMMON /zzzzzz/ Z
   REAL betasp , block(11)
   INTEGER buffr1 , buffr2 , clsrw , dsnos(2) , file1 , file2 , file3 , i , ibeta , iblock(11) , idummy , iflag , ind , iundef ,    &
         & izmax , j , kaa , kbaa , kbfs , kbss , kdaa , kdfs , kdss , kfs , kss , left , mcb(7) , mpt , name(2) , neor , pbl ,     &
         & pbs , pl , ps , setno , uboov , uoov , ybs , ys
   INTEGER korsz
!*****
! THIS MODULE PERFORMS THE FOLLOWING MATRIX OPERATIONS...
!
!     KBAA  = KAA + BETA * KDAA
!     KBFS  = KFS + BETA * KDFS
!     KBSS  = KSS + BETA * KDSS
!     PBL   = BETA * PL
!     PBS   = BETA * PS
!     YBS   = BETA * YS
!     UBOOV = BETA * UOOV
!
! THE VALUE OF BETA LIES IN THE DIFFERENTIAL STIFFNESS COEFFICIENT SET
! NO. SPECIFIED BY THE INPUT PARAMETER DSCSET.  THE PARTICULAR VALUE OF
! BETA TO BE USED ON ANY PASS THROUGH THIS MODULE IS THE NDSKIP(TH)
! VALUE IN THE DSCSET SET.  IREPTD IS SET EQUAL TO -1 AFTER THE LAST
! BETA IN THE SET HAS BEEN ENCOUNTERED, THEREBY TERMINATING THE
! DIFFERENTIAL STIFFNESS RIGID FORMAT DMAP LOOP BEGINNING AT THE
! LABEL DSLOOP AND ENDING AT THE REPT DSLOOP,100$ STATEMENT.
!
!
!  DMAP CALL...
!
!
!     DSMG2    MPT,KAA,KDAA,KFS,KDFS,KSS,KDSS,PL,PS,YS,UOOV/KBAA,KBFS,
!              KBSS,PBL,PBS,YBS,UBOOV/V,N,NDSKIP/V,N,REPEATD/
!              V,N,DSCOSET/ $
!*****
!
!
!
!
!
!
   !>>>>EQUIVALENCE (block(1),iblock(1)) , (betasp,ibeta)
!
! MODULE PARAMETERS
!
!
! SYSTEM COMMON
!
!
!
!
!
!
!
   DATA name(1)/4HDSMG/ , name(2)/4H2   /
   DATA mpt , kaa , kdaa , kfs , kdfs , kss , kdss , pl , ps , ys , uoov , kbaa , kbfs , kbss , pbl , pbs , ybs , uboov/101 , 102 , &
      & 103 , 104 , 105 , 106 , 107 , 108 , 109 , 110 , 111 , 201 , 202 , 203 , 204 , 205 , 206 , 207/
   DATA clsrw/1/
   DATA neor , dsnos(1) , dsnos(2)/0 , 53 , 10/
!
!
!
   izmax = korsz(Z)
   buffr1 = izmax - Sysbuf
   buffr2 = buffr1 - Sysbuf
   left = buffr2 - 1
!
! TURN DIFFERENTIAL STIFFNESS LOOPING FLAG ON AND INCREMENT THE INDEX
! OF BETA.  NOTE THAT NDSKIP MUST BE SET TO ZERO IN THE MODULE
! PROPERTIES TABLE.
!
   Ireptd = 1
   Ndskip = Ndskip + 1
!
! CALL LOCATE TO FIND THE RECORD OF THE MPT WHERE THE DSFACT CARDS ARE.
! THIS IS DONE ONLY IF A D.S. COEFFICIENT SET NO. IS GIVEN.
!
   IF ( Dscset/=(-1) ) THEN
      CALL preloc(*300,Z(buffr1),mpt)
      CALL locate(*400,Z(buffr1),dsnos,idummy)
      DO
!
!
!
         CALL read(*500,*600,mpt,setno,1,neor,idummy)
         IF ( setno/=Dscset ) THEN
!
! READ ONE WORD AT A TIME UNTIL A -1 (END OF SET INDICATOR) IS READ.
!
            DO i = 1 , 32000
               CALL read(*700,*800,mpt,j,1,neor,idummy)
               IF ( j==(-1) ) GOTO 50
            ENDDO
            CALL mesage(-30,84,1)
         ENDIF
!
! TEST TO SEE IF WORDS MUST BE SKIPPED.
!
!
! SKIP NDSKIP - 1 WORDS
!
         IF ( Ndskip/=1 ) CALL read(*900,*1000,mpt,0,-(Ndskip-1),neor,idummy)
!
! READ THE VALUE OF BETA
!
         CALL read(*1100,*1200,mpt,betasp,1,neor,idummy)
         IF ( ibeta==(-1) ) CALL mesage(-30,84,2)
!
! IF THE NEXT WORD IS A -1, WE HAVE READ THE LAST BETA.  HENCE SET
! IREPTD = -1
!
         CALL read(*1300,*1400,mpt,j,1,neor,iflag)
         IF ( j==(-1) ) Ireptd = -1
         CALL close(mpt,clsrw)
!
! PERFORM THE 4 SCALAR MULTIPLICATIONS.  N.B.---IF DSCSET = -1, THAT IS,
! ONLY ONE BETA WILL BE USED AND THAT HAS AN ASSUMED VALUE OF 1.0, IT IS
! ASSUMED THAT EQUIVALENCES HAVE BEEN MADE BETWEEN PL AND PBL, PS AND
! PBS, YS AND YBS, AND UOOV AND UBOOV.
!
         ind = 0
         file1 = pl
         file2 = pbl
         EXIT
 50   ENDDO
   ELSE
!
! THERE IS NO LOOPING.  TURN LOOPING INDICATOR OFF.
! SEE COMMENTS ABOVE FORTRAN STATEMENT NO. 70 RE THE 4 SCALAR MULTIPLI-
! CATIONS WHEN DSCSET = -1.
!
      Ireptd = -1
!
! PERFORM MATRIX ADDITIONS
!
      betasp = 1.0
      GOTO 100
   ENDIF
   DO
      mcb(1) = file1
      CALL rdtrl(mcb)
!
! A FATAL ERROR OCCURS IF PL IS PURGED.
!
      IF ( mcb(1)<0 .AND. ind==0 ) CALL mesage(-1,file1,name)
!
! IF THE INPUT FILE IS NOT PURGED AND IS NOT PL, SKIP THE OPERATION.
!
      IF ( mcb(1)>=0 ) THEN
!
! THE INPUT FILE IS NOT PURGED. IF THE OUTPUT FILE IS PURGED, A FATAL
! ERROR OCCURS.
!
         mcb(1) = file2
         CALL rdtrl(mcb)
         IF ( mcb(1)<0 ) CALL mesage(-1,file2,name)
         iblock(1) = 1
         block(2) = betasp
         block(3) = 0.0
         block(4) = 0.0
         block(5) = 0.0
         block(6) = 0.0
         iblock(7) = 1
         block(8) = 0.0
         block(9) = 0.0
         block(10) = 0.0
         block(11) = 0.0
         CALL ssg2c(file1,0,file2,0,block(1))
      ENDIF
      ind = ind + 1
      IF ( ind==2 ) THEN
         file1 = ys
         file2 = ybs
      ELSEIF ( ind==3 ) THEN
         file1 = uoov
         file2 = uboov
      ELSEIF ( ind==4 ) THEN
         EXIT
      ELSE
         file1 = ps
         file2 = pbs
      ENDIF
   ENDDO
 100  file1 = kaa
   file2 = kdaa
   file3 = kbaa
   ind = 0
 200  iundef = 0
   mcb(1) = file1
   CALL rdtrl(mcb(1))
   IF ( mcb(1)<0 ) THEN
      IF ( ind<=0 ) CALL mesage(-1,file1,name)
      iundef = 1
   ENDIF
   mcb(1) = file2
   CALL rdtrl(mcb(1))
   IF ( mcb(1)>=0 ) THEN
      IF ( iundef==1 ) CALL mesage(-30,84,15)
      iblock(1) = 1
      block(2) = 1.0
      block(3) = 0.0
      block(4) = 0.0
      block(5) = 0.0
      block(6) = 0.0
      iblock(7) = 1
      block(8) = betasp
      block(9) = 0.0
      block(10) = 0.0
      block(11) = 0.0
      CALL ssg2c(file1,file2,file3,0,block(1))
   ELSEIF ( iundef<=0 ) THEN
      CALL mesage(-1,file2,name)
      GOTO 300
   ENDIF
   ind = ind + 1
   IF ( ind==2 ) THEN
      file1 = kss
      file2 = kdss
      file3 = kbss
      GOTO 200
   ELSEIF ( ind==3 ) THEN
      RETURN
   ELSE
      file1 = kfs
      file2 = kdfs
      file3 = kbfs
      GOTO 200
   ENDIF
 300  i = 3
   GOTO 1500
 400  i = 4
   GOTO 1500
 500  i = 5
   GOTO 1500
 600  i = 6
   GOTO 1500
 700  i = 7
   GOTO 1500
 800  i = 8
   GOTO 1500
 900  i = 9
   GOTO 1500
 1000 i = 10
   GOTO 1500
 1100 i = 11
   GOTO 1500
 1200 i = 12
   GOTO 1500
 1300 i = 13
   GOTO 1500
 1400 i = 14
 1500 CALL mesage(-30,84,i)
END SUBROUTINE dsmg2