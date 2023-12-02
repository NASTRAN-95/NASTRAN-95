!*==dsmg2.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE dsmg2
   IMPLICIT NONE
   USE C_BLANK
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   REAL :: betasp
   REAL , DIMENSION(11) :: block
   INTEGER :: buffr1 , buffr2 , file1 , file2 , file3 , i , ibeta , idummy , iflag , ind , iundef , izmax , j , left , setno
   INTEGER , SAVE :: clsrw , kaa , kbaa , kbfs , kbss , kdaa , kdfs , kdss , kfs , kss , mpt , neor , pbl , pbs , pl , ps , uboov , &
                   & uoov , ybs , ys
   INTEGER , DIMENSION(2) , SAVE :: dsnos , name
   INTEGER , DIMENSION(11) :: iblock
   INTEGER , DIMENSION(7) :: mcb
!
! End of declarations rewritten by SPAG
!
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
      CALL preloc(*200,Z(buffr1),mpt)
      CALL locate(*300,Z(buffr1),dsnos,idummy)
      DO
!
!
!
         CALL read(*400,*500,mpt,setno,1,neor,idummy)
         IF ( setno/=Dscset ) THEN
!
! READ ONE WORD AT A TIME UNTIL A -1 (END OF SET INDICATOR) IS READ.
!
            DO i = 1 , 32000
               CALL read(*600,*700,mpt,j,1,neor,idummy)
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
         IF ( Ndskip/=1 ) CALL read(*800,*900,mpt,0,-(Ndskip-1),neor,idummy)
!
! READ THE VALUE OF BETA
!
         CALL read(*1000,*1100,mpt,betasp,1,neor,idummy)
         IF ( ibeta==(-1) ) CALL mesage(-30,84,2)
!
! IF THE NEXT WORD IS A -1, WE HAVE READ THE LAST BETA.  HENCE SET
! IREPTD = -1
!
         CALL read(*1200,*1300,mpt,j,1,neor,iflag)
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
   DO
      iundef = 0
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
         EXIT
      ENDIF
      ind = ind + 1
      IF ( ind==2 ) THEN
         file1 = kss
         file2 = kdss
         file3 = kbss
      ELSEIF ( ind==3 ) THEN
         RETURN
      ELSE
         file1 = kfs
         file2 = kdfs
         file3 = kbfs
      ENDIF
   ENDDO
 200  i = 3
   GOTO 1400
 300  i = 4
   GOTO 1400
 400  i = 5
   GOTO 1400
 500  i = 6
   GOTO 1400
 600  i = 7
   GOTO 1400
 700  i = 8
   GOTO 1400
 800  i = 9
   GOTO 1400
 900  i = 10
   GOTO 1400
 1000 i = 11
   GOTO 1400
 1100 i = 12
   GOTO 1400
 1200 i = 13
   GOTO 1400
 1300 i = 14
 1400 CALL mesage(-30,84,i)
END SUBROUTINE dsmg2
