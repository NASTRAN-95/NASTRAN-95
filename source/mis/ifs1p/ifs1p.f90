!*==ifs1p.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifs1p() !HIDESTARS (*,*,*)
   USE c_blank
   USE c_cifs1p
   USE c_ifpdta
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: abort
   INTEGER , SAVE :: bcdc , bcdr , bcds , blk , it1 , it2 , it3 , thru
   INTEGER , DIMENSION(7) , SAVE :: hbdyix
   INTEGER , DIMENSION(2,7) , SAVE :: hbdynm
   INTEGER :: icell , iz , k914 , kl , kz , l , l1 , l2 , l3 , l4 , l50 , nmo , nout
   INTEGER , DIMENSION(5) :: klotdf
   INTEGER , DIMENSION(100) :: m
   EXTERNAL ifpdco , page2 , write
!
! End of declarations rewritten by SPAG
!
!
   !>>>>EQUIVALENCE (Ksystm(2),Nout) , (Ksystm(3),Abort) , (M(1),Rm(1)) , (Slotdf(1),Klotdf(1))
   DATA hbdynm/4HPOIN , 4HT    , 4HLINE , 4H     , 4HREV  , 4H     , 4HAREA , 4H3    , 4HAREA , 4H4    , 4HELCY , 4HL    , 4HFTUB , &
       &4HE   /
   DATA hbdyix/1 , 2 , 2 , 3 , 4 , 2 , 2/
   DATA thru/4HTHRU/
   DATA blk , bcdc , bcdr , bcds/1H  , 1HC , 1HR , 1HS/
   DATA it1 , it2 , it3/2HT1 , 2HT2 , 2HT3/
!
   IF ( k<=100 ) THEN
      IF ( k==1 .OR. k==2 .OR. k==3 .OR. k==12 .OR. k==13 .OR. k==17 .OR. k==28 .OR. k==32 .OR. k==51 .OR. k==79 .OR. k==80 .OR.    &
         & k==82 .OR. k==83 .OR. k==84 .OR. k==85 .OR. k==86 .OR. k==87 .OR. k==88 .OR. k==89 .OR. k==90 .OR. k==91 .OR. k==92 .OR. &
         & k==93 .OR. k==94 .OR. k==95 .OR. k==96 .OR. k==97 .OR. k==98 .OR. k==99 .OR. k==100 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
      IF ( k==4 ) THEN
         CALL spag_block_7
         RETURN
      ENDIF
      IF ( k==5 ) THEN
!
!*****         5-CORD1R        *****************************************
!
         l50 = 1
         CALL spag_block_9
         RETURN
      ELSEIF ( k==6 ) THEN
!
!*****         6-CORD1C        *****************************************
!
         l50 = 2
         CALL spag_block_9
         RETURN
      ELSEIF ( k==7 ) THEN
!
!*****         7-CORD1S        *****************************************
!
         l50 = 3
         CALL spag_block_9
         RETURN
      ELSEIF ( k==8 ) THEN
!
!*****         8-CORD2R        *****************************************
!
         i(2) = 1
         CALL spag_block_10
         RETURN
      ELSEIF ( k==9 ) THEN
!
!*****         9-CORD2C        *****************************************
!
         i(2) = 2
         CALL spag_block_10
         RETURN
      ELSEIF ( k==10 ) THEN
!
!*****         10-CORD2S       *****************************************
!
         i(2) = 3
         CALL spag_block_10
         RETURN
      ELSEIF ( k==11 ) THEN
         CALL spag_block_11
         RETURN
      ELSEIF ( k==14 .OR. k==15 ) THEN
!
!*****         14-SUPORT,15-OMIT,215-ASET         **********************
!
         l = 1
         CALL spag_block_17
         RETURN
      ELSEIF ( k==16 ) THEN
         CALL spag_block_20
         RETURN
      ELSEIF ( k==18 .OR. k==19 ) THEN
!
!*******       18-FORCE,19-MOMENT   **************************
!
         IF ( m(2)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         CALL spag_block_21
         RETURN
      ELSEIF ( k==20 .OR. k==21 ) THEN
!
!*****         20-FORCE1,21-MOMENT1   **********************************
!
         IF ( m(1)<=0 .OR. m(2)<=0 .OR. m(4)<=0 .OR. m(5)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(4)==m(5) ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         n = 5
         CALL spag_block_4
         RETURN
      ELSEIF ( k==22 .OR. k==23 ) THEN
!
!*****         22-FORCE2,23-MOMENT2   **********************************
!
         IF ( m(1)<=0 .OR. m(2)<=0 .OR. m(4)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(5)<=0 .OR. m(6)<=0 .OR. m(7)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(4)==m(5) .OR. m(6)==m(7) .OR. m(4)==m(6) .AND. m(5)==m(7) .OR. m(4)==m(7) .AND. m(5)==m(6) ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         n = 7
         CALL spag_block_4
         RETURN
      ELSEIF ( k==24 ) THEN
!
!*****         24-PLOAD        *****************************************
!
         IF ( m(1)<=0 .OR. m(3)<=0 .OR. m(4)<=0 .OR. m(5)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(6)<0 .OR. m(6)==0 .AND. mf(6)/=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         DO l = 4 , 6
            DO l1 = l , 6
               IF ( m(l-1)==m(l1) ) THEN
                  CALL spag_block_3
                  RETURN
               ENDIF
            ENDDO
         ENDDO
         n = 6
         CALL spag_block_4
         RETURN
      ELSEIF ( k==25 .OR. k==27 .OR. k==81 ) THEN
!
!*****         25-SLOAD,27-TEMP,81-DEFORM    ***************************
!
         IF ( m(1)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         DO l = 2 , 6 , 2
            IF ( m(l)/=0 .OR. m(l+1)/=0 ) THEN
               IF ( m(l)<=0 ) THEN
                  CALL spag_block_3
                  RETURN
               ENDIF
               n = n + 3
               i(n-2) = m(1)
               i(n-1) = m(l)
               i(n) = m(l+1)
               IF ( n>3 ) THEN
                  DO l1 = 6 , n , 3
                     IF ( i(n-1)==i(l1-4) ) THEN
                        CALL spag_block_3
                        RETURN
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF
         ENDDO
         IF ( n<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         CALL spag_block_5
         RETURN
      ELSEIF ( k==26 ) THEN
!
!*****         26-GRAV         *****************************************
!
         IF ( m(1)<=0 .OR. m(2)<0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(4)/=0 .OR. m(5)/=0 .OR. m(6)/=0 ) THEN
            n = 6
            CALL spag_block_4
            RETURN
         ELSE
            IF ( m(3)/=0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            rm(4) = 1.0
            n = 6
            CALL spag_block_4
            RETURN
         ENDIF
      ELSEIF ( k==29 ) THEN
!
!*****         29-PROD         *****************************************
!
         n = 6
         CALL spag_block_22
         RETURN
      ELSEIF ( k==30 ) THEN
!
!*****         30-PTUBE        *****************************************
!
         n = 5
         IF ( rm(3)<=0.0 .OR. rm(4)<0.0 .OR. rm(4)>0.5*rm(3) ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( rm(4)==0.0 ) rm(4) = 0.5*rm(3)
         CALL spag_block_22
         RETURN
      ELSEIF ( k==31 ) THEN
         kl = 33
!
!*****         31-PVISC        *****************************************
!
         DO l = 1 , 5 , 4
            IF ( m(l)/=0 .OR. m(l+1)/=0 .OR. m(l+2)/=0 ) THEN
               IF ( m(l)<=0 ) THEN
                  CALL spag_block_3
                  RETURN
               ENDIF
               n = n + 3
               i(n-2) = m(l)
               i(n-1) = m(l+1)
               i(n) = m(l+2)
               IF ( e(kl)>=0 ) THEN
                  IF ( m(l)>e(kl) ) THEN
                     e(kl) = m(l)
                  ELSE
                     e(kl) = -m(l)
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
         IF ( n<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         CALL spag_block_5
         RETURN
      ELSEIF ( k==33 .OR. k==38 ) THEN
!
!*****         33-PTRIA1,38-PQUAD1    **********************************
!
         IF ( m(2)<0 .OR. m(4)<0 .OR. m(6)<0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(2)==0 .AND. m(4)==0 .AND. m(6)==0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         DO l = 2 , 6 , 2
            IF ( m(l)==0 .AND. m(l+1)/=0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
         ENDDO
         n = 10
         CALL spag_block_23
         RETURN
      ELSEIF ( k==34 ) THEN
         kl = 30
         CALL spag_block_24
         RETURN
      ELSEIF ( k==35 .OR. k==36 .OR. k==40 ) THEN
!
!*****         35-PTRBSC,36-PTRPLT,40-PQDPLT     ***********************
!
         IF ( m(2)<0 .OR. m(4)<0 .OR. m(2)==0 .AND. m(4)==0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         DO l = 2 , 4 , 2
            IF ( m(l)==0 .AND. m(l+1)/=0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
         ENDDO
         n = 8
         CALL spag_block_23
         RETURN
      ELSEIF ( k==37 ) THEN
         kl = 31
         CALL spag_block_24
         RETURN
      ELSEIF ( k==39 ) THEN
         kl = 27
         CALL spag_block_24
         RETURN
      ELSEIF ( k==41 ) THEN
         kl = 24
         CALL spag_block_24
         RETURN
      ELSEIF ( k==42 ) THEN
         kl = 28
         CALL spag_block_24
         RETURN
      ELSEIF ( k==43 ) THEN
         kl = 32
         CALL spag_block_24
         RETURN
      ELSEIF ( k==44 ) THEN
         kl = 23
         CALL spag_block_25
         RETURN
      ELSEIF ( k==45 ) THEN
         kl = 21
         CALL spag_block_25
         RETURN
      ELSEIF ( k==46 ) THEN
         kl = 22
!
!*****         46-PELAS             ************************************
!
         DO l = 1 , 5 , 4
            IF ( m(l)/=0 .OR. m(l+1)/=0 .OR. m(l+2)/=0 ) THEN
               IF ( m(l)<=0 ) THEN
                  CALL spag_block_3
                  RETURN
               ENDIF
               n = n + 4
               i(n-3) = m(l)
               i(n-2) = m(l+1)
               i(n-1) = m(l+2)
               i(n) = m(l+3)
               IF ( e(kl)>=0 ) THEN
                  IF ( m(l)>e(kl) ) THEN
                     e(kl) = m(l)
                  ELSE
                     e(kl) = -m(l)
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
         IF ( n<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         CALL spag_block_5
         RETURN
      ELSEIF ( k==47 ) THEN
!
!*****         47-CONROD       *****************************************
!
         IF ( m(1)<=0 .OR. m(2)<=0 .OR. m(3)<=0 .OR. m(4)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(2)==m(3) ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         n = 8
         CALL spag_block_4
         RETURN
      ELSEIF ( k==48 ) THEN
         kl = 1
         CALL spag_block_26
         RETURN
      ELSEIF ( k==49 ) THEN
         kl = 2
         CALL spag_block_26
         RETURN
      ELSEIF ( k==50 ) THEN
         kl = 3
         CALL spag_block_26
         RETURN
      ELSEIF ( k==52 .OR. k==53 .OR. k==54 .OR. k==55 .OR. k==56 ) THEN
         CALL spag_block_27
         RETURN
      ELSEIF ( k==57 .OR. k==58 .OR. k==59 .OR. k==60 ) THEN
         CALL spag_block_28
         RETURN
      ELSEIF ( k==61 .OR. k==62 ) THEN
!
!*****         61-CSHEAR,62-CTWIST    **********************************
!
         IF ( m(3)<=0 .OR. m(4)<=0 .OR. m(5)<=0 .OR. m(6)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(3)==m(4) .OR. m(4)==m(5) .OR. m(5)==m(6) .OR. m(3)==m(5) .OR. m(4)==m(6) .OR. m(3)==m(6) ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         n = 6
         IF ( mf(2)==0 ) m(2) = m(1)
         CALL spag_block_22
         RETURN
      ELSEIF ( k==63 ) THEN
!
!*****         63-CONM1        *****************************************
!
         IF ( m(1)<0 .OR. m(2)<=0 .OR. m(3)<0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         n = 24
         CALL spag_block_4
         RETURN
      ELSEIF ( k==64 ) THEN
!
!*****         64-CONM2        *****************************************
!
         IF ( m(1)<0 .OR. m(2)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         DO l = 1 , 7
            i(l) = m(l)
         ENDDO
         DO l = 8 , 13
            i(l) = m(l+1)
         ENDDO
         n = 13
         CALL spag_block_5
         RETURN
      ELSEIF ( k==65 .OR. k==69 .OR. k==73 ) THEN
!
!*****         65-CMASS1,69-CDAMP1,73-CELAS1,70-CDAMP2,66-CMASS2    ****
!
         IF ( mf(2)==0 ) m(2) = m(1)
         IF ( m(2)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         n = 6
         CALL spag_block_29
         RETURN
      ELSEIF ( k==66 .OR. k==70 ) THEN
         n = 6
         CALL spag_block_29
         RETURN
      ELSEIF ( k==67 ) THEN
         kl = 4
         CALL spag_block_30
         RETURN
      ELSEIF ( k==68 ) THEN
         kl = 7
         CALL spag_block_31
         RETURN
      ELSEIF ( k==71 ) THEN
         kl = 5
         CALL spag_block_30
         RETURN
      ELSEIF ( k==72 ) THEN
         kl = 8
         CALL spag_block_31
         RETURN
      ELSEIF ( k==74 ) THEN
!
!*****         74-CELAS2       *****************************************
!
         n = 8
         CALL spag_block_29
         RETURN
      ELSEIF ( k==75 ) THEN
         kl = 6
         CALL spag_block_30
         RETURN
      ELSEIF ( k==76 ) THEN
         kl = 9
         CALL spag_block_31
         RETURN
      ELSEIF ( k==77 ) THEN
!
!*****         77-MAT1         *****************************************
!
         IF ( m(1)<=0 .OR. (rm(2)==0 .AND. rm(3)==0) ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( (rm(2)<0. .OR. rm(3)<0.) .AND. ksystm(78)>=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         n = 12
         IF ( m(12)<0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         l = 3
         IF ( mf(2)==0 .OR. rm(2)==0. ) l = l - 1
         IF ( mf(3)==0 .OR. rm(3)==0. ) l = l - 1
         IF ( mf(4)==0 .OR. rm(4)==0. ) l = l - 1
         IF ( l<2 ) THEN
            CALL page2(3)
            WRITE (nout,99001) uwm , m(1)
99001       FORMAT (A25,' 2251, TWO OF THE E, G AND NU ON MAT1 CARD ',I8,' ARE ZEROS OR BLANKS.',/5X,                               &
                   &'POTENTIAL ERROR MAY OCCUR LATER')
         ENDIF
         IF ( mf(2)/=2 .OR. mf(3)/=2 .OR. mf(4)/=2 ) THEN
            IF ( mf(2)==0 ) rm(2) = 2.0*rm(3)*(1.0+rm(4))
            IF ( mf(3)==0 ) rm(3) = rm(2)/(2.0*(1.0+rm(4)))
            IF ( mf(4)==0 ) rm(4) = rm(2)/(2.0*rm(3)) - 1.0
            IF ( rm(4)<-1.0 .OR. rm(4)>0.5 ) THEN
               CALL page2(2)
               WRITE (nout,99002) uwm , m(1) , rm(4)
99002          FORMAT (A25,' 2251, PHYSICALLY UNREALISTIC VALUE FOR NU ON MAT1 ','CARD ',I8,'.  VALUE = ',1P,E16.4)
            ENDIF
         ENDIF
         CALL spag_block_4
         RETURN
      ELSEIF ( k==78 ) THEN
!
!*****         78-MAT2         *****************************************
!
         n = 17
         IF ( m(17)<0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(1)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         CALL spag_block_4
         RETURN
      ENDIF
   ENDIF
   IF ( kx<=100 ) THEN
      IF ( kx==1 .OR. kx==2 .OR. kx==3 .OR. kx==4 .OR. kx==5 .OR. kx==6 .OR. kx==7 .OR. kx==8 .OR. kx==9 .OR. kx==10 .OR.           &
         & kx==11 .OR. kx==12 .OR. kx==13 .OR. kx==14 .OR. kx==15 .OR. kx==16 .OR. kx==17 .OR. kx==18 .OR. kx==19 .OR. kx==20 .OR.  &
         & kx==22 .OR. kx==23 .OR. kx==24 .OR. kx==26 .OR. kx==31 .OR. kx==32 .OR. kx==33 .OR. kx==34 .OR. kx==38 .OR. kx==39 .OR.  &
         & kx==40 .OR. kx==41 .OR. kx==43 .OR. kx==44 .OR. kx==45 .OR. kx==46 .OR. kx==47 .OR. kx==48 .OR. kx==49 .OR. kx==50 .OR.  &
         & kx==51 .OR. kx==52 .OR. kx==53 .OR. kx==54 .OR. kx==55 .OR. kx==56 .OR. kx==57 .OR. kx==59 .OR. kx==60 .OR. kx==61 .OR.  &
         & kx==62 .OR. kx==63 .OR. kx==64 .OR. kx==65 .OR. kx==67 .OR. kx==68 .OR. kx==69 .OR. kx==70 .OR. kx==71 .OR. kx==72 .OR.  &
         & kx==73 .OR. kx==74 .OR. kx==75 .OR. kx==76 .OR. kx==77 .OR. kx==78 .OR. kx==82 .OR. kx==83 .OR. kx==84 .OR. kx==85 .OR.  &
         & kx==86 .OR. kx==87 .OR. kx==88 .OR. kx==89 .OR. kx==91 .OR. kx==92 .OR. kx==93 .OR. kx==94 .OR. kx==95 .OR. kx==96 .OR.  &
         & kx==97 .OR. kx==98 .OR. kx==99 .OR. kx==100 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
      IF ( kx==21 ) THEN
         kl = 29
         CALL spag_block_24
         RETURN
      ELSEIF ( kx==25 ) THEN
         CALL spag_block_12
         RETURN
      ELSEIF ( kx==27 ) THEN
         CALL spag_block_13
         RETURN
      ELSEIF ( kx==28 ) THEN
!
!*****         128-NOLIN2        ***************************************
!
         IF ( m(8)<0 .OR. mf(8)/=1 .AND. mf(8)/=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( (m(8)>6 .AND. m(8)<10) .OR. m(8)>16 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         CALL spag_block_15
         RETURN
      ELSEIF ( kx==29 .OR. kx==30 ) THEN
!
!*****         129-NOLIN3,130-NOLIN4        ****************************
!
         IF ( mf(8)/=0 .OR. mf(7)/=2 .AND. mf(7)/=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         CALL spag_block_14
         RETURN
      ELSEIF ( kx==35 ) THEN
         CALL spag_block_7
         RETURN
      ELSEIF ( kx==36 ) THEN
!
!*****         136-TF         ******************************************
!
         IF ( km==0 ) THEN
            nmo = 5
            id = m(1)
            IF ( id<=0 .OR. m(2)<=0 .OR. m(3)<0 ) THEN
               baddat = .TRUE.
               CALL spag_block_19
               RETURN
            ELSE
               IF ( mf(1)/=1 .OR. mf(2)/=1 .OR. mf(3)>1 ) badfor = .TRUE.
               IF ( (mf(4)/=2 .AND. mf(4)/=0) .OR. (mf(5)/=2 .AND. mf(5)/=0) .OR. (mf(6)/=2 .AND. mf(6)/=0) ) badfor = .TRUE.
               n = 6
               CALL spag_block_16
               RETURN
            ENDIF
         ELSEIF ( m(1)<=0 .OR. m(2)<0 ) THEN
            baddat = .TRUE.
            CALL spag_block_19
            RETURN
         ELSE
            IF ( mf(1)/=1 .OR. mf(2)>1 ) badfor = .TRUE.
            IF ( (mf(3)/=2 .AND. mf(3)/=0) .OR. (mf(4)/=2 .AND. mf(4)/=0) .OR. (mf(5)/=2 .AND. mf(5)/=0) ) badfor = .TRUE.
            n = 5
            CALL spag_block_16
            RETURN
         ENDIF
      ELSEIF ( kx==37 ) THEN
!
!*****         137-TIC        ******************************************
!
         IF ( m(1)<=0 .OR. m(2)<=0 .OR. m(3)<0 .OR. m(3)>6 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         n = 5
         CALL spag_block_4
         RETURN
      ELSEIF ( kx==42 ) THEN
!
!*****         142-TSTEP        ****************************************
!
         IF ( mf(5)/=0 .OR. mf(6)/=0 .OR. mf(7)/=0 .OR. mf(8)/=0 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         IF ( km/=0 ) THEN
            IF ( mf(1)==0 ) THEN
               CALL spag_block_18
               RETURN
            ENDIF
            baddat = .TRUE.
            CALL spag_block_19
            RETURN
         ELSE
            nmo = 3
            id = m(1)
            IF ( id<=0 .OR. mf(1)/=1 ) THEN
               baddat = .TRUE.
               CALL spag_block_19
               RETURN
            ELSE
               n = 1
               i(n) = m(1)
               CALL spag_block_18
               RETURN
            ENDIF
         ENDIF
      ELSEIF ( kx==58 ) THEN
!
!*****         158-EIGP        *****************************************
!
         IF ( m(1)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         DO l = 2 , 5 , 3
            IF ( m(l)/=0 .OR. m(l+1)/=0 .OR. m(l+2)/=0 ) THEN
               IF ( m(l+2)<=0 ) THEN
                  CALL spag_block_3
                  RETURN
               ENDIF
               n = n + 4
               i(n-3) = m(1)
               i(n-2) = m(l)
               i(n-1) = m(l+1)
               i(n) = m(l+2)
            ENDIF
         ENDDO
         IF ( n<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         CALL spag_block_5
         RETURN
      ELSEIF ( kx==66 ) THEN
!
!***********       166-FREQ2      **************************************
!
         IF ( rm(2)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         CALL spag_block_12
         RETURN
      ELSEIF ( kx==79 ) THEN
!
!*****         179-BAROR       *****************************************
!
         IF ( b1==0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         b1 = 0
         IF ( m(2)==0 .AND. m(5)==0 .AND. m(6)==0 .AND. m(7)==0 .AND. m(8)==0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(2)<0 .OR. m(8)<0 .OR. m(8)>2 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( mf(8)/=0 ) THEN
            CALL spag_block_8
            RETURN
         ENDIF
         IF ( mf(5)==1 .AND. mf(6)/=0 .AND. mf(7)/=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( mf(5)==1 .AND. mf(6)==0 .AND. mf(7)==0 ) m(8) = 2
         IF ( mf(5)==2 .OR. mf(6)==2 .OR. mf(7)==2 ) m(8) = 1
         CALL spag_block_8
         RETURN
      ELSEIF ( kx==80 ) THEN
!
!*****         180-CBAR        *****************************************
!
         IF ( mf(2)==0 ) THEN
            IF ( bardf2==0 ) THEN
               m(2) = m(1)
            ELSE
               m(2) = bardf2
            ENDIF
         ENDIF
         IF ( mf(5)==0 ) m(5) = bardf5
         IF ( mf(8)==0 ) m(8) = bardf8
         IF ( mf(5)>=3 .OR. mf(6)>=3 .OR. mf(7)>=3 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(8)==0 .AND. (mf(5)==2 .OR. mf(6)==2 .OR. mf(7)==2) ) m(8) = 1
         IF ( m(8)==0 .AND. mf(5)==1 .AND. mf(6)+mf(7)==0 ) m(8) = 2
         IF ( m(8)<=0 .OR. m(8)>2 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(8)/=2 ) THEN
            IF ( mf(6)==0 ) m(6) = bardf6
            IF ( mf(7)==0 ) m(7) = bardf7
         ENDIF
         IF ( m(1)<=0 .OR. m(2)<=0 .OR. m(3)<=0 .OR. m(4)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(8)==1 .AND. (mf(5)/=2 .AND. mf(5)/=0 .OR. m(5)==0 .AND. m(6)==0 .AND. m(7)==0) ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( (m(8)==2 .OR. m(8)==3) .AND. (mf(5)/=1 .AND. mf(5)/=0 .OR. m(5)<=0 .OR. m(6)/=0 .OR. m(7)/=0) ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( ifpdco(m(9)) ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(9)>65432 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( ifpdco(m(10)) ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(10)>65432 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(3)==m(4) .OR. m(3)==m(5) .AND. m(8)==2 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(8)==2 .AND. m(4)==m(5) ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         n = 16
         CALL spag_block_4
         RETURN
      ELSEIF ( kx==81 ) THEN
!
!*****         181-PBAR        *****************************************
!
         n = 19
         IF ( rm(4)<0. .OR. rm(5)<0. .OR. rm(4)*rm(5)<rm(19)**2 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         CALL spag_block_22
         RETURN
      ELSEIF ( kx==90 ) THEN
         CALL spag_block_21
         RETURN
      ENDIF
   ENDIF
   IF ( ky<=100 ) THEN
      IF ( ky==1 .OR. ky==2 .OR. ky==3 .OR. ky==4 .OR. ky==5 .OR. ky==6 .OR. ky==7 .OR. ky==8 .OR. ky==9 .OR. ky==10 .OR.           &
         & ky==11 .OR. ky==12 .OR. ky==13 .OR. ky==14 .OR. ky==16 .OR. ky==17 .OR. ky==18 .OR. ky==19 .OR. ky==20 .OR. ky==21 .OR.  &
         & ky==22 .OR. ky==39 .OR. ky==42 .OR. ky==44 .OR. ky==45 .OR. ky==46 .OR. ky==47 .OR. ky==48 .OR. ky==51 .OR. ky==52 .OR.  &
         & ky==53 .OR. ky==54 .OR. ky==55 .OR. ky==61 .OR. ky==62 .OR. ky==63 .OR. ky==64 .OR. ky==65 .OR. ky==66 .OR. ky==67 .OR.  &
         & ky==69 .OR. ky==70 .OR. ky==71 .OR. ky==72 .OR. ky==73 .OR. ky==74 .OR. ky==75 .OR. ky==76 .OR. ky==77 .OR. ky==78 .OR.  &
         & ky==79 .OR. ky==80 .OR. ky==81 .OR. ky==82 .OR. ky==83 .OR. ky==84 .OR. ky==85 .OR. ky==86 .OR. ky==87 .OR. ky==88 .OR.  &
         & ky==90 .OR. ky==95 .OR. ky==96 .OR. ky==97 .OR. ky==98 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
      IF ( ky==15 ) THEN
         l = 1
         CALL spag_block_17
         RETURN
      ELSEIF ( ky==23 ) THEN
!
!*****         223-AXSLOT         **************************************
!
         IF ( slot ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         slot = .TRUE.
         iaxf = iaxf + 2
         slotdf(1) = rm(1)
         slotdf(2) = rm(2)
         IF ( m(3)<0 ) baddat = .TRUE.
         klotdf(3) = m(3)
         slotdf(4) = rm(4)
         IF ( m(5)<0 ) baddat = .TRUE.
         klotdf(5) = m(5)
         n = 5
         CALL spag_block_4
         RETURN
      ELSEIF ( ky==24 ) THEN
!
!*****         224-CAXIF2         **************************************
!
         IF ( mf(4)/=0 .OR. mf(5)/=0 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         n = 3
         CALL spag_block_33
         RETURN
      ELSEIF ( ky==25 ) THEN
!
!*****         225-CAXIF3         **************************************
!
         IF ( mf(5)/=0 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         n = 4
         CALL spag_block_33
         RETURN
      ELSEIF ( ky==26 ) THEN
!
!*****         226-CAXIF4         **************************************
!
         n = 5
         CALL spag_block_33
         RETURN
      ELSEIF ( ky==27 ) THEN
!
!*****         227-CSLOT3         **************************************
!
         IF ( mf(5)/=0 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         n = 4
         CALL spag_block_34
         RETURN
      ELSEIF ( ky==28 ) THEN
!
!*****         228-CSLOT4         **************************************
!
         n = 5
         CALL spag_block_34
         RETURN
      ELSEIF ( ky==29 ) THEN
!
!*****         229-GRIDF          **************************************
!
         IF ( m(1)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( rm(2)<=0.0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         n = 3
         CALL spag_block_4
         RETURN
      ELSEIF ( ky==30 ) THEN
!
!*****         230-GRIDS          **************************************
!
         IF ( m(1)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(5)<0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( mf(4)==0 ) rm(4) = slotdf(4)
         n = 5
         CALL spag_block_4
         RETURN
      ELSEIF ( ky==31 ) THEN
!
!*****         231-SLBDY          **************************************
!
         IF ( km/=0 ) THEN
            iz = 1
         ELSE
            km = 1
            IF ( mf(1)/=2 .AND. mf(1)/=0 ) badfor = .TRUE.
            IF ( mf(1)==0 ) m(1) = klotdf(1)
            IF ( mf(2)/=1 .AND. mf(2)/=0 ) badfor = .TRUE.
            IF ( mf(2)==0 ) m(2) = klotdf(5)
            IF ( m(2)<0 ) baddat = .TRUE.
            i(1) = m(1)
            i(2) = m(2)
            n = 2
            iz = 3
         ENDIF
         DO l = iz , 8
            IF ( mf(l)==0 ) THEN
               CALL spag_block_36
               RETURN
            ENDIF
            IF ( m(l)<=0 ) baddat = .TRUE.
            n = n + 1
            i(n) = m(l)
         ENDDO
         CALL spag_block_35
         RETURN
      ELSEIF ( ky==32 ) THEN
!
!*****         232-CHBDY           *************************************
!
         IF ( m(1)>0 ) THEN
            i(1) = m(1)
            IF ( m(2)>=0 ) THEN
               i(2) = m(2)
               DO l = 1 , 7
                  IF ( m(3)==hbdynm(1,l) .AND. m(4)==hbdynm(2,l) ) THEN
                     CALL spag_block_37
                     RETURN
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
         CALL spag_block_3
         RETURN
      ELSEIF ( ky==33 ) THEN
!
!*****         233-QHBDY           *************************************
!
         IF ( m(1)>0 ) THEN
            i(1) = m(1)
            DO l = 1 , 5
               IF ( m(2)==hbdynm(1,l) .AND. m(3)==hbdynm(2,l) ) THEN
                  CALL spag_block_38
                  RETURN
               ENDIF
            ENDDO
         ENDIF
         CALL spag_block_3
         RETURN
      ELSEIF ( ky==34 ) THEN
         CALL spag_block_32
         RETURN
      ELSEIF ( ky==35 ) THEN
!
!*****         235-MAT5            *************************************
!
         IF ( m(1)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( rm(8)<=0.0 .AND. mf(8)==2 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         n = 8
         CALL spag_block_4
         RETURN
      ELSEIF ( ky==36 ) THEN
!
!*****         236-PHBDY           *************************************
!
         IF ( m(1)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(2)<0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( rm(3)<0.0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( rm(4)<0.0 .OR. rm(4)>1.0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( rm(5)<0.0 .OR. rm(5)>1.0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( mf(5)==0 ) rm(5) = rm(4)
         n = 7
         CALL spag_block_4
         RETURN
      ELSEIF ( ky==37 ) THEN
!
!*****         237-MATT4           *************************************
!
         IF ( m(1)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(2)<0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         n = 2
         CALL spag_block_4
         RETURN
      ELSEIF ( ky==38 ) THEN
!
!*****         238-MATT5           *************************************
!
         IF ( m(1)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( mf(8)/=0 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         n = 7
         CALL spag_block_4
         RETURN
      ELSEIF ( ky==40 ) THEN
!
!*****         240-QBDY2           *************************************
!
         IF ( m(1)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(2)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         n = 6
         CALL spag_block_4
         RETURN
      ELSEIF ( ky==41 ) THEN
!
!*****         241-QVECT           *************************************
!
         IF ( km/=0 ) THEN
            l = 1
         ELSE
            IF ( m(1)<=0 ) baddat = .TRUE.
            IF ( mf(2)/=2 .AND. mf(2)/=0 ) badfor = .TRUE.
            i(1) = m(1)
            i(2) = m(2)
            DO l = 3 , 6
               IF ( mf(l)==1 ) THEN
                  IF ( m(l)<0 ) baddat = .TRUE.
                  i(l) = m(l)
               ELSE
                  IF ( mf(l)/=2 .AND. mf(l)/=0 ) badfor = .TRUE.
                  i(l) = m(l)
               ENDIF
            ENDDO
            l = 6
            k914 = 209
         ENDIF
         km = 1
         kn = 1
         n = 6
         l4 = l
         IF ( mf(l)/=1 ) badfor = .TRUE.
         IF ( m(l4)<=0 ) baddat = .TRUE.
         SPAG_Loop_1_2: DO WHILE ( l/=8 )
            IF ( mf(l)==3 ) THEN
               IF ( mf(l+1)==1 .AND. m(l4)==thru ) THEN
                  IF ( m(l4-1)<m(l4+2) ) THEN
                     l1 = m(l4-1) + 1
                     l2 = m(l4+2) - 1
                     IF ( l2>l1 ) THEN
                        SPAG_Loop_2_1: DO
                           l3 = l1
                           i(n) = l3
                           CALL write(k914,i,n,0)
                           l1 = l1 + 1
                           IF ( l1>l2 ) EXIT SPAG_Loop_2_1
                        ENDDO SPAG_Loop_2_1
                     ENDIF
                     l = l + 1
                     l4 = l4 + 2
                     CYCLE
                  ENDIF
               ENDIF
               baddat = .TRUE.
               l = l + 1
               l4 = l4 + 2
            ELSEIF ( mf(l+1)==0 ) THEN
               IF ( m1(1)==0 .AND. m1(2)==0 ) badfor = .TRUE.
               EXIT SPAG_Loop_1_2
            ELSE
               IF ( m(l4)<=0 ) baddat = .TRUE.
               i(n) = m(l4)
               l = l + 1
               l4 = l4 + 1
               CALL write(k914,i,n,0)
            ENDIF
         ENDDO SPAG_Loop_1_2
         IF ( mf(l)/=1 ) badfor = .TRUE.
         i(n) = m(l4)
         IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
            kn = 0
            km = 0
         ENDIF
         CALL spag_block_6
         RETURN
      ELSEIF ( ky==43 ) THEN
!
!*****         243-RADLST          *************************************
!
         IF ( km==0 ) THEN
            IF ( idrdl==1 ) badfor = .TRUE.
            idrdl = 1
            k914 = 214
         ENDIF
         CALL spag_block_39
         RETURN
      ELSEIF ( ky==49 .OR. ky==59 ) THEN
         CALL spag_block_28
         RETURN
      ELSEIF ( ky==50 ) THEN
         kl = 25
         CALL spag_block_24
         RETURN
      ELSEIF ( ky==56 ) THEN
         CALL spag_block_20
         RETURN
      ELSEIF ( ky==57 ) THEN
!
!*****         257-CYJOIN       ***************************************
!
         IF ( km/=0 ) THEN
            CALL spag_block_39
            RETURN
         ENDIF
         IF ( m(1)/=1 .AND. m(1)/=2 ) baddat = .TRUE.
         i(1) = m(1)
         IF ( mf(2)==3 ) THEN
            IF ( m(2)/=bcdc .AND. m(2)/=bcdr .AND. m(2)/=bcds .AND. m(2)/=it1 .AND. m(2)/=it2 .AND. m(2)/=it3 ) baddat = .TRUE.
            i(2) = m(2)
            l4 = 4
         ELSE
            IF ( mf(2)/=0 ) badfor = .TRUE.
            i(2) = blk
            l4 = 3
         ENDIF
         km = 1
         i(3) = blk
         n = 3
         l = 3
         k914 = 210
         IF ( mf(l)/=1 ) badfor = .TRUE.
         IF ( m(l4)<=0 ) baddat = .TRUE.
         CALL spag_block_41
         RETURN
      ELSEIF ( ky==58 ) THEN
!
!*****         258-CNGRNT          *************************************
!
         IF ( km==0 ) k914 = 208
         CALL spag_block_39
         RETURN
      ELSEIF ( ky==60 ) THEN
         kl = 26
         CALL spag_block_24
         RETURN
      ELSEIF ( ky==68 ) THEN
!
!*****         268-SET1       ******************************************
!
         IF ( km/=0 ) THEN
            CALL spag_block_39
            RETURN
         ENDIF
         IF ( mf(1)/=1 ) badfor = .TRUE.
         i(1) = m(1)
         n = 1
         l = 2
         k914 = 204
         CALL spag_block_40
         RETURN
      ELSEIF ( ky==89 ) THEN
!
!*****                  289-VIEW                 ***************
!
         n = 6
         IF ( m(1)>0 ) THEN
            CALL spag_block_4
            RETURN
         ENDIF
         CALL spag_block_3
         RETURN
      ELSEIF ( ky==91 ) THEN
         CALL spag_block_42
         RETURN
      ELSEIF ( ky==92 ) THEN
!
!*****       292-PTRIM6         ****************************************
!
         IF ( m(2)<0 .OR. rm(3)<0.0 .OR. rm(4)<0.0 .OR. rm(5)<0.0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( rm(3)==0.0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( mf(1)/=1 .AND. mf(2)/=1 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         IF ( mf(3)/=2 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         DO l = 4 , 6
            IF ( mf(l)/=0 .AND. mf(l)/=2 ) THEN
               CALL spag_block_2
               RETURN
            ENDIF
         ENDDO
!
!*****       293-CTRPLT1        ****************************************
!
         n = 6
         CALL spag_block_23
         RETURN
      ELSEIF ( ky==93 ) THEN
         CALL spag_block_42
         RETURN
      ELSEIF ( ky==94 ) THEN
!
!*****       294-PTRPLT1        ****************************************
!
         IF ( m(2)<0 .OR. m(6)<0 .OR. m(2)==0 .AND. m(6)==0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(2)==0 .AND. m(3)/=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(6)==0 .AND. m(7)/=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( mf(1)/=1 .AND. mf(2)/=1 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         IF ( mf(6)/=0 .AND. mf(6)/=1 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         IF ( mf(3)/=2 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         IF ( mf(4)/=0 .AND. mf(4)/=2 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         IF ( mf(5)/=0 .AND. mf(5)/=2 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         DO l = 7 , 16
            IF ( mf(l)/=0 .AND. mf(l)/=2 ) THEN
               CALL spag_block_2
               RETURN
            ENDIF
         ENDDO
!
!*****       295-CTRSHL         ****************************************
!
         n = 16
         CALL spag_block_23
         RETURN
      ELSEIF ( ky==99 ) THEN
         CALL spag_block_42
         RETURN
      ELSEIF ( ky==100 ) THEN
!
!*****       296-PTRSHL         ****************************************
!
         IF ( m(2)<0 .OR. m(6)<0 .OR. m(10)<0 .OR. m(2)==0 .AND. m(6)==0 .AND. m(10)==0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(2)==0 .AND. rm(3)/=0.0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(6)==0 .AND. rm(7)/=0.0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(10)==0 .AND. rm(11)/=0.0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( rm(3)<0.0 .OR. rm(4)<0.0 .OR. rm(5)<0.0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( rm(7)<0.0 .OR. rm(8)<0.0 .OR. rm(9)<0.0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( rm(11)<0.0 .OR. rm(12)<0.0 .OR. rm(13)<0.0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( mf(10)/=0 .AND. mf(10)/=1 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         IF ( mf(1)/=1 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         IF ( mf(2)/=0 .AND. mf(2)/=1 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         IF ( mf(6)/=0 .AND. mf(6)/=1 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         DO l = 3 , 11 , 4
            IF ( mf(l)/=0 .AND. mf(l)/=2 ) THEN
               CALL spag_block_2
               RETURN
            ENDIF
            IF ( mf(l+1)/=0 .AND. mf(l+1)/=2 ) THEN
               CALL spag_block_2
               RETURN
            ENDIF
            IF ( mf(l+2)/=0 .AND. mf(l+2)/=2 ) THEN
               CALL spag_block_2
               RETURN
            ENDIF
         ENDDO
         n = 20
         CALL spag_block_23
         RETURN
      ENDIF
   ENDIF
   kz = ky - 100
   IF ( kz<=59 ) THEN
      IF ( kz==15 .OR. kz==19 ) THEN
!
!*****  315-MATPZ1,   319-MAT6  ****************************************
!
         IF ( m(1)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         n = 15
         IF ( k==319 ) n = 31
         CALL spag_block_4
         RETURN
      ELSEIF ( kz==16 ) THEN
!
!*****    316-MATPZ2        ********************************************
!
         IF ( m(1)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         n = 52
         CALL spag_block_4
         RETURN
      ELSEIF ( kz==17 .OR. kz==20 ) THEN
!
!*****     317-MTTPZ1,   320-MATT6   ***********************************
!
         n = 15
         IF ( k==320 ) n = 31
         DO l = 1 , n
            IF ( m(l)<0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
         ENDDO
         IF ( m(1)==0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         CALL spag_block_4
         RETURN
      ELSEIF ( kz==18 ) THEN
!
!*****   318-MTTPZ2    *************************************************
!
         DO l = 1 , 52
            IF ( m(l)<0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
         ENDDO
         IF ( m(1)==0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         n = 52
         CALL spag_block_4
         RETURN
      ELSEIF ( kz==31 ) THEN
         CALL spag_block_11
         RETURN
      ELSEIF ( kz==37 ) THEN
         CALL spag_block_32
         RETURN
      ELSEIF ( kz==40 ) THEN
!
!*****             340-NOLIN5         **********************************
!
         IF ( km/=0 ) THEN
            n = 8
            nmo = nmo + 8
            DO l = 1 , 8
               i(l) = m(l)
               IF ( mf(l)/=0 ) THEN
                  IF ( mf(l)/=1 .OR. m(l)<=0 ) baddat = .TRUE.
               ENDIF
            ENDDO
         ELSE
            km = 1
            kn = 1
            nmo = 8
            IF ( mf(1)/=1 .OR. m(1)<=0 ) baddat = .TRUE.
            IF ( mf(2)/=2 .OR. rm(2)<=0. ) baddat = .TRUE.
            IF ( mf(3)/=2 .OR. rm(3)<=0. ) baddat = .TRUE.
            IF ( mf(4)/=2 .OR. rm(4)<=0. ) baddat = .TRUE.
            IF ( mf(5)==1 .AND. m(5)<0 ) baddat = .TRUE.
            IF ( mf(6)==1 .AND. m(6)<0 ) baddat = .TRUE.
            IF ( mf(7)==1 .AND. m(7)<0 ) baddat = .TRUE.
            IF ( mf(8)==1 .AND. m(8)<0 ) baddat = .TRUE.
            IF ( mf(5)==2 .AND. rm(5)<0. ) baddat = .TRUE.
            IF ( mf(6)==2 .AND. rm(6)<0. ) baddat = .TRUE.
            IF ( mf(7)==2 .AND. rm(7)<0. ) baddat = .TRUE.
            IF ( mf(8)==2 .AND. rm(8)<0. ) baddat = .TRUE.
            n = 8
            DO l = 1 , 8
               i(l) = m(l)
            ENDDO
         ENDIF
         IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
            km = 0
            kn = 0
            IF ( nmo/=16 ) THEN
               IF ( nmo>16 ) baddat = .TRUE.
               DO l = 1 , 8
                  n = n + 1
                  i(n) = 0
               ENDDO
            ENDIF
         ENDIF
         CALL spag_block_6
         RETURN
      ELSEIF ( kz==41 ) THEN
         CALL spag_block_13
         RETURN
      ELSEIF ( kz==42 ) THEN
!
!*********       342-CFTUBE    *****************************************
!
         n = 4
         IF ( m(1)<=0 .OR. m(3)<=0 .OR. m(4)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( mf(2)==0 ) m(2) = m(1)
         IF ( m(2)<=3 .OR. m(3)==m(4) ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         CALL spag_block_4
         RETURN
      ELSEIF ( kz==43 ) THEN
!
!*********       343-PFTUBE    ****************************************
!
         n = 5
         IF ( m(1)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( rm(2)<=0. .OR. rm(3)<0. .OR. rm(4)<=0. ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( rm(5)==0. ) rm(5) = rm(4)
         IF ( rm(5)<0. ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         CALL spag_block_4
         RETURN
      ELSEIF ( kz==44 ) THEN
!
!*********       344-NFTUBE    *****************************************
!
         n = 5
         IF ( mf(2)/=1 .OR. m(1)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( mf(2)/=1 .OR. m(2)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( mf(3)/=1 .OR. m(3)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(2)==m(3) ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( mf(4)/=0 .AND. mf(4)/=2 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( mf(5)==1 .AND. m(5)<0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( mf(5)>2 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         CALL spag_block_4
         RETURN
      ELSEIF ( kz==56 ) THEN
         CALL spag_block_26
         RETURN
      ELSEIF ( kz==57 ) THEN
         CALL spag_block_27
         RETURN
      ELSEIF ( kz==58 ) THEN
         CALL spag_block_28
         RETURN
      ELSEIF ( kz==59 ) THEN
!
!*********     359-PPSE       ******************************************
!
         n = 5
         IF ( m(1)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( rm(2)==0. ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         rm(3) = 0.0
         rm(4) = 0.0
         rm(5) = 0.0
         CALL spag_block_4
         RETURN
      ENDIF
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      CALL page2(2)
      WRITE (Nout,99001) sfm
99001 FORMAT (A25,' 322, ILLEGAL ENTRY TO IFS1P.')
      Abort = .TRUE.
      RETURN 1
   END SUBROUTINE spag_block_1
   SUBROUTINE spag_block_2
      Badfor = .TRUE.
      RETURN 1
   END SUBROUTINE spag_block_2
   SUBROUTINE spag_block_3
      Baddat = .TRUE.
      RETURN 1
   END SUBROUTINE spag_block_3
   SUBROUTINE spag_block_4
      DO L = 1 , N
         i(L) = M(L)
      ENDDO
      CALL spag_block_5
   END SUBROUTINE spag_block_4
   SUBROUTINE spag_block_5
   END SUBROUTINE spag_block_5
   SUBROUTINE spag_block_6
      RETURN 3
   END SUBROUTINE spag_block_6
   SUBROUTINE spag_block_7
!
!*****         4-SEQGP,135-SEQEP    ************************************
!
      DO L = 1 , 7 , 2
         IF ( M(L)/=0 .OR. M(L+1)/=0 ) THEN
            IF ( M(L)<=0 .OR. M(L+1)<=0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            N = N + 2
            i(N-1) = M(L)
            i(N) = M(L+1)
            IF ( N>2 ) THEN
               DO L1 = 4 , N , 2
                  IF ( i(N-1)==i(L1-3) .OR. i(N)==i(L1-2) ) THEN
                     CALL spag_block_3
                     RETURN
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
      ENDDO
      IF ( N<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      CALL spag_block_5
   END SUBROUTINE spag_block_7
   SUBROUTINE spag_block_8
      Bardf2 = M(2)
      Bardf5 = M(5)
      Bardf6 = M(6)
      Bardf7 = M(7)
      Bardf8 = M(8)
      RETURN 2
   END SUBROUTINE spag_block_8
   SUBROUTINE spag_block_9
      DO L = 1 , 5 , 4
         IF ( M(L)/=0 .OR. M(L+1)/=0 .OR. M(L+2)/=0 .OR. M(L+3)/=0 ) THEN
            IF ( M(L)<=0 .OR. M(L+1)<=0 .OR. M(L+2)<=0 .OR. M(L+3)<=0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            IF ( M(L+1)==M(L+2) .OR. M(L+1)==M(L+3) .OR. M(L+3)==M(L+2) ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            N = N + 6
            IF ( N>6 .AND. M(L)==M(L-4) ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            i(N-5) = M(L)
            i(N-4) = L50
            i(N-3) = 1
            i(N-2) = M(L+1)
            i(N-1) = M(L+2)
            i(N) = M(L+3)
         ENDIF
      ENDDO
      IF ( N<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      CALL spag_block_5
   END SUBROUTINE spag_block_9
   SUBROUTINE spag_block_10
      i(1) = M(1)
      IF ( M(1)<=0 .OR. M(2)<0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( M(3)==M(6) .AND. M(4)==M(7) .AND. M(5)==M(8) ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( M(3)==M(9) .AND. M(4)==M(10) .AND. M(5)==M(11) ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( M(6)==M(9) .AND. M(7)==M(10) .AND. M(8)==M(11) ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      i(3) = 2
      DO L = 2 , 11
         i(L+2) = M(L)
      ENDDO
      N = 13
      CALL spag_block_5
   END SUBROUTINE spag_block_10
   SUBROUTINE spag_block_11
      Kl = 10
!
!*****   11-PLOTEL,   331-CFFREE   *************************************
!
      DO L = 1 , 5 , 4
         IF ( M(L)/=0 .OR. M(L+1)/=0 .OR. M(L+2)/=0 ) THEN
            IF ( M(L)<=0 .OR. M(L+1)<=0 .OR. M(L+2)<=0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            IF ( M(L+1)==M(L+2) ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            N = N + 3
            i(N-2) = M(L)
            i(N-1) = M(L+1)
            i(N) = M(L+2)
            IF ( e(Kl)>=0 ) THEN
               IF ( M(L)>e(Kl) ) THEN
                  e(Kl) = M(L)
               ELSE
                  e(Kl) = -M(L)
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      IF ( N<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      CALL spag_block_5
   END SUBROUTINE spag_block_11
   SUBROUTINE spag_block_12
!
!***********       125-FREQ1      **************************************
!
      IF ( M(1)<=0 .OR. rm(2)<0. .OR. rm(3)<=0. .OR. M(4)<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      N = 4
      CALL spag_block_4
   END SUBROUTINE spag_block_12
   SUBROUTINE spag_block_13
!
!*****         127-NOLIN1,341-NOLIN6    ********************************
!
      IF ( mf(8)/=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      CALL spag_block_15
   END SUBROUTINE spag_block_13
   SUBROUTINE spag_block_14
      IF ( M(1)<=0 .OR. M(2)<=0 .OR. M(3)<0 .OR. M(5)<=0 .OR. M(6)<0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( M(3)>6 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( (M(6)>6 .AND. M(6)<10) .OR. M(6)>16 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      N = 8
      CALL spag_block_4
   END SUBROUTINE spag_block_14
   SUBROUTINE spag_block_15
      IF ( mf(7)/=1 .OR. M(7)<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      CALL spag_block_14
   END SUBROUTINE spag_block_15
   SUBROUTINE spag_block_16
      DO L = 1 , N
         i(L) = M(L)
      ENDDO
      CALL spag_block_19
   END SUBROUTINE spag_block_16
   SUBROUTINE spag_block_17
      DO
         IF ( M(L)/=0 .OR. M(L+1)/=0 ) THEN
            IF ( M(L)<=0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            IF ( ifpdco(M(L+1)) ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            Iz = 6
            IF ( M(L+1)==0 ) Iz = 1
            DO L2 = 1 , Iz
               IF ( Iz==1 .OR. ll(L2)/=0 ) THEN
                  N = N + 2
                  i(N-1) = M(L)
                  i(N) = ll(L2)
                  IF ( N>2 ) THEN
                     DO L1 = 4 , N , 2
                        IF ( i(N-1)==i(L1-3) .AND. i(N)==i(L1-2) ) THEN
                           CALL spag_block_3
                           RETURN
                        ENDIF
                     ENDDO
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
         L = L + 2
         IF ( L>7 ) THEN
            IF ( N<=0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            CALL spag_block_5
            RETURN
         ENDIF
      ENDDO
      CALL spag_block_18
   END SUBROUTINE spag_block_17
   SUBROUTINE spag_block_18
      IF ( mf(2)/=1 .OR. mf(4)/=1 .OR. mf(3)/=2 ) THEN
         Baddat = .TRUE.
      ELSEIF ( M(4)<=0 .OR. rm(3)<=0. .OR. M(2)<M(4) ) THEN
         Baddat = .TRUE.
      ELSE
         N = N + 3
         i(N-2) = M(2)
         i(N-1) = M(3)
         i(N) = M(4)
      ENDIF
      CALL spag_block_19
   END SUBROUTINE spag_block_18
   SUBROUTINE spag_block_19
      IF ( m1(1)==0 .AND. m1(2)==0 ) THEN
         Km = 1
         Kn = 1
      ELSE
         Km = 0
         Kn = 0
         IF ( Nmo>0 ) THEN
            DO L = 1 , Nmo
               N = N + 1
               i(N) = -1
            ENDDO
         ENDIF
      ENDIF
      CALL spag_block_6
   END SUBROUTINE spag_block_19
   SUBROUTINE spag_block_20
!
!*****         16-SPC , 256-SPCD ***********************************
!
      IF ( M(1)<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      L = 2
      DO
         IF ( M(L)/=0 .OR. M(L+1)/=0 .OR. M(L+2)/=0 ) THEN
            IF ( M(L)<=0 .OR. M(L+1)<0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            IF ( ifpdco(M(L+1)) ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            N = N + 4
            IF ( N>4 .AND. M(L)==M(L-3) .AND. M(L+1)==M(L-2) ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            i(N-3) = M(1)
            i(N-2) = M(L)
            i(N-1) = M(L+1)
            i(N) = M(L+2)
         ENDIF
         L = L + 3
         IF ( L/=5 ) THEN
            IF ( N<=0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            CALL spag_block_5
            RETURN
         ENDIF
      ENDDO
      CALL spag_block_21
   END SUBROUTINE spag_block_20
   SUBROUTINE spag_block_21
!
!***************        190-RFORCE    *****************************
!
      IF ( mf(3)/=0 .AND. mf(3)/=1 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( M(1)<=0 .OR. M(2)<0 .OR. M(3)<0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( M(5)/=0 .OR. M(6)/=0 .OR. M(7)/=0 ) THEN
         N = 7
         CALL spag_block_4
         RETURN
      ELSE
         IF ( M(4)/=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         rm(5) = 1.0
         N = 7
         CALL spag_block_4
         RETURN
      ENDIF
   END SUBROUTINE spag_block_21
   SUBROUTINE spag_block_22
!WKBDB 2/95 SPR94015
!      IF (K .NE. 190) GO TO 3
!      IF (M(8) .EQ. 0) M(8) = 1
!      IF (M(8) .LT.0 .OR. M(8).GT.2) GO TO 8
!      N = 8
!WKBDE 2/95 SPR94015
      IF ( M(2)<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      CALL spag_block_23
   END SUBROUTINE spag_block_22
   SUBROUTINE spag_block_23
      IF ( M(1)<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      CALL spag_block_4
   END SUBROUTINE spag_block_23
   SUBROUTINE spag_block_24
!
!*****      34-PTRIA2,37-PTRMEM,39-PQUAD2,41-PQDMEM  *******************
!*****      42-PSHEAR,43-PTWIST,121-PTORDRG,250-PQDMEM1    *************
!*****      260-PQDMEM2
!
      DO L = 1 , 5 , 4
         IF ( M(L)/=0 .OR. M(L+1)/=0 .OR. M(L+2)/=0 ) THEN
            IF ( M(L)<=0 .OR. M(L+1)<=0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            IF ( rm(L+2)<=0.0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            N = N + 4
            i(N-3) = M(L)
            i(N-2) = M(L+1)
            i(N-1) = M(L+2)
            i(N) = M(L+3)
            IF ( e(Kl)>=0 ) THEN
               IF ( M(L)>e(Kl) ) THEN
                  e(Kl) = M(L)
               ELSE
                  e(Kl) = -M(L)
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      IF ( N<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      CALL spag_block_5
   END SUBROUTINE spag_block_24
   SUBROUTINE spag_block_25
!
!*****         44-PMASS,45-PDAMP    ***********************************
!
      DO L = 1 , 7 , 2
         IF ( M(L)/=0 .OR. M(L+1)/=0 ) THEN
            IF ( M(L)<=0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            N = N + 2
            i(N-1) = M(L)
            i(N) = M(L+1)
            IF ( e(Kl)>=0 ) THEN
               IF ( M(L)>e(Kl) ) THEN
                  e(Kl) = M(L)
               ELSE
                  e(Kl) = -M(L)
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      IF ( N<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      CALL spag_block_5
   END SUBROUTINE spag_block_25
   SUBROUTINE spag_block_26
!
!*****         48-CROD,49-CTUBE,50-CVISC,356-CPSE2    *****************
!
      DO L = 1 , 5 , 4
         IF ( M(L)/=0 .OR. M(L+1)/=0 .OR. M(L+2)/=0 .OR. M(L+3)/=0 ) THEN
            IF ( M(L)<=0 .OR. M(L+2)<=0 .OR. M(L+3)<=0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            IF ( mf(L+1)==0 ) M(L+1) = M(L)
            IF ( M(L+1)<=0 .OR. M(L+2)==M(L+3) ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            N = N + 4
            i(N-3) = M(L)
            i(N-2) = M(L+1)
            i(N-1) = M(L+2)
            i(N) = M(L+3)
            IF ( e(Kl)>=0 ) THEN
               IF ( M(L)>e(Kl) ) THEN
                  e(Kl) = M(L)
               ELSE
                  e(Kl) = -M(L)
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      IF ( N<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      CALL spag_block_5
   END SUBROUTINE spag_block_26
   SUBROUTINE spag_block_27
!
!*****       52-CTRIA1,53-CTRIA2,54-CTRBSC,55-CTRPLT,56-CTRMEM    ****
!            357-CPSE3
!
      IF ( M(3)<=0 .OR. M(4)<=0 .OR. M(5)<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( M(3)==M(4) .OR. M(4)==M(5) .OR. M(3)==M(5) ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      N = 6
      IF ( K==357 ) N = 5
      IF ( mf(2)==0 ) M(2) = M(1)
      CALL spag_block_22
   END SUBROUTINE spag_block_27
   SUBROUTINE spag_block_28
!
!*****       57-CQUAD1,58-CQUAD2,59-CQDPLT,60-CQDMEM,249-CQDMEM1    ****
!*****       259-CQDMEM2,358-CPSE4
!
      IF ( M(3)<=0 .OR. M(4)<=0 .OR. M(5)<=0 .OR. M(6)<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( M(3)==M(4) .OR. M(4)==M(5) .OR. M(5)==M(6) .OR. M(3)==M(5) .OR. M(4)==M(6) .OR. M(3)==M(6) ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      N = 7
      IF ( K==358 ) N = 6
      IF ( mf(2)==0 ) M(2) = M(1)
      CALL spag_block_22
   END SUBROUTINE spag_block_28
   SUBROUTINE spag_block_29
      IF ( M(1)<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( M(3)<0 .OR. M(4)<0 .OR. M(5)<0 .OR. M(6)<0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( M(4)>6 .OR. M(6)>6 .OR. M(3)==0 .AND. M(5)==0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( M(3)==0 .AND. M(4)/=0 .OR. M(5)==0 .AND. M(6)/=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( M(3)==M(5) .AND. M(4)==M(6) ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      Icell = M(4)
      M(4) = M(5)
      M(5) = Icell
      CALL spag_block_4
   END SUBROUTINE spag_block_29
   SUBROUTINE spag_block_30
!
!*****         67-CMASS3,75-CELAS3,71-CDAMP3    ************************
!
      DO L = 1 , 5 , 4
         IF ( M(L)/=0 .OR. M(L+1)/=0 .OR. M(L+2)/=0 .OR. M(L+3)/=0 ) THEN
            IF ( M(L)<=0 .OR. M(L+2)<0 .OR. M(L+3)<0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            IF ( mf(L+1)==0 ) M(L+1) = M(L)
            IF ( M(L+1)<=0 .OR. M(L+2)==M(L+3) ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            N = N + 4
            i(N-3) = M(L)
            i(N-2) = M(L+1)
            i(N-1) = M(L+2)
            i(N) = M(L+3)
            IF ( e(Kl)>=0 ) THEN
               IF ( M(L)>e(Kl) ) THEN
                  e(Kl) = M(L)
               ELSE
                  e(Kl) = -M(L)
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      IF ( N<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      CALL spag_block_5
   END SUBROUTINE spag_block_30
   SUBROUTINE spag_block_31
!
!*****         68-CMASS4,76-CELAS4,72-CDAMP4    ************************
!
      DO L = 1 , 5 , 4
         IF ( M(L)/=0 .OR. M(L+1)/=0 .OR. M(L+2)/=0 .OR. M(L+3)/=0 ) THEN
            IF ( M(L)<=0 .OR. M(L+2)<0 .OR. M(L+3)<0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            IF ( M(L+2)==M(L+3) ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            N = N + 4
            i(N-3) = M(L)
            i(N-2) = M(L+1)
            i(N-1) = M(L+2)
            i(N) = M(L+3)
            IF ( e(Kl)>=0 ) THEN
               IF ( M(L)>e(Kl) ) THEN
                  e(Kl) = M(L)
               ELSE
                  e(Kl) = -M(L)
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      IF ( N<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      CALL spag_block_5
   END SUBROUTINE spag_block_31
   SUBROUTINE spag_block_32
!
!*****     234-MAT4,   337-MATF   **************************************
!
      IF ( M(1)<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( rm(2)<=0.0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( rm(3)<=0.0 .AND. mf(3)==2 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      N = 3
      CALL spag_block_4
   END SUBROUTINE spag_block_32
   SUBROUTINE spag_block_33
      IF ( M(1)<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( mf(6)==0 ) rm(6) = slotdf(1)
      IF ( mf(7)==0 ) rm(7) = slotdf(2)
      IF ( mf(8)==0 ) M(8) = Klotdf(3)
      DO L = 2 , N
         IF ( M(L)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( L/=2 ) THEN
            DO L1 = 3 , L
               IF ( M(L1-1)==M(L) ) THEN
                  CALL spag_block_3
                  RETURN
               ENDIF
            ENDDO
         ENDIF
      ENDDO
!     CHECK FOR RHO .GE. 0.0
!     CHECK FOR B .GE. 0.0
!     CHECK FOR N .GE. 0
      DO L = 6 , 8
         L1 = L + N - 5
         i(L1) = M(L)
      ENDDO
      DO L = 1 , N
         i(L) = M(L)
      ENDDO
      N = N + 3
      CALL spag_block_5
   END SUBROUTINE spag_block_33
   SUBROUTINE spag_block_34
      IF ( mf(6)==0 ) rm(6) = slotdf(1)
      IF ( mf(7)==0 ) rm(7) = slotdf(2)
      IF ( mf(8)==0 ) M(8) = Klotdf(5)
!     CHECK FOR ALL KINDS OF THINGS
      DO L = 6 , 8
         L1 = L + N - 5
         i(L1) = M(L)
      ENDDO
      DO L = 1 , N
         i(L) = M(L)
      ENDDO
      N = N + 4
      i(N) = Klotdf(3)
      CALL spag_block_5
   END SUBROUTINE spag_block_34
   SUBROUTINE spag_block_35
      IF ( m1(1)/=0 .OR. m1(2)/=0 ) THEN
         Km = 0
         N = N + 1
         i(N) = -1
         Kn = 0
      ENDIF
      CALL spag_block_6
   END SUBROUTINE spag_block_35
   SUBROUTINE spag_block_36
      Iz = L + 1
      DO L = Iz , 8
         IF ( mf(L)/=0 ) Badfor = .TRUE.
      ENDDO
      IF ( m1(1)==0 .AND. m1(2)==0 ) Badfor = .TRUE.
      CALL spag_block_35
   END SUBROUTINE spag_block_36
   SUBROUTINE spag_block_37
      i(3) = L
      L1 = Hbdyix(L)
      DO L2 = 1 , L1
         IF ( M(L2+4)<=0 .OR. M(L2+9)<0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         i(L2+3) = M(L2+4)
         i(L2+7) = M(L2+9)
      ENDDO
      IF ( L1/=4 ) THEN
         DO L2 = L1 , 3
            IF ( M(L2+5)/=0 .OR. M(L2+10)/=0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            i(L2+4) = 0
            i(L2+8) = 0
         ENDDO
      ENDIF
      DO L2 = 12 , 14
         i(L2) = M(L2+2)
      ENDDO
      N = 15
      i(15) = M(9)
      CALL spag_block_5
   END SUBROUTINE spag_block_37
   SUBROUTINE spag_block_38
      i(2) = L
      L1 = Hbdyix(L)
      DO L2 = 1 , L1
         IF ( M(L2+5)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         i(L2+4) = M(L2+5)
      ENDDO
      IF ( L1/=4 ) THEN
         DO L2 = L1 , 3
            IF ( M(L2+6)/=0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            i(L2+5) = 0
         ENDDO
      ENDIF
      i(3) = M(4)
      IF ( L>=3 .AND. mf(4)/=0 ) THEN
         CALL spag_block_2
         RETURN
      ENDIF
      IF ( L<3 .AND. rm(5)<=0.0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      i(4) = M(5)
      N = 8
      CALL spag_block_5
   END SUBROUTINE spag_block_38
   SUBROUTINE spag_block_39
      L = 1
      N = 0
      CALL spag_block_40
   END SUBROUTINE spag_block_39
   SUBROUTINE spag_block_40
      Km = 1
      L4 = L
      IF ( mf(L)/=1 ) Badfor = .TRUE.
      IF ( M(L4)<=0 ) Baddat = .TRUE.
      CALL spag_block_41
   END SUBROUTINE spag_block_40
   SUBROUTINE spag_block_41
      SPAG_Loop_1_3: DO
         DO WHILE ( L<=8 )
            IF ( mf(L)==0 ) THEN
               DO L2 = L , 8
                  IF ( mf(L2)/=0 ) Badfor = .TRUE.
               ENDDO
               IF ( m1(1)/=0 .OR. m1(2)/=0 ) EXIT SPAG_Loop_1_3
               Badfor = .TRUE.
               Kn = 1
               CALL spag_block_6
               RETURN
            ELSEIF ( mf(L)==3 ) THEN
               IF ( L/=8 ) THEN
                  IF ( mf(L+1)==1 .AND. M(L4)==Thru ) THEN
                     IF ( M(L4-1)<M(L4+2) ) THEN
                        L1 = M(L4-1) + 1
                        L2 = M(L4+2)
                        DO
                           L3 = L1
                           IF ( N>=49 ) THEN
                              CALL write(K914,i,N,0)
                              N = 0
                           ENDIF
                           N = N + 1
                           i(N) = L3
                           L1 = L1 + 1
                           IF ( L1>L2 ) THEN
                              L = L + 2
                              L4 = L4 + 3
                              CYCLE SPAG_Loop_1_3
                           ENDIF
                        ENDDO
                     ENDIF
                  ENDIF
               ENDIF
               Baddat = .TRUE.
               L = L + 1
               L4 = L4 + 2
               CYCLE SPAG_Loop_1_3
            ELSE
               IF ( M(L4)<=0 ) Baddat = .TRUE.
               IF ( N>=49 ) THEN
                  CALL write(K914,i,N,0)
                  N = 0
               ENDIF
               N = N + 1
               i(N) = M(L4)
               L = L + 1
               L4 = L4 + 1
            ENDIF
         ENDDO
         IF ( m1(1)/=0 .OR. m1(2)/=0 ) EXIT SPAG_Loop_1_3
         Kn = 1
         CALL spag_block_6
         RETURN
      ENDDO SPAG_Loop_1_3
      Km = 0
      N = N + 1
      i(N) = -1
      Kn = 0
      CALL spag_block_6
   END SUBROUTINE spag_block_41
   SUBROUTINE spag_block_42
!
!*****       291-CTRIM6         ****************************************
!
      IF ( M(3)<=0 .OR. M(4)<=0 .OR. M(5)<=0 .OR. M(6)<=0 .OR. M(7)<=0 .OR. M(8)<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( M(3)==M(4) .OR. M(3)==M(5) .OR. M(3)==M(6) .OR. M(3)==M(7) .OR. M(3)==M(8) .OR. M(4)==M(5) .OR. M(4)==M(6) .OR.          &
         & M(4)==M(7) .OR. M(4)==M(8) ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( M(5)==M(6) .OR. M(5)==M(7) .OR. M(5)==M(8) .OR. M(6)==M(7) .OR. M(6)==M(8) .OR. M(7)==M(8) ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      DO L = 1 , 8
         IF ( mf(L)/=1 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
      ENDDO
      IF ( mf(9)/=0 .AND. mf(9)/=2 ) THEN
         CALL spag_block_2
         RETURN
      ENDIF
      IF ( mf(2)==0 ) M(2) = M(1)
      N = 9
      CALL spag_block_22
   END SUBROUTINE spag_block_42
!
END SUBROUTINE ifs1p
