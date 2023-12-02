!*==ifs1p.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ifs1p() !HIDESTARS (*,*,*)
   IMPLICIT NONE
   USE C_BLANK
   USE C_CIFS1P
   USE C_IFPDTA
   USE C_SYSTEM
   USE C_XMSSG
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
   IF ( K<=100 ) THEN
      IF ( K==1 .OR. K==2 .OR. K==3 .OR. K==12 .OR. K==13 .OR. K==17 .OR. K==28 .OR. K==32 .OR. K==51 .OR. K==79 .OR. K==80 .OR.    &
         & K==82 .OR. K==83 .OR. K==84 .OR. K==85 .OR. K==86 .OR. K==87 .OR. K==88 .OR. K==89 .OR. K==90 .OR. K==91 .OR. K==92 .OR. &
         & K==93 .OR. K==94 .OR. K==95 .OR. K==96 .OR. K==97 .OR. K==98 .OR. K==99 .OR. K==100 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
      IF ( K==4 ) THEN
         CALL spag_block_7
         RETURN
      ENDIF
      IF ( K==5 ) THEN
!
!*****         5-CORD1R        *****************************************
!
         l50 = 1
         CALL spag_block_9
         RETURN
      ELSEIF ( K==6 ) THEN
!
!*****         6-CORD1C        *****************************************
!
         l50 = 2
         CALL spag_block_9
         RETURN
      ELSEIF ( K==7 ) THEN
!
!*****         7-CORD1S        *****************************************
!
         l50 = 3
         CALL spag_block_9
         RETURN
      ELSEIF ( K==8 ) THEN
!
!*****         8-CORD2R        *****************************************
!
         I(2) = 1
         CALL spag_block_10
         RETURN
      ELSEIF ( K==9 ) THEN
!
!*****         9-CORD2C        *****************************************
!
         I(2) = 2
         CALL spag_block_10
         RETURN
      ELSEIF ( K==10 ) THEN
!
!*****         10-CORD2S       *****************************************
!
         I(2) = 3
         CALL spag_block_10
         RETURN
      ELSEIF ( K==11 ) THEN
         CALL spag_block_11
         RETURN
      ELSEIF ( K==14 .OR. K==15 ) THEN
!
!*****         14-SUPORT,15-OMIT,215-ASET         **********************
!
         l = 1
         CALL spag_block_17
         RETURN
      ELSEIF ( K==16 ) THEN
         CALL spag_block_20
         RETURN
      ELSEIF ( K==18 .OR. K==19 ) THEN
!
!*******       18-FORCE,19-MOMENT   **************************
!
         IF ( m(2)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         CALL spag_block_21
         RETURN
      ELSEIF ( K==20 .OR. K==21 ) THEN
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
         N = 5
         CALL spag_block_4
         RETURN
      ELSEIF ( K==22 .OR. K==23 ) THEN
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
         N = 7
         CALL spag_block_4
         RETURN
      ELSEIF ( K==24 ) THEN
!
!*****         24-PLOAD        *****************************************
!
         IF ( m(1)<=0 .OR. m(3)<=0 .OR. m(4)<=0 .OR. m(5)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(6)<0 .OR. m(6)==0 .AND. Mf(6)/=0 ) THEN
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
         N = 6
         CALL spag_block_4
         RETURN
      ELSEIF ( K==25 .OR. K==27 .OR. K==81 ) THEN
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
               N = N + 3
               I(N-2) = m(1)
               I(N-1) = m(l)
               I(N) = m(l+1)
               IF ( N>3 ) THEN
                  DO l1 = 6 , N , 3
                     IF ( I(N-1)==I(l1-4) ) THEN
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
         RETURN
      ELSEIF ( K==26 ) THEN
!
!*****         26-GRAV         *****************************************
!
         IF ( m(1)<=0 .OR. m(2)<0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(4)/=0 .OR. m(5)/=0 .OR. m(6)/=0 ) THEN
            N = 6
            CALL spag_block_4
            RETURN
         ELSE
            IF ( m(3)/=0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            Rm(4) = 1.0
            N = 6
            CALL spag_block_4
            RETURN
         ENDIF
      ELSEIF ( K==29 ) THEN
!
!*****         29-PROD         *****************************************
!
         N = 6
         CALL spag_block_22
         RETURN
      ELSEIF ( K==30 ) THEN
!
!*****         30-PTUBE        *****************************************
!
         N = 5
         IF ( Rm(3)<=0.0 .OR. Rm(4)<0.0 .OR. Rm(4)>0.5*Rm(3) ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( Rm(4)==0.0 ) Rm(4) = 0.5*Rm(3)
         CALL spag_block_22
         RETURN
      ELSEIF ( K==31 ) THEN
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
               N = N + 3
               I(N-2) = m(l)
               I(N-1) = m(l+1)
               I(N) = m(l+2)
               IF ( E(kl)>=0 ) THEN
                  IF ( m(l)>E(kl) ) THEN
                     E(kl) = m(l)
                  ELSE
                     E(kl) = -m(l)
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
         IF ( N<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         CALL spag_block_5
         RETURN
      ELSEIF ( K==33 .OR. K==38 ) THEN
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
         N = 10
         CALL spag_block_23
         RETURN
      ELSEIF ( K==34 ) THEN
         kl = 30
         CALL spag_block_24
         RETURN
      ELSEIF ( K==35 .OR. K==36 .OR. K==40 ) THEN
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
         N = 8
         CALL spag_block_23
         RETURN
      ELSEIF ( K==37 ) THEN
         kl = 31
         CALL spag_block_24
         RETURN
      ELSEIF ( K==39 ) THEN
         kl = 27
         CALL spag_block_24
         RETURN
      ELSEIF ( K==41 ) THEN
         kl = 24
         CALL spag_block_24
         RETURN
      ELSEIF ( K==42 ) THEN
         kl = 28
         CALL spag_block_24
         RETURN
      ELSEIF ( K==43 ) THEN
         kl = 32
         CALL spag_block_24
         RETURN
      ELSEIF ( K==44 ) THEN
         kl = 23
         CALL spag_block_25
         RETURN
      ELSEIF ( K==45 ) THEN
         kl = 21
         CALL spag_block_25
         RETURN
      ELSEIF ( K==46 ) THEN
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
               N = N + 4
               I(N-3) = m(l)
               I(N-2) = m(l+1)
               I(N-1) = m(l+2)
               I(N) = m(l+3)
               IF ( E(kl)>=0 ) THEN
                  IF ( m(l)>E(kl) ) THEN
                     E(kl) = m(l)
                  ELSE
                     E(kl) = -m(l)
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
         IF ( N<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         CALL spag_block_5
         RETURN
      ELSEIF ( K==47 ) THEN
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
         N = 8
         CALL spag_block_4
         RETURN
      ELSEIF ( K==48 ) THEN
         kl = 1
         CALL spag_block_26
         RETURN
      ELSEIF ( K==49 ) THEN
         kl = 2
         CALL spag_block_26
         RETURN
      ELSEIF ( K==50 ) THEN
         kl = 3
         CALL spag_block_26
         RETURN
      ELSEIF ( K==52 .OR. K==53 .OR. K==54 .OR. K==55 .OR. K==56 ) THEN
         CALL spag_block_27
         RETURN
      ELSEIF ( K==57 .OR. K==58 .OR. K==59 .OR. K==60 ) THEN
         CALL spag_block_28
         RETURN
      ELSEIF ( K==61 .OR. K==62 ) THEN
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
         N = 6
         IF ( Mf(2)==0 ) m(2) = m(1)
         CALL spag_block_22
         RETURN
      ELSEIF ( K==63 ) THEN
!
!*****         63-CONM1        *****************************************
!
         IF ( m(1)<0 .OR. m(2)<=0 .OR. m(3)<0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         N = 24
         CALL spag_block_4
         RETURN
      ELSEIF ( K==64 ) THEN
!
!*****         64-CONM2        *****************************************
!
         IF ( m(1)<0 .OR. m(2)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         DO l = 1 , 7
            I(l) = m(l)
         ENDDO
         DO l = 8 , 13
            I(l) = m(l+1)
         ENDDO
         N = 13
         CALL spag_block_5
         RETURN
      ELSEIF ( K==65 .OR. K==69 .OR. K==73 ) THEN
!
!*****         65-CMASS1,69-CDAMP1,73-CELAS1,70-CDAMP2,66-CMASS2    ****
!
         IF ( Mf(2)==0 ) m(2) = m(1)
         IF ( m(2)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         N = 6
         CALL spag_block_29
         RETURN
      ELSEIF ( K==66 .OR. K==70 ) THEN
         N = 6
         CALL spag_block_29
         RETURN
      ELSEIF ( K==67 ) THEN
         kl = 4
         CALL spag_block_30
         RETURN
      ELSEIF ( K==68 ) THEN
         kl = 7
         CALL spag_block_31
         RETURN
      ELSEIF ( K==71 ) THEN
         kl = 5
         CALL spag_block_30
         RETURN
      ELSEIF ( K==72 ) THEN
         kl = 8
         CALL spag_block_31
         RETURN
      ELSEIF ( K==74 ) THEN
!
!*****         74-CELAS2       *****************************************
!
         N = 8
         CALL spag_block_29
         RETURN
      ELSEIF ( K==75 ) THEN
         kl = 6
         CALL spag_block_30
         RETURN
      ELSEIF ( K==76 ) THEN
         kl = 9
         CALL spag_block_31
         RETURN
      ELSEIF ( K==77 ) THEN
!
!*****         77-MAT1         *****************************************
!
         IF ( m(1)<=0 .OR. (Rm(2)==0 .AND. Rm(3)==0) ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( (Rm(2)<0. .OR. Rm(3)<0.) .AND. Ksystm(78)>=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         N = 12
         IF ( m(12)<0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         l = 3
         IF ( Mf(2)==0 .OR. Rm(2)==0. ) l = l - 1
         IF ( Mf(3)==0 .OR. Rm(3)==0. ) l = l - 1
         IF ( Mf(4)==0 .OR. Rm(4)==0. ) l = l - 1
         IF ( l<2 ) THEN
            CALL page2(3)
            WRITE (nout,99001) Uwm , m(1)
99001       FORMAT (A25,' 2251, TWO OF THE E, G AND NU ON MAT1 CARD ',I8,' ARE ZEROS OR BLANKS.',/5X,                               &
                   &'POTENTIAL ERROR MAY OCCUR LATER')
         ENDIF
         IF ( Mf(2)/=2 .OR. Mf(3)/=2 .OR. Mf(4)/=2 ) THEN
            IF ( Mf(2)==0 ) Rm(2) = 2.0*Rm(3)*(1.0+Rm(4))
            IF ( Mf(3)==0 ) Rm(3) = Rm(2)/(2.0*(1.0+Rm(4)))
            IF ( Mf(4)==0 ) Rm(4) = Rm(2)/(2.0*Rm(3)) - 1.0
            IF ( Rm(4)<-1.0 .OR. Rm(4)>0.5 ) THEN
               CALL page2(2)
               WRITE (nout,99002) Uwm , m(1) , Rm(4)
99002          FORMAT (A25,' 2251, PHYSICALLY UNREALISTIC VALUE FOR NU ON MAT1 ','CARD ',I8,'.  VALUE = ',1P,E16.4)
            ENDIF
         ENDIF
         CALL spag_block_4
         RETURN
      ELSEIF ( K==78 ) THEN
!
!*****         78-MAT2         *****************************************
!
         N = 17
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
   IF ( Kx<=100 ) THEN
      IF ( Kx==1 .OR. Kx==2 .OR. Kx==3 .OR. Kx==4 .OR. Kx==5 .OR. Kx==6 .OR. Kx==7 .OR. Kx==8 .OR. Kx==9 .OR. Kx==10 .OR.           &
         & Kx==11 .OR. Kx==12 .OR. Kx==13 .OR. Kx==14 .OR. Kx==15 .OR. Kx==16 .OR. Kx==17 .OR. Kx==18 .OR. Kx==19 .OR. Kx==20 .OR.  &
         & Kx==22 .OR. Kx==23 .OR. Kx==24 .OR. Kx==26 .OR. Kx==31 .OR. Kx==32 .OR. Kx==33 .OR. Kx==34 .OR. Kx==38 .OR. Kx==39 .OR.  &
         & Kx==40 .OR. Kx==41 .OR. Kx==43 .OR. Kx==44 .OR. Kx==45 .OR. Kx==46 .OR. Kx==47 .OR. Kx==48 .OR. Kx==49 .OR. Kx==50 .OR.  &
         & Kx==51 .OR. Kx==52 .OR. Kx==53 .OR. Kx==54 .OR. Kx==55 .OR. Kx==56 .OR. Kx==57 .OR. Kx==59 .OR. Kx==60 .OR. Kx==61 .OR.  &
         & Kx==62 .OR. Kx==63 .OR. Kx==64 .OR. Kx==65 .OR. Kx==67 .OR. Kx==68 .OR. Kx==69 .OR. Kx==70 .OR. Kx==71 .OR. Kx==72 .OR.  &
         & Kx==73 .OR. Kx==74 .OR. Kx==75 .OR. Kx==76 .OR. Kx==77 .OR. Kx==78 .OR. Kx==82 .OR. Kx==83 .OR. Kx==84 .OR. Kx==85 .OR.  &
         & Kx==86 .OR. Kx==87 .OR. Kx==88 .OR. Kx==89 .OR. Kx==91 .OR. Kx==92 .OR. Kx==93 .OR. Kx==94 .OR. Kx==95 .OR. Kx==96 .OR.  &
         & Kx==97 .OR. Kx==98 .OR. Kx==99 .OR. Kx==100 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
      IF ( Kx==21 ) THEN
         kl = 29
         CALL spag_block_24
         RETURN
      ELSEIF ( Kx==25 ) THEN
         CALL spag_block_12
         RETURN
      ELSEIF ( Kx==27 ) THEN
         CALL spag_block_13
         RETURN
      ELSEIF ( Kx==28 ) THEN
!
!*****         128-NOLIN2        ***************************************
!
         IF ( m(8)<0 .OR. Mf(8)/=1 .AND. Mf(8)/=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( (m(8)>6 .AND. m(8)<10) .OR. m(8)>16 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         CALL spag_block_15
         RETURN
      ELSEIF ( Kx==29 .OR. Kx==30 ) THEN
!
!*****         129-NOLIN3,130-NOLIN4        ****************************
!
         IF ( Mf(8)/=0 .OR. Mf(7)/=2 .AND. Mf(7)/=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         CALL spag_block_14
         RETURN
      ELSEIF ( Kx==35 ) THEN
         CALL spag_block_7
         RETURN
      ELSEIF ( Kx==36 ) THEN
!
!*****         136-TF         ******************************************
!
         IF ( Km==0 ) THEN
            nmo = 5
            Id = m(1)
            IF ( Id<=0 .OR. m(2)<=0 .OR. m(3)<0 ) THEN
               Baddat = .TRUE.
               CALL spag_block_19
               RETURN
            ELSE
               IF ( Mf(1)/=1 .OR. Mf(2)/=1 .OR. Mf(3)>1 ) Badfor = .TRUE.
               IF ( (Mf(4)/=2 .AND. Mf(4)/=0) .OR. (Mf(5)/=2 .AND. Mf(5)/=0) .OR. (Mf(6)/=2 .AND. Mf(6)/=0) ) Badfor = .TRUE.
               N = 6
               CALL spag_block_16
               RETURN
            ENDIF
         ELSEIF ( m(1)<=0 .OR. m(2)<0 ) THEN
            Baddat = .TRUE.
            CALL spag_block_19
            RETURN
         ELSE
            IF ( Mf(1)/=1 .OR. Mf(2)>1 ) Badfor = .TRUE.
            IF ( (Mf(3)/=2 .AND. Mf(3)/=0) .OR. (Mf(4)/=2 .AND. Mf(4)/=0) .OR. (Mf(5)/=2 .AND. Mf(5)/=0) ) Badfor = .TRUE.
            N = 5
            CALL spag_block_16
            RETURN
         ENDIF
      ELSEIF ( Kx==37 ) THEN
!
!*****         137-TIC        ******************************************
!
         IF ( m(1)<=0 .OR. m(2)<=0 .OR. m(3)<0 .OR. m(3)>6 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         N = 5
         CALL spag_block_4
         RETURN
      ELSEIF ( Kx==42 ) THEN
!
!*****         142-TSTEP        ****************************************
!
         IF ( Mf(5)/=0 .OR. Mf(6)/=0 .OR. Mf(7)/=0 .OR. Mf(8)/=0 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         IF ( Km/=0 ) THEN
            IF ( Mf(1)==0 ) THEN
               CALL spag_block_18
               RETURN
            ENDIF
            Baddat = .TRUE.
            CALL spag_block_19
            RETURN
         ELSE
            nmo = 3
            Id = m(1)
            IF ( Id<=0 .OR. Mf(1)/=1 ) THEN
               Baddat = .TRUE.
               CALL spag_block_19
               RETURN
            ELSE
               N = 1
               I(N) = m(1)
               CALL spag_block_18
               RETURN
            ENDIF
         ENDIF
      ELSEIF ( Kx==58 ) THEN
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
               N = N + 4
               I(N-3) = m(1)
               I(N-2) = m(l)
               I(N-1) = m(l+1)
               I(N) = m(l+2)
            ENDIF
         ENDDO
         IF ( N<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         CALL spag_block_5
         RETURN
      ELSEIF ( Kx==66 ) THEN
!
!***********       166-FREQ2      **************************************
!
         IF ( Rm(2)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         CALL spag_block_12
         RETURN
      ELSEIF ( Kx==79 ) THEN
!
!*****         179-BAROR       *****************************************
!
         IF ( B1==0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         B1 = 0
         IF ( m(2)==0 .AND. m(5)==0 .AND. m(6)==0 .AND. m(7)==0 .AND. m(8)==0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(2)<0 .OR. m(8)<0 .OR. m(8)>2 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( Mf(8)/=0 ) THEN
            CALL spag_block_8
            RETURN
         ENDIF
         IF ( Mf(5)==1 .AND. Mf(6)/=0 .AND. Mf(7)/=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( Mf(5)==1 .AND. Mf(6)==0 .AND. Mf(7)==0 ) m(8) = 2
         IF ( Mf(5)==2 .OR. Mf(6)==2 .OR. Mf(7)==2 ) m(8) = 1
         CALL spag_block_8
         RETURN
      ELSEIF ( Kx==80 ) THEN
!
!*****         180-CBAR        *****************************************
!
         IF ( Mf(2)==0 ) THEN
            IF ( Bardf2==0 ) THEN
               m(2) = m(1)
            ELSE
               m(2) = Bardf2
            ENDIF
         ENDIF
         IF ( Mf(5)==0 ) m(5) = Bardf5
         IF ( Mf(8)==0 ) m(8) = Bardf8
         IF ( Mf(5)>=3 .OR. Mf(6)>=3 .OR. Mf(7)>=3 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(8)==0 .AND. (Mf(5)==2 .OR. Mf(6)==2 .OR. Mf(7)==2) ) m(8) = 1
         IF ( m(8)==0 .AND. Mf(5)==1 .AND. Mf(6)+Mf(7)==0 ) m(8) = 2
         IF ( m(8)<=0 .OR. m(8)>2 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(8)/=2 ) THEN
            IF ( Mf(6)==0 ) m(6) = Bardf6
            IF ( Mf(7)==0 ) m(7) = Bardf7
         ENDIF
         IF ( m(1)<=0 .OR. m(2)<=0 .OR. m(3)<=0 .OR. m(4)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(8)==1 .AND. (Mf(5)/=2 .AND. Mf(5)/=0 .OR. m(5)==0 .AND. m(6)==0 .AND. m(7)==0) ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( (m(8)==2 .OR. m(8)==3) .AND. (Mf(5)/=1 .AND. Mf(5)/=0 .OR. m(5)<=0 .OR. m(6)/=0 .OR. m(7)/=0) ) THEN
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
         N = 16
         CALL spag_block_4
         RETURN
      ELSEIF ( Kx==81 ) THEN
!
!*****         181-PBAR        *****************************************
!
         N = 19
         IF ( Rm(4)<0. .OR. Rm(5)<0. .OR. Rm(4)*Rm(5)<Rm(19)**2 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         CALL spag_block_22
         RETURN
      ELSEIF ( Kx==90 ) THEN
         CALL spag_block_21
         RETURN
      ENDIF
   ENDIF
   IF ( Ky<=100 ) THEN
      IF ( Ky==1 .OR. Ky==2 .OR. Ky==3 .OR. Ky==4 .OR. Ky==5 .OR. Ky==6 .OR. Ky==7 .OR. Ky==8 .OR. Ky==9 .OR. Ky==10 .OR.           &
         & Ky==11 .OR. Ky==12 .OR. Ky==13 .OR. Ky==14 .OR. Ky==16 .OR. Ky==17 .OR. Ky==18 .OR. Ky==19 .OR. Ky==20 .OR. Ky==21 .OR.  &
         & Ky==22 .OR. Ky==39 .OR. Ky==42 .OR. Ky==44 .OR. Ky==45 .OR. Ky==46 .OR. Ky==47 .OR. Ky==48 .OR. Ky==51 .OR. Ky==52 .OR.  &
         & Ky==53 .OR. Ky==54 .OR. Ky==55 .OR. Ky==61 .OR. Ky==62 .OR. Ky==63 .OR. Ky==64 .OR. Ky==65 .OR. Ky==66 .OR. Ky==67 .OR.  &
         & Ky==69 .OR. Ky==70 .OR. Ky==71 .OR. Ky==72 .OR. Ky==73 .OR. Ky==74 .OR. Ky==75 .OR. Ky==76 .OR. Ky==77 .OR. Ky==78 .OR.  &
         & Ky==79 .OR. Ky==80 .OR. Ky==81 .OR. Ky==82 .OR. Ky==83 .OR. Ky==84 .OR. Ky==85 .OR. Ky==86 .OR. Ky==87 .OR. Ky==88 .OR.  &
         & Ky==90 .OR. Ky==95 .OR. Ky==96 .OR. Ky==97 .OR. Ky==98 ) THEN
         CALL spag_block_1
         RETURN
      ENDIF
      IF ( Ky==15 ) THEN
         l = 1
         CALL spag_block_17
         RETURN
      ELSEIF ( Ky==23 ) THEN
!
!*****         223-AXSLOT         **************************************
!
         IF ( Slot ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         Slot = .TRUE.
         Iaxf = Iaxf + 2
         Slotdf(1) = Rm(1)
         Slotdf(2) = Rm(2)
         IF ( m(3)<0 ) Baddat = .TRUE.
         klotdf(3) = m(3)
         Slotdf(4) = Rm(4)
         IF ( m(5)<0 ) Baddat = .TRUE.
         klotdf(5) = m(5)
         N = 5
         CALL spag_block_4
         RETURN
      ELSEIF ( Ky==24 ) THEN
!
!*****         224-CAXIF2         **************************************
!
         IF ( Mf(4)/=0 .OR. Mf(5)/=0 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         N = 3
         CALL spag_block_33
         RETURN
      ELSEIF ( Ky==25 ) THEN
!
!*****         225-CAXIF3         **************************************
!
         IF ( Mf(5)/=0 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         N = 4
         CALL spag_block_33
         RETURN
      ELSEIF ( Ky==26 ) THEN
!
!*****         226-CAXIF4         **************************************
!
         N = 5
         CALL spag_block_33
         RETURN
      ELSEIF ( Ky==27 ) THEN
!
!*****         227-CSLOT3         **************************************
!
         IF ( Mf(5)/=0 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         N = 4
         CALL spag_block_34
         RETURN
      ELSEIF ( Ky==28 ) THEN
!
!*****         228-CSLOT4         **************************************
!
         N = 5
         CALL spag_block_34
         RETURN
      ELSEIF ( Ky==29 ) THEN
!
!*****         229-GRIDF          **************************************
!
         IF ( m(1)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( Rm(2)<=0.0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         N = 3
         CALL spag_block_4
         RETURN
      ELSEIF ( Ky==30 ) THEN
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
         IF ( Mf(4)==0 ) Rm(4) = Slotdf(4)
         N = 5
         CALL spag_block_4
         RETURN
      ELSEIF ( Ky==31 ) THEN
!
!*****         231-SLBDY          **************************************
!
         IF ( Km/=0 ) THEN
            iz = 1
         ELSE
            Km = 1
            IF ( Mf(1)/=2 .AND. Mf(1)/=0 ) Badfor = .TRUE.
            IF ( Mf(1)==0 ) m(1) = klotdf(1)
            IF ( Mf(2)/=1 .AND. Mf(2)/=0 ) Badfor = .TRUE.
            IF ( Mf(2)==0 ) m(2) = klotdf(5)
            IF ( m(2)<0 ) Baddat = .TRUE.
            I(1) = m(1)
            I(2) = m(2)
            N = 2
            iz = 3
         ENDIF
         DO l = iz , 8
            IF ( Mf(l)==0 ) THEN
               CALL spag_block_36
               RETURN
            ENDIF
            IF ( m(l)<=0 ) Baddat = .TRUE.
            N = N + 1
            I(N) = m(l)
         ENDDO
         CALL spag_block_35
         RETURN
      ELSEIF ( Ky==32 ) THEN
!
!*****         232-CHBDY           *************************************
!
         IF ( m(1)>0 ) THEN
            I(1) = m(1)
            IF ( m(2)>=0 ) THEN
               I(2) = m(2)
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
      ELSEIF ( Ky==33 ) THEN
!
!*****         233-QHBDY           *************************************
!
         IF ( m(1)>0 ) THEN
            I(1) = m(1)
            DO l = 1 , 5
               IF ( m(2)==hbdynm(1,l) .AND. m(3)==hbdynm(2,l) ) THEN
                  CALL spag_block_38
                  RETURN
               ENDIF
            ENDDO
         ENDIF
         CALL spag_block_3
         RETURN
      ELSEIF ( Ky==34 ) THEN
         CALL spag_block_32
         RETURN
      ELSEIF ( Ky==35 ) THEN
!
!*****         235-MAT5            *************************************
!
         IF ( m(1)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( Rm(8)<=0.0 .AND. Mf(8)==2 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         N = 8
         CALL spag_block_4
         RETURN
      ELSEIF ( Ky==36 ) THEN
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
         IF ( Rm(3)<0.0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( Rm(4)<0.0 .OR. Rm(4)>1.0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( Rm(5)<0.0 .OR. Rm(5)>1.0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( Mf(5)==0 ) Rm(5) = Rm(4)
         N = 7
         CALL spag_block_4
         RETURN
      ELSEIF ( Ky==37 ) THEN
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
         N = 2
         CALL spag_block_4
         RETURN
      ELSEIF ( Ky==38 ) THEN
!
!*****         238-MATT5           *************************************
!
         IF ( m(1)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( Mf(8)/=0 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         N = 7
         CALL spag_block_4
         RETURN
      ELSEIF ( Ky==40 ) THEN
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
         N = 6
         CALL spag_block_4
         RETURN
      ELSEIF ( Ky==41 ) THEN
!
!*****         241-QVECT           *************************************
!
         IF ( Km/=0 ) THEN
            l = 1
         ELSE
            IF ( m(1)<=0 ) Baddat = .TRUE.
            IF ( Mf(2)/=2 .AND. Mf(2)/=0 ) Badfor = .TRUE.
            I(1) = m(1)
            I(2) = m(2)
            DO l = 3 , 6
               IF ( Mf(l)==1 ) THEN
                  IF ( m(l)<0 ) Baddat = .TRUE.
                  I(l) = m(l)
               ELSE
                  IF ( Mf(l)/=2 .AND. Mf(l)/=0 ) Badfor = .TRUE.
                  I(l) = m(l)
               ENDIF
            ENDDO
            l = 6
            k914 = 209
         ENDIF
         Km = 1
         Kn = 1
         N = 6
         l4 = l
         IF ( Mf(l)/=1 ) Badfor = .TRUE.
         IF ( m(l4)<=0 ) Baddat = .TRUE.
         SPAG_Loop_1_2: DO WHILE ( l/=8 )
            IF ( Mf(l)==3 ) THEN
               IF ( Mf(l+1)==1 .AND. m(l4)==thru ) THEN
                  IF ( m(l4-1)<m(l4+2) ) THEN
                     l1 = m(l4-1) + 1
                     l2 = m(l4+2) - 1
                     IF ( l2>l1 ) THEN
                        SPAG_Loop_2_1: DO
                           l3 = l1
                           I(N) = l3
                           CALL write(k914,I,N,0)
                           l1 = l1 + 1
                           IF ( l1>l2 ) EXIT SPAG_Loop_2_1
                        ENDDO SPAG_Loop_2_1
                     ENDIF
                     l = l + 1
                     l4 = l4 + 2
                     CYCLE
                  ENDIF
               ENDIF
               Baddat = .TRUE.
               l = l + 1
               l4 = l4 + 2
            ELSEIF ( Mf(l+1)==0 ) THEN
               IF ( M1(1)==0 .AND. M1(2)==0 ) Badfor = .TRUE.
               EXIT SPAG_Loop_1_2
            ELSE
               IF ( m(l4)<=0 ) Baddat = .TRUE.
               I(N) = m(l4)
               l = l + 1
               l4 = l4 + 1
               CALL write(k914,I,N,0)
            ENDIF
         ENDDO SPAG_Loop_1_2
         IF ( Mf(l)/=1 ) Badfor = .TRUE.
         I(N) = m(l4)
         IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
            Kn = 0
            Km = 0
         ENDIF
         CALL spag_block_6
         RETURN
      ELSEIF ( Ky==43 ) THEN
!
!*****         243-RADLST          *************************************
!
         IF ( Km==0 ) THEN
            IF ( Idrdl==1 ) Badfor = .TRUE.
            Idrdl = 1
            k914 = 214
         ENDIF
         CALL spag_block_39
         RETURN
      ELSEIF ( Ky==49 .OR. Ky==59 ) THEN
         CALL spag_block_28
         RETURN
      ELSEIF ( Ky==50 ) THEN
         kl = 25
         CALL spag_block_24
         RETURN
      ELSEIF ( Ky==56 ) THEN
         CALL spag_block_20
         RETURN
      ELSEIF ( Ky==57 ) THEN
!
!*****         257-CYJOIN       ***************************************
!
         IF ( Km/=0 ) THEN
            CALL spag_block_39
            RETURN
         ENDIF
         IF ( m(1)/=1 .AND. m(1)/=2 ) Baddat = .TRUE.
         I(1) = m(1)
         IF ( Mf(2)==3 ) THEN
            IF ( m(2)/=bcdc .AND. m(2)/=bcdr .AND. m(2)/=bcds .AND. m(2)/=it1 .AND. m(2)/=it2 .AND. m(2)/=it3 ) Baddat = .TRUE.
            I(2) = m(2)
            l4 = 4
         ELSE
            IF ( Mf(2)/=0 ) Badfor = .TRUE.
            I(2) = blk
            l4 = 3
         ENDIF
         Km = 1
         I(3) = blk
         N = 3
         l = 3
         k914 = 210
         IF ( Mf(l)/=1 ) Badfor = .TRUE.
         IF ( m(l4)<=0 ) Baddat = .TRUE.
         CALL spag_block_41
         RETURN
      ELSEIF ( Ky==58 ) THEN
!
!*****         258-CNGRNT          *************************************
!
         IF ( Km==0 ) k914 = 208
         CALL spag_block_39
         RETURN
      ELSEIF ( Ky==60 ) THEN
         kl = 26
         CALL spag_block_24
         RETURN
      ELSEIF ( Ky==68 ) THEN
!
!*****         268-SET1       ******************************************
!
         IF ( Km/=0 ) THEN
            CALL spag_block_39
            RETURN
         ENDIF
         IF ( Mf(1)/=1 ) Badfor = .TRUE.
         I(1) = m(1)
         N = 1
         l = 2
         k914 = 204
         CALL spag_block_40
         RETURN
      ELSEIF ( Ky==89 ) THEN
!
!*****                  289-VIEW                 ***************
!
         N = 6
         IF ( m(1)>0 ) THEN
            CALL spag_block_4
            RETURN
         ENDIF
         CALL spag_block_3
         RETURN
      ELSEIF ( Ky==91 ) THEN
         CALL spag_block_42
         RETURN
      ELSEIF ( Ky==92 ) THEN
!
!*****       292-PTRIM6         ****************************************
!
         IF ( m(2)<0 .OR. Rm(3)<0.0 .OR. Rm(4)<0.0 .OR. Rm(5)<0.0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( Rm(3)==0.0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( Mf(1)/=1 .AND. Mf(2)/=1 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         IF ( Mf(3)/=2 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         DO l = 4 , 6
            IF ( Mf(l)/=0 .AND. Mf(l)/=2 ) THEN
               CALL spag_block_2
               RETURN
            ENDIF
         ENDDO
!
!*****       293-CTRPLT1        ****************************************
!
         N = 6
         CALL spag_block_23
         RETURN
      ELSEIF ( Ky==93 ) THEN
         CALL spag_block_42
         RETURN
      ELSEIF ( Ky==94 ) THEN
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
         IF ( Mf(1)/=1 .AND. Mf(2)/=1 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         IF ( Mf(6)/=0 .AND. Mf(6)/=1 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         IF ( Mf(3)/=2 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         IF ( Mf(4)/=0 .AND. Mf(4)/=2 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         IF ( Mf(5)/=0 .AND. Mf(5)/=2 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         DO l = 7 , 16
            IF ( Mf(l)/=0 .AND. Mf(l)/=2 ) THEN
               CALL spag_block_2
               RETURN
            ENDIF
         ENDDO
!
!*****       295-CTRSHL         ****************************************
!
         N = 16
         CALL spag_block_23
         RETURN
      ELSEIF ( Ky==99 ) THEN
         CALL spag_block_42
         RETURN
      ELSEIF ( Ky==100 ) THEN
!
!*****       296-PTRSHL         ****************************************
!
         IF ( m(2)<0 .OR. m(6)<0 .OR. m(10)<0 .OR. m(2)==0 .AND. m(6)==0 .AND. m(10)==0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(2)==0 .AND. Rm(3)/=0.0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(6)==0 .AND. Rm(7)/=0.0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(10)==0 .AND. Rm(11)/=0.0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( Rm(3)<0.0 .OR. Rm(4)<0.0 .OR. Rm(5)<0.0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( Rm(7)<0.0 .OR. Rm(8)<0.0 .OR. Rm(9)<0.0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( Rm(11)<0.0 .OR. Rm(12)<0.0 .OR. Rm(13)<0.0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( Mf(10)/=0 .AND. Mf(10)/=1 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         IF ( Mf(1)/=1 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         IF ( Mf(2)/=0 .AND. Mf(2)/=1 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         IF ( Mf(6)/=0 .AND. Mf(6)/=1 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
         DO l = 3 , 11 , 4
            IF ( Mf(l)/=0 .AND. Mf(l)/=2 ) THEN
               CALL spag_block_2
               RETURN
            ENDIF
            IF ( Mf(l+1)/=0 .AND. Mf(l+1)/=2 ) THEN
               CALL spag_block_2
               RETURN
            ENDIF
            IF ( Mf(l+2)/=0 .AND. Mf(l+2)/=2 ) THEN
               CALL spag_block_2
               RETURN
            ENDIF
         ENDDO
         N = 20
         CALL spag_block_23
         RETURN
      ENDIF
   ENDIF
   kz = Ky - 100
   IF ( kz<=59 ) THEN
      IF ( kz==15 .OR. kz==19 ) THEN
!
!*****  315-MATPZ1,   319-MAT6  ****************************************
!
         IF ( m(1)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         N = 15
         IF ( K==319 ) N = 31
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
         N = 52
         CALL spag_block_4
         RETURN
      ELSEIF ( kz==17 .OR. kz==20 ) THEN
!
!*****     317-MTTPZ1,   320-MATT6   ***********************************
!
         N = 15
         IF ( K==320 ) N = 31
         DO l = 1 , N
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
         N = 52
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
         IF ( Km/=0 ) THEN
            N = 8
            nmo = nmo + 8
            DO l = 1 , 8
               I(l) = m(l)
               IF ( Mf(l)/=0 ) THEN
                  IF ( Mf(l)/=1 .OR. m(l)<=0 ) Baddat = .TRUE.
               ENDIF
            ENDDO
         ELSE
            Km = 1
            Kn = 1
            nmo = 8
            IF ( Mf(1)/=1 .OR. m(1)<=0 ) Baddat = .TRUE.
            IF ( Mf(2)/=2 .OR. Rm(2)<=0. ) Baddat = .TRUE.
            IF ( Mf(3)/=2 .OR. Rm(3)<=0. ) Baddat = .TRUE.
            IF ( Mf(4)/=2 .OR. Rm(4)<=0. ) Baddat = .TRUE.
            IF ( Mf(5)==1 .AND. m(5)<0 ) Baddat = .TRUE.
            IF ( Mf(6)==1 .AND. m(6)<0 ) Baddat = .TRUE.
            IF ( Mf(7)==1 .AND. m(7)<0 ) Baddat = .TRUE.
            IF ( Mf(8)==1 .AND. m(8)<0 ) Baddat = .TRUE.
            IF ( Mf(5)==2 .AND. Rm(5)<0. ) Baddat = .TRUE.
            IF ( Mf(6)==2 .AND. Rm(6)<0. ) Baddat = .TRUE.
            IF ( Mf(7)==2 .AND. Rm(7)<0. ) Baddat = .TRUE.
            IF ( Mf(8)==2 .AND. Rm(8)<0. ) Baddat = .TRUE.
            N = 8
            DO l = 1 , 8
               I(l) = m(l)
            ENDDO
         ENDIF
         IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
            Km = 0
            Kn = 0
            IF ( nmo/=16 ) THEN
               IF ( nmo>16 ) Baddat = .TRUE.
               DO l = 1 , 8
                  N = N + 1
                  I(N) = 0
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
         N = 4
         IF ( m(1)<=0 .OR. m(3)<=0 .OR. m(4)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( Mf(2)==0 ) m(2) = m(1)
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
         N = 5
         IF ( m(1)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( Rm(2)<=0. .OR. Rm(3)<0. .OR. Rm(4)<=0. ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( Rm(5)==0. ) Rm(5) = Rm(4)
         IF ( Rm(5)<0. ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         CALL spag_block_4
         RETURN
      ELSEIF ( kz==44 ) THEN
!
!*********       344-NFTUBE    *****************************************
!
         N = 5
         IF ( Mf(2)/=1 .OR. m(1)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( Mf(2)/=1 .OR. m(2)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( Mf(3)/=1 .OR. m(3)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( m(2)==m(3) ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( Mf(4)/=0 .AND. Mf(4)/=2 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( Mf(5)==1 .AND. m(5)<0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( Mf(5)>2 ) THEN
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
         N = 5
         IF ( m(1)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( Rm(2)==0. ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         Rm(3) = 0.0
         Rm(4) = 0.0
         Rm(5) = 0.0
         CALL spag_block_4
         RETURN
      ENDIF
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      CALL page2(2)
      WRITE (nout,99003) Sfm
99003 FORMAT (A25,' 322, ILLEGAL ENTRY TO IFS1P.')
      abort = .TRUE.
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
      DO l = 1 , N
         I(l) = m(l)
      ENDDO
      CALL spag_block_5
   END SUBROUTINE spag_block_4
   SUBROUTINE spag_block_5
      RETURN
   END SUBROUTINE spag_block_5
   SUBROUTINE spag_block_6
      RETURN 3
   END SUBROUTINE spag_block_6
   SUBROUTINE spag_block_7
!
!*****         4-SEQGP,135-SEQEP    ************************************
!
      DO l = 1 , 7 , 2
         IF ( m(l)/=0 .OR. m(l+1)/=0 ) THEN
            IF ( m(l)<=0 .OR. m(l+1)<=0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            N = N + 2
            I(N-1) = m(l)
            I(N) = m(l+1)
            IF ( N>2 ) THEN
               DO l1 = 4 , N , 2
                  IF ( I(N-1)==I(l1-3) .OR. I(N)==I(l1-2) ) THEN
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
      RETURN
   END SUBROUTINE spag_block_7
   SUBROUTINE spag_block_8
      Bardf2 = m(2)
      Bardf5 = m(5)
      Bardf6 = m(6)
      Bardf7 = m(7)
      Bardf8 = m(8)
      RETURN 2
   END SUBROUTINE spag_block_8
   SUBROUTINE spag_block_9
      DO l = 1 , 5 , 4
         IF ( m(l)/=0 .OR. m(l+1)/=0 .OR. m(l+2)/=0 .OR. m(l+3)/=0 ) THEN
            IF ( m(l)<=0 .OR. m(l+1)<=0 .OR. m(l+2)<=0 .OR. m(l+3)<=0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            IF ( m(l+1)==m(l+2) .OR. m(l+1)==m(l+3) .OR. m(l+3)==m(l+2) ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            N = N + 6
            IF ( N>6 .AND. m(l)==m(l-4) ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            I(N-5) = m(l)
            I(N-4) = l50
            I(N-3) = 1
            I(N-2) = m(l+1)
            I(N-1) = m(l+2)
            I(N) = m(l+3)
         ENDIF
      ENDDO
      IF ( N<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      CALL spag_block_5
      RETURN
   END SUBROUTINE spag_block_9
   SUBROUTINE spag_block_10
      I(1) = m(1)
      IF ( m(1)<=0 .OR. m(2)<0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( m(3)==m(6) .AND. m(4)==m(7) .AND. m(5)==m(8) ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( m(3)==m(9) .AND. m(4)==m(10) .AND. m(5)==m(11) ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( m(6)==m(9) .AND. m(7)==m(10) .AND. m(8)==m(11) ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      I(3) = 2
      DO l = 2 , 11
         I(l+2) = m(l)
      ENDDO
      N = 13
      CALL spag_block_5
      RETURN
   END SUBROUTINE spag_block_10
   SUBROUTINE spag_block_11
      kl = 10
!
!*****   11-PLOTEL,   331-CFFREE   *************************************
!
      DO l = 1 , 5 , 4
         IF ( m(l)/=0 .OR. m(l+1)/=0 .OR. m(l+2)/=0 ) THEN
            IF ( m(l)<=0 .OR. m(l+1)<=0 .OR. m(l+2)<=0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            IF ( m(l+1)==m(l+2) ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            N = N + 3
            I(N-2) = m(l)
            I(N-1) = m(l+1)
            I(N) = m(l+2)
            IF ( E(kl)>=0 ) THEN
               IF ( m(l)>E(kl) ) THEN
                  E(kl) = m(l)
               ELSE
                  E(kl) = -m(l)
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      IF ( N<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      CALL spag_block_5
      RETURN
   END SUBROUTINE spag_block_11
   SUBROUTINE spag_block_12
!
!***********       125-FREQ1      **************************************
!
      IF ( m(1)<=0 .OR. Rm(2)<0. .OR. Rm(3)<=0. .OR. m(4)<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      N = 4
      CALL spag_block_4
      RETURN
   END SUBROUTINE spag_block_12
   SUBROUTINE spag_block_13
!
!*****         127-NOLIN1,341-NOLIN6    ********************************
!
      IF ( Mf(8)/=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      CALL spag_block_15
      RETURN
   END SUBROUTINE spag_block_13
   SUBROUTINE spag_block_14
      IF ( m(1)<=0 .OR. m(2)<=0 .OR. m(3)<0 .OR. m(5)<=0 .OR. m(6)<0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( m(3)>6 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( (m(6)>6 .AND. m(6)<10) .OR. m(6)>16 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      N = 8
      CALL spag_block_4
      RETURN
   END SUBROUTINE spag_block_14
   SUBROUTINE spag_block_15
      IF ( Mf(7)/=1 .OR. m(7)<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      CALL spag_block_14
      RETURN
   END SUBROUTINE spag_block_15
   SUBROUTINE spag_block_16
      DO l = 1 , N
         I(l) = m(l)
      ENDDO
      CALL spag_block_19
      RETURN
   END SUBROUTINE spag_block_16
   SUBROUTINE spag_block_17
      DO
         IF ( m(l)/=0 .OR. m(l+1)/=0 ) THEN
            IF ( m(l)<=0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            IF ( ifpdco(m(l+1)) ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            iz = 6
            IF ( m(l+1)==0 ) iz = 1
            DO l2 = 1 , iz
               IF ( iz==1 .OR. Ll(l2)/=0 ) THEN
                  N = N + 2
                  I(N-1) = m(l)
                  I(N) = Ll(l2)
                  IF ( N>2 ) THEN
                     DO l1 = 4 , N , 2
                        IF ( I(N-1)==I(l1-3) .AND. I(N)==I(l1-2) ) THEN
                           CALL spag_block_3
                           RETURN
                        ENDIF
                     ENDDO
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
         l = l + 2
         IF ( l>7 ) THEN
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
      IF ( Mf(2)/=1 .OR. Mf(4)/=1 .OR. Mf(3)/=2 ) THEN
         Baddat = .TRUE.
      ELSEIF ( m(4)<=0 .OR. Rm(3)<=0. .OR. m(2)<m(4) ) THEN
         Baddat = .TRUE.
      ELSE
         N = N + 3
         I(N-2) = m(2)
         I(N-1) = m(3)
         I(N) = m(4)
      ENDIF
      CALL spag_block_19
   END SUBROUTINE spag_block_18
   SUBROUTINE spag_block_19
      IF ( M1(1)==0 .AND. M1(2)==0 ) THEN
         Km = 1
         Kn = 1
      ELSE
         Km = 0
         Kn = 0
         IF ( nmo>0 ) THEN
            DO l = 1 , nmo
               N = N + 1
               I(N) = -1
            ENDDO
         ENDIF
      ENDIF
      CALL spag_block_6
      RETURN
   END SUBROUTINE spag_block_19
   SUBROUTINE spag_block_20
!
!*****         16-SPC , 256-SPCD ***********************************
!
      IF ( m(1)<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      l = 2
      DO
         IF ( m(l)/=0 .OR. m(l+1)/=0 .OR. m(l+2)/=0 ) THEN
            IF ( m(l)<=0 .OR. m(l+1)<0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            IF ( ifpdco(m(l+1)) ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            N = N + 4
            IF ( N>4 .AND. m(l)==m(l-3) .AND. m(l+1)==m(l-2) ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            I(N-3) = m(1)
            I(N-2) = m(l)
            I(N-1) = m(l+1)
            I(N) = m(l+2)
         ENDIF
         l = l + 3
         IF ( l/=5 ) THEN
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
      IF ( Mf(3)/=0 .AND. Mf(3)/=1 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( m(1)<=0 .OR. m(2)<0 .OR. m(3)<0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( m(5)/=0 .OR. m(6)/=0 .OR. m(7)/=0 ) THEN
         N = 7
         CALL spag_block_4
         RETURN
      ELSE
         IF ( m(4)/=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         Rm(5) = 1.0
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
      IF ( m(2)<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      CALL spag_block_23
   END SUBROUTINE spag_block_22
   SUBROUTINE spag_block_23
      IF ( m(1)<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      CALL spag_block_4
      RETURN
   END SUBROUTINE spag_block_23
   SUBROUTINE spag_block_24
!
!*****      34-PTRIA2,37-PTRMEM,39-PQUAD2,41-PQDMEM  *******************
!*****      42-PSHEAR,43-PTWIST,121-PTORDRG,250-PQDMEM1    *************
!*****      260-PQDMEM2
!
      DO l = 1 , 5 , 4
         IF ( m(l)/=0 .OR. m(l+1)/=0 .OR. m(l+2)/=0 ) THEN
            IF ( m(l)<=0 .OR. m(l+1)<=0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            IF ( Rm(l+2)<=0.0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            N = N + 4
            I(N-3) = m(l)
            I(N-2) = m(l+1)
            I(N-1) = m(l+2)
            I(N) = m(l+3)
            IF ( E(kl)>=0 ) THEN
               IF ( m(l)>E(kl) ) THEN
                  E(kl) = m(l)
               ELSE
                  E(kl) = -m(l)
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      IF ( N<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      CALL spag_block_5
      RETURN
   END SUBROUTINE spag_block_24
   SUBROUTINE spag_block_25
!
!*****         44-PMASS,45-PDAMP    ***********************************
!
      DO l = 1 , 7 , 2
         IF ( m(l)/=0 .OR. m(l+1)/=0 ) THEN
            IF ( m(l)<=0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            N = N + 2
            I(N-1) = m(l)
            I(N) = m(l+1)
            IF ( E(kl)>=0 ) THEN
               IF ( m(l)>E(kl) ) THEN
                  E(kl) = m(l)
               ELSE
                  E(kl) = -m(l)
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      IF ( N<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      CALL spag_block_5
      RETURN
   END SUBROUTINE spag_block_25
   SUBROUTINE spag_block_26
!
!*****         48-CROD,49-CTUBE,50-CVISC,356-CPSE2    *****************
!
      DO l = 1 , 5 , 4
         IF ( m(l)/=0 .OR. m(l+1)/=0 .OR. m(l+2)/=0 .OR. m(l+3)/=0 ) THEN
            IF ( m(l)<=0 .OR. m(l+2)<=0 .OR. m(l+3)<=0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            IF ( Mf(l+1)==0 ) m(l+1) = m(l)
            IF ( m(l+1)<=0 .OR. m(l+2)==m(l+3) ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            N = N + 4
            I(N-3) = m(l)
            I(N-2) = m(l+1)
            I(N-1) = m(l+2)
            I(N) = m(l+3)
            IF ( E(kl)>=0 ) THEN
               IF ( m(l)>E(kl) ) THEN
                  E(kl) = m(l)
               ELSE
                  E(kl) = -m(l)
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      IF ( N<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      CALL spag_block_5
      RETURN
   END SUBROUTINE spag_block_26
   SUBROUTINE spag_block_27
!
!*****       52-CTRIA1,53-CTRIA2,54-CTRBSC,55-CTRPLT,56-CTRMEM    ****
!            357-CPSE3
!
      IF ( m(3)<=0 .OR. m(4)<=0 .OR. m(5)<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( m(3)==m(4) .OR. m(4)==m(5) .OR. m(3)==m(5) ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      N = 6
      IF ( K==357 ) N = 5
      IF ( Mf(2)==0 ) m(2) = m(1)
      CALL spag_block_22
      RETURN
   END SUBROUTINE spag_block_27
   SUBROUTINE spag_block_28
!
!*****       57-CQUAD1,58-CQUAD2,59-CQDPLT,60-CQDMEM,249-CQDMEM1    ****
!*****       259-CQDMEM2,358-CPSE4
!
      IF ( m(3)<=0 .OR. m(4)<=0 .OR. m(5)<=0 .OR. m(6)<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( m(3)==m(4) .OR. m(4)==m(5) .OR. m(5)==m(6) .OR. m(3)==m(5) .OR. m(4)==m(6) .OR. m(3)==m(6) ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      N = 7
      IF ( K==358 ) N = 6
      IF ( Mf(2)==0 ) m(2) = m(1)
      CALL spag_block_22
      RETURN
   END SUBROUTINE spag_block_28
   SUBROUTINE spag_block_29
      IF ( m(1)<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( m(3)<0 .OR. m(4)<0 .OR. m(5)<0 .OR. m(6)<0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( m(4)>6 .OR. m(6)>6 .OR. m(3)==0 .AND. m(5)==0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( m(3)==0 .AND. m(4)/=0 .OR. m(5)==0 .AND. m(6)/=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( m(3)==m(5) .AND. m(4)==m(6) ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      icell = m(4)
      m(4) = m(5)
      m(5) = icell
      CALL spag_block_4
      RETURN
   END SUBROUTINE spag_block_29
   SUBROUTINE spag_block_30
!
!*****         67-CMASS3,75-CELAS3,71-CDAMP3    ************************
!
      DO l = 1 , 5 , 4
         IF ( m(l)/=0 .OR. m(l+1)/=0 .OR. m(l+2)/=0 .OR. m(l+3)/=0 ) THEN
            IF ( m(l)<=0 .OR. m(l+2)<0 .OR. m(l+3)<0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            IF ( Mf(l+1)==0 ) m(l+1) = m(l)
            IF ( m(l+1)<=0 .OR. m(l+2)==m(l+3) ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            N = N + 4
            I(N-3) = m(l)
            I(N-2) = m(l+1)
            I(N-1) = m(l+2)
            I(N) = m(l+3)
            IF ( E(kl)>=0 ) THEN
               IF ( m(l)>E(kl) ) THEN
                  E(kl) = m(l)
               ELSE
                  E(kl) = -m(l)
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      IF ( N<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      CALL spag_block_5
      RETURN
   END SUBROUTINE spag_block_30
   SUBROUTINE spag_block_31
!
!*****         68-CMASS4,76-CELAS4,72-CDAMP4    ************************
!
      DO l = 1 , 5 , 4
         IF ( m(l)/=0 .OR. m(l+1)/=0 .OR. m(l+2)/=0 .OR. m(l+3)/=0 ) THEN
            IF ( m(l)<=0 .OR. m(l+2)<0 .OR. m(l+3)<0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            IF ( m(l+2)==m(l+3) ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            N = N + 4
            I(N-3) = m(l)
            I(N-2) = m(l+1)
            I(N-1) = m(l+2)
            I(N) = m(l+3)
            IF ( E(kl)>=0 ) THEN
               IF ( m(l)>E(kl) ) THEN
                  E(kl) = m(l)
               ELSE
                  E(kl) = -m(l)
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      IF ( N<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      CALL spag_block_5
      RETURN
   END SUBROUTINE spag_block_31
   SUBROUTINE spag_block_32
!
!*****     234-MAT4,   337-MATF   **************************************
!
      IF ( m(1)<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( Rm(2)<=0.0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( Rm(3)<=0.0 .AND. Mf(3)==2 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      N = 3
      CALL spag_block_4
      RETURN
   END SUBROUTINE spag_block_32
   SUBROUTINE spag_block_33
      IF ( m(1)<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( Mf(6)==0 ) Rm(6) = Slotdf(1)
      IF ( Mf(7)==0 ) Rm(7) = Slotdf(2)
      IF ( Mf(8)==0 ) m(8) = klotdf(3)
      DO l = 2 , N
         IF ( m(l)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         IF ( l/=2 ) THEN
            DO l1 = 3 , l
               IF ( m(l1-1)==m(l) ) THEN
                  CALL spag_block_3
                  RETURN
               ENDIF
            ENDDO
         ENDIF
      ENDDO
!     CHECK FOR RHO .GE. 0.0
!     CHECK FOR B .GE. 0.0
!     CHECK FOR N .GE. 0
      DO l = 6 , 8
         l1 = l + N - 5
         I(l1) = m(l)
      ENDDO
      DO l = 1 , N
         I(l) = m(l)
      ENDDO
      N = N + 3
      CALL spag_block_5
      RETURN
   END SUBROUTINE spag_block_33
   SUBROUTINE spag_block_34
      IF ( Mf(6)==0 ) Rm(6) = Slotdf(1)
      IF ( Mf(7)==0 ) Rm(7) = Slotdf(2)
      IF ( Mf(8)==0 ) m(8) = klotdf(5)
!     CHECK FOR ALL KINDS OF THINGS
      DO l = 6 , 8
         l1 = l + N - 5
         I(l1) = m(l)
      ENDDO
      DO l = 1 , N
         I(l) = m(l)
      ENDDO
      N = N + 4
      I(N) = klotdf(3)
      CALL spag_block_5
      RETURN
   END SUBROUTINE spag_block_34
   SUBROUTINE spag_block_35
      IF ( M1(1)/=0 .OR. M1(2)/=0 ) THEN
         Km = 0
         N = N + 1
         I(N) = -1
         Kn = 0
      ENDIF
      CALL spag_block_6
      RETURN
   END SUBROUTINE spag_block_35
   SUBROUTINE spag_block_36
      iz = l + 1
      DO l = iz , 8
         IF ( Mf(l)/=0 ) Badfor = .TRUE.
      ENDDO
      IF ( M1(1)==0 .AND. M1(2)==0 ) Badfor = .TRUE.
      CALL spag_block_35
      RETURN
   END SUBROUTINE spag_block_36
   SUBROUTINE spag_block_37
      I(3) = l
      l1 = hbdyix(l)
      DO l2 = 1 , l1
         IF ( m(l2+4)<=0 .OR. m(l2+9)<0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         I(l2+3) = m(l2+4)
         I(l2+7) = m(l2+9)
      ENDDO
      IF ( l1/=4 ) THEN
         DO l2 = l1 , 3
            IF ( m(l2+5)/=0 .OR. m(l2+10)/=0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            I(l2+4) = 0
            I(l2+8) = 0
         ENDDO
      ENDIF
      DO l2 = 12 , 14
         I(l2) = m(l2+2)
      ENDDO
      N = 15
      I(15) = m(9)
      CALL spag_block_5
      RETURN
   END SUBROUTINE spag_block_37
   SUBROUTINE spag_block_38
      I(2) = l
      l1 = hbdyix(l)
      DO l2 = 1 , l1
         IF ( m(l2+5)<=0 ) THEN
            CALL spag_block_3
            RETURN
         ENDIF
         I(l2+4) = m(l2+5)
      ENDDO
      IF ( l1/=4 ) THEN
         DO l2 = l1 , 3
            IF ( m(l2+6)/=0 ) THEN
               CALL spag_block_3
               RETURN
            ENDIF
            I(l2+5) = 0
         ENDDO
      ENDIF
      I(3) = m(4)
      IF ( l>=3 .AND. Mf(4)/=0 ) THEN
         CALL spag_block_2
         RETURN
      ENDIF
      IF ( l<3 .AND. Rm(5)<=0.0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      I(4) = m(5)
      N = 8
      CALL spag_block_5
      RETURN
   END SUBROUTINE spag_block_38
   SUBROUTINE spag_block_39
      l = 1
      N = 0
      CALL spag_block_40
   END SUBROUTINE spag_block_39
   SUBROUTINE spag_block_40
      Km = 1
      l4 = l
      IF ( Mf(l)/=1 ) Badfor = .TRUE.
      IF ( m(l4)<=0 ) Baddat = .TRUE.
      CALL spag_block_41
   END SUBROUTINE spag_block_40
   SUBROUTINE spag_block_41
      SPAG_Loop_1_3: DO
         DO WHILE ( l<=8 )
            IF ( Mf(l)==0 ) THEN
               DO l2 = l , 8
                  IF ( Mf(l2)/=0 ) Badfor = .TRUE.
               ENDDO
               IF ( M1(1)/=0 .OR. M1(2)/=0 ) EXIT SPAG_Loop_1_3
               Badfor = .TRUE.
               Kn = 1
               CALL spag_block_6
               RETURN
            ELSEIF ( Mf(l)==3 ) THEN
               IF ( l/=8 ) THEN
                  IF ( Mf(l+1)==1 .AND. m(l4)==thru ) THEN
                     IF ( m(l4-1)<m(l4+2) ) THEN
                        l1 = m(l4-1) + 1
                        l2 = m(l4+2)
                        DO
                           l3 = l1
                           IF ( N>=49 ) THEN
                              CALL write(k914,I,N,0)
                              N = 0
                           ENDIF
                           N = N + 1
                           I(N) = l3
                           l1 = l1 + 1
                           IF ( l1>l2 ) THEN
                              l = l + 2
                              l4 = l4 + 3
                              CYCLE SPAG_Loop_1_3
                           ENDIF
                        ENDDO
                     ENDIF
                  ENDIF
               ENDIF
               Baddat = .TRUE.
               l = l + 1
               l4 = l4 + 2
               CYCLE SPAG_Loop_1_3
            ELSE
               IF ( m(l4)<=0 ) Baddat = .TRUE.
               IF ( N>=49 ) THEN
                  CALL write(k914,I,N,0)
                  N = 0
               ENDIF
               N = N + 1
               I(N) = m(l4)
               l = l + 1
               l4 = l4 + 1
            ENDIF
         ENDDO
         IF ( M1(1)/=0 .OR. M1(2)/=0 ) EXIT SPAG_Loop_1_3
         Kn = 1
         CALL spag_block_6
         RETURN
      ENDDO SPAG_Loop_1_3
      Km = 0
      N = N + 1
      I(N) = -1
      Kn = 0
      CALL spag_block_6
      RETURN
   END SUBROUTINE spag_block_41
   SUBROUTINE spag_block_42
!
!*****       291-CTRIM6         ****************************************
!
      IF ( m(3)<=0 .OR. m(4)<=0 .OR. m(5)<=0 .OR. m(6)<=0 .OR. m(7)<=0 .OR. m(8)<=0 ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( m(3)==m(4) .OR. m(3)==m(5) .OR. m(3)==m(6) .OR. m(3)==m(7) .OR. m(3)==m(8) .OR. m(4)==m(5) .OR. m(4)==m(6) .OR.          &
         & m(4)==m(7) .OR. m(4)==m(8) ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      IF ( m(5)==m(6) .OR. m(5)==m(7) .OR. m(5)==m(8) .OR. m(6)==m(7) .OR. m(6)==m(8) .OR. m(7)==m(8) ) THEN
         CALL spag_block_3
         RETURN
      ENDIF
      DO l = 1 , 8
         IF ( Mf(l)/=1 ) THEN
            CALL spag_block_2
            RETURN
         ENDIF
      ENDDO
      IF ( Mf(9)/=0 .AND. Mf(9)/=2 ) THEN
         CALL spag_block_2
         RETURN
      ENDIF
      IF ( Mf(2)==0 ) m(2) = m(1)
      N = 9
      CALL spag_block_22
      RETURN
   END SUBROUTINE spag_block_42
!
END SUBROUTINE ifs1p
