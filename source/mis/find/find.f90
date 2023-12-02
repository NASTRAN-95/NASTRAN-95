!*==find.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE find(Mode,Buf1,Buf4,Setid,X)
USE C_BLANK
USE C_PLTDAT
USE C_RSTXXX
USE C_SYSTEM
USE C_XMSSG
USE C_XXPARM
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Mode
   INTEGER :: Buf1
   INTEGER :: Buf4
   INTEGER , DIMENSION(1) :: Setid
   INTEGER , DIMENSION(1) :: X
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a , b , diam , fwrd , imsep , ratio
   INTEGER , DIMENSION(2) :: awrd
   REAL(REAL64) :: dwrd
   INTEGER , DIMENSION(3) :: err
   INTEGER , SAVE :: hset , nmsg1 , nmsg3 , nmsg6 , orig , poin , regi , scal , vant
   INTEGER :: i , icrq , iwrd , j , nogo , region , set , tra , word
   REAL , SAVE :: mm17p5 , rdist , sqrt3
   INTEGER , DIMENSION(20) , SAVE :: msg1 , msg6
   INTEGER , DIMENSION(21) , SAVE :: msg3
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL close , fndset , fread , fwdrec , gopen , mesage , perpec , proces , rdmode , rdmodx , rdword , wrtprt
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
   !>>>>EQUIVALENCE (word,awrd(1),iwrd,fwrd,dwrd)
   DATA name/4H  FI , 4HND  /
   DATA mm17p5 , rdist , sqrt3/.688975 , 29. , 1.732051/ , orig/4HORIG/ , regi/4HREGI/ , scal/4HSCAL/ , hset/3HSET/ , vant/4HVANT/ ,&
      & poin/4HPOIN/
   DATA nmsg1 , msg1/20 , 4H(34X , 4H,45H , 4HAN A , 4HTTEM , 4HPT H , 4HAS B , 4HEEN  , 4HMADE , 4H TO  , 4HDEFI , 4HNE M ,        &
      & 4HORE  , 4HTHAN , 4H ,I2 , 4H,17H , 4H DIS , 4HTINC , 4HT OR , 4HIGIN , 4HS)  /
   DATA nmsg3 , msg3/21 , 4H(25X , 4H,27H , 4HAN U , 4HNREC , 4HOGNI , 4HZABL , 4HE RE , 4HQUES , 4HT (, , 4H2A4, , 4H37H) ,        &
      & 4H HAS , 4H BEE , 4HN SP , 4HECIF , 4HIED  , 4HON A , 4H -FI , 4HND-  , 4HCARD , 4H)   /
   DATA nmsg6 , msg6/20 , 4H(33X , 4H,71H , 4HMAXI , 4HMUM  , 4HDEFO , 4HRMAT , 4HION  , 4HCARD , 4H NEE , 4HDED  , 4H- 5  ,        &
      & 4HPER  , 4HCENT , 4H OF  , 4HMAXI , 4HMUM  , 4HDIME , 4HNSIO , 4HN US , 4HED.)/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         CALL rdmodx(Parm,Mode,word)
         set = Setd
         region = 0
         Reg(1) = 0.
         Reg(2) = 0.
         Reg(3) = 1.
         Reg(4) = 1.
         ratio = 0.
         nogo = 0
         IF ( Mode<0 ) GOTO 200
!
!     INTERPRET THE REQUESTS ON THE -FIND- CARD.
!
 20      IF ( Mode<=0 ) CALL rdmode(*20,*40,*200,Mode,word)
 40      CALL rdword(Mode,word)
         spag_nextblock_1 = 2
      CASE (2)
!
!     IS AN ORIGIN TO BE FOUND
!
         IF ( word==orig ) THEN
            IF ( Mode/=0 ) GOTO 20
            ASSIGN 60 TO tra
            CALL rdmode(*160,*20,*200,Mode,word)
            GOTO 160
!
!     IS A REGION SPECIFIED
!
         ELSEIF ( word==regi ) THEN
            IF ( Mode/=0 ) GOTO 20
            region = 1
            ASSIGN 80 TO tra
            j = 0
            j = j + 1
            CALL rdmode(*180,*20,*200,Mode,word)
            GOTO 180
!
!     IS THE SCALE TO BE FOUND
!
         ELSEIF ( word==scal ) THEN
            Fscale = 1
            IF ( Mode/=0 ) GOTO 20
            ASSIGN 100 TO tra
!
!     READ A REAL NUMBER FROM THE FIND CARD
!
            CALL rdmode(*180,*20,*200,Mode,word)
            GOTO 180
!
!     IS THERE A SET ON THE FIND CARD
!
         ELSEIF ( word==hset ) THEN
            IF ( Mode/=0 ) GOTO 20
            ASSIGN 120 TO tra
!
!     READ AN INTEGER FROM THE FIND CARD
!
            CALL rdmode(*160,*20,*200,Mode,word)
            GOTO 160
!
!     IS THE VANTAGE POINT TO BE FOUND
!
         ELSEIF ( word/=vant ) THEN
!
!     UNRECOGNIZABLE OPTION ON THE FIND CARD
!
            IF ( Prnt>=0 ) THEN
               err(1) = 2
               err(2) = awrd(1)
               err(3) = awrd(2)
               CALL wrtprt(Merr,err,msg3,nmsg3)
            ENDIF
            GOTO 20
         ELSE
            IF ( Mode==0 ) CALL rdmode(*20,*140,*200,Mode,word)
            GOTO 140
         ENDIF
 60      IF ( Org/=0 ) THEN
            DO j = 1 , Org
               IF ( Origin(j)==iwrd ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            IF ( Org>=Norg ) THEN
               IF ( Prnt>=0 ) THEN
                  err(1) = 1
                  err(2) = Norg
                  CALL wrtprt(Merr,err,msg1,nmsg1)
               ENDIF
               Org = Norg
               i = Org + 1
               Edge(i,1) = 0.0
               Edge(i,2) = 0.0
               Edge(i,3) = 1.0
               Edge(i,4) = 1.0
            ENDIF
         ENDIF
         Org = Org + 1
         Origin(Org) = iwrd
         j = Org
         spag_nextblock_1 = 3
      CASE (3)
         For = j
         GOTO 20
 80      Reg(j) = amin1(1.,abs(fwrd))
         IF ( j>=4 ) GOTO 20
         j = j + 1
         CALL rdmode(*180,*20,*200,Mode,word)
         GOTO 180
 100     ratio = fwrd
         GOTO 20
 120     DO j = 1 , Nsets
            IF ( iwrd==Setid(j) ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         WRITE (Nout,99001) Uwm , iwrd
99001    FORMAT (A25,' 700, SET',I9,' REQUESTED ON FIND CARD HAS NOT BEEN',' DEFINED. DEFAULT SET',I9,' USED')
         nogo = 1
         GOTO 20
      CASE (4)
         set = j
         GOTO 20
 140     CALL rdword(Mode,word)
         IF ( word/=poin ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Fvp = 1
         GOTO 20
 160     IF ( Mode/=-1 ) THEN
            IF ( Mode==-4 ) THEN
               iwrd = dwrd
            ELSE
               iwrd = fwrd
            ENDIF
         ENDIF
         GOTO tra
 180     IF ( Mode==-4 ) THEN
            fwrd = dwrd
         ELSEIF ( Mode==-1 ) THEN
            fwrd = iwrd
         ENDIF
         GOTO tra
!
!     END OF THE FIND CARD
!
 200     IF ( Org<=0 ) THEN
!
!     ALLOW NO ORIGIN REQUEST ON FIRST FIND CARD
!     ORIGIN ID IS ZERO
!
            Org = 1
            Origin(1) = 0
            region = 1
         ENDIF
         IF ( For/=0 ) THEN
            IF ( region==0 ) THEN
               Reg(1) = Edge(For,1)
               Reg(2) = Edge(For,2)
               Reg(3) = Edge(For,3)
               Reg(4) = Edge(For,4)
            ELSE
               Edge(For,1) = Reg(1)
               Edge(For,2) = Reg(2)
               Edge(For,3) = Reg(3)
               Edge(For,4) = Reg(4)
            ENDIF
         ENDIF
         Reg(1) = Reg(1)*Axymax(1)
         IF ( Reg(2)/=0. ) THEN
            Reg(2) = Reg(2)*Axymax(2)
         ELSE
            Reg(2) = 4.*Cntchr(2)
         ENDIF
         Reg(3) = Reg(3)*Axymax(1) - Cntchr(1)*8.
         Reg(4) = Reg(4)*Axymax(2) - Cntchr(2)
!
!     CALCULATE THE ROTATION MATRIX + ROTATE THE CO-ORDINATES OF THE SET
!
         CALL gopen(Gpset,X(Buf4),0)
         i = 1
         CALL fwdrec(*220,Gpset)
         IF ( set/=1 ) THEN
            DO i = 2 , set
               CALL fwdrec(*220,Gpset)
            ENDDO
         ENDIF
!
!     READ NGPSET
!
         CALL fread(Gpset,Ngpset,1,0)
!
!     CHECK CORE
!
         icrq = 3*Ngpset + Ngp - Buf4 - Bufsiz - 1
         IF ( icrq>0 ) THEN
!
            CALL mesage(-8,icrq,name)
         ELSE
            CALL fread(Gpset,X,Ngp,0)
            CALL close(Gpset,1)
            CALL fndset(X,X(Ngp+1),Buf1,0)
            DO i = 1 , 3
               Min(i) = +1.E+20
               Max(i) = -1.E+20
            ENDDO
            CALL proces(X(Ngp+1))
            IF ( Maxdef==0.0 .AND. Prnt<0 ) THEN
!
!     DEFORMED PLOTS AND MAXDEF WAS NOT SPECIFIED
!
               err(1) = 0
               CALL wrtprt(Merr,err,msg6,nmsg6)
               Maxdef = amax1(D(2),D(3))
               IF ( Maxdef<=0.0 ) Maxdef = 1.0
               Maxdef = 0.05*Maxdef
            ENDIF
            IF ( Prject==1 ) THEN
            ELSEIF ( Prject==3 ) THEN
!
!     STEREO PROJECTION
!
!     FIND SCALE FACTORS (IF REQUESTED).
!
               IF ( Fscale/=0 ) THEN
                  diam = sqrt(D(1)**2+D(2)**2+D(3)**2)
                  a = sqrt3*Maxdef
                  IF ( D(2)+a>=diam .OR. D(3)+a>=diam ) diam = diam + Maxdef
                  IF ( diam==0.0 ) diam = 1.E-5
                  Objmod = 10./diam
                  Scale = amin1(Reg(3)-Reg(1),Reg(4)-Reg(2))/mm17p5
                  IF ( ratio/=0. ) Scale = ratio*Scale
               ENDIF
!
!     FIND VANTAGE POINT (IF REQUESTED)
!
               CALL perpec(X(Ngp+1),0)
               Fvp = 0
!
!     FIND ORIGIN -FOR- IF REQUESTED
!
               IF ( For/=0 ) THEN
                  imsep = S0s*(rdist-D0)/(2.*rdist)
                  Xy(For,1) = Scale*(Aver(2)*Objmod-imsep) - (Reg(1)+Reg(3))/2.
                  Xy(For,2) = Scale*(Aver(2)*Objmod+imsep) - (Reg(1)+Reg(3))/2.
                  Xy(For,3) = Scale*(Aver(3)*Objmod) - (Reg(2)+Reg(4))/2.
               ENDIF
               GOTO 210
            ELSE
!
!     PERSPECTIVE PROJECTION (FIND VANTAGE POINT IF REQUESTED)
!
               DO i = 1 , 3
                  Min(i) = +1.E+20
                  Max(i) = -1.E+20
               ENDDO
               CALL perpec(X(Ngp+1),0)
               Fvp = 0
            ENDIF
!
!     ORTHOGRAPHIC OR PERSPECTIVE PROJECTION
!
!     FIND SCALE FACTOR (IF REQUESTED).
!
            IF ( Fscale/=0 ) THEN
               a = D(2) + 2.*Maxdef*sqrt3
               IF ( a/=0.0 ) a = (Reg(3)-Reg(1))/a
               b = D(3) + 2.*Maxdef*sqrt3
               IF ( b/=0.0 ) b = (Reg(4)-Reg(2))/b
               Scale = amin1(a,b)
               IF ( Scale<=0. ) Scale = amax1(a,b)
               IF ( Scale<=0. ) Scale = 1.
               IF ( ratio/=0. ) Scale = ratio*Scale
            ENDIF
!
!     FIND ORIGIN -FOR- IF REQUESTED
!
            IF ( For/=0 ) THEN
               Xy(For,1) = Aver(2)*Scale - (Reg(1)+Reg(3))/2.
               Xy(For,3) = Aver(3)*Scale - (Reg(2)+Reg(4))/2.
            ENDIF
!
 210        Fscale = 0
            For = 0
            IF ( nogo/=0 ) CALL mesage(-37,0,name)
            RETURN
         ENDIF
!
 220     WRITE (Nout,99002) Ufm , Setid(set)
99002    FORMAT (A23,' 703, SET',I9,' REQUESTED ON FIND CARD NOT IN ','GPSETS FILE.')
         nogo = 1
         CALL close(Gpset,1)
         IF ( nogo/=0 ) CALL mesage(-37,0,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE find
