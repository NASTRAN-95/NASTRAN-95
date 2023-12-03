!*==forwrt.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE forwrt(Form,Indata,Nwds)
   USE c_system
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nwds
   CHARACTER(1) , DIMENSION(1000) :: Form
   INTEGER(INT32) , DIMENSION(Nwds) :: Indata
!
! Local variable declarations rewritten by SPAG
!
   CHARACTER(1) , SAVE :: a , blank , comma , d , e , f , g , h , i , lparen , p , period , rparen , slash , x
   CHARACTER(4) , DIMENSION(200) :: cdata
   REAL(REAL32) , DIMENSION(200) :: data
   REAL(REAL64) , DIMENSION(100) :: ddata
   INTEGER :: i1 , ichar , icol , icsave , icycle , idec , ifield , ileft , iloop , imult , itype , kb , kk , last , lchar ,        &
            & length , ncnt , nend
   INTEGER(INT32) , DIMENSION(200) :: idata
   CHARACTER(132) :: line , tform
   CHARACTER(1) , DIMENSION(10) , SAVE :: number
   CHARACTER(2) :: pfact
   EXTERNAL fornum , pexit
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!********************************************************************
!    EXPECTED TYPES OF FORMAT CODES ARE AS FOLLOWS
!        NH------       NENN.N       NDNN.N         NX
!        NFNN.N         NINN         NGNN.N         NAN
!        NPENN.N        NPFNN.N      NPN(----)      NP,ENN.N
!        NP,FNN.N       NP,N(----)
!        SPECIAL CHARACTERS:  /(),
!     ICHAR = CURRENT CHARACTER NUMBER BEING PROCESSED IN "FORM"
!     ICOL  = CURRENT CHARACTER COLUMN POSITION WITHIN THE LINE
!     NCNT  = NUMBER OF VALUES OF IDATA AND DATA THAT HAVE BEEN PROCESSE
!********************************************************************
   !>>>>EQUIVALENCE (idata,data,ddata,cdata)
   DATA h/'H'/ , e/'E'/ , d/'D'/ , x/'X'/ , f/'F'/
   DATA i/'I'/ , g/'G'/ , a/'A'/ , p/'P'/
   DATA lparen/'('/ , rparen/')'/ , period/'.'/
   DATA comma/','/ , slash/'/'/ , blank/' '/
   DATA number/'0' , '1' , '2' , '3' , '4' , '5' , '6' , '7' , '8' , '9'/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
         IF ( Nwds>200 ) THEN
            PRINT * , ' LIMIT OF WORDS REACHED IN FORWRT, LIMIT=200'
            CALL pexit
         ENDIF
         DO kb = 1 , Nwds
            idata(kb) = Indata(kb)
         ENDDO
         iloop = 0
         ichar = 1
         ncnt = 1
         icol = 1
         line = blank
         pfact = blank
         icycle = 0
         DO WHILE ( Form(ichar)/=lparen )
            ichar = ichar + 1
            IF ( ichar>1000 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         ichar = ichar + 1
         SPAG_Loop_1_1: DO WHILE ( ichar<=1000 )
            IF ( Form(ichar)==blank ) THEN
               ichar = ichar + 1
            ELSEIF ( Form(ichar)==slash ) THEN
! PROCESS SLASH
               IF ( line/=blank ) WRITE (iwr,99007) line
               IF ( line==blank ) WRITE (iwr,99001)
99001          FORMAT (/)
               line = blank
               IF ( icycle==0 ) pfact = blank
               icol = 1
               ichar = ichar + 1
            ELSEIF ( Form(ichar)>=number(1) .AND. Form(ichar)<=number(10) ) THEN
! GET MULTIPLIER FOR FIELD CONVERSION
               CALL fornum(Form,ichar,imult)
            ELSE
               IF ( Form(ichar)==a ) THEN
! PROCESS ALPHA FIELD--FORMAT(NNANNN) (NN=IMULT,NNN=IFIELD)
                  ichar = ichar + 1
                  IF ( ncnt<=Nwds ) THEN
                     CALL fornum(Form,ichar,ifield)
                     ileft = Nwds - ncnt + 1
                     IF ( ileft<imult ) imult = ileft
                     IF ( imult==0 ) imult = 1
                     WRITE (tform,99002) imult , ifield
99002                FORMAT ('(',I2,'A',I2,')')
                     i1 = icol
                     length = imult*ifield
                     nend = ncnt + imult - 1
                     last = icol + length - 1
                     WRITE (line(icol:last),tform) (cdata(kk),kk=ncnt,nend)
                     icol = icol + length
                     ncnt = ncnt + imult
                     imult = 1
                     CYCLE
                  ENDIF
               ELSEIF ( Form(ichar)==i ) THEN
! PROCESS INTEGER FIELD -- FORMAT(NNINNN) (NN=IMULT,NNN=IFIELD)
                  ichar = ichar + 1
                  IF ( ncnt<=Nwds ) THEN
                     CALL fornum(Form,ichar,ifield)
                     IF ( imult==0 ) imult = 1
                     WRITE (tform,99003) imult , ifield
99003                FORMAT ('(',I2,'I',I2,')')
                     i1 = icol
                     length = imult*ifield
                     nend = ncnt + imult - 1
                     last = icol + length - 1
                     WRITE (line(icol:last),tform) (idata(kk),kk=ncnt,nend)
                     icol = icol + length
                     ncnt = ncnt + imult
                     imult = 1
                     CYCLE
                  ENDIF
               ELSEIF ( Form(ichar)==h ) THEN
! PROCESS HOLERITH FIELD -- FORMAT(NNH----) (NN=IMULT)
                  last = icol + imult - 1
                  ichar = ichar + 1
                  lchar = ichar + imult - 1
                  WRITE (line(icol:last),99008) (Form(kk),kk=ichar,lchar)
                  icol = icol + imult
                  ichar = lchar
                  imult = 1
                  ichar = ichar + 1
                  CYCLE
               ELSEIF ( Form(ichar)==x ) THEN
! PROCESS X FIELD -- FORMAT(NNX) (NN=IMULT)
                  WRITE (tform,99004) imult
99004             FORMAT ('(',I2,'X',')')
                  last = icol + imult - 1
                  WRITE (line(icol:last),tform)
                  icol = icol + imult
                  imult = 1
                  ichar = ichar + 1
                  CYCLE
               ELSEIF ( Form(ichar)==p ) THEN
! PROCESS P FACTOR FOR FLOATING FORMAT
                  WRITE (pfact,99008) Form(ichar-1) , Form(ichar)
                  IF ( ncnt<=Nwds ) THEN
                     DO WHILE ( Form(ichar+1)==blank .OR. Form(ichar+1)==comma )
                        ichar = ichar + 1
                        IF ( ichar>1000 ) EXIT SPAG_Loop_1_1
                     ENDDO
                     ichar = ichar + 1
                     CYCLE
                  ENDIF
               ELSE
                  IF ( Form(ichar)/=f ) THEN
                     IF ( Form(ichar)/=g ) THEN
                        IF ( Form(ichar)/=d ) THEN
                           IF ( Form(ichar)/=e ) THEN
                              IF ( Form(ichar)==lparen ) THEN
! PROCESS LEFT PAREN (NOT THE FIRST LEFT PAREN BUT ONE FOR A GROUP)
! IMULT HAS THE MULTIPLIER TO BE APPLIED TO THE GROUP
                                 icycle = imult - 1
                                 icsave = ichar + 1
                                 iloop = 1
                                 imult = 1
                                 ichar = ichar + 1
                                 CYCLE
                              ELSEIF ( Form(ichar)==rparen ) THEN
! PROCESS RIGHT PAREN ( CHECK IF IT IS THE LAST OF THE FORMAT)
! IF IT IS PART OF A GROUP, THEN ICYCLE WILL BE NON-ZERO
                                 IF ( icycle>0 ) THEN
! GROUP BEING PROCESSED, DECREMENT COUNT AND RESET ICHAR TO BEGINNING
! OF THE GROUP
                                    icycle = icycle - 1
                                    ichar = icsave
                                    CYCLE
                                 ELSEIF ( iloop/=0 ) THEN
! FINISHED WITH LOOP, CONTINUE WITH FORMAT
                                    iloop = 0
                                    icycle = 0
                                    ichar = ichar + 1
                                    CYCLE
                                 ELSE
                                    IF ( ncnt>Nwds ) GOTO 5
! NO GROUP, THEREFORE MUST RE CYCLE THROUGH FORMAT
! UNTIL LIST IS SATISFIED
                                    WRITE (iwr,99007) line
                                    ichar = 2
                                    line = blank
                                    pfact = blank
                                    icol = 1
                                    CYCLE
                                 ENDIF
                              ELSE
                                 IF ( Form(ichar)/=comma ) EXIT SPAG_Loop_1_1
                                 IF ( icycle==0 ) pfact = blank
                                 ichar = ichar + 1
                                 CYCLE
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
! PROCESS FLOATING FIELD -- FORMAT(NPNNXNNN.NNNN)  WHERE
!          (NP = PFACT, NN=IMULT, NNN=IFIELD, NNNN=IDEC)
                  itype = ichar
                  IF ( ncnt<=Nwds ) THEN
                     ichar = ichar + 1
                     CALL fornum(Form,ichar,ifield)
                     DO WHILE ( Form(ichar)/=period )
                        ichar = ichar + 1
                     ENDDO
                     ichar = ichar + 1
                     CALL fornum(Form,ichar,idec)
                     IF ( imult==0 ) imult = 1
                     WRITE (tform,99005) pfact , imult , Form(itype) , ifield , idec
99005                FORMAT ('(',A2,I2,A1,I2,'.',I2,')')
                     i1 = icol
                     length = imult*ifield
                     nend = ncnt + imult - 1
                     last = icol + length - 1
                     IF ( Form(itype)==d ) WRITE (line(icol:last),tform) (ddata(kk),kk=ncnt,nend)
                     IF ( Form(itype)/=d ) WRITE (line(icol:last),tform) (data(kk),kk=ncnt,nend)
                     icol = icol + length
                     ncnt = ncnt + imult
                     imult = 1
                     CYCLE
                  ENDIF
               ENDIF
 5             WRITE (iwr,99007) line
               RETURN
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 2
      CASE (2)
         WRITE (iwr,99006) ichar , Form
99006    FORMAT (///' SUBROUTINE FORWRT UNABLE TO DECIPHER THE FOLLOWING',' FORMAT AT CHARACTER ',I4,/,                             &
                &' FORMAT GIVEN WAS THE FOLLOWING:',/,(1X,131A1))
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99007 FORMAT (A132)
99008 FORMAT (133A1)
END SUBROUTINE forwrt