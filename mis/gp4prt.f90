
SUBROUTINE gp4prt(Ibuf)
   IMPLICIT NONE
   INTEGER Iautsp , Idsub , Junk(6) , Line , Luset , Mpcf1 , Mpcf2 , Mtemp , Nlpp , Noa , Nol , Nosets , Nout , Npage , Nskip ,     &
         & Sysbuf , Two(32) , Ua , Uf , Ug , Ul , Um , Un , Uo , Ur , Us , Usb , Usg , Z(1)
   REAL Omit1 , React , Repeat , Single
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /bitpos/ Um , Uo , Ur , Usg , Usb , Ul , Ua , Uf , Us , Un , Ug
   COMMON /blank / Luset , Mpcf1 , Mpcf2 , Single , Omit1 , React , Nskip , Repeat , Nosets , Nol , Noa , Idsub , Iautsp
   COMMON /system/ Sysbuf , Nout , Junk , Nlpp , Mtemp , Npage , Line
   COMMON /two   / Two
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /zzzzzz/ Z
   INTEGER Ibuf
   INTEGER andf , orf
   INTEGER blank , buf , d21 , d22 , dash , eqexin , erec1 , exflag , extype , file , i , iafrmt(2) , ibegn , id1 , idof , iend ,   &
         & iflag , iflg(8) , ifrmat(32) , iifrmt(2) , imk , index , inos , ip , ipas , iprint , irem , itm , iu , j , k , kk , kl , &
         & kn , ku , l , msk(12) , nam204(2) , name(3) , sbit(12) , scr1 , tdb204 , title(2,8) , trl(7) , upbit(12) , zcom(10) ,    &
         & zdum(10)
   EXTERNAL andf , orf
!
!     1. PRINTS  DOF VS. DISP. SETS IF DIAG 21 ON.
!     2. PRINTS  DISP. SETS VS. DOF IF DIAG 22 ON.
!     3. CREATES SUBSTRUCTURE COUPLING DATA TABLE.
!
!     IF IBUF IS .LT. 0, SOME FILES MAY NOT BE CLOSED PROPERLY WHEN
!     THIS ROUTINE IS CALLED
!
   DATA scr1/301/ , eqexin/103/ , tdb204/204/
   DATA name/4HGP4P , 4HRT   , 4H    /
   DATA ibegn , iend/4HBEGN , 4HEND /
   DATA title/4H     , 4H MPC , 4H     , 4H SPC , 4H     , 4HOMIT , 4HANAL , 4HYSIS , 4H  SU , 4HPORT , 4HPERM , 4H SPC , 4HBDRY ,  &
       &4H SPC , 4HAUTO , 4H SPC/
   DATA blank/1H /
   DATA dash/1H-/
   DATA ifrmat/4H(13X , 4H,I6, , 4H3X,I , 4H8,1X , 4H,A1, , 4HI2,1 , 4HX    , 4H,1X, , 4H  I6 , 4H,1X, , 4H  I6 , 4H,1X, , 4H  I6 , &
       &4H,1X, , 4H  I6 , 4H,1X, , 4H  I6 , 4H,1X, , 4H  I6 , 4H,1X, , 4H  I6 , 4H,1X, , 4H  I6 , 4H,1X, , 4H  I6 , 4H,1X, ,        &
      & 4H  I6 , 4H,1X, , 4H  I6 , 4H,1X, , 4H  I6 , 4H)   /
   DATA iifrmt/4H,1X, , 4H  I6/
   DATA iafrmt/4H,3X, , 4H  A4/
   DATA iprint/0/
!
!
   CALL sswtch(21,d21)
   CALL sswtch(22,d22)
   IF ( d21/=1 .AND. d22/=1 .AND. Idsub<=0 ) RETURN
   IF ( iprint==1 ) RETURN
   iprint = 1
   name(3) = ibegn
   CALL conmsg(name,3,0)
   buf = iabs(Ibuf)
   file = eqexin
   IF ( Ibuf<0 ) CALL close(eqexin,1)
   CALL open(*200,eqexin,Z(buf),0)
   CALL fwdrec(*300,eqexin)
   CALL fwdrec(*300,eqexin)
   erec1 = Luset + 1
   CALL read(*300,*100,eqexin,Z(erec1),buf-erec1,1,kn)
   GOTO 500
 100  CALL close(eqexin,1)
   CALL sort(0,0,2,2,Z(erec1),kn)
!
   IF ( d21==1 ) THEN
      ku = 1
      msk(ku+1) = Two(Usb)
      msk(ku+2) = Two(Usg)
      msk(ku+3) = Two(Ul)
      msk(ku+4) = Two(Ua)
      msk(ku+5) = Two(Uf)
      msk(ku+6) = Two(Un)
      msk(ku+7) = Two(Ug)
      msk(ku+8) = Two(Ur)
      msk(ku+9) = Two(Uo)
      msk(ku+10) = Two(Us)
      msk(ku+11) = Two(Um)
      DO ku = 1 , 12
         sbit(ku) = 0
      ENDDO
      CALL page1
      Line = Line + 2
      WRITE (Nout,99001) Uim
!
99001 FORMAT (A29,' 2118, SUBROUTINE GP4PRT - DIAG 21 SET-DOF VS. DISP',' SETS FOLLOWS.')
      Line = Line + 4
      WRITE (Nout,99005)
      i = erec1
      kl = 0
      DO k = 1 , kn , 2
         itm = Z(k+i)/10
         itm = Z(k+i) - 10*itm
         l = 6
         IF ( itm==2 ) l = 1
         DO kk = 1 , l
            kl = kl + 1
            iu = Z(kl)
            ip = Z(i+k-1)
            idof = kk
            IF ( andf(msk(11),iu)/=0 ) THEN
               IF ( andf(msk(2),iu)==0 .AND. andf(msk(3),iu)==0 ) THEN
                  sbit(1) = sbit(1) + 1
                  upbit(1) = sbit(1)
                  ifrmat(8) = iifrmt(1)
                  ifrmat(9) = iifrmt(2)
                  GOTO 110
               ENDIF
            ENDIF
            upbit(1) = blank
            ifrmat(8) = iafrmt(1)
            ifrmat(9) = iafrmt(2)
 110        DO ku = 2 , 12
               index = 2*(ku-1) + 8
               IF ( andf(msk(ku),iu)==msk(ku) ) THEN
                  sbit(ku) = sbit(ku) + 1
                  upbit(ku) = sbit(ku)
                  ifrmat(index) = iifrmt(1)
                  ifrmat(index+1) = iifrmt(2)
               ELSE
                  upbit(ku) = blank
                  ifrmat(index) = iafrmt(1)
                  ifrmat(index+1) = iafrmt(2)
               ENDIF
            ENDDO
            IF ( l==1 ) idof = 0
            Line = Line + 1
            IF ( Line>Nlpp ) THEN
               CALL page1
               WRITE (Nout,99005)
               Line = Line + 5
            ENDIF
            WRITE (Nout,ifrmat) kl , ip , dash , idof , upbit
         ENDDO
      ENDDO
      WRITE (Nout,99002) sbit
99002 FORMAT (1H0,34H--- C O L U M N   T O T A L S --- ,12I7)
      Line = Line + 2
   ENDIF
!
   IF ( d22/=1 .AND. Idsub<=0 ) RETURN
   msk(1) = Two(Um)
   msk(2) = Two(Us)
   msk(3) = Two(Uo)
   msk(4) = Two(Ua)
   msk(5) = Two(Ur)
   msk(6) = Two(Usg)
   msk(7) = Two(Usb)
   exflag = 0
   extype = 0
   IF ( d22==1 ) THEN
      CALL page1
      Line = Line + 2
      WRITE (Nout,99003) Uim
99003 FORMAT (A29,' 2119, SUBROUTINE GP4PRT - DIAG 22 SET DISP SETS VS','. DOF FOLLOWS')
      Line = Line + 4
   ENDIF
   file = scr1
   IF ( Ibuf<0 ) CALL close(scr1,1)
   CALL open(*200,scr1,Z(buf),1)
   DO imk = 1 , 8
      iflg(imk) = 0
      i = erec1
      ip = 0
      kl = 0
      DO k = 1 , kn , 2
         itm = Z(k+i)/10
         itm = Z(k+i) - 10*itm
         l = 6
         IF ( itm==2 ) l = 1
         DO kk = 1 , l
            kl = kl + 1
            iu = Z(kl)
            IF ( Z(i+k-1)<ip ) exflag = 1
            ip = Z(i+k-1)
            IF ( l==1 ) THEN
               idof = 0
               extype = orf(extype,1)
            ELSE
               idof = kk
               extype = orf(extype,2)
            ENDIF
            IF ( imk==8 ) THEN
               IF ( andf(iu,msk(2))==0 ) CYCLE
               IF ( andf(iu,msk(6))/=0 .OR. andf(iu,msk(7))/=0 ) CYCLE
            ELSEIF ( andf(iu,msk(imk))/=msk(imk) ) THEN
               CYCLE
            ENDIF
            CALL write(scr1,10*ip+idof,1,0)
            iflg(imk) = 1
         ENDDO
      ENDDO
      IF ( iflg(imk)==1 ) CALL write(scr1,0,0,1)
   ENDDO
   CALL write(scr1,Z(1),Luset,1)
   CALL close(scr1,1)
   CALL open(*200,scr1,Z(buf),0)
   iflag = 0
   DO i = 1 , 8
      IF ( iflg(i)/=1 ) CYCLE
      iflag = iflag + 1
      CALL read(*300,*150,scr1,Z(1),buf,1,kn)
      CALL page2(-4)
      WRITE (Nout,99008) file
      EXIT
 150  IF ( Idsub>0 .AND. i==4 ) THEN
         CALL close(scr1,2)
         file = tdb204
         CALL open(*200,tdb204,Z(buf),1)
         CALL fname(tdb204,nam204)
         CALL write(tdb204,nam204,2,1)
         CALL write(tdb204,Z(1),kn,1)
         CALL close(tdb204,1)
         trl(1) = tdb204
         trl(2) = 0
         trl(3) = kn
         trl(4) = 0
         trl(5) = Idsub
         trl(6) = exflag
         trl(7) = extype
         CALL wrttrl(trl)
         CALL open(*200,scr1,Z(buf),2)
      ENDIF
      IF ( d22==1 ) THEN
         ipas = kn/10
         irem = kn - 10*ipas
         IF ( iflag>1 ) Line = Nlpp
         id1 = -9
         inos = 0
         IF ( ipas>=1 ) THEN
            DO k = 1 , ipas
               DO j = 1 , 10
                  inos = inos + 1
                  zdum(j) = Z(inos)/10
                  zcom(j) = Z(inos) - 10*zdum(j)
               ENDDO
               Line = Line + 1
               IF ( iflag/=1 .OR. k/=1 ) THEN
                  IF ( Line<=Nlpp ) GOTO 155
                  CALL page1
               ENDIF
               WRITE (Nout,99006) title(1,i) , title(2,i)
               Line = Line + 5
 155           id1 = id1 + 10
               WRITE (Nout,99007) id1 , (zdum(kk),zcom(kk),kk=1,10)
            ENDDO
         ENDIF
         IF ( irem==0 ) CYCLE
         DO j = 1 , irem
            inos = inos + 1
            zdum(j) = Z(inos)/10
            zcom(j) = Z(inos) - zdum(j)*10
         ENDDO
         Line = Line + 1
         IF ( iflag/=1 .OR. ipas/=0 ) THEN
            IF ( Line<=Nlpp ) GOTO 160
            CALL page1
         ENDIF
         WRITE (Nout,99006) title(1,i) , title(2,i)
         Line = Line + 5
 160     id1 = id1 + 10
         WRITE (Nout,99007) id1 , (zdum(kk),zcom(kk),kk=1,irem)
      ENDIF
   ENDDO
!
!     RE-ESTABLISH USET IN OPEN CORE.
!
   CALL read(*300,*500,scr1,Z(1),Luset,1,kn)
   CALL close(scr1,1)
   name(3) = iend
   CALL conmsg(name,3,0)
!
!     TERMINATE RUN IF DIAG 21 OR 22, AND DIAG 20 ARE REQUESTED BY UESER
!     SIMLUTANEOUSLY
!
   CALL sswtch(20,j)
   IF ( j==0 .OR. d21+d22==0 ) RETURN
   WRITE (Nout,99004)
99004 FORMAT (10X,25HJOB TERMINATED BY DIAG 20)
   CALL pexit
!
 200  j = -1
   GOTO 400
 300  j = -2
 400  CALL mesage(j,file,name)
   RETURN
!
!     ERRORS
!
 500  CALL page2(-4)
   WRITE (Nout,99008) Uwm , file
!
   CALL close(file,1)
99005 FORMAT (1H0,14X,5H(SIL),/14X,48HINT DOF   EXT GP. DOF  SAUTO     SB     SG      ,                                             &
             &49HL      A      F      N      G      R      O      ,8HS      M,/1H ,131(1H-))
99006 FORMAT (1H0,52X,2A4,17H DISPLACEMENT SET,/1H0,15X,3H-1-,8X,3H-2-,8X,3H-3-,8X,3H-4-,8X,3H-5-,8X,3H-6-,8X,3H-7-,8X,3H-8-,8X,    &
             &3H-9-,7X,4H-10-,/1H )
99007 FORMAT (1H ,I6,1H=,10(1X,I8,1H-,I1))
99008 FORMAT (A25,' 2110, INSUFFICIENT CORE TO HOLD CONTENTS OF GINO ','FILE',I4,//5X,                                              &
             &'FURTHER PROCESSING OF THIS DATA BLOCK IS ABANDONED.')
!
END SUBROUTINE gp4prt
