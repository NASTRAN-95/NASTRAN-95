
SUBROUTINE contor(Gplst,X,U,Dd,Z,Iz,Ppen,Deform,B1,Opcor)
   IMPLICIT NONE
   INTEGER Bufsiz , Color , Est , Icntvl , Iset , Jset , Layer , Ncntr , Newoes , Nout , Parm , Pedge , Scr1 , Sort , Stress
   REAL Cntr(50) , Sk18(18) , Skip(10) , Skip2(157) , Skip3(2) , Skip6(5) , Skip7(14) , Skp4 , Skp8(11) , Xmin
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   CHARACTER*25 Uwm
   COMMON /blank / Skip , Parm , Skp4 , Est , Skp8 , Stress , Sort , Newoes , Scr1
   COMMON /drwdat/ Jset , Skip7 , Pedge
   COMMON /pltdat/ Skip3 , Xmin
   COMMON /system/ Bufsiz , Nout
   COMMON /xmssg / Ufm , Uwm , Uim
   COMMON /xxparm/ Skip2 , Ncntr , Cntr , Icntvl , Skip6 , Iset , Sk18 , Color , Layer
   INTEGER B1 , Deform , Opcor , Ppen
   REAL Dd(3,1) , U(2,1) , X(3,1) , Z(1)
   INTEGER Gplst(1) , Iz(1)
   INTEGER b2 , b3 , elid , elmid , err , esym , gpts(12) , i , ib , ibegin(2) , ic , icen , icolor , id , id1 , iflag , ig , ig1 , &
         & igdpt , ii , ik , ik1 , ik2 , ik3 , ik4 , imhere , iread , irr , irtn , is , isav , iv , ival , j , jc , jrr , js , jv , &
         & k , kbar , kq4 , kt3 , l , labl(50) , lines , lopcor , lopcox , m , nel , ngppe , offset , pen , sub(2)
   REAL center , cenval , conmax , conmin , d , delta , essym , pt(8) , rcntrl , rcolor , s , sum1 , sum2 , v , xb(8) , xmid , ymid
!
   EQUIVALENCE (ibegin(1),lines) , (ibegin(2),igdpt)
   DATA kbar , kt3 , kq4/2HBR , 2HT3 , 2HQ4/
   DATA sub/4HCONT , 4HOR  /
!
   b2 = B1 - 2*Bufsiz
   b3 = b2 - Bufsiz
   lopcor = Opcor/5
   id = 1
   err = 0
   irr = 0
   Ncntr = iabs(Ncntr)
!
!     COLOR = 0 IS NO COLOR CONTOUR,
!     COLOR = 1 TO  31 IS DRAW CONTOUR LINES IN COLOR.
!     COLOR =-1 TO -31 ID COLOR FILL ELEMENTS BY STRESS
!
!     THIS IS THE CODE FOR THE COLOR BAR SCALE
!     AT THE TOP OF THE PLOT
!
   IF ( Color/=0 ) THEN
      CALL line(0.0,0.0,0.0,0.0,32,-1)
      icolor = iabs(Color)
      rcolor = 535.0/icolor
!              535.0 IS BASED ON 1000X1000 PLOT SCREEN COORDINATE FRAME
      DO ib = 1 , icolor
         pen = ib + 31
         xb(1) = 368.14 + rcolor*(ib-1)
         xb(2) = 959.67
         xb(3) = xb(1) + rcolor
         xb(4) = xb(2)
         xb(5) = xb(3)
         xb(6) = 977.51
         xb(7) = xb(1)
         xb(8) = xb(6)
         ik = 0
         ik1 = 1
         ik2 = 2
         ik3 = 3
         ik4 = 4
         DO ii = 1 , 4
            ik1 = ik1 + ik
            ik2 = ik2 + ik
            ik3 = ik3 + ik
            ik4 = ik4 + ik
            ik = 2
            IF ( ii==4 ) pen = 0
            IF ( ii==4 ) THEN
               ik1 = 7
               ik2 = 8
               ik3 = 1
               ik4 = 2
            ENDIF
            CALL line(xb(ik1),xb(ik2),xb(ik3),xb(ik4),pen,0)
         ENDDO
      ENDDO
      CALL line(0.0,0.0,0.0,0.0,pen,+1)
   ENDIF
   isav = lopcor + id
   ival = lopcor + isav
   icen = lopcor + ival
   lopcor = lopcor - 1
   pen = Ppen
   IF ( Icntvl<=9 .OR. Icntvl>=14 .OR. Pedge==1 ) THEN
      CALL close(Parm,2)
      IF ( Icntvl<=9 .OR. Icntvl>13 ) CALL create(Gplst,X,U,Deform,conmin,conmax,Z(ival),Z(icen),lopcor,B1,b2)
      IF ( Iset/=Jset ) CALL order(Gplst,Z(ival),Z(isav),Z(icen),Z(id),lopcor,B1,b2,b3)
      Iset = Jset
   ENDIF
   IF ( Icntvl>9 .AND. Icntvl<14 ) CALL displa(Gplst,X,U,Dd,pen,Deform,labl,pt,B1)
   IF ( Icntvl>9 .AND. Icntvl<14 ) GOTO 1300
   IF ( conmin==conmax ) GOTO 1600
   IF ( Color<0 ) CALL close(Est,2)
   CALL gopen(Scr1,Gplst(B1),1)
   CALL close(Scr1,2)
   CALL gopen(Sort,Gplst(b2),2)
   CALL gopen(Stress,Gplst(B1),0)
!
!     BUFFERS ASSIGNEMENT HERE -
!     B1 IS USED BY STRESS (SCRATCH1/301), AND BY SCR1 (SCRATCH4/304)
!     FOR SHORT PERIODS OF TIME ONLY
!     B2 IS USED BY SORT (SCRATCH2/302)
!     B3 IS USED BY EST (ELEST/103) AND BY SCR1 (SCRATCH4/304)
!
   Ncntr = min0(Ncntr,50)
   IF ( Cntr(1)==Cntr(2) ) THEN
!
!     IF INTERVALS SPECIFIED, DEFINE CONTOUR VALUES
!
      delta = (conmax-conmin)/float(Ncntr-1)
      Cntr(1) = conmin
      j = Ncntr - 1
      DO i = 2 , j
         Cntr(i) = Cntr(i-1) + delta
      ENDDO
      Cntr(Ncntr) = conmax
   ENDIF
   CALL line(0.,0.,0.,0.,pen,-1)
   DO i = 1 , Ncntr
      labl(i) = 3
   ENDDO
!
!     READ AND STORE CONTOUR VALUES AND CENTROIDS
!
   elid = 0
   lopcox = lopcor + 1
   DO i = 1 , lopcox
      is = isav + i - 1
      Iz(is) = 0
   ENDDO
   IF ( Color>=0 ) GOTO 200
   CALL gopen(Est,Gplst(b3),2)
   CALL bckrec(Est)
   imhere = 120
 100  CALL read(*900,*800,Est,esym,1,0,m)
   irr = 0
   CALL fread(Est,ngppe,1,0)
 200  CALL fwdrec(*1200,Sort)
 300  CALL read(*1200,*1200,Sort,iflag,1,0,m)
   IF ( iflag==0 ) GOTO 1200
   IF ( iflag==-2 ) GOTO 200
   CALL fread(Sort,ibegin,2,0)
   CALL read(*1200,*400,Sort,Iz(id),lines,1,i)
 400  iread = 0
   nel = 0
   DO i = 1 , lines
      ic = icen + 2*(i-1)
      iv = ival + i - 1
      id1 = id + i - 1
      is = isav + i - 1
      DO j = 1 , lopcor
         js = isav + j - 1
         jv = ival + j - 1
         jc = icen + 2*(j-1)
         IF ( Iz(js)==0 ) EXIT
         IF ( Iz(id1)==Iz(js) ) THEN
            Z(iv) = Z(jv)
            Z(ic) = Z(jc)
            Z(ic+1) = Z(jc+1)
            Iz(is) = Iz(id1)
            nel = nel + 1
            EXIT
         ENDIF
      ENDDO
   ENDDO
   IF ( elid>0 ) GOTO 600
 500  CALL read(*1100,*1100,Stress,essym,1,0,m)
 600  DO
      CALL read(*1100,*500,Stress,elid,1,0,m)
      IF ( elid==0 ) GOTO 500
      CALL fread(Stress,v,1,0)
      CALL fread(Stress,pt,2,0)
      DO i = 1 , lines
         id1 = id + i - 1
         is = isav + i - 1
         iv = ival + i - 1
         ic = icen + 2*i - 2
         IF ( Iz(id1)/=elid ) CYCLE
         IF ( Iz(is)==elid ) CYCLE
         Z(iv) = v
         Z(ic) = pt(1)
         Z(ic+1) = pt(2)
         Iz(is) = elid
         IF ( Color>=0 ) GOTO 700
         imhere = 200
         ASSIGN 680 TO irtn
         jrr = 0
 620     offset = 0
         IF ( esym==kbar ) offset = 6
         IF ( esym==kt3 .OR. esym==kq4 ) offset = 1
         DO
            CALL read(*640,*660,Est,elmid,1,0,m)
            IF ( elmid==0 ) THEN
!
               jrr = jrr + 1
               IF ( jrr<=1 ) CALL bckrec(Est)
               imhere = 203
               ASSIGN 100 TO irtn
               CALL read(*640,*640,Est,esym,1,0,m)
               CALL fread(Est,ngppe,1,0)
               GOTO 620
            ELSE
               CALL fread(Est,0,-1,0)
               CALL fread(Est,gpts,ngppe,0)
               IF ( offset/=0 ) CALL fread(Est,0,-offset,0)
               IF ( elmid==elid ) THEN
!
!     START TO CONTOUR FILL HERE
!
                  rcolor = icolor
                  rcntrl = Ncntr
                  DO ik = 1 , Ncntr
                     pen = 32 + (1.0-(rcntrl-ik+1)/rcntrl)*rcolor
                     ik1 = ik + 1
                     IF ( ik==Ncntr ) ik1 = ik
                     IF ( v>=Cntr(ik) .AND. v<=Cntr(ik1) ) THEN
                        DO j = 1 , ngppe
                           k = j + 1
                           ig = gpts(j)
                           ig = iabs(Gplst(ig))
                           IF ( j==ngppe ) k = 1
                           ig1 = gpts(k)
                           ig1 = iabs(Gplst(ig1))
                           IF ( j==ngppe ) pen = 0
                           CALL line(X(2,ig),X(3,ig),X(2,ig1),X(3,ig1),pen,0)
                        ENDDO
                        EXIT
                     ENDIF
                  ENDDO
                  GOTO 700
               ENDIF
            ENDIF
         ENDDO
!
 640     err = err + 1
         IF ( err>3 ) GOTO 1000
         CALL rewind(Est)
         CALL skprec(Est,1)
         GOTO irtn
!
 660     imhere = 205
         ASSIGN 660 TO irtn
 680     CALL read(*640,*660,Est,esym,1,0,m)
         CALL fread(Est,ngppe,1,0)
         jrr = 0
         GOTO 620
 700     nel = nel + 1
         EXIT
      ENDDO
      IF ( nel>=lines ) THEN
!
!     END DATA SEARCH
!
         l = lines
         is = lines + isav
         Iz(is) = 0
         IF ( lines>3 ) THEN
            ig = iabs(Gplst(igdpt))
            IF ( Deform/=0 ) THEN
               xmid = U(1,ig)
               ymid = U(2,ig)
            ELSE
               xmid = X(2,ig)
               ymid = X(3,ig)
            ENDIF
            sum1 = 0.0
            sum2 = 0.0
            DO i = 1 , lines
               iv = ival + i - 1
               ic = icen + 2*i - 2
               s = sqrt((xmid-Z(ic))**2+(ymid-Z(ic+1))**2)
               sum1 = sum1 + Z(iv)*s
               sum2 = sum2 + s
            ENDDO
            cenval = sum1/sum2
         ELSE
            xmid = Z(icen+4)
            ymid = Z(icen+5)
            cenval = Z(ival+2)
            l = 1
         ENDIF
         iv = ival + lines
         ic = icen + 2*lines
         Z(iv) = Z(ival)
         Z(ic) = Z(icen)
         Z(ic+1) = Z(icen+1)
!
!     PLOT CONTOURS.
!
         IF ( Color>=0 ) THEN
            rcolor = icolor
            rcntrl = Ncntr
!
            CALL close(Est,2)
            CALL gopen(Scr1,Gplst(b3),3)
!
            DO i = 1 , Ncntr
               IF ( Color/=0 ) pen = 1 + (1.0-(rcntrl-i+1)/rcntrl)*rcolor
               DO j = 1 , l
                  pt(1) = Xmin - 1.0
                  pt(3) = pt(1)
                  pt(5) = pt(1)
                  jc = icen + 2*j - 2
                  jv = ival + j - 1
                  d = (Z(jv)-Z(jv+1))
                  IF ( abs(Z(jv)-Cntr(i))<=abs(d) .AND. abs(Z(jv+1)-Cntr(i))<=abs(d) ) THEN
                     IF ( d==0.0 ) d = 1.0
                     pt(1) = Z(jc) + (Z(jc+2)-Z(jc))*(Z(jv)-Cntr(i))/d
                     pt(2) = Z(jc+1) + (Z(jc+3)-Z(jc+1))*(Z(jv)-Cntr(i))/d
                  ENDIF
                  d = Z(jv+1) - cenval
                  IF ( abs(Z(jv+1)-Cntr(i))<=abs(d) .AND. abs(cenval-Cntr(i))<=abs(d) ) THEN
                     IF ( d==0.0 ) d = 1.0
                     pt(3) = Z(jc+2) + (xmid-Z(jc+2))*(Z(jv+1)-Cntr(i))/d
                     pt(4) = Z(jc+3) + (ymid-Z(jc+3))*(Z(jv+1)-Cntr(i))/d
                  ENDIF
                  d = cenval - Z(jv)
                  IF ( abs(cenval-Cntr(i))<=abs(d) .AND. abs(Z(jv)-Cntr(i))<=abs(d) ) THEN
                     IF ( d==0.0 ) d = 1.0
                     pt(5) = xmid + (Z(jc)-xmid)*(cenval-Cntr(i))/d
                     pt(6) = ymid + (Z(jc+1)-ymid)*(cenval-Cntr(i))/d
                  ENDIF
                  pt(7) = pt(1)
                  pt(8) = pt(2)
                  DO k = 1 , 5 , 2
                     IF ( pt(k)>=Xmin .AND. pt(k+2)>=Xmin ) THEN
                        CALL line(pt(k),pt(k+1),pt(k+2),pt(k+3),pen,0)
                        labl(i) = labl(i) + 1
                        IF ( labl(i)==4 ) THEN
                           labl(i) = 0
                           CALL write(Scr1,i,1,0)
                           CALL write(Scr1,pt(k),2,0)
                        ENDIF
                     ENDIF
                  ENDDO
               ENDDO
            ENDDO
!
            CALL close(Scr1,2)
            CALL gopen(Est,Gplst(b3),2)
         ENDIF
         GOTO 300
      ENDIF
   ENDDO
!
 800  CALL bckrec(Est)
   irr = irr + 1
   IF ( irr<3 ) GOTO 100
!
!     END OF FILE ON EST
!
 900  err = err + 1
   IF ( err<=3 ) THEN
      CALL rewind(Est)
      CALL skprec(Est,1)
      GOTO 100
   ENDIF
 1000 WRITE (Nout,99001) Uim , elid , imhere , err , irr , ngppe
99001 FORMAT (A29,', CONTOUR FAILED TO LOCATE ELMENT ID =',I8,/5X,'IMHERE =',I5,5X,'ERR,IRR,NGPPE =',3I8)
   GOTO 600
!
!     END OF FILE ON STRESS
!
 1100 CALL rewind(Stress)
   CALL fwdrec(*1200,Stress)
   IF ( iread==1 ) GOTO 300
   iread = 1
   GOTO 500
!
 1200 CALL close(Sort,1)
   CALL close(Stress,1)
   CALL close(Scr1,1)
!     IF (COLOR .LT. 0) CALL CLOSE (EST,1)
!     IF (COLOR .GE. 0) CALL GOPEN (EST,GPLST(B3),2)
 1300 CALL line(0.,0.,0.,0.,pen,+1)
   IF ( Color/=0 ) THEN
      CALL typflt(0.0,0.0,0,0,0,-1)
      CALL typflt(368.14,990.0,1,Cntr(1),-8,0)
      center = (Cntr(1)+Cntr(Ncntr))/2.0
      CALL typflt(585.90,990.0,1,center,-8,0)
      CALL typflt(796.3,990.0,1,Cntr(Ncntr),-8,0)
      CALL typflt(0.0,0.0,0,0,0,+1)
      IF ( Color<0 ) GOTO 1500
   ENDIF
   CALL gopen(Scr1,Gplst(B1),0)
   IF ( Color==0 ) CALL typint(0.,0.,0,0,0,-1)
   DO
      CALL read(*1400,*1400,Scr1,i,1,0,m)
      CALL fread(Scr1,pt,2,0)
      IF ( Color==0 ) CALL typint(pt(1),pt(2),1,i,1,0)
   ENDDO
 1400 IF ( Color==0 ) CALL typint(0.,0.,0,0,0,+1)
   CALL close(Scr1,1)
 1500 CALL pltopr
 1600 IF ( (Icntvl>9 .AND. Icntvl<14) .AND. Pedge/=1 ) RETURN
   CALL gopen(Parm,Gplst(b2),2)
END SUBROUTINE contor
