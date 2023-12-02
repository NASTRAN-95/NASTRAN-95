!*==contor.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE contor(Gplst,X,U,Dd,Z,Iz,Ppen,Deform,B1,Opcor)
   USE c_blank
   USE c_drwdat
   USE c_pltdat
   USE c_system
   USE c_xmssg
   USE c_xxparm
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Gplst
   REAL , DIMENSION(3,1) :: X
   REAL , DIMENSION(2,1) :: U
   REAL , DIMENSION(3,1) :: Dd
   REAL , DIMENSION(1) :: Z
   INTEGER , DIMENSION(1) :: Iz
   INTEGER :: Ppen
   INTEGER :: Deform
   INTEGER :: B1
   INTEGER :: Opcor
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: b2 , b3 , elid , elmid , err , esym , i , ib , ic , icen , icolor , id , id1 , iflag , ig , ig1 , igdpt , ii , ik ,   &
            & ik1 , ik2 , ik3 , ik4 , imhere , iread , irr , irtn , is , isav , iv , ival , j , jc , jrr , js , jv , k , l , lines ,&
            & lopcor , lopcox , m , nel , ngppe , offset , pen
   REAL :: center , cenval , conmax , conmin , d , delta , essym , rcntrl , rcolor , s , sum1 , sum2 , v , xmid , ymid
   INTEGER , DIMENSION(12) :: gpts
   INTEGER , DIMENSION(2) :: ibegin
   INTEGER , SAVE :: kbar , kq4 , kt3
   INTEGER , DIMENSION(50) :: labl
   REAL , DIMENSION(8) :: pt , xb
   INTEGER , DIMENSION(2) , SAVE :: sub
   EXTERNAL bckrec , close , create , displa , fread , fwdrec , gopen , line , order , pltopr , read , rewind , skprec , typflt ,   &
          & typint , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
   !>>>>EQUIVALENCE (ibegin(1),lines) , (ibegin(2),igdpt)
   DATA kbar , kt3 , kq4/2HBR , 2HT3 , 2HQ4/
   DATA sub/4HCONT , 4HOR  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         b2 = B1 - 2*bufsiz
         b3 = b2 - bufsiz
         lopcor = Opcor/5
         id = 1
         err = 0
         irr = 0
         ncntr = iabs(ncntr)
!
!     COLOR = 0 IS NO COLOR CONTOUR,
!     COLOR = 1 TO  31 IS DRAW CONTOUR LINES IN COLOR.
!     COLOR =-1 TO -31 ID COLOR FILL ELEMENTS BY STRESS
!
!     THIS IS THE CODE FOR THE COLOR BAR SCALE
!     AT THE TOP OF THE PLOT
!
         IF ( color/=0 ) THEN
            CALL line(0.0,0.0,0.0,0.0,32,-1)
            icolor = iabs(color)
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
         IF ( icntvl<=9 .OR. icntvl>=14 .OR. pedge==1 ) THEN
            CALL close(parm,2)
            IF ( icntvl<=9 .OR. icntvl>13 ) CALL create(Gplst,X,U,Deform,conmin,conmax,Z(ival),Z(icen),lopcor,B1,b2)
            IF ( iset/=jset ) CALL order(Gplst,Z(ival),Z(isav),Z(icen),Z(id),lopcor,B1,b2,b3)
            iset = jset
         ENDIF
         IF ( icntvl>9 .AND. icntvl<14 ) CALL displa(Gplst,X,U,Dd,pen,Deform,labl,pt,B1)
         IF ( icntvl>9 .AND. icntvl<14 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( conmin==conmax ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( color<0 ) CALL close(est,2)
         CALL gopen(scr1,Gplst(B1),1)
         CALL close(scr1,2)
         CALL gopen(sort,Gplst(b2),2)
         CALL gopen(stress,Gplst(B1),0)
!
!     BUFFERS ASSIGNEMENT HERE -
!     B1 IS USED BY STRESS (SCRATCH1/301), AND BY SCR1 (SCRATCH4/304)
!     FOR SHORT PERIODS OF TIME ONLY
!     B2 IS USED BY SORT (SCRATCH2/302)
!     B3 IS USED BY EST (ELEST/103) AND BY SCR1 (SCRATCH4/304)
!
         ncntr = min0(ncntr,50)
         IF ( cntr(1)==cntr(2) ) THEN
!
!     IF INTERVALS SPECIFIED, DEFINE CONTOUR VALUES
!
            delta = (conmax-conmin)/float(ncntr-1)
            cntr(1) = conmin
            j = ncntr - 1
            DO i = 2 , j
               cntr(i) = cntr(i-1) + delta
            ENDDO
            cntr(ncntr) = conmax
         ENDIF
         CALL line(0.,0.,0.,0.,pen,-1)
         DO i = 1 , ncntr
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
         IF ( color>=0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL gopen(est,Gplst(b3),2)
         CALL bckrec(est)
         imhere = 120
 20      CALL read(*100,*80,est,esym,1,0,m)
         irr = 0
         CALL fread(est,ngppe,1,0)
         spag_nextblock_1 = 2
      CASE (2)
         CALL fwdrec(*140,sort)
         spag_nextblock_1 = 3
      CASE (3)
         CALL read(*140,*140,sort,iflag,1,0,m)
         IF ( iflag==0 ) GOTO 140
         IF ( iflag==-2 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         CALL fread(sort,ibegin,2,0)
         CALL read(*140,*40,sort,Iz(id),lines,1,i)
 40      iread = 0
         nel = 0
         DO i = 1 , lines
            ic = icen + 2*(i-1)
            iv = ival + i - 1
            id1 = id + i - 1
            is = isav + i - 1
            SPAG_Loop_2_1: DO j = 1 , lopcor
               js = isav + j - 1
               jv = ival + j - 1
               jc = icen + 2*(j-1)
               IF ( Iz(js)==0 ) EXIT SPAG_Loop_2_1
               IF ( Iz(id1)==Iz(js) ) THEN
                  Z(iv) = Z(jv)
                  Z(ic) = Z(jc)
                  Z(ic+1) = Z(jc+1)
                  Iz(is) = Iz(id1)
                  nel = nel + 1
                  EXIT SPAG_Loop_2_1
               ENDIF
            ENDDO SPAG_Loop_2_1
         ENDDO
         IF ( elid>0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 60      CALL read(*120,*120,stress,essym,1,0,m)
         spag_nextblock_1 = 4
      CASE (4)
         DO
            CALL read(*120,*60,stress,elid,1,0,m)
            IF ( elid==0 ) GOTO 60
            CALL fread(stress,v,1,0)
            CALL fread(stress,pt,2,0)
            SPAG_Loop_2_4: DO i = 1 , lines
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
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
                     IF ( color>=0 ) THEN
                        spag_nextblock_2 = 3
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                     imhere = 200
                     ASSIGN 66 TO irtn
                     jrr = 0
                     spag_nextblock_2 = 2
                  CASE (2)
                     SPAG_Loop_3_2: DO
                        offset = 0
                        IF ( esym==kbar ) offset = 6
                        IF ( esym==kt3 .OR. esym==kq4 ) offset = 1
                        DO
                           CALL read(*62,*64,est,elmid,1,0,m)
                           IF ( elmid==0 ) THEN
!
                              jrr = jrr + 1
                              IF ( jrr<=1 ) CALL bckrec(est)
                              imhere = 203
                              ASSIGN 20 TO irtn
                              CALL read(*62,*62,est,esym,1,0,m)
                              CALL fread(est,ngppe,1,0)
                              CYCLE SPAG_Loop_3_2
                           ELSE
                              CALL fread(est,0,-1,0)
                              CALL fread(est,gpts,ngppe,0)
                              IF ( offset/=0 ) CALL fread(est,0,-offset,0)
                              IF ( elmid==elid ) THEN
!
!     START TO CONTOUR FILL HERE
!
                                 rcolor = icolor
                                 rcntrl = ncntr
                                 SPAG_Loop_5_3: DO ik = 1 , ncntr
                                    pen = 32 + (1.0-(rcntrl-ik+1)/rcntrl)*rcolor
                                    ik1 = ik + 1
                                    IF ( ik==ncntr ) ik1 = ik
                                    IF ( v>=cntr(ik) .AND. v<=cntr(ik1) ) THEN
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
                                       EXIT SPAG_Loop_5_3
                                    ENDIF
                                 ENDDO SPAG_Loop_5_3
                                 spag_nextblock_2 = 3
                                 CYCLE SPAG_DispatchLoop_2
                              ENDIF
                           ENDIF
                        ENDDO
                        EXIT SPAG_Loop_3_2
                     ENDDO SPAG_Loop_3_2
!
 62                  err = err + 1
                     IF ( err>3 ) THEN
                        spag_nextblock_1 = 5
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     CALL rewind(est)
                     CALL skprec(est,1)
                     GOTO irtn
!
 64                  imhere = 205
                     ASSIGN 64 TO irtn
 66                  CALL read(*62,*64,est,esym,1,0,m)
                     CALL fread(est,ngppe,1,0)
                     jrr = 0
                     spag_nextblock_2 = 2
                  CASE (3)
                     nel = nel + 1
                     EXIT SPAG_Loop_2_4
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
            ENDDO SPAG_Loop_2_4
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
               IF ( color>=0 ) THEN
                  rcolor = icolor
                  rcntrl = ncntr
!
                  CALL close(est,2)
                  CALL gopen(scr1,Gplst(b3),3)
!
                  DO i = 1 , ncntr
                     IF ( color/=0 ) pen = 1 + (1.0-(rcntrl-i+1)/rcntrl)*rcolor
                     DO j = 1 , l
                        pt(1) = xmin - 1.0
                        pt(3) = pt(1)
                        pt(5) = pt(1)
                        jc = icen + 2*j - 2
                        jv = ival + j - 1
                        d = (Z(jv)-Z(jv+1))
                        IF ( abs(Z(jv)-cntr(i))<=abs(d) .AND. abs(Z(jv+1)-cntr(i))<=abs(d) ) THEN
                           IF ( d==0.0 ) d = 1.0
                           pt(1) = Z(jc) + (Z(jc+2)-Z(jc))*(Z(jv)-cntr(i))/d
                           pt(2) = Z(jc+1) + (Z(jc+3)-Z(jc+1))*(Z(jv)-cntr(i))/d
                        ENDIF
                        d = Z(jv+1) - cenval
                        IF ( abs(Z(jv+1)-cntr(i))<=abs(d) .AND. abs(cenval-cntr(i))<=abs(d) ) THEN
                           IF ( d==0.0 ) d = 1.0
                           pt(3) = Z(jc+2) + (xmid-Z(jc+2))*(Z(jv+1)-cntr(i))/d
                           pt(4) = Z(jc+3) + (ymid-Z(jc+3))*(Z(jv+1)-cntr(i))/d
                        ENDIF
                        d = cenval - Z(jv)
                        IF ( abs(cenval-cntr(i))<=abs(d) .AND. abs(Z(jv)-cntr(i))<=abs(d) ) THEN
                           IF ( d==0.0 ) d = 1.0
                           pt(5) = xmid + (Z(jc)-xmid)*(cenval-cntr(i))/d
                           pt(6) = ymid + (Z(jc+1)-ymid)*(cenval-cntr(i))/d
                        ENDIF
                        pt(7) = pt(1)
                        pt(8) = pt(2)
                        DO k = 1 , 5 , 2
                           IF ( pt(k)>=xmin .AND. pt(k+2)>=xmin ) THEN
                              CALL line(pt(k),pt(k+1),pt(k+2),pt(k+3),pen,0)
                              labl(i) = labl(i) + 1
                              IF ( labl(i)==4 ) THEN
                                 labl(i) = 0
                                 CALL write(scr1,i,1,0)
                                 CALL write(scr1,pt(k),2,0)
                              ENDIF
                           ENDIF
                        ENDDO
                     ENDDO
                  ENDDO
!
                  CALL close(scr1,2)
                  CALL gopen(est,Gplst(b3),2)
               ENDIF
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
!
 80      CALL bckrec(est)
         irr = irr + 1
         IF ( irr<3 ) GOTO 20
!
!     END OF FILE ON EST
!
 100     err = err + 1
         IF ( err<=3 ) THEN
            CALL rewind(est)
            CALL skprec(est,1)
            GOTO 20
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         WRITE (nout,99001) uim , elid , imhere , err , irr , ngppe
99001    FORMAT (A29,', CONTOUR FAILED TO LOCATE ELMENT ID =',I8,/5X,'IMHERE =',I5,5X,'ERR,IRR,NGPPE =',3I8)
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
!
!     END OF FILE ON STRESS
!
 120     CALL rewind(stress)
         CALL fwdrec(*140,stress)
         IF ( iread==1 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         iread = 1
         GOTO 60
!
 140     CALL close(sort,1)
         CALL close(stress,1)
         CALL close(scr1,1)
         spag_nextblock_1 = 6
      CASE (6)
!     IF (COLOR .LT. 0) CALL CLOSE (EST,1)
!     IF (COLOR .GE. 0) CALL GOPEN (EST,GPLST(B3),2)
         CALL line(0.,0.,0.,0.,pen,+1)
         IF ( color/=0 ) THEN
            CALL typflt(0.0,0.0,0,0,0,-1)
            CALL typflt(368.14,990.0,1,cntr(1),-8,0)
            center = (cntr(1)+cntr(ncntr))/2.0
            CALL typflt(585.90,990.0,1,center,-8,0)
            CALL typflt(796.3,990.0,1,cntr(ncntr),-8,0)
            CALL typflt(0.0,0.0,0,0,0,+1)
            IF ( color<0 ) THEN
               spag_nextblock_1 = 7
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         CALL gopen(scr1,Gplst(B1),0)
         IF ( color==0 ) CALL typint(0.,0.,0,0,0,-1)
         DO
            CALL read(*160,*160,scr1,i,1,0,m)
            CALL fread(scr1,pt,2,0)
            IF ( color==0 ) CALL typint(pt(1),pt(2),1,i,1,0)
         ENDDO
 160     IF ( color==0 ) CALL typint(0.,0.,0,0,0,+1)
         CALL close(scr1,1)
         spag_nextblock_1 = 7
      CASE (7)
         CALL pltopr
         spag_nextblock_1 = 8
      CASE (8)
         IF ( (icntvl>9 .AND. icntvl<14) .AND. pedge/=1 ) RETURN
         CALL gopen(parm,Gplst(b2),2)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE contor
