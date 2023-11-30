
SUBROUTINE displa(Gplst,X,S,U,Pen,Deform,Label,Pt,B1)
   IMPLICIT NONE
   INTEGER Axis(3) , Color , Daxis(3) , Ect2 , Icntvl , Merr , Ncntr , Ngpset , Scr1
   REAL Cntr(50) , Defmax , Maxdef , Sk(6) , Sk18(18) , Ski(6) , Skip(5) , Skip1(39) , Skip2(110) , Skip3(2) , Skp(7) , Skppar(6) , &
      & Xmin
   COMMON /blank / Skip , Ngpset , Sk , Ect2 , Skp , Merr , Ski , Scr1
   COMMON /pltdat/ Skip3 , Xmin
   COMMON /xxparm/ Skip1 , Maxdef , Defmax , Axis , Daxis , Skip2 , Ncntr , Cntr , Icntvl , Skppar , Sk18 , Color
   INTEGER B1 , Deform , Pen
   INTEGER Gplst(1) , Label(50)
   REAL Pt(8) , S(2,1) , U(3,1) , X(3,1)
   REAL a(4) , conmax , conmin , d , delta , dmax , sign(3) , xx(4) , yy(4)
   INTEGER elid , gp , gpts(12) , i , ig , ij , ik , index , itype , j , kbar , kq4 , kt3 , m , msg(13) , mvect(3) , ngppe , nmsg , &
         & offset
!
   DATA nmsg/13/ , msg/4H(33X , 4H,41H , 4H***  , 4HINCO , 4HMPLE , 4HTE P , 4HLOT  , 4HDUE  , 4HTO I , 4HNPUT , 4H OR  , 4HFILE ,  &
       &4H.)  /
   DATA mvect/3*0/ , kbar , kt3 , kq4/2HBR , 2HT3 , 2HQ4/
!
!
   CALL gopen(Scr1,Gplst(B1),1)
   IF ( abs(Defmax)>1.E-8 ) THEN
!
      DO i = 1 , 3
         sign(i) = Daxis(i)/Axis(i)
      ENDDO
      DO gp = 1 , Ngpset
         DO i = 1 , 3
            j = Axis(i)
            ij = iabs(j)
            a(ij) = sign(ij)*U(i,gp)
         ENDDO
         dmax = Maxdef
         IF ( dmax<.00001 ) dmax = 1.0
         DO i = 1 , 3
            U(i,gp) = a(i)*(Defmax/dmax)
         ENDDO
      ENDDO
      index = Icntvl - 9
      Ncntr = min0(Ncntr,50)
      IF ( Cntr(1)==Cntr(2) ) THEN
         IF ( index<=3 ) conmin = U(index,1)
         IF ( index>3 ) conmin = sqrt(U(1,1)**2+U(2,1)**2+U(3,1)**2)
         conmax = conmin
         DO gp = 1 , Ngpset
            IF ( index>3 ) THEN
               d = sqrt(U(1,gp)**2+U(2,gp)**2+U(3,gp)**2)
               conmin = amin1(conmin,d)
               conmax = amax1(conmax,d)
            ELSE
               conmin = amin1(conmin,U(index,gp))
               conmax = amax1(conmax,U(index,gp))
            ENDIF
         ENDDO
         delta = (conmax-conmin)/float(Ncntr-1)
         Cntr(1) = conmin
         j = Ncntr - 1
         DO i = 2 , j
            Cntr(i) = Cntr(i-1) + delta
         ENDDO
         Cntr(Ncntr) = conmax
      ENDIF
      CALL line(0.,0.,0.,0.,Pen,+1)
      DO i = 1 , Ncntr
         Label(i) = 3
      ENDDO
   ELSE
      CALL wrtprt(Merr,mvect,msg,nmsg)
      GOTO 300
   ENDIF
 100  CALL read(*200,*200,Ect2,itype,1,0,m)
   offset = 0
   IF ( itype==kbar ) offset = 6
   IF ( itype==kt3 .OR. itype==kq4 ) offset = 1
   CALL fread(Ect2,ngppe,1,0)
   DO
      CALL fread(Ect2,elid,1,0)
      IF ( elid==0 ) GOTO 100
      CALL fread(Ect2,0,-1,0)
      CALL fread(Ect2,gpts,ngppe,0)
      IF ( offset/=0 ) CALL fread(Ect2,0,-offset,0)
      IF ( ngppe>2 ) THEN
         ij = 1
         ik = 3
         DO
            j = 0
            DO i = ij , ik
               j = j + 1
               ig = gpts(i)
               ig = iabs(Gplst(ig))
               IF ( index<=3 ) a(j) = U(index,ig)
               IF ( index>3 ) a(j) = sqrt(U(1,ig)**2+U(2,ig)**2+U(3,ig)**2)
               IF ( Deform/=0 ) THEN
                  Pt(2*j-1) = S(1,ig)
                  Pt(2*j) = S(2,ig)
               ELSE
                  Pt(2*j-1) = X(2,ig)
                  Pt(2*j) = X(3,ig)
               ENDIF
            ENDDO
            Pt(7) = Pt(1)
            Pt(8) = Pt(2)
            a(4) = a(1)
            DO i = 1 , Ncntr
               IF ( Color/=0 ) THEN
                  j = iabs(Color)
                  IF ( Ncntr<=j ) Pen = i*j/Ncntr
                  IF ( Ncntr>j ) Pen = 1 + i/(Ncntr/j)
                  IF ( Pen>j ) Pen = j
               ENDIF
               DO j = 1 , 3
                  xx(j) = Xmin - 1.0
                  d = a(j) - a(j+1)
                  IF ( abs(a(j)-Cntr(i))<=abs(d) .AND. abs(a(j+1)-Cntr(i))<=abs(d) ) THEN
                     IF ( d==0.0 ) d = 1.0
                     xx(j) = Pt(2*j-1) + (Pt(2*j+1)-Pt(2*j-1))*(a(j)-Cntr(i))/d
                     yy(j) = Pt(2*j) + (Pt(2*j+2)-Pt(2*j))*(a(j)-Cntr(i))/d
                  ENDIF
               ENDDO
               xx(4) = xx(1)
               yy(4) = yy(1)
               DO j = 1 , 3
                  IF ( xx(j)>=Xmin .AND. xx(j+1)>=Xmin ) THEN
                     CALL line(xx(j),yy(j),xx(j+1),yy(j+1),Pen,0)
                     Label(i) = Label(i) + 1
                     IF ( Label(i)==4 ) THEN
                        Label(i) = 0
                        CALL write(Scr1,i,1,0)
                        CALL write(Scr1,xx(j),1,0)
                        CALL write(Scr1,yy(j),1,0)
                     ENDIF
                  ENDIF
               ENDDO
            ENDDO
            IF ( ngppe==3 .OR. ij==3 ) EXIT
            gpts(ngppe+1) = gpts(1)
            ij = 3
            ik = 5
         ENDDO
      ENDIF
   ENDDO
 200  CALL bckrec(Ect2)
   DO gp = 1 , Ngpset
      DO i = 1 , 3
         j = Axis(i)
         ij = iabs(j)
         a(i) = sign(i)*U(ij,gp)
      ENDDO
      DO i = 1 , 3
         U(i,gp) = a(i)*(dmax/Defmax)
      ENDDO
   ENDDO
 300  CALL close(Scr1,1)
END SUBROUTINE displa