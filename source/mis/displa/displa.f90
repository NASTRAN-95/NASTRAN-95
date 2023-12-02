!*==displa.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE displa(Gplst,X,S,U,Pen,Deform,Label,Pt,B1)
   IMPLICIT NONE
   USE C_BLANK
   USE C_PLTDAT
   USE C_XXPARM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Gplst
   REAL , DIMENSION(3,1) :: X
   REAL , DIMENSION(2,1) :: S
   REAL , DIMENSION(3,1) :: U
   INTEGER :: Pen
   INTEGER :: Deform
   INTEGER , DIMENSION(50) :: Label
   REAL , DIMENSION(8) :: Pt
   INTEGER :: B1
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(4) :: a , xx , yy
   REAL :: conmax , conmin , d , delta , dmax
   INTEGER :: elid , gp , i , ig , ij , ik , index , itype , j , m , ngppe , offset
   INTEGER , DIMENSION(12) :: gpts
   INTEGER , SAVE :: kbar , kq4 , kt3 , nmsg
   INTEGER , DIMENSION(13) , SAVE :: msg
   INTEGER , DIMENSION(3) , SAVE :: mvect
   REAL , DIMENSION(3) :: sign
   EXTERNAL bckrec , close , fread , gopen , line , read , write , wrtprt
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
   DATA nmsg/13/ , msg/4H(33X , 4H,41H , 4H***  , 4HINCO , 4HMPLE , 4HTE P , 4HLOT  , 4HDUE  , 4HTO I , 4HNPUT , 4H OR  , 4HFILE ,  &
       &4H.)  /
   DATA mvect/3*0/ , kbar , kt3 , kq4/2HBR , 2HT3 , 2HQ4/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         SPAG_Loop_1_1: DO
            CALL read(*20,*20,Ect2,itype,1,0,m)
            offset = 0
            IF ( itype==kbar ) offset = 6
            IF ( itype==kt3 .OR. itype==kq4 ) offset = 1
            CALL fread(Ect2,ngppe,1,0)
            DO
               CALL fread(Ect2,elid,1,0)
               IF ( elid==0 ) CYCLE SPAG_Loop_1_1
               CALL fread(Ect2,0,-1,0)
               CALL fread(Ect2,gpts,ngppe,0)
               IF ( offset/=0 ) CALL fread(Ect2,0,-offset,0)
               IF ( ngppe>2 ) THEN
                  ij = 1
                  ik = 3
                  SPAG_Loop_3_2: DO
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
                     IF ( ngppe==3 .OR. ij==3 ) EXIT SPAG_Loop_3_2
                     gpts(ngppe+1) = gpts(1)
                     ij = 3
                     ik = 5
                  ENDDO SPAG_Loop_3_2
               ENDIF
            ENDDO
            EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
 20      CALL bckrec(Ect2)
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
         spag_nextblock_1 = 2
      CASE (2)
         CALL close(Scr1,1)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE displa
