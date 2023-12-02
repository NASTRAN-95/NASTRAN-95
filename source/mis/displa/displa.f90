!*==displa.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE displa(Gplst,X,S,U,Pen,Deform,Label,Pt,B1)
   USE c_blank
   USE c_pltdat
   USE c_xxparm
   IMPLICIT NONE
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
         CALL gopen(scr1,Gplst(B1),1)
         IF ( abs(defmax)>1.E-8 ) THEN
!
            DO i = 1 , 3
               sign(i) = daxis(i)/axis(i)
            ENDDO
            DO gp = 1 , ngpset
               DO i = 1 , 3
                  j = axis(i)
                  ij = iabs(j)
                  a(ij) = sign(ij)*U(i,gp)
               ENDDO
               dmax = maxdef
               IF ( dmax<.00001 ) dmax = 1.0
               DO i = 1 , 3
                  U(i,gp) = a(i)*(defmax/dmax)
               ENDDO
            ENDDO
            index = icntvl - 9
            ncntr = min0(ncntr,50)
            IF ( cntr(1)==cntr(2) ) THEN
               IF ( index<=3 ) conmin = U(index,1)
               IF ( index>3 ) conmin = sqrt(U(1,1)**2+U(2,1)**2+U(3,1)**2)
               conmax = conmin
               DO gp = 1 , ngpset
                  IF ( index>3 ) THEN
                     d = sqrt(U(1,gp)**2+U(2,gp)**2+U(3,gp)**2)
                     conmin = amin1(conmin,d)
                     conmax = amax1(conmax,d)
                  ELSE
                     conmin = amin1(conmin,U(index,gp))
                     conmax = amax1(conmax,U(index,gp))
                  ENDIF
               ENDDO
               delta = (conmax-conmin)/float(ncntr-1)
               cntr(1) = conmin
               j = ncntr - 1
               DO i = 2 , j
                  cntr(i) = cntr(i-1) + delta
               ENDDO
               cntr(ncntr) = conmax
            ENDIF
            CALL line(0.,0.,0.,0.,Pen,+1)
            DO i = 1 , ncntr
               Label(i) = 3
            ENDDO
         ELSE
            CALL wrtprt(merr,mvect,msg,nmsg)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         SPAG_Loop_1_1: DO
            CALL read(*20,*20,ect2,itype,1,0,m)
            offset = 0
            IF ( itype==kbar ) offset = 6
            IF ( itype==kt3 .OR. itype==kq4 ) offset = 1
            CALL fread(ect2,ngppe,1,0)
            DO
               CALL fread(ect2,elid,1,0)
               IF ( elid==0 ) CYCLE SPAG_Loop_1_1
               CALL fread(ect2,0,-1,0)
               CALL fread(ect2,gpts,ngppe,0)
               IF ( offset/=0 ) CALL fread(ect2,0,-offset,0)
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
                     DO i = 1 , ncntr
                        IF ( color/=0 ) THEN
                           j = iabs(color)
                           IF ( ncntr<=j ) Pen = i*j/ncntr
                           IF ( ncntr>j ) Pen = 1 + i/(ncntr/j)
                           IF ( Pen>j ) Pen = j
                        ENDIF
                        DO j = 1 , 3
                           xx(j) = xmin - 1.0
                           d = a(j) - a(j+1)
                           IF ( abs(a(j)-cntr(i))<=abs(d) .AND. abs(a(j+1)-cntr(i))<=abs(d) ) THEN
                              IF ( d==0.0 ) d = 1.0
                              xx(j) = Pt(2*j-1) + (Pt(2*j+1)-Pt(2*j-1))*(a(j)-cntr(i))/d
                              yy(j) = Pt(2*j) + (Pt(2*j+2)-Pt(2*j))*(a(j)-cntr(i))/d
                           ENDIF
                        ENDDO
                        xx(4) = xx(1)
                        yy(4) = yy(1)
                        DO j = 1 , 3
                           IF ( xx(j)>=xmin .AND. xx(j+1)>=xmin ) THEN
                              CALL line(xx(j),yy(j),xx(j+1),yy(j+1),Pen,0)
                              Label(i) = Label(i) + 1
                              IF ( Label(i)==4 ) THEN
                                 Label(i) = 0
                                 CALL write(scr1,i,1,0)
                                 CALL write(scr1,xx(j),1,0)
                                 CALL write(scr1,yy(j),1,0)
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
 20      CALL bckrec(ect2)
         DO gp = 1 , ngpset
            DO i = 1 , 3
               j = axis(i)
               ij = iabs(j)
               a(i) = sign(i)*U(ij,gp)
            ENDDO
            DO i = 1 , 3
               U(i,gp) = a(i)*(dmax/defmax)
            ENDDO
         ENDDO
         spag_nextblock_1 = 2
      CASE (2)
         CALL close(scr1,1)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE displa
