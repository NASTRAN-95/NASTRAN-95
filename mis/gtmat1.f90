
SUBROUTINE gtmat1(Sym,Tt)
   IMPLICIT NONE
   INTEGER Len1 , Loc1 , Trn
   REAL Tc6(6,6) , Tt6(6,6) , Z(1)
   COMMON /gtmatx/ Loc1 , Len1 , Trn , Tt6 , Tc6
   COMMON /zzzzzz/ Z
   INTEGER Ihelp , Len2 , Loc2 , Sym , Tran
   REAL Acpt(1) , Tc(3,3) , Tg(3,3) , Tg6(6,6) , Tt(3,3)
   INTEGER andf , orf , rshift
   INTEGER chk1 , chk2 , ecpt1 , i , idir , iflag , ikind , isav , j , list(32) , name(2) , ndir
   REAL ecpt(4) , prod(6) , rflag , smat(6,3) , symm(6,6) , t(6,6) , tid(3,3)
   EXTERNAL andf , orf , rshift
!
!     THIS SUBROUTINE PROCESSES TRANSFORMATION MATRICES
!     IT IS CALLED ONLY BY CMSFIL
!
   !>>>>EQUIVALENCE (ecpt1,ecpt(1))
   !>>>>EQUIVALENCE (iflag,rflag)
   DATA tid/1. , 0. , 0. , 0. , 1. , 0. , 0. , 0. , 1./
   DATA smat/ - 1. , 1. , 1. , 1. , -1. , -1. , 1. , -1. , 1. , -1. , 1. , -1. , 1. , 1. , -1. , -1. , -1. , 1./
   DATA name/4HGTMT , 4H1Z  /
!
   ikind = 0
   DO i = 1 , 6
      DO j = 1 , 6
         Tt6(i,j) = 0.0
      ENDDO
   ENDDO
   IF ( Trn==0 .AND. Sym==0 ) THEN
!
      DO i = 1 , 6
         Tt6(i,i) = 1.0
      ENDDO
      DO i = 1 , 3
         DO j = 1 , 3
            Tt(i,j) = tid(i,j)
         ENDDO
      ENDDO
      isav = ikind
      chk1 = 13579
      RETURN
   ELSE
      IF ( Loc1==0 .OR. Trn==0 ) THEN
         DO i = 1 , 3
            DO j = 1 , 3
               Tt(i,j) = tid(i,j)
            ENDDO
         ENDDO
      ELSE
         CALL pretrs(Z(Loc1),Len1)
         ikind = orf(ikind,1)
         DO i = 2 , 4
            ecpt(i) = 0.0
         ENDDO
         ecpt1 = Trn
         CALL transs(ecpt,Tt)
      ENDIF
      DO i = 1 , 3
         DO j = 1 , 3
            Tt6(i,j) = Tt(i,j)
            Tt6(i+3,j+3) = Tt(i,j)
         ENDDO
      ENDDO
      DO i = 1 , 6
         DO j = 1 , 6
            symm(i,j) = 0.0
         ENDDO
      ENDDO
      IF ( Sym==0 ) THEN
         DO i = 1 , 6
            symm(i,i) = 1.0
         ENDDO
      ELSE
         ikind = orf(ikind,1)
         CALL decode(Sym,list,ndir)
         DO i = 1 , 6
            prod(i) = 1.0
         ENDDO
         DO i = 1 , ndir
            idir = list(i) + 1
            idir = 4 - idir
            DO j = 1 , 6
               prod(j) = prod(j)*smat(j,idir)
            ENDDO
         ENDDO
         DO i = 1 , 6
            symm(i,i) = prod(i)
         ENDDO
      ENDIF
      CALL gmmats(Tt6,6,6,0,symm,6,6,0,t)
      DO i = 1 , 6
         DO j = 1 , 6
            Tt6(i,j) = t(i,j)
         ENDDO
      ENDDO
      DO i = 1 , 3
         DO j = 1 , 3
            Tt(i,j) = Tt6(i,j)
         ENDDO
      ENDDO
      isav = ikind
      RETURN
   ENDIF
!
!
   ENTRY gtmat2(Loc2,Len2,Acpt,Tc)
!     ================================
!
   ikind = isav
   DO i = 1 , 6
      DO j = 1 , 6
         Tc6(i,j) = 0.0
      ENDDO
   ENDDO
   rflag = Acpt(1)
   IF ( Loc2==0 .OR. iflag==0 ) THEN
      DO i = 1 , 3
         DO j = 1 , 3
            Tc(i,j) = tid(i,j)
         ENDDO
      ENDDO
   ELSE
      CALL pretrs(Z(Loc2),Len2)
      CALL transs(Acpt,Tc)
      ikind = orf(ikind,2)
   ENDIF
   DO i = 1 , 3
      DO j = 1 , 3
         Tc6(i,j) = Tc(i,j)
         Tc6(i+3,j+3) = Tc(i,j)
      ENDDO
   ENDDO
   chk2 = 24680
   RETURN
!
!
   ENTRY gtmat3(Tran,Tg,Tg6,Ihelp)
!     ================================
!
   IF ( chk1/=13579 .AND. chk2/=24680 ) CALL mesage(-37,0,name)
   DO i = 1 , 6
      DO j = 1 , 6
         Tg6(i,j) = 0.0
      ENDDO
   ENDDO
   IF ( Tran<0 ) THEN
   ELSEIF ( Tran==0 ) THEN
      ikind = orf(ikind,4)
   ELSE
      CALL pretrs(Z(Loc1),Len1)
      DO i = 2 , 4
         ecpt(i) = 0.0
      ENDDO
      ecpt1 = Tran
      ikind = orf(ikind,8)
      IF ( Tran/=Trn ) ikind = orf(ikind,16)
      CALL transs(ecpt,Tg)
      ikind = orf(ikind,4)
      GOTO 100
   ENDIF
   DO i = 1 , 3
      DO j = 1 , 3
         Tg(i,j) = tid(i,j)
      ENDDO
   ENDDO
   IF ( andf(rshift(ikind,1),1)==1 .AND. Tran==-1 ) THEN
      CALL gmmats(Tt6,6,6,0,Tc6,6,6,0,Tg6)
      Ihelp = ikind
      RETURN
   ENDIF
!
 100  DO i = 1 , 3
      DO j = 1 , 3
         Tg6(i,j) = Tg(i,j)
         Tg6(i+3,j+3) = Tg(i,j)
      ENDDO
   ENDDO
   Ihelp = ikind
END SUBROUTINE gtmat1