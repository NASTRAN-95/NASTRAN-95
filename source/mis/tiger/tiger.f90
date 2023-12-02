!*==tiger.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE tiger(Ig,List,Inv,Ii3,Norig,Kg,Jg)
   IMPLICIT NONE
   USE C_BANDA
   USE C_BANDB
   USE C_BANDD
   USE C_BANDS
   USE C_GEOMX
   USE C_NAMES
   USE C_SYSTEM
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ii3
   INTEGER , DIMENSION(1) :: Ig
   INTEGER , DIMENSION(1) :: List
   INTEGER , DIMENSION(Ii3,1) :: Inv
   INTEGER , DIMENSION(1) :: Norig
   INTEGER , DIMENSION(1) :: Kg
   INTEGER , DIMENSION(1) :: Jg
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , igrid , ii , is , j , j2 , jj , k , kdim4 , kk , l , m , mm1 , n , np , nq , nterm
   REAL , DIMENSION(2) , SAVE :: sub
   EXTERNAL bpack , bunpak , bunpk , close , mesage , open , read , scat , setig
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE MAKES ADDITIONS TO THE CONNECTION TABLE IG TO REFLECT
!     THE PRESENCE OF MPC'S AND STORES THE DEPENDENT POINTS IN LIST.
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!     NEQ =NUMBER OF MPC EQUATIONS.
!     NEQR=NUMBER OF MPC EQUATIONS COMING FROM RIGID ELEMENTS
!
   DATA sub/4HTIGE , 4HR   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         IF ( Neq+Neqr==0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         kdim4 = Kdim*4
         CALL open(*20,Scr1,Iz(Ibuf1),Rdrew)
!
!     GENERATE NEW CONNECTIONS.
!     TWO PASSES.   FIRST PASS FOR MPC CARDS, AND SECOND FOR RIGID ELEM.
!
         DO jj = 1 , 2
            IF ( jj==1 ) nq = Neq
            IF ( jj==2 ) nq = Neqr
            IF ( nq/=0 ) THEN
!
!     READ MPC EQUATIONS AND RIGID ELEMENT GRIDS
!     AND CONVERT ORIGINAL GRID NOS. TO INTERNAL LABELS.
!
               DO ii = 1 , nq
                  CALL read(*40,*40,Scr1,nterm,1,0,m)
                  kk = 1
                  j2 = 2
                  IF ( jj/=1 ) THEN
                     k = mod(nterm,1000)
                     nterm = nterm/1000
                     kk = nterm - k
                     j2 = nterm
                  ENDIF
                  IF ( nterm>kdim4 ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  CALL read(*40,*40,Scr1,Kg,nterm,1,m)
                  CALL scat(Kg,nterm,Inv,Ii3,Norig)
!
                  DO k = 1 , kk
                     igrid = Kg(k)
                     IF ( Nodep==+1 ) List(igrid) = igrid
!
!     IGRID=DEPENDENT GRID POINT IN AN MPC EQUATION.
!
                     CALL bunpak(Ig,igrid,Maxdeg,Jg)
                     SPAG_Loop_4_1: DO i = 1 , Maxdeg
                        l = Jg(i)
                        IF ( l<=0 ) EXIT SPAG_Loop_4_1
!
!     L= A GRID POINT THAT IGRID IS CONNECTED TO BEFORE THE MPC IS APPLI
!
                        IF ( nterm>=2 ) THEN
                           DO j = j2 , nterm
                              CALL setig(l,Kg(j),Ig,Norig)
                           ENDDO
                        ENDIF
                     ENDDO SPAG_Loop_4_1
                  ENDDO
               ENDDO
            ENDIF
         ENDDO
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
!
         WRITE (Nout,99001)
99001    FORMAT (72H0*** MPC CARDS NOT PROCESSED IN BANDIT DUE TO INSUFFICIENT SCRATCH SPACE,//)
         Neq = 0
         Neqr = 0
         spag_nextblock_1 = 3
      CASE (3)
         CALL close(Scr1,Rew)
!
!     QUIT HERE IF MPC DEPENDENT POINTS ARE NOT TO BE DELETED FROM THE
!     CONNECTION TABLE IG.
!
         IF ( Nodep==+1 ) THEN
!
!     COMPRESS OUT ZEROS FORM LIST
!
            n = 0
            DO i = 1 , Nn
               IF ( List(i)/=0 ) THEN
                  n = n + 1
                  List(n) = List(i)
               ENDIF
            ENDDO
!
!     DELETES ALL REFERENCE IN THE CONNECTION TABLE IG TO THOSE POINTS
!     IN LIST
!
            IF ( n>0 ) THEN
               mm1 = Mm - 1
               DO ii = 1 , n
                  i = List(ii)
                  CALL bunpak(Ig,i,Mm,Jg)
                  SPAG_Loop_2_2: DO j = 1 , Mm
                     l = Jg(j)
                     IF ( l==0 ) EXIT SPAG_Loop_2_2
                     Nedge = Nedge - 1
                     k = 0
                     SPAG_Loop_3_3: DO
                        k = k + 1
                        m = bunpk(Ig,l,k)
                        IF ( m==i ) THEN
                           IF ( k<Mm ) THEN
                              DO np = k , mm1
                                 is = bunpk(Ig,l,np+1)
                                 CALL bpack(Ig,l,np,is)
                              ENDDO
                           ENDIF
                           CALL bpack(Ig,l,mm1+1,0)
                           CALL bpack(Ig,i,j,0)
                           EXIT SPAG_Loop_3_3
                        ENDIF
                     ENDDO SPAG_Loop_3_3
                  ENDDO SPAG_Loop_2_2
               ENDDO
            ENDIF
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         RETURN
!
!     SCR1 FILE ERROR
!
 20      k = -1
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 40      k = -2
         spag_nextblock_1 = 5
      CASE (5)
         CALL mesage(k,Scr1,sub)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE tiger
