
SUBROUTINE tiger(Ig,List,Inv,Ii3,Norig,Kg,Jg)
   IMPLICIT NONE
   REAL Dum(7) , Dum2s(2) , Dum3s(3) , Dum6b(6) , Gdum(3)
   INTEGER Ibuf , Ibuf1 , Iz(1) , Kdim , Maxdeg , Maxgrd , Mm , Ndum(2) , Nedge , Neq , Neqr , Nn , Nodep , Nompc , Nout , Rd ,     &
         & Rdrew , Rew , Scr1
   COMMON /banda / Ibuf1 , Nompc , Nodep
   COMMON /bandb / Dum6b , Kdim
   COMMON /bandd / Dum , Neq , Neqr
   COMMON /bands / Nn , Mm , Dum2s , Maxgrd , Maxdeg , Dum3s , Nedge
   COMMON /geomx / Gdum , Scr1
   COMMON /names / Rd , Rdrew , Ndum , Rew
   COMMON /system/ Ibuf , Nout
   COMMON /zzzzzz/ Iz
   INTEGER Ii3
   INTEGER Ig(1) , Inv(Ii3,1) , Jg(1) , Kg(1) , List(1) , Norig(1)
   INTEGER bunpk
   INTEGER i , igrid , ii , is , j , j2 , jj , k , kdim4 , kk , l , m , mm1 , n , np , nq , nterm
   REAL sub(2)
!
!     THIS ROUTINE MAKES ADDITIONS TO THE CONNECTION TABLE IG TO REFLECT
!     THE PRESENCE OF MPC'S AND STORES THE DEPENDENT POINTS IN LIST.
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!     NEQ =NUMBER OF MPC EQUATIONS.
!     NEQR=NUMBER OF MPC EQUATIONS COMING FROM RIGID ELEMENTS
!
   DATA sub/4HTIGE , 4HR   /
!
   IF ( Neq+Neqr==0 ) GOTO 300
   kdim4 = Kdim*4
   CALL open(*400,Scr1,Iz(Ibuf1),Rdrew)
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
            CALL read(*500,*500,Scr1,nterm,1,0,m)
            kk = 1
            j2 = 2
            IF ( jj/=1 ) THEN
               k = mod(nterm,1000)
               nterm = nterm/1000
               kk = nterm - k
               j2 = nterm
            ENDIF
            IF ( nterm>kdim4 ) GOTO 100
            CALL read(*500,*500,Scr1,Kg,nterm,1,m)
            CALL scat(Kg,nterm,Inv,Ii3,Norig)
!
            DO k = 1 , kk
               igrid = Kg(k)
               IF ( Nodep==+1 ) List(igrid) = igrid
!
!     IGRID=DEPENDENT GRID POINT IN AN MPC EQUATION.
!
               CALL bunpak(Ig,igrid,Maxdeg,Jg)
               DO i = 1 , Maxdeg
                  l = Jg(i)
                  IF ( l<=0 ) EXIT
!
!     L= A GRID POINT THAT IGRID IS CONNECTED TO BEFORE THE MPC IS APPLI
!
                  IF ( nterm>=2 ) THEN
                     DO j = j2 , nterm
                        CALL setig(l,Kg(j),Ig,Norig)
                     ENDDO
                  ENDIF
               ENDDO
            ENDDO
         ENDDO
      ENDIF
   ENDDO
   GOTO 200
!
 100  WRITE (Nout,99001)
99001 FORMAT (72H0*** MPC CARDS NOT PROCESSED IN BANDIT DUE TO INSUFFICIENT SCRATCH SPACE,//)
   Neq = 0
   Neqr = 0
 200  CALL close(Scr1,Rew)
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
            DO j = 1 , Mm
               l = Jg(j)
               IF ( l==0 ) EXIT
               Nedge = Nedge - 1
               k = 0
               DO
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
                     EXIT
                  ENDIF
               ENDDO
            ENDDO
         ENDDO
      ENDIF
   ENDIF
 300  RETURN
!
!     SCR1 FILE ERROR
!
 400  k = -1
   GOTO 600
 500  k = -2
 600  CALL mesage(k,Scr1,sub)
END SUBROUTINE tiger