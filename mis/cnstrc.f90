
SUBROUTINE cnstrc(Gp,Ele,Buf,Max)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Bufsiz , Incr , Inprew , Last , Merr , Mset , Ne(1) , Ngp , Norew , Nsets , Ntyps , Outnor , Outrew , Rew
   REAL Ect1 , Ect2 , Exgpid , Grid , Rd , Skp1(8) , Skp2 , Skp3(8) , Skp4 , Skp5(6)
   COMMON /blank / Ngp , Nsets , Skp1 , Skp2 , Exgpid , Skp3 , Merr , Skp4 , Grid , Ect2 , Skp5 , Mset , Ect1
   COMMON /gpta1 / Ntyps , Last , Incr , Ne
   COMMON /names / Rd , Inprew , Outnor , Outrew , Rew , Norew
   COMMON /system/ Bufsiz
!
! Dummy argument declarations
!
   INTEGER Max
   INTEGER Buf(1) , Ele(1) , Gp(1)
!
! Local variable declarations
!
   INTEGER ae , b1 , b2 , b3 , br , eid(2) , elid , err(2) , etype , exgp , gpt , gpts(32) , i , idx , ingp , j , labgp , m ,       &
         & msg1(14) , mtype , n , n1 , n2 , name(2) , ne16 , nel , neltyp , ngppe , ngpts , nmsg1 , ntypes , off(6) , offset , q4 , &
         & setid , setnum , sign , t3 , type(50)
!
! End of declarations
!
!
!     THIS SUBROUTINE BUILDS THE ELSETS FILE
!     THIS SUBROUTINE IS CALLED ONLY BY DPLTST, WHICH IS THE DRIVER OF
!     DMAP MODULE PLTSET
!     THE SUBROUITNE PLTSET OF THE PLOT MODULE HAS NOTHING TO DO WITH
!     THIS SUBROUTINE
!
!     REVISED 10/1990 BY G.CHAN/UNISYS TO INCLUDE OFFSET FOR BAR, TRIA3
!     AND QUAD4 ELEMENTS
!
   EQUIVALENCE (eid(1),elid)
   DATA name/4H CNS , 4HTRC / , ae/72/
   DATA nmsg1/14/
   DATA msg1/4H(33X , 4H,44H , 4HNO P , 4HLOTA , 4HBLE  , 4HSTRU , 4HCTUR , 4HAL E , 4HLEME , 4HNTS  , 4HEXIS , 4HT IN , 4H SET ,   &
       &4H,I8)/
   DATA br , t3 , q4/2HBR , 2HT3 , 2HQ4/
!
   b1 = 1
   b2 = b1 + Bufsiz
   b3 = b2 + Bufsiz
   CALL gopen(Mset,Buf(b3),Inprew)
   CALL gopen(Ect2,Buf(b2),Outrew)
!
   DO setnum = 1 , Nsets
      CALL fread(Mset,setid,1,0)
      DO i = 1 , Ngp
         Gp(i) = 0
      ENDDO
!
!     READ THE EXPLICIT ELEMENT NUMBERS IN THIS SET.
!
      CALL fread(Mset,nel,1,0)
      IF ( nel>=Max ) CALL mesage(-8,0,name)
      Ele(nel+1) = 0
      CALL fread(Mset,Ele,nel,0)
!
!     READ THE ELEMENT TYPES TO BE INCLUDED OR EXCLUDED IN THIS SET.
!
      CALL fread(Mset,ntypes,1,0)
      CALL fread(Mset,type,ntypes,0)
!
!     GENERATE AN ECT FOR THE ELEMENTS INCLUDED IN THIS SET.
!
      CALL gopen(Ect1,Buf(b1),Inprew)
 50   CALL read(*500,*500,Ect1,etype,1,0,i)
!
!     CHECK WHETHER OR NOT THIS ELEMENT TYPE IS TO BE EXCLUDED.
!
      mtype = -1
      labgp = 1
      IF ( etype==ae ) labgp = -2
      IF ( ntypes/=0 ) THEN
         DO i = 1 , ntypes , 2
            IF ( -etype==type(i) ) GOTO 100
         ENDDO
      ENDIF
      GOTO 150
 100  mtype = type(i+1)
!
!     THIS ELEMENT TYPE MAY BE INCLUDED AS A TYPE AND/OR SOME OF THEM
!     MAY BE INCLUDED SPECIFICALLY. READ -NGPPE- = NUMBER OF GRID
!     POINTS PER ELEMENT FOR THIS TYPE.
!
 150  CALL fread(Ect1,ngppe,1,0)
      IF ( ngppe<=0 ) THEN
!
!     SKIP THIS ELEMENT TYPE (NON-EXISTENT)
!
         CALL fread(Ect1,0,0,1)
         GOTO 50
      ELSE
         idx = (etype-1)*Incr
         neltyp = 0
         ne16 = Ne(idx+16)
         offset = 0
         IF ( ne16==br ) offset = 6
         IF ( ne16==t3 .OR. ne16==q4 ) offset = 1
!
!     CHECK WHETHER OR NOT THIS ELEMENT TYPE IS TO BE INCLUDED.
!
         IF ( ntypes/=0 .AND. mtype<0 ) THEN
            DO i = 1 , ntypes , 2
               IF ( etype==type(i) .OR. type(i)==Ntyps+1 ) GOTO 300
            ENDDO
         ENDIF
      ENDIF
 200  DO
!
!     NOW CHECK WHETHER OR NOT ANY OF THE ELEMENTS OF THIS TYPE ARE
!     EXPLICITLY INCLUDED. PUT ALL SUCH ON THE NEW ECT (ECT2).
!
         CALL read(*450,*450,Ect1,eid,2,0,i)
         CALL fread(Ect1,gpts,ngppe,0)
         IF ( offset/=0 ) CALL fread(Ect1,off,offset,0)
         IF ( nel>0 ) THEN
            m = 0
            n = 1
!
!     FOR TYPES DELETED ONLY SEARCH LIST AFTER TYPE WAS KNOWN TO BE
!     DELETED (2ND WORD OF TYPE)
!
            IF ( mtype>0 ) n = mtype
            IF ( n<=nel ) EXIT
            GOTO 250
         ENDIF
      ENDDO
      DO
         CALL intlst(Ele,n,sign,n1,n2)
         IF ( sign<0 ) THEN
            IF ( elid>=n1 .AND. elid<=n2 ) m = 0
         ELSE
            IF ( elid>=n1 .AND. elid<=n2 ) m = 1
         ENDIF
         IF ( n>nel ) EXIT
      ENDDO
 250  IF ( m/=0 ) THEN
         IF ( neltyp==0 ) THEN
            CALL write(Ect2,Ne(idx+16),1,0)
            CALL write(Ect2,ngppe,1,0)
         ENDIF
         CALL write(Ect2,eid,2,0)
         CALL write(Ect2,gpts,ngppe,0)
         IF ( offset/=0 ) CALL write(Ect2,off,offset,0)
         neltyp = neltyp + 1
         DO i = 1 , ngppe
            j = gpts(i)
            Gp(j) = labgp
         ENDDO
!
!     AERO ELEMENT - CENTER ONLY LABELED
!
         IF ( etype==ae ) Gp(j) = 1
      ENDIF
      GOTO 200
!
!     THIS ELEMENT TYPE IS TO BE INCLUDED, EXCEPT THE ONES EXPLICITLY
!     EXCLUDED
!
!     ONLY SEARCH LIST AFTER TYPE WAS INCLUDED
!
 300  mtype = type(i+1)
 350  CALL read(*450,*450,Ect1,eid,2,0,i)
      CALL fread(Ect1,gpts,ngppe,0)
      IF ( offset/=0 ) CALL fread(Ect1,off,offset,0)
      IF ( nel<=0 ) GOTO 400
      m = 1
      n = 1
      IF ( mtype>0 ) n = mtype
      IF ( n>nel ) GOTO 400
      DO
         CALL intlst(Ele,n,sign,n1,n2)
         IF ( sign>0 ) THEN
            IF ( elid>=n1 .AND. elid<=n2 ) m = 1
         ELSE
            IF ( elid>=n1 .AND. elid<=n2 ) m = 0
         ENDIF
         IF ( n>nel ) THEN
            IF ( m/=0 ) EXIT
            GOTO 350
         ENDIF
      ENDDO
 400  IF ( neltyp==0 ) THEN
         CALL write(Ect2,Ne(idx+16),1,0)
         CALL write(Ect2,ngppe,1,0)
      ENDIF
      CALL write(Ect2,eid,2,0)
      CALL write(Ect2,gpts,ngppe,0)
      IF ( offset/=0 ) CALL write(Ect2,off,offset,0)
      DO i = 1 , ngppe
         j = gpts(i)
         Gp(j) = labgp
      ENDDO
!
!     AERO ELEMENT - CENTER ONLY LABELED
!
      IF ( etype==ae ) Gp(j) = 1
      neltyp = neltyp + 1
      GOTO 350
!
!     END OF NEW ECT FOR THIS ELEMENT TYPE
!
 450  IF ( neltyp>0 ) CALL write(Ect2,0,1,0)
      GOTO 50
!
!     END OF ECT FOR THIS ELEMENT SET
!
 500  CALL close(Ect1,Rew)
      CALL write(Ect2,0,0,1)
!
!     FLAG ALL GRID POINTS TO BE EXCLUDED FROM A DEFORMED SHAPE.
!
      CALL fread(Mset,ngpts,1,0)
      IF ( ngpts>=Max ) CALL mesage(-8,0,name)
      Ele(ngpts+1) = 0
      CALL fread(Mset,Ele,ngpts,1)
      IF ( ngpts>0 ) THEN
         CALL gopen(Exgpid,Buf(b1),Inprew)
         DO gpt = 1 , Ngp
            CALL fread(Exgpid,exgp,1,0)
            CALL fread(Exgpid,ingp,1,0)
            m = 0
            n = 1
            DO
               CALL intlst(Ele,n,sign,n1,n2)
               IF ( sign>0 ) THEN
                  IF ( exgp>=n1 .AND. exgp<=n2 ) m = 0
               ELSE
                  IF ( exgp>=n1 .AND. exgp<=n2 ) m = ingp
               ENDIF
               IF ( n>ngpts ) THEN
                  IF ( m/=0 ) THEN
                     IF ( Gp(m)/=-2 ) Gp(m) = -Gp(m)
                  ENDIF
                  EXIT
               ENDIF
            ENDDO
         ENDDO
         CALL close(Exgpid,Rew)
      ENDIF
!
!     GENERATE A GRID POINT LIST FOR THIS SET (CONVERT THE INTERNAL
!     GRID POINT NUMBERS TO POINTERS TO THE GRID POINTS PECULIAR TO
!     THIS SET)
!
      CALL gopen(Grid,Buf(b1),Outnor)
      ngpts = 0
      DO i = 1 , Ngp
         IF ( Gp(i)/=0 ) THEN
            ngpts = ngpts + 1
            Gp(i) = isign(ngpts,Gp(i))
         ENDIF
      ENDDO
      IF ( ngpts==0 ) THEN
         err(1) = 1
         err(2) = setid
         CALL wrtprt(Merr,err,msg1,nmsg1)
      ENDIF
!
      CALL write(Grid,ngpts,1,0)
      CALL write(Grid,Gp,Ngp,0)
      IF ( setnum/=Nsets ) CALL close(Grid,Norew)
   ENDDO
!
!     ALL DONE. THE SET DEFINITION FILE (MSET) + THE SHORT ECT FILE
!     (ECT1) WILL NOT BE NEEDED AGAIN.
!
   CALL clstab(Grid,Rew)
   CALL clstab(Ect2,Rew)
   CALL close(Mset,Rew)
END SUBROUTINE cnstrc
