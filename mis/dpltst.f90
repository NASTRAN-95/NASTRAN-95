
SUBROUTINE dpltst
!
   IMPLICIT NONE
   INTEGER Bufsiz , Ect , Elset , Eqexin , Gpset , Merr , Mset , Ngp , Nsets , Parm , Pcdb , Pect , Skp1(8) , Skp2(7) , Skp3(6) ,   &
         & X(1)
   COMMON /blank / Ngp , Nsets , Skp1 , Pcdb , Eqexin , Ect , Skp2 , Merr , Parm , Gpset , Elset , Skp3 , Mset , Pect
   COMMON /system/ Bufsiz
   COMMON /zzzzzz/ X
   INTEGER ept , errttl(32) , i1 , i2 , i3 , outrew , rew
   INTEGER korsz
   DATA outrew , rew/1 , 1/
   DATA errttl/8*2H   , 4HERRO , 4HR ME , 4HSSAG , 4HES F , 4HROM  , 4HTHE  , 4HPLOT , 4H SET , 4H DEF , 4HINIT , 4HION  , 4HMODU , &
       &4HLE ( , 4HPLTS , 4HET)  , 9*1H /
!
   Nsets = 0
   Pcdb = 101
   Eqexin = 102
   Ect = 103
   ept = 104
   Merr = 201
   Parm = 202
   Gpset = 203
   Elset = 204
   Mset = 301
   Pect = 302
   CALL totape(1,X(1))
!
   X(1) = Eqexin
   CALL rdtrl(X)
   i2 = 2
   i3 = 3
   Ngp = X(i2) - X(i3)
   i1 = korsz(X) - Bufsiz + 1
   CALL gopen(Merr,X(i1),outrew)
   CALL write(Merr,-4,1,0)
   CALL write(Merr,errttl,32,0)
   CALL setinp
   IF ( Nsets/=0 ) THEN
      i1 = Nsets + 1
      i2 = i1 + Ngp
      i3 = korsz(X) - 4*Bufsiz + 1
      CALL comect(X(i2),i3-i2)
      CALL cnstrc(X(i1),X(i2),X(i3),i3-i2)
   ELSE
      Nsets = -1
   ENDIF
!
   CALL clstab(Merr,rew)
END SUBROUTINE dpltst