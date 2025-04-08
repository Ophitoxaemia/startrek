restart: Print "Star Trek"
Print
Option angle degrees
sd=3427
sdlose=sd+19
numk=0
numcmd=0
epwr=5000
es=0
numpt=10
Dim dmg(7)
bcount=0

For d=0 To 6
 dmg(d)=0 'damage in turns to repair
Next d

Dim quad(8,8,3)
For qy=0 To 7
  For qx=0 To 7
    stars=Int(Math(rand)*6+2)
    If stars<0 Then stars=0
    bases=Int(Math(rand)+.10)
    If bases<0 Then bases=0
    klingons=Int(Math(rand)*7-3)
    If klingons<0 Then klingons=0
    numk=numk+klingons
    quad(qx,qy,0)=stars
    quad(qx,qy,1)=bases
    quad(qx,qy,2)=klingons
  Next qx
Next qy

eqx=r8()
eqy=r8()

bqx=r8() '1 borg
bqy=r8()

Dim kh(6)
Dim kx(6)
Dim ky(6)
Dim sec$(8,8)
Dim sys$(8)
sys$(0)="Short range scan"
sys$(1)="Long range scan"
sys$(2)="Impulse drive"
sys$(3)="Warp drive"
sys$(4)="Shields"
sys$(5)="Phasers"
sys$(6)="Photon torpedoes"

newquad: For sy=0 To 7
  For sx=0 To 7
    sec$(sx,sy)=" . "
  Next sx
Next sy

stars = quad(eqx,eqy,0)
bases = quad(eqx,eqy,1)
klingons = quad(eqx,eqy,2)

For s=0 To stars-1
  x=r8()
  y=r8()
  sec$(x,y)=" * "
Next s

For b=0 To bases-1
  x=r8()
  y=r8()
  sec$(x,y)="-B-"
  bx=x
  by=y
Next b

For k=0 To klingons-1
  Do
    x=r8()
    y=r8()
  Loop Until sec$(x,y)=" . "
  sec$(x,y)=" K "
  kh(k)=Int(Math(rand)*100+150)
  kx(k)=x
  ky(k)=y
Next k

'Print eqx,bqx,eqy,bqy
If eqx=bqx And eqy=bqy Then
Do
  bsx=r8()
  bsy=r8()
Loop Until sec$(bsx,bsy)=" . "
sec$(bsx,bsy)="[|]"
EndIf

Do
  esx=r8()
  esy=r8()
Loop Until sec$(esx,esy)=" . "

sec$(esx,esy)="-E-"

lp:
Print
Print "Stardate" sd
Input "Command"; cmd$
Print
numcmd=numcmd+1
For d=0 To 7
  If dmg(d)>0 Then
    dmg(d)=dmg(d)-1
    If dmg(d)=0 Then
      Print sys$(d) " repaired!"
    EndIf
  EndIf
Next d

If Left$(cmd$,1) = "c" Then
  Print "s - Short range scan"
  Print "l - Long range scan"
  Print "i - Impulse drive"
  Print "w - Warp drive"
  Print "r - Raise shields"
  Print "p - Phasers"
  Print "t - Photon torpedoes"
  Print "d - Damage report"
  Print "o - End game"
  GoTo lp
EndIf

If Left$(cmd$,1) = "s" Then
  If klingons > 0 Then
    Print "    R E D  A L E R T"
  EndIf
  If dmg(0)>0 Then
    Print "Short range sensors damaged."
    GoTo skip
  EndIf
  For sy=0 To 7
    For sx=0 To 7
      Print sec$(sx, sy);
    Next sx
    If sy=0 Then
      Print "Quadrant  " eqx "-"; eqy
    ElseIf sy=1 Then
      Print "Klingons  "numk
    ElseIf sy=2 Then
      Print "Energy    "epwr
    ElseIf sy=3 Then
      Print "Photon Ts "numpt
    ElseIf sy=4 Then
      Print "Shields   "es
    ElseIf sy=5 Then
      Print "Mission SD"sdlose
    Else
      Print
    EndIf
  Next sy
  GoTo skipscan
EndIf

If Left$(cmd$,1) = "l" Then
  If dmg(1)>0 Then
    Print "Long range sensors damaged"
    GoTo skip
  EndIf
  epwr=epwr-50
  Print "    Quadrant  " eqx "-"; eqy
  For y = eqy-1 To eqy+1
    For x = eqx-1 To eqx+1
  If x>=0 And x<8 And y>=0 And y<8 Then
      Print quad(x,y,0) quad(x,y,1);
      Print quad(x,y,2); "  ";
    Else
      Print " - - -  ";
    EndIf
    Next x
  Print
  Next y
EndIf

If Left$(cmd$,1) = "w" Then
If dmg(2)>0 Then
  Print "Warp engines under repair"
  GoTo skip
EndIf
  Print "Warp engines ready"
  Call "diag"
  Input "Course"; course
  Print
  Input "Warp factor"; power
  epwr=epwr-power*25
  deg = (course-1)*360/8
  peqx=eqx : peqy=eqy
  eqx = Cint(eqx+Cos(deg)*power)
  eqy = Cint(eqy-Sin(deg)*power)
  If eqx<0 Then
    eqx=0
    Call "bounce"
  ElseIf eqx>7 Then
    eqx=7
    Call "bounce"
  EndIf
  If eqy<0 Then
    eqy=0
    Call "bounce"
  ElseIf eqy>7 Then
    eqy=7
    Call "bounce"
  EndIf
  If peqx <> eqx Or peqy <> eqy Then
    GoTo newquad
  EndIf
EndIf

If Left$(cmd$,1) = "i" Then
  If dmg(3)>0 Then
    Print "Impulse engines under repair"
    GoTo skip
  EndIf
  Print "Impulse engines ready"
  Call "diag"
  Input "course"; course
  Print
  Input "power"; power
  epwr=epwr-power*5
  deg = (course-1)*360/8
  sec$(esx,esy)=" . "
  esx = Cint(esx+Cos(deg)*power)
  esy = Cint(esy-Sin(deg)*power)
  sec$(esx,esy)="-E-"
  b1 = bx=esx-1 And by=esy
  b2 = bx=esx+1 And by=esy
  b3 = by=esy-1 And bx=esx
  b4 = by=esy+1 And bx=esx
  If b1 Or b2 Or b3 Or b4 Then
    epwr=5000
    numpt=10
    Print
    Print "Ship resupplied!"
  EndIf
EndIf

If Left$(cmd$,1) = "p" Then
  If dmg(4)>0 Then
    Print "Phasers are under repair"
    GoTo skip
  EndIf
  If dmg(0)>0 Then
Print "Phasers require short ";
Print "range sensors"
    GoTo skip
  EndIf

  Print "Phasers ready"
  Input "Power"; pp
  epwr=epwr-pp
  If klingons>0 Then
    eachpwr=pp/klingons
    For k=0 To 5
      If kh(k)>0 Then
      'Print kh(k) eachpwr
        kh(k)=kh(k)-eachpwr
        If kh(k)<0 Then
          sec$(kx(k),ky(k))=" . "
quad(eqx,eqy,2)=quad(eqx,eqy,2)-1
          Call "checkk"
        EndIf
      EndIf
    Next k
  EndIf
EndIf

If Left$(cmd$,1) = "t" Then
  If dmg(5)>0 Then
    Print "Torpedos under repair"
    GoTo skip
  EndIf
  If numpt<1 Then
    Print "Out of torpedoes"
    GoTo skip
  EndIf
  Call "diag"
  Input "Aim"; paim
  numpt=numpt-1
  Print
  Print "Tracking torpedo..."
  deg = (paim-1)*360/8
  For c=1 To 8
    CPU sleep 1
    tx = Cint(esx+Cos(deg)*c)
    ty = Cint(esy-Sin(deg)*c)
    If tx<0 Or tx>7 Or ty<0 Or ty>7 Then
      Exit For
    EndIf
    Print tx,ty
    If sec$(tx,ty)=" * " Then
      Print "Torpedo hit a star."
      Exit For
    ElseIf sec$(tx,ty)=" K " Then
      done=true
      sec$(tx,ty)=" . "
      For k=0 To 5
        If kx(k)=tx And ky(k)=ty Then
          kh(k)=0
quad(eqx,eqy,2)=quad(eqx,eqy,2)-1
          Call "checkk"
        EndIf
      Next k
      Exit For
    EndIf
  Next c
EndIf

If Left$(cmd$,1) = "r" Then
  If dmg(6)>0 Then
    Print "Shields under repair"
    GoTo skip
  EndIf
  Input "Raise shields, amount"; rs
  epwr=epwr-rs
  Call "checkpwr"
  es=es+rs
EndIf

If Left$(cmd$,1) = "d" Then
Print "Status  System"
For d=0 To 6
If dmg(d)>0 Then
  Print "  "dmg(d)"    "sys$(d)
Else
  Print "  Ok    "sys$(d)
EndIf
Next d
EndIf

If Left$(cmd$,1) = "o" Then
  Print "numcmd" numcmd
  End
  Clear
  GoTo restart
EndIf

skip:
sd=sd+.2
If sd>sdlose Then
  Print "Time ran out, game over"
  End
EndIf

If klingons>0 Then
  For k=0 To 5
  If kh(k)>0 Then
  Print
  Print "Klingon at"kx(k)"-";
  Print ky(k)" fires!"
  kp = Int(Math(rand)*50+100)
  es=es-kp

  If es<=0 Then
    es=0
    Print "Shields down!"
    system=Int(Math(rand)*7)
    len=Int(Math(rand)*3+2)
    dmg(system)=dmg(system)+len
    Print sys$(system)" damaged."
  Else
    Print kp" damage to shields"
  EndIf
  EndIf
  Next k
EndIf

If bcount>0 Then
  If quad(bqx,bqy,1)>0 Then
    quad(bqx,bqy,1)=0
    bases=0
    Print "Borg have destroyed ";
    Print "the base in"bqx"-";bqy"!"
  Else
    If bqx=eqx And bqy=eqy Then
      GoTo skipb
    EndIf
    Inc bqx
    If bqx>7 Then
      bqx=0
      Inc bqy
      If bqy>7 Then
        bqy=0
      EndIf
    EndIf
Print "The Borg have moved to"bqx"-"bqy
    If bqx=eqx And bqy=eqy Then
      Do
        bsx=r8()
        bsy=r8()
      Loop Until sec$(bsx,bsy)=" . "
      sec$(bsx,bsy)="[|]"
    EndIf
  EndIf
EndIf

skipb:
If bqx=eqx And bqy=eqy Then
If bcount=0 Then
  Print "Borg scanning Enterprise..."
  Inc bcount
Else
  bdmg=Int(Math(rand)*900+100)
  Print "Borg drains"bdmg" energy"
  epwr=epwr-bdmg
  Call "checkpwr"
EndIf
EndIf

skipscan:

GoTo lp

Sub diag
  Print "   4  3  2"
  Print "    \ | /"
  Print " 5----*----1"
  Print "    / | \"
  Print "   6  7  8"
  Print
End Sub

Sub bounce
Print
Print "You hit the edge"
pwr=Int(Math(rand)*100+500)
Print "You lose"pwr" energy"
epwr=epwr-pwr
Call "checkpwr"
End Sub

Sub kdie
Print "The Klingon is destroyed!"
klingons=klingons-1
numk=numk-1
If numk<=0 Then
  Print "You won!"
  End
EndIf
End Sub

Sub checkpwr
If epwr<0 Then
Print "Enterprise out of power, game ove"
r""
End
EndIf
End Sub

Sub checkk
Print
Print "Klingon at"kx(k)"-";
Print ky(k)" is destroyed!"
klingons=klingons-1
numk=numk-1
If numk<=0 Then
  Print "You won!"
  End
EndIf

End Sub

Function r8()
  r8=Int(Math(rand)*8)
End Function
