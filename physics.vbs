' General physics originally taken from the BigDeal table, but flippers have
' been redone and a lot of other stuff removed.

'****************************************
' Realtime updates
'****************************************
' Call every 10ms to update misc physics
Sub Physics_Tick
    ' Make the visual flipper match the physics flipper
    VisLeftFlipper.RotZ = LeftFlipper.CurrentAngle
    VisRightFlipper.RotZ = RightFlipper.CurrentAngle

    ' Calculate the ball velocities. Does anybody use this? FIXME
    Cor.Update
End Sub

'***************************************************************
'             Supporting Ball & Sound Functions v4.0
'  includes random pitch in PlaySoundAt and PlaySoundAtBall
'***************************************************************

Dim TableWidth, TableHeight

TableWidth = Table1.width
TableHeight = Table1.height

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / TableWidth-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = (SQR((ball.VelX ^2) + (ball.VelY ^2)))
End Function

Function AudioFade(ball) 'only on VPX 10.4 and newer
    Dim tmp
    tmp = ball.y * 2 / TableHeight-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
End Function

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0.2, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.2, Pitch(ActiveBall) * 10, 0, 0, AudioFade(ActiveBall)
End Sub

Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
End Function

'****************************************************
'   JP's VPX Rolling Sounds with ball speed control
'****************************************************

Const tnob = 5    'total number of balls used mostly for testing
Const lob = 0     'number of locked balls
Const maxvel = 40 ' Max ball velocity FIXME: Important! 40+ for modern tables
ReDim rolling(tnob) ' USes tnob as upper index not size (because VBScript)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

' Call every 10 ms for rolling sounds
Sub Rolling_Tick()
    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball
    Dim limit
    If UBound(BOT) > tnob Then
        limit = tnob
    Else
        limit = UBound(BOT)
    End If
    For b = lob to limit
        If BallVel(BOT(b) )> 1 Then
            If BOT(b).z <0 Then
                ballpitch = Pitch(BOT(b) ) - 5000 'decrease the pitch under the playfield
                ballvol = Vol(BOT(b) )
            ElseIf BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b) )
                ballvol = Vol(BOT(b) )
            Else
                ballpitch = Pitch(BOT(b) ) + 25000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b) ) * 3
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b) ), 0, ballpitch, 1, 0, AudioFade(BOT(b) )
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

        ' dropping sounds
        If BOT(b).VelZ <-1 Then
            'from ramp
            If BOT(b).z <55 and BOT(b).z> 27 Then PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
            'down a hole
            If BOT(b).z <10 and BOT(b).z> -10 Then PlaySound "fx_hole_enter", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        End If

        ' Ball speed & spin control was here, but it shouldn't have been and
        ' it wasn't good.
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'***************************************
'     Collection collision sounds
'***************************************

Sub aMetals_Hit(idx):PlaySoundAtBall "fx_metalhit":End Sub
Sub aMetalWires_Hit(idx):PlaySoundAtBall "fx_metalwire":End Sub
Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_plastichit":End Sub
Sub aGates_Hit(idx):PlaySoundAtBall "fx_gate":End Sub
Sub aWoods_Hit(idx):PlaySoundAtBall "fx_woodhit":End Sub

Dim Cor
Set Cor = New CorTracker

' Call every 10 ms for rubber dampening... which isn't implemented yet FIXME
Class CorTracker
    Public ballvel, ballvelx, ballvely

    Private Sub Class_Initialize()
        ReDim ballvel(0)
        ReDim ballvelx(0)
        ReDim ballvely(0)
    End Sub

    ' Call from a ~10ms timer to track ball velocity
    Public Sub Update()
        Dim b, AllBalls, HighestID
        AllBalls = GetBalls ' GetBalls = VPX API that returns Ball objects

        For Each b in AllBalls
            If b.ID > HighestID then HighestID = b.ID
        Next

        If UBound(ballvel) < HighestID Then ReDim ballvel(HighestID)
        if UBound(ballvelx) < HighestID Then ReDim ballvelx(HighestID)
        if UBound(ballvely) < HighestID Then ReDim ballvely(HighestID)

        For Each b in AllBalls
            ballvel(b.id) = BallSpeed(b)
            ballvelx(b.id) = b.VelX
            ballvely(b.id) = b.VelY
        Next
    End Sub
End Class

Function BallSpeed(ball)
    BallSpeed = Sqr(ball.VelX ^ 2 + ball.VelY ^ 2 + ball.VelZ ^ 2)
End Function

Function Distance(ax, ay, bx, by)
    Distance = Sqr((ax - bx) ^ 2 + (ay - by) ^ 2)
End Function