' Flipper physics, ball rolling and collision sounds, etc. taken from
' the Big Deal table. This will normally not need to be modified and
' is included by the table script at run time for development builds.
'
' Various global variables are accessed and created, so conflicts are
' likely because VBScript
'
' Table Requirements:
'   Table1 = The table (using its default name)
'   RollingTimer = A 10ms timer which is set disabled in the editor.
'       This is used to create the ball rolling sounds.
'   LeftFlipper = The left flipper
'   RightFlipper = The right flipper
'   GameTimer = A 10ms timer which is set enabled in the editor.
'       This rotates the flipper tops and gate tops to match the base parts.
'
' For correct collision sounds, items should be placed in a collection in
' the editor based on their materials:
'   aMetals, aMetalWires, aRubber_Bands, aRubber_Posts, aRubber_Pins,
'   aPlastics, aGates, aWoods
'
' Call "SolLFlipper" and "SolRFlipper" when flipper keys are hit

'****************************************
' Realtime updates
'****************************************
' A 10ms timer that rotates top parts to match the base parts
Sub GameTimer_Timer
    VisLeftFlipper.RotZ = LeftFlipper.CurrentAngle
    VisRightFlipper.RotZ = RightFlipper.CurrentAngle
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
    RollingTimer.Enabled = 1
End Sub

' A 10ms timer to do ball rolling sounds which is enabled when needed
Sub RollingTimer_Timer()
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

        ' jps ball speed & spin control
        ' FIXME This is astonishingly bad code
        'BOT(b).AngMomZ = BOT(b).AngMomZ * 0.95
        'If BOT(b).VelX AND BOT(b).VelY <> 0 Then
        '    speedfactorx = ABS(maxvel / BOT(b).VelX)
        '    speedfactory = ABS(maxvel / BOT(b).VelY)
        '    If speedfactorx <1 Then
        '        BOT(b).VelX = BOT(b).VelX * speedfactorx
        '        BOT(b).VelY = BOT(b).VelY * speedfactorx
        '    End If
        '    If speedfactory <1 Then
        '        BOT(b).VelX = BOT(b).VelX * speedfactory
        '        BOT(b).VelY = BOT(b).VelY * speedfactory
        '    End If
        '    If speedfactorx < 1 Or speedfactory < 1 Then
        '        debug.print "Ball Speed Limited " + CStr(maxvel)
        '    End If
        'End If
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

'**********************************************
'    Flipper adjustments - enable tricks
'             by JLouLouLou
'**********************************************

Dim FlipperPower
Dim FlipperElasticity
Dim SOSTorque, SOSAngle
Dim FullStrokeEOS_Torque, LiveStrokeEOS_Torque
Dim LeftFlipperOn
Dim RightFlipperOn

Dim LLiveCatchTimer
Dim RLiveCatchTimer
Dim LiveCatchSensivity

FlipperPower = 5000
FlipperElasticity = 0.85
FullStrokeEOS_Torque = 0.3  ' EOS Torque when flipper hold up ( EOS Coil is fully charged. Ampere increase due to flipper can't move or when it pushed back when "On". EOS Coil have more power )
LiveStrokeEOS_Torque = 0.2  ' EOS Torque when flipper rotate to end ( When flipper move, EOS coil have less Ampere due to flipper can freely move. EOS Coil have less power )

'LeftFlipper.EOSTorqueAngle = 10
'RightFlipper.EOSTorqueAngle = 10

SOSTorque = 0.1
SOSAngle = 6

LiveCatchSensivity = 10

LLiveCatchTimer = 0
RLiveCatchTimer = 0

' Flipper objects have built in timers
'LeftFlipper.TimerInterval = 1
'LeftFlipper.TimerEnabled = 1

Sub LeftFlipper_Timer 'flipper's tricks timer
'Start Of Stroke Flipper Stroke Routine : Start of Stroke for Tap pass and Tap shoot
'    If LeftFlipper.CurrentAngle >= LeftFlipper.StartAngle - SOSAngle Then LeftFlipper.Strength = FlipperPower * SOSTorque else LeftFlipper.Strength = FlipperPower : End If

'End Of Stroke Routine : Livecatch and Emply/Full-Charged EOS
'    If LeftFlipperOn = 1 Then
'        If LeftFlipper.CurrentAngle = LeftFlipper.EndAngle then
'            LeftFlipper.EOSTorque = FullStrokeEOS_Torque
'            LLiveCatchTimer = LLiveCatchTimer + 1
'            If LLiveCatchTimer < LiveCatchSensivity Then
'                LeftFlipper.Elasticity = 0
'            Else
'                LeftFlipper.Elasticity = FlipperElasticity
'                LLiveCatchTimer = LiveCatchSensivity
'            End If
'        End If
'    Else
'        LeftFlipper.Elasticity = FlipperElasticity
'        LeftFlipper.EOSTorque = LiveStrokeEOS_Torque
'        LLiveCatchTimer = 0
'    End If


'Start Of Stroke Flipper Stroke Routine : Start of Stroke for Tap pass and Tap shoot
'    If RightFlipper.CurrentAngle <= RightFlipper.StartAngle + SOSAngle Then RightFlipper.Strength = FlipperPower * SOSTorque else RightFlipper.Strength = FlipperPower : End If

'End Of Stroke Routine : Livecatch and Emply/Full-Charged EOS
'    If RightFlipperOn = 1 Then
'        If RightFlipper.CurrentAngle = RightFlipper.EndAngle Then
'            RightFlipper.EOSTorque = FullStrokeEOS_Torque
'            RLiveCatchTimer = RLiveCatchTimer + 1
'            If RLiveCatchTimer < LiveCatchSensivity Then
'                RightFlipper.Elasticity = 0
'           Else
'                RightFlipper.Elasticity = FlipperElasticity
'                RLiveCatchTimer = LiveCatchSensivity
'            End If
'        End If
'    Else
'        RightFlipper.Elasticity = FlipperElasticity
'        RightFlipper.EOSTorque = LiveStrokeEOS_Torque
'        RLiveCatchTimer = 0
'    End If

End Sub
