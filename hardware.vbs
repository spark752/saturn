' ***********************
'  Table Hardware Events
' ***********************

' Key Handlers
' Ultimately these should only handle hardware things, depending only on
' simulated hardware state and triggering events to the software if needed.
Sub Table1_KeyDown(ByVal Keycode)
    ' Add coins. This is a (simulated) mechanical mechanism making a mechanical
    ' sound, but there should probably be some states where a credit is not
    ' added and the coin is returned. Not a big priority to change right now.
    If Keycode = AddCreditKey Then
        swCredit_Hit
        PlaySoundAt "fx_coin", plunger
    End If

    ' Plunger. This is a simulated mechanical plunger which moves and makes a
    ' mechanical sound so it doesn't matter what state the machine is in.
    If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySoundAt "fx_plungerpull", plunger
    End If

    ' Nudge. VPX has tilt sensor hardware simulation that is currently not used
    ' probably because it's newer than the starting code. "Nudge" is a VPX
    ' global API that takes an angle and a force. "TiltMech.Nudge" is a platform
    ' API which registers the effect of being nudged towards possible tilt.
    ' Nudging and detection thereof is a physical thing that should happen no
    ' matter the state of the machine.
    Const NUDGE_FORCE = 5
    If keycode = LeftTiltKey Then
        Nudge 90, NUDGE_FORCE
        TiltMech.Nudge
        PlaySound "fx_nudge", 0, 1, -0.1, 0.25
    End If
    If keycode = RightTiltKey Then
        Nudge 270, NUDGE_FORCE
        TiltMech.Nudge
        PlaySound "fx_nudge", 0, 1, 0.1, 0.25
    End If
    If keycode = CenterTiltKey Then
        Nudge 0, NUDGE_FORCE
        TiltMech.Nudge
        PlaySound "fx_nudge", 0, 1, 1, 0.25
    End If

    ' Flippers. The hardware circuit connects the flipper buttons to the coils
    ' without the CPU being involved. The CPU controls power to the circuit
    ' through the coil relay.
    If keycode = LeftFlipperKey Then
        swLeftFlipper_Hit
        If Coils.IsPowerOn Then FlipperControl.LeftActivate
    End If
    If keycode = RightFlipperKey Then
        swRightFlipper_Hit
        If Coils.IsPowerOn Then FlipperControl.RightActivate
    End If

    ' Start game button. This is only handled by software.
    If keycode = StartGameKey Then swStart_Hit
End Sub

Sub Table1_KeyUp(ByVal keycode)
    ' Plunger. This is a simulated mechanical plunger which moves and makes a
    ' mechanical sound so it doesn't matter what state the machine is in.
    ' But the sound will be different if it is hitting a ball, so a check is
    ' mad
    If keycode = PlungerKey Then
        Plunger.Fire
        If IsInPlungerLane Then
            PlaySoundAt "fx_plunger", plunger
        Else
            PlaySoundAt "fx_plunger_empty", plunger
        End If
    End If

    ' Flippers. See KeyDown for circuit details. If the power turns off,
    ' flippers should be released anyway so KeyUp can also be ignored.
    If Coils.IsPowerOn Then
        If keycode = LeftFlipperKey Then
            FlipperControl.LeftDeactivate
        End If
        If keycode = RightFlipperKey Then
            FlipperControl.RightDeactivate
        End If
    End If
End Sub

Sub Table1_Paused
End Sub

Sub Table1_UnPaused
End Sub

' *************
'  Ball Trough
' *************
' This attempts to simulate a modern single coil trough design with 4 balls.
' In real hardware the ball rolls from the drain into a trough under the
' playfield without needing a drain coil. A coil is used to eject a ball
' vertically from the other end of the trough to put it in play.
' The simulation uses kickers for the trough positions so that balls can
' be placed "uphill" from each other to get to the release point.
' Initialization will create a ball in each of the 4 trough positions.
' The system may be able to function with fewer than 4 balls or even with a 5th
' ball stored in the drain kicker.
Class hwTrough
    Private Sub Class_Initialize()
        Const BALL_RADIUS = 25
        Const BALL_MASS = 1
        Trough1.CreateSizedBallWithMass BALL_RADIUS, BALL_MASS
        Trough2.CreateSizedBallWithMass BALL_RADIUS, BALL_MASS
        Trough3.CreateSizedBallWithMass BALL_RADIUS, BALL_MASS
        Trough4.CreateSizedBallWithMass BALL_RADIUS, BALL_MASS
    End Sub

    Public Sub Tick()
        Const TROUGH_KICK_SPEED = 30
        Const TROUGH_KICK_ANGLE = 60
        If Drain.BallCntOver > 0 And Trough4.BallCntOver = 0 Then
            Drain.Kick TROUGH_KICK_ANGLE, TROUGH_KICK_SPEED
        End If
        If Trough4.BallCntOver > 0 And Trough3.BallCntOver = 0 Then
            Trough4.Kick TROUGH_KICK_ANGLE, TROUGH_KICK_SPEED
        End If
        If Trough3.BallCntOver > 0 And Trough2.BallCntOver = 0 Then
            Trough3.Kick TROUGH_KICK_ANGLE, TROUGH_KICK_SPEED
        End If
        If Trough2.BallCntOver > 0 And Trough1.BallCntOver = 0 Then
            Trough2.Kick TROUGH_KICK_ANGLE, TROUGH_KICK_SPEED
        End If
    End Sub

    Public Sub Release()
        Trough1.Kick 90, 4
    End Sub
End Class

Sub Drain_Hit()
    PlaySoundAt "Drain_10", Drain ' Rolling down the trough
    swDrain_Hit
End Sub

' ************
'  Slingshots
' ************
' Slingshots are implemented much the same as the Rubbers section above, but
' with four bands to switch between to show a large deflection. There is also
' a kicker mesh which is animated via rotation.
'
' The slingshots are disabled when tilted so there is no reason to check
' that here.
Dim LStep, RStep

Sub LeftSlingShot_Slingshot()
    PlaySoundAt "fx_slingshot", LSlingMech
    LSling.Visible = False
    LSling1.Visible = True
    LSlingMech.RotX = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    swSlingshot_Hit
End Sub

Sub LeftSlingShot_Timer()
    Select Case LStep
        Case 3:
            LSling1.Visible = False
            LSling2.Visible = True
            LSlingMech.RotX = 10
        Case 4:
            LSling2.Visible = False
            LSling.Visible = True
            LSlingMech.RotX = 0
            LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot()
    PlaySoundAt "fx_slingshot", RSlingMech
    RSling.Visible = False
    RSling1.Visible = True
    RSlingMech.RotX = 20
    RStep = 0
    RightSlingShot.TimerEnabled = True
    swSlingshot_Hit
End Sub

Sub RightSlingShot_Timer()
    Select Case RStep
        Case 3:
            RSling1.Visible = False
            RSling2.Visible = True
            RSlingMech.RotX = 10
        Case 4:
            RSling2.Visible = False
            RSling.Visible = True
            RSlingMech.RotX = 0
            RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

' *********
'  Bumpers
' *********
' Bumpers function as standard VPX bumpers. The bumper threshold is raised
' to an impractical level when the table is tilted so there is no need to
' check for that here.
Sub Bumper001_Hit()
    PlaySoundAt "fx_Bumper", Bumper001
    swBumper_Hit
End Sub

Sub Bumper002_Hit()
    PlaySoundAt "fx_Bumper", Bumper002
    swBumper_Hit
End Sub

Sub Bumper003_Hit()
    PlaySoundAt "fx_Bumper", Bumper003
    swBumper_Hit
End Sub

' ****************
'  Tilt Mechanism
' ****************
Class hwTiltMech
    Private TiltValue

    Private Sub Class_Initialize()
        Reset
    End Sub

    Public Sub Reset()
        TiltValue = 0
    End Sub

    Public Sub Nudge()
        Const TILT_SENSITIVITY = 6
        Const MAX_TILT = 15
        TiltValue = TiltValue + TILT_SENSITIVITY
        If TiltValue > MAX_TILT Then
            SetTilted True
        End If
    End Sub

    Public Sub Tick()
        ' Accumulated tilt decays over time
        If TiltValue > 0 Then
            TiltValue = TiltValue - 0.1
        End If
    End Sub
End Class

' **********************
'  CPU Controlled Coils
' **********************
Class hwCoils
    Private mIsPowerOn ' Relay for coil power to flippers, bumpers, etc.

    Private Sub Class_Initialize()
        mIsPowerOn = False
    End Sub
    Public Property Get IsPowerOn()
        IsPowerOn = mIsPowerOn
    End Property
    Public Property Let IsPowerOn(Enabled)
        Const BUMPER_ON_THRESHOLD = 1.5
        Const BUMPER_OFF_THRESHOLD = 100

        If Enabled Then
            If Not mIsPowerOn Then
                PlaySound "fx_relay"
                Bumper001.Threshold = BUMPER_ON_THRESHOLD
                Bumper002.Threshold = BUMPER_ON_THRESHOLD
                Bumper003.Threshold = BUMPER_ON_THRESHOLD
                LeftSlingShot.Disabled = False
                RightSlingShot.Disabled = False
                mIsPowerOn = True
            End If
        Else
            If mIsPowerOn Then
                PlaySound "fx_relay"
                Bumper001.Threshold = BUMPER_OFF_THRESHOLD
                Bumper002.Threshold = BUMPER_OFF_THRESHOLD
                Bumper003.Threshold = BUMPER_ON_THRESHOLD
                LeftSlingShot.Disabled = True
                RightSlingShot.Disabled = True
                FlipperControl.BothDeactivate
                mIsPowerOn = False
            End If
        End If
    End Property
End Class

' *********************
'  Non Volatile Memory
' *********************
Class hwNV
    Private DEFAULT_HIGH_SCORE ' Would be class level const if those worked
    Public HighScore ' Just one for now
    Public GameCount ' Number of games finished
    Public PlayTime ' Total time played in seconds

    Private Sub Class_Initialize()
        DEFAULT_HIGH_SCORE = 10000
        Load
    End Sub
    Private Sub Class_Terminate()
        Save
    End Sub

    Private Sub Load()
        Dim x
        x = LoadValue(TableName, "HighScore1")
        If IsNumeric(x) Then
            HighScore = CDbl(x)
        Else
            HighScore = DEFAULT_HIGH_SCORE
        End If
        x = LoadValue(TableName, "GameCount")
        If IsNumeric(x) Then
            GameCount = CDbl(x)
        Else
            GameCount = 0
        End If
        x = LoadValue(TableName, "PlayTime")
        If IsNumeric(x) Then
            PlayTime = CDbl(x)
        Else
            PlayTime = 0.0
        End If
    End Sub
    Public Sub Save()
        SaveValue TableName, "HighScore1", HighScore
        SaveValue TableName, "GameCount", GameCount
        SaveValue TableName, "PlayTime", PlayTime
    End Sub
    Public Sub Reset()
        HighScore = DEFAULT_HIGH_SCORE
        GameCount = 0
        PlayTime = 0.0
    End Sub

    Public Sub EndOfGame(pScore, pPlayTime)
        If pScore > HighScore Then HighScore = pScore
        GameCount = GameCount + 1
        PlayTime = PlayTime + pPlayTime
    End Sub
End Class