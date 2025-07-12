Const DMD_GREY_4 = 1
Const DMD_RGB = 2
Const ALIGN_TOP_LEFT = 0
Const ALIGN_TOP = 1
Const ALIGN_TOP_RIGHT = 2
Const ALIGN_LEFT = 3
Const ALIGN_CENTER = 4
Const ALIGN_RIGHT = 5
Const ALIGN_BOTTOM_LEFT = 6
Const ALIGN_BOTTOM = 7
Const ALIGN_BOTTOM_RIGHT = 8

Const DISP_SCOREBOARD = 0
Const DISP_ONE_ROW = 1
Const DISP_TWO_ROWS = 2
Const DISP_TWO_ROWS_SEP = 3
Const DISP_SATURN = 4

Class hwDisplay
    Private mFlexDMD
    Private mScenes(10) ' VBScript doesn't support Const for array size
    Private mCurrentScene, mNewScene, mNewText1, mNewText2, mText1, mText2
    Private mIsAttractMode, mNextSeqTime, mSeqScreen

    Private Sub Class_Initialize()
        ' Depends on global "TableName" since it can't use a parameter.
        ' That value ends up in FlexDMD.GameName and will be used for storing
        ' table specific settings in the FlexDMD .ini file.
        ' The VPX filename ends up in FlexDMD.TableFile. This apparently allows
        ' images stored in the VPX to be accessed as "VPX.imagename".
        ' FlexDMD can also load external files and that is often done in demos
        ' since it is easier for development, but not as good for distribution.
        Set mFlexDMD = CreateObject("FlexDMD.FlexDMD")
        If mFlexDMD is Nothing Then
            MsgBox "No FlexDMD Found"
            Exit Sub
        End If
        With mFlexDMD
            .GameName = TableName
            .TableFile = Table1.Filename & ".vpx"
            .Color = RGB(255, 88, 32) ' Classic orange as default...
            .RenderMode = DMD_RGB ' ...but RGB mode
            .Width = 128
            .Height = 32
            .Clear = True
            .Run = True
        End With
        CreateScenes
        mNewText1 = " "
        mNewText2 = " "
        mText1 = " "
        mText2 = " "
        mIsAttractMode = False
        mNextSeqTime = GameTime
        mSeqScreen = -1
        mNewScene = DISP_ONE_ROW
        mCurrentScene = DISP_SCOREBOARD
    End Sub
    Private Sub Class_Terminate()
        debug.print "hwDisplay Terminate"
        If Not mFlexDMD Is Nothing Then ' IsNot doesn't seem to work
            mFlexDMD.Show = False
            mFlexDMD.Run = False
            mFlexDMD = NULL
        End If
    End Sub

    Private Sub CreateScenes()
        Dim Font1, Font2, Font3, Font4
        Const ORANGE_TEXT = &H2058FF ' FlexDMD fonts have R in the LSB...
        Const ORANGE_LINE = &HFF5820 ' ...but frames have R in the MSB ???

        ' From FlexDMD API:
        ' NewFont(string font, Color tint, Color borderTint, int borderSize)
        '   where borderSize must be 0 or 1
        ' Font names can be found by looing at the FlexDMD source code.
        ' A 12x24 font provides one line with 9 characters on a standard
        ' 128x32 display, enough for "GAME OVER" but not much else unless
        ' it scrolls.
        Set Font1 = mFlexDMD.NewFont("FlexDMD.Resources.udmd-f7by13.fnt", vbYellow, vbWhite, 0)
        Set Font2 = mFlexDMD.NewFont("FlexDMD.Resources.udmd-f5by7.fnt", vbCyan, vbWhite, 0)
        Set Font3 = mFlexDMD.NewFont("FlexDMD.Resources.udmd-f12by24.fnt", ORANGE_TEXT, vbWhite, 0)
        Set Font4 = mFlexDMD.NewFont("FlexDMD.Resources.udmd-f7by13.fnt", ORANGE_TEXT, vbWhite, 0)

        ' Labels with fixed width or placement may be aligned here. Labels
        ' that change width (like scores that add digits) need to be aligned
        ' after updating the text. To avoid duplicate code with magic numbers,
        ' those labels should not be aligned here and the system should be
        ' sure not to display them until they have been updated.

        ' Basic score board with score, ball number, and credits
        Set mScenes(DISP_SCOREBOARD) = mFlexDMD.NewGroup("ScoreBoard")
        With mScenes(DISP_SCOREBOARD)
            .AddActor mFlexDMD.NewLabel("Score", Font1, "0")
            .AddActor mFlexDMD.NewLabel("Ball", Font2, "GAME OVER")
            .GetLabel("Ball").SetAlignedPosition 2, 31, ALIGN_BOTTOM_LEFT
            .AddActor mFlexDMD.NewLabel("Credits", Font2, "CREDITS 0")
            .GetLabel("Credits").SetAlignedPosition 126, 31, ALIGN_BOTTOM_RIGHT
        End With

        ' Single line of large text. It can't fit many characters.
        Set mScenes(DISP_ONE_ROW) = mFlexDMD.NewGroup("OneRow")
        mScenes(DISP_ONE_ROW).AddActor mFlexDMD.NewLabel("Text", Font3, " ")

        ' Two lines of text without horizontal separator
        Set mScenes(DISP_TWO_ROWS) = mFlexDMD.NewGroup("TwoRows")
        mScenes(DISP_TWO_ROWS).AddActor mFlexDMD.NewLabel("Top", Font4, " ")
        mScenes(DISP_TWO_ROWS).AddActor mFlexDMD.NewLabel("Bottom", Font4, " ")

        ' Two lines of text. There is a horizontal separator to try and avoid
        ' the "You matter don't give up" meme.
        Set mScenes(DISP_TWO_ROWS_SEP) = mFlexDMD.NewGroup("TwoRowsSep")
        With mScenes(DISP_TWO_ROWS_SEP)
            .AddActor mFlexDMD.NewLabel("Top", Font4, " ")
            .AddActor mFlexDMD.NewLabel("Bottom", Font4, " ")
            .AddActor mFlexDMD.NewFrame("HSeparator")
            With .GetFrame("HSeparator")
                .Thickness = 1 ' 1 pixel wide frame bounded into a 2 wide line
                .SetBounds 0, 15, 128, 2 ' X, Y, W, H
                .BorderColor = ORANGE_LINE
            End With
        End With

        ' Image of Saturn since this system is apparently called that.
        ' This loading consistantly failed with an exception in FlexDMD 1.8.0.
        Set mScenes(DISP_SATURN) = mFlexDMD.NewGroup("Saturn")
        mScenes(DISP_SATURN).AddActor mFlexDMD.NewImage("Logo", "VPX.saturn")
    End Sub

    Private Sub AttractModeTick()
        Const SEQ_INTERVAL = 2000 ' Time per screen in ms

        If GameTime > mNextSeqTime Then
            mSeqScreen = mSeqScreen + 1
            If mSeqScreen > 3 Then mSeqScreen = 0
            mNextSeqTime = GameTime + SEQ_INTERVAL
            Select Case mSeqScreen
                Case 0
                    ' Tell the user how to start a game (keep as first screen)
                    If IsFreePlay Then
                        SetText "FREE PLAY", "PRESS START"
                    ElseIf Credits > 0 Then
                        SetText "CREDITS " & Credits, "PRESS START"
                    Else
                        SetText "CREDITS " & Credits, "INSERT COIN"
                    End If
                    SetScene DISP_TWO_ROWS
                Case 1
                    ' Highscore with separators but no decimal
                    SetText "HIGHEST SCORE", FormatNumber(NV.HighScore, 0, -1, 0, -1)
                    SetScene DISP_TWO_ROWS
                Case 2
                    ' Scoreboard so user doesn't miss seeing their final score
                    SetScene DISP_SCOREBOARD
                Case 3
                    ' Saturn logo
                    SetScene DISP_SATURN
            End Select
        End If
    End Sub

    Public Sub SetScene(pIndex)
        ' Actual scene change will be done in Tick so that the labels are
        ' updated first
        If pIndex <= UBound(mScenes) Then
            If Not mScenes(pIndex) Is Nothing Then
                mNewScene = pIndex
            End If
        End If
    End Sub
    Public Sub SetText(pText1, pText2)
        mNewText1 = pText1
        mNewText2 = pText2
    End Sub
    Public Sub AttractMode(pEnabled)
        If pEnabled And Not mIsAttractMode Then
            mSeqScreen = -1 ' First increment will be first screen
            mNextSeqTime = GameTime + 3000 ' Stay on current screen for a bit
        End If
        mIsAttractMode = pEnabled
    End Sub
    Public Sub CoinIn()
        If mIsAttractMode Then
            mSeqScreen = -1 ' First increment will be credits screen
            mNextSeqTime = GameTime ' Change to that screen immediately
        End If
    End Sub

    ' Call from timer for each frame
    Public Sub Tick()
        Dim Label

        ' Cycle through screens for attract mode sequence if relevant
        If mIsAttractMode Then AttractModeTick

        ' Prevent rendering thread access while updating
        mFlexDMD.LockRenderThread

        ' Scene change?
        If mCurrentScene <> mNewScene Then
            mCurrentScene = mNewScene ' Validated when mNewScene was set
            mFlexDMD.Stage.RemoveAll
            mFlexDMD.Stage.AddActor mScenes(mCurrentScene)
        End If

        ' Update content
        mText1 = mNewText1
        mText2 = mNewText2
        Select Case mCurrentScene
            Case DISP_SCOREBOARD
                ' Score has separators but no decimal
                Set Label = mFlexDMD.Stage.GetLabel("Score")
                Label.Text = FormatNumber(Score, 0, -1, 0, -1)
                Label.SetAlignedPosition 64, 4, ALIGN_TOP
                Set Label = mFlexDMD.Stage.GetLabel("Ball")
                If BallNumber < 1 Then
                    Label.Text = "GAME OVER"
                Else
                    Label.Text = "BALL " & BallNumber
                End If
                Set Label = mFlexDMD.Stage.GetLabel("Credits")
                If IsFreePlay Then
                    Label.Text = "FREE PLAY"
                Else
                    If Credits > 9 Then
                        Label.Text = "CREDITS 9"
                    Else
                        Label.Text = "CREDITS " & Credits
                    End If
                End If
            Case DISP_ONE_ROW
                Set Label = mFlexDMD.Stage.GetLabel("Text")
                Label.Text = mText1
                Label.SetAlignedPosition 64, 17, ALIGN_CENTER
            Case DISP_TWO_ROWS, DISP_TWO_ROWS_SEP
                Set Label = mFlexDMD.Stage.GetLabel("Top")
                Label.Text = mText1
                Label.SetAlignedPosition 64, 1, ALIGN_TOP
                Set Label = mFlexDMD.Stage.GetLabel("Bottom")
                Label.Text = mText2
                Label.SetAlignedPosition 64, 32, ALIGN_BOTTOM
        End Select

        ' Update & unlock
        mFlexDMD.Show = True
        mFlexDMD.UnlockRenderThread
    End Sub
End Class
