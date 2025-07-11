Sub CreateScoreScene()
    Dim font_big
    Set font_big = FlexDMD.NewFont("FlexDMD.Resources.udmd-f7by13.fnt", vbWhite, vbWhite, 0)
    Dim font_small
    Set font_small = FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt", vbBlue, vbBlue, 0)

    ' Contents and alignment will be set in DMDTimer_Timer
    Dim group
    Set group = FlexDMD.NewGroup("ScoreBoard")
	group.AddActor FlexDMD.NewLabel("Score", font_big, "0")
	group.AddActor FlexDMD.NewLabel("Ball", font_small, "GAME OVER")
	group.AddActor FlexDMD.NewLabel("Credits", font_small, "CREDITS 0")

	FlexDMD.LockRenderThread
	FlexDMD.RenderMode = DMD_RGB
	FlexDMD.Stage.RemoveAll
	FlexDMD.Stage.AddActor group
	FlexDMD.Show = True
	FlexDMD.UnlockRenderThread
End Sub

' A 16ms timer which runs to refresh the display
Sub DMDTimer_Timer()
    Dim label
	FlexDMD.LockRenderThread
    Set label = FlexDMD.Stage.GetLabel("Score")
    label.Text = FormatNumber(Score, 0, -1, 0, -1) ' Separators but no decimal
    label.SetBounds 0, 4, 128, 16
    Set label = FlexDMD.Stage.GetLabel("Ball")
    If BallNumber < 1 Then
        label.Text = "GAME OVER"
    Else
        label.Text = "BALL " & CStr(BallNumber)
    End If
    label.SetAlignedPosition 2, 33, ALIGN_BOTTOM_LEFT
    Set label = FlexDMD.Stage.GetLabel("Credits")
    If bFreePlay Then
        label.Text = "FREE PLAY"
    Else
        label.Text = "CREDITS " & Credits
    End If
    label.SetAlignedPosition 126, 33, ALIGN_BOTTOM_RIGHT
    FlexDMD.UnlockRenderThread
End Sub
