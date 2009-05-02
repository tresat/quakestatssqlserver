<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmMain
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.cmdParse = New System.Windows.Forms.Button
        Me.rtbConsole = New System.Windows.Forms.RichTextBox
        Me.cmdCalculateAllFlags = New System.Windows.Forms.Button
        Me.cmdCalculateMaps = New System.Windows.Forms.Button
        Me.cmdDoLinking = New System.Windows.Forms.Button
        Me.lblPercentDoneCurrentStep = New System.Windows.Forms.Label
        Me.txtPercentDoneCurrentStep = New System.Windows.Forms.TextBox
        Me.txtCurrentStep = New System.Windows.Forms.TextBox
        Me.lblCurrentStep = New System.Windows.Forms.Label
        Me.txtSuccesses = New System.Windows.Forms.TextBox
        Me.txtFailures = New System.Windows.Forms.TextBox
        Me.lblSuccesses = New System.Windows.Forms.Label
        Me.lblFailures = New System.Windows.Forms.Label
        Me.cmdCalculateNextXFlags = New System.Windows.Forms.Button
        Me.mtbX = New System.Windows.Forms.MaskedTextBox
        Me.lblX = New System.Windows.Forms.Label
        Me.chkResetFlagCalculations = New System.Windows.Forms.CheckBox
        Me.SuspendLayout()
        '
        'cmdParse
        '
        Me.cmdParse.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cmdParse.Location = New System.Drawing.Point(713, 12)
        Me.cmdParse.Name = "cmdParse"
        Me.cmdParse.Size = New System.Drawing.Size(107, 23)
        Me.cmdParse.TabIndex = 0
        Me.cmdParse.Text = "Parse Log"
        Me.cmdParse.UseVisualStyleBackColor = True
        '
        'rtbConsole
        '
        Me.rtbConsole.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
                    Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.rtbConsole.Location = New System.Drawing.Point(12, 12)
        Me.rtbConsole.Name = "rtbConsole"
        Me.rtbConsole.ReadOnly = True
        Me.rtbConsole.Size = New System.Drawing.Size(693, 418)
        Me.rtbConsole.TabIndex = 1
        Me.rtbConsole.Text = ""
        Me.rtbConsole.WordWrap = False
        '
        'cmdCalculateAllFlags
        '
        Me.cmdCalculateAllFlags.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cmdCalculateAllFlags.Location = New System.Drawing.Point(712, 154)
        Me.cmdCalculateAllFlags.Name = "cmdCalculateAllFlags"
        Me.cmdCalculateAllFlags.Size = New System.Drawing.Size(107, 23)
        Me.cmdCalculateAllFlags.TabIndex = 2
        Me.cmdCalculateAllFlags.Text = "Calculate All Flags"
        Me.cmdCalculateAllFlags.UseVisualStyleBackColor = True
        '
        'cmdCalculateMaps
        '
        Me.cmdCalculateMaps.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cmdCalculateMaps.Location = New System.Drawing.Point(713, 41)
        Me.cmdCalculateMaps.Name = "cmdCalculateMaps"
        Me.cmdCalculateMaps.Size = New System.Drawing.Size(107, 23)
        Me.cmdCalculateMaps.TabIndex = 3
        Me.cmdCalculateMaps.Text = "Create Maps"
        Me.cmdCalculateMaps.UseVisualStyleBackColor = True
        '
        'cmdDoLinking
        '
        Me.cmdDoLinking.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cmdDoLinking.Location = New System.Drawing.Point(713, 70)
        Me.cmdDoLinking.Name = "cmdDoLinking"
        Me.cmdDoLinking.Size = New System.Drawing.Size(106, 23)
        Me.cmdDoLinking.TabIndex = 4
        Me.cmdDoLinking.Text = "Do Linking"
        Me.cmdDoLinking.UseVisualStyleBackColor = True
        '
        'lblPercentDoneCurrentStep
        '
        Me.lblPercentDoneCurrentStep.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblPercentDoneCurrentStep.AutoSize = True
        Me.lblPercentDoneCurrentStep.Location = New System.Drawing.Point(711, 306)
        Me.lblPercentDoneCurrentStep.Name = "lblPercentDoneCurrentStep"
        Me.lblPercentDoneCurrentStep.Size = New System.Drawing.Size(112, 13)
        Me.lblPercentDoneCurrentStep.TabIndex = 5
        Me.lblPercentDoneCurrentStep.Text = "% Done (Current Step)"
        '
        'txtPercentDoneCurrentStep
        '
        Me.txtPercentDoneCurrentStep.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.txtPercentDoneCurrentStep.Location = New System.Drawing.Point(714, 322)
        Me.txtPercentDoneCurrentStep.Name = "txtPercentDoneCurrentStep"
        Me.txtPercentDoneCurrentStep.Size = New System.Drawing.Size(108, 20)
        Me.txtPercentDoneCurrentStep.TabIndex = 6
        '
        'txtCurrentStep
        '
        Me.txtCurrentStep.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.txtCurrentStep.Location = New System.Drawing.Point(714, 283)
        Me.txtCurrentStep.Name = "txtCurrentStep"
        Me.txtCurrentStep.Size = New System.Drawing.Size(108, 20)
        Me.txtCurrentStep.TabIndex = 8
        '
        'lblCurrentStep
        '
        Me.lblCurrentStep.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblCurrentStep.AutoSize = True
        Me.lblCurrentStep.Location = New System.Drawing.Point(711, 267)
        Me.lblCurrentStep.Name = "lblCurrentStep"
        Me.lblCurrentStep.Size = New System.Drawing.Size(66, 13)
        Me.lblCurrentStep.TabIndex = 7
        Me.lblCurrentStep.Text = "Current Step"
        '
        'txtSuccesses
        '
        Me.txtSuccesses.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.txtSuccesses.Location = New System.Drawing.Point(719, 406)
        Me.txtSuccesses.Name = "txtSuccesses"
        Me.txtSuccesses.Size = New System.Drawing.Size(43, 20)
        Me.txtSuccesses.TabIndex = 11
        '
        'txtFailures
        '
        Me.txtFailures.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.txtFailures.Location = New System.Drawing.Point(784, 406)
        Me.txtFailures.Name = "txtFailures"
        Me.txtFailures.Size = New System.Drawing.Size(39, 20)
        Me.txtFailures.TabIndex = 12
        '
        'lblSuccesses
        '
        Me.lblSuccesses.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblSuccesses.AutoSize = True
        Me.lblSuccesses.Location = New System.Drawing.Point(717, 387)
        Me.lblSuccesses.Name = "lblSuccesses"
        Me.lblSuccesses.Size = New System.Drawing.Size(59, 13)
        Me.lblSuccesses.TabIndex = 13
        Me.lblSuccesses.Text = "Successes"
        '
        'lblFailures
        '
        Me.lblFailures.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblFailures.AutoSize = True
        Me.lblFailures.Location = New System.Drawing.Point(781, 387)
        Me.lblFailures.Name = "lblFailures"
        Me.lblFailures.Size = New System.Drawing.Size(43, 13)
        Me.lblFailures.TabIndex = 14
        Me.lblFailures.Text = "Failures"
        '
        'cmdCalculateNextXFlags
        '
        Me.cmdCalculateNextXFlags.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cmdCalculateNextXFlags.Location = New System.Drawing.Point(711, 209)
        Me.cmdCalculateNextXFlags.Name = "cmdCalculateNextXFlags"
        Me.cmdCalculateNextXFlags.Size = New System.Drawing.Size(107, 23)
        Me.cmdCalculateNextXFlags.TabIndex = 15
        Me.cmdCalculateNextXFlags.Text = "Do Next X Flags"
        Me.cmdCalculateNextXFlags.UseVisualStyleBackColor = True
        '
        'mtbX
        '
        Me.mtbX.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.mtbX.AsciiOnly = True
        Me.mtbX.HidePromptOnLeave = True
        Me.mtbX.Location = New System.Drawing.Point(730, 183)
        Me.mtbX.Mask = "0000"
        Me.mtbX.Name = "mtbX"
        Me.mtbX.PromptChar = Global.Microsoft.VisualBasic.ChrW(32)
        Me.mtbX.Size = New System.Drawing.Size(46, 20)
        Me.mtbX.TabIndex = 17
        Me.mtbX.Text = "5"
        Me.mtbX.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'lblX
        '
        Me.lblX.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblX.AutoSize = True
        Me.lblX.Location = New System.Drawing.Point(711, 183)
        Me.lblX.Name = "lblX"
        Me.lblX.Size = New System.Drawing.Size(17, 13)
        Me.lblX.TabIndex = 18
        Me.lblX.Text = "X:"
        '
        'chkResetFlagCalculations
        '
        Me.chkResetFlagCalculations.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.chkResetFlagCalculations.Location = New System.Drawing.Point(711, 99)
        Me.chkResetFlagCalculations.Name = "chkResetFlagCalculations"
        Me.chkResetFlagCalculations.Size = New System.Drawing.Size(124, 49)
        Me.chkResetFlagCalculations.TabIndex = 19
        Me.chkResetFlagCalculations.Text = "Reset Flag Calculations Before Do Calculate Flags"
        Me.chkResetFlagCalculations.UseVisualStyleBackColor = True
        '
        'frmMain
        '
        Me.AcceptButton = Me.cmdCalculateNextXFlags
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(832, 442)
        Me.Controls.Add(Me.chkResetFlagCalculations)
        Me.Controls.Add(Me.lblX)
        Me.Controls.Add(Me.mtbX)
        Me.Controls.Add(Me.cmdCalculateNextXFlags)
        Me.Controls.Add(Me.lblFailures)
        Me.Controls.Add(Me.lblSuccesses)
        Me.Controls.Add(Me.txtFailures)
        Me.Controls.Add(Me.txtSuccesses)
        Me.Controls.Add(Me.txtCurrentStep)
        Me.Controls.Add(Me.lblCurrentStep)
        Me.Controls.Add(Me.txtPercentDoneCurrentStep)
        Me.Controls.Add(Me.lblPercentDoneCurrentStep)
        Me.Controls.Add(Me.cmdDoLinking)
        Me.Controls.Add(Me.cmdCalculateMaps)
        Me.Controls.Add(Me.cmdCalculateAllFlags)
        Me.Controls.Add(Me.rtbConsole)
        Me.Controls.Add(Me.cmdParse)
        Me.DoubleBuffered = True
        Me.Name = "frmMain"
        Me.Text = "Quake Stats Games.Log Parser"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents cmdParse As System.Windows.Forms.Button
    Friend WithEvents rtbConsole As System.Windows.Forms.RichTextBox
    Friend WithEvents cmdCalculateAllFlags As System.Windows.Forms.Button
    Friend WithEvents cmdCalculateMaps As System.Windows.Forms.Button
    Friend WithEvents cmdDoLinking As System.Windows.Forms.Button
    Friend WithEvents lblPercentDoneCurrentStep As System.Windows.Forms.Label
    Friend WithEvents txtPercentDoneCurrentStep As System.Windows.Forms.TextBox
    Friend WithEvents txtCurrentStep As System.Windows.Forms.TextBox
    Friend WithEvents lblCurrentStep As System.Windows.Forms.Label
    Friend WithEvents txtSuccesses As System.Windows.Forms.TextBox
    Friend WithEvents txtFailures As System.Windows.Forms.TextBox
    Friend WithEvents lblSuccesses As System.Windows.Forms.Label
    Friend WithEvents lblFailures As System.Windows.Forms.Label
    Friend WithEvents cmdCalculateNextXFlags As System.Windows.Forms.Button
    Friend WithEvents mtbX As System.Windows.Forms.MaskedTextBox
    Friend WithEvents lblX As System.Windows.Forms.Label
    Friend WithEvents chkResetFlagCalculations As System.Windows.Forms.CheckBox

End Class
