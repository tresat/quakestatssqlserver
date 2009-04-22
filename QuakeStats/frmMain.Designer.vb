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
        Me.cmdCalculateFlags = New System.Windows.Forms.Button
        Me.cmdCalculateMaps = New System.Windows.Forms.Button
        Me.cmdDoLinking = New System.Windows.Forms.Button
        Me.lblGameEventTotal = New System.Windows.Forms.Label
        Me.txtGameEventTotal = New System.Windows.Forms.TextBox
        Me.txtGameEventCurrent = New System.Windows.Forms.TextBox
        Me.lblGameEventCurrent = New System.Windows.Forms.Label
        Me.txtWorkingSetSize = New System.Windows.Forms.TextBox
        Me.lblWorkingSetSize = New System.Windows.Forms.Label
        Me.txtSuccesses = New System.Windows.Forms.TextBox
        Me.txtFailures = New System.Windows.Forms.TextBox
        Me.lblSuccesses = New System.Windows.Forms.Label
        Me.lblFailures = New System.Windows.Forms.Label
        Me.SuspendLayout()
        '
        'cmdParse
        '
        Me.cmdParse.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cmdParse.Location = New System.Drawing.Point(667, 12)
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
        Me.rtbConsole.Size = New System.Drawing.Size(649, 418)
        Me.rtbConsole.TabIndex = 1
        Me.rtbConsole.Text = ""
        Me.rtbConsole.WordWrap = False
        '
        'cmdCalculateFlags
        '
        Me.cmdCalculateFlags.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cmdCalculateFlags.Location = New System.Drawing.Point(666, 99)
        Me.cmdCalculateFlags.Name = "cmdCalculateFlags"
        Me.cmdCalculateFlags.Size = New System.Drawing.Size(107, 23)
        Me.cmdCalculateFlags.TabIndex = 2
        Me.cmdCalculateFlags.Text = "Calculate Flags"
        Me.cmdCalculateFlags.UseVisualStyleBackColor = True
        '
        'cmdCalculateMaps
        '
        Me.cmdCalculateMaps.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cmdCalculateMaps.Location = New System.Drawing.Point(667, 41)
        Me.cmdCalculateMaps.Name = "cmdCalculateMaps"
        Me.cmdCalculateMaps.Size = New System.Drawing.Size(107, 23)
        Me.cmdCalculateMaps.TabIndex = 3
        Me.cmdCalculateMaps.Text = "Create Maps"
        Me.cmdCalculateMaps.UseVisualStyleBackColor = True
        '
        'cmdDoLinking
        '
        Me.cmdDoLinking.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cmdDoLinking.Location = New System.Drawing.Point(667, 70)
        Me.cmdDoLinking.Name = "cmdDoLinking"
        Me.cmdDoLinking.Size = New System.Drawing.Size(106, 23)
        Me.cmdDoLinking.TabIndex = 4
        Me.cmdDoLinking.Text = "Do Linking"
        Me.cmdDoLinking.UseVisualStyleBackColor = True
        '
        'lblGameEventTotal
        '
        Me.lblGameEventTotal.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblGameEventTotal.AutoSize = True
        Me.lblGameEventTotal.Location = New System.Drawing.Point(664, 394)
        Me.lblGameEventTotal.Name = "lblGameEventTotal"
        Me.lblGameEventTotal.Size = New System.Drawing.Size(108, 13)
        Me.lblGameEventTotal.TabIndex = 5
        Me.lblGameEventTotal.Text = "Total # Game Events"
        '
        'txtGameEventTotal
        '
        Me.txtGameEventTotal.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.txtGameEventTotal.Location = New System.Drawing.Point(667, 410)
        Me.txtGameEventTotal.Name = "txtGameEventTotal"
        Me.txtGameEventTotal.Size = New System.Drawing.Size(108, 20)
        Me.txtGameEventTotal.TabIndex = 6
        '
        'txtGameEventCurrent
        '
        Me.txtGameEventCurrent.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.txtGameEventCurrent.Location = New System.Drawing.Point(667, 371)
        Me.txtGameEventCurrent.Name = "txtGameEventCurrent"
        Me.txtGameEventCurrent.Size = New System.Drawing.Size(108, 20)
        Me.txtGameEventCurrent.TabIndex = 8
        '
        'lblGameEventCurrent
        '
        Me.lblGameEventCurrent.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblGameEventCurrent.AutoSize = True
        Me.lblGameEventCurrent.Location = New System.Drawing.Point(664, 355)
        Me.lblGameEventCurrent.Name = "lblGameEventCurrent"
        Me.lblGameEventCurrent.Size = New System.Drawing.Size(113, 13)
        Me.lblGameEventCurrent.TabIndex = 7
        Me.lblGameEventCurrent.Text = "Current Game Event #"
        '
        'txtWorkingSetSize
        '
        Me.txtWorkingSetSize.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.txtWorkingSetSize.Location = New System.Drawing.Point(667, 332)
        Me.txtWorkingSetSize.Name = "txtWorkingSetSize"
        Me.txtWorkingSetSize.Size = New System.Drawing.Size(108, 20)
        Me.txtWorkingSetSize.TabIndex = 10
        '
        'lblWorkingSetSize
        '
        Me.lblWorkingSetSize.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblWorkingSetSize.AutoSize = True
        Me.lblWorkingSetSize.Location = New System.Drawing.Point(664, 316)
        Me.lblWorkingSetSize.Name = "lblWorkingSetSize"
        Me.lblWorkingSetSize.Size = New System.Drawing.Size(89, 13)
        Me.lblWorkingSetSize.TabIndex = 9
        Me.lblWorkingSetSize.Text = "Working Set Size"
        '
        'txtSuccesses
        '
        Me.txtSuccesses.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.txtSuccesses.Location = New System.Drawing.Point(668, 209)
        Me.txtSuccesses.Name = "txtSuccesses"
        Me.txtSuccesses.Size = New System.Drawing.Size(43, 20)
        Me.txtSuccesses.TabIndex = 11
        '
        'txtFailures
        '
        Me.txtFailures.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.txtFailures.Location = New System.Drawing.Point(733, 209)
        Me.txtFailures.Name = "txtFailures"
        Me.txtFailures.Size = New System.Drawing.Size(39, 20)
        Me.txtFailures.TabIndex = 12
        '
        'lblSuccesses
        '
        Me.lblSuccesses.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblSuccesses.AutoSize = True
        Me.lblSuccesses.Location = New System.Drawing.Point(666, 190)
        Me.lblSuccesses.Name = "lblSuccesses"
        Me.lblSuccesses.Size = New System.Drawing.Size(59, 13)
        Me.lblSuccesses.TabIndex = 13
        Me.lblSuccesses.Text = "Successes"
        '
        'lblFailures
        '
        Me.lblFailures.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblFailures.AutoSize = True
        Me.lblFailures.Location = New System.Drawing.Point(730, 190)
        Me.lblFailures.Name = "lblFailures"
        Me.lblFailures.Size = New System.Drawing.Size(43, 13)
        Me.lblFailures.TabIndex = 14
        Me.lblFailures.Text = "Failures"
        '
        'frmMain
        '
        Me.AcceptButton = Me.cmdParse
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(786, 442)
        Me.Controls.Add(Me.lblFailures)
        Me.Controls.Add(Me.lblSuccesses)
        Me.Controls.Add(Me.txtFailures)
        Me.Controls.Add(Me.txtSuccesses)
        Me.Controls.Add(Me.txtWorkingSetSize)
        Me.Controls.Add(Me.lblWorkingSetSize)
        Me.Controls.Add(Me.txtGameEventCurrent)
        Me.Controls.Add(Me.lblGameEventCurrent)
        Me.Controls.Add(Me.txtGameEventTotal)
        Me.Controls.Add(Me.lblGameEventTotal)
        Me.Controls.Add(Me.cmdDoLinking)
        Me.Controls.Add(Me.cmdCalculateMaps)
        Me.Controls.Add(Me.cmdCalculateFlags)
        Me.Controls.Add(Me.rtbConsole)
        Me.Controls.Add(Me.cmdParse)
        Me.Name = "frmMain"
        Me.Text = "Quake Stats Games.Log Parser"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents cmdParse As System.Windows.Forms.Button
    Friend WithEvents rtbConsole As System.Windows.Forms.RichTextBox
    Friend WithEvents cmdCalculateFlags As System.Windows.Forms.Button
    Friend WithEvents cmdCalculateMaps As System.Windows.Forms.Button
    Friend WithEvents cmdDoLinking As System.Windows.Forms.Button
    Friend WithEvents lblGameEventTotal As System.Windows.Forms.Label
    Friend WithEvents txtGameEventTotal As System.Windows.Forms.TextBox
    Friend WithEvents txtGameEventCurrent As System.Windows.Forms.TextBox
    Friend WithEvents lblGameEventCurrent As System.Windows.Forms.Label
    Friend WithEvents txtWorkingSetSize As System.Windows.Forms.TextBox
    Friend WithEvents lblWorkingSetSize As System.Windows.Forms.Label
    Friend WithEvents txtSuccesses As System.Windows.Forms.TextBox
    Friend WithEvents txtFailures As System.Windows.Forms.TextBox
    Friend WithEvents lblSuccesses As System.Windows.Forms.Label
    Friend WithEvents lblFailures As System.Windows.Forms.Label

End Class
