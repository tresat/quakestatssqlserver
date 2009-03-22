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
        Me.cmdCalculateFlags.Location = New System.Drawing.Point(667, 70)
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
        Me.cmdDoLinking.Location = New System.Drawing.Point(668, 100)
        Me.cmdDoLinking.Name = "cmdDoLinking"
        Me.cmdDoLinking.Size = New System.Drawing.Size(106, 23)
        Me.cmdDoLinking.TabIndex = 4
        Me.cmdDoLinking.Text = "Do Linking"
        Me.cmdDoLinking.UseVisualStyleBackColor = True
        '
        'frmMain
        '
        Me.AcceptButton = Me.cmdParse
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(786, 442)
        Me.Controls.Add(Me.cmdDoLinking)
        Me.Controls.Add(Me.cmdCalculateMaps)
        Me.Controls.Add(Me.cmdCalculateFlags)
        Me.Controls.Add(Me.rtbConsole)
        Me.Controls.Add(Me.cmdParse)
        Me.Name = "frmMain"
        Me.Text = "Quake Stats Games.Log Parser"
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents cmdParse As System.Windows.Forms.Button
    Friend WithEvents rtbConsole As System.Windows.Forms.RichTextBox
    Friend WithEvents cmdCalculateFlags As System.Windows.Forms.Button
    Friend WithEvents cmdCalculateMaps As System.Windows.Forms.Button
    Friend WithEvents cmdDoLinking As System.Windows.Forms.Button

End Class
