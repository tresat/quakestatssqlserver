Option Explicit On
Option Strict On

Imports System.Data.SqlClient

Module modGlobalRoutines
    ''' <summary>
    ''' Verifies the connection to the StatsDB is connected.
    ''' </summary>
    ''' <param name="pcxnStatsDB">The connection to test.</param>
    Public Sub VerifyDBConnected(ByVal pcxnStatsDB As SqlConnection)
        If pcxnStatsDB.State <> ConnectionState.Open Then
            pcxnStatsDB.Open()
        End If
    End Sub

    ''' <summary>
    ''' Adds the message to the console textbox.  Calls DoEvents to refresh the visible text.
    ''' </summary>
    ''' <param name="pstrMsg">The text to add to the textbox.</param>
    Public Sub Print(ByVal pstrMsg As String, Optional ByVal pblnNewLine As Boolean = True)
        frmMain.rtbConsole.Text = frmMain.rtbConsole.Text & pstrMsg & CStr(IIf(pblnNewLine, vbCrLf, String.Empty))
        Application.DoEvents()
    End Sub
End Module
