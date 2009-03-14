Option Explicit On
Option Strict On

Imports System.Data.SqlClient
Imports QuakeStats.LogParsing
Imports System.Configuration

Public Class frmMain

#Region "Member Vars"
    Private mobjParser As clsStatsParser
#End Region

#Region "Events"
    Private Sub frmMain_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        Dim strGamesLogPath As String = ConfigurationManager.AppSettings("GameLogFilePath")
        Dim cxnStatsDB As New SqlConnection(ConfigurationManager.ConnectionStrings("QuakeStats").ConnectionString)

        'Give system settings class connection reference
        clsSystemSettings.DBConnection = cxnStatsDB

        'Create new Stats Parser
        mobjParser = New clsStatsParser(strGamesLogPath, cxnStatsDB, rtbConsole)
    End Sub

    Private Sub cmdParse_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdParse.Click
        mobjParser.Parse()
    End Sub
#End Region
End Class
