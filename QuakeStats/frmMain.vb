Option Explicit On
Option Strict On

Imports System.Data.SqlClient
Imports QuakeStats.LogParsing
Imports System.Configuration
Imports QuakeStats.LogParsing.FlagCalculator

Public Class frmMain

#Region "Member Vars"
    Private mobjParser As clsStatsParser
    Private WithEvents mobjFlagCalculator As clsFlagCalculator
    Private mobjMapCalculator As clsMapCalculator
    Private mobjAdjacencyCalculator As clsAdjacencyCalculator
#End Region

#Region "Events"
    Private Sub frmMain_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        Dim strGamesLogPath As String = ConfigurationManager.AppSettings("GameLogFilePath")
        Dim cxnStatsDB As New SqlConnection(ConfigurationManager.ConnectionStrings("QuakeStats").ConnectionString)

        'Give system settings class connection reference
        clsSystemSettings.DBConnection = cxnStatsDB

        'Create new buisness objects
        mobjParser = New clsStatsParser(strGamesLogPath, cxnStatsDB)
        mobjFlagCalculator = New clsFlagCalculator(cxnStatsDB)
        mobjMapCalculator = New clsMapCalculator(cxnStatsDB)
        mobjAdjacencyCalculator = New clsAdjacencyCalculator(cxnStatsDB)
    End Sub

    Private Sub cmdParse_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdParse.Click
        mobjParser.Parse()
    End Sub

    Private Sub cmdCalculateFlags_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdCalculateFlags.Click
        mobjFlagCalculator.CalculateAllGames()
    End Sub

    Private Sub cmdCalculateMaps_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdCalculateMaps.Click
        mobjMapCalculator.CalculateAllGames()
    End Sub

    Private Sub cmdDoLinking_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdDoLinking.Click
        mobjAdjacencyCalculator.CalculateAllAdjacencies()
    End Sub

    Private Sub mobjFlagCalculator_GameEventParsed(ByVal pintCurrentEvent As Integer, ByVal pintTotalEvents As Integer, ByVal pintWorkingSetSize As Integer) Handles mobjFlagCalculator.GameEventParsed
        txtGameEventCurrent.Text = CStr(pintCurrentEvent)
        txtGameEventTotal.Text = CStr(pintTotalEvents)
        txtWorkingSetSize.Text = CStr(pintWorkingSetSize)
        Application.DoEvents()
    End Sub

    Private Sub mobjFlagCalculator_GameParsed(ByVal pblnSuccess As Boolean) Handles mobjFlagCalculator.GameParsed
        Static sintSuccesses As Integer = 0
        Static sintFailures As Integer = 0

        If pblnSuccess Then
            sintSuccesses += 1
        Else
            sintFailures += 1
        End If

        txtSuccesses.Text = CStr(sintSuccesses)
        txtFailures.Text = CStr(sintFailures)
    End Sub
#End Region
End Class
