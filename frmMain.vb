Option Explicit On
Option Strict On

Imports System.Data.SqlClient
Imports QuakeStats.LogParsing
Imports System.Configuration

Public Class frmMain

#Region "Member Vars"
    Private mobjParser As clsStatsParser
    Private mobjFlagCalculator As clsFlagCalculator
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
#End Region
End Class
