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

        'Set validating type on masked text boxes
        mtbX.ValidatingType = System.Type.GetType("Integer")
    End Sub

    Private Sub cmdParse_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdParse.Click
        mobjParser.Parse()
    End Sub

    Private Sub cmdCalculateFlags_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdCalculateAllFlags.Click
        mobjFlagCalculator.CalculateAllGames(pblnResetFlagCalculationsFirst:=chkResetFlagCalculations.Checked)
    End Sub

    Private Sub cmdCalculateMaps_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdCalculateMaps.Click
        mobjMapCalculator.CalculateAllGames()
    End Sub

    Private Sub cmdDoLinking_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdDoLinking.Click
        mobjAdjacencyCalculator.CalculateAllAdjacencies()
    End Sub

    Private Sub cmdCalculateNextXFlags_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdCalculateNextXFlags.Click
        mobjFlagCalculator.CalculateAllGames(CInt(mtbX.Text), , chkResetFlagCalculations.Checked)
    End Sub

    Private Sub mobjFlagCalculator_GameCalculationStatusChanged(ByVal penuCurrentStep As LogParsing.FlagCalculator.clsFlagCalculator.enuCalculationStepType, ByVal plngIdx As Long, ByVal plngLimit As Long) Handles mobjFlagCalculator.GameCalculationStatusChanged
        Select Case penuCurrentStep
            Case clsFlagCalculator.enuCalculationStepType.ResetFlagCalculationsInDB
                txtCurrentStep.Text = "Reseting DB"
                txtPercentDoneCurrentStep.Text = "n/a"
            Case clsFlagCalculator.enuCalculationStepType.FetchingGameEvents
                txtCurrentStep.Text = "Fetching Events"
                txtPercentDoneCurrentStep.Text = "n/a"
            Case clsFlagCalculator.enuCalculationStepType.FetchedGameEvents
                txtCurrentStep.Text = "Fetched Events"
                txtPercentDoneCurrentStep.Text = "n/a"
            Case clsFlagCalculator.enuCalculationStepType.BuildingGameGraph
                txtCurrentStep.Text = "Building Graph"
                txtPercentDoneCurrentStep.Text = String.Format("{0}%", (plngIdx / plngLimit) * 100)
            Case clsFlagCalculator.enuCalculationStepType.FindingPathsThroughGraph
                txtCurrentStep.Text = "Finding Paths"
                txtPercentDoneCurrentStep.Text = "n/a"
            Case clsFlagCalculator.enuCalculationStepType.FilteringPathsToScore
                txtCurrentStep.Text = "Filtering Paths"
                txtPercentDoneCurrentStep.Text = String.Format("{0}%", (plngIdx / plngLimit) * 100)
            Case Else
                Throw New ArgumentException("Bad step type: " & penuCurrentStep)
        End Select

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
