Option Strict On
Option Explicit On

Imports NUnit.Framework
Imports UnitTestingUtilitiesLibrary
Imports QuakeStats.LogParsing.FlagCalculator
Imports GraphLibrary.DirectedGraph
Imports System.Data.SqlClient
Imports System.Configuration
Imports System.IO

<TestFixture()> _
Public Class clsFlagCalculatorTester
    Inherits clsBaseUnitTester
#Region "Tests"
    <Test(), Description("Tests game graph building."), Category("Game Graph")> _
    Public Sub TestBuildGameGraph()
        Dim dgGraph As clsDirectedGraph(Of stuStatusNode, stuStatusTransition) = BuildGameGraph(23532)
        Dim lstPaths As List(Of List(Of Long)) = dgGraph.GetAllNonLoopingSourceSinkPaths()

        For Each lstPath As List(Of Long) In lstPaths
            Assert.True(lstPath.Count = 52)
            Assert.True(lstPath(0) = 1)
            Assert.True(lstPath(51) = 69)
        Next
    End Sub

    <Test(), Description("Tests game graph building."), Category("Game Graph")> _
    Public Sub TestBuildGameGraph2()
        Dim dgGraph As clsDirectedGraph(Of stuStatusNode, stuStatusTransition) = BuildGameGraph(23597)
        Dim lstPaths As List(Of List(Of Long)) = dgGraph.GetAllNonLoopingSourceSinkPaths()

    End Sub

    <Test(), Description("Tests game graph snapshot printing."), Category("Game Graph")> _
    Public Sub TestPrintSnapshots()
        Dim fc As New clsFlagCalculator(New SqlConnection("Data Source=T30\TOM_SQL_SVR;Initial Catalog=QuakeStats;User ID=QuakeStats;Password=excellent"))
        Dim lngGameID As Long = 23597
        Dim strOutputPath As String = "C:\Projects\QuakeStats\sqlserver_version\Output\GraphStates\TestOutput.txt"

        If My.Computer.FileSystem.FileExists(strOutputPath) Then
            My.Computer.FileSystem.DeleteFile(strOutputPath)
        End If

        Using fs As New FileStream(strOutputPath, FileMode.OpenOrCreate)
        End Using

        CallPrivateMethod(fc, "InitCalculateGame", New Object() {lngGameID})
        CallPrivateMethod(fc, "BuildGameGraph", New Object() {lngGameID, False})
        CallPrivateMethod(fc, "PrintSnapshot", New Object() {False, strOutputPath})

        Assert.True(My.Computer.FileSystem.FileExists(strOutputPath))
    End Sub
#End Region

#Region "Helpers"
    Private Function BuildGameGraph(ByVal plngGameID As Long) As clsDirectedGraph(Of stuStatusNode, stuStatusTransition)
        Dim fc As New clsFlagCalculator(New SqlConnection("Data Source=T30\TOM_SQL_SVR;Initial Catalog=QuakeStats;User ID=QuakeStats;Password=excellent"))

        CallPrivateMethod(fc, "InitCalculateGame", New Object() {plngGameID})
        CallPrivateMethod(fc, "BuildGameGraph", New Object() {plngGameID, False})

        Return fc.GameGraph
    End Function
#End Region
End Class
