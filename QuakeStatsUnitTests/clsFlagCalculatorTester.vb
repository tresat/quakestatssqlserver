Option Strict On
Option Explicit On

Imports NUnit.Framework
Imports UnitTestingUtilitiesLibrary
Imports QuakeStats.LogParsing.FlagCalculator
Imports GraphLibrary.DirectedGraph
Imports System.Data.SqlClient
Imports System.Configuration

<TestFixture()> _
Public Class clsFlagCalculatorTester
    Inherits clsBaseUnitTester

    <Test(), Description("Tests game graph building."), Category("Game Graph")> _
    Public Sub TestCreateDirectedGraph()
        Dim dgGraph As clsDirectedGraph(Of stuStatusNode, stuStatusTransition) = BuildGameGraph(23532)
        Dim lstPaths As List(Of List(Of Long)) = dgGraph.GetAllNonLoopingSourceSinkPaths()

        For Each lstPath As List(Of Long) In lstPaths
            Assert.True(lstPath.Count = 52)
            Assert.True(lstPath(0) = 1)
            Assert.True(lstPath(51) = 69)
        Next
    End Sub

    Private Function BuildGameGraph(ByVal plngGameID As Long) As clsDirectedGraph(Of stuStatusNode, stuStatusTransition)
        Dim fc As New clsFlagCalculator(New SqlConnection("Data Source=T30\TOM_SQL_SVR;Initial Catalog=QuakeStats;User ID=QuakeStats;Password=excellent"))

        CallPrivateMethod(fc, "InitCalculateGame", New Object() {plngGameID})
        CallPrivateMethod(fc, "BuildGameGraph", New Object() {plngGameID})

        Return fc.GameGraph
    End Function
End Class
