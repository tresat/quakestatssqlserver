Option Strict On
Option Explicit On

Imports NUnit.Framework
Imports QuakeStats.Graph
Imports QuakeStats.LogParsing
Imports QuakeStats.LogParsing.QuakeObjects

<TestFixture()> _
Public Class clsDirectedGraphTester
    <Test(), Description("Tests basic constructor calls."), Category("Constructor")> _
    Public Sub TestCreateDirectedGraph()
        Dim dgGraph As New clsDirectedGraph(Of Long, Long)
        Dim dgGraph2 As New clsDirectedGraph(Of Object, Object)
        Dim dgGraph3 As New clsDirectedGraph(Of clsFlagCalculator.clsStatusSnapshot, clsFlagCalculator.clsStatusTransition)
        Dim dgGraph4 As New clsDirectedGraph(Of clsFlagCalculator.clsStatusSnapshot, clsFlagCalculator.clsStatusTransition)(5)
    End Sub

    <Test(), Description("Test constructor call with payload list"), Category("Constructor")> _
    Public Sub TestCreateDirectedGraphWithPayloadList()
        Dim lstPayloads As New List(Of Object)
        lstPayloads.Add("yo")
        lstPayloads.Add(5)
        lstPayloads.Add(#3/3/2003#)

        Dim dgGraph3 As New clsDirectedGraph(Of Object, Object)(3, lstPayloads)
    End Sub
End Class
