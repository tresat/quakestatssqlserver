Option Strict On
Option Explicit On

Imports NUnit.Framework
Imports UnitTestingUtilitiesLibrary
Imports GraphLibrary.DirectedGraph
Imports QuakeStats.LogParsing
Imports QuakeStats.LogParsing.QuakeObjects
Imports QuakeStats.LogParsing.FlagCalculator

<TestFixture()> _
Public Class clsDirectedGraphTester
    Inherits clsBaseUnitTester

    <Test(), Description("Tests basic constructor calls."), Category("Constructor")> _
    Public Sub TestCreateDirectedGraph()
        Dim dgGraph As New clsDirectedGraph(Of Long, Long)
        Dim dgGraph2 As New clsDirectedGraph(Of Object, Object)
        Dim dgGraph3 As New clsDirectedGraph(Of stuStatusNode, stuStatusTransition)
        Dim dgGraph4 As New clsDirectedGraph(Of stuStatusNode, stuStatusTransition)(5)
    End Sub

    <Test(), Description("Test constructor call with payload list"), Category("Constructor")> _
    Public Sub TestCreateDirectedGraphWithPayloadList()
        Dim lstPayloads As New List(Of Object)
        lstPayloads.Add("yo")
        lstPayloads.Add(5)
        lstPayloads.Add(#3/3/2003#)

        Dim dgGraph3 As New clsDirectedGraph(Of Object, Object)(3, lstPayloads)
    End Sub

    <Test(), Description("Test SetEdgeEnd() function"), Category("Method Tests")> _
    Public Sub TestSetEdgeEnd()
        Dim dgGraph As New clsDirectedGraph(Of String, String)

        Dim lngVertex1ID As Long = dgGraph.AddNewVertex("The")
        Dim lngVertex2ID As Long = dgGraph.AddNewVertex("quick")
        Dim lngVertex3ID As Long = dgGraph.AddNewVertex("brown")
        Dim lngVertex4ID As Long = dgGraph.AddNewVertex("fox")

        Dim lngEdge1ID As Long = dgGraph.AddNewEdge(lngVertex1ID, lngVertex2ID)
        Dim lngEdge2ID As Long = dgGraph.AddNewEdge(lngVertex2ID, lngVertex3ID)
        Dim lngEdge3ID As Long = dgGraph.AddNewEdge(lngVertex3ID, lngVertex4ID)

        Debug.Assert(dgGraph.GetEdge(lngEdge3ID).StartVertexID = lngVertex3ID)
        Debug.Assert(dgGraph.GetEdge(lngEdge3ID).EndVertexID = lngVertex4ID)

        dgGraph.SetEdgeEnd(lngEdge3ID, lngVertex1ID)

        Debug.Assert(dgGraph.GetEdge(lngEdge3ID).StartVertexID = lngVertex3ID)
        Debug.Assert(dgGraph.GetEdge(lngEdge3ID).EndVertexID = lngVertex1ID)

        Debug.Assert(dgGraph.GetVertex(lngVertex1ID).Edges.Count = 2)
        Debug.Assert(dgGraph.GetEdge(dgGraph.GetVertex(lngVertex1ID).Edges(0)).StartVertexID = lngVertex1ID)
        Debug.Assert(dgGraph.GetEdge(dgGraph.GetVertex(lngVertex1ID).Edges(0)).EndVertexID = lngVertex2ID)
        Debug.Assert(dgGraph.GetEdge(dgGraph.GetVertex(lngVertex1ID).Edges(1)).StartVertexID = lngVertex3ID)
        Debug.Assert(dgGraph.GetEdge(dgGraph.GetVertex(lngVertex1ID).Edges(1)).EndVertexID = lngVertex1ID)
    End Sub
End Class
