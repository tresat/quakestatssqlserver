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

    <Test(), Description("Tests basic constructor calls."), Category("Constructors")> _
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

    <Test(), Description("Test SetEdgeEnd() function"), Category("Methods")> _
    Public Sub TestSetEdgeEnd()
        Dim dgGraph As New clsDirectedGraph(Of String, String)(0)

        Dim lngVertex1ID As Long = dgGraph.AddNewVertex("The")
        Dim lngVertex2ID As Long = dgGraph.AddNewVertex("quick")
        Dim lngVertex3ID As Long = dgGraph.AddNewVertex("brown")
        Dim lngVertex4ID As Long = dgGraph.AddNewVertex("fox")

        Dim lngEdge1ID As Long = dgGraph.AddNewEdge(lngVertex1ID, lngVertex2ID)
        Dim lngEdge2ID As Long = dgGraph.AddNewEdge(lngVertex2ID, lngVertex3ID)
        Dim lngEdge3ID As Long = dgGraph.AddNewEdge(lngVertex3ID, lngVertex4ID)

        Assert.True(dgGraph.GetEdge(lngEdge3ID).StartVertexID = lngVertex3ID)
        Assert.True(dgGraph.GetEdge(lngEdge3ID).EndVertexID = lngVertex4ID)

        dgGraph.SetEdgeEnd(lngEdge3ID, lngVertex1ID)

        Assert.True(dgGraph.GetEdge(lngEdge3ID).StartVertexID = lngVertex3ID)
        Assert.True(dgGraph.GetEdge(lngEdge3ID).EndVertexID = lngVertex1ID)

        Assert.True(dgGraph.GetVertex(lngVertex1ID).Edges.Count = 2)
        Assert.True(dgGraph.GetEdge(dgGraph.GetVertex(lngVertex1ID).Edges(0)).StartVertexID = lngVertex1ID)
        Assert.True(dgGraph.GetEdge(dgGraph.GetVertex(lngVertex1ID).Edges(0)).EndVertexID = lngVertex2ID)
        Assert.True(dgGraph.GetEdge(dgGraph.GetVertex(lngVertex1ID).Edges(1)).StartVertexID = lngVertex3ID)
        Assert.True(dgGraph.GetEdge(dgGraph.GetVertex(lngVertex1ID).Edges(1)).EndVertexID = lngVertex1ID)
    End Sub

    <Test(), Description("Test SetEdgeEnd() function"), Category("Methods")> _
    Public Sub TestSetEdgeEnd2()
        Dim dgGraph As New clsDirectedGraph(Of String, String)(0)

        Dim lngVertex1ID As Long = dgGraph.AddNewVertex("central")
        Dim lngVertex2ID As Long = dgGraph.AddNewVertex("exit")
        Dim lngVertex3ID As Long = dgGraph.AddNewVertex("exit")
        Dim lngVertex4ID As Long = dgGraph.AddNewVertex("exit")

        Dim lngEdge1ID As Long = dgGraph.AddNewEdge(lngVertex1ID, lngVertex2ID)
        Dim lngEdge2ID As Long = dgGraph.AddNewEdge(lngVertex1ID, lngVertex3ID)
        Dim lngEdge3ID As Long = dgGraph.AddNewEdge(lngVertex1ID, lngVertex4ID)

        Assert.True(dgGraph.GetOutgoingEdges(lngVertex1ID).Count = 3)
        Assert.True(dgGraph.GetIncomingEdges(lngVertex1ID).Count = 0)

        dgGraph.SetEdgeEnd(lngEdge1ID, lngVertex1ID)

        Assert.True(dgGraph.GetOutgoingEdges(lngVertex1ID).Count = 3)
        Assert.True(dgGraph.GetIncomingEdges(lngVertex1ID).Count = 1)

        dgGraph.SetEdgeEnd(lngEdge2ID, lngVertex1ID)

        Assert.True(dgGraph.GetOutgoingEdges(lngVertex1ID).Count = 3)
        Assert.True(dgGraph.GetIncomingEdges(lngVertex1ID).Count = 2)

        dgGraph.SetEdgeEnd(lngEdge3ID, lngVertex1ID)

        Assert.True(dgGraph.GetOutgoingEdges(lngVertex1ID).Count = 3)
        Assert.True(dgGraph.GetIncomingEdges(lngVertex1ID).Count = 3)
    End Sub

    <Test(), Description("Test RemoveVertex() function"), Category("Methods")> _
    Public Sub TestRemoveVertex()
        Dim dgGraph As New clsDirectedGraph(Of String, String)(0)

        Dim lngVertex1ID As Long = dgGraph.AddNewVertex("The")
        Dim lngVertex2ID As Long = dgGraph.AddNewVertex("quick")
        Dim lngVertex3ID As Long = dgGraph.AddNewVertex("brown")

        Dim lngEdge1ID As Long = dgGraph.AddNewEdge(lngVertex1ID, lngVertex2ID)
        Dim lngEdge2ID As Long = dgGraph.AddNewEdge(lngVertex2ID, lngVertex3ID)

        dgGraph.RemoveVertex(lngVertex2ID)

        Assert.True(dgGraph.GetVertex(lngVertex1ID).Edges.Count = 0)
        Assert.True(dgGraph.GetVertex(lngVertex3ID).Edges.Count = 0)

        For Each lngEdge In dgGraph.GetVertex(lngVertex1ID).Edges

        Next
    End Sub

    <Test(), ExpectedException(), Description("Test vertex not found exception"), Category("Methods")> _
    Public Sub TestRemoveVertex2()
        Dim dgGraph As New clsDirectedGraph(Of String, String)(0)

        Dim lngVertex1ID As Long = dgGraph.AddNewVertex("The")

        dgGraph.GetVertex(lngVertex1ID)

        dgGraph.RemoveVertex(lngVertex1ID)

        dgGraph.GetVertex(lngVertex1ID)
    End Sub

    <Test(), ExpectedException(), Description("Test edge not found exception"), Category("Methods")> _
    Public Sub TestRemoveEdge()
        Dim dgGraph As New clsDirectedGraph(Of String, String)(0)

        Dim lngVertex1ID As Long = dgGraph.AddNewVertex("The")
        Dim lngVertex2ID As Long = dgGraph.AddNewVertex("quick")
        Dim lngVertex3ID As Long = dgGraph.AddNewVertex("brown")

        Dim lngEdge1ID As Long = dgGraph.AddNewEdge(lngVertex1ID, lngVertex2ID)
        Dim lngEdge2ID As Long = dgGraph.AddNewEdge(lngVertex2ID, lngVertex3ID)
        Dim lngedge3ID As Long = dgGraph.AddNewEdge(lngVertex1ID, lngVertex3ID)

        dgGraph.GetEdge(lngEdge2ID)

        dgGraph.RemoveEdge(lngEdge2ID)

        dgGraph.GetEdge(lngEdge2ID)
    End Sub

    <Test(), Description("Test remove paths to vertex"), Category("Methods")> _
    Public Sub TestRemovePathsToVertex()
        Dim dgGraph As New clsDirectedGraph(Of String, String)(0)

        Dim lngVertex1ID As Long = dgGraph.AddNewVertex("the")
        Dim lngVertex2ID As Long = dgGraph.AddNewVertex("quick")
        Dim lngVertex3ID As Long = dgGraph.AddNewVertex("brown")
        Dim lngVertex4ID As Long = dgGraph.AddNewVertex("fox")
        Dim lngVertex5ID As Long = dgGraph.AddNewVertex("jumped")
        Dim lngVertex6ID As Long = dgGraph.AddNewVertex("a")
        Dim lngVertex7ID As Long = dgGraph.AddNewVertex("fence")
        Dim lngVertex8ID As Long = dgGraph.AddNewVertex("slow")
        Dim lngVertex9ID As Long = dgGraph.AddNewVertex("turtle")
        Dim lngVertex10ID As Long = dgGraph.AddNewVertex("obnoxious")
        Dim lngVertex11ID As Long = dgGraph.AddNewVertex("otter")
        Dim lngVertex12ID As Long = dgGraph.AddNewVertex("escaped")
        Dim lngVertex13ID As Long = dgGraph.AddNewVertex("high")

        Dim lngEdge1ID As Long = dgGraph.AddNewEdge(lngVertex1ID, lngVertex2ID)
        Dim lngEdge2ID As Long = dgGraph.AddNewEdge(lngVertex2ID, lngVertex3ID)
        Dim lngEdge3ID As Long = dgGraph.AddNewEdge(lngVertex3ID, lngVertex4ID)
        Dim lngEdge4ID As Long = dgGraph.AddNewEdge(lngVertex4ID, lngVertex5ID)
        Dim lngEdge5ID As Long = dgGraph.AddNewEdge(lngVertex1ID, lngVertex8ID)
        Dim lngEdge6ID As Long = dgGraph.AddNewEdge(lngVertex8ID, lngVertex9ID)
        Dim lngEdge7ID As Long = dgGraph.AddNewEdge(lngVertex9ID, lngVertex5ID)
        Dim lngEdge8ID As Long = dgGraph.AddNewEdge(lngVertex5ID, lngVertex6ID)
        Dim lngEdge9ID As Long = dgGraph.AddNewEdge(lngVertex6ID, lngVertex7ID)
        Dim lngEdge10ID As Long = dgGraph.AddNewEdge(lngVertex1ID, lngVertex10ID)
        Dim lngEdge11ID As Long = dgGraph.AddNewEdge(lngVertex10ID, lngVertex11ID)
        Dim lngEdge12ID As Long = dgGraph.AddNewEdge(lngVertex11ID, lngVertex12ID)
        Dim lngEdge13ID As Long = dgGraph.AddNewEdge(lngVertex5ID, lngVertex13ID)

        ' 1"the" -1> 2"quick" -2> 3"brown" -3> 4"fox" -4> 5"jumped"
        '       -5> 8"slow" -6> 9"turtle" -7> 5"jumped" -8> 6"a" -9> 7"fence"
        '       -10> 10"obnoxious" -11> 11"otter" -12> 12"escaped"
        ' 5"jumped" -13> 13"high"

        dgGraph.RemovePathsToVertex(lngVertex7ID)

        ' 1"the" -1> 2"quick" -2> 3"brown" -3> 4"fox" -4> 5"jumped"
        '       -5> 8"slow" -6> 9"turtle" -7> 5"jumped"
        '       -10> 10"obnoxious" -11> 11"otter" -12> 12"escaped"
        ' 5"jumped" -13> 13"high"

        Assert.True(dgGraph.NumVertices = 11)
        Assert.True(dgGraph.NumEdges = 11)
        Assert.True(dgGraph.GetOutgoingEdges(lngVertex5ID).Count = 1)
        Assert.True(dgGraph.GetIncomingEdges(lngVertex5ID).Count = 2)

        dgGraph.RemovePathsToVertex(lngVertex9ID)

        ' 1"the" -1> 2"quick" -2> 3"brown" -3> 4"fox" -4> 5"jumped"
        '       -10> 10"obnoxious" -11> 11"otter" -12> 12"escaped"
        ' 5"jumped" -13> 13"high"

        Assert.True(dgGraph.NumVertices = 9)
        Assert.True(dgGraph.NumEdges = 8)
        Assert.True(dgGraph.GetOutgoingEdges(lngVertex1ID).Count = 2)
        Assert.True(dgGraph.GetOutgoingEdges(lngVertex5ID).Count = 1)
        Assert.True(dgGraph.GetIncomingEdges(lngVertex5ID).Count = 1)
    End Sub

    <Test(), Description("Tests source->sink pathfinding"), Category("Methods")> _
    Public Sub TestCompleteSourceSinkPathFinding()
        Dim dgGraph As New clsDirectedGraph(Of String, String)(0)

        Dim lngVertex1ID As Long = dgGraph.AddNewVertex("the")
        Dim lngVertex2ID As Long = dgGraph.AddNewVertex("quick")
        Dim lngVertex3ID As Long = dgGraph.AddNewVertex("brown")
        Dim lngVertex4ID As Long = dgGraph.AddNewVertex("fox")
        Dim lngVertex5ID As Long = dgGraph.AddNewVertex("jumped")
        Dim lngVertex6ID As Long = dgGraph.AddNewVertex("a")
        Dim lngVertex7ID As Long = dgGraph.AddNewVertex("fence")
        Dim lngVertex8ID As Long = dgGraph.AddNewVertex("slow")
        Dim lngVertex9ID As Long = dgGraph.AddNewVertex("turtle")
        Dim lngVertex10ID As Long = dgGraph.AddNewVertex("obnoxious")
        Dim lngVertex11ID As Long = dgGraph.AddNewVertex("otter")
        Dim lngVertex12ID As Long = dgGraph.AddNewVertex("escaped")
        Dim lngVertex13ID As Long = dgGraph.AddNewVertex("high")

        Dim lngEdge1ID As Long = dgGraph.AddNewEdge(lngVertex1ID, lngVertex2ID)
        Dim lngEdge2ID As Long = dgGraph.AddNewEdge(lngVertex2ID, lngVertex3ID)
        Dim lngEdge3ID As Long = dgGraph.AddNewEdge(lngVertex3ID, lngVertex4ID)
        Dim lngEdge4ID As Long = dgGraph.AddNewEdge(lngVertex4ID, lngVertex5ID)
        Dim lngEdge5ID As Long = dgGraph.AddNewEdge(lngVertex1ID, lngVertex8ID)
        Dim lngEdge6ID As Long = dgGraph.AddNewEdge(lngVertex8ID, lngVertex9ID)
        Dim lngEdge7ID As Long = dgGraph.AddNewEdge(lngVertex9ID, lngVertex5ID)
        Dim lngEdge8ID As Long = dgGraph.AddNewEdge(lngVertex5ID, lngVertex6ID)
        Dim lngEdge9ID As Long = dgGraph.AddNewEdge(lngVertex6ID, lngVertex7ID)
        Dim lngEdge10ID As Long = dgGraph.AddNewEdge(lngVertex1ID, lngVertex10ID)
        Dim lngEdge11ID As Long = dgGraph.AddNewEdge(lngVertex10ID, lngVertex11ID)
        Dim lngEdge12ID As Long = dgGraph.AddNewEdge(lngVertex11ID, lngVertex12ID)
        Dim lngEdge13ID As Long = dgGraph.AddNewEdge(lngVertex5ID, lngVertex13ID)

        ' 1"the" -1> 2"quick" -2> 3"brown" -3> 4"fox" -4> 5"jumped"
        '       -5> 8"slow" -6> 9"turtle" -7> 5"jumped" -8> 6"a" -9> 7"fence"
        '       -10> 10"obnoxious" -11> 11"otter" -12> 12"escaped"
        ' 5"jumped" -13> 13"high"

        Dim lstPaths As List(Of List(Of Long)) = dgGraph.GetAllNonLoopingSourceSinkPaths()

        For Each lstPath As List(Of Long) In lstPaths
            Debug.Assert(dgGraph.IsSink(lstPath.Last))
        Next
    End Sub
End Class