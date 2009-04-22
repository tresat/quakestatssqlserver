Option Explicit On
Option Strict On

Namespace Graph
    Public Class clsDirectedGraph(Of GraphVertexPayload, GraphEdgePayload)
        Inherits clsGraph(Of GraphVertexPayload, GraphEdgePayload)
#Region "Inner Types"
#Region "Vertex"
        Protected Friend Class clsDirectedGraphVertex(Of VertexPayload)
            Inherits clsGraph(Of GraphVertexPayload, GraphEdgePayload).clsVertex(Of VertexPayload)

            Public Sub New(ByVal plngVertexID As Long, Optional ByRef pvpPayload As VertexPayload = Nothing)
                MyBase.New(plngVertexID, pvpPayload)
            End Sub
        End Class
#End Region

#Region "Edge"
        Protected Friend Class clsDirectedGraphEdge(Of EdgePayload)
            Inherits clsGraph(Of GraphVertexPayload, GraphEdgePayload).clsEdge(Of EdgePayload)
#Region "Member Vars"
            Private mlngStartVertex As Long
#End Region

#Region "Constructors"
            Protected Sub New(ByVal plngEdgeID As Long, ByVal plngStartVertexID As Long, ByVal plngEndVertexID As Long, Optional ByRef pepPayload As EdgePayload = Nothing)
                MyBase.New(plngEdgeID, plngStartVertexID, plngEndVertexID, pepPayload)

                mlngStartVertex = plngStartVertexID
            End Sub
#End Region

#Region "Public Functionality"
            ''' <summary>
            ''' Reverses the direction of the edge
            ''' </summary>
            Public Sub ReverseDirection()
                mlngStartVertex = CLng(IIf(mlngStartVertex = mlngVertexID1, mlngVertexID2, mlngVertexID1))
            End Sub
#End Region
        End Class
#End Region
#End Region

#Region "Member Vars"
        Private mdctSourceVertices As Dictionary(Of Long, clsDirectedGraphVertex(Of GraphVertexPayload))
        Private mdctSinkVertices As Dictionary(Of Long, clsDirectedGraphVertex(Of GraphVertexPayload))
#End Region

#Region "Constructors"
        ''' <summary>
        ''' Initializes a new instance of the <see cref="clsDirectedGraph(Of GraphVertexPayload, GraphEdgePayload)" /> class.
        ''' If source payloads provided, count must match number of source nodes to create.
        ''' </summary>
        ''' <exception cref="ArgumentException">When count of source payloads doesn't match num source nodes to create.</exception>
        ''' <param name="pintNumSources">The number of disconnected source nodes to create.</param>
        ''' <param name="plstSourcePayloads">The source payloads for each source node.</param>
        Public Sub New(Optional ByVal pintNumSources As Integer = 1, Optional ByVal plstSourcePayloads As List(Of GraphVertexPayload) = Nothing)
            MyBase.New()

            If plstSourcePayloads IsNot Nothing AndAlso pintNumSources <> plstSourcePayloads.Count Then
                Throw New ArgumentException("If source payloads list provided, count must match number of sources!")
            End If

            mdctSourceVertices = New Dictionary(Of Long, clsDirectedGraphVertex(Of GraphVertexPayload))
            mdctSinkVertices = New Dictionary(Of Long, clsDirectedGraphVertex(Of GraphVertexPayload))

            For intIdx As Integer = 0 To pintNumSources - 1
                'Use nothing as source for all nodes, if no list of payloads provided.
                If plstSourcePayloads IsNot Nothing Then
                    AddSourceVertex(plstSourcePayloads(intIdx))
                Else
                    AddSourceVertex(Nothing)
                End If
            Next
        End Sub
#End Region

#Region "Public Functionality"
        ''' <summary>
        ''' Adds a new source vertex.
        ''' </summary>
        ''' <param name="pvpPayload">The payload for the new vertex.</param>
        ''' <returns>The new vertex ID.</returns>
        Public Function AddSourceVertex(Optional ByRef pvpPayload As GraphVertexPayload = Nothing) As Long
            Dim vNew As New clsDirectedGraphVertex(Of GraphVertexPayload)(mlngNextVertexID, pvpPayload)

            mdctSourceVertices.Add(vNew.VertexID, vNew)
            mdctVertices.Add(vNew.VertexID, vNew)

            mlngNextVertexID += 1

            Return vNew.VertexID
        End Function
#End Region
    End Class
End Namespace
