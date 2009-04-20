Option Explicit On
Option Strict On

Namespace Graph
    Public Class clsDirectedGraph(Of DirectedGraphVertexPayload, DirectedGraphEdgePayload)
        Inherits clsGraph(Of DirectedGraphVertexPayload, DirectedGraphEdgePayload)
#Region "Inner Types"
#Region "Vertex"
        Public Class clsDirectedGraphVertex(Of VertexPayload)
            Inherits clsGraph(Of DirectedGraphVertexPayload, DirectedGraphEdgePayload).clsVertex(Of VertexPayload)

            Public Sub New(ByVal plngVertexID As Long, Optional ByRef pvpPayload As VertexPayload = Nothing)
                MyBase.New(plngVertexID, pvpPayload)
            End Sub
        End Class
#End Region

#Region "Edge"
        Public Class clsDirectedGraphEdge(Of EdgePayload)
            Inherits clsGraph(Of DirectedGraphVertexPayload, DirectedGraphEdgePayload).clsEdge(Of EdgePayload)

            Public Sub New(ByVal plngEdgeID As Long, Optional ByRef pepPayload As EdgePayload = Nothing)
                MyBase.New(plngEdgeID, pepPayload)
            End Sub
        End Class
#End Region
#End Region

#Region "Member Vars"
        Private mdctSourceVertices As Dictionary(Of Long, clsDirectedGraphVertex(Of DirectedGraphVertexPayload))
#End Region

#Region "Constructors"
        Public Sub New(Optional ByVal pintNumSources As Integer = 1)
            MyBase.New()

            mdctSourceVertices = New Dictionary(Of Long, clsDirectedGraphVertex(Of DirectedGraphVertexPayload))

            For intIdx As Integer = 1 To pintNumSources
                AddSourceVertex()
            Next
        End Sub
#End Region

#Region "Public Functionality"
        ''' <summary>
        ''' Adds a new source vertex.
        ''' </summary>
        ''' <param name="pvpPayload">The payload for the new vertex.</param>
        ''' <returns>The new vertex ID.</returns>
        Public Function AddSourceVertex(Optional ByRef pvpPayload As DirectedGraphVertexPayload = Nothing) As Long
            Dim vNew As New clsDirectedGraphVertex(Of DirectedGraphVertexPayload)(mlngNextVertexID, pvpPayload)

            mdctSourceVertices.Add(vNew.VertexID, vNew)

            mlngNextVertexID += 1

            Return vNew.VertexID
        End Function
#End Region
    End Class
End Namespace
