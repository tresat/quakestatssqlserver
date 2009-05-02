Option Strict On
Option Explicit On

Imports NUnit.Framework
Imports GraphLibrary.Graph

<TestFixture()> _
Public Class clsGraphTester
    <Test(), Description("TestGraphCreation"), Category("Constructors")> _
    Public Sub TestCreateGraph()
        Dim g As clsGraph(Of Object, Object)

        g = New clsGraph(Of Object, Object)

        Assert.IsNotNull(g)
    End Sub
End Class