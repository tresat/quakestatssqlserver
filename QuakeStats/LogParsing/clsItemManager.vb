Option Explicit On
Option Strict On

Imports System.Data.SqlClient

Namespace LogParsing
    Public Class clsItemManager
#Region "Member Vars"
        Private mcxnStatsDB As SqlConnection
#End Region

#Region "Properties"
        Public Property StatsDB() As SqlConnection
            Get
                Return mcxnStatsDB
            End Get
            Set(ByVal value As SqlConnection)
                mcxnStatsDB = value
                VerifyDBConnected(mcxnStatsDB)
            End Set
        End Property
#End Region

#Region "Constructors"
        Public Sub New(ByVal pcxnStatsDB As SqlConnection)
            StatsDB = pcxnStatsDB
        End Sub
#End Region

#Region "Public Functionality"
        ''' <summary>
        ''' Querys DB and populates a dictionary of Item Name (from log file) -> ItemID (primary key),
        ''' for use with getting correct FK for insert into certain tables.
        ''' </summary>
        ''' <returns>Dictionary of item names.</returns>
        Public Function GetCurrentItemLookupTable() As Dictionary(Of String, Long)
            Dim dctResult As New Dictionary(Of String, Long)
            Dim strSQL As String
            Dim cmdGet As New SqlCommand

            strSQL = "SELECT i.ItemID, i.ItemName " & _
                    "FROM CalculatedData.Item i "

            cmdGet.Connection = mcxnStatsDB
            cmdGet.CommandText = strSQL

            Using reader As SqlDataReader = cmdGet.ExecuteReader
                If reader.HasRows Then
                    Do While reader.Read
                        dctResult.Add(UCase$(CStr(reader("ItemName"))), CLng(reader("ItemID")))
                    Loop
                End If
            End Using

            Return dctResult
        End Function

        ''' <summary>
        ''' Inserts a new item record into the DB.  Determines item attributes from name.
        ''' </summary>
        ''' <param name="pstrItemName">Name of the item.</param>
        ''' <param name="plngLineNumber">First line number on which item is encountered.</param>
        ''' <returns>New item ID on success, 0 on fail.</returns>
        Public Function CreateNewItem(ByVal pstrItemName As String, ByVal plngLineNumber As Long) As Long
            Dim strSQL As String
            Dim cmdCreate As SqlCommand
            Dim trnCreate As SqlTransaction = Nothing
            Dim lngNewItemID As Long
            Dim blnIsWeapon As Boolean = pstrItemName.ToUpper().StartsWith("WEAPON")
            Dim blnIsAmmo As Boolean = pstrItemName.ToUpper().StartsWith("AMMO")
            Dim blnIsFlag As Boolean = (pstrItemName.ToUpper().Contains("BLUEFLAG") OrElse pstrItemName.ToUpper().Contains("REDFLAG"))

            Try
                trnCreate = mcxnStatsDB.BeginTransaction

                strSQL = "INSERT INTO CalculatedData.Item (ItemName, IsWeapon, IsAmmo, IsFlag, fkFirstEncounteredItemLineNumber) " & _
                        "VALUES (@Name, @IsWeapon, @IsAmmo, @IsFlag, @FirstLine) "

                cmdCreate = New SqlCommand(strSQL, mcxnStatsDB)
                cmdCreate.Parameters.AddWithValue("Name", pstrItemName)
                cmdCreate.Parameters.AddWithValue("IsWeapon", IIf(blnIsWeapon, 1, 0))
                cmdCreate.Parameters.AddWithValue("IsAmmo", IIf(blnIsAmmo, 1, 0))
                cmdCreate.Parameters.AddWithValue("IsFlag", IIf(blnIsFlag, 1, 0))
                cmdCreate.Parameters.AddWithValue("FirstLine", plngLineNumber)
                cmdCreate.Transaction = trnCreate

                cmdCreate.ExecuteNonQuery()

                strSQL = "SELECT @@IDENTITY "

                cmdCreate = New SqlCommand(strSQL, mcxnStatsDB)
                cmdCreate.Transaction = trnCreate

                lngNewItemID = CLng(cmdCreate.ExecuteScalar())

                trnCreate.Commit()
            Catch ex As Exception
                If trnCreate IsNot Nothing Then
                    trnCreate.Rollback()
                End If

                lngNewItemID = 0
            End Try

            Return lngNewItemID
        End Function
#End Region
    End Class
End Namespace
