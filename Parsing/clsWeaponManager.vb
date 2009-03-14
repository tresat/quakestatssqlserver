Option Explicit On
Option Strict On

Imports System.Data.SqlClient

Namespace LogParsing
    Public Class clsWeaponManager
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
        ''' Querys DB and populates a dictionary of Weapon Name (from log file) -> WeaponID (primary key),
        ''' for use with getting correct FK for insert into certain tables.
        ''' </summary>
        ''' <returns>Dictionary of weapon names.</returns>
        Public Function GetCurrentWeaponLookupTable() As Dictionary(Of String, Long)
            Dim dctResult As New Dictionary(Of String, Long)
            Dim strSQL As String
            Dim cmdGet As New SqlCommand

            strSQL = "SELECT w.WeaponID, w.WeaponName " & _
                    "FROM CalculatedData.Weapon w "

            cmdGet.Connection = mcxnStatsDB
            cmdGet.CommandText = strSQL

            Using reader As SqlDataReader = cmdGet.ExecuteReader
                If reader.HasRows Then
                    Do While reader.Read
                        dctResult.Add(UCase$(CStr(reader("WeaponName"))), CLng(reader("WeaponID")))
                    Loop
                End If
            End Using

            Return dctResult
        End Function

        ''' <summary>
        ''' Inserts a new weapon record into the DB.
        ''' </summary>
        ''' <param name="pstrWeaponName">Name from log file.</param>
        ''' <param name="pintWeaponLogID">Log ID of the weapon.</param>
        ''' <returns>New weapon ID on success, 0 on fail.</returns>
        Public Function CreateNewWeapon(ByVal pstrWeaponName As String, ByVal pintWeaponLogID As Integer) As Long
            Dim strSQL As String
            Dim cmdCreate As SqlCommand
            Dim trnCreate As SqlTransaction = Nothing
            Dim lngNewWeaponID As Long

            Try
                trnCreate = mcxnStatsDB.BeginTransaction

                strSQL = "INSERT INTO CalculatedData.Weapon (WeaponName, WeaponLogID) " & _
                        "VALUES (@Name, @LogID) "

                cmdCreate = New SqlCommand(strSQL, mcxnStatsDB)
                cmdCreate.Parameters.AddWithValue("Name", pstrWeaponName)
                cmdCreate.Parameters.AddWithValue("LogID", pintWeaponLogID)
                cmdCreate.Transaction = trnCreate

                cmdCreate.ExecuteNonQuery()

                strSQL = "SELECT @@IDENTITY "

                cmdCreate = New SqlCommand(strSQL, mcxnStatsDB)
                cmdCreate.Transaction = trnCreate

                lngNewWeaponID = CLng(cmdCreate.ExecuteScalar())

                trnCreate.Commit()
            Catch ex As Exception
                If trnCreate IsNot Nothing Then
                    trnCreate.Rollback()
                End If

                lngNewWeaponID = 0
            End Try

            Return lngNewWeaponID
        End Function
#End Region
    End Class
End Namespace
