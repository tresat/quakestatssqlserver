Option Strict On
Option Explicit On

Imports System.Data.SqlClient
Imports QuakeStats.LogParsing

Namespace LogParsing.QuakeObjects
    Public Class clsServerUppage
#Region "Member Vars"
        Private mcxnStatsDB As SqlConnection

        Private mlstGames As List(Of clsGame)
        Private mlngServerUppageID As Long            'If this is non-0, indicates existing uppage in db
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

        Public Property ServerUppageID() As Long
            Get
                Return mlngServerUppageID
            End Get
            Set(ByVal value As Long)
                mlngServerUppageID = value
            End Set
        End Property
#End Region

#Region "Constructors"
        Public Sub New(ByVal pcxnStatsDB As SqlConnection)
            StatsDB = pcxnStatsDB

            mlstGames = New List(Of clsGame)
            mlngServerUppageID = 0
        End Sub
#End Region

#Region "Public Functionality"
        ''' <summary>
        ''' Gets a server uppage object, with the ID populated if the uppage has
        ''' already been created in the DB.  If it has not, ID will be 0.  Will
        ''' NOT load games data for existing games.
        ''' </summary>
        ''' <param name="pcxnStatsDB">The stats DB connection.</param>
        ''' <returns>Server uppage object.</returns>
        Public Shared Function GetCurrentServerUppage(ByVal pcxnStatsDB As SqlConnection) As clsServerUppage
            Dim objServerUppage As New clsServerUppage(pcxnStatsDB)

            'Attempt to retrieve the current server uppage ID from the database
            'If we got back a 0, we have a new server uppage
            objServerUppage.ServerUppageID = CLng(clsSystemSettings.GetSystemSetting("CurrentServerUppageID", "0"))

            Return objServerUppage
        End Function

        ''' <summary>
        ''' Adds a game to the list of games
        ''' </summary>
        ''' <param name="pobjGame">The game object to add.</param>
        Public Sub AddGame(ByRef pobjGame As clsGame)
            mlstGames.Add(pobjGame)
        End Sub

        ''' <summary>
        ''' Writes the Server Uppage to the database.  Writes all games
        ''' contained in the server uppage to the db, also.
        ''' </summary>
        ''' <returns>ID of the server uppage in the DB on success, 0 on fail.</returns>
        Public Function WriteDB() As Long
            Dim strSQL As String
            Dim cmdWrite As SqlCommand = Nothing
            Dim trnWrite As SqlTransaction = Nothing
            Dim lngGameID As Long
            Dim intNumGamesWritten As Integer = 0

            Try
                'Begin tran and wrap inserts/updates so only 1 server uppage can spoil at a time
                trnWrite = mcxnStatsDB.BeginTransaction

                'Create the serveruppage in the db, if it is a new uppage
                If mlngServerUppageID = 0 Then
                    strSQL = "INSERT INTO CalculatedData.ServerUppage(fkBeginGameDelimiterLineNumber) " & _
                            "VALUES(@BeginGameLineNo) "

                    cmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
                    cmdWrite.Parameters.AddWithValue("BeginGameLineNo", mlstGames.First().BeginGameDelimiterLineNo)
                    cmdWrite.Transaction = trnWrite

                    cmdWrite.ExecuteNonQuery()

                    strSQL = "SELECT @@IDENTITY"

                    cmdWrite = New SqlCommand(strSQL, mcxnStatsDB)
                    cmdWrite.Transaction = trnWrite

                    'And save the new uppage id
                    mlngServerUppageID = CLng(cmdWrite.ExecuteScalar)
                    If mlngServerUppageID = 0 Then
                        Throw New Exception("Server uppage failed to write!")
                    End If
                End If

                For Each objGame As clsGame In mlstGames
                    'Only write those games which are fully present in the log.
                    'Partial games will not update the DB
                    If objGame.IsCompletelyPresentInLog Then
                        'Only write the games which appear in the file AFTER the last 
                        'parsed log line
                        If objGame.ShutdownGameLineNo > CLng(clsSystemSettings.GetSystemSetting("LastSavedGameShutdownLineNo", "0", trnWrite)) Then
                            'Write the current game
                            lngGameID = objGame.WriteDB(mlngServerUppageID, trnWrite)
                            If lngGameID = 0 Then
                                Throw New Exception("Game failed to write for server uppage: " & mlngServerUppageID & "!")
                            End If
                            intNumGamesWritten += 1
                        End If
                    End If
                Next

                'Update the last uppage id in the server
                clsSystemSettings.SetSystemSetting("CurrentServerUppageID", CStr(mlngServerUppageID), trnWrite)

                trnWrite.Commit()

                Print("Wrote " & intNumGamesWritten & " new games to DB...")
                Return mlngServerUppageID
            Catch ex As Exception
                trnWrite.Rollback()
                Return 0
            End Try
        End Function
#End Region
    End Class
End Namespace
