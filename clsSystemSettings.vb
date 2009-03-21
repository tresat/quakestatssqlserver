Option Strict On
Option Explicit On

Imports System.Data.SqlClient

Public Class clsSystemSettings
    Public Shared mcxnDB As SqlConnection

    Public Shared Property DBConnection() As SqlConnection
        Get
            Return mcxnDB
        End Get
        Set(ByVal value As SqlConnection)
            mcxnDB = value
            VerifyDBConnected(mcxnDB)
        End Set
    End Property


    Public Shared Function GetSystemSetting(ByVal pstrKey As String, _
            Optional ByVal pstrValue As String = Nothing, _
            Optional ByRef ptrnCurrent As SqlTransaction = Nothing) As String
        Dim strSQL As String
        Dim cmdGet As SqlCommand
        Dim objResult As Object

        strSQL = "SELECT ss.SettingValue " & _
                "FROM SiteData.SystemSetting ss " & _
                "WHERE ss.SettingKey = @Key "

        cmdGet = New SqlCommand
        cmdGet.CommandText = strSQL
        cmdGet.Connection = mcxnDB
        If ptrnCurrent IsNot Nothing Then cmdGet.Transaction = ptrnCurrent

        cmdGet.Parameters.AddWithValue("@Key", pstrKey)

        objResult = cmdGet.ExecuteScalar

        'If result was nothing and we were supplied a default value, insert it
        If (objResult Is Nothing) And pstrValue IsNot Nothing Then
            strSQL = "INSERT INTO SiteData.SystemSetting (SettingKey, SettingValue) " & _
                    "VALUES (@Key, @Value) "

            cmdGet = New SqlCommand
            cmdGet.CommandText = strSQL
            cmdGet.Connection = mcxnDB
            If ptrnCurrent IsNot Nothing Then cmdGet.Transaction = ptrnCurrent

            cmdGet.Parameters.AddWithValue("@Key", pstrKey)
            cmdGet.Parameters.AddWithValue("@Value", pstrValue)

            cmdGet.ExecuteNonQuery()
            objResult = pstrValue
        End If

        Return CStr(objResult)
    End Function

    Public Shared Sub SetSystemSetting(ByVal pstrKey As String, _
            ByVal pstrValue As String, _
            Optional ByRef ptrnCurrent As SqlTransaction = Nothing)
        Dim strSQL As String
        Dim cmdSet As SqlCommand
        Dim objResult As Object

        strSQL = "SELECT * " & _
                "FROM SiteData.SystemSetting S " & _
                "WHERE S.SettingKey = @Key "

        cmdSet = New SqlCommand(strSQL, mcxnDB)
        cmdSet.Parameters.AddWithValue("@Key", pstrKey)
        If ptrnCurrent IsNot Nothing Then cmdSet.Transaction = ptrnCurrent

        objResult = cmdSet.ExecuteScalar

        'If result was nothing, insert it
        If (objResult Is Nothing) And pstrValue IsNot Nothing Then
            strSQL = "INSERT INTO SiteData.SystemSetting (SettingKey, SettingValue) " & _
                        "VALUES (@Key, @Value) "

            cmdSet = New SqlCommand
            cmdSet.CommandText = strSQL
            cmdSet.Connection = mcxnDB
            If ptrnCurrent IsNot Nothing Then cmdSet.Transaction = ptrnCurrent

            cmdSet.Parameters.AddWithValue("@Key", pstrKey)
            cmdSet.Parameters.AddWithValue("@Value", pstrValue)

            cmdSet.ExecuteNonQuery()
        Else 'update the setting
            strSQL = "UPDATE SiteData.SystemSetting SET SettingValue = @Value WHERE SettingKey = @Key "

            cmdSet = New SqlCommand
            cmdSet.CommandText = strSQL
            cmdSet.Connection = mcxnDB
            If ptrnCurrent IsNot Nothing Then cmdSet.Transaction = ptrnCurrent

            cmdSet.Parameters.AddWithValue("@Key", pstrKey)
            cmdSet.Parameters.AddWithValue("@Value", pstrValue)

            cmdSet.ExecuteNonQuery()
        End If
    End Sub
End Class
