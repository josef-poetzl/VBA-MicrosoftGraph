VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "Form_ListMessages"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Compare Database
Option Explicit
Dim sGrantType As String

Private Sub cmdListEmails_Click()
    CurrentDb.Execute "DELETE FROM TempEmailsTable"
    If Nz(Me.fromMailbox, "") = "" And sGrantType = "client_credentials" Then
        MsgBox "From whose mailbox is mandatory and must be a valid mailbox in this client."
        Me.fromMailbox.SetFocus
    Else
        'Second parameter is optional, if filled in use the name of a folder,
        ListOutlookMessages Graph.ListMessages(Nz(Me.fromMailbox, ""), Nz(Me.folderName, ""))
        Me.lstEmails.Requery
        Me.lstEmails.Visible = True
    End If
End Sub

Private Sub Form_Open(Cancel As Integer)
    CurrentDb.Execute "DELETE FROM TempEmailsTable"
    sGrantType = DLookup("GrantType", "AdminTable") 'Authorization grant type
    If sGrantType = "client_credentials" Then
        Me.fromMailbox.Visible = True
    Else
        Me.fromMailbox.Visible = False
    End If
End Sub

Private Sub ListOutlookMessages(Response As WebResponse)
    If Response.StatusCode = WebStatusCode.OK Then
        Dim EmailInfo As Dictionary
        Dim FromAddress As Dictionary
        Dim EmailAddress As Dictionary
        Dim EmailBody As Dictionary
        Dim rs As Recordset
                
        If IsEmpty(Response.Data("value")) Then
            MsgBox "No contacts for this folder in this tenant."
        Else
            Set rs = CurrentDb.OpenRecordset("TempEmailsTable")
            For Each EmailInfo In Response.Data("value")
                rs.AddNew
                rs.Fields("id").Value = EmailInfo("id")
                rs.Fields("createdDateTime").Value = EmailInfo("createdDateTime")
                rs.Fields("receivedDateTime").Value = EmailInfo("receivedDateTime")
                Set FromAddress = EmailInfo("from")
                Set EmailAddress = FromAddress("emailAddress")
                rs.Fields("fromemailAddress").Value = EmailAddress("address")
                rs.Fields("subject").Value = EmailInfo("subject")
                rs.Fields("bodyPreview").Value = Left(EmailInfo("bodyPreview"), 255)
                Set EmailBody = EmailInfo("body")
                rs.Fields("bodyContent").Value = EmailBody("content")
                rs.Update
            Next EmailInfo
            rs.Close
        End If
        Set rs = Nothing
    Else
        MsgBox "Error " & Response.StatusCode & ": " & Response.Content
    End If
End Sub
