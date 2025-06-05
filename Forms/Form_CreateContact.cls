VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "Form_CreateContact"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Compare Database
Option Explicit
Dim sGrantType As String

Private Sub cmdCreateContact_Click()
    If IsNull(fileAs) Or IsNull(sEmailAddresses) Then
        MsgBox "File As and Email are mandatory"
        Exit Sub
    End If
    If Nz(Me.fromMailbox, "") = "" And sGrantType = "client_credentials" Then
        MsgBox "From whose mailbox is mandatory and must be a valid mailbox in this client."
        Me.fromMailbox.SetFocus
    Else
        'Second parameter should be either an empty string "" or use the name of a folder
        'for example my contacts are in TempContacts so I use that
        CreateGraphContact Graph.CreateContact(Nz(fromMailbox, ""), Nz(folderName, ""), Nz(Me.givenName, ""), Nz(Me.surname, ""), Me.fileAs, Nz(Me.jobTitle, ""), Nz(Me.companyName, ""), Nz(sBusinessPhones, ""), Me.sEmailAddresses)
    End If
End Sub

Private Sub CreateGraphContact(Response As WebResponse)
    If Response.StatusCode = WebStatusCode.Created Then
        MsgBox "Contact created."
    Else
        MsgBox "Error " & Response.StatusCode & ": " & Response.Content
    End If
End Sub

Private Sub surname_AfterUpdate()
    If IsNull(Me.fileAs) Then Me.fileAs = Me.surname & ", " & Me.givenName
End Sub

Private Sub Form_Open(Cancel As Integer)
    sGrantType = DLookup("GrantType", "AdminTable") 'Authorization grant type
    If sGrantType = "client_credentials" Then
        Me.fromMailbox.Visible = True
    Else
        Me.fromMailbox.Visible = False
    End If
End Sub

