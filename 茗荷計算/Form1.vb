Public Class Form1
    Inherits System.Windows.Forms.Form

#Region " Windows フォーム デザイナで生成されたコード "
    Public Sub New()
        MyBase.New() ' この呼び出しは Windows フォーム デザイナで必要です。
        InitializeComponent() ' InitializeComponent() 呼び出しの後に初期化を追加します。
    End Sub
    ' Form は、コンポーネント一覧に後処理を実行するために dispose をオーバーライドします。
    Protected Overloads Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing Then
            If Not (components Is Nothing) Then
                components.Dispose()
            End If
        End If
        MyBase.Dispose(disposing)
    End Sub
    ' Windows フォーム デザイナで必要です。
    Private components As System.ComponentModel.IContainer
    ' メモ : 以下のプロシージャは、Windows フォーム デザイナで必要です。
    'Windows フォーム デザイナを使って変更してください。  
    ' コード エディタを使って変更しないでください。
    Friend WithEvents PictureBox1 As System.Windows.Forms.PictureBox
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.PictureBox1 = New System.Windows.Forms.PictureBox
        Me.SuspendLayout()
        '
        'PictureBox1
        '
        Me.PictureBox1.BackColor = System.Drawing.Color.White
        Me.PictureBox1.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.PictureBox1.Location = New System.Drawing.Point(8, 144)
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.Size = New System.Drawing.Size(616, 400)
        Me.PictureBox1.TabIndex = 0
        Me.PictureBox1.TabStop = False
        '
        'Form1
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 12)
        Me.ClientSize = New System.Drawing.Size(632, 549)
        Me.Controls.Add(Me.PictureBox1)
        Me.Name = "Form1"
        Me.Text = "Form1"
        Me.ResumeLayout(False)

    End Sub
#End Region
    Public picture1 As PictureBox = PictureBox1


    Public monitor1 As monitor 'monitor form を参照
    Public monitorshowed As Boolean ' monitor form がすでに表示されたかどうか
    Public grf1 As System.Drawing.Graphics = Me.creategraphics
    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        graph.setgraphics(PictureBox1.CreateGraphics)
        forms.setParentForm(Me)
        forms.CreateButton(IntPoint(100, 40), IntPoint(300, 139), "0", "square2")

        Dim p1 As New Point(0, 10)
        Dim p2 As New Point(49, 39)
        Dim a
        For a = 0 To 5
            p1.X += 50
            p2.X += 50
            forms.CreateButton(p1, p2, a + 1, "square2")
        Next a

        forms.CreateButton(IntPoint(0, 40), IntPoint(100, 139), "f", "circle")
        forms.CreateButton(IntPoint(350, 10), IntPoint(399, 59), "graph1", "square2")
        forms.CreateButton(IntPoint(400, 120), IntPoint(439, 139), "左", "square")
        forms.CreateButton(IntPoint(440, 120), IntPoint(479, 139), "右", "square")
        forms.CreateButton(IntPoint(480, 100), IntPoint(519, 119), "奥", "square")
        forms.CreateButton(IntPoint(480, 120), IntPoint(519, 139), "手前", "square")
        forms.buttons(7).text = "  環境の表示"
        forms.buttons(forms.IndexFromID("右")).text = " 右"
        forms.buttons(forms.IndexFromID("左")).text = " 左"
        forms.buttons(forms.IndexFromID("手前")).text = " 手前"
        forms.buttons(forms.IndexFromID("奥")).text = " 奥"
    End Sub
#Region "some events"
    Private Sub Form1_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles MyBase.MouseDown
        forms.MouseDown(grf1, PointToClient(MousePosition))
        If monitorshowed = True Then monitor1.showmousebuttons()
    End Sub
    Private Sub Form1_MouseUp(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles MyBase.MouseUp
        forms.MouseUp(grf1, PointToClient(MousePosition))
        If monitorshowed = True Then monitor1.showmousebuttons()
    End Sub
    Private Sub Form1_MouseMove(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles MyBase.MouseMove
        If monitorshowed = True Then monitor1.mousemoving()
    End Sub
    Private Sub Form1_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles MyBase.KeyDown
        forms.KeyDown(e.KeyCode Mod 65532)
        If monitorshowed = True Then monitor1.showkeys()
    End Sub
    Private Sub Form1_KeyUp(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles MyBase.KeyUp
        forms.KeyUp(e.KeyCode Mod 65532)
        If monitorshowed = True Then monitor1.showkeys()
    End Sub
    Private Sub Form1_LostFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.LostFocus
        forms.LFocus()
        If monitorshowed = True Then monitor1.showkeys()
    End Sub
    Private Sub Form1_Activated(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Activated
        Call reset()
    End Sub
    Private Sub Form1_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Resize
        grf1 = Me.CreateGraphics
        ' graph.setgraphics(grf1)
        Call reset()
    End Sub
    Public Sub reset()
        Dim a
        For a = 0 To forms.button_cnt - 1
            forms.buttons(a).showButton(grf1)
        Next
    End Sub
#End Region


End Class
