Public Class monitor
    Inherits System.Windows.Forms.Form

#Region " Windows フォーム デザイナで生成されたコード "

    Public Sub New()
        MyBase.New()

        ' この呼び出しは Windows フォーム デザイナで必要です。
        InitializeComponent()

        ' InitializeComponent() 呼び出しの後に初期化を追加します。

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
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Label6 As System.Windows.Forms.Label
    Friend WithEvents Label7 As System.Windows.Forms.Label
    Friend WithEvents Timer1 As System.Windows.Forms.Timer
    Friend WithEvents Label8 As System.Windows.Forms.Label
    Friend WithEvents ListBox1 As System.Windows.Forms.ListBox
    Friend WithEvents Label9 As System.Windows.Forms.Label
    Friend WithEvents Label10 As System.Windows.Forms.Label
    Friend WithEvents Label11 As System.Windows.Forms.Label
    Friend WithEvents Label12 As System.Windows.Forms.Label
    Friend WithEvents Label13 As System.Windows.Forms.Label
    Friend WithEvents Label14 As System.Windows.Forms.Label
    Friend WithEvents Label15 As System.Windows.Forms.Label
    Friend WithEvents Label16 As System.Windows.Forms.Label
    Friend WithEvents Label17 As System.Windows.Forms.Label
    Friend WithEvents Label18 As System.Windows.Forms.Label
    Friend WithEvents Label19 As System.Windows.Forms.Label
    Friend WithEvents Label20 As System.Windows.Forms.Label
    Friend WithEvents Label21 As System.Windows.Forms.Label
    Friend WithEvents Label22 As System.Windows.Forms.Label
    Friend WithEvents Label23 As System.Windows.Forms.Label
    Friend WithEvents Label24 As System.Windows.Forms.Label
    Friend WithEvents Label25 As System.Windows.Forms.Label
    Friend WithEvents Label26 As System.Windows.Forms.Label
    Friend WithEvents Label27 As System.Windows.Forms.Label
    Friend WithEvents Label28 As System.Windows.Forms.Label
    Friend WithEvents Label29 As System.Windows.Forms.Label
    Friend WithEvents Label30 As System.Windows.Forms.Label
    Friend WithEvents Label31 As System.Windows.Forms.Label
    Friend WithEvents Label32 As System.Windows.Forms.Label
    Friend WithEvents Label33 As System.Windows.Forms.Label
    Friend WithEvents Label34 As System.Windows.Forms.Label
    Friend WithEvents Label35 As System.Windows.Forms.Label
    Friend WithEvents Label36 As System.Windows.Forms.Label
    Friend WithEvents Label37 As System.Windows.Forms.Label
    Friend WithEvents Label38 As System.Windows.Forms.Label
    Friend WithEvents Label39 As System.Windows.Forms.Label
    Friend WithEvents Label40 As System.Windows.Forms.Label
    Friend WithEvents Label41 As System.Windows.Forms.Label
    Friend WithEvents Label42 As System.Windows.Forms.Label
    Friend WithEvents Label43 As System.Windows.Forms.Label
    Friend WithEvents Label44 As System.Windows.Forms.Label
    Friend WithEvents Label45 As System.Windows.Forms.Label
    Friend WithEvents Label46 As System.Windows.Forms.Label
    Friend WithEvents Label47 As System.Windows.Forms.Label
    Friend WithEvents Label48 As System.Windows.Forms.Label
    Friend WithEvents Label49 As System.Windows.Forms.Label
    Friend WithEvents Label50 As System.Windows.Forms.Label
    Friend WithEvents Label51 As System.Windows.Forms.Label
    Friend WithEvents Label52 As System.Windows.Forms.Label
    Friend WithEvents Label53 As System.Windows.Forms.Label
    Friend WithEvents Label54 As System.Windows.Forms.Label
    Friend WithEvents Label55 As System.Windows.Forms.Label
    Friend WithEvents Label56 As System.Windows.Forms.Label
    Friend WithEvents Label57 As System.Windows.Forms.Label
    Friend WithEvents Label58 As System.Windows.Forms.Label
    Friend WithEvents Label59 As System.Windows.Forms.Label
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Me.Label5 = New System.Windows.Forms.Label
        Me.Label3 = New System.Windows.Forms.Label
        Me.Label4 = New System.Windows.Forms.Label
        Me.Label2 = New System.Windows.Forms.Label
        Me.Label1 = New System.Windows.Forms.Label
        Me.Label6 = New System.Windows.Forms.Label
        Me.Label7 = New System.Windows.Forms.Label
        Me.Timer1 = New System.Windows.Forms.Timer(Me.components)
        Me.Label8 = New System.Windows.Forms.Label
        Me.ListBox1 = New System.Windows.Forms.ListBox
        Me.Label9 = New System.Windows.Forms.Label
        Me.Label10 = New System.Windows.Forms.Label
        Me.Label11 = New System.Windows.Forms.Label
        Me.Label12 = New System.Windows.Forms.Label
        Me.Label13 = New System.Windows.Forms.Label
        Me.Label14 = New System.Windows.Forms.Label
        Me.Label15 = New System.Windows.Forms.Label
        Me.Label16 = New System.Windows.Forms.Label
        Me.Label17 = New System.Windows.Forms.Label
        Me.Label18 = New System.Windows.Forms.Label
        Me.Label19 = New System.Windows.Forms.Label
        Me.Label20 = New System.Windows.Forms.Label
        Me.Label21 = New System.Windows.Forms.Label
        Me.Label22 = New System.Windows.Forms.Label
        Me.Label23 = New System.Windows.Forms.Label
        Me.Label24 = New System.Windows.Forms.Label
        Me.Label25 = New System.Windows.Forms.Label
        Me.Label26 = New System.Windows.Forms.Label
        Me.Label27 = New System.Windows.Forms.Label
        Me.Label28 = New System.Windows.Forms.Label
        Me.Label29 = New System.Windows.Forms.Label
        Me.Label30 = New System.Windows.Forms.Label
        Me.Label31 = New System.Windows.Forms.Label
        Me.Label32 = New System.Windows.Forms.Label
        Me.Label33 = New System.Windows.Forms.Label
        Me.Label34 = New System.Windows.Forms.Label
        Me.Label35 = New System.Windows.Forms.Label
        Me.Label36 = New System.Windows.Forms.Label
        Me.Label37 = New System.Windows.Forms.Label
        Me.Label38 = New System.Windows.Forms.Label
        Me.Label39 = New System.Windows.Forms.Label
        Me.Label40 = New System.Windows.Forms.Label
        Me.Label41 = New System.Windows.Forms.Label
        Me.Label42 = New System.Windows.Forms.Label
        Me.Label43 = New System.Windows.Forms.Label
        Me.Label44 = New System.Windows.Forms.Label
        Me.Label45 = New System.Windows.Forms.Label
        Me.Label46 = New System.Windows.Forms.Label
        Me.Label47 = New System.Windows.Forms.Label
        Me.Label48 = New System.Windows.Forms.Label
        Me.Label49 = New System.Windows.Forms.Label
        Me.Label50 = New System.Windows.Forms.Label
        Me.Label51 = New System.Windows.Forms.Label
        Me.Label52 = New System.Windows.Forms.Label
        Me.Label53 = New System.Windows.Forms.Label
        Me.Label54 = New System.Windows.Forms.Label
        Me.Label55 = New System.Windows.Forms.Label
        Me.Label56 = New System.Windows.Forms.Label
        Me.Label57 = New System.Windows.Forms.Label
        Me.Label58 = New System.Windows.Forms.Label
        Me.Label59 = New System.Windows.Forms.Label
        Me.SuspendLayout()
        '
        'Label5
        '
        Me.Label5.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(192, Byte))
        Me.Label5.Location = New System.Drawing.Point(424, 248)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(48, 16)
        Me.Label5.TabIndex = 10
        Me.Label5.Text = "下ボタン"
        '
        'Label3
        '
        Me.Label3.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(192, Byte))
        Me.Label3.Location = New System.Drawing.Point(464, 168)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(16, 48)
        Me.Label3.TabIndex = 9
        Me.Label3.Text = "中ボタン"
        '
        'Label4
        '
        Me.Label4.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(192, Byte))
        Me.Label4.Location = New System.Drawing.Point(424, 224)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(48, 16)
        Me.Label4.TabIndex = 8
        Me.Label4.Text = "上ボタン"
        '
        'Label2
        '
        Me.Label2.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(192, Byte))
        Me.Label2.Location = New System.Drawing.Point(488, 168)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(16, 48)
        Me.Label2.TabIndex = 7
        Me.Label2.Text = "右ボタン"
        '
        'Label1
        '
        Me.Label1.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(192, Byte))
        Me.Label1.Location = New System.Drawing.Point(440, 168)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(16, 48)
        Me.Label1.TabIndex = 6
        Me.Label1.Text = "左ボタン"
        '
        'Label6
        '
        Me.Label6.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(192, Byte))
        Me.Label6.Location = New System.Drawing.Point(480, 248)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(48, 16)
        Me.Label6.TabIndex = 12
        Me.Label6.Text = "y:0"
        '
        'Label7
        '
        Me.Label7.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(192, Byte))
        Me.Label7.Location = New System.Drawing.Point(480, 224)
        Me.Label7.Name = "Label7"
        Me.Label7.Size = New System.Drawing.Size(48, 16)
        Me.Label7.TabIndex = 11
        Me.Label7.Text = "x:0"
        '
        'Timer1
        '
        Me.Timer1.Interval = 1000
        '
        'Label8
        '
        Me.Label8.BackColor = System.Drawing.Color.FromArgb(CType(192, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label8.ForeColor = System.Drawing.Color.Black
        Me.Label8.Location = New System.Drawing.Point(400, 8)
        Me.Label8.Name = "Label8"
        Me.Label8.Size = New System.Drawing.Size(32, 16)
        Me.Label8.TabIndex = 13
        Me.Label8.Text = "Ctrl"
        Me.Label8.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'ListBox1
        '
        Me.ListBox1.ItemHeight = 12
        Me.ListBox1.Location = New System.Drawing.Point(8, 8)
        Me.ListBox1.Name = "ListBox1"
        Me.ListBox1.Size = New System.Drawing.Size(376, 208)
        Me.ListBox1.TabIndex = 14
        '
        'Label9
        '
        Me.Label9.BackColor = System.Drawing.Color.FromArgb(CType(192, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label9.Location = New System.Drawing.Point(400, 32)
        Me.Label9.Name = "Label9"
        Me.Label9.Size = New System.Drawing.Size(32, 16)
        Me.Label9.TabIndex = 15
        Me.Label9.Text = "Alt"
        Me.Label9.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label10
        '
        Me.Label10.BackColor = System.Drawing.Color.FromArgb(CType(192, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label10.Location = New System.Drawing.Point(400, 56)
        Me.Label10.Name = "Label10"
        Me.Label10.Size = New System.Drawing.Size(32, 16)
        Me.Label10.TabIndex = 17
        Me.Label10.Text = "Shift"
        Me.Label10.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label11
        '
        Me.Label11.BackColor = System.Drawing.Color.FromArgb(CType(192, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label11.ForeColor = System.Drawing.Color.Black
        Me.Label11.Location = New System.Drawing.Point(400, 80)
        Me.Label11.Name = "Label11"
        Me.Label11.Size = New System.Drawing.Size(32, 16)
        Me.Label11.TabIndex = 16
        Me.Label11.Text = "Fn"
        Me.Label11.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label12
        '
        Me.Label12.BackColor = System.Drawing.Color.FromArgb(CType(192, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label12.Location = New System.Drawing.Point(440, 80)
        Me.Label12.Name = "Label12"
        Me.Label12.Size = New System.Drawing.Size(24, 16)
        Me.Label12.TabIndex = 21
        Me.Label12.Text = "F4"
        Me.Label12.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label13
        '
        Me.Label13.BackColor = System.Drawing.Color.FromArgb(CType(192, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label13.ForeColor = System.Drawing.Color.Black
        Me.Label13.Location = New System.Drawing.Point(440, 56)
        Me.Label13.Name = "Label13"
        Me.Label13.Size = New System.Drawing.Size(24, 16)
        Me.Label13.TabIndex = 20
        Me.Label13.Text = "F3"
        Me.Label13.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label14
        '
        Me.Label14.BackColor = System.Drawing.Color.FromArgb(CType(192, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label14.Location = New System.Drawing.Point(440, 32)
        Me.Label14.Name = "Label14"
        Me.Label14.Size = New System.Drawing.Size(24, 16)
        Me.Label14.TabIndex = 19
        Me.Label14.Text = "F2"
        Me.Label14.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label15
        '
        Me.Label15.BackColor = System.Drawing.Color.FromArgb(CType(192, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label15.ForeColor = System.Drawing.Color.Black
        Me.Label15.Location = New System.Drawing.Point(440, 8)
        Me.Label15.Name = "Label15"
        Me.Label15.Size = New System.Drawing.Size(24, 16)
        Me.Label15.TabIndex = 18
        Me.Label15.Text = "F1"
        Me.Label15.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label16
        '
        Me.Label16.BackColor = System.Drawing.Color.FromArgb(CType(192, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label16.Location = New System.Drawing.Point(472, 80)
        Me.Label16.Name = "Label16"
        Me.Label16.Size = New System.Drawing.Size(24, 16)
        Me.Label16.TabIndex = 25
        Me.Label16.Text = "F8"
        Me.Label16.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label17
        '
        Me.Label17.BackColor = System.Drawing.Color.FromArgb(CType(192, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label17.ForeColor = System.Drawing.Color.Black
        Me.Label17.Location = New System.Drawing.Point(472, 56)
        Me.Label17.Name = "Label17"
        Me.Label17.Size = New System.Drawing.Size(24, 16)
        Me.Label17.TabIndex = 24
        Me.Label17.Text = "F7"
        Me.Label17.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label18
        '
        Me.Label18.BackColor = System.Drawing.Color.FromArgb(CType(192, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label18.Location = New System.Drawing.Point(472, 32)
        Me.Label18.Name = "Label18"
        Me.Label18.Size = New System.Drawing.Size(24, 16)
        Me.Label18.TabIndex = 23
        Me.Label18.Text = "F6"
        Me.Label18.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label19
        '
        Me.Label19.BackColor = System.Drawing.Color.FromArgb(CType(192, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label19.ForeColor = System.Drawing.Color.Black
        Me.Label19.Location = New System.Drawing.Point(472, 8)
        Me.Label19.Name = "Label19"
        Me.Label19.Size = New System.Drawing.Size(24, 16)
        Me.Label19.TabIndex = 22
        Me.Label19.Text = "F5"
        Me.Label19.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label20
        '
        Me.Label20.BackColor = System.Drawing.Color.FromArgb(CType(192, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label20.Location = New System.Drawing.Point(504, 80)
        Me.Label20.Name = "Label20"
        Me.Label20.Size = New System.Drawing.Size(24, 16)
        Me.Label20.TabIndex = 29
        Me.Label20.Text = "F12"
        Me.Label20.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label21
        '
        Me.Label21.BackColor = System.Drawing.Color.FromArgb(CType(192, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label21.ForeColor = System.Drawing.Color.Black
        Me.Label21.Location = New System.Drawing.Point(504, 56)
        Me.Label21.Name = "Label21"
        Me.Label21.Size = New System.Drawing.Size(24, 16)
        Me.Label21.TabIndex = 28
        Me.Label21.Text = "F11"
        Me.Label21.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label22
        '
        Me.Label22.BackColor = System.Drawing.Color.FromArgb(CType(192, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label22.Location = New System.Drawing.Point(504, 32)
        Me.Label22.Name = "Label22"
        Me.Label22.Size = New System.Drawing.Size(24, 16)
        Me.Label22.TabIndex = 27
        Me.Label22.Text = "F10"
        Me.Label22.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label23
        '
        Me.Label23.BackColor = System.Drawing.Color.FromArgb(CType(192, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label23.ForeColor = System.Drawing.Color.Black
        Me.Label23.Location = New System.Drawing.Point(504, 8)
        Me.Label23.Name = "Label23"
        Me.Label23.Size = New System.Drawing.Size(24, 16)
        Me.Label23.TabIndex = 26
        Me.Label23.Text = "F9"
        Me.Label23.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label24
        '
        Me.Label24.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label24.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label24.Location = New System.Drawing.Point(32, 272)
        Me.Label24.Name = "Label24"
        Me.Label24.Size = New System.Drawing.Size(24, 24)
        Me.Label24.TabIndex = 30
        Me.Label24.Text = "A"
        Me.Label24.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label25
        '
        Me.Label25.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label25.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label25.Location = New System.Drawing.Point(136, 296)
        Me.Label25.Name = "Label25"
        Me.Label25.Size = New System.Drawing.Size(24, 24)
        Me.Label25.TabIndex = 31
        Me.Label25.Text = "B"
        Me.Label25.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label26
        '
        Me.Label26.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label26.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label26.Location = New System.Drawing.Point(88, 296)
        Me.Label26.Name = "Label26"
        Me.Label26.Size = New System.Drawing.Size(24, 24)
        Me.Label26.TabIndex = 32
        Me.Label26.Text = "C"
        Me.Label26.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label27
        '
        Me.Label27.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label27.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label27.Location = New System.Drawing.Point(80, 272)
        Me.Label27.Name = "Label27"
        Me.Label27.Size = New System.Drawing.Size(24, 24)
        Me.Label27.TabIndex = 35
        Me.Label27.Text = "D"
        Me.Label27.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label28
        '
        Me.Label28.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label28.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label28.Location = New System.Drawing.Point(72, 248)
        Me.Label28.Name = "Label28"
        Me.Label28.Size = New System.Drawing.Size(24, 24)
        Me.Label28.TabIndex = 34
        Me.Label28.Text = "E"
        Me.Label28.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label29
        '
        Me.Label29.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label29.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label29.Location = New System.Drawing.Point(104, 272)
        Me.Label29.Name = "Label29"
        Me.Label29.Size = New System.Drawing.Size(24, 24)
        Me.Label29.TabIndex = 33
        Me.Label29.Text = "F"
        Me.Label29.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label30
        '
        Me.Label30.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label30.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label30.Location = New System.Drawing.Point(128, 272)
        Me.Label30.Name = "Label30"
        Me.Label30.Size = New System.Drawing.Size(24, 24)
        Me.Label30.TabIndex = 41
        Me.Label30.Text = "G"
        Me.Label30.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label31
        '
        Me.Label31.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label31.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label31.Location = New System.Drawing.Point(152, 272)
        Me.Label31.Name = "Label31"
        Me.Label31.Size = New System.Drawing.Size(24, 24)
        Me.Label31.TabIndex = 40
        Me.Label31.Text = "H"
        Me.Label31.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label32
        '
        Me.Label32.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label32.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label32.Location = New System.Drawing.Point(192, 248)
        Me.Label32.Name = "Label32"
        Me.Label32.Size = New System.Drawing.Size(24, 24)
        Me.Label32.TabIndex = 39
        Me.Label32.Text = "I"
        Me.Label32.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label33
        '
        Me.Label33.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label33.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label33.Location = New System.Drawing.Point(176, 272)
        Me.Label33.Name = "Label33"
        Me.Label33.Size = New System.Drawing.Size(24, 24)
        Me.Label33.TabIndex = 38
        Me.Label33.Text = "J"
        Me.Label33.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label34
        '
        Me.Label34.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label34.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label34.Location = New System.Drawing.Point(200, 272)
        Me.Label34.Name = "Label34"
        Me.Label34.Size = New System.Drawing.Size(24, 24)
        Me.Label34.TabIndex = 37
        Me.Label34.Text = "K"
        Me.Label34.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label35
        '
        Me.Label35.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label35.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label35.Location = New System.Drawing.Point(224, 272)
        Me.Label35.Name = "Label35"
        Me.Label35.Size = New System.Drawing.Size(24, 24)
        Me.Label35.TabIndex = 36
        Me.Label35.Text = "L"
        Me.Label35.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label36
        '
        Me.Label36.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label36.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label36.Location = New System.Drawing.Point(184, 296)
        Me.Label36.Name = "Label36"
        Me.Label36.Size = New System.Drawing.Size(24, 24)
        Me.Label36.TabIndex = 47
        Me.Label36.Text = "M"
        Me.Label36.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label37
        '
        Me.Label37.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label37.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label37.Location = New System.Drawing.Point(160, 296)
        Me.Label37.Name = "Label37"
        Me.Label37.Size = New System.Drawing.Size(24, 24)
        Me.Label37.TabIndex = 46
        Me.Label37.Text = "N"
        Me.Label37.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label38
        '
        Me.Label38.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label38.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label38.Location = New System.Drawing.Point(216, 248)
        Me.Label38.Name = "Label38"
        Me.Label38.Size = New System.Drawing.Size(24, 24)
        Me.Label38.TabIndex = 45
        Me.Label38.Text = "O"
        Me.Label38.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label39
        '
        Me.Label39.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label39.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label39.Location = New System.Drawing.Point(240, 248)
        Me.Label39.Name = "Label39"
        Me.Label39.Size = New System.Drawing.Size(24, 24)
        Me.Label39.TabIndex = 44
        Me.Label39.Text = "P"
        Me.Label39.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label40
        '
        Me.Label40.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label40.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label40.Location = New System.Drawing.Point(24, 248)
        Me.Label40.Name = "Label40"
        Me.Label40.Size = New System.Drawing.Size(24, 24)
        Me.Label40.TabIndex = 43
        Me.Label40.Text = "Q"
        Me.Label40.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label41
        '
        Me.Label41.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label41.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label41.Location = New System.Drawing.Point(96, 248)
        Me.Label41.Name = "Label41"
        Me.Label41.Size = New System.Drawing.Size(24, 24)
        Me.Label41.TabIndex = 42
        Me.Label41.Text = "R"
        Me.Label41.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label42
        '
        Me.Label42.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label42.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label42.Location = New System.Drawing.Point(56, 272)
        Me.Label42.Name = "Label42"
        Me.Label42.Size = New System.Drawing.Size(24, 24)
        Me.Label42.TabIndex = 53
        Me.Label42.Text = "S"
        Me.Label42.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label43
        '
        Me.Label43.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label43.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label43.Location = New System.Drawing.Point(120, 248)
        Me.Label43.Name = "Label43"
        Me.Label43.Size = New System.Drawing.Size(24, 24)
        Me.Label43.TabIndex = 52
        Me.Label43.Text = "T"
        Me.Label43.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label44
        '
        Me.Label44.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label44.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label44.Location = New System.Drawing.Point(168, 248)
        Me.Label44.Name = "Label44"
        Me.Label44.Size = New System.Drawing.Size(24, 24)
        Me.Label44.TabIndex = 51
        Me.Label44.Text = "U"
        Me.Label44.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label45
        '
        Me.Label45.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label45.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label45.Location = New System.Drawing.Point(112, 296)
        Me.Label45.Name = "Label45"
        Me.Label45.Size = New System.Drawing.Size(24, 24)
        Me.Label45.TabIndex = 50
        Me.Label45.Text = "V"
        Me.Label45.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label46
        '
        Me.Label46.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label46.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label46.Location = New System.Drawing.Point(48, 248)
        Me.Label46.Name = "Label46"
        Me.Label46.Size = New System.Drawing.Size(24, 24)
        Me.Label46.TabIndex = 49
        Me.Label46.Text = "W"
        Me.Label46.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label47
        '
        Me.Label47.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label47.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label47.Location = New System.Drawing.Point(64, 296)
        Me.Label47.Name = "Label47"
        Me.Label47.Size = New System.Drawing.Size(24, 24)
        Me.Label47.TabIndex = 48
        Me.Label47.Text = "X"
        Me.Label47.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label48
        '
        Me.Label48.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label48.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label48.Location = New System.Drawing.Point(144, 248)
        Me.Label48.Name = "Label48"
        Me.Label48.Size = New System.Drawing.Size(24, 24)
        Me.Label48.TabIndex = 59
        Me.Label48.Text = "Y"
        Me.Label48.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label49
        '
        Me.Label49.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label49.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label49.Location = New System.Drawing.Point(40, 296)
        Me.Label49.Name = "Label49"
        Me.Label49.Size = New System.Drawing.Size(24, 24)
        Me.Label49.TabIndex = 58
        Me.Label49.Text = "Z"
        Me.Label49.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label50
        '
        Me.Label50.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label50.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label50.Location = New System.Drawing.Point(232, 224)
        Me.Label50.Name = "Label50"
        Me.Label50.Size = New System.Drawing.Size(24, 24)
        Me.Label50.TabIndex = 62
        Me.Label50.Text = "0"
        Me.Label50.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label51
        '
        Me.Label51.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label51.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label51.Location = New System.Drawing.Point(16, 224)
        Me.Label51.Name = "Label51"
        Me.Label51.Size = New System.Drawing.Size(24, 24)
        Me.Label51.TabIndex = 61
        Me.Label51.Text = "1"
        Me.Label51.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label52
        '
        Me.Label52.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label52.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label52.Location = New System.Drawing.Point(40, 224)
        Me.Label52.Name = "Label52"
        Me.Label52.Size = New System.Drawing.Size(24, 24)
        Me.Label52.TabIndex = 60
        Me.Label52.Text = "2"
        Me.Label52.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label53
        '
        Me.Label53.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label53.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label53.Location = New System.Drawing.Point(64, 224)
        Me.Label53.Name = "Label53"
        Me.Label53.Size = New System.Drawing.Size(24, 24)
        Me.Label53.TabIndex = 65
        Me.Label53.Text = "3"
        Me.Label53.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label54
        '
        Me.Label54.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label54.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label54.Location = New System.Drawing.Point(88, 224)
        Me.Label54.Name = "Label54"
        Me.Label54.Size = New System.Drawing.Size(24, 24)
        Me.Label54.TabIndex = 64
        Me.Label54.Text = "4"
        Me.Label54.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label55
        '
        Me.Label55.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label55.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label55.Location = New System.Drawing.Point(112, 224)
        Me.Label55.Name = "Label55"
        Me.Label55.Size = New System.Drawing.Size(24, 24)
        Me.Label55.TabIndex = 63
        Me.Label55.Text = "5"
        Me.Label55.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label56
        '
        Me.Label56.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label56.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label56.Location = New System.Drawing.Point(136, 224)
        Me.Label56.Name = "Label56"
        Me.Label56.Size = New System.Drawing.Size(24, 24)
        Me.Label56.TabIndex = 68
        Me.Label56.Text = "6"
        Me.Label56.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label57
        '
        Me.Label57.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label57.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label57.Location = New System.Drawing.Point(160, 224)
        Me.Label57.Name = "Label57"
        Me.Label57.Size = New System.Drawing.Size(24, 24)
        Me.Label57.TabIndex = 67
        Me.Label57.Text = "7"
        Me.Label57.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label58
        '
        Me.Label58.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label58.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label58.Location = New System.Drawing.Point(184, 224)
        Me.Label58.Name = "Label58"
        Me.Label58.Size = New System.Drawing.Size(24, 24)
        Me.Label58.TabIndex = 66
        Me.Label58.Text = "8"
        Me.Label58.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label59
        '
        Me.Label59.BackColor = System.Drawing.Color.FromArgb(CType(0, Byte), CType(192, Byte), CType(0, Byte))
        Me.Label59.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.Label59.Location = New System.Drawing.Point(208, 224)
        Me.Label59.Name = "Label59"
        Me.Label59.Size = New System.Drawing.Size(24, 24)
        Me.Label59.TabIndex = 71
        Me.Label59.Text = "9"
        Me.Label59.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'monitor
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 12)
        Me.ClientSize = New System.Drawing.Size(536, 341)
        Me.Controls.Add(Me.Label59)
        Me.Controls.Add(Me.Label56)
        Me.Controls.Add(Me.Label57)
        Me.Controls.Add(Me.Label58)
        Me.Controls.Add(Me.Label53)
        Me.Controls.Add(Me.Label54)
        Me.Controls.Add(Me.Label55)
        Me.Controls.Add(Me.Label50)
        Me.Controls.Add(Me.Label51)
        Me.Controls.Add(Me.Label52)
        Me.Controls.Add(Me.Label48)
        Me.Controls.Add(Me.Label49)
        Me.Controls.Add(Me.Label42)
        Me.Controls.Add(Me.Label43)
        Me.Controls.Add(Me.Label44)
        Me.Controls.Add(Me.Label45)
        Me.Controls.Add(Me.Label46)
        Me.Controls.Add(Me.Label47)
        Me.Controls.Add(Me.Label36)
        Me.Controls.Add(Me.Label37)
        Me.Controls.Add(Me.Label38)
        Me.Controls.Add(Me.Label39)
        Me.Controls.Add(Me.Label40)
        Me.Controls.Add(Me.Label41)
        Me.Controls.Add(Me.Label30)
        Me.Controls.Add(Me.Label31)
        Me.Controls.Add(Me.Label32)
        Me.Controls.Add(Me.Label33)
        Me.Controls.Add(Me.Label34)
        Me.Controls.Add(Me.Label35)
        Me.Controls.Add(Me.Label27)
        Me.Controls.Add(Me.Label28)
        Me.Controls.Add(Me.Label29)
        Me.Controls.Add(Me.Label26)
        Me.Controls.Add(Me.Label25)
        Me.Controls.Add(Me.Label24)
        Me.Controls.Add(Me.Label20)
        Me.Controls.Add(Me.Label21)
        Me.Controls.Add(Me.Label22)
        Me.Controls.Add(Me.Label23)
        Me.Controls.Add(Me.Label16)
        Me.Controls.Add(Me.Label17)
        Me.Controls.Add(Me.Label18)
        Me.Controls.Add(Me.Label19)
        Me.Controls.Add(Me.Label12)
        Me.Controls.Add(Me.Label13)
        Me.Controls.Add(Me.Label14)
        Me.Controls.Add(Me.Label15)
        Me.Controls.Add(Me.Label10)
        Me.Controls.Add(Me.Label11)
        Me.Controls.Add(Me.Label9)
        Me.Controls.Add(Me.ListBox1)
        Me.Controls.Add(Me.Label8)
        Me.Controls.Add(Me.Label6)
        Me.Controls.Add(Me.Label7)
        Me.Controls.Add(Me.Label5)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.Label4)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Label1)
        Me.Name = "monitor"
        Me.Text = "monitor"
        Me.ResumeLayout(False)

    End Sub

#End Region

    Public Sub showmousebuttons()
        Dim cl1 As Color = Color.Aqua
        Dim cl2 As Color = Color.FromArgb(0, 192, 192)
        Me.Label1.BackColor = IIf(forms.MouseButton(Form1.MouseButtons, 1), cl1, cl2)
        Me.Label2.BackColor = IIf(forms.MouseButton(Form1.MouseButtons, 2), cl1, cl2)
        Me.Label3.BackColor = IIf(forms.MouseButton(Form1.MouseButtons, 3), cl1, cl2)
        Me.Label4.BackColor = IIf(forms.MouseButton(Form1.MouseButtons, 4), cl1, cl2)
        Me.Label5.BackColor = IIf(forms.MouseButton(Form1.MouseButtons, 5), cl1, cl2)
    End Sub
    Public Sub mousemoving()
        Label6.Text = "x : " & MousePosition.X
        Label7.Text = "y : " & MousePosition.Y
    End Sub
    Public Sub showkeys()
        Dim cl1 = Color.Yellow
        Dim cl2 = Color.FromArgb(192, 192, 0)
        Label15.BackColor = IIf(Mid(forms.keyF, 1, 1) = "t", cl1, cl2)
        Label14.BackColor = IIf(Mid(forms.keyF, 2, 1) = "t", cl1, cl2)
        Label13.BackColor = IIf(Mid(forms.keyF, 3, 1) = "t", cl1, cl2)
        Label12.BackColor = IIf(Mid(forms.keyF, 4, 1) = "t", cl1, cl2)
        Label19.BackColor = IIf(Mid(forms.keyF, 5, 1) = "t", cl1, cl2)
        Label18.BackColor = IIf(Mid(forms.keyF, 6, 1) = "t", cl1, cl2)
        Label17.BackColor = IIf(Mid(forms.keyF, 7, 1) = "t", cl1, cl2)
        Label16.BackColor = IIf(Mid(forms.keyF, 8, 1) = "t", cl1, cl2)
        Label23.BackColor = IIf(Mid(forms.keyF, 9, 1) = "t", cl1, cl2)
        Label22.BackColor = IIf(Mid(forms.keyF, 10, 1) = "t", cl1, cl2)
        Label21.BackColor = IIf(Mid(forms.keyF, 11, 1) = "t", cl1, cl2)
        Label20.BackColor = IIf(Mid(forms.keyF, 12, 1) = "t", cl1, cl2)
        Label10.BackColor = IIf(Mid(forms.keyS, 1, 1) = "t", cl1, cl2)
        Label8.BackColor = IIf(Mid(forms.keyS, 2, 1) = "t", cl1, cl2)
        Label9.BackColor = IIf(Mid(forms.keyS, 3, 1) = "t", cl1, cl2)
        Label11.BackColor = IIf(Mid(forms.keyS, 4, 1) = "t", cl1, cl2)
        cl1 = Color.FromArgb(0, 255, 0)
        cl2 = Color.FromArgb(0, 192, 0)
        Dim n
        For n = 0 To 25
            forms.keylabels(n).BackColor = IIf(Mid(forms.keyL, n + 1, 1) = "t", cl1, cl2)
        Next
        For n = 26 To 35
            forms.keylabels(n).BackColor = IIf(Mid(forms.keyN, n - 25, 1) = "t", cl1, cl2)
        Next
    End Sub
    Public parent1 As Form1
    Private Sub monitor_Closing(ByVal sender As Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles MyBase.Closing
        parent1.monitorshowed = False
    End Sub


    Private Sub monitor_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        forms.keylabels(0) = Label24
        forms.keylabels(1) = Label25
        forms.keylabels(2) = Label26
        forms.keylabels(3) = Label27
        forms.keylabels(4) = Label28
        forms.keylabels(5) = Label29
        forms.keylabels(6) = Label30
        forms.keylabels(7) = Label31
        forms.keylabels(8) = Label32
        forms.keylabels(9) = Label33
        forms.keylabels(10) = Label34
        forms.keylabels(11) = Label35
        forms.keylabels(12) = Label36
        forms.keylabels(13) = Label37
        forms.keylabels(14) = Label38
        forms.keylabels(15) = Label39
        forms.keylabels(16) = Label40
        forms.keylabels(17) = Label41
        forms.keylabels(18) = Label42
        forms.keylabels(19) = Label43
        forms.keylabels(20) = Label44
        forms.keylabels(21) = Label45
        forms.keylabels(22) = Label46
        forms.keylabels(23) = Label47
        forms.keylabels(24) = Label48
        forms.keylabels(25) = Label49
        forms.keylabels(26) = Label50
        forms.keylabels(27) = Label51
        forms.keylabels(28) = Label52
        forms.keylabels(29) = Label53
        forms.keylabels(30) = Label54
        forms.keylabels(31) = Label55
        forms.keylabels(32) = Label56
        forms.keylabels(33) = Label57
        forms.keylabels(34) = Label58
        forms.keylabels(35) = Label59

        ListBox1.Items.Add("最小化Windowの配列 : " & SystemInformation.ArrangeDirection.ToString & " ; " & SystemInformation.ArrangeStartingPosition.ToString)
        ListBox1.Items.Add("起動方法 : " & SystemInformation.BootMode.ToString)
        ListBox1.Items.Add("3D境界線 : " & SystemInformation.Border3DSize.ToString)
        ListBox1.Items.Add("Win境界線 : " & SystemInformation.BorderSize.ToString)
        ListBox1.Items.Add("TitlebarButton : " & SystemInformation.CaptionButtonSize.ToString)
        ListBox1.Items.Add("Titlebarheight : " & SystemInformation.CaptionHeight.ToString)
        ListBox1.Items.Add("Com名 : " & SystemInformation.ComputerName.ToString)
        ListBox1.Items.Add("CursorSize : " & SystemInformation.CursorSize.ToString)
        ListBox1.Items.Add("DBCS使用の是非 : " & SystemInformation.DbcsEnabled.ToString)
        ListBox1.Items.Add("Debug User.exe の有無 : " & SystemInformation.DebugOS.ToString)
        ListBox1.Items.Add("DblClick 範囲 : " & SystemInformation.DoubleClickSize.ToString & "; 間隔 : " & SystemInformation.DoubleClickTime.ToString & "ms")
        ListBox1.Items.Add("WinDragの可否 : " & SystemInformation.DragFullWindows.ToString)
        ListBox1.Items.Add("最小Dragサイズ : " & SystemInformation.DragSize.ToString)
        ListBox1.Items.Add("固定Win境界線 : " & SystemInformation.FixedFrameBorderSize.ToString)
        ListBox1.Items.Add("可変Win境界線 : " & SystemInformation.FrameBorderSize.ToString)
        ListBox1.Items.Add("HiContrast 是非 : " & SystemInformation.HighContrast.ToString)
        ListBox1.Items.Add("水平ScrollBar 矢印 : " & SystemInformation.HorizontalScrollBarArrowWidth.ToString & " : 高 : " & SystemInformation.HorizontalScrollBarHeight.ToString & " ; 幅 : " & SystemInformation.HorizontalScrollBarThumbWidth.ToString)
        ListBox1.Items.Add("Icon 大きさ : " & SystemInformation.IconSize.ToString & " ; 周囲 : " & SystemInformation.IconSpacingSize.ToString)
        ListBox1.Items.Add("漢字Win高さ : " & SystemInformation.KanjiWindowHeight.ToString)
        ListBox1.Items.Add("最大Winサイズ : " & SystemInformation.MaxWindowTrackSize.ToString)
        ListBox1.Items.Add("Menu ボタン : " & SystemInformation.MenuButtonSize.ToString & " ; CheckMark : " & SystemInformation.MenuCheckSize.ToString & " ; 文字 : " & SystemInformation.MenuFont.ToString & " ; 高さ : " & SystemInformation.MenuHeight.ToString)
        ListBox1.Items.Add("ヘブライアラビア語可否 : " & SystemInformation.MidEastEnabled.ToString)
        ListBox1.Items.Add("最小化Win サイズ : " & SystemInformation.MinimizedWindowSize.ToString & " ; 間隔 : " & SystemInformation.MinimizedWindowSpacingSize.ToString)
    End Sub
End Class
